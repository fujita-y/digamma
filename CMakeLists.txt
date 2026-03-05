# ==============================================================================
# Nanos Build Configuration
# ==============================================================================

cmake_minimum_required(VERSION 3.13.4)

project(Nanos)

# ==============================================================================
# Configuration
# ==============================================================================

find_program(CCACHE_PROGRAM ccache)
if(CCACHE_PROGRAM)
  message(STATUS "Found ccache: ${CCACHE_PROGRAM}")
  set(CMAKE_C_COMPILER_LAUNCHER "${CCACHE_PROGRAM}")
  set(CMAKE_CXX_COMPILER_LAUNCHER "${CCACHE_PROGRAM}")
endif()

# --- LLVM ---
execute_process(
  COMMAND llvm-config --cmakedir
  OUTPUT_VARIABLE llvm_cmakedir
  OUTPUT_STRIP_TRAILING_WHITESPACE)

find_package(LLVM REQUIRED CONFIG PATHS ${llvm_cmakedir} NO_DEFAULT_PATH)

set(CMAKE_C_COMPILER "${LLVM_DIR}/../../../bin/clang")
set(CMAKE_CXX_COMPILER "${LLVM_DIR}/../../../bin/clang++")

if(NOT CMAKE_CONFIGURATION_TYPES AND NOT CMAKE_BUILD_TYPE)
  set(CMAKE_BUILD_TYPE Release)
endif()

if(CMAKE_BUILD_TYPE)
  message(STATUS "Build type: ${CMAKE_BUILD_TYPE}")
endif()

message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
message(STATUS "Using LLVMConfig.cmake in: ${LLVM_DIR}")

execute_process(
  COMMAND llvm-config --libs all
  OUTPUT_VARIABLE llvm_libs
  OUTPUT_STRIP_TRAILING_WHITESPACE)

# ==============================================================================
# Dependencies
# ==============================================================================

# --- Boost (via vcpkg) ---
if(NOT DEFINED CMAKE_TOOLCHAIN_FILE)
  # Automatically detect vcpkg installation for the current architecture
  if(EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/vcpkg_installed")
    # Detect triplet based on system processor
    if(CMAKE_SYSTEM_PROCESSOR MATCHES "aarch64|arm64")
      set(VCPKG_TRIPLET "arm64-linux")
    elseif(CMAKE_SYSTEM_PROCESSOR MATCHES "x86_64|AMD64")
      set(VCPKG_TRIPLET "x64-linux")
    else()
      set(VCPKG_TRIPLET "${CMAKE_SYSTEM_PROCESSOR}-linux")
    endif()
    list(APPEND CMAKE_PREFIX_PATH "${CMAKE_CURRENT_SOURCE_DIR}/vcpkg_installed/${VCPKG_TRIPLET}")
  endif()
endif()

find_package(Boost CONFIG REQUIRED COMPONENTS context)
find_package(replxx CONFIG REQUIRED)
find_package(Threads REQUIRED)

# ==============================================================================
# Compiler Configuration
# ==============================================================================

add_link_options("-fuse-ld=lld")

# ==============================================================================
# Installation Directories
# ==============================================================================

set(NANOS_SHARE_DIRECTORY ${CMAKE_INSTALL_PREFIX}/share/nanos)
set(NANOS_EXTENSION_DIRECTORY ${CMAKE_INSTALL_PREFIX}/lib/nanos)
set(NANOS_SITELIB_DIRECTORY ${NANOS_SHARE_DIRECTORY}/sitelib)

# ==============================================================================
# Interface Configuration
# ==============================================================================

add_library(nanos_config INTERFACE)

# Define compile features
target_compile_features(nanos_config INTERFACE cxx_std_20)

# Define include directories
target_include_directories(nanos_config INTERFACE
  ${LLVM_INCLUDE_DIRS}
  ${Boost_INCLUDE_DIRS}
  ${CMAKE_CURRENT_SOURCE_DIR}/src
)

# Define compile definitions
target_compile_definitions(nanos_config INTERFACE
  ${LLVM_DEFINITIONS}
  SYSTEM_SHARE_PATH="${NANOS_SHARE_DIRECTORY}"
  SYSTEM_EXTENSION_PATH="${NANOS_EXTENSION_DIRECTORY}"
  $<$<EQUAL:${CMAKE_SIZEOF_VOID_P},4>:DEFAULT_HEAP_LIMIT=512>
  $<$<NOT:$<EQUAL:${CMAKE_SIZEOF_VOID_P},4>>:DEFAULT_HEAP_LIMIT=2048>
)

# Define compile options
target_compile_options(nanos_config INTERFACE
  -Werror
  $<$<CONFIG:Release>:-flto>
  $<$<CONFIG:Debug>:-fno-omit-frame-pointer>
  $<$<CONFIG:Debug>:-fno-optimize-sibling-calls>
  $<$<CONFIG:Debug>:-fsanitize=hwaddress>
  $<$<CONFIG:RelWithDebInfo>:-fno-omit-frame-pointer>
  $<$<CONFIG:RelWithDebInfo>:-fno-optimize-sibling-calls>
  $<$<CONFIG:RelWithDebInfo>:-fsanitize=hwaddress>
)

# Define link options
target_link_options(nanos_config INTERFACE
  -Werror
  $<$<CONFIG:Release>:-flto>
  $<$<CONFIG:Debug>:-fsanitize=hwaddress>
  $<$<CONFIG:RelWithDebInfo>:-fsanitize=hwaddress>
)

# ==============================================================================
# Sources
# ==============================================================================

set(NANOS_CORE_SOURCES
  src/bit.cpp
  src/object_heap.cpp
  src/object.cpp
  src/concurrent_heap.cpp
  src/concurrent_pool.cpp
  src/concurrent_slab.cpp
  src/utf8.cpp
  src/hash.cpp
  src/vector.cpp
  src/codegen.cpp
  src/nanos_jit.cpp
  src/codegen_aux.cpp
  src/codegen_emit.cpp
  src/nanos_subr.cpp
  src/nanos_context.cpp
  src/arch_arm64.cpp
  src/equiv.cpp
)

# ==============================================================================
# Common Dependencies
# ==============================================================================

set(NANOS_LINK_LIBRARIES ${llvm_libs} Boost::context replxx::replxx Threads::Threads -pthread -ldl)

# ==============================================================================
# Targets
# ==============================================================================

# --- Core Library ---
# Object library for core sources shared across all executables
add_library(nanos_core OBJECT ${NANOS_CORE_SOURCES})
target_link_libraries(nanos_core PUBLIC nanos_config)

# --- I/O Library ---
# Object library for reader/printer sources used by multiple targets
add_library(nanos_io OBJECT src/reader.cpp src/printer.cpp)
target_link_libraries(nanos_io PUBLIC nanos_config)

# --- Main Executable ---
add_executable(nanos src/main.cpp src/nanos.cpp)
target_link_libraries(nanos PRIVATE nanos_core nanos_io nanos_config ${NANOS_LINK_LIBRARIES})

# ==============================================================================
# Installation
# ==============================================================================

install(DIRECTORY sitelib/ DESTINATION ${NANOS_SITELIB_DIRECTORY})
install(TARGETS nanos DESTINATION bin)

# ==============================================================================
# Testing
# ==============================================================================

enable_testing()

# Macro to add tests easily
macro(add_nanos_test test_name test_source)
  add_executable(
    ${test_name}
    ${test_source}
    ${ARGN}  # Additional sources
  )
  target_link_libraries(${test_name} PRIVATE nanos_core nanos_io nanos_config ${NANOS_LINK_LIBRARIES})
  add_test(
    NAME ${test_name}
    COMMAND $<TARGET_FILE:${test_name}>
    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
  )
endmacro()

add_nanos_test(test_object tests/test_object.cpp)
add_nanos_test(test_gc tests/test_gc.cpp)
add_nanos_test(test_printer tests/test_printer.cpp)
add_nanos_test(test_printer_printf tests/test_printer_printf.cpp)
add_nanos_test(test_reader tests/test_reader.cpp)
add_nanos_test(test_r7rs tests/test_r7rs.cpp)
add_nanos_test(test_radix tests/test_radix.cpp)
add_nanos_test(test_hash tests/test_hash.cpp)
add_nanos_test(test_symbol tests/test_symbol.cpp)
add_nanos_test(test_closure_analysis tests/test_closure_analysis.cpp)
add_nanos_test(test_codegen tests/test_codegen.cpp)
add_nanos_test(test_closure_patterns tests/test_closure_patterns.cpp)
add_nanos_test(test_apply_patterns tests/test_apply_patterns.cpp)
add_nanos_test(test_deriv tests/test_deriv.cpp)
add_nanos_test(test_equiv tests/test_equiv.cpp)
add_nanos_test(test_call_ec tests/test_call_ec.cpp)
add_nanos_test(test_call_cc tests/test_call_cc.cpp)
add_nanos_test(test_dynamic_wind tests/test_dynamic_wind.cpp)
add_nanos_test(test_subr tests/test_subr.cpp)
