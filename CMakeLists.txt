# ==============================================================================
# Nanos Build Configuration
# ==============================================================================

cmake_minimum_required(VERSION 3.13.4)

project(Nanos)

# ==============================================================================
# Configuration
# ==============================================================================

if(NOT CMAKE_CONFIGURATION_TYPES AND NOT CMAKE_BUILD_TYPE)
  set(CMAKE_BUILD_TYPE Release)
endif()

if(CMAKE_BUILD_TYPE)
  message(STATUS "Build type: ${CMAKE_BUILD_TYPE}")
endif()

# ==============================================================================
# Dependencies
# ==============================================================================

# --- LLVM ---
execute_process(
  COMMAND llvm-config --cmakedir
  OUTPUT_VARIABLE llvm_cmakedir
  OUTPUT_STRIP_TRAILING_WHITESPACE)

find_package(LLVM REQUIRED CONFIG PATHS ${llvm_cmakedir} NO_DEFAULT_PATH)
message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
message(STATUS "Using LLVMConfig.cmake in: ${LLVM_DIR}")

execute_process(
  COMMAND llvm-config --libs all
  OUTPUT_VARIABLE llvm_libs
  OUTPUT_STRIP_TRAILING_WHITESPACE)

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

# ==============================================================================
# Compiler Configuration
# ==============================================================================

set(CMAKE_C_COMPILER "${LLVM_DIR}/../../../bin/clang")
set(CMAKE_CXX_COMPILER "${LLVM_DIR}/../../../bin/clang++")

# Use LLD linker
add_link_options("-fuse-ld=lld")

# Common compiler flags
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wundef -Werror")

# Build-specific flags
set(CMAKE_CXX_FLAGS_RELEASE "${CMAKE_CXX_FLAGS_RELEASE} -flto")
set(CMAKE_LINKER_FLAGS_RELEASE "${CMAKE_LINKER_FLAGS_RELEASE} -Wundef -Werror -flto")

# Debug flags with hardware address sanitizer
set(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} -fno-omit-frame-pointer -fno-optimize-sibling-calls -fsanitize=hwaddress")
set(CMAKE_LINKER_FLAGS_DEBUG "${CMAKE_LINKER_FLAGS_DEBUG} -Wundef -Werror -fsanitize=hwaddress")

# RelWithDebInfo flags
set(CMAKE_CXX_FLAGS_RELWITHDEBINFO "${CMAKE_CXX_FLAGS_RELWITHDEBINFO} -fno-omit-frame-pointer -fno-optimize-sibling-calls -fsanitize=hwaddress")
set(CMAKE_LINKER_FLAGS_RELWITHDEBINFO "${CMAKE_LINKER_FLAGS_RELWITHDEBINFO} -Wundef -Werror")

# ==============================================================================
# Installation Directories
# ==============================================================================

set(NANOS_SHARE_DIRECTORY ${CMAKE_INSTALL_PREFIX}/share/nanos)
set(NANOS_EXTENSION_DIRECTORY ${CMAKE_INSTALL_PREFIX}/lib/nanos)
set(NANOS_SITELIB_DIRECTORY ${NANOS_SHARE_DIRECTORY}/sitelib)

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
)

# ==============================================================================
# Common Dependencies
# ==============================================================================

set(NANOS_LINK_LIBRARIES ${llvm_libs} Boost::context -pthread -ldl)

# ==============================================================================
# Targets
# ==============================================================================

# --- Core Library ---
# Static library for core sources shared across all executables
add_library(nanos_core STATIC ${NANOS_CORE_SOURCES})
target_include_directories(nanos_core PRIVATE ${LLVM_INCLUDE_DIRS} ${CMAKE_CURRENT_SOURCE_DIR}/src)
target_compile_definitions(nanos_core PRIVATE
  ${LLVM_DEFINITIONS}
  SYSTEM_SHARE_PATH="${NANOS_SHARE_DIRECTORY}"
  SYSTEM_EXTENSION_PATH="${NANOS_EXTENSION_DIRECTORY}"
  $<IF:$<EQUAL:${CMAKE_SIZEOF_VOID_P},4>,DEFAULT_HEAP_LIMIT=512,DEFAULT_HEAP_LIMIT=2048>
)

# --- I/O Library ---
# Static library for reader/printer sources used by multiple targets
add_library(nanos_io STATIC src/reader.cpp src/printer.cpp)
target_include_directories(nanos_io PRIVATE ${LLVM_INCLUDE_DIRS} ${CMAKE_CURRENT_SOURCE_DIR}/src)
target_compile_definitions(nanos_io PRIVATE
  ${LLVM_DEFINITIONS}
  SYSTEM_SHARE_PATH="${NANOS_SHARE_DIRECTORY}"
  SYSTEM_EXTENSION_PATH="${NANOS_EXTENSION_DIRECTORY}"
  $<IF:$<EQUAL:${CMAKE_SIZEOF_VOID_P},4>,DEFAULT_HEAP_LIMIT=512,DEFAULT_HEAP_LIMIT=2048>
)

# --- Main Executable ---
add_executable(nanos src/nanos.cpp)
target_include_directories(nanos PRIVATE ${LLVM_INCLUDE_DIRS} ${CMAKE_CURRENT_SOURCE_DIR}/src)
target_compile_definitions(nanos PRIVATE
  ${LLVM_DEFINITIONS}
  SYSTEM_SHARE_PATH="${NANOS_SHARE_DIRECTORY}"
  SYSTEM_EXTENSION_PATH="${NANOS_EXTENSION_DIRECTORY}"
  $<IF:$<EQUAL:${CMAKE_SIZEOF_VOID_P},4>,DEFAULT_HEAP_LIMIT=512,DEFAULT_HEAP_LIMIT=2048>
)
target_link_libraries(nanos nanos_core nanos_io ${NANOS_LINK_LIBRARIES})

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
  target_include_directories(${test_name} PRIVATE ${LLVM_INCLUDE_DIRS} ${CMAKE_CURRENT_SOURCE_DIR}/src)
  target_compile_definitions(${test_name} PRIVATE
    ${LLVM_DEFINITIONS}
    SYSTEM_SHARE_PATH="${NANOS_SHARE_DIRECTORY}"
    SYSTEM_EXTENSION_PATH="${NANOS_EXTENSION_DIRECTORY}"
    $<IF:$<EQUAL:${CMAKE_SIZEOF_VOID_P},4>,DEFAULT_HEAP_LIMIT=512,DEFAULT_HEAP_LIMIT=2048>
  )
  target_link_libraries(${test_name} nanos_core ${NANOS_LINK_LIBRARIES})
  add_test(
    NAME ${test_name}
    COMMAND $<TARGET_FILE:${test_name}>
    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
  )
endmacro()

add_nanos_test(test_object tests/test_object.cpp)
add_nanos_test(test_gc tests/test_gc.cpp)

# Tests that need I/O library
add_nanos_test(test_printer tests/test_printer.cpp)
target_link_libraries(test_printer nanos_io)

add_nanos_test(test_reader tests/test_reader.cpp)
target_link_libraries(test_reader nanos_io)

add_nanos_test(test_r7rs tests/test_r7rs.cpp)
target_link_libraries(test_r7rs nanos_io)

add_nanos_test(test_radix tests/test_radix.cpp)
target_link_libraries(test_radix nanos_io)

add_nanos_test(test_hash tests/test_hash.cpp)
target_link_libraries(test_hash nanos_io)

add_nanos_test(test_symbol tests/test_symbol.cpp)
