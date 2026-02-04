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

include_directories(${LLVM_INCLUDE_DIRS} ${CMAKE_CURRENT_SOURCE_DIR}/src)
separate_arguments(LLVM_DEFINITIONS_LIST NATIVE_COMMAND ${LLVM_DEFINITIONS})
add_definitions(${LLVM_DEFINITIONS_LIST})

# ==============================================================================
# Compiler Configuration
# ==============================================================================

set(CMAKE_C_COMPILER "${LLVM_DIR}/../../../bin/clang")
set(CMAKE_CXX_COMPILER "${LLVM_DIR}/../../../bin/clang++")

# Use LLD linker
add_link_options("-fuse-ld=lld")

# Common warning flags
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wundef -Werror")

# Release flags
set(CMAKE_CXX_FLAGS_RELEASE "${CMAKE_CXX_FLAGS_RELEASE} -flto")
set(CMAKE_LINKER_FLAGS_RELEASE "${CMAKE_LINKER_FLAGS_RELEASE} -Wundef -Werror -flto")

# Debug flags
set(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} -fno-omit-frame-pointer -fno-optimize-sibling-calls -fsanitize=hwaddress")
set(CMAKE_LINKER_FLAGS_DEBUG "${CMAKE_LINKER_FLAGS_DEBUG} -Wundef -Werror -fsanitize=hwaddress")

# RelWithDebInfo flags
set(CMAKE_CXX_FLAGS_RELWITHDEBINFO "${CMAKE_CXX_FLAGS_RELWITHDEBINFO} -fno-omit-frame-pointer -fno-optimize-sibling-calls -fsanitize=hwaddress")
set(CMAKE_LINKER_FLAGS_RELWITHDEBINFO "${CMAKE_LINKER_FLAGS_RELWITHDEBINFO} -Wundef -Werror")

# ==============================================================================
# Definitions
# ==============================================================================

set(NANOS_SHARE_DIRECTORY ${CMAKE_INSTALL_PREFIX}/share/nanos)
set(NANOS_EXTENSION_DIRECTORY ${CMAKE_INSTALL_PREFIX}/lib/nanos)

add_compile_definitions(SYSTEM_SHARE_PATH="${NANOS_SHARE_DIRECTORY}")
add_compile_definitions(SYSTEM_EXTENSION_PATH="${NANOS_EXTENSION_DIRECTORY}")

if(CMAKE_SIZEOF_VOID_P EQUAL 4)
  add_compile_definitions(DEFAULT_HEAP_LIMIT=512)
else()
  add_compile_definitions(DEFAULT_HEAP_LIMIT=2048)
endif()

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
# Library Dependencies
# ==============================================================================

execute_process(
  COMMAND llvm-config --libs all
  OUTPUT_VARIABLE llvm_libs
  OUTPUT_STRIP_TRAILING_WHITESPACE)

# ==============================================================================
# Targets
# ==============================================================================

add_executable(
  nanos
  ${NANOS_CORE_SOURCES}
  src/reader.cpp
  src/printer.cpp
  src/nanos.cpp
)

target_link_libraries(nanos ${llvm_libs} -pthread -ldl)

# ==============================================================================
# Installation
# ==============================================================================

set(NANOS_SITELIB_DIRECTORY ${NANOS_SHARE_DIRECTORY}/sitelib)
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
    ${NANOS_CORE_SOURCES}
  )
  target_link_libraries(${test_name} ${llvm_libs} -pthread -ldl)
  add_test(
    NAME ${test_name}
    COMMAND $<TARGET_FILE:${test_name}>
    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
  )
endmacro()

add_nanos_test(test_object tests/test_object.cpp)
add_nanos_test(test_gc tests/test_gc.cpp)
add_nanos_test(test_printer tests/test_printer.cpp src/printer.cpp)
add_nanos_test(test_reader tests/test_reader.cpp src/reader.cpp src/printer.cpp)
add_nanos_test(test_r7rs tests/test_r7rs.cpp src/reader.cpp src/printer.cpp)
add_nanos_test(test_radix tests/test_radix.cpp src/reader.cpp src/printer.cpp)
add_nanos_test(test_hash tests/test_hash.cpp src/reader.cpp src/printer.cpp)
add_nanos_test(test_symbol tests/test_symbol.cpp)
