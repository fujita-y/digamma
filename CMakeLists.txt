cmake_minimum_required(VERSION 3.13.4)

project(Nanos)

if(NOT CMAKE_CONFIGURATION_TYPES AND NOT CMAKE_BUILD_TYPE)
  set(CMAKE_BUILD_TYPE Release) # Debug Release RelWithDebInfo MinSizeRel
endif()
if(CMAKE_BUILD_TYPE)
  message(STATUS "Build type: ${CMAKE_BUILD_TYPE}")
endif()
if(CMAKE_CONFIGURATION_TYPES)
  message(STATUS "Configuration types: ${CMAKE_CONFIGURATION_TYPES}")
endif()

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

set(CMAKE_C_COMPILER "${LLVM_DIR}/../../../bin/clang")
set(CMAKE_CXX_COMPILER "${LLVM_DIR}/../../../bin/clang++")

if(NOT APPLE)
  add_link_options("-fuse-ld=lld")
endif(NOT APPLE)

set(CMAKE_CXX_FLAGS_RELEASE "${CMAKE_CXX_FLAGS_RELEASE} -Wundef -Werror -flto")
set(CMAKE_LINKER_FLAGS_RELEASE "${CMAKE_LINKER_FLAGS_RELEASE} -Wundef -Werror -flto")
set(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} -Wundef -Werror -fno-omit-frame-pointer -fno-optimize-sibling-calls -fsanitize=hwaddress")
set(CMAKE_LINKER_FLAGS_DEBUG "${CMAKE_LINKER_FLAGS_DEBUG} -Wundef -Werror -fsanitize=hwaddress")
set(CMAKE_CXX_FLAGS_RELWITHDEBINFO "${CMAKE_CXX_FLAGS_RELWITHDEBINFO} -Wundef -Werror -fno-omit-frame-pointer -fno-optimize-sibling-calls -fsanitize=hwaddress")
set(CMAKE_LINKER_FLAGS_RELWITHDEBINFO "${CMAKE_LINKER_FLAGS_RELWITHDEBINFO} -Wundef -Werror")

set(NANOS_SHARE_DIRECTORY ${CMAKE_INSTALL_PREFIX}/share/nanos)
set(NANOS_EXTENSION_DIRECTORY ${CMAKE_INSTALL_PREFIX}/lib/nanos)
add_compile_definitions(SYSTEM_SHARE_PATH="${NANOS_SHARE_DIRECTORY}")
add_compile_definitions(SYSTEM_EXTENSION_PATH="${NANOS_EXTENSION_DIRECTORY}")

if(CMAKE_SIZEOF_VOID_P EQUAL 4)
  add_compile_definitions(DEFAULT_HEAP_LIMIT=512)
else()
  add_compile_definitions(DEFAULT_HEAP_LIMIT=2048)
endif()

set(NANOS_CORE_SOURCES
  src/bit.cpp
  src/object_heap.cpp
  src/object.cpp
  src/concurrent_heap.cpp
  src/concurrent_pool.cpp
  src/concurrent_slab.cpp
  src/utf8.cpp
  src/hash.cpp)

add_executable(
  nanos
  ${NANOS_CORE_SOURCES}
  src/reader.cpp
  src/printer.cpp
  src/nanos.cpp)

execute_process(
  COMMAND llvm-config --libs all
  OUTPUT_VARIABLE llvm_libs
  OUTPUT_STRIP_TRAILING_WHITESPACE)
target_link_libraries(nanos ${llvm_libs} -pthread -ldl)


add_executable(
  test_object
  tests/test_object.cpp
  ${NANOS_CORE_SOURCES})
target_link_libraries(test_object ${llvm_libs} -pthread -ldl)

add_test(
  NAME test_object
  COMMAND $<TARGET_FILE:test_object>
  WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})

add_executable(
  test_gc
  tests/test_gc.cpp
  ${NANOS_CORE_SOURCES})
target_link_libraries(test_gc ${llvm_libs} -pthread -ldl)

add_test(
  NAME test_gc
  COMMAND $<TARGET_FILE:test_gc>
  WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})

add_executable(
  test_printer
  tests/test_printer.cpp
  src/printer.cpp
  ${NANOS_CORE_SOURCES})
target_link_libraries(test_printer ${llvm_libs} -pthread -ldl)

add_test(
  NAME test_printer
  COMMAND $<TARGET_FILE:test_printer>
  WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})

add_executable(
  test_reader
  tests/test_reader.cpp
  src/reader.cpp
  src/printer.cpp
  ${NANOS_CORE_SOURCES})
target_link_libraries(test_reader ${llvm_libs} -pthread -ldl)

add_test(
  NAME test_reader
  COMMAND $<TARGET_FILE:test_reader>
  WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})

add_executable(
  test_r7rs
  tests/test_r7rs.cpp
  src/reader.cpp
  src/printer.cpp
  ${NANOS_CORE_SOURCES})
target_link_libraries(test_r7rs ${llvm_libs} -pthread -ldl)

add_test(
  NAME test_r7rs
  COMMAND $<TARGET_FILE:test_r7rs>
  WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})

if(APPLE)

  execute_process(
    COMMAND llvm-config --ldflags
    OUTPUT_VARIABLE llvm_ldflags
    OUTPUT_STRIP_TRAILING_WHITESPACE)
  target_link_libraries(nanos ${llvm_ldflags})
endif(APPLE)

file(MAKE_DIRECTORY $ENV{HOME}/.nanos)

set(NANOS_SITELIB_DIRECTORY ${NANOS_SHARE_DIRECTORY}/sitelib)
install(CODE "execute_process(COMMAND rm -rf ${NANOS_SITELIB_DIRECTORY})")
install(DIRECTORY sitelib/ DESTINATION ${NANOS_SITELIB_DIRECTORY})
install(TARGETS nanos DESTINATION bin)

enable_testing()

add_executable(
  test_radix
  tests/test_radix.cpp
  src/reader.cpp
  src/printer.cpp
  ${NANOS_CORE_SOURCES})
target_link_libraries(test_radix ${llvm_libs} -pthread -ldl)

add_test(
  NAME test_radix
  COMMAND $<TARGET_FILE:test_radix>
  WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})

add_executable(
  test_hash
  tests/test_hash.cpp
  src/reader.cpp
  src/printer.cpp
  ${NANOS_CORE_SOURCES})
target_link_libraries(test_hash ${llvm_libs} -pthread -ldl)

add_test(
  NAME test_hash
  COMMAND $<TARGET_FILE:test_hash>
  WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})

