cmake_minimum_required(VERSION 3.13.4)

project(Ypsilon)

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

set(YPSILON_SHARE_DIRECTORY ${CMAKE_INSTALL_PREFIX}/share/ypsilon)
set(YPSILON_EXTENSION_DIRECTORY ${CMAKE_INSTALL_PREFIX}/lib/ypsilon)
add_compile_definitions(SYSTEM_SHARE_PATH="${YPSILON_SHARE_DIRECTORY}")
add_compile_definitions(SYSTEM_EXTENSION_PATH="${YPSILON_EXTENSION_DIRECTORY}")

if(CMAKE_SIZEOF_VOID_P EQUAL 4)
  add_compile_definitions(DEFAULT_HEAP_LIMIT=512)
else()
  add_compile_definitions(DEFAULT_HEAP_LIMIT=2048)
endif()

add_executable(
  nanos
  src/bit.cpp
  src/object_heap.cpp
  src/object.cpp
  src/concurrent_heap.cpp
  src/concurrent_pool.cpp
  src/concurrent_slab.cpp
  src/nanos.cpp)

execute_process(
  COMMAND llvm-config --libs all
  OUTPUT_VARIABLE llvm_libs
  OUTPUT_STRIP_TRAILING_WHITESPACE)
target_link_libraries(nanos ${llvm_libs} -pthread -ldl)

add_executable(
  test_object
  tests/test_object.cpp
  src/bit.cpp
  src/object_heap.cpp
  src/object.cpp
  src/concurrent_heap.cpp
  src/concurrent_pool.cpp
  src/concurrent_slab.cpp)
target_link_libraries(test_object ${llvm_libs} -pthread -ldl)

add_test(
  NAME test_object
  COMMAND $<TARGET_FILE:test_object>
  WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})

if(APPLE)

  execute_process(
    COMMAND llvm-config --ldflags
    OUTPUT_VARIABLE llvm_ldflags
    OUTPUT_STRIP_TRAILING_WHITESPACE)
  target_link_libraries(nanos ${llvm_ldflags})
endif(APPLE)

file(MAKE_DIRECTORY $ENV{HOME}/.nanos)

set(YPSILON_SITELIB_DIRECTORY ${YPSILON_SHARE_DIRECTORY}/sitelib)
install(CODE "execute_process(COMMAND rm -rf ${YPSILON_SITELIB_DIRECTORY})")
install(DIRECTORY sitelib/ DESTINATION ${YPSILON_SITELIB_DIRECTORY})
install(TARGETS nanos DESTINATION bin)

enable_testing()
