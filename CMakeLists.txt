cmake_minimum_required(VERSION 3.13.4)
project(Digamma)

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
include_directories(${LLVM_INCLUDE_DIRS})
separate_arguments(LLVM_DEFINITIONS_LIST NATIVE_COMMAND ${LLVM_DEFINITIONS})
add_definitions(${LLVM_DEFINITIONS_LIST})

set(CMAKE_CXX_COMPILER "clang++")
add_link_options("-fuse-ld=lld")

set(DIGAMMA_SHARE_DIRECTORY ${CMAKE_INSTALL_PREFIX}/share/digamma)
set(DIGAMMA_EXTENSION_DIRECTORY ${CMAKE_INSTALL_PREFIX}/lib/digamma)
add_compile_definitions(SYSTEM_SHARE_PATH="${DIGAMMA_SHARE_DIRECTORY}")
add_compile_definitions(SYSTEM_EXTENSION_PATH="${DIGAMMA_EXTENSION_DIRECTORY}")

add_compile_definitions(DEFAULT_HEAP_LIMIT=128)

add_executable(
  digamma
  src/arith.cpp
  src/bit.cpp
  src/codegen.cpp
  src/equiv.cpp
  src/fasl.cpp
  src/file.cpp
  src/hash.cpp
  src/ioerror.cpp
  src/list.cpp
  src/main.cpp
  src/object_factory.cpp
  src/object_heap_compact.cpp
  src/object_heap.cpp
  src/object_set.cpp
  src/object_slab.cpp
  src/port.cpp
  src/printer.cpp
  src/reader.cpp
  src/serialize.cpp
  src/socket.cpp
  src/subr_base_arith.cpp
  src/subr_base.cpp
  src/subr_bitwise.cpp
  src/subr_bvector.cpp
  src/subr_c_ffi.cpp
  src/subr_codegen.cpp
  src/subr_file.cpp
  src/subr_fixnum.cpp
  src/subr_flonum.cpp
  src/subr_hash.cpp
  src/subr_linmath.cpp
  src/subr_list.cpp
  src/subr_others.cpp
  src/subr_port.cpp
  src/subr_process.cpp
  src/subr_r5rs_arith.cpp
  src/subr_socket.cpp
  src/subr_unicode.cpp
  src/ucs4.cpp
  src/utf8.cpp
  src/uuid.cpp
  src/violation.cpp
  src/vm0.cpp
  src/vm1.cpp
  src/vm2.cpp
  src/vm3.cpp)
execute_process(
  COMMAND llvm-config --libs all
  OUTPUT_VARIABLE llvm_libs
  OUTPUT_STRIP_TRAILING_WHITESPACE)
target_link_libraries(digamma ${llvm_libs} -pthread -ldl)
install(TARGETS digamma DESTINATION bin)

set(DIGAMMA_SITELIB_DIRECTORY ${DIGAMMA_SHARE_DIRECTORY}/sitelib)
install(DIRECTORY sitelib/ DESTINATION ${DIGAMMA_SITELIB_DIRECTORY})

enable_testing()

set(DIGAMMA_COMMON_TEST_OPTIONS --heap-limit=128 --acc=/tmp --clean-acc --sitelib=./test:./sitelib)
add_test(
  NAME r4rstest
  COMMAND $<TARGET_FILE:digamma> ${DIGAMMA_COMMON_TEST_OPTIONS} ./test/r4rstest.scm
  WORKING_DIRECTORY ..)
add_test(
  NAME tspl
  COMMAND $<TARGET_FILE:digamma> ${DIGAMMA_COMMON_TEST_OPTIONS} ./test/tspl.scm
  WORKING_DIRECTORY ..)
add_test(
  NAME arith
  COMMAND $<TARGET_FILE:digamma> ${DIGAMMA_COMMON_TEST_OPTIONS} ./test/arith.scm
  WORKING_DIRECTORY ..)
add_test(
  NAME r5rs-pitfall
  COMMAND $<TARGET_FILE:digamma> ${DIGAMMA_COMMON_TEST_OPTIONS} --r6rs ./test/r5rs-pitfall.scm
  WORKING_DIRECTORY ..)
add_test(
  NAME r6rs
  COMMAND $<TARGET_FILE:digamma> ${DIGAMMA_COMMON_TEST_OPTIONS} --r6rs ./test/r6rs.scm
  WORKING_DIRECTORY ..)
add_test(
  NAME r6rs-lib
  COMMAND $<TARGET_FILE:digamma> ${DIGAMMA_COMMON_TEST_OPTIONS} --r6rs ./test/r6rs-lib.scm
  WORKING_DIRECTORY ..)
add_test(
  NAME r6rs-more
  COMMAND $<TARGET_FILE:digamma> ${DIGAMMA_COMMON_TEST_OPTIONS} --r6rs ./test/r6rs-more.scm
  WORKING_DIRECTORY ..)
add_test(
  NAME r7rs-test
  COMMAND $<TARGET_FILE:digamma> ${DIGAMMA_COMMON_TEST_OPTIONS} --r7rs ./test/r7rs-test.scm
  WORKING_DIRECTORY ..)
add_test(
  NAME c-ffi-test
  COMMAND $<TARGET_FILE:digamma> ${DIGAMMA_COMMON_TEST_OPTIONS} ./test/c-ffi-test.scm
  WORKING_DIRECTORY ..)
add_test(
  NAME syntax-rule-stress-test
  COMMAND $<TARGET_FILE:digamma> ${DIGAMMA_COMMON_TEST_OPTIONS} ./test/syntax-rule-stress-test.scm
  WORKING_DIRECTORY ..)
