cmake_minimum_required(VERSION 3.1...3.11)

if(${CMAKE_VERSION} VERSION_LESS 3.10)
  cmake_policy(VERSION ${CMAKE_MAJOR_VERSION}.${CMAKE_MINOR_VERSION})
else()
  cmake_policy(VERSION 3.11)
endif()

project(FortranRBTree VERSION 0.1
  DESCRIPTION "Red-Black Trees in Fortran"
  LANGUAGES Fortran)

set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -g")

add_library(rbtree SHARED
  src/binary-tree.f90
  src/redblack-tree.f90)
target_compile_options(rbtree PRIVATE -Wall -Wextra)

add_executable(simple_test_binary_tree
  tests/simple_test_binary_tree.f90)
target_link_libraries(simple_test_binary_tree PUBLIC rbtree)
target_include_directories(simple_test_binary_tree
  PUBLIC $<TARGET_PROPERTY:rbtree,Fortran_MODULE_DIRECTORY>)

add_executable(simple_test_redblack_tree
  tests/simple_test_redblack_tree.f90)
target_link_libraries(simple_test_redblack_tree PUBLIC rbtree)
target_include_directories(simple_test_redblack_tree
  PUBLIC $<TARGET_PROPERTY:rbtree,Fortran_MODULE_DIRECTORY>)

find_package(pFUnit)
if(PFUNIT_FOUND)
  include(CTest)
  enable_testing()
  set(TEST_SOURCES
    tests/test_binary_tree.pf
    tests/test_redblack_tree.pf
    )
  add_pfunit_test(rb_tests "${TEST_SOURCES}" "" "")
  target_link_libraries(rb_tests rbtree)
endif()
