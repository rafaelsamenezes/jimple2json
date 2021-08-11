cmake_minimum_required(VERSION 3.8)
project(JimpleParser)

set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CMAKE_SOURCE_DIR}/cmake/")
include(ExternalDependencies)
include_directories(include)

add_subdirectory(lib)
add_subdirectory(test)