cmake_minimum_required(VERSION 3.25)
project(nirvana)

set(CMAKE_CXX_STANDARD 23)

add_executable(nirvana src/main.cpp src/object.h src/object.cpp src/util.h src/eval.h src/eval.cpp src/util.cpp)
