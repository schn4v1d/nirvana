cmake_minimum_required(VERSION 3.25)
project(nirvana)

set(CMAKE_CXX_STANDARD 23)

add_executable(nirvana src/main.cpp src/util.h src/util.cpp
        src/Value.h
        src/Object.h
        src/Object.cpp
        src/GarbageCollector.cpp
        src/GarbageCollector.h
        src/Cons.cpp
        src/Cons.h
        src/Value.cpp
        src/Symbol.cpp
        src/Symbol.h
        src/Package.cpp
        src/Package.h
)
