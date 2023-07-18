cmake_minimum_required(VERSION 3.25)
project(nirvana)

set(CMAKE_CXX_STANDARD 23)
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})

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
        src/reader.h
        src/reader.cpp
        src/cl_fun.h
        src/cl_fun.cpp
        src/eval.h
        src/eval.cpp
        src/Environment.cpp
        src/Environment.h
        src/Binding.cpp
        src/Binding.h
        src/DynamicBindings.cpp
        src/DynamicBindings.h
        src/BuiltinFunction.cpp
        src/BuiltinFunction.h
        src/Function.h
        src/Function.cpp
        src/errors.h
        src/errors.cpp
        src/OrdinaryLambdaList.cpp
        src/OrdinaryLambdaList.h
        src/Lambda.cpp
        src/Lambda.h
        src/String.cpp
        src/String.h
        src/MacroFunction.cpp
        src/MacroFunction.h
        src/Block.cpp
        src/Block.h
)
