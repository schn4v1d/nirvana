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
        src/call_function.h
        src/call_function.cpp
        src/errors.h
        src/errors.cpp
        src/OrdinaryLambdaList.cpp
        src/OrdinaryLambdaList.h
        src/Lambda.cpp
        src/Lambda.h
        src/LispString.cpp
        src/LispString.h
        src/MacroFunction.cpp
        src/MacroFunction.h
        src/LexicalBlock.cpp
        src/LexicalBlock.h
        src/Frame.cpp
        src/Frame.h
        src/special_operators.h
        src/special_operators.cpp
        src/special_operators/backquote.cpp
        src/special_operators/block.cpp
        src/special_operators/catch.cpp
        src/special_operators/function.cpp
        src/special_operators/if.cpp
        src/special_operators/let.cpp
        src/special_operators/quote.cpp
        src/special_operators/return_from.cpp
        src/special_operators/setq.cpp
        src/special_operators/throw.cpp
        src/special_operators/unwind_protect.cpp
        src/special_operators/tagbody.cpp
        src/special_operators/go.cpp
        src/GoTag.cpp
        src/GoTag.h
        src/special_operators/progn.cpp
        src/ClosObject.cpp
        src/ClosObject.h
        src/Vector.cpp
        src/Vector.h
        src/Values.cpp
        src/Values.h
        src/special_operators/multiple_value_call.cpp
)

target_include_directories(nirvana PRIVATE
    ${CMAKE_CURRENT_SOURCE_DIR}/src
)
