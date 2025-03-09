cmake_minimum_required(VERSION 3.10)
project(hello)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)

# 显式添加编译选项（如果需要确保生成的命令中包含 -std=c++17）
add_compile_options(-std=c++17)

add_executable(hello main.cpp)
