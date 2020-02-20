cmake_minimum_required(VERSION 3.4.3)
project(rete C CXX)

set(CMAKE_CXX_FLAGS "-fsanitize=address,undefined")
add_executable(rete
    rete.cpp)
set_target_properties(rete PROPERTIES CXX_STANDARD 14)
