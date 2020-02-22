cmake_minimum_required(VERSION 3.4.3)
project(rete C CXX)

set(CMAKE_CXX_FLAGS "-fsanitize=address,undefined -g -O0")
add_executable(rete rete.cpp)
target_link_libraries(rete cgraph gvc)
set_target_properties(rete PROPERTIES CXX_STANDARD 14)
