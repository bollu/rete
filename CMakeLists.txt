cmake_minimum_required(VERSION 3.4.3)
project(rete C CXX)

set(CMAKE_CXX_FLAGS "-fsanitize=address,undefined -g -O0")
add_executable(rete0 rete0.cpp)
target_link_libraries(rete0 cgraph gvc)
set_target_properties(rete0 PROPERTIES CXX_STANDARD 14)


add_executable(rete1 rete1.cpp)
target_link_libraries(rete1 cgraph gvc)
set_target_properties(rete1 PROPERTIES CXX_STANDARD 14)
