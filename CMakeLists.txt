cmake_minimum_required(VERSION 3.12)
project(scheme_generator)

if (CYGWIN)
  set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=gnu++11")
else()
  set(CMAKE_CXX_STANDARD 11)
  set(CMAKE_CXX_STANDARD_REQUIRED ON)
  set(CMAKE_CXX_EXTENSIONS OFF)
endif()

find_package(Protobuf REQUIRED)

include_directories(${Protobuf_INCLUDE_DIRS})
add_executable(protoc-gen-scheme scheme_generator.cc)
target_link_libraries(protoc-gen-scheme ${Protobuf_LIBRARIES} ${Protobuf_PROTOC_LIBRARIES})
