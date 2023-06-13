cmake_minimum_required(VERSION 3.12)
project(scheme_generator)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

find_package(Protobuf REQUIRED)
find_package(absl REQUIRED)
find_package(utf8_range REQUIRED)
# From protobuf/cmake/abseil-cpp.cmake
set(protobuf_ABSL_USED_TARGETS
    absl::absl_check
    absl::absl_log
    absl::algorithm
    absl::base
    absl::bind_front
    absl::bits
    absl::btree
    absl::cleanup
    absl::cord
    absl::core_headers
    absl::debugging
    absl::die_if_null
    absl::dynamic_annotations
    absl::flags
    absl::flat_hash_map
    absl::flat_hash_set
    absl::function_ref
    absl::hash
    absl::layout
    absl::log_initialize
    absl::log_severity
    absl::memory
    absl::node_hash_map
    absl::node_hash_set
    absl::optional
    absl::span
    absl::status
    absl::statusor
    absl::strings
    absl::synchronization
    absl::time
    absl::type_traits
    absl::utility
    absl::variant
  )
include_directories(${Protobuf_INCLUDE_DIRS})
add_executable(protoc-gen-scheme scheme_generator.cc)
target_link_libraries(protoc-gen-scheme ${Protobuf_LIBRARIES} ${Protobuf_PROTOC_LIBRARIES}
  ${protobuf_ABSL_USED_TARGETS} utf8_range::utf8_validity utf8_range::utf8_range)
