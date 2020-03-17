cmake_minimum_required(VERSION 3.14)
project(astl CXX)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

if(CMAKE_CXX_COMPILER_ID STREQUAL "MSVC")
        set(CMAKE_CXX_FLAGS "/std:c++latest /IC:/Users/User/CLionProjects/astl/include /W4 /EHsc \
                /Zc:__cplusplus /permissive-")
elseif(CMAKE_CXX_COMPILER_ID STREQUAL "GNU")
        set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++2a -Wall -Wextra -Wno-attributes -Wduplicated-cond \
                -Wduplicated-branches -Wlogical-op -Wnull-dereference -Wdouble-promotion \
                -Wno-unknown-pragmas -Wreorder -Wzero-as-null-pointer-constant -fdiagnostics-show-template-tree \
                -I${PROJECT_SOURCE_DIR}/include -Wsuggest-attribute=pure \
                -Wsuggest-attribute=const -Wsuggest-attribute=noreturn -Wsuggest-final-types -Wsuggest-final-methods \
                -Wsuggest-override")
elseif(CMAKE_CXX_COMPILER_ID STREQUAL "Clang")
        set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++2a -Wall -Wextra -Wno-attributes  \
                 -Wnull-dereference -Wdouble-promotion \
                -Wno-unknown-pragmas -Wreorder -Wzero-as-null-pointer-constant -fdiagnostics-show-template-tree \
                -I/${PROJECT_SOURCE_DIR}/include")
endif()


set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

set(ASTL_FILES include/astl/algorithm.hpp include/astl/concat_ranges.hpp include/astl/count_iterator.hpp
        include/astl/ebo_base.hpp include/astl/enumerate_range.hpp include/astl/filter_iterator.hpp
        include/astl/functional.hpp include/astl/iterator.hpp
        include/astl/iterator_adaptors.hpp include/astl/iterator_function_adaptors.hpp
        include/astl/iterator_range.hpp include/astl/map_iterator.hpp include/astl/memory.hpp
        include/astl/optional.hpp include/astl/pointee_iterator.hpp
        include/astl/pointer_iterator.hpp include/astl/range_access.hpp include/astl/ref_iter.hpp
        include/astl/temporary_buffer.hpp include/astl/zip.hpp include/astl/utility.hpp)

set(ASTL_INTERNAL_FILES include/astl/internal/accumulate.hpp include/astl/internal/adjacent_difference.hpp
        include/astl/internal/adjacent_find.hpp include/astl/internal/all_of.hpp include/astl/internal/any_of.hpp
        include/astl/internal/binary_search.hpp include/astl/internal/clamp.hpp include/astl/internal/compiler.hpp
        include/astl/internal/copy.hpp include/astl/internal/count.hpp include/astl/internal/destroy.hpp
        include/astl/internal/dynamic_programing.hpp include/astl/internal/equal.hpp include/astl/internal/equal_range.hpp
        include/astl/internal/erase_if.hpp include/astl/internal/fill.hpp include/astl/internal/find.hpp
        include/astl/internal/for_each.hpp include/astl/internal/gather.hpp include/astl/internal/heap.hpp
        include/astl/internal/inner_product.hpp include/astl/internal/inplace_merge.hpp include/astl/internal/iota.hpp
        include/astl/internal/is_contained.hpp include/astl/internal/is_palindrome.hpp include/astl/internal/lexicographical_compare.hpp
        include/astl/internal/lower_bound.hpp include/astl/internal/max.hpp include/astl/internal/merge.hpp
        include/astl/internal/min.hpp include/astl/internal/minmax_element.hpp include/astl/internal/mismatch.hpp
        include/astl/internal/move.hpp include/astl/internal/none_of.hpp include/astl/internal/partial_sum.hpp
        include/astl/internal/partition.hpp include/astl/internal/permutation.hpp include/astl/internal/starts_ends.hpp
        include/astl/internal/reduce.hpp include/astl/internal/remove.hpp include/astl/internal/replace.hpp
        include/astl/internal/reverse.hpp include/astl/internal/rotate.hpp include/astl/internal/scan.hpp
        include/astl/internal/search.hpp include/astl/internal/set_algos.hpp include/astl/internal/shift.hpp
        include/astl/internal/slide.hpp include/astl/internal/sort.hpp include/astl/internal/stable_partition.hpp
        include/astl/internal/swap.hpp include/astl/internal/transform.hpp include/astl/internal/trim.hpp
        include/astl/internal/unique.hpp include/astl/internal/upper_bound.hpp include/astl/internal/select.hpp
        include/function_ref.hpp include/astl/internal/permutation_combinations.hpp)



add_executable(astl main.cpp ${ASTL_INTERNAL_FILES} ${ASTL_FILES})
