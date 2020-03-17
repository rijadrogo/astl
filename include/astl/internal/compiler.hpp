//
// Created by Rijad on 31-Jul-19.
//

#ifndef ASTL_INCLUDE_COMPILER_HPP
#define ASTL_INCLUDE_COMPILER_HPP

#ifndef __has_cpp_attribute
#define __has_cpp_attribute(x) 0
#endif//#ifndef __has_cpp_attribute

#ifndef _HAS_CXX17
#define _HAS_CXX17 0
#endif// !_HAS_CXX17

#if _HAS_CXX17
#define HAS_DEDUCTION_GUIDES 1
#define ASTL_NODISCARD [[nodiscard]]
#endif//  #if _HAS_CXX17

#ifndef __cplusplus
#define __cplusplus 199711L
#endif// !__cplusplus

#if !_HAS_CXX17 && __cplusplus > 201402L
#define HAS_DEDUCTION_GUIDES 1
#define ASTL_NODISCARD [[nodiscard]]
#endif//#if __cplusplus > 201402L

#ifndef HAS_DEDUCTION_GUIDES
#define HAS_DEDUCTION_GUIDES 0
#endif// !HAS_DEDUCTION_GUIDES

/// ASTL_NODISCARD - Warn if a type or return value is discarded.
#ifndef ASTL_NODISCARD
#if __has_cpp_attribute(clang::warn_unused_result)
#define ASTL_NODISCARD [[clang::warn_unused_result]]
#elif __has_cpp_attribute(warn_unused_result)//#if __has_cpp_attribute(clang::warn_unused_result)
#define ASTL_NODISCARD __attribute__((warn_unused_result))
#elif defined(_MSVC) && (_MSC_VER >= 1700)
#define ASTL_NODISCARD _Check_return_
#else
#define ASTL_NODISCARD
#endif
#endif// !ASTL_NODISCARD

#endif// ASTL_INCLUDE_COMPILER_HPP
