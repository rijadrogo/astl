//
// Created by Rijad on 30-Jul-18.
//

#ifndef ASTL_INCLUDE_LEXICOGRAPHICAL_COMPARE_HPP
#define ASTL_INCLUDE_LEXICOGRAPHICAL_COMPARE_HPP

#include <algorithm>
#include <cstring>
#include <functional>
#include <type_traits>

#include "astl/functional.hpp"
#include "astl/iterator.hpp"
#include "astl/range_access.hpp"

// ReSharper disable CppInconsistentNaming
namespace astl
{
namespace internal_lexcomp
{

template <typename> inline constexpr bool is_character = false; // by default, not a character type

template <> inline constexpr bool is_character<char> = true; // chars are characters

template <>
inline constexpr bool is_character<signed char> = true; // signed chars are also characters

template <>
inline constexpr bool is_character<unsigned char> = true; // unsigned chars are also characters

#ifdef __cpp_char8_t
template <>
inline constexpr bool is_character<char8_t> =
    true; // UTF-8 code units are sort-of characters __cpp_char8_t
#endif    // __cpp_char8_t

// checks the lex_compare element types for memcmp safety for builtin functors (e.g., less<unsigned char>)
template <typename Ty1, typename Ty2, typename FTy>
inline constexpr bool lex_compare_check_element_types_helper =
    is_character<Ty1> &&is_character<Ty2> &&is_character<FTy> &&std::is_unsigned_v<FTy>;

template <typename Ty1, typename Ty2>
inline constexpr bool lex_compare_check_element_types_helper<Ty1, Ty2, void> =
    is_character<Ty1> &&is_character<Ty2> &&std::is_unsigned_v<Ty1> &&std::is_unsigned_v<Ty2>;

#ifdef __cpp_lib_byte

// std::byte with builtin functors (e.g. less<byte>) is memcmp safe
template <>
inline constexpr bool lex_compare_check_element_types_helper<std::byte, std::byte, std::byte> =
    true;

// std::byte with transparent functors (e.g. less<>) is memcmp safe
template <>
inline constexpr bool lex_compare_check_element_types_helper<std::byte, std::byte, void> = true;

#endif // __cpp_lib_byte

template <typename> struct lex_compare_optimize {
    explicit lex_compare_optimize() = default;
}; // optimization tag for lexicographical_compare

// checks the lex_compare element types for memcmp safety
template <typename MemcmpPred, typename Ty1, typename Ty2, typename FTy>
using lex_compare_check_element_types = lex_compare_optimize<if_t<
    lex_compare_check_element_types_helper<std::remove_const_t<Ty1>, std::remove_const_t<Ty2>, FTy>,
    MemcmpPred, void>>;

template <typename InIt1, typename InIt2, typename Comparator>
auto lex_compare_memcmp_classify(const InIt1 &, const InIt2 &, const Comparator &)
{
    // return lex_compare optimization category for arbitrary iterators
    return lex_compare_optimize<void>{};
}

template <typename Ty1, typename Ty2, typename FTy>
auto lex_compare_memcmp_classify(Ty1 *const &, Ty2 *const &, const std::less<FTy> &)
{
    // return lex_compare optimization category for pointer iterators and less<_FTy>
    return lex_compare_check_element_types<std::less<int>, Ty1, Ty2, FTy>{};
}

template <typename Ty1, typename Ty2, typename FTy>
auto lex_compare_memcmp_classify(Ty1 *const &, Ty2 *const &, const std::greater<FTy> &)
{
    // return lex_compare optimization category for pointer iterators and greater<_FTy>
    return lex_compare_check_element_types<std::greater<int>, Ty1, Ty2, FTy>{};
}

template <typename InIt1, typename InIt2, typename Comparator>
auto lex_compare_unchecked(InIt1 first1, InIt1 last1, InIt2 first2, InIt2 last2, Comparator comp,
                           lex_compare_optimize<void>) -> bool
{
    while (first1 != last1 && first2 != last2) {

        if (comp(*first1, *first2)) return true;

        if (comp(*first2, *first1)) return false;

        ++first1;
        ++first2;
    }
    return first1 == last1 && first2 != last2;
}

template <typename InIt1, typename InIt2, typename Comparator, typename MemcmpPred>
auto lex_compare_unchecked(InIt1 first1, InIt1 last1, InIt2 first2, InIt2 last2, Comparator,
                           lex_compare_optimize<MemcmpPred>) -> bool
{
    const auto num1(static_cast<size_t>(last1 - first1));
    const auto num2(static_cast<size_t>(last2 - first2));
    const int ans(std::memcmp(first1, first2, num1 < num2 ? num1 : num2));
    return MemcmpPred{}(ans, 0) || (ans == 0 && num1 < num2);
}

template <typename InIt1, typename InIt2, typename Comparator>
auto lex_3way_compare_unchecked(InIt1 first1, InIt1 last1, InIt2 first2, InIt2 last2,
                                Comparator comp, lex_compare_optimize<void>) -> int
{
    while (first1 != last1 && first2 != last2) {
        if (comp(*first1, *first2)) return -1;

        if (comp(*first2, *first1)) return 1;

        ++first1;
        ++first2;
    }
    return first2 == last2 ? !(first1 == last1) : -1;
}

template <typename InIt1, typename InIt2, typename Comparator, typename MemcmpPred>
auto lex_3way_compare_unchecked(InIt1 first1, InIt1 last1, InIt2 first2, InIt2 last2, Comparator,
                                lex_compare_optimize<MemcmpPred>) -> int
{
    const auto num1(static_cast<size_t>(last1 - first1));
    const auto num2(static_cast<size_t>(last2 - first2));
    const int ans(std::memcmp(first1, first2, num1 < num2 ? num1 : num2));
    const int real_ans(std::is_same_v<std::less<>, MemcmpPred> ? ans : -ans);
    return num1 == num2 ? real_ans : (num2 < num1 ? 1 : -1);
}

} // namespace internal_lexcomp

namespace i
{

// Libcpp doesn't optimise to memcmp if possible for std::lexicographical_compare
#if defined(_LIBCPP_VERSION)
template <typename InIt1, typename InIt2, typename Comparator>
// requires InIt1 InputIterator
// requires InIt2 InputIterator
// requires Comparator StrictWeakOrdering on value_type(FwdIt)
ASTL_NODISCARD auto lexicographical_compare(InIt1 first1, InIt1 last1, InIt2 first2, InIt2 last2,
                                            Comparator comp) -> bool
{
    return internal_lexcomp::lex_compare_unchecked(
        first1, last1, first2, last2, astl::pass_fn(comp),
        internal_lexcomp::lex_compare_memcmp_classify(first1, first2, comp));
}

template <typename InIt1, typename InIt2>
// requires InIt1 InputIterator
// requires InIt2 InputIterator
ASTL_NODISCARD auto lexicographical_compare(InIt1 first1, InIt1 last1, InIt2 first2, InIt2 last2)
    -> bool
{
    return i::lexicographical_compare(first1, last1, first2, last2, std::equal_to{});
}
#else  // !defined(_LIBCPP_VERSION)
using std::lexicographical_compare; // NOLINT(misc-unused-using-decls)
#endif // defined(_LIBCPP_VERSION)
template <typename InIt1, typename InIt2, typename Comparator, typename P1, typename P2>
ASTL_NODISCARD auto lexicographical_compare(InIt1 first1, InIt1 last1, InIt2 first2, InIt2 last2,
                                            Comparator comp, P1 p1, P2 p2) -> bool
{
    return std::lexicographical_compare(
        first1, last1, first2, last2,
        astl::lockstep(astl::pass_fn(comp), astl::pass_fn(p1), astl::pass_fn(p2)));
}

template <typename InIt1, typename InIt2, typename Comparator>
// requires InIt1 InputIterator
// requires InIt2 InputIterator
// requires Comparator function, returns bool, two arguments value_type(InIt1), value_type(InIt2)
ASTL_NODISCARD auto lexicographical_compare_3way(InIt1 first1, InIt1 last1, InIt2 first2,
                                                 InIt2 last2, Comparator comp) -> int
{
    return internal_lexcomp::lex_3way_compare_unchecked(
        first1, last1, first2, last2, astl::pass_fn(comp),
        internal_lexcomp::lex_compare_memcmp_classify(first1, first2, comp));
}

template <typename InIt1, typename InIt2>
// requires InIt1 InputIterator
// requires InIt2 InputIterator
ASTL_NODISCARD auto lexicographical_compare_3way(InIt1 first1, InIt1 last1, InIt2 first2,
                                                 InIt2 last2) -> int
{
    return i::lexicographical_compare_3way(first1, last1, first2, last2, std::less{});
}

template <typename InIt1, typename InIt2, typename Comparator, typename P1, typename P2>
ASTL_NODISCARD auto lexicographical_compare_3way(InIt1 first1, InIt1 last1, InIt2 first2,
                                                 InIt2 last2, Comparator comp, P1 p1, P2 p2) -> int
{
    return i::lexicographical_compare_3way(
        first1, last1, first2, last2,
        astl::lockstep(astl::pass_fn(comp), astl::pass_fn(p1), astl::pass_fn(p2)));
}

template <typename InIt1, typename N1, typename InIt2, typename N2, typename Comparator>
// requires InIt1 InputIterator
// requires N1 integral type
// requires InIt2 InputIterator
// requires N2 integral type
// requires Comparator function, returns bool, arguments value_type(InIt1), value_type(InIt2)
ASTL_NODISCARD auto lexicographical_compare_3way_n(InIt1 first1, N1 n1, InIt2 first2, N2 n2,
                                                   Comparator comp) -> int
{
    if constexpr (is_random_access_it_v<InIt1, InIt2>) {
        return i::lexicographical_compare_3way(first1, first1 + n1, first2, first2 + n2,
                                               astl::pass_fn(comp));
    }
    else {
        // order [first1, n1) vs. [first2, n2) using comp, no special optimization
        while (n1 != N1(0) && n2 != N2(0)) {
            // something to compare, do it
            if (comp(*first1, *first2)) return -1;

            if (comp(*first2, *first1)) return 1;

            --n1;
            --n2;
            ++first1;
            ++first2;
        }
        return n2 == N2(0) ? !(n1 == N1(0)) : -1;
    }
}

template <typename InIt1, typename N1, typename InIt2, typename N2>
// requires InIt1 InputIterator
// requires N1 integral type
// requires InIt2 InputIterator
// requires N2 integral type
ASTL_NODISCARD auto lexicographical_compare_3way_n(InIt1 first1, N1 n1, InIt2 first2, N2 n2) -> int
{
    return i::lexicographical_compare_3way_n(first1, n1, first2, n2, std::less{});
}

template <typename InIt1, typename N, typename InIt2, typename Comparator, typename P1, typename P2>
ASTL_NODISCARD auto lexicographical_compare_3way_n(InIt1 first1, N n1, InIt2 first2, N n2,
                                                   Comparator comp, P1 p1, P2 p2) -> int
{
    return i::lexicographical_compare_3way_n(
        first1, n1, first2, n2,
        astl::lockstep(astl::pass_fn(comp), astl::pass_fn(p1), astl::pass_fn(p2)));
}

template <typename InIt1, typename N1, typename InIt2, typename N2, typename Comparator>
ASTL_NODISCARD auto lexicographical_compare_n(InIt1 first1, N1 n1, InIt2 first2, N2 n2,
                                              Comparator comp) -> bool
{
    // order [first1, n1) vs. [first2, n2) using comp
    auto res(i::lexicographical_compare_3way_n(first1, n1, first2, n2, astl::pass_fn(comp)));
    return res != 0 ? res < 0 : n1 < n2;
}

template <typename InIt1, typename N1, typename InIt2, typename N2>
ASTL_NODISCARD auto lexicographical_compare_n(InIt1 first1, N1 n1, InIt2 first2, N2 n2) -> bool
{
    return i::lexicographical_compare_n(first1, n1, first2, n2, std::less{});
}

template <typename InIt1, typename N1, typename InIt2, typename N2, typename Comparator,
          typename P1, typename P2>
ASTL_NODISCARD auto lexicographical_compare_n(InIt1 first1, N1 n1, InIt2 first2, N2 n2,
                                              Comparator comp, P1 p1, P2 p2) -> bool
{
    // order [first1, n1) vs. [first2, n2) using comp
    return i::lexicographical_compare_n(
        first1, n1, first2, n2,
        astl::lockstep(astl::pass_fn(comp), astl::pass_fn(p1), astl::pass_fn(p2)));
}
} // namespace i

namespace r
{
template <typename R1, typename R2>
// requires R1 InputIterator range
// requires R2 InputIterator range
ASTL_NODISCARD auto lexicographical_compare(R1 &&r1, R2 &&r2) -> bool
{
    return i::lexicographical_compare(adl::begin(r1), adl::end(r1), adl::begin(r2), adl::end(r2));
}

template <typename R1, typename R2, typename Comparator>
// requires R1 InputIterator range
// requires R2 InputIterator range
// requires Comparator function, returns bool, takes two arguments of type
// value_type(R1)
//                                                               and
//                                                               value_type(R2)
ASTL_NODISCARD auto lexicographical_compare(R1 &&r1, R2 &&r2, Comparator comp) -> bool
{
    return i::lexicographical_compare(adl::begin(r1), adl::end(r1), adl::begin(r2), adl::end(r2),
                                      astl::pass_fn(comp));
}

template <typename R1, typename R2, typename Comparator, typename P1, typename P2>
ASTL_NODISCARD auto lexicographical_compare(R1 &&r1, R2 &&r2, Comparator comp, P1 p1, P2 p2) -> bool
{
    return i::lexicographical_compare(adl::begin(r1), adl::end(r1), adl::begin(r2), adl::end(r2),
                                      astl::pass_fn(comp), astl::pass_fn(p1), astl::pass_fn(p2));
}

template <typename R1, typename R2>
// requires R1 InputIterator range
// requires R2 InputIterator range
ASTL_NODISCARD auto lexicographical_compare_3way(R1 &&r1, R2 &&r2) -> int
{
    return i::lexicographical_compare_3way(adl::begin(r1), adl::end(r1), adl::begin(r2),
                                           adl::end(r2));
}

template <typename R1, typename R2, typename Comparator>
// requires R1 InputIterator range
// requires R2 InputIterator range
// requires Comparator function, returns bool, takes two arguments of type
// value_type(R1)
//                                                               and
//                                                               value_type(R2)
ASTL_NODISCARD auto lexicographical_compare_3way(R1 &&r1, R2 &&r2, Comparator comp) -> int
{
    return i::lexicographical_compare_3way(adl::begin(r1), adl::end(r1), adl::begin(r2),
                                           adl::end(r2), astl::pass_fn(comp));
}

template <typename R1, typename R2, typename Comparator, typename P1, typename P2>
ASTL_NODISCARD auto lexicographical_compare_3way(R1 &&r1, R2 &&r2, Comparator comp, P1 p1, P2 p2)
    -> int
{
    return i::lexicographical_compare_3way(adl::begin(r1), adl::end(r1), adl::begin(r2),
                                           adl::end(r2), astl::pass_fn(comp), astl::pass_fn(p1),
                                           astl::pass_fn(p2));
}

template <typename R1, typename N1, typename R2, typename N2>
// requires R1 InputIterator range
// requires N1 integral type
// requires R2 InputIterator range
// requires N2 integral type
// requires Comparator function, returns bool, takes two arguments of type
// value_type(R1)
//                                                               and
//                                                               value_type(R2)
ASTL_NODISCARD auto lexicographical_compare_3way_n(R1 &&r1, N1 n1, R2 &&r2, N2 n2) -> int
{
    return i::lexicographical_compare_3way(adl::begin(r1), n1, adl::begin(r2), n2);
}

template <typename R1, typename N1, typename R2, typename N2, typename Comparator>
// requires R1 InputIterator range
// requires N1 integral type
// requires R2 InputIterator range
// requires N2 integral type
// requires Comparator function, returns bool, takes two arguments of type
// value_type(R1)
//                                                               and
//                                                               value_type(R2)
ASTL_NODISCARD auto lexicographical_compare_3way_n(R1 &&r1, N1 n1, R2 &&r2, N2 n2, Comparator comp)
    -> int
{
    return i::lexicographical_compare_3way(adl::begin(r1), n1, adl::begin(r2), n2,
                                           astl::pass_fn(comp));
}

template <typename R1, typename N1, typename R2, typename N2, typename Comparator, typename P1,
          typename P2>
ASTL_NODISCARD auto lexicographical_compare_3way_n(R1 &&r1, N1 n1, R2 &&r2, N2 n2, Comparator comp,
                                                   P1 p1, P2 p2) -> int
{
    return i::lexicographical_compare_3way_n(adl::begin(r1), n1, adl::begin(r2), n2,
                                             astl::pass_fn(comp), astl::pass_fn(p1),
                                             astl::pass_fn(p2));
}

template <typename R1, typename N1, typename R2, typename N2>
// requires R1 InputIterator range
// requires N1 integral type
// requires R2 InputIterator range
// requires N2 integral type
ASTL_NODISCARD auto lexicographical_compare_n(R1 &&r1, N1 n1, R2 &&r2, N2 n2) -> bool
{
    return i::lexicographical_compare_n(adl::begin(r1), n1, adl::begin(r2), n2);
}

template <typename R1, typename N1, typename R2, typename N2, typename Comparator>
// requires R1 InputIterator range
// requires N1 integral type
// requires R2 InputIterator range
// requires N2 integral type
// requires Comparator function, returns bool, takes two arguments of type
// value_type(R1)
//                                                               and
//                                                               value_type(R2)
ASTL_NODISCARD auto lexicographical_compare_n(R1 &&r1, N1 n1, R2 &&r2, N2 n2, Comparator comp)
    -> bool
{
    return i::lexicographical_compare_n(adl::begin(r1), n1, adl::begin(r2), n2,
                                        astl::pass_fn(comp));
}

template <typename R1, typename N1, typename R2, typename N2, typename Comparator, typename P1,
          typename P2>
ASTL_NODISCARD auto lexicographical_compare_n(R1 &&r1, N1 n1, R2 &&r2, N2 n2, Comparator comp,
                                              P1 p1, P2 p2) -> bool
{
    return i::lexicographical_compare_n(adl::begin(r1), n1, adl::begin(r2), n2, astl::pass_fn(comp),
                                        astl::pass_fn(p1), astl::pass_fn(p2));
}
} // namespace r
} // namespace astl
// ReSharper restore CppInconsistentNaming
#endif // ASTL_INCLUDE_LEXICOGRAPHICAL_COMPARE_HPP
