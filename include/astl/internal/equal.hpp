//
// Created by Rijad on 29-Jul-18.
//

#ifndef ASTL_INCLUDE_EQUAL_HPP
#define ASTL_INCLUDE_EQUAL_HPP

#include <algorithm>
#include <cstring>
#include <type_traits>

#include "astl/functional.hpp"
#include "astl/range_access.hpp"

namespace astl
{
namespace equal_internal
{

template <typename Ty>
inline constexpr bool is_nonbool_integral =
    std::is_integral_v<Ty> && !std::is_same_v<std::remove_cv_t<Ty>, bool>;

template <typename Ty1, typename Ty2>
inline constexpr bool value_equality_is_bitwise_equality = (static_cast<Ty1>(-1)
                                                            == static_cast<Ty2>(-1));

// determines whether it is safe to call memcmp to compare things; defaults to false
template <typename Ty1, typename Ty2, typename Pr>
inline constexpr bool equal_memcmp_is_safe_helper = false;

// order matters here: being integral is a precondition of _Value_equality_is_bitwise_equality
// allow memcmping same-size integral non-bool non-volatile bitwise types using equal_to<>
template <typename Ty1, typename Ty2>
inline constexpr bool equal_memcmp_is_safe_helper<Ty1, Ty2, std::equal_to<>> =
    (sizeof(Ty1) == sizeof(Ty2) && is_nonbool_integral<Ty1> && is_nonbool_integral<Ty2>) &&!std::
        is_volatile_v<
            Ty1> && !std::is_volatile_v<Ty2> && value_equality_is_bitwise_equality<Ty1, Ty2>;

// allow memcmping pointers-to-cv-T with equal_to<>
template <typename Ty1, typename Ty2>
inline constexpr bool equal_memcmp_is_safe_helper<Ty1 *, Ty2 *, std::equal_to<>> =
    std::is_same_v<std::remove_cv_t<Ty1>, std::remove_cv_t<Ty2>>;

// treat equal_to with exact T as equal_to<> this is safe because we only activate the optimization for builtin
// _Elem (and std::byte)
template <typename Elem>
inline constexpr bool equal_memcmp_is_safe_helper<Elem, Elem, std::equal_to<Elem>> =
    equal_memcmp_is_safe_helper<Elem, Elem, std::equal_to<>>;

// return equal optimization category for arbitrary iterator
template <typename Iter1, typename Iter2, typename Pr>
inline constexpr bool equal_memcmp_is_safe = false;

template <typename Obj1, typename Obj2, typename Pr>
inline constexpr bool equal_memcmp_is_safe<Obj1 *, Obj2 *, Pr> =
    equal_memcmp_is_safe_helper<std::remove_const_t<Obj1>, std::remove_const_t<Obj2>, Pr>;

// Libcpp doesnt optimise to memcmp if possible for std::equal
#if defined(_LIBCPP_VERSION)

template <typename InIt1, typename InIt2, typename BinaryPredicate>
// requires InIt1 InputIterator
// requires InIt2 InputIterator
// requires BinaryPredicate, returns bool, arguments value_type(InIt1), value_type(InIt2)
ASTL_NODISCARD auto equal(InIt1 first1, InIt1 last1, InIt2 first2, BinaryPredicate pred) -> bool
{
    if (first1 == last1) return true;

    if constexpr (equal_internal::equal_memcmp_is_safe<InIt1, InIt2, BinaryPredicate>) {
        const auto first1_ch(reinterpret_cast<const char *>(first1));
        const auto first2_ch(reinterpret_cast<const char *>(first2));
        const auto count(static_cast<size_t>(reinterpret_cast<const char *>(last1) - first1_ch));
        return std::memcmp(first1_ch, first2_ch, count) == 0;
    }
    else {
        while (first1 != last1) {
            if (!pred(*first1, *first2)) return false;

            ++first1;
            ++first2;
        }
        return true;
    }
}

template <typename InIt1, typename InIt2>
// requires InIt1 InputIterator
// requires InIt2 InputIterator
ASTL_NODISCARD auto equal(InIt1 first1, InIt1 last1, InIt2 first2) -> bool
{
    return equal_internal::equal(first1, last1, first2, std::equal_to<>{});
}

template <typename InIt1, typename InIt2, typename BinaryPredicate>
// requires InIt1 InputIterator
// requires InIt2 InputIterator
// requires BinaryPredicate, returns bool, arguments value_type(InIt1), value_type(InIt2)
ASTL_NODISCARD auto equal(InIt1 first1, InIt1 last1, InIt2 first2, InIt2 last2,
                          BinaryPredicate pred) -> bool
{
    if (first1 == last1) return true;

    if constexpr (is_random_access_it_v<InIt1, InIt2>) {
        if (last1 - first1 != last2 - first2) return false;

        return i::equal(first1, last1, first2, astl::pass_fn(pred));
    }
    else {
        while (true) {
            if (first1 == last1) return first2 == last2;

            if (first2 == last2) return false;

            if (!pred(*first1, *first2)) return false;

            ++first1;
            ++first2;
        }
    }
}

template <typename InIt1, typename InIt2>
// requires InIt1 InputIterator
// requires InIt2 InputIterator
ASTL_NODISCARD auto equal(InIt1 first1, InIt1 last1, InIt2 first2, InIt2 last2) -> bool
{
    return equal_internal::equal(first1, last1, first2, last2, std::equal_to<>{});
}

#else  // !defined(_LIBCPP_VERSION)
using std::equal;
#endif //defined(_LIBCPP_VERSION)

} // namespace equal_internal

namespace i
{

inline constexpr struct {
    template <typename InIt1, typename InIt2, typename BinaryPredicate = std::equal_to<>>
    // requires InIt1 InputIterator
    // requires InIt2 InputIterator
    // requires BinaryPredicate, returns bool, arguments value_type(InIt1), value_type(InIt2)
    ASTL_NODISCARD auto operator()(InIt1 first1, InIt1 last1, InIt2 first2,
                                   BinaryPredicate pred = BinaryPredicate{}) const -> bool
    {
        return equal_internal::equal(first1, last1, first2, astl::pass_fn(pred));
    }

    template <typename InIt1, typename InIt2, typename BinaryPredicate = std::equal_to<>>
    // requires InIt1 InputIterator
    // requires InIt2 InputIterator
    // requires BinaryPredicate, returns bool, arguments value_type(InIt1), value_type(InIt2)
    ASTL_NODISCARD auto operator()(InIt1 first1, InIt1 last1, InIt2 first2, InIt2 last2,
                                   BinaryPredicate pred = BinaryPredicate{}) const -> bool
    {
        return equal_internal::equal(first1, last1, first2, last2, astl::pass_fn(pred));
    }

    template <typename FwdIt1, typename FwdIt2, typename BinaryPredicate, typename P1,
              typename P2 = P1>
    ASTL_NODISCARD auto operator()(FwdIt1 first1, FwdIt1 last1, FwdIt2 first2, FwdIt2 last2,
                                   BinaryPredicate pred, P1 p1, P2 p2 = P1{}) const -> bool
    {
        return (*this)(first1, last1, first2, last2,
                       astl::lockstep(astl::pass_fn(pred), astl::pass_fn(p1), astl::pass_fn(p2)));
    }

} equal{};

inline constexpr struct {

    template <typename InIt1, typename N, typename InIt2,
              typename BinaryPredicate = std::equal_to<>>
    // requires InIt1 InputIterator
    // requires N integral type
    // requires InIt2 InputIterator
    // requires BinaryPredicate, returns bool arguments value_type(FwdIt1) and value_type(FwdIt2)
    ASTL_NODISCARD auto operator()(InIt1 first1, N n, InIt2 first2,
                                   BinaryPredicate pred = BinaryPredicate{}) const -> bool
    {
        if (n == N(0)) return true;

        if constexpr (equal_internal::equal_memcmp_is_safe<InIt1, InIt2, BinaryPredicate>) {
            const auto first1_ch(reinterpret_cast<const char *>(first1));
            const auto first2_ch(reinterpret_cast<const char *>(first2));
            const auto count(static_cast<size_t>(n));
            return std::memcmp(first1_ch, first2_ch, count) == 0;
        }
        else {
            while (n != N(0)) {
                if (!pred(*first1, *first2)) return false;

                ++first1;
                ++first2;
                --n;
            }
            return true;
        }
    }

    template <typename FwdIt1, typename N, typename FwdIt2, typename Eq, typename P1, typename P2>
    ASTL_NODISCARD auto operator()(FwdIt1 first1, N n, FwdIt2 first2, Eq eq, P1 p1, P2 p2) const
        -> bool
    {
        return (*this)(first1, n, first2,
                       astl::lockstep(astl::pass_fn(eq), astl::pass_fn(p1), astl::pass_fn(p2)));
    }

    template <typename FwdIt1, typename N, typename FwdIt2, typename Eq = std::equal_to<>>
    // requires FwdIt1 ForwardIterator
    // requires N integral type
    // requires FwdIt2 ForwardIterator
    // requires Eq, returns bool(are two arguments equivalent),
    //              arguments value_type(FwdIt1) and value_type(FwdIt2)
    ASTL_NODISCARD auto operator()(FwdIt1 first1, N n1, FwdIt2 first2, N n2, Eq eq = Eq{}) const
        -> bool
    {
        return n1 == n2 && (*this)(first1, n1, first2, astl::pass_fn(eq));
    }

    template <typename FwdIt1, typename N, typename FwdIt2, typename Eq, typename P1, typename P2>
    ASTL_NODISCARD auto operator()(FwdIt1 first1, N n1, FwdIt2 first2, N n2, Eq eq, P1 p1,
                                   P2 p2) const -> bool
    {
        return n1 == n2
            && (*this)(first1, n1, first2, astl::pass_fn(eq), astl::pass_fn(p1), astl::pass_fn(p2));
    }
} equal_n{};

} // namespace i

namespace r
{
inline constexpr struct {

    template <typename R1, typename R2, typename BinaryPredicate = std::equal_to<>>
    // requires R1 ForwardIterator range
    // requires R2 ForwardIterator range
    // requires BinaryPredicate, returns bool(are two arguments equivalent),
    //                      arguments value_type(R1) and value_type(R2)
    ASTL_NODISCARD auto operator()(R1 &&r1, R2 &&r2, BinaryPredicate pred = BinaryPredicate{}) const
        -> bool
    {
        return i::equal(adl::begin(r1), adl::end(r1), adl::begin(r2), adl::end(r2),
                        astl::pass_fn(pred));
    }

    template <typename R1, typename R2, typename BinaryPredicate, typename P1, typename P2>
    ASTL_NODISCARD auto operator()(R1 &&r1, R2 &&r2, BinaryPredicate pred, P1 p1, P2 p2) const
        -> bool
    {
        return i::equal(adl::begin(r1), adl::end(r1), adl::begin(r2), adl::end(r2),
                        astl::pass_fn(pred), astl::pass_fn(p1), astl::pass_fn(p2));
    }
} equal{};

inline constexpr struct {
    template <typename R1, typename N, typename R2, typename BinaryPredicate = std::equal_to<>>
    // requires R1 ForwardIterator range
    // requires N integral type
    // requires R2 ForwardIterator range
    // requires BinaryPredicate, returns bool(are two arguments equivalent),
    //                      arguments value_type(R1) and value_type(R2)
    ASTL_NODISCARD auto operator()(R1 &&r1, N n, R2 &&r2,
                                   BinaryPredicate pred = BinaryPredicate{}) const -> bool
    {
        // precondition r1.size() <= n
        // precondition r2.size() <= n
        return i::equal_n(adl::begin(r1), n, adl::begin(r2), astl::pass_fn(pred));
    }

    template <typename R1, typename R2, typename N, typename BinaryPredicate, typename P1,
              typename P2>
    ASTL_NODISCARD auto operator()(R1 &&r1, N n, R2 &&r2, BinaryPredicate pred, P1 p1, P2 p2) const
        -> bool
    {
        return i::equal_n(adl::begin(r1), n, adl::begin(r2), astl::pass_fn(pred), astl::pass_fn(p1),
                          astl::pass_fn(p2));
    }

    template <typename R1, typename N, typename R2, typename BinaryPredicate = std::equal_to<>>
    // requires R1 ForwardIterator range
    // requires N integral type
    // requires R2 ForwardIterator range
    // requires BinaryPredicate, returns bool(are two arguments equivalent),
    //                      arguments value_type(R1) and value_type(R2)
    ASTL_NODISCARD auto operator()(R1 &&r1, N n1, R2 &&r2, N n2,
                                   BinaryPredicate pred = BinaryPredicate{}) const -> bool
    {
        // precondition r1.size() <= n1
        // precondition r2.size() <= n2
        return i::equal_n(adl::begin(r1), n1, adl::begin(r2), n2, astl::pass_fn(pred));
    }

    template <typename R1, typename R2, typename N, typename BinaryPredicate, typename P1,
              typename P2>
    ASTL_NODISCARD auto operator()(R1 &&r1, N n1, R2 &&r2, N n2, BinaryPredicate pred, P1 p1,
                                   P2 p2) const -> bool
    {
        return i::equal_n(adl::begin(r1), n1, adl::begin(r2), n2, astl::pass_fn(pred),
                          astl::pass_fn(p1), astl::pass_fn(p2));
    }
} equal_n{};

} // namespace r
} // namespace astl

#endif // ASTL_INCLUDE_EQUAL_HPP
