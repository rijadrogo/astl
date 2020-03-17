//
// Created by Rijad on 14-Aug-18.
//

#ifndef ASTL_INCLUDE_STARTS_ENDS_HPP
#define ASTL_INCLUDE_STARTS_ENDS_HPP

#include <algorithm>
#include <iterator>

#include "mismatch.hpp"

#include "astl/functional.hpp"
#include "astl/range_access.hpp"

namespace astl
{
namespace i
{

template <typename FwdIt1, typename FwdIt2, typename BinaryPredicate>
ASTL_NODISCARD auto starts_with(FwdIt1 first1, FwdIt1 last1, FwdIt2 first2, FwdIt2 last2,
                                BinaryPredicate pred) -> bool
{
    // TODO test if equal is faster than mismatch
    if constexpr (is_random_access_it_v<FwdIt1, FwdIt2>) {
        auto size1(std::distance(first1, last1));
        auto size2(std::distance(first2, last2));
        if (size1 < size2) return false;

        return std::equal(first1, first1 + size2, first2, astl::pass_fn(pred));
    }
    else {
        return i::mismatch(first2, last2, first1, last1, astl::pass_fn(pred)).first == last2;
    }
}

template <typename FwdIt1, typename FwdIt2>
ASTL_NODISCARD auto starts_with(FwdIt1 first1, FwdIt1 last1, FwdIt2 first2, FwdIt2 last2) -> bool
{
    return i::starts_with(first1, last1, first2, last2, std::equal_to{});
}

template <typename FwdIt1, typename FwdIt2, typename BinaryPredicate, typename P1, typename P2>
ASTL_NODISCARD auto starts_with(FwdIt1 first1, FwdIt1 last1, FwdIt2 first2, FwdIt2 last2,
                                BinaryPredicate pred, P1 p1, P2 p2) -> bool
{
    return i::mismatch(first2, last2, first1, last1, astl::pass_fn(pred), astl::pass_fn(p2),
                       astl::pass_fn(p1))
               .first
        == last2;
}

template <typename FwdIt1, typename FwdIt2, typename BinaryPredicate>
ASTL_NODISCARD auto ends_with(FwdIt1 first1, FwdIt1 last1, FwdIt2 first2, FwdIt2 last2,
                              BinaryPredicate pred) -> bool
{
    auto const drop(std::distance(first1, last1) - std::distance(first2, last2));
    if (drop < 0) return false;

    return std::equal(std::next(first1, drop), last1, first2, astl::pass_fn(pred));
}

template <typename FwdIt1, typename FwdIt2>
ASTL_NODISCARD auto ends_with(FwdIt1 first1, FwdIt1 last1, FwdIt2 first2, FwdIt2 last2) -> bool
{
    return i::ends_with(first1, last1, first2, last2, std::equal_to{});
}

template <typename FwdIt1, typename FwdIt2, typename BinaryPredicate, typename P>
ASTL_NODISCARD auto ends_with(FwdIt1 first1, FwdIt1 last1, FwdIt2 first2, FwdIt2 last2,
                              BinaryPredicate pred, P p) -> bool
{
    return i::ends_with(first1, last1, first2, last2,
                        astl::combine(astl::pass_fn(pred), astl::pass_fn(p)));
}
} // namespace i

namespace r1
{

template <typename R1, typename R2, typename BinaryPredicate>
ASTL_NODISCARD auto starts_with(R1 &&r1, R2 &&r2, BinaryPredicate pred) -> bool
{
    return i::starts_with(adl::begin(r1), adl::end(r1), adl::begin(r2), adl::end(r2),
                          astl::pass_fn(pred));
}

template <typename R1, typename R2> ASTL_NODISCARD auto starts_with(R1 &&r1, R2 r2) -> bool
{
    return i::starts_with(adl::begin(r1), adl::end(r1), adl::begin(r2), adl::end(r2),
                          std::equal_to{});
}

template <typename R1, typename R2, typename BinaryPredicate, typename P1, typename P2>
ASTL_NODISCARD auto starts_with(R1 &&r1, R2 &&r2, BinaryPredicate pred, P1 p1, P2 p2) -> bool
{
    return i::starts_with(adl::begin(r1), adl::end(r1), adl::begin(r2), adl::end(r2),
                          astl::pass_fn(pred), astl::pass_fn(p1), astl::pass_fn(p2));
}

//

template <typename R1, typename R2, typename BinaryPredicate>
ASTL_NODISCARD auto ends_with(R1 &&r1, R2 &&r2, BinaryPredicate pred) -> bool
{
    return i::ends_with(adl::begin(r1), adl::end(r1), adl::begin(r2), adl::end(r2),
                        astl::pass_fn(pred));
}

template <typename R1, typename R2> ASTL_NODISCARD auto startends_withs_with(R1 &&r1, R2 r2) -> bool
{
    return i::ends_with(adl::begin(r1), adl::end(r1), adl::begin(r2), adl::end(r2),
                        std::equal_to{});
}

template <typename R1, typename R2, typename BinaryPredicate, typename P1, typename P2>
ASTL_NODISCARD auto ends_with(R1 &&r1, R2 &&r2, BinaryPredicate pred, P1 p1, P2 p2) -> bool
{
    return i::ends_with(adl::begin(r1), adl::end(r1), adl::begin(r2), adl::end(r2),
                        astl::pass_fn(pred), astl::pass_fn(p1), astl::pass_fn(p2));
}

} // namespace r1
} // namespace astl

#endif // ASTL_INCLUDE_STARTS_ENDS_HPP
