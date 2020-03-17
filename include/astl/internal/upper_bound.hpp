//
// Created by Rijad on 29-Jul-18.
//

#ifndef ASTL_INCLUDE_UPPER_BOUND_HPP
#define ASTL_INCLUDE_UPPER_BOUND_HPP

#include <algorithm>
#include <utility>

#include "astl/functional.hpp"
#include "astl/iterator.hpp"
#include "astl/range_access.hpp"

namespace astl
{
namespace i
{
using std::upper_bound;// NOLINT(misc-unused-using-decls)

template <typename FwdIt, typename T, typename Comparator, typename P>
ASTL_NODISCARD auto upper_bound(FwdIt first, FwdIt last, T const &value, Comparator comp, P p) -> FwdIt
{
    return std::upper_bound(first, last, value,
                            astl::combine(astl::pass_fn(comp), astl::pass_fn(p)));
}

template <typename FwdIt, typename N, typename T, typename Comparator>
// requires FwdIt ForwardIterator
// requires N integral type
// requires T to be comparable with value_type(FwdIt) via Comparator
// requires Comparator, returns bool, takes two arguments of type T, value_type(FwdIt)
ASTL_NODISCARD auto upper_bound_n(FwdIt first, N n, T const &value, Comparator comp) -> std::pair<FwdIt, N>
{
    // precondition: i::is_sorted_n(first, n, comp)
    return i::partition_point_n(first, n,
                                [c(astl::pass_fn(comp)), &value](auto &&x) { return !c(value, x); });
}

template <typename FwdIt, typename N, typename T>
// requires FwdIt ForwardIterator
// requires N integral type
// requires T to be comparable with value_type(FwdIt) via Comparator
ASTL_NODISCARD auto upper_bound_n(FwdIt first, N n, T const &value) -> std::pair<FwdIt, N>
{
    // precondition: i::is_sorted_n(first, n)
    return i::upper_bound_n(first, n, value, std::less{});
}

template <typename FwdIt, typename N, typename T, typename Comparator, typename P>
ASTL_NODISCARD auto upper_bound_n(FwdIt first, N n, T const &value, Comparator comp, P p)
    -> std::pair<FwdIt, N>
{
    // precondition: i::is_sorted_n(first, n, comp, p)
    return i::upper_bound_n(
        first, n, value,
        [proj(astl::pass_fn(p)), c(astl::pass_fn(comp))](auto &&elem, auto &&value) {
            return c(elem, invoke(proj, value));
        });
}
}// namespace i

namespace r
{
template <typename R, typename T>
// requires R ForwardIterator range
// requires T to be comparable with value_type(R) via Comparator
ASTL_NODISCARD auto upper_bound(R &&r, T const &value) -> iter_of_range<R>
{
    // precondition: is_sorted(begin(r), end(r))
    return i::upper_bound(adl::begin(r), adl::end(r), value);
}

template <typename R, typename T, typename C>
// requires R ForwardIterator range
// requires T to be comparable with value_type(R) via Comparator
// requires Comparator, returns bool, takes two arguments of type T, value_type(R)
ASTL_NODISCARD auto upper_bound(R &&r, T const &value, C comp) -> iter_of_range<R>
{
    // precondition: is_sorted(begin(r), end(r), comp)
    return i::upper_bound(adl::begin(r), adl::end(r), value, astl::pass_fn(comp));
}

template <typename R, typename T, typename C, typename P>
ASTL_NODISCARD auto upper_bound(R &&r, T const &value, C comp, P p) -> iter_of_range<R>
{
    // precondition: is_sorted(begin(r), end(r), comp, p)
    return i::upper_bound(adl::begin(r), adl::end(r), value, astl::pass_fn(comp), astl::pass_fn(p));
}

template <typename R, typename N, typename T, typename Comparator>
// requires R ForwardIterator range
// requires N integral type
// requires T to be comparable with value_type(R) via Comparator
// requires Comparator, returns bool, takes two arguments of type T, value_type(R)
ASTL_NODISCARD auto upper_bound_n(R &&r, N n, T const &value, Comparator comp)
    -> std::pair<astl::iter_of_range<R>, N>
{
    // precondition: is_sorted_n(begin(r), n, comp)
    return i::upper_bound_n(adl::begin(r), n, value, pass_fn(comp));
}

template <typename R, typename N, typename T>
// requires R ForwardIterator range
// requires N integral type
// requires T to be comparable with value_type(R) via Comparator
ASTL_NODISCARD auto upper_bound_n(R &&r, N n, T const &value)
    -> std::pair<astl::iter_of_range<R>, N>
{
    // precondition: is_sorted_n(begin(r), n)
    return r::upper_bound_n(r, n, value, std::less{});
}

template <typename R, typename N, typename T, typename Comparator, typename P>
ASTL_NODISCARD auto upper_bound_n(R &&r, N n, T const &value, Comparator comp, P p)
    -> std::pair<astl::iter_of_range<R>, N>
{
    // precondition: is_sorted_n(begin(r), n, comp, p)
    return i::upper_bound_n(adl::begin(r), adl::end(r), n, value, astl::pass_fn(comp),
                            astl::pass_fn(p));
}
}// namespace r
}// namespace astl

#endif// ASTL_INCLUDE_UPPER_BOUND_HPP
