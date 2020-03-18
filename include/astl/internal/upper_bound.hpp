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

inline constexpr struct {

    template <typename FwdIt, typename T, typename Comparator = std::less<>>
    ASTL_NODISCARD auto operator()(FwdIt first, FwdIt last, T const &value,
                                   Comparator comp = Comparator{}) const -> FwdIt
    {
        return std::upper_bound(first, last, value, astl::pass_fn(comp));
    }

    template <typename FwdIt, typename T, typename Comparator, typename P>
    ASTL_NODISCARD auto operator()(FwdIt first, FwdIt last, T const &value, Comparator comp,
                                   P p) const -> FwdIt
    {
        return std::upper_bound(first, last, value,
                                astl::combine(astl::pass_fn(comp), astl::pass_fn(p)));
    }

} upper_bound{};

inline constexpr struct {
    template <typename FwdIt, typename N, typename T, typename Comparator = std::less<>>
    // requires FwdIt ForwardIterator
    // requires N integral type
    // requires T to be comparable with value_type(FwdIt) via Comparator
    // requires Comparator, returns bool, takes two arguments of type T, value_type(FwdIt)
    ASTL_NODISCARD auto operator()(FwdIt first, N n, T const &value,
                                   Comparator comp = Comparator{}) const -> std::pair<FwdIt, N>
    {
        // precondition: i::is_sorted_n(first, n, comp)
        return i::partition_point_n(
            first, n, [c(astl::pass_fn(comp)), &value](auto &&x) { return !c(value, x); });
    }

    template <typename FwdIt, typename N, typename T, typename Comparator, typename P>
    ASTL_NODISCARD auto upper_bound_n(FwdIt first, N n, T const &value, Comparator comp, P p) const
        -> std::pair<FwdIt, N>
    {
        // precondition: i::is_sorted_n(first, n, comp, p)
        return (*this)(first, n, value,
                       [proj(astl::pass_fn(p)), c(astl::pass_fn(comp))](auto &&elem, auto &&value) {
                           return c(elem, invoke(proj, value));
                       });
    }
} upper_bound_n{};

} // namespace i

namespace r
{

inline constexpr struct {
    template <typename R, typename T, typename Comparator = std::less<>>
    // requires R ForwardIterator range
    // requires T to be comparable with value_type(R) via Comparator
    // requires Comparator, returns bool, takes two arguments of type T, value_type(R)
    ASTL_NODISCARD auto operator()(R &&r, T const &value, Comparator comp = Comparator{}) const
        -> iter_of_range<R>
    {
        // precondition: is_sorted(begin(r), end(r), comp)
        return i::upper_bound(adl::begin(r), adl::end(r), value, astl::pass_fn(comp));
    }

    template <typename R, typename T, typename Comparator, typename P>
    ASTL_NODISCARD auto operator()(R &&r, T const &value, Comparator comp, P p) const
        -> iter_of_range<R>
    {
        // precondition: is_sorted(begin(r), end(r), comp, p)
        return i::upper_bound(adl::begin(r), adl::end(r), value, astl::pass_fn(comp),
                              astl::pass_fn(p));
    }
} upper_bound{};

inline constexpr struct {
    template <typename R, typename N, typename T, typename Comparator = std::less<>>
    // requires R ForwardIterator range
    // requires N integral type
    // requires T to be comparable with value_type(R) via Comparator
    // requires Comparator, returns bool, takes two arguments of type T, value_type(R)
    ASTL_NODISCARD auto operator()(R &&r, N n, T const &value, Comparator comp = Comparator{}) const
        -> std::pair<astl::iter_of_range<R>, N>
    {
        // precondition: is_sorted_n(begin(r), n, comp)
        return i::upper_bound_n(adl::begin(r), n, value, pass_fn(comp));
    }

    template <typename R, typename N, typename T, typename Comparator, typename P>
    ASTL_NODISCARD auto operator()(R &&r, N n, T const &value, Comparator comp, P p) const
        -> std::pair<astl::iter_of_range<R>, N>
    {
        // precondition: is_sorted_n(begin(r), n, comp, p)
        return i::upper_bound_n(adl::begin(r), adl::end(r), n, value, astl::pass_fn(comp),
                                astl::pass_fn(p));
    }
} upper_bound_n{};

} // namespace r
} // namespace astl

#endif // ASTL_INCLUDE_UPPER_BOUND_HPP
