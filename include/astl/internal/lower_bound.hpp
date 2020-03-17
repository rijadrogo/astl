//
// Created by Rijad on 29-Jul-18.
//

#ifndef ASTL_INCLUDE_LOWER_BOUND_HPP
#define ASTL_INCLUDE_LOWER_BOUND_HPP

#include <algorithm>
#include <cassert>
#include <utility>

#include "astl/functional.hpp"
#include "astl/iterator.hpp"
#include "astl/range_access.hpp"

namespace astl
{
namespace i
{

inline constexpr struct {
    template <typename FwdIt, typename N, typename UnaryPredicate>
    // requires FwdIt ForwardIterator
    // requires N integral type
    // requires UnaryPredicate, returns bool, arguments of value value_type(FwdIt)
    ASTL_NODISCARD auto operator()(FwdIt first, N n, UnaryPredicate pred) const
        -> std::pair<FwdIt, N>
    {
        assert(n >= 0 && "number of elements must be positive"); //-V2528
        N pos(0);
        N n1(n);
        while (n1 != 0) {
            N half(n1 >> 1);
            FwdIt m(astl::next(first, half));
            if (pred(*m)) {
                first = ++m;
                n1 -= half + 1;
                pos = half + pos + 1;
            }
            else {
                n1 = half;
            }
        }
        return std::make_pair(first, pos);
    }

    template <typename FwdIt, typename N, typename UnaryPredicate, typename P>
    ASTL_NODISCARD auto operator()(FwdIt first, N n, UnaryPredicate pred, P p) const
        -> std::pair<FwdIt, N>
    {
        return (*this)(first, n, astl::combine(astl::pass_fn(pred), astl::pass_fn(p)));
    }

} partition_point_n{};

inline constexpr struct {

    template <typename FwdIt, typename T, typename Comparator = std::less<>>
    ASTL_NODISCARD auto operator()(FwdIt first, FwdIt last, T const &value,
                                   Comparator comp = Comparator{}) const -> FwdIt
    {
        return std::lower_bound(first, last, value, astl::pass_fn(comp));
    }

    template <typename FwdIt, typename T, typename Comparator, typename P>
    ASTL_NODISCARD auto operator()(FwdIt first, FwdIt last, T const &value, Comparator comp,
                                   P p) const -> FwdIt
    {
        // precondition: is_sorted(first, last, comp, p)
        return std::lower_bound(first, last, value,
                                astl::combine(astl::pass_fn(comp), astl::pass_fn(p)));
    }
} lower_bound{};

inline constexpr struct {

    template <typename FwdIt, typename N, typename T, typename Comparator = std::less<>>
    // requires FwdIt ForwardIterator
    // requires N integral type
    // requires T to be comparable with value_type(FwdIt) via Comparator
    // requires Comparator, returns bool, takes two arguments of type T, value_type(FwdIt)
    ASTL_NODISCARD auto operator()(FwdIt first, N n, T const &value,
                                   Comparator comp = Comparator{}) const -> std::pair<FwdIt, N>
    {
        // precondition: is_sorted_n(first, n, comp)
        return i::partition_point_n(first, n, astl::bind2nd(astl::pass_fn(comp), value));
    }

    template <typename FwdIt, typename N, typename T, typename Comparator, typename P>
    ASTL_NODISCARD auto lower_bound_n(FwdIt first, N n, T const &value, Comparator comp, P p)
        -> std::pair<FwdIt, N>
    {
        // precondition: is_sorted_n(first, n, comp, p)
        return (*this)(first, n, value,
                       [proj(astl::pass_fn(p)), c(astl::pass_fn(comp))](auto &&elem, auto &&value) {
                           return c(invoke(proj, elem), value);
                       });
    }
} lower_bound_n{};

} // namespace i

namespace r
{
inline constexpr struct {

    template <typename R, typename T, typename Comparator = std::less<>>
    // requires R ForwardIterator range
    // requires T to be comparable with value_type(R) via Comparator
    // requires Comparator, returns bool, takes two arguments of type T, value_type(R)
    auto operator()(R &&r, T const &value, Comparator comp = Comparator{}) const -> iter_of_range<R>
    {
        // precondition: is_sorted(r, comp)
        return i::lower_bound(adl::begin(r), adl::end(r), value, astl::pass_fn(comp));
    }

    template <typename R, typename T, typename C, typename P>
    auto operator()(R &&r, T const &value, C comp, P p) const -> iter_of_range<R>
    {
        // precondition: is_sorted(r, comp, p)
        return i::lower_bound(adl::begin(r), adl::end(r), value, astl::pass_fn(comp),
                              astl::pass_fn(p));
    }

} lower_bound{};

inline constexpr struct {
    template <typename R, typename N, typename T, typename Comparator = std::less<>>
    // requires R ForwardIterator range
    // requires N integral type
    // requires T to be comparable with value_type(R) via Comparator
    // requires Comparator, returns bool, takes two arguments of type T, value_type(R)
    auto operator()(R &&r, N n, T const &value, Comparator comp = Comparator{}) const
        -> std::pair<astl::iter_of_range<R>, N>
    {
        // precondition: is_sorted_n(r, comp)
        return i::lower_bound_n(adl::begin(r), n, value, astl::pass_fn(comp));
    }

    template <typename R, typename N, typename T, typename Comparator, typename P>
    auto operator()(R &&r, N n, T const &value, Comparator comp, P p) const
        -> std::pair<astl::iter_of_range<R>, N>
    {
        // precondition: is_sorted_n(r, comp, p)
        return i::lower_bound_n(adl::begin(r), adl::end(r), n, value, astl::pass_fn(comp),
                                astl::pass_fn(p));
    }

} lower_bound_n{};

inline constexpr struct {
    template <typename R, typename N, typename UnaryPredicate>
    // requires R ForwardIterator range
    // requires N integral type
    // requires UnaryPredicate, returns bool, arguments of value value_type(I)
    auto operator()(R &&r, N n, UnaryPredicate pred) const -> std::pair<astl::iter_of_range<R>, N>
    {
        return i::partition_point_n(adl::begin(r), n, astl::pass_fn(pred));
    }

    template <typename R, typename N, typename UnaryPredicate, typename P>
    auto operator()(R &&r, N n, UnaryPredicate pred, P p) const
        -> std::pair<astl::iter_of_range<R>, N>
    {
        return i::partition_point_n(adl::begin(r), n, astl::pass_fn(pred), astl::pass_fn(p));
    }
} partition_point_n{};
} // namespace r
} // namespace astl
#endif // ASTL_INCLUDE_LOWER_BOUND_HPP
