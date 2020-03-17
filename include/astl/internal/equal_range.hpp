//
// Created by Rijad on 29-Jul-18.
//

#ifndef ASTL_INCLUDE_EQUAL_RANGE_HPP
#define ASTL_INCLUDE_EQUAL_RANGE_HPP

#include <algorithm>
#include <utility>

#include "lower_bound.hpp"
#include "upper_bound.hpp"

#include "astl/functional.hpp"
#include "astl/iterator.hpp"
#include "astl/map_iterator.hpp"
#include "astl/range_access.hpp"

namespace astl
{
namespace i
{

inline constexpr struct {

    template <typename FwdIt, typename T, typename Comparator = std::less<>>
    ASTL_NODISCARD auto operator()(FwdIt first, FwdIt last, T const &elem,
                                   Comparator comp = Comparator{}) const -> std::pair<FwdIt, FwdIt>
    {
        // precondition: is_sorted(first, last, comp)
        return std::equal_range(first, last, elem, astl::pass_fn(comp));
    }

    template <typename FwdIt, typename T, typename Comparator, typename P>
    ASTL_NODISCARD auto operator()(FwdIt first, FwdIt last, T const &elem, Comparator comp,
                                   P p) const -> std::pair<FwdIt, FwdIt>
    {
        // precondition: is_sorted(first, last, comp, p)
        auto proj(astl::pass_fn(p));
        auto pair(std::equal_range(astl::map_iterator(first, proj), astl::map_iterator(last, proj),
                                   elem, astl::pass_fn(comp)));

        return std::make_pair(pair.first.base(), pair.second.base());
    }
} equal_range{};

inline constexpr struct {
    template <typename FwdIt, typename N, typename T, typename Comparator = std::less<>>
    // requires FwdIt ForwardIterator
    // requires N integral type
    // requires T to be comparable with value_type(FwdIt) via Comparator
    // requires Comparator, returns bool, takes two arguments of type T, value_type(FwdIt)
    ASTL_NODISCARD auto operator()(FwdIt first, N n, T const &value,
                                   Comparator comp = Comparator{}) const -> std::pair<FwdIt, FwdIt>
    {
        // precondition: is_sorted_n(first, n, comp)
        N n1(n);
        while (n1 != 0) {
            N n2(n1 >> 1);
            FwdIt mid(astl::next(first, n2));
            if (comp(*mid, value)) {
                first = astl::next(mid);
                n1 -= n2 + 1;
            }
            else if (comp(value, *mid)) {
                n1 = n2;
            }
            else {
                auto p(astl::pass_fn(comp));
                return std::make_pair(
                    i::lower_bound_n(first, n2, value, p).first,
                    i::upper_bound_n(astl::next(mid), n - n2 - N(1), value, p).first);
            }
        }
        return std::make_pair(first, first);
    }

    template <typename FwdIt, typename T, typename BinaryPredicate, typename P>
    ASTL_NODISCARD auto operator()(FwdIt first, iter_diff_type<FwdIt> n, T const &value,
                                   BinaryPredicate pred, P p) const -> std::pair<FwdIt, FwdIt>
    {
        // precondition: is_sorted_n(first, n, pred, p)
        auto pair(
            (*this)(astl::map_iterator(first, astl::pass_fn(p)), n, value, astl::pass_fn(pred)));
        return std::make_pair(pair.first.base(), pair.second.base());
    }
} equal_range_n{};

} // namespace i

namespace internal_equal_range
{
template <typename R, typename T>
auto equal_range1(R &&r, T &&value, internal_adl::rank<0>) -> iter_of_range<R>
{
    return i::equal_range(adl::begin(r), adl::end(r), value);
}

template <typename R, typename T>
auto equal_range1(R &&r, T &&value, internal_adl::rank<1>)
    -> decltype(r.equal_range(static_cast<T &&>(value)))
{
    return r.equal_range(static_cast<T &&>(value));
}
} // namespace internal_equal_range

namespace r
{

inline constexpr struct {
    template <typename R, typename T>
    // requires R ForwardIterator range
    // requires T to be comparable with value_type(R) via op<
    ASTL_NODISCARD auto operator()(R &&r, T const &elem) const
        -> std::pair<iter_of_range<R>, iter_of_range<R>>
    {
        // precondition: is_sorted(r, _pred)
        return internal_equal_range::equal_range1(r, elem, internal_adl::rank<1>{});
    }

    template <typename R, typename T, typename Comparator>
    // requires R ForwardIterator range
    // requires T to be comparable with value_type(R) via Comparator
    // requires Comparator, StrictWeakOrdering, takes two arguments of type T,
    // value_type(R)
    ASTL_NODISCARD auto operator()(R &&r, T const &elem, Comparator comp) const
        -> std::pair<iter_of_range<R>, iter_of_range<R>>
    {
        // precondition: is_sorted_n(first, n, _pred)
        return i::equal_range(adl::begin(r), adl::end(r), elem, astl::pass_fn(comp));
    }

    template <typename R, typename T, typename Comparator, typename P>
    ASTL_NODISCARD auto operator()(R &&r, T const &elem, Comparator comp, P p) const
        -> std::pair<iter_of_range<R>, iter_of_range<R>>
    {
        // precondition: is_sorted(r, comp)
        return i::equal_range(adl::begin(r), adl::end(r), elem, astl::pass_fn(comp),
                              astl::pass_fn(p));
    }

} equal_range{};

inline constexpr struct {

    template <typename R, typename N, typename T, typename BinaryPredicate = std::less<>>
    // requires R ForwardIterator range
    // requires N integral type
    // requires T to be comparable with value_type(R) via Comparator
    // requires Comparator, returns bool, takes two arguments of type T, value_type(R)
    ASTL_NODISCARD auto operator()(R &&r, N n, T const &value,
                                   BinaryPredicate pred = BinaryPredicate{}) const
        -> std::pair<iter_of_range<R>, iter_of_range<R>>
    {
        // precondition: is_sorted_n(r, n, pred)
        return i::equal_range_n(adl::begin(r), n, value, astl::pass_fn(pred));
    }

    template <typename R, typename T, typename BinaryPredicate, typename P>
    ASTL_NODISCARD auto operator()(R &&r, range_diff_type<R> n, T const &value,
                                   BinaryPredicate pred, P p) const
        -> std::pair<iter_of_range<R>, iter_of_range<R>>
    {
        // precondition: is_sorted_n(r, n, pred, p)
        return i::equal_range_n(adl::begin(r), n, value, astl::pass_fn(pred), astl::pass_fn(p));
    }
} equal_range_n{};

} // namespace r
} // namespace astl

#endif // ASTL_INCLUDE_EQUAL_RANGE_HPP
