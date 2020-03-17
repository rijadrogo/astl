//
// Created by Rijad on 17-Aug-19.
//

#ifndef ASTL_INCLUDE_MAX_HPP
#define ASTL_INCLUDE_MAX_HPP

#include <algorithm>
#include <initializer_list>
#include <utility>

#include "astl/internal/find.hpp"

#include "astl/functional.hpp"
#include "astl/iterator.hpp"
#include "astl/range_access.hpp"

namespace astl
{

template <typename T> ASTL_NODISCARD auto max(std::initializer_list<T> ilist) -> T
{
    return std::max_element(ilist.begin(), ilist.end());
}

template <typename T> ASTL_NODISCARD auto max(T &a, T &b) noexcept -> T & { return a < b ? b : a; }

template <typename T> ASTL_NODISCARD auto max(T const &a, T const &b) noexcept -> T const &
{
    return a < b ? b : a;
}

template <typename T, typename Comparator>
ASTL_NODISCARD auto max(T &a, T &b, Comparator comp) -> T &
{
    return comp(a, b) ? b : a;
}

template <typename T, typename Comparator>
ASTL_NODISCARD auto max(T const &a, T const &b, Comparator comp) -> T const &
{
    return comp(a, b) ? b : a;
}

template <typename T, typename Comparator, typename P>
ASTL_NODISCARD auto max(T &a, T &b, Comparator comp, P p) -> T &
{
    return comp(invoke(p, a), invoke(p, b)) ? b : a;
}

template <typename T, typename Comparator, typename P>
ASTL_NODISCARD auto max(T const &a, T const &b, Comparator comp, P p) -> T const &
{
    return comp(invoke(p, a), invoke(p, b)) ? b : a;
}

namespace i
{
inline constexpr struct {

    template <typename FwdIt, typename Comparator = std::less<>>
    ASTL_NODISCARD auto operator()(FwdIt first, FwdIt last, Comparator comp = Comparator{}) const
        -> FwdIt
    {
        return std::max_element(first, last, astl::pass_fn(comp));
    }

    template <typename FwdIt, typename Comparator, typename P>
    ASTL_NODISCARD auto operator()(FwdIt first, FwdIt last, Comparator comp, P p) const -> FwdIt
    {
        return std::max_element(first, last, astl::combine(astl::pass_fn(comp), astl::pass_fn(p)));
    }

} max_element{};

inline constexpr struct {

    template <typename FwdIt, typename N, typename Comparator = std::less<>>
    ASTL_NODISCARD auto operator()(FwdIt first, N n, Comparator comp = Comparator{}) const
        -> std::pair<FwdIt, N>
    {
        if (n != N(0)) {
            FwdIt result(first);
            N i(n);
            ++first;
            while (--n != N(0)) {
                if (comp(*result, *first)) {
                    result = first;
                    i = n;
                }
                ++first;
            }
            return std::make_pair(result, --i);
        }
        return std::make_pair(first, n);
    }

    template <typename FwdIt, typename N, typename Comparator, typename P>
    ASTL_NODISCARD auto operator()(FwdIt first, N n, Comparator comp, P p) const
        -> std::pair<FwdIt, N>
    {
        return (*this)(first, n, astl::combine(astl::pass_fn(comp), astl::pass_fn(p)));
    }

} max_element_n{};

inline constexpr struct {
    // Function to find contiguous sub-array with the largest sum
    // in given set of integers (handles negative numbers as well)
    template <typename FwdIt, typename BinaryOp = std::plus<>, typename CompareF = std::less<>>
    // requires FwdIt ForwardIterator
    // requires BinaryPredicate, returns ValueType(FwdIt), two arguments of
    // ValueType(FwdIt) requires CompareF StrictWeakOrdering
    ASTL_NODISCARD auto operator()(FwdIt first, FwdIt last, BinaryOp op = BinaryOp{},
                                   CompareF comp = CompareF{}) const
        -> optional<astl::iter_value_type<FwdIt>>
    {
        return std::get<2>(
            i::find_max_sum_of_subarray(first, last, astl::pass_fn(op), astl::pass_fn(comp)));
    }

    // Function to find contiguous sub-array with the largest sum
    // in given set of integers (handles negative numbers as well)
    template <typename FwdIt, typename BinaryOp, typename CompareF, typename P>
    ASTL_NODISCARD auto operator()(FwdIt first, FwdIt last, BinaryOp op, CompareF comp, P p) const
        -> optional<astl::iter_value_type<FwdIt>>
    {
        auto pp(astl::pass_fn(p));
        return (*this)(astl::map_iterator(first, pp), astl::map_iterator(last, pp),
                       astl::pass_fn(op), astl::pass_fn(comp));
    }
} max_sum_of_subarray{};

} // namespace i
namespace r
{
inline constexpr struct {

    template <typename R, typename Comparator, typename P>
    ASTL_NODISCARD auto operator()(R &&r, Comparator comp, P p) const -> iter_of_range<R>
    {
        return i::max_element(adl::begin(r), adl::end(r), astl::pass_fn(comp), astl::pass_fn(p));
    }

    template <typename R, typename Comparator = std::less<>>
    ASTL_NODISCARD auto operator()(R &&r, Comparator comp = Comparator{}) const -> iter_of_range<R>
    {
        return i::max_element(adl::begin(r), adl::end(r), astl::pass_fn(comp));
    }

} max_element{};

inline constexpr struct {
    template <typename R, typename N, typename Comparator, typename P>
    ASTL_NODISCARD auto operator()(R &&r, N n, Comparator comp, P p) const
        -> std::pair<astl::iter_of_range<R>, N>
    {
        return i::max_element_n(adl::begin(r), n, astl::pass_fn(comp), astl::pass_fn(p));
    }

    template <typename R, typename N, typename Comparator = std::less<>>
    ASTL_NODISCARD auto operator()(R &&r, N n, Comparator comp = Comparator{}) const
        -> std::pair<astl::iter_of_range<R>, N>
    {
        return i::max_element_n(adl::begin(r), n, astl::pass_fn(comp));
    }

} max_element_n{};

inline constexpr struct {
    template <typename R, typename BinaryOp, typename CompareF>
    // requires R ForwardIterator range
    // requires BinaryPredicate, returns ValueType(R), two arguments of ValueType(R)
    // requires CompareF StrictWeakOrdering
    ASTL_NODISCARD auto operator()(R &&r, BinaryOp op, CompareF comp) const
        -> optional<astl::range_value_type<R>>
    {
        return i::max_sum_of_subarray(adl::begin(r), adl::end(r), astl::pass_fn(op),
                                      astl::pass_fn(comp));
    }

    template <typename R, typename BinaryOp, typename CompareF, typename P>
    ASTL_NODISCARD auto operator()(R &&r, BinaryOp op, CompareF comp, P p) const
        -> optional<astl::range_value_type<R>>
    {
        return i::max_sum_of_subarray(adl::begin(r), adl::end(r), astl::pass_fn(op),
                                      astl::pass_fn(comp), astl::pass_fn(p));
    }

} max_sum_of_subarray{};

} // namespace r
} // namespace astl

#endif // ASTL_INCLUDE_MAX_HPP
