//
// Created by Rijad on 04-Aug-18.
//

#ifndef ASTL_INCLUDE_GATHER_HPP
#define ASTL_INCLUDE_GATHER_HPP

#include <utility>

#include "stable_partition.hpp"

#include "astl/functional.hpp"
#include "astl/iterator.hpp"
#include "astl/range_access.hpp"

namespace astl
{
namespace i
{
/// gather() takes a collection of elements defined by a pair of iterators and
/// moves the ones satisfying a predicate to them to a position (called the
/// pivot) within the sequence. The algorithm is stable. The result is a pair of
/// iterators that contains the items that satisfy the predicate. Given an
/// sequence containing: <pre> 0 1 2 3 4 5 6 7 8 9
/// </pre>
/// a call to gather ( arr, arr + 10, arr + 4, IsEven ()) will result in:
/// <pre>
/// 1 3 0 2 4 6 8 5 7 9
///     |---|-----|
///  first  |   second
///       pivot
/// </pre>
/// The problem is broken down into two basic steps, namely, moving the items
/// before the pivot and then moving the items from the pivot to the end. These
/// "moves" are done with calls to stable_partition. \par Storage Requirements:
/// The algorithm uses stable_partition, which will attempt to allocate
/// temporary memory, but will work in-situ if there is none available. \par
/// Time Complexity: If there is sufficient memory available, the run time is
/// linear in <code>O(N)</code>. If there is not any memory available, then the
/// run time is <code>O(N log N)</code>.
inline constexpr struct {
    template <typename FwdIt, typename UnaryPredicate>
    // requires FwdIt ForwardIterator
    // requires UnaryPredicate, returns bool, argument of type value_type(FwdIt)
    auto operator()(FwdIt first, FwdIt last, FwdIt pivot, UnaryPredicate pred) const
        -> std::pair<FwdIt, FwdIt>
    {
        // precondition first <= pivot <= last
        // The first call partitions everything up to (but not including) the pivot
        // element, while the second call partitions the rest of the sequence.
        auto fp(astl::pass_fn(pred));
        return std::make_pair(i::stable_partition(first, pivot, astl::not_fn(fp)),
                              i::stable_partition(pivot, last, fp));
    }

    template <typename FwdIt, typename UnaryPredicate, typename P>
    auto operator()(FwdIt first, FwdIt last, FwdIt pivot, UnaryPredicate pred, P p) const
        -> std::pair<FwdIt, FwdIt>
    {
        return (*this)(first, last, pivot, astl::combine(astl::pass_fn(pred), astl::pass_fn(p)));
    }

} gather{};

inline constexpr struct {

    template <typename FwdIt, typename N, typename UnaryPredicate>
    // requires FwdIt ForwardIterator
    // requires UnaryPredicate, returns bool, argument of type value_type(FwdIt)
    auto operator()(FwdIt first, N n, FwdIt pivot, UnaryPredicate pred) const
        -> std::pair<FwdIt, FwdIt>
    {
        // precondition first <= pivot <= first + n
        // The first call partitions everything up to (but not including) the pivot
        // element, while the second call partitions the rest of the sequence.
        auto fp(astl::pass_fn(pred));
        N lhs(astl::distance(first, pivot));
        N rhs(n - lhs);
        return std::make_pair(i::stable_partition_n(first, lhs, astl::not_fn(fp)),
                              i::stable_partition_n(pivot, rhs, fp));
    }

    template <typename FwdIt, typename N, typename UnaryPredicate, typename P>
    auto operator()(FwdIt first, N n, FwdIt pivot, UnaryPredicate pred, P p) const
        -> std::pair<FwdIt, FwdIt>
    {
        return (*this)(first, n, pivot, astl::combine(astl::pass_fn(pred), astl::pass_fn(p)));
    }

} gather_n{};

} // namespace i

namespace r
{
inline constexpr struct {
    template <typename R, typename FwdIt, typename UnaryPredicate>
    // requires R ForwardIterator range
    // requires FwdIt BidirectionalIterator
    // requires UnaryPredicate, returns bool, argument of type value_type(R)
    auto operator()(R &&r, FwdIt pivot, UnaryPredicate pred) const
        -> std::pair<iter_of_range<R>, iter_of_range<R>>
    {
        // precondition r.begin() <= pivot <= r.end()
        return i::gather(adl::begin(r), adl::end(r), pivot, astl::pass_fn(pred));
    }

    template <typename R, typename FwdIt, typename UnaryPredicate, typename P>
    auto operator()(R &&r, FwdIt pivot, UnaryPredicate pred, P p) const
        -> std::pair<iter_of_range<R>, iter_of_range<R>>
    {
        // precondition r.begin() <= pivot <= r.end()
        return i::gather(adl::begin(r), adl::end(r), pivot, astl::pass_fn(pred), astl::pass_fn(p));
    }

} gather{};

inline constexpr struct {
    template <typename R, typename N, typename FwdIt, typename UnaryPredicate>
    // requires R ForwardIterator range
    // requires FwdIt BidirectionalIterator
    // requires UnaryPredicate, returns bool, argument of type value_type(R)
    auto operator()(R &&r, N n, FwdIt pivot, UnaryPredicate pred) const
        -> std::pair<iter_of_range<R>, iter_of_range<R>>
    {
        // precondition r.begin() <= pivot <= r.end()
        return i::gather_n(adl::begin(r), n, pivot, astl::pass_fn(pred));
    }

    template <typename R, typename N, typename FwdIt, typename UnaryPredicate, typename P>
    auto operator()(R &&r, N n, FwdIt pivot, UnaryPredicate pred, P p) const
        -> std::pair<iter_of_range<R>, iter_of_range<R>>
    {
        // precondition r.begin() <= pivot <= r.end()
        return i::gather_n(adl::begin(r), n, pivot, astl::pass_fn(pred), astl::pass_fn(p));
    }

} gather_n{};

} // namespace r
} // namespace astl

#endif // ASTL_INCLUDE_GATHER_HPP
