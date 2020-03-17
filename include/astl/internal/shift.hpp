//
// Created by Rijad on 30-Jul-18.
//

#ifndef ASTL_INCLUDE_SHIFT_HPP
#define ASTL_INCLUDE_SHIFT_HPP

#include <algorithm>

#include "astl/iterator.hpp"

namespace astl
{
namespace i
{
template <typename FwdIt>
// requires FwdIt ForwardIterator
auto shift_left(FwdIt first, FwdIt last, iter_diff_type<FwdIt> n) -> FwdIt
{
    if (n <= 0) return last;

    FwdIt mid(first);
    if (astl::advance(mid, n, last)) return first;

    return std::move(std::move(mid), std::move(last), std::move(first));
}

template <typename FwdIt, typename UnaryPredicate>
// requires FwdIt ForwardIterator
// requires UnaryPredicate, returns bool, argument of type value_type(FwdIt)
auto shift_left_if(FwdIt first, FwdIt last, iter_diff_type<FwdIt> n, UnaryPredicate pred) -> FwdIt
{
    return i::all_of_n(first, n, astl::pass_fn(pred)) ? i::shift_left(first, last, n) : first;
}

template <typename FwdIt>
// requires FwdIt ForwardIterator
auto shift_right(FwdIt first, FwdIt last, iter_diff_type<FwdIt> n) -> FwdIt
{
    if (n <= 0) return first;

    if constexpr (is_bidirectional_it_v<FwdIt>) {// Bidirectional Iterator
        auto mid(last);
        if (astl::advance(mid, -n, first)) return last;

        return std::move_backward(std::move(first), std::move(mid), std::move(last));
    }
    else {// Input Iterator
        auto result(first);
        if (astl::advance(result, n, last)) return last;
        // Invariant: next(first, n) == result
        // Invariant: next(trail, n) == lead
        auto lead(result);
        auto trail(first);

        while (trail != result) {
            if (lead == last) {
                // The range looks like:
                //
                //   |-- (n - k) elements --|-- k elements --|-- (n - k) elements --|
                //   ^-first          trail-^                ^-result          last-^
                //
                // Note that distance(first, trail) == distance(result, last)
                std::move(std::move(first), std::move(trail), result);
                return result;
            }
            ++lead;
            ++trail;
        }

        while (true) {
            auto mid(first);
            while (mid != result) {
                if (lead == last) {
                    // The range looks like:
                    //
                    //   |-- (n - k) elements --|-- k elements --|-- ... --|-- n elements
                    //   --|
                    //   ^-first            mid-^         result-^         ^-trail last-^
                    //
                    trail = std::move(mid, result, std::move(trail));
                    std::move(std::move(first), std::move(mid), std::move(trail));
                    return result;
                }
                std::iter_swap(mid, trail);
                ++lead;
                ++trail;
                ++mid;
            }
        }
    }
}

template <typename FwdIt, typename UnaryPredicate>
// requires FwdIt ForwardIterator
// requires UnaryPredicate, returns bool, argument of type value_type(FwdIt)
auto shift_right_if(FwdIt first, FwdIt last, iter_diff_type<FwdIt> n, UnaryPredicate pred) -> FwdIt
{
    return i::all_of_n(first, n, astl::pass_fn(pred)) ? i::shift_right(first, last, n) : first;
}

template <typename FwdIt>
// requires FwdIt ForwardIterator
auto shift(FwdIt first, FwdIt last, iter_diff_type<FwdIt> n) -> FwdIt
{
    return n < 0 ? i::shift_left(first, last, -n) : i::shift_right(first, last, n);
}

template <typename FwdIt, typename UnaryPredicate>
// requires FwdIt ForwardIterator
// requires UnaryPredicate, returns bool, argument of type value_type(FwdIt)
auto shift_if(FwdIt first, FwdIt last, iter_diff_type<FwdIt> n, UnaryPredicate pred) -> FwdIt
{
    return i::all_of_n(first, n, astl::pass_fn(pred)) ? i::shift(first, last, n) : first;
}
}// namespace i
}// namespace astl

#endif// ASTL_INCLUDE_SHIFT_HPP
