//
// Created by Rijad on 29-Jul-18.
//

#ifndef ASTL_INCLUDE_MINMAX_ELEMENT_HPP
#define ASTL_INCLUDE_MINMAX_ELEMENT_HPP

#include <algorithm>
#include <utility>

#include "astl/functional.hpp"
#include "astl/iterator.hpp"
#include "astl/optional.hpp"

namespace astl
{
namespace i
{
template <typename T>
ASTL_NODISCARD auto in_range_inclusive(T const &val, T const &lo, T const &hi) noexcept -> bool
{
    return (val <= hi && lo <= val);
}

template <typename T>
ASTL_NODISCARD auto in_range_exlusive(T const &val, T const &lo, T const &hi) noexcept -> bool
{
    return (val < hi && lo < val);
}

/*
 * Boyer–Moore majority vote algorithm
 * https://en.wikipedia.org/wiki/Boyer%E2%80%93Moore_majority_vote_algorithm
 * majority means: an element that occurs repeatedly for more than half of the
 * elements of the input
 */
template <typename InIt, typename BinaryPredicate>
// requires InIt InputIterator
// requires BinaryPredicate, returns bool, two arguments of value_type(InIt)
ASTL_NODISCARD auto majority_element(InIt first, InIt last, BinaryPredicate pred)
    -> optional<astl::iter_value_type<InIt>>
{
    if (first == last) return nullopt;

    if constexpr (is_forward_it_v<InIt>)// Forward Iterator
    {
        using FwdIt = InIt;
        int weight(0);
        FwdIt winner(first);
        ++first;
        while (first != last) {
            if (weight == 0) {
                winner = first;
                ++weight;
            }
            else {
                pred(*winner, *first) ? ++weight : --weight;
            }
            ++first;
        }
        return astl::make_optional(*winner);
    }
    else {// Input Iterator
        int weight(0);
        auto winner(*first);
        ++first;
        while (first != last) {
            if (weight == 0) {
                winner = *first;
                ++weight;
            }
            else {
                pred(winner, *first) ? ++weight : --weight;
            }
            ++first;
        }
        return astl::make_optional(std::move(winner));
    }
}

/*
 * Boyer–Moore majority vote algorithm
 * https://en.wikipedia.org/wiki/Boyer%E2%80%93Moore_majority_vote_algorithm
 * majority means: an element that occurs repeatedly for more than half of the
 * elements of the input
 */
template <typename FwdIt>
// requires FwdIt ForwardIterator
ASTL_NODISCARD auto majority_element(FwdIt first, FwdIt last)
    -> optional<astl::iter_value_type<FwdIt>>
{
    return i::majority_element(first, last, std::equal_to{});
}

/*
 * Boyer–Moore majority vote algorithm
 * https://en.wikipedia.org/wiki/Boyer%E2%80%93Moore_majority_vote_algorithm
 * majority means: an element that occurs repeatedly for more than half of the
 * elements of the input
 */
template <typename FwdIt, typename BinaryPredicate, typename P>
ASTL_NODISCARD auto majority_element(FwdIt first, FwdIt last, BinaryPredicate pred, P p)
    -> optional<astl::iter_value_type<FwdIt>>
{
    return i::majority_element(first, last, astl::combine(astl::pass_fn(pred), astl::pass_fn(p)));
}

using std::minmax_element;
template <typename FwdIt, typename Comparator, typename P>
ASTL_NODISCARD auto minmax_element(FwdIt first, FwdIt last, Comparator comp, P p)
    -> std::pair<FwdIt, FwdIt>
{
    return std::minmax_element(first, last, astl::combine(astl::pass_fn(comp), astl::pass_fn(p)));
}

template <typename FwdIt, typename N, typename Comparator>
ASTL_NODISCARD auto minmax_element_n(FwdIt first, N n, Comparator comp) -> std::pair<FwdIt, FwdIt>
{
    using std::swap;
    std::pair<FwdIt, FwdIt> result(first, first);
    N zero(0);
    if (n == zero || --n == zero) return result;

    ++first;
    ++first;
    --n;
    result.second = first;
    if (comp(*result.second, *result.first)) swap(result.first, result.second);

    N prev(n - N(1));
    while (n != zero && prev != zero) {
        FwdIt potential_min(first);
        FwdIt potential_max(astl::next(first));
        if (comp(*potential_max, *potential_min)) swap(potential_max, potential_min);

        if (!comp(*potential_max, *result.second)) result.second = potential_max;

        if (comp(*potential_min, *result.first)) result.first = potential_min;

        ++first;
        ++first;
        --n;
        --n;
        prev = n - N(1);
    }
    if (n != zero) {
        if (!comp(*first, *result.second)) result.second = first;

        if (comp(*first, *result.first)) result.first = first;
    }
    return result;
}

template <typename FwdIt, typename N>
ASTL_NODISCARD auto minmax_element_n(FwdIt first, N n) -> std::pair<FwdIt, FwdIt>
{
    return i::minmax_element_n(first, n, std::less{});
}

template <typename FwdIt, typename N, typename Comparator, typename P>
ASTL_NODISCARD auto minmax_element_n(FwdIt first, N n, Comparator comp, P p) -> std::pair<FwdIt, FwdIt>
{
    return i::minmax_element_n(first, n, astl::combine(astl::pass_fn(comp), astl::pass_fn(p)));
}

template <typename FwdIt, typename Comparator>
// requires FwdIt ForwardIterator
// requires Comparator StrictWeakOrdering on value_type(FwdIt)
ASTL_NODISCARD auto select_1_2(FwdIt first, FwdIt last, Comparator comp) -> std::pair<FwdIt, FwdIt>
{
    if (first == last || astl::next(first) == last) return std::make_pair(first, last);

    FwdIt first_place(first);
    ++first;
    FwdIt second_place(first);
    ++first;

    if (comp(*second_place, *first_place)) {
        using std::swap;
        swap(first_place, second_place);
    }

    while (first != last) {
        if (comp(*first, *second_place)) {
            if (comp(*first, *first_place)) second_place = std::exchange(first_place, first);
            else
                second_place = first;
        }
        ++first;
    }
    return std::make_pair(first_place, second_place);
}

template <typename FwdIt>
// requires FwdIt ForwardIterator
ASTL_NODISCARD auto select_1_2(FwdIt first, FwdIt last) -> std::pair<FwdIt, FwdIt>
{
    return i::select_1_2(first, last, std::less{});
}

template <typename FwdIt, typename Comparator, typename P>
ASTL_NODISCARD auto select_1_2(FwdIt first, FwdIt last, Comparator comp, P p) -> std::pair<FwdIt, FwdIt>
{
    return i::select_1_2(first, last, astl::combine(astl::pass_fn(comp), astl::pass_fn(p)));
}

template <typename FwdIt, typename N, typename Comparator>
// requires FwdIt ForwardIterator
// requires N integral type
// requires Comparator StrictWeakOrdering on value_type(FwdIt)
ASTL_NODISCARD auto select_1_2_n(FwdIt first, N n, Comparator comp) -> std::pair<FwdIt, FwdIt>
{
    if (n == N(0)) return std::make_pair(first, first);
    if (n == N(1)) return std::make_pair(first, astl::next(first));

    FwdIt first_place(first);
    ++first;
    --n;
    FwdIt second_place(first);
    ++first;
    --n;

    if (comp(*second_place, *first_place)) {
        using std::swap;
        swap(first_place, second_place);
    }

    while (n != N(0)) {
        if (comp(*first, *second_place)) {
            if (comp(*first, *first_place)) second_place = std::exchange(first_place, first);
            else
                second_place = first;
        }
        ++first;
        --n;
    }
    return std::make_pair(first_place, second_place);
}

template <typename FwdIt, typename N>
// requires FwdIt ForwardIterator
// requires N integral type
ASTL_NODISCARD auto select_1_2_n(FwdIt first, N n) -> std::pair<FwdIt, FwdIt>
{
    return i::select_1_2_n(first, n, std::less{});
}

template <typename FwdIt, typename N, typename Comparator, typename P>
ASTL_NODISCARD auto select_1_2_n(FwdIt first, N n, Comparator comp, P p) -> std::pair<FwdIt, FwdIt>
{
    return i::select_1_2_n(first, n, astl::combine(astl::pass_fn(comp), astl::pass_fn(p)));
}

}// namespace i

namespace r
{
/*
 * Boyer–Moore majority vote algorithm
 * https://en.wikipedia.org/wiki/Boyer%E2%80%93Moore_majority_vote_algorithm
 * majority means: an element that occurs repeatedly for more than half of the
 * elements of the input
 */
template <typename R>
ASTL_NODISCARD auto majority_element(R &&r) -> optional<astl::range_value_type<R>>
{
    return i::majority_element(adl::begin(r), adl::end(r));
}

/*
 * Boyer–Moore majority vote algorithm
 * https://en.wikipedia.org/wiki/Boyer%E2%80%93Moore_majority_vote_algorithm
 * majority means: an element that occurs repeatedly for more than half of the
 * elements of the input
 */
template <typename R, typename BinaryPredicate>
ASTL_NODISCARD auto majority_element(R &&r, BinaryPredicate pred) -> optional<astl::range_value_type<R>>
{
    return i::majority_element(adl::begin(r), adl::end(r), astl::pass_fn(pred));
}

/*
 * Boyer–Moore majority vote algorithm
 * https://en.wikipedia.org/wiki/Boyer%E2%80%93Moore_majority_vote_algorithm
 * majority means: an element that occurs repeatedly for more than half of the
 * elements of the input
 */
template <typename R, typename BinaryPredicate, typename P>
ASTL_NODISCARD auto majority_element(R &&r, BinaryPredicate pred, P p)
    -> optional<astl::range_value_type<R>>
{
    return i::majority_element(adl::begin(r), adl::end(r), astl::pass_fn(pred), astl::pass_fn(p));
}

template <typename R>
ASTL_NODISCARD auto minmax_element(R &&r)
    -> std::pair<astl::iter_of_range<R>, astl::iter_of_range<R>>
{
    return i::minmax_element(adl::begin(r), adl::end(r));
}

template <typename R, typename Comparator>
ASTL_NODISCARD auto minmax_element(R &&r, Comparator comp)
    -> std::pair<astl::iter_of_range<R>, astl::iter_of_range<R>>
{
    return i::minmax_element(adl::begin(r), adl::end(r), astl::pass_fn(comp));
}

template <typename R, typename Comparator, typename P>
ASTL_NODISCARD auto minmax_element(R &&r, Comparator comp, P p)
    -> std::pair<astl::iter_of_range<R>, astl::iter_of_range<R>>
{
    return i::minmax_element(adl::begin(r), adl::end(r), astl::pass_fn(comp), astl::pass_fn(p));
}

template <typename R, typename N, typename Comparator>
ASTL_NODISCARD auto minmax_element_n(R &&r, N n, Comparator comp)
    -> std::pair<astl::iter_of_range<R>, astl::iter_of_range<R>>
{
    return i::minmax_element_n(adl::begin(r), n, astl::pass_fn(comp));
}

template <typename R, typename N>
ASTL_NODISCARD auto minmax_element_n(R &&r, N n)
    -> std::pair<astl::iter_of_range<R>, astl::iter_of_range<R>>
{
    return r::minmax_element_n(r, n, std::less{});
}

template <typename R, typename N, typename Comparator, typename P>
ASTL_NODISCARD auto minmax_element_n(R &&r, N n, Comparator comp, P p)
    -> std::pair<astl::iter_of_range<R>, astl::iter_of_range<R>>
{
    return i::minmax_element_n(adl::begin(r), n, astl::pass_fn(comp), astl::pass_fn(p));
}

template <typename R>
ASTL_NODISCARD auto select_1_2(R &&r) -> std::pair<astl::iter_of_range<R>, astl::iter_of_range<R>>
{
    return i::select_1_2(adl::begin(r), adl::end(r));
}

template <typename R, typename Comparator>
ASTL_NODISCARD auto select_1_2(R &&r, Comparator comp)
    -> std::pair<astl::iter_of_range<R>, astl::iter_of_range<R>>
{
    return i::select_1_2(adl::begin(r), adl::end(r), astl::pass_fn(comp));
}

template <typename R, typename Comparator, typename P>
ASTL_NODISCARD auto select_1_2(R &&r, Comparator comp, P p)
    -> std::pair<astl::iter_of_range<R>, astl::iter_of_range<R>>
{
    return i::select_1_2(adl::begin(r), adl::end(r), astl::pass_fn(comp), astl::pass_fn(p));
}
//
template <typename R, typename N>
ASTL_NODISCARD auto select_1_2_n(R &&r, N n)
    -> std::pair<astl::iter_of_range<R>, astl::iter_of_range<R>>
{
    return i::select_1_2_n(adl::begin(r), n);
}

template <typename R, typename N, typename Comparator>
ASTL_NODISCARD auto select_1_2_n(R &&r, N n, Comparator comp)
    -> std::pair<astl::iter_of_range<R>, astl::iter_of_range<R>>
{
    return i::select_1_2_n(adl::begin(r), n, astl::pass_fn(comp));
}

template <typename R, typename N, typename Comparator, typename P>
ASTL_NODISCARD auto select_1_2_n(R &&r, N n, Comparator comp, P p)
    -> std::pair<astl::iter_of_range<R>, astl::iter_of_range<R>>
{
    return i::select_1_2_n(adl::begin(r), n, astl::pass_fn(comp), astl::pass_fn(p));
}

}// namespace r
}// namespace astl

#endif// ASTL_INCLUDE_MINMAX_ELEMENT_HPP
