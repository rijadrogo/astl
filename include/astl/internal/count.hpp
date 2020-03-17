//
// Created by Rijad on 29-Jul-18.
//

#ifndef ASTL_INCLUDE_COUNT_HPP
#define ASTL_INCLUDE_COUNT_HPP

#include <algorithm>
#include <type_traits>
#include <utility>

#include "astl/functional.hpp"
#include "astl/iterator.hpp"
#include "astl/range_access.hpp"

namespace astl
{
namespace internal_count
{
template <int N, typename FwdIt, typename NaryPred>
auto count_if_adjacent1(FwdIt first, NaryPred pred, iter_diff_type<FwdIt> d)
    -> iter_diff_type<FwdIt>
{
    iter_diff_type<FwdIt> sum1(0);
    iter_diff_type<FwdIt> sum2(0);
    iter_diff_type<FwdIt> sum3(0);
    iter_diff_type<FwdIt> sum4(0);
    std::make_integer_sequence<int, N> seq{};
    d = d - N + 1;
    constexpr iter_diff_type<FwdIt> loop_unroll_num(4);
    auto trip_count(d >> 2);
    while (trip_count > 0) {
        if (internal::for_each1(pred, first, seq)) ++sum1;

        ++first;
        if (internal::for_each1(pred, first, seq)) ++sum2;

        ++first;
        if (internal::for_each1(pred, first, seq)) ++sum3;

        ++first;
        if (internal::for_each1(pred, first, seq)) ++sum4;

        ++first;
        d -= loop_unroll_num;
        --trip_count;
    }

    switch (d) {
    case 3:
        if (internal::for_each1(pred, first, seq)) ++sum1;

        ++first;
    case 2:
        if (internal::for_each1(pred, first, seq)) ++sum2;

        ++first;
    case 1:
        if (internal::for_each1(pred, first, seq)) ++sum3;

    case 0:
    default: return sum1 + sum2 + sum3 + sum4;
    }
}
} // namespace internal_count

namespace i
{
using std::count; // NOLINT(misc-unused-using-decls)
template <typename FwdIt, typename T, typename P>
ASTL_NODISCARD auto count(FwdIt first, FwdIt last, T const &value, P p) -> iter_diff_type<FwdIt>
{
    auto proj(astl::pass_fn(p));
    return std::count(astl::map_iterator(first, proj), astl::map_iterator(last, proj), value);
}

using std::count_if; // NOLINT(misc-unused-using-decls)
template <typename FwdIt, typename UnaryPredicate, typename P>
ASTL_NODISCARD auto count_if(FwdIt first, FwdIt last, UnaryPredicate pred, P p) -> iter_diff_type<FwdIt>
{
    return std::count_if(first, last, astl::combine(astl::pass_fn(pred), astl::pass_fn(p)));
}

template <int N = 2, typename Iter, typename NaryPred, typename P>
ASTL_NODISCARD auto count_if_adjacent(Iter first, Iter last, NaryPred pred, P p) ->
    typename std::enable_if<(N > 0), astl::iter_diff_type<Iter>>::type
{
    return internal_count::count_if_adjacent1<N>(
        first, astl::combine(astl::pass_fn(pred), astl::pass_fn(p)), astl::distance(first, last));
}

template <int N = 2, typename Iter, typename NaryPred>
ASTL_NODISCARD auto count_if_adjacent(Iter first, Iter last, NaryPred pred) ->
    typename std::enable_if<(N > 0), astl::iter_diff_type<Iter>>::type
{
    return internal_count::count_if_adjacent1<N>(first, astl::pass_fn(pred),
                                                 astl::distance(first, last));
}

template <typename InIt, typename N, typename UnaryPredicate>
// requires InIt InputIterator
// requires N integral type
// requires UnaryPredicate, return bool, argument value_type(InIt)
ASTL_NODISCARD auto count_if_n(InIt first, N n, UnaryPredicate pred)
    -> std::pair<astl::iter_diff_type<InIt>, InIt>
{
    iter_diff_type<InIt> sum1(0);
    iter_diff_type<InIt> sum2(0);
    iter_diff_type<InIt> sum3(0);
    iter_diff_type<InIt> sum4(0);
    constexpr iter_diff_type<InIt> loop_unroll_num(4);
    auto trip_count(n >> 2);
    while (trip_count > 0) {
        if (pred(*first)) ++sum1;

        ++first;
        if (pred(*first)) ++sum2;

        ++first;
        if (pred(*first)) ++sum3;

        ++first;
        if (pred(*first)) ++sum4;

        ++first;
        n -= loop_unroll_num;
        --trip_count;
    }
    switch (integral_t<N>(n)) {
    case 3:
        if (pred(*first)) ++sum1;

        ++first;
    case 2:
        if (pred(*first)) ++sum2;

        ++first;
    case 1:
        if (pred(*first)) ++sum3;

        ++first;
    case 0:
    default: break;
    }

    return std::make_pair(sum1 + sum2 + sum3 + sum4, first);
}

template <typename InIt, typename N, typename UnaryPredicate, typename P>
ASTL_NODISCARD auto count_if_n(InIt first, N n, UnaryPredicate pred, P p)
    -> std::pair<astl::iter_diff_type<InIt>, InIt>
{
    return i::count_if_n(first, n, astl::combine(astl::pass_fn(pred), astl::pass_fn(p)));
}

template <typename InIt, typename N, typename T>
// requires InIt InputIterator
// requires N integral type
// requires T, equality comparable with value_type(InIt)
ASTL_NODISCARD auto count_n(InIt first, N n, T const &value)
    -> std::pair<astl::iter_diff_type<InIt>, InIt>
{
    iter_diff_type<InIt> sum1(0);
    iter_diff_type<InIt> sum2(0);
    iter_diff_type<InIt> sum3(0);
    iter_diff_type<InIt> sum4(0);
    constexpr iter_diff_type<InIt> loop_unroll_num(4);
    auto trip_count(n >> 2);
    while (trip_count > 0) {
        if (value == *first) ++sum1;

        ++first;
        if (value == *first) ++sum2;

        ++first;
        if (value == *first) ++sum3;

        ++first;
        if (value == *first) ++sum4;

        ++first;
        n -= loop_unroll_num;
        --trip_count;
    }
    switch (integral_t<N>(n)) {
    case 3:
        if (value == *first) ++sum1;

        ++first;
    case 2:
        if (value == *first) ++sum2;

        ++first;
    case 1:
        if (value == *first) ++sum3;

        ++first;
    case 0:
    default: break;
    }

    return std::make_pair(sum1 + sum2 + sum3 + sum4, first);
}

template <typename InIt, typename N, typename T, typename P>
ASTL_NODISCARD auto count_n(InIt first, N n, T const &value, P p) -> std::pair<N, InIt>
{
    return i::count_n(astl::map_iterator(first, astl::pass_fn(p)), n, value);
}
} // namespace i

namespace r
{
template <typename R, typename T>
ASTL_NODISCARD auto count(R &&r, T const &value) -> iter_diff_type<astl::iter_of_range<R>>
{
    return i::count(adl::begin(r), adl::end(r), value);
}

template <typename R, typename T, typename P>
ASTL_NODISCARD auto count(R &&r, T const &value, P p) -> iter_diff_type<astl::iter_of_range<R>>
{
    return i::count(adl::begin(r), adl::end(r), value, astl::pass_fn(p));
}

template <typename R, typename UnaryPredicate>
ASTL_NODISCARD auto count_if(R &&r, UnaryPredicate pred) -> iter_diff_type<astl::iter_of_range<R>>
{
    return i::count_if(adl::begin(r), adl::end(r), pred);
}

template <typename R, typename UnaryPredicate, typename P>
ASTL_NODISCARD auto count_if(R &&r, UnaryPredicate pred, P p) -> iter_diff_type<astl::iter_of_range<R>>
{
    return i::count_if(adl::begin(r), adl::end(r), pred, astl::pass_fn(p));
}

template <int N = 2, typename R, typename NaryPred, typename P>
ASTL_NODISCARD auto count_if_adjacent(R &&r, NaryPred pred, P p) ->
    typename std::enable_if<(N > 0), astl::iter_diff_type<astl::iter_of_range<R>>>::type
{
    return internal_count::count_if_adjacent1<N>(
        adl::begin(r), astl::combine(astl::pass_fn(pred), astl::pass_fn(p)),
        astl::size_or_distance(r));
}

template <int N = 2, typename R, typename NaryPred>
ASTL_NODISCARD auto count_if_adjacent(R &&r, NaryPred pred) ->
    typename std::enable_if<(N > 0), astl::iter_diff_type<astl::iter_of_range<R>>>::type
{
    return internal_count::count_if_adjacent1<N>(adl::begin(r), astl::pass_fn(pred),
                                                 astl::size_or_distance(r));
}

template <typename R, typename N, typename UnaryPredicate>
// requires R InputIterator range
// requires N integral type
// requires UnaryPredicate, return bool, argument value_type(R)
ASTL_NODISCARD auto count_if_n(R &&r, N n, UnaryPredicate pred)
    -> std::pair<astl::range_diff_type<R>, astl::iter_of_range<R>>
{
    return i::count_if_n(adl::begin(r), n, astl::pass_fn(pred));
}

template <typename R, typename N, typename UnaryPredicate, typename P>
ASTL_NODISCARD auto count_if_n(R &&r, N n, UnaryPredicate pred, P p)
    -> std::pair<astl::range_diff_type<R>, astl::iter_of_range<R>>
{
    return i::count_if_n(adl::begin(r), n, astl::pass_fn(pred), astl::pass_fn(p));
}

template <typename R, typename N, typename T>
// requires R InputIterator range
// requires N integral type
// requires T, equality comparable with value_type(R)
ASTL_NODISCARD auto count_n(R &&r, N n, T const &value)
    -> std::pair<astl::range_diff_type<R>, astl::iter_of_range<R>>
{
    return i::count_n(adl::begin(r), n, value);
}

template <typename R, typename N, typename T, typename P>
ASTL_NODISCARD auto count_n(R &&r, N n, T const &value, P p)
    -> std::pair<astl::range_diff_type<R>, astl::iter_of_range<R>>
{
    return i::count_n(adl::begin(r), n, value, astl::pass_fn(p));
}
} // namespace r
} // namespace astl

#endif // ASTL_INCLUDE_COUNT_HPP
