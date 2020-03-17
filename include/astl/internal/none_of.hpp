//
// Created by Rijad on 29-Jul-18.
//

#ifndef ASTL_INCLUDE_NONE_OF_HPP
#define ASTL_INCLUDE_NONE_OF_HPP

#include <algorithm>
#include <type_traits>

#include "find.hpp"

#include "astl/functional.hpp"
#include "astl/iterator.hpp"
#include "astl/range_access.hpp"

namespace astl
{
namespace internal_none_of
{
template <int N, typename I, typename NaryPred>
auto none_of_adjacent1(I first, NaryPred pred, iter_diff_type<I> d) -> bool
{
    std::make_integer_sequence<int, N> seq{};
    d = d - N + 1;
    constexpr iter_diff_type<I> loop_unroll_num(4);
    auto trip_count(d >> 2);
    while (trip_count > 0) {
        if (internal::for_each1(pred, first, seq)) return false;

        ++first;
        if (internal::for_each1(pred, first, seq)) return false;

        ++first;
        if (internal::for_each1(pred, first, seq)) return false;

        ++first;
        if (internal::for_each1(pred, first, seq)) return false;

        ++first;
        d -= loop_unroll_num;
        --trip_count;
    }
    switch (d) {
    case 3:
        if (internal::for_each1(pred, first, seq)) return false;

        ++first;
    case 2:
        if (internal::for_each1(pred, first, seq)) return false;

        ++first;
    case 1:
        if (internal::for_each1(pred, first, seq)) return false;

    case 0:
    default: return true;
    }
}
} // namespace internal_none_of

namespace i
{
using std::none_of; // NOLINT(misc-unused-using-decls)

template <typename InIt>
// requires InIt InputIterator
ASTL_NODISCARD auto none_of(InIt first, InIt last) -> bool
{
    return std::find(first, last, true) == last;
}

template <typename InIt, typename F, typename P>
ASTL_NODISCARD auto none_of(InIt first, InIt last, F f, P p) -> bool
{
    return std::none_of(first, last, astl::combine(astl::pass_fn(f), astl::pass_fn(p)));
}

template <int N, typename Iter, typename NaryFunction>
ASTL_NODISCARD auto none_of_adjacent(Iter first, Iter last, NaryFunction f) ->
    typename std::enable_if<(N > 0), bool>::type
{
    return internal_none_of::none_of_adjacent1<N>(first, astl::pass_fn(f),
                                                  astl::distance(first, last));
}

template <int N, typename Iter, typename NaryFunction, typename P>
ASTL_NODISCARD auto none_of_adjacent(Iter first, Iter last, NaryFunction f, P p) ->
    typename std::enable_if<(N > 0), bool>::type
{
    return internal_none_of::none_of_adjacent1<N>(
        first, astl::combine(astl::pass_fn(f), astl::pass_fn(p)), astl::distance(first, last));
}

template <typename InIt, typename E>
// requires InIt InputIterator
// requires E, equality comparable with value_type(InIt)
ASTL_NODISCARD auto none_of_equal(InIt first, InIt last, E const &elem) -> bool
{
    return std::find(first, last, elem) == last;
}

template <typename InIt, typename E, typename P>
ASTL_NODISCARD auto none_of_equal(InIt first, InIt last, E const &elem, P p) -> bool
{
    return i::find(first, last, elem, astl::pass_fn(p)) == last;
}

template <typename InIt, typename N, typename E>
// requires InIt InputIterator
// requires N integral type
// requires E, equality comparable with value_type(InIt)
ASTL_NODISCARD auto none_of_equal_n(InIt first, N n, E const &e) -> bool
{
    return i::find_n(first, n, e).second == n;
}

template <typename InIt, typename N, typename E, typename P>
ASTL_NODISCARD auto none_of_equal_n(InIt first, N n, E const &e, P p) -> bool
{
    return i::find_n(first, n, e, astl::pass_fn(p)).second == n;
}

template <typename InIt, typename N>
// requires InIt InputIterator
// requires N integral type
ASTL_NODISCARD auto none_of_n(InIt first, N n) -> bool
{
    return i::none_of_n(first, n, true);
}

template <typename InIt, typename N, typename UnaryPredicate>
// requires InIt InputIterator
// requires N integral type
// requires UnaryPredicate, returns bool, argument value_type(InIt)
ASTL_NODISCARD auto none_of_n(InIt first, N n, UnaryPredicate pred) -> bool
{
    return i::find_if_n(first, n, astl::pass_fn(pred)).second == n;
}

template <typename InIt, typename N, typename UnaryPredicate, typename P>
ASTL_NODISCARD auto none_of_n(InIt first, N n, UnaryPredicate pred, P p) -> bool
{
    return i::none_of_n(first, n, astl::combine(astl::pass_fn(pred), astl::pass_fn(p)));
}
} // namespace i

namespace r
{
template <typename R>
// requires R InputIterator range
ASTL_NODISCARD auto none_of(R &&r) -> bool
{
    return i::none_of(adl::begin(r), adl::end(r));
}

template <typename R, typename UnaryPredicate>
// requires R InputIterator range
// requires UnaryPredicate, returns bool, argument value_type(R)
ASTL_NODISCARD auto none_of(R &&r, UnaryPredicate pred) -> bool
{
    return i::none_of(adl::begin(r), adl::end(r), astl::pass_fn(pred));
}

template <typename R, typename F, typename P> ASTL_NODISCARD auto none_of(R &&r, F f, P p) -> bool
{
    return i::none_of(adl::begin(r), adl::end(r), astl::pass_fn(f), astl::pass_fn(p));
}

template <int N, typename R, typename NaryFunction>
ASTL_NODISCARD auto none_of_adjacent(R &&r, NaryFunction f) ->
    typename std::enable_if<(N > 0), bool>::type
{
    return internal_none_of::none_of_adjacent1<N>(adl::begin(r), astl::pass_fn(f),
                                                  astl::size_or_distance(r));
}

template <int N, typename R, typename NaryFunction, typename P>
ASTL_NODISCARD auto none_of_adjacent(R &&r, NaryFunction f, P p) ->
    typename std::enable_if<(N > 0), bool>::type
{
    return internal_none_of::none_of_adjacent1<N>(adl::begin(r),
                                                  astl::combine(astl::pass_fn(f), astl::pass_fn(p)),
                                                  astl::size_or_distance(r));
}

template <typename R, typename E>
// requires R InputIterator range
// requires E, equality comparable with value_type(InIt)
ASTL_NODISCARD auto none_of_equal(R &&r, E const &elem) -> bool
{
    return i::none_of_equal(adl::begin(r), adl::end(r), elem);
}

template <typename R, typename E, typename P>
ASTL_NODISCARD auto none_of_equal(R &&r, E const &elem, P p) -> bool
{
    return i::none_of_equal(adl::begin(r), adl::end(r), elem, astl::pass_fn(p));
}

template <typename R, typename N, typename E>
// requires R InputIterator range
// requires N integral type
// requires E, equality comparable with value_type(InIt)
ASTL_NODISCARD auto none_of_equal_n(R &&r, N n, E const &e) -> bool
{
    return i::none_of_equal_n(adl::begin(r), n, e);
}

template <typename R, typename N, typename E, typename P>
ASTL_NODISCARD auto none_of_equal_n(R &&r, N n, E const &e, P p) -> bool
{
    return i::none_of_equal_n(adl::begin(r), n, e, astl::pass_fn(p));
}

template <typename R, typename N>
// requires R InputIterator range
// requires N integral type
ASTL_NODISCARD auto none_of_n(R &&r, N n) -> bool
{
    return i::none_of_n(adl::begin(r), n);
}

template <typename R, typename N, typename UnaryPredicate>
// requires R InputIterator range
// requires N integral type
// requires UnaryPredicate, returns bool, argument value_type(R)
ASTL_NODISCARD auto none_of_n(R &&r, N n, UnaryPredicate pred) -> bool
{
    return i::none_of_n(adl::begin(r), n, astl::pass_fn(pred));
}

template <typename R, typename N, typename UnaryPredicate, typename P>
ASTL_NODISCARD auto none_of_n(R &&r, N n, UnaryPredicate pred, P p) -> bool
{
    return i::none_of_n(adl::begin(r), n, astl::pass_fn(pred), astl::pass_fn(p));
}
} // namespace r
} // namespace astl

#endif // ASTL_INCLUDE_NONE_OF_HPP
