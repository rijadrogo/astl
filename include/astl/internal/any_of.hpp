//
// Created by Rijad on 29-Jul-18.
//

#ifndef ASTL_INCLUDE_ANY_OF_HPP
#define ASTL_INCLUDE_ANY_OF_HPP

#include <algorithm>
#include <type_traits>

#include "count.hpp"
#include "find.hpp"
#include "none_of.hpp"

#include "astl/functional.hpp"
#include "astl/range_access.hpp"

namespace astl
{
namespace i
{
using std::any_of; // NOLINT(misc-unused-using-decls)
template <typename InIt>
// requires InIt InputIterator
ASTL_NODISCARD auto any_of(InIt first, InIt last) -> bool
{
    return std::find(first, last, true) != last;
}

template <typename InIt, typename UnaryPredicate, typename P>
ASTL_NODISCARD auto any_of(InIt first, InIt last, UnaryPredicate pred, P p) -> bool
{
    return std::any_of(first, last, astl::combine(astl::pass_fn(pred), astl::pass_fn(p)));
}

template <int N, typename FwdIt, typename NaryPred>
ASTL_NODISCARD auto any_of_adjacent(FwdIt first, FwdIt last, NaryPred pred) ->
    typename std::enable_if<(N > 0), bool>::type
{
    return !i::none_of_adjacent<N>(first, last, astl::pass_fn(pred));
}

template <int N, typename FwdIt, typename NaryPred, typename P>
ASTL_NODISCARD auto any_of_adjacent(FwdIt first, FwdIt last, NaryPred pred, P p) ->
    typename std::enable_if<(N > 0), bool>::type
{
    return !i::none_of_adjacent<N>(first, last,
                                   astl::combine(astl::pass_fn(pred), astl::pass_fn(p)));
}

template <typename InIt, typename E>
// requires InIt InputIterator
// requires E quality comparable with value_type(InIt)
ASTL_NODISCARD auto any_of_equal(InIt first, InIt last, E const &e) -> bool
{
    return std::find(first, last, e) != last;
}

template <typename InIt, typename E, typename P>
ASTL_NODISCARD auto any_of_equal(InIt first, InIt last, E const &e, P p) -> bool
{
    return i::find(first, last, e, astl::pass_fn(p)) != last;
}

template <typename InIt, typename N, typename E>
// requires InIt InputIterator
// requires N integral type
// requires E quality comparable with value_type(InIt)
ASTL_NODISCARD auto any_of_equal_n(InIt first, N n, E const &e) -> bool
{
    return !i::none_of_equal_n(first, n, e);
}

template <typename InIt, typename N, typename E, typename P>
ASTL_NODISCARD auto any_of_equal_n(InIt first, N n, E const &e, P p) -> bool
{
    return !i::none_of_equal_n(first, n, e, astl::pass_fn(p));
}

template <typename InIt, typename N>
// requires InIt InputIterator
// requires N integral type
ASTL_NODISCARD auto any_of_n(InIt first, N n) -> bool
{
    return i::any_of_equal_n(first, n, true);
}

template <typename InIt, typename N, typename UnaryPredicate>
// requires InIt InputIterator
// requires N integral type
// requires UnaryPredicate, returns bool, argument value_type(InIt)
ASTL_NODISCARD auto any_of_n(InIt first, N n, UnaryPredicate pred) -> bool
{
    return !i::none_of_n(first, n, astl::pass_fn(pred));
}

template <typename InIt, typename N, typename UnaryPredicate, typename P>
ASTL_NODISCARD auto any_of_n(InIt first, N n, UnaryPredicate pred, P p) -> bool
{
    return i::any_of_n(first, n, astl::combine(astl::pass_fn(pred), astl::pass_fn(p)));
}

template <typename InIt, typename UnaryPredicate>
// requires InIt InputIterator
// requires UnaryPredicate, returns bool argument value_type(InIt)
ASTL_NODISCARD auto has_n_items(InIt first, InIt last, iter_diff_type<InIt> n, UnaryPredicate pred)
    -> bool
{
    if (n < 0 || first == last) return false;
    if constexpr (is_random_access_it_v<InIt>) {
        if (last - first < n) return false;
    }
    return i::count_if(first, last, astl::pass_fn(pred)) == n;
}

template <typename InIt, typename UnaryPredicate, typename P>
ASTL_NODISCARD auto has_n_items(InIt first, InIt last, iter_diff_type<InIt> n, UnaryPredicate pred,
                                P p) -> bool
{
    return i::has_n_items(first, last, n, astl::combine(astl::pass_fn(pred), astl::pass_fn(p)));
}

template <typename InIt, typename UnaryPredicate>
// requires InIt InputIterator
// requires UnaryPredicate, returns bool argument value_type(InIt)
ASTL_NODISCARD auto has_n_items_or_more(InIt first, InIt last, iter_diff_type<InIt> n,
                                        UnaryPredicate pred) -> bool
{
    if (n < 0 || first == last) return false;
    if constexpr (is_random_access_it_v<InIt>) {
        if (last - first < n) return false;
    }
    return i::count_if(first, last, astl::pass_fn(pred)) >= n;
}

template <typename InIt, typename UnaryPredicate, typename P>
ASTL_NODISCARD auto has_n_items_or_more(InIt first, InIt last, iter_diff_type<InIt> n,
                                        UnaryPredicate pred, P p) -> bool
{
    return i::has_n_items_or_more(first, last, n,
                                  astl::combine(astl::pass_fn(pred), astl::pass_fn(p)));
}

/// \fn one_of ( InputIterator first, InputIterator last, Predicate p )
/// \return true if the predicate 'p' is true for exactly one item in [first,
/// last).
///
/// \param first The start of the input sequence
/// \param last  One past the end of the input sequence
/// \param pred     A predicate for testing the elements of the sequence
///
template <typename InIt, typename UnaryPredicate>
// requires InIt InputIterator
// requires UnaryPredicate, returns bool, argument value_type(InIt)
ASTL_NODISCARD auto one_of(InIt first, InIt last, UnaryPredicate pred) -> bool
{
    return i::find_if_unique(first, last, astl::pass_fn(pred)) != last;
}

template <typename InIt, typename UnaryPredicate, typename P>
ASTL_NODISCARD auto one_of(InIt first, InIt last, UnaryPredicate pred, P p) -> bool
{
    return i::one_of(first, last, astl::combine(astl::pass_fn(pred), astl::pass_fn(p)));
}

template <typename InIt, typename N, typename UnaryPredicate>
// requires InIt InputIterator
// requires N integral type
// requires UnaryPredicate, returns bool, argument value_type(InIt)
ASTL_NODISCARD auto one_of_n(InIt first, N n, UnaryPredicate pred) -> bool
{
    return i::find_if_unique_n(first, n, astl::pass_fn(pred)).second != n;
}

template <typename InIt, typename N, typename UnaryPredicate, typename P>
ASTL_NODISCARD auto one_of_n(InIt first, N n, UnaryPredicate pred, P p) -> bool
{
    return i::one_of_n(first, n, astl::combine(astl::pass_fn(pred), astl::pass_fn(p)));
}
} // namespace i

namespace r
{
template <typename R>
// requires R InputIterator range
ASTL_NODISCARD auto any_of(R &&r) -> bool
{
    return i::any_of(adl::begin(r), adl::end(r));
}

template <typename R, typename UnaryPredicate>
ASTL_NODISCARD auto any_of(R &&r, UnaryPredicate pred) -> bool
{
    return i::any_of(adl::begin(r), adl::end(r), astl::pass_fn(pred));
}

template <typename R, typename UnaryPredicate, typename P>
ASTL_NODISCARD auto any_of(R &&r, UnaryPredicate pred, P p) -> bool
{
    return i::any_of(adl::begin(r), adl::end(r), astl::pass_fn(pred), astl::pass_fn(p));
}

template <int N, typename R, typename NaryPred>
ASTL_NODISCARD auto any_of_adjacent(R &&r, NaryPred pred) ->
    typename std::enable_if<(N > 0), bool>::type
{
    return !r::none_of_adjacent<N>(r, astl::pass_fn(pred));
}

template <int N, typename R, typename NaryPred, typename P>
ASTL_NODISCARD auto any_of_adjacent(R &&r, NaryPred pred, P p) ->
    typename std::enable_if<(N > 0), bool>::type
{
    return !r::none_of_adjacent<N>(r, astl::pass_fn(pred), astl::pass_fn(p));
}

template <typename R, typename E>
// requires R InputIterator range
// requires E quality comparable with value_type(InIt)
ASTL_NODISCARD auto any_of_equal(R &&r, E const &e) -> bool
{
    return i::any_of_equal(adl::begin(r), adl::end(r), e);
}

template <typename R, typename E, typename P>
ASTL_NODISCARD auto any_of_equal(R &&r, E const &e, P p) -> bool
{
    return i::any_of_equal(adl::begin(r), adl::end(r), e, astl::pass_fn(p));
}

template <typename R, typename N, typename E>
// requires R InputIterator range
// requires N integral type
// requires E quality comparable with value_type(InIt)
ASTL_NODISCARD auto any_of_equal_n(R &&r, N n, E const &e) -> bool
{
    return i::any_of_equal_n(adl::begin(r), n, e);
}

template <typename R, typename N, typename E, typename P>
ASTL_NODISCARD auto any_of_equal_n(R &&r, N n, E const &e, P p) -> bool
{
    return i::any_of_equal_n(adl::begin(r), n, e, astl::pass_fn(p));
}

template <typename R, typename N>
// requires R InputIterator range
// requires N integral type
ASTL_NODISCARD auto any_of_n(R &&r, N n) -> bool
{
    return i::any_of_n(adl::begin(r), n);
}

template <typename R, typename N, typename UnaryPredicate>
// requires R InputIterator range
// requires N integral type
// requires UnaryPredicate, returns bool, argument value_type(InIt)
ASTL_NODISCARD auto any_of_n(R &&r, N n, UnaryPredicate pred) -> bool
{
    return i::any_of_n(adl::begin(r), n, astl::pass_fn(pred));
}

template <typename R, typename N, typename UnaryPredicate, typename P>
ASTL_NODISCARD auto any_of_n(R &&r, N n, UnaryPredicate pred, P p) -> bool
{
    return i::any_of_n(adl::begin(r), n, astl::pass_fn(pred), astl::pass_fn(p));
}

template <typename R, typename UnaryPredicate>
// requires R InputIterator range
// requires UnaryPredicate, returns bool argument value_type(R)
ASTL_NODISCARD auto has_n_items(R r, range_diff_type<R> n, UnaryPredicate pred) -> bool
{
    return i::has_n_items(adl::begin(r), adl::end(r), astl::pass_fn(pred));
}

template <typename R, typename UnaryPredicate, typename P>
ASTL_NODISCARD auto has_n_items(R r, range_diff_type<R> n, UnaryPredicate pred, P p) -> bool
{
    return i::has_n_items(adl::begin(r), adl::end(r), n,
                          astl::combine(astl::pass_fn(pred), astl::pass_fn(p)));
}

template <typename R, typename UnaryPredicate>
// requires R InputIterator range
// requires UnaryPredicate, returns bool argument value_type(R)
ASTL_NODISCARD auto has_n_items_or_more(R r, range_diff_type<R> n, UnaryPredicate pred) -> bool
{
    return i::has_n_items(adl::begin(r), adl::end(r), astl::pass_fn(pred));
}

template <typename R, typename UnaryPredicate, typename P>
ASTL_NODISCARD auto has_n_items_or_more(R r, range_diff_type<R> n, UnaryPredicate pred, P p) -> bool
{
    return i::has_n_items(adl::begin(r), adl::end(r), n,
                          astl::combine(astl::pass_fn(pred), astl::pass_fn(p)));
}

template <typename R, typename UnaryPredicate>
// requires InIt InputIterator
// requires UnaryPredicate, returns bool, argument value_type(InIt)
ASTL_NODISCARD auto one_of(R &&r, UnaryPredicate pred) -> bool
{
    return i::one_of(adl::begin(r), adl::end(r), astl::pass_fn(pred));
}

template <typename R, typename UnaryPredicate, typename P>
ASTL_NODISCARD auto one_of(R &&r, UnaryPredicate pred, P p) -> bool
{
    return i::one_of(adl::begin(r), adl::end(r), astl::pass_fn(pred), astl::pass_fn(p));
}

template <typename R, typename N, typename UnaryPredicate>
// requires InIt InputIterator
// requires N integral type
// requires UnaryPredicate, returns bool, argument value_type(InIt)
ASTL_NODISCARD auto one_of_n(R &&r, N n, UnaryPredicate pred) -> bool
{
    return i::one_of_n(adl::begin(r), n, astl::pass_fn(pred));
}

template <typename R, typename N, typename UnaryPredicate, typename P>
ASTL_NODISCARD auto one_of_n(R &&r, N n, UnaryPredicate pred, P p) -> bool
{
    return i::one_of_n(adl::begin(r), n, astl::pass_fn(pred), astl::pass_fn(p));
}
} // namespace r
} // namespace astl

#endif // ASTL_INCLUDE_ANY_OF_HPP
