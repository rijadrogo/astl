//
// Created by Rijad on 30-Jul-18.
//

#ifndef ASTL_INCLUDE_MOVE_HPP
#define ASTL_INCLUDE_MOVE_HPP

#include <algorithm>
#include <type_traits>
#include <utility>

#include "astl/functional.hpp"
#include "astl/iterator.hpp"
#include "astl/range_access.hpp"

namespace astl
{
namespace i
{
using std::move;          // NOLINT(misc-unused-using-decls)
using std::move_backward; // NOLINT(misc-unused-using-decls)

template <typename InIt, typename OutIt>
// requires InIt InputIterator
// requires OutIt OutputIterator
auto move_bounded(InIt first, InIt last, OutIt result_first, OutIt result_last)
    -> std::pair<OutIt, InIt>
{
    auto p(i::copy_bounded(std::make_move_iterator(first), std::make_move_iterator(last),
                           result_first, result_last));
    return std::make_pair(p.first, p.second.base());
}

template <typename InIt, typename N1, typename OutIt, typename N2>
// requires InIt InputIterator
// requires N1 integral type
// requires OutIt OutputIterator
// requires N2 integral type
auto move_bounded_n(InIt first, N1 n1, OutIt result_first, N2 n2) -> std::pair<OutIt, InIt>
{
    auto p(i::copy_bounded_n(std::make_move_iterator(first), n1, result_first, n2));
    return std::make_pair(p.first, p.second.base());
}

template <typename InIt, typename OutIt, typename UnaryPredicate>
// requires InIt InputIterator
// requires OutIt OutputIterator
// requires UnaryPredicate, returns bool, argument type value_type(InIt)
auto move_if(InIt first, InIt last, OutIt d_first, UnaryPredicate pred) -> OutIt
{
    auto lam([p(astl::pass_fn(pred))](auto &&x) { return p(x); });
    return std::copy_if(std::make_move_iterator(first), std::make_move_iterator(last), d_first,
                        lam);
}

template <typename InIt, typename OutIt, typename UnaryPredicateicate, typename P>
auto move_if(InIt first, InIt last, OutIt d_first, UnaryPredicateicate pred, P p) -> OutIt
{
    return i::move_if(first, last, d_first, astl::combine(astl::pass_fn(pred), astl::pass_fn(p)));
}

template <typename InIt, typename N, typename OutIt, typename UnaryPredicate>
// requires InIt InputIterator
// requires N integral type
// requires OutIt OutputIterator
// requires UnaryPredicate, returns bool, argument type value_type(InIt)
auto move_if_n(InIt first, N n, OutIt d_first, UnaryPredicate pred) -> std::pair<OutIt, InIt>
{
    auto lam([p(astl::pass_fn(pred))](auto &&x) { return p(x); });
    return i::copy_if_n(first, n, d_first, lam);
}

template <typename InIt, typename OutIt, typename UnaryPredicate, typename P>
auto move_if_n(InIt first, InIt last, OutIt d_first, UnaryPredicate pred, P p) -> std::pair<OutIt, InIt>
{
    return i::move_if_n(first, last, d_first, astl::combine(astl::pass_fn(pred), astl::pass_fn(p)));
}

template <typename InIt, typename N, typename OutIt>
// requires InIt InputIterator
// requires N integral type
// requires OutIt OutputIterator
auto move_n(InIt first, N count, OutIt dest) -> std::pair<OutIt, InIt>
{
    auto p(i::copy_n(std::make_move_iterator(first), count, dest));
    return std::make_pair(p.first, p.second.base());
}

template <typename InIt, typename OutIt, typename T>
// requires InIt InputIterator
// requires OutIt OutputIterator
// requires T equality comparable with value_type(InIt)
auto move_until_sentinel(InIt first, InIt const last, OutIt dest, T &&val) -> std::pair<OutIt, InIt>
{
    while (first != last && *first == val) {
        *dest = std::move(*first);
        ++first;
        ++dest;
    }
    return std::make_pair(dest, first);
}

template <typename InIt, typename OutIt, typename T, typename P>
auto move_until_sentinel(InIt first, InIt const last, OutIt dest, T &&val, P p)
    -> std::pair<OutIt, InIt>
{
    while (first != last && invoke(p, *first) == val) {
        *dest = std::move(*first);
        ++first;
        ++dest;
    }
    return std::make_pair(dest, first);
}

template <typename InIt, typename N, typename OutIt, typename T>
// requires InIt InputIterator
// requires N integral type
// requires OutIt OutputIterator
// requires T equality comparable with value_type(InIt)
auto move_until_sentinel_n(InIt first, N n, OutIt dest, T &&val) -> std::pair<OutIt, InIt>
{
    while (n != N(0) && *first == val) {
        *dest = std::move(*first);
        ++first;
        ++dest;
        --n;
    }
    return std::make_pair(dest, first);
}

template <typename InIt, typename N, typename OutIt, typename T, typename P>
auto move_until_sentinel_n(InIt first, N n, OutIt dest, T &&val, P p) -> std::pair<OutIt, InIt>
{
    while (n != N(0) && invoke(p, *first) == val) {
        *dest = std::move(*first);
        ++first;
        ++dest;
        --n;
    }
    return std::make_pair(dest, first);
}

template <typename InIt, typename OutIt, typename UnaryPredicate>
// requires InIt InputIterator
// requires OutIt OutputIterator
// requires UnaryPredicate, returns bool, argument type value_type(InIt)
auto move_while(InIt first, InIt last, OutIt dest, UnaryPredicate pred) -> std::pair<InIt, OutIt>
{
    // calling with move iterator so this function just takes r-value and call
    // pred with l-value
    auto lam([p(astl::pass_fn(pred))](auto &&x) { return p(x); });
    auto p(i::copy_while(std::make_move_iterator(first), std::make_move_iterator(last), dest, lam));
    return std::make_pair(p.first.base(), p.second);
}

template <typename InIt, typename OutIt, typename UnaryPredicate, typename P>
auto move_while(InIt first, InIt last, OutIt dest, UnaryPredicate pred, P p) -> std::pair<InIt, OutIt>
{
    return i::move_while(first, last, dest, astl::combine(astl::pass_fn(pred), astl::pass_fn(p)));
}

template <typename InIt, typename OutIt, typename UnaryPredicate>
// requires InIt InputIterator
// requires OutIt OutputIterator
// requires UnaryPredicate, returns bool, argument type value_type(InIt)
auto move_until(InIt first, InIt last, OutIt dest, UnaryPredicate pred) -> std::pair<InIt, OutIt>
{
    return i::move_while(first, last, dest, astl::not_fn(astl::pass_fn(pred)));
}

template <typename InIt, typename OutIt, typename UnaryPredicate, typename P>
auto move_until(InIt first, InIt last, OutIt dest, UnaryPredicate pred, P p) -> std::pair<InIt, OutIt>
{
    return i::move_while(first, last, dest, astl::not_fn(astl::pass_fn(pred)), astl::pass_fn(p));
}

template <typename InIt, typename OutIt, typename BinaryPredicate>
// requires InIt InputIterator
// requires OutIt OutputIterator
// requires BinaryPredicate, returns bool, two arguments of value_type(InIt)
auto move_until_adjacent_check(InIt first, InIt last, OutIt dest, BinaryPredicate pred)
    -> std::pair<OutIt, InIt>
{
    auto p(i::copy_until_adjacent_check(std::make_move_iterator(first),
                                        std::make_move_iterator(last), dest, astl::pass_fn(pred)));
    return std::make_pair(p.first, p.second.base());
}

template <typename InIt, typename OutIt, typename BinaryPredicate, typename P>
auto move_until_adjacent_check(InIt first, InIt last, OutIt dest, BinaryPredicate pred, P p)
    -> std::pair<OutIt, InIt>
{
    return i::move_until_adjacent_check(first, last, dest,
                                        astl::combine(astl::pass_fn(pred), astl::pass_fn(p)));
}

template <typename InIt, typename N, typename OutIt, typename BinaryPredicate>
// requires InIt InputIterator
// requires N integral type
// requires OutIt OutputIterator
// requires BinaryPredicate, returns bool, two arguments of value_type(InIt)
auto move_until_adjacent_check_n(InIt first, N n, OutIt dest, BinaryPredicate pred)
    -> std::pair<OutIt, InIt>
{
    auto p(i::copy_until_adjacent_check_n(std::make_move_iterator(first), n, dest,
                                          astl::pass_fn(pred)));
    return std::make_pair(p.first, p.second.base());
}

template <typename InIt, typename N, typename OutIt, typename BinaryPredicate, typename P>
auto move_until_adjacent_check_n(InIt first, N n, OutIt dest, BinaryPredicate pred, P p)
    -> std::pair<OutIt, InIt>
{
    return i::move_until_adjacent_check_n(first, n, dest,
                                          astl::combine(astl::pass_fn(pred), astl::pass_fn(p)));
}

template <typename InIt, typename OutIt, typename Comparator>
// requires InIt InputIterator
// requires OutIt OutputIterator
// requires Comparator, returns bool, two arguments of value_type(InIt)
auto move_until_sorted(InIt first, InIt last, OutIt dest, Comparator comp) -> OutIt
{
    return i::copy_until_sorted(std::make_move_iterator(first), std::make_move_iterator(last), dest,
                                astl::pass_fn(comp));
}

template <typename InIt, typename OutIt>
// requires InIt InputIterator
// requires OutIt OutputIterator
auto move_until_sorted(InIt first, InIt last, OutIt dest) -> OutIt
{
    return i::move_until_sorted(first, last, dest, std::less{});
}

template <typename InIt, typename OutIt, typename Comparator, typename P>
auto move_until_sorted(InIt first, InIt last, OutIt dest, Comparator comp, P p) -> OutIt
{
    return i::copy_until_sorted(std::make_move_iterator(first), std::make_move_iterator(last), dest,
                                astl::pass_fn(comp), astl::pass_fn(p));
}

template <typename InIt, typename N, typename OutIt, typename Comparator>
// requires InIt InputIterator
// requires N integral type
// requires OutIt OutputIterator
// requires Comparator, returns bool, two arguments of value_type(InIt)
auto move_until_sorted_n(InIt first, N n, OutIt dest, Comparator comp) -> std::pair<OutIt, InIt>
{
    auto p(i::copy_until_sorted_n(std::make_move_iterator(first), n, dest, astl::pass_fn(comp)));
    return std::make_pair(p.first, p.second.base());
}

template <typename InIt, typename N, typename OutIt>
// requires InIt InputIterator
// requires N integral type
// requires OutIt OutputIterator
auto move_until_sorted_n(InIt first, N n, OutIt dest) -> std::pair<OutIt, InIt>
{
    return i::move_until_sorted_n(first, n, dest, std::less{});
}

template <typename InIt, typename N, typename OutIt, typename Comparator, typename P>
auto move_until_sorted_n(InIt first, N n, OutIt dest, Comparator comp, P p) -> std::pair<OutIt, InIt>
{
    auto pr(i::copy_until_sorted_n(std::make_move_iterator(first), n, dest, astl::pass_fn(comp),
                                   astl::pass_fn(p)));
    return std::make_pair(pr.first, pr.second.base());
}

template <typename InIt, typename N, typename OutIt, typename UnaryPredicate>
// requires InIt InputIterator
// requires N integral type
// requires OutIt OutputIterator
// requires UnaryPredicate, returns bool, argument type value_type(InIt)
auto move_while_n(InIt first, N n, OutIt dest, UnaryPredicate pred) -> std::pair<InIt, OutIt>
{
    while (n != N(0) && pred(*first)) {
        *dest = std::move(*first);
        ++first;
        ++dest;
        --n;
    }
    return std::make_pair(first, dest);
}

template <typename InIt, typename N, typename OutIt, typename UnaryPredicate, typename P>
auto move_while_n(InIt first, N n, OutIt dest, UnaryPredicate pred, P p) -> std::pair<InIt, OutIt>
{
    return i::move_while_n(first, n, dest, astl::combine(astl::pass_fn(pred), astl::pass_fn(p)));
}

template <typename InIt, typename N, typename OutIt, typename UnaryPredicate>
// requires InIt InputIterator
// requires N integral type
// requires OutIt OutputIterator
// requires UnaryPredicate, returns bool, argument type value_type(InIt)
auto move_until_n(InIt first, N n, OutIt dest, UnaryPredicate pred) -> std::pair<InIt, OutIt>
{
    return i::move_while_n(first, n, dest, astl::not_fn(astl::pass_fn(pred)));
}

template <typename InIt, typename N, typename OutIt, typename UnaryPredicate, typename P>
auto move_until_n(InIt first, N n, OutIt dest, UnaryPredicate pred, P p) -> std::pair<InIt, OutIt>
{
    return i::move_while_n(first, n, dest, astl::not_fn(astl::pass_fn(pred)), astl::pass_fn(p));
}

template <typename BidiIt, typename OutIt>
// requires BidiIt BidirectionalIterator
// requires OutIt BidirectionalIterator
auto reverse_move_backward(BidiIt first, BidiIt last, OutIt dest) -> OutIt
{
    // precondition: not_overlapped(first, last, dest − (last−first), last)
    return i::reverse_copy_backward(std::make_move_iterator(first), std::make_move_iterator(last),
                                    dest);
}

} // namespace i

namespace r
{
template <typename R, typename OutIt>
// requires R InputIterator range
// requires OutIt OutputIterator
auto move(R &&r, OutIt d_first) -> OutIt
{
    return i::move(adl::begin(r), adl::end(r), d_first);
}

template <typename R, typename OutIt>
// requires R BidirectionalIterator range
// requires OutIt BidirectionalIterator
auto move_backward(R &&r, OutIt d_first) -> OutIt
{
    return i::move_backward(adl::begin(r), adl::end(r), d_first);
}

template <typename R1, typename R2>
// requires R1 InputIterator range
// requires R2 OutputIterator range
auto move_bounded(R1 &&r1, R2 &&r2) -> std::pair<astl::iter_of_range<R2>, astl::iter_of_range<R1>>
{
    return i::move_bounded(adl::begin(r1), adl::end(r1), adl::begin(r2), adl::end(r2));
}

template <typename R1, typename N1, typename R2, typename N2>
// requires R1 InputIterator range
// requires R2 OutputIterator range
auto move_bounded_n(R1 &&r1, N1 n1, R2 &&r2, N2 n2)
    -> std::pair<astl::iter_of_range<R2>, astl::iter_of_range<R1>>
{
    return i::move_bounded_n(adl::begin(r1), n1, adl::begin(r2), n2);
}

template <typename R, typename OutIt, typename UnaryPredicateicate>
// requires R InputIterator range
// requires OutIt OutputIterator
// requires UnaryPredicate, returns bool, argument type value_type(InIt)
auto move_if(R &&r, OutIt d_first, UnaryPredicateicate pred) -> OutIt
{
    return i::move_if(adl::begin(r), adl::end(r), d_first, astl::pass_fn(pred));
}

template <typename R, typename OutIt, typename UnaryPredicateicate, typename P>
auto move_if(R &&r, OutIt d_first, UnaryPredicateicate pred, P p) -> OutIt
{
    return i::move_if(adl::begin(r), adl::end(r), d_first, astl::pass_fn(pred), astl::pass_fn(p));
}

template <typename R, typename N, typename OutIt, typename UnaryPredicate>
// requires R InputIterator range
// requires N integral type
// requires OutIt OutputIterator
// requires UnaryPredicate, returns bool, argument type value_type(InIt)
auto move_if_n(R &&r, N n, OutIt d_first, UnaryPredicate pred)
    -> std::pair<OutIt, astl::iter_of_range<R>>
{
    return i::move_if_n(adl::begin(r), n, d_first, astl::pass_fn(pred));
}

template <typename R, typename N, typename OutIt, typename UnaryPredicate, typename P>
auto move_if_n(R &&r, N n, OutIt d_first, UnaryPredicate pred, P p)
    -> std::pair<OutIt, astl::iter_of_range<R>>
{
    return i::move_if_n(adl::begin(r), n, d_first, astl::pass_fn(pred), astl::pass_fn(p));
}

template <typename R, typename N, typename OutIt>
// requires R InputIterator range
// requires N integral type
// requires OutIt OutputIterator
auto move_n(R &&r, N n, OutIt d_first) -> std::pair<OutIt, astl::iter_of_range<R>>
{
    return i::move_n(adl::begin(r), n, d_first);
}

template <typename R, typename OutIt, typename T>
// requires R InputIterator range
// requires OutIt OutputIterator
// requires T equality comparable with value_type(InIt)
auto move_until_sentinel(R &&r, OutIt dest, T &&val) -> std::pair<OutIt, astl::iter_of_range<R>>
{
    return i::move_until_sentinel(adl::begin(r), adl::end(r), dest, val);
}

template <typename R, typename OutIt, typename T, typename P>
auto move_until_sentinel(R &&r, OutIt dest, T &&val, P p)
    -> std::pair<OutIt, astl::iter_of_range<R>>
{
    return i::move_until_sentinel(adl::begin(r), adl::end(r), dest, val, astl::pass_fn(p));
}

template <typename R, typename N, typename OutIt, typename T>
// requires R InputIterator range
// requires N integral type
// requires OutIt OutputIterator
// requires T equality comparable with value_type(InIt)
auto move_until_sentinel_n(R &&r, N n, OutIt dest, T &&val)
    -> std::pair<OutIt, astl::iter_of_range<R>>
{
    return i::move_until_sentinel_n(adl::begin(r), n, dest, val);
}

template <typename R, typename N, typename OutIt, typename T, typename P>
auto move_until_sentinel_n(R &&r, N n, OutIt dest, T &&val, P p)
    -> std::pair<OutIt, astl::iter_of_range<R>>
{
    return i::move_until_sentinel_n(adl::begin(r), n, dest, val, astl::pass_fn(p));
}

template <typename R, typename OutIt, typename UnaryPredicate>
// requires R InputIterator range
// requires OutIt OutputIterator
// requires UnaryPredicate, returns bool, argument type value_type(InIt)
auto move_until(R &&r, OutIt dest, UnaryPredicate pred) -> std::pair<astl::iter_of_range<R>, OutIt>
{
    return i::move_until(adl::begin(r), adl::end(r), dest, astl::pass_fn(pred));
}

template <typename R, typename OutIt, typename UnaryPredicate, typename P>
auto move_until(R &&r, OutIt dest, UnaryPredicate pred, P p) -> std::pair<astl::iter_of_range<R>, OutIt>
{
    return i::move_until(adl::begin(r), adl::end(r), dest, astl::pass_fn(pred), astl::pass_fn(p));
}

template <typename R, typename OutIt, typename BinaryPredicate>
// requires R InputIterator range
// requires OutIt OutputIterator
// requires BinaryPredicate, returns bool, two arguments of value_type(R)
auto move_until_adjacent_check(R &&r, OutIt dest, BinaryPredicate pred)
    -> std::pair<OutIt, astl::iter_of_range<R>>
{
    return i::move_until_adjacent_check(adl::begin(r), adl::end(r), dest, astl::pass_fn(pred));
}

template <typename R, typename OutIt, typename BinaryPredicate, typename P>
auto move_until_adjacent_check(R &&r, OutIt dest, BinaryPredicate pred, P p)
    -> std::pair<OutIt, astl::iter_of_range<R>>
{
    return i::move_until_adjacent_check(adl::begin(r), adl::end(r), dest, astl::pass_fn(pred),
                                        astl::pass_fn(p));
}

template <typename R, typename N, typename OutIt, typename BinaryPredicate>
// requires R InputIterator range
// requires N integral type
// requires OutIt OutputIterator
// requires BinaryPredicate, returns bool, two arguments of value_type(R)
auto move_until_adjacent_check_n(R &&r, N n, OutIt dest, BinaryPredicate pred)
    -> std::pair<OutIt, astl::iter_of_range<R>>
{
    return i::move_until_adjacent_check_n(adl::begin(r), n, dest, astl::pass_fn(pred));
}

template <typename R, typename N, typename OutIt, typename BinaryPredicate, typename P>
auto move_until_adjacent_check_n(R &&r, N n, OutIt dest, BinaryPredicate pred, P p)
    -> std::pair<OutIt, astl::iter_of_range<R>>
{
    return i::move_until_adjacent_check_n(adl::begin(r), n, dest, astl::pass_fn(pred),
                                          astl::pass_fn(p));
}

template <typename R, typename N, typename OutIt, typename UnaryPredicate>
// requires R InputIterator range
// requires N integral type
// requires OutIt OutputIterator
// requires UnaryPredicate, returns bool, argument type value_type(R)
auto move_until_n(R &&r, N n, OutIt dest, UnaryPredicate pred)
    -> std::pair<astl::iter_of_range<R>, OutIt>
{
    return i::move_until_n(adl::begin(r), n, dest, astl::pass_fn(pred));
}

template <typename R, typename N, typename OutIt, typename UnaryPredicate, typename P>
auto move_until_n(R &&r, N n, OutIt dest, UnaryPredicate pred, P p)
    -> std::pair<astl::iter_of_range<R>, OutIt>
{
    return i::move_until_n(adl::begin(r), n, dest, astl::pass_fn(pred), astl::pass_fn(p));
}

template <typename R, typename OutIt, typename Comparator>
// requires R InputIterator range
// requires OutIt OutputIterator
// requires Cmp, returns bool, two arguments of value_type(R)
auto move_until_sorted(R &&r, OutIt dest, Comparator comp) -> OutIt
{
    return i::move_until_sorted(adl::begin(r), adl::end(r), dest, astl::pass_fn(comp));
}

template <typename R, typename OutIt>
// requires R InputIterator range
// requires OutIt OutputIterator
auto move_until_sorted(R &&r, OutIt dest) -> OutIt
{
    return r::move_until_sorted(r, dest, std::less{});
}

template <typename R, typename OutIt, typename Comparator, typename P>
auto move_until_sorted(R &&r, OutIt dest, Comparator comp, P p) -> OutIt
{
    return i::move_until_sorted(adl::begin(r), adl::end(r), dest, astl::pass_fn(comp),
                                astl::pass_fn(p));
}

template <typename R, typename N, typename OutIt, typename Comparator>
// requires R InputIterator range
// requires N integral type
// requires OutIt OutputIterator
// requires Cmp, returns bool, two arguments of value_type(R)
auto move_until_sorted_n(R &&r, N n, OutIt dest, Comparator comp) -> OutIt
{
    return i::move_until_sorted_n(adl::begin(r), n, dest, astl::pass_fn(comp));
}

template <typename R, typename N, typename OutIt>
// requires R InputIterator range
// requires N integral type
// requires OutIt OutputIterator
auto move_until_sorted_n(R &&r, N n, OutIt dest) -> OutIt
{
    return r::move_until_sorted_n(r, n, dest, std::less{});
}

template <typename R, typename N, typename OutIt, typename Comparator, typename P>
auto move_until_sorted_n(R &&r, N n, OutIt dest, Comparator comp, P p) -> OutIt
{
    return i::move_until_sorted_n(adl::begin(r), n, dest, astl::pass_fn(comp), astl::pass_fn(p));
}

template <typename R, typename OutIt, typename UnaryPredicate>
// requires R InputIterator range
// requires OutIt OutputIterator
// requires UnaryPredicate, returns bool, argument type value_type(InIt)
auto move_while(R &&r, OutIt dest, UnaryPredicate pred) -> std::pair<astl::iter_of_range<R>, OutIt>
{
    return i::move_while(adl::begin(r), adl::end(r), dest, astl::pass_fn(pred));
}

template <typename R, typename OutIt, typename UnaryPredicate, typename P>
auto move_while(R &&r, OutIt dest, UnaryPredicate pred, P p) -> std::pair<astl::iter_of_range<R>, OutIt>
{
    return i::move_while(adl::begin(r), adl::end(r), dest, astl::pass_fn(pred), astl::pass_fn(p));
}

template <typename R, typename N, typename OutIt, typename UnaryPredicate>
// requires R InputIterator range
// requires N integral type
// requires OutIt OutputIterator
// requires UnaryPredicate, returns bool, argument type value_type(R)
auto move_while_n(R &&r, N n, OutIt dest, UnaryPredicate pred)
    -> std::pair<astl::iter_of_range<R>, OutIt>
{
    return i::move_while_n(adl::begin(r), n, dest, astl::pass_fn(pred));
}

template <typename R, typename N, typename OutIt, typename UnaryPredicate, typename P>
auto move_while_n(R &&r, N n, OutIt dest, UnaryPredicate pred, P p)
    -> std::pair<astl::iter_of_range<R>, OutIt>
{
    return i::move_while_n(adl::begin(r), n, dest, astl::pass_fn(pred), astl::pass_fn(p));
}

template <typename R, typename OutIt>
// requires R BidirectionalIterator range
// requires OutIt BidirectionalIterator range
auto reverse_move_backward(R &&r, OutIt d_first) -> OutIt
{
    return i::reverse_move_backward(adl::begin(r), adl::end(r), d_first);
}

} // namespace r
} // namespace astl

#endif // ASTL_INCLUDE_MOVE_HPP
