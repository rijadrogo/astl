//
// Created by Rijad on 30-Jul-18.
//

#ifndef ASTL_INCLUDE_REMOVE_HPP
#define ASTL_INCLUDE_REMOVE_HPP

#include <algorithm>
#include <utility>

#include "find.hpp"
#include "search.hpp"

#include "astl/functional.hpp"
#include "astl/iterator.hpp"
#include "astl/range_access.hpp"

namespace astl
{
namespace i
{

// If the predicate evaluates to false,
// the first element of the pair is included in the result range;
// otherwise, it is skipped.
// Note: Last element is always copied
template <typename FwdIt, typename OutIt, typename BinaryPredicate>
// requires FwdIt ForwardIterator
// requires OutIt OutputIterator
// requires BinaryPredicate, returns bool, two arguments value_type(FwdIt)
auto adjacent_remove_copy_if(FwdIt first, FwdIt last, OutIt dest, BinaryPredicate pred) -> OutIt
{
    if (first == last) return dest;

    FwdIt trail(first);
    ++first;
    while (first != last) {
        if (!pred(*trail, *first)) {
            *dest = std::move(*trail);
            ++dest;
        }
        ++trail;
        ++first;
    }

    *dest = std::move(*trail);
    ++dest;
    return dest;
}

// If the predicate evaluates to false,
// the first element of the pair is included in the result range;
// otherwise, it is skipped.
// Note: Last element is always copied
template <typename FwdIt, typename OutIt, typename BinaryPredicate, typename P>
auto adjacent_remove_copy_if(FwdIt first, FwdIt last, OutIt dest, BinaryPredicate pred, P p)
    -> OutIt
{
    return i::adjacent_remove_copy_if(first, last, dest,
                                      astl::combine(astl::pass_fn(pred), astl::pass_fn(p)));
}

// If the predicate evaluates to false,
// the first element of the pair is included in the result range;
// otherwise, it is skipped.
// Note: Last element is always copied
template <typename FwdIt, typename BinaryPredicate>
// requires FwdIt ForwardIterator
// requires BinaryPredicate, returns bool, two arguments value_type(FwdIt)
auto adjacent_remove_if(FwdIt first, FwdIt last, BinaryPredicate pred) -> FwdIt
{
    first = i::adjacent_find(first, last, astl::pass_fn(pred));
    if (first == last) return first;

    FwdIt trail(std::next(first));
    FwdIt current(std::next(trail));
    while (current != last) {
        if (!pred(*trail, *current)) {
            *first = std::move(*trail);
            ++first;
        }
        ++trail;
        ++current;
    }
    *first = std::move(*trail);
    ++first;
    return first;
}

// If the predicate evaluates to false,
// the first element of the pair is included in the result range;
// otherwise, it is skipped.
// Note: Last element is always copied
template <typename FwdIt, typename BinaryPredicate, typename P>
auto adjacent_remove_if(FwdIt first, FwdIt last, BinaryPredicate pred, P p) -> FwdIt
{
    return i::adjacent_remove_if(first, last, astl::combine(astl::pass_fn(pred), astl::pass_fn(p)));
}

using std::remove_copy; // NOLINT(misc-unused-using-decls)
template <typename InputIt, typename OutIt, typename T, typename P>
auto remove_copy(InputIt first, InputIt const last, OutIt dest, T const &value, P p) -> OutIt
{
    while (first != last) {
        if (!(invoke(p, *first) == value)) {
            *dest = *first;
            ++dest;
        }
        ++first;
    }
    return dest;
}

using std::remove; // NOLINT(misc-unused-using-decls)
template <typename FwdIt, typename T, typename P>
auto remove(FwdIt first, FwdIt last, T const &value, P p) -> FwdIt
{
    auto proj(astl::pass_fn(p));
    first = i::find(first, last, value, proj);
    FwdIt i(first);
    return first == last ? first : i::remove_copy(++i, last, first, value, proj);
}

using std::remove_copy_if; // NOLINT(misc-unused-using-decls)
template <typename InputIt, typename OutIt, typename UnaryPredicate, typename P>
auto remove_copy_if(InputIt first, InputIt last, OutIt dest, UnaryPredicate pred, P p) -> OutIt
{
    return i::remove_copy_if(first, last, dest,
                             astl::combine(astl::pass_fn(pred), astl::pass_fn(p)));
}

using std::remove_if; // NOLINT(misc-unused-using-decls)
template <typename FwdIt, typename UnaryPredicate, typename P>
auto remove_if(FwdIt first, FwdIt last, UnaryPredicate pred, P p) -> FwdIt
{
    return std::remove_if(first, last, astl::combine(astl::pass_fn(pred), astl::pass_fn(p)));
}

template <typename InputIt, typename OutIt, typename T>
auto remove_move(InputIt first, InputIt last, OutIt dest, T const &value) -> OutIt
{
    return std::remove_copy(std::make_move_iterator(first), std::make_move_iterator(last), dest,
                            value);
}

template <typename InputIt, typename OutIt, typename T, typename P>
auto remove_move(InputIt first, InputIt last, OutIt dest, T const &value, P p) -> OutIt
{
    return i::remove_copy(std::make_move_iterator(first), std::make_move_iterator(last), dest,
                          value, astl::pass_fn(p));
}

template <typename InputIt, typename OutIt, typename UnaryPredicate, typename P>
auto remove_move_if(InputIt first, InputIt last, OutIt dest, UnaryPredicate pred) -> OutIt
{
    return std::remove_copy_if(std::make_move_iterator(first), std::make_move_iterator(last), dest,
                               astl::pass_fn(pred));
}

template <typename InputIt, typename OutIt, typename UnaryPredicate, typename P>
auto remove_move_if(InputIt first, InputIt last, OutIt dest, UnaryPredicate pred, P p) -> OutIt
{
    return i::remove_copy_if(std::make_move_iterator(first), std::make_move_iterator(last), dest,
                             astl::pass_fn(pred), astl::pass_fn(p));
}

template <typename FwdIt1, typename FwdIt2, typename BinaryPredicate = std::equal_to<>>
auto remove_range(FwdIt1 first, FwdIt1 const last, FwdIt2 s_first, FwdIt2 const s_last,
                  BinaryPredicate pred = std::equal_to{}) -> FwdIt1
{
    using PairType = std::pair<FwdIt1, FwdIt1>;
    PairType p(i::searchp(first, last, s_first, s_last, astl::pass_fn(pred)));
    if (p.first == last) return last;

    return std::move(p.second, last, p.first);
}

template <typename FwdIt1, typename FwdIt2, typename BinaryPredicate, typename P>
auto remove_range(FwdIt1 first, FwdIt1 last, FwdIt2 s_first, FwdIt2 s_last, BinaryPredicate pred,
                  P p) -> FwdIt1
{
    using PairType = std::pair<FwdIt1, FwdIt1>;
    PairType pr(i::searchp(first, last, s_first, s_last, astl::pass_fn(pred), astl::pass_fn(p)));
    if (pr.first == last) return last;

    return std::move(pr.second, last, pr.first);
}

template <typename BidiIt, typename UnaryPredicate>
auto unstable_remove_if(BidiIt first, BidiIt last, UnaryPredicate pred) -> BidiIt
{
    while (true) {
        first = std::find_if(first, last, astl::pass_fn(pred));
        last = std::find_if_not(std::make_reverse_iterator(last), std::make_reverse_iterator(first),
                                astl::pass_fn(pred))
                   .base();
        if (first == last) return first;

        *first = std::move(*--last);
    }
}

template <typename BidiIt, typename UnaryPredicate, typename P>
auto unstable_remove_if(BidiIt first, BidiIt last, UnaryPredicate pred, P p) -> BidiIt
{
    return i::unstable_remove_if(first, last, astl::combine(astl::pass_fn(pred), astl::pass_fn(p)));
}

} // namespace i

namespace r
{

// Note: Last element is always copied
template <typename R, typename OutIt, typename BinaryPredicate>
auto adjacent_remove_copy_if(R &&r, OutIt dest, BinaryPredicate pred)
{
    return i::adjacent_remove_copy_if(adl::begin(r), adl::end(r), dest, astl::pass_fn(pred));
}

// Note: Last element is always copied
template <typename R, typename OutIt, typename BinaryPredicate, typename P>
auto adjacent_remove_copy_if(R &&r, OutIt dest, BinaryPredicate pred, P p)
{
    return i::adjacent_remove_copy_if(adl::begin(r), adl::end(r), dest, astl::pass_fn(pred),
                                      astl::pass_fn(p));
}

// Note: Last element is always copied
template <typename R, typename BinaryPredicate>
auto adjacent_remove_if(R &&r, BinaryPredicate pred) -> iter_of_range<R>
{
    return i::adjacent_remove_if(adl::begin(r), adl::end(r), astl::pass_fn(pred));
}

// Note: Last element is always copied
template <typename R, typename BinaryPredicate, typename P>
auto adjacent_remove_if(R &&r, BinaryPredicate pred, P p) -> iter_of_range<R>
{
    return i::adjacent_remove_if(adl::begin(r), adl::end(r), astl::pass_fn(pred), astl::pass_fn(p));
}

template <typename R, typename T> auto remove(R &&r, T const &value) -> iter_of_range<R>
{
    return i::remove(adl::begin(r), adl::end(r), value);
}

template <typename R, typename T, typename P>
auto remove(R &&r, T const &value, P p) -> iter_of_range<R>
{
    return i::remove(adl::begin(r), adl::end(r), value, astl::pass_fn(p));
}

template <typename R, typename OutIt, typename T>
auto remove_copy(R &&r, OutIt dest, T const &value) -> OutIt
{
    return i::remove_copy(adl::begin(r), adl::end(r), dest, value);
}

template <typename R, typename OutIt, typename T, typename P>
auto remove_copy(R &&r, OutIt dest, T const &value, P p) -> OutIt
{
    return i::remove_copy(adl::begin(r), adl::end(r), dest, value, astl::pass_fn(p));
}

template <typename R, typename OutIt, typename UnaryPredicate>
auto remove_copy_if(R &&r, OutIt dest, UnaryPredicate pred) -> OutIt
{
    return i::remove_copy_if(adl::begin(r), adl::end(r), dest, astl::pass_fn(pred));
}

template <typename R, typename OutIt, typename UnaryPredicate, typename P>
auto remove_copy_if(R &&r, OutIt dest, UnaryPredicate pred, P p) -> OutIt
{
    return i::remove_copy_if(adl::begin(r), adl::end(r), dest, astl::pass_fn(pred),
                             astl::pass_fn(p));
}

template <typename R, typename UnaryPredicate>
auto remove_if(R &&r, UnaryPredicate pred) -> iter_of_range<R>
{
    return i::remove_if(adl::begin(r), adl::end(r), astl::pass_fn(pred));
}

template <typename R, typename UnaryPredicate, typename P>
auto remove_if(R &&r, UnaryPredicate pred, P p) -> iter_of_range<R>
{
    return i::remove_if(adl::begin(r), adl::end(r), astl::pass_fn(pred), astl::pass_fn(p));
}

template <typename R, typename OutIt, typename T>
auto remove_move(R &&r, OutIt dest, T const &value) -> OutIt
{
    return i::remove_move(adl::begin(r), adl::end(r), dest, value);
}

template <typename R, typename OutIt, typename T, typename P>
auto remove_move(R &&r, OutIt dest, T const &value, P p) -> OutIt
{
    return i::remove_move(adl::begin(r), adl::end(r), dest, value, astl::pass_fn(p));
}

template <typename R, typename OutIt, typename UnaryPredicate>
auto remove_move_if(R &&r, OutIt dest, UnaryPredicate pred) -> OutIt
{
    return i::remove_move_if(adl::begin(r), adl::end(r), dest, astl::pass_fn(pred));
}

template <typename R, typename OutIt, typename UnaryPredicate, typename P>
auto remove_move_if(R &&r, OutIt dest, UnaryPredicate pred, P p) -> OutIt
{
    return i::remove_move_if(adl::begin(r), adl::end(r), dest, astl::pass_fn(pred),
                             astl::pass_fn(p));
}

template <typename R1, typename R2, typename BinaryPredicate = std::equal_to<>>
auto remove_range(R1 &&r1, R2 &&r2, BinaryPredicate pred = std::equal_to{}) -> iter_of_range<R1>
{
    return i::remove_range(adl::begin(r1), adl::end(r1), adl::begin(r2), adl::end(r2),
                           astl::pass_fn(pred));
}

template <typename R1, typename R2, typename BinaryPredicate, typename P>
auto remove_range(R1 &&r1, R2 &&r2, BinaryPredicate pred, P p) -> iter_of_range<R1>
{
    return i::remove_range(adl::begin(r1), adl::end(r1), adl::begin(r2), adl::end(r2),
                           astl::pass_fn(pred), astl::pass_fn(p));
}

template <typename R, typename UnaryPredicate>
auto unstable_remove_if(R &&r, UnaryPredicate pred) -> iter_of_range<R>
{
    return i::unstable_remove_if(adl::begin(r), adl::end(r), astl::pass_fn(pred));
}

template <typename R, typename UnaryPredicate, typename P>
auto unstable_remove_if(R &&r, UnaryPredicate pred, P p) -> iter_of_range<R>
{
    return i::unstable_remove_if(adl::begin(r), adl::end(r), astl::pass_fn(pred), astl::pass_fn(p));
}

} // namespace r
} // namespace astl

#endif // ASTL_INCLUDE_REMOVE_HPP
