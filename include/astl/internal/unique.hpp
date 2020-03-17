//
// Created by Rijad on 30-Jul-18.
//

#ifndef ASTL_INCLUDE_UNIQUE_HPP
#define ASTL_INCLUDE_UNIQUE_HPP

#include <algorithm>
#include <type_traits>
#include <utility>

#include "adjacent_find.hpp"

#include "astl/functional.hpp"
#include "astl/iterator.hpp"
#include "astl/range_access.hpp"

namespace astl
{

namespace i
{
using std::unique; // NOLINT(misc-unused-using-decls)
template <typename FwdIt, typename BinaryPredicate, typename P>
auto unique(FwdIt first, FwdIt last, BinaryPredicate pred, P p) -> FwdIt
{
    return std::unique(first, last, astl::combine(astl::pass_fn(pred), astl::pass_fn(p)));
}

using std::unique_copy; // NOLINT(misc-unused-using-decls)
template <typename InputIt, typename OutIt, typename BinaryPredicate, typename P>
auto unique_copy(InputIt first, InputIt last, OutIt dest, BinaryPredicate pred, P p) -> OutIt
{
    return std::unique_copy(first, last, dest,
                            astl::combine(astl::pass_fn(pred), astl::pass_fn(p)));
}

template <typename InIt, typename N, typename OutIt, typename BinaryPredicate>
// requires InIt InputIterator
// requires N integral type
// requires OutIt InputIterator
// requires BinaryPredicate, returns bool, arguments two value_type(InIt)
auto unique_copy_n(InIt first, N n, OutIt dest, BinaryPredicate pred) -> std::pair<OutIt, InIt>
{

    using SameValueT = std::is_same<iter_value_type<InIt>, iter_value_type<OutIt>>;
    if (n == N(0)) return std::make_pair(dest, first);

    if constexpr (is_forward_it_v<InIt>) {
        using FwdIt = InIt;

        FwdIt firstb(first);
        *dest = *firstb;
        ++dest;
        ++first;
        --n;
        while (n != N(0)) {
            if (!pred(*firstb, *first)) { // copy unmatched
                firstb = first;
                *dest = *firstb;
                ++dest;
            }
            ++first;
            --n;
        }
    }
    else if constexpr (is_forward_it_v<OutIt> && SameValueT::value) {
        *dest = *first;
        ++first;
        --n;
        while (n != N(0)) {
            if (!pred(*dest, *first)) *++dest = *first;

            ++first;
            --n;
        }
        ++dest;
    }
    else {
        auto val(*first);
        *dest = val;
        ++dest;
        ++first;
        --n;
        while (n != N(0)) {
            if (!pred(val, *first)) { // copy unmatched
                val = *first;
                *dest = val;
                ++dest;
            }
            ++first;
            --n;
        }
    }

    return std::make_pair(dest, first);
}

template <typename InIt, typename N, typename OutIt>
// requires InIt InputIterator
// requires N integral type
// requires OutIt InputIterator
auto unique_copy_n(InIt first, N n, OutIt dest) -> std::pair<OutIt, InIt>
{
    return i::unique_copy_n(first, n, dest, std::equal_to{});
}

template <typename InIt, typename N, typename OutIt, typename BinaryPredicate, typename P>
auto unique_copy_n(InIt first, N n, OutIt dest, BinaryPredicate pred, P p) -> std::pair<OutIt, InIt>
{
    return i::unique_copy_n(first, n, dest, astl::combine(astl::pass_fn(pred), astl::pass_fn(p)));
}

template <typename FwdIt, typename OutIt>
// requires FwdIt ForwardIterator
// requires OutIt ForwardIterator
auto unique_move(FwdIt first, FwdIt last, OutIt dest) -> OutIt
{
    return std::unique_copy(std::make_move_iterator(first), std::make_move_iterator(last), dest);
}

template <typename FwdIt, typename OutIt, typename BinaryPredicate>
// requires FwdIt ForwardIterator
// requires OutIt ForwardIterator
// requires BinaryPredicate, returns bool, arguments value_type(FwdIt)
auto unique_move(FwdIt first, FwdIt last, OutIt dest, BinaryPredicate pred) -> OutIt
{
    return std::unique_copy(std::make_move_iterator(first), std::make_move_iterator(last), dest,
                            astl::pass_fn(pred));
}

template <typename FwdIt, typename OutIt, typename BinaryPredicate, typename P>
auto unique_move(FwdIt first, FwdIt last, OutIt dest, BinaryPredicate pred, P p) -> OutIt
{
    return i::unique_copy(std::make_move_iterator(first), std::make_move_iterator(last), dest,
                          astl::pass_fn(pred), astl::pass_fn(p));
}

template <typename InIt, typename N, typename OutIt, typename BinaryPredicate>
// requires InIt InputIterator
// requires N integral type
// requires OutIt InputIterator
// requires BinaryPredicate, returns bool, arguments two value_type(InIt)
auto unique_move_n(InIt first, N n, OutIt dest, BinaryPredicate pred) -> std::pair<OutIt, InIt>
{
    auto p(i::unique_copy_n(std::make_move_iterator(first), n, dest, astl::pass_fn(pred)));
    return std::make_pair(p.first, p.second.base());
}

template <typename InIt, typename N, typename OutIt>
// requires InIt InputIterator
// requires N integral type
// requires OutIt InputIterator
auto unique_move_n(InIt first, N n, OutIt dest) -> std::pair<OutIt, InIt>
{
    return i::unique_move_n(first, n, dest, std::equal_to{});
}

template <typename InIt, typename N, typename OutIt, typename BinaryPredicate, typename P>
auto unique_move_n(InIt first, N n, OutIt dest, BinaryPredicate pred, P p) -> std::pair<OutIt, InIt>
{
    return i::unique_move_n(first, n, dest, astl::combine(astl::pass_fn(pred), astl::pass_fn(p)));
}

template <typename FwdIt, typename N, typename BinaryPredicate>
// requires FwdIt ForwardIterator
// requires N integral type
// requires BinaryPredicate, returns bool, arguments tow value_type(FwdIt)
auto unique_n(FwdIt first, N n, BinaryPredicate pred) -> FwdIt
{
    // Skip the beginning, if already unique.
    std::pair<FwdIt, N> i(i::adjacent_find_n(first, n, astl::pass_fn(pred)));
    n = i.second;
    if (n == N(0)) return first;
    // Do the real copy work.
    FwdIt dest(i.first);
    first = std::next(dest);
    while (--n != N(0)) {
        if (!pred(*dest, *first)) *++dest = std::move(*first);

        ++first;
    }
    return ++dest;
}

template <typename FwdIt, typename N>
// requires FwdIt ForwardIterator
// requires N integral type
auto unique_n(FwdIt first, N n) -> FwdIt
{
    return i::unique_n(first, n, std::equal_to{});
}

template <typename FwdIt, typename N, typename BinaryPredicate, typename P>
auto unique_n(FwdIt first, N n, BinaryPredicate pred, P p) -> FwdIt
{
    return i::unique_n(first, n, astl::combine(astl::pass_fn(pred), astl::pass_fn(p)));
}
} // namespace i

namespace r
{
template <typename R>
// requires R ForwardIterator range
auto unique(R &&r) -> iter_of_range<R>
{
    return i::unique(adl::begin(r), adl::end(r));
}

template <typename R, typename BinaryPredicate>
// requires R ForwardIterator range
// requires BinaryPredicate, returns bool, arguments value_type(R)
auto unique(R &&r, BinaryPredicate pred) -> iter_of_range<R>
{
    return i::unique(adl::begin(r), adl::end(r), astl::pass_fn(pred));
}

template <typename R, typename BinaryPredicate, typename P>
auto unique(R &&r, BinaryPredicate pred, P p) -> iter_of_range<R>
{
    return i::unique(adl::begin(r), adl::end(r), astl::pass_fn(pred), astl::pass_fn(p));
}

template <typename R, typename OutIt>
// requires R ForwardIterator range
// requires OutIt ForwardIterator
auto unique_copy(R &&r, OutIt dest) -> OutIt
{
    return i::unique_copy(adl::begin(r), adl::end(r), dest);
}

template <typename R, typename OutIt, typename BinaryPredicate>
// requires R ForwardIterator range
// requires OutIt ForwardIterator
// requires BinaryPredicate, returns bool, arguments value_type(R)
auto unique_copy(R &&r, OutIt dest, BinaryPredicate pred) -> OutIt
{
    return i::unique_copy(adl::begin(r), adl::end(r), dest, astl::pass_fn(pred));
}

template <typename R, typename OutIt, typename BinaryPredicate, typename P>
auto unique_copy(R &&r, OutIt dest, BinaryPredicate pred, P p) -> OutIt
{
    return i::unique_copy(adl::begin(r), adl::end(r), dest, astl::pass_fn(pred), astl::pass_fn(p));
}

template <typename R, typename N, typename OutIt, typename BinaryPredicate>
// requires R InputIterator range
// requires N integral type
// requires OutIt InputIterator
// requires BinaryPredicate, returns bool, arguments two value_type(InIt)
auto unique_copy_n(R &&r, N n, OutIt dest, BinaryPredicate pred)
    -> std::pair<OutIt, astl::iter_of_range<R>>
{
    return i::unique_copy_n(adl::begin(r), n, dest, astl::pass_fn(pred));
}

template <typename R, typename N, typename OutIt>
// requires R InputIterator range
// requires N integral type
// requires OutIt InputIterator
auto unique_copy_n(R &&r, N n, OutIt dest) -> std::pair<OutIt, astl::iter_of_range<R>>
{
    return r::unique_copy_n(r, n, dest, std::equal_to{});
}

template <typename R, typename N, typename OutIt, typename BinaryPredicate, typename P>
auto unique_copy_n(R &&r, N n, OutIt dest, BinaryPredicate pred, P p)
    -> std::pair<OutIt, astl::iter_of_range<R>>
{
    return i::unique_copy_n(adl::begin(r), n, dest, astl::pass_fn(pred), astl::pass_fn(p));
}

template <typename R, typename OutIt>
// requires R ForwardIterator range
// requires OutIt ForwardIterator
auto unique_move(R &&r, OutIt dest) -> OutIt
{
    return i::unique_move(adl::begin(r), adl::end(r), dest);
}

template <typename R, typename OutIt, typename BinaryPredicate>
// requires R ForwardIterator range
// requires OutIt ForwardIterator
// requires BinaryPredicate, returns bool, arguments value_type(R)
auto unique_move(R &&r, OutIt dest, BinaryPredicate pred) -> OutIt
{
    return i::unique_move(adl::begin(r), adl::end(r), dest, astl::pass_fn(pred));
}

template <typename R, typename OutIt, typename BinaryPredicate, typename P>
auto unique_move(R &&r, OutIt dest, BinaryPredicate pred, P p) -> OutIt
{
    return i::unique_move(adl::begin(r), adl::end(r), dest, astl::pass_fn(pred), astl::pass_fn(p));
}

template <typename R, typename N, typename OutIt, typename BinaryPredicate>
// requires R InputIterator range
// requires N integral type
// requires OutIt InputIterator
// requires BinaryPredicate, returns bool, arguments two value_type(InIt)
auto unique_move_n(R &&r, N n, OutIt dest, BinaryPredicate pred)
    -> std::pair<OutIt, astl::iter_of_range<R>>
{
    return i::unique_move_n(adl::begin(r), n, dest, astl::pass_fn(pred));
}

template <typename R, typename N, typename OutIt>
// requires R InputIterator range
// requires N integral type
// requires OutIt InputIterator
auto unique_move_n(R &&r, N n, OutIt dest) -> std::pair<OutIt, astl::iter_of_range<R>>
{
    return r::unique_move_n(r, n, dest, std::equal_to{});
}

template <typename R, typename N, typename OutIt, typename BinaryPredicate, typename P>
auto unique_move_n(R &&r, N n, OutIt dest, BinaryPredicate pred, P p)
    -> std::pair<OutIt, astl::iter_of_range<R>>
{
    return i::unique_move_n(adl::begin(r), n, dest, astl::pass_fn(pred), astl::pass_fn(p));
}

template <typename R, typename N, typename BinaryPredicate>
auto unique_n(R &&r, N n, BinaryPredicate pred) -> iter_of_range<R>
{
    return i::unique_n(adl::begin(r), n, astl::pass_fn(pred));
}

template <typename R, typename N> auto unique_n(R &&r, N n) -> iter_of_range<R>
{
    return r::unique_n(r, n, std::equal_to{});
}

template <typename R, typename N, typename BinaryPredicate, typename P>
auto unique_n(R &&r, N n, BinaryPredicate pred, P p) -> iter_of_range<R>
{
    return i::unique_n(adl::begin(r), n, astl::pass_fn(pred), astl::pass_fn(p));
}
} // namespace r
} // namespace astl

#endif // ASTL_INCLUDE_UNIQUE_HPP
