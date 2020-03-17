//
// Created by Rijad on 04-Aug-18.
//

#ifndef ASTL_INCLUDE_SCAN_HPP
#define ASTL_INCLUDE_SCAN_HPP

#include <utility>

#include "astl/functional.hpp"
#include "astl/iterator.hpp"
#include "astl/range_access.hpp"

namespace astl
{
namespace i
{
template <typename InIt, typename OutIt, typename T>
// requires InIt InputIterator
// requires OutIt OutputIterator
// requires T, to be convertible to value_type(InIt) and convertible to
// value_type(OutIt)
auto exclusive_scan(InIt first, InIt last, OutIt dest, T val) -> OutIt
{
    // set each value in [dest, dest + (last - first)) to the associative
    // reduction of predecessors and val
    while (first != last) {
        *dest = val;
        ++dest;
        val = val + *first;
        ++first;
    }
    return dest;
}

template <typename InIt, typename OutIt, typename T, typename BinOp>
// requires InIt InputIterator
// requires OutIt OutputIterator
// requires T, to be convertible to value_type(InIt) and convertible to
// value_type(OutIt) requires BinOp ??
auto exclusive_scan(InIt first, InIt last, OutIt dest, T val, BinOp reduce_op) -> OutIt
{
    // set each value in [dest, dest + (last - first)) to the associative
    // reduction of predecessors and val
    while (first != last) {
        *dest = val;
        ++dest;
        val = reduce_op(std::move(val), *first);
        ++first;
    }
    return dest;
}

template <typename InIt, typename OutIt, typename T, typename BinOp, typename P>
auto exclusive_scan(InIt first, InIt last, OutIt dest, T val, BinOp reduce_op, P p) -> OutIt
{
    // set each value in [dest, dest + (last - first)) to the associative
    // reduction of predecessors and val
    while (first != last) {
        *dest = val;
        ++dest;
        val = reduce_op(std::move(val), invoke(p, *first));
        ++first;
    }
    return dest;
}

template <typename BidiIt, typename OutIt, typename T>
// requires BidiIt BidirectionalIterator
// requires OutIt OutputIterator
// requires T, to be convertible to value_type(BidiIt) and convertible to
// value_type(OutIt)
auto exclusive_scan_backward(BidiIt first, BidiIt last, OutIt dest, T val) -> OutIt
{
    return i::exclusive_scan(std::make_reverse_iterator(last), std::make_reverse_iterator(first),
                             dest, std::move(val));
}

template <typename BidiIt, typename OutIt, typename T, typename BinOp>
// requires BidiIt BidirectionalIterator
// requires OutIt OutputIterator
// requires T, to be convertible to value_type(BidiIt) and convertible to
// value_type(OutIt) requires BinOp ??
auto exclusive_scan_backward(BidiIt first, BidiIt last, OutIt dest, T val, BinOp reduce_op) -> OutIt
{
    return i::exclusive_scan(std::make_reverse_iterator(last), std::make_reverse_iterator(first),
                             dest, std::move(val), astl::pass_fn(reduce_op));
}

template <typename BidiIt, typename OutIt, typename T, typename BinOp, typename P>
auto exclusive_scan_backward(BidiIt first, BidiIt last, OutIt dest, T val, BinOp reduce_op, P p)
    -> OutIt
{
    return i::exclusive_scan(std::make_reverse_iterator(last), std::make_reverse_iterator(first),
                             dest, std::move(val), astl::pass_fn(reduce_op), astl::pass_fn(p));
}

template <typename InIt, typename N, typename OutIt, typename T>
// requires InIt InputIterator
// requires OutIt OutputIterator
// requires T, to be convertible to value_type(InIt) and convertible to
// value_type(OutIt)
auto exclusive_scan_n(InIt first, N n, OutIt dest, T val) -> std::pair<OutIt, InIt>
{
    // set each value in [dest, dest + (last - first)) to the associative
    // reduction of predecessors and val
    while (n != N(0)) {
        *dest = val;
        ++dest;
        val += *first;
        ++first;
        --n;
    }
    return std::make_pair(dest, first);
}

template <typename InIt, typename N, typename OutIt, typename T, typename BinOp>
// requires InIt InputIterator
// requires OutIt OutputIterator
// requires T, to be convertible to value_type(InIt) and convertible to
// value_type(OutIt) requires BinOp ??
auto exclusive_scan_n(InIt first, N n, OutIt dest, T val, BinOp reduce_op) -> std::pair<OutIt, InIt>
{
    // set each value in [dest, dest + (last - first)) to the associative
    // reduction of predecessors and val
    while (n != N(0)) {
        *dest = val;
        ++dest;
        val = reduce_op(std::move(val), *first);
        ++first;
        --n;
    }
    return std::make_pair(dest, first);
}

template <typename InIt, typename N, typename OutIt, typename T, typename BinOp, typename P>
auto exclusive_scan_n(InIt first, N n, OutIt dest, T val, BinOp reduce_op, P p)
    -> std::pair<OutIt, InIt>
{
    // set each value in [dest, dest + (last - first)) to the associative
    // reduction of predecessors and val
    while (n != N(0)) {
        *dest = val;
        ++dest;
        val = reduce_op(std::move(val), invoke(p, *first));
        ++first;
        --n;
    }
    return std::make_pair(dest, first);
}

template <typename InIt, typename OutIt, typename BinOp>
auto inclusive_scan(InIt first, InIt last, OutIt dest, BinOp reduce_op) -> OutIt
{
    // compute partial non commutative and associative reductions into dest, using
    // reduce_op
    if (first != last) {
        auto val(*first);
        while (true) {
            *dest = val;
            ++dest;
            ++first;
            if (first == last) break;

            val = reduce_op(std::move(val), *first);
        }
    }
    return dest;
}

template <typename InIt, typename OutIt>
// requires InIt InputIterator
// requires OutIt OutputIterator
auto inclusive_scan(InIt first, InIt last, OutIt dest) -> OutIt
{
    return i::inclusive_scan(first, last, dest, std::plus{});
}

template <typename InIt, typename OutIt, typename BinOp, typename T>
auto inclusive_scan(InIt first, InIt last, OutIt dest, BinOp reduce_op, T val) -> OutIt
{
    // compute partial non commutative and associative reductions including val
    // into dest, using reduce_op
    while (first != last) {
        val = reduce_op(std::move(val), *first);
        *dest = val;
        ++first;
        ++dest;
    }
    return dest;
}

template <typename InIt, typename OutIt, typename BinOp, typename T, typename P>
auto inclusive_scan(InIt first, InIt last, OutIt dest, BinOp reduce_op, T val, P p) -> OutIt
{
    // compute partial non commutative and associative reductions including val
    // into dest, using reduce_op
    while (first != last) {
        val = reduce_op(std::move(val), invoke(p, *first));
        *dest = val;
        ++first;
        ++dest;
    }
    return dest;
}

template <typename BidiIt, typename OutIt>
// requires BidiIt BidirectionalIterator
// requires OutIt OutputIterator
auto inclusive_scan_backward(BidiIt first, BidiIt last, OutIt dest) -> OutIt
{
    return i::inclusive_scan_backward(first, last, dest, std::plus{});
}

template <typename BidiIt, typename OutIt, typename BinOp>
auto inclusive_scan_backward(BidiIt first, BidiIt last, OutIt dest, BinOp reduce_op) -> OutIt
{
    return i::inclusive_scan(std::make_reverse_iterator(last), std::make_reverse_iterator(first),
                             dest, astl::pass_fn(reduce_op));
}

template <typename BidiIt, typename OutIt, typename BinOp, typename T>
auto inclusive_scan_backward(BidiIt first, BidiIt last, OutIt dest, BinOp reduce_op, T val) -> OutIt
{
    return i::inclusive_scan(std::make_reverse_iterator(last), std::make_reverse_iterator(first),
                             dest, astl::pass_fn(reduce_op), std::move(val));
}

template <typename InIt, typename OutIt, typename BinOp, typename T, typename P>
auto inclusive_scan_backward(InIt first, InIt last, OutIt dest, BinOp reduce_op, T val, P p)
    -> OutIt
{
    return i::inclusive_scan(std::make_reverse_iterator(last), std::make_reverse_iterator(first),
                             dest, astl::pass_fn(reduce_op), std::move(val), astl::pass_fn(p));
}

template <typename InIt, typename N, typename OutIt, typename BinOp>
auto inclusive_scan_n(InIt first, N n, OutIt dest, BinOp reduce_op) -> std::pair<OutIt, InIt>
{
    // compute partial non commutative and associative reductions into dest, using
    // reduce_op
    if (n != N(0)) {
        auto val(*first);
        while (true) {
            *dest = val;
            ++dest;
            ++first;
            --n;
            if (n != N(0)) break;

            val = reduce_op(std::move(val), *first);
        }
    }
    return std::make_pair(dest, first);
}

template <typename InIt, typename N, typename OutIt>
// requires InIt InputIterator
// requires OutIt OutputIterator
auto inclusive_scan_n(InIt first, N n, OutIt dest) -> std::pair<OutIt, InIt>
{
    return i::inclusive_scan_n(first, n, dest, std::plus{});
}

template <typename InIt, typename N, typename OutIt, typename BinOp, typename T>
auto inclusive_scan_n(InIt first, N n, OutIt dest, BinOp reduce_op, T val) -> std::pair<OutIt, InIt>
{
    // compute partial non commutative and associative reductions including val
    // into dest, using reduce_op
    while (n != N(0)) {
        val = reduce_op(std::move(val), *first);
        *dest = val;
        ++first;
        ++dest;
        --n;
    }

    return std::make_pair(dest, first);
}

template <typename InIt, typename N, typename OutIt, typename BinOp, typename T, typename P>
auto inclusive_scan_n(InIt first, N n, OutIt dest, BinOp reduce_op, T val, P p)
    -> std::pair<OutIt, InIt>
{
    // compute partial non commutative and associative reductions including val
    // into dest, using reduce_op
    while (n != N(0)) {
        val = reduce_op(std::move(val), invoke(p, *first));
        *dest = val;
        ++first;
        ++dest;
        --n;
    }
    return std::make_pair(dest, first);
}
}// namespace i

namespace r
{
template <typename R, typename OutIt, typename T>
auto exclusive_scan(R &&r, OutIt dest, T val) -> OutIt
{
    return i::exclusive_scan(adl::begin(r), adl::end(r), dest, std::move(val));
}

template <typename R, typename OutIt, typename T, typename BinOp>
auto exclusive_scan(R &&r, OutIt dest, T val, BinOp reduce_op) -> OutIt
{
    return i::exclusive_scan(adl::begin(r), adl::end(r), dest, std::move(val),
                             astl::pass_fn(reduce_op));
}

template <typename R, typename OutIt, typename T, typename BinOp, typename P>
auto exclusive_scan(R &&r, OutIt dest, T val, BinOp reduce_op, P p) -> OutIt
{
    return i::exclusive_scan(adl::begin(r), adl::end(r), dest, std::move(val),
                             astl::pass_fn(reduce_op), astl::pass_fn(p));
}

template <typename R, typename OutIt, typename T>
auto exclusive_scan_backward(R &&r, OutIt dest, T val) -> OutIt
{
    return i::exclusive_scan_backward(adl::begin(r), adl::end(r), dest, std::move(val));
}

template <typename R, typename OutIt, typename T, typename BinOp>
auto exclusive_scan_backward(R &&r, OutIt dest, T val, BinOp reduce_op) -> OutIt
{
    return i::exclusive_scan_backward(adl::begin(r), adl::end(r), dest, std::move(val),
                                      astl::pass_fn(reduce_op));
}

template <typename R, typename OutIt, typename T, typename BinOp, typename P>
auto exclusive_scan_backward(R &&r, OutIt dest, T val, BinOp reduce_op, P p) -> OutIt
{
    return i::exclusive_scan_backward(adl::begin(r), adl::end(r), dest, std::move(val),
                                      astl::pass_fn(reduce_op), astl::pass_fn(p));
}

template <typename R, typename N, typename OutIt, typename T>
auto exclusive_scan_n(R &&r, N n, OutIt dest, T val) -> std::pair<OutIt, astl::iter_of_range<R>>
{
    return i::exclusive_scan_n(adl::begin(r), n, dest, std::move(val));
}

template <typename R, typename N, typename OutIt, typename T, typename BinOp>
auto exclusive_scan_n(R &&r, N n, OutIt dest, T val, BinOp reduce_op)
    -> std::pair<OutIt, astl::iter_of_range<R>>
{
    return i::exclusive_scan_n(adl::begin(r), n, dest, std::move(val), astl::pass_fn(reduce_op));
}

template <typename R, typename N, typename OutIt, typename T, typename BinOp, typename P>
auto exclusive_scan_n(R &&r, N n, OutIt dest, T val, BinOp reduce_op, P p)
    -> std::pair<OutIt, astl::iter_of_range<R>>
{
    return i::exclusive_scan_n(adl::begin(r), n, dest, std::move(val), astl::pass_fn(reduce_op),
                               astl::pass_fn(p));
}

template <typename R, typename OutIt> auto inclusive_scan(R &&r, OutIt dest) -> OutIt
{
    return i::inclusive_scan(adl::begin(r), adl::end(r), dest);
}

template <typename R, typename OutIt, typename BinOp>
auto inclusive_scan(R &&r, OutIt dest, BinOp reduce_op) -> OutIt
{
    return i::inclusive_scan(adl::begin(r), adl::end(r), dest, astl::pass_fn(reduce_op));
}

template <typename R, typename OutIt, typename BinOp, typename T>
auto inclusive_scan(R &&r, OutIt dest, BinOp reduce_op, T val) -> OutIt
{
    return i::inclusive_scan(adl::begin(r), adl::end(r), dest, astl::pass_fn(reduce_op),
                             std::move(val));
}

template <typename R, typename OutIt, typename BinOp, typename T, typename P>
auto inclusive_scan(R &&r, OutIt dest, BinOp reduce_op, T val, P p) -> OutIt
{
    return i::inclusive_scan(adl::begin(r), adl::end(r), dest, astl::pass_fn(reduce_op),
                             std::move(val), astl::pass_fn(p));
}

template <typename R, typename OutIt> auto inclusive_scan_backward(R &&r, OutIt dest) -> OutIt
{
    return i::inclusive_scan_backward(adl::begin(r), adl::end(r), dest);
}

template <typename R, typename OutIt, typename BinOp>
auto inclusive_scan_backward(R &&r, OutIt dest, BinOp reduce_op) -> OutIt
{
    return i::inclusive_scan_backward(adl::begin(r), adl::end(r), dest, astl::pass_fn(reduce_op));
}

template <typename R, typename OutIt, typename BinOp, typename T>
auto inclusive_scan_backward(R &&r, OutIt dest, BinOp reduce_op, T val) -> OutIt
{
    return i::inclusive_scan_backward(adl::begin(r), adl::end(r), dest, astl::pass_fn(reduce_op),
                                      std::move(val));
}

template <typename R, typename OutIt, typename BinOp, typename T, typename P>
auto inclusive_scan_backward(R &&r, OutIt dest, BinOp reduce_op, T val, P p) -> OutIt
{
    return i::inclusive_scan_backward(adl::begin(r), adl::end(r), dest, astl::pass_fn(reduce_op),
                                      std::move(val), astl::pass_fn(p));
}

template <typename R, typename N, typename OutIt>
auto inclusive_scan_n(R &&r, N n, OutIt dest) -> std::pair<OutIt, astl::iter_of_range<R>>
{
    return i::inclusive_scan_n(adl::begin(r), n, dest);
}

template <typename R, typename N, typename OutIt, typename BinOp>
auto inclusive_scan_n(R &&r, N n, OutIt dest, BinOp reduce_op)
    -> std::pair<OutIt, astl::iter_of_range<R>>
{
    return i::inclusive_scan_n(adl::begin(r), n, dest, astl::pass_fn(reduce_op));
}

template <typename R, typename N, typename OutIt, typename BinOp, typename T>
auto inclusive_scan_n(R &&r, N n, OutIt dest, BinOp reduce_op, T val)
    -> std::pair<OutIt, astl::iter_of_range<R>>
{
    return i::inclusive_scan_n(adl::begin(r), n, dest, astl::pass_fn(reduce_op), std::move(val));
}

template <typename R, typename N, typename OutIt, typename BinOp, typename T, typename P>
auto inclusive_scan_n(R &&r, N n, OutIt dest, BinOp reduce_op, T val, P p)
    -> std::pair<OutIt, astl::iter_of_range<R>>
{
    return i::inclusive_scan_n(adl::begin(r), n, dest, astl::pass_fn(reduce_op), std::move(val),
                               astl::pass_fn(p));
}
}// namespace r
}// namespace astl

#endif// ASTL_INCLUDE_SCAN_HPP
