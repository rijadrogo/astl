//
// Created by Rijad on 30-Jul-18.
//

#ifndef ASTL_INCLUDE_REPLACE_HPP
#define ASTL_INCLUDE_REPLACE_HPP

#include <algorithm>
#include <utility>

#include "astl/functional.hpp"
#include "astl/iterator.hpp"
#include "astl/range_access.hpp"

namespace astl
{
namespace i
{
using std::replace;// NOLINT(misc-unused-using-decls)
template <typename FwdIt, typename T, typename P>
auto replace(FwdIt first, FwdIt const last, T const &old_value, T const &new_value, P p) -> void
{
    while (first != last) {
        if (invoke(p, *first) == old_value) *first = new_value;

        ++first;
    }
}

using std::replace_copy;// NOLINT(misc-unused-using-decls)
template <typename InIt, typename OutIt, typename T, typename P>
auto replace_copy(InIt first, InIt const last, OutIt dest, T const &old_value, T const &new_value,
                  P p) -> OutIt
{
    while (first != last) {
        *dest = (invoke(p, *first) == old_value ? new_value : *first);
        ++first;
        ++dest;
    }
    return dest;
}

using std::replace_copy_if;// NOLINT(misc-unused-using-decls)
template <typename InIt, typename OutIt, typename UnaryPredicate, typename T, typename P>
auto replace_copy_if(InIt first, InIt last, OutIt dest, UnaryPredicate pred, T const &new_value, P p)
    -> OutIt
{
    return std::replace_copy_if(first, last, dest,
                                astl::combine(astl::pass_fn(pred), astl::pass_fn(p)), new_value);
}

template <typename InIt, typename N, typename OutIt, typename UnaryPredicate, typename U>
auto replace_copy_if_n(InIt first, N n, OutIt dest, UnaryPredicate pred, U const &new_value)
    -> std::pair<OutIt, InIt>
{
    while (n != N(0)) {
        *dest = (pred(*first) ? new_value : *first);
        ++dest;
        ++first;
        --n;
    }
    return std::make_pair(dest, first);
}

template <typename InIt, typename N, typename OutIt, typename UnaryPredicate, typename U, typename P>
auto replace_copy_if_n(InIt first, N n, OutIt dest, UnaryPredicate pred, U const &new_value, P p)
    -> std::pair<OutIt, InIt>
{
    return i::replace_copy_if_n(first, n, dest,
                                astl::combine(astl::pass_fn(pred), astl::pass_fn(p)), new_value);
}

template <typename InIt, typename N, typename OutIt, typename T>
auto replace_copy_n(InIt first, N n, OutIt dest, T const &old_value, T const &new_value)
    -> std::pair<OutIt, InIt>
{
    while (n != N(0)) {
        *dest = (*first == old_value ? new_value : *first);
        ++first;
        ++dest;
        --n;
    }
    return std::make_pair(dest, first);
}

template <typename InIt, typename N, typename OutIt, typename T, typename P>
auto replace_copy_n(InIt first, N n, OutIt dest, T const &old_value, T const &new_value, P p)
    -> std::pair<OutIt, InIt>
{
    while (n != N(0)) {
        *dest = (invoke(p, *first) == old_value ? new_value : *first);
        ++first;
        ++dest;
        --n;
    }
    return std::make_pair(dest, first);
}

using std::replace_if;// NOLINT(misc-unused-using-decls)
template <typename FwdIt, typename UnaryPredicate, typename T, typename P>
auto replace_if(FwdIt first, FwdIt last, UnaryPredicate pred, T const &new_value, P p) -> void
{
    i::replace_if(first, last, astl::combine(astl::pass_fn(pred), astl::pass_fn(p)), new_value);
}

template <typename FwdIt, typename N, typename UnaryPredicate, typename T>
auto replace_if_n(FwdIt first, N n, UnaryPredicate pred, T const &new_value) -> FwdIt
{
    while (n != N(0)) {
        if (pred(*first)) *first = new_value;

        ++first;
        --n;
    }
    return first;
}

template <typename FwdIt, typename N, typename UnaryPredicate, typename T, typename P>
auto replace_if_n(FwdIt first, N n, UnaryPredicate pred, T const &new_value, P p) -> FwdIt
{
    return i::replace_if_n(first, n, astl::combine(astl::pass_fn(pred), astl::pass_fn(p)),
                           new_value);
}

template <typename InIt, typename OutIt, typename T>
auto replace_move(InIt first, InIt last, OutIt dest, T const &old_value, T const &new_value)
    -> OutIt
{
    return std::replace_copy(std::make_move_iterator(first), std::make_move_iterator(last), dest,
                             old_value, new_value);
}

template <typename InIt, typename OutIt, typename T, typename P>
auto replace_move(InIt first, InIt const last, OutIt dest, T const &old_value, T const &new_value,
                  P p) -> OutIt
{
    while (first != last) {
        if (invoke(p, *first) == old_value) *dest = new_value;
        else
            *dest = std::move(*first);

        ++first;
        ++dest;
    }
    return dest;
}

template <typename InIt, typename OutIt, typename UnaryPredicate, typename T>
auto replace_move_if(InIt first, InIt last, OutIt dest, UnaryPredicate pred, T const &new_value) -> OutIt
{
    return std::replace_copy_if(std::make_move_iterator(first), std::make_move_iterator(last), dest,
                                astl::pass_fn(pred), new_value);
}

template <typename InIt, typename OutIt, typename UnaryPredicate, typename T, typename P>
auto replace_move_if(InIt first, InIt const last, OutIt dest, UnaryPredicate pred, T const &new_value,
                     P p) -> OutIt
{
    while (first != last) {
        if (pred(invoke(p, *first))) *dest = new_value;
        else
            *dest = std::move(*first);

        ++first;
        ++dest;
    }
    return dest;
}

template <typename InIt, typename N, typename OutIt, typename UnaryPredicate, typename T>
auto replace_move_if_n(InIt first, N n, OutIt dest, UnaryPredicate pred, T const &new_value)
    -> std::pair<OutIt, InIt>
{
    auto i(i::replace_copy_if_n(std::make_move_iterator(first), n, dest, astl::pass_fn(pred),
                                new_value));
    return std::make_pair(i.first, i.second.base());
}

template <typename InIt, typename N, typename OutIt, typename UnaryPredicate, typename T, typename P>
auto replace_move_if_n(InIt first, N n, OutIt dest, UnaryPredicate pred, T const &new_value, P p)
    -> std::pair<OutIt, InIt>
{
    auto i(i::replace_copy_if_n(std::make_move_iterator(first), n, dest, astl::pass_fn(pred),
                                new_value, astl::pass_fn(p)));
    return std::make_pair(i.first, i.second.base());
}

template <typename InIt, typename N, typename OutIt, typename T>
auto replace_move_n(InIt first, N n, OutIt dest, T const &old_value, T const &new_value)
    -> std::pair<OutIt, InIt>
{
    auto i(i::replace_copy_n(std::make_move_iterator(first), n, dest, old_value, new_value));
    return std::make_pair(i.first, i.second.base());
}

template <typename InIt, typename N, typename OutIt, typename T, typename P>
auto replace_move_n(InIt first, N n, OutIt dest, T const &old_value, T const &new_value, P p)
    -> std::pair<OutIt, InIt>
{
    auto i(i::replace_copy_n(std::make_move_iterator(first), n, dest, old_value, new_value,
                             astl::pass_fn(p)));
    return std::make_pair(i.first, i.second.base());
}

template <typename FwdIt, typename N, typename T, typename U>
auto replace_n(FwdIt first, N n, T const &old_value, U const &new_value) -> FwdIt
{
    while (n != N(0)) {
        if (*first == old_value) *first = new_value;

        ++first;
        --n;
    }
    return first;
}

template <typename FwdIt, typename N, typename T, typename U, typename P>
auto replace_n(FwdIt first, N n, T const &old_value, U const &new_value, P p) -> FwdIt
{
    while (n != N(0)) {
        if (invoke(p, *first) == old_value) *first = new_value;

        ++first;
        --n;
    }
    return first;
}
}// namespace i

namespace r
{
template <typename R, typename T>
auto replace(R &&r, T const &old_value, T const &new_value) -> void
{
    i::replace(adl::begin(r), adl::end(r), old_value, new_value);
}

template <typename R, typename T, typename P>
auto replace(R &&r, T const &old_value, T const &new_value, P p) -> void
{
    i::replace(adl::begin(r), adl::end(r), old_value, new_value, astl::pass_fn(p));
}

/// Given a sequence container Cont, replace the range [cont_it, cont_end) with
/// the range [val_it, val_end) (which is not from the same container).
template <typename R, typename RandIt>
auto replace(R &cont, iter_of_range<R> cont_it, iter_of_range<R> cont_end, RandIt val_it,
             RandIt val_end) -> void
{
    while (true) {
        if (val_it == val_end) {
            cont.erase(cont_it, cont_end);
            return;
        }
        if (cont_it == cont_end) {
            cont.insert(cont_it, val_it, val_end);
            return;
        }
        *cont_it = *val_it;
        ++cont_it;
        ++val_it;
    }
}

/// Given a sequence container Cont, replace the range [cont_it, cont_end) with
/// the range R.
template <typename R, typename Range = std::initializer_list<range_value_type<R>>>
void replace(R &cont, iter_of_range<R> cont_it, iter_of_range<R> cont_end, Range r)
{
    replace(cont, cont_it, cont_end, r.begin(), r.end());
}

template <typename R, typename OutIt, typename T>
auto replace_copy(R &&r, OutIt dest, T const &old_value, T const &new_value) -> OutIt
{
    return i::replace_copy(adl::begin(r), adl::end(r), dest, old_value, new_value);
}

template <typename R, typename OutIt, typename T, typename P>
auto replace_copy(R &&r, OutIt dest, T const &old_value, T const &new_value, P p) -> OutIt
{
    return i::replace_copy(adl::begin(r), adl::end(r), dest, old_value, new_value,
                           astl::pass_fn(p));
}

template <typename R, typename OutIt, typename UnaryPredicate, typename T>
auto replace_copy_if(R &&r, OutIt dest, UnaryPredicate pred, T const &new_value) -> OutIt
{
    return i::replace_copy_if(adl::begin(r), adl::end(r), dest, astl::pass_fn(pred), new_value);
}

template <typename R, typename OutIt, typename UnaryPredicate, typename T, typename P>
auto replace_copy_if(R &&r, OutIt dest, UnaryPredicate pred, T const &new_value, P p) -> OutIt
{
    return i::replace_copy_if(adl::begin(r), adl::end(r), dest, astl::pass_fn(pred), new_value,
                              astl::pass_fn(p));
}

template <typename R, typename N, typename OutIt, typename UnaryPredicate, typename T>
auto replace_copy_if_n(R &&r, N n, OutIt dest, UnaryPredicate pred, T const &new_value)
    -> std::pair<OutIt, astl::iter_of_range<R>>
{
    return i::replace_copy_if_n(adl::begin(r), n, dest, astl::pass_fn(pred), new_value);
}

template <typename R, typename N, typename OutIt, typename UnaryPredicate, typename T, typename P>
auto replace_copy_if_n(R &&r, N n, OutIt dest, UnaryPredicate pred, T const &new_value, P p)
    -> std::pair<OutIt, astl::iter_of_range<R>>
{
    return i::replace_copy_if_n(adl::begin(r), n, dest, astl::pass_fn(pred), new_value,
                                astl::pass_fn(p));
}

template <typename R, typename UnaryPredicate, typename T>
auto replace_if(R &&r, UnaryPredicate pred, T const &new_value) -> void
{
    i::replace_if(adl::begin(r), adl::end(r), astl::pass_fn(pred), new_value);
}

template <typename R, typename UnaryPredicate, typename T, typename P>
auto replace_if(R &&r, UnaryPredicate pred, T const &new_value, P p) -> void
{
    i::replace_if(adl::begin(r), adl::end(r), astl::pass_fn(pred), new_value, astl::pass_fn(p));
}

template <typename R, typename N, typename UnaryPredicate, typename T>
auto replace_if_n(R &&r, N n, UnaryPredicate pred, T const &new_value) -> iter_of_range<R>
{
    return i::replace_if_n(adl::begin(r), n, astl::pass_fn(pred), new_value);
}

template <typename R, typename N, typename UnaryPredicate, typename T, typename P>
auto replace_if_n(R &&r, N n, UnaryPredicate pred, T const &new_value, P p) -> iter_of_range<R>
{
    return i::replace_if_n(adl::begin(r), n, astl::pass_fn(pred), new_value, astl::pass_fn(p));
}

template <typename R, typename OutIt, typename T>
auto replace_move(R &&r, OutIt dest, T const &old_value, T const &new_value) -> OutIt
{
    return i::replace_move(adl::begin(r), adl::end(r), dest, old_value, new_value);
}

template <typename R, typename OutIt, typename T, typename P>
auto replace_move(R &&r, OutIt dest, T const &old_value, T const &new_value, P p) -> OutIt
{
    return i::replace_move(adl::begin(r), adl::end(r), dest, old_value, new_value,
                           astl::pass_fn(p));
}

template <typename R, typename OutIt, typename UnaryPredicate, typename T>
auto replace_move_if(R &&r, OutIt dest, UnaryPredicate pred, T const &new_value) -> OutIt
{
    return i::replace_move_if(adl::begin(r), adl::end(r), dest, astl::pass_fn(pred), new_value);
}

template <typename R, typename OutIt, typename UnaryPredicate, typename T, typename P>
auto replace_move_if(R &&r, OutIt dest, UnaryPredicate pred, T const &new_value, P p) -> OutIt
{
    return i::replace_move_if(adl::begin(r), adl::end(r), dest, astl::pass_fn(pred), new_value,
                              astl::pass_fn(p));
}

template <typename R, typename N, typename OutIt, typename UnaryPredicate, typename T>
auto replace_move_if_n(R &&r, N n, OutIt dest, UnaryPredicate pred, T const &new_value)
    -> std::pair<OutIt, astl::iter_of_range<R>>
{
    return i::replace_move_if_n(adl::begin(r), n, dest, astl::pass_fn(pred), new_value);
}

template <typename R, typename N, typename OutIt, typename UnaryPredicate, typename T, typename P>
auto replace_move_if_n(R &&r, N n, OutIt dest, UnaryPredicate pred, T const &new_value, P p)
    -> std::pair<OutIt, astl::iter_of_range<R>>
{
    return i::replace_move_if_n(adl::begin(r), n, dest, astl::pass_fn(pred), new_value,
                                astl::pass_fn(p));
}

template <typename R, typename N, typename OutIt, typename T>
auto replace_move_n(R &&r, N n, OutIt dest, T const &old_value, T const &new_value)
    -> std::pair<OutIt, astl::iter_of_range<R>>
{
    return i::replace_move_n(adl::begin(r), n, dest, old_value, new_value);
}

template <typename R, typename N, typename OutIt, typename T, typename P>
auto replace_move_n(R &&r, N n, OutIt dest, T const &old_value, T const &new_value, P p)
    -> std::pair<OutIt, astl::iter_of_range<R>>
{
    return i::replace_move_n(adl::begin(r), n, dest, old_value, new_value, astl::pass_fn(p));
}

template <typename R, typename N, typename T>
auto replace_n(R &&r, N n, T const &old_value, T const &new_value) -> iter_of_range<R>
{
    return i::replace(adl::begin(r), n, old_value, new_value);
}

template <typename R, typename N, typename T, typename P>
auto replace_n(R &&r, N n, T const &old_value, T const &new_value, P p) -> iter_of_range<R>
{
    return i::replace_n(adl::begin(r), n, old_value, new_value, astl::pass_fn(p));
}
}// namespace r
}// namespace astl

#endif// ASTL_INCLUDE_REPLACE_HPP
