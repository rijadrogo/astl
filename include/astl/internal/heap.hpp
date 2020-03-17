//
// Created by Rijad on 04-Aug-18.
//

#ifndef ASTL_INCLUDE_HEAP_HPP
#define ASTL_INCLUDE_HEAP_HPP

#include <algorithm>

#include "astl/functional.hpp"
#include "astl/iterator.hpp"
#include "astl/range_access.hpp"

namespace astl
{
namespace i
{
using std::is_heap;// NOLINT(misc-unused-using-decls)
template <typename RandIt, typename Comparator, typename P>
ASTL_NODISCARD auto is_heap(RandIt first, RandIt last, Comparator comp, P p) -> bool
{
    return std::is_heap(first, last, astl::combine(astl::pass_fn(comp), astl::pass_fn(p)));
}

template <typename RandIt, typename N, typename Comparator, typename P>
auto is_heap_n(RandIt first, N n, Comparator comp, P p) -> bool
{
    return std::is_heap(first, first + n, astl::combine(astl::pass_fn(comp), astl::pass_fn(p)));
}

template <typename RandIt, typename N, typename Comparator>
// requires RandIt RandomAccessIterator
// requires N integral type
// requires Comparator is StrictWeakOrdering on the value_type(RandIt)
ASTL_NODISCARD auto is_heap_n(RandIt first, N n, Comparator comp) -> bool
{
    return std::is_heap(first, first + n, astl::pass_fn(comp));
}

template <typename RandIt, typename N>
// requires RandIt RandomAccessIterator
// requires N integral type
ASTL_NODISCARD auto is_heap_n(RandIt first, N n) -> bool
{
    return std::is_heap(first, first + n);
}

using std::is_heap_until;// NOLINT(misc-unused-using-decls)
template <typename RandIt, typename Comparator, typename P>
ASTL_NODISCARD auto is_heap_until(RandIt first, RandIt last, Comparator comp, P p) -> RandIt
{
    return std::is_heap_until(first, last, astl::combine(astl::pass_fn(comp), astl::pass_fn(p)));
}

template <typename RandIt, typename N, typename Comparator, typename P>
ASTL_NODISCARD auto is_heap_until_n(RandIt first, N n, Comparator comp, P p) -> RandIt
{
    return std::is_heap_until(first, first + n,
                              astl::combine(astl::pass_fn(comp), astl::pass_fn(p)));
}

template <typename RandIt, typename N, typename Comparator>
// requires RandIt RandomAccessIterator
// requires N integral type
// requires Comparator is StrictWeakOrdering on the value_type(RandIt)
ASTL_NODISCARD auto is_heap_until_n(RandIt first, N n, Comparator comp) -> RandIt
{
    return std::is_heap_until(first, first + n, astl::pass_fn(comp));
}

template <typename RandIt, typename N>
// requires RandIt RandomAccessIterator
// requires N integral type
ASTL_NODISCARD auto is_heap_until_n(RandIt first, N n) -> RandIt
{
    return std::is_heap(first, first + n);
}

template <typename RandIt, typename Comparator, typename P>
auto make_heap(RandIt first, RandIt last, Comparator comp, P p) -> RandIt
{
    std::make_heap(first, last, astl::combine(astl::pass_fn(comp), astl::pass_fn(p)));
    return first;
}

template <typename RandIt, typename Comparator>
// requires RandIt RandomAccessIterator
// requires Comparator is StrictWeakOrdering on the value_type(RandIt)
auto make_heap(RandIt first, RandIt last, Comparator comp) -> RandIt
{
    std::make_heap(first, last, astl::pass_fn(comp));
    return first;
}

template <typename RandIt>
// requires RandIt RandomAccessIterator
auto make_heap(RandIt first, RandIt last) -> RandIt
{
    std::make_heap(first, last);
    return first;
}

template <typename RandIt, typename N, typename Comparator, typename P>
auto make_heap_n(RandIt first, N n, Comparator comp, P p) -> RandIt
{
    return i::make_heap(first, first + n, astl::combine(astl::pass_fn(comp), astl::pass_fn(p)));
}

template <typename RandIt, typename N, typename Comparator>
// requires RandIt RandomAccessIterator
// requires N integral type
// requires Comparator is StrictWeakOrdering on the value_type(RandIt)
auto make_heap_n(RandIt first, N n, Comparator comp) -> RandIt
{
    return i::make_heap(first, first + n, astl::pass_fn(comp));
}

template <typename RandIt, typename N>
// requires RandIt RandomAccessIterator
// requires N integral type
auto make_heap_n(RandIt first, N n) -> RandIt
{
    return std::make_heap(first, first + n);
}

using std::pop_heap;// NOLINT(misc-unused-using-decls)
template <typename RandIt, typename Comparator, typename P>
auto pop_heap(RandIt first, RandIt last, Comparator comp, P p) -> void
{
    std::pop_heap(first, last, astl::combine(astl::pass_fn(comp), astl::pass_fn(p)));
}

template <typename RandIt, typename N, typename Comparator, typename P>
auto pop_heap_n(RandIt first, N n, Comparator comp, P p) -> void
{
    std::pop_heap(first, first + n, astl::combine(astl::pass_fn(comp), astl::pass_fn(p)));
}

template <typename RandIt, typename N, typename Comparator>
// requires RandIt RandomAccessIterator
// requires N integral type
// requires Comparator is StrictWeakOrdering on the value_type(RandIt)
auto pop_heap_n(RandIt first, N n, Comparator comp) -> void
{
    std::pop_heap(first, first + n, astl::pass_fn(comp));
}

template <typename RandIt, typename N>
// requires RandIt RandomAccessIterator
// requires N integral type
auto pop_heap_n(RandIt first, N n) -> void
{
    std::pop_heap(first, first + n);
}

using std::push_heap;// NOLINT(misc-unused-using-decls)
template <typename RandIt, typename Comparator, typename P>
auto push_heap(RandIt first, RandIt last, Comparator comp, P p) -> void
{
    std::push_heap(first, last, astl::combine(astl::pass_fn(comp), astl::pass_fn(p)));
}

template <typename RandIt, typename N, typename Comparator, typename P>
auto push_heap_n(RandIt first, N n, Comparator comp, P p) -> void
{
    std::push_heap(first, first + n, astl::combine(astl::pass_fn(comp), astl::pass_fn(p)));
}

template <typename RandIt, typename N, typename Comparator>
// requires RandIt RandomAccessIterator
// requires N integral type
// requires Comparator is StrictWeakOrdering on the value_type(RandIt)
auto push_heap_n(RandIt first, N n, Comparator comp) -> void
{
    std::push_heap(first, first + n, astl::pass_fn(comp));
}

template <typename RandIt, typename N>
// requires RandIt RandomAccessIterator
// requires N integral type
auto push_heap_n(RandIt first, N n) -> void
{
    std::push_heap(first, first + n);
}

using std::sort_heap;// NOLINT(misc-unused-using-decls)
template <typename RandIt, typename Comparator, typename P>
auto sort_heap(RandIt first, RandIt last, Comparator comp, P p) -> void
{
    std::sort_heap(first, last, astl::combine(astl::pass_fn(comp), astl::pass_fn(p)));
}

template <typename RandIt, typename N, typename Comparator, typename P>
auto sort_heap_n(RandIt first, N n, Comparator comp, P p) -> void
{
    std::sort_heap(first, first + n, astl::combine(astl::pass_fn(comp), astl::pass_fn(p)));
}

template <typename RandIt, typename N, typename Comparator>
// requires RandIt RandomAccessIterator
// requires N integral type
// requires Comparator is StrictWeakOrdering on the value_type(RandIt)
auto sort_heap_n(RandIt first, N n, Comparator comp) -> void
{
    std::sort_heap(first, first + n, astl::pass_fn(comp));
}

template <typename RandIt, typename N>
// requires RandIt RandomAccessIterator
// requires N integral type
auto sort_heap_n(RandIt first, N n) -> void
{
    std::sort_heap(first, first + n);
}
}// namespace i

namespace r
{
template <typename R>
// requires R RandomAccessIterator range
ASTL_NODISCARD auto is_heap(R &&r) -> bool
{
    return i::is_heap(adl::begin(r), adl::end(r));
}

template <typename R, typename Comparator>
// requires R RandomAccessIterator range
// requires Comparator is StrictWeakOrdering on the value_type(R)
ASTL_NODISCARD auto is_heap(R &&r, Comparator comp) -> bool
{
    return i::is_heap(adl::begin(r), adl::end(r), astl::pass_fn(comp));
}

template <typename R, typename Comparator, typename P>
ASTL_NODISCARD auto is_heap(R &&r, Comparator comp, P p) -> bool
{
    return i::is_heap(adl::begin(r), adl::end(r), astl::pass_fn(comp), astl::pass_fn(p));
}

template <typename R, typename N>
// requires R RandomAccessIterator range
ASTL_NODISCARD auto is_heap_n(R &&r, N n) -> bool
{
    return i::is_heap_n(adl::begin(r), n);
}

template <typename R, typename N, typename Comparator>
// requires R RandomAccessIterator range
// requires N integral type
// requires Comparator is StrictWeakOrdering on the value_type(R)
ASTL_NODISCARD auto is_heap_n(R &&r, N n, Comparator comp) -> bool
{
    return i::is_heap_n(adl::begin(r), n, astl::pass_fn(comp));
}

template <typename R, typename N, typename Comparator, typename P>
ASTL_NODISCARD auto is_heap_n(R &&r, N n, Comparator comp, P p) -> bool
{
    return i::is_heap_n(adl::begin(r), n, astl::pass_fn(comp), astl::pass_fn(p));
}

template <typename R>
// requires R RandomAccessIterator range
ASTL_NODISCARD auto is_heap_until(R &&r) -> bool
{
    return i::is_heap_until(adl::begin(r), adl::end(r));
}

template <typename R, typename Comparator>
// requires R RandomAccessIterator range
// requires Comparator is StrictWeakOrdering on the value_type(R)
ASTL_NODISCARD auto is_heap_until(R &&r, Comparator comp) -> bool
{
    return i::is_heap_until(adl::begin(r), adl::end(r), astl::pass_fn(comp));
}

template <typename R, typename Comparator, typename P>
ASTL_NODISCARD auto is_heap_until(R &&r, Comparator comp, P p) -> bool
{
    return i::is_heap_until(adl::begin(r), adl::end(r), astl::pass_fn(comp), astl::pass_fn(p));
}

template <typename R, typename N>
// requires R RandomAccessIterator range
ASTL_NODISCARD auto is_heap_until_n(R &&r, N n) -> bool
{
    return i::is_heap_until_n(adl::begin(r), n);
}
template <typename R, typename N, typename Comparator>
// requires R RandomAccessIterator range
// requires N integral type
// requires Comparator is StrictWeakOrdering on the value_type(R)
ASTL_NODISCARD auto is_heap_until_n(R &&r, N n, Comparator comp) -> bool
{
    return i::is_heap_until_n(adl::begin(r), n, astl::pass_fn(comp));
}

template <typename R, typename N, typename Comparator, typename P>
ASTL_NODISCARD auto is_heap_until_n(R &&r, N n, Comparator comp, P p) -> bool
{
    return i::is_heap_until_n(adl::begin(r), n, astl::pass_fn(comp), astl::pass_fn(p));
}

template <typename R>
// requires R RandomAccessIterator range
auto make_heap(R &&r) -> iter_of_range<R>
{
    return i::make_heap(adl::begin(r), adl::end(r));
}

template <typename R, typename Comparator>
// requires R RandomAccessIterator range
// requires Comparator is StrictWeakOrdering on the value_type(R)
auto make_heap(R &&r, Comparator comp) -> iter_of_range<R>
{
    return i::make_heap(adl::begin(r), adl::end(r), astl::pass_fn(comp));
}

template <typename R, typename Comparator, typename P>
auto make_heap(R &&r, Comparator comp, P p) -> iter_of_range<R>
{
    return i::make_heap(adl::begin(r), adl::end(r), astl::pass_fn(comp), astl::pass_fn(p));
}

template <typename R, typename N>
// requires R RandomAccessIterator range
auto make_heap_n(R &&r, N n) -> iter_of_range<R>
{
    return i::make_heap_n(adl::begin(r), n);
}

template <typename R, typename N, typename Comparator>
// requires R RandomAccessIterator range
// requires N integral type
// requires Comparator is StrictWeakOrdering on the value_type(R)
auto make_heap_n(R &&r, N n, Comparator comp) -> iter_of_range<R>
{
    return i::make_heap_n(adl::begin(r), n, astl::pass_fn(comp));
}

template <typename R, typename N, typename Comparator, typename P>
auto make_heap_n(R &&r, N n, Comparator comp, P p) -> iter_of_range<R>
{
    return i::make_heap_n(adl::begin(r), n, astl::pass_fn(comp), astl::pass_fn(p));
}

template <typename R>
// requires R RandomAccessIterator range
auto pop_heap(R &&r) -> void
{
    return i::pop_heap(adl::begin(r), adl::end(r));
}

template <typename R, typename Comparator>
// requires R RandomAccessIterator range
// requires Comparator is StrictWeakOrdering on the value_type(R)
auto pop_heap(R &&r, Comparator comp) -> void
{
    return i::pop_heap(adl::begin(r), adl::end(r), astl::pass_fn(comp));
}

template <typename R, typename Comparator, typename P> auto pop_heap(R &&r, Comparator comp, P p) -> void
{
    return i::pop_heap(adl::begin(r), adl::end(r), astl::pass_fn(comp), astl::pass_fn(p));
}

template <typename R, typename N>
// requires R RandomAccessIterator range
auto pop_heap_n(R &&r, N n) -> void
{
    i::pop_heap_n(adl::begin(r), n);
}

template <typename R, typename N, typename Comparator>
// requires R RandomAccessIterator range
// requires N integral type
// requires Comparator is StrictWeakOrdering on the value_type(R)
auto pop_heap_n(R &&r, N n, Comparator comp) -> void
{
    i::pop_heap_n(adl::begin(r), n, astl::pass_fn(comp));
}

template <typename R, typename N, typename Comparator, typename P>
auto pop_heap_n(R &&r, N n, Comparator comp, P p) -> void
{
    i::pop_heap_n(adl::begin(r), n, astl::pass_fn(comp), astl::pass_fn(p));
}

template <typename R>
// requires R RandomAccessIterator range
auto push_heap(R &&r) -> void
{
    return i::push_heap(adl::begin(r), adl::end(r));
}

template <typename R, typename Comparator>
// requires R RandomAccessIterator range
// requires Comparator is StrictWeakOrdering on the value_type(R)
auto push_heap(R &&r, Comparator comp) -> void
{
    return i::push_heap(adl::begin(r), adl::end(r), astl::pass_fn(comp));
}

template <typename R, typename Comparator, typename P> auto push_heap(R &&r, Comparator comp, P p) -> void
{
    return i::push_heap(adl::begin(r), adl::end(r), astl::pass_fn(comp), astl::pass_fn(p));
}

template <typename R, typename N>
// requires R RandomAccessIterator range
auto push_heap_n(R &&r, N n) -> void
{
    i::push_heap_n(adl::begin(r), n);
}

template <typename R, typename N, typename Comparator>
// requires R RandomAccessIterator range
// requires N integral type
// requires Comparator is StrictWeakOrdering on the value_type(R)
auto push_heap_n(R &&r, N n, Comparator comp) -> void
{
    i::push_heap_n(adl::begin(r), n, astl::pass_fn(comp));
}

template <typename R, typename N, typename Comparator, typename P>
auto push_heap_n(R &&r, N n, Comparator comp, P p) -> void
{
    i::push_heap_n(adl::begin(r), n, astl::pass_fn(comp), astl::pass_fn(p));
}

template <typename R>
// requires R RandomAccessIterator range
// requires Comparator is StrictWeakOrdering on the value_type(R)
auto sort_heap(R &&r) -> void
{
    return i::sort_heap(adl::begin(r), adl::end(r));
}

template <typename R, typename Comparator>
// requires R RandomAccessIterator range
// requires Comparator is StrictWeakOrdering on the value_type(R)
auto sort_heap(R &&r, Comparator comp) -> void
{
    return i::sort_heap(adl::begin(r), adl::end(r), astl::pass_fn(comp));
}

template <typename R, typename Comparator, typename P> auto sort_heap(R &&r, Comparator comp, P p) -> void
{
    return i::sort_heap(adl::begin(r), adl::end(r), astl::pass_fn(comp), astl::pass_fn(p));
}

template <typename R, typename N>
// requires R RandomAccessIterator range
auto sort_heap_n(R &&r, N n) -> void
{
    i::sort_heap_n(adl::begin(r), n);
}

template <typename R, typename N, typename Comparator>
// requires R RandomAccessIterator range
// requires N integral type
// requires Comparator is StrictWeakOrdering on the value_type(R)
auto sort_heap_n(R &&r, N n, Comparator comp) -> void
{
    i::sort_heap_n(adl::begin(r), n, astl::pass_fn(comp));
}

template <typename R, typename N, typename Comparator, typename P>
auto sort_heap_n(R &&r, N n, Comparator comp, P p) -> void
{
    i::sort_heap_n(adl::begin(r), n, astl::pass_fn(comp), astl::pass_fn(p));
}
}// namespace r
}// namespace astl

#endif// ASTL_INCLUDE_HEAP_HPP
