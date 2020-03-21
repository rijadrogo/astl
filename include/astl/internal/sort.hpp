//
// Created by Rijad on 03-Aug-18.
//

#ifndef ASTL_INCLUDE_SORT_HPP
#define ASTL_INCLUDE_SORT_HPP

#include <algorithm>
#include <utility>

#include "inplace_merge.hpp"
#include "merge.hpp"
#include "rotate.hpp"
#include "upper_bound.hpp"

#include "astl/functional.hpp"
#include "astl/iterator.hpp"
#include "astl/range_access.hpp"
#include "astl/temporary_buffer.hpp"

namespace astl
{
namespace internal_sort
{
template <typename BidiIt, typename T, typename Comparator>
auto unguarded_linear_insert(BidiIt last, T val, Comparator comp) -> void
{
    BidiIt previous(last);
    --previous;
    while (comp(val, *previous)) {
        *last = std::move(*previous);
        last = previous;
        --previous;
    }
    *last = std::move(val);
}

template <typename BidiIt, typename Comparator>
auto linear_insert(BidiIt first, BidiIt last, Comparator comp) -> void
{
    auto val(std::move(*last));
    if (comp(val, *first)) {
        std::move_backward(first, last, astl::next(last));
        *first = std::move(val);
    }
    else {
        internal_sort::unguarded_linear_insert(last, val, comp);
    }
}

template <typename FwdIt, typename N, typename Comparator>
// requires FwdIt is ForwardIterator
// requires N is integral
// requires Comparator is StrictWeakOrdering on the value_type(FwdIt)
auto binary_insert_n(FwdIt first, N n, FwdIt current, Comparator comp) -> FwdIt
{
    // precondition is_sorted(first, current, comp)
    // precondition current is a valid iterator
    // precondition distance(first, current) == n
    FwdIt insertion_point(i::upper_bound_n(first, n, *current, comp).first);
    i::rotate_one_right(insertion_point, ++current);
    return insertion_point;
}

template <typename FwdIt, typename Comparator>
// requires FwdIt is ForwardIterator
// requires N is integral
// requires Comparator is StrictWeakOrdering on the value_type(FwdIt)
auto binary_insert(FwdIt first, FwdIt current, Comparator comp) -> FwdIt
{
    // precondition is_sorted(first, current, comp)
    // precondition current is a valid iterator
    // precondition distance(first, current) == n
    FwdIt insertion_point(i::upper_bound(first, current, *current, comp));
    i::rotate_one_right(insertion_point, ++current);
    return insertion_point;
}

} // namespace internal_sort

namespace i
{
template <typename BidiIt, typename Comparator = std::less<>>
// requires BidiIt Bidirectional iterator
// requires Comparator is StrictWeakOrdering on the value_type(BidiIt)
auto insertion_sort(BidiIt first, BidiIt last, Comparator comp = Comparator{}) -> void;

template <typename BidiIt, typename Comparator, typename P>
auto insertion_sort(BidiIt first, BidiIt last, Comparator comp, P p) -> void;
} // namespace i

namespace internal_sort
{
template <typename BidiIt, typename Comparator>
auto inplace_stable_sort1(BidiIt first, BidiIt last, Comparator comp) -> void
{
    if (astl::next(first, 17, last) == last) return i::insertion_sort(first, last, comp);

    BidiIt mid(astl::middle(first, last));
    internal_sort::inplace_stable_sort1(first, mid, comp);
    internal_sort::inplace_stable_sort1(mid, last, comp);
    internal_in_merge::merge_without_buffer(first, mid, last, astl::distance(first, mid),
                                            astl::distance(mid, last), comp);
}
} // namespace internal_sort

namespace i
{
template <typename FwdIt, typename Comparator = std::less<>>
// requires FwdIt is ForwardIterator
// requires  Comparator is StrictWeakOrdering on the value_type(FwdIt)
auto binary_insertion_sort(FwdIt first, FwdIt last, Comparator comp = Comparator{}) -> void
{
    if (first == last) return;

    FwdIt current(first);
    ++current;
    while (current != last) {
        // invariant: is_sorted_n(first, i, comp) && std::distance(first, current) ==
        // i
        internal_sort::binary_insert(first, current, astl::pass_fn(comp));
        ++current;
    }
}

template <typename FwdIt, typename Comparator, typename P>
auto binary_insertion_sort(FwdIt first, FwdIt last, Comparator comp, P p) -> void
{
    return i::binary_insertion_sort(first, last,
                                    astl::combine(astl::pass_fn(comp), astl::pass_fn(p)));
}

template <typename FwdIt, typename N, typename Comparator = std::less<>>
// requires FwdIt is ForwardIterator
// requires N is integral type
// requires  Comparator is StrictWeakOrdering on the value_type(FwdIt)
auto binary_insertion_sort_n(FwdIt first, N n, Comparator comp = Comparator{}) -> FwdIt
{
    if (n == N(0)) return first;

    FwdIt current(first);
    ++current;
    N i(1);
    while (i < n)
        // invariant: is_sorted_n(first, i, comp) && std::distance(first, current) == i
        internal_sort::binary_insert_n(first, i++, current++, astl::pass_fn(comp));

    return current;
}

template <typename FwdIt, typename N, typename Comparator, typename P>
auto binary_insertion_sort_n(FwdIt first, N n, Comparator comp, P p) -> FwdIt
{
    return i::binary_insertion_sort_n(first, n,
                                      astl::combine(astl::pass_fn(comp), astl::pass_fn(p)));
}

template <typename BidiIt, typename Comparator = std::less<>>
// requires BidiIt ForwardIterator
// requires Comparator is StrictWeakOrdering on the value_type (BidiIt)
auto bubble_sort(BidiIt first, BidiIt const last, Comparator comp = Comparator{}) -> void
{
    using std::swap;
    auto j_sent(last);
    BidiIt i(first);
    while (i != last) {
        BidiIt j(first);
        while (j != j_sent) {
            auto j_next(j);
            if (comp(*++j_next, *j)) swap(*j_next, *j);

            ++j;
        }
        --j_sent;
        ++i;
    }
}

template <typename BidiIt, typename Comparator, typename P>
auto bubble_sort(BidiIt first, BidiIt last, Comparator comp, P p) -> void
{
    i::bubble_sort(first, last, astl::combine(astl::pass_fn(comp), astl::pass_fn(p)));
}

template <typename RandIt, typename Comparator = std::less<>>
// requires RandIt Random access iterator
// requires Comparator is StrictWeakOrdering on the value_type (RandIt)
auto heap_sort(RandIt first, RandIt last, Comparator comp = Comparator{}) -> void
{
    auto pred(astl::pass_fn(comp));
    std::make_heap(first, last, pred);
    std::sort_heap(first, last, pred);
}

template <typename RandIt, typename Comparator, typename P>
auto heap_sort(RandIt first, RandIt last, Comparator comp, P p) -> void
{
    i::heap_sort(first, last, astl::combine(astl::pass_fn(comp), astl::pass_fn(p)));
}

// same as stable_sort, guarantees no allocation
// complexity O(N*log(N)^2)
template <typename BidiIt, typename Comparator = std::less<>>
// requires BidiIt Bidirectional iterator
// requires Comparator is StrictWeakOrdering on the value_type(BidiIt)
auto inplace_stable_sort(BidiIt first, BidiIt last, Comparator comp = Comparator{}) -> void
{
    internal_sort::inplace_stable_sort1(first, last, astl::pass_fn(comp));
}

// same as stable_sort, guarantees no allocation
// complexity O(N*log(N)^2)
template <typename BidiIt, typename Comparator, typename P>
auto inplace_stable_sort(BidiIt first, BidiIt last, Comparator comp, P p) -> void
{
    i::inplace_stable_sort(first, last, astl::combine(astl::pass_fn(comp), astl::pass_fn(p)));
}

template <typename FwdIt, typename Comparator>
// requires FwdIt Bidirectional iterator
// requires Comparator is StrictWeakOrdering on the value_type(I)
auto insertion_sort(FwdIt first, FwdIt const last, Comparator comp) -> void
{
    if (first == last) return;

    if constexpr (is_bidirectional_it_v<FwdIt>) { // Bidirectional Iterator
        using BidiIt = FwdIt;
        BidiIt i(astl::next(first));
        while (i != last) {
            internal_sort::linear_insert(first, i, comp);
            ++i;
        }
    }
    else { // Forward Iterator
        for (auto it = first; it != last; ++it) {
            FwdIt const insertion(i::upper_bound(first, it, *it, comp));
            (void) std::rotate(insertion, it, astl::next(it));
        }
    }
}

template <typename BidiIt, typename Comparator, typename P>
auto insertion_sort(BidiIt first, BidiIt last, Comparator comp, P p) -> void
{
    i::insertion_sort(first, last, astl::combine(astl::pass_fn(comp), astl::pass_fn(p)));
}

using std::is_sorted;
template <typename FwdIt, typename Comparator, typename P>
auto is_sorted(FwdIt first, FwdIt last, Comparator comp, P p) -> bool
{
    return std::is_sorted(first, last, astl::combine(astl::pass_fn(comp), astl::pass_fn(p)));
}

using std::is_sorted_until; // NOLINT(misc-unused-using-decls)
template <typename FwdIt, typename Comparator, typename P>
auto is_sorted_until(FwdIt first, FwdIt last, Comparator comp, P p) -> bool
{
    return std::is_sorted_until(first, last, astl::combine(astl::pass_fn(comp), astl::pass_fn(p)));
}

template <typename FwdIt, typename N, typename Comparator = std::less<>>
// requires FwdIt is ForwardIterator
// requires N is integral type
// requires Comparator is StrictWeakOrdering on the value_type(FwdIt)
auto is_sorted_n(FwdIt first, N n, Comparator comp = Comparator{}) -> bool
{
    if (n == N(0)) return true;

    FwdIt previous(first);
    while (n != N(0) && !comp(*++first, *previous)) {
        previous = first;
        --n;
    }
    return n != N(0);
}

template <typename FwdIt, typename N, typename Comparator, typename P>
auto is_sorted_n(FwdIt first, N n, Comparator comp, P p) -> bool
{
    return i::is_sorted_n(first, n, astl::combine(astl::pass_fn(comp), astl::pass_fn(p)));
}

template <typename I, typename Comparator = std::less<>>
// requires I ForwardIterator
// requires Comparator is StrictWeakOrdering on the value_type (I)
auto median_of_three_iterators(I a, I b, I c, Comparator comp = Comparator{}) -> I
{
    if (comp(*a, *b)) {
        if (comp(*b, *c)) return b;

        if (comp(*a, *c)) return comp;

        return a;
    }
    // Just swap a and b.
    if (comp(*a, *c)) return a;

    if (comp(*b, *c)) return comp;

    return b;
}

template <typename I, typename Comparator, typename P>
auto median_of_three_iterators(I a, I b, I c, Comparator comp, P p) -> I
{
    return i::median_of_three_iterators(a, b, c,
                                        astl::combine(astl::pass_fn(comp), astl::pass_fn(p)));
}

// stable_sort
template <typename BidiIt, typename Comparator = std::less<>>
// requires BidiIt Bidirectional iterator
// requires Comparator is StrictWeakOrdering on the value_type (BidiIt)
auto merge_sort(BidiIt first, BidiIt last, Comparator comp = Comparator{}) -> void
{
    if (first == last || astl::next(first) == last) return;

    auto cmp(astl::pass_fn(comp));
    BidiIt mid(astl::middle(first, last));
    i::merge_sort(first, mid, cmp);
    i::merge_sort(mid, last, cmp);
    std::inplace_merge(first, mid, last, cmp);
}

template <typename BidiIt, typename Comparator, typename P>
auto merge_sort(BidiIt first, BidiIt last, Comparator comp, P p) -> void
{
    i::merge_sort(first, last, astl::combine(astl::pass_fn(comp), astl::pass_fn(p)));
}

template <typename FwdIt, typename N, typename Comparator, typename B, typename Size>
// requires FwdIt is ForwardIterator
// requires N is integral type
// requires Comparator is StrictWeakOrdering on the value_type (FwdIt)
// requires B ForwardIterator
// requires Size integral type
auto merge_sort_n(FwdIt first, N n, Comparator comp, B buffer, Size buffer_size) -> FwdIt
{
    if (n == N(0)) return first;

    auto cmp(astl::pass_fn(comp));
    constexpr N bsort_cuttof(32);
    if (n < bsort_cuttof) return i::binary_insertion_sort_n(first, n, cmp);

    N half(n >> 1);
    if (half == N(0)) return ++first;

    FwdIt mid(i::merge_sort_n(first, half, cmp, buffer, buffer_size));
    FwdIt last(i::merge_sort_n(mid, n - half, cmp, buffer, buffer_size));
    i::merge_adaptive_n(first, half, mid, n - half, cmp, buffer, buffer_size);
    return last;
}

template <typename FwdIt, typename N, typename Comparator, typename B, typename P>
auto merge_sort_n(FwdIt first, N n, Comparator comp, B buffer, N buffer_size, P p) -> FwdIt
{
    return i::merge_sort_n(first, n, astl::combine(astl::pass_fn(comp), astl::pass_fn(p)), buffer,
                           buffer_size);
}

template <typename FwdIt, typename N, typename Comparator = std::less<>>
// requires FwdIt is ForwardIterator
// requires N is integral type
// requires Comparator is StrictWeakOrdering on the value_type (FwdIt)
auto merge_sort_n(FwdIt first, N n, Comparator comp = Comparator{}) -> FwdIt
{
    N half(n >> 1);
    if (half == N(0)) return ++first;

    using T = iter_value_type<FwdIt>;
    inline_temporary_buffer<T> buffer(n - half, *first);
    return i::merge_sort_n(first, n, astl::pass_fn(comp), buffer.begin(), N(buffer.size()));
}

template <typename FwdIt, typename N, typename Comparator, typename P>
auto merge_sort_n(FwdIt first, N n, Comparator comp, P p) -> FwdIt
{
    return i::merge_sort_n(first, n, astl::combine(astl::pass_fn(comp), astl::pass_fn(p)));
}

/// Swaps the median value of *a, *b and *comp under comp to *result
template <typename I, typename Comparator = std::less<>>
// requires I ForwardIterator
// requires Comparator is StrictWeakOrdering on the value_type (I)
auto move_median_to_first(I result, I a, I b, I c, Comparator comp = Comparator{}) -> void
{
    using std::swap;
    if (comp(*a, *b)) {
        if (comp(*b, *c)) swap(*result, *b);
        else if (comp(*a, *c))
            swap(*result, *c);
        else
            swap(*result, *a);
    }
    else if (comp(*a, *c))
        swap(*result, *a);
    else if (comp(*b, *c))
        swap(*result, *c);
    else
        swap(*result, *b);
}

template <typename I, typename Comparator, typename P>
auto move_median_to_first(I result, I a, I b, I c, Comparator comp, P p) -> void
{
    i::move_median_to_first(result, a, b, c, astl::combine(astl::pass_fn(comp), astl::pass_fn(p)));
}

using std::partial_sort; // NOLINT(misc-unused-using-decls)
template <typename RandIt, typename Comparator, typename P>
auto partial_sort(RandIt first, RandIt middle, RandIt last, Comparator comp, P p) -> void
{
    std::partial_sort(first, middle, last, astl::combine(astl::pass_fn(comp), astl::pass_fn(p)));
}

using std::partial_sort_copy; // NOLINT(misc-unused-using-decls)
template <typename InputIt, typename RandIt, typename Comparator, typename P>
auto partial_sort_copy(InputIt first, InputIt last, RandIt d_first, RandIt d_last, Comparator comp,
                       P p) -> RandIt
{
    return std::partial_sort_copy(first, last, d_first, d_last,
                                  astl::combine(astl::pass_fn(comp), astl::pass_fn(p)));
}

template <typename RandIt, typename Comparator = std::less<>>
// requires RandIt RandomAccessIterator
// requires Comparator is StrictWeakOrdering on the value_type (RandIt)
auto partial_sort_left(RandIt first, RandIt middle, RandIt last, Comparator comp = Comparator{})
    -> void
{
    auto pred(astl::pass_fn(comp));
    std::nth_element(first, middle, last, pred);
    std::sort(first, middle, pred);
}

template <typename RandIt, typename Comparator, typename P>
auto partial_sort_left(RandIt first, RandIt middle, RandIt last, Comparator comp, P p) -> void
{
    i::partial_sort_left(first, middle, last, astl::combine(astl::pass_fn(comp), astl::pass_fn(p)));
}

template <typename InputIt, typename RandIt>
// requires InputIt InputIterator
// requires RandIt random access iterator
auto partial_sort_move(InputIt first, InputIt last, RandIt d_first, RandIt d_last) -> RandIt
{
    return std::partial_sort_copy(std::make_move_iterator(first), std::make_move_iterator(last),
                                  d_first, d_last);
}

template <typename InputIt, typename RandIt, typename Comparator>
// requires InputIt ForwardIterator
// requires RandIt random access iterator
// requires Comparator is StrictWeakOrdering on the value_type (InputIt)
auto partial_sort_move(InputIt first, InputIt last, RandIt d_first, RandIt d_last, Comparator comp)
    -> RandIt
{
    return std::partial_sort_copy(std::make_move_iterator(first), std::make_move_iterator(last),
                                  d_first, d_last, astl::pass_fn(comp));
}

template <typename InputIt, typename RandIt, typename Comparator, typename P>
auto partial_sort_move(InputIt first, InputIt last, RandIt d_first, RandIt d_last, Comparator comp,
                       P p) -> RandIt
{
    return i::partial_sort_copy(std::make_move_iterator(first), std::make_move_iterator(last),
                                d_first, d_last, astl::pass_fn(comp), astl::pass_fn(p));
}

template <typename RandIt, typename Comparator = std::less<>>
// requires RandIt RandomAccessIterator
// requires Comparator is StrictWeakOrdering on the value_type (RandIt)
auto partial_sort_right(RandIt first, RandIt middle, RandIt last, Comparator comp = Comparator{})
    -> void
{
    auto pred(astl::pass_fn(comp));
    std::nth_element(first, middle, last, pred);
    std::sort(++middle, last, pred);
}

template <typename RandIt, typename Comparator, typename P>
auto partial_sort_right(RandIt first, RandIt middle, RandIt last, Comparator comp, P p) -> void
{
    i::partial_sort_right(first, middle, last,
                          astl::combine(astl::pass_fn(comp), astl::pass_fn(p)));
}

template <typename FwdIt, typename Comparator = std::less<>>
// requires FwdIt ForwardIterator
// requires Comparator is StrictWeakOrdering on the value_type (FwdIt)
auto selection_sort(FwdIt first, FwdIt last, Comparator comp = Comparator{}) -> void;

template <typename FwdIt, typename Comparator = std::less<>>
// requires FwdIt ForwardIterator
// requires Comparator is StrictWeakOrdering on the value_type (FwdIt)
auto quicksort(FwdIt first, FwdIt last, Comparator comp = Comparator{}) -> void
{
    if (first == last) return;

    auto cmp(astl::pass_fn(comp));
    auto it(first);
    constexpr int bsort_cuttof(32);
    astl::advance(it, bsort_cuttof, last);
    if (it == last) {
        if constexpr (astl::is_bidirectional_it_v<FwdIt>) { // Bidirectional Iterator
            i::insertion_sort(first, last, cmp);
        }
        else { // Forward Iterator
            i::selection_sort(first, last, cmp);
        }
        return;
    }
    iter_diff_type<FwdIt> dist(astl::distance(first, last));
    auto pivot(*astl::next(first, (dist >> 1)));
    FwdIt mid1(std::partition(first, last, [&pivot, cmp](auto &&em) { return cmp(em, pivot); }));
    FwdIt mid2(std::partition(mid1, last, [&pivot, cmp](auto &&em) { return !cmp(pivot, em); }));
    i::quicksort(first, mid1, cmp);
    i::quicksort(mid2, last, cmp);
}

template <typename FwdIt, typename Comparator, typename P>
auto quicksort(FwdIt first, FwdIt last, Comparator comp, P p) -> void
{
    i::quicksort(first, last, astl::combine(astl::pass_fn(comp), astl::pass_fn(p)));
}

template <typename FwdIt, typename Comparator /* = less */>
// requires FwdIt ForwardIterator
// requires Comparator is StrictWeakOrdering on the value_type (FwdIt)
auto selection_sort(FwdIt first, FwdIt last, Comparator comp /* = std::less{} */) -> void
{
    auto cmp(astl::pass_fn(comp));
    while (first != last) {
        using std::swap;
        swap(*first, *std::min_element(first, last, cmp));
        ++first;
    }
}

template <typename FwdIt, typename Comparator, typename P>
auto selection_sort(FwdIt first, FwdIt last, Comparator comp, P p) -> void
{
    i::selection_sort(first, last, astl::combine(astl::pass_fn(comp), astl::pass_fn(p)));
}

template <typename RandIt, typename Comparator = std::less<>>
// requires RandIt RandomAccessIterator
// requires Comparator is StrictWeakOrdering on the value_type(RandIt)
auto shell_sort(RandIt first, RandIt const last, Comparator comp = Comparator{}) -> void
{
    using difference_type = typename std::iterator_traits<RandIt>::difference_type;

    // We use the Knuth 'h' sequence below, as it is easy to calculate at runtime.
    // However, possibly we are better off using a different sequence based on a
    // table. One such sequence which averages slightly better than Knuth is:
    //    1, 5, 19, 41, 109, 209, 505, 929, 2161, 3905, 8929, 16001, 36289,
    //    64769, 146305, 260609, 587521, 1045505, 2354689, 4188161, 9427969,
    //    16764929

    if (first == last) return;

    RandIt i_current;
    difference_type n_size(last - first);
    difference_type n_space(1); // nSpace is the 'h' value of the ShellSort algorithm.

    // This is the Knuth 'h' sequence: 1, 4, 13, 40, 121, 364, 1093, 3280, 9841,
    // 29524, 88573, 265720, 797161, 2391484, 7174453, 21523360, 64570081,
    // 193710244,
    while (n_space < n_size) n_space = (n_space * 3) + 1;

    // Integer division is less than ideal.
    n_space = (n_space - 1) / 3;
    while (n_space >= 1) {
        difference_type i(0);
        while (i < n_space) {
            RandIt i_insert_first(first + i);
            RandIt i_sorted(i_insert_first + n_space);
            while (i_sorted < last) {
                RandIt i_back(i_current);
                i_current = i_sorted;
                i_back -= n_space;
                while (i_current != i_insert_first && comp(*i_current, *i_back)) {
                    using std::swap;
                    swap(*i_current, *i_back);
                    i_current = i_back;
                    i_back -= n_space;
                }
                i_sorted += n_space;
            }
            ++i;
        }
        n_space = (n_space - 1) / 3;
    }
}

template <typename RandIt, typename Comparator, typename P>
auto shell_sort(RandIt first, RandIt last, Comparator comp, P p) -> void
{
    i::shell_sort(first, last, astl::combine(astl::pass_fn(comp), astl::pass_fn(p)));
}

// stable, 1 compare, 0-1 swap
template <typename Comparator, typename FwdIt>
// requires FwdIt ForwardIterator
// requires Comparator is StrictWeakOrdering on the value_type (FwdIt)
auto sort2(FwdIt x, FwdIt y, Comparator comp) -> unsigned
{
    unsigned r(0);
    if (comp(*y, *x)) {
        ++r;
        using std::swap;
        swap(*x, *y);
    }
    return r;
}

template <typename FwdIt, typename Comparator>
// requires FwdIt ForwardIterator
// requires Comparator is StrictWeakOrdering on the value_type (FwdIt)
auto sort2(FwdIt x, Comparator comp) -> unsigned
{
    FwdIt f(x);
    return i::sort2(f, ++x, astl::pass_fn(comp));
}

// stable, 2-3 compares, 0-2 swaps
template <typename FwdIt, typename Comparator>
// requires FwdIt ForwardIterator
// requires Comparator is StrictWeakOrdering on the value_type (FwdIt)
auto sort3(FwdIt x, FwdIt y, FwdIt z, Comparator comp) -> unsigned
{
    using std::swap;
    unsigned r(0);
    if (!comp(*y, *x)) {     // if x <= y
        if (!comp(*z, *y)) { // if y <= z
            return r;        // x <= y && y <= z
        }                    // x <= y && y > z
        swap(*y, *z);        // x <= z && y < z
        r = 1;
        if (comp(*y, *x)) { // if x > y
            swap(*x, *y);   // x < y && y <= z
            r = 2;
        }
        return r; // x <= y && y < z
    }
    if (comp(*z, *y)) { // x > y, if y > z
        swap(*x, *z);   // x < y && y < z
        r = 1;
        return r;
    }
    swap(*x, *y);       // x > y && y <= z
    r = 1;              // x < y && x <= z
    if (comp(*z, *y)) { // if y > z
        swap(*y, *z);   // x <= y && y < z
        r = 2;
    }
    return r;
} // x <= y && y <= z

template <typename FwdIt, typename Comparator>
// requires FwdIt ForwardIterator
// requires Comparator is StrictWeakOrdering on the value_type(FwdIt)
auto sort3(FwdIt first, Comparator comp) -> unsigned
{
    FwdIt x1(first);
    FwdIt x2(++first);
    return i::sort3(x1, x2, ++first, astl::pass_fn(comp));
}

// stable, 3-6 compares, 0-5 swaps
template <typename FwdIt, typename Comparator = std::less<>>
// requires FwdIt ForwardIterator
// requires Comparator is StrictWeakOrdering on the value_type(FwdIt)
auto sort4(FwdIt x1, FwdIt x2, FwdIt x3, FwdIt x4, Comparator comp = Comparator{}) -> unsigned
{
    using std::swap;
    unsigned r(i::sort3(x1, x2, x3, comp));
    if (comp(*x4, *x3)) {
        swap(*x3, *x4);
        ++r;
        if (comp(*x3, *x2)) {
            swap(*x2, *x3);
            ++r;
            if (comp(*x2, *x1)) {
                swap(*x1, *x2);
                ++r;
            }
        }
    }
    return r;
}

template <typename FwdIt, typename Comparator = std::less<>>
// requires FwdIt ForwardIterator
// requires Comparator is StrictWeakOrdering on the value_type(FwdIt)
auto sort4(FwdIt first, Comparator comp = Comparator{}) -> unsigned
{
    FwdIt x1(first);
    FwdIt x2(++first);
    FwdIt x3(++first);
    return i::sort4(x1, x2, x3, ++first, astl::pass_fn(comp));
}

// stable, 4-10 compares, 0-9 swaps

template <typename FwdIt, typename Comparator = std::less<>>
// requires FwdIt ForwardIterator
// requires Comparator is StrictWeakOrdering on the value_type(FwdIt)
auto sort5(FwdIt x1, FwdIt x2, FwdIt x3, FwdIt x4, FwdIt x5, Comparator comp = Comparator{})
    -> unsigned
{
    using std::swap;
    unsigned r(i::sort4(x1, x2, x3, x4, comp));
    if (comp(*x5, *x4)) {
        swap(*x4, *x5);
        ++r;
        if (comp(*x4, *x3)) {
            swap(*x3, *x4);
            ++r;
            if (comp(*x3, *x2)) {
                swap(*x2, *x3);
                ++r;
                if (comp(*x2, *x1)) {
                    swap(*x1, *x2);
                    ++r;
                }
            }
        }
    }
    return r;
}

template <typename FwdIt, typename Comparator = std::less<>>
// requires FwdIt ForwardIterator
// requires Comparator is StrictWeakOrdering on the value_type(FwdIt)
auto sort5(FwdIt first, Comparator comp = Comparator{}) -> unsigned
{
    FwdIt x1(first);
    FwdIt x2(++first);
    FwdIt x3(++first);
    FwdIt x4(++first);
    return i::sort5(x1, x2, x3, x4, ++first, astl::pass_fn(comp));
}

// stable, 5-15 compares, 0-14 swaps

template <typename FwdIt, typename Comparator = std::less<>>
// requires FwdIt ForwardIterator
// requires Comparator is StrictWeakOrdering on the value_type (FwdIt)
auto sort6(FwdIt x1, FwdIt x2, FwdIt x3, FwdIt x4, FwdIt x5, FwdIt x6,
           Comparator comp = Comparator{}) -> unsigned
{
    using std::swap;
    unsigned r(i::sort5(x1, x2, x3, x4, x5, comp));
    if (comp(*x6, *x5)) {
        swap(*x6, *x5);
        ++r;
        if (comp(*x5, *x4)) {
            swap(*x5, *x4);
            ++r;
            if (comp(*x4, *x3)) {
                swap(*x4, *x3);
                ++r;
                if (comp(*x3, *x2)) {
                    swap(*x3, *x2);
                    ++r;
                    if (comp(*x2, *x1)) {
                        swap(*x2, *x1);
                        ++r;
                    }
                }
            }
        }
    }
    return r;
}

template <typename FwdIt, typename Comparator = std::less<>>
// requires FwdIt ForwardIterator
// requires Comparator is StrictWeakOrdering on the value_type(FwdIt)
auto sort6(FwdIt first, Comparator comp = Comparator{}) -> unsigned
{
    FwdIt x1(first);
    FwdIt x2(++first);
    FwdIt x3(++first);
    FwdIt x4(++first);
    FwdIt x5(++first);
    return i::sort6(x1, x2, x3, x4, x5, ++first, astl::pass_fn(comp));
}

template <typename RandIt, typename Comparator = std::less<>>
// requires RandIt RandomAccessIterator
// requires Comparator is StrictWeakOrdering on the value_type(RandIt)
auto sort(RandIt first, RandIt const last, Comparator comp = Comparator{}) -> void
{
    auto pr(astl::pass_fn(comp));
    switch (last - first) {
    case 0:
    case 1: return;
    case 2: i::sort2(first, pr); return;
    case 3: i::sort3(first, pr); return;
    case 4: i::sort4(first, pr); return;
    case 5: i::sort5(first, pr); return;
    case 6: i::sort6(first, pr); return;
    default: std::sort(first, last, pr);
    }
}

template <typename RandIt, typename Comparator, typename P>
auto sort(RandIt first, RandIt last, Comparator comp, P p) -> void
{
    i::sort(first, last, astl::combine(astl::pass_fn(comp), astl::pass_fn(p)));
}

template <typename RandIt, typename Comparator = std::less<>>
// requires RandIt RandomAccessIterator
// requires Comparator is StrictWeakOrdering on the value_type(RandIt)
auto sort_and_unique(RandIt f, RandIt l, Comparator comp = Comparator{}) -> RandIt
{
    i::sort(f, l, comp);
    return std::unique(f, l, astl::not_fn(comp));
}

template <typename RandIt, typename Comparator, typename P>
auto sort_and_unique(RandIt f, RandIt l, Comparator comp, P p) -> RandIt
{
    return i::sort_and_unique(f, l, astl::combine(astl::pass_fn(comp), astl::pass_fn(p)));
}

/// \brief Sort the subrange [sub_f, sub_l) that is inside
///     the range [f, l) as if you had sorted the entire range.
///
/// \param first   The start of the larger range
/// \param last    The end of the larger range
/// \param sub_f   The start of the sub range
/// \param sub_l   The end of the sub range
/// \param comp       A predicate to use to Comparator the values. comp ( a, b ) returns a
/// boolean.
///
template <typename RandIt, typename Comparator = std::less<>>
// requires RandIt RandomAccessIterator
// requires Comparator is StrictWeakOrdering on the value_type(RandIt)
auto sort_subrange(RandIt first, RandIt last, RandIt sub_f, RandIt sub_l,
                   Comparator comp = Comparator{}) -> std::pair<RandIt, RandIt>
{
    // precondition sub_f <= sub_l
    // precondition first <= sub_f && sub_l <= last
    // the empty sub-range is already sorted.
    if (sub_f == sub_l) return std::make_pair(sub_f, sub_l);

    auto cmp(astl::pass_fn(comp));
    if (sub_f != first) { // sub-range is at the start, don't need to partition
        std::nth_element(first, sub_f, last, cmp);
        ++sub_f;
    }
    std::partial_sort(sub_f, sub_l, last, cmp);
    return std::make_pair(sub_f, sub_l);
}

template <typename RandIt, typename Comparator, typename P>
auto sort_subrange(RandIt first, RandIt last, RandIt sub_f, RandIt sub_l, Comparator comp, P p)
    -> std::pair<RandIt, RandIt>
{
    return i::sort_subrange(first, last, sub_f, sub_l,
                            astl::combine(astl::pass_fn(comp), astl::pass_fn(p)));
}

using std::stable_sort; // NOLINT(misc-unused-using-decls)
template <typename RandIt, typename Comparator, typename P>
auto stable_sort(RandIt first, RandIt last, Comparator comp, P p) -> void
{
    std::stable_sort(first, last, astl::combine(astl::pass_fn(comp), astl::pass_fn(p)));
}

} // namespace i

namespace r
{
template <typename R, typename N, typename Comparator = std::less<>>
// requires R is ForwardIterator range
// requires N is integral
// requires Comparator is StrictWeakOrdering on the value_type(R)
auto binary_insertion_sort_n(R &&r, N n, Comparator comp = Comparator{}) -> iter_of_range<R>
{
    return i::binary_insertion_sort_n(adl::begin(r), n, astl::pass_fn(comp));
}

template <typename R, typename N, typename Comparator, typename P>
auto binary_insertion_sort_n(R &&r, N n, Comparator comp, P p) -> iter_of_range<R>
{
    return i::binary_insertion_sort_n(adl::begin(r), n, astl::pass_fn(comp), astl::pass_fn(p));
}

template <typename R, typename Comparator, typename P>
auto bubble_sort(R &&r, Comparator comp, P p) -> void
{
    i::bubble_sort(adl::begin(r), adl::end(r), astl::pass_fn(comp), astl::pass_fn(p));
}

template <typename R, typename Comparator = std::less<>>
// requires R ForwardIterator range
// requires Comparator is StrictWeakOrdering on the value_type(R)
auto bubble_sort(R &&r, Comparator comp = Comparator{}) -> void
{
    i::bubble_sort(adl::begin(r), adl::end(r), astl::pass_fn(comp));
}

template <typename R, typename Comparator, typename P>
// requires R RandomAccessIterator range
// requires Comparator is StrictWeakOrdering on the value_type(R)
auto heap_sort(R &&r, Comparator comp, P p) -> void
{
    i::heap_sort(adl::begin(r), adl::end(r), astl::pass_fn(comp), astl::pass_fn(p));
}

template <typename R, typename Comparator = std::less<>>
auto heap_sort(R &&r, Comparator comp = Comparator{}) -> void
{
    i::heap_sort(adl::begin(r), adl::end(r), astl::pass_fn(comp));
}

// same as stable_sort, guarantees no allocation
// complexity O(N*log(N)^2)
template <typename R, typename Comparator, typename P>
// requires R BidirectionalIterator range
// requires Comparator is StrictWeakOrdering on the value_type(R)
auto inplace_stable_sort(R &&r, Comparator comp, P p) -> void
{
    i::inplace_stable_sort(adl::begin(r), adl::end(r), astl::pass_fn(comp), astl::pass_fn(p));
}
// same as stable_sort, guarantees no allocation
// complexity O(N*log(N)^2)
template <typename R, typename Comparator = std::less<>>
auto inplace_stable_sort(R &&r, Comparator comp = Comparator{}) -> void
{
    i::inplace_stable_sort(adl::begin(r), adl::end(r), astl::pass_fn(comp));
}

template <typename R, typename Comparator, typename P>
auto insertion_sort(R &&r, Comparator comp, P p) -> void
{
    i::insertion_sort(adl::begin(r), adl::end(r), astl::pass_fn(comp), astl::pass_fn(p));
}

template <typename R, typename Comparator = std::less<>>
// requires R BidirectionalIterator range
// requires Comparator is StrictWeakOrdering on the value_type(R)
auto insertion_sort(R &&r, Comparator comp = Comparator{}) -> void
{
    i::insertion_sort(adl::begin(r), adl::end(r), astl::pass_fn(comp));
}

template <typename R, typename Comparator, typename P>
auto is_sorted(R &&r, Comparator comp, P p) -> bool
{
    return i::is_sorted(adl::begin(r), adl::end(r), astl::pass_fn(comp), astl::pass_fn(p));
}

template <typename R, typename Comparator>
// requires R ForwardIterator range
// requires Comparator is StrictWeakOrdering on the value_type(R)
auto is_sorted(R &&r, Comparator comp) -> bool
{
    return i::is_sorted(adl::begin(r), adl::end(r), astl::pass_fn(comp));
}

template <typename R>
// requires R ForwardIterator range
auto is_sorted(R &&r) -> bool
{
    return i::is_sorted(adl::begin(r), adl::end(r));
}

template <typename R, typename N, typename Comparator = std::less<>>
// requires R ForwardIterator range
// requires Comparator is StrictWeakOrdering on the value_type(R)
auto is_sorted_n(R &&r, N n, Comparator comp = Comparator{}) -> bool
{
    return i::is_sorted_n(adl::begin(r), n, astl::pass_fn(comp));
}

template <typename R, typename N, typename Comparator, typename P>
auto is_sorted_n(R &&r, N n, Comparator comp, P p) -> bool
{
    return i::is_sorted_n(adl::begin(r), n, astl::pass_fn(comp), astl::pass_fn(p));
}

template <typename R, typename Comparator, typename P>
// requires R ForwardIterator range
// requires Comparator is StrictWeakOrdering on the value_type(R)
auto is_sorted_until(R &&r, Comparator comp, P p) -> bool
{
    return i::is_sorted_until(adl::begin(r), adl::end(r), astl::pass_fn(comp), astl::pass_fn(p));
}

template <typename R, typename Comparator>
// requires R ForwardIterator range
// requires Comparator is StrictWeakOrdering on the value_type(R)
auto is_sorted_until(R &&r, Comparator comp) -> bool
{
    return i::is_sorted_until(adl::begin(r), adl::end(r), astl::pass_fn(comp));
}

template <typename R>
// requires R ForwardIterator range
// requires Comparator is StrictWeakOrdering on the value_type(R)
auto is_sorted_until(R &&r) -> bool
{
    return i::is_sorted_until(adl::begin(r), adl::end(r));
}

template <typename R, typename Comparator, typename P>
auto merge_sort(R &&r, Comparator comp, P p) -> void
{
    i::merge_sort(adl::begin(r), adl::end(r), astl::pass_fn(comp), astl::pass_fn(p));
}

template <typename R, typename Comparator = std::less<>>
// requires R BidirectionalIterator range
// requires Comparator is StrictWeakOrdering on the value_type(R)
auto merge_sort(R &&r, Comparator comp = Comparator{}) -> void
{
    i::merge_sort(adl::begin(r), adl::end(r), astl::pass_fn(comp));
}

template <typename R, typename N, typename Comparator, typename B>
// requires R is ForwardIterator range
// requires N is integral type
// requires Comparator is StrictWeakOrdering on the value_type(R)
auto merge_sort_n(R &&r, N n, Comparator comp, B buffer, N buffer_size) -> iter_of_range<R>
{
    return i::merge_sort_n(adl::begin(r), n, astl::pass_fn(comp), buffer, buffer_size);
}

template <typename R, typename N, typename Comparator, typename B, typename P>
auto merge_sort_n(R &&r, N n, Comparator comp, B buffer, N buffer_size, P p) -> iter_of_range<R>
{
    return i::merge_sort_n(adl::begin(r), n, astl::pass_fn(comp), buffer, buffer_size,
                           astl::pass_fn(p));
}

template <typename R, typename N, typename Comparator = std::less<>>
// requires R is ForwardIterator range
// requires N is integral type
// requires Comparator is StrictWeakOrdering on the value_type(R)
auto merge_sort_n(R &&r, N n, Comparator comp = Comparator{}) -> iter_of_range<R>
{
    return i::merge_sort_n(adl::begin(r), n, astl::pass_fn(comp));
}

template <typename R, typename N, typename Comparator, typename P>
auto merge_sort_n(R &&r, N n, Comparator comp, P p) -> iter_of_range<R>
{
    return i::merge_sort_n(adl::begin(r), n, astl::pass_fn(comp), astl::pass_fn(p));
}

template <typename R, typename RandIt, typename Comparator, typename P>
auto partial_sort(R &&r, RandIt middle, Comparator comp, P p) -> void
{
    i::partial_sort(adl::begin(r), middle, adl::end(r), astl::pass_fn(comp), astl::pass_fn(p));
}

template <typename R, typename RandIt, typename Comparator>
// requires R RandomAccessIterator range
// requires Comparator is StrictWeakOrdering on the value_type(R)
auto partial_sort(R &&r, RandIt middle, Comparator comp) -> void
{
    i::partial_sort(adl::begin(r), middle, adl::end(r), astl::pass_fn(comp));
}

template <typename R, typename RandIt> auto partial_sort(R &&r, RandIt middle) -> void
{
    i::partial_sort(adl::begin(r), middle, adl::end(r));
}

template <typename InRng, typename OutRng, typename Comparator>
// requires InRng InputIterator range
// requires OutRng RandomAccessIterator range
// requires Comparator is StrictWeakOrdering on the value_type(InRng)
auto partial_sort_copy(InRng &&i_r, OutRng &&o_r, Comparator comp) -> OutRng
{
    return i::partial_sort_copy(adl::begin(i_r), adl::end(i_r), adl::begin(o_r), adl::end(o_r),
                                astl::pass_fn(comp));
}

template <typename InRng, typename OutRng>
// requires InRng InputIterator range
// requires OutRng RandomAccessIterator range
auto partial_sort_copy(InRng &&i_r, OutRng &&o_r) -> OutRng
{
    return i::partial_sort_copy(adl::begin(i_r), adl::end(i_r), adl::begin(o_r), adl::end(o_r));
}

template <typename InRng, typename OutRng, typename Comparator, typename P>
auto partial_sort_copy(InRng &&i_r, OutRng &&o_r, Comparator comp, P p) -> OutRng
{
    return i::partial_sort_copy(adl::begin(i_r), adl::end(i_r), adl::begin(o_r), adl::end(o_r),
                                astl::pass_fn(comp), astl::pass_fn(p));
}

template <typename R, typename RandIt, typename Comparator, typename P>
// requires R RandomAccessIterator range
// requires RandIt RandomAccessIterator
// requires Comparator is StrictWeakOrdering on the value_type(R)
auto partial_sort_left(R &&r, RandIt middle, Comparator comp, P p) -> void
{
    i::partial_sort_left(adl::begin(r), middle, adl::end(r), astl::pass_fn(comp), astl::pass_fn(p));
}

template <typename R, typename RandIt, typename Comparator = std::less<>>
auto partial_sort_left(R &&r, RandIt middle, Comparator comp = Comparator{}) -> void
{
    i::partial_sort_left(adl::begin(r), middle, adl::end(r), astl::pass_fn(comp));
}

template <typename R, typename RandIt, typename Comparator, typename P>
auto partial_sort_move(R &&r, RandIt d_first, RandIt d_last, Comparator comp, P p) -> RandIt
{
    return i::partial_sort_move(adl::begin(r), adl::end(r), d_first, d_last, astl::pass_fn(comp),
                                astl::pass_fn(p));
}

template <typename R, typename RandIt, typename Comparator>
// requires R InputIterator range
// requires RandIt RandomAccessIterator
// requires Comparator is StrictWeakOrdering on the value_type(R)
auto partial_sort_move(R &&r, RandIt d_first, RandIt d_last, Comparator comp) -> RandIt
{
    return i::partial_sort_move(adl::begin(r), adl::end(r), d_first, d_last, astl::pass_fn(comp));
}

template <typename R, typename RandIt>
// requires R InputIterator range
// requires RandIt RandomAccessIterator
auto partial_sort_move(R &&r, RandIt d_first, RandIt d_last) -> RandIt
{
    return i::partial_sort_move(adl::begin(r), adl::end(r), d_first, d_last);
}

template <typename R, typename RandIt, typename Comparator, typename P>
auto partial_sort_right(R &&r, RandIt middle, Comparator comp, P p) -> void
{
    i::partial_sort_right(adl::begin(r), middle, adl::end(r), astl::pass_fn(comp),
                          astl::pass_fn(p));
}

template <typename R, typename RandIt, typename Comparator = std::less<>>
// requires R RandomAccessIterator range
// requires RandIt RandomAccessIterator
// requires Comparator is StrictWeakOrdering on the value_type(R)
auto partial_sort_right(R &&r, RandIt middle, Comparator comp = Comparator{}) -> void
{
    i::partial_sort_right(adl::begin(r), middle, adl::end(r), astl::pass_fn(comp));
}

template <typename R, typename Comparator, typename P>
auto quicksort(R &&r, Comparator comp, P p) -> void
{
    i::quicksort(adl::begin(r), adl::end(r), astl::pass_fn(comp), astl::pass_fn(p));
}

template <typename R, typename Comparator = std::less<>>
// requires R ForwardIterator range
// requires Comparator is StrictWeakOrdering on the value_type(R)
auto quicksort(R &&r, Comparator comp = Comparator{}) -> void
{
    i::quicksort(adl::begin(r), adl::end(r), astl::pass_fn(comp));
}

template <typename R, typename Comparator, typename P>
auto selection_sort(R &&r, Comparator comp, P p) -> void
{
    i::selection_sort(adl::begin(r), adl::end(r), astl::pass_fn(comp), astl::pass_fn(p));
}

template <typename R, typename Comparator = std::less<>>
// requires R ForwardIterator range
// requires Comparator is StrictWeakOrdering on the value_type(R)
auto selection_sort(R &&r, Comparator comp = Comparator{}) -> void
{
    i::selection_sort(adl::begin(r), adl::end(r), astl::pass_fn(comp));
}

template <typename R, typename Comparator, typename P>
auto shell_sort(R &&r, Comparator comp, P p) -> void
{
    i::shell_sort(adl::begin(r), adl::end(r), astl::pass_fn(comp), astl::pass_fn(p));
}

template <typename R, typename Comparator = std::less<>>
// requires R RandomAccessIterator range
// requires Comparator is StrictWeakOrdering on the value_type(R)
auto shell_sort(R &&r, Comparator comp = Comparator{}) -> void
{
    i::shell_sort(adl::begin(r), adl::end(r), astl::pass_fn(comp));
}

namespace internal_s
{
template <typename Cont, typename Comparator>
auto sort1_c(Cont &&cont, Comparator comp, internal_adl::rank<1>)
    -> decltype(cont.sort(astl::pass_fn(comp)))
{
    return cont.sort(astl::pass_fn(comp));
}

template <typename Cont, typename Comparator>
auto sort1_c(Cont &&cont, Comparator comp, internal_adl::rank<0>) -> void
{
    return i::sort(adl::begin(cont), adl::end(cont), astl::pass_fn(comp));
}

template <typename Cont> auto sort1(Cont &&comp, internal_adl::rank<1>) -> decltype(comp.sort())
{
    return comp.sort();
}

template <typename Cont> auto sort1(Cont &&comp, internal_adl::rank<0>) -> void
{
    return i::sort(adl::begin(comp), adl::end(comp));
}
} // namespace internal_s

template <typename R, typename Comparator, typename P>
auto sort(R &&r, Comparator comp, P p) -> void
{
    internal_s::sort1(r, astl::combine(astl::pass_fn(comp), astl::pass_fn(p)),
                      internal_adl::rank<1>{});
}

template <typename R, typename Comparator>
// requires R RandomAccessIterator range
// requires Comparator is StrictWeakOrdering on the value_type(R)
auto sort(R &&r, Comparator comp) -> void
{
    internal_s::sort1_c(r, astl::pass_fn(comp), internal_adl::rank<1>{});
}

template <typename R>
// requires R RandomAccessIterator range
auto sort(R &&r) -> void
{
    internal_s::sort1(r, internal_adl::rank<1>{});
}

template <typename R, typename Comparator = std::less<>>
auto sort2(R &&r, Comparator comp = Comparator{}) -> unsigned
{
    return i::sort2(adl::begin(r), astl::pass_fn(comp));
}

template <typename R, typename Comparator, typename P>
auto sort2(R &&r, Comparator comp, P p) -> unsigned
{
    return i::sort2(adl::begin(r), astl::combine(astl::pass_fn(comp), astl::pass_fn(p)));
}

template <typename R, typename Comparator = std::less<>>
// requires R ForwardIterator range
// requires Comparator is StrictWeakOrdering on the value_type(R)
auto sort3(R &&r, Comparator comp = Comparator{}) -> unsigned
{
    return i::sort3(adl::begin(r), astl::pass_fn(comp));
}

template <typename R, typename Comparator, typename P>
auto sort3(R &&r, Comparator comp, P p) -> unsigned
{
    return i::sort3(adl::begin(r), astl::pass_fn(comp), astl::pass_fn(p));
}

template <typename R, typename Comparator = std::less<>>
// requires R ForwardIterator range
// requires Comparator is StrictWeakOrdering on the value_type(R)
auto sort4(R &&r, Comparator comp = Comparator{}) -> unsigned
{
    return i::sort4(adl::begin(r), astl::pass_fn(comp));
}

template <typename R, typename Comparator, typename P>
auto sort4(R &&r, Comparator comp, P p) -> unsigned
{
    return i::sort4(adl::begin(r), astl::pass_fn(comp), astl::pass_fn(p));
}

template <typename R, typename Comparator = std::less<>>
// requires R ForwardIterator range
// requires Comparator is StrictWeakOrdering on the value_type(R)
auto sort5(R &&r, Comparator comp = Comparator{}) -> unsigned
{
    return i::sort5(adl::begin(r), astl::pass_fn(comp));
}

template <typename R, typename Comparator, typename P>
auto sort5(R &&r, Comparator comp, P p) -> unsigned
{
    return i::sort5(adl::begin(r), astl::pass_fn(comp), astl::pass_fn(p));
}

template <typename R, typename Comparator = std::less<>>
// requires R ForwardIterator range
// requires Comparator is StrictWeakOrdering on the value_type(R)
auto sort6(R &&r, Comparator comp = Comparator{}) -> unsigned
{
    return i::sort6(adl::begin(r), astl::pass_fn(comp));
}

template <typename R, typename Comparator, typename P>
auto sort6(R &&r, Comparator comp, P p) -> unsigned
{
    return i::sort6(adl::begin(r), astl::pass_fn(comp), astl::pass_fn(p));
}

template <typename R, typename Comparator, typename P>
auto sort_and_unique(R &&r, Comparator comp, P p) -> iter_of_range<R>
{
    return i::sort_and_unique(adl::begin(r), adl::end(r), astl::pass_fn(comp), astl::pass_fn(p));
}

template <typename R, typename Comparator = std::less<>>
// requires R RandomAccessIterator range
// requires Comparator is StrictWeakOrdering on the value_type(R)
auto sort_and_unique(R &&r, Comparator comp = Comparator{}) -> iter_of_range<R>
{
    return i::sort_and_unique(adl::begin(r), adl::end(r), astl::pass_fn(comp));
}

template <typename R, typename RandIt, typename Comparator, typename P>
auto sort_subrange(R &&r, RandIt sub_f, RandIt sub_l, Comparator comp, P p)
    -> std::pair<RandIt, RandIt>
{
    return i::sort_subrange(adl::begin(r), adl::end(r), sub_f, sub_l, astl::pass_fn(comp),
                            astl::pass_fn(p));
}

template <typename R, typename RandIt, typename Comparator>
// requires R RandomAccessIterator range
// requires RandIt RandomAccessIterator
// requires Comparator is StrictWeakOrdering on the value_type(R)
auto sort_subrange(R &&r, RandIt sub_f, RandIt sub_l, Comparator comp) -> std::pair<RandIt, RandIt>
{
    // precondition r.begin() <= sub_f <= sub_l <= r.end()
    return i::sort_subrange(adl::begin(r), adl::end(r), sub_f, sub_l, astl::pass_fn(comp));
}

template <typename R, typename RandIt>
// R RandomAccessIterator range
// RandIt RandomAccessIterator
auto sort_subrange(R &&r, RandIt sub_f, RandIt sub_l) -> std::pair<RandIt, RandIt>
{
    // precondition r.begin() <= sub_f <= sub_l <= r.end()
    return i::sort_subrange(adl::begin(r), adl::end(r), sub_f, sub_l);
}

template <typename R, typename Comparator, typename P>
auto stable_sort(R &&r, Comparator comp, P p) -> void
{
    i::stable_sort(adl::begin(r), adl::end(r), astl::pass_fn(comp), astl::pass_fn(p));
}

template <typename R, typename Comparator>
// requires R RandomAccessIterator range
// requires Comparator is StrictWeakOrdering on the value_type(R)
auto stable_sort(R &&r, Comparator comp) -> void
{
    i::stable_sort(adl::begin(r), adl::end(r), astl::pass_fn(comp));
}

template <typename R>
// requires R RandomAccessIterator range
auto stable_sort(R &&r) -> void
{
    i::stable_sort(adl::begin(r), adl::end(r));
}

} // namespace r
} // namespace astl

#endif
