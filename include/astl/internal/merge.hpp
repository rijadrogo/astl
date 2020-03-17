//
// Created by Rijad on 21-Aug-18.
//

#ifndef ASTL_INCLUDE_MERGE_HPP
#define ASTL_INCLUDE_MERGE_HPP

#include <algorithm>
#include <type_traits>

#include "inplace_merge.hpp"

#include "astl/functional.hpp"
#include "astl/iterator.hpp"
#include "astl/map_iterator.hpp"
#include "astl/range_access.hpp"
#include "astl/temporary_buffer.hpp"

namespace astl
{
namespace internal_merge
{
template <typename FwdIt, typename Comparator, typename B>
// requires FwdIt is ForwardIterator
// requires Comparator StrictWeakOrdering on value_type(FwdIt)
// requires B ForwardIterator
auto merge_with_buffer(FwdIt first, FwdIt mid, FwdIt last, Comparator comp, B buffer) -> FwdIt
{
    // precondition is_sorted(first, mid, comp)
    // precondition is_sorted(mid, last, comp)
    B buffer_last(std::move(first, mid, buffer));
    return i::merge_move(buffer, buffer_last, mid, last, first, comp);
}
} // namespace internal_merge

namespace i
{
template <typename FwdIt1, typename FwdIt2, typename OutIt, typename Comparator>
// requires FwdIt1 ForwardIterator
// requires FwdIt2 ForwardIterator
// requires OutIt ForwardIterator
// requires Comparator StrictWeakOrdering, arguments value_type(FwdIt1) and
// value_type(FwdIt2)
auto merge(FwdIt1 first1, FwdIt1 last1, FwdIt2 first2, FwdIt2 last2, OutIt dest,
           Comparator comp /* = std::less{} */) -> OutIt
{
    // precondition is_sorted(first1, last1, comp)
    // precondition is_sorted(first2, last2, comp)
    auto c(astl::pass_fn(comp));
    while (first1 != last1 && first2 != last2) {
        if (comp(*first2, *first1)) {
            FwdIt2 it(std::lower_bound(first2, last2, *first1, c));
            dest = std::copy(first2, it, dest);
            first2 = it;
        }
        else {
            FwdIt1 it(std::upper_bound(first1, last1, *first2, c));
            dest = std::copy(first1, it, dest);
            first1 = it;
        }
    }
    return std::copy(first2, last2, std::copy(first1, last1, dest));
}

template <typename InputIt1, typename InputIt2, typename OutIt, typename Comparator, typename P1,
          typename P2>
auto merge(InputIt1 first1, InputIt1 last1, InputIt2 first2, InputIt2 last2, OutIt dest,
           Comparator comp, P1 p1, P2 p2) -> OutIt
{
    auto proj1(astl::pass_fn(p1));
    auto proj2(astl::pass_fn(p2));
    return i::merge(astl::map_iterator(first1, proj1), astl::map_iterator(last1, proj1),
                    astl::map_iterator(first2, proj2), astl::map_iterator(last2, proj2), dest,
                    astl::pass_fn(comp));
}

template <typename FwdIt, typename N, typename Comparator, typename B, typename Size>
// requires FwdIt is ForwardIterator
// requires N is integral type
// requires Comparator is StrictWeakOrdering on the value_type(FwdIt)
// requires B is ForwardIterator
// requires Size is integral type
auto merge_adaptive_n(FwdIt first1, N size1, FwdIt first2, N size2, Comparator comp, B buffer,
                      Size buffer_size) -> FwdIt
{
    // precondition distance(first1, first2) == size1
    // precondition is_sorted_n(first1, size1, comp)
    // precondition is_sorted_n(first2, size2, comp)
    if (size1 == N(0) || size2 == N(0)) return first1;

    auto c(astl::pass_fn(comp));
    if (Size(size1) <= buffer_size) {
        FwdIt last(astl::next(first2, size2));
        return internal_merge::merge_with_buffer(first1, first2, last, c, buffer);
    }
    using Ct = typename std::common_type<N, Size>::type;
    FwdIt f0_0;
    FwdIt f0_1;
    FwdIt f1_0;
    FwdIt f1_1;
    Ct n0_0;
    Ct n0_1;
    Ct n1_0;
    Ct n1_1;
    Ct buff_size(buffer_size);
    if (size1 < size2)
        internal_merge::mer_inp_lhs_sprob_buff(first1, size1, first2, size2, f0_0, n0_0, f0_1, n0_1,
                                               f1_0, n1_0, f1_1, n1_1, c, buffer, buff_size);

    else
        internal_merge::mer_inp_rhs_sprob_buff(first1, size1, first2, size2, f0_0, n0_0, f0_1, n0_1,
                                               f1_0, n1_0, f1_1, n1_1, c, buffer, buff_size);

    i::merge_adaptive_n(f0_0, n0_0, f0_1, n0_1, c, buffer, buff_size);
    return i::merge_adaptive_n(f1_0, n1_0, f1_1, n1_1, c, buffer, buff_size);
}

template <typename FwdIt, typename N, typename Comparator, typename B, typename Size, typename P>
auto merge_adaptive_n(FwdIt first1, N size1, FwdIt first2, N size2, Comparator comp, B buffer,
                      Size buffer_size, P p) -> FwdIt
{
    // precondition distance(first1, first2) == size1
    // precondition is_sorted_n(first1, size1, comp)
    // precondition is_sorted_n(first2, size2, comp)
    return i::merge_adaptive_n(first1, size1, first2, size2,
                               astl::combine(astl::pass_fn(comp), astl::pass_fn(p)), buffer,
                               buffer_size);
}

template <typename FwdIt, typename N, typename Comparator>
// requires FwdIt is ForwardIterator
// requires N is integral type
// requires Comparator is StrictWeakOrdering on the value type of FwdIt
auto merge_adaptive_n(FwdIt first1, N size1, FwdIt first2, N size2, Comparator comp) -> FwdIt
{
    // precondition distance(first1, first2) == size1
    // precondition is_sorted_n(first1, size1, comp)
    // precondition is_sorted_n(first2, size2, comp)
    if (size1 == N(0) || size2 == N(0)) return first1;

    using T = iter_value_type<FwdIt>;
    inline_temporary_buffer<T> buffer(size1, *first1);
    auto c(astl::pass_fn(comp));
    if (buffer.size() == 0) return i::merge_inplace_n_no_buffer(first1, size1, first2, size2, c);

    return i::merge_adaptive_n(first1, size1, first2, size2, c, buffer.begin(), buffer.size());
}

template <typename FwdIt, typename N, typename Comparator, typename P>
auto merge_adaptive_n(FwdIt first1, N size1, FwdIt first2, N size2, Comparator comp, P p) -> FwdIt
{
    // precondition distance(first1, first2) == size1
    // precondition is_sorted_n(first1, size1, comp)
    // precondition is_sorted_n(first2, size2, comp)
    return i::merge_adaptive_n(first1, size1, first2, size2,
                               astl::combine(astl::pass_fn(comp), astl::pass_fn(p)));
}

template <typename BidiIt1, typename BidiIt2, typename OutIt, typename Comparator>
// requires BidiIt1 bidirectional iterator
// requires BidiIt2 bidirectional iterator
// requires OutIt bidirectional iterator
// requires Comparator returns bool, arguments ValueType(BidiIt2),
// ValueType(BidiIt1)
auto merge_backward(BidiIt1 first1, BidiIt1 last1, BidiIt2 first2, BidiIt2 last2, OutIt result,
                    Comparator pred) -> OutIt
{
    // precondition is_sorted(first1, last1, comp)
    // precondition is_sorted(first2, last2, comp)
    if (first1 == last1) return std::copy_backward(first2, last2, result);

    if (first2 == last2) return std::copy_backward(first1, last1, result);

    --last1;
    --last2;
    while (true) {
        if (pred(*last2, *last1)) {
            *--result = *last1;
            if (first1 == last1) return std::copy_backward(first2, ++last2, result);

            --last1;
        }
        else {
            *--result = *last2;
            if (first2 == last2) return std::copy_backward(first1, ++last1, result);

            --last2;
        }
    }
}

template <typename BidiIt1, typename BidiIt2, typename OutIt, typename Comparator>
// requires BidiIt1 bidirectional iterator
// requires BidiIt2 bidirectional iterator
// requires OutIt bidirectional iterator
auto merge_backward(BidiIt1 first1, BidiIt1 last1, BidiIt2 first2, BidiIt2 last2, OutIt dest)
    -> OutIt
{
    return i::merge_backward(first1, last1, first2, last2, dest, std::equal_to{});
}

template <typename BidiIt1, typename BidiIt2, typename OutIt, typename Comparator, typename P>
auto merge_backward(BidiIt1 first1, BidiIt1 last1, BidiIt2 first2, BidiIt2 last2, OutIt dest,
                    Comparator pred, P p) -> OutIt
{
    // precondition is_sorted(first1, last1, comp, p)
    // precondition is_sorted(first2, last2, comp, p)
    return i::merge_backward(first1, last1, first2, last2, dest,
                             astl::combine(astl::pass_fn(pred), astl::pass_fn(p)));
}

template <typename BidiIt1, typename BidiIt2, typename OutIt, typename Comparator>
// requires BidiIt1 bidirectional iterator
// requires BidiIt2 bidirectional iterator
// requires OutIt bidirectional iterator
// requires Comparator returns bool, arguments ValueType(BidiIt2),
// ValueType(BidiIt1)
auto merge_move_backward(BidiIt1 first1, BidiIt1 last1, BidiIt2 first2, BidiIt2 last2, OutIt result,
                         Comparator pred) -> OutIt
{
    // precondition is_sorted(first1, last1, comp)
    // precondition is_sorted(first2, last2, comp)
    if (first1 == last1) return std::move_backward(first2, last2, result);

    if (first2 == last2) return std::move_backward(first1, last1, result);

    --last1;
    --last2;
    while (true) {
        if (pred(*last2, *last1)) {
            *--result = std::move(*last1);
            if (first1 == last1) return std::move_backward(first2, ++last2, result);

            --last1;
        }
        else {
            *--result = std::move(*last2);
            if (first2 == last2) return std::move_backward(first1, ++last1, result);

            --last2;
        }
    }
}

template <typename BidiIt1, typename BidiIt2, typename OutIt, typename Comparator>
// requires BidiIt1 bidirectional iterator
// requires BidiIt2 bidirectional iterator
// requires OutIt bidirectional iterator
auto merge_move_backward(BidiIt1 first1, BidiIt1 last1, BidiIt2 first2, BidiIt2 last2, OutIt dest)
    -> OutIt
{
    return i::merge_move_backward(first1, last1, first2, last2, dest, std::equal_to{});
}

template <typename BidiIt1, typename BidiIt2, typename OutIt, typename Comparator, typename P>
auto merge_move_backward(BidiIt1 first1, BidiIt1 last1, BidiIt2 first2, BidiIt2 last2, OutIt dest,
                         Comparator pred, P p) -> OutIt
{
    // precondition is_sorted(first1, last1, comp, p)
    // precondition is_sorted(first2, last2, comp, p)
    return i::merge_move_backward(first1, last1, first2, last2, dest,
                                  astl::combine(astl::pass_fn(pred), astl::pass_fn(p)));
}

template <typename FwdIt1, typename FwdIt2, typename FwdIt3, typename Comparator = std::less<>,
          typename Equal = std::equal_to<>>
// requires FwdIt1 is ForwardIterator
// requires FwdIt2 is ForwardIterator
// requires FwdIt3 is ForwardIterator
// requires Comparator is StrictWeakOrdering on the value_type(FwdIt)
// requires Equal, equivalency of value_type(FwdIt1) and value_type(FwdIt2)
auto merge_unique(FwdIt1 first1, FwdIt1 last1, FwdIt2 first2, FwdIt2 last2, FwdIt3 dest,
                  Comparator comp = std::less{}, Equal eq = std::equal_to{}) -> FwdIt3
{
    // precondition is_sorted(first1, last1, comp)
    // precondition is_sorted(first2, last2, comp)
    auto e(astl::pass_fn(eq));
    if (first1 == last1) return std::unique_copy(first2, last2, dest, e);

    if (first2 == last2) return std::unique_copy(first1, last1, dest, e);

    if (comp(*first2, *first1)) *dest = *first2++;
    else
        *dest = *first1++;

    while (true) {
        if (first1 == last1) return std::unique_copy(first2, last2, ++dest, e);

        if (first2 == last2) return std::unique_copy(first1, last1, ++dest, e);

        if (comp(*first2, *first1)) {
            if (!e(*dest, *first2)) *++dest = *first2;

            ++first2;
        }
        else {
            if (!e(*dest, *first1)) *++dest = *first1;

            ++first1;
        }
    }
}

template <typename FwdIt1, typename FwdIt2, typename FwdIt3, typename Comparator, typename Equal,
          typename P1, typename P2>
auto merge_unique(FwdIt1 first1, FwdIt1 last1, FwdIt2 first2, FwdIt2 last2, FwdIt3 dest,
                  Comparator comp, Equal eq, P1 p1, P2 p2) -> FwdIt3
{
    auto proj1(astl::pass_fn(p1));
    auto proj2(astl::pass_fn(p2));
    return i::merge_unique(astl::map_iterator(first1, proj1), astl::map_iterator(last1, proj1),
                           astl::map_iterator(first2, proj2), astl::map_iterator(last2, proj2),
                           dest, astl::pass_fn(comp), astl::pass_fn(eq));
}
} // namespace i

namespace r
{
template <typename R1, typename R2, typename OutIt, typename Comparator>
auto merge(R1 &&r1, R2 &&r2, OutIt dest, Comparator comp) -> OutIt
{
    return i::merge(adl::begin(r1), adl::end(r1), adl::begin(r2), adl::end(r2), dest,
                    astl::pass_fn(comp));
}

template <typename R1, typename R2, typename OutIt>
auto merge(R1 &&r1, R2 &&r2, OutIt dest) -> OutIt
{
    return r::merge(r1, r2, dest, std::less{});
}

template <typename R1, typename R2, typename OutIt, typename Comparator, typename P1, typename P2>
auto merge(R1 &&r1, R2 &&r2, OutIt dest, Comparator comp, P1 p1, P2 p2) -> OutIt
{
    return i::merge(adl::begin(r1), adl::end(r1), adl::begin(r2), adl::end(r2), dest,
                    astl::pass_fn(comp), astl::pass_fn(p1), astl::pass_fn(p2));
}

template <typename R1, typename R2, typename OutIt, typename Comparator>
auto merge_backward(R1 &&r1, R2 &&r2, OutIt dest, Comparator comp) -> OutIt
{
    return i::merge_backward(adl::begin(r1), adl::end(r1), adl::begin(r2), adl::end(r2), dest,
                             astl::pass_fn(comp));
}

template <typename R1, typename R2, typename OutIt>
auto merge_backward(R1 &&r1, R2 &&r2, OutIt dest) -> OutIt
{
    return r::merge_backward(r1, r2, dest, std::less{});
}

template <typename R1, typename R2, typename OutIt, typename Comparator, typename P1, typename P2>
auto merge_backward(R1 &&r1, R2 &&r2, OutIt dest, Comparator comp, P1 p1, P2 p2) -> OutIt
{
    return i::merge_backward(adl::begin(r1), adl::end(r1), adl::begin(r2), adl::end(r2), dest,
                             astl::pass_fn(comp), astl::pass_fn(p1), astl::pass_fn(p2));
}

template <typename R1, typename R2, typename OutIt, typename Comparator>
auto merge_move_backward(R1 &&r1, R2 &&r2, OutIt dest, Comparator comp) -> OutIt
{
    return i::merge_move_backward(adl::begin(r1), adl::end(r1), adl::begin(r2), adl::end(r2), dest,
                                  astl::pass_fn(comp));
}

template <typename R1, typename R2, typename OutIt>
auto merge_move_backward(R1 &&r1, R2 &&r2, OutIt dest) -> OutIt
{
    return r::merge_move_backward(r1, r2, dest, std::less{});
}

template <typename R1, typename R2, typename OutIt, typename Comparator, typename P1, typename P2>
auto merge_move_backward(R1 &&r1, R2 &&r2, OutIt dest, Comparator comp, P1 p1, P2 p2) -> OutIt
{
    return i::merge_move_backward(adl::begin(r1), adl::end(r1), adl::begin(r2), adl::end(r2), dest,
                                  astl::pass_fn(comp), astl::pass_fn(p1), astl::pass_fn(p2));
}

template <typename R1, typename R2, typename OutIt, typename Comparator = std::less<>,
          typename Equal = std::equal_to<>>
auto merge_unique(R1 &&r1, R2 &&r2, OutIt dest, Comparator comp = std::less{},
                  Equal eq = std::equal_to{}) -> OutIt
{
    return i::merge_unique(adl::begin(r1), adl::end(r1), adl::begin(r2), adl::end(r2), dest,
                           astl::pass_fn(comp), astl::pass_fn(eq));
}

template <typename R1, typename R2, typename OutIt, typename Comparator, typename Equal,
          typename P1, typename P2 = identity>
auto merge_unique(R1 &&r1, R2 &&r2, OutIt dest, Comparator comp, Equal eq, P1 p1, P2 p2 = P2{})
    -> OutIt
{
    return i::merge_unique(adl::begin(r1), adl::end(r1), adl::begin(r2), adl::end(r2), dest,
                           astl::pass_fn(comp), astl::pass_fn(eq), astl::pass_fn(p1),
                           astl::pass_fn(p2));
}
} // namespace r
} // namespace astl

#endif // ASTL_INCLUDE_MERGE_HPP
