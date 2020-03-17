//
// Created by Rijad on 03-Aug-18.
//

#ifndef ASTL_INCLUDE_PARTITION_HPP
#define ASTL_INCLUDE_PARTITION_HPP

#include <algorithm>
#include <memory>
#include <utility>

#include "find.hpp"
#include "lower_bound.hpp"
#include "rotate.hpp"
#include "sort.hpp"

#include "astl/functional.hpp"
#include "astl/iterator.hpp"
#include "astl/range_access.hpp"

namespace astl
{
namespace i
{
template <typename FwdIt, typename BinaryPredicate>
// requires FwdIt ForwardIterator
// requires BinaryPredicate, two arguments of value_type(FwdIt)
auto group_same_elements(FwdIt first, FwdIt last, BinaryPredicate pred) -> FwdIt
{
    FwdIt i(first);
    while (first != last) {
        FwdIt next(astl::next(first));
        FwdIt it(std::find_if(next, last, astl::bind2nd(astl::pass_fn(pred), *first)));
        if (it != last) i::rotate_one_right(next, ++it);

        first = next;
    }
    return i;
}

template <typename FwdIt>
// requires FwdIt ForwardIterator
auto group_same_elements(FwdIt first, FwdIt last) -> FwdIt
{
    return i::group_same_elements(first, last, std::equal_to{});
}

template <typename FwdIt, typename BinaryPredicate, typename P>
auto group_same_elements(FwdIt first, FwdIt last, BinaryPredicate pred, P p) -> FwdIt
{
    return i::group_same_elements(first, last,
                                  astl::combine(astl::pass_fn(pred), astl::pass_fn(p)));
}

using std::is_partitioned; // NOLINT(misc-unused-using-decls)
template <typename InIt, typename UnaryPredicate, typename P>
// requires InIt InputIterator
// requires UnaryPredicate, returns bool, argument value_type(InIt)
ASTL_NODISCARD auto is_partitioned(InIt first, InIt last, UnaryPredicate pred, P p) -> bool
{
    return std::is_partitioned(first, last, astl::combine(astl::pass_fn(pred), astl::pass_fn(p)));
}

template <typename FwdIt, typename UnaryPredicate>
// requires FwdIt ForwardIterator
// requires UnaryPredicate, returns bool, argument value_type(FwdIt)
ASTL_NODISCARD auto is_partitioned(FwdIt first, FwdIt partition_point, FwdIt last,
                                   UnaryPredicate pred) -> bool
{
    if (first == last) return true;

    return std::none_of(first, partition_point, astl::pass_fn(pred))
        && std::all_of(partition_point, last, astl::pass_fn(pred));
}

template <typename FwdIt, typename UnaryPredicate, typename P>
ASTL_NODISCARD auto is_partitioned(FwdIt first, FwdIt partition_point, FwdIt last,
                                   UnaryPredicate pred, P p) -> bool
{
    return i::is_partitioned(first, partition_point, last,
                             astl::combine(astl::pass_fn(pred), astl::pass_fn(p)));
}

template <typename FwdIt, typename UnaryPredicate>
// requires FwdIt ForwardIterator
// requires UnaryPredicate, returns int argument value_type(FwdIt)
ASTL_NODISCARD auto is_partitioned_3way(FwdIt first, FwdIt last, UnaryPredicate pred) -> bool
{
    std::equal_to<int> eq{};
    first = std::find_if_not(first, last, astl::combine(bind_back(eq, 0), pred));
    first = std::find_if_not(first, last, astl::combine(bind_back(eq, 1), pred));
    first = std::find_if_not(first, last, astl::combine(bind_back(eq, 2), pred));
    return first == last;
}

template <typename FwdIt, typename UnaryPredicate, typename P>
ASTL_NODISCARD auto is_partitioned_3way(FwdIt first, FwdIt last, UnaryPredicate pred, P p) -> bool
{
    return i::is_partitioned_3way(first, last,
                                  astl::combine(astl::pass_fn(pred), astl::pass_fn(p)));
}

template <typename InIt, typename N, typename UnaryPredicate>
// requires InIt is InputIterator,
// requires N is integral type,
// requires UnaryPredicate, returns bool, argument value_type(InIt)
ASTL_NODISCARD auto is_partitioned_n(InIt first, N n, UnaryPredicate pred) -> bool
{
    std::pair<InIt, N> first_false(i::find_if_not_n(first, n, pred));
    std::pair<InIt, N> next_true(i::find_if_n(first_false.first, first_false.second, pred));
    return next_true.second == first_false.second;
}

template <typename InIt, typename N, typename UnaryPredicate, typename P>
ASTL_NODISCARD auto is_partitioned_n(InIt first, N n, UnaryPredicate pred, P p) -> bool
{
    return i::is_partitioned_n(first, n, astl::combine(astl::pass_fn(pred), astl::pass_fn(p)));
}

template <typename FwdIt, typename UnaryPredicate>
// requires FwdIt ForwardIterator
// requires UnaryPredicate, return int, argument value_type(FwdIt)
ASTL_NODISCARD auto is_partitioned_n_way(FwdIt first, FwdIt last, UnaryPredicate pred) -> bool
{
    return i::is_sorted(first, last, std::less{}, astl::pass_fn(pred));
}

template <typename FwdIt, typename UnaryPredicate, typename P>
ASTL_NODISCARD auto is_partitioned_n_way(FwdIt first, FwdIt last, UnaryPredicate pred, P p) -> bool
{
    return i::is_partitioned_n_way(first, last,
                                   astl::combine(astl::pass_fn(pred), astl::pass_fn(p)));
}

template <typename InIt, typename UnaryPredicate>
// requires FwdIt ForwardIterator
// requires UnaryPredicate, returns bool, argument value_type(InIt)
auto is_partitioned_until(InIt first, InIt last, UnaryPredicate pred) -> InIt
{
    while (true) { // skip true partition
        if (first == last) return last;

        if (!pred(*first)) break;

        ++first;
    }

    while (++first != last) {           // verify false partition
        if (pred(*first)) return first; // found out of place element
    }

    return last;
}

template <typename InIt, typename UnaryPredicate, typename P>
auto is_partitioned_until(InIt first, InIt last, UnaryPredicate pred, P p) -> InIt
{
    return i::is_partitioned_until(first, last,
                                   astl::combine(astl::pass_fn(pred), astl::pass_fn(p)));
}

// tests if the first range is a partition of the second
template <typename FwdIt1, typename FwdIt2, typename UnaryPredicate>
// requires FwdIt1 ForwardIterator
// requires FwdIt2 ForwardIterator
// requires UnaryPredicate UnaryPredicateicate, returns bool, argument value_type(FwdIt1)
ASTL_NODISCARD auto is_partitioning(FwdIt1 first1, FwdIt1 last1, FwdIt2 first2, FwdIt2 last2,
                                    UnaryPredicate pred) -> bool
{
    return std::is_partitioned(first1, last1, astl::pass_fn(pred))
        && std::is_permutation(first1, last1, first2, last2);
}

// tests if the first range is a partition of the second
template <typename FwdIt1, typename FwdIt2, typename UnaryPredicate, typename P>
ASTL_NODISCARD auto is_partitioning(FwdIt1 first1, FwdIt1 last1, FwdIt2 first2, FwdIt2 last2,
                                    UnaryPredicate pred, P p) -> bool
{
    return i::is_partitioning(first1, last1, first2, last2,
                              astl::combine(astl::pass_fn(pred), astl::pass_fn(p)));
}

template <typename RandIt>
// requires RandIt RandomAccessIterator
auto nth_element(RandIt first, RandIt nth, RandIt last) -> RandIt
{
    std::nth_element(first, nth, last);
    return nth;
}

template <typename RandIt, typename Comparator>
// requires RandIt RandomAccessIterator
// requires Comparator StrictWeakOrdering, arguments two value_type(RandIt)
auto nth_element(RandIt first, RandIt nth, RandIt last, Comparator comp) -> RandIt
{
    std::nth_element(first, nth, last, astl::pass_fn(comp));
    return nth;
}

template <typename RandIt, typename BinaryPredicate, typename P>
auto nth_element(RandIt first, RandIt nth, RandIt last, BinaryPredicate pred, P p) -> RandIt
{
    std::nth_element(first, nth, last, astl::combine(astl::pass_fn(pred), astl::pass_fn(p)));
    return nth;
}

template <typename RandIt, typename N>
// requires RandIt RandomAccessIterator
// requires N integral type
auto nth_element_n(RandIt first, RandIt nth, N n) -> RandIt
{
    std::nth_element(first, nth, first + n);
    return nth;
}

template <typename RandIt, typename N, typename Comparator>
// requires RandIt RandomAccessIterator
// requires N integral type
// requires Comparator StrictWeakOrdering, arguments two value_type(RandIt)
auto nth_element_n(RandIt first, RandIt nth, N n, Comparator comp) -> RandIt
{
    std::nth_element(first, nth, first + n, astl::pass_fn(comp));
    return nth;
}

template <typename RandIt, typename N, typename Comparator, typename P>
auto nth_element_n(RandIt first, RandIt nth, N n, Comparator comp, P p) -> RandIt
{
    std::nth_element(first, nth, first + n, astl::combine(astl::pass_fn(comp), astl::pass_fn(p)));
    return nth;
}

using std::partition; // NOLINT(misc-unused-using-decls)
template <typename FwdIt, typename UnaryPredicate, typename P>
auto partition(FwdIt first, FwdIt last, UnaryPredicate pred, P p) -> FwdIt
{
    return std::partition(first, last, astl::combine(astl::pass_fn(pred), astl::pass_fn(p)));
}

template <typename FwdIt, typename UnaryPredicate>
// requires FwdIt ForwardIterator
// requires UnaryPredicate return int, argument value_type(FwdIt)
ASTL_NODISCARD auto partition_3way(FwdIt first, FwdIt last, UnaryPredicate pred)
    -> std::pair<FwdIt, FwdIt>
{
    if constexpr (is_bidirectional_it_v<FwdIt>) { // Bidirectional Iterator
        using BidiIt = FwdIt;
        BidiIt s(first);
        while (s != last) {
            int const key(pred(*s));
            if (key == 0) {
                std::iter_swap(first, s);
                ++first;
                ++s;
            }
            else if (key == 2) {
                --last;
                std::iter_swap(last, s);
            }
            else // assert(key == 1);
                ++s;
        }
        return std::make_pair(first, last);
    }
    else { // Forward Iterator
        FwdIt t(first);
        FwdIt s(first);
        while (t != last) {
            int const key(pred(*t));
            if (key == 0) {
                using std::swap;
                std::iter_swap(t, s);
                std::iter_swap(s, first);
                ++s;
                ++first;
            }
            else if (key == 1) {
                std::iter_swap(s, t);
                ++s;
            }
            else {
                //
            }
            ++t;
        }
        return std::make_pair(first, s);
    }
}

template <typename FwdIt, typename UnaryPredicate, typename P>
ASTL_NODISCARD auto partition_3way(FwdIt first, FwdIt last, UnaryPredicate pred, P p)
    -> std::pair<FwdIt, FwdIt>
{
    return i::partition_3way(first, last, astl::combine(astl::pass_fn(pred), astl::pass_fn(p)));
}

// Note this algorithm is stable
template <typename FwdIt1, typename OutIt1, typename OutIt2, typename OutIt3,
          typename UnaryPredicate>
// requires FwdIt1 ForwardIterator
// requires OutIt1 OutputIterator
// requires OutIt2 OutputIterator
// requires OutIt3 OutputIterator
// requires UnaryPredicate, returns int, argument value_type(first)
auto partition_3way_copy(FwdIt1 first, FwdIt1 last, OutIt1 dest0, OutIt2 dest1, OutIt3 dest2,
                         UnaryPredicate pred) -> std::tuple<OutIt1, OutIt2, OutIt3>
{
    while (first != last) {
        int const key(pred(*first));
        switch (key) {
        case 0:
            *dest0 = *first;
            ++dest0;
            break;
        case 1:
            *dest1 = *first;
            ++dest1;
            break;
        case 2:
            *dest2 = *first;
            ++dest2;
            break;
        default:;
        }
        ++first;
    }
    return std::tuple<OutIt1, OutIt2, OutIt3>(dest0, dest1, dest2);
}

// Note this algorithm is stable
template <typename FwdIt1, typename FwdIt2, typename FwdIt3, typename FwdIt4,
          typename UnaryPredicate, typename P>
auto partition_3way_copy(FwdIt1 first, FwdIt1 last, FwdIt2 dest0, FwdIt3 dest1, FwdIt4 dest2,
                         UnaryPredicate pred, P p) -> std::tuple<FwdIt2, FwdIt3, FwdIt4>
{
    return i::partition_3way_copy(first, last, dest0, dest1, dest2,
                                  astl::combine(astl::pass_fn(pred), astl::pass_fn(p)));
}

// Note this algorithm is stable
template <typename FwdIt, typename OutIt1, typename OutIt2, typename OutIt3,
          typename UnaryPredicate>
// requires FwdIt ForwardIterator
// requires OutIt1 OutputIterator
// requires OutIt2 OutputIterator
// requires OutIt3 OutputIterator
// requires UnaryPredicate, returns int, argument value_type(FwdIt)
auto partition_3way_move(FwdIt first, FwdIt last, OutIt1 dest0, OutIt2 dest1, OutIt3 dest2,
                         UnaryPredicate pred) -> std::tuple<OutIt1, OutIt2, OutIt3>
{
    return i::partition_3way_copy(std::make_move_iterator(first), std::make_move_iterator(last),
                                  dest0, dest1, dest2,
                                  [p(astl::pass_fn(pred))](auto &&x) { return p(x); });
}

// Note this algorithm is stable
template <typename FwdIt1, typename OutIt1, typename OutIt2, typename OutIt3,
          typename UnaryPredicate, typename P>
auto partition_3way_move(FwdIt1 first, FwdIt1 last, OutIt1 dest0, OutIt2 dest1, OutIt3 dest2,
                         UnaryPredicate pred, P p) -> std::tuple<OutIt1, OutIt2, OutIt3>
{
    return i::partition_3way_move(first, last, dest0, dest1, dest2,
                                  astl::combine(astl::pass_fn(pred), astl::pass_fn(p)));
}

// Note this algorithm is stable
using std::partition_copy; // NOLINT(misc-unused-using-decls)
template <typename InIt, typename OutIt1, typename OutIt2, typename UnaryPredicate, typename P>
auto partition_copy(InIt first, InIt last, OutIt1 d_first_true, OutIt2 d_first_false,
                    UnaryPredicate pred, P p) -> std::pair<OutIt1, OutIt2>
{
    return std::partition_copy(first, last, d_first_true, d_first_false,
                               astl::combine(astl::pass_fn(pred), astl::pass_fn(p)));
}

// Note this algorithm is stable
template <typename InIt, typename N, typename OutIt1, typename OutIt2, typename UnaryPredicate>
// requires R InputIterator range
// requires N integral type
// requires OutIt1 OutputIterator
// requires UnaryPredicate, returns bool, argument value_type(InIt)
auto partition_copy_n(InIt first, N n, OutIt1 d_first_true, OutIt2 d_first_false,
                      UnaryPredicate pred) -> std::tuple<OutIt1, OutIt2, InIt>
{
    while (n != N(0)) {
        if (pred(*first)) {
            *d_first_true = *first;
            ++d_first_true;
        }
        else {
            *d_first_false = *first;
            ++d_first_false;
        }
        ++first;
        --n;
    }
    return std::tuple<OutIt1, OutIt2, InIt>{d_first_true, d_first_false, first};
}

// Note this algorithm is stable
template <typename InIt, typename N, typename OutIt1, typename OutIt2, typename UnaryPredicate,
          typename P>
auto partition_copy_n(InIt first, N n, OutIt1 d_first_true, OutIt2 d_first_false,
                      UnaryPredicate pred, P p) -> std::tuple<OutIt1, OutIt2, InIt>
{
    return i::partition_copy_n(first, n, d_first_true, d_first_false,
                               astl::combine(astl::pass_fn(pred), astl::pass_fn(p)));
}

template <typename FwdIt, typename UnaryPredicate>
// requires FwdIt ForwardIterator
// requires UnaryPredicate, returns int, argument value_type(FwdIt)
auto partition_n_way(FwdIt first, FwdIt last, UnaryPredicate pred) -> void
{
    if constexpr (is_random_access_it_v<FwdIt>) // Random Access Iterator
        i::sort(first, last, std::less{}, astl::pass_fn(pred));
    else // Forward Iterator
        i::quicksort(first, last, std::less{}, astl::pass_fn(pred));
}

template <typename FwdIt, typename UnaryPredicate, typename P>
auto partition_n_way(FwdIt first, FwdIt last, UnaryPredicate pred, P p) -> void
{
    i::partition_n_way(first, last, astl::combine(astl::pass_fn(pred), astl::pass_fn(p)));
}

// Note this algorithm is stable
template <typename InIt, typename OutIt1, typename OutIt2, typename UnaryPredicate>
// requires InIt InputIterator
// requires OutIt1 OutputIterator
// requires UnaryPredicate, returns bool, argument value_type(InIt)
auto partition_move(InIt first, InIt last, OutIt1 d_first_true, OutIt2 d_first_false,
                    UnaryPredicate pred) -> std::pair<OutIt1, OutIt2>
{
    return std::partition_copy(std::make_move_iterator(first), std::make_move_iterator(last),
                               d_first_true, d_first_false, astl::pass_fn(pred));
}

// Note this algorithm is stable
template <typename InIt, typename OutIt1, typename OutIt2, typename UnaryPredicate, typename P>
auto partition_move(InIt first, InIt last, OutIt1 d_first_true, OutIt2 d_first_false,
                    UnaryPredicate pred, P p) -> std::pair<OutIt1, OutIt2>
{
    return i::partition_copy(std::make_move_iterator(first), std::make_move_iterator(last),
                             d_first_true, d_first_false, astl::pass_fn(pred), astl::pass_fn(p));
}

// Note this algorithm is stable
template <typename InIt, typename N, typename OutIt1, typename OutIt2, typename UnaryPredicate>
// requires R InputIterator range
// requires N integral type
// requires OutIt1 OutputIterator
// requires UnaryPredicate, returns bool, argument value_type(InIt)
auto partition_move_n(InIt first, N n, OutIt1 d_first_true, OutIt2 d_first_false,
                      UnaryPredicate pred) -> std::tuple<OutIt1, OutIt2, InIt>
{
    auto tuple(i::partition_copy_n(std::make_move_iterator(first), n, d_first_true, d_first_false,
                                   astl::pass_fn(pred)));
    return std::tuple<OutIt1, OutIt2, InIt>{std::get<0>(tuple), std::get<1>(tuple),
                                            std::get<2>(tuple).base()};
}

// Note this algorithm is stable
template <typename InIt, typename N, typename OutIt1, typename OutIt2, typename UnaryPredicate,
          typename P>
auto partition_move_n(InIt first, N n, OutIt1 d_first_true, OutIt2 d_first_false,
                      UnaryPredicate pred, P p) -> std::tuple<OutIt1, OutIt2, InIt>
{
    auto tuple(i::partition_copy_n(std::make_move_iterator(first), n, d_first_true, d_first_false,
                                   astl::combine(astl::pass_fn(pred), astl::pass_fn(p))));
    return std::tuple<OutIt1, OutIt2, InIt>{std::get<0>(tuple), std::get<1>(tuple),
                                            std::get<2>(tuple).base()};
}

template <typename FwdIt, typename N, typename UnaryPredicate>
// requires FwdIt ForwardIterator
// requires N integral type
// requires UnaryPredicate, returns bool, argument value_type(FwdIt)
auto partition_n(FwdIt first, N n, UnaryPredicate pred) -> FwdIt
{
    if (n == N(0)) return first;

    using std::swap;
    if constexpr (is_random_access_it_v<FwdIt>) { // Random Access Iterator
        return std::partition(first, first + n, astl::pass_fn(pred));
    }
    else { // Input Iterator
        while (pred(*first)) {
            ++first;
            if (--n == N(0)) return first;
        }
        FwdIt next(first);
        while (--n != N(0)) {
            if (pred(*++next)) {
                swap(*first, *next);
                ++first;
            }
        }
        return first;
    }
}

template <typename FwdIt, typename N, typename UnaryPredicate, typename P>
auto partition_n(FwdIt first, N n, UnaryPredicate pred, P p) -> FwdIt
{
    return i::partition_n(first, n, astl::combine(astl::pass_fn(pred), astl::pass_fn(p)));
}

using std::partition_point; // NOLINT(misc-unused-using-decls)
template <typename FwdIt, typename UnaryPredicate, typename P>
auto partition_point(FwdIt first, FwdIt last, UnaryPredicate pred, P p) -> FwdIt
{
    return i::partition_point(first, last, astl::combine(astl::pass_fn(pred), astl::pass_fn(p)));
}

template <typename FwdIt, typename UnaryPredicate>
ASTL_NODISCARD auto partition_point_3way(FwdIt first, FwdIt last, UnaryPredicate pred)
    -> std::pair<FwdIt, FwdIt>
{
    std::equal_to<int> eq{};
    iter_diff_type<FwdIt> n(astl::distance(first, last));
    while (n > 0) {
        iter_diff_type<FwdIt> h(n >> 1);
        FwdIt m(astl::next(first, h));
        switch (pred(*m++)) {
        case 0:
            first = m;
            n = n - h - 1;
            break;
        case 1: {
            auto p(astl::pass_fn(pred));
            FwdIt i(i::partition_point_n(first, n - h - 1,
                                         astl::combine(astl::bind_back(eq, 0), p).first));
            FwdIt j(i::partition_point_n(m, h, astl::combine(astl::bind_back(eq, 1), p)).first);
            return std::make_pair(i, j);
        }
        case 2: n = h;
        default:;
        }
    }
    return std::make_pair(first, first);
}

template <typename FwdIt, typename UnaryPredicate, typename P>
ASTL_NODISCARD auto partition_point_3way(FwdIt first, FwdIt last, UnaryPredicate pred, P p)
    -> std::pair<FwdIt, FwdIt>
{
    return i::partition_point_3way(first, last,
                                   astl::combine(astl::pass_fn(pred), astl::pass_fn(p)));
}

/* NOTE:
 * partition_point_n in lower_bound.hpp
 */

/// \brief Gather the elements of the sub range [sub_f, sub_l) that is
///     inside the range [f, l) as if you had sorted the entire range.
///
/// \param first       The start of the larger range
/// \param last        The end of the larger range
/// \param sub_f       The start of the sub range
/// \param sub_l       The end of the sub range
/// \param comp           A predicate to use to Comparator the values.
///                        comp ( a, b ) returns a boolean.
///
template <typename RandIt, typename Comparator>
// requires RandIt RandomAccessIterator
// requires Comparator StrictWeakOrdering, arguments two value_type(RandIt)
auto partition_subrange(RandIt first, RandIt last, RandIt sub_f, RandIt sub_l, Comparator comp)
    -> std::pair<RandIt, RandIt>
{
    // precondition first <= sub_f <= sub_l <= last
    auto pc(astl::pass_fn(comp));
    if (sub_f != first) {
        std::nth_element(first, sub_f, last, pc);
        ++sub_f;
    }
    if (sub_l != last) std::nth_element(sub_f, sub_l, last, pc);

    return std::make_pair(sub_f, sub_l);
}

template <typename RandIt, typename Comparator, typename P>
auto partition_subrange(RandIt first, RandIt last, RandIt sub_f, RandIt sub_l, Comparator comp, P p)
    -> std::pair<RandIt, RandIt>
{
    return i::partition_subrange(first, last, sub_f, sub_l,
                                 astl::combine(astl::pass_fn(comp), astl::pass_fn(p)));
}

template <typename FwdIt, typename BinaryPredicate>
// requires FwdIt ForwardIterator
// requires BinaryPredicate, two arguments of value_type(FwdIt)
auto remove_duplicates(FwdIt first, FwdIt const last, BinaryPredicate pred) -> FwdIt
{
    auto p(astl::pass_fn(pred));
    i::group_same_elements(first, last, p);
    return std::unique(first, last, p);
}

template <typename FwdIt>
// requires FwdIt ForwardIterator
auto remove_duplicates(FwdIt first, FwdIt const last) -> FwdIt
{
    return i::remove_duplicates(first, last, std::equal_to{});
}

template <typename FwdIt, typename BinaryPredicate, typename P>
// requires FwdIt ForwardIterator
// requires BinaryPredicate, two arguments of value_type(FwdIt)
auto remove_duplicates(FwdIt first, FwdIt const last, BinaryPredicate pred, P p) -> FwdIt
{
    return i::remove_duplicates(first, last, astl::combine(astl::pass_fn(pred), astl::pass_fn(p)));
}

template <typename InIt, typename FwdIt, typename Comparator>
// requires InIt InputIterator
// requires FwdIt ForwardIterator
// requires Comparator is StrictWeakOrdering on the value_type(InRng)
auto top_n(InIt first, InIt last, FwdIt d_first, FwdIt d_last, Comparator comp) -> FwdIt
{
    if (first == last || d_first == d_last) return d_first;

    auto it_pair(i::copy_bounded(first, last, d_first, d_last));
    if (it_pair.second == last) return it_pair.first;

    auto current_first(it_pair.second);
    auto current_max(std::max_element(d_first, d_last, astl::pass_fn(comp)));
    while (current_first != last) {
        if (comp(*current_first, *current_max)) {
            using std::swap;
            swap(*current_first, *current_max);
            current_max = std::max_element(d_first, d_last, astl::pass_fn(comp));
        }
        ++current_first;
    }
    return d_last;
}

template <typename InIt, typename FwdIt>
// requires InIt InputIterator
// requires FwdIt ForwardIterator
auto top_n(InIt first, InIt last, FwdIt d_first, FwdIt d_last) -> FwdIt
{
    return i::top_n(first, last, d_first, d_last, std::less{});
}

template <typename InIt, typename RandIt, typename Comparator, typename P>
auto top_n(InIt first, InIt last, RandIt d_first, RandIt d_last, Comparator comp, P p) -> RandIt
{
    return i::top_n(first, last, d_first, d_last,
                    astl::combine(astl::pass_fn(comp), astl::pass_fn(p)));
}

} // namespace i

namespace r
{
template <typename R, typename BinaryPredicate>
// requires R ForwardIterator range
// requires BinaryPredicate, two arguments of value_type(R)
auto group_same_elements(R r, BinaryPredicate pred) -> iter_of_range<R>
{
    return i::group_same_elements(adl::begin(r), adl::end(r), astl::pass_fn(pred));
}

template <typename R>
// requires R ForwardIterator range
auto group_same_elements(R r) -> iter_of_range<R>
{
    return i::group_same_elements(adl::begin(r), adl::end(r));
}

template <typename R, typename BinaryPredicate, typename P>
auto group_same_elements(R r, BinaryPredicate pred, P p) -> iter_of_range<R>
{
    return i::group_same_elements(adl::begin(r), adl::end(r), astl::pass_fn(pred),
                                  astl::pass_fn(p));
}

template <typename R, typename UnaryPredicate>
// requires R ForwardIterator range
// requires UnaryPredicate returns bool, argument value_type(R)
ASTL_NODISCARD auto is_partitioned(R &&r, UnaryPredicate pred) -> bool
{
    return i::is_partitioned(adl::begin(r), adl::end(r), astl::pass_fn(pred));
}

template <typename R, typename UnaryPredicate, typename P>
ASTL_NODISCARD auto is_partitioned(R &&r, UnaryPredicate pred, P p) -> bool
{
    return i::is_partitioned(adl::begin(r), adl::end(r), astl::pass_fn(pred), astl::pass_fn(p));
}

template <typename R, typename FwdIt, typename UnaryPredicate>
// requires FwdIt ForwardIterator
// requires UnaryPredicate, returns bool, argument value_type(FwdIt)
ASTL_NODISCARD auto is_partitioned(R &&r, iter_of_range<R> partition_point, UnaryPredicate pred)
    -> bool
{
    return i::is_partitioned(adl::begin(r), partition_point, adl::end(r), astl::pass_fn(pred));
}

template <typename R, typename FwdIt, typename UnaryPredicate, typename P>
ASTL_NODISCARD auto is_partitioned(R &&r, FwdIt partition_point, UnaryPredicate pred, P p) -> bool
{
    return i::is_partitioned(adl::begin(r), partition_point, adl::end(r), astl::pass_fn(pred),
                             astl::pass_fn(p));
}

template <typename R, typename UnaryPredicate>
// requires R ForwardIterator range
// requires UnaryPredicate return int, argument value_type(R)
ASTL_NODISCARD auto is_partitioned_3way(R &&r, UnaryPredicate pred) -> bool
{
    return i::is_partitioned_3way(adl::begin(r), adl::end(r), astl::pass_fn(pred));
}

template <typename R, typename UnaryPredicate, typename P>
ASTL_NODISCARD auto is_partitioned_3way(R &&r, UnaryPredicate pred, P p) -> bool
{
    return i::is_partitioned_3way(adl::begin(r), adl::end(r), astl::pass_fn(pred),
                                  astl::pass_fn(p));
}

template <typename R, typename N, typename UnaryPredicate>
// requires R is InputIterator range
// requires N is integral type
// requires UnaryPredicate, returns bool, argument value_type(R)
ASTL_NODISCARD auto is_partitioned_n(R &&r, N n, UnaryPredicate pred) -> bool
{
    return i::is_partitioned_n(adl::begin(r), n, astl::pass_fn(pred));
}

template <typename R, typename N, typename UnaryPredicate, typename P>
ASTL_NODISCARD auto is_partitioned_n(R &&r, N n, UnaryPredicate pred, P p) -> bool
{
    return i::is_partitioned_n(adl::begin(r), n, astl::pass_fn(pred), astl::pass_fn(p));
}

template <typename R, typename UnaryPredicate>
// requires R ForwardIterator range
// requires UnaryPredicate returns bool, argument value_type(R)
ASTL_NODISCARD auto is_partitioned_n_way(R &&r, UnaryPredicate pred) -> bool
{
    return i::is_partitioned_n_way(adl::begin(r), adl::end(r), astl::pass_fn(pred));
}

template <typename R, typename UnaryPredicate, typename P>
ASTL_NODISCARD auto is_partitioned_n_way(R &&r, UnaryPredicate pred, P p) -> bool
{
    return i::is_partitioned_n_way(adl::begin(r), adl::end(r), astl::pass_fn(pred),
                                   astl::pass_fn(p));
}

template <typename R, typename UnaryPredicate>
// requires FwdIt ForwardIterator
// requires UnaryPredicate, returns bool, argument value_type(InIt)
auto is_partitioned_until(R &&r, UnaryPredicate pred) -> iter_of_range<R>
{
    return i::is_partitioned_until(adl::begin(r), adl::end(r), astl::pass_fn(pred));
}

template <typename R, typename UnaryPredicate, typename P>
auto is_partitioned_until(R &&r, UnaryPredicate pred, P p) -> iter_of_range<R>
{
    return i::is_partitioned_until(adl::begin(r), adl::end(r), astl::pass_fn(pred),
                                   astl::pass_fn(p));
}

// tests if the first range is a partition of the second
template <typename R1, typename R2, typename UnaryPredicate>
// requires FwdIt1 ForwardIterator
// requires FwdIt2 ForwardIterator
// requires UnaryPredicate UnaryPredicateicate, return bool argument value_type(FwdIt1)
ASTL_NODISCARD auto is_partitioning(R1 &&r1, R2 &&r2, UnaryPredicate pred) -> bool
{
    return i::is_partitioning(adl::begin(r1), adl::end(r1), adl::begin(r2), adl::end(r2),
                              astl::pass_fn(pred));
}

// tests if the first range is a partition of the second
template <typename R1, typename R2, typename UnaryPredicate, typename P>
ASTL_NODISCARD auto is_partitioning(R1 &&r1, R2 &&r2, UnaryPredicate pred, P p) -> bool
{
    return i::is_partitioning(adl::begin(r1), adl::end(r1), adl::begin(r2), adl::end(r2),
                              astl::pass_fn(pred), astl::pass_fn(p));
}

template <typename R, typename RandIt>
// requires R RandomAccessIterator range
// requires RandIt RandomAccessIterator
auto nth_element(R &&r, RandIt nth) -> iter_of_range<R>
{
    return i::nth_element(adl::begin(r), nth, adl::end(r));
}

template <typename R, typename RandIt, typename Comparator>
// requires R RandomAccessIterator range
// requires RandIt RandomAccessIterator
// requires Comparator StrictWeakOrdering, arguments two value_type(RandIt)
auto nth_element(R &&r, RandIt nth, Comparator comp) -> RandIt
{
    return i::nth_element(adl::begin(r), nth, adl::end(r), astl::pass_fn(comp));
}

template <typename R, typename RandIt, typename Comparator, typename P>
auto nth_element(R &&r, RandIt nth, Comparator comp, P p) -> RandIt
{
    return i::nth_element(adl::begin(r), nth, adl::end(r), astl::pass_fn(comp), astl::pass_fn(p));
}

template <typename R, typename N, typename RandIt>
// requires R RandomAccessIterator range
// requires N integral type
// requires RandIt RandomAccessIterator
auto nth_element_n(R &&r, N n, RandIt nth) -> iter_of_range<R>
{
    return i::nth_element_n(adl::begin(r), nth, n);
}

template <typename R, typename N, typename RandIt, typename Comparator>
// requires R RandomAccessIterator range
// requires N integral type
// requires RandIt RandomAccessIterator
// requires Comparator StrictWeakOrdering, arguments two value_type(RandIt)
auto nth_element_n(R &&r, RandIt nth, N n, Comparator comp) -> RandIt
{
    return i::nth_element_n(adl::begin(r), nth, n, astl::pass_fn(comp));
}

template <typename R, typename N, typename RandIt, typename Comparator, typename P>
auto nth_element_n(R &&r, RandIt nth, N n, Comparator comp, P p) -> RandIt
{
    return i::nth_element_n(adl::begin(r), nth, n, astl::pass_fn(comp), astl::pass_fn(p));
}

template <typename R, typename UnaryPredicate>
// requires R ForwardIterator range
// requires UnaryPredicate, return int, argument value_type(FwdIt)
auto partition(R &&r, UnaryPredicate pred) -> iter_of_range<R>
{
    return i::partition(adl::begin(r), adl::end(r), astl::pass_fn(pred));
}

template <typename R, typename UnaryPredicate, typename P>
auto partition(R &&r, UnaryPredicate pred, P p) -> iter_of_range<R>
{
    return i::partition(adl::begin(r), adl::end(r), astl::pass_fn(pred), astl::pass_fn(p));
}

template <typename R, typename UnaryPredicate>
// requires R ForwardIterator range
// requires UnaryPredicate, returns int, argument value_type(first)
auto partition_3way(R &&r, UnaryPredicate pred) -> iter_of_range<R>
{
    return i::partition_3way(adl::begin(r), adl::end(r), astl::pass_fn(pred));
}

template <typename R, typename UnaryPredicate, typename P>
auto partition_3way(R &&r, UnaryPredicate pred, P p) -> iter_of_range<R>
{
    return i::partition_3way(adl::begin(r), adl::end(r), astl::pass_fn(pred), astl::pass_fn(p));
}

// Note this algorithm is stable
template <typename R, typename OutIt1, typename OutIt2, typename OutIt3, typename UnaryPredicate>
// requires R ForwardIterator range
// requires OutIt1 OutputIterator
// requires OutIt2 OutputIterator
// requires OutIt3 OutputIterator
// requires UnaryPredicate, returns int, argument value_type(first)
auto partition_3way_copy(R &&r, OutIt1 dest0, OutIt2 dest1, OutIt3 dest2, UnaryPredicate pred)
    -> std::tuple<OutIt1, OutIt2, OutIt3>
{
    return i::partition_3way_copy(adl::begin(r), adl::end(r), dest0, dest1, dest2,
                                  astl::pass_fn(pred));
}

// Note this algorithm is stable
template <typename R, typename OutIt1, typename OutIt2, typename OutIt3, typename UnaryPredicate,
          typename P>
auto partition_3way_copy(R &&r, OutIt1 dest0, OutIt2 dest1, OutIt3 dest2, UnaryPredicate pred, P p)
    -> std::tuple<OutIt1, OutIt2, OutIt3>
{
    return i::partition_3way_copy(adl::begin(r), adl::end(r), dest0, dest1, dest2,
                                  astl::combine(astl::pass_fn(pred), astl::pass_fn(p)));
}

// Note this algorithm is stable
template <typename R, typename OutIt1, typename OutIt2, typename OutIt3, typename UnaryPredicate>
// requires R ForwardIterator range
// requires OutIt1 OutputIterator
// requires OutIt2 OutputIterator
// requires OutIt3 OutputIterator
// requires UnaryPredicate, returns int, argument value_type(first)
auto partition_3way_move(R &&r, OutIt1 dest0, OutIt2 dest1, OutIt3 dest2, UnaryPredicate pred)
    -> std::tuple<OutIt1, OutIt2, OutIt3>
{
    return i::partition_3way_move(adl::begin(r), adl::end(r), dest0, dest1, dest2,
                                  astl::pass_fn(pred));
}

// Note this algorithm is stable
template <typename R, typename OutIt1, typename OutIt2, typename OutIt3, typename UnaryPredicate,
          typename P>
auto partition_3way_move(R &&r, OutIt1 dest0, OutIt2 dest1, OutIt3 dest2, UnaryPredicate pred, P p)
    -> std::tuple<OutIt1, OutIt2, OutIt3>
{
    return i::partition_3way_move(adl::begin(r), adl::end(r), dest0, dest1, dest2,
                                  astl::combine(astl::pass_fn(pred), astl::pass_fn(p)));
}

// Note this algorithm is stable
template <typename R, typename OutIt1, typename OutIt2, typename UnaryPredicate>
// requires R InputIterator range
// requires OutIt1 OutputIterator
// requires UnaryPredicate, returns bool, argument value_type(InIt)
auto partition_copy(R &&r, OutIt1 d_first_true, OutIt2 d_first_false, UnaryPredicate pred)
    -> std::pair<OutIt1, OutIt2>
{
    return i::partition_copy(adl::begin(r), adl::end(r), d_first_true, d_first_false,
                             astl::pass_fn(pred));
}

// Note this algorithm is stable
template <typename R, typename OutIt1, typename OutIt2, typename UnaryPredicate, typename P>
auto partition_copy(R &&r, OutIt1 d_first_true, OutIt2 d_first_false, UnaryPredicate pred, P p)
    -> std::pair<OutIt1, OutIt2>
{
    return i::partition_copy(adl::begin(r), adl::end(r), d_first_true, d_first_false,
                             astl::pass_fn(pred), astl::pass_fn(p));
}

// Note this algorithm is stable
template <typename R, typename N, typename OutIt1, typename OutIt2, typename UnaryPredicate>
// requires R InputIterator range
// requires OutIt1 OutputIterator
// requires UnaryPredicate, returns bool, argument value_type(InIt)
auto partition_copy_n(R &&r, N n, OutIt1 d_first_true, OutIt2 d_first_false, UnaryPredicate pred)
    -> std::tuple<OutIt1, OutIt2, astl::iter_of_range<R>>
{
    return i::partition_copy_n(adl::begin(r), n, d_first_true, d_first_false, astl::pass_fn(pred));
}

// Note this algorithm is stable
template <typename R, typename N, typename OutIt1, typename OutIt2, typename UnaryPredicate,
          typename P>
auto partition_copy_n(R &&r, N n, OutIt1 d_first_true, OutIt2 d_first_false, UnaryPredicate pred,
                      P p) -> std::tuple<OutIt1, OutIt2, astl::iter_of_range<R>>
{
    return i::partition_copy_n(adl::begin(r), n, d_first_true, d_first_false, astl::pass_fn(pred),
                               astl::pass_fn(p));
}

template <typename R, typename UnaryPredicate>
// requires R ForwardIterator range
// requires UnaryPredicate, return int, argument value_type(FwdIt)
auto partition_n_way(R &&r, UnaryPredicate pred) -> void
{
    i::partition_n_way(adl::begin(r), adl::end(r), astl::pass_fn(pred));
}

template <typename R, typename UnaryPredicate, typename P>
auto partition_n_way(R &&r, UnaryPredicate pred, P p) -> void
{
    i::partition_n_way(adl::begin(r), adl::end(r), astl::pass_fn(pred), astl::pass_fn(p));
}

// Note this algorithm is stable
template <typename R, typename OutIt1, typename OutIt2, typename UnaryPredicate>
// requires R InputIterator range
// requires OutIt1 OutputIterator
// requires UnaryPredicate, returns bool, argument value_type(InIt)
auto partition_move(R &&r, OutIt1 d_first_true, OutIt2 d_first_false, UnaryPredicate pred)
    -> std::pair<OutIt1, OutIt2>
{
    return i::partition_move(adl::begin(r), adl::end(r), d_first_true, d_first_false,
                             astl::pass_fn(pred));
}

// Note this algorithm is stable
template <typename R, typename OutIt1, typename OutIt2, typename UnaryPredicate, typename P>
auto partition_move(R &&r, OutIt1 d_first_true, OutIt2 d_first_false, UnaryPredicate pred, P p)
    -> std::pair<OutIt1, OutIt2>
{
    return i::partition_move(adl::begin(r), adl::end(r), d_first_true, d_first_false,
                             astl::pass_fn(pred), astl::pass_fn(p));
}

// Note this algorithm is stable
template <typename R, typename N, typename OutIt1, typename OutIt2, typename UnaryPredicate>
// requires R InputIterator range
// requires OutIt1 OutputIterator
// requires UnaryPredicate, returns bool, argument value_type(InIt)
auto partition_move_n(R &&r, N n, OutIt1 d_first_true, OutIt2 d_first_false, UnaryPredicate pred)
    -> std::tuple<OutIt1, OutIt2, astl::iter_of_range<R>>
{
    return i::partition_move_n(adl::begin(r), n, d_first_true, d_first_false, astl::pass_fn(pred));
}

// Note this algorithm is stable
template <typename R, typename N, typename OutIt1, typename OutIt2, typename UnaryPredicate,
          typename P>
auto partition_move_n(R &&r, N n, OutIt1 d_first_true, OutIt2 d_first_false, UnaryPredicate pred,
                      P p) -> std::tuple<OutIt1, OutIt2, astl::iter_of_range<R>>
{
    return i::partition_move_n(adl::begin(r), n, d_first_true, d_first_false, astl::pass_fn(pred),
                               astl::pass_fn(p));
}

template <typename R, typename N, typename UnaryPredicate>
// requires R ForwardIterator range
// requires UnaryPredicate, returns bool, argument value_type(R)
auto partition_n(R &&r, N n, UnaryPredicate pred) -> iter_of_range<R>
{
    return i::partition_n(adl::begin(r), n, astl::pass_fn(pred));
}

template <typename R, typename N, typename UnaryPredicate, typename P>
auto partition_n(R &&r, N n, UnaryPredicate pred, P p) -> iter_of_range<R>
{
    return i::partition_n(adl::begin(r), n, astl::pass_fn(pred), astl::pass_fn(p));
}

template <typename R, typename UnaryPredicate>
// requires R ForwardIterator range
// requires UnaryPredicate, return bool, argument value_type(R)
auto partition_point(R &&r, UnaryPredicate pred) -> iter_of_range<R>
{
    return i::partition_point(adl::begin(r), adl::end(r), astl::pass_fn(pred));
}

template <typename R, typename UnaryPredicate, typename P>
auto partition_point(R &&r, UnaryPredicate pred, P p) -> iter_of_range<R>
{
    return i::partition_point(adl::begin(r), adl::end(r), astl::pass_fn(pred), astl::pass_fn(p));
}

/* NOTE:
 * partition_point_n in lower_bound.hpp
 */

template <typename R, typename RandIt, typename Comparator>
// requires R RandomAccessIterator range
// requires RandIt RandomAccessIterator
// requires Comparator StrictWeakOrdering, arguments two value_type(RandIt)
auto partition_subrange(R &&r, RandIt sub_f, RandIt sub_l, Comparator comp)
    -> std::pair<RandIt, RandIt>
{
    // precondition r.begin() <= sub_f <= sub_l <= r.end()
    return i::partition_subrange(adl::begin(r), adl::end(r), sub_f, sub_l, astl::pass_fn(comp));
}

template <typename R, typename RandIt, typename Comparator, typename P>
auto partition_subrange(R &&r, RandIt sub_f, RandIt sub_l, Comparator comp, P p)
    -> std::pair<RandIt, RandIt>
{
    return i::partition_subrange(adl::begin(r), adl::end(r), sub_f, sub_l, astl::pass_fn(comp),
                                 astl::pass_fn(p));
}

template <typename R, typename BinaryPredicate>
// requires R ForwardIterator range
// requires BinaryPredicate, two arguments of value_type(R)
auto remove_duplicates(R r, BinaryPredicate pred) -> iter_of_range<R>
{
    return i::remove_duplicates(adl::begin(r), adl::end(r), astl::pass_fn(pred));
}

template <typename R>
// requires R ForwardIterator range
auto remove_duplicates(R r) -> iter_of_range<R>
{
    return i::remove_duplicates(adl::begin(r), adl::end(r));
}

template <typename R, typename BinaryPredicate, typename P>
auto remove_duplicates(R r, BinaryPredicate pred, P p) -> iter_of_range<R>
{
    return i::remove_duplicates(adl::begin(r), adl::end(r), astl::pass_fn(pred), astl::pass_fn(p));
}

template <typename InRng, typename OutRng, typename Comparator>
// requires InRng InputIterator range
// requires OutRng ForwardIterator range
// requires Comparator is StrictWeakOrdering on the value_type(InRng)
auto top_n(InRng &&i_r, OutRng &&o_r, Comparator c) -> OutRng
{
    return i::top_n(adl::begin(i_r), adl::end(i_r), adl::begin(o_r), adl::end(o_r),
                    astl::pass_fn(c));
}

template <typename InRng, typename OutRng>
// requires InRng InputIterator range
// requires OutRng ForwardIterator range
auto top_n(InRng &&i_r, OutRng &&o_r) -> OutRng
{
    return i::top_n(adl::begin(i_r), adl::end(i_r), adl::begin(o_r), adl::end(o_r));
}

template <typename InRng, typename OutRng, typename Comparator, typename P>
auto top_n(InRng &&i_r, OutRng &&o_r, Comparator c, P p) -> OutRng
{
    return i::top_n(adl::begin(i_r), adl::end(i_r), adl::begin(o_r), adl::end(o_r),
                    astl::pass_fn(c), astl::pass_fn(p));
}

} // namespace r
} // namespace astl

#endif // ASTL_INCLUDE_PARTITION_HPP
