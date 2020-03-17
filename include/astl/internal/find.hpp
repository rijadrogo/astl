//
// Created by Rijad on 29-Jul-18.
//

#ifndef ASTL_INCLUDE_FIND_HPP
#define ASTL_INCLUDE_FIND_HPP

#include <algorithm>
#include <cstring>
#include <type_traits>
#include <utility>

#include "astl/functional.hpp"
#include "astl/iterator.hpp"
#include "astl/map_iterator.hpp"
#include "astl/optional.hpp"
#include "astl/range_access.hpp"

namespace astl
{
namespace internal_find
{
template <int N, typename FwdIt, typename NaryPred>
auto find_if_adjacent1(FwdIt first, FwdIt last, NaryPred pred, iter_diff_type<FwdIt> d) -> FwdIt
{
    std::make_integer_sequence<int, N> seq{};
    d = d - N + 1;
    constexpr iter_diff_type<FwdIt> loop_unroll_num(4);
    auto trip_count(d >> 2);
    while (trip_count > 0) {
        if (internal::for_each1(pred, first, seq)) return first;

        ++first;
        if (internal::for_each1(pred, first, seq)) return first;

        ++first;
        if (internal::for_each1(pred, first, seq)) return first;

        ++first;
        if (internal::for_each1(pred, first, seq)) return first;

        ++first;
        d -= loop_unroll_num;
        --trip_count;
    }

    switch (d) {
    case 3:
        if (internal::for_each1(pred, first, seq)) return first;

        ++first;
        // fallthrough
    case 2:
        if (internal::for_each1(pred, first, seq)) return first;

        ++first;
        // fallthrough
    case 1:
        if (internal::for_each1(pred, first, seq)) return first;

        // fallthrough
    case 0:
    default: return last;
    }
}

template <typename InIt, typename T> auto within_limits(InIt, const T &e) -> bool
{ // check whether e is within the limits of Elem
    using Elem = std::remove_pointer_t<InIt>;

    constexpr bool signed_elem(std::is_signed<Elem>::value);
    constexpr bool signed_T(std::is_signed<T>::value);
    constexpr bool any(static_cast<T>(-1) == -1);

    if constexpr (std::is_same<T, bool>::value) { return true; }
    if constexpr (signed_elem && signed_T) {
        // signed Elem, signed T
        return SCHAR_MIN <= e && e <= SCHAR_MAX;
    }
    if constexpr (signed_elem && !signed_T && any) {
        // signed Elem, unsigned T, -1 == static_cast<T>(-1)
        return e <= SCHAR_MAX || static_cast<T>(SCHAR_MIN) <= e;
    }
    if constexpr (signed_elem && !signed_T && !any) {
        // signed Elem, unsigned T, -1 != static_cast<T>(-1)
        return e <= SCHAR_MAX;
    }
    if constexpr (!signed_elem && signed_T) {
        // unsigned Elem, signed T
        return 0 <= e && e <= UCHAR_MAX;
    }
    if constexpr (!signed_elem && !signed_T) {
        // unsigned Elem, unsigned T
        return e <= UCHAR_MAX;
    }
    return false; // NOT REACHABLE
}

} // namespace internal_find

namespace i
{
using std::find; // NOLINT(misc-unused-using-decls)
template <typename FwdIt, typename T, typename P>
ASTL_NODISCARD auto find(FwdIt first, FwdIt last, T const &value, P p) -> FwdIt
{
    auto proj(astl::pass_fn(p));
    return std::find(astl::map_iterator(first, proj), astl::map_iterator(last, proj), value).base();
}

template <typename InIt, typename OutIt, typename T>
// requires InIt InputIterator
// requires OutIt OutputIterator, InIt is assignable to value_type(OutIt)
// requires T equality comparable with value_type(InIt)
ASTL_NODISCARD auto find_all(InIt first, InIt last, OutIt dest, T const &value) -> OutIt
{
    while (first != last) {
        if (*first == value) {
            *dest = first;
            ++dest;
        }
        ++first;
    }
    return dest;
}

template <typename InIt, typename OutIt, typename T, typename P>
ASTL_NODISCARD auto find_all(InIt first, InIt const last, OutIt dest, T const &value, P p) -> OutIt
{
    while (first != last) {
        if (invoke(p, *first) == value) {
            *dest = first;
            ++dest;
        }
        ++first;
    }
    return dest;
}

template <typename InIt, typename OutIt, typename UnaryPredicate>
// requires InIt InputIterator
// requires OutIt OutputIterator, InIt is assignable to value_type(OutIt)
// requires UnaryPredicate, returns bool, argument value_type(InIt)
ASTL_NODISCARD auto find_all_if(InIt first, InIt const last, OutIt dest, UnaryPredicate pred)
    -> OutIt
{
    while (first != last) {
        if (pred(*first)) {
            *dest = first;
            ++dest;
        }
        ++first;
    }
    return dest;
}

template <typename InIt, typename OutIt, typename UnaryPredicate, typename P>
ASTL_NODISCARD auto find_all_if(InIt first, InIt last, OutIt dest, UnaryPredicate pred, P p)
    -> OutIt
{
    return i::find_all_if(first, last, dest, astl::combine(astl::pass_fn(pred), astl::pass_fn(p)));
}

template <typename InIt, typename N, typename OutIt, typename UnaryPredicate>
// requires InIt InputIterator
// requires N integral type
// requires OutIt OutputIterator, InIt is assignable to value_type(OutIt)
// requires UnaryPredicate, returns bool, argument value_type(InIt)
ASTL_NODISCARD auto find_all_if_n(InIt first, N n, OutIt dest, UnaryPredicate pred)
    -> std::pair<OutIt, InIt>
{
    while (n != N(0)) {
        if (pred(*first)) {
            *dest = first;
            ++dest;
        }
        ++first;
        --n;
    }
    return std::make_pair(dest, first);
}

template <typename InIt, typename N, typename OutIt, typename UnaryPredicate, typename P>
ASTL_NODISCARD auto find_all_if_n(InIt first, N n, OutIt dest, UnaryPredicate pred, P p) -> OutIt
{
    return i::find_all_if_n(first, n, dest, astl::combine(astl::pass_fn(pred), astl::pass_fn(p)));
}

template <typename InIt, typename N, typename OutIt, typename T>
// requires InIt InputIterator
// requires N integral type
// requires OutIt OutputIterator
// requires T equality comparable with value_type(InIt)
ASTL_NODISCARD auto find_all_n(InIt first, N n, OutIt dest, T const &value)
    -> std::pair<OutIt, InIt>
{
    while (n != N(0)) {
        if (*first == value) {
            *dest = first;
            ++dest;
        }
        ++first;
        --n;
    }
    return std::make_pair(dest, first);
}

template <typename InIt, typename N, typename OutIt, typename T, typename P>
ASTL_NODISCARD auto find_all_n(InIt first, N n, OutIt dest, T const &value, P p)
    -> std::pair<OutIt, InIt>
{
    while (n != N(0)) {
        if (invoke(p, *first) == value) {
            *dest = first;
            ++dest;
        }
        ++first;
        --n;
    }
    return std::make_pair(dest, first);
}

template <typename BidiIt, typename T>
// requires BidiIt BidirectionalIterator
// requires T, equality comparable with value_type(BidiIt)
ASTL_NODISCARD auto find_backward(BidiIt first, BidiIt last, T const &e) -> BidiIt
{
    BidiIt ret(last);
    while (true) {
        if (*--last == e) return last;

        if (last == first) break;
    }
    return ret;
}

template <typename BidiIt, typename T, typename P>
ASTL_NODISCARD auto find_backward(BidiIt first, BidiIt last, T const &e, P p) -> BidiIt
{
    BidiIt ret(last);
    while (true) {
        if (invoke(p, *--last) == e) return last;

        if (last == first) break;
    }
    return ret;
}

template <typename BidiIt, typename UnaryPredicate>
// requires BidiIt BidirectionalIterator
// requires UnaryPredicate returns bool, argument value_type(BidiIt)
ASTL_NODISCARD auto find_closest_if(BidiIt first, BidiIt pos, BidiIt last, UnaryPredicate pred,
                                    bool const skip_self = false) -> BidiIt
{
    // precondition first <= pos <= last
    if (first == last) // empty range
        return last;

    if (!skip_self && pos != last && pred(*pos)) // found at pos
        return pos;

    BidiIt find_left(astl::prev(pos));
    BidiIt find_right(astl::next(pos));
    // look on both sides, while right comes to the end or left comes to first
    while (find_right != last) {
        if (pred(*find_left)) return find_left;

        if (pred(*find_right)) return find_right;

        if (find_left == first) break;

        --find_left;
        ++find_right;
    }
    // if there are elements on right check them
    while (find_right != last) {
        if (pred(*find_right)) return find_right;

        ++find_right;
    }
    // if there are elements on left check them
    while (true) {
        if (pred(*find_left)) return find_left;

        if (find_left == first) break;

        --find_left;
    }
    return last;
}

template <typename BidiIt, typename UnaryPredicate, typename P>
ASTL_NODISCARD auto find_closest_if(BidiIt first, BidiIt last, BidiIt pos, UnaryPredicate pred, P p,
                                    bool skip_self = false) -> BidiIt
{
    return i::find_closest_if(first, last, pos,
                              astl::combine(astl::pass_fn(pred), astl::pass_fn(p)), skip_self);
}

using std::find_end; // NOLINT(misc-unused-using-decls)
template <typename FwdIt1, typename FwdIt2, typename BinaryPredicate, typename P1, typename P2 = P1>
ASTL_NODISCARD auto find_end(FwdIt1 first, FwdIt1 last, FwdIt2 s_first, FwdIt2 s_last,
                             BinaryPredicate pred, P1 p1, P2 p2 = P1{}) -> FwdIt1
{
    return std::find_end(first, last, s_first, s_last,
                         astl::lockstep(astl::pass_fn(pred), astl::pass_fn(p1), astl::pass_fn(p2)));
}

template <typename FwdIt1, typename N1, typename FwdIt2, typename N2, typename BinaryPredicate>
// requires FwdIt1 ForwardIterator
// requires N1 integral type
// requires FwdIt2 ForwardIterator
// requires N2 integral type
// requires BinaryPredicate, returns bool, arguments value_type(FwdIt1) and
// value_type(FwdIt2)
ASTL_NODISCARD auto find_end_n(FwdIt1 first1, N1 n1, FwdIt2 first2, N2 n2, BinaryPredicate pred)
    -> std::pair<FwdIt1, FwdIt1>
{
    if constexpr (is_random_access_it_v<FwdIt1, FwdIt2>) { // Random Access Iterators
        using RandIt1 = FwdIt1;
        RandIt1 found(std::find_end(first1, first1 + n1, first2, first2 + n2, astl::pass_fn(pred)));
        return std::make_pair(found, first1 + n1);
    }
    else { // Forward Iterators
        FwdIt1 result;
        while (true) { // try a match at first1
            FwdIt1 next1(first1);
            FwdIt2 next2(first2);
            N1 n11(0);
            N2 n22(0);
            while (true) { // test if [first2, first2 + n2) is a prefix of [first1,
                           // first1 + n1)
                bool const end_of_needle(n22 == n2);
                if (end_of_needle) { // match candidate found
                    result = first1;
                }

                if (n11 == n1) {
                    // trying the next candidate would make [first1, first1 + n1) shorter
                    // than [first2, first2
                    // + n2), done
                    return std::make_pair(result, first1);
                }

                if (end_of_needle || !pred(*next1, *next2)) {
                    break; // end of match or counterexample found, go to the next
                           // candidate
                }

                ++next1;
                ++n11;
                ++next2;
                ++n22;
            }

            ++first1;
            --n1;
        }
    }
}

template <typename FwdIt1, typename N1, typename FwdIt2, typename N2>
// requires FwdIt1 ForwardIterator
// requires N1 integral type
// requires FwdIt2 ForwardIterator
// requires N2 integral type
ASTL_NODISCARD auto find_end_n(FwdIt1 first1, N1 n1, FwdIt2 first2, N2 n2) -> FwdIt1
{
    return i::find_end_n(first1, n1, first2, n2, std::equal_to{});
}

template <typename FwdIt1, typename N1, typename FwdIt2, typename N2, typename BinaryPredicate,
          typename P1, typename P2>
ASTL_NODISCARD auto find_end_n(FwdIt1 first1, N1 n1, FwdIt2 first2, N2 n2, BinaryPredicate pred,
                               P1 p1, P2 p2 = P1{}) -> FwdIt1
{
    return i::find_end_n(first1, n1, first2, n2,
                         astl::lockstep(astl::pass_fn(pred), astl::pass_fn(p1), astl::pass_fn(p2)));
}

using std::find_first_of; // NOLINT(misc-unused-using-decls)
template <typename FwdIt1, typename FwdIt2, typename BinaryPredicate, typename P1, typename P2>
ASTL_NODISCARD auto find_first_of(FwdIt1 first, FwdIt1 last, FwdIt2 s_first, FwdIt2 s_last,
                                  BinaryPredicate pred, P1 p1, P2 p2) -> FwdIt1
{
    return std::find_first_of(
        first, last, s_first, s_last,
        astl::lockstep(astl::pass_fn(pred), astl::pass_fn(p1), astl::pass_fn(p2)));
}

template <typename FwdIt1, typename N1, typename FwdIt2, typename N2, typename BinaryPredicate>
// requires FwdIt1 ForwardIterator
// requires N1 integral type
// requires FwdIt2 ForwardIterator
// requires N2 integral type
// requires BinaryPredicate, returns bool, arguments value_type(FwdIt1) and
// value_type(FwdIt2)
ASTL_NODISCARD auto find_first_of_n(FwdIt1 first1, N1 n1, FwdIt2 first2, N2 n2,
                                    BinaryPredicate pred) -> FwdIt1
{
    while (n1 != N1(0)) {
        FwdIt2 i(first2);
        N2 n22(n2);
        while (n22 != N2(0)) {
            if (pred(*first1, *i)) return first1;

            ++i;
            --n22;
        }
        ++first1;
        --n1;
    }
    return first1;
}

template <typename FwdIt1, typename N1, typename FwdIt2, typename N2>
// requires FwdIt1 ForwardIterator
// requires N1 integral type
// requires FwdIt2 ForwardIterator
// requires N2 integral type
ASTL_NODISCARD auto find_first_of_n(FwdIt1 first1, N1 n1, FwdIt2 first2, N2 n2) -> FwdIt1
{
    return i::find_first_of_n(first1, n1, first2, n2, std::equal_to{});
}

template <typename FwdIt1, typename N1, typename FwdIt2, typename N2, typename BinaryPredicate,
          typename P1, typename P2>
ASTL_NODISCARD auto find_first_of_n(FwdIt1 first1, N1 n1, FwdIt2 first2, N2 n2,
                                    BinaryPredicate pred, P1 p1, P2 p2 = P1{}) -> FwdIt1
{
    return i::find_first_of_n(
        first1, n1, first2, n2,
        astl::lockstep(astl::pass_fn(pred), astl::pass_fn(p1), astl::pass_fn(p2)));
}

using std::find_if; // NOLINT(misc-unused-using-decls)
template <typename InIt, typename UnaryPredicate, typename P>
ASTL_NODISCARD auto find_if(InIt first, InIt last, UnaryPredicate pred, P p) -> InIt
{
    return std::find_if(first, last, astl::combine(astl::pass_fn(pred), astl::pass_fn(p)));
}

template <int N = 2, typename FwdIt, typename NaryPred>
ASTL_NODISCARD auto find_if_adjacent(FwdIt first, FwdIt last, NaryPred pred) ->
    typename std::enable_if<(N > 0), FwdIt>::type
{
    return internal_find::find_if_adjacent1<N>(first, last, astl::pass_fn(pred),
                                               astl::distance(first, last));
}

template <int N = 2, typename FwdIt, typename NaryPred, typename P>
ASTL_NODISCARD auto find_if_adjacent(FwdIt first, FwdIt last, NaryPred pred, P p) ->
    typename std::enable_if<(N > 0), FwdIt>::type
{
    return i::find_if_adjacent<N>(first, last,
                                  astl::combine(astl::pass_fn(pred), astl::pass_fn(p)));
}

template <typename BidiIt, typename UnaryPredicate>
// requires BidiIt BidirectionalIterator
// requires UnaryPredicate, returns bool, argument value_type(BidiIt)
ASTL_NODISCARD auto find_if_backward(BidiIt first, BidiIt last, UnaryPredicate pred) -> BidiIt
{
    BidiIt ret(last);
    if (first == last) return ret;

    while (true) {
        if (pred(*--last)) return last;

        if (last == first) break;
    }
    return ret;
}

template <typename BidiIt, typename UnaryPredicate, typename P>
ASTL_NODISCARD auto find_if_backward(BidiIt first, BidiIt last, UnaryPredicate pred, P p) -> BidiIt
{
    BidiIt ret(last);
    if (first == last) return ret;

    while (true) {
        if (pred(invoke(p, *--last))) return last;

        if (last == first) break;
    }
    return ret;
}

template <typename InIt, typename N, typename UnaryPredicate>
// requires InIt InputIterator
// requires N integral type
// requires UnaryPredicate, returns bool, argument value_type(InIt)
ASTL_NODISCARD auto find_if_n(InIt first, N n, UnaryPredicate pred) -> std::pair<InIt, N>
{
    N ret(n);
    auto trip_count(n >> 2);
    while (trip_count > 0) {
        if (pred(*first)) return std::make_pair(first, --n);

        ++first;
        --n;
        if (pred(*first)) return std::make_pair(first, --n);

        ++first;
        --n;
        if (pred(*first)) return std::make_pair(first, --n);

        ++first;
        --n;
        if (pred(*first)) return std::make_pair(first, --n);

        ++first;
        --n;
        --trip_count;
    }
    switch (integral_t<N>(n)) {
    case 3:
        if (pred(*first)) return std::make_pair(first, --n);

        ++first;
        --n;
        // fallthrough
    case 2:
        if (pred(*first)) return std::make_pair(first, --n);

        ++first;
        --n;
        // fallthrough
    case 1:
        if (pred(*first)) return std::make_pair(first, --n);
        ++first;
        // fallthrough
    case 0:
    default: return std::make_pair(first, ret);
    }
}

template <typename InIt, typename N, typename UnaryPredicate, typename P>
ASTL_NODISCARD auto find_if_n(InIt first, N n, UnaryPredicate pred, P p) -> std::pair<InIt, N>
{
    return i::find_if_n(first, n, astl::combine(astl::pass_fn(pred), astl::pass_fn(p)));
}

using std::find_if_not; // NOLINT(misc-unused-using-decls)
template <typename FwdIt, typename UnaryPredicate, typename P>
ASTL_NODISCARD auto find_if_not(FwdIt first, FwdIt last, UnaryPredicate pred, P p) -> FwdIt
{
    return std::find_if_not(first, last, astl::combine(astl::pass_fn(pred), astl::pass_fn(p)));
}

template <typename InIt, typename N, typename UnaryPredicate>
// requires InIt is InputIterator,
// requires N is integral type,
// requires UnaryPredicate, return bool, argument value_type(InIt)
ASTL_NODISCARD auto find_if_not_n(InIt first, N n, UnaryPredicate pred) -> std::pair<InIt, N>
{
    return i::find_if_n(first, n, astl::not_fn(astl::pass_fn(pred)));
}

template <typename InIt, typename UnaryPredicate, typename N, typename P>
ASTL_NODISCARD auto find_if_not_n(InIt first, N len, UnaryPredicate pred, P p) -> std::pair<InIt, N>
{
    return i::find_if_not_n(first, len, astl::combine(astl::pass_fn(pred), astl::pass_fn(p)));
}

template <typename FwdIt, typename UnaryPredicate>
// requires FwdIt ForwardIterator
// requires UnaryPredicate, returns bool, argument value_type(FwdIt)
ASTL_NODISCARD auto find_if_unique(FwdIt first, FwdIt last, UnaryPredicate pred) -> FwdIt
{
    auto p(astl::pass_fn(pred));
    FwdIt find_first(i::find_if(first, last, p));
    if (find_first == last) return last;

    FwdIt find_again(i::find_if(astl::next(find_first), last, p));
    return find_again == last ? find_first : last;
}

template <typename FwdIt, typename BinaryPredicate, typename P>
ASTL_NODISCARD auto find_if_unique(FwdIt first, FwdIt last, BinaryPredicate pred, P p) -> FwdIt
{
    return i::find_if_unique(first, last, astl::combine(astl::pass_fn(pred), astl::pass_fn(p)));
}

template <typename FwdIt, typename N, typename UnaryPredicate>
// requires FwdIt ForwardIterator
// requires UnaryPredicate, returns bool, argument value_type(FwdIt)
ASTL_NODISCARD auto find_if_unique_n(FwdIt first, N n, UnaryPredicate pred) -> std::pair<FwdIt, N>
{
    auto pr(astl::pass_fn(pred));
    using PairType = std::pair<FwdIt, N>;

    PairType find_first(i::find_if_n(first, n, pr));

    if (find_first.second == n || find_first.second == N(0)) return find_first;

    PairType find_again(i::find_if_n(astl::next(find_first.first), find_first.second, pr));

    if (find_again.second == find_first.second) return find_first;

    find_again.second = n;
    return find_again;
}

template <typename FwdIt, typename N, typename BinaryPredicate, typename P>
ASTL_NODISCARD auto find_if_unique_n(FwdIt first, N n, BinaryPredicate pred, P p)
    -> std::pair<FwdIt, N>
{
    return i::find_if_unique_n(first, n, astl::combine(astl::pass_fn(pred), astl::pass_fn(p)));
}

template <typename FwdIt, typename UnaryPredicate, typename Comparator>
// requires FwdIt ForwardIterator
// requires UnaryPredicate, returns bool, argument value_type(FwdIt)
// requires Comparator, returns bool, argument value_type(FwdIt)
ASTL_NODISCARD auto find_if_while(FwdIt first, FwdIt last, UnaryPredicate pred, Comparator comp)
    -> FwdIt
{
    while (first != last && pred(*first)) {
        if (comp(*first)) return first;

        ++first;
    }
    return last;
}

template <typename FwdIt, typename UnaryPredicate, typename Comparator, typename Pc,
          typename Pup = Pc>
ASTL_NODISCARD auto find_if_while(FwdIt first, FwdIt last, UnaryPredicate pred, Comparator comp,
                                  Pc pc, Pup p = Pc{}) -> FwdIt
{
    return i::find_if_while(first, last, astl::combine(astl::pass_fn(pred), astl::pass_fn(p)),
                            astl::combine(astl::pass_fn(comp), astl::pass_fn(pc)));
}

// Function to find contiguous sub-array with the largest sum
// in given set of integers (handles negative numbers as well)
template <typename FwdIt, typename BinaryOp, typename CompareF>
// requires FwdIt ForwardIterator
// requires BinaryPredicate, returns ValueType(FwdIt), two arguments of
// ValueType(FwdIt) requires CompareF StrictWeakOrdering
ASTL_NODISCARD auto find_max_sum_of_subarray(FwdIt first, FwdIt last, BinaryOp op, CompareF comp)
    -> std::tuple<FwdIt, FwdIt, optional<astl::iter_value_type<FwdIt>>>
{
    using Ret = std::tuple<FwdIt, FwdIt, optional<iter_value_type<FwdIt>>>;
    if (first == last) return Ret{first, last, nullopt};

    FwdIt s(first);
    FwdIt start(first);
    FwdIt end(first);

    // stores maximum sum sub-array found so far
    auto max_so_far(*first);

    // stores maximum sum of sub-array ending at current position
    auto max_ending_here(*first);

    ++first;
    // traverse the given array
    while (first != last) {
        max_ending_here = op(max_ending_here, *first);

        if (comp(max_so_far, max_ending_here)) {
            max_so_far = max_ending_here;
            start = s;
            end = astl::next(first);
        }

        if (comp(max_ending_here, *first)) {
            max_ending_here = *first;
            s = first;
        }

        ++first;
    }
    return Ret{start, end, astl::make_optional(*max_so_far)};
}

template <typename FwdIt>
// requires FwdIt ForwardIterator
ASTL_NODISCARD auto find_max_sum_of_subarray(FwdIt first, FwdIt last)
    -> std::tuple<FwdIt, FwdIt, optional<astl::iter_value_type<FwdIt>>>
{
    return i::find_max_sum_of_subarray(first, last, std::plus{}, std::less{});
}

template <typename FwdIt, typename BinaryOp, typename CompareF, typename P>
ASTL_NODISCARD auto find_max_sum_of_subarray(FwdIt first, FwdIt last, BinaryOp op, CompareF comp,
                                             P p)
    -> std::tuple<FwdIt, FwdIt, optional<astl::iter_value_type<FwdIt>>>
{
    auto pp(astl::pass_fn(p));
    return i::find_max_sum_of_subarray(astl::map_iterator(first, pp), astl::map_iterator(last, pp),
                                       astl::pass_fn(op), astl::pass_fn(comp));
}

template <typename R, typename N, typename T>
ASTL_NODISCARD auto find_n(R &&r, N n, T const &e) -> std::pair<astl::iter_of_range<R>, N>
{
    return i::find_n(adl::begin(r), n, e);
}

template <typename InIt, typename N, typename T>
// requires FwdIt ForwardIterator
// requires N integral type
// requires T, equality comparable with value_type(InIt)
ASTL_NODISCARD auto find_n(InIt first, N n, T const &e) -> std::pair<InIt, N>
{
    // find first matching e; choose optimization
    // activate optimization for pointers to (const) bytes and integral values

    if (n == N(0)) return std::make_pair(first, n);

    constexpr bool memchr_opt(
        std::is_integral<T>::value
        && (std::is_same<T, char *>::value || std::is_same<T, signed char *>::value
            || std::is_same<T, unsigned char *>::value || std::is_same<T, char const *>::value
            || std::is_same<T, signed char const *>::value
            || std::is_same<T, unsigned char const *>::value));

    if constexpr (memchr_opt) { // find first byte matching integral e
        if (!internal_find::within_limits(first, e)) return std::make_pair(first, n);

        auto found(static_cast<InIt>(
            std::memchr(first, static_cast<unsigned char>(e), static_cast<std::size_t>(n))));

        if (found) return std::make_pair(found, n - (found - first) - 1);

        return std::make_pair(first, n);
    }
    else { // find first matching e
        N ret(n);
        auto trip_count(n >> 2);
        while (trip_count > 0) {
            if (*first == e) return std::make_pair(first, --n);

            ++first;
            --n;
            if (*first == e) return std::make_pair(first, --n);

            ++first;
            --n;
            if (*first == e) return std::make_pair(first, --n);

            ++first;
            --n;
            if (*first == e) return std::make_pair(first, --n);

            ++first;
            --n;
            --trip_count;
        }

        switch (integral_t<N>(n)) {
        case 3:
            if (*first == e) return std::make_pair(first, --n);

            ++first;
            --n;
            // fallthrough
        case 2:
            if (*first == e) return std::make_pair(first, --n);

            ++first;
            --n;
            // fallthrough
        case 1:
            if (*first == e) return std::make_pair(first, --n);
            ++first;
            // fallthrough
        case 0:
        default: return std::make_pair(first, ret);
        }
    }
}

template <typename FwdIt, typename N, typename T, typename P>
ASTL_NODISCARD auto find_n(FwdIt first, N n, T const &e, P p) -> std::pair<FwdIt, N>
{
    N ret(n);
    auto trip_count(n >> 2);
    while (trip_count > 0) {
        if (invoke(p, *first) == e) return std::make_pair(first, --n);

        ++first;
        --n;
        if (invoke(p, *first) == e) return std::make_pair(first, --n);

        ++first;
        --n;
        if (invoke(p, *first) == e) return std::make_pair(first, --n);

        ++first;
        --n;
        if (invoke(p, *first) == e) return std::make_pair(first, --n);

        ++first;
        --n;
        --trip_count;
    }

    switch (integral_t<N>(n)) {
    case 3:
        if (invoke(p, *first) == e) return std::make_pair(first, --n);

        ++first;
        --n;
        // fallthrough
    case 2:
        if (invoke(p, *first) == e) return std::make_pair(first, --n);

        ++first;
        --n;
        // fallthrough
    case 1:
        if (invoke(p, *first) == e) return std::make_pair(first, --n);
        ++first;
        // fallthrough
    case 0:
    default: return std::make_pair(first, ret);
    }
}

template <typename InIt, typename T>
// requires FwdIt ForwardIterator
// requires T, inequality comparable with value_type(FwdIt)
ASTL_NODISCARD auto find_not(InIt first, InIt last, T const &val) -> InIt
{
    return std::find_if(first, last, astl::bind2nd(std::not_equal_to{}, val));
}

template <typename FwdIt, typename T, typename P>
ASTL_NODISCARD auto find_not(FwdIt first, FwdIt last, T const &val, P p) -> FwdIt
{
    return std::find_if(first, last,
                        [p(astl::pass_fn(p)), &val](auto &&x) { return invoke(p, x) != val; });
}

template <typename FwdIt, typename N, typename T>
// requires FwdIt ForwardIterator
// requires N integral type
// requires T, inequality comparable with value_type(FwdIt)
ASTL_NODISCARD auto find_not_n(FwdIt first, N n, T const &val) -> std::pair<FwdIt, N>
{
    return i::find_if_n(first, n, astl::bind2nd(std::not_equal_to{}, val));
}

template <typename FwdIt, typename N, typename T, typename P>
ASTL_NODISCARD auto find_not_n(FwdIt first, N n, T const &val, P p) -> std::pair<FwdIt, N>
{
    return i::find_if_n(first, n,
                        [&val, p(astl::pass_fn(p))](auto &&x) { return invoke(p, x) != val; });
}

template <typename BidIt, typename BinaryPredicate>
// requires BidIt BidirectionalIterator
// requires BinaryPredicate, returns bool, arguments two of value_type(BidIt)
ASTL_NODISCARD auto find_peek(BidIt first, BidIt last, BinaryPredicate pred) -> BidIt
{
    using N = iter_diff_type<BidIt>;
    N start(0);
    N end(astl::distance(first, last) - 1);
    if (end == 1)
        // notice - 1                ^^^^
        // only two elements
        return pred(*first, *--last) ? last : first;

    while (true) {
        N n((end - start) >> 1);
        BidIt mid(astl::next(first, n));
        BidIt mid_next(astl::next(mid));
        BidIt mid_prev(astl::prev(mid));

        // Compare middle element with its neighbours (if neighbours exist)
        if ((mid == first || !pred(*mid, *mid_prev)) && (mid == last || !pred(*mid, *mid_next)))
            return mid;

        // If middle element is not peak and its left neighbour is greater than it,
        // then left half must have a peak element
        if (mid != first && pred(*mid, *mid_prev)) end = n + 1;

        // If middle element is not peak and its right neighbour is greater than it,
        // then right half must have a peak element
        else
            start = n;
    }
}

template <typename BidIt>
// requires BidIt BidirectionalIterator
ASTL_NODISCARD auto find_peek(BidIt first, BidIt last) -> BidIt
{
    return i::find_peek(first, last, std::less{});
}

template <typename BidIt, typename BinaryPredicate, typename P>
ASTL_NODISCARD auto find_peek(BidIt first, BidIt last, BinaryPredicate pred, P p) -> BidIt
{
    return i::find_peek(first, last, astl::combine(astl::pass_fn(pred), astl::pass_fn(p)));
}

template <typename FwdIt, typename T>
// requires FwdIt ForwardIterator
// requires T, inequality comparable with value_type(FwdIt)
ASTL_NODISCARD auto find_range(FwdIt first, FwdIt last, T const &val) -> std::pair<FwdIt, FwdIt>
{
    first = i::find(first, last, val);
    if (first != last) last = i::find_not(astl::next(first), last, val);

    return std::make_pair(first, last);
}

template <typename FwdIt, typename T, typename P>
ASTL_NODISCARD auto find_range(FwdIt first, FwdIt last, T const &val, P p)
    -> std::pair<FwdIt, FwdIt>
{
    auto proj(astl::pass_fn(p));
    first = i::find(first, last, val, proj);
    if (first != last) last = i::find_not(astl::next(first), last, val, proj);

    return std::make_pair(first, last);
}

template <typename FwdIt, typename UnaryPredicate>
// requires FwdIt ForwardIterator
// requires UnaryPredicate, return bool, argument value_type(FwdIt)
ASTL_NODISCARD auto find_range_if(FwdIt first, FwdIt last, UnaryPredicate pred)
    -> std::pair<FwdIt, FwdIt>
{
    auto pr(astl::pass_fn(pred));
    first = i::find_if(first, last, pr);
    if (first != last) last = i::find_if_not(astl::next(first), last, pr);

    return std::make_pair(first, last);
}

template <typename FwdIt, typename UnaryPredicate, typename P>
ASTL_NODISCARD auto find_range_if(FwdIt first, FwdIt last, UnaryPredicate pred, P p)
    -> std::pair<FwdIt, FwdIt>
{
    return i::find_range_if(first, last, astl::combine(astl::pass_fn(pred), astl::pass_fn(p)));
}

template <typename FwdIt, typename N, typename UnaryPredicate>
// requires FwdIt ForwardIterator
// requires N integral type
// requires UnaryPredicate, return bool, argument value_type(FwdIt)
ASTL_NODISCARD auto find_range_if_n(FwdIt first, N n, UnaryPredicate pred)
    -> std::pair<FwdIt, FwdIt>
{
    auto pr(astl::pass_fn(pred));
    std::pair<FwdIt, N> i(i::find_if_n(first, n, pr));
    if (i.second == n) return std::make_pair(i.first, i.first);

    std::pair<FwdIt, N> j(i::find_if_not_n(astl::next(i.first), --i.second, pr));
    return std::make_pair(i.first, j.first);
}

template <typename FwdIt, typename N, typename UnaryPredicate, typename P>
ASTL_NODISCARD auto find_range_if_n(FwdIt first, N n, UnaryPredicate pred, P p)
    -> std::pair<FwdIt, FwdIt>
{
    return i::find_range_if_n(first, n, astl::combine(astl::pass_fn(pred), astl::pass_fn(p)));
}

template <typename FwdIt, typename N, typename T>
// requires FwdIt ForwardIterator
// requires N integral type
// requires T, inequality comparable with value_type(FwdIt)
ASTL_NODISCARD auto find_range_n(FwdIt first, N n, T const &val) -> std::pair<FwdIt, FwdIt>
{
    std::pair<FwdIt, N> i(i::find_n(first, n, val));
    if (i.second == n) return std::make_pair(i.first, i.first);

    std::pair<FwdIt, N> j(i::find_not_n(astl::next(i.first), --i.second, val));
    return std::make_pair(i.first, j.first);
}

template <typename FwdIt, typename N, typename T, typename P>
ASTL_NODISCARD auto find_range_n(FwdIt first, N n, T const &val, P p) -> std::pair<FwdIt, FwdIt>
{
    auto proj(astl::pass_fn(p));
    std::pair<FwdIt, N> i(i::find_n(first, n, val, proj));
    if (i.second == N(0)) return std::make_pair(i.first, i.first);

    std::pair<FwdIt, N> j(i::find_not_n(astl::next(i.first), --i.second, val, proj));
    return std::make_pair(i.first, j.first);
}

template <typename FwdIt, typename T>
// requires FwdIt ForwardIterator
// requires T, equality comparable with value_type(FwdIt)
ASTL_NODISCARD auto find_unique(FwdIt first, FwdIt last, T const &value) -> FwdIt
{
    FwdIt find_first(std::find(first, last, value));
    if (find_first == last) return last;

    FwdIt find_again(std::find(astl::next(find_first), last, value));
    return find_again == last ? find_first : last;
}

template <typename FwdIt, typename T, typename P>
ASTL_NODISCARD auto find_unique(FwdIt first, FwdIt last, T const &value, P p) -> FwdIt
{
    auto proj(astl::pass_fn(p));
    FwdIt find_first(i::find(first, last, value, proj));
    if (find_first == last) return last;

    FwdIt find_again(i::find(astl::next(find_first), last, value, proj));
    return find_again == last ? find_first : last;
}

template <typename FwdIt, typename N, typename T>
// requires FwdIt ForwardIterator
// requires N integral type
// requires T, equality comparable with value_type(FwdIt)
ASTL_NODISCARD auto find_unique_n(FwdIt first, N n, T const &value) -> std::pair<FwdIt, N>
{
    using PairType = std::pair<FwdIt, N>;
    PairType find_first(i::find_n(first, n, value));

    if (find_first.second == n || find_first.second == N(0)) return find_first;

    PairType find_again(i::find_n(astl::next(find_first.first), find_first.second, value));
    if (find_again.second == find_first.second) {
        ++find_first.second;
        return find_first;
    }

    find_again.second = n;
    return find_again;
}

template <typename FwdIt, typename N, typename T, typename P>
ASTL_NODISCARD auto find_unique_n(FwdIt first, N n, T const &value, P p) -> FwdIt
{
    using PairType = std::pair<FwdIt, N>;
    auto proj(astl::pass_fn(p));
    PairType find_first(i::find_n(first, n, value, proj));

    if (find_first.second == n || find_first.second == N(0)) return find_first;

    PairType find_again(i::find_n(astl::next(find_first.first), find_first.second, value, proj));
    if (find_again.second == find_first.second) return find_first;

    find_again.second = n;
    return find_again;
}

template <typename InIt, typename T, typename UnaryPredicate>
// requires InIt InputIterator
// requires T, equality comparable with value_type(FwdIt)
// requires UnaryPredicate, returns bool, argument value_type(FwdIt)
ASTL_NODISCARD auto find_while(InIt first, InIt last, T const &value, UnaryPredicate pred) -> InIt
{
    while (first != last && pred(*first)) {
        if (*first == value) return first;

        ++first;
    }
    return last;
}

template <typename InIt, typename T, typename UnaryPredicate, typename P1, typename P2 = P1>
ASTL_NODISCARD auto find_while(InIt first, InIt last, T const &value, UnaryPredicate pred, P1 p1,
                               P2 p2 = P1{}) -> InIt
{
    while (first != last && pred(astl::invoke(p2, *first))) {
        if (astl::invoke(p1, *first) == value) return first;

        ++first;
    }
    return last;
}

} // namespace i

namespace internal_find
{
template <typename R, typename T>
auto find1(R &&r, T &&value, internal_adl::rank<0>) -> iter_of_range<R>
{
    return i::find(adl::begin(r), adl::end(r), value);
}

template <typename R, typename T>
auto find1(R &&r, T &&value, internal_adl::rank<1>) -> decltype(r.find(static_cast<T &&>(value)))
{
    return r.find(static_cast<T &&>(value));
}
} // namespace internal_find

namespace r
{
template <typename R, typename T>
ASTL_NODISCARD auto find(R &&r, T &&value)
    -> decltype(internal_find::find1(r, static_cast<T &&>(value), internal_adl::rank<1>{}))
{
    return internal_find::find1(r, static_cast<T &&>(value), internal_adl::rank<1>{});
}

template <typename R, typename T, typename P>
ASTL_NODISCARD auto find(R &&r, T const &value, P p) -> iter_of_range<R>
{
    return i::find(adl::begin(r), adl::end(r), value, astl::pass_fn(p));
}

template <typename R, typename OutIt, typename T>
ASTL_NODISCARD auto find_all(R &&r, OutIt dest, T const &value) -> OutIt
{
    return i::find_all(adl::begin(r), adl::end(r), dest, value);
}

template <typename R, typename OutIt, typename T, typename P>
ASTL_NODISCARD auto find_all(R &&r, OutIt dest, T const &value, P p) -> OutIt
{
    return i::find_all(adl::begin(r), adl::end(r), dest, value, astl::pass_fn(p));
}

template <typename R, typename OutIt, typename UnaryPredicate>
ASTL_NODISCARD auto find_all_if(R &&r, OutIt dest, UnaryPredicate pred) -> OutIt
{
    return i::find_all_if(adl::begin(r), adl::end(r), dest, astl::pass_fn(pred));
}

template <typename R, typename OutIt, typename UnaryPredicate, typename P>
ASTL_NODISCARD auto find_all_if(R &&r, OutIt dest, UnaryPredicate pred, P p) -> OutIt
{
    return i::find_all_if(adl::begin(r), adl::end(r), dest, astl::pass_fn(pred), astl::pass_fn(p));
}

template <typename R, typename N, typename OutIt, typename UnaryPredicate>
ASTL_NODISCARD auto find_all_if_n(R &&r, N n, OutIt dest, UnaryPredicate pred) -> OutIt
{
    return i::find_all_if_n(adl::begin(r), n, dest, astl::pass_fn(pred));
}

template <typename R, typename N, typename OutIt, typename UnaryPredicate, typename P>
ASTL_NODISCARD auto find_all_if_n(R &&r, N n, OutIt dest, UnaryPredicate pred, P p) -> OutIt
{
    return i::find_all_if_n(adl::begin(r), n, dest, astl::pass_fn(pred), astl::pass_fn(p));
}

template <typename R, typename N, typename OutIt, typename T>
ASTL_NODISCARD auto find_all_n(R &&r, N n, OutIt dest, T const &value) -> OutIt
{
    return i::find_all_n(adl::begin(r), n, dest, value);
}

template <typename R, typename N, typename OutIt, typename T, typename P>
ASTL_NODISCARD auto find_all_n(R &&r, N n, OutIt dest, T const &value, P p) -> OutIt
{
    return i::find_all_n(adl::begin(r), n, dest, value, astl::pass_fn(p));
}

template <typename R, typename T>
ASTL_NODISCARD auto find_backward(R &&r, T const &value) -> iter_of_range<R>
{
    return i::find_backward(adl::begin(r), adl::end(r), value);
}

template <typename R, typename T, typename P>
ASTL_NODISCARD auto find_backward(R &&r, T const &value, P p) -> iter_of_range<R>
{
    return i::find_backward(adl::begin(r), adl::end(r), value, astl::pass_fn(p));
}

template <typename R, typename I, typename UnaryPredicate>
ASTL_NODISCARD auto find_closest_if(R &&r, I pos, UnaryPredicate pred, bool skip_self = false)
    -> iter_of_range<R>
{
    return i::find_closest_if(adl::begin(r), adl::end(r), pos, astl::pass_fn(pred), skip_self);
}

template <typename R, typename I, typename UnaryPredicate, typename P>
ASTL_NODISCARD auto find_closest_if(R &&r, I pos, UnaryPredicate pred, P p, bool skip_self = false)
    -> iter_of_range<R>
{
    return i::find_closest_if(adl::begin(r), adl::end(r), pos, astl::pass_fn(pred),
                              astl::pass_fn(p), skip_self);
}

template <typename R1, typename R2>
ASTL_NODISCARD auto find_end(R1 &&r1, R2 &&r2) -> iter_of_range<R1>
{
    return i::find_end(adl::begin(r1), adl::end(r1), adl::begin(r2), adl::end(r2));
}

template <typename R1, typename R2, typename BinaryPredicate>
ASTL_NODISCARD auto find_end(R1 &&r1, R2 &&r2, BinaryPredicate pred) -> iter_of_range<R1>
{
    return i::find_end(adl::begin(r1), adl::end(r1), adl::begin(r2), adl::end(r2),
                       astl::pass_fn(pred));
}

template <typename R1, typename R2, typename BinaryPredicate, typename P1, typename P2 = P1>
ASTL_NODISCARD auto find_end(R1 &&r1, R2 &&r2, BinaryPredicate pred, P1 p1, P2 p2 = P1{})
    -> iter_of_range<R1>
{
    return i::find_end(adl::begin(r1), adl::end(r1), adl::begin(r2), adl::end(r2),
                       astl::pass_fn(pred), astl::pass_fn(p1), astl::pass_fn(p2));
}

template <typename R1, typename N1, typename R2, typename N2>
ASTL_NODISCARD auto find_end_n(R1 &&r1, N1 n1, R2 &&r2, N2 n2) -> iter_of_range<R1>
{
    return i::find_end_n(adl::begin(r1), n1, adl::begin(r2), n2);
}

template <typename R1, typename N1, typename R2, typename N2, typename BinaryPredicate>
ASTL_NODISCARD auto find_end_n(R1 &&r1, N1 n1, R2 &&r2, N2 n2, BinaryPredicate pred)
    -> iter_of_range<R1>
{
    return i::find_end_n(adl::begin(r1), n1, adl::begin(r2), n2, astl::pass_fn(pred));
}

template <typename R1, typename N1, typename R2, typename N2, typename BinaryPredicate, typename P1,
          typename P2 = P1>
ASTL_NODISCARD auto find_end_n(R1 &&r1, N1 n1, R2 &&r2, N2 n2, BinaryPredicate pred, P1 p1,
                               P2 p2 = P1{}) -> iter_of_range<R1>
{
    return i::find_end_n(adl::begin(r1), n1, adl::begin(r2), n2, astl::pass_fn(pred),
                         astl::pass_fn(p1), astl::pass_fn(p2));
}

template <typename R1, typename R2>
ASTL_NODISCARD auto find_first_of(R1 &&r1, R2 &&r2) -> iter_of_range<R1>
{
    return i::find_first_of(adl::begin(r1), adl::end(r1), adl::begin(r2), adl::end(r2));
}

template <typename R1, typename R2, typename BinaryPredicate>
ASTL_NODISCARD auto find_first_of(R1 &&r1, R2 &&r2, BinaryPredicate pred) -> iter_of_range<R1>
{
    return i::find_first_of(adl::begin(r1), adl::end(r1), adl::begin(r2), adl::end(r2),
                            astl::pass_fn(pred));
}

template <typename R1, typename R2, typename BinaryPredicate, typename P1, typename P2 = P1>
ASTL_NODISCARD auto find_first_of(R1 &&r1, R2 &&r2, BinaryPredicate pred, P1 p1, P2 p2 = P1{})
    -> iter_of_range<R1>
{
    return i::find_first_of(adl::begin(r1), adl::end(r1), adl::begin(r2), adl::end(r2),
                            astl::pass_fn(pred), astl::pass_fn(p1), astl::pass_fn(p2));
}

template <typename R1, typename N1, typename R2, typename N2>
ASTL_NODISCARD auto find_first_of_n(R1 &&r1, N1 n1, R2 &&r2, N2 n2) -> iter_of_range<R1>
{
    return i::find_first_of_n(adl::begin(r1), n1, adl::begin(r2), n2);
}

template <typename R1, typename N1, typename R2, typename N2, typename BinaryPredicate>
ASTL_NODISCARD auto find_first_of_n(R1 &&r1, N1 n1, R2 &&r2, N2 n2, BinaryPredicate pred)
    -> iter_of_range<R1>
{
    return i::find_first_of_n(adl::begin(r1), n1, adl::begin(r2), n2, astl::pass_fn(pred));
}

template <typename R1, typename N1, typename R2, typename N2, typename BinaryPredicate, typename P1,
          typename P2 = P1>
ASTL_NODISCARD auto find_first_of_n(R1 &&r1, N1 n1, R2 &&r2, N2 n2, BinaryPredicate pred, P1 p1,
                                    P2 p2 = P1{}) -> iter_of_range<R1>
{
    return i::find_first_of_n(adl::begin(r1), n1, adl::begin(r2), n2, astl::pass_fn(pred),
                              astl::pass_fn(p1), astl::pass_fn(p2));
}

template <typename R, typename UnaryPredicate>
ASTL_NODISCARD auto find_if(R &&r, UnaryPredicate pred) -> iter_of_range<R>
{
    return i::find_if(adl::begin(r), adl::end(r), astl::pass_fn(pred));
}

template <typename R, typename UnaryPredicate, typename P>
ASTL_NODISCARD auto find_if(R &&r, UnaryPredicate pred, P p) -> iter_of_range<R>
{
    return i::find_if(adl::begin(r), adl::end(r), astl::pass_fn(pred), astl::pass_fn(p));
}

template <int N = 2, typename R, typename NaryPred, typename P>
ASTL_NODISCARD auto find_if_adjacent(R &&r, NaryPred pred, P p) ->
    typename std::enable_if<(N > 0), astl::iter_of_range<R>>::type
{
    return internal_find::find_if_adjacent1<N>(adl::begin(r), adl::end(r),
                                               astl::combine(astl::pass_fn(pred), astl::pass_fn(p)),
                                               astl::size_or_distance(r));
}

template <int N = 2, typename R, typename NaryPred>
ASTL_NODISCARD auto find_if_adjacent(R &&r, NaryPred pred) ->
    typename std::enable_if<(N > 0), astl::iter_of_range<R>>::type
{
    return internal_find::find_if_adjacent1<N>(adl::begin(r), adl::end(r), astl::pass_fn(pred),
                                               astl::size_or_distance(r));
}

template <typename R, typename UnaryPredicate, typename Distance>
ASTL_NODISCARD auto find_if_n(R &&r, Distance len, UnaryPredicate pred)
    -> std::pair<astl::iter_of_range<R>, Distance>
{
    return i::find_if_n(adl::begin(r), len, astl::pass_fn(pred));
}

template <typename R, typename UnaryPredicate, typename Distance, typename P>
ASTL_NODISCARD auto find_if_n(R &&r, Distance len, UnaryPredicate pred, P p)
    -> std::pair<astl::iter_of_range<R>, Distance>
{
    return i::find_if_n(adl::begin(r), len, astl::pass_fn(pred), astl::pass_fn(p));
}

template <typename R, typename UnaryPredicate>
ASTL_NODISCARD auto find_if_not(R &&r, UnaryPredicate pred) -> iter_of_range<R>
{
    return i::find_if_not(adl::begin(r), adl::end(r), astl::pass_fn(pred));
}

template <typename R, typename UnaryPredicate, typename P>
ASTL_NODISCARD auto find_if_not(R &&r, UnaryPredicate pred, P p) -> iter_of_range<R>
{
    return i::find_if_not(adl::begin(r), adl::end(r), astl::pass_fn(pred), astl::pass_fn(p));
}

template <typename R, typename UnaryPredicate, typename Distance>
ASTL_NODISCARD auto find_if_not_n(R &&r, Distance len, UnaryPredicate pred)
    -> std::pair<astl::iter_of_range<R>, Distance>
{
    return i::find_if_not_n(adl::begin(r), len, astl::pass_fn(pred));
}

template <typename R, typename UnaryPredicate, typename Distance, typename P>
ASTL_NODISCARD auto find_if_not_n(R &&r, Distance len, UnaryPredicate pred, P p)
    -> std::pair<astl::iter_of_range<R>, Distance>
{
    return i::find_if_not_n(adl::begin(r), len, astl::pass_fn(pred), astl::pass_fn(p));
}

template <typename R, typename UnaryPredicate>
ASTL_NODISCARD auto find_if_unique(R &&r, UnaryPredicate pred) -> iter_of_range<R>
{
    return i::find_if_unique(adl::begin(r), adl::end(r), astl::pass_fn(pred));
}

template <typename R, typename UnaryPredicate, typename P>
ASTL_NODISCARD auto find_if_unique(R &&r, UnaryPredicate pred, P p) -> iter_of_range<R>
{
    return i::find_if_unique(adl::begin(r), adl::end(r), astl::pass_fn(pred), astl::pass_fn(p));
}

template <typename R, typename N, typename UnaryPredicate>
ASTL_NODISCARD auto find_if_unique_n(R &&r, N n, UnaryPredicate pred)
    -> std::pair<astl::iter_of_range<R>, N>
{
    return i::find_if_unique_n(adl::begin(r), n, pred);
}

template <typename R, typename N, typename UnaryPredicate, typename P>
ASTL_NODISCARD auto find_if_unique_n(R &&r, N n, UnaryPredicate pred, P p)
    -> std::pair<astl::iter_of_range<R>, N>
{
    return i::find_if_unique_n(adl::begin(r), n, pred, astl::pass_fn(p));
}

template <typename R, typename Comparator, typename UnaryPredicate>
ASTL_NODISCARD auto find_if_while(R &&r, Comparator comp, UnaryPredicate pred) -> iter_of_range<R>
{
    return i::find_if_while(adl::begin(r), adl::end(r), astl::pass_fn(pred), astl::pass_fn(comp));
}

template <typename R, typename Comparator, typename UnaryPredicate, typename Pc, typename Pup = Pc>
ASTL_NODISCARD auto find_if_while(R &&r, Comparator comp, UnaryPredicate pred, Pc pc, Pup p = Pc{})
    -> iter_of_range<R>
{
    return i::find_if_while(adl::begin(r), adl::end(r), astl::pass_fn(pred), astl::pass_fn(comp),
                            astl::pass_fn(pc), astl::pass_fn(p));
}

template <typename R, typename UnaryPredicate>
ASTL_NODISCARD auto find_last_if(R &&r, UnaryPredicate pred) -> iter_of_range<R>
{
    return i::find_if_backward(adl::begin(r), adl::end(r), astl::pass_fn(pred));
}

template <typename R, typename UnaryPredicate, typename P>
ASTL_NODISCARD auto find_last_if(R &&r, UnaryPredicate pred, P p) -> iter_of_range<R>
{
    return i::find_if_backward(adl::begin(r), adl::end(r), astl::pass_fn(pred), astl::pass_fn(p));
}

template <typename R>
// requires R ForwardIterator range
ASTL_NODISCARD auto find_max_sum_of_subarray(R &&r)
    -> std::tuple<astl::iter_of_range<R>, astl::iter_of_range<R>,
                  optional<astl::range_value_type<R>>>
{
    return i::find_max_sum_of_subarray(adl::begin(r), adl::end(r));
}

template <typename R, typename BinaryOp, typename CompareF>
// requires R ForwardIterator range
// requires BinaryPredicate, returns ValueType(R), two arguments of ValueType(R)
// requires CompareF StrictWeakOrdering
ASTL_NODISCARD auto find_max_sum_of_subarray(R &&r, BinaryOp op, CompareF comp)
    -> std::tuple<astl::iter_of_range<R>, astl::iter_of_range<R>,
                  optional<astl::range_value_type<R>>>
{
    return i::find_max_sum_of_subarray(adl::begin(r), adl::end(r), astl::pass_fn(op),
                                       astl::pass_fn(comp));
}

template <typename R, typename BinaryOp, typename CompareF, typename P>
ASTL_NODISCARD auto find_max_sum_of_subarray(R &&r, BinaryOp op, CompareF comp, P p)
    -> std::tuple<astl::iter_of_range<R>, astl::iter_of_range<R>,
                  optional<astl::range_value_type<R>>>
{
    return i::find_max_sum_of_subarray(adl::begin(r), adl::end(r), astl::pass_fn(op),
                                       astl::pass_fn(comp), astl::pass_fn(p));
}

template <typename R, typename N, typename T, typename P>
ASTL_NODISCARD auto find_n(R &&r, N n, T const &e, P p) -> std::pair<astl::iter_of_range<R>, N>
{
    return i::find_n(adl::begin(r), n, e, astl::pass_fn(p));
}

template <typename R, typename T>
// requires R ForwardIterator range
// requires T, inequality comparable with value_type(R)
ASTL_NODISCARD auto find_not(R &&r, T &&val) -> iter_of_range<R>
{
    return i::find_not(adl::begin(r), adl::end(r), val);
}

template <typename R, typename T, typename P> auto find_not(R &&r, T &&val, P p) -> iter_of_range<R>
{
    return i::find_not(adl::begin(r), adl::end(r), val, astl::pass_fn(p));
}

template <typename R, typename N, typename T>
// requires R ForwardIterator range
// requires N integral type
// requires T, inequality comparable with value_type(R)
ASTL_NODISCARD auto find_not_n(R &&r, N n, T const &val) -> std::pair<astl::iter_of_range<R>, N>
{
    return i::find_not_n(adl::begin(r), n, val);
}

template <typename R, typename N, typename T, typename P>
ASTL_NODISCARD auto find_not_n(R &&r, N n, T const &val, P p)
    -> std::pair<astl::iter_of_range<R>, N>
{
    return i::find_not_n(adl::begin(r), n, val, astl::pass_fn(p));
}

template <typename R, typename BinaryPredicate>
// requires R BidirectionalIterator range
// requires  BinaryPredicate, returns bool, arguments two of value_type(R)
ASTL_NODISCARD auto find_peek(R &&r, BinaryPredicate pred) -> iter_of_range<R>
{
    return i::find_peek(adl::begin(r), adl::end(r), astl::pass_fn(pred));
}

template <typename R>
// requires R BidirectionalIterator range
ASTL_NODISCARD auto find_peek(R &&r) -> iter_of_range<R>
{
    return r::find_peek(r, std::less{});
}

template <typename R, typename BinaryPredicate, typename P>
ASTL_NODISCARD auto find_peek(R &&r, BinaryPredicate pred, P p) -> iter_of_range<R>
{
    return i::find_peek(adl::begin(r), adl::end(r), astl::pass_fn(pred), astl::pass_fn(p));
}

template <typename R, typename T>
// requires R ForwardIterator range
// requires T, inequality comparable with value_type(R)
ASTL_NODISCARD auto find_range(R &&r, T const &val)
    -> std::pair<astl::iter_of_range<R>, astl::iter_of_range<R>>
{
    return i::find_range(adl::begin(r), adl::end(r), val);
}

template <typename R, typename T, typename P>
ASTL_NODISCARD auto find_range(R &&r, T const &val, P p)
    -> std::pair<astl::iter_of_range<R>, astl::iter_of_range<R>>
{
    return i::find_range(adl::begin(r), adl::end(r), val, astl::pass_fn(p));
}

template <typename R, typename UnaryPredicate>
// requires R ForwardIterator range
// requires UnaryPredicate, return bool, argument value_type(R)
ASTL_NODISCARD auto find_range_if(R &&r, UnaryPredicate pred)
    -> std::pair<astl::iter_of_range<R>, astl::iter_of_range<R>>
{
    return i::find_range_if(adl::begin(r), adl::end(r), astl::pass_fn(pred));
}

template <typename R, typename UnaryPredicate, typename P>
ASTL_NODISCARD auto find_range_if(R &&r, UnaryPredicate pred, P p)
    -> std::pair<astl::iter_of_range<R>, astl::iter_of_range<R>>
{
    return i::find_range_if(adl::begin(r), adl::end(r), astl::pass_fn(pred), astl::pass_fn(p));
}

template <typename R, typename N, typename UnaryPredicate>
// requires R ForwardIterator range
// requires N integral type
// requires UnaryPredicate, return bool, argument value_type(R)
ASTL_NODISCARD auto find_range_if_n(R &&r, N n, UnaryPredicate pred)
    -> std::pair<astl::iter_of_range<R>, astl::iter_of_range<R>>
{
    return i::find_range_if_n(adl::begin(r), n, astl::pass_fn(pred));
}

template <typename R, typename N, typename UnaryPredicate, typename P>
ASTL_NODISCARD auto find_range_if_n(R &&r, N n, UnaryPredicate pred, P p)
    -> std::pair<astl::iter_of_range<R>, astl::iter_of_range<R>>
{
    return i::find_range_if_n(adl::begin(r), n, astl::pass_fn(pred), astl::pass_fn(p));
}

template <typename R, typename N, typename T>
// requires R ForwardIterator range
// requires N integral type
// requires T, inequality comparable with value_type(R)
ASTL_NODISCARD auto find_range_n(R &&r, N n, T const &val)
    -> std::pair<astl::iter_of_range<R>, astl::iter_of_range<R>>
{
    return i::find_range_n(adl::begin(r), n, val);
}

template <typename R, typename N, typename T, typename P>
ASTL_NODISCARD auto find_range_n(R &&r, N n, T const &val, P p)
    -> std::pair<astl::iter_of_range<R>, astl::iter_of_range<R>>
{
    return i::find_range_n(adl::begin(r), n, val, astl::pass_fn(p));
}

template <typename R, typename T>
ASTL_NODISCARD auto find_unique(R &&r, T &&value) -> iter_of_range<R>
{
    return i::find_unique(adl::begin(r), adl::end(r), value);
}

template <typename R, typename T, typename P>
ASTL_NODISCARD auto find_unique(R &&r, T &&value, P p) -> iter_of_range<R>
{
    return i::find_unique(adl::begin(r), adl::end(r), value, astl::pass_fn(p));
}

template <typename R, typename N, typename T>
ASTL_NODISCARD auto find_unique_n(R &&r, N n, T const &value)
    -> std::pair<astl::iter_of_range<R>, N>
{
    return i::find_unique_n(adl::begin(r), n, value);
}

template <typename R, typename N, typename T, typename P>
ASTL_NODISCARD auto find_unique_n(R &&r, N n, T const &value, P p)
    -> std::pair<astl::iter_of_range<R>, N>
{
    return i::find_unique_n(adl::begin(r), n, value, astl::pass_fn(p));
}

template <typename R, typename T, typename UnaryPredicate>
ASTL_NODISCARD auto find_while(R &&r, T &&value, UnaryPredicate pred) -> iter_of_range<R>
{
    return i::find_while(adl::begin(r), adl::end(r), value, astl::pass_fn(pred));
}

template <typename R, typename T, typename UnaryPredicate, typename Pc, typename Pup = Pc>
ASTL_NODISCARD auto find_while(R &&r, T &&value, UnaryPredicate pred, Pc pc, Pup p = Pc{})
    -> iter_of_range<R>
{
    return i::find_while(adl::begin(r), adl::end(r), value, astl::pass_fn(pred), astl::pass_fn(pc),
                         astl::pass_fn(p));
}
} // namespace r
} // namespace astl

#endif // ASTL_INCLUDE_FIND_HPP
