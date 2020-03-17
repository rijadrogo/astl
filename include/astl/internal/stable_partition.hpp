//
// Created by Rijad on 10-Sep-18.
//

#ifndef ASTL_INCLUDE_STABLE_PARTITION_HPP
#define ASTL_INCLUDE_STABLE_PARTITION_HPP

#include <algorithm>
#include <type_traits>
#include <utility>

#include "find.hpp"
#include "partition.hpp"
#include "reverse.hpp"
#include "rotate.hpp"
#include "sort.hpp"

#include "astl/functional.hpp"
#include "astl/iterator.hpp"
#include "astl/range_access.hpp"
#include "astl/temporary_buffer.hpp"

namespace astl
{

namespace internal_spartition
{
template <typename FwdIt, typename UnaryPredicate>
auto find_last_if(FwdIt first, FwdIt last, UnaryPredicate pred) -> FwdIt
{
    if constexpr (is_bidirectional_it_v<FwdIt>) { // Bidirectional Iterator
        auto i(i::find_if_backward(first, last, pred));
        if (i == first) return i;
        return ++i;
    }
    else
        return last;
}

template <typename FwdIt, typename UnaryPredicate>
// requires FwdIt ForwardIterator
// requires UnaryPredicate, returns bool, argument value_type(FwdIt)
auto partition_two_elements(FwdIt first, FwdIt last, UnaryPredicate pred) -> FwdIt
{
    // precondition: distance(first, last) == 2
    FwdIt prev_last(astl::next(first));
    bool const pred_prev_last(pred(prev_last));

    if (pred(first)) {
        if (pred_prev_last) return last;

        return prev_last;
    }
    if (pred_prev_last) {
        using std::swap;
        swap(*first, *prev_last);
        return prev_last;
    }
    return first;
}

} // namespace internal_spartition

namespace i
{
template <typename FwdIt, typename UnaryPredicate, typename B>
// requires FwdIt ForwardIterator
// requires UnaryPredicate, returns bool, argument value_type(FwdIt)
// requires B ForwardIterator
auto stable_partition_buffered(FwdIt first, FwdIt last, UnaryPredicate pred, B buffer) -> FwdIt
{
    // precondition: std::distance(first, last) < buffer.size()
    if (first == last) return first;

    B result_buffer(buffer);
    FwdIt result(first);

    while (first != last) {
        if (!pred(first)) {
            *result_buffer = std::move(*first);
            ++result_buffer;
        }
        else {
            *result = std::move(*first);
            ++result;
        }
        ++first;
    }
    std::move(buffer, result_buffer, result);
    return result;
}

template <typename FwdIt, typename UnaryPredicate, typename B, typename P>
auto stable_partition_buffered(FwdIt first, FwdIt last, UnaryPredicate pred, B buffer, P p) -> FwdIt
{
    // precondition: std::distance(first, last) < buffer.size()
    return i::stable_partition_buffered(
        first, last, astl::combine(astl::pass_fn(pred), astl::pass_fn(p)), buffer);
}

template <typename FwdIt, typename N, typename UnaryPredicate, typename B>
// requires FwdIt ForwardIterator
// requires N integral type
// requires UnaryPredicate, returns bool, argument value_type(FwdIt)
// requires B ForwardIterator
auto stable_partition_n_buffered(FwdIt first, N n, UnaryPredicate pred, B buffer)
    -> std::pair<FwdIt, FwdIt>
{
    // precondition: n <= buffer.size()
    B result_buffer(buffer);
    FwdIt result(first);
    while (n != N(0)) {
        if (!pred(first)) {
            *result_buffer = std::move(*first);
            ++result_buffer;
        }
        else {
            *result = std::move(*first);
            ++result;
        }
        ++first;
        --n;
    }
    std::move(buffer, result_buffer, result);
    return std::make_pair(result, first);
}

template <typename FwdIt, typename N, typename UnaryPredicate, typename B, typename P>
auto stable_partition_n_buffered(FwdIt first, N n, UnaryPredicate pred, B buffer, P p)
    -> std::pair<FwdIt, FwdIt>
{
    // precondition: n <= buffer.size()
    return i::stable_partition_n_buffered(
        first, n, astl::combine(astl::pass_fn(pred), astl::pass_fn(p)), buffer);
}

template <typename FwdIt, typename Size, typename UnaryPredicate, typename B, typename N>
// requires FwdIt ForwardIterator
// requires Size integral type
// requires UnaryPredicate, returns bool, argument value_type(FwdIt)
// requires B ForwardIterator
// requires N integral type
auto stable_partition_n_adaptive(FwdIt first, Size n, UnaryPredicate pred, B buffer, N buffer_size)
    -> FwdIt
{
    // precondition n > 0
    switch (integral_t<N>(n)) {
    case 0: return first;
    case 1: return pred(*first) ? first : astl::next(first);
    case 2: return internal_spartition::partition_two_elements(first, astl::next(first, 2), pred);
    default: break;
    }
    using Ct = typename std::common_type<N, Size>::type;
    if (Ct(n) <= Ct(buffer_size))
        return i::stable_partition_n_buffered(first, n, pred, buffer).first;

    Size lhs_size(n >> 1);
    Size rhs_size(n - lhs_size);
    FwdIt mid(astl::next(first, lhs_size));

    return i::rotate_adaptive(
        i::stable_partition_n_adaptive(first, lhs_size, pred, buffer, buffer_size), mid,
        i::stable_partition_n_adaptive(mid, rhs_size, pred, buffer, buffer_size), buffer,
        buffer_size);
}

template <typename FwdIt, typename Size, typename UnaryPredicate, typename B, typename N,
          typename P>
auto stable_partition_n_adaptive(FwdIt first, Size n, UnaryPredicate pred, B buffer, N buffer_size,
                                 P p) -> FwdIt
{
    return i::stable_partition_n_adaptive(
        first, n, astl::combine(astl::pass_fn(pred), astl::pass_fn(p)), buffer, buffer_size);
}

} // namespace i
namespace internal_spartition
{
template <typename FwdIt, typename UnaryPredicate, typename Distance>
// requires FwdIt ForwardIterator
// requires UnaryPredicate, argument value_type(FwdIt)
// requires Distance integral type
auto inplace_stable_partition_n(FwdIt first, Distance len, UnaryPredicate pred)
    -> std::pair<FwdIt, FwdIt>
{
    if (len == Distance(0)) return std::make_pair(first, first);

    if (len == Distance(1)) {
        FwdIt l(astl::next(first));
        if (pred(*first)) return std::make_pair(first, l);

        return std::make_pair(l, l);
    }
    Distance half = len / Distance(2);
    std::pair<FwdIt, FwdIt> i(internal_spartition::inplace_stable_partition_n(first, half, pred));
    std::pair<FwdIt, FwdIt> j(
        internal_spartition::inplace_stable_partition_n(i.second, len - half, pred));
    return std::make_pair(std::rotate(i.first, i.second, j.first), j.second);
}

template <typename InIt1, typename InIt2, typename InIt3, typename UnaryPredicate, typename Eqv>
// requires InIt1 InputIterator
// requires InIt2 InputIterator
// requires InIt3 InputIterator
// requires UnaryPredicate, return bool argument value_type(InIt1)
// requires Eqv, return are two element equivalent
auto mismatch_partitioned(InIt1 first1, InIt1 last1, InIt2 first2, InIt2 last2, InIt3 first3,
                          InIt3 last3, UnaryPredicate pred, Eqv eqv)
    -> std::tuple<InIt1, InIt2, InIt3>
{
    while (first1 != last1) {
        if (!pred(*first1)) {
            if (first2 == last2 || !eqv(*first1, *first2)) break;
            ++first2;
        }
        else {
            if (first3 == last3 || !eqv(*first1, *first3)) break;
            ++first3;
        }
        ++first1;
    }
    return std::tuple<InIt1, InIt2, InIt3>(first1, first2, first3);
}

template <typename InIt1, typename InIt2, typename InIt3, typename UnaryPredicate>
// requires InIt1 InputIterator
// requires InIt2 InputIterator
// requires InIt3 InputIterator
// requires UnaryPredicate, return bool argument value_type(InIt1)
auto mismatch_partitioned(InIt1 first1, InIt1 last1, InIt2 first2, InIt2 last2, InIt3 first3,
                          InIt3 last3, UnaryPredicate pred) -> std::tuple<InIt1, InIt2, InIt3>
{
    return internal_spartition::mismatch_partitioned(first1, last1, first2, last2, first3, last3,
                                                     astl::pass_fn(pred), std::equal_to{});
}
} // namespace internal_spartition

namespace i
{
template <typename FwdIt, typename UnaryPredicate, typename B>
// requires FwdIt ForwardIterator
// requires UnaryPredicate Unary Function, returns in argument value_type(FwdIt)
// requires B BidirectionalIterator
auto stable_partition_3way_buffered(FwdIt first, FwdIt last, UnaryPredicate pred, B b_first,
                                    B b_last) -> std::pair<FwdIt, FwdIt>
{
    // precondition std::distance(first, last) <= std::distance(b_first, b_last)
    if (first == last) return std::make_pair(last, last);

    auto iters(i::partition_3way_move(first, last, first, b_first,
                                      std::make_reverse_iterator(b_last), astl::pass_fn(pred)));

    FwdIt res1(std::get<0>(iters));
    FwdIt res2(std::move(b_first, std::get<1>(iters), res1));
    i::reverse_move(std::get<2>(iters).base(), b_last, res2);
    return std::make_pair(res1, res2);
}

template <typename FwdIt, typename UnaryPredicate, typename B, typename P>
auto stable_partition_3way_buffered(FwdIt first, FwdIt last, UnaryPredicate pred, B b_first,
                                    B b_last, P p) -> std::pair<FwdIt, FwdIt>
{
    // precondition std::distance(first, last) <= std::distance(b_first, b_last)
    return i::stable_partition_3way_buffered(
        first, last, astl::combine(astl::pass_fn(pred), astl::pass_fn(p)), b_first, b_last);
}

} // namespace i

namespace internal_spartition
{
template <typename FwdIt, typename B, typename UnaryPredicate, typename N>
auto stable_partition_3way_non_empty(FwdIt first, FwdIt last, UnaryPredicate pred, N len, B buffer,
                                     B buffer_end, N buffer_size) -> std::pair<FwdIt, FwdIt>
{
    if (len == 1) return std::make_pair(last, last);

    if (len <= buffer_size)
        return i::stable_partition_3way_buffered(first, last, pred, buffer, buffer_end);

    FwdIt mid(std::next(first, len / 2));
    auto left_split(internal_spartition::stable_partition_3way_non_empty(
        first, mid, pred, len / 2, buffer, buffer_end, buffer_size));

    // skip elements in right place
    N right_len(len - len / 2);
    FwdIt right1(mid);
    while (right1 != last && pred(*right1) == 0) {
        ++right1;
        --right_len;
    }

    if (right_len) {
        auto right_split(internal_spartition::stable_partition_3way_non_empty(
            right1, last, pred, right_len, buffer, buffer_end, buffer_size));
        FwdIt first_partition(std::rotate(left_split.first, mid, right_split.first));
        mid = std::find_if(first_partition, last, [=](auto x) { return pred(x) != 1; });
        FwdIt k(std::find_if(mid, last, [&](auto x) { return pred(x) != 2; }));
        if (k != last) {
            FwdIt second_partition(std::rotate(mid, k, right_split.second));
            return std::make_pair(first_partition, second_partition);
        }
        return std::make_pair(first_partition, mid);
    }
    FwdIt first_partition(std::rotate(left_split.first, mid, right1));
    FwdIt second_partition(std::lower_bound(first_partition, last, 2,
                                            [=](auto &&x, auto &&y) { return int(pred(x)) < y; }));
    return std::make_pair(first_partition, second_partition);
}

template <typename FwdIt, typename UnaryPredicate>
auto shrink_range_partion(FwdIt first, FwdIt last, UnaryPredicate pred)
    -> std::tuple<FwdIt, FwdIt, bool>
{
    first = std::find_if(first, last, [=](auto &&x) { return int(pred(x)) != 0; });
    if (first == last) return std::tuple<FwdIt, FwdIt, bool>{last, last, true};

    FwdIt it(last);
    if constexpr (is_bidirectional_it_v<FwdIt>) { // Bidrectional Iterator
        while (true) {
            --it;
            if (it == first) return std::tuple<FwdIt, FwdIt, bool>{first, last, true};

            if (int(pred(*it)) != 2)
                return std::tuple<FwdIt, FwdIt, bool>{first, astl::next(it), false};
        }
    }
    return std::tuple<FwdIt, FwdIt, bool>{first, it, false};
}

} // namespace internal_spartition

namespace i
{
// tests if the first range is a stable partition of the second
template <typename FwdIt1, typename FwdIt2, typename UnaryPredicate>
// requires FwdIt1 ForwardIterator
// requires FwdIt2 ForwardIterator
// requires UnaryPredicate  , return bool argument value_type(InIt1)
ASTL_NODISCARD auto is_stable_partitioning(FwdIt1 first1, FwdIt1 last1, FwdIt2 first2, FwdIt2 last2,
                                           UnaryPredicate pred) -> bool
{
    auto p(astl::pass_fn(pred));
    FwdIt1 m1(std::find_if(first1, last1, p));
    return std::find_if_not(m1, last1, p) == last1
        && internal_spartition::mismatch_partitioned(first2, last2, first1, m1, m1, last1, p)
        == std::tuple<FwdIt2, FwdIt1, FwdIt1>(last2, m1, last1);
}

// tests if the first range is a stable partition of the second
template <typename FwdIt1, typename FwdIt2, typename UnaryPredicate, typename P>
ASTL_NODISCARD auto is_stable_partitioning(FwdIt1 first1, FwdIt1 last1, FwdIt2 first2, FwdIt2 last2,
                                           UnaryPredicate pred, P p) -> bool
{
    return i::is_stable_partitioning(first1, last1, first2, last2,
                                     astl::combine(astl::pass_fn(pred), astl::pass_fn(p)));
}

template <typename FwdIt, typename UnaryPredicate>
// requires FwdIt ForwardIterator
// requires UnaryPredicate, returns bool, argument value_type(FwdIt),
auto stable_partition(FwdIt first, FwdIt last, UnaryPredicate pred) -> FwdIt
{
#if defined(__GNUC__) || defined(_LIBCPP_VERSION)
    return std::stable_partition(first, last, astl::pass_fn(pred));
#else  // #if defined(__GNUC__) || defined(_LIBCPP_VERSION)
    if constexpr (is_bidirectional_it_v<FwdIt>) { // Bidirectional Iterator
        return std::stable_partition(first, last, astl::pass_fn(pred));
    }
    else { // Forward Iterator
        if (first == last) return last;

        auto pr(astl::pass_fn(pred));
        first = std::find_if_not(first, last, pr);

        using T = iter_value_type<FwdIt>;
        inline_temporary_buffer<T> buffer(astl::distance(first, last), *first);
        return i::stable_partition_n_adaptive(first, buffer.requested_size(), pr, buffer.begin(),
                                              buffer.size());
    }
#endif //#if defined(__GNUC__) || defined(_LIBCPP_VERSION) #else
}

template <typename FwdIt, typename UnaryPredicate, typename P>
auto stable_partition(FwdIt first, FwdIt last, UnaryPredicate pred, P p) -> FwdIt
{
    return i::stable_partition(first, last, astl::combine(astl::pass_fn(pred), astl::pass_fn(p)));
}

template <typename FwdIt, typename B, typename UnaryPredicate, typename N>
auto stable_partition_3way_adaptive(FwdIt first, FwdIt last, UnaryPredicate pred, B buffer,
                                    B buffer_end, N buffer_size) -> std::pair<FwdIt, FwdIt>
{
    auto p(astl::pass_fn(pred));
    auto iters(internal_spartition::shrink_range_partion(first, last, p));
    if (std::get<2>(iters)) return std::make_pair(std::get<0>(iters), std::get<1>(iters));

    auto len(astl::distance(std::get<0>(iters), std::get<1>(iters)));
    using CommonT = typename std::common_type<decltype(len), N>::type;
    return internal_spartition::stable_partition_3way_non_empty(
        std::get<0>(iters), std::get<1>(iters), pred, CommonT(len), buffer, buffer_end,
        CommonT(buffer_size));
}

template <typename FwdIt, typename B, typename UnaryPredicate, typename N, typename P>
auto stable_partition_3way_adaptive(FwdIt first, FwdIt last, UnaryPredicate pred, B buffer,
                                    B buffer_end, N buffer_size, P p) -> std::pair<FwdIt, FwdIt>
{
    return i::stable_partition_3way_adaptive(first, last,
                                             astl::combine(astl::pass_fn(pred), astl::pass_fn(p)),
                                             buffer, buffer_end, buffer_size);
}

template <typename FwdIt, typename UnaryPredicate>
auto stable_partition_3way(FwdIt first, FwdIt last, UnaryPredicate pred) -> std::pair<FwdIt, FwdIt>
{
    auto p(astl::pass_fn(pred));
    auto iters(internal_spartition::shrink_range_partion(first, last, p));
    if (std::get<2>(iters)) return std::make_pair(std::get<0>(iters), std::get<1>(iters));

    auto len(astl::distance(std::get<0>(iters), std::get<1>(iters)));
    using T = iter_value_type<FwdIt>;
    temporary_buffer<T> buffer(len, *iters.first);
    using CommonT = typename std::common_type<decltype(len), decltype(buffer.size())>::type;
    return internal_spartition::stable_partition_3way_non_empty(
        std::get<0>(iters), std::get<1>(iters), p, CommonT(len), buffer.begin(), buffer.end(),
        CommonT(buffer.size()));
}

template <typename FwdIt, typename UnaryPredicate, typename P>
auto stable_partition_3way(FwdIt first, FwdIt last, UnaryPredicate pred, P p)
    -> std::pair<FwdIt, FwdIt>
{
    return i::stable_partition_3way(first, last,
                                    astl::combine(astl::pass_fn(pred), astl::pass_fn(p)));
}

template <typename FwdIt, typename UnaryPredicate, typename B, typename N>
// requires FwdIt ForwardIterator
// requires UnaryPredicate, returns bool, argument value_type(FwdIt)
// requires B ForwardIterator
// requires N integral type
auto stable_partition_adaptive(FwdIt first, FwdIt last, UnaryPredicate pred, B buffer,
                               N buffer_size) -> FwdIt
{
    auto pr(astl::pass_fn(pred));
    first = std::find_if_not(first, last, pr);
    last = internal_spartition::find_last_if(first, last, pr);
    return i::stable_partition_n_adaptive(first, astl::distance(first, last), pr, buffer,
                                          buffer_size);
}

template <typename FwdIt, typename UnaryPredicate, typename B, typename N, typename P>
auto stable_partition_adaptive(FwdIt first, FwdIt last, UnaryPredicate pred, B buffer,
                               N buffer_size, P p) -> FwdIt
{
    return i::stable_partition_adaptive(
        first, last, astl::combine(astl::pass_fn(pred), astl::pass_fn(p)), buffer, buffer_size);
}

// same as stable_partition, guarantees no allocations
// complexity  O(N log N) swaps.
template <typename FwdIt, typename UnaryPredicate>
// requires FwdIt ForwardIterator
// requires UnaryPredicate, argument value_type(FwdIt)
auto stable_partition_inplace(FwdIt first, FwdIt last, UnaryPredicate pred) -> FwdIt
{
    auto p(astl::pass_fn(pred));
    first = std::find_if_not(first, last, p);
    last = internal_spartition::find_last_if(first, last, p);
    return internal_spartition::inplace_stable_partition_n(first, astl::distance(first, last), p)
        .first;
}

template <typename FwdIt, typename UnaryPredicate, typename P>
auto stable_partition_inplace(FwdIt first, FwdIt last, UnaryPredicate pred, P p) -> FwdIt
{
    return i::stable_partition_inplace(first, last,
                                       astl::combine(astl::pass_fn(pred), astl::pass_fn(p)));
}

template <typename FwdIt, typename N, typename UnaryPredicate>
// requires FwdIt ForwardIterator
// requires N integral type
// requires UnaryPredicate, returns bool, argument value_type(FwdIt)
auto stable_partition_n(FwdIt first, N n, UnaryPredicate pred) -> FwdIt
{
    if (n == N(0)) return first;

    auto pr(astl::pass_fn(pred));
    std::pair<FwdIt, N> i(i::find_if_not_n(first, n, pr));

    using T = iter_value_type<FwdIt>;
    inline_temporary_buffer<T> buffer(i.second, *first);
    return i::stable_partition_n_adaptive(i.first, i.second, pr, buffer.begin(), buffer.size());
}

template <typename FwdIt, typename N, typename UnaryPredicate, typename P>
auto stable_partition_n(FwdIt first, N n, UnaryPredicate pred, P p) -> FwdIt
{
    return i::stable_partition_n(first, n, astl::combine(astl::pass_fn(pred), astl::pass_fn(p)));
}

template <typename FwdIt, typename UnaryPredicate>
// requires FwdIt ForwardIterator
// requires UnaryPredicate, return int, argument value_type(FwdIt)
auto stable_partition_n_way(FwdIt first, FwdIt last, UnaryPredicate pred) -> void
{
    if constexpr (is_random_access_it_v<FwdIt>) // Random Access Iterator
        i::stable_sort(first, last, std::less{}, astl::pass_fn(pred));
    else if constexpr (is_bidirectional_it_v<FwdIt>) // Bidirectional Iterator
        i::merge_sort(first, last, std::less{}, astl::pass_fn(pred));
    else // Forward Iterator
        i::binary_insertion_sort(first, last, std::less{}, astl::pass_fn(pred));
}

template <typename FwdIt, typename UnaryPredicate, typename P>
auto stable_partition_n_way(FwdIt first, FwdIt last, UnaryPredicate pred, P p) -> void
{
    i::stable_partition_n_way(first, last, astl::combine(astl::pass_fn(pred), astl::pass_fn(p)));
}

} // namespace i

namespace r
{
// tests if the first range is a partition of the second
template <typename R1, typename R2, typename UnaryPredicate>
// requires FwdIt1 ForwardIterator
// requires FwdIt2 ForwardIterator
// requires UnaryPredicate  , return bool argument value_type(FwdIt1)
ASTL_NODISCARD auto is_stable_partitioning(R1 &&r1, R2 &&r2, UnaryPredicate pred) -> bool
{
    return i::is_stable_partitioning(adl::begin(r1), adl::end(r1), adl::begin(r2), adl::end(r2),
                                     astl::pass_fn(pred));
}

// tests if the first range is a partition of the second
template <typename R1, typename R2, typename UnaryPredicate, typename P>
ASTL_NODISCARD auto is_stable_partitioning(R1 &&r1, R2 &&r2, UnaryPredicate pred, P p) -> bool
{
    return i::is_stable_partitioning(adl::begin(r1), adl::end(r1), adl::begin(r2), adl::end(r2),
                                     astl::pass_fn(pred), astl::pass_fn(p));
}

template <typename R, typename UnaryPredicate>
// requires R ForwardIterator range
// requires UnaryPredicate, returns bool, argument value_type(R)
auto stable_partition(R &&r, UnaryPredicate pred) -> iter_of_range<R>
{
    return i::stable_partition(adl::begin(r), adl::end(r), astl::pass_fn(pred));
}

template <typename R, typename UnaryPredicate, typename P>
auto stable_partition(R &&r, UnaryPredicate pred, P p) -> iter_of_range<R>
{
    return i::stable_partition(adl::begin(r), adl::end(r), astl::pass_fn(pred), astl::pass_fn(p));
}

template <typename R, typename UnaryPredicate>
// requires R ForwardIterator range
// requires UnaryPredicate, returns bool, argument value_type(R)
auto stable_partition_3way(R &&r, UnaryPredicate pred)
    -> std::pair<iter_of_range<R>, iter_of_range<R>>
{
    return i::stable_partition_3way(adl::begin(r), adl::end(r), astl::pass_fn(pred));
}

template <typename R, typename UnaryPredicate, typename P>
auto stable_partition_3way(R &&r, UnaryPredicate pred, P p)
    -> std::pair<iter_of_range<R>, iter_of_range<R>>
{
    return i::stable_partition_3way(adl::begin(r), adl::end(r), astl::pass_fn(pred),
                                    astl::pass_fn(p));
}

template <typename R, typename UnaryPredicate, typename B, typename N>
// requires R ForwardIterator range
// requires UnaryPredicate, returns bool, argument value_type(R)
// requires B ForwardIterator
// requires N integral type
auto stable_partition_3way_adaptive(R &&r, UnaryPredicate pred, B buffer, B buffer_end,
                                    N buffer_size) -> std::pair<iter_of_range<R>, iter_of_range<R>>
{
    return i::stable_partition_3way_adaptive(adl::begin(r), adl::end(r), astl::pass_fn(pred),
                                             buffer, buffer_end, buffer_size);
}

template <typename R, typename UnaryPredicate, typename B, typename N, typename P>
auto stable_partition_3way_adaptive(R &&r, UnaryPredicate pred, B buffer, B buffer_end,
                                    N buffer_size, P p)
    -> std::pair<iter_of_range<R>, iter_of_range<R>>
{
    return i::stable_partition_3way_adaptive(adl::begin(r), adl::end(r), astl::pass_fn(pred),
                                             buffer, buffer_end, buffer_size, astl::pass_fn(p));
}

template <typename R, typename UnaryPredicate, typename B>
// requires R ForwardIterator range
// requires UnaryPredicate Unary Function, returns in argument value_type(R)
// requires B BidirectionalIterator
auto stable_partition_3way_buffered(R &&r, UnaryPredicate pred, B b_first, B b_last)
    -> std::pair<astl::iter_of_range<R>, astl::iter_of_range<R>>
{
    // precondition std::distance(first, last) <= std::distance(b_first, b_last)
    return i::stable_partition_3way_buffered(adl::begin(r), adl::end(r), astl::pass_fn(pred),
                                             b_first, b_last);
}

template <typename R, typename UnaryPredicate, typename B, typename P>
auto stable_partition_3way_buffered(R &&r, UnaryPredicate pred, B b_first, B b_last, P p)
    -> std::pair<astl::iter_of_range<R>, astl::iter_of_range<R>>
{
    // precondition r.size() <= std::distance(b_first, b_last)
    return i::stable_partition_3way_buffered(adl::begin(r), adl::end(r), astl::pass_fn(pred),
                                             b_first, b_last, astl::pass_fn(p));
}

template <typename R, typename UnaryPredicate, typename B, typename N>
// requires R ForwardIterator range
// requires UnaryPredicate, returns bool, argument value_type(R)
// requires B ForwardIterator
// requires N integral type
auto stable_partition_adaptive(R &&r, UnaryPredicate pred, B buffer, N buffer_size)
    -> iter_of_range<R>
{
    return i::stable_partition_adaptive(adl::begin(r), adl::end(r), astl::pass_fn(pred), buffer,
                                        buffer_size);
}

template <typename R, typename UnaryPredicate, typename B, typename N, typename P>
auto stable_partition_adaptive(R &&r, UnaryPredicate pred, B buffer, N buffer_size, P p)
    -> iter_of_range<R>
{
    return i::stable_partition_adaptive(adl::begin(r), adl::end(r), astl::pass_fn(pred), buffer,
                                        buffer_size, astl::pass_fn(p));
}

template <typename R, typename UnaryPredicate, typename B>
// requires R ForwardIterator range
// requires UnaryPredicate, returns bool, argument value_type(R)
// requires B ForwardIterator
auto stable_partition_buffered(R &&r, UnaryPredicate pred, B buffer) -> iter_of_range<R>
{
    return i::stable_partition_buffered(adl::begin(r), adl::end(r), astl::pass_fn(pred), buffer);
}

template <typename R, typename UnaryPredicate, typename B, typename P>
auto stable_partition_buffered(R &&r, UnaryPredicate pred, B buffer, P p) -> iter_of_range<R>
{
    // precondition: r.size() < buffer.size()
    return i::stable_partition_buffered(adl::begin(r), adl::end(r), astl::pass_fn(pred), buffer,
                                        astl::pass_fn(p));
}

// same as stable_partition, guarantee that wont allocate
// complexity  O(N log N) swaps.
template <typename R, typename UnaryPredicate>
// requires R ForwardIterator range
// requires UnaryPredicate, argument value_type(R)
auto stable_partition_inplace(R &&r, UnaryPredicate pred) -> iter_of_range<R>
{
    // precondition: r.size() < buffer.size()
    return internal_spartition::inplace_stable_partition_n(adl::begin(r), astl::size_or_distance(r),
                                                           astl::pass_fn(pred))
        .first;
}

// same as stable_partition, guarantee no allocations
// complexity  O(N log N) swaps.
template <typename R, typename UnaryPredicate, typename P>
// requires R ForwardIterator range
// requires UnaryPredicate, argument value_type(R)
auto stable_partition_inplace(R &&r, UnaryPredicate pred, P p) -> iter_of_range<R>
{
    return r::stable_partition_inplace(r, astl::combine(astl::pass_fn(pred), astl::pass_fn(p)));
}

template <typename R, typename N, typename UnaryPredicate>
// requires R ForwardIterator range
// requires N integral type
// requires UnaryPredicate, returns bool, argument value_type(R)
auto stable_partition_n(R &&r, N n, UnaryPredicate pred) -> iter_of_range<R>
{
    return i::stable_partition_n(adl::begin(r), n, astl::pass_fn(pred));
}

template <typename R, typename N, typename UnaryPredicate, typename P>
auto stable_partition_n(R &&r, N n, UnaryPredicate pred, P p) -> iter_of_range<R>
{
    return i::stable_partition_n(adl::begin(r), n, astl::pass_fn(pred), astl::pass_fn(p));
}

template <typename R, typename N, typename UnaryPredicate, typename B>
// requires R ForwardIterator range
// requires N integral type
// requires UnaryPredicate, returns bool, argument value_type(R)
// requires B ForwardIterator
auto stable_partition_n_buffered(R &&r, N n, UnaryPredicate pred, B buffer)
    -> std::pair<astl::iter_of_range<R>, astl::iter_of_range<R>>
{
    // precondtion: n <= buffer.size()
    return i::stable_partition_n_buffered(adl::begin(r), n, astl::pass_fn(pred), buffer);
}

template <typename R, typename N, typename UnaryPredicate, typename B, typename P>
auto stable_partition_n_buffered(R &&r, N n, UnaryPredicate pred, B buffer, P p)
    -> std::pair<astl::iter_of_range<R>, astl::iter_of_range<R>>
{
    // precondtion: n <= buffer.size()
    return i::stable_partition_n_buffered(adl::begin(r), n, astl::pass_fn(pred), buffer,
                                          astl::pass_fn(p));
}

template <typename R, typename UnaryPredicate>
// requires R ForwardIterator range
// requires UnaryPredicate, returns bool, argument value_type(R)
auto stable_partition_n_way(R &&r, UnaryPredicate pred) -> void
{
    i::stable_partition_n_way(adl::begin(r), adl::end(r), astl::pass_fn(pred));
}

template <typename R, typename UnaryPredicate, typename P>
auto stable_partition_n_way(R &&r, UnaryPredicate pred, P p) -> void
{
    i::stable_partition_n_way(adl::begin(r), adl::end(r), astl::pass_fn(pred), astl::pass_fn(p));
}

} // namespace r
} // namespace astl

#endif // ASTL_INCLUDE_STABLE_PARTITION_HPP
