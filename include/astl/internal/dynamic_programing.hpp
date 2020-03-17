//
// Created by Rijad on 29-Jul-19.
//

#ifndef ASTL_INCLUDE_DYNAMIC_PROGRAMING_HPP
#define ASTL_INCLUDE_DYNAMIC_PROGRAMING_HPP

#include <cstddef>
#include <stdexcept>
#include <type_traits>
#include <unordered_map>
#include <unordered_set>
#include <utility>

#include "astl/functional.hpp"
#include "astl/iterator.hpp"
#include "astl/optional.hpp"
#include "astl/temporary_buffer.hpp"

// TODO test if memory allocation algorithms are faster then O(n^2) algorithms
namespace astl
{
namespace internal_ref
{

template <typename T> struct reference_wrapper: private std::reference_wrapper<T> {
private:
    using base = std::reference_wrapper<T>;

public:
    using base::base;
    using base::get;
    using base::operator();
    using base::operator T &;
};

template <typename T> auto ref(T &val) noexcept -> reference_wrapper<T>
{
    return reference_wrapper<T>(val);
}

template <typename T> void ref(const T &&) = delete;

template <typename T> auto ref(reference_wrapper<T> val) noexcept -> reference_wrapper<T>
{
    return internal_ref::ref(val.get());
}

} // namespace internal_ref
} // namespace astl

namespace std
{
template <typename T> struct hash<astl::internal_ref::reference_wrapper<T>> {
    auto operator()(const astl::internal_ref::reference_wrapper<T> &r) const
    {
        return hash<T>()(r.get());
    }
};
} // namespace std

namespace astl
{
namespace internal_dp
{
using internal_ref::ref;
using internal_ref::reference_wrapper;

template <typename T, typename = void> constexpr bool is_hashable_v = false;

template <typename T>
constexpr bool is_hashable_v<T, void_t<decltype(std::hash<T>{}(std::declval<T const &>()))>> = true;

// Time Complexity : O(m * n);
// Auxiliary Space : O(2n);
template <typename FwdIt1, typename N1, typename FwdIt2, typename N2, typename BinaryPredicate,
          typename B>
// requires FwdIt1 ForwardIterator
// requires N1 Integral
// requires FwdIt2 ForwardIterator
// requires N2 Integral
// requires BinaryPredicate, returns bool, arguments ValueType(FwdIt1) and
//                                              ValueType(FwdIt2)
auto lcs_unchecked(FwdIt1 first1, N1 m, FwdIt2 first2, N2 n, BinaryPredicate pred, B buffer)
    -> iter_diff_type<FwdIt1>
{
    iter_diff_type<FwdIt1> result(0);
    N1 i(1);
    char row(0);
    while (i < m) {
        FwdIt2 it(first2);
        N2 j(1);
        while (j < n) {
            if (pred(*first1, *it)) {
                buffer[row * n + j] = buffer[(1 - row) * n + (j - 1)] + 1;
                result = std::max(result, buffer[row * n + j]);
            }
            else {
                buffer[row * n + j] = 0;
            }
            ++it;
            ++j;
        }

        ++first1;
        row = 1 - row;
        ++i;
    }
    return result;
}

template <typename FwdIt1, typename N1, typename FwdIt2, typename N2, typename BinaryPredicate,
          typename B, typename N3>
// requires FwdIt1 ForwardIterator
// requires N1 Integral
// requires FwdIt2 ForwardIterator
// requires N2 Integral
// requires BinaryPredicate, returns bool, arguments ValueType(FwdIt1) and
//                                              ValueType(FwdIt2)
auto lcs_checked(FwdIt1 first1, N1 m, FwdIt2 first2, N2 n, BinaryPredicate pred, B buffer,
                 N3 buffer_size) -> optional<iter_diff_type<FwdIt1>>
{
    if (m < n) return internal_dp::lcs_checked(first2, n, first1, m, pred, buffer, buffer_size);

    if (2 * (n + 1) > buffer_size) return nullopt;

    return astl::make_optional(
        internal_dp::lcs_unchecked(first1, n + 1, first2, m + 1, pred, buffer));
}

// Time Complexity : O(m * n);
// Auxiliary Space : O(2n);
template <typename FwdIt1, typename N1, typename FwdIt2, typename N2, typename BinaryPredicate>
// requires FwdIt1 ForwardIterator
// requires N1 Integral
// requires FwdIt2 ForwardIterator
// requires N2 Integral
// requires BinaryPredicate, returns bool, arguments ValueType(FwdIt1) and
// ValueType(FwdIt2)
auto longest_common_subarray1(FwdIt1 first1, N1 m, FwdIt2 first2, N2 n, BinaryPredicate pred)
    -> optional<iter_diff_type<FwdIt1>>
{
    if (n == N1(0) || m == N2(0)) return astl::make_optional(iter_diff_type<FwdIt1>(0));

    ++m;
    ++n;

    inline_temporary_buffer<iter_diff_type<FwdIt1>> dp_buffer(2 * n, 0);
    if (dp_buffer.size() != dp_buffer.requested_size()) return nullopt;

    return astl::make_optional(internal_dp::lcs_unchecked(first1, m, first2, n, pred, dp_buffer));
}

template <typename FwdIt1, typename N1, typename FwdIt2, typename N2, typename BinaryPredicate>
// longest common subarray recursive
auto lcsr(FwdIt1 first1, N1 n1, FwdIt2 first2, N2 n2, BinaryPredicate pred)
    -> iter_diff_type<FwdIt1>
{
    if (n1 == N1(0) || n2 == N2(0)) return 0;

    if (pred(*first1, *first2))
        return 1 + internal_dp::lcsr(astl::next(first1), n1 - 1, astl::next(first2), n2 - 1, pred);

    return std::max(internal_dp::lcsr(first1, n1, astl::next(first2), n2 - 1, pred),
                    internal_dp::lcsr(astl::next(first1), n1 - 1, first2, n2, pred));
}

// Time Complexity : O(m * n);
// Auxiliary Space : O(n + 1);
// Given two arrays, find the length of longest subsequence present in both of them.
// Subsequence is a sequence that appears in the same relative order, but not necessarily contiguous.
template <typename FwdIt1, typename N1, typename FwdIt2, typename N2, typename BinaryPredicate>
// requires FwdIt1 ForwardIterator
// requires N1 Integral
// requires FwdIt2 ForwardIterator
// requires N2 Integral
// requires BinaryPredicate, returns bool, arguments value_type(FwdIt1) and
// value_type(FwdIt2)
auto longest_common_subseq1(FwdIt1 first1, N1 m, FwdIt2 first2, N2 n, BinaryPredicate pred)
    -> iter_diff_type<FwdIt1>
{
    if (m < n) return internal_dp::longest_common_subseq1(first2, n, first1, m, pred);

    if (n == N2(0) || m == N1(0)) return 0;

    ++m;
    ++n;

    inline_temporary_buffer<iter_diff_type<FwdIt1>> dp_buffer(n, 0);
    if (dp_buffer.size() != dp_buffer.requested_size())
        return internal_dp::lcsr(first1, m - 1, first2, n - 1, pred);

    auto dp(dp_buffer.begin());
    N1 i(1);
    while (i < m) {
        FwdIt2 it(first2);
        iter_diff_type<FwdIt1> prev(dp[0]);
        N2 j(1);
        while (j < n) {
            iter_diff_type<FwdIt1> const backup(dp[j]);
            if (pred(*first1, *it)) dp[j] = prev + 1;
            else
                dp[j] = std::max(dp[j], dp[j - 1]);

            prev = backup;
            ++it;
            ++j;
        }
        ++first1;
        ++i;
    }
    return dp[n - 1];
}

// Time Complexity O(n * n); n = r.size()
// Auxiliary Space O(n); n = r.size()
// The Longest Subsequence problem is to find the length of the longest subsequence of a given sequence
// such that all elements of the subsequence are sorted in order. Order is define with predicate function
template <typename FwdIt, typename Integral, typename BinaryPredicate>
// requires FwdIt ForwardIterator
// requires Integral Integral
// requires BinaryPredicate, returns bool, two arguments of ValueType(FwdIt)
auto longest_subseq1(FwdIt first, Integral n, BinaryPredicate pred) -> iter_diff_type<FwdIt>
{
    if (n == 0) return 0;

    inline_temporary_buffer<iter_diff_type<FwdIt>> dp_buffer(n, 0);
    if (dp_buffer.size() != dp_buffer.requested_size()) {
        // TODO implement
        throw std::runtime_error("Implement");
    }
    auto dp(dp_buffer.begin());
    FwdIt start(first);
    Integral i(1);
    while (i < n) {
        ++first;
        FwdIt it(start);
        Integral j(0);
        while (j < i) {
            if (pred(*it, *first) && dp[i] < dp[j] + 1) dp[i] = dp[j] + 1;

            ++it;
            ++j;
        }
        ++i;
    }
    return *std::max_element(dp, dp + n) + 1;
}

template <typename F, typename T> struct unwrap_ref {
    F _binary_pred;
    auto operator()(T x, T y) const -> decltype(_binary_pred(x.get(), y.get()))
    {
        return _binary_pred(x.get(), y.get());
    }
};

} // namespace internal_dp

namespace i
{

template <typename FwdIt, typename EqualityComparable>
// requires FwdIt ForwardIterator
// requires EqualityComparable, returns bool, two arguments of value_type(FwdIt)
ASTL_NODISCARD auto first_non_repeating_element(FwdIt first, FwdIt last, EqualityComparable pred)
    -> FwdIt
{
    if (first == last) return last;

    if constexpr (internal_dp::is_hashable_v<iter_value_type<FwdIt>>) { // is hashable
        auto p(astl::pass_fn(pred));
        using MapT = iter_diff_type<FwdIt>;
        using KeyT = internal_ref::reference_wrapper<iter_value_type<FwdIt>>;
        using PredT = internal_dp::unwrap_ref<decltype(p), KeyT>;
        std::unordered_map<KeyT, MapT, std::hash<KeyT>, PredT> map(20, std::hash<KeyT>{}, PredT{p});

        FwdIt start = first;
        while (first != last) {
            ++map[internal_ref::ref(*first)];
            ++first;
        }

        while (start != last) {
            if (map[*start] == MapT(1)) return start;
            ++start;
        }
        return last;
    }
    else {
        FwdIt save_first = first;
        while (first != last) {
            bool duplicate(false);
            FwdIt start = save_first;
            while (start != last) {
                if (start != first && pred(*first, *start)) {
                    duplicate = true;
                    break;
                }
                ++start;
            }
            if (!duplicate) return first;
            ++first;
        }
        return last;
    }
}

template <typename FwdIt>
// requires FwdIt ForwardIterator
// requires EqualityComparable, returns bool, two arguments of value_type(FwdIt)
ASTL_NODISCARD auto first_non_repeating_element(FwdIt first, FwdIt last) -> FwdIt
{
    return i::first_non_repeating_element(first, last, std::equal_to{});
}

template <typename FwdIt, typename EqualityComparable, typename P>
ASTL_NODISCARD auto first_non_repeating_element(FwdIt first, FwdIt last, EqualityComparable pred,
                                                P p) -> FwdIt
{
    return i::first_non_repeating_element(first, last, astl::pass_fn(astl::combine(pred, p)));
}

template <typename FwdIt, typename EqualityComparable>
// requires FwdIt ForwardIterator
// requires EqualityComparable, returns bool, two arguments of value_type(FwdIt)
ASTL_NODISCARD auto first_repeating_element(FwdIt first, FwdIt last, EqualityComparable pred)
    -> FwdIt
{

    if (first == last) return last;

    if constexpr (internal_dp::is_hashable_v<iter_value_type<FwdIt>>) { // is hashable

        auto p(astl::pass_fn(pred));
        using KeyT = internal_ref::reference_wrapper<iter_value_type<FwdIt>>;
        using PredT = internal_dp::unwrap_ref<decltype(p), KeyT>;
        std::unordered_set<KeyT, std::hash<KeyT>, PredT> set(20, std::hash<KeyT>{}, PredT{p});

        FwdIt start = first;
        while (first != last) {
            if (!set.insert(internal_ref::ref(*first)).second) return first;

            ++first;
        }

        return start;
    }
    else {
        FwdIt save_first = first;
        while (first != last) {
            bool duplicate(false);
            FwdIt start = save_first;
            while (start != last) {
                if (start != first && pred(*first, *start)) {
                    duplicate = true;
                    break;
                }
                ++start;
            }
            if (duplicate) return first;
            ++first;
        }
        return last;
    }
}

template <typename FwdIt>
// requires FwdIt ForwardIterator

ASTL_NODISCARD auto first_repeating_element(FwdIt first, FwdIt last) -> FwdIt
{
    return i::first_repeating_element(first, last, std::equal_to{});
}

template <typename FwdIt, typename EqualityComparable, typename P>
ASTL_NODISCARD auto first_repeating_element(FwdIt first, FwdIt last, EqualityComparable pred, P p)
    -> FwdIt
{
    return i::first_repeating_element(first, last, astl::pass_fn(astl::combine(pred, p)));
}

template <typename FwdIt1, typename FwdIt2, typename BinaryPredicate>
// requires FwdIt1 ForwardIterator
// requires FwdIt2 ForwardIterator
// requires BinaryPredicate, returns bool, arguments ValueType(FwdIt1) and
//                                              ValueType(FwdIt2)
ASTL_NODISCARD auto longest_common_subarray(FwdIt1 first1, FwdIt1 last1, FwdIt2 first2,
                                            FwdIt2 last2, BinaryPredicate pred)
    -> optional<iter_diff_type<FwdIt1>>
{
    return internal_dp::longest_common_subarray1(first1, astl::distance(first1, last1), first2,
                                                 astl::distance(first2, last2),
                                                 astl::pass_fn(pred));
}

template <typename FwdIt1, typename FwdIt2>
// requires FwdIt1 ForwardIterator
// requires FwdIt2 ForwardIterator
ASTL_NODISCARD auto longest_common_subarray(FwdIt1 first1, FwdIt1 last1, FwdIt2 first2,
                                            FwdIt2 last2) -> optional<iter_diff_type<FwdIt1>>
{
    return internal_dp::longest_common_subarray1(first1, astl::distance(first1, last1), first2,
                                                 astl::distance(first2, last2), std::equal_to{});
}

template <typename FwdIt1, typename FwdIt2, typename BinaryPredicate, typename P1, typename P2>
ASTL_NODISCARD auto longest_common_subarray(FwdIt1 first1, FwdIt1 last1, FwdIt2 first2,
                                            FwdIt2 last2, BinaryPredicate pred, P1 p1, P2 p2)
    -> optional<iter_diff_type<FwdIt1>>
{
    return internal_dp::longest_common_subarray1(
        first1, astl::distance(first1, last1), first2, astl::distance(first2, last2),
        astl::lockstep(astl::pass_fn(pred), astl::pass_fn(p1), astl::pass_fn(p2)));
}

template <typename FwdIt1, typename FwdIt2, typename BinaryPredicate, typename B, typename N>
// requires FwdIt1 ForwardIterator
// requires FwdIt2 ForwardIterator
// requires BinaryPredicate, returns bool, arguments
// ValueType(FwdIt1),ValueType(FwdIt2) requires B RandomAccessIterator requires
// N integral
ASTL_NODISCARD auto longest_common_subarray_buffered(FwdIt1 first1, FwdIt1 last1, FwdIt2 first2,
                                                     FwdIt2 last2, BinaryPredicate pred, B buffer,
                                                     N buffer_size)
    -> optional<iter_diff_type<FwdIt1>>
{
    return internal_dp::lcs_checked(first1, astl::distance(first1, last1), first2,
                                    astl::distance(first2, last2), astl::pass_fn(pred), buffer,
                                    buffer_size);
}

template <typename FwdIt1, typename FwdIt2, typename B, typename N>
// requires FwdIt1 ForwardIterator
// requires FwdIt2 ForwardIterator
// requires B RandomAccessIterator
// requires N integral
ASTL_NODISCARD auto longest_common_subarray_buffered(FwdIt1 first1, FwdIt1 last1, FwdIt2 first2,
                                                     FwdIt2 last2, B buffer, N buffer_size)
    -> optional<iter_diff_type<FwdIt1>>
{
    return internal_dp::lcs_checked(first1, astl::distance(first1, last1), first2,
                                    astl::distance(first2, last2), std::equal_to{}, buffer,
                                    buffer_size);
}

template <typename FwdIt1, typename FwdIt2, typename BinaryPredicate, typename B, typename N,
          typename P1, typename P2>
ASTL_NODISCARD auto longest_common_subarray_buffered(FwdIt1 first1, FwdIt1 last1, FwdIt2 first2,
                                                     FwdIt2 last2, BinaryPredicate pred, B buffer,
                                                     N buffer_size, P1 p1, P2 p2)
    -> optional<iter_diff_type<FwdIt1>>
{
    return internal_dp::lcs_checked(
        first1, astl::distance(first1, last1), first2, astl::distance(first2, last2),
        astl::lockstep(astl::pass_fn(pred), astl::pass_fn(p1), astl::pass_fn(p2)), buffer,
        buffer_size);
}

// Subsequence is a sequence that appears in the same relative order, but not
// necessarily contiguous.
template <typename FwdIt1, typename FwdIt2, typename BinaryPredicate>
// requires FwdIt1 ForwardIterator
// requires FwdIt2 ForwardIterator
// requires BinaryPredicate, returns bool, arguments ValueType(FwdIt1) and
// ValueType(FwdIt2)
ASTL_NODISCARD auto longest_common_subseq(FwdIt1 first1, FwdIt1 last1, FwdIt2 first2, FwdIt2 last2,
                                          BinaryPredicate pred) -> iter_diff_type<FwdIt1>
{
    return internal_dp::longest_common_subseq1(first1, astl::distance(first1, last1), first2,
                                               astl::distance(first2, last2), astl::pass_fn(pred));
}

// Subsequence is a sequence that appears in the same relative order, but not
// necessarily contiguous.
template <typename FwdIt1, typename FwdIt2>
// requires FwdIt1 ForwardIterator
// requires FwdIt2 ForwardIterator
ASTL_NODISCARD auto longest_common_subseq(FwdIt1 first1, FwdIt1 last1, FwdIt2 first2, FwdIt2 last2)
    -> iter_diff_type<FwdIt1>
{
    return internal_dp::longest_common_subseq1(first1, astl::distance(first1, last1), first2,
                                               astl::distance(first2, last2), std::equal_to{});
}

// Subsequence is a sequence that appears in the same relative order, but not
// necessarily contiguous.
template <typename FwdIt1, typename FwdIt2, typename BinaryPredicate, typename P1, typename P2>
ASTL_NODISCARD auto longest_common_subseq(FwdIt1 first1, FwdIt1 last1, FwdIt2 first2, FwdIt2 last2,
                                          BinaryPredicate pred, P1 p1, P2 p2)
    -> iter_diff_type<FwdIt1>
{
    return internal_dp::longest_common_subseq1(
        first1, astl::distance(first1, last1), first2, astl::distance(first2, last2),
        astl::lockstep(astl::pass_fn(pred), astl::pass_fn(p1), astl::pass_fn(p2)));
}

// Time Complexity: O(n)
template <typename BidiIt, typename BinaryPredicate>
// requires BidiIt BidirectionalIterator
// requires BinaryPredicate, returns bool, two arguments of ValueType(BidiIt)
ASTL_NODISCARD auto longest_subarray(BidiIt first, BidiIt last, BinaryPredicate pred)
    -> std::pair<BidiIt, BidiIt>
{
    iter_diff_type<BidiIt> max(0);
    BidiIt end(last);
    if (first != last) {
        iter_diff_type<BidiIt> len(1);
        BidiIt trailer(first);
        ++first;
        ++max;
        while (first != last) {
            if (pred(*first, *trailer)) { ++len; }
            else {
                if (max < len) {
                    max = len;
                    end = first;
                }
                len = 1;
            }
            ++first;
            ++trailer;
        }
        if (max < len) { max = len; }
    }
    return std::make_pair((max == 1 ? last : astl::prev(end, max)), end);
}

template <typename BidiIt>
// requires BidiIt BidirectionalIterator
ASTL_NODISCARD auto longest_subarray(BidiIt first, BidiIt last) -> std::pair<BidiIt, BidiIt>
{
    return i::longest_subarray(first, last, std::equal_to{});
}

template <typename BidiIt, typename BinaryPredicate, typename P>
ASTL_NODISCARD auto longest_subarray(BidiIt first, BidiIt last, BinaryPredicate pred, P p)
    -> std::pair<BidiIt, BidiIt>
{
    return i::longest_subarray(first, last, astl::combine(astl::pass_fn(pred), astl::pass_fn(p)));
}

template <typename FwdIt, typename BinaryPredicate>
// requires FwdIt ForwardIterator
// requires BinaryPredicate, returns bool, two arguments of ValueType(FwdIt)
ASTL_NODISCARD auto longest_subseq(FwdIt first, FwdIt last, BinaryPredicate pred)
    -> iter_diff_type<FwdIt>
{
    return internal_dp::longest_subseq1(first, astl::distance(first, last), astl::pass_fn(pred));
}

template <typename FwdIt>
// requires FwdIt ForwardIterator
ASTL_NODISCARD auto longest_subseq(FwdIt first, FwdIt last) -> iter_diff_type<FwdIt>
{
    return i::longest_subseq(first, last, std::equal_to{});
}

template <typename FwdIt, typename BinaryPredicate, typename P>
ASTL_NODISCARD auto longest_subseq(FwdIt first, FwdIt last, BinaryPredicate pred, P p)
    -> iter_diff_type<FwdIt>
{
    return internal_dp::longest_subseq1(astl::map_iterator(first, astl::pass_fn(p)),
                                        astl::distance(first, last), astl::pass_fn(pred));
}

template <typename FwdIt, typename BinaryPredicate>
// requires FwdIt ForwardIterator
// requires BinaryPredicate, returns bool, two arguments of value_type(FwdIt)
ASTL_NODISCARD auto most_frequent_element(FwdIt first, FwdIt last, BinaryPredicate pred)
    -> optional<astl::iter_value_type<FwdIt>>
{
    if (first == last) return nullopt;

    if constexpr (internal_dp::is_hashable_v<iter_value_type<FwdIt>>) { // is hashable
        auto p(astl::pass_fn(pred));
        using MapT = iter_diff_type<FwdIt>;
        using KeyT = internal_ref::reference_wrapper<iter_value_type<FwdIt>>;
        using PredT = internal_dp::unwrap_ref<decltype(p), KeyT>;
        std::unordered_map<KeyT, MapT, std::hash<KeyT>, PredT> map(20, std::hash<KeyT>{}, PredT{p});

        MapT most_frequent(1);
        FwdIt max_so_far(first);

        while (first != last) {
            MapT occurances(++map[internal_ref::ref(*first)]);
            if (occurances > most_frequent) {
                most_frequent = occurances;
                max_so_far = first;
            }
            ++first;
        }
        return astl::make_optional(*max_so_far);
    }
    else { // not hashable
        iter_diff_type<FwdIt> max(0);
        FwdIt max_so_far(first);
        auto p(astl::pass_fn(pred));
        while (first != last) {
            auto occurances(
                std::count_if(first, last, [p, first](auto &&x) { return p(x, *first); }));

            if (occurances > max) {
                max = occurances;
                max_so_far = first;
            }
            ++first;
        }
        return astl::make_optional(*max_so_far);
    }
}

template <typename FwdIt>
// requires FwdIt ForwardIterator
ASTL_NODISCARD auto most_frequent_element(FwdIt first, FwdIt last)
    -> optional<astl::iter_value_type<FwdIt>>
{
    return i::most_frequent_element(first, last, std::equal_to{});
}

template <typename FwdIt, typename BinaryPredicate, typename P>
ASTL_NODISCARD auto most_frequent_element(FwdIt first, FwdIt last, BinaryPredicate pred, P p)
    -> optional<astl::iter_value_type<FwdIt>>
{
    return i::most_frequent_element(first, last,
                                    astl::combine(astl::pass_fn(pred), astl::pass_fn(p)));
}

template <typename FwdIt, typename BinaryPredicate>
// requires FwdIt ForwardIterator
// requires BinaryPredicate, returns bool, two arguments of value_type(FwdIt)
ASTL_NODISCARD auto nth_most_frequent_element(FwdIt first, FwdIt last, iter_diff_type<FwdIt> n,
                                              BinaryPredicate pred)
    -> optional<astl::iter_value_type<FwdIt>>
{
    if (first == last) return nullopt;

    if (n == 1) return i::most_frequent_element(first, last, astl::pass_fn(pred));

    if constexpr (internal_dp::is_hashable_v<iter_value_type<FwdIt>>) { // hashable
        auto p(astl::pass_fn(pred));
        using MapT = iter_diff_type<FwdIt>;
        using KeyT = internal_ref::reference_wrapper<iter_value_type<FwdIt>>;
        using PredT = internal_dp::unwrap_ref<decltype(p), KeyT>;
        std::unordered_map<KeyT, MapT, std::hash<KeyT>, PredT> map(20, std::hash<KeyT>{}, PredT{p});

        while (first != last) {
            ++map[internal_ref::ref(*first)];
            ++first;
        }

        if (n < 0 || n > static_cast<iter_diff_type<FwdIt>>(map.size())) return nullopt;

        while (true) {
            auto const it(std::max_element(map.begin(), map.end(), less_second{}));

            if (--n == 0) return astl::make_optional(it->first.get());

            it->second = 0;
        }
    }
    else { // not hashable
        // TODO implement
        static_assert(std::is_array<FwdIt>::value, "Implement");
        return nullopt;
    }
}

template <typename FwdIt>
// requires FwdIt ForwardIterator
ASTL_NODISCARD auto nth_most_frequent_element(FwdIt first, FwdIt last, iter_diff_type<FwdIt> n = 1)
    -> optional<astl::iter_value_type<FwdIt>>
{
    return i::nth_most_frequent_element(first, last, n, std::equal_to{});
}

template <typename FwdIt, typename BinaryPredicate, typename P>
ASTL_NODISCARD auto nth_most_frequent_element(FwdIt first, FwdIt last, iter_diff_type<FwdIt> n,
                                              BinaryPredicate pred, P p)
    -> optional<astl::iter_value_type<FwdIt>>
{
    return i::nth_most_frequent_element(first, last, n,
                                        astl::combine(astl::pass_fn(pred), astl::pass_fn(p)));
}

template <typename FwdIt, typename BinaryPredicate>
// requires FwdIt ForwardIterator
// requires BinaryPredicate, returns bool, two arguments of value_type(FwdIt)
ASTL_NODISCARD auto num_of_unique_elements(FwdIt first, FwdIt last, BinaryPredicate pred)
    -> iter_diff_type<FwdIt>
{
    if (first == last) return 0;

    if constexpr (internal_dp::is_hashable_v<iter_value_type<FwdIt>>) { // hashable
        using T = internal_ref::reference_wrapper<iter_value_type<FwdIt>>;
        auto p(astl::pass_fn(pred));
        using PredT = internal_dp::unwrap_ref<decltype(p), T>;
        std::unordered_set<T, std::hash<T>, PredT> set(20, std::hash<T>{}, PredT{p});

        while (first != last) {
            set.insert(internal_dp::ref(*first));
            ++first;
        }
        return set.size();
    }
    else { // not hashable
        iter_diff_type<FwdIt> res(1);
        auto next(astl::next(first));
        while (next != last) {
            auto it(first);
            while (it != next) {
                if (pred(*it, *next)) break;

                ++it;
            }
            if (next == it) ++res;

            ++next;
        }
        return res;
    }
}

template <typename FwdIt>
// requires FwdIt ForwardIterator
ASTL_NODISCARD auto num_of_unique_elements(FwdIt first, FwdIt last) -> iter_diff_type<FwdIt>
{
    return i::num_of_unique_elements(first, last, std::equal_to{});
}

template <typename FwdIt, typename BinaryPredicate, typename P>
ASTL_NODISCARD auto num_of_unique_elements(FwdIt first, FwdIt last, BinaryPredicate pred, P p)
    -> iter_diff_type<FwdIt>
{
    return i::num_of_unique_elements(first, last,
                                     astl::combine(astl::pass_fn(pred), astl::pass_fn(p)));
}

} // namespace i

namespace r
{

template <typename R, typename EqualityComparable>
// requires FwdIt ForwardIterator
// requires EqualityComparable, returns bool, two arguments of value_type(FwdIt)
ASTL_NODISCARD auto first_non_repeating_element(R r, EqualityComparable pred) -> iter_of_range<R>
{
    return i::first_non_repeating_element(adl::begin(r), adl::end(r), astl::pass_fn(pred));
}

template <typename R, typename EqualityComparable, typename P>
ASTL_NODISCARD auto first_non_repeating_element(R r, EqualityComparable pred, P p)
    -> iter_of_range<R>
{
    return i::first_non_repeating_element(adl::begin(r), adl::end(r), astl::pass_fn(pred),
                                          astl::pass_fn(p));
}

template <typename R, typename EqualityComparable>
// requires FwdIt ForwardIterator
// requires EqualityComparable, returns bool, two arguments of value_type(FwdIt)
ASTL_NODISCARD auto first_repeating_element(R r, EqualityComparable pred) -> iter_of_range<R>
{
    return i::first_repeating_element(adl::begin(r), adl::end(r), astl::pass_fn(pred));
}

template <typename R, typename EqualityComparable, typename P>
ASTL_NODISCARD auto first_repeating_element(R r, EqualityComparable pred, P p) -> iter_of_range<R>
{
    return i::first_repeating_element(adl::begin(r), adl::end(r), astl::pass_fn(pred),
                                      astl::pass_fn(p));
}

template <typename R1, typename R2, typename BinaryPredicate>
// requires R1 ForwardIterator range
// requires R2 ForwardIterator range
// requires BinaryPredicate, returns bool, arguments ValueType(R1) and ValueType(R2)
ASTL_NODISCARD auto longest_common_subarray(R1 &&r1, R2 &&r2, BinaryPredicate pred)
    -> range_diff_type<R1>
{
    return internal_dp::longest_common_subarray1(adl::begin(r1), astl::size_or_distance(r1),
                                                 adl::begin(r2), astl::size_or_distance(r2),
                                                 astl::pass_fn(pred));
}

template <typename R1, typename R2>
// requires R1 ForwardIterator range
// requires R2 ForwardIterator range
ASTL_NODISCARD auto longest_common_subarray(R1 &&r1, R2 &&r2) -> range_diff_type<R1>
{
    return internal_dp::longest_common_subarray1(adl::begin(r1), astl::size_or_distance(r1),
                                                 adl::begin(r2), astl::size_or_distance(r2),
                                                 std::equal_to{});
}

template <typename R1, typename R2, typename BinaryPredicate, typename P1, typename P2>
ASTL_NODISCARD auto longest_common_subarray(R1 &&r1, R2 &&r2, BinaryPredicate pred, P1 p1, P2 p2)
    -> range_diff_type<R1>
{
    return internal_dp::longest_common_subarray1(
        adl::begin(r1), astl::size_or_distance(r1), adl::begin(r2), astl::size_or_distance(r2),
        astl::lockstep(astl::pass_fn(pred), astl::pass_fn(p1), astl::pass_fn(p2)));
}

template <typename R1, typename R2, typename BinaryPredicate, typename B, typename N>
// requires R1 ForwardIterator range
// requires R2 ForwardIterator range
// requires BinaryPredicate, returns bool, arguments ValueType(R1),ValueType(R2)
// requires B RandomAccessIterator
// requires N integral
ASTL_NODISCARD auto longest_common_subarray_buffered(R1 &&r1, R2 &&r2, BinaryPredicate pred,
                                                     B buffer, N buffer_size)
    -> optional<range_diff_type<R1>>
{
    return internal_dp::lcs_checked(adl::begin(r1), astl::size_or_distance(r1), adl::begin(r2),
                                    astl::size_or_distance(r2), astl::pass_fn(pred), buffer,
                                    buffer_size);
}

template <typename R1, typename R2, typename B, typename N>
// requires R1 ForwardIterator range
// requires R2 ForwardIterator range
// requires B RandomAccessIterator
// requires N integral
ASTL_NODISCARD auto longest_common_subarray_buffered(R1 &&r1, R2 &&r2, B buffer, N buffer_size)
    -> optional<range_diff_type<R1>>
{
    return internal_dp::lcs_checked(adl::begin(r1), astl::size_or_distance(r1), adl::begin(r2),
                                    astl::size_or_distance(r2), std::equal_to{}, buffer,
                                    buffer_size);
}

template <typename R1, typename R2, typename BinaryPredicate, typename B, typename N, typename P1,
          typename P2>
ASTL_NODISCARD auto longest_common_subarray_buffered(R1 &&r1, R2 &&r2, BinaryPredicate pred,
                                                     B buffer, N buffer_size, P1 p1, P2 p2)
    -> optional<range_diff_type<R1>>
{
    return internal_dp::lcs_checked(
        adl::begin(r1), astl::size_or_distance(r1), adl::begin(r2), astl::size_or_distance(r2),
        astl::lockstep(astl::pass_fn(pred), astl::pass_fn(p1), astl::pass_fn(p2)), buffer,
        buffer_size);
}

// Subsequence is a sequence that appears in the same relative order, but not
// necessarily contiguous.
template <typename R1, typename R2, typename BinaryPredicate>
// requires R1 ForwardIterator range
// requires R2 ForwardIterator range
// requires BinaryPredicate, returns bool, arguments ValueType(R1) and ValueType(R2)
ASTL_NODISCARD auto longest_common_subseq(R1 &&r1, R2 &&r2, BinaryPredicate pred)
    -> range_diff_type<R1>
{
    return internal_dp::longest_common_subseq1(adl::begin(r1), astl::size_or_distance(r1),
                                               adl::begin(r2), astl::size_or_distance(r2),
                                               astl::pass_fn(pred));
}

// Subsequence is a sequence that appears in the same relative order, but not
// necessarily contiguous.
template <typename R1, typename R2>
// requires R1 ForwardIterator range
// requires R2 ForwardIterator range
ASTL_NODISCARD auto longest_common_subseq(R1 &&r1, R2 &&r2) -> range_diff_type<R1>
{
    return internal_dp::longest_common_subseq1(adl::begin(r1), astl::size_or_distance(r1),
                                               adl::begin(r2), astl::size_or_distance(r2),
                                               std::equal_to{});
}

// Subsequence is a sequence that appears in the same relative order, but not
// necessarily contiguous.
template <typename R1, typename R2, typename BinaryPredicate, typename P1, typename P2>
ASTL_NODISCARD auto longest_common_subseq(R1 &&r1, R2 &&r2, BinaryPredicate pred, P1 p1, P2 p2)
    -> range_diff_type<R1>
{
    return internal_dp::longest_common_subseq1(
        adl::begin(r1), astl::size_or_distance(r1), adl::begin(r2), astl::size_or_distance(r2),
        astl::lockstep(astl::pass_fn(pred), astl::pass_fn(p1), astl::pass_fn(p2)));
}

template <typename R1, typename BinaryPredicate>
// requires R1 BidirectionalIterator range
ASTL_NODISCARD auto longest_subarray(R1 &&r1, BinaryPredicate pred)
    -> std::pair<astl::iter_of_range<R1>, astl::iter_of_range<R1>>
{
    return i::longest_subarray(adl::begin(r1), adl::end(r1), astl::pass_fn(pred));
}

template <typename R>
// requires R BidirectionalIterator range
// requires BinaryPredicate, returns bool, arguments ValueType(R) and ValueType(R)
ASTL_NODISCARD auto longest_subarray(R &&r)
    -> std::pair<astl::iter_of_range<R>, astl::iter_of_range<R>>
{
    return i::longest_subarray(adl::begin(r), adl::end(r));
}

template <typename R, typename BinaryPredicate, typename P>
ASTL_NODISCARD auto longest_subarray(R &&r, BinaryPredicate pred, P p)
    -> std::pair<astl::iter_of_range<R>, astl::iter_of_range<R>>
{
    return i::longest_subarray(adl::begin(r), adl::end(r), astl::pass_fn(pred), astl::pass_fn(p));
}

template <typename R, typename BinaryPredicate>
// requires R ForwardIterator range
// requires BinaryPredicate, returns bool, two arguments of ValueType(R)
ASTL_NODISCARD auto longest_subseq(R &&r, BinaryPredicate pred) -> range_diff_type<R>
{
    return internal_dp::longest_subseq1(adl::begin(r), astl::size_or_distance(r),
                                        astl::pass_fn(pred));
}

template <typename R>
// requires R ForwardIterator range
ASTL_NODISCARD auto longest_subseq(R &&r) -> range_diff_type<R>
{
    return internal_dp::longest_subseq1(adl::begin(r), astl::size_or_distance(r), std::equal_to{});
}

template <typename R, typename BinaryPredicate, typename P>
ASTL_NODISCARD auto longest_subseq(R &&r, BinaryPredicate pred, P p) -> range_diff_type<R>
{
    return internal_dp::longest_subseq1(astl::map_iterator(adl::begin(r), astl::pass_fn(p)),
                                        astl::size_or_distance(r), astl::pass_fn(pred));
}

template <typename R>
// requires R ForwardIterator range
ASTL_NODISCARD auto most_frequent_element(R &&r) -> optional<astl::range_value_type<R>>
{
    return i::most_frequent_element(adl::begin(r), adl::end(r));
}

template <typename R, typename BinaryPredicate>
// requires R ForwardIterator range
// requires BinaryPredicate, returns bool, two arguments of value_type(FwdIt)
ASTL_NODISCARD auto most_frequent_element(R &&r, BinaryPredicate pred)
    -> optional<astl::range_value_type<R>>
{
    return i::most_frequent_element(adl::begin(r), adl::end(r), astl::pass_fn(pred));
}

template <typename R, typename BinaryPredicate, typename P>
ASTL_NODISCARD auto most_frequent_element(R &&r, BinaryPredicate pred, P p)
    -> optional<astl::range_value_type<R>>
{
    return i::most_frequent_element(adl::begin(r), adl::end(r), astl::pass_fn(pred),
                                    astl::pass_fn(p));
}

template <typename R>
// requires R ForwardIterator range
ASTL_NODISCARD auto nth_most_frequent_element(R &&r, range_diff_type<R> n = 1)
    -> optional<astl::range_value_type<R>>
{
    return i::nth_most_frequent_element(adl::begin(r), adl::end(r), n);
}

template <typename R, typename BinaryPredicate>
// requires R ForwardIterator range
// requires BinaryPredicate, returns bool, two arguments of value_type(FwdIt)
ASTL_NODISCARD auto nth_most_frequent_element(R &&r, range_diff_type<R> n, BinaryPredicate pred)
    -> optional<astl::range_value_type<R>>
{
    return i::nth_most_frequent_element(adl::begin(r), adl::end(r), n, astl::pass_fn(pred));
}

template <typename R, typename BinaryPredicate, typename P>
ASTL_NODISCARD auto nth_most_frequent_element(R &&r, BinaryPredicate pred, range_diff_type<R> n,
                                              P p) -> optional<astl::range_value_type<R>>
{
    return i::nth_most_frequent_element(adl::begin(r), adl::end(r), n, astl::pass_fn(pred),
                                        astl::pass_fn(p));
}

template <typename R, typename BinaryPredicate>
// requires R ForwardIterator range
// requires BinaryPredicate, returns bool, two arguments of value_type(FwdIt)
ASTL_NODISCARD auto num_of_unique_elements(R &&r, BinaryPredicate pred) -> range_diff_type<R>
{
    return i::num_of_unique_elements(adl::begin(r), adl::end(r), astl::pass_fn(pred));
}

template <typename R>
// requires R ForwardIterator range
// requires BinaryPredicate, returns bool, two arguments of value_type(FwdIt)
ASTL_NODISCARD auto num_of_unique_elements(R &&r) -> range_diff_type<R>
{
    return i::num_of_unique_elements(adl::begin(r), adl::end(r));
}

template <typename R, typename BinaryPredicate, typename P>
ASTL_NODISCARD auto num_of_unique_elements(R &&r, BinaryPredicate pred, P p) -> range_diff_type<R>
{
    return i::num_of_unique_elements(adl::begin(r), adl::end(r), astl::pass_fn(pred),
                                     astl::pass_fn(p));
}
} // namespace r
} // namespace astl

#endif // ASTL_INCLUDE_DYNAMIC_PROGRAMING_HPP
