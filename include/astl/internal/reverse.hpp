//
// Created by Rijad on 30-Jul-18.
//

#ifndef ASTL_INCLUDE_REVERSE_HPP
#define ASTL_INCLUDE_REVERSE_HPP

#include <algorithm>
#include <utility>

#include "astl/internal/for_each.hpp"
#include "astl/internal/move.hpp"
#include "astl/internal/rotate.hpp"
#include "astl/internal/swap.hpp"

#include "astl/iterator.hpp"
#include "astl/range_access.hpp"
#include "astl/temporary_buffer.hpp"

namespace astl
{
namespace i
{
// For ForwardIterator
// if there is enough space complexity O(n) otherwise complexity O(n log n)
// For other iterators
// complexity O(n)
template <typename FwdIt, typename OutIt>
// requires FwdIt ForwardIterator
// requires OutIt OutputIterator
auto reverse_copy(FwdIt first, FwdIt last, OutIt dest) -> OutIt;

template <typename FwdIt, typename OutIt>
// requires FwdIt ForwardIterator
// requires OutIt OutputIterator
auto reverse_move(FwdIt first, FwdIt last, OutIt dest) -> OutIt
{
    return i::reverse_copy(std::make_move_iterator(first), std::make_move_iterator(last), dest);
}

template <typename FwdIt, typename OutIt, typename B>
// requires FwdIt ForwardIterator
// requires OutIt OutputIterator
// requires B BidirectionalIterator
auto reverse_copy_with_buffer(FwdIt first, FwdIt last, OutIt dest, B buffer) -> OutIt
{
    // precondition: std::distance(first, last) <= buffer.size()
    B buffer_end(std::move(first, last, buffer));
    return std::reverse_copy(buffer, buffer_end, dest);
}

template <typename FwdIt, typename B>
// requires FwdIt ForwardIterator
// requires B BidirectionalIterator
auto reverse_with_buffer(FwdIt first, FwdIt last, B buffer) -> void
{
    // precondition: std::distance(first, last) <= buffer.size()
    B buff_end(std::move(first, last, buffer));
    i::reverse_move(buffer, buff_end, first);
}

template <typename FwdIt, typename N, typename BidirIt>
// requires FwdIt ForwardIterator
// requires N integral
// requires BidiIt BidirectionalIterator
auto reverse_n_bufferd(FwdIt first, N n, BidirIt buffer) -> FwdIt
{
    // precondition: n <= buffer.size()
    return i::reverse_move(buffer, i::move_n(first, n, buffer).first, first);
}

} // namespace i

namespace internal_rev
{
template <typename FwdIt, typename N, typename B>
// requires FwdIt ForwardIterator
// requires N Integral
// requires B BidirectionalIterator
auto reverse_adaptive1(FwdIt first, FwdIt last, N n, B buffer, N buffer_size) -> void
{
    if (n == 1 || n == 0) return;

    if (n <= buffer_size) {
        i::reverse_with_buffer(first, last, buffer);
        return;
    }

    N const half(n >> 1);
    N const other_half(n - half);
    FwdIt mid(astl::next(first, half));

    if (half <= buffer_size) i::reverse_with_buffer(first, mid, buffer);
    else
        internal_rev::reverse_adaptive1(first, mid, half, buffer, buffer_size);

    if (other_half <= buffer_size) i::reverse_with_buffer(mid, last, buffer);
    else
        internal_rev::reverse_adaptive1(mid, last, other_half, buffer, buffer_size);

    i::rotate_adaptive(first, half, mid, other_half, last, buffer, buffer_size);
}

template <typename FwdIt, typename OutIt, typename N, typename B>
// requires FwdIt ForwardIterator
// requires OutIt OutputIterator
// requires N Integral
// requires B BidirectionalIterator
auto reverse_copy_adaptive1(FwdIt first, FwdIt last, OutIt dest, N n, B buffer, N buffer_size)
    -> OutIt
{
    if (n == 0) return dest;

    if (n == 1) {
        *dest = *first;
        return dest;
    }
    if (n <= buffer_size) return i::reverse_copy_with_buffer(first, last, dest, buffer);

    N const half(n >> 1);
    N const other_half(n - half);
    FwdIt m(astl::next(first, half));
    OutIt k(astl::next(dest, other_half));
    OutIt it(std::rotate_copy(first, m, last, dest));
    if (half <= buffer_size) i::reverse_copy_with_buffer(first, m, k, buffer);
    else
        internal_rev::reverse_copy_adaptive1(first, m, k, half, buffer, buffer_size);

    if (other_half <= buffer_size) i::reverse_copy_with_buffer(m, last, dest, buffer);
    else
        internal_rev::reverse_copy_adaptive1(m, last, dest, other_half, buffer, buffer_size);

    return it;
}

// For ForwardIterator
// if there is enough space complexity O(n) otherwise complexity O(n log n)
// For other iterators
// complexity O(n)
template <typename FwdIt>
// requires FwdIt ForwardIterator
auto reverse_adaptive1(FwdIt first, FwdIt last) -> void
{
    if (first == last) return;
    if constexpr (is_bidirectional_it_v<FwdIt>) { // Bidirectional Iterator
        std::reverse(first, last);
    }
    else { // Forward Iterator
        using T = iter_value_type<FwdIt>;
        auto n(astl::distance(first, last));
        inline_temporary_buffer<T> buffer(n, *first);

        using CommonT =
            typename std::common_type<iter_diff_type<FwdIt>, decltype(buffer.size())>::type;
        internal_rev::reverse_adaptive1(first, last, CommonT(astl::distance(first, last)),
                                        buffer.begin(), CommonT(buffer.size()));
    }
}

template <typename T> // T models Binary Integer
auto halve_non_negative(T a) -> T
{
    // precondition: a >= 0
    return a >> 1;
}

// For ForwardIterator
// if there is enough space complexity O(n) otherwise complexity O(n logn)
// For other iterators
// complexity O(n)
template <typename FwdIt, typename OutIt>
// requires FwdIt ForwardIterator
// requires OutIt OutputIterator
auto reverse_copy_adaptive(FwdIt first, FwdIt last, OutIt dest) -> OutIt
{
    if (first == last) return dest;
    if (astl::next(first) == last) {
        *dest = *first;
        return dest;
    }
    if constexpr (is_bidirectional_it_v<FwdIt>) { // Bidirectional Iterator
        return std::reverse_copy(first, last, dest);
    }
    else { // Forward Iterator

        auto n(astl::distance(first, last));
        using T = iter_value_type<FwdIt>;
        inline_temporary_buffer<T> buffer(n, *first);
        using CommonT =
            typename std::common_type<iter_diff_type<FwdIt>, decltype(buffer.size())>::type;
        return internal_rev::reverse_copy_adaptive1(first, last, dest, CommonT(n), buffer.begin(),
                                                    CommonT(buffer.size()));
    }
}

} // namespace internal_rev

namespace i
{
// For ForwardIterator
// if there is enough space complexity O(n) otherwise complexity O(n log n)
// For other iterators
// complexity O(n)
template <typename FwdIt>
// requires FwdIt ForwardIterator
auto reverse(FwdIt first, FwdIt last) -> void
{
    if constexpr (is_bidirectional_it_v<FwdIt>) // Bidirectional Iterator
        std::reverse(first, last);
    else // Forward Iterator
        internal_rev::reverse_adaptive1(first, last);
}

template <typename FwdIt, typename N1, typename BidirIt, typename N2>
// requires FwdIt ForwardIterator
// requires N1 integral
// requires BidiIt BidirectionalIterator
// requires N2 integral
auto reverse_n_adaptive(FwdIt first, N1 n, BidirIt buffer, N2 buffer_size) -> FwdIt
{
    if (n < N1(2)) return astl::next(first, n);

    using CommonT = typename std::common_type<N1, N2>::type;
    if (CommonT(n) <= CommonT(buffer_size)) return i::reverse_n_bufferd(first, n, buffer);

    CommonT h_i(internal_rev::halve_non_negative(n));
    CommonT n_mod_2(n - (h_i + h_i));
    FwdIt m_i(astl::next(i::reverse_n_adaptive(first, h_i, buffer, buffer_size), n_mod_2));
    FwdIt l_i(i::reverse_n_adaptive(m_i, h_i, buffer, buffer_size));
    i::swap_ranges_n(first, h_i, m_i);
    return l_i;
}

// For ForwardIterator
// if there is enough space complexity O(n) otherwise complexity O(n logn)
// For other iterators
// complexity O(n)
template <typename FwdIt, typename N, typename B>
// requires FwdIt ForwardIterator
// requires N Integral
// requires B BidirectionalIterator
auto reverse_adaptive(FwdIt first, FwdIt last, B buffer, N buffer_size) -> void
{
    i::reverse_n_adaptive(first, astl::distance(first, last), buffer, buffer_size);
}

// For ForwardIterator
// if there is enough space complexity O(n) otherwise complexity O(n logn),
// space complexity O(n)
// For other iterators complexity O(n)
template <typename FwdIt, typename N>
// requires FwdIt ForwardIterator
// requires N integral type
auto reverse_n(FwdIt first, N n) -> FwdIt
{
    if (n == N(0)) return first;

    if constexpr (is_bidirectional_it_v<FwdIt>) { // Bidirectional Iterator
        FwdIt last(astl::next(first, n));
        std::reverse(first, last);
        return last;
    }
    else { // Forward Iterator
        using T = iter_value_type<FwdIt>;
        inline_temporary_buffer<T> buffer(n, *first);
        return i::reverse_n_adaptive(first, n, buffer.begin(), buffer.size());
    }
}

// For ForwardIterator
// if there is enough space complexity O(n) otherwise complexity O(n logn)
// For other iterators
// complexity O(n)
template <typename FwdIt, typename OutIt, typename N, typename B>
// requires FwdIt ForwardIterator
// requires OutIt OutputIterator
// requires N Integral
// requires B BidirectionalIterator
auto reverse_copy_adaptive(FwdIt first, FwdIt last, OutIt dest, B buffer, N buffer_size) -> OutIt
{
    if constexpr (is_bidirectional_it_v<FwdIt>) { // Bidirectional Iterator
        return std::reverse_copy(first, last, dest);
    }
    else { // Forward Iterator
        using CommonT = typename std::common_type<iter_diff_type<FwdIt>, N>::type;
        return internal_rev::reverse_copy_adaptive1(
            first, last, dest, CommonT(astl::distance(first, last)), buffer, CommonT(buffer_size));
    }
}

// For ForwardIterator
// if there is enough space complexity O(n) otherwise complexity O(n log n)
// For other iterators
// complexity O(n)
template <typename FwdIt, typename OutIt>
// requires FwdIt ForwardIterator
// requires OutIt OutputIterator
auto reverse_copy(FwdIt first, FwdIt last, OutIt dest) -> OutIt
{
    if constexpr (is_bidirectional_it_v<FwdIt>) // Bidirectional Iterator
        return std::reverse_copy(first, last, dest);
    else // Forward Iterator
        return internal_rev::reverse_copy_adaptive(first, last, dest);
}

// For ForwardIterator
// if there is enough space complexity O(n) otherwise complexity O(n log n)
// For other iterators
// complexity O(n)
template <typename FwdIt, typename N, typename OutIt>
// requires FwdIt ForwardIterator
// requires N integral type
// requires OutIt OutputIterator
auto reverse_copy_n(FwdIt first, N n, OutIt dest) -> std::pair<OutIt, FwdIt>
{
    FwdIt last(astl::next(first, n));
    OutIt last_dest(i::reverse_copy(first, last, dest));
    return std::make_pair(last_dest, last);
}

template <typename FwdIt, typename OutIt, typename N, typename B>
// requires FwdIt ForwardIterator
// requires OutIt OutputIterator
// requires N Integral
// requires B BidirectionalIterator
auto reverse_move_adaptive(FwdIt first, FwdIt last, OutIt dest, B buffer, N buffer_size) -> OutIt
{
    return i::reverse_copy_adaptive(std::make_move_iterator(first), std::make_move_iterator(last),
                                    dest, buffer, buffer_size);
}

template <typename FwdIt, typename N, typename OutIt>
// requires FwdIt ForwardIterator
// requires N integral type
// requires OutIt OutputIterator
auto reverse_move_n(FwdIt first, N n, OutIt dest) -> std::pair<OutIt, FwdIt>
{
    FwdIt last(astl::next(first, n));
    OutIt last_dest(i::reverse_move(first, last, dest));
    return std::make_pair(last_dest, last);
}

template <typename FwdIt, typename OutIt, typename B>
// requires FwdIt ForwardIterator
// requires OutIt OutputIterator
// requires B BidirectionalIterator
auto reverse_move_with_buffer(FwdIt first, FwdIt last, OutIt dest, B buffer) -> OutIt
{
    // precondition std::distance(first, last) <= buffer.size()
    B buffer_end(std::move(first, last, buffer));
    return i::reverse_move(buffer, buffer_end, dest);
}

// returns true iff function swapped all elements that satisfied UnaryPredicate, otherwise
// false
template <typename BidiIt, typename UnaryPredicate>
// requires BidiIt BidirectionalIterator
// requires UnaryPredicate returns bool, argument ValueType(BidiIt)
auto reverse_if(BidiIt first, const BidiIt last, UnaryPredicate pred) -> bool
{
    return std::get<2>(
        i::bidirectional_traversal(first, last, [pred(astl::pass_fn(pred))](auto &x, auto &y) {
            using std::swap;
            if (pred(x) && pred(y)) swap(x, y);
        }));
}

template <typename BidiIt, typename UnaryPredicate, typename P>
auto reverse_if(BidiIt first, const BidiIt last, UnaryPredicate pred, P p) -> bool
{
    return i::reverse_if(first, last, astl::combine(astl::pass_fn(pred), astl::pass_fn(p)));
}

template <typename BidiIt>
// requires BidiIt BidirectionalIterator
auto reverse_until_sentinel(BidiIt first, BidiIt sentinel, BidiIt last) -> std::pair<BidiIt, BidiIt>
{
    // precondition first <= sentinel <= last
    using std::swap;
    while (first != sentinel && sentinel != last) {
        --last;

        swap(*first, *last);

        ++first;
    }
    return std::make_pair(first, last);
}

} // namespace i

namespace internal_rev
{
template <typename R> auto reverse_range1(R &&r, internal_adl::rank<0>) -> void
{
    return i::reverse(adl::begin(r), adl::end(r));
}

template <typename R> auto reverse_range1(R &&r, internal_adl::rank<1>) -> decltype(r.reverse())
{
    return r.reverse();
}

} // namespace internal_rev

namespace r
{
template <typename R>
// requires R ForwardIterator range
auto reverse(R &&r) -> void
{
    internal_rev::reverse_range1(r, internal_adl::rank<1>{});
}

template <typename R, typename B, typename N>
// requires R ForwardIterator range
// requires B BidirectionalIterator
// requires N integral type
auto reverse_adaptive(R &&r, B buffer, N buffer_size) -> void
{
    if constexpr (is_bidirectional_it_v<iter_of_range<R>>) // Bidirectional Iterator
        std::reverse(adl::begin(r), adl::end(r));
    else // Forward Iterator
        i::reverse_n_adaptive(adl::begin(r), astl::size_or_distance(r), buffer, buffer_size);
}

template <typename R, typename N>
// requires R ForwardIterator range
// requires N integral type
auto reverse_n(R &&r, N n) -> iter_of_range<R>
{
    return i::reverse_n(adl::begin(r), n);
}

template <typename R, typename OutIt>
// requires R ForwardIterator range
// requires OutIt OutputIterator
auto reverse_copy(R &&r, OutIt dest) -> OutIt
{
    return i::reverse_copy(adl::begin(r), adl::end(r), dest);
}

template <typename R, typename OutIt, typename B, typename N>
// requires R ForwardIterator range
// requires OutIt OutputIterator
auto reverse_copy_adaptive(R &&r, OutIt dest, B buffer, N buffer_size) -> OutIt
{
    if constexpr (is_bidirectional_it_v<iter_of_range<R>>) { // Bidirectional Iterator
        return std::reverse_copy(adl::begin(r), adl::end(r), dest);
    }
    else { // Forward Iterator
        using CommonT = typename std::common_type<range_diff_type<R>, N>::type;
        return internal_rev::reverse_copy_adaptive1(adl::begin(r), adl::end(r), dest,
                                                    CommonT(astl::size_or_distance(r)), buffer,
                                                    CommonT(buffer_size));
    }
}

template <typename R, typename N, typename OutIt>
// requires R ForwardIterator range
// requires N integral type
// requires OutIt OutputIterator
auto reverse_copy_n(R &&r, N n, OutIt dest) -> std::pair<OutIt, astl::iter_of_range<R>>
{
    return i::reverse_copy_n(adl::begin(r), n, dest);
}

template <typename R, typename OutIt, typename B>
// requires R ForwardIterator range
// requires OutIt OutputIterator
// requires B BidirectionalIterator
auto reverse_copy_with_buffer(R &&r, OutIt dest, B buffer) -> OutIt
{
    return i::reverse_copy_with_buffer(adl::begin(r), adl::end(r), dest, buffer);
}

template <typename R, typename OutIt>
// requires R ForwardIterator range
// requires OutIt OutputIterator
auto reverse_move(R &&r, OutIt dest) -> OutIt
{
    return i::reverse_move(adl::begin(r), adl::end(r), dest);
}

template <typename R, typename OutIt, typename B, typename N>
// requires R ForwardIterator range
// requires OutIt OutputIterator
auto reverse_move_adaptive(R &&r, OutIt dest, B buffer, N buffer_size) -> OutIt
{
    auto first_move(std::make_move_iterator(adl::begin(r)));
    auto last_move(std::make_move_iterator(adl::end(r)));
    if constexpr (is_bidirectional_it_v<iter_of_range<R>>) { // Bidirectional Iterator
        return std::reverse_copy(first_move, last_move, dest);
    }
    else { // Forward Iterator
        using CommonT = typename std::common_type<range_diff_type<R>, N>::type;
        return internal_rev::reverse_copy_adaptive1(first_move, last_move, dest,
                                                    CommonT(astl::size_or_distance(r)), buffer,
                                                    CommonT(buffer_size));
    }
}

template <typename R, typename N, typename OutIt>
// requires R ForwardIterator range
// requires N integral type
// requires OutIt OutputIterator
auto reverse_move_n(R &&r, N n, OutIt dest) -> std::pair<OutIt, astl::iter_of_range<R>>
{
    return i::reverse_move_n(adl::begin(r), n, dest);
}

template <typename R, typename OutIt, typename B>
// requires R ForwardIterator range
// requires OutIt OutputIterator
// requires B BidirectionalIterator
auto reverse_move_with_buffer(R &&r, OutIt dest, B buffer) -> OutIt
{
    return i::reverse_move_with_buffer(adl::begin(r), adl::end(r), dest, buffer);
}

template <typename R, typename UnaryPredicate>
// requires R Bidirectional range
// requires UnaryPredicate UnaryFunction returns bool, argument ValueType(R)
auto reverse_if(R &&r, UnaryPredicate pred) -> bool
{
    return i::reverse_if(adl::begin(r), adl::end(r), astl::pass_fn(pred));
}

template <typename R, typename UnaryPredicate, typename P>
auto reverse_if(R &&r, UnaryPredicate pred, P p) -> bool
{
    return i::reverse_if(adl::begin(r), adl::end(r),
                         astl::combine(astl::pass_fn(pred), astl::pass_fn(p)));
}

template <typename R, typename BidiIt>
// requires R BidirectionalIterator range
// requires BidiIt BidirectionalIterator
auto reverse_until_sentinel(R &&r, BidiIt mid) -> std::pair<BidiIt, BidiIt>
{
    // precondition r.begin() <= mid <= r.end()
    return i::reverse_until_sentinel(adl::begin(r), mid, adl::end(r));
}

template <typename R, typename OutIt, typename B>
// requires R ForwardIterator range
// requires B BidirectionalIterator
auto reverse_with_buffer(R &&r, B buffer) -> void
{
    i::reverse_with_buffer(adl::begin(r), adl::end(r), buffer);
}
} // namespace r
} // namespace astl

#endif // ASTL_INCLUDE_REVERSE_HPP
