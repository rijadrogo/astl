//
// Created by Rijad on 30-Jul-18.
//

#ifndef ASTL_INCLUDE_ROTATE_HPP
#define ASTL_INCLUDE_ROTATE_HPP

#include <algorithm>
#include <type_traits>

#include "astl/iterator.hpp"
#include "astl/range_access.hpp"
#include "astl/temporary_buffer.hpp"

namespace astl
{
namespace i
{
inline constexpr struct {
    template <typename FwdIt>
    // requires FwdIt ForwardIterator
    auto operator()(FwdIt first, FwdIt last) const -> FwdIt
    {
        if (first == last) return last;

        if constexpr (is_bidirectional_it_v<FwdIt>) { // Bidirectional Iterator
            FwdIt buttlast(astl::prev(last));
            auto temp(std::move(*buttlast));
            FwdIt end(std::move_backward(first, buttlast, last));
            *first = std::move(temp);
            return end;
        }
        else { // Forward Iterator
            using std::swap;
            FwdIt current(first);
            while (++current != last) swap(*first, *current);

            return first;
        }
    }
} rotate_one_right{};

} // namespace i

namespace internal_rot
{
template <typename FwdIt, typename B>
// requires FwdIt ForwardIterator
// requires B ForwardIterator
auto rot_buff_lhs(FwdIt first, FwdIt mid, FwdIt last, B buffer) -> FwdIt
{
    // precondition first <= mid <= last
    // precondition distance(first, mid) < distance(mid, last)
    B buf_l(std::move(first, mid, buffer));
    FwdIt res(std::move(mid, last, first));
    std::move(buffer, buf_l, res);
    return res;
}

template <typename FwdIt, typename B>
// requires FwdIt ForwardIterator
// requires B ForwardIterator
auto rot_buff_rhs(FwdIt first, FwdIt mid, FwdIt last, B buffer) -> FwdIt
{
    // precondition first <= mid <= last
    // precondition distance(mid, last) < distance(first, mid)
    if (astl::next(mid) == last) return i::rotate_one_right(first, last);

    if constexpr (is_bidirectional_it_v<FwdIt>) { // Bidirectional Iterator
        B buf_l(std::move(mid, last, buffer));
        std::move_backward(first, mid, last);
        return std::move(buffer, buf_l, first);
    }
    else { // Forward Iterator
        (void) buffer;
        // TODO use buffer somehow
        return std::rotate(first, mid, last);
    }
}

template <typename FwdIt, typename N, typename B, typename Size>
// requires FwdIt ForwardIterator
// requires N integral type
// requires B ForwardIterator
// requires Size integral type
auto rotate_adaptive_unchecked(FwdIt first, N lhs_size, FwdIt mid, N rhs_size, FwdIt last, B buffer,
                               Size buffer_size) -> FwdIt
{
    // precondition first <= mid <= last
    // precondition distance(first, mid) == lhs_size
    // precondition distance(mid, last) == rhs_size
    using Ct = typename std::common_type<N, Size>::type;
    Ct ls(lhs_size);
    Ct rs(rhs_size);
    Ct bs(buffer_size);

    if (ls <= rs) {
        if (bs >= ls) return internal_rot::rot_buff_lhs(first, mid, last, buffer);
    }
    else {
        if (bs >= rs) return internal_rot::rot_buff_rhs(first, mid, last, buffer);
    }
    return std::rotate(first, mid, last);
}
} // namespace internal_rot

namespace i
{

inline constexpr struct {

    template <typename FwdIt> auto operator()(FwdIt first, FwdIt mid, FwdIt last) const -> FwdIt
    {
        return std::rotate(first, mid, last);
    }
} rotate{};

inline constexpr struct {
    template <typename FwdIt, typename N, typename B, typename Size>
    // requires FwdIt ForwardIterator
    // requires N integral type
    // requires B ForwardIterator
    // requires Size integral type
    auto operator()(FwdIt first, N lhs_size, FwdIt mid, N rhs_size, FwdIt last, B buffer,
                    Size buffer_size) const -> FwdIt
    {
        // precondition first <= mid <= last
        // precondition distance(first, mid) == lhs_size
        // precondition distance(mid, last) == rhs_size
        if (lhs_size == N(0)) return last;

        if (rhs_size == N(0)) return first;

        return internal_rot::rotate_adaptive_unchecked(first, lhs_size, mid, rhs_size, last, buffer,
                                                       buffer_size);
    }

    template <typename FwdIt, typename B, typename N>
    // requires FwdIt ForwardIterator
    // requires B ForwardIterator
    // requires N integral type
    auto operator()(FwdIt first, FwdIt mid, FwdIt last, B buffer, N buffer_size) const -> FwdIt
    {
        // precondition first <= mid <= last
        if (first == mid) return last;

        if (mid == last) return first;

        if (buffer_size == N(0)) return std::rotate(first, mid, last);

        return internal_rot::rotate_adaptive_unchecked(first, astl::distance(first, mid), mid,
                                                       astl::distance(mid, last), last, buffer,
                                                       buffer_size);
    }

    template <typename FwdIt>
    // requires FwdIt ForwardIterator
    auto operator()(FwdIt first, FwdIt mid, FwdIt last) const -> FwdIt
    {
        // precondition first <= mid <= last
        if (first == mid) return last;

        if (mid == last) return first;

        using Diff = iter_diff_type<FwdIt>;
        Diff lhs_size(astl::distance(first, mid));
        Diff rhs_size(astl::distance(mid, last));

        using T = iter_value_type<FwdIt>;
        inline_temporary_buffer<T> buffer(std::min(lhs_size, rhs_size), *first);
        return internal_rot::rotate_adaptive_unchecked(first, lhs_size, mid, rhs_size,
                                                       astl::next(mid, rhs_size), buffer.begin(),
                                                       buffer.size());
    }
} rotate_adaptive{};

inline constexpr struct {

    template <typename FwdIt, typename OutIt>
    auto operator()(FwdIt first, FwdIt mid, FwdIt last, OutIt dest) const -> FwdIt
    {
        return std::rotate_copy(first, mid, last, dest);
    }
} rotate_copy{};

inline constexpr struct {
    template <typename FwdIt>
    // requires FwdIt ForwardIterator
    auto operator()(FwdIt first, FwdIt last) const -> FwdIt
    {
        if (first == last) return last;

        auto tmp(std::move(*first));
        FwdIt end(std::move(astl::next(first), last, first));
        *end = std::move(tmp);
        return end;
    }
} rotate_one_left{};

inline constexpr struct {
    template <typename FwdIt, typename OutIt>
    // requires FwdIt ForwardIterator
    // requires OutIt OutputIterator
    auto operator()(FwdIt first, FwdIt mid, FwdIt last, OutIt dest) const -> OutIt
    {
        // precondition first <= mid <= last
        return i::rotate_copy(std::make_move_iterator(first), std::make_move_iterator(mid),
                              std::make_move_iterator(last), dest);
    }
} rotate_move{};

} // namespace i

namespace r
{

inline constexpr struct {
    template <typename R, typename FwdIt>
    // requires R ForwardIterator range
    // requires FwdIt ForwardIterator
    auto operator()(R &&r, FwdIt mid) const -> iter_of_range<R>
    {
        // precondition r.begin() <= mid <= r.end()
        return i::rotate(adl::begin(r), mid, adl::end(r));
    }
} rotate{};

inline constexpr struct {
    template <typename R, typename FwdIt>
    // requires R ForwardIterator range
    // requires FwdIt ForwardIterator
    auto operator()(R &&r, FwdIt mid) const -> iter_of_range<R>
    {
        // precondition r.begin() <= mid <= r.end()
        return i::rotate_adaptive(adl::begin(r), mid, adl::end(r));
    }

    template <typename R, typename FwdIt, typename B, typename N>
    // requires R ForwardIterator range
    // requires FwdIt ForwardIterator
    // requires B ForwardIterator
    // requires N integral type
    auto operator()(R &&r, FwdIt mid, B buffer, N buffer_size) const -> iter_of_range<R>
    {
        // precondition r.begin() <= mid <= r.end()
        return i::rotate_adaptive(adl::begin(r), mid, adl::end(r), buffer, buffer_size);
    }
} rotate_adaptive{};

inline constexpr struct {
    template <typename R, typename FwdIt, typename OutIt>
    // requires R ForwardIterator range
    // requires FwdIt ForwardIterator
    // requires OutIt OutputIterator
    auto operator()(R &&r, FwdIt mid, OutIt dest) const -> OutIt
    {
        return i::rotate_copy(adl::begin(r), mid, adl::end(r), dest);
    }
} rotate_copy{};

inline constexpr struct {
    template <typename R> auto operator()(R &&r) const -> iter_of_range<R>
    {
        return i::rotate_one_left(adl::begin(r), adl::end(r));
    }
} rotate_one_left{};

inline constexpr struct {
    template <typename R, typename FwdIt, typename OutIt>
    // requires R ForwardIterator range
    // requires FwdIt ForwardIterator
    // requires OutIt OutputIterator
    auto operator()(R &&r, FwdIt mid, OutIt dest) const -> OutIt
    {
        return i::rotate_move(adl::begin(r), mid, adl::end(r), dest);
    }
} rotate_move{};

inline constexpr struct {
    template <typename R>
    // requires R ForwardIterator range
    auto operator()(R &&r) const -> iter_of_range<R>
    {
        return i::rotate_one_right(adl::begin(r), adl::end(r));
    }
} rotate_one_right{};

} // namespace r
} // namespace astl

#endif // ASTL_INCLUDE_ROTATE_HPP
