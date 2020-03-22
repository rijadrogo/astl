//
// Created by Rijad on 30-Jul-18.
//

#ifndef ASTL_INCLUDE_COPY_HPP
#define ASTL_INCLUDE_COPY_HPP

#include <algorithm>
#include <type_traits>
#include <utility>

#include "astl/functional.hpp"
#include "astl/iterator.hpp"
#include "astl/range_access.hpp"

namespace astl
{
namespace i
{

inline constexpr struct {
    template <typename InIt, typename OutIt>
    auto operator()(InIt first, InIt last, OutIt dest) const -> OutIt
    {
        return std::copy(first, last, dest);
    }
} copy{};

inline constexpr struct {
    template <typename InIt, typename OutIt>
    auto operator()(InIt first, InIt last, OutIt dest_last) const -> OutIt
    {
        return std::copy_backward(first, last, dest_last);
    }
} copy_backward{};

inline constexpr struct {
    template <typename InIt, typename FwdIt>
    // requires InIt InputIterator
    // requires FwdIt ForwardIterator
    auto operator()(InIt first, InIt const last, FwdIt dest_first, FwdIt const dest_last) const
        -> std::pair<FwdIt, InIt>
    {
        if constexpr (is_random_access_it_v<InIt, FwdIt>) { // Random Access Iterators
            using RandIt1 = InIt;
            using RandIt2 = FwdIt;
            using Ct =
                typename std::common_type<iter_diff_type<RandIt1>, iter_diff_type<RandIt2>>::type;
            Ct min_of(std::min(Ct(last - first), Ct(dest_last - dest_first)));
            RandIt2 res(std::copy_n(first, min_of, dest_first));
            return std::make_pair(res, first + min_of);
        }
        else { // Input iterators
            while (first != last && dest_first != dest_last) {
                *dest_first = *first;
                ++first;
                ++dest_first;
            }
            return std::make_pair(dest_first, first);
        }
    }
} copy_bounded{};

inline constexpr struct {
    template <typename InIt, typename N1, typename FwdIt, typename N2>
    // requires InIt InputIterator
    // requires N1 integral type
    // requires FwdIt ForwardIterator
    // requires N2 integral type
    auto operator()(InIt first, N1 n1, FwdIt dest, N2 n2) const -> std::pair<FwdIt, InIt>
    {
        if constexpr (is_random_access_it_v<InIt, FwdIt>) { // Random Access Iterators
            return i::copy_bounded(first, first + n1, dest, dest + n2);
        }
        else { // Input iterators
            using Ct = typename std::common_type<N1, N2>::type;
            Ct min_of(std::min(Ct(n1), Ct(n2)));
            while (min_of != Ct(0)) {
                *dest = *first;
                ++first;
                ++dest;
                --min_of;
            }
            return std::make_pair(dest, first);
        }
    }
} copy_bounded_n{};

inline constexpr struct {
    template <typename InIt, typename OutIt, typename UnaryPredicate>
    auto operator()(InIt first, InIt last, OutIt dest, UnaryPredicate pred) const -> OutIt
    {
        return std::copy_if(first, last, dest, astl::pass_fn(pred));
    }

    template <typename InIt, typename OutIt, typename UnaryPredicate, typename P>
    auto operator()(InIt first, InIt last, OutIt dest, UnaryPredicate pred, P p) const -> OutIt
    {
        while (first != last) {
            if (pred(invoke(p, *first))) {
                *dest = *first;
                ++dest;
            }
            ++first;
        }
        return dest;
    }
} copy_if{};

inline constexpr struct {
    template <typename InIt, typename N, typename OutIt, typename UnaryPredicate>
    // requires InIt InputIterator
    // requires N integral type
    // requires OutIt OutputIterator
    // requires UnaryPredicate, returns bool, argument type value_type(InIt)
    auto operator()(InIt first, N n, OutIt dest, UnaryPredicate pred) const -> OutIt
    {
        while (n != N(0)) {
            if (pred(*first)) {
                *dest = *first;
                ++dest;
            }
            ++first;
            --n;
        }
        return std::make_pair(dest, first);
    }

    template <typename InIt, typename N, typename OutIt, typename UnaryPredicate, typename P>
    auto operator()(InIt first, N n, OutIt dest, UnaryPredicate pred, P p) const
        -> std::pair<OutIt, InIt>
    {
        while (n != N(0)) {
            if (pred(invoke(p, *first))) {
                *dest = *first;
                ++dest;
            }
            ++first;
            --n;
        }
        return std::make_pair(dest, first);
    }
} copy_if_n{};

inline constexpr struct {
    template <typename InIt, typename N, typename OutIt>
    // requires InIt InputIterator
    // requires N integral type
    // requires OutIt OutputIterator
    auto operator()(InIt first, N n, OutIt dest) const -> std::pair<OutIt, InIt>
    {
        if constexpr (is_random_access_it_v<InIt, OutIt>) { // Random Access Iterator
            OutIt last(std::copy_n(first, n, dest));
            return std::make_pair(last, first + n);
        }
        else { // Input Iterator
            auto trip_count(n >> 2);
            while (trip_count > N(0)) {
                *dest = *first;
                ++first;
                ++dest;
                --n;

                *dest = *first;
                ++first;
                ++dest;
                --n;

                *dest = *first;
                ++first;
                ++dest;
                --n;

                *dest = *first;
                ++first;
                ++dest;
                --n;

                --trip_count;
            }
            switch (integral_t<N>(n)) {
            case 3:
                *dest = *first;
                ++first;
                ++dest;

            case 2:
                *dest = *first;
                ++first;
                ++dest;

            case 1:
                *dest = *first;
                ++first;
                ++dest;
            case 0:
            default: break;
            }
            return std::make_pair(dest, first);
        }
    }
} copy_n{};

inline constexpr struct {
    template <typename InIt, typename OutIt, typename T>
    // requires InIt InputIterator
    // requires OutIt OutputIterator
    // requires T equality comparable with value_type(InIt)
    auto operator()(InIt first, InIt last, OutIt dest, T const &val) const -> std::pair<OutIt, InIt>
    {
        while (first != last && *first == val) {
            *dest = *first;
            ++first;
            ++dest;
        }
        return std::make_pair(dest, first);
    }

    template <typename InIt, typename OutIt, typename T, typename P>
    auto operator()(InIt first, InIt last, OutIt dest, T const &val, P p) const
        -> std::pair<OutIt, InIt>
    {
        while (first != last && invoke(p, *first) == val) {
            *dest = *first;
            ++first;
            ++dest;
        }
        return std::make_pair(dest, first);
    }
} copy_until_sentinel{};

inline constexpr struct {
    template <typename InIt, typename N, typename OutIt, typename T>
    // requires InIt InputIterator
    // requires N integral type
    // requires OutIt OutputIterator
    // requires T equality comparable with value_type(InIt)
    auto operator()(InIt first, N n, OutIt dest, T const &val) const -> std::pair<OutIt, InIt>
    {
        while (n != N(0) && *first == val) {
            *dest = *first;
            ++first;
            ++dest;
            --n;
        }
        return std::make_pair(dest, first);
    }

    template <typename InIt, typename N, typename OutIt, typename T, typename P>
    auto operator()(InIt first, N n, OutIt dest, T const &val, P p) const -> std::pair<OutIt, InIt>
    {
        while (n != N(0) && invoke(p, *first) == val) {
            *dest = *first;
            ++first;
            ++dest;
            --n;
        }
        return std::make_pair(dest, first);
    }
} copy_until_sentinel_n{};

inline constexpr struct {
    template <typename InIt, typename OutIt, typename UnaryPredicate>
    // requires InIt InputIterator
    // requires OutIt OutputIterator
    // requires UnaryPredicate, returns bool, argument type value_type(InIt)
    auto operator()(InIt first, InIt last, OutIt dest, UnaryPredicate pred) const
        -> std::pair<InIt, OutIt>
    {
        while (first != last && pred(*first)) {
            *dest = *first;
            ++first;
            ++dest;
        }
        return std::make_pair(dest, first);
    }

    template <typename InIt, typename OutIt, typename UnaryPredicate, typename P>
    auto operator()(InIt first, InIt last, OutIt dest, UnaryPredicate pred, P p) const
        -> std::pair<InIt, OutIt>
    {
        return (*this)(first, last, dest, astl::combine(astl::pass_fn(pred), astl::pass_fn(p)));
    }
} copy_while{};

inline constexpr struct {
    template <typename InIt, typename OutIt, typename UnaryPredicate>
    // requires InIt InputIterator
    // requires OutIt OutputIterator
    // requires UnaryPredicate, returns bool, argument type value_type(InIt)
    auto operator()(InIt first, InIt last, OutIt dest, UnaryPredicate pred) const
        -> std::pair<InIt, OutIt>
    {
        return i::copy_while(first, last, dest, astl::not_fn(astl::pass_fn(pred)));
    }

    template <typename InIt, typename OutIt, typename UnaryPredicate, typename P>
    auto operator()(InIt first, InIt last, OutIt dest, UnaryPredicate pred, P p) const
        -> std::pair<InIt, OutIt>
    {
        return i::copy_while(first, last, dest, astl::not_fn(astl::pass_fn(pred)),
                             astl::pass_fn(p));
    }
} copy_until{};

inline constexpr struct {
    template <typename InIt, typename OutIt, typename BinaryPredicate>
    // requires InIt InputIterator
    // requires OutIt OutputIterator
    // requires BinaryPredicate, returns bool, two arguments of value_type(InIt)
    auto operator()(InIt first, InIt last, OutIt dest, BinaryPredicate pred) const
        -> std::pair<OutIt, InIt>
    {
        if constexpr (is_forward_it_v<InIt>) { // Forward Iterator
            using FwdIt = InIt;
            if (first != last) {
                FwdIt next(astl::next(first));
                while (next != last && pred(*first, *next)) {
                    *dest = *first;
                    ++first;
                    ++next;
                    ++dest;
                }
                *dest = *first;
                ++dest;
            }
            return std::make_pair(dest, first);
        }
        else { // Input Iterator
            if (first != last) {
                auto t1(*first);
                InIt next(astl::next(first));
                while (next != last) {
                    auto t2(*next);
                    if (!pred(t1, t2)) break;

                    *dest = t1;
                    t1 = std::move(t2);
                    ++next;
                    ++dest;
                }
                *dest = t1;
                ++dest;
            }
            return std::make_pair(dest, first);
        }
    }

    template <typename InIt, typename OutIt, typename BinaryPredicate, typename P>
    auto operator()(InIt first, InIt last, OutIt dest, BinaryPredicate pred, P p) const
        -> std::pair<OutIt, InIt>
    {
        return (*this)(first, last, dest, astl::combine(astl::pass_fn(pred), astl::pass_fn(p)));
    }
} copy_until_adjacent_check{};

inline constexpr struct {
    template <typename InIt, typename N, typename OutIt, typename BinaryPredicate>
    // requires InIt InputIterator
    // requires N integral type
    // requires OutIt OutputIterator
    // requires BinaryPredicate, returns bool, two arguments of value_type(InIt)
    auto operator()(InIt first, N n, OutIt dest, BinaryPredicate pred) const
        -> std::pair<OutIt, InIt>
    {
        if constexpr (is_forward_it_v<InIt>) { // Forward Iterator
            using FwdIt = InIt;
            if (n != N(0)) {
                FwdIt next(astl::next(first));
                while (--n != N(0) && pred(*first, *next)) {
                    *dest = *first;
                    ++first;
                    ++next;
                    ++dest;
                }
                *dest = *first;
                ++dest;
            }
            return std::make_pair(dest, first);
        }
        else { // Input Iterator
            if (n != N(0)) {
                auto t1(*first);
                ++first;
                while (--n != N(0)) {
                    auto t2(*first);
                    if (!pred(t1, t2)) break;

                    *dest = std::move(t1);
                    t1 = std::move(t2);
                    ++first;
                    ++dest;
                }
                *dest = t1;
                ++dest;
            }
            return std::make_pair(dest, first);
        }
    }

    template <typename FwdIt, typename N, typename OutIt, typename BinaryPredicate, typename P>
    auto operator()(FwdIt first, N n, OutIt dest, BinaryPredicate pred, P p) const
        -> std::pair<OutIt, FwdIt>
    {
        return (*this)(first, n, dest, astl::combine(astl::pass_fn(pred), astl::pass_fn(p)));
    }
} copy_until_adjacent_check_n{};

inline constexpr struct {
    template <typename InIt, typename N, typename OutIt, typename UnaryPredicate>
    // requires InIt InputIterator
    // requires N integral type
    // requires OutIt OutputIterator
    // requires UnaryPredicate, returns bool, argument value_type(InIt)
    auto operator()(InIt first, N n, OutIt dest, UnaryPredicate pred) const
        -> std::pair<InIt, OutIt>
    {
        while (n != N(0) && pred(*first)) {
            *dest = *first;
            ++first;
            ++dest;
            --n;
        }
        return std::make_pair(first, dest);
    }

    template <typename InIt, typename N, typename OutIt, typename UnaryPredicate, typename P>
    auto operator()(InIt first, N n, OutIt dest, UnaryPredicate pred, P p) const
        -> std::pair<InIt, OutIt>
    {
        return (*this)(first, n, dest, astl::combine(astl::pass_fn(pred), astl::pass_fn(p)));
    }
} copy_while_n{};

inline constexpr struct {
    template <typename InIt, typename N, typename OutIt, typename UnaryPredicate>
    // requires InIt InputIterator
    // requires OutIt OutputIterator
    // requires UnaryPredicate, returns bool, argument type value_type(InIt)
    auto operator()(InIt first, N n, OutIt dest, UnaryPredicate pred) const
        -> std::pair<InIt, OutIt>
    {
        return i::copy_while_n(first, n, dest, astl::not_fn(astl::pass_fn(pred)));
    }

    template <typename InIt, typename N, typename OutIt, typename UnaryPredicate, typename P>
    auto operator()(InIt first, N n, OutIt dest, UnaryPredicate pred, P p) const
        -> std::pair<InIt, OutIt>
    {
        return i::copy_while_n(first, n, dest, astl::not_fn(astl::pass_fn(pred)), astl::pass_fn(p));
    }
} copy_until_n{};

inline constexpr struct {
    template <typename InIt, typename OutIt, typename Comparator = std::less<>>
    // requires InIt InputIterator
    // requires OutIt OutputIterator
    // requires Comparator, StrictWeakOrdering on value_type(InIt)
    auto operator()(InIt first, InIt last, OutIt dest, Comparator comp = Comparator{}) const
        -> std::pair<OutIt, InIt>
    {
        return i::copy_until_adjacent_check(first, last, dest, astl::pass_fn(comp));
    }

    template <typename InIt, typename OutIt, typename Compare, typename P>
    auto operator()(InIt first, InIt last, OutIt dest, Compare comp, P p) const
        -> std::pair<OutIt, InIt>
    {
        return i::copy_until_adjacent_check(first, last, dest, astl::pass_fn(comp),
                                            astl::pass_fn(p));
    }
} copy_until_sorted{};

inline constexpr struct {
    template <typename InIt, typename N, typename OutIt, typename Comparator = std::less<>>
    // requires InIt InputIterator
    // requires OutIt OutputIterator
    // requires Comparator, StrictWeakOrdering on value_type(InIt)
    auto operator()(InIt first, N n, OutIt dest, Comparator comp = Comparator{}) const
        -> std::pair<OutIt, InIt>
    {
        return i::copy_until_adjacent_check_n(first, n, dest, astl::pass_fn(comp));
    }

    template <typename InIt, typename N, typename OutIt, typename Compare, typename P>
    auto operator()(InIt first, N n, OutIt dest, Compare comp, P p) const -> std::pair<OutIt, InIt>
    {
        return i::copy_until_adjacent_check_n(first, n, dest, astl::pass_fn(comp),
                                              astl::pass_fn(p));
    }
} copy_until_sorted_n{};

inline constexpr struct {
    template <typename BidiIt, typename OutIt>
    // requires BidiIt BidirectionalIterator
    // requires OutIt BidirectionalIterator
    auto operator()(BidiIt first, BidiIt last, OutIt dest) const -> OutIt
    {
        // if first and dest are same type we cant use copy because overlapping
        if (is_random_access_it_v<BidiIt, OutIt> && !std::is_same_v<BidiIt, OutIt>) {
            OutIt dest_first(dest - (last - first));
            i::copy(first, last, dest_first);
            return dest_first;
        }
        else {
            while (first != last) *--dest = *first--;

            return dest;
        }
    }
} reverse_copy_backward{};

} // namespace i

namespace r
{
inline constexpr struct {
    template <typename R, typename OutIt>
    // requires R InputIterator range
    // requires OutIt OutputIterator
    auto operator()(R &&r, OutIt dest) const -> OutIt
    {
        return i::copy(adl::begin(r), adl::end(r), dest);
    }
} copy{};

inline constexpr struct {
    template <typename R, typename OutIt>
    // requires R BidirectionalIterator range
    // requires OutIt BidirectionalIterator
    auto operator()(R &&r, OutIt dest) const -> OutIt
    {
        return i::copy_backward(adl::begin(r), adl::end(r), dest);
    }
} copy_backward{};

inline constexpr struct {
    template <typename R1, typename R2>
    // requires R1 InputIterator range
    // requires R2 ForwardIterator range
    auto copy_bounded(R1 &&r1, R2 &&r2) const
        -> std::pair<astl::iter_of_range<R2>, astl::iter_of_range<R1>>
    {
        return i::copy_bounded(adl::begin(r1), adl::end(r1), adl::begin(r2), adl::end(r2));
    }
} copy_bounded{};

inline constexpr struct {
    template <typename R1, typename N1, typename R2, typename N2>
    // requires R1 InputIterator range
    // requires N1 integral type
    // requires R2 ForwardIterator range
    // requires N2 integral type
    auto operator()(R1 &&r1, N1 n1, R2 &&r2, N2 n2) const
        -> std::pair<astl::iter_of_range<R2>, astl::iter_of_range<R1>>
    {
        return i::copy_bounded_n(adl::begin(r1), n1, adl::begin(r2), n2);
    }
} copy_bounded_n{};

inline constexpr struct {
    template <typename R, typename OutIt, typename UnaryPredicate>
    // requires R InputIterator range
    // requires OutIt OutputIterator
    // requires UnaryPredicate, returns bool, argument type value_type(R)
    auto operator()(R &&r, OutIt dest, UnaryPredicate pred) const -> OutIt
    {
        return i::copy_if(adl::begin(r), adl::end(r), dest, astl::pass_fn(pred));
    }

    template <typename R, typename OutIt, typename UnaryPredicate, typename P>
    auto operator()(R &&r, OutIt dest, UnaryPredicate pred, P p) const -> OutIt
    {
        return i::copy_if(adl::begin(r), adl::end(r), dest, astl::pass_fn(pred), astl::pass_fn(p));
    }
} copy_if{};

inline constexpr struct {
    template <typename R, typename N, typename OutIt, typename UnaryPredicate>
    // requires R InputIterator range
    // requires N integral type
    // requires OutIt OutputIterator
    // requires UnaryPredicate, returns bool, argument type value_type(R)
    auto operator()(R &&r, N n, OutIt dest, UnaryPredicate pred) const
        -> std::pair<OutIt, astl::iter_of_range<R>>
    {
        return i::copy_if_n(adl::begin(r), n, dest, astl::pass_fn(pred));
    }

    template <typename R, typename N, typename OutIt, typename UnaryPredicate, typename P>
    auto operator()(R &&r, N n, OutIt dest, UnaryPredicate pred, P p) const
        -> std::pair<OutIt, astl::iter_of_range<R>>
    {
        return i::copy_if_n(adl::begin(r), n, dest, astl::pass_fn(pred), astl::pass_fn(p));
    }
} copy_if_n{};

inline constexpr struct {
    template <typename R, typename N, typename OutIt>
    // requires R InputIterator range
    // requires N integral type
    // requires OutIt OutputIterator
    auto operator()(R &&r, N n, OutIt dest) const -> std::pair<OutIt, astl::iter_of_range<R>>
    {
        return i::copy_n(adl::begin(r), n, dest);
    }
} copy_n{};

inline constexpr struct {
    template <typename R, typename OutIt, typename T>
    // requires R InputIterator range
    // requires OutIt OutputIterator
    // requires T equality comparable with value_type(R)
    auto operator()(R &&r, OutIt dest, T const &val) const
        -> std::pair<OutIt, astl::iter_of_range<R>>
    {
        return i::copy_until_sentinel(adl::begin(r), adl::end(r), dest, val);
    }

    template <typename R, typename OutIt, typename T, typename P>
    auto operator()(R &&r, OutIt dest, T const &val, P p) const
        -> std::pair<OutIt, astl::iter_of_range<R>>
    {
        return i::copy_until_sentinel(adl::begin(r), adl::end(r), dest, val, astl::pass_fn(p));
    }
} copy_until_sentinel{};

inline constexpr struct {
    template <typename R, typename N, typename OutIt, typename T>
    // requires R InputIterator range
    // requires N integral type
    // requires OutIt OutputIterator
    // requires T equality comparable with value_type(R)
    auto operator()(R &&r, N n, OutIt dest, T const &val) const
        -> std::pair<OutIt, astl::iter_of_range<R>>
    {
        return i::copy_until_sentinel_n(adl::begin(r), n, dest, val);
    }

    template <typename R, typename N, typename OutIt, typename T, typename P>
    auto operator()(R &&r, N n, OutIt dest, T const &val, P p) const
        -> std::pair<OutIt, astl::iter_of_range<R>>
    {
        return i::copy_until_sentinel_n(adl::begin(r), n, dest, val, astl::pass_fn(p));
    }
} copy_until_sentinel_n{};

inline constexpr struct {
    template <typename R, typename OutIt, typename UnaryPredicate>
    // requires R InputIterator range
    // requires OutIt OutputIterator
    // requires UnaryPredicate, returns bool, argument type value_type(R)
    auto operator()(R &&r, OutIt dest, UnaryPredicate pred) const
        -> std::pair<astl::iter_of_range<R>, OutIt>
    {
        return i::copy_until(adl::begin(r), adl::end(r), dest, astl::pass_fn(pred));
    }

    template <typename R, typename OutIt, typename UnaryPredicate, typename P>
    auto operator()(R &&r, OutIt dest, UnaryPredicate pred, P p) const
        -> std::pair<astl::iter_of_range<R>, OutIt>
    {
        return i::copy_until(adl::begin(r), adl::end(r), dest, astl::pass_fn(pred),
                             astl::pass_fn(p));
    }
} copy_until{};

inline constexpr struct {
    template <typename R, typename OutIt, typename BinaryPredicate>
    // requires R InputIterator range
    // requires OutIt OutputIterator
    // requires BinaryPredicate, returns bool, two arguments of value_type(R)
    auto operator()(R &&r, OutIt dest, BinaryPredicate pred) const
        -> std::pair<OutIt, astl::iter_of_range<R>>
    {
        return i::copy_until_adjacent_check(adl::begin(r), adl::end(r), dest, astl::pass_fn(pred));
    }

    template <typename R, typename OutIt, typename BinaryPredicate, typename P>
    auto operator()(R &&r, OutIt dest, BinaryPredicate pred, P p) const
        -> std::pair<OutIt, astl::iter_of_range<R>>
    {
        return i::copy_until_adjacent_check(adl::begin(r), adl::end(r), dest, astl::pass_fn(pred),
                                            astl::pass_fn(p));
    }
} copy_until_adjacent_check{};

inline constexpr struct {
    template <typename R, typename N, typename OutIt, typename BinaryPredicate>
    // requires R InputIterator range
    // requires N integral type
    // requires OutIt OutputIterator
    // requires BinaryPredicate, returns bool, two arguments of value_type(R)
    auto operator()(R &&r, N n, OutIt dest, BinaryPredicate pred) const -> OutIt
    {
        return i::copy_until_adjacent_check_n(adl::begin(r), n, dest, astl::pass_fn(pred));
    }

    template <typename R, typename N, typename OutIt, typename BinaryPredicate, typename P>
    auto operator()(R &&r, N n, OutIt dest, BinaryPredicate pred, P p) const -> OutIt
    {
        return i::copy_until_adjacent_check_n(adl::begin(r), n, dest, astl::pass_fn(pred),
                                              astl::pass_fn(p));
    }
} copy_until_adjacent_check_n{};

inline constexpr struct {
    template <typename R, typename N, typename OutIt, typename UnaryPredicate>
    // requires R InputIterator range
    // requires N integral type
    // requires OutIt OutputIterator
    // requires UnaryPredicate, returns bool, argument type value_type(R)
    auto operator()(R &&r, N n, OutIt dest, UnaryPredicate pred) const
        -> std::pair<astl::iter_of_range<R>, OutIt>
    {
        return i::copy_until_n(adl::begin(r), n, dest, astl::pass_fn(pred));
    }

    template <typename R, typename N, typename OutIt, typename UnaryPredicate, typename P>
    auto operator()(R &&r, N n, OutIt dest, UnaryPredicate pred, P p) const
        -> std::pair<astl::iter_of_range<R>, OutIt>
    {
        return i::copy_until_n(adl::begin(r), n, dest, astl::pass_fn(pred), astl::pass_fn(p));
    }
} copy_until_n{};

inline constexpr struct {
    template <typename R, typename OutIt, typename Comparator = std::less<>>
    // requires R InputIterator range
    // requires OutIt OutputIterator
    // requires Comparator, StrictWeakOrdering on value_type(R)
    auto operator()(R &&r, OutIt dest, Comparator comp = Comparator{}) const -> OutIt
    {
        return i::copy_until_sorted(adl::begin(r), adl::end(r), dest, astl::pass_fn(comp));
    }

    template <typename R, typename OutIt, typename Compare, typename P>
    auto operator()(R &&r, OutIt dest, Compare comp, P p) const -> OutIt
    {
        return i::copy_until_sorted(adl::begin(r), adl::end(r), dest, astl::pass_fn(comp),
                                    astl::pass_fn(p));
    }
} copy_until_sorted{};

inline constexpr struct {
    template <typename R, typename N, typename OutIt, typename Comparator = std::less<>>
    // requires R InputIterator range
    // requires N integral type
    // requires OutIt OutputIterator
    // requires Comparator, StrictWeakOrdering on value_type(R)
    auto operator()(R &&r, N n, OutIt dest, Comparator comp = Comparator{}) const -> OutIt
    {
        return i::copy_until_sorted_n(adl::begin(r), n, dest, astl::pass_fn(comp));
    }

    template <typename R, typename N, typename OutIt, typename Compare, typename P>
    auto operator()(R &&r, N n, OutIt dest, Compare comp, P p) const -> OutIt
    {
        return i::copy_until_sorted_n(adl::begin(r), n, dest, astl::pass_fn(comp),
                                      astl::pass_fn(p));
    }
} copy_until_sorted_n{};

inline constexpr struct {
    template <typename R, typename OutIt, typename UnaryPredicate>
    // requires R InputIterator range
    // requires OutIt OutputIterator
    // requires UnaryPredicate, returns bool, argument type value_type(R)
    auto operator()(R &&r, OutIt dest, UnaryPredicate pred) const
        -> std::pair<astl::iter_of_range<R>, OutIt>
    {
        return i::copy_while(adl::begin(r), adl::end(r), dest, astl::pass_fn(pred));
    }

    template <typename R, typename OutIt, typename UnaryPredicate, typename P>
    auto operator()(R &&r, OutIt dest, UnaryPredicate pred, P p) const
        -> std::pair<astl::iter_of_range<R>, OutIt>
    {
        return i::copy_while(adl::begin(r), adl::end(r), dest, astl::pass_fn(pred),
                             astl::pass_fn(p));
    }
} copy_while{};

inline constexpr struct {
    template <typename R, typename N, typename OutIt, typename UnaryPredicate>
    // requires R InputIterator range
    // requires N integral type
    // requires OutIt OutputIterator
    // requires UnaryPredicate, returns bool, argument type value_type(R)
    auto operator()(R &&r, N n, OutIt dest, UnaryPredicate pred) const
        -> std::pair<astl::iter_of_range<R>, OutIt>
    {
        return i::copy_while_n(adl::begin(r), n, dest, astl::pass_fn(pred));
    }

    template <typename R, typename N, typename OutIt, typename UnaryPredicate, typename P>
    auto operator()(R &&r, N n, OutIt dest, UnaryPredicate pred, P p) const
        -> std::pair<astl::iter_of_range<R>, OutIt>
    {
        return i::copy_while_n(adl::begin(r), n, dest, astl::pass_fn(pred), astl::pass_fn(p));
    }
} copy_while_n{};

inline constexpr struct {
    template <typename R, typename OutIt>
    // requires R Bidirectional range
    // requires OutIt BidirectionalIterator
    auto operator()(R &&r, OutIt dest) const -> OutIt
    {
        return i::reverse_copy_backward(adl::begin(r), adl::end(r), dest);
    }
} reverse_copy_backward{};

} // namespace r
} // namespace astl

#endif // ASTL_INCLUDE_COPY_HPP
