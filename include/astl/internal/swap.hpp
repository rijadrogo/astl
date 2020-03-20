//
// Created by Rijad on 30-Jul-18.
//

#ifndef ASTL_INCLUDE_SWAP_HPP
#define ASTL_INCLUDE_SWAP_HPP

#include <algorithm>
#include <type_traits>
#include <utility>

#include "astl/iterator.hpp"
#include "astl/range_access.hpp"

namespace astl
{

namespace i
{

inline constexpr struct {
    template <typename InputIterator1, typename InputIterator2>
    auto operator()(InputIterator1 first1, InputIterator2 last1, InputIterator2 first2) const
    {
        return std::swap_ranges(first1, last1, first2);
    }

} swap_ranges{};

inline constexpr struct {
    template <typename FwdIt>
    // requires FwdIt ForwardIterator
    auto operator()(FwdIt first, FwdIt last) const -> void
    {
        if constexpr (astl::is_random_access_it_v<FwdIt>) { // Random Access Iterator
            auto dist(last - first);
            while (dist > 1) {
                std::iter_swap(first, first + 1);
                first += 2;
                dist -= 2;
            }
        }
        else // Forward Iterator
        {
            while (true) {
                if (first == last) return;

                FwdIt it(astl::next(first));
                if (it == last) return;

                std::iter_swap(first, it);
                first = astl::next(it);
            }
        }
    }
} adjacent_swap{};

inline constexpr struct {
    template <typename FwdIt1, typename FwdIt2>
    // requires FwdIt1 ForwardIterator
    // requires FwdIt2 ForwardIterator
    auto operator()(FwdIt1 first, FwdIt1 last, FwdIt2 dest, FwdIt2 result_last) const
        -> std::pair<FwdIt2, FwdIt1>
    {
        if constexpr (is_random_access_it_v<FwdIt1, FwdIt2>) { // Random Access Iterators
            using RandIt1 = FwdIt1;
            using RandIt2 = FwdIt2;
            using Ct =
                typename std::common_type<iter_diff_type<RandIt1>, iter_diff_type<RandIt2>>::type;
            RandIt1 it1(first + std::min(Ct(last - first), Ct(result_last - dest)));
            RandIt2 it2(std::swap_ranges(first, it1, dest));
            return std::make_pair(it1, it2);
        }
        else // Forward Iterators
        {
            while (first != last && dest != result_last) {
                using std::swap;
                swap(*dest, *first);
                ++first;
                ++dest;
            }
            return std::make_pair(dest, first);
        }
    }
} swap_ranges_bounded{};

inline constexpr struct {
    template <typename FwdIt1, typename N1, typename FwdIt2, typename N2>
    // requires FwdIt1 ForwardIterator
    // requires N1 integral type
    // requires FwdIt2 ForwardIterator
    // requires N2 integral type
    auto operator()(FwdIt1 first, N1 n1, FwdIt2 d_first, N2 n2) const -> std::pair<FwdIt2, FwdIt1>
    {
        if constexpr (is_random_access_it_v<FwdIt1, FwdIt2>) { // Random Access Iterators
            return i::swap_ranges_bounded(first, first + n1, d_first, d_first + n2);
        }
        else { // Forward Iterators
            using Ct = typename std::common_type<N1, N2>::type;
            Ct min_of(std::min(Ct(n1), Ct(n2)));
            while (min_of != Ct(0)) {
                *d_first = *first;
                ++first;
                ++d_first;
                --min_of;
            }
            return std::make_pair(d_first, first);
        }
    }
} swap_ranges_bounded_n{};

inline constexpr struct {
    template <typename FwdIt1, typename N, typename FwdIt2>
    // requires FwdIt1 ForwardIterator
    // requires N integral type
    // requires FwdIt2 ForwardIterator
    auto operator()(FwdIt1 first1, N n, FwdIt2 first2) const -> std::pair<FwdIt2, FwdIt1>
    {
        while (n != N(0)) {
            using std::swap;
            swap(*first1, *first2);
            ++first1;
            ++first2;
            --n;
        }
        return std::make_pair(first2, first1);
    }
} swap_ranges_n{};

inline constexpr struct {
    template <typename BidiIt, typename FwdIt>
    // requires BidiIt BidirectionalIterator
    // requires FwdIt ForwardIterator
    auto operator()(BidiIt first1, BidiIt last1, FwdIt first2) const -> FwdIt
    {
        while (first1 != last1) std::iter_swap(--last1, first2++);

        return first2;
    }
} reverse_swap_ranges{};

} // namespace i

namespace r
{

inline constexpr struct {
    template <typename R>
    // requires R ForwardIterator range
    auto operator()(R &&r) const -> void
    {
        i::adjacent_swap(adl::begin(r), adl::end(r));
    }
} adjacent_swap{};

inline constexpr struct {
    template <typename R1, typename R2>
    // requires R1 ForwardIterator range
    // requires R2 ForwardIterator range
    auto operator()(R1 &&r1, R2 &&r2) const
        -> std::pair<astl::iter_of_range<R2>, astl::iter_of_range<R1>>
    {
        return i::swap_ranges_bounded(adl::begin(r1), adl::end(r1), adl::begin(r2), adl::end(r2));
    }
} swap_ranges_bounded{};

inline constexpr struct {
    template <typename R1, typename N1, typename R2, typename N2>
    // requires R1 ForwardIterator range
    // requires N1 integral type
    // requires R2 ForwardIterator range
    // requires N2 integral type
    auto operator()(R1 &&r1, N1 n1, R2 &&r2, N2 n2) const
        -> std::pair<astl::iter_of_range<R2>, astl::iter_of_range<R1>>
    {
        return i::swap_ranges_bounded_n(adl::begin(r1), n1, adl::begin(r2), n2);
    }
} swap_ranges_bounded_n{};

inline constexpr struct {
    template <typename R1, typename R2>
    // requires R1 ForwardIterator range
    // requires R2 ForwardIterator range
    auto operator()(R1 &&r1, R2 &&r2) const -> iter_of_range<R2>
    {
        return std::swap_ranges(adl::begin(r1), adl::end(r1), adl::begin(r2));
    }
} swap_ranges{};

inline constexpr struct {
    template <typename R1, typename N, typename R2>
    // requires R1 ForwardIterator range
    // requires N integral type
    // requires R2 ForwardIterator range
    auto operator()(R1 &&r1, N n, R2 &&r2) const
        -> std::pair<astl::iter_of_range<R2>, astl::iter_of_range<R1>>
    {
        return i::swap_ranges_n(adl::begin(r1), n, adl::begin(r2));
    }
} swap_ranges_n{};

inline constexpr struct {
    template <typename R1, typename R2>
    // requires R1 BidirectionalIterator range
    // requires R2 ForwardIterator range
    auto operator()(R1 &&r1, R2 &&r2) const -> iter_of_range<R2>
    {
        return i::reverse_swap_ranges(adl::begin(r1), adl::end(r1), adl::begin(r2));
    }
} reverse_swap_ranges{};

} // namespace r
} // namespace astl

#endif // ASTL_INCLUDE_SWAP_HPP
