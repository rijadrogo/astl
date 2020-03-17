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
using std::iter_swap;  // NOLINT(misc-unused-using-decls)
using std::swap;       // NOLINT(misc-unused-using-decls)
using std::swap_ranges;// NOLINT(misc-unused-using-decls)

template <typename FwdIt>
// requires FwdIt ForwardIterator
auto adjacent_swap(FwdIt first, FwdIt last) -> void
{
    if constexpr (astl::is_random_access_it_v<FwdIt>) {// Random Access Iterator
        auto dist(last - first);
        while (dist > 1) {
            std::iter_swap(first, first + 1);
            first += 2;
            dist -= 2;
        }
    }
    else// Forward Iterator
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

template <typename FwdIt1, typename FwdIt2>
// requires FwdIt1 ForwardIterator
// requires FwdIt2 ForwardIterator
auto swap_ranges_bounded(FwdIt1 first, FwdIt1 last, FwdIt2 dest, FwdIt2 result_last)
    -> std::pair<FwdIt2, FwdIt1>
{
    if constexpr (is_random_access_it_v<FwdIt1, FwdIt2>) {// Random Access Iterators
        using RandIt1 = FwdIt1;
        using RandIt2 = FwdIt2;
        using Ct =
            typename std::common_type<iter_diff_type<RandIt1>, iter_diff_type<RandIt2>>::type;
        RandIt1 it1(first + std::min(Ct(last - first), Ct(result_last - dest)));
        RandIt2 it2(std::swap_ranges(first, it1, dest));
        return std::make_pair(it1, it2);
    }
    else// Forward Iterators
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

template <typename FwdIt1, typename N1, typename FwdIt2, typename N2>
// requires FwdIt1 ForwardIterator
// requires N1 integral type
// requires FwdIt2 ForwardIterator
// requires N2 integral type
auto swap_ranges_bounded_n(FwdIt1 first, N1 n1, FwdIt2 d_first, N2 n2) -> std::pair<FwdIt2, FwdIt1>
{
    if constexpr (is_random_access_it_v<FwdIt1, FwdIt2>) {// Random Access Iterators
        return i::swap_ranges_bounded(first, first + n1, d_first, d_first + n2);
    }
    else {// Forward Iterators
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

template <typename FwdIt1, typename N, typename FwdIt2>
// requires FwdIt1 ForwardIterator
// requires N integral type
// requires FwdIt2 ForwardIterator
auto swap_ranges_n(FwdIt1 first1, N n, FwdIt2 first2) -> std::pair<FwdIt2, FwdIt1>
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

template <typename BidiIt, typename FwdIt>
// requires BidiIt BidirectionalIterator
// requires FwdIt ForwardIterator
auto reverse_swap_ranges(BidiIt first1, BidiIt last1, FwdIt first2) -> FwdIt
{
    while (first1 != last1) std::iter_swap(--last1, first2++);

    return first2;
}

}// namespace i

namespace r
{
template <typename R>
// requires R ForwardIterator range
auto adjacent_swap(R &&r) -> void
{
    i::adjacent_swap(adl::begin(r), adl::end(r));
}

template <typename R1, typename R2>
// requires R1 ForwardIterator range
// requires R2 ForwardIterator range
auto swap_ranges_bounded(R1 &&r1, R2 &&r2)
    -> std::pair<astl::iter_of_range<R2>, astl::iter_of_range<R1>>
{
    return i::swap_ranges_bounded(adl::begin(r1), adl::end(r1), adl::begin(r2), adl::end(r2));
}

template <typename R1, typename N1, typename R2, typename N2>
// requires R1 ForwardIterator range
// requires N1 integral type
// requires R2 ForwardIterator range
// requires N2 integral type
auto swap_ranges_bounded_n(R1 &&r1, N1 n1, R2 &&r2, N2 n2)
    -> std::pair<astl::iter_of_range<R2>, astl::iter_of_range<R1>>
{
    return i::swap_ranges_bounded_n(adl::begin(r1), n1, adl::begin(r2), n2);
}

template <typename R1, typename R2>
// requires R1 ForwardIterator range
// requires R2 ForwardIterator range
auto swap_ranges(R1 &&r1, R2 &&r2) -> iter_of_range<R2>
{
    return std::swap_ranges(adl::begin(r1), adl::end(r1), adl::begin(r2));
}

template <typename R1, typename N, typename R2>
// requires R1 ForwardIterator range
// requires N integral type
// requires R2 ForwardIterator range
auto swap_ranges_n(R1 &&r1, N n, R2 &&r2)
    -> std::pair<astl::iter_of_range<R2>, astl::iter_of_range<R1>>
{
    return i::swap_ranges_n(adl::begin(r1), n, adl::begin(r2));
}

template <typename R1, typename R2>
// requires R1 BidirectionalIterator range
// requires R2 ForwardIterator range
auto reverse_swap_ranges(R1 &&r1, R2 &&r2) -> iter_of_range<R2>
{
    return i::reverse_swap_ranges(adl::begin(r1), adl::end(r1), adl::begin(r2));
}

}// namespace r
}// namespace astl

#endif// ASTL_INCLUDE_SWAP_HPP
