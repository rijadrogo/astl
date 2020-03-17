//
// Created by Rijad on 29-Jul-18.
//

#ifndef ASTL_INCLUDE_SEARCH_HPP
#define ASTL_INCLUDE_SEARCH_HPP

#include <algorithm>
#include <type_traits>
#include <utility>

#include "astl/functional.hpp"
#include "astl/iterator.hpp"
#include "astl/range_access.hpp"

namespace astl
{
namespace internal_search
{
template <typename FwdItHay, typename FwdItPat, typename BinaryPredicate>
// requires FwdItHay ForwardIterator
// requires FwdItPat ForwardIterator
// requires BinaryPredicate binary predicate on value_type(FwdItHay) and
// value_type(FwdItPat)
auto searchp1(FwdItHay first1, FwdItHay last1, FwdItPat first2, FwdItPat last2, BinaryPredicate pred,
              std::forward_iterator_tag) -> std::pair<FwdItHay, FwdItHay>
{
    using PairType = std::pair<FwdItHay, FwdItHay>;
    if (first2 == last2) return PairType{first1, last1};// Everything matches an empty sequence

    while (true) {
        // Find first element in sequence 1 that matches *first2, with a minimum of
        // loop checks
        while (true) {
            if (first1 == last1)
                // return last1 if no element matches *first2
                return PairType{last1, last1};

            if (pred(*first1, *first2)) break;

            ++first1;
        }
        // *first1 matches *first2, now match elements after here
        FwdItHay m1(first1);
        FwdItPat m2(first2);
        while (true) {
            if (++m2 == last2)
                // If pattern exhausted, first1 is the answer (works for 1 element
                // pattern)
                return PairType{first1, m1};

            if (++m1 == last1)
                // Otherwise if source exhausted, pattern not found
                return PairType{last1, last1};

            if (!pred(*m1, *m2))// if there is a mismatch, restart with a new first1
            {
                ++first1;
                break;
            }// else there is a match, check next elements
        }
    }
}

template <typename RandIt1, typename RandIt2, typename BinaryPredicate>
// requires FwdItHay RandomAccessIterator
// requires FwdItPat RandomAccessIterator
// requires BinaryPredicate binary predicate on value_type(FwdItHay) and
// value_type(FwdItPat)
auto searchp1(RandIt1 first1, RandIt1 last1, RandIt2 first2, RandIt2 last2, BinaryPredicate pred,
              std::random_access_iterator_tag) -> std::pair<RandIt1, RandIt1>
{
    // Take advantage of knowing source and pattern lengths. Stop short when
    // source is smaller than pattern
    using Diff1 = iter_diff_type<RandIt2>;
    using Diff2 = iter_diff_type<RandIt1>;
    using PairType = std::pair<RandIt1, RandIt1>;

    Diff1 const len2(last2 - first2);
    if (len2 == 0) return PairType{first1, first1};

    using Ct = typename std::common_type<Diff1, Diff2>::type;
    Diff2 const len1(last1 - first1);
    if (Ct(len1) < Ct(len2)) return PairType{last1, last1};

    RandIt1 const s(last1 - (len2 - Diff1(1)));// Start of pattern match can't go beyond here

    while (true) {
        while (true) {
            if (first1 == s) return PairType(last1, last1);

            if (pred(*first1, *first2)) break;

            ++first1;
        }

        RandIt1 m1(first1);
        RandIt2 m2(first2);
        while (true) {
            if (++m2 == last2) return PairType{first1, first1 + len2};

            ++m1;// no need to check range on m1 because s guarantees we have enough
                 // source
            if (!pred(*m1, *m2)) {
                ++first1;
                break;
            }
        }
    }
}

template <typename FwdItHay, typename NHay, typename FwdItPat, typename NPat, typename BinaryPredicate>
// requires FwdItHay ForwardIterator
// requires NHay integral type
// requires FwdItPat ForwardIterator
// requires NPat integral type
// requires BinaryPredicate binary predicate on value_type(FwdItHay) and
// value_type(FwdItPat)
auto search_n1(FwdItHay first1, NHay n1, FwdItPat first2, NPat n2, BinaryPredicate pred,
               std::forward_iterator_tag) -> std::pair<FwdItHay, FwdItHay>
{
    using PairType = std::pair<FwdItHay, FwdItHay>;
    if (n1 == NHay(0))
        return PairType{first1, astl::next(first1, n1)};// Everything matches an empty sequence

    while (true) {
        // Find first element in sequence 1 that match's *first2, with a minimum of
        // loop checks
        while (true) {
            if (n1 == NHay(0))
                // return last1 if no element matches *first2
                return PairType{first1, first1};

            if (pred(*first1, *first2)) break;

            ++first1;
        }
        // *first1 matches *first2, now match elements after here
        FwdItHay m1(first1);
        FwdItPat m2(first2);
        NHay l1(n1);
        NPat l2(n2);
        while (true) {
            ++m2;
            --l2;
            if (l2 == NPat(0))
                // If pattern exhausted, first1 is the answer (works for 1 element
                // pattern)
                return PairType{first1, true};

            ++m1;
            --l1;
            if (l1 == NHay(0))
                // Otherwise if source exhausted, pattern not found
                return PairType{m1, m1};

            if (!pred(*m1, *m2))// if there is a mismatch, restart with a new first1
            {
                ++first1;
                break;
            }// else there is a match, check next elements
        }
    }
}

template <typename RandIt1, typename NHay, typename RandIt2, typename NPat, typename BinaryPredicate>
// requires RandIt1 RandomAccessIterator
// requires NHay integral type
// requires RandIt2 RandomAccessIterator
// requires NPat integral type
// requires BinaryPredicate binary predicate on value_type(RandIt1) and
// value_type(RandIt2)
auto search_n1(RandIt1 first1, NHay len1, RandIt2 first2, NPat len2, BinaryPredicate pred,
               std::random_access_iterator_tag) -> std::pair<RandIt1, RandIt1>
{
    return internal_search::searchp1(first1, first1 + len1, first2, first2 + len2,
                                     astl::pass_fn(pred));
}
}// namespace internal_search
namespace i
{
template <typename FwdIt1, typename FwdIt2>
ASTL_NODISCARD auto is_subarray(FwdIt1 first1, FwdIt1 last1, FwdIt2 first2, FwdIt2 last2) -> bool
{
    return std::search(first1, last1, first2, last2) != last1;
}

template <typename FwdIt1, typename FwdIt2, typename Eqv>
ASTL_NODISCARD auto is_subarray(FwdIt1 first1, FwdIt1 last1, FwdIt2 first2, FwdIt2 last2, Eqv e)
    -> bool
{
    return std::search(first1, last1, first2, last2, astl::pass_fn(e)) != last1;
}

template <typename FwdIt1, typename FwdIt2, typename Eqv, typename P1, typename P2>
ASTL_NODISCARD auto is_subarray(FwdIt1 first1, FwdIt1 last1, FwdIt2 first2, FwdIt2 last2, Eqv e,
                                P1 p1, P2 p2) -> bool
{
    return i::is_subarray(first1, last1, first2, last2,
                          astl::lockstep(astl::pass_fn(e), astl::pass_fn(p1), astl::pass_fn(p2)));
}

template <typename FwdIt1, typename FwdIt2, typename Eqv>
ASTL_NODISCARD auto is_subseq(FwdIt1 first1, FwdIt1 last1, FwdIt2 first2, FwdIt2 last2, Eqv e)
    -> bool
{
    if (first1 == last1 || first2 == last2) return false;

    while (true) {
        auto i(std::find_if(first1, last1, astl::bind2nd(e, *first2)));
        if (i == last1) { return false; }

        first1 = astl::next(i);
        ++first2;
        if (first2 == last2) return true;
    }
}
template <typename FwdIt1, typename FwdIt2>
ASTL_NODISCARD auto is_subseq(FwdIt1 first1, FwdIt1 last1, FwdIt2 first2, FwdIt2 last2) -> bool
{
    return i::is_subseq(first1, last1, first2, last2, std::equal_to{});
}

template <typename FwdIt1, typename FwdIt2, typename Eqv, typename P1, typename P2>
ASTL_NODISCARD auto is_subseq(FwdIt1 first1, FwdIt1 last1, FwdIt2 first2, FwdIt2 last2, Eqv e,
                              P1 p1, P2 p2) -> bool
{
    return i::is_subseq(first1, last1, first2, last2,
                        astl::lockstep(astl::pass_fn(e), astl::pass_fn(p1), astl::pass_fn(p2)));
}

using std::search;// NOLINT(misc-unused-using-decls)
template <typename FwdItHay, typename FwdItPat, typename BinaryPredicate, typename P1, typename P2>
ASTL_NODISCARD auto search(FwdItHay first1, FwdItHay last1, FwdItPat s_first, FwdItPat s_last,
                           BinaryPredicate pred, P1 p1, P2 p2) -> FwdItHay
{
    return i::search(first1, last1, s_first, s_last,
                     astl::lockstep(astl::pass_fn(pred), astl::pass_fn(p1), astl::pass_fn(p2)));
}

template <typename FwdIt, typename N, typename E, typename BinaryPredicate, typename P>
ASTL_NODISCARD auto search_n(FwdIt first1, FwdIt last1, N count, E &&elem, BinaryPredicate pred, P p)
    -> FwdIt
{
    return std::search_n(first1, last1, count, elem,
                         astl::combine(astl::pass_fn(pred), astl::pass_fn(p)));
}

// same as std::search, just returns pair of iterators to the founded sequence
// inside of [first1, last1)
template <typename FwdItHay, typename FwdItPat, typename BinaryPredicate>
// requires FwdItHay ForwardIterator
// requires FwdItPat ForwardIterator
// requires BinaryPredicate binary predicate on value_type(FwdItHay) and
// value_type(FwdItPat)
ASTL_NODISCARD auto searchp(FwdItHay first1, FwdItHay last1, FwdItPat first2, FwdItPat last2,
                            BinaryPredicate pred) -> std::pair<FwdItHay, FwdItHay>
{
    return internal_search::searchp1(first1, last1, first2, last2, astl::pass_fn(pred),
                                     astl::iterator_category(first1, first2));
}

// same as std::search, just returns pair of iterators to the founded sequence
// inside of [first1, last1)
template <typename FwdItHay, typename FwdItPat>
// requires FwdItHay ForwardIterator
// requires FwdItPat ForwardIterator
ASTL_NODISCARD auto searchp(FwdItHay first1, FwdItHay last1, FwdItPat first2, FwdItPat last2)
    -> std::pair<FwdItHay, FwdItHay>
{
    return i::searchp(first1, last1, first2, last2, std::equal_to{});
}

template <typename FwdItHay, typename FwdItPat, typename BinaryPredicate, typename P>
ASTL_NODISCARD auto searchp(FwdItHay first1, FwdItHay last1, FwdItPat s_first, FwdItPat s_last,
                            BinaryPredicate pred, P p) -> std::pair<FwdItHay, FwdItHay>
{
    return i::searchp(first1, last1, s_first, s_last,
                      astl::combine(astl::pass_fn(pred), astl::pass_fn(p)));
}

// same as std::search, just returns pair of iterators to the founded sequence
// inside of [first1, last1)
template <typename FwdItHay, typename NHay, typename FwdItPat, typename NPat, typename BinaryPredicate>
// requires FwdItHay ForwardIterator
// requires NHay integral type
// requires FwdItPat ForwardIterator
// requires NPat integral type
// requires BinaryPredicate binary predicate on value_type(FwdItHay) and
// value_type(FwdItPat)
ASTL_NODISCARD auto searchp_n(FwdItHay first1, NHay n1, FwdItPat first2, NPat n2, BinaryPredicate pred)
    -> std::pair<FwdItHay, FwdItHay>
{
    return internal_search::search_n1(first1, n1, first2, n2, astl::pass_fn(pred),
                                      astl::iterator_category(first1, first2));
}

// same as std::search, just returns pair of iterators to the founded sequence
// inside of [first1, last1)
template <typename FwdItHay, typename NHay, typename FwdItPat, typename NPat>
// requires FwdItHay ForwardIterator
// requires NHay integral type
// requires FwdItPat ForwardIterator
// requires NPat integral type
ASTL_NODISCARD auto searchp_n(FwdItHay first1, NHay n1, FwdItPat first2, NPat n2)
    -> std::pair<FwdItHay, FwdItHay>
{
    return i::searchp_n(first1, n1, first2, n2, std::equal_to{});
}

template <typename FwdItHay, typename NHay, typename FwdItPat, typename NPat, typename BinaryPredicate,
          typename P>
ASTL_NODISCARD auto searchp_n(FwdItHay first1, NHay n1, FwdItPat s_first, NPat n2, BinaryPredicate pred,
                              P p) -> std::pair<FwdItHay, FwdItHay>
{
    return i::searchp(first1, n1, s_first, n2,
                      astl::combine(astl::pass_fn(pred), astl::pass_fn(p)));
}
}// namespace i

namespace r
{
template <typename R1, typename R2>
// requires R1 ForwardIterator range
// requires R2 ForwardIterator
// requires value_type(R) and value_type(FwdIt) equality comparable
ASTL_NODISCARD auto is_subarray(R1 &&r1, R2 &&r2) -> bool
{
    return i::is_subarray(adl::begin(r1), adl::end(r1), adl::begin(r2), adl::end(r2));
}

template <typename R1, typename R2, typename BinaryPredicate>
// requires R1 ForwardIterator range
// requires R2 ForwardIterator
// requires BinaryPredicate binary predicate on value_type(R) and value_type(FwdIt)
ASTL_NODISCARD auto is_subarray(R1 &&r1, R2 &&r2, BinaryPredicate pred) -> bool
{
    return i::is_subarray(adl::begin(r1), adl::end(r1), adl::begin(r2), adl::end(r2),
                          astl::pass_fn(pred));
}

template <typename R, typename R2, typename BinaryPredicate, typename P1, typename P2>
ASTL_NODISCARD auto is_subarray(R &&r1, R2 &&r2, BinaryPredicate pred, P1 p1, P2 p2) -> bool
{
    return i::is_subarray(adl::begin(r1), adl::end(r1), adl::begin(r2), adl::end(r2),
                          astl::pass_fn(pred), astl::pass_fn(p1), astl::pass_fn(p2));
}

template <typename R1, typename R2>
// requires R1 ForwardIterator range
// requires R2 ForwardIterator
// requires value_type(R) and value_type(FwdIt) equality comparable
ASTL_NODISCARD auto is_subseq(R1 &&r1, R2 &&r2) -> bool
{
    return i::is_subseq(adl::begin(r1), adl::end(r1), adl::begin(r2), adl::end(r2));
}

template <typename R1, typename R2, typename BinaryPredicate>
// requires R1 ForwardIterator range
// requires R2 ForwardIterator
// requires BinaryPredicate binary predicate on value_type(R) and value_type(FwdIt)
ASTL_NODISCARD auto is_subseq(R1 &&r1, R2 &&r2, BinaryPredicate pred) -> bool
{
    return i::is_subseq(adl::begin(r1), adl::end(r1), adl::begin(r2), adl::end(r2),
                        astl::pass_fn(pred));
}

template <typename R, typename R2, typename BinaryPredicate, typename P1, typename P2>
ASTL_NODISCARD auto is_subseq(R &&r1, R2 &&r2, BinaryPredicate pred, P1 p1, P2 p2) -> bool
{
    return i::is_subseq(adl::begin(r1), adl::end(r1), adl::begin(r2), adl::end(r2),
                        astl::pass_fn(pred), astl::pass_fn(p1), astl::pass_fn(p2));
}

template <typename R1, typename R2>
// requires R2 ForwardIterator range
// requires R2 ForwardIterator
// requires value_type(R) and value_type(FwdIt) equality comparable
ASTL_NODISCARD auto search(R1 &&r1, R2 &&r2) -> iter_of_range<R1>
{
    return i::search(adl::begin(r1), adl::end(r1), adl::begin(r2), adl::end(r2));
}

template <typename R1, typename R2, typename BinaryPredicate>
// requires R1 ForwardIterator range
// requires R2 ForwardIterator
// requires BinaryPredicate binary predicate on value_type(R) and value_type(FwdIt)
ASTL_NODISCARD auto search(R1 &&r1, R2 &&r2, BinaryPredicate pred) -> iter_of_range<R1>
{
    return i::search(adl::begin(r1), adl::end(r1), adl::begin(r2), adl::end(r2),
                     astl::pass_fn(pred));
}

template <typename R1, typename R2, typename BinaryPredicate, typename P1, typename P2>
ASTL_NODISCARD auto search(R1 &&r1, R2 &&r2, BinaryPredicate pred, P1 p1, P2 p2) -> iter_of_range<R1>
{
    return i::search(adl::begin(r1), adl::end(r1), adl::begin(r2), adl::end(r2),
                     astl::pass_fn(pred), astl::pass_fn(p1), astl::pass_fn(p2));
}

template <typename R, typename N, typename E>
// requires R ForwardIterator range
// requires S integral type
// requires E equality comparable with value_type(R)
ASTL_NODISCARD auto search_n(R &&r, N count, E &&elem) -> iter_of_range<R>
{
    return i::search_n(adl::begin(r), adl::end(r), count, elem);
}

template <typename R, typename N, typename E, typename BinaryPredicate>
// requires R ForwardIterator range
// requires S integral type
// requires E comparable with value_type(R) via BinaryPredicate
// requires BinaryPredicate, returns bool, arguments two value_type(R)
ASTL_NODISCARD auto search_n(R &&r, N count, E &&elem, BinaryPredicate pred) -> iter_of_range<R>
{
    return i::search_n(adl::begin(r), adl::end(r), count, elem, astl::pass_fn(pred));
}

template <typename R, typename N, typename E, typename BinaryPredicate, typename P>
ASTL_NODISCARD auto search_n(R &&r, N count, E &&elem, BinaryPredicate pred, P p) -> iter_of_range<R>
{
    return i::search_n(adl::begin(r), adl::end(r), count, elem, astl::pass_fn(pred),
                       astl::pass_fn(p));
}

template <typename R, typename FwdIt, typename BinaryPredicate>
// requires R ForwardIterator range
// requires FwdIt ForwardIterator
// requires BinaryPredicate binary predicate on value_type(R) and value_type(FwdIt)
ASTL_NODISCARD auto searchp(R &&r1, FwdIt s_first, FwdIt s_last, BinaryPredicate pred)
    -> std::pair<astl::iter_of_range<R>, astl::iter_of_range<R>>
{
    return i::searchp(adl::begin(r1), adl::end(r1), s_first, s_last, astl::pass_fn(pred));
}

template <typename R, typename FwdIt>
// requires R ForwardIterator range
// requires FwdIt ForwardIterator
ASTL_NODISCARD auto searchp(R &&r1, FwdIt s_first, FwdIt s_last)
    -> std::pair<astl::iter_of_range<R>, astl::iter_of_range<R>>
{
    return r::searchp(r1, s_first, s_last, std::equal_to{});
}

template <typename R, typename FwdIt, typename BinaryPredicate, typename P>
ASTL_NODISCARD auto searchp(R &&r1, FwdIt s_first, FwdIt s_last, BinaryPredicate pred, P p)
    -> std::pair<astl::iter_of_range<R>, astl::iter_of_range<R>>
{
    return i::searchp(adl::begin(r1), adl::end(r1), s_first, s_last, astl::pass_fn(pred),
                      astl::pass_fn(p));
}

template <typename R, typename N1, typename FwdIt, typename N2, typename BinaryPredicate>
// requires R ForwardIterator range
// requires N1 integral type
// requires FwdIt ForwardIterator
// requires N2 integral type
// requires BinaryPredicate binary predicate on value_type(R) and value_type(FwdIt)
auto searchp_n(R &&r1, N1 n1, FwdIt s_first, N2 n2, BinaryPredicate pred)
    -> std::pair<astl::iter_of_range<R>, astl::iter_of_range<R>>
{
    return i::searchp_n(adl::begin(r1), n1, s_first, n2, astl::pass_fn(pred));
}

template <typename R, typename N1, typename FwdIt, typename N2>
// requires R ForwardIterator range
// requires N1 integral type
// requires FwdIt ForwardIterator
// requires N2 integral type
ASTL_NODISCARD auto searchp_n(R &&r1, N1 n1, FwdIt s_first, N2 n2)
    -> std::pair<astl::iter_of_range<R>, astl::iter_of_range<R>>
{
    return r::searchp_n(r1, n1, s_first, n2, std::equal_to{});
}

template <typename R, typename N1, typename FwdIt, typename N2, typename BinaryPredicate, typename P>
ASTL_NODISCARD auto searchp_n(R &&r1, N1 n1, FwdIt s_first, N2 n2, BinaryPredicate pred, P p)
    -> std::pair<astl::iter_of_range<R>, astl::iter_of_range<R>>
{
    return i::searchp_n(adl::begin(r1), n1, s_first, n2, astl::pass_fn(pred), astl::pass_fn(p));
}
}// namespace r
}// namespace astl

#endif// ASTL_INCLUDE_SEARCH_HPP
