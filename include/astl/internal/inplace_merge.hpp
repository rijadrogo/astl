//
// Created by Rijad on 29-Jul-18.
//

#ifndef ASTL_INCLUDE_INPLACE_MERGE_HPP
#define ASTL_INCLUDE_INPLACE_MERGE_HPP

#include <algorithm>
#include <utility>

#include "lower_bound.hpp"
#include "rotate.hpp"
#include "upper_bound.hpp"

#include "astl/functional.hpp"
#include "astl/iterator.hpp"
#include "astl/range_access.hpp"
#include "astl/temporary_buffer.hpp"

// TODO merge_inplace_n with buffer

namespace astl
{
namespace i
{
inline constexpr struct {
    template <typename FwdIt1, typename FwdIt2, typename OutIt, typename Comparator = std::less<>>
    // requires FwdIt1 ForwardIterator
    // requires FwdIt2 ForwardIterator
    // requires OutIt ForwardIterator
    // requires Comparator StrictWeakOrdering, arguments value_type(FwdIt1) and
    // value_type(FwdIt2)
    auto operator()(FwdIt1 first1, FwdIt1 last1, FwdIt2 first2, FwdIt2 last2, OutIt dest,
                    Comparator comp = Comparator{}) const -> OutIt
    {
        // precondition is_sorted(first1, last1, comp)
        // precondition is_sorted(first2, last2, comp)
        auto c(astl::pass_fn(comp));
        while (first1 != last1 && first2 != last2) {
            if (comp(*first2, *first1)) {
                FwdIt2 it(std::lower_bound(first2, last2, *first1, c));
                dest = std::move(first2, it, dest);
                first2 = it;
            }
            else {
                FwdIt1 it(std::upper_bound(first1, last1, *first2, c));
                dest = std::move(first1, it, dest);
                first1 = it;
            }
        }
        return std::move(first2, last2, std::move(first1, last1, dest));
    }

    template <typename FwdIt1, typename FwdIt2, typename OutIt, typename Comparator, typename P1,
              typename P2>
    auto operator()(FwdIt1 first1, FwdIt1 last1, FwdIt2 first2, FwdIt2 last2, OutIt dest,
                    Comparator comp, P1 p1, P2 p2) const -> OutIt
    {
        auto proj1(astl::pass_fn(p1));
        auto proj2(astl::pass_fn(p2));
        return (*this)(astl::map_iterator(first1, proj1), astl::map_iterator(last1, proj1),
                       astl::map_iterator(first2, proj2), astl::map_iterator(last2, proj2), dest,
                       astl::pass_fn(comp));
    }
} merge_move{};
} // namespace i

namespace internal_in_merge
{
template <typename FwdIt, typename Distance, typename Comparator>
// requires FwdIt ForwardIterator
// requires Distance integral type
// requires Comparator, return bool, two arguments of value_type(FwdIt)
auto merge_without_buffer(FwdIt first, FwdIt mid, FwdIt last, Distance len1, Distance len2,
                          Comparator comp) -> void
{
    // precondition distance(first, mid) == len1
    // precondition distance(mid, last) == len2
    // precondition is_sorted(f1rst, mid, comp)
    // precondition is_sorted(last, mid, comp)
    if (len1 == 0 || len2 == 0) return;

    if (len1 + len2 == 2) {
        if (comp(*mid, *first)) std::iter_swap(first, mid);

        return;
    }
    FwdIt first_cut(first);
    FwdIt second_cut(mid);
    Distance len11(0);
    Distance len22(0);

    if (len1 > len2) {
        len11 = len1 >> 1;
        std::advance(first_cut, len11);
        second_cut = std::lower_bound(mid, last, *first_cut, comp);
        len22 = std::distance(mid, second_cut);
    }
    else {
        len22 = len2 >> 1;
        std::advance(second_cut, len22);
        first_cut = std::upper_bound(first, mid, *second_cut, comp);
        len11 = std::distance(first, first_cut);
    }
    FwdIt new_middle(std::rotate(first_cut, mid, second_cut));

    internal_in_merge::merge_without_buffer(first, first_cut, new_middle, len11, len22, comp);

    internal_in_merge::merge_without_buffer(new_middle, second_cut, last, len1 - len11,
                                            len2 - len22, comp);
}
} // namespace internal_in_merge

namespace internal_merge
{
template <typename FwdIt, typename N, typename Comparator, typename B>
// requires FwdIt is ForwardIterator
// requires N is integral type
// requires Comparator StrictWeakOrdering on the value_type (FwdIt)
// requires B ForwardIterator
auto mer_inp_lhs_sprob_buff(FwdIt f0, N n0, FwdIt f1, N n1, FwdIt &f0_0, N &n0_0, FwdIt &f0_1,
                            N &n0_1, FwdIt &f1_0, N &n1_0, FwdIt &f1_1, N &n1_1, Comparator comp,
                            B buff, N buffer_size) -> void
{
    // precondition distance(f0, f1) == n0
    // precondition is_sorted_n(f0, n0, comp)
    // precondition is_sorted(f1, n1, comp)
    // precondition n0 > 0
    // precondition n1 > 0
    f0_0 = f0;
    n0_0 = n0 >> 1;
    f0_1 = astl::next(f0, n0_0);
    f1_1 = i::lower_bound_n(f1, n1, *f0_1, comp).first;
    f1_0 = i::rotate_adaptive(f0_1, f1, f1_1, buff, buffer_size);
    n0_1 = std::distance(f0_1, f1_0);
    ++f1_0;
    n1_0 = (n0 - n0_0) - 1;
    n1_1 = n1 - n0_1;
}

template <typename FwdIt, typename N, typename Comparator>
// requires FwdIt is ForwardIterator
// requires N is integral type
// requires Comparator is StrictWeakOrdering on the value_type (FwdIt)
auto merge_inp_left_sprob(FwdIt f0, N n0, FwdIt f1, N n1, FwdIt &f0_0, N &n0_0, FwdIt &f0_1,
                          N &n0_1, FwdIt &f1_0, N &n1_0, FwdIt &f1_1, N &n1_1, Comparator comp)
    -> void
{
    // precondition distance(f0, f1) == n0
    // precondition is_sorted_n(f0, n0, comp)
    // precondition is_sorted(f1, n1, comp)
    // precondition n0 > 0
    // precondition n1 > 0
    f0_0 = f0;
    n0_0 = n0 >> 1;
    f0_1 = astl::next(f0, n0_0);
    f1_1 = i::lower_bound_n(f1, n1, *f0_1, comp).first;
    f1_0 = std::rotate(f0_1, f1, f1_1);
    n0_1 = std::distance(f0_1, f1_0);
    ++f1_0;
    n1_0 = (n0 - n0_0) - 1;
    n1_1 = n1 - n0_1;
}

template <typename FwdIt, typename N, typename Comparator, typename B>
// requires FwdIt is ForwardIterator
// requires N is integral type
// requires Comparator is StrictWeakOrdering on the value_type (FwdIt)
// requires B ForwardIterator
auto mer_inp_rhs_sprob_buff(FwdIt f0, N n0, FwdIt f1, N n1, FwdIt &f0_0, N &n0_0, FwdIt &f0_1,
                            N &n0_1, FwdIt &f1_0, N &n1_0, FwdIt &f1_1, N &n1_1, Comparator comp,
                            B buff, N n) -> void
{
    // precondition distance(f0, f1) == n0
    // precondition is_sorted_n(f0, n0, comp)
    // precondition  is_sorted(f1, n1, comp)
    // precondition n0 > 0
    // precondition n1 > 0
    f0_0 = f0;
    n0_1 = n1 >> 1;
    f1_1 = astl::next(f1, n0_1);
    f0_1 = i::upper_bound_n(f0, n0, *f1_1, comp).first;
    ++f1_1;
    f1_0 = i::rotate_adaptive(f0_1, f1, f1_1, buff, n);
    n0_0 = std::distance(f0_0, f0_1);
    n1_0 = n0 - n0_0;
    n1_1 = (n1 - n0_1) - 1;
}

template <typename FwdIt, typename N, typename Comparator>
// requires FwdIt is ForwardIterator
// requires N integral type
// requires Comparator  StrictWeakOrdering on the value_type(FwdIt)
auto merge_inp_right_sprob(FwdIt f0, N n0, FwdIt f1, N n1, FwdIt &f0_0, N &n0_0, FwdIt &f0_1,
                           N &n0_1, FwdIt &f1_0, N &n1_0, FwdIt &f1_1, N &n1_1, Comparator comp)
    -> void
{
    // precondition distance(f0, f1) == n0
    // precondition is_sorted_n(f0, n0, comp)
    // precondition is_sorted_n(f1, n1, comp)
    // precondition n0 > 0
    // precondition n1 > 0
    f0_0 = f0;
    n0_1 = n1 >> 1;
    f1_1 = astl::next(f1, n0_1);
    f0_1 = i::upper_bound_n(f0, n0, *f1_1, comp).first;
    ++f1_1;
    f1_0 = std::rotate(f0_1, f1, f1_1);
    n0_0 = std::distance(f0_0, f0_1);
    n1_0 = n0 - n0_0;
    n1_1 = (n1 - n0_1) - 1;
}

} // namespace internal_merge

namespace i
{
inline constexpr struct {
    template <typename FwdIt, typename Comparator = std::less<>>
    // requires FwdIt is ForwardIterator
    // requires Comparator  StrictWeakOrdering on the value_type(FwdIt)
    auto operator()(FwdIt first, FwdIt mid, FwdIt last, Comparator comp = Comparator{}) const
        -> void
    {
        // precondition first <= mid <= last
        // precondition is_sorted(first, mid)
        // precondition is_sorted(mid, last)
        if (first == mid || mid == last) return;

        if constexpr (is_bidirectional_it_v<FwdIt>) { // Bidirectional Iterator
            std::inplace_merge(first, mid, last, astl::pass_fn(comp));
        }
        else { // Forward Iterator
            auto c(astl::pass_fn(comp));
            iter_diff_type<FwdIt> lhs(std::distance(first, mid));

            using T = iter_value_type<FwdIt>;
            inline_temporary_buffer<T> buffer(lhs, *first);

            if (lhs < buffer.size()) {
                auto buffer_end(std::move(first, mid, buffer.begin));
                i::merge_move(buffer, buffer_end, mid, last, first, c);
            }
            internal_in_merge::merge_without_buffer(first, mid, last, lhs, std::distance(mid, last),
                                                    c);
        }
    }

    template <typename FwdIt, typename Comparator, typename P>
    auto operator()(FwdIt first, FwdIt mid, FwdIt last, Comparator comp, P p) const -> void
    {
        // precondition first <= mid <= last
        // precondition is_sorted(first, mid)
        // precondition is_sorted(mid, last)
        (*this)(first, mid, last, astl::combine(astl::pass_fn(comp), astl::pass_fn(p)));
    }
} merge_inplace{};

inline constexpr struct {
    // complexity O(N log N) comparisons.
    // No allocations
    template <typename FwdIt, typename Comparator = std::less<>>
    // requires FwdIt ForwardIterator
    // requires Distance integral type
    // requires Comparator function returns bool, two arguments of value_type(FwdIt)
    auto operator()(FwdIt first, FwdIt mid, FwdIt last, Comparator comp) const -> void
    {
        // precondition first <= mid <= last
        // precondition is_sorted(first, mid, comp)
        // precondition is_sorted(mid, last, comp)
        if (first == mid || mid == last) return;

        internal_in_merge::merge_without_buffer(first, mid, last, std::distance(first, mid),
                                                std::distance(mid, last), astl::pass_fn(comp));
    }

    // complexity O(N log N) comparisons.
    // No allocations
    template <typename FwdIt, typename Comparator, typename P>
    auto operator()(FwdIt first, FwdIt mid, FwdIt last, Comparator comp, P p) const -> void
    {
        (*this)(first, mid, last, astl::combine(astl::pass_fn(comp), astl::pass_fn(p)));
    }
} merge_inplace_no_buffer{};

inline constexpr struct {
    // No allocations
    template <typename FwdIt, typename N, typename Comparator>
    // requires FwdIt ForwardIterator
    // requires N integral type
    // requires Comparator StrictWeakOrdering on the value_type (FwdIt)
    auto operator()(FwdIt first1, N size1, FwdIt first2, N size2, Comparator comp) const -> void
    {
        // precondition distance(first1, first2) == size1
        // precondition is_sorted_n(first1, size1, comp)
        // precondition is_sorted_n(first2, size2, comp)
        if (!size1 || !size2) return;

        FwdIt f0_0;
        FwdIt f0_1;
        FwdIt f1_0;
        FwdIt f1_1;
        N n0_0;
        N n0_1;
        N n1_0;
        N n1_1;
        auto c(astl::pass_fn(comp));
        if (size1 < size2)
            internal_merge::merge_inp_left_sprob(first1, size1, first2, size2, f0_0, n0_0, f0_1,
                                                 n0_1, f1_0, n1_0, f1_1, n1_1, c);

        else
            internal_merge::merge_inp_right_sprob(first1, size1, first2, size2, f0_0, n0_0, f0_1,
                                                  n0_1, f1_0, n1_0, f1_1, n1_1, c);

        (*this)(f0_0, n0_0, f0_1, n0_1, c);
        (*this)(f1_0, n1_0, f1_1, n1_1, c);
    }

    template <typename FwdIt, typename N, typename Comparator, typename P>
    auto operator()(FwdIt first1, N size1, FwdIt first2, N size2, Comparator comp, P p) const
        -> void
    {
        // precondition distance(first1, first2) == size1
        // precondition is_sorted_n(first1, size1, comp)
        // precondition is_sorted_n(first2, size2, comp)
        (*this)(first1, size1, first2, size2, astl::combine(astl::pass_fn(comp), astl::pass_fn(p)));
    }
} merge_inplace_n_no_buffer{};

} // namespace i

namespace r
{
inline constexpr struct {
    template <typename R, typename BidiIt, typename Comparator = std::less<>>
    // requires R BidirectionalIterator range
    // requires BidiIt BidirectionalIterator
    // requires Comparator StrictWeakOrdering on the value_type (FwdIt)
    auto inplace_merge(R &&r, BidiIt mid, Comparator comp = Comparator{}) -> void
    {
        // precondition r.begin() <= mid <= r.end()
        // precondition is_sorted(r.begin(), mid, comp);
        // precondition is_sorted(mid, r.end(), comp);
        i::merge_inplace(adl::begin(r), mid, adl::end(r), astl::pass_fn(comp));
    }

    template <typename R, typename BidiIt, typename Comparator, typename P>
    auto inplace_merge(R &&r, BidiIt mid, Comparator comp, P p) -> void
    {
        i::merge_inplace(adl::begin(r), adl::end(r), mid, astl::pass_fn(comp), astl::pass_fn(p));
    }
} merge_inplace{};

inline constexpr struct {
    // complexity O(N log N) comparisons.
    template <typename R, typename FwdIt, typename Comparator = std::less<>>
    // requires R ForwardIterator range
    // requires FwdIt ForwardIterator
    // requires Comparator StrictWeakOrdering on the value_type (FwdIt)
    auto operator()(R &&r, FwdIt mid, Comparator comp) const -> void
    {
        // precondition r.begin() <= mid <= r.end()
        // precondition is_sorted(r.begin(), mid, comp);
        // precondition is_sorted(mid, r.end(), comp);
        (*this)(adl::begin(r), adl::end(r), mid, astl::pass_fn(comp));
    }

    // complexity O(N log N) comparisons.
    template <typename R, typename FwdIt, typename Comparator, typename P>
    auto operator()(R &&r, FwdIt mid, Comparator comp, P p) const -> void
    {
        // precondition r.begin() <= mid <= r.end()
        // precondition is_sorted(r.begin(), mid, comp);
        // precondition is_sorted(mid, r.end(), comp);
        (*this)(adl::begin(r), adl::end(r), mid, astl::pass_fn(comp), astl::pass_fn(p));
    }
} merge_inplace_no_buffer{};

inline constexpr struct {
    template <typename R1, typename R2, typename OutIt, typename Comparator = std::less<>>
    // requires R ForwardIterator range
    // requires OutIt OutputIterator
    // requires Comparator StrictWeakOrdering on the value_type (R)
    auto operator()(R1 &&r1, R2 &&r2, OutIt dest, Comparator comp) const -> OutIt
    {
        // precondition is_sorted(r1.begin(), r1.end(), comp);
        // precondition is_sorted(r2.begin(), r2.end(), comp);
        return i::merge_move(adl::begin(r1), adl::end(r1), adl::begin(r2), adl::end(r2), dest,
                             astl::pass_fn(comp));
    }

    template <typename R1, typename R2, typename OutIt, typename Comparator, typename P1,
              typename P2>
    auto operator()(R1 &&r1, R2 &&r2, OutIt dest, Comparator comp, P1 p1, P2 p2) const -> OutIt
    {
        return i::merge_move(adl::begin(r1), adl::end(r1), adl::begin(r2), adl::end(r2), dest,
                             astl::pass_fn(comp), astl::pass_fn(p1), astl::pass_fn(p2));
    }
} merge_move{};

} // namespace r
} // namespace astl

#endif // ASTL_INCLUDE_INPLACE_MERGE_HPP
