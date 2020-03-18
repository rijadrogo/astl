//
// Created by Rijad on 29-Jul-18.
//

#ifndef ASTL_INCLUDE_PERMUTATION_HPP
#define ASTL_INCLUDE_PERMUTATION_HPP

#include <algorithm>
#include <cassert>

#include "astl/functional.hpp"
#include "astl/iterator.hpp"
#include "astl/range_access.hpp"

namespace astl
{
namespace i
{

inline constexpr struct {

    /// \fn apply_permutation (RandIt1 item_first, RandIt1 item_last, RandIt2
    /// ind_first) \brief Reorder item sequence with index sequence order
    ///
    /// \param item_first    The start of the item sequence
    /// \param item_last     One past the end of the item sequence
    /// \param ind_first     The start of the index sequence.
    ///
    /// \return numebr of swaps
    /// \note Item sequence size should be equal to index size. Otherwise behavior
    /// is undefined.
    ///       Complexity: O(N).
    template <typename RandIt1, typename RandIt2>
    // requires RandIt1 RandomAccessIterator
    // requires RandIt2 RandomAccessIterator
    auto apply_permutation(RandIt1 item_first, RandIt1 item_last, RandIt2 ind_first)
        -> iter_diff_type<RandIt1>
    {
        using Diff = iter_diff_type<RandIt1>;
        Diff i(0);
        Diff num_of_swaps(0);
        Diff new_size(item_last - item_first);
        while (i < new_size) {
            Diff current(i);
            while (i != ind_first[current]) {
                auto next(ind_first[current]);
                using std::swap;
                swap(item_first[current], item_first[next]);
                ++num_of_swaps;
                ind_first[current] = current;
                current = next;
            }
            ind_first[current] = current;
            ++i;
        }
        return num_of_swaps;
    }

    template <typename RandIt1, typename RandIt2>
    // requires RandIt1 RandomAccessIterator
    // requires RandIt2 RandomAccessIterator
    auto apply_permutation(RandIt1 item_first, RandIt1 item_last, RandIt2 ind_first,
                           RandIt2 ind_last) -> iter_diff_type<RandIt1>
    {
        // precondition: item_last - item_first == ind_last - ind_first
        assert(item_last - item_first == ind_last - ind_first); //-V2528
        (void) ind_last;
        return (*this)(item_first, item_last, ind_first);
    }

} apply_permutation{};

inline constexpr struct {
    /// \fn apply_reverse_permutation ( RandIt1 item_begin, RandIt1 item_end,
    /// RandIt2 ind_begin ) \brief Reorder item sequence with index sequence order
    ///
    /// \param item_begin    The start of the item sequence
    /// \param item_end      One past the end of the item sequence
    /// \param ind_begin     The start of the index sequence.
    ///
    /// \note Item sequence size should be equal to index size. Otherwise behavior
    /// is undefined.
    ///       Complexity: O(N).
    template <typename RandIt1, typename RandIt2>
    // requires RandIt1 RandomAccessIterator
    // requires RandIt2 RandomAccessIterator
    auto operator()(RandIt1 item_begin, RandIt1 item_end, RandIt2 ind_begin) const
        -> iter_diff_type<RandIt1>
    {
        using Diff = iter_diff_type<RandIt1>;
        using std::swap;
        Diff i(0);
        Diff num_of_swaps(0);
        Diff length(item_end - item_begin);
        while (i < length) {
            while (i != ind_begin[i]) {
                Diff next(ind_begin[i]);
                swap(item_begin[i], item_begin[next]);
                ++num_of_swaps;
                swap(ind_begin[i], ind_begin[next]);
            }
            ++i;
        }
        return num_of_swaps;
    }

    template <typename RandIt1, typename RandIt2>
    // requires RandIt1 RandomAccessIterator
    // requires RandIt2 RandomAccessIterator
    auto operator()(RandIt1 item_first, RandIt1 item_last, RandIt2 ind_first,
                    RandIt2 ind_last) const -> iter_diff_type<RandIt1>
    {
        // precondition: item_last - item_first == ind_last - ind_first
        assert(item_last - item_first == ind_last - ind_first); //-V2528
        (void) ind_last;
        return (*this)(item_first, item_last, ind_first);
    }
} apply_reverse_permutation{};

inline constexpr struct {

    template <typename BidiIt1, typename BidiIt2, typename Comparator = std::equal_to<>>
    ASTL_NODISCARD auto operator()(BidiIt1 first1, BidiIt1 last1, BidiIt2 first2,
                                   Comparator comp = Comparator{}) const -> bool
    {
        return std::is_permutation(first1, last1, first2, astl::pass_fn(comp));
    }

    template <typename BidiIt1, typename BidiIt2, typename Comparator = std::equal_to<>>
    ASTL_NODISCARD auto operator()(BidiIt1 first1, BidiIt1 last1, BidiIt2 first2, BidiIt2 last2,
                                   Comparator comp = Comparator{}) const -> bool
    {
        return std::is_permutation(first1, last1, first2, last2, astl::pass_fn(comp));
    }

    template <typename BidiIt1, typename BidiIt2, typename Comparator, typename P1, typename P2>
    ASTL_NODISCARD auto operator()(BidiIt1 first1, BidiIt1 last1, BidiIt2 first2, Comparator comp,
                                   P1 p1, P2 p2) const -> bool
    {
        return std::is_permutation(
            first1, last1, first2,
            astl::lockstep(astl::pass_fn(comp), astl::pass_fn(p1), astl::pass_fn(p2)));
    }

    template <typename BidiIt1, typename BidiIt2, typename Comparator, typename P1, typename P2>
    ASTL_NODISCARD auto operator()(BidiIt1 first1, BidiIt1 last1, BidiIt2 first2, BidiIt2 last2,
                                   Comparator comp, P1 p1, P2 p2) const -> bool
    {
        return std::is_permutation(
            first1, last1, first2, last2,
            astl::lockstep(astl::pass_fn(comp), astl::pass_fn(p1), astl::pass_fn(p2)));
    }
} is_permutation{};

inline constexpr struct {
    template <typename FwdIt1, typename FwdIt2, typename EqualityCompare = std::equal_to<>>
    // requires FwdIt1 ForwardIterator
    // requires FwdIt2 ForwardIterator
    // requires EqualityCompare BinaryFunction, bool(value_type(FwdIt1), value_type(FwdIt1))
    auto operator()(FwdIt1 first1, FwdIt1 last1, FwdIt2 first2, FwdIt2 last2,
                    EqualityCompare eqv = EqualityCompare{}) const -> std::pair<FwdIt1, FwdIt2>
    {
        while (first1 != last1) {
            FwdIt2 i(std::find_if(first2, last2, astl::bind2nd(eqv, *first1)));
            if (i == last2) break;
            std::iter_swap(first2, i);
            ++first1;
            ++first2;
        }
        return std::make_pair(first1, first2);
    }

    template <typename FwdIt1, typename FwdIt2, typename EqualityCompare, typename P1, typename P2>
    auto operator()(FwdIt1 first1, FwdIt1 last1, FwdIt2 first2, FwdIt2 last2, EqualityCompare eqv,
                    P1 p1, P2 p2) const -> std::pair<FwdIt1, FwdIt2>
    {
        return (*this)(first1, last1, first2, last2,
                       astl::lockstep(astl::pass_fn(eqv), astl::pass_fn(p1), astl::pass_fn(p2)));
    }
} mismatch_permuted{};

inline constexpr struct {
    template <typename BidiIt, typename Comparator = std::equal_to<>>
    auto operator()(BidiIt first, BidiIt last, Comparator comp = Comparator{}) const -> bool
    {
        return std::next_permutation(first, last, astl::pass_fn(comp));
    }

    template <typename BidiIt, typename Comparator, typename P>
    auto operator()(BidiIt first, BidiIt last, Comparator comp, P p) const -> bool
    {
        return std::next_permutation(first, last,
                                     astl::combine(astl::pass_fn(comp), astl::pass_fn(p)));
    }
} next_permutation{};

inline constexpr struct {
    template <typename BidiIt, typename Comparator = std::equal_to<>>
    auto operator()(BidiIt first, BidiIt last, Comparator comp = Comparator{}) const -> bool
    {
        return std::prev_permutation(first, last, astl::pass_fn(comp));
    }

    template <typename BidiIt, typename Comparator, typename P>
    auto operator()(BidiIt first, BidiIt last, Comparator comp, P p) const -> bool
    {
        return std::prev_permutation(first, last,
                                     astl::combine(astl::pass_fn(comp), astl::pass_fn(p)));
    }
} prev_permutation{};

} // namespace i

namespace r
{

inline constexpr struct {
    template <typename R1, typename R2>
    // requires R1 RandomAccessIterator range
    // requires R2 RandomAccessIterator range
    auto operator()(R1 &&r1, R2 &&r2) const -> range_diff_type<R1>
    {
        return i::apply_permutation(adl::begin(r1), adl::end(r1), adl::begin(r2), adl::end(r2));
    }
} apply_permutation{};

inline constexpr struct {
    template <typename R1, typename R2>
    // requires R1 RandomAccessIterator range
    // requires R2 RandomAccessIterator range
    auto operator()(R1 &&r1, R2 &&r2) const -> range_diff_type<R1>
    {
        return i::apply_permutation(adl::begin(r1), adl::end(r1), adl::begin(r2), adl::end(r2));
    }
} apply_reverse_permutation{};

inline constexpr struct {

    template <typename R1, typename R2, typename Comparator = std::equal_to<>>
    ASTL_NODISCARD auto operator()(R1 &&r1, R2 &&r2, Comparator comp = Comparator{}) const -> bool
    {
        return i::is_permutation(adl::begin(r1), adl::end(r1), adl::begin(r2), adl::end(r2),
                                 astl::pass_fn(comp));
    }

    template <typename R1, typename R2, typename Comparator, typename P1, typename P2>
    ASTL_NODISCARD auto operator()(R1 &&r1, R2 &&r2, Comparator comp, P1 p1, P2 p2) const -> bool
    {
        return i::is_permutation(adl::begin(r1), adl::end(r1), adl::begin(r2), adl::end(r2),
                                 astl::pass_fn(comp), astl::pass_fn(p1), astl::pass_fn(p2));
    }
} is_permutation{};

inline constexpr struct {

    template <typename R1, typename R2, typename Comparator = std::equal_to<>>
    auto operator()(R1 &&r1, R2 &&r2, Comparator comp = Comparator{}) const
        -> std::pair<astl::iter_of_range<R1>, astl::iter_of_range<R2>>
    {
        return i::mismatch_permuted(adl::begin(r1), adl::end(r1), adl::begin(r2), adl::end(r2),
                                    astl::pass_fn(comp));
    }

    template <typename R1, typename R2, typename Comparator, typename P1, typename P2>
    auto operator()(R1 &&r1, R2 &&r2, Comparator comp, P1 p1, P2 p2) const
        -> std::pair<astl::iter_of_range<R1>, astl::iter_of_range<R2>>
    {
        return i::mismatch_permuted(adl::begin(r1), adl::end(r1), adl::begin(r2), adl::end(r2),
                                    astl::pass_fn(comp), astl::pass_fn(p1), astl::pass_fn(p2));
    }

} mismatch_permuted{};

inline constexpr struct {

    template <typename R, typename Comparator = std::equal_to<>>
    auto next_permutation(R &&r, Comparator comp = Comparator{}) const -> bool
    {
        return i::next_permutation(adl::begin(r), adl::end(r), astl::pass_fn(comp));
    }

    template <typename R, typename Comparator, typename P>
    auto operator()(R &&r, Comparator comp, P p) const -> bool
    {
        return i::next_permutation(adl::begin(r), adl::end(r), astl::pass_fn(comp),
                                   astl::pass_fn(p));
    }
} next_permutation{};

inline constexpr struct {

    template <typename R, typename Comparator = std::equal_to<>>
    auto next_permutation(R &&r, Comparator comp = Comparator{}) const -> bool
    {
        return i::prev_permutation(adl::begin(r), adl::end(r), astl::pass_fn(comp));
    }

    template <typename R, typename Comparator, typename P>
    auto operator()(R &&r, Comparator comp, P p) const -> bool
    {
        return i::prev_permutation(adl::begin(r), adl::end(r), astl::pass_fn(comp),
                                   astl::pass_fn(p));
    }
} prev_permutation{};

} // namespace r
} // namespace astl

#endif // ASTL_INCLUDE_PERMUTATION_HPP
