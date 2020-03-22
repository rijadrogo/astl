//
// Created by Rijad on 29-Jul-18.
//

#ifndef ASTL_INCLUDE_FIND_HPP
#define ASTL_INCLUDE_FIND_HPP

#include <algorithm>
#include <climits>
#include <cstring>
#include <type_traits>
#include <utility>

#include "astl/functional.hpp"
#include "astl/iterator.hpp"
#include "astl/map_iterator.hpp"
#include "astl/optional.hpp"
#include "astl/range_access.hpp"

namespace astl
{
namespace internal_find
{
template <int N, typename FwdIt, typename NaryPred>
auto find_if_adjacent1(FwdIt first, FwdIt last, NaryPred pred, iter_diff_type<FwdIt> d) -> FwdIt
{
    std::make_integer_sequence<int, N> seq{};
    d = d - N + 1;
    constexpr iter_diff_type<FwdIt> loop_unroll_num(4);
    auto trip_count(d >> 2);
    while (trip_count > 0) {
        if (internal::for_each1(pred, first, seq)) return first;

        ++first;
        if (internal::for_each1(pred, first, seq)) return first;

        ++first;
        if (internal::for_each1(pred, first, seq)) return first;

        ++first;
        if (internal::for_each1(pred, first, seq)) return first;

        ++first;
        d -= loop_unroll_num;
        --trip_count;
    }

    switch (d) {
    // NOLINTNEXTLINE(bugprone-branch-clone)
    case 3:
        if (internal::for_each1(pred, first, seq)) return first;

        ++first;
        // fallthrough
    case 2:
        if (internal::for_each1(pred, first, seq)) return first;

        ++first;
        // fallthrough
    case 1:
        if (internal::for_each1(pred, first, seq)) return first;

        // fallthrough
    case 0:
    default: return last;
    }
}

template <typename InIt, typename T> auto within_limits(InIt, const T &e) -> bool
{ // check whether e is within the limits of Elem
    using Elem = std::remove_pointer_t<InIt>;

    constexpr bool signed_elem(std::is_signed<Elem>::value);
    constexpr bool signed_t(std::is_signed<T>::value);
    constexpr bool any(static_cast<T>(-1) == -1);

    if constexpr (std::is_same<T, bool>::value) { return true; }
    if constexpr (signed_elem && signed_t) {
        // signed Elem, signed T
        return SCHAR_MIN <= e && e <= SCHAR_MAX;
    }
    if constexpr (signed_elem && !signed_t && any) {
        // signed Elem, unsigned T, -1 == static_cast<T>(-1)
        return e <= SCHAR_MAX || static_cast<T>(SCHAR_MIN) <= e;
    }
    if constexpr (signed_elem && !signed_t && !any) {
        // signed Elem, unsigned T, -1 != static_cast<T>(-1)
        return e <= SCHAR_MAX;
    }
    if constexpr (!signed_elem && signed_t) {
        // unsigned Elem, signed T
        return 0 <= e && e <= UCHAR_MAX;
    }
    if constexpr (!signed_elem && !signed_t) {
        // unsigned Elem, unsigned T
        return e <= UCHAR_MAX;
    }
    return false; // NOT REACHABLE
}

template <int N, bool> struct find_if_adjacent_t {
    template <typename FwdIt, typename NaryPred>
    ASTL_NODISCARD auto operator()(FwdIt first, FwdIt last, NaryPred pred) const ->
        typename std::enable_if<(N > 0), FwdIt>::type
    {
        return internal_find::find_if_adjacent1<N>(first, last, astl::pass_fn(pred),
                                                   astl::distance(first, last));
    }

    template <typename FwdIt, typename NaryPred, typename P>
    ASTL_NODISCARD auto operator()(FwdIt first, FwdIt last, NaryPred pred, P p) const ->
        typename std::enable_if<(N > 0), FwdIt>::type
    {
        return (*this)(first, last, astl::combine(astl::pass_fn(pred), astl::pass_fn(p)));
    }
};
template <int N> struct find_if_adjacent_t<N, true> {
    template <typename R, typename NaryPred, typename P>
    ASTL_NODISCARD auto operator()(R &&r, NaryPred pred, P p) const ->
        typename std::enable_if<(N > 0), astl::iter_of_range<R>>::type
    {
        return internal_find::find_if_adjacent1<N>(
            adl::begin(r), adl::end(r), astl::combine(astl::pass_fn(pred), astl::pass_fn(p)),
            astl::size_or_distance(r));
    }

    template <typename R, typename NaryPred>
    ASTL_NODISCARD auto operator()(R &&r, NaryPred pred) const ->
        typename std::enable_if<(N > 0), astl::iter_of_range<R>>::type
    {
        return internal_find::find_if_adjacent1<N>(adl::begin(r), adl::end(r), astl::pass_fn(pred),
                                                   astl::size_or_distance(r));
    }
};

} // namespace internal_find

namespace i
{

inline constexpr struct {

    template <typename FwdIt, typename T>
    ASTL_NODISCARD auto operator()(FwdIt first, FwdIt last, T const &value) const -> FwdIt
    {
        return std::find(first, last, value);
    }

    template <typename FwdIt, typename T, typename P>
    ASTL_NODISCARD auto operator()(FwdIt first, FwdIt last, T const &value, P p) const -> FwdIt
    {
        auto proj(astl::pass_fn(p));
        return std::find(astl::map_iterator(first, proj), astl::map_iterator(last, proj), value)
            .base();
    }
} find{};

inline constexpr struct {
    template <typename InIt, typename OutIt, typename T>
    // requires InIt InputIterator
    // requires OutIt OutputIterator, InIt is assignable to value_type(OutIt)
    // requires T equality comparable with value_type(InIt)
    ASTL_NODISCARD auto operator()(InIt first, InIt last, OutIt dest, T const &value) const -> OutIt
    {
        while (first != last) {
            if (*first == value) {
                *dest = first;
                ++dest;
            }
            ++first;
        }
        return dest;
    }

    template <typename InIt, typename OutIt, typename T, typename P>
    ASTL_NODISCARD auto operator()(InIt first, InIt const last, OutIt dest, T const &value,
                                   P p) const -> OutIt
    {
        while (first != last) {
            if (invoke(p, *first) == value) {
                *dest = first;
                ++dest;
            }
            ++first;
        }
        return dest;
    }
} find_all{};

inline constexpr struct {
    template <typename InIt, typename OutIt, typename UnaryPredicate>
    // requires InIt InputIterator
    // requires OutIt OutputIterator, InIt is assignable to value_type(OutIt)
    // requires UnaryPredicate, returns bool, argument value_type(InIt)
    ASTL_NODISCARD auto operator()(InIt first, InIt const last, OutIt dest,
                                   UnaryPredicate pred) const -> OutIt
    {
        while (first != last) {
            if (pred(*first)) {
                *dest = first;
                ++dest;
            }
            ++first;
        }
        return dest;
    }

    template <typename InIt, typename OutIt, typename UnaryPredicate, typename P>
    ASTL_NODISCARD auto operator()(InIt first, InIt last, OutIt dest, UnaryPredicate pred,
                                   P p) const -> OutIt
    {
        return (*this)(first, last, dest, astl::combine(astl::pass_fn(pred), astl::pass_fn(p)));
    }
} find_all_if{};

inline constexpr struct {
    template <typename InIt, typename N, typename OutIt, typename UnaryPredicate>
    // requires InIt InputIterator
    // requires N integral type
    // requires OutIt OutputIterator, InIt is assignable to value_type(OutIt)
    // requires UnaryPredicate, returns bool, argument value_type(InIt)
    ASTL_NODISCARD auto operator()(InIt first, N n, OutIt dest, UnaryPredicate pred) const
        -> std::pair<OutIt, InIt>
    {
        while (n != N(0)) {
            if (pred(*first)) {
                *dest = first;
                ++dest;
            }
            ++first;
            --n;
        }
        return std::make_pair(dest, first);
    }

    template <typename InIt, typename N, typename OutIt, typename UnaryPredicate, typename P>
    ASTL_NODISCARD auto operator()(InIt first, N n, OutIt dest, UnaryPredicate pred, P p) const
        -> OutIt
    {
        return (*this)(first, n, dest, astl::combine(astl::pass_fn(pred), astl::pass_fn(p)));
    }
} find_all_if_n{};

inline constexpr struct {

    template <typename InIt, typename N, typename OutIt, typename T>
    // requires InIt InputIterator
    // requires N integral type
    // requires OutIt OutputIterator
    // requires T equality comparable with value_type(InIt)
    ASTL_NODISCARD auto operator()(InIt first, N n, OutIt dest, T const &value) const
        -> std::pair<OutIt, InIt>
    {
        while (n != N(0)) {
            if (*first == value) {
                *dest = first;
                ++dest;
            }
            ++first;
            --n;
        }
        return std::make_pair(dest, first);
    }

    template <typename InIt, typename N, typename OutIt, typename T, typename P>
    ASTL_NODISCARD auto operator()(InIt first, N n, OutIt dest, T const &value, P p) const
        -> std::pair<OutIt, InIt>
    {
        while (n != N(0)) {
            if (invoke(p, *first) == value) {
                *dest = first;
                ++dest;
            }
            ++first;
            --n;
        }
        return std::make_pair(dest, first);
    }

} find_all_n{};

inline constexpr struct {
    template <typename BidiIt, typename T>
    // requires BidiIt BidirectionalIterator
    // requires T, equality comparable with value_type(BidiIt)
    ASTL_NODISCARD auto operator()(BidiIt first, BidiIt last, T const &e) const -> BidiIt
    {
        BidiIt ret(last);
        while (true) {
            if (*--last == e) return last;

            if (last == first) break;
        }
        return ret;
    }

    template <typename BidiIt, typename T, typename P>
    ASTL_NODISCARD auto operator()(BidiIt first, BidiIt last, T const &e, P p) const -> BidiIt
    {
        BidiIt ret(last);
        while (true) {
            if (invoke(p, *--last) == e) return last;

            if (last == first) break;
        }
        return ret;
    }
} find_backward{};

inline constexpr struct {
    template <typename BidiIt, typename UnaryPredicate>
    // requires BidiIt BidirectionalIterator
    // requires UnaryPredicate returns bool, argument value_type(BidiIt)
    ASTL_NODISCARD auto operator()(BidiIt first, BidiIt pos, BidiIt last, UnaryPredicate pred,
                                   bool const skip_self = false) const -> BidiIt
    {
        // precondition first <= pos <= last
        if (first == last) // empty range
            return last;

        if (!skip_self && pos != last && pred(*pos)) // found at pos
            return pos;

        BidiIt find_left(astl::prev(pos));
        BidiIt find_right(astl::next(pos));
        // look on both sides, while right comes to the end or left comes to first
        while (find_right != last) {
            if (pred(*find_left)) return find_left;

            if (pred(*find_right)) return find_right;

            if (find_left == first) break;

            --find_left;
            ++find_right;
        }
        // if there are elements on right check them
        while (find_right != last) {
            if (pred(*find_right)) return find_right;

            ++find_right;
        }
        // if there are elements on left check them
        while (true) {
            if (pred(*find_left)) return find_left;

            if (find_left == first) break;

            --find_left;
        }
        return last;
    }

    template <typename BidiIt, typename UnaryPredicate, typename P>
    ASTL_NODISCARD auto operator()(BidiIt first, BidiIt last, BidiIt pos, UnaryPredicate pred, P p,
                                   bool skip_self = false) const -> BidiIt
    {
        return (*this)(first, last, pos, astl::combine(astl::pass_fn(pred), astl::pass_fn(p)),
                       skip_self);
    }
} find_closest_if{};

inline constexpr struct {
    template <typename FwdIt1, typename FwdIt2, typename BinaryPredicate>
    ASTL_NODISCARD auto operator()(FwdIt1 first, FwdIt1 last, FwdIt2 s_first, FwdIt2 s_last,
                                   BinaryPredicate pred) const -> FwdIt1
    {
        return std::find_end(first, last, s_first, s_last, astl::pass_fn(pred));
    }

    template <typename FwdIt1, typename FwdIt2, typename BinaryPredicate, typename P1,
              typename P2 = P1>
    ASTL_NODISCARD auto operator()(FwdIt1 first, FwdIt1 last, FwdIt2 s_first, FwdIt2 s_last,
                                   BinaryPredicate pred, P1 p1, P2 p2 = P1{}) const -> FwdIt1
    {
        return std::find_end(
            first, last, s_first, s_last,
            astl::lockstep(astl::pass_fn(pred), astl::pass_fn(p1), astl::pass_fn(p2)));
    }
} find_end{};

inline constexpr struct {
    template <typename FwdIt1, typename N1, typename FwdIt2, typename N2,
              typename BinaryPredicate = std::equal_to<>>
    // requires FwdIt1 ForwardIterator
    // requires N1 integral type
    // requires FwdIt2 ForwardIterator
    // requires N2 integral type
    // requires BinaryPredicate, returns bool, arguments value_type(FwdIt1) and
    // value_type(FwdIt2)
    ASTL_NODISCARD auto operator()(FwdIt1 first1, N1 n1, FwdIt2 first2, N2 n2,
                                   BinaryPredicate pred = BinaryPredicate{}) const
        -> std::pair<FwdIt1, FwdIt1>
    {
        if constexpr (is_random_access_it_v<FwdIt1, FwdIt2>) { // Random Access Iterators
            using RandIt1 = FwdIt1;
            RandIt1 found(
                std::find_end(first1, first1 + n1, first2, first2 + n2, astl::pass_fn(pred)));
            return std::make_pair(found, first1 + n1);
        }
        else { // Forward Iterators
            FwdIt1 result;
            while (true) { // try a match at first1
                FwdIt1 next1(first1);
                FwdIt2 next2(first2);
                N1 n11(0);
                N2 n22(0);
                while (true) { // test if [first2, first2 + n2) is a prefix of [first1,
                    // first1 + n1)
                    bool const end_of_needle(n22 == n2);
                    if (end_of_needle) { // match candidate found
                        result = first1;
                    }

                    if (n11 == n1) {
                        // trying the next candidate would make [first1, first1 + n1) shorter
                        // than [first2, first2
                        // + n2), done
                        return std::make_pair(result, first1);
                    }

                    if (end_of_needle || !pred(*next1, *next2)) {
                        break; // end of match or counterexample found, go to the next
                        // candidate
                    }

                    ++next1;
                    ++n11;
                    ++next2;
                    ++n22;
                }

                ++first1;
                --n1;
            }
        }
    }

    template <typename FwdIt1, typename N1, typename FwdIt2, typename N2, typename BinaryPredicate,
              typename P1, typename P2>
    ASTL_NODISCARD auto operator()(FwdIt1 first1, N1 n1, FwdIt2 first2, N2 n2, BinaryPredicate pred,
                                   P1 p1, P2 p2 = P1{}) const -> FwdIt1
    {
        return (*this)(first1, n1, first2, n2,
                       astl::lockstep(astl::pass_fn(pred), astl::pass_fn(p1), astl::pass_fn(p2)));
    }
} find_end_n{};

inline constexpr struct {

    template <typename FwdIt1, typename FwdIt2, typename BinaryPredicate = std::equal_to<>>
    ASTL_NODISCARD auto operator()(FwdIt1 first, FwdIt1 last, FwdIt2 s_first, FwdIt2 s_last,
                                   BinaryPredicate pred = BinaryPredicate{}) const -> FwdIt1
    {
        return std::find_first_of(first, last, s_first, s_last, astl::pass_fn(pred));
    }

    template <typename FwdIt1, typename FwdIt2, typename BinaryPredicate, typename P1, typename P2>
    ASTL_NODISCARD auto operator()(FwdIt1 first, FwdIt1 last, FwdIt2 s_first, FwdIt2 s_last,
                                   BinaryPredicate pred, P1 p1, P2 p2) const -> FwdIt1
    {
        return std::find_first_of(
            first, last, s_first, s_last,
            astl::lockstep(astl::pass_fn(pred), astl::pass_fn(p1), astl::pass_fn(p2)));
    }
} find_first_of{};

inline constexpr struct {
    template <typename FwdIt1, typename N1, typename FwdIt2, typename N2,
              typename BinaryPredicate = std::equal_to<>>
    // requires FwdIt1 ForwardIterator
    // requires N1 integral type
    // requires FwdIt2 ForwardIterator
    // requires N2 integral type
    // requires BinaryPredicate, returns bool, arguments value_type(FwdIt1) and
    // value_type(FwdIt2)
    ASTL_NODISCARD auto operator()(FwdIt1 first1, N1 n1, FwdIt2 first2, N2 n2,
                                   BinaryPredicate pred = BinaryPredicate{}) const -> FwdIt1
    {
        while (n1 != N1(0)) {
            FwdIt2 i(first2);
            N2 n22(n2);
            while (n22 != N2(0)) {
                if (pred(*first1, *i)) return first1;

                ++i;
                --n22;
            }
            ++first1;
            --n1;
        }
        return first1;
    }

    template <typename FwdIt1, typename N1, typename FwdIt2, typename N2, typename BinaryPredicate,
              typename P1, typename P2>
    ASTL_NODISCARD auto find_first_of_n(FwdIt1 first1, N1 n1, FwdIt2 first2, N2 n2,
                                        BinaryPredicate pred, P1 p1, P2 p2 = P1{}) -> FwdIt1
    {
        return (*this)(first1, n1, first2, n2,
                       astl::lockstep(astl::pass_fn(pred), astl::pass_fn(p1), astl::pass_fn(p2)));
    }
} find_first_of_n{};

inline constexpr struct {

    template <typename InIt, typename UnaryPredicate>
    ASTL_NODISCARD auto operator()(InIt first, InIt last, UnaryPredicate pred) const -> InIt
    {
        return std::find_if(first, last, astl::pass_fn(pred));
    }

    template <typename InIt, typename UnaryPredicate, typename P>
    ASTL_NODISCARD auto operator()(InIt first, InIt last, UnaryPredicate pred, P p) const -> InIt
    {
        return std::find_if(first, last, astl::combine(astl::pass_fn(pred), astl::pass_fn(p)));
    }
} find_if{};

template <int N>
inline constexpr auto find_if_adjacent = internal_find::find_if_adjacent_t<N, false>{};

inline constexpr struct {
    template <typename BidiIt, typename UnaryPredicate>
    // requires BidiIt BidirectionalIterator
    // requires UnaryPredicate, returns bool, argument value_type(BidiIt)
    ASTL_NODISCARD auto operator()(BidiIt first, BidiIt last, UnaryPredicate pred) const -> BidiIt
    {
        BidiIt ret(last);
        if (first == last) return ret;

        while (true) {
            if (pred(*--last)) return last;

            if (last == first) break;
        }
        return ret;
    }

    template <typename BidiIt, typename UnaryPredicate, typename P>
    ASTL_NODISCARD auto operator()(BidiIt first, BidiIt last, UnaryPredicate pred, P p) const
        -> BidiIt
    {
        BidiIt ret(last);
        if (first == last) return ret;

        while (true) {
            if (pred(invoke(p, *--last))) return last;

            if (last == first) break;
        }
        return ret;
    }
} find_if_backward{};

inline constexpr struct {
    template <typename InIt, typename N, typename UnaryPredicate>
    // requires InIt InputIterator
    // requires N integral type
    // requires UnaryPredicate, returns bool, argument value_type(InIt)
    ASTL_NODISCARD auto operator()(InIt first, N n, UnaryPredicate pred) const -> std::pair<InIt, N>
    {
        N ret(n);
        auto trip_count(n >> 2);
        while (trip_count > 0) {
            if (pred(*first)) return std::make_pair(first, --n);

            ++first;
            --n;
            if (pred(*first)) return std::make_pair(first, --n);

            ++first;
            --n;
            if (pred(*first)) return std::make_pair(first, --n);

            ++first;
            --n;
            if (pred(*first)) return std::make_pair(first, --n);

            ++first;
            --n;
            --trip_count;
        }
        switch (integral_t<N>(n)) {
        // NOLINTNEXTLINE(bugprone-branch-clone)
        case 3:
            if (pred(*first)) return std::make_pair(first, --n);

            ++first;
            --n;
            // fallthrough
        case 2:
            if (pred(*first)) return std::make_pair(first, --n);

            ++first;
            --n;
            // fallthrough
        case 1:
            if (pred(*first)) return std::make_pair(first, --n);
            ++first;
            // fallthrough
        case 0:
        default: return std::make_pair(first, ret);
        }
    }

    template <typename InIt, typename N, typename UnaryPredicate, typename P>
    ASTL_NODISCARD auto operator()(InIt first, N n, UnaryPredicate pred, P p) const
        -> std::pair<InIt, N>
    {
        return (*this)(first, n, astl::combine(astl::pass_fn(pred), astl::pass_fn(p)));
    }
} find_if_n{};

inline constexpr struct {

    template <typename FwdIt, typename UnaryPredicate>
    ASTL_NODISCARD auto operator()(FwdIt first, FwdIt last, UnaryPredicate pred) const -> FwdIt
    {
        return std::find_if_not(first, last, astl::pass_fn(pred));
    }

    template <typename FwdIt, typename UnaryPredicate, typename P>
    ASTL_NODISCARD auto operator()(FwdIt first, FwdIt last, UnaryPredicate pred, P p) const -> FwdIt
    {
        return std::find_if_not(first, last, astl::combine(astl::pass_fn(pred), astl::pass_fn(p)));
    }
} find_if_not{};

inline constexpr struct {
    template <typename InIt, typename N, typename UnaryPredicate>
    // requires InIt is InputIterator,
    // requires N is integral type,
    // requires UnaryPredicate, return bool, argument value_type(InIt)
    ASTL_NODISCARD auto operator()(InIt first, N n, UnaryPredicate pred) const -> std::pair<InIt, N>
    {
        return i::find_if_n(first, n, astl::not_fn(astl::pass_fn(pred)));
    }

    template <typename InIt, typename UnaryPredicate, typename N, typename P>
    ASTL_NODISCARD auto operator()(InIt first, N len, UnaryPredicate pred, P p) const
        -> std::pair<InIt, N>
    {
        return (*this)(first, len, astl::combine(astl::pass_fn(pred), astl::pass_fn(p)));
    }
} find_if_not_n{};

inline constexpr struct {
    template <typename FwdIt, typename UnaryPredicate>
    // requires FwdIt ForwardIterator
    // requires UnaryPredicate, returns bool, argument value_type(FwdIt)
    ASTL_NODISCARD auto operator()(FwdIt first, FwdIt last, UnaryPredicate pred) const -> FwdIt
    {
        auto p(astl::pass_fn(pred));
        FwdIt find_first(i::find_if(first, last, p));
        if (find_first == last) return last;

        FwdIt find_again(i::find_if(astl::next(find_first), last, p));
        return find_again == last ? find_first : last;
    }

    template <typename FwdIt, typename BinaryPredicate, typename P>
    ASTL_NODISCARD auto operator()(FwdIt first, FwdIt last, BinaryPredicate pred, P p) const
        -> FwdIt
    {
        return (*this)(first, last, astl::combine(astl::pass_fn(pred), astl::pass_fn(p)));
    }
} find_if_unique{};

inline constexpr struct {
    template <typename FwdIt, typename N, typename UnaryPredicate>
    // requires FwdIt ForwardIterator
    // requires UnaryPredicate, returns bool, argument value_type(FwdIt)
    ASTL_NODISCARD auto operator()(FwdIt first, N n, UnaryPredicate pred) const
        -> std::pair<FwdIt, N>
    {
        auto pr(astl::pass_fn(pred));
        using PairType = std::pair<FwdIt, N>;

        PairType find_first(i::find_if_n(first, n, pr));

        if (find_first.second == n || find_first.second == N(0)) return find_first;

        PairType find_again(i::find_if_n(astl::next(find_first.first), find_first.second, pr));

        if (find_again.second == find_first.second) return find_first;

        find_again.second = n;
        return find_again;
    }

    template <typename FwdIt, typename N, typename BinaryPredicate, typename P>
    ASTL_NODISCARD auto operator()(FwdIt first, N n, BinaryPredicate pred, P p) const
        -> std::pair<FwdIt, N>
    {
        return (*this)(first, n, astl::combine(astl::pass_fn(pred), astl::pass_fn(p)));
    }
} find_if_unique_n{};

inline constexpr struct {
    template <typename FwdIt, typename UnaryPredicate, typename Comparator>
    // requires FwdIt ForwardIterator
    // requires UnaryPredicate, returns bool, argument value_type(FwdIt)
    // requires Comparator, returns bool, argument value_type(FwdIt)
    ASTL_NODISCARD auto operator()(FwdIt first, FwdIt last, UnaryPredicate pred,
                                   Comparator comp) const -> FwdIt
    {
        while (first != last && pred(*first)) {
            if (comp(*first)) return first;

            ++first;
        }
        return last;
    }

    template <typename FwdIt, typename UnaryPredicate, typename Comparator, typename Pc,
              typename Pup = Pc>
    ASTL_NODISCARD auto operator()(FwdIt first, FwdIt last, UnaryPredicate pred, Comparator comp,
                                   Pc pc, Pup p = Pc{}) const -> FwdIt
    {
        return (*this)(first, last, astl::combine(astl::pass_fn(pred), astl::pass_fn(p)),
                       astl::combine(astl::pass_fn(comp), astl::pass_fn(pc)));
    }
} find_if_while{};

inline constexpr struct {
    // Function to find contiguous sub-array with the largest sum
    // in given set of integers (handles negative numbers as well)
    template <typename FwdIt, typename BinaryOp = std::plus<>, typename CompareF = std::less<>>
    // requires FwdIt ForwardIterator
    // requires BinaryPredicate, returns ValueType(FwdIt), two arguments of
    // ValueType(FwdIt) requires CompareF StrictWeakOrdering
    ASTL_NODISCARD auto operator()(FwdIt first, FwdIt last, BinaryOp op, CompareF comp) const
        -> std::tuple<FwdIt, FwdIt, optional<astl::iter_value_type<FwdIt>>>
    {
        using Ret = std::tuple<FwdIt, FwdIt, optional<iter_value_type<FwdIt>>>;
        if (first == last) return Ret{first, last, nullopt};

        FwdIt s(first);
        FwdIt start(first);
        FwdIt end(first);

        // stores maximum sum sub-array found so far
        auto max_so_far(*first);

        // stores maximum sum of sub-array ending at current position
        auto max_ending_here(*first);

        ++first;
        // traverse the given array
        while (first != last) {
            max_ending_here = op(max_ending_here, *first);

            if (comp(max_so_far, max_ending_here)) {
                max_so_far = max_ending_here;
                start = s;
                end = astl::next(first);
            }

            if (comp(max_ending_here, *first)) {
                max_ending_here = *first;
                s = first;
            }

            ++first;
        }
        return Ret{start, end, astl::make_optional(*max_so_far)};
    }

    template <typename FwdIt, typename BinaryOp, typename CompareF, typename P>
    ASTL_NODISCARD auto operator()(FwdIt first, FwdIt last, BinaryOp op, CompareF comp, P p) const
        -> std::tuple<FwdIt, FwdIt, optional<astl::iter_value_type<FwdIt>>>
    {
        auto pp(astl::pass_fn(p));
        return (*this)(astl::map_iterator(first, pp), astl::map_iterator(last, pp),
                       astl::pass_fn(op), astl::pass_fn(comp));
    }
} find_max_sum_of_subarray{};

inline constexpr struct {

    template <typename InIt, typename N, typename T>
    // requires FwdIt ForwardIterator
    // requires N integral type
    // requires T, equality comparable with value_type(InIt)
    ASTL_NODISCARD auto operator()(InIt first, N n, T const &e) const -> std::pair<InIt, N>
    {
        // find first matching e; choose optimization
        // activate optimization for pointers to (const) bytes and integral values

        if (n == N(0)) return std::make_pair(first, n);

        constexpr bool memchr_opt(
            std::is_integral<T>::value
            && (std::is_same<T, char *>::value || std::is_same<T, signed char *>::value
                || std::is_same<T, unsigned char *>::value || std::is_same<T, char const *>::value
                || std::is_same<T, signed char const *>::value
                || std::is_same<T, unsigned char const *>::value));

        if constexpr (memchr_opt) { // find first byte matching integral e
            if (!internal_find::within_limits(first, e)) return std::make_pair(first, n);

            auto found(static_cast<InIt>(
                std::memchr(first, static_cast<unsigned char>(e), static_cast<std::size_t>(n))));

            if (found) return std::make_pair(found, n - (found - first) - 1);

            return std::make_pair(first, n);
        }
        else { // find first matching e
            N ret(n);
            auto trip_count(n >> 2);
            while (trip_count > 0) {
                if (*first == e) return std::make_pair(first, --n);

                ++first;
                --n;
                if (*first == e) return std::make_pair(first, --n);

                ++first;
                --n;
                if (*first == e) return std::make_pair(first, --n);

                ++first;
                --n;
                if (*first == e) return std::make_pair(first, --n);

                ++first;
                --n;
                --trip_count;
            }

            switch (integral_t<N>(n)) {
            // NOLINTNEXTLINE(bugprone-branch-clone)
            case 3:
                if (*first == e) return std::make_pair(first, --n);

                ++first;
                --n;
                // fallthrough
            case 2:
                if (*first == e) return std::make_pair(first, --n);

                ++first;
                --n;
                // fallthrough
            case 1:
                if (*first == e) return std::make_pair(first, --n);
                ++first;
                // fallthrough
            case 0:
            default: return std::make_pair(first, ret);
            }
        }
    }

    template <typename FwdIt, typename N, typename T, typename P>
    ASTL_NODISCARD auto operator()(FwdIt first, N n, T const &e, P p) const -> std::pair<FwdIt, N>
    {
        N ret(n);
        auto trip_count(n >> 2);
        while (trip_count > 0) {
            if (invoke(p, *first) == e) return std::make_pair(first, --n);

            ++first;
            --n;
            if (invoke(p, *first) == e) return std::make_pair(first, --n);

            ++first;
            --n;
            if (invoke(p, *first) == e) return std::make_pair(first, --n);

            ++first;
            --n;
            if (invoke(p, *first) == e) return std::make_pair(first, --n);

            ++first;
            --n;
            --trip_count;
        }

        switch (integral_t<N>(n)) {
        // NOLINTNEXTLINE(bugprone-branch-clone)
        case 3:
            if (invoke(p, *first) == e) return std::make_pair(first, --n);

            ++first;
            --n;
            // fallthrough
        case 2:
            if (invoke(p, *first) == e) return std::make_pair(first, --n);

            ++first;
            --n;
            // fallthrough
        case 1:
            if (invoke(p, *first) == e) return std::make_pair(first, --n);
            ++first;
            // fallthrough
        case 0:
        default: return std::make_pair(first, ret);
        }
    }

} find_n{};

inline constexpr struct {
    template <typename InIt, typename T>
    // requires FwdIt ForwardIterator
    // requires T, inequality comparable with value_type(FwdIt)
    ASTL_NODISCARD auto operator()(InIt first, InIt last, T const &val) const -> InIt
    {
        return std::find_if(first, last, astl::bind2nd(std::not_equal_to{}, val));
    }

    template <typename FwdIt, typename T, typename P>
    ASTL_NODISCARD auto operator()(FwdIt first, FwdIt last, T const &val, P p) const -> FwdIt
    {
        return std::find_if(first, last,
                            [p(astl::pass_fn(p)), &val](auto &&x) { return invoke(p, x) != val; });
    }
} find_not{};

inline constexpr struct {
    template <typename FwdIt, typename N, typename T>
    // requires FwdIt ForwardIterator
    // requires N integral type
    // requires T, inequality comparable with value_type(FwdIt)
    ASTL_NODISCARD auto operator()(FwdIt first, N n, T const &val) const -> std::pair<FwdIt, N>
    {
        return i::find_if_n(first, n, astl::bind2nd(std::not_equal_to{}, val));
    }

    template <typename FwdIt, typename N, typename T, typename P>
    ASTL_NODISCARD auto operator()(FwdIt first, N n, T const &val, P p) const -> std::pair<FwdIt, N>
    {
        return i::find_if_n(first, n,
                            [&val, p(astl::pass_fn(p))](auto &&x) { return invoke(p, x) != val; });
    }
} find_not_n{};

inline constexpr struct {
    template <typename BidIt, typename BinaryPredicate = std::less<>>
    // requires BidIt BidirectionalIterator
    // requires BinaryPredicate, returns bool, arguments two of value_type(BidIt)
    ASTL_NODISCARD auto operator()(BidIt first, BidIt last, BinaryPredicate pred) const -> BidIt
    {
        using N = iter_diff_type<BidIt>;
        N start(0);
        N end(astl::distance(first, last) - 1);
        if (end == 1)
            // notice - 1                ^^^^
            // only two elements
            return pred(*first, *--last) ? last : first;

        while (true) {
            N n((end - start) >> 1);
            BidIt mid(astl::next(first, n));
            BidIt mid_next(astl::next(mid));
            BidIt mid_prev(astl::prev(mid));

            // Compare middle element with its neighbours (if neighbours exist)
            if ((mid == first || !pred(*mid, *mid_prev)) && (mid == last || !pred(*mid, *mid_next)))
                return mid;

            // If middle element is not peak and its left neighbour is greater than it,
            // then left half must have a peak element
            if (mid != first && pred(*mid, *mid_prev)) end = n + 1;

            // If middle element is not peak and its right neighbour is greater than it,
            // then right half must have a peak element
            else
                start = n;
        }
    }

    template <typename BidIt, typename BinaryPredicate, typename P>
    ASTL_NODISCARD auto operator()(BidIt first, BidIt last, BinaryPredicate pred, P p) const
        -> BidIt
    {
        return (*this)(first, last, astl::combine(astl::pass_fn(pred), astl::pass_fn(p)));
    }
} find_peek{};

inline constexpr struct {
    template <typename FwdIt, typename T>
    // requires FwdIt ForwardIterator
    // requires T, inequality comparable with value_type(FwdIt)
    ASTL_NODISCARD auto operator()(FwdIt first, FwdIt last, T const &val) const
        -> std::pair<FwdIt, FwdIt>
    {
        first = i::find(first, last, val);
        if (first != last) last = i::find_not(astl::next(first), last, val);

        return std::make_pair(first, last);
    }

    template <typename FwdIt, typename T, typename P>
    ASTL_NODISCARD auto operator()(FwdIt first, FwdIt last, T const &val, P p) const
        -> std::pair<FwdIt, FwdIt>
    {
        auto proj(astl::pass_fn(p));
        first = i::find(first, last, val, proj);
        if (first != last) last = i::find_not(astl::next(first), last, val, proj);

        return std::make_pair(first, last);
    }

} find_range{};

inline constexpr struct {
    template <typename FwdIt, typename UnaryPredicate>
    // requires FwdIt ForwardIterator
    // requires UnaryPredicate, return bool, argument value_type(FwdIt)
    ASTL_NODISCARD auto operator()(FwdIt first, FwdIt last, UnaryPredicate pred) const
        -> std::pair<FwdIt, FwdIt>
    {
        auto pr(astl::pass_fn(pred));
        first = i::find_if(first, last, pr);
        if (first != last) last = i::find_if_not(astl::next(first), last, pr);

        return std::make_pair(first, last);
    }

    template <typename FwdIt, typename UnaryPredicate, typename P>
    ASTL_NODISCARD auto operator()(FwdIt first, FwdIt last, UnaryPredicate pred, P p) const
        -> std::pair<FwdIt, FwdIt>
    {
        return (*this)(first, last, astl::combine(astl::pass_fn(pred), astl::pass_fn(p)));
    }
} find_range_if{};

inline constexpr struct {
    template <typename FwdIt, typename N, typename UnaryPredicate>
    // requires FwdIt ForwardIterator
    // requires N integral type
    // requires UnaryPredicate, return bool, argument value_type(FwdIt)
    ASTL_NODISCARD auto operator()(FwdIt first, N n, UnaryPredicate pred) const
        -> std::pair<FwdIt, FwdIt>
    {
        auto pr(astl::pass_fn(pred));
        std::pair<FwdIt, N> i(i::find_if_n(first, n, pr));
        if (i.second == n) return std::make_pair(i.first, i.first);

        std::pair<FwdIt, N> j(i::find_if_not_n(astl::next(i.first), --i.second, pr));
        return std::make_pair(i.first, j.first);
    }

    template <typename FwdIt, typename N, typename UnaryPredicate, typename P>
    ASTL_NODISCARD auto operator()(FwdIt first, N n, UnaryPredicate pred, P p) const
        -> std::pair<FwdIt, FwdIt>
    {
        return (*this)(first, n, astl::combine(astl::pass_fn(pred), astl::pass_fn(p)));
    }
} find_range_if_n{};

inline constexpr struct {
    template <typename FwdIt, typename N, typename T>
    // requires FwdIt ForwardIterator
    // requires N integral type
    // requires T, inequality comparable with value_type(FwdIt)
    ASTL_NODISCARD auto operator()(FwdIt first, N n, T const &val) const -> std::pair<FwdIt, FwdIt>
    {
        std::pair<FwdIt, N> i(i::find_n(first, n, val));
        if (i.second == n) return std::make_pair(i.first, i.first);

        std::pair<FwdIt, N> j(i::find_not_n(astl::next(i.first), --i.second, val));
        return std::make_pair(i.first, j.first);
    }

    template <typename FwdIt, typename N, typename T, typename P>
    ASTL_NODISCARD auto operator()(FwdIt first, N n, T const &val, P p) const
        -> std::pair<FwdIt, FwdIt>
    {
        auto proj(astl::pass_fn(p));
        std::pair<FwdIt, N> i(i::find_n(first, n, val, proj));
        if (i.second == N(0)) return std::make_pair(i.first, i.first);

        std::pair<FwdIt, N> j(i::find_not_n(astl::next(i.first), --i.second, val, proj));
        return std::make_pair(i.first, j.first);
    }
} find_range_n{};

inline constexpr struct {
    template <typename FwdIt, typename T>
    // requires FwdIt ForwardIterator
    // requires T, equality comparable with value_type(FwdIt)
    ASTL_NODISCARD auto operator()(FwdIt first, FwdIt last, T const &value) const -> FwdIt
    {
        FwdIt find_first(std::find(first, last, value));
        if (find_first == last) return last;

        FwdIt find_again(std::find(astl::next(find_first), last, value));
        return find_again == last ? find_first : last;
    }

    template <typename FwdIt, typename T, typename P>
    ASTL_NODISCARD auto operator()(FwdIt first, FwdIt last, T const &value, P p) const -> FwdIt
    {
        auto proj(astl::pass_fn(p));
        FwdIt find_first(i::find(first, last, value, proj));
        if (find_first == last) return last;

        FwdIt find_again(i::find(astl::next(find_first), last, value, proj));
        return find_again == last ? find_first : last;
    }
} find_unique{};

inline constexpr struct {
    template <typename FwdIt, typename N, typename T>
    // requires FwdIt ForwardIterator
    // requires N integral type
    // requires T, equality comparable with value_type(FwdIt)
    ASTL_NODISCARD auto operator()(FwdIt first, N n, T const &value) const -> std::pair<FwdIt, N>
    {
        using PairType = std::pair<FwdIt, N>;
        PairType find_first(i::find_n(first, n, value));

        if (find_first.second == n || find_first.second == N(0)) return find_first;

        PairType find_again(i::find_n(astl::next(find_first.first), find_first.second, value));
        if (find_again.second == find_first.second) {
            ++find_first.second;
            return find_first;
        }

        find_again.second = n;
        return find_again;
    }

    template <typename FwdIt, typename N, typename T, typename P>
    ASTL_NODISCARD auto operator()(FwdIt first, N n, T const &value, P p) const -> FwdIt
    {
        using PairType = std::pair<FwdIt, N>;
        auto proj(astl::pass_fn(p));
        PairType find_first(i::find_n(first, n, value, proj));

        if (find_first.second == n || find_first.second == N(0)) return find_first;

        PairType find_again(
            i::find_n(astl::next(find_first.first), find_first.second, value, proj));
        if (find_again.second == find_first.second) return find_first;

        find_again.second = n;
        return find_again;
    }
} find_unique_n{};

inline constexpr struct {
    template <typename InIt, typename T, typename UnaryPredicate>
    // requires InIt InputIterator
    // requires T, equality comparable with value_type(FwdIt)
    // requires UnaryPredicate, returns bool, argument value_type(FwdIt)
    ASTL_NODISCARD auto operator()(InIt first, InIt last, T const &value, UnaryPredicate pred) const
        -> InIt
    {
        while (first != last && pred(*first)) {
            if (*first == value) return first;

            ++first;
        }
        return last;
    }

    template <typename InIt, typename T, typename UnaryPredicate, typename P1, typename P2 = P1>
    ASTL_NODISCARD auto operator()(InIt first, InIt last, T const &value, UnaryPredicate pred,
                                   P1 p1, P2 p2 = P1{}) const -> InIt
    {
        while (first != last && pred(astl::invoke(p2, *first))) {
            if (astl::invoke(p1, *first) == value) return first;

            ++first;
        }
        return last;
    }
} find_while{};

} // namespace i

namespace internal_find
{
template <typename R, typename T>
auto find1(R &&r, T &&value, internal_adl::rank<0>) -> iter_of_range<R>
{
    return i::find(adl::begin(r), adl::end(r), value);
}

template <typename R, typename T>
auto find1(R &&r, T &&value, internal_adl::rank<1>) -> decltype(r.find(static_cast<T &&>(value)))
{
    return r.find(static_cast<T &&>(value));
}
} // namespace internal_find

namespace r
{

inline constexpr struct {
    template <typename R, typename T>
    ASTL_NODISCARD auto operator()(R &&r, T &&value) const
        -> decltype(internal_find::find1(r, static_cast<T &&>(value), internal_adl::rank<1>{}))
    {
        return internal_find::find1(r, static_cast<T &&>(value), internal_adl::rank<1>{});
    }

    template <typename R, typename T, typename P>
    ASTL_NODISCARD auto operator()(R &&r, T const &value, P p) const -> iter_of_range<R>
    {
        return i::find(adl::begin(r), adl::end(r), value, astl::pass_fn(p));
    }
} find{};

inline constexpr struct {

    template <typename R, typename OutIt, typename T>
    ASTL_NODISCARD auto operator()(R &&r, OutIt dest, T const &value) const -> OutIt
    {
        return i::find_all(adl::begin(r), adl::end(r), dest, value);
    }

    template <typename R, typename OutIt, typename T, typename P>
    ASTL_NODISCARD auto operator()(R &&r, OutIt dest, T const &value, P p) const -> OutIt
    {
        return i::find_all(adl::begin(r), adl::end(r), dest, value, astl::pass_fn(p));
    }

} find_all{};

inline constexpr struct {

    template <typename R, typename OutIt, typename UnaryPredicate>
    ASTL_NODISCARD auto operator()(R &&r, OutIt dest, UnaryPredicate pred) const -> OutIt
    {
        return i::find_all_if(adl::begin(r), adl::end(r), dest, astl::pass_fn(pred));
    }

    template <typename R, typename OutIt, typename UnaryPredicate, typename P>
    ASTL_NODISCARD auto operator()(R &&r, OutIt dest, UnaryPredicate pred, P p) const -> OutIt
    {
        return i::find_all_if(adl::begin(r), adl::end(r), dest, astl::pass_fn(pred),
                              astl::pass_fn(p));
    }

} find_all_if{};

inline constexpr struct {
    template <typename R, typename N, typename OutIt, typename UnaryPredicate>
    ASTL_NODISCARD auto operator()(R &&r, N n, OutIt dest, UnaryPredicate pred) const -> OutIt
    {
        return i::find_all_if_n(adl::begin(r), n, dest, astl::pass_fn(pred));
    }

    template <typename R, typename N, typename OutIt, typename UnaryPredicate, typename P>
    ASTL_NODISCARD auto operator()(R &&r, N n, OutIt dest, UnaryPredicate pred, P p) const -> OutIt
    {
        return i::find_all_if_n(adl::begin(r), n, dest, astl::pass_fn(pred), astl::pass_fn(p));
    }
} find_all_if_n{};

inline constexpr struct {
    template <typename R, typename N, typename OutIt, typename T>
    ASTL_NODISCARD auto find_all_n(R &&r, N n, OutIt dest, T const &value) -> OutIt
    {
        return i::find_all_n(adl::begin(r), n, dest, value);
    }

    template <typename R, typename N, typename OutIt, typename T, typename P>
    ASTL_NODISCARD auto operator()(R &&r, N n, OutIt dest, T const &value, P p) const -> OutIt
    {
        return i::find_all_n(adl::begin(r), n, dest, value, astl::pass_fn(p));
    }
} find_all_n{};

inline constexpr struct {
    template <typename R, typename T>
    ASTL_NODISCARD auto operator()(R &&r, T const &value) const -> iter_of_range<R>
    {
        return i::find_backward(adl::begin(r), adl::end(r), value);
    }

    template <typename R, typename T, typename P>
    ASTL_NODISCARD auto operator()(R &&r, T const &value, P p) const -> iter_of_range<R>
    {
        return i::find_backward(adl::begin(r), adl::end(r), value, astl::pass_fn(p));
    }
} find_backward{};

inline constexpr struct {
    template <typename R, typename I, typename UnaryPredicate>
    ASTL_NODISCARD auto operator()(R &&r, I pos, UnaryPredicate pred, bool skip_self = false) const
        -> iter_of_range<R>
    {
        return i::find_closest_if(adl::begin(r), adl::end(r), pos, astl::pass_fn(pred), skip_self);
    }

    template <typename R, typename I, typename UnaryPredicate, typename P>
    ASTL_NODISCARD auto operator()(R &&r, I pos, UnaryPredicate pred, P p,
                                   bool skip_self = false) const -> iter_of_range<R>
    {
        return i::find_closest_if(adl::begin(r), adl::end(r), pos, astl::pass_fn(pred),
                                  astl::pass_fn(p), skip_self);
    }
} find_closest_if{};

inline constexpr struct {

    template <typename R1, typename R2, typename BinaryPredicate = std::equal_to<>>
    ASTL_NODISCARD auto find_end(R1 &&r1, R2 &&r2, BinaryPredicate pred = BinaryPredicate{})
        -> iter_of_range<R1>
    {
        return i::find_end(adl::begin(r1), adl::end(r1), adl::begin(r2), adl::end(r2),
                           astl::pass_fn(pred));
    }

    template <typename R1, typename R2, typename BinaryPredicate, typename P1, typename P2 = P1>
    ASTL_NODISCARD auto find_end(R1 &&r1, R2 &&r2, BinaryPredicate pred, P1 p1, P2 p2 = P1{})
        -> iter_of_range<R1>
    {
        return i::find_end(adl::begin(r1), adl::end(r1), adl::begin(r2), adl::end(r2),
                           astl::pass_fn(pred), astl::pass_fn(p1), astl::pass_fn(p2));
    }
} find_end{};

inline constexpr struct {
    template <typename R1, typename N1, typename R2, typename N2,
              typename BinaryPredicate = std::equal_to<>>
    ASTL_NODISCARD auto operator()(R1 &&r1, N1 n1, R2 &&r2, N2 n2,
                                   BinaryPredicate pred = BinaryPredicate{}) const
        -> iter_of_range<R1>
    {
        return i::find_end_n(adl::begin(r1), n1, adl::begin(r2), n2, astl::pass_fn(pred));
    }

    template <typename R1, typename N1, typename R2, typename N2, typename BinaryPredicate,
              typename P1, typename P2 = P1>
    ASTL_NODISCARD auto operator()(R1 &&r1, N1 n1, R2 &&r2, N2 n2, BinaryPredicate pred, P1 p1,
                                   P2 p2 = P1{}) const -> iter_of_range<R1>
    {
        return i::find_end_n(adl::begin(r1), n1, adl::begin(r2), n2, astl::pass_fn(pred),
                             astl::pass_fn(p1), astl::pass_fn(p2));
    }
} find_end_n{};

inline constexpr struct {

    template <typename R1, typename R2, typename BinaryPredicate = std::equal_to<>>
    ASTL_NODISCARD auto operator()(R1 &&r1, R2 &&r2, BinaryPredicate pred = BinaryPredicate{}) const
        -> iter_of_range<R1>
    {
        return i::find_first_of(adl::begin(r1), adl::end(r1), adl::begin(r2), adl::end(r2),
                                astl::pass_fn(pred));
    }

    template <typename R1, typename R2, typename BinaryPredicate, typename P1, typename P2 = P1>
    ASTL_NODISCARD auto operator()(R1 &&r1, R2 &&r2, BinaryPredicate pred, P1 p1,
                                   P2 p2 = P1{}) const -> iter_of_range<R1>
    {
        return i::find_first_of(adl::begin(r1), adl::end(r1), adl::begin(r2), adl::end(r2),
                                astl::pass_fn(pred), astl::pass_fn(p1), astl::pass_fn(p2));
    }

} find_first_of{};

inline constexpr struct {
    template <typename R1, typename N1, typename R2, typename N2>
    ASTL_NODISCARD auto find_first_of_n(R1 &&r1, N1 n1, R2 &&r2, N2 n2) -> iter_of_range<R1>
    {
        return i::find_first_of_n(adl::begin(r1), n1, adl::begin(r2), n2);
    }

    template <typename R1, typename N1, typename R2, typename N2, typename BinaryPredicate>
    ASTL_NODISCARD auto find_first_of_n(R1 &&r1, N1 n1, R2 &&r2, N2 n2, BinaryPredicate pred)
        -> iter_of_range<R1>
    {
        return i::find_first_of_n(adl::begin(r1), n1, adl::begin(r2), n2, astl::pass_fn(pred));
    }

    template <typename R1, typename N1, typename R2, typename N2, typename BinaryPredicate,
              typename P1, typename P2 = P1>
    ASTL_NODISCARD auto find_first_of_n(R1 &&r1, N1 n1, R2 &&r2, N2 n2, BinaryPredicate pred, P1 p1,
                                        P2 p2 = P1{}) -> iter_of_range<R1>
    {
        return i::find_first_of_n(adl::begin(r1), n1, adl::begin(r2), n2, astl::pass_fn(pred),
                                  astl::pass_fn(p1), astl::pass_fn(p2));
    }
} find_first_of_n{};

inline constexpr struct {
    template <typename R, typename UnaryPredicate>
    ASTL_NODISCARD auto operator()(R &&r, UnaryPredicate pred) const -> iter_of_range<R>
    {
        return i::find_if(adl::begin(r), adl::end(r), astl::pass_fn(pred));
    }

    template <typename R, typename UnaryPredicate, typename P>
    ASTL_NODISCARD auto operator()(R &&r, UnaryPredicate pred, P p) const -> iter_of_range<R>
    {
        return i::find_if(adl::begin(r), adl::end(r), astl::pass_fn(pred), astl::pass_fn(p));
    }
} find_if{};

template <int N>
inline constexpr auto find_if_adjacent = internal_find::find_if_adjacent_t<N, true>{};

inline constexpr struct {
    template <typename R, typename UnaryPredicate, typename Distance>
    ASTL_NODISCARD auto operator()(R &&r, Distance len, UnaryPredicate pred) const
        -> std::pair<astl::iter_of_range<R>, Distance>
    {
        return i::find_if_n(adl::begin(r), len, astl::pass_fn(pred));
    }

    template <typename R, typename UnaryPredicate, typename Distance, typename P>
    ASTL_NODISCARD auto operator()(R &&r, Distance len, UnaryPredicate pred, P p) const
        -> std::pair<astl::iter_of_range<R>, Distance>
    {
        return i::find_if_n(adl::begin(r), len, astl::pass_fn(pred), astl::pass_fn(p));
    }
} find_if_n{};

inline constexpr struct {
    template <typename R, typename UnaryPredicate>
    ASTL_NODISCARD auto operator()(R &&r, UnaryPredicate pred) const -> iter_of_range<R>
    {
        return i::find_if_not(adl::begin(r), adl::end(r), astl::pass_fn(pred));
    }

    template <typename R, typename UnaryPredicate, typename P>
    ASTL_NODISCARD auto operator()(R &&r, UnaryPredicate pred, P p) const -> iter_of_range<R>
    {
        return i::find_if_not(adl::begin(r), adl::end(r), astl::pass_fn(pred), astl::pass_fn(p));
    }
} find_if_not{};

inline constexpr struct {
    template <typename R, typename UnaryPredicate, typename Distance>
    ASTL_NODISCARD auto operator()(R &&r, Distance len, UnaryPredicate pred) const
        -> std::pair<astl::iter_of_range<R>, Distance>
    {
        return i::find_if_not_n(adl::begin(r), len, astl::pass_fn(pred));
    }

    template <typename R, typename UnaryPredicate, typename Distance, typename P>
    ASTL_NODISCARD auto operator()(R &&r, Distance len, UnaryPredicate pred, P p) const
        -> std::pair<astl::iter_of_range<R>, Distance>
    {
        return i::find_if_not_n(adl::begin(r), len, astl::pass_fn(pred), astl::pass_fn(p));
    }
} find_if_not_n{};

inline constexpr struct {
    template <typename R, typename UnaryPredicate>
    ASTL_NODISCARD auto operator()(R &&r, UnaryPredicate pred) const -> iter_of_range<R>
    {
        return i::find_if_unique(adl::begin(r), adl::end(r), astl::pass_fn(pred));
    }

    template <typename R, typename UnaryPredicate, typename P>
    ASTL_NODISCARD auto operator()(R &&r, UnaryPredicate pred, P p) const -> iter_of_range<R>
    {
        return i::find_if_unique(adl::begin(r), adl::end(r), astl::pass_fn(pred), astl::pass_fn(p));
    }
} find_if_unique{};

inline constexpr struct {
    template <typename R, typename N, typename UnaryPredicate>
    ASTL_NODISCARD auto operator()(R &&r, N n, UnaryPredicate pred) const
        -> std::pair<astl::iter_of_range<R>, N>
    {
        return i::find_if_unique_n(adl::begin(r), n, pred);
    }

    template <typename R, typename N, typename UnaryPredicate, typename P>
    ASTL_NODISCARD auto operator()(R &&r, N n, UnaryPredicate pred, P p) const
        -> std::pair<astl::iter_of_range<R>, N>
    {
        return i::find_if_unique_n(adl::begin(r), n, pred, astl::pass_fn(p));
    }
} find_if_unique_n{};

inline constexpr struct {
    template <typename R, typename Comparator, typename UnaryPredicate>
    ASTL_NODISCARD auto operator()(R &&r, Comparator comp, UnaryPredicate pred) const
        -> iter_of_range<R>
    {
        return i::find_if_while(adl::begin(r), adl::end(r), astl::pass_fn(pred),
                                astl::pass_fn(comp));
    }

    template <typename R, typename Comparator, typename UnaryPredicate, typename Pc,
              typename Pup = Pc>
    ASTL_NODISCARD auto operator()(R &&r, Comparator comp, UnaryPredicate pred, Pc pc,
                                   Pup p = Pc{}) const -> iter_of_range<R>
    {
        return i::find_if_while(adl::begin(r), adl::end(r), astl::pass_fn(pred),
                                astl::pass_fn(comp), astl::pass_fn(pc), astl::pass_fn(p));
    }
} find_if_while{};

inline constexpr struct {
    template <typename R, typename UnaryPredicate>
    ASTL_NODISCARD auto operator()(R &&r, UnaryPredicate pred) const -> iter_of_range<R>
    {
        return i::find_if_backward(adl::begin(r), adl::end(r), astl::pass_fn(pred));
    }

    template <typename R, typename UnaryPredicate, typename P>
    ASTL_NODISCARD auto operator()(R &&r, UnaryPredicate pred, P p) const -> iter_of_range<R>
    {
        return i::find_if_backward(adl::begin(r), adl::end(r), astl::pass_fn(pred),
                                   astl::pass_fn(p));
    }
} find_last_if{};

inline constexpr struct {
    template <typename R, typename BinaryOp = std::plus<>, typename CompareF = std::less<>>
    // requires R ForwardIterator range
    // requires BinaryPredicate, returns ValueType(R), two arguments of ValueType(R)
    // requires CompareF StrictWeakOrdering
    ASTL_NODISCARD auto operator()(R &&r, BinaryOp op, CompareF comp) const
        -> std::tuple<astl::iter_of_range<R>, astl::iter_of_range<R>,
                      optional<astl::range_value_type<R>>>
    {
        return i::find_max_sum_of_subarray(adl::begin(r), adl::end(r), astl::pass_fn(op),
                                           astl::pass_fn(comp));
    }

    template <typename R, typename BinaryOp, typename CompareF, typename P>
    ASTL_NODISCARD auto operator()(R &&r, BinaryOp op, CompareF comp, P p) const
        -> std::tuple<astl::iter_of_range<R>, astl::iter_of_range<R>,
                      optional<astl::range_value_type<R>>>
    {
        return i::find_max_sum_of_subarray(adl::begin(r), adl::end(r), astl::pass_fn(op),
                                           astl::pass_fn(comp), astl::pass_fn(p));
    }
} find_max_sum_of_subarray{};

inline constexpr struct {

    template <typename R, typename N, typename T>
    ASTL_NODISCARD auto operator()(R &&r, N n, T const &e) const
        -> std::pair<astl::iter_of_range<R>, N>
    {
        return i::find_n(adl::begin(r), n, e);
    }

    template <typename R, typename N, typename T, typename P>
    ASTL_NODISCARD auto operator()(R &&r, N n, T const &e, P p) const
        -> std::pair<astl::iter_of_range<R>, N>
    {
        return i::find_n(adl::begin(r), n, e, astl::pass_fn(p));
    }
} find_n{};

inline constexpr struct {
    template <typename R, typename T>
    // requires R ForwardIterator range
    // requires T, inequality comparable with value_type(R)
    ASTL_NODISCARD auto operator()(R &&r, T &&val) const -> iter_of_range<R>
    {
        return i::find_not(adl::begin(r), adl::end(r), val);
    }

    template <typename R, typename T, typename P>
    auto operator()(R &&r, T &&val, P p) const -> iter_of_range<R>
    {
        return i::find_not(adl::begin(r), adl::end(r), val, astl::pass_fn(p));
    }
} find_not{};

inline constexpr struct {
    template <typename R, typename N, typename T>
    // requires R ForwardIterator range
    // requires N integral type
    // requires T, inequality comparable with value_type(R)
    ASTL_NODISCARD auto operator()(R &&r, N n, T const &val) const
        -> std::pair<astl::iter_of_range<R>, N>
    {
        return i::find_not_n(adl::begin(r), n, val);
    }

    template <typename R, typename N, typename T, typename P>
    ASTL_NODISCARD auto operator()(R &&r, N n, T const &val, P p) const
        -> std::pair<astl::iter_of_range<R>, N>
    {
        return i::find_not_n(adl::begin(r), n, val, astl::pass_fn(p));
    }
} find_not_n{};

inline constexpr struct {
    template <typename R, typename BinaryPredicate = std::less<>>
    // requires R BidirectionalIterator range
    // requires  BinaryPredicate, returns bool, arguments two of value_type(R)
    ASTL_NODISCARD auto operator()(R &&r, BinaryPredicate pred = BinaryPredicate{}) const
        -> iter_of_range<R>
    {
        return i::find_peek(adl::begin(r), adl::end(r), astl::pass_fn(pred));
    }

    template <typename R, typename BinaryPredicate, typename P>
    ASTL_NODISCARD auto operator()(R &&r, BinaryPredicate pred, P p) const -> iter_of_range<R>
    {
        return i::find_peek(adl::begin(r), adl::end(r), astl::pass_fn(pred), astl::pass_fn(p));
    }
} find_peek{};

inline constexpr struct {
    template <typename R, typename T>
    // requires R ForwardIterator range
    // requires T, inequality comparable with value_type(R)
    ASTL_NODISCARD auto operator()(R &&r, T const &val) const
        -> std::pair<astl::iter_of_range<R>, astl::iter_of_range<R>>
    {
        return i::find_range(adl::begin(r), adl::end(r), val);
    }

    template <typename R, typename T, typename P>
    ASTL_NODISCARD auto operator()(R &&r, T const &val, P p) const
        -> std::pair<astl::iter_of_range<R>, astl::iter_of_range<R>>
    {
        return i::find_range(adl::begin(r), adl::end(r), val, astl::pass_fn(p));
    }
} find_range{};

inline constexpr struct {
    template <typename R, typename UnaryPredicate>
    // requires R ForwardIterator range
    // requires UnaryPredicate, return bool, argument value_type(R)
    ASTL_NODISCARD auto operator()(R &&r, UnaryPredicate pred) const
        -> std::pair<astl::iter_of_range<R>, astl::iter_of_range<R>>
    {
        return i::find_range_if(adl::begin(r), adl::end(r), astl::pass_fn(pred));
    }

    template <typename R, typename UnaryPredicate, typename P>
    ASTL_NODISCARD auto operator()(R &&r, UnaryPredicate pred, P p) const
        -> std::pair<astl::iter_of_range<R>, astl::iter_of_range<R>>
    {
        return i::find_range_if(adl::begin(r), adl::end(r), astl::pass_fn(pred), astl::pass_fn(p));
    }
} find_range_if{};

inline constexpr struct {
    template <typename R, typename N, typename UnaryPredicate>
    // requires R ForwardIterator range
    // requires N integral type
    // requires UnaryPredicate, return bool, argument value_type(R)
    ASTL_NODISCARD auto operator()(R &&r, N n, UnaryPredicate pred) const
        -> std::pair<astl::iter_of_range<R>, astl::iter_of_range<R>>
    {
        return i::find_range_if_n(adl::begin(r), n, astl::pass_fn(pred));
    }

    template <typename R, typename N, typename UnaryPredicate, typename P>
    ASTL_NODISCARD auto operator()(R &&r, N n, UnaryPredicate pred, P p) const
        -> std::pair<astl::iter_of_range<R>, astl::iter_of_range<R>>
    {
        return i::find_range_if_n(adl::begin(r), n, astl::pass_fn(pred), astl::pass_fn(p));
    }
} find_range_if_n{};

inline constexpr struct {
    template <typename R, typename N, typename T>
    // requires R ForwardIterator range
    // requires N integral type
    // requires T, inequality comparable with value_type(R)
    ASTL_NODISCARD auto operator()(R &&r, N n, T const &val) const
        -> std::pair<astl::iter_of_range<R>, astl::iter_of_range<R>>
    {
        return i::find_range_n(adl::begin(r), n, val);
    }

    template <typename R, typename N, typename T, typename P>
    ASTL_NODISCARD auto operator()(R &&r, N n, T const &val, P p) const
        -> std::pair<astl::iter_of_range<R>, astl::iter_of_range<R>>
    {
        return i::find_range_n(adl::begin(r), n, val, astl::pass_fn(p));
    }
} find_range_n{};

inline constexpr struct {
    template <typename R, typename T>
    ASTL_NODISCARD auto operator()(R &&r, T &&value) const -> iter_of_range<R>
    {
        return i::find_unique(adl::begin(r), adl::end(r), value);
    }

    template <typename R, typename T, typename P>
    ASTL_NODISCARD auto operator()(R &&r, T &&value, P p) const -> iter_of_range<R>
    {
        return i::find_unique(adl::begin(r), adl::end(r), value, astl::pass_fn(p));
    }
} find_unique{};

inline constexpr struct {
    template <typename R, typename N, typename T>
    ASTL_NODISCARD auto operator()(R &&r, N n, T const &value) const
        -> std::pair<astl::iter_of_range<R>, N>
    {
        return i::find_unique_n(adl::begin(r), n, value);
    }

    template <typename R, typename N, typename T, typename P>
    ASTL_NODISCARD auto operator()(R &&r, N n, T const &value, P p) const
        -> std::pair<astl::iter_of_range<R>, N>
    {
        return i::find_unique_n(adl::begin(r), n, value, astl::pass_fn(p));
    }
} find_unique_n{};

inline constexpr struct {
    template <typename R, typename T, typename UnaryPredicate>
    ASTL_NODISCARD auto operator()(R &&r, T &&value, UnaryPredicate pred) const -> iter_of_range<R>
    {
        return i::find_while(adl::begin(r), adl::end(r), value, astl::pass_fn(pred));
    }

    template <typename R, typename T, typename UnaryPredicate, typename Pc, typename Pup = Pc>
    ASTL_NODISCARD auto operator()(R &&r, T &&value, UnaryPredicate pred, Pc pc, Pup p = Pc{}) const
        -> iter_of_range<R>
    {
        return i::find_while(adl::begin(r), adl::end(r), value, astl::pass_fn(pred),
                             astl::pass_fn(pc), astl::pass_fn(p));
    }
} find_while{};

} // namespace r
} // namespace astl

#endif // ASTL_INCLUDE_FIND_HPP
