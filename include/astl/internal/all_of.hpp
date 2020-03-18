//
// Created by Rijad on 29-Jul-18.
//

#ifndef ASTL_INCLUDE_ALL_OF_HPP
#define ASTL_INCLUDE_ALL_OF_HPP

#include <algorithm>
#include <type_traits>
#include <utility>

#include "find.hpp"

#include "astl/functional.hpp"
#include "astl/iterator.hpp"
#include "astl/range_access.hpp"

namespace astl
{
namespace internal_all_of
{
template <int N, typename I, typename NaryPred>
// requires I ForwardIterator
// requires NaryPred, returns bool, N arguments value_type(I)
auto all_of_adjacent1(I first, NaryPred pred, iter_diff_type<I> d) -> bool
{
    std::make_integer_sequence<int, N> seq{};
    d = d - N + 1;
    constexpr iter_diff_type<I> loop_unroll_num(4);
    auto trip_count(d >> 2);
    while (trip_count > 0) {
        if (!internal::for_each1(pred, first, seq)) return false;

        ++first;
        if (!internal::for_each1(pred, first, seq)) return false;

        ++first;
        if (!internal::for_each1(pred, first, seq)) return false;

        ++first;
        if (!internal::for_each1(pred, first, seq)) return false;

        ++first;
        d -= loop_unroll_num;
        --trip_count;
    }

    switch (d) {
    case 3:
        if (!internal::for_each1(pred, first, seq)) return false;

        ++first;
        // fallthrough
    case 2:
        if (!internal::for_each1(pred, first, seq)) return false;

        ++first;
        // fallthrough
    case 1:
        if (!internal::for_each1(pred, first, seq)) return false;

        // fallthrough
    case 0:
    default: return true;
    }
}

} // namespace internal_all_of

namespace i
{

inline constexpr struct {
    template <typename FwdIt, typename BinaryPredicate = std::equal_to<>>
    // requires FwdIt ForwardIterator
    // requires BinaryPredicate, returns bool, two arguments value_type(FwdIt)
    ASTL_NODISCARD auto operator()(FwdIt first, FwdIt const last,
                                   BinaryPredicate pred = BinaryPredicate{}) const -> bool
    {
        while (first != last) {
            FwdIt i(
                i::find_if(astl::next(first), last, astl::bind2nd(astl::pass_fn(pred), *first)));
            if (i != last) return false;

            ++first;
        }
        return true;
    }

    template <typename FwdIt, typename Comparator, typename P>
    ASTL_NODISCARD auto operator()(FwdIt first, FwdIt last, Comparator comp, P p) const -> bool
    {
        return (*this)(first, last, astl::combine(astl::pass_fn(comp), astl::pass_fn(p)));
    }
} all_different{};

inline constexpr struct {
    template <typename FwdIt, typename N, typename BinaryPredicate = std::equal_to<>>
    // requires FwdIt ForwardIterator
    // requires N integral type
    // requires BinaryPredicate, returns bool, two arguments value_type(FwdIt)
    ASTL_NODISCARD auto operator()(FwdIt first, N n, BinaryPredicate pred = BinaryPredicate{}) const
        -> bool
    {
        while (n != N(0)) {
            std::pair<FwdIt, N> i(
                i::find_if_n(astl::next(first), n, astl::bind2nd(astl::pass_fn(pred), *first)));
            if (i.second != n) return false;

            ++first;
            --n;
        }
        return true;
    }

    template <typename FwdIt, typename N, typename Comparator, typename P>
    ASTL_NODISCARD auto operator()(FwdIt first, N n, Comparator comp, P p) const -> bool
    {
        return (*this)(first, n, astl::combine(astl::pass_fn(comp), astl::pass_fn(p)));
    }
} all_different_n{};

inline constexpr struct {
    template <typename InIt>
    // requires InIt InputIterator
    ASTL_NODISCARD auto operator()(InIt first, InIt last) -> bool
    {
        return std::find(first, last, false) == last;
    }

    template <typename InIt, typename UnaryPredicate>
    ASTL_NODISCARD auto operator()(InIt first, InIt last, UnaryPredicate pred) const -> bool
    {
        return std::all_of(first, last, astl::pass_fn(pred));
    }

    template <typename InIt, typename UnaryPredicate, typename P>
    ASTL_NODISCARD auto operator()(InIt first, InIt last, UnaryPredicate pred, P p) const -> bool
    {
        return std::all_of(first, last, astl::combine(astl::pass_fn(pred), astl::pass_fn(p)));
    }
} all_of{};

namespace internal_a_adjacent
{
template <int N, bool Range> struct all_of_adjacent_t {
    template <typename FwdIt, typename NaryPred>
    ASTL_NODISCARD auto operator()(FwdIt first, FwdIt last, NaryPred fn) const ->
        typename std::enable_if<(N > 0), bool>::type
    {
        return internal_all_of::all_of_adjacent1<N>(first, astl::pass_fn(fn),
                                                    astl::distance(first, last));
    }

    template <typename FwdIt, typename NaryPred, typename P>
    ASTL_NODISCARD auto operator()(FwdIt first, FwdIt last, NaryPred pred, P p) const ->
        typename std::enable_if<(N > 0), bool>::type
    {
        return internal_all_of::all_of_adjacent1<N>(
            first, astl::combine(astl::pass_fn(pred), astl::pass_fn(p)),
            astl::distance(first, last));
    }
};

template <int N> struct all_of_adjacent_t<N, true> {
    template <typename R, typename NaryPred>
    ASTL_NODISCARD auto operator()(R &&r, NaryPred pred) const ->
        typename std::enable_if<(N > 0), bool>::type
    {
        return internal_all_of::all_of_adjacent1<N>(adl::begin(r), astl::pass_fn(pred),
                                                    astl::size_or_distance(r));
    }

    template <typename R, typename NaryPred, typename P>
    ASTL_NODISCARD auto operator()(R &&r, NaryPred pred, P p) const ->
        typename std::enable_if<(N > 0), bool>::type
    {
        return internal_all_of::all_of_adjacent1<N>(
            adl::begin(r), astl::combine(astl::pass_fn(pred), astl::pass_fn(p)),
            astl::size_or_distance(r));
    }
};

} // namespace internal_a_adjacent

template <int N>
inline constexpr auto all_of_adjacent = internal_a_adjacent::all_of_adjacent_t<N, false>{};

inline constexpr struct {
    template <typename InIt, typename E>
    // requires InIt InputIterator
    // requires E inequality comparable with value_type(InIt)
    ASTL_NODISCARD auto operator()(InIt first, InIt last, E &&elem) const -> bool
    {
        return std::find_if_not(first, last, astl::bind2nd(std::equal_to{}, elem)) == last;
    }

    template <typename InIt, typename E, typename P>
    ASTL_NODISCARD auto operator()(InIt first, InIt last, E &&elem, P p) const -> bool
    {
        while (first != last) {
            if (invoke(p, *first) != elem) return false;

            ++first;
        }
        return true;
    }
} all_of_equal{};

inline constexpr struct {
    template <typename InIt, typename N, typename E>
    // requires InIt InputIterator
    // requires N integral type
    // requires E, equality comparable with value_type(InIt)
    ASTL_NODISCARD auto operator()(InIt first, N n, E &&e) const -> bool
    {
        return i::find_if_n(first, n, astl::bind2nd(astl::pass_fn(std::equal_to{}, e))).second == n;
    }

    template <typename I, typename N, typename E, typename P>
    ASTL_NODISCARD auto operator()(I first, N n, E &&val, P p) const -> bool
    {
        auto proj(astl::pass_fn(p));
        return i::find_if_n(first, n,
                            [&val, proj](auto &&elem) { return !(val == invoke(proj, elem)); })
                   .second
            == n;
    }
} all_of_equal_n{};

inline constexpr struct {
    template <typename InIt, typename N>
    // requires InIt ForwardIterator
    // requires N integral type
    ASTL_NODISCARD auto operator()(InIt first, N n) const -> bool
    {
        return i::all_of_equal_n(first, n, true);
    }

    template <typename InIt, typename N, typename UnaryPredicate>
    // requires InIt InputIterator
    // requires N integral type
    // requires UnaryPredicate, returns bool, argument value_type(InIt)
    ASTL_NODISCARD auto operator()(InIt first, N n, UnaryPredicate pred) const -> bool
    {
        return i::find_if_not_n(first, n, astl::pass_fn(pred)).second == 0;
    }

    template <typename InIt, typename N, typename UnaryPredicate, typename P>
    ASTL_NODISCARD auto operator()(InIt first, N n, UnaryPredicate pred, P p) const -> bool
    {
        return (*this)(first, n, astl::combine(astl::pass_fn(pred), astl::pass_fn(p)));
    }

} all_of_n{};

inline constexpr struct {

    template <typename InIt, typename BinaryPredicate = std::equal_to<>>
    // requires InIt InputIterator
    // requires BinaryPredicate, returns bool, two arguments value_type(InIt)
    ASTL_NODISCARD auto all_same(InIt first, InIt last, BinaryPredicate pred = BinaryPredicate{})
        -> bool
    {
        if (first == last) return true;

        if constexpr (is_forward_it_v<InIt>) { // Forward Iterator
            return i::find_if_not(astl::next(first), last,
                                  astl::bind2nd(astl::pass_fn(pred), *first))
                == last;
        }
        else { // Input Iterator
            auto val(*first);
            return i::find_if_not(astl::next(first), last, astl::bind2nd(astl::pass_fn(pred), val))
                == last;
        }
    }

    template <typename InIt, typename BinaryPredicate, typename P>
    ASTL_NODISCARD auto operator()(InIt first, InIt last, BinaryPredicate pred, P p) const -> bool
    {
        return (*this)(first, last, astl::combine(astl::pass_fn(pred), astl::pass_fn(p)));
    }
} all_same{};

inline constexpr struct {

    template <typename InIt, typename N, typename BinaryPredicate = std::equal_to<>>
    // requires InIt InputIterator
    // requires N integral type
    // requires BinaryPredicate, returns bool, two arguments value_type(InIt)
    ASTL_NODISCARD auto operator()(InIt first, N n, BinaryPredicate pred = BinaryPredicate{}) const
        -> bool
    {
        if (n == N(0)) return true;

        if constexpr (is_forward_it_v<InIt>) { // Forward Iterator
            return i::find_if_not_n(astl::next(first), --n,
                                    astl::bind2nd(astl::pass_fn(pred), *first))
                       .second
                == n;
        }
        else { // Input Iterator
            auto val(*first);
            return i::find_if_not_n(astl::next(first), --n, astl::bind2nd(astl::pass_fn(pred), val))
                       .second
                == n;
        }
    }

    template <typename InIt, typename N, typename Comparator, typename P>
    ASTL_NODISCARD auto operator()(InIt first, N n, Comparator comp, P p) const -> bool
    {
        return (*this)(first, n, astl::combine(astl::pass_fn(comp), astl::pass_fn(p)));
    }
} all_same_n{};
} // namespace i

namespace r
{

inline constexpr struct {

    template <typename R, typename BinaryPredicate = std::equal_to<>>
    // requires R ForwardIterator range
    // requires BinaryPredicate, returns bool, two arguments value_type(R)
    ASTL_NODISCARD auto operator()(R &&r, BinaryPredicate pred = BinaryPredicate{}) const -> bool
    {
        return i::all_different(adl::begin(r), adl::end(r), astl::pass_fn(pred));
    }

    template <typename R, typename BinaryPredicate, typename P>
    ASTL_NODISCARD auto operator()(R &&r, BinaryPredicate pred, P p) const -> bool
    {
        return i::all_different(adl::begin(r), adl::end(r), astl::pass_fn(pred), astl::pass_fn(p));
    }

} all_different{};

inline constexpr struct {
    template <typename R, typename N, typename BinaryPredicate = std::equal_to<>>
    // requires R ForwardIterator range
    // requires N integral type
    // requires BinaryPredicate, returns bool, two arguments value_type(R)
    ASTL_NODISCARD auto operator()(R &&r, N n, BinaryPredicate pred = BinaryPredicate{}) const
        -> bool
    {
        return i::all_different_n(adl::begin(r), n, astl::pass_fn(pred));
    }

    template <typename R, typename N, typename BinaryPredicate, typename P>
    ASTL_NODISCARD auto operator()(R &&r, N n, BinaryPredicate pred, P p) const -> bool
    {
        return i::all_different_n(adl::begin(r), n, astl::pass_fn(pred), astl::pass_fn(p));
    }
} all_different_n{};

inline constexpr struct {
    template <typename R>
    // requires R InputIterator range
    ASTL_NODISCARD auto operator()(R &&r) const -> bool
    {
        return i::all_of(adl::begin(r), adl::end(r));
    }

    template <typename R, typename UnaryPredicate>
    // requires R InputIterator range
    // requires C, returns bool, argument value_type(R)
    ASTL_NODISCARD auto operator()(R &&r, UnaryPredicate pred) const -> bool
    {
        return i::all_of(adl::begin(r), adl::end(r), astl::pass_fn(pred));
    }

    template <typename R, typename UnaryPredicate, typename P>
    ASTL_NODISCARD auto operator()(R &&r, UnaryPredicate pred, P p) const -> bool
    {
        return i::all_of(adl::begin(r), adl::end(r), astl::pass_fn(pred), astl::pass_fn(p));
    }
} all_of{};

template <int N>
inline constexpr auto all_of_adjacent = i::internal_a_adjacent::all_of_adjacent_t<N, true>{};

inline constexpr struct {
    template <typename R, typename E>
    // requires R InputIterator range
    // requires E, equality comparable with value_type(R)
    ASTL_NODISCARD auto operator()(R &&r, E &&elem) const -> bool
    {
        return i::all_of(adl::begin(r), adl::end(r), elem);
    }

    template <typename R, typename E, typename P>
    ASTL_NODISCARD auto operator()(R &&r, E &&elem, P p) const -> bool
    {
        return i::all_of(adl::begin(r), adl::end(r), elem, astl::pass_fn(p));
    }
} all_of_equal{};

inline constexpr struct {
    template <typename R, typename N, typename E>
    // requires R InputIterator range
    // requires N integral type
    // requires E, equality comparable with value_type(R)
    ASTL_NODISCARD auto operator()(R &&r, N n, E &&e) const -> bool
    {
        return i::all_of_equal_n(adl::begin(r), n, e);
    }

    template <typename R, typename N, typename E, typename P>
    ASTL_NODISCARD auto operator()(R &&r, N n, E &&e, P p) const -> bool
    {
        return i::all_of_equal_n(adl::begin(r), n, e, astl::pass_fn(p));
    }
} all_of_equal_n{};

inline constexpr struct {
    template <typename R, typename N>
    // requires R InputIterator range
    // requires N integral type
    ASTL_NODISCARD auto operator()(R &&r, N n) const -> bool
    {
        return i::all_of_n(adl::begin(r), n);
    }

    template <typename R, typename N, typename UnaryPredicate>
    // requires R InputIterator range
    // requires N integral type
    // requires UnaryPredicate, returns bool, argument value_type(R)
    ASTL_NODISCARD auto operator()(R &&r, N n, UnaryPredicate pred) const -> bool
    {
        return i::all_of_n(adl::begin(r), n, astl::pass_fn(pred));
    }

    template <typename R, typename N, typename UnaryPredicate, typename P>
    ASTL_NODISCARD auto operator()(R &&r, N n, UnaryPredicate pred, P p) const -> bool
    {
        return i::all_of_n(adl::begin(r), n, astl::pass_fn(pred), astl::pass_fn(p));
    }
} all_of_n{};

inline constexpr struct {
    template <typename R, typename BinaryPredicate = std::equal_to<>>
    // requires R InputIterator range
    // requires BinaryPredicate, returns bool, two arguments value_type(R)
    ASTL_NODISCARD auto operator()(R &&r, BinaryPredicate pred = BinaryPredicate{}) const -> bool
    {
        return i::all_same(adl::begin(r), adl::end(r), astl::pass_fn(pred));
    }

    template <typename R, typename BinaryPredicate, typename P>
    ASTL_NODISCARD auto operator()(R &&r, BinaryPredicate pred, P p) const -> bool
    {
        return i::all_same(adl::begin(r), adl::end(r), astl::pass_fn(pred), astl::pass_fn(p));
    }
} all_same{};

inline constexpr struct {
    template <typename R, typename N, typename BinaryPredicate = std::equal_to<>>
    // requires R InputIterator range
    // requires N integral type
    // requires BinaryPredicate, returns bool, two arguments value_type(R)
    ASTL_NODISCARD auto operator()(R &&r, N n, BinaryPredicate pred = BinaryPredicate{}) const
        -> bool
    {
        return i::all_same_n(adl::begin(r), n, astl::pass_fn(pred));
    }

    template <typename R, typename N, typename BinaryPredicate, typename P>
    ASTL_NODISCARD auto operator()(R &&r, N n, BinaryPredicate pred, P p) const -> bool
    {
        return i::all_same_n(adl::begin(r), n, astl::pass_fn(pred), astl::pass_fn(p));
    }
} all_same_n{};

} // namespace r
} // namespace astl

#endif // ASTL_INCLUDE_ALL_OF_HPP
