//
// Created by Rijad on 30-Jul-18.
//

#ifndef ASTL_INCLUDE_UNIQUE_HPP
#define ASTL_INCLUDE_UNIQUE_HPP

#include <algorithm>
#include <type_traits>
#include <utility>

#include "astl/internal/adjacent_find.hpp"

#include "astl/functional.hpp"
#include "astl/iterator.hpp"
#include "astl/range_access.hpp"

namespace astl
{

namespace i
{
inline constexpr struct {
    template <typename FwdIt, typename BinaryPredicate = std::equal_to<>>
    auto operator()(FwdIt first, FwdIt last, BinaryPredicate pred = BinaryPredicate{}) const
        -> FwdIt
    {
        return std::unique(first, last, astl::pass_fn(pred));
    }

    template <typename FwdIt, typename BinaryPredicate, typename P>
    auto operator()(FwdIt first, FwdIt last, BinaryPredicate pred, P p) const -> FwdIt
    {
        return std::unique(first, last, astl::combine(astl::pass_fn(pred), astl::pass_fn(p)));
    }
} unique{};

inline constexpr struct {
    template <typename InputIt, typename OutIt, typename BinaryPredicate = std::equal_to<>>
    auto operator()(InputIt first, InputIt last, OutIt dest,
                    BinaryPredicate pred = BinaryPredicate{}) const -> OutIt
    {
        return std::unique_copy(first, last, dest, astl::pass_fn(pred));
    }

    template <typename InputIt, typename OutIt, typename BinaryPredicate, typename P>
    auto operator()(InputIt first, InputIt last, OutIt dest, BinaryPredicate pred, P p) const
        -> OutIt
    {
        return std::unique_copy(first, last, dest,
                                astl::combine(astl::pass_fn(pred), astl::pass_fn(p)));
    }
} unique_copy{};

inline constexpr struct {

    template <typename InIt, typename N, typename OutIt, typename BinaryPredicate = std::equal_to<>>
    // requires InIt InputIterator
    // requires N integral type
    // requires OutIt InputIterator
    // requires BinaryPredicate, returns bool, arguments two value_type(InIt)
    auto operator()(InIt first, N n, OutIt dest, BinaryPredicate pred = BinaryPredicate{}) const
        -> std::pair<OutIt, InIt>
    {

        using SameValueT = std::is_same<iter_value_type<InIt>, iter_value_type<OutIt>>;
        if (n == N(0)) return std::make_pair(dest, first);

        if constexpr (is_forward_it_v<InIt>) {
            using FwdIt = InIt;

            FwdIt firstb(first);
            *dest = *firstb;
            ++dest;
            ++first;
            --n;
            while (n != N(0)) {
                if (!pred(*firstb, *first)) { // copy unmatched
                    firstb = first;
                    *dest = *firstb;
                    ++dest;
                }
                ++first;
                --n;
            }
        }
        else if constexpr (is_forward_it_v<OutIt> && SameValueT::value) {
            *dest = *first;
            ++first;
            --n;
            while (n != N(0)) {
                if (!pred(*dest, *first)) *++dest = *first;

                ++first;
                --n;
            }
            ++dest;
        }
        else {
            auto val(*first);
            *dest = val;
            ++dest;
            ++first;
            --n;
            while (n != N(0)) {
                if (!pred(val, *first)) { // copy unmatched
                    val = *first;
                    *dest = val;
                    ++dest;
                }
                ++first;
                --n;
            }
        }

        return std::make_pair(dest, first);
    }

    template <typename InIt, typename N, typename OutIt, typename BinaryPredicate, typename P>
    auto operator()(InIt first, N n, OutIt dest, BinaryPredicate pred, P p) const
        -> std::pair<OutIt, InIt>
    {
        return (*this)(first, n, dest, astl::combine(astl::pass_fn(pred), astl::pass_fn(p)));
    }

} unique_copy_n{};

inline constexpr struct {

    template <typename FwdIt, typename OutIt, typename BinaryPredicate = std::equal_to<>>
    // requires FwdIt ForwardIterator
    // requires OutIt ForwardIterator
    // requires BinaryPredicate, returns bool, arguments value_type(FwdIt)
    auto operator()(FwdIt first, FwdIt last, OutIt dest,
                    BinaryPredicate pred = BinaryPredicate{}) const -> OutIt
    {
        return std::unique_copy(std::make_move_iterator(first), std::make_move_iterator(last), dest,
                                astl::pass_fn(pred));
    }

    template <typename FwdIt, typename OutIt, typename BinaryPredicate, typename P>
    auto operator()(FwdIt first, FwdIt last, OutIt dest, BinaryPredicate pred, P p) const -> OutIt
    {
        return i::unique_copy(std::make_move_iterator(first), std::make_move_iterator(last), dest,
                              astl::pass_fn(pred), astl::pass_fn(p));
    }
} unique_move{};

inline constexpr struct {
    template <typename InIt, typename N, typename OutIt, typename BinaryPredicate = std::equal_to<>>
    // requires InIt InputIterator
    // requires N integral type
    // requires OutIt InputIterator
    // requires BinaryPredicate, returns bool, arguments two value_type(InIt)
    auto operator()(InIt first, N n, OutIt dest, BinaryPredicate pred = BinaryPredicate{}) const
        -> std::pair<OutIt, InIt>
    {
        auto p(i::unique_copy_n(std::make_move_iterator(first), n, dest, astl::pass_fn(pred)));
        return std::make_pair(p.first, p.second.base());
    }

    template <typename InIt, typename N, typename OutIt, typename BinaryPredicate, typename P>
    auto operator()(InIt first, N n, OutIt dest, BinaryPredicate pred, P p) const
        -> std::pair<OutIt, InIt>
    {
        return (*this)(first, n, dest, astl::combine(astl::pass_fn(pred), astl::pass_fn(p)));
    }
} unique_move_n{};

inline constexpr struct {
    template <typename FwdIt, typename N, typename BinaryPredicate = std::equal_to<>>
    // requires FwdIt ForwardIterator
    // requires N integral type
    // requires BinaryPredicate, returns bool, arguments tow value_type(FwdIt)
    auto operator()(FwdIt first, N n, BinaryPredicate pred = BinaryPredicate{}) const -> FwdIt
    {
        // Skip the beginning, if already unique.
        std::pair<FwdIt, N> i(i::adjacent_find_n(first, n, astl::pass_fn(pred)));
        n = i.second;
        if (n == N(0)) return first;
        // Do the real copy work.
        FwdIt dest(i.first);
        first = std::next(dest);
        while (--n != N(0)) {
            if (!pred(*dest, *first)) *++dest = std::move(*first);

            ++first;
        }
        return ++dest;
    }

    template <typename FwdIt, typename N, typename BinaryPredicate, typename P>
    auto operator()(FwdIt first, N n, BinaryPredicate pred, P p) const -> FwdIt
    {
        return (*this)(first, n, astl::combine(astl::pass_fn(pred), astl::pass_fn(p)));
    }
} unique_n{};

} // namespace i

namespace r
{

inline constexpr struct {

    template <typename R, typename BinaryPredicate = std::equal_to<>>
    // requires R ForwardIterator range
    // requires BinaryPredicate, returns bool, arguments value_type(R)
    auto operator()(R &&r, BinaryPredicate pred = BinaryPredicate{}) const -> iter_of_range<R>
    {
        return i::unique(adl::begin(r), adl::end(r), astl::pass_fn(pred));
    }

    template <typename R, typename BinaryPredicate, typename P>
    auto operator()(R &&r, BinaryPredicate pred, P p) const -> iter_of_range<R>
    {
        return i::unique(adl::begin(r), adl::end(r), astl::pass_fn(pred), astl::pass_fn(p));
    }
} unique{};

inline constexpr struct {

    template <typename R, typename OutIt, typename BinaryPredicate = std::equal_to<>>
    // requires R ForwardIterator range
    // requires OutIt ForwardIterator
    // requires BinaryPredicate, returns bool, arguments value_type(R)
    auto operator()(R &&r, OutIt dest, BinaryPredicate pred = BinaryPredicate{}) const -> OutIt
    {
        return i::unique_copy(adl::begin(r), adl::end(r), dest, astl::pass_fn(pred));
    }

    template <typename R, typename OutIt, typename BinaryPredicate, typename P>
    auto operator()(R &&r, OutIt dest, BinaryPredicate pred, P p) const -> OutIt
    {
        return i::unique_copy(adl::begin(r), adl::end(r), dest, astl::pass_fn(pred),
                              astl::pass_fn(p));
    }
} unique_copy{};

inline constexpr struct {
    template <typename R, typename N, typename OutIt, typename BinaryPredicate = std::equal_to<>>
    // requires R InputIterator range
    // requires N integral type
    // requires OutIt InputIterator
    // requires BinaryPredicate, returns bool, arguments two value_type(InIt)
    auto operator()(R &&r, N n, OutIt dest, BinaryPredicate pred = BinaryPredicate{}) const
        -> std::pair<OutIt, astl::iter_of_range<R>>
    {
        return i::unique_copy_n(adl::begin(r), n, dest, astl::pass_fn(pred));
    }

    template <typename R, typename N, typename OutIt, typename BinaryPredicate, typename P>
    auto operator()(R &&r, N n, OutIt dest, BinaryPredicate pred, P p) const
        -> std::pair<OutIt, astl::iter_of_range<R>>
    {
        return i::unique_copy_n(adl::begin(r), n, dest, astl::pass_fn(pred), astl::pass_fn(p));
    }
} unique_copy_n{};

inline constexpr struct {

    template <typename R, typename OutIt, typename BinaryPredicate = std::equal_to<>>
    // requires R ForwardIterator range
    // requires OutIt ForwardIterator
    // requires BinaryPredicate, returns bool, arguments value_type(R)
    auto operator()(R &&r, OutIt dest, BinaryPredicate pred = BinaryPredicate{}) const -> OutIt
    {
        return i::unique_move(adl::begin(r), adl::end(r), dest, astl::pass_fn(pred));
    }

    template <typename R, typename OutIt, typename BinaryPredicate, typename P>
    auto operator()(R &&r, OutIt dest, BinaryPredicate pred, P p) const -> OutIt
    {
        return i::unique_move(adl::begin(r), adl::end(r), dest, astl::pass_fn(pred),
                              astl::pass_fn(p));
    }
} unique_move{};

inline constexpr struct {
    template <typename R, typename N, typename OutIt, typename BinaryPredicate = std::equal_to<>>
    // requires R InputIterator range
    // requires N integral type
    // requires OutIt InputIterator
    // requires BinaryPredicate, returns bool, arguments two value_type(InIt)
    auto operator()(R &&r, N n, OutIt dest, BinaryPredicate pred = BinaryPredicate{}) const
        -> std::pair<OutIt, astl::iter_of_range<R>>
    {
        return i::unique_move_n(adl::begin(r), n, dest, astl::pass_fn(pred));
    }

    template <typename R, typename N, typename OutIt, typename BinaryPredicate, typename P>
    auto operator()(R &&r, N n, OutIt dest, BinaryPredicate pred, P p) const
        -> std::pair<OutIt, astl::iter_of_range<R>>
    {
        return i::unique_move_n(adl::begin(r), n, dest, astl::pass_fn(pred), astl::pass_fn(p));
    }
} unique_move_n{};

inline constexpr struct {
    template <typename R, typename N, typename BinaryPredicate = std::equal_to<>>
    auto operator()(R &&r, N n, BinaryPredicate pred = BinaryPredicate{}) const -> iter_of_range<R>
    {
        return i::unique_n(adl::begin(r), n, astl::pass_fn(pred));
    }

    template <typename R, typename N, typename BinaryPredicate, typename P>
    auto operator()(R &&r, N n, BinaryPredicate pred, P p) const -> iter_of_range<R>
    {
        return i::unique_n(adl::begin(r), n, astl::pass_fn(pred), astl::pass_fn(p));
    }
} unique_n{};

} // namespace r
} // namespace astl

#endif // ASTL_INCLUDE_UNIQUE_HPP
