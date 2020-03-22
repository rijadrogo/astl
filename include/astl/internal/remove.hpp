//
// Created by Rijad on 30-Jul-18.
//

#ifndef ASTL_INCLUDE_REMOVE_HPP
#define ASTL_INCLUDE_REMOVE_HPP

#include <algorithm>
#include <utility>

#include "astl/internal/find.hpp"
#include "astl/internal/search.hpp"

#include "astl/functional.hpp"
#include "astl/iterator.hpp"
#include "astl/range_access.hpp"

namespace astl
{
namespace i
{

inline constexpr struct {
    // If the predicate evaluates to false,
    // the first element of the pair is included in the result range;
    // otherwise, it is skipped.
    // Note: Last element is always copied
    template <typename FwdIt, typename OutIt, typename BinaryPredicate>
    // requires FwdIt ForwardIterator
    // requires OutIt OutputIterator
    // requires BinaryPredicate, returns bool, two arguments value_type(FwdIt)
    auto operator()(FwdIt first, FwdIt last, OutIt dest, BinaryPredicate pred) const -> OutIt
    {
        if (first == last) return dest;

        FwdIt trail(first);
        ++first;
        while (first != last) {
            if (!pred(*trail, *first)) {
                *dest = std::move(*trail);
                ++dest;
            }
            ++trail;
            ++first;
        }

        *dest = std::move(*trail);
        ++dest;
        return dest;
    }

    // If the predicate evaluates to false,
    // the first element of the pair is included in the result range;
    // otherwise, it is skipped.
    // Note: Last element is always copied
    template <typename FwdIt, typename OutIt, typename BinaryPredicate, typename P>
    auto operator()(FwdIt first, FwdIt last, OutIt dest, BinaryPredicate pred, P p) const -> OutIt
    {
        return (*this)(first, last, dest, astl::combine(astl::pass_fn(pred), astl::pass_fn(p)));
    }
} adjacent_remove_copy_if{};

inline constexpr struct {
    // If the predicate evaluates to false,
    // the first element of the pair is included in the result range;
    // otherwise, it is skipped.
    // Note: Last element is always copied
    template <typename FwdIt, typename BinaryPredicate>
    // requires FwdIt ForwardIterator
    // requires BinaryPredicate, returns bool, two arguments value_type(FwdIt)
    auto operator()(FwdIt first, FwdIt last, BinaryPredicate pred) const -> FwdIt
    {
        first = std::adjacent_find(first, last, astl::pass_fn(pred));
        if (first == last) return first;

        FwdIt trail(std::next(first));
        FwdIt current(std::next(trail));
        while (current != last) {
            if (!pred(*trail, *current)) {
                *first = std::move(*trail);
                ++first;
            }
            ++trail;
            ++current;
        }
        *first = std::move(*trail);
        ++first;
        return first;
    }

    // If the predicate evaluates to false,
    // the first element of the pair is included in the result range;
    // otherwise, it is skipped.
    // Note: Last element is always copied
    template <typename FwdIt, typename BinaryPredicate, typename P>
    auto operator()(FwdIt first, FwdIt last, BinaryPredicate pred, P p) const -> FwdIt
    {
        return (*this)(first, last, astl::combine(astl::pass_fn(pred), astl::pass_fn(p)));
    }
} adjacent_remove_if{};

inline constexpr struct {
    template <typename InputIt, typename OutIt, typename T>
    auto operator()(InputIt first, InputIt const last, OutIt dest, T const &value) const -> OutIt
    {
        return std::remove_copy(first, last, dest, value);
    }

    template <typename InputIt, typename OutIt, typename T, typename P>
    auto operator()(InputIt first, InputIt const last, OutIt dest, T const &value, P p) const
        -> OutIt
    {
        while (first != last) {
            if (!(invoke(p, *first) == value)) {
                *dest = *first;
                ++dest;
            }
            ++first;
        }
        return dest;
    }
} remove_copy{};

inline constexpr struct {

    template <typename FwdIt, typename T>
    auto operator()(FwdIt first, FwdIt last, T const &value) const -> FwdIt
    {
        return std::remove(first, last, value);
    }

    template <typename FwdIt, typename T, typename P>
    auto operator()(FwdIt first, FwdIt last, T const &value, P p) const -> FwdIt
    {
        auto proj(astl::pass_fn(p));
        first = i::find(first, last, value, proj);
        FwdIt i(first);
        return first == last ? first : i::remove_copy(++i, last, first, value, proj);
    }
} remove{};

inline constexpr struct {

    template <typename InputIt, typename OutIt, typename UnaryPredicate>
    auto operator()(InputIt first, InputIt last, OutIt dest, UnaryPredicate pred) const -> OutIt
    {
        return std::remove_copy_if(first, last, dest, pred);
    }

    template <typename InputIt, typename OutIt, typename UnaryPredicate, typename P>
    auto operator()(InputIt first, InputIt last, OutIt dest, UnaryPredicate pred, P p) const
        -> OutIt
    {
        return std::remove_copy_if(first, last, dest,
                                   astl::combine(astl::pass_fn(pred), astl::pass_fn(p)));
    }
} remove_copy_if{};

inline constexpr struct {

    template <typename FwdIt, typename UnaryPredicate>
    auto operator()(FwdIt first, FwdIt last, UnaryPredicate pred) const -> FwdIt
    {
        return std::remove_if(first, last, astl::pass_fn(pred));
    }

    template <typename FwdIt, typename UnaryPredicate, typename P>
    auto operator()(FwdIt first, FwdIt last, UnaryPredicate pred, P p) const -> FwdIt
    {
        return std::remove_if(first, last, astl::combine(astl::pass_fn(pred), astl::pass_fn(p)));
    }
} remove_if{};

inline constexpr struct {
    template <typename InputIt, typename OutIt, typename T>
    auto operator()(InputIt first, InputIt last, OutIt dest, T const &value) const -> OutIt
    {
        return std::remove_copy(std::make_move_iterator(first), std::make_move_iterator(last), dest,
                                value);
    }

    template <typename InputIt, typename OutIt, typename T, typename P>
    auto operator()(InputIt first, InputIt last, OutIt dest, T const &value, P p) const -> OutIt
    {
        return i::remove_copy(std::make_move_iterator(first), std::make_move_iterator(last), dest,
                              value, astl::pass_fn(p));
    }
} remove_move{};

inline constexpr struct {
    template <typename InputIt, typename OutIt, typename UnaryPredicate, typename P>
    auto operator()(InputIt first, InputIt last, OutIt dest, UnaryPredicate pred) const -> OutIt
    {
        return std::remove_copy_if(std::make_move_iterator(first), std::make_move_iterator(last),
                                   dest, astl::pass_fn(pred));
    }

    template <typename InputIt, typename OutIt, typename UnaryPredicate, typename P>
    auto operator()(InputIt first, InputIt last, OutIt dest, UnaryPredicate pred, P p) const
        -> OutIt
    {
        return i::remove_copy_if(std::make_move_iterator(first), std::make_move_iterator(last),
                                 dest, astl::pass_fn(pred), astl::pass_fn(p));
    }
} remove_move_if{};

inline constexpr struct {
    template <typename FwdIt1, typename FwdIt2, typename BinaryPredicate = std::equal_to<>>
    auto operator()(FwdIt1 first, FwdIt1 const last, FwdIt2 s_first, FwdIt2 const s_last,
                    BinaryPredicate pred = std::equal_to{}) const -> FwdIt1
    {
        using PairType = std::pair<FwdIt1, FwdIt1>;
        PairType p(i::searchp(first, last, s_first, s_last, astl::pass_fn(pred)));
        if (p.first == last) return last;

        return std::move(p.second, last, p.first);
    }

    template <typename FwdIt1, typename FwdIt2, typename BinaryPredicate, typename P>
    auto operator()(FwdIt1 first, FwdIt1 last, FwdIt2 s_first, FwdIt2 s_last, BinaryPredicate pred,
                    P p) const -> FwdIt1
    {
        using PairType = std::pair<FwdIt1, FwdIt1>;
        PairType pr(
            i::searchp(first, last, s_first, s_last, astl::pass_fn(pred), astl::pass_fn(p)));
        if (pr.first == last) return last;

        return std::move(pr.second, last, pr.first);
    }
} remove_range{};

inline constexpr struct {
    template <typename FwdIt, typename UnaryPredicate>
    auto operator()(FwdIt first, FwdIt last, UnaryPredicate pred) const -> FwdIt
    {
        if constexpr (is_bidirectional_it_v<FwdIt>) {
            while (true) {
                first = std::find_if(first, last, astl::pass_fn(pred));
                last = std::find_if_not(std::make_reverse_iterator(last),
                                        std::make_reverse_iterator(first), astl::pass_fn(pred))
                           .base();
                if (first == last) return first;

                *first = std::move(*--last);
            }
        }
        else {
            return std::remove_if(first, last, astl::pass_fn(pred));
        }
    }

    template <typename BidiIt, typename UnaryPredicate, typename P>
    auto operator()(BidiIt first, BidiIt last, UnaryPredicate pred, P p) const -> BidiIt
    {
        return (*this)(first, last, astl::combine(astl::pass_fn(pred), astl::pass_fn(p)));
    }
} unstable_remove_if{};

} // namespace i

namespace r
{

inline constexpr struct {
    // Note: Last element is always copied
    template <typename R, typename OutIt, typename BinaryPredicate>
    auto operator()(R &&r, OutIt dest, BinaryPredicate pred) const -> OutIt
    {
        return i::adjacent_remove_copy_if(adl::begin(r), adl::end(r), dest, astl::pass_fn(pred));
    }

    // Note: Last element is always copied
    template <typename R, typename OutIt, typename BinaryPredicate, typename P>
    auto operator()(R &&r, OutIt dest, BinaryPredicate pred, P p) const -> OutIt
    {
        return i::adjacent_remove_copy_if(adl::begin(r), adl::end(r), dest, astl::pass_fn(pred),
                                          astl::pass_fn(p));
    }
} adjacent_remove_copy_if{};

inline constexpr struct {
    // Note: Last element is always copied
    template <typename R, typename BinaryPredicate>
    auto operator()(R &&r, BinaryPredicate pred) const -> iter_of_range<R>
    {
        return i::adjacent_remove_if(adl::begin(r), adl::end(r), astl::pass_fn(pred));
    }

    // Note: Last element is always copied
    template <typename R, typename BinaryPredicate, typename P>
    auto operator()(R &&r, BinaryPredicate pred, P p) const -> iter_of_range<R>
    {
        return i::adjacent_remove_if(adl::begin(r), adl::end(r), astl::pass_fn(pred),
                                     astl::pass_fn(p));
    }
} adjacent_remove_if{};

inline constexpr struct {
    template <typename R, typename T>
    auto operator()(R &&r, T const &value) const -> iter_of_range<R>
    {
        return i::remove(adl::begin(r), adl::end(r), value);
    }

    template <typename R, typename T, typename P>
    auto operator()(R &&r, T const &value, P p) const -> iter_of_range<R>
    {
        return i::remove(adl::begin(r), adl::end(r), value, astl::pass_fn(p));
    }
} remove{};

inline constexpr struct {
    template <typename R, typename OutIt, typename T>
    auto operator()(R &&r, OutIt dest, T const &value) const -> OutIt
    {
        return i::remove_copy(adl::begin(r), adl::end(r), dest, value);
    }

    template <typename R, typename OutIt, typename T, typename P>
    auto operator()(R &&r, OutIt dest, T const &value, P p) const -> OutIt
    {
        return i::remove_copy(adl::begin(r), adl::end(r), dest, value, astl::pass_fn(p));
    }
} remove_copy{};

inline constexpr struct {
    template <typename R, typename OutIt, typename UnaryPredicate>
    auto operator()(R &&r, OutIt dest, UnaryPredicate pred) const -> OutIt
    {
        return i::remove_copy_if(adl::begin(r), adl::end(r), dest, astl::pass_fn(pred));
    }

    template <typename R, typename OutIt, typename UnaryPredicate, typename P>
    auto operator()(R &&r, OutIt dest, UnaryPredicate pred, P p) const -> OutIt
    {
        return i::remove_copy_if(adl::begin(r), adl::end(r), dest, astl::pass_fn(pred),
                                 astl::pass_fn(p));
    }
} remove_copy_if{};

inline constexpr struct {
    template <typename R, typename UnaryPredicate>
    auto operator()(R &&r, UnaryPredicate pred) const -> iter_of_range<R>
    {
        return i::remove_if(adl::begin(r), adl::end(r), astl::pass_fn(pred));
    }

    template <typename R, typename UnaryPredicate, typename P>
    auto operator()(R &&r, UnaryPredicate pred, P p) const -> iter_of_range<R>
    {
        return i::remove_if(adl::begin(r), adl::end(r), astl::pass_fn(pred), astl::pass_fn(p));
    }
} remove_if{};

inline constexpr struct {
    template <typename R, typename OutIt, typename T>
    auto operator()(R &&r, OutIt dest, T const &value) const -> OutIt
    {
        return i::remove_move(adl::begin(r), adl::end(r), dest, value);
    }

    template <typename R, typename OutIt, typename T, typename P>
    auto operator()(R &&r, OutIt dest, T const &value, P p) const -> OutIt
    {
        return i::remove_move(adl::begin(r), adl::end(r), dest, value, astl::pass_fn(p));
    }
} remove_move{};

inline constexpr struct {
    template <typename R, typename OutIt, typename UnaryPredicate>
    auto operator()(R &&r, OutIt dest, UnaryPredicate pred) const -> OutIt
    {
        return i::remove_move_if(adl::begin(r), adl::end(r), dest, astl::pass_fn(pred));
    }

    template <typename R, typename OutIt, typename UnaryPredicate, typename P>
    auto operator()(R &&r, OutIt dest, UnaryPredicate pred, P p) const -> OutIt
    {
        return i::remove_move_if(adl::begin(r), adl::end(r), dest, astl::pass_fn(pred),
                                 astl::pass_fn(p));
    }
} remove_move_if{};

inline constexpr struct {
    template <typename R1, typename R2, typename BinaryPredicate = std::equal_to<>>
    auto operator()(R1 &&r1, R2 &&r2, BinaryPredicate pred = std::equal_to{}) const
        -> iter_of_range<R1>
    {
        return i::remove_range(adl::begin(r1), adl::end(r1), adl::begin(r2), adl::end(r2),
                               astl::pass_fn(pred));
    }

    template <typename R1, typename R2, typename BinaryPredicate, typename P>
    auto operator()(R1 &&r1, R2 &&r2, BinaryPredicate pred, P p) const -> iter_of_range<R1>
    {
        return i::remove_range(adl::begin(r1), adl::end(r1), adl::begin(r2), adl::end(r2),
                               astl::pass_fn(pred), astl::pass_fn(p));
    }
} remove_range{};

inline constexpr struct {
    template <typename R, typename UnaryPredicate>
    auto operator()(R &&r, UnaryPredicate pred) const -> iter_of_range<R>
    {
        return i::unstable_remove_if(adl::begin(r), adl::end(r), astl::pass_fn(pred));
    }

    template <typename R, typename UnaryPredicate, typename P>
    auto operator()(R &&r, UnaryPredicate pred, P p) const -> iter_of_range<R>
    {
        return i::unstable_remove_if(adl::begin(r), adl::end(r), astl::pass_fn(pred),
                                     astl::pass_fn(p));
    }
} unstable_remove_if{};

} // namespace r
} // namespace astl

#endif // ASTL_INCLUDE_REMOVE_HPP
