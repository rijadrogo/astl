//
// Created by Rijad on 04-Aug-18.
//

#ifndef ASTL_INCLUDE_SCAN_HPP
#define ASTL_INCLUDE_SCAN_HPP

#include <utility>

#include "astl/functional.hpp"
#include "astl/iterator.hpp"
#include "astl/range_access.hpp"

namespace astl
{
namespace i
{

inline constexpr struct {
    template <typename InIt, typename OutIt, typename T>
    // requires InIt InputIterator
    // requires OutIt OutputIterator
    // requires T, to be convertible to value_type(InIt) and convertible to
    // value_type(OutIt)
    auto operator()(InIt first, InIt last, OutIt dest, T val) const -> OutIt
    {
        // set each value in [dest, dest + (last - first)) to the associative
        // reduction of predecessors and val
        while (first != last) {
            *dest = val;
            ++dest;
            val = val + *first;
            ++first;
        }
        return dest;
    }

    template <typename InIt, typename OutIt, typename T, typename BinOp>
    // requires InIt InputIterator
    // requires OutIt OutputIterator
    // requires T, to be convertible to value_type(InIt) and convertible to
    // value_type(OutIt) requires BinOp ??
    auto operator()(InIt first, InIt last, OutIt dest, T val, BinOp reduce_op) const -> OutIt
    {
        // set each value in [dest, dest + (last - first)) to the associative
        // reduction of predecessors and val
        while (first != last) {
            *dest = val;
            ++dest;
            val = reduce_op(std::move(val), *first);
            ++first;
        }
        return dest;
    }

    template <typename InIt, typename OutIt, typename T, typename BinOp, typename P>
    auto operator()(InIt first, InIt last, OutIt dest, T val, BinOp reduce_op, P p) const -> OutIt
    {
        // set each value in [dest, dest + (last - first)) to the associative
        // reduction of predecessors and val
        while (first != last) {
            *dest = val;
            ++dest;
            val = reduce_op(std::move(val), invoke(p, *first));
            ++first;
        }
        return dest;
    }
} exclusive_scan{};

inline constexpr struct {
    template <typename BidiIt, typename OutIt, typename T>
    // requires BidiIt BidirectionalIterator
    // requires OutIt OutputIterator
    // requires T, to be convertible to value_type(BidiIt) and convertible to
    // value_type(OutIt)
    auto operator()(BidiIt first, BidiIt last, OutIt dest, T val) const -> OutIt
    {
        return i::exclusive_scan(std::make_reverse_iterator(last),
                                 std::make_reverse_iterator(first), dest, std::move(val));
    }

    template <typename BidiIt, typename OutIt, typename T, typename BinOp>
    // requires BidiIt BidirectionalIterator
    // requires OutIt OutputIterator
    // requires T, to be convertible to value_type(BidiIt) and convertible to
    // value_type(OutIt) requires BinOp ??
    auto operator()(BidiIt first, BidiIt last, OutIt dest, T val, BinOp reduce_op) const -> OutIt
    {
        return i::exclusive_scan(std::make_reverse_iterator(last),
                                 std::make_reverse_iterator(first), dest, std::move(val),
                                 astl::pass_fn(reduce_op));
    }

    template <typename BidiIt, typename OutIt, typename T, typename BinOp, typename P>
    auto operator()(BidiIt first, BidiIt last, OutIt dest, T val, BinOp reduce_op, P p) const
        -> OutIt
    {
        return i::exclusive_scan(std::make_reverse_iterator(last),
                                 std::make_reverse_iterator(first), dest, std::move(val),
                                 astl::pass_fn(reduce_op), astl::pass_fn(p));
    }
} exclusive_scan_backward{};

inline constexpr struct {
    template <typename InIt, typename N, typename OutIt, typename T>
    // requires InIt InputIterator
    // requires OutIt OutputIterator
    // requires T, to be convertible to value_type(InIt) and convertible to
    // value_type(OutIt)
    auto operator()(InIt first, N n, OutIt dest, T val) const -> std::pair<OutIt, InIt>
    {
        // set each value in [dest, dest + (last - first)) to the associative
        // reduction of predecessors and val
        while (n != N(0)) {
            *dest = val;
            ++dest;
            val += *first;
            ++first;
            --n;
        }
        return std::make_pair(dest, first);
    }

    template <typename InIt, typename N, typename OutIt, typename T, typename BinOp>
    // requires InIt InputIterator
    // requires OutIt OutputIterator
    // requires T, to be convertible to value_type(InIt) and convertible to
    // value_type(OutIt) requires BinOp ??
    auto operator()(InIt first, N n, OutIt dest, T val, BinOp reduce_op) const
        -> std::pair<OutIt, InIt>
    {
        // set each value in [dest, dest + (last - first)) to the associative
        // reduction of predecessors and val
        while (n != N(0)) {
            *dest = val;
            ++dest;
            val = reduce_op(std::move(val), *first);
            ++first;
            --n;
        }
        return std::make_pair(dest, first);
    }

    template <typename InIt, typename N, typename OutIt, typename T, typename BinOp, typename P>
    auto operator()(InIt first, N n, OutIt dest, T val, BinOp reduce_op, P p) const
        -> std::pair<OutIt, InIt>
    {
        // set each value in [dest, dest + (last - first)) to the associative
        // reduction of predecessors and val
        while (n != N(0)) {
            *dest = val;
            ++dest;
            val = reduce_op(std::move(val), invoke(p, *first));
            ++first;
            --n;
        }
        return std::make_pair(dest, first);
    }
} exclusive_scan_n{};

inline constexpr struct {
    template <typename InIt, typename OutIt, typename BinOp = std::plus<>>
    auto operator()(InIt first, InIt last, OutIt dest, BinOp reduce_op = BinOp{}) const -> OutIt
    {
        // compute partial non commutative and associative reductions into dest, using
        // reduce_op
        if (first != last) {
            auto val(*first);
            while (true) {
                *dest = val;
                ++dest;
                ++first;
                if (first == last) break;

                val = reduce_op(std::move(val), *first);
            }
        }
        return dest;
    }

    template <typename InIt, typename OutIt, typename BinOp, typename T>
    auto operator()(InIt first, InIt last, OutIt dest, BinOp reduce_op, T val) const -> OutIt
    {
        // compute partial non commutative and associative reductions including val
        // into dest, using reduce_op
        while (first != last) {
            val = reduce_op(std::move(val), *first);
            *dest = val;
            ++first;
            ++dest;
        }
        return dest;
    }

    template <typename InIt, typename OutIt, typename BinOp, typename T, typename P>
    auto operator()(InIt first, InIt last, OutIt dest, BinOp reduce_op, T val, P p) const -> OutIt
    {
        // compute partial non commutative and associative reductions including val
        // into dest, using reduce_op
        while (first != last) {
            val = reduce_op(std::move(val), invoke(p, *first));
            *dest = val;
            ++first;
            ++dest;
        }
        return dest;
    }
} inclusive_scan{};

inline constexpr struct {

    template <typename BidiIt, typename OutIt, typename BinOp = std::plus<>>
    auto operator()(BidiIt first, BidiIt last, OutIt dest, BinOp reduce_op = BinOp{}) const -> OutIt
    {
        return i::inclusive_scan(std::make_reverse_iterator(last),
                                 std::make_reverse_iterator(first), dest, astl::pass_fn(reduce_op));
    }

    template <typename BidiIt, typename OutIt, typename BinOp, typename T>
    auto operator()(BidiIt first, BidiIt last, OutIt dest, BinOp reduce_op, T val) const -> OutIt
    {
        return i::inclusive_scan(std::make_reverse_iterator(last),
                                 std::make_reverse_iterator(first), dest, astl::pass_fn(reduce_op),
                                 std::move(val));
    }

    template <typename InIt, typename OutIt, typename BinOp, typename T, typename P>
    auto operator()(InIt first, InIt last, OutIt dest, BinOp reduce_op, T val, P p) const -> OutIt
    {
        return i::inclusive_scan(std::make_reverse_iterator(last),
                                 std::make_reverse_iterator(first), dest, astl::pass_fn(reduce_op),
                                 std::move(val), astl::pass_fn(p));
    }
} inclusive_scan_backward{};

inline constexpr struct {
    template <typename InIt, typename N, typename OutIt, typename BinOp = std::plus<>>
    auto operator()(InIt first, N n, OutIt dest, BinOp reduce_op = BinOp{}) const
        -> std::pair<OutIt, InIt>
    {
        // compute partial non commutative and associative reductions into dest, using
        // reduce_op
        if (n != N(0)) {
            auto val(*first);
            while (true) {
                *dest = val;
                ++dest;
                ++first;
                --n;
                if (n != N(0)) break;

                val = reduce_op(std::move(val), *first);
            }
        }
        return std::make_pair(dest, first);
    }

    template <typename InIt, typename N, typename OutIt, typename BinOp, typename T>
    auto operator()(InIt first, N n, OutIt dest, BinOp reduce_op, T val) const
        -> std::pair<OutIt, InIt>
    {
        // compute partial non commutative and associative reductions including val
        // into dest, using reduce_op
        while (n != N(0)) {
            val = reduce_op(std::move(val), *first);
            *dest = val;
            ++first;
            ++dest;
            --n;
        }

        return std::make_pair(dest, first);
    }

    template <typename InIt, typename N, typename OutIt, typename BinOp, typename T, typename P>
    auto operator()(InIt first, N n, OutIt dest, BinOp reduce_op, T val, P p) const
        -> std::pair<OutIt, InIt>
    {
        // compute partial non commutative and associative reductions including val
        // into dest, using reduce_op
        while (n != N(0)) {
            val = reduce_op(std::move(val), invoke(p, *first));
            *dest = val;
            ++first;
            ++dest;
            --n;
        }
        return std::make_pair(dest, first);
    }
} inclusive_scan_n{};

} // namespace i

namespace r
{

inline constexpr struct {

    template <typename R, typename OutIt, typename T, typename BinOp = std::plus<>>
    auto operator()(R &&r, OutIt dest, T val, BinOp reduce_op = BinOp{}) const -> OutIt
    {
        return i::exclusive_scan(adl::begin(r), adl::end(r), dest, std::move(val),
                                 astl::pass_fn(reduce_op));
    }

    template <typename R, typename OutIt, typename T, typename BinOp, typename P>
    auto operator()(R &&r, OutIt dest, T val, BinOp reduce_op, P p) -> OutIt
    {
        return i::exclusive_scan(adl::begin(r), adl::end(r), dest, std::move(val),
                                 astl::pass_fn(reduce_op), astl::pass_fn(p));
    }
} exclusive_scan{};

inline constexpr struct {

    template <typename R, typename OutIt, typename T, typename BinOp = std::plus<>>
    auto operator()(R &&r, OutIt dest, T val, BinOp reduce_op = BinOp{}) const -> OutIt
    {
        return i::exclusive_scan_backward(adl::begin(r), adl::end(r), dest, std::move(val),
                                          astl::pass_fn(reduce_op));
    }

    template <typename R, typename OutIt, typename T, typename BinOp, typename P>
    auto operator()(R &&r, OutIt dest, T val, BinOp reduce_op, P p) const -> OutIt
    {
        return i::exclusive_scan_backward(adl::begin(r), adl::end(r), dest, std::move(val),
                                          astl::pass_fn(reduce_op), astl::pass_fn(p));
    }
} exclusive_scan_backward{};

inline constexpr struct {

    template <typename R, typename N, typename OutIt, typename T, typename BinOp = std::plus<>>
    auto operator()(R &&r, N n, OutIt dest, T val, BinOp reduce_op = BinOp{}) const
        -> std::pair<OutIt, astl::iter_of_range<R>>
    {
        return i::exclusive_scan_n(adl::begin(r), n, dest, std::move(val),
                                   astl::pass_fn(reduce_op));
    }

    template <typename R, typename N, typename OutIt, typename T, typename BinOp, typename P>
    auto operator()(R &&r, N n, OutIt dest, T val, BinOp reduce_op, P p) const
        -> std::pair<OutIt, astl::iter_of_range<R>>
    {
        return i::exclusive_scan_n(adl::begin(r), n, dest, std::move(val), astl::pass_fn(reduce_op),
                                   astl::pass_fn(p));
    }
} exclusive_scan_n{};

inline constexpr struct {

    template <typename R, typename OutIt, typename BinOp = std::plus<>>
    auto operator()(R &&r, OutIt dest, BinOp reduce_op = BinOp{}) const -> OutIt
    {
        return i::inclusive_scan(adl::begin(r), adl::end(r), dest, astl::pass_fn(reduce_op));
    }

    template <typename R, typename OutIt, typename BinOp, typename T>
    auto operator()(R &&r, OutIt dest, BinOp reduce_op, T val) const -> OutIt
    {
        return i::inclusive_scan(adl::begin(r), adl::end(r), dest, astl::pass_fn(reduce_op),
                                 std::move(val));
    }

    template <typename R, typename OutIt, typename BinOp, typename T, typename P>
    auto operator()(R &&r, OutIt dest, BinOp reduce_op, T val, P p) const -> OutIt
    {
        return i::inclusive_scan(adl::begin(r), adl::end(r), dest, astl::pass_fn(reduce_op),
                                 std::move(val), astl::pass_fn(p));
    }
} inclusive_scan{};

inline constexpr struct {

    template <typename R, typename OutIt, typename BinOp = std::plus<>>
    auto operator()(R &&r, OutIt dest, BinOp reduce_op = BinOp{}) const -> OutIt
    {
        return i::inclusive_scan_backward(adl::begin(r), adl::end(r), dest,
                                          astl::pass_fn(reduce_op));
    }

    template <typename R, typename OutIt, typename BinOp, typename T>
    auto operator()(R &&r, OutIt dest, BinOp reduce_op, T val) const -> OutIt
    {
        return i::inclusive_scan_backward(adl::begin(r), adl::end(r), dest,
                                          astl::pass_fn(reduce_op), std::move(val));
    }

    template <typename R, typename OutIt, typename BinOp, typename T, typename P>
    auto operator()(R &&r, OutIt dest, BinOp reduce_op, T val, P p) const -> OutIt
    {
        return i::inclusive_scan_backward(adl::begin(r), adl::end(r), dest,
                                          astl::pass_fn(reduce_op), std::move(val),
                                          astl::pass_fn(p));
    }
} inclusive_scan_backward{};

inline constexpr struct {

    template <typename R, typename N, typename OutIt, typename BinOp = std::plus<>>
    auto operator()(R &&r, N n, OutIt dest, BinOp reduce_op = BinOp{}) const
        -> std::pair<OutIt, astl::iter_of_range<R>>
    {
        return i::inclusive_scan_n(adl::begin(r), n, dest, astl::pass_fn(reduce_op));
    }

    template <typename R, typename N, typename OutIt, typename BinOp, typename T>
    auto operator()(R &&r, N n, OutIt dest, BinOp reduce_op, T val) const
        -> std::pair<OutIt, astl::iter_of_range<R>>
    {
        return i::inclusive_scan_n(adl::begin(r), n, dest, astl::pass_fn(reduce_op),
                                   std::move(val));
    }

    template <typename R, typename N, typename OutIt, typename BinOp, typename T, typename P>
    auto operator()(R &&r, N n, OutIt dest, BinOp reduce_op, T val, P p) const
        -> std::pair<OutIt, astl::iter_of_range<R>>
    {
        return i::inclusive_scan_n(adl::begin(r), n, dest, astl::pass_fn(reduce_op), std::move(val),
                                   astl::pass_fn(p));
    }
} inclusive_scan_n{};

} // namespace r
} // namespace astl

#endif // ASTL_INCLUDE_SCAN_HPP
