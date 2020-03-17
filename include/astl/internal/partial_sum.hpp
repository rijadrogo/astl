//
// Created by Rijad on 04-Aug-18.
//

#ifndef ASTL_INCLUDE_PARTIAL_SUM_HPP
#define ASTL_INCLUDE_PARTIAL_SUM_HPP

#include <numeric>
#include <utility>

#include "astl/functional.hpp"
#include "astl/iterator.hpp"
#include "astl/range_access.hpp"

namespace astl
{
namespace i
{
inline constexpr struct {

    template <typename InIt, typename OutIt, typename BinaryOp = std::plus<>>
    auto operator()(InIt first, InIt const last, OutIt dest, BinaryOp binary_op = BinaryOp{}) const
        -> OutIt
    {
        return std::partial_sum(first, last, dest,
                                [op(astl::pass_fn(binary_op))](auto &&x, auto &&y) {
                                    // NOLINTNEXTLINE(bugprone-move-forwarding-reference)
                                    return op(std::move(x), y);
                                });
    }

    template <typename InIt, typename OutIt, typename BinaryOp, typename P>
    auto operator()(InIt first, InIt const last, OutIt dest, BinaryOp binary_op, P p) const -> OutIt
    {
        if (first == last) return dest;

        auto sum(invoke(p, *first));
        *dest = sum;
        while (++first != last) {
            sum = binary_op(std::move(sum), invoke(p, *first));
            *++dest = sum;
        }
        ++dest;
        return dest;
    }

} partial_sum{};

inline constexpr struct {

    template <typename InIt, typename N, typename OutIt, typename BinaryOp = std::plus<>>
    auto operator()(InIt first, N n, OutIt dest, BinaryOp binary_op = BinaryOp{}) const
        -> std::pair<OutIt, InIt>
    {
        if (n == N(0)) return std::make_pair(dest, first);

        auto sum(*first);
        *dest = sum;
        while (--n != N(0)) {
            sum = binary_op(std::move(sum), *++first);
            *++dest = sum;
        }
        return std::make_pair(++dest, first);
    }

    template <typename InIt, typename N, typename OutIt, typename BinaryOp, typename P>
    auto operator()(InIt first, N n, OutIt dest, BinaryOp binary_op, P p) const
        -> std::pair<OutIt, InIt>
    {
        if (n == N(0)) return std::make_pair(dest, first);

        auto sum(invoke(p, *first));
        *dest = sum;
        while (--n != N(0)) {
            sum = binary_op(std::move(sum), invoke(p, *++first));
            *++dest = sum;
        }
        return std::make_pair(++dest, first);
    }

} partial_sum_n{};

} // namespace i

namespace r
{
inline constexpr struct {

    template <typename R, typename OutIt, typename BinaryOp = std::plus<>>
    auto operator()(R &&r, OutIt dest, BinaryOp binary_op = BinaryOp{}) const -> OutIt
    {
        return i::partial_sum(adl::begin(r), adl::end(r), dest, astl::pass_fn(binary_op));
    }

    template <typename R, typename OutIt, typename BinaryOp, typename P>
    auto operator()(R &&r, OutIt dest, BinaryOp binary_op, P p) const -> OutIt
    {
        return i::partial_sum(adl::begin(r), adl::end(r), dest, astl::pass_fn(binary_op),
                              astl::pass_fn(p));
    }

} partial_sum{};

inline constexpr struct {

    template <typename R, typename N, typename OutIt, typename BinaryOp = std::plus<>>
    auto operator()(R &&r, N n, OutIt dest, BinaryOp binary_op = BinaryOp{}) const
        -> std::pair<OutIt, astl::iter_of_range<R>>
    {
        return i::partial_sum_n(adl::begin(r), n, dest, astl::pass_fn(binary_op));
    }

    template <typename R, typename N, typename OutIt, typename BinaryOp, typename P>
    auto operator()(R &&r, N n, OutIt dest, BinaryOp binary_op, P p) const
        -> std::pair<OutIt, astl::iter_of_range<R>>
    {
        return i::partial_sum_n(adl::begin(r), n, dest, astl::pass_fn(binary_op), astl::pass_fn(p));
    }
} partial_sum_n{};

} // namespace r
} // namespace astl

#endif // ASTL_INCLUDE_PARTIAL_SUM_HPP
