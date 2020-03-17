//
// Created by Rijad on 04-Aug-18.
//

#ifndef ASTL_INCLUDE_ACCUMULATE_HPP
#define ASTL_INCLUDE_ACCUMULATE_HPP

#include <numeric>
#include <utility>

#include "reduce.hpp"

#include "astl/functional.hpp"
#include "astl/iterator.hpp"
#include "astl/range_access.hpp"

namespace astl
{
namespace i
{
inline constexpr struct {

    template <typename InIt, typename T, typename BinaryOp = std::plus<>>
    ASTL_NODISCARD auto operator()(InIt first, InIt const last, T init,
                                   BinaryOp op = BinaryOp{}) const -> T
    {
        return i::reduce(first, last, std::move(init), astl::pass_fn(op));
    }

    template <typename InIt, typename T, typename BinaryOp, typename P>
    ASTL_NODISCARD auto operator()(InIt first, InIt const last, T init, BinaryOp op, P p) const -> T
    {
        return i::reduce(first, last, std::move(init), astl::pass_fn(op), astl::pass_fn(p));
    }

} accumulate{};

inline constexpr struct {

    template <typename BidiIt, typename T, typename BinaryOp = std::plus<>>
    ASTL_NODISCARD auto operator()(BidiIt first, BidiIt last, T init,
                                   BinaryOp op = BinaryOp{}) const -> T
    {
        return std::accumulate(std::make_reverse_iterator(last), std::make_reverse_iterator(first),
                               std::move(init), astl::pass_fn(op));
    }

    template <typename BidiIt, typename T, typename BinaryOp, typename P>
    ASTL_NODISCARD auto operator()(BidiIt first, BidiIt last, T init, BinaryOp op, P p) const -> T
    {
        return (*this)(first, last, std::move(init),
                       astl::combine(astl::pass_fn(op), astl::pass_fn(p)));
    }

} accumulate_backward{};

inline constexpr struct {

    template <typename InIt, typename N, typename T, typename BinaryOp = std::plus<>>
    ASTL_NODISCARD auto operator()(InIt first, N n, T init, BinaryOp op = BinaryOp{}) const
        -> std::pair<T, InIt>
    {
        return i::reduce_n(first, n, std::move(init), astl::pass_fn(op));
    }

    template <typename InIt, typename N, typename T, typename BinaryOp, typename P>
    ASTL_NODISCARD auto operator()(InIt first, N n, T init, BinaryOp op, P p) const
        -> std::pair<T, InIt>
    {
        return i::reduce_n(first, n, std::move(init), astl::pass_fn(op), astl::pass_fn(p));
    }
} accumulate_n{};

} // namespace i

namespace r
{

inline constexpr struct {

    template <typename R, typename T, typename BinaryOp = std::plus<>>
    ASTL_NODISCARD auto operator()(R &&r, T init, BinaryOp op = BinaryOp{}) const -> T
    {
        return i::accumulate(adl::begin(r), adl::end(r), std::move(init), astl::pass_fn(op));
    }

    template <typename R, typename T, typename BinaryOp, typename P>
    ASTL_NODISCARD auto operator()(R &&r, T init, BinaryOp op, P p) const -> T
    {
        return i::accumulate(adl::begin(r), adl::end(r), std::move(init), astl::pass_fn(op),
                             astl::pass_fn(p));
    }
} accumulate{};

inline constexpr struct {

    template <typename R, typename T, typename BinaryOp = std::plus<>>
    ASTL_NODISCARD auto operator()(R &&r, T init, BinaryOp op = BinaryOp{}) const -> T
    {
        return i::accumulate_backward(adl::begin(r), adl::end(r), std::move(init),
                                      astl::pass_fn(op));
    }

    template <typename R, typename T, typename BinaryOp, typename P>
    ASTL_NODISCARD auto operator()(R &&r, T init, BinaryOp op, P p) const -> T
    {
        return i::accumulate_backward(adl::begin(r), adl::end(r), std::move(init),
                                      astl::pass_fn(op), astl::pass_fn(p));
    }
} accumulate_backward{};

inline constexpr struct {
    template <typename R, typename N, typename T>
    ASTL_NODISCARD auto operator()(R &&r, N n, T init) const -> std::pair<T, iter_of_range<R>>
    {
        return i::accumulate_n(adl::begin(r), n, std::move(init));
    }

    template <typename R, typename N, typename T, typename BinaryOp = std::plus<>>
    ASTL_NODISCARD auto operator()(R &&r, N n, T init, BinaryOp op = BinaryOp{}) const
        -> std::pair<T, iter_of_range<R>>
    {
        return i::accumulate_n(adl::begin(r), n, std::move(init), astl::pass_fn(op));
    }

    template <typename R, typename N, typename T, typename BinaryOp, typename P>
    ASTL_NODISCARD auto operator()(R &&r, N n, T init, BinaryOp op, P p) const
        -> std::pair<T, iter_of_range<R>>
    {
        return i::accumulate_n(adl::begin(r), n, std::move(init), astl::pass_fn(op),
                               astl::pass_fn(p));
    }
} accumulate_n{};

} // namespace r
} // namespace astl

#endif // ASTL_INCLUDE_ACCUMULATE_HPP
