//
// Created by Rijad on 04-Aug-18.
//

#ifndef ASTL_INCLUDE_ADJACENT_DIFFERENCE_HPP
#define ASTL_INCLUDE_ADJACENT_DIFFERENCE_HPP

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
    template <typename InIt, typename OutIt, typename BinaryOp>
    auto operator()(InIt first, InIt last, OutIt result, BinaryOp binary_op) const -> OutIt
    {
        return std::adjacent_difference(first, last, result,
                                        [op(astl::pass_fn(binary_op))](auto &&x, auto &&y) {
                                            // NOLINTNEXTLINE(bugprone-move-forwarding-reference)
                                            return op(std::move(x), y);
                                        });
    }

    template <typename InIt, typename OutIt, typename BinaryOp, typename P>
    auto operator()(InIt first, InIt last, OutIt result, BinaryOp binary_op, P p) const -> OutIt
    {
        return std::adjacent_difference(first, last, result,
                                        astl::combine(astl::pass_fn(binary_op), astl::pass_fn(p)));
    }
} adjacent_difference{};

inline constexpr struct {

    template <typename InIt, typename N, typename OutIt, typename BinaryOp = std::minus<>>
    // requires InIt InputIterator
    // requires N integral type
    // requires OutIt OutputIterator
    // requires BinaryOp, returns value_type(OutIt), two arguments value_type(InIt)
    auto operator()(InIt first, N n, OutIt result, BinaryOp binary_op = BinaryOp{}) const
        -> std::pair<OutIt, InIt>
    {
        if constexpr (is_forward_it_v<InIt>) { // Forward Iterator
            using FwdIt = InIt;
            if (n != N(0)) {
                *result = *first;
                ++result;
                FwdIt previous(first);
                ++first;
                while (--n != N(0)) {
                    *result = binary_op(*first, *previous);
                    previous = first;
                    ++first;
                    ++result;
                }
            }
            return std::make_pair(result, first);
        }
        else { // Input Iterator
            if (n != N(0)) {
                auto t1(*first);
                *result = t1;
                ++result;
                ++first;
                while (--n != N(0)) {
                    auto t2(*first);
                    *result = binary_op(t2, std::move(t1));
                    t1 = std::move(t2);
                    ++first;
                    ++result;
                }
            }
            return std::make_pair(result, first);
        }
    }

    template <typename InIt, typename N, typename OutIt, typename BinaryOp, typename P>
    auto operator()(InIt first, N n, OutIt result, BinaryOp binary_op, P p) const
        -> std::pair<OutIt, InIt>
    {
        return (*this)(first, n, result, astl::combine(astl::pass_fn(binary_op), astl::pass_fn(p)));
    }
} adjacent_difference_n{};

} // namespace i

namespace r
{

inline constexpr struct {

    template <typename R, typename OutIt, typename BinaryOp = std::minus<>>
    // requires R InputIterator range
    // requires OutIt OutputIterator
    // requires BinaryOp, returns value_type(OutIt), two arguments value_type(R)
    auto operator()(R &&r, OutIt result, BinaryOp binary_op = BinaryOp{}) const -> OutIt
    {
        return std::adjacent_difference(adl::begin(r), adl::end(r), result,
                                        astl::pass_fn(binary_op));
    }

    template <typename R, typename OutIt, typename BinaryOp, typename P>
    auto operator()(R &&r, OutIt result, BinaryOp binary_op, P p) const -> OutIt
    {
        return i::adjacent_difference(adl::begin(r), adl::end(r), result, astl::pass_fn(binary_op),
                                      astl::pass_fn(p));
    }

} adjacent_difference{};

inline constexpr struct {
    template <typename R, typename N, typename OutIt, typename BinaryOp = std::minus<>>
    // requires R InputIterator range
    // requires N integral type
    // requires OutIt OutputIterator
    // requires BinaryOp, returns value_type(OutIt), two arguments value_type(R)
    auto operator()(R &&r, N n, OutIt result, BinaryOp binary_op = BinaryOp{}) const
        -> std::pair<OutIt, astl::iter_of_range<R>>
    {
        return i::adjacent_difference_n(adl::begin(r), n, result, astl::pass_fn(binary_op));
    }

    template <typename R, typename N, typename OutIt, typename BinaryOp, typename P>
    auto operator()(R &&r, N n, OutIt result, BinaryOp binary_op, P p) const
        -> std::pair<OutIt, astl::iter_of_range<R>>
    {
        return i::adjacent_difference_n(adl::begin(r), n, result, astl::pass_fn(binary_op),
                                        astl::pass_fn(p));
    }
} adjacent_difference_n{};

} // namespace r
} // namespace astl

#endif // ASTL_INCLUDE_ADJACENT_DIFFERENCE_HPP
