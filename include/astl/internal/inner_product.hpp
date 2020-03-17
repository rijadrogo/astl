//
// Created by Rijad on 04-Aug-18.
//

#ifndef ASTL_INCLUDE_INNER_PRODUCT_HPP
#define ASTL_INCLUDE_INNER_PRODUCT_HPP

#include <numeric>

#include "astl/functional.hpp"
#include "astl/range_access.hpp"

namespace astl
{
namespace i
{
inline constexpr struct {
    template <typename InIt1, typename InIt2, typename T, typename BinaryOp1 = std::plus<>,
              typename BinaryOp2 = std::multiplies<>>
    ASTL_NODISCARD auto operator()(InIt1 first1, InIt1 last1, InIt2 first2, T init,
                                   BinaryOp1 binary_op1, BinaryOp2 binary_op2) const -> T
    {
        return std::inner_product(
            first1, last1, first2, std::move(init),
            // NOLINTNEXTLINE(bugprone-move-forwarding-reference)
            [op1(astl::pass_fn(binary_op1))](auto &&x, auto &&y) { return op1(std::move(x), y); },
            astl::pass_fn(binary_op2));
    }

    template <typename InIt1, typename InIt2, typename T, typename BinaryOp1, typename BinaryOp2,
              typename P1, typename P2>
    ASTL_NODISCARD auto operator()(InIt1 first1, InIt1 last1, InIt2 first2, T init,
                                   BinaryOp1 binary_op1, BinaryOp2 binary_op2, P1 p1, P2 p2) const
        -> T
    {
        while (first1 != last1) {
            init =
                binary_op1(std::move(init), binary_op2(invoke(p1, *first1), invoke(p2, *first2)));
            ++first1;
            ++first2;
        }
        return init;
    }
} inner_product{};

inline constexpr struct {
    template <typename InIt1, typename N, typename InIt2, typename T,
              typename BinaryOp1 = std::plus<>, typename BinaryOp2 = std::multiplies<>>
    // requires InIt1 InputIterator
    // requires InIt2 InputIterator
    // requires N integral type
    // requires T copy assignable
    // requires BinaryOp1, returns T, arguments T and ResultType(BinaryOp2)
    // requires BinaryOp2, two arguments value_type(InIt1) and value_type(InIt2)
    ASTL_NODISCARD auto operator()(InIt1 first1, N n, InIt2 first2, T init,
                                   BinaryOp1 binary_op1 = BinaryOp1{},
                                   BinaryOp2 binary_op2 = BinaryOp2{}) const -> T
    {
        while (n != N(0)) {
            init = binary_op1(std::move(init), binary_op2(*first1, *first2));
            ++first1;
            ++first2;
            --n;
        }
        return init;
    }

    template <typename InIt1, typename N, typename InIt2, typename T, typename BinaryOp1,
              typename BinaryOp2, typename P1, typename P2>
    ASTL_NODISCARD auto operator()(InIt1 first1, N n, InIt2 first2, T init, BinaryOp1 binary_op1,
                                   BinaryOp2 binary_op2, P1 p1, P2 p2) const -> T
    {
        while (n != N(0)) {
            init =
                binary_op1(std::move(init), binary_op2(invoke(p1, *first1), invoke(p2, *first2)));
            ++first1;
            ++first2;
            --n;
        }
        return init;
    }
} inner_product_n{};
} // namespace i

namespace r
{
inline constexpr struct {
    template <typename R1, typename R2, typename T, typename BinaryOp1 = std::plus<>,
              typename BinaryOp2 = std::multiplies<>>
    ASTL_NODISCARD auto operator()(R1 &&r1, R2 &&r2, T init, BinaryOp1 binary_op1 = BinaryOp1{},
                                   BinaryOp2 binary_op2 = BinaryOp2{}) const -> T
    {
        return i::inner_product(adl::begin(r1), adl::end(r1), adl::begin(r2), std::move(init),
                                astl::pass_fn(binary_op1), astl::pass_fn(binary_op2));
    }

    template <typename R1, typename R2, typename T, typename BinaryOp1, typename BinaryOp2,
              typename P1, typename P2>
    ASTL_NODISCARD auto operator()(R1 &&r1, R2 &&r2, T init, BinaryOp1 binary_op1,
                                   BinaryOp2 binary_op2, P1 p1, P2 p2) const -> T
    {
        return i::inner_product(adl::begin(r1), adl::end(r1), adl::begin(r2), std::move(init),
                                astl::pass_fn(binary_op1), astl::pass_fn(binary_op2),
                                astl::pass_fn(p1), astl::pass_fn(p2));
    }
} inner_product{};
inline constexpr struct {

    template <typename R1, typename N, typename R2, typename T, typename BinaryOp1 = std::plus<>,
              typename BinaryOp2 = std::multiplies<>>
    // requires R1 InputIterator range
    // requires R2 InputIterator range
    // requires N integral type
    // requires T copy assignable
    // requires BinaryOp1, returns T, arguments T and ResultType(BinaryOp2)
    // requires BinaryOp2, two arguments value_type(InIt1) and value_type(InIt2)
    ASTL_NODISCARD auto operator()(R1 &&r1, N n, R2 &&r2, T init,
                                   BinaryOp1 binary_op1 = BinaryOp1{},
                                   BinaryOp2 binary_op2 = BinaryOp2{}) const -> T
    {
        return i::inner_product_n(adl::begin(r1), n, adl::begin(r2), std::move(init),
                                  astl::pass_fn(binary_op1), astl::pass_fn(binary_op2));
    }

    template <typename R1, typename N, typename R2, typename T, typename BinaryOp1,
              typename BinaryOp2, typename P1, typename P2>
    ASTL_NODISCARD auto operator()(R1 &&r1, N n, R2 &&r2, T init, BinaryOp1 binary_op1,
                                   BinaryOp2 binary_op2, P1 p1, P2 p2) const -> T
    {
        return i::inner_product_n(adl::begin(r1), n, adl::begin(r2), std::move(init),
                                  astl::pass_fn(binary_op1), astl::pass_fn(binary_op2),
                                  astl::pass_fn(p1), astl::pass_fn(p2));
    }
} inner_product_n{};
} // namespace r
} // namespace astl

#endif // ASTL_INCLUDE_INNER_PRODUCT_HPP
