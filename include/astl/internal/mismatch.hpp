//
// Created by Rijad on 29-Jul-18.
//

#ifndef ASTL_INCLUDE_MISMATCH_HPP
#define ASTL_INCLUDE_MISMATCH_HPP

#include <algorithm>
#include <type_traits>
#include <utility>

#include "astl/functional.hpp"
#include "astl/iterator.hpp"
#include "astl/range_access.hpp"

namespace astl
{
namespace i
{

inline constexpr struct {

    template <typename InIt1, typename InIt2, typename BinaryPredicate = std::equal_to<>>
    ASTL_NODISCARD auto operator()(InIt1 first1, InIt1 last1, InIt2 first2, InIt2 last2,
                                   BinaryPredicate pred = BinaryPredicate{}) const
        -> std::pair<InIt1, InIt2>
    {
        return std::mismatch(first1, last1, first2, last2, astl::pass_fn(pred));
    }

    template <typename InIt1, typename InIt2, typename BinaryPredicate = std::equal_to<>>
    ASTL_NODISCARD auto operator()(InIt1 first1, InIt1 last1, InIt2 first2,
                                   BinaryPredicate pred = BinaryPredicate{}) const
        -> std::pair<InIt1, InIt2>
    {
        return std::mismatch(first1, last1, first2, astl::pass_fn(pred));
    }

    template <typename InIt1, typename InIt2, typename BinaryPredicate, typename P1, typename P2>
    ASTL_NODISCARD auto operator()(InIt1 first1, InIt1 last1, InIt2 first2, InIt2 last2,
                                   BinaryPredicate pred, P1 p1, P2 p2) const
        -> std::pair<InIt1, InIt2>
    {
        return std::mismatch(
            first1, last1, first2, last2,
            astl::lockstep(astl::pass_fn(pred), astl::pass_fn(p1), astl::pass_fn(p2)));
    }

    template <typename InIt1, typename InIt2, typename BinaryPredicate, typename P1, typename P2>
    ASTL_NODISCARD auto operator()(InIt1 first1, InIt1 last1, InIt2 first2, BinaryPredicate pred,
                                   P1 p1, P2 p2) const -> std::pair<InIt1, InIt2>
    {
        return std::mismatch(
            first1, last1, first2,
            astl::lockstep(astl::pass_fn(pred), astl::pass_fn(p1), astl::pass_fn(p2)));
    }

} mismatch{};

inline constexpr struct {
    template <typename InIt1, typename N, typename InIt2,
              typename BinaryPredicate = std::equal_to<>>
    ASTL_NODISCARD auto operator()(InIt1 first1, N n1, InIt2 first2, N n2,
                                   BinaryPredicate pred = BinaryPredicate{}) const
        -> std::pair<InIt1, InIt2>
    {
        while (n1 != N(0) && n2 != N(0) && pred(*first1, *first2)) {
            ++first1;
            ++first2;
            --n1;
            --n2;
        }
        return std::make_pair(first1, first2);
    }

    template <typename InIt1, typename N, typename InIt2, typename BinaryPredicate, typename P1,
              typename P2>
    ASTL_NODISCARD auto operator()(InIt1 first1, N n1, InIt2 first2, N n2, BinaryPredicate pred,
                                   P1 p1, P2 p2) const -> std::pair<InIt1, InIt2>
    {
        return (*this)(first1, n1, first2, n2,
                       astl::lockstep(astl::pass_fn(pred), astl::pass_fn(p1), astl::pass_fn(p2)));
    }
} mismatch_n{};

} // namespace i

namespace r
{

inline constexpr struct {
    template <typename R1, typename R2, typename BinaryPredicate = std::equal_to<>>
    ASTL_NODISCARD auto operator()(R1 &&r1, R2 &&r2, BinaryPredicate pred = BinaryPredicate{}) const
        -> std::pair<astl::iter_of_range<R1>, astl::iter_of_range<R2>>
    {
        return i::mismatch(adl::begin(r1), adl::end(r1), adl::begin(r2), adl::end(r2),
                           astl::pass_fn(pred));
    }

    template <typename R1, typename R2, typename BinaryPredicate, typename P1, typename P2>
    ASTL_NODISCARD auto operator()(R1 &&r1, R2 &&r2, BinaryPredicate pred, P1 p1, P2 p2) const
        -> std::pair<astl::iter_of_range<R1>, astl::iter_of_range<R2>>
    {
        return i::mismatch(adl::begin(r1), adl::end(r1), adl::begin(r2), adl::end(r2),
                           astl::pass_fn(pred), astl::pass_fn(p1), astl::pass_fn(p2));
    }
} mismatch{};

inline constexpr struct {
    template <typename R1, typename N1, typename R2, typename N2,
              typename BinaryPredicate = std::equal_to<>>
    ASTL_NODISCARD auto operator()(R1 &&r1, N1 n1, R2 &&r2, N2 n2,
                                   BinaryPredicate pred = BinaryPredicate{}) const
        -> std::pair<astl::iter_of_range<R1>, astl::iter_of_range<R2>>
    {
        using Ct = typename std::common_type<N1, N2>::type;
        return i::mismatch_n(adl::begin(r1), Ct(n1), adl::begin(r2), Ct(n2), astl::pass_fn(pred));
    }

    template <typename R1, typename N1, typename R2, typename N2, typename BinaryPredicate,
              typename P1, typename P2>
    ASTL_NODISCARD auto operator()(R1 &&r1, N1 n1, R2 &&r2, N2 n2, BinaryPredicate pred, P1 p1,
                                   P2 p2) const
        -> std::pair<astl::iter_of_range<R1>, astl::iter_of_range<R2>>
    {
        using Ct = typename std::common_type<N1, N2>::type;
        return i::mismatch_n(adl::begin(r1), Ct(n1), adl::begin(r2), Ct(n2), astl::pass_fn(pred),
                             astl::pass_fn(p1), astl::pass_fn(p2));
    }
} mismatch_n{};

} // namespace r
} // namespace astl

#endif // ASTL_INCLUDE_MISMATCH_HPP
