//
// Created by Rijad on 29-Jul-18.
//

#ifndef ASTL_INCLUDE_ADJACENT_FIND_HPP
#define ASTL_INCLUDE_ADJACENT_FIND_HPP

#include <algorithm>
#include <utility>

#include "astl/functional.hpp"
#include "astl/iterator.hpp"
#include "astl/range_access.hpp"

namespace astl
{
namespace i
{

inline constexpr struct {
    template <typename FwdIt, typename BinaryPredicate = std::equal_to<>>
    ASTL_NODISCARD auto operator()(FwdIt first, FwdIt last,
                                   BinaryPredicate pred = BinaryPredicate{}) const -> FwdIt
    {
        return std::adjacent_find(first, last, astl::pass_fn(pred));
    }

    template <typename FwdIt, typename BinaryPredicate, typename P>
    ASTL_NODISCARD auto operator()(FwdIt first, FwdIt last, BinaryPredicate pred, P p) const
        -> FwdIt
    {
        return std::adjacent_find(first, last,
                                  astl::combine(astl::pass_fn(pred), astl::pass_fn(p)));
    }
} adjacent_find{};

inline constexpr struct {
    template <typename FwdIt, typename N, typename BinaryPredicate = std::equal_to<>>
    // requires FwdIt ForwardIterator
    // requires N integral type
    // requires BinaryPredicate, returns bool, arguments tow value_type(FwdIt)
    ASTL_NODISCARD auto operator()(FwdIt first, N n, BinaryPredicate pred = BinaryPredicate{}) const
        -> std::pair<FwdIt, N>
    {
        std::pair<FwdIt, N> p(first, n);
        if (n != N(0)) {
            FwdIt next(p.first);
            ++next;
            while (--p.second != N(0)) {
                if (pred(*p.first, *next)) return p;

                p.first = next;
                ++next;
            }
            p.second = n;
        }
        return p;
    }

    template <typename FwdIt, typename N, typename BinaryPredicate, typename P>
    ASTL_NODISCARD auto operator()(FwdIt first, N n, BinaryPredicate pred, P p) const
        -> std::pair<FwdIt, N>
    {
        return (*this)(first, n, astl::combine(astl::pass_fn(pred), astl::pass_fn(p)));
    }
} adjacent_find_n{};

} // namespace i

namespace r
{
inline constexpr struct {

    template <typename R, typename BinaryPredicate = std::equal_to<>>
    // requires R ForwardIterator range
    // requires BinaryPredicate, returns bool, arguments tow value_type(R)
    ASTL_NODISCARD auto operator()(R &&r, BinaryPredicate pred = BinaryPredicate{}) const
        -> iter_of_range<R>
    {
        return std::adjacent_find(adl::begin(r), adl::end(r), astl::pass_fn(pred));
    }

    template <typename R, typename BinaryPredicate, typename P>
    ASTL_NODISCARD auto operator()(R &&r, BinaryPredicate pred, P p) const -> iter_of_range<R>
    {
        return i::adjacent_find(adl::begin(r), adl::end(r), astl::pass_fn(pred), astl::pass_fn(p));
    }

} adjacent_find{};

inline constexpr struct {
    template <typename R, typename N, typename BinaryPredicate = std::equal_to<>>
    // requires R ForwardIterator range
    // requires N integral type
    // requires BinaryPredicate, returns bool, arguments tow value_type(R)
    ASTL_NODISCARD auto operator()(R &&r, N n, BinaryPredicate pred = BinaryPredicate{}) const
        -> std::pair<astl::iter_of_range<R>, N>
    {
        return i::adjacent_find_n(adl::begin(r), n, astl::pass_fn(pred));
    }

    template <typename R, typename N, typename BinaryPredicate, typename P>
    ASTL_NODISCARD auto operator()(R &&r, N n, BinaryPredicate pred, P p) const
        -> std::pair<astl::iter_of_range<R>, N>
    {
        return i::adjacent_find_n(adl::begin(r), n, astl::pass_fn(pred), astl::pass_fn(p));
    }
} adjacent_find_n{};

} // namespace r
} // namespace astl

#endif // ASTL_INCLUDE_ADJACENT_FIND_HPP
