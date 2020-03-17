//
// Created by Rijad on 30-Jul-18.
//

#ifndef ASTL_INCLUDE_FILL_HPP
#define ASTL_INCLUDE_FILL_HPP

#include <algorithm>

#include "astl/iterator.hpp"
#include "astl/range_access.hpp"

namespace astl
{
namespace i
{

inline constexpr struct {
    template <typename FwdIt, typename T>
    // requires FwdIt ForwardIterator
    // requires T, assignable to value_type(FwdIt)
    auto operator()(FwdIt first, FwdIt last, T const &val) const -> void
    {
        std::fill(first, last, val);
    }

} fill{};

inline constexpr struct {
    template <typename FwdIt, typename N, typename T>
    // requires FwdIt ForwardIterator range
    // requires N integral type
    // requires T, assignable to value_type(FwdIt)
    auto operator()(FwdIt first, N n, T const &val) const -> FwdIt
    {
        return std::fill_n(first, n, val);
    }
} fill_n{};

} // namespace i

namespace r
{
inline constexpr struct {
    template <typename R, typename T>
    // requires R ForwardIterator range
    // requires T, assignable to value_type(R)
    auto operator()(R &&r, T const &val) const -> void
    {
        std::fill(adl::begin(r), adl::end(r), val);
    }

} fill{};

inline constexpr struct {
    template <typename R, typename N, typename T>
    // requires R ForwardIterator range
    // requires N integral type
    // requires T, assignable to value_type(R)
    auto operator()(R &&r, N n, T const &val) const -> iter_of_range<R>
    {
        return std::fill_n(adl::begin(r), n, val);
    }
} fill_n{};

} // namespace r
} // namespace astl

#endif // ASTL_INCLUDE_FILL_HPP
