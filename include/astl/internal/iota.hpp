//
// Created by Rijad on 04-Aug-18.
//

#ifndef ASTL_INCLUDE_IOTA_HPP
#define ASTL_INCLUDE_IOTA_HPP

#include <numeric>

#include "astl/functional.hpp"
#include "astl/iterator.hpp"
#include "astl/range_access.hpp"

namespace astl
{
namespace i
{

inline constexpr struct {

    template <typename FwdIt, typename T>
    auto operator()(FwdIt first, FwdIt last, T value) const -> void
    {
        std::iota(first, last, value);
    }

    template <typename FwdIt, typename T, typename P>
    auto operator()(FwdIt first, FwdIt last, T value, P p) const -> void
    {
        while (first != last) {
            *first = invoke(p, value);
            ++first;
            ++value;
        }
    }
} iota{};

inline constexpr struct {
    template <typename FwdIt, typename N, typename T>
    // requires FwdIt ForwardIterator
    // requires N integral type
    // requires T assignable to value_type(FwdIt), incrementable
    auto operator()(FwdIt first, N n, T value) const -> FwdIt
    {
        while (n != N(0)) {
            *first = value;
            ++first;
            ++value;
            --n;
        }
        return first;
    }

    template <typename FwdIt, typename N, typename T, typename P>
    auto operator()(FwdIt first, N n, T value, P p) const -> FwdIt
    {
        while (n != N(0)) {
            *first = invoke(p, value);
            ++first;
            ++value;
            --n;
        }
        return first;
    }
} iota_n{};

inline constexpr struct {
    template <typename BidiIt, typename T>
    // requires BidiIt BidirectionalIterator
    // requires T assignable to value_type(FwdIt), incrementable
    auto operator()(BidiIt first, BidiIt last, T value) const -> void
    {
        if (last != first)
            while (true) {
                *--last = value;
                ++value;
                if (first == last) return;
            }
    }

    template <typename BidiIt, typename T, typename P>
    auto operator()(BidiIt first, BidiIt last, T value, P p) const -> void
    {
        if (first != last)
            while (true) {
                *--last = invoke(p, value);
                ++value;
                if (first == last) return;
            }
    }
} iota_reverse{};
} // namespace i

namespace r
{

inline constexpr struct {
    template <typename R, typename T>
    // requires R ForwardIterator range
    // requires T assignable to value_type(R), incrementable
    auto operator()(R &&r, T value) -> void
    {
        i::iota(adl::begin(r), adl::end(r), std::move(value));
    }

    template <typename R, typename T, typename P> auto operator()(R &&r, T value, P p) const -> void
    {
        i::iota(adl::begin(r), adl::end(r), std::move(value), astl::pass_fn(p));
    }

} iota{};

inline constexpr struct {
    template <typename R, typename N, typename T>
    // requires R ForwardIterator range
    // requires N integral type
    // requires T assignable to value_type(R), incrementable
    auto operator()(R &&r, N n, T value) const -> iter_of_range<R>
    {
        return i::iota_n(adl::begin(r), n, std::move(value));
    }

    template <typename R, typename N, typename T, typename P>
    auto operator()(R &&r, N n, T value, P p) const -> iter_of_range<R>
    {
        return i::iota_n(adl::begin(r), n, std::move(value), astl::pass_fn(p));
    }

} iota_n{};

inline constexpr struct {

    template <typename R, typename T>
    // requires R ForwardIterator range
    // requires T assignable to value_type(R), incrementable
    auto operator()(R &&r, T value) const -> void
    {
        i::iota_reverse(adl::begin(r), adl::end(r), std::move(value));
    }

    template <typename R, typename T, typename P> auto operator()(R &&r, T value, P p) const -> void
    {
        i::iota_reverse(adl::begin(r), adl::end(r), std::move(value), astl::pass_fn(p));
    }
} iota_reverse{};

} // namespace r
} // namespace astl

#endif // ASTL_INCLUDE_IOTA_HPP
