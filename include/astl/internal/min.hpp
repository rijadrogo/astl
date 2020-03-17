//
// Created by Rijad on 17-Aug-19.
//

#ifndef ASTL_INCLUDE_MIN_HPP
#define ASTL_INCLUDE_MIN_HPP

#include <algorithm>
#include <initializer_list>
#include <utility>

#include "astl/functional.hpp"
#include "astl/iterator.hpp"
#include "astl/range_access.hpp"

namespace astl
{

template <typename T> ASTL_NODISCARD auto min(std::initializer_list<T> ilist) -> T
{
    return *std::min_element(ilist.begin(), ilist.end());
}

template <typename T> ASTL_NODISCARD auto min(T &a, T &b) noexcept -> T &
{
    return !(b < a) ? a : b;
}

template <typename T> ASTL_NODISCARD auto min(T const &a, T const &b) noexcept -> T const &
{
    return !(b < a) ? a : b;
}

template <typename T, typename Comparator>
ASTL_NODISCARD auto min(T &a, T &b, Comparator comp) -> T &
{
    return !(comp(b, a)) ? a : b;
}

template <typename T, typename Comparator>
ASTL_NODISCARD auto min(T const &a, T const &b, Comparator comp) -> T const &
{
    return !(comp(b, a)) ? a : b;
}

template <typename T, typename Comparator, typename P>
ASTL_NODISCARD auto min(T &a, T &b, Comparator comp, P p) -> T &
{
    return !(comp(invoke(p, b), invoke(p, a))) ? a : b;
}

template <typename T, typename Comparator, typename P>
ASTL_NODISCARD auto min(T const &a, T const &b, Comparator comp, P p) -> T const &
{
    return !(comp(invoke(p, b), invoke(p, a))) ? a : b;
}

namespace i
{

inline constexpr struct {

    template <typename FwdIt, typename Comparator = std::less<>>
    ASTL_NODISCARD auto operator()(FwdIt first, FwdIt last, Comparator comp = Comparator{}) const
        -> FwdIt
    {
        return std::min_element(first, last, astl::pass_fn(comp));
    }

    template <typename FwdIt, typename Comparator, typename P>
    ASTL_NODISCARD auto operator()(FwdIt first, FwdIt last, Comparator comp, P p) const -> FwdIt
    {
        return std::min_element(first, last, astl::combine(astl::pass_fn(comp), astl::pass_fn(p)));
    }

} min_element{};

inline constexpr struct {

    template <typename FwdIt, typename N, typename Comparator = std::less<>>
    ASTL_NODISCARD auto operator()(FwdIt first, N n, Comparator comp = Comparator{}) const
        -> std::pair<FwdIt, N>
    {
        return i::max_element_n(first, n, astl::transpose(astl::pass_fn(comp)));
    }

    template <typename FwdIt, typename N, typename Comparator, typename P>
    ASTL_NODISCARD auto operator()(FwdIt first, N n, Comparator comp, P p) const
        -> std::pair<FwdIt, N>
    {
        return (*this)(first, n, astl::combine(astl::pass_fn(comp), astl::pass_fn(p)));
    }

} min_element_n{};

} // namespace i
namespace r
{

inline constexpr struct {

    template <typename R, typename Comparator, typename P>
    ASTL_NODISCARD auto operator()(R &&r, Comparator comp, P p) const -> iter_of_range<R>
    {
        return i::min_element(adl::begin(r), adl::end(r), astl::pass_fn(comp), astl::pass_fn(p));
    }

    template <typename R, typename Comparator = std::less<>>
    ASTL_NODISCARD auto operator()(R &&r, Comparator comp = Comparator{}) const -> iter_of_range<R>
    {
        return i::min_element(adl::begin(r), adl::end(r), astl::pass_fn(comp));
    }

} min_element{};

inline constexpr struct {
    template <typename R, typename N, typename Comparator, typename P>
    ASTL_NODISCARD auto operator()(R &&r, N n, Comparator comp, P p) const
        -> std::pair<astl::iter_of_range<R>, N>
    {
        return i::min_element_n(adl::begin(r), n, astl::pass_fn(comp), astl::pass_fn(p));
    }

    template <typename R, typename N, typename Comparator = std::less<>>
    ASTL_NODISCARD auto operator()(R &&r, N n, Comparator comp = Comparator{}) const
        -> std::pair<astl::iter_of_range<R>, N>
    {
        return i::min_element_n(adl::begin(r), n, astl::pass_fn(comp));
    }

} min_element_n{};

} // namespace r
} // namespace astl

#endif // ASTL_INCLUDE_MIN_HPP
