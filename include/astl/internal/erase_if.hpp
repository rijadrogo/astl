//
// Created by Rijad on 06-Aug-18.
//

#ifndef ASTL_INCLUDE_ERASE_IF_HPP
#define ASTL_INCLUDE_ERASE_IF_HPP

#include <algorithm>

#include "remove.hpp"

#include "astl/functional.hpp"
#include "astl/range_access.hpp"

namespace astl
{
namespace internal_ei
{
template <typename Container, typename UnaryPredicate>
auto erase_if_impl(Container &&c, UnaryPredicate pred, internal_adl::rank<0>)
    -> decltype(c.erase(i::remove_if(adl::begin(c), adl::end(c), pred), adl::end(c)))
{
    return c.erase(i::remove_if(adl::begin(c), adl::end(c), pred), adl::end(c));
}

template <typename Container, typename UnaryPredicate>
auto erase_if_impl(Container &&c, UnaryPredicate pred, internal_adl::rank<1>)
    -> decltype(c.erase_if(pred))
{
    return c.erase_if(pred);
}

template <typename Containter, typename UnaryPredicate>
auto erase_if_impl(Containter &&c, UnaryPredicate pred, internal_adl::rank<2>)
    -> decltype(c.remove_if(pred))
{
    return c.remove_if(pred);
}

template <typename Container, typename T>
auto erase_impl(Container &&c, T &&val, internal_adl::rank<0>)
    -> decltype(c.erase(i::remove(adl::begin(c), adl::end(c), val), adl::end(c)))
{
    return c.erase(i::remove(adl::begin(c), adl::end(c), val), adl::end(c));
}

template <typename Container, typename T>
auto erase_impl(Container &&c, T &&val, internal_adl::rank<1>) -> decltype(c.erase(val))
{
    return c.erase(val);
}

template <typename Containter, typename T>
auto erase_impl(Containter &&c, T &&val, internal_adl::rank<2>) -> decltype(c.remove(val))
{
    return c.remove(val);
}
} // namespace internal_ei

namespace r
{
inline constexpr struct {
    template <typename Container, typename T>
    auto operator()(Container &&c, T &&val) const
        -> decltype(internal_ei::erase_impl(c, val, internal_adl::rank<2>{}))
    {
        return internal_ei::erase_impl(c, val, internal_adl::rank<2>{});
    }
} erase{};

inline constexpr struct {
    template <typename Container, typename UnaryPredicate>
    auto operator()(Container &&c, UnaryPredicate p) const
        -> decltype(internal_ei::erase_if_impl(c, astl::pass_fn(p), internal_adl::rank<2>{}))
    {
        return internal_ei::erase_if_impl(c, astl::pass_fn(p), internal_adl::rank<2>{});
    }

    template <typename Container, typename UnaryPredicate, typename P>
    auto operator()(Container &&c, UnaryPredicate pred, P p) const
        -> decltype(internal_ei::erase_if_impl(c,
                                               astl::combine(astl::pass_fn(pred), astl::pass_fn(p)),
                                               internal_adl::rank<2>{}))
    {
        return internal_ei::erase_if_impl(c, astl::combine(astl::pass_fn(pred), astl::pass_fn(p)),
                                          internal_adl::rank<2>{});
    }
} erase_if{};

} // namespace r
} // namespace astl

#endif // ASTL_INCLUDE_ERASE_IF_HPP
