//
// Created by Rijad on 28-Jul-18.
//

#ifndef ASTL_INCLUDE_EBO_BASE_HPP
#define ASTL_INCLUDE_EBO_BASE_HPP

#include <type_traits>

#include "internal/compiler.hpp"

namespace astl
{
////////////////////////////////////////////////////////////////////////////////////////////////////
// ebo<K, V>
//
// Building block to implement the Empty Base Optimization (EBO). We use a short
// name and define it in a short namespace to reduce symbol lengths, since this
// type is used as a building block for other widely used types such as
// `hana::pair`.
//
// When available, we use compiler intrinsics to reduce the number of
// instantiations.
//
// `ebo` provides a limited set of constructors to reduce instantiations. Also,
// the constructors are open-ended and they do not check for the validity of
// their arguments, again to reduce compile-time costs. Users of `ebo` should
// make sure that they only try to construct an `ebo` from a compatible value.
//
// EBOs can be indexed using an arbitrary type. The recommended usage is to
// define an integral constant wrapper for the specific container using EBO, and
// then index using that wrapper:
//
//      template <int> struct idx; // wrapper for tuple
//      template <typename ...ValueT>
//      struct tuple : ebo<idx<0>, T0>, ebo<idx<1>, T1>, ... { };
//
// The reason for defining one wrapper per container is to avoid any issues that
// can arise when using `ebo_get`, which casts to the Base class. If `tuple` and
// `pair` are inheriting from `ebo`s with the same indexing scheme, trying to
// use `ebo_get` on a tuple of pairs will trigger an ambiguous Base class
// conversion, since both tuple and pair inherit from `ebo`s with the same keys.
////////////////////////////////////////////////////////////////////////////////////////////////////
template <typename K, typename V, bool = std::is_empty<V>::value && !std::is_final<V>::value>
struct ebo;

// Specialize storage for empty types
template <typename K, typename V> struct ebo<K, V, true>: V {
    constexpr ebo() = default;

    template <typename T>
    // NOLINTNEXTLINE (bugprone-forwarding-reference-overload)
    constexpr explicit ebo(T &&t) noexcept : V(static_cast<T &&>(t))
    {}
};

// Specialize storage for non-empty types
template <typename K, typename V> struct ebo<K, V, false> {
    constexpr ebo() = default;

    template <typename T>
    // NOLINTNEXTLINE (bugprone-forwarding-reference-overload)
    constexpr explicit ebo(T &&t) noexcept(std::is_nothrow_constructible<V, T>::value)
        : _data(static_cast<T &&>(t))
    {}

    V _data;
};

////////////////////////////////////////////////////////////////////////////////////////////////////
// ebo_get
////////////////////////////////////////////////////////////////////////////////////////////////////
template <typename K, typename V>
ASTL_NODISCARD constexpr auto ebo_get(ebo<K, V, true> const &x) noexcept -> V const &
{
    return x;
}

template <typename K, typename V>
ASTL_NODISCARD constexpr auto ebo_get(ebo<K, V, true> &x) noexcept -> V &
{
    return x;
}

template <typename K, typename V>
ASTL_NODISCARD constexpr auto ebo_get(ebo<K, V, true> &&x) noexcept -> V &&
{
    return static_cast<V &&>(x);
}

template <typename K, typename V>
ASTL_NODISCARD constexpr auto ebo_get(ebo<K, V, true> const &&x) noexcept -> V &&
{
    return static_cast<V const &&>(x);
}

template <typename K, typename V>
ASTL_NODISCARD constexpr auto ebo_get(ebo<K, V, false> const &x) noexcept -> V const &
{
    return x._data;
}

template <typename K, typename V>
ASTL_NODISCARD constexpr auto ebo_get(ebo<K, V, false> &x) noexcept -> V &
{
    return x._data;
}

template <typename K, typename V>
ASTL_NODISCARD constexpr auto ebo_get(ebo<K, V, false> &&x) noexcept -> V &&
{
    return static_cast<V &&>(x._data);
}

template <typename K, typename V>
ASTL_NODISCARD constexpr auto ebo_get(ebo<K, V, false> const &&x) noexcept -> V &&
{
    return static_cast<V const &&>(x._data);
}
}// namespace astl

#endif// ASTL_INCLUDE_EBO_BASE_HPP
