//
// Created by Rijad on 11-Aug-18.
//

#ifndef ASTL_INCLUDE_MEMORY_HPP
#define ASTL_INCLUDE_MEMORY_HPP

#include <cstring>
#include <memory>
#include <type_traits>

#include "iterator.hpp"
#include "range_access.hpp"
#include "temporary_buffer.hpp"

namespace astl
{
template <typename T, typename... Ts>
auto construct_at(T &obj, Ts &&... args) noexcept(::new (const_cast<void *>(
    static_cast<void const volatile *>(std::addressof(obj)))) T(static_cast<Ts &&>(args)...))
    -> decltype(::new (const_cast<void *>(static_cast<void const volatile *>(std::addressof(obj))))
                    T(static_cast<Ts &&>(args)...))
{
    // invoke True Placement New to initialize the referenced object with args...
    return ::new (const_cast<void *>(static_cast<void const volatile *>(std::addressof(obj))))
        T(static_cast<Ts &&>(args)...);
}

template <typename T, typename... Ts>
auto construct_at(T *const location, Ts &&... args) noexcept(::new (const_cast<void *>(
    static_cast<void const volatile *>(std::addressof(*location)))) T(static_cast<Ts &&>(args)...))
    -> decltype(
        ::new (const_cast<void *>(static_cast<void const volatile *>(std::addressof(*location))))
            T(static_cast<Ts &&>(args)...))
{
    // invoke True Placement New to initialize the referenced object with args...
    return ::new (const_cast<void *>(static_cast<void const volatile *>(std::addressof(*location))))
        T(static_cast<Ts &&>(args)...);
}

namespace i
{
using std::uninitialized_default_construct;   // NOLINT(misc-unused-using-decls)
using std::uninitialized_default_construct_n; // NOLINT(misc-unused-using-decls)

using std::uninitialized_value_construct;   // NOLINT(misc-unused-using-decls)
using std::uninitialized_value_construct_n; // NOLINT(misc-unused-using-decls)

using std::uninitialized_copy;   // NOLINT(misc-unused-using-decls)
using std::uninitialized_copy_n; // NOLINT(misc-unused-using-decls)

using std::uninitialized_move;   // NOLINT(misc-unused-using-decls)
using std::uninitialized_move_n; // NOLINT(misc-unused-using-decls)

using std::uninitialized_fill;   // NOLINT(misc-unused-using-decls)
using std::uninitialized_fill_n; // NOLINT(misc-unused-using-decls)

} // namespace i

namespace r
{
template <typename R, typename FwdIt>
// requires R InputIterator range
// requires FwdIt ForwardIterator
auto uninitialized_copy(R &&r, FwdIt d_first) noexcept(
    std::is_nothrow_copy_constructible<astl::range_value_type<R>>::value) -> FwdIt
{
    return i::uninitialized_copy(adl::begin(r), adl::end(r), d_first);
}

template <typename R, typename N, typename FwdIt>
// requires R InputIterator range
// requires N integral type
// requires FwdIt ForwardIterator
auto uninitialized_copy_n(R &&r, N n, FwdIt d_first) noexcept(
    std::is_nothrow_copy_constructible<astl::range_value_type<R>>::value) -> FwdIt
{
    return i::uninitialized_copy_n(adl::begin(r), n, d_first);
}

template <typename R>
// requires R ForwardIterator range
auto uninitialized_default_construct(R &&r) noexcept(
    std::is_nothrow_default_constructible<astl::range_value_type<R>>::value) -> void
{
    i::uninitialized_default_construct(adl::begin(r), adl::end(r));
}

template <typename R, typename N>
// requires R ForwardIterator range
// requires N integral type
auto uninitialized_default_construct_n(R &&r, N count) noexcept(
    std::is_nothrow_default_constructible<astl::range_value_type<R>>::value) -> iter_of_range<R>
{
    return i::uninitialized_default_construct(adl::begin(r), count);
}

template <typename R, typename T>
// requires R ForwardIterator range
// requires T assignable to value_type(R)
auto uninitialized_fill(R &&r, T &&val) noexcept(
    std::is_nothrow_constructible<astl::range_value_type<R>, T>::value) -> void
{
    std::uninitialized_fill(adl::begin(r), adl::end(r), val);
}

template <typename R, typename N, typename T>
// requires R ForwardIterator range
// requires N integral type
// requires T assignable to value_type(R)
auto uninitialized_fill_n(R &&r, N n, T &&val) noexcept(
    std::is_nothrow_constructible<astl::range_value_type<R>, T>::value) -> iter_of_range<R>
{
    return std::uninitialized_fill_n(adl::begin(r), n, val);
}

template <typename R, typename FwdIt>
// requires R InputIterator range
// requires FwdIt ForwardIterator
auto uninitialized_move(R &&r, FwdIt d_first) noexcept(
    std::is_nothrow_move_constructible<astl::range_value_type<R>>::value) -> FwdIt
{
    return i::uninitialized_move(adl::begin(r), adl::end(r), d_first);
}

template <typename R, typename N, typename FwdIt>
// requires R InputIterator range
// requires N integral type
// requires FwdIt ForwardIterator
auto uninitialized_move_n(R &&r, N n, FwdIt d_first) noexcept(
    std::is_nothrow_move_constructible<astl::range_value_type<R>>::value) -> FwdIt
{
    return i::uninitialized_move_n(adl::begin(r), n, d_first);
}

template <typename R>
// requires FwdIt ForwardIterator
auto uninitialized_value_construct(R &&r) noexcept(
    std::is_nothrow_copy_constructible<astl::range_value_type<R>>::value) -> void
{
    i::uninitialized_value_construct(adl::begin(r), adl::end(r));
}

template <typename R, typename N>
// requires FwdIt ForwardIterator
// requires N integral type
auto uninitialized_value_construct_n(R &&r, N count) noexcept(
    std::is_nothrow_copy_constructible<astl::range_value_type<R>>::value) -> iter_of_range<R>
{
    return i::uninitialized_value_construct_n(adl::begin(r), count);
}
} // namespace r
} // namespace astl

#endif // ASTL_INCLUDE_MEMORY_HPP
