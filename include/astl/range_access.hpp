//
// Created by Rijad on 13-Aug-18.
//

#ifndef ASTL_INCLUDE_RANGE_ACCESS_HPP
#define ASTL_INCLUDE_RANGE_ACCESS_HPP

#include <cstddef>
#include <initializer_list>
#include <iterator>
#include <type_traits>

#include "internal/compiler.hpp"

namespace astl
{
template <typename T, std::size_t Size>
ASTL_NODISCARD constexpr auto data(T (&arr)[Size]) noexcept -> T *
{
    return arr;
}

template <typename T>
ASTL_NODISCARD constexpr auto data(T &&c) noexcept(noexcept(c.data())) -> decltype(c.data())
{
    return c.data();
}

template <typename Elem>
ASTL_NODISCARD constexpr auto data(std::initializer_list<Elem> ilist) noexcept
    -> decltype(ilist.begin())
{
    // get pointer to data of initializer_list
    return ilist.begin();
}

template <typename Elem>
ASTL_NODISCARD constexpr auto empty(std::initializer_list<Elem> ilist) noexcept -> bool
{
    // get pointer to data of initializer_list
    return ilist.size() == 0;
}

template <typename T, std::size_t Size>
ASTL_NODISCARD constexpr auto empty(T const (&)[Size]) noexcept -> bool
{ // get dimension==0 for array (can't happen)
    return false;
}

template <typename T>
ASTL_NODISCARD constexpr auto empty(T &&c) noexcept(noexcept(c.empty())) -> decltype(c.empty())
{
    return c.empty();
}

template <typename T, std::size_t Size>
ASTL_NODISCARD constexpr auto size(T (&)[Size]) noexcept -> std::size_t
{
    return Size;
}

template <typename T> ASTL_NODISCARD constexpr auto size(T &&c) noexcept -> decltype(c.size())
{
    return c.size();
}

template <typename Cont>
ASTL_NODISCARD constexpr auto ssize(Cont const &c) noexcept(noexcept(
    static_cast<std::common_type_t<std::ptrdiff_t, std::make_signed_t<decltype(c.size())>>>(
        c.size()))) -> std::common_type_t<std::ptrdiff_t, std::make_signed_t<decltype(c.size())>>
{
    return static_cast<std::common_type_t<std::ptrdiff_t, std::make_signed_t<decltype(c.size())>>>(
        c.size());
}

template <typename T, std::ptrdiff_t Sz>
ASTL_NODISCARD constexpr auto ssize(T (&)[Sz]) noexcept -> std::ptrdiff_t
{
    return Sz;
}

template <typename T, std::size_t Size>
ASTL_NODISCARD constexpr auto begin(T (&arr)[Size]) noexcept -> T *
{
    return arr;
}

template <typename T>
ASTL_NODISCARD constexpr auto begin(T &&c) noexcept(noexcept(c.begin())) -> decltype(c.begin())
{
    return c.begin();
}

template <typename T, std::size_t Size>
ASTL_NODISCARD constexpr auto cbegin(T (&arr)[Size]) noexcept -> T const *
{
    return arr;
}

template <typename T>
ASTL_NODISCARD constexpr auto cbegin(T &&c) noexcept(noexcept(c.cbegin())) -> decltype(c.cbegin())
{
    return c.cbegin();
}

template <typename T, std::size_t Size>
ASTL_NODISCARD auto rbegin(T (&arr)[Size]) noexcept -> std::reverse_iterator<T *>
{
    return std::reverse_iterator<T *>(arr + Size);
}

template <typename T>
ASTL_NODISCARD constexpr auto rbegin(T &&c) noexcept(noexcept(c.rbegin())) -> decltype(c.rbegin())
{
    return c.rbegin();
}

template <typename T, std::size_t Size>
ASTL_NODISCARD auto crbegin(T (&arr)[Size]) noexcept -> std::reverse_iterator<T const *>
{
    return std::reverse_iterator<T const *>(arr + Size);
}

template <typename T>
ASTL_NODISCARD constexpr auto crbegin(T &&c) noexcept(noexcept(c.crbegin()))
    -> decltype(c.crbegin())
{
    return c.crbegin();
}

template <typename T, std::size_t Size>
ASTL_NODISCARD constexpr auto end(T (&arr)[Size]) noexcept -> T *
{
    return arr + Size;
}

template <typename T>
ASTL_NODISCARD constexpr auto end(T &&c) noexcept(noexcept(c.end())) -> decltype(c.end())
{
    return c.end();
}

template <typename T, std::size_t Size>
ASTL_NODISCARD constexpr auto cend(T (&arr)[Size]) noexcept -> T const *
{
    return arr + Size;
}

template <typename T>
ASTL_NODISCARD constexpr auto cend(T &&c) noexcept(noexcept(c.cend())) -> decltype(c.cend())
{
    return c.cend();
}

template <typename T, std::size_t Size>
ASTL_NODISCARD auto rend(T (&arr)[Size]) noexcept -> std::reverse_iterator<T *>
{
    return std::reverse_iterator<T *>(arr);
}

template <typename T>
ASTL_NODISCARD constexpr auto rend(T &&c) noexcept(noexcept(c.rend())) -> decltype(c.rend())
{
    return c.rend();
}

template <typename T, std::size_t Size>
ASTL_NODISCARD auto crend(T (&arr)[Size]) noexcept -> std::reverse_iterator<T const *>
{
    return std::reverse_iterator<T const *>(arr);
}

template <typename T>
ASTL_NODISCARD constexpr auto crend(T &&c) noexcept(noexcept(c.crend())) -> decltype(c.crend())
{
    return c.crend();
}

namespace internal_adl
{
template <int I> struct rank: rank<I - 1> {};

template <> struct rank<0> {};

using astl::begin;
template <typename Cont>
static constexpr auto begin1(Cont &&c, rank<0>) noexcept(noexcept(begin(static_cast<Cont &&>(c))))
    -> decltype(begin(static_cast<Cont &&>(c)))
{
    return begin(static_cast<Cont &&>(c));
}

template <typename Cont>
static constexpr auto begin1(Cont &&c, rank<1>) noexcept(noexcept(static_cast<Cont &&>(c).begin()))
    -> decltype(static_cast<Cont &&>(c).begin())
{
    return static_cast<Cont &&>(c).begin();
}

using astl::end;
template <typename Cont>
static constexpr auto end1(Cont &&c, rank<0>) noexcept(noexcept(end(static_cast<Cont &&>(c))))
    -> decltype(end(static_cast<Cont &&>(c)))
{
    return end(static_cast<Cont &&>(c));
}

template <typename Cont>
static constexpr auto end1(Cont &&c, rank<1>) noexcept(noexcept(static_cast<Cont &&>(c).end()))
    -> decltype(static_cast<Cont &&>(c).end())
{
    return static_cast<Cont &&>(c).end();
}

using astl::size;
template <typename Cont>
static constexpr auto size1(Cont &&c, rank<0>) noexcept(noexcept(size(static_cast<Cont &&>(c))))
    -> decltype(size(static_cast<Cont &&>(c)))
{
    return size(static_cast<Cont &&>(c));
}

template <typename Cont>
static constexpr auto size1(Cont &&c, rank<1>) noexcept(noexcept(static_cast<Cont &&>(c).size()))
    -> decltype(static_cast<Cont &&>(c).size())
{
    return static_cast<Cont &&>(c).size();
}
} // namespace internal_adl

namespace adl
{
template <typename Cont>
ASTL_NODISCARD constexpr auto begin(Cont &&c) noexcept(
    noexcept(internal_adl::begin1(static_cast<Cont &&>(c), internal_adl::rank<1>{})))
    -> decltype(internal_adl::begin1(static_cast<Cont &&>(c), internal_adl::rank<1>{}))
{
    return internal_adl::begin1(static_cast<Cont &&>(c), internal_adl::rank<1>{});
}

template <typename Cont>
ASTL_NODISCARD constexpr auto end(Cont &&c) noexcept(
    noexcept(internal_adl::end1(static_cast<Cont &&>(c), internal_adl::rank<1>{})))
    -> decltype(internal_adl::end1(static_cast<Cont &&>(c), internal_adl::rank<1>{}))
{
    return internal_adl::end1(static_cast<Cont &&>(c), internal_adl::rank<1>{});
}

template <typename Cont>
ASTL_NODISCARD constexpr auto size(Cont &&c) noexcept(
    noexcept(internal_adl::size1(static_cast<Cont &&>(c), internal_adl::rank<1>{})))
    -> decltype(internal_adl::size1(static_cast<Cont &&>(c), internal_adl::rank<1>{}))
{
    return internal_adl::size1(static_cast<Cont &&>(c), internal_adl::rank<1>{});
}
} // namespace adl

template <typename R> ASTL_NODISCARD constexpr auto mbegin(R &&c)
{
    return std::make_move_iterator(adl::begin(c));
}

template <typename R> ASTL_NODISCARD constexpr auto mend(R &&c)
{
    return std::make_move_iterator(adl::end(c));
}

namespace internal
{
template <typename T, bool = std::is_enum<T>::value> struct sfinae_underlying_type {
    using type = typename std::underlying_type<T>::type;
    using PromotedType = decltype(((type) 1) + 0);
};

template <typename T> struct sfinae_underlying_type<T, false> {};
} // namespace internal

ASTL_NODISCARD constexpr auto convert_to_integral(int const value) noexcept -> int { return value; }

ASTL_NODISCARD constexpr auto convert_to_integral(unsigned const value) noexcept -> unsigned
{
    return value;
}

ASTL_NODISCARD constexpr auto convert_to_integral(long const value) noexcept -> long
{
    return value;
}

ASTL_NODISCARD constexpr auto convert_to_integral(unsigned long const value) noexcept
    -> unsigned long
{
    return value;
}

ASTL_NODISCARD constexpr auto convert_to_integral(long long const value) noexcept -> long long
{
    return value;
}

ASTL_NODISCARD constexpr auto convert_to_integral(unsigned long long const value) noexcept
    -> unsigned long long
{
    return value;
}

template <typename Fp>
ASTL_NODISCARD constexpr auto
convert_to_integral(Fp value) noexcept(std::is_nothrow_constructible<Fp, long long>::value) ->
    typename std::enable_if<std::is_floating_point<Fp>::value, long long>::type
{
    return static_cast<long long>(value);
}

template <typename T>
ASTL_NODISCARD constexpr auto convert_to_integral(T value) ->
    typename internal::sfinae_underlying_type<T>::PromotedType
{
    using U = typename internal::sfinae_underlying_type<T>::PromotedType;
    return U{value};
}

template <typename T> using integral_t = decltype(astl::convert_to_integral(std::declval<T>()));
} // namespace astl

#endif // ASTL_INCLUDE_RANGE_ACCESS_HPP
