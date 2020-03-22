//
// Created by Rijad on 19-Aug-19.
//
#ifndef ASTL_INCLUDE_UTILITY_HPP
#define ASTL_INCLUDE_UTILITY_HPP

#include <cstddef>
#include <tuple>
#include <type_traits>

#include "ebo_base.hpp"

namespace astl
{
namespace internal_singl
{
template <int N> struct sin_tag {};

} // namespace internal_singl

template <typename T> struct singleton: private ebo<internal_singl::sin_tag<1>, T> {
private:
    using base1 = ebo<internal_singl::sin_tag<1>, T>;

public:
    template <typename U = T,
              std::enable_if_t<!std::is_same<singleton<T>, std::decay_t<U>>::value
                                   && std::is_constructible<T, U &&>::value
                                   && std::is_convertible<U &&, T>::value,
                               bool> = true>
    // NOLINTNEXTLINE(bugprone-forwarding-reference-overload, google-explicit-constructor)
    constexpr singleton(U &&t) noexcept(std::is_nothrow_constructible<T, U>::value)
        : base1(static_cast<U &&>(t))
    {}

    template <typename U = T,
              std::enable_if_t<!std::is_same<singleton<T>, std::decay_t<U>>::value
                                   && std::is_constructible<T, U &&>::value
                                   && !std::is_convertible<U &&, T>::value,
                               bool> = true>
    // NOLINTNEXTLINE(bugprone-forwarding-reference-overload)
    constexpr explicit singleton(U &&t) noexcept(std::is_nothrow_constructible<T, U>::value)
        : base1(static_cast<U &&>(t))
    {}

    singleton() = default;

    singleton(singleton const &) = default;
    singleton(singleton &&) noexcept(std::is_nothrow_move_constructible<T>::value) = default;

    auto operator=(singleton const &) -> singleton & = default;
    auto operator=(singleton &&) noexcept(std::is_nothrow_move_assignable<T>::value)
        -> singleton & = default;

    // get reference to T
    ASTL_NODISCARD constexpr auto m1() & noexcept -> T & { return ebo_get(*this); }

    // get const reference to T
    ASTL_NODISCARD constexpr auto m1() const & noexcept -> T const & { return ebo_get(*this); }

    // get rvalue reference to T
    ASTL_NODISCARD constexpr auto m1() && noexcept -> T && { return ebo_get(std::move(*this)); }

    // get const rvalue reference to T
    ASTL_NODISCARD constexpr auto m1() const && noexcept -> T const &&
    {
        return ebo_get(std::move(*this));
    }
};

#if HAS_DEDUCTION_GUIDES

template <typename T> singleton(T)->singleton<T>;

#endif //#if HAS_DEDUCTION_GUIDES

template <typename T>
ASTL_NODISCARD constexpr auto make_singleton(T &&x) -> singleton<std::decay_t<T>>
{
    return singleton<std::decay_t<T>>{static_cast<T &&>(x)};
}

// Regular
template <typename T>
ASTL_NODISCARD auto operator==(const singleton<T> &x, const singleton<T> &y) -> bool
{
    return x.m1() == y.m1();
}

template <typename T>
ASTL_NODISCARD auto operator!=(const singleton<T> &x, const singleton<T> &y) -> bool
{
    return !(x == y);
}

template <typename T>
ASTL_NODISCARD auto operator<(const singleton<T> &x, const singleton<T> &y) -> bool
{
    return x.m1() < y.m1();
}

template <typename T>
ASTL_NODISCARD auto operator>(const singleton<T> &x, const singleton<T> &y) -> bool
{
    return y < x;
}

template <typename T>
ASTL_NODISCARD auto operator<=(const singleton<T> &x, const singleton<T> &y) -> bool
{
    return !(y < x);
}

template <typename T>
ASTL_NODISCARD auto operator>=(const singleton<T> &x, const singleton<T> &y) -> bool
{
    return !(x < y);
}
} // namespace astl

namespace std
{
// TUPLE INTERFACE TO astl::singleton
template <typename T>
struct tuple_size<astl::singleton<T>>: integral_constant<size_t, 1> { // size of pair
};

template <size_t Idx, typename T>
struct tuple_element<Idx, astl::singleton<T>> { // struct to determine type of
                                                // element Idx in singleton
    static_assert(Idx < 1, "singleton index out of bounds");

    using type = T;
};
} // namespace std

namespace astl
{
template <std::size_t Idx, typename T>
ASTL_NODISCARD constexpr auto get(singleton<T> &s) noexcept
    -> std::tuple_element_t<Idx, singleton<T>> &
{ // get reference to element at Idx in singleton s
    return s.m1();
}

template <typename T> ASTL_NODISCARD constexpr auto get(singleton<T> &s) noexcept -> T &
{ // get reference to element T in singleton s
    return s.m1();
}

template <std::size_t Idx, typename T>
ASTL_NODISCARD constexpr auto get(singleton<T> const &s) noexcept
    -> std::tuple_element_t<Idx, singleton<T>> const &
{ // get const reference to element at Idx in singleton s
    return s.m1();
}

template <typename T> ASTL_NODISCARD constexpr auto get(singleton<T> const &s) noexcept -> T const &
{ // get const reference to element T in singleton s
    return s.m1();
}

template <std::size_t Idx, typename T>
ASTL_NODISCARD constexpr auto get(singleton<T> &&s) noexcept
    -> std::tuple_element_t<Idx, singleton<T>> &&
{ // get rvalue reference to element at Idx in singleton s
    return std::move(s).m1();
}

template <typename T> ASTL_NODISCARD constexpr auto get(singleton<T> &&s) noexcept -> T &&
{ // get rvalue reference to element T in singleton s
    return std::move(s).m1();
}

template <std::size_t Idx, typename T>
ASTL_NODISCARD constexpr auto get(singleton<T> const &&s) noexcept
    -> std::tuple_element_t<Idx, singleton<T>> const &&
{ // get const rvalue reference to element at Idx in singleton s
    return std::move(s).m1();
}

template <typename T>
ASTL_NODISCARD constexpr auto get(singleton<T> const &&s) noexcept -> T const &&
{ // get const rvalue reference to element T in singleton s
    return std::move(s).m1();
}

namespace internal_pair
{
template <int N> struct pid_tag {};
} // namespace internal_pair

template <typename T, typename U>
struct pair: private ebo<internal_pair::pid_tag<1>, T>, private ebo<internal_pair::pid_tag<2>, U> {
private:
    using tag1 = internal_pair::pid_tag<1>;
    using tag2 = internal_pair::pid_tag<2>;

    using base1 = ebo<tag1, T>;
    using base2 = ebo<tag2, U>;

    template <typename T1, typename U1>
    using is_nt_construtible =
        std::integral_constant<bool,
                               std::is_nothrow_constructible<T, T1>::value
                                   && std::is_nothrow_constructible<U, U1>::value>;

    using is_nt_move_const =
        std::integral_constant<bool,
                               std::is_nothrow_move_constructible<T>::value
                                   && std::is_nothrow_move_constructible<U>::value>;

    using is_nt_move_ass = std::integral_constant<bool,
                                                  std::is_nothrow_move_assignable<T>::value
                                                      && std::is_nothrow_move_assignable<U>::value>;

public:
    using first_type = T;
    using second_type = U;

    template <typename T1, typename U1>
    pair(T1 &&x, U1 &&y) noexcept(is_nt_construtible<T1, U1>::value)
        : base1(static_cast<T1 &&>(x)), base2(static_cast<U1 &&>(y))
    {}

    pair() = default;

    pair(pair const &) = default;
    pair(pair &&) noexcept(is_nt_move_const::value) = default;

    auto operator=(pair const &) -> pair & = default;
    auto operator=(pair &&) noexcept(is_nt_move_ass::value) -> pair & = default;

    // get reference to U
    ASTL_NODISCARD constexpr auto m1() & noexcept -> T & { return ebo_get<tag1>(*this); }

    // get const reference to U
    ASTL_NODISCARD constexpr auto m1() const & noexcept -> T const &
    {
        return ebo_get<tag1>(*this);
    }

    // get rvalue reference to U
    ASTL_NODISCARD constexpr auto m1() && noexcept -> T &&
    {
        return ebo_get<tag1>(std::move(*this));
    }

    // get const rvalue reference to U
    ASTL_NODISCARD constexpr auto m1() const && noexcept -> T const &&
    {
        return ebo_get<tag1>(std::move(*this));
    }

    // get reference to U
    ASTL_NODISCARD constexpr auto m2() & noexcept -> U & { return ebo_get<tag2>(*this); }

    // get const reference to U
    ASTL_NODISCARD constexpr auto m2() const & noexcept -> U const &
    {
        return ebo_get<tag2>(*this);
    }

    // get rvalue reference to U
    ASTL_NODISCARD constexpr auto m2() && noexcept -> U &&
    {
        return ebo_get<tag2>(std::move(*this));
    }

    // get const rvalue reference to U
    ASTL_NODISCARD constexpr auto m2() const && noexcept -> U const &&
    {
        return ebo_get<tag2>(std::move(*this));
    }
};

#if HAS_DEDUCTION_GUIDES

template <typename T, typename U> pair(T, U)->pair<T, U>;

#endif //#if HAS_DEDUCTION_GUIDES

template <typename T, typename U>
ASTL_NODISCARD constexpr auto make_pair(T &&x, U &&y) -> pair<std::decay_t<T>, std::decay_t<U>>
{
    return pair<std::decay_t<T>, std::decay_t<U>>{static_cast<T &&>(x), static_cast<U &&>(y)};
}

/// Two pairs of the same type are equal iff their members are equal.
template <typename T, typename U>
ASTL_NODISCARD constexpr auto operator==(pair<T, U> const &x, pair<T, U> const &y) noexcept -> bool
{
    return x.first == y.first && x.second == y.second;
}

template <typename T, typename U>
ASTL_NODISCARD constexpr auto operator<(pair<T, U> const &x, pair<T, U> const &y) noexcept -> bool
{
    return x.first < y.first || (!(y.first < x.first) && x.second < y.second);
}

/// Uses @c operator== to find the result.
template <typename T, typename U>
ASTL_NODISCARD constexpr auto operator!=(pair<T, U> const &x, pair<T, U> const &y) noexcept -> bool
{
    return !(x == y);
}

/// Uses @c operator< to find the result.
template <typename T, typename U>
ASTL_NODISCARD constexpr auto operator>(pair<T, U> const &x, pair<T, U> const &y) noexcept -> bool
{
    return y < x;
}

/// Uses @c operator< to find the result.
template <typename T, typename U>
ASTL_NODISCARD constexpr auto operator<=(pair<T, U> const &x, pair<T, U> const &y) noexcept -> bool
{
    return !(y < x);
}

/// Uses @c operator< to find the result.
template <typename T, typename U>
ASTL_NODISCARD constexpr auto operator>=(pair<T, U> const &x, pair<T, U> const &y) noexcept -> bool
{
    return !(x < y);
}

namespace internal_pair
{
template <std::size_t Idx, typename Pair> auto pair_get(Pair &&p)
{
    if constexpr (Idx == 0) return static_cast<Pair &&>(p).m1();
    else if constexpr (Idx == 1)
        return static_cast<Pair &&>(p).m2();
    else {
        static_assert(Idx < 2, "pair index out of bounds");
        return p.m1();
    }
}
} // namespace internal_pair
} // namespace astl

namespace std
{
// TUPLE INTERFACE TO astl::pair
template <typename T, typename U>
struct tuple_size<astl::pair<T, U>>: integral_constant<size_t, 2> { // size of pair
};

template <size_t Idx, typename T, typename U> struct tuple_element<Idx, astl::pair<T, U>> {
    // struct to determine type ofelement Idx in pair
    static_assert(Idx < 2, "pair index out of bounds");

    using type = conditional_t<Idx == 0, T, U>;
};
} // namespace std

namespace astl
{
template <std::size_t Idx, typename T, typename U>
ASTL_NODISCARD constexpr auto get(pair<T, U> &p) noexcept -> std::tuple_element_t<Idx, pair<T, U>> &
{ // get reference to element at Idx in pair p
    return internal_pair::pair_get<Idx>(p);
}

template <typename T, typename U> ASTL_NODISCARD constexpr auto get(pair<T, U> &p) noexcept -> T &
{ // get reference to element T in pair p
    return p.m1();
}

template <typename U, typename T> ASTL_NODISCARD constexpr auto get(pair<T, U> &p) noexcept -> U &
{ // get reference to element U in pair p
    return p.m2();
}

template <std::size_t Idx, typename T, typename U>
ASTL_NODISCARD constexpr auto get(pair<T, U> const &p) noexcept
    -> std::tuple_element_t<Idx, pair<T, U>> const &
{ // get const reference to element at Idx in pair p
    return internal_pair::pair_get<Idx>(p);
}

template <typename T, typename U>
ASTL_NODISCARD constexpr auto get(pair<T, U> const &p) noexcept -> T const &
{ // get const reference to element T in pair p
    return p.m1();
}

template <typename U, typename T>
ASTL_NODISCARD constexpr auto get(pair<T, U> const &p) noexcept -> U const &
{ // get const reference to element U in pair p
    return p.m2();
}

template <std::size_t Idx, typename T, typename U>
ASTL_NODISCARD constexpr auto get(pair<T, U> &&p) noexcept
    -> std::tuple_element_t<Idx, pair<T, U>> &&
{ // get rvalue reference to element at Idx in pair p
    return internal_pair::pair_get<Idx>(std::move(p));
}

template <typename T, typename U> ASTL_NODISCARD constexpr auto get(pair<T, U> &&p) noexcept -> T &&
{ // get rvalue reference to element T in pair p
    return std::move(p).m1();
}

template <typename U, typename T> ASTL_NODISCARD constexpr auto get(pair<T, U> &&p) noexcept -> U &&
{ // get rvalue reference to element U in pair p
    return std::move(p).m2();
}

template <std::size_t Idx, typename T, typename U>
ASTL_NODISCARD constexpr auto get(pair<T, U> const &&p) noexcept
    -> std::tuple_element_t<Idx, pair<T, U>> const &&
{ // get const rvalue reference to element at Idx in pair p
    return internal_pair::pair_get<Idx>(std::move(p));
}

template <typename T, typename U>
ASTL_NODISCARD constexpr auto get(pair<T, U> const &&p) noexcept -> T const &&
{ // get const rvalue reference to element T in pair p
    return std::move(p).m1();
}

template <typename U, typename T>
ASTL_NODISCARD constexpr auto get(pair<T, U> const &&p) noexcept -> U const &&
{ // get const rvalue reference to element U in pair p
    return std::move(p).m2();
}

namespace internal_triple
{
template <int N> struct tid_tag {};
} // namespace internal_triple

template <typename T, typename U, typename V>
struct triple: private ebo<internal_triple::tid_tag<1>, T>,
               private ebo<internal_triple::tid_tag<2>, U>,
               private ebo<internal_triple::tid_tag<3>, V> {
private:
    using tag1 = internal_triple::tid_tag<1>;
    using tag2 = internal_triple::tid_tag<2>;
    using tag3 = internal_triple::tid_tag<3>;

    using base1 = ebo<tag1, T>;
    using base2 = ebo<tag2, U>;
    using base3 = ebo<tag3, V>;

    template <typename T1, typename U1, typename V1>
    using is_nt_construtible =
        std::integral_constant<bool,
                               std::is_nothrow_constructible<T, T1>::value
                                   && std::is_nothrow_constructible<U, U1>::value
                                   && std::is_nothrow_constructible<V, V1>::value>;

    using is_nt_move_const =
        std::integral_constant<bool,
                               std::is_nothrow_move_constructible<T>::value
                                   && std::is_nothrow_move_constructible<U>::value
                                   && std::is_nothrow_move_constructible<V>::value>;

    using is_nt_move_ass = std::integral_constant<bool,
                                                  std::is_nothrow_move_assignable<T>::value
                                                      && std::is_nothrow_move_assignable<U>::value
                                                      && std::is_nothrow_move_assignable<V>::value>;

public:
    using first_type = T;
    using second_type = U;
    using third_type = V;

    template <typename T1, typename U1, typename V1>
    triple(T1 &&x, U1 &&y, V1 &&z) noexcept(is_nt_construtible<T1, U1, V1>::value)
        : base1(static_cast<T1 &&>(x)), base2(static_cast<U1 &&>(y)), base3(static_cast<V1 &&>(z))
    {}

    triple() = default;
    triple(triple const &) = default;
    triple(triple &&) noexcept(is_nt_move_const::value) = default;

    auto operator=(triple const &) -> triple & = default;
    auto operator=(triple &&) noexcept(is_nt_move_ass::value) -> triple & = default;

    // get reference to T
    ASTL_NODISCARD constexpr auto m1() & noexcept -> T & { return ebo_get<tag1>(*this); }

    // get const reference to T
    ASTL_NODISCARD constexpr auto m1() const & noexcept -> T const &
    {
        return ebo_get<tag1>(*this);
    }

    // get rvalue reference to T
    ASTL_NODISCARD constexpr auto m1() && noexcept -> T &&
    {
        return ebo_get<tag1>(std::move(*this));
    }

    // get const rvalue reference to T
    ASTL_NODISCARD constexpr auto m1() const && noexcept -> T const &&
    {
        return ebo_get<tag1>(std::move(*this));
    }

    // get reference to U
    ASTL_NODISCARD constexpr auto m2() & noexcept -> U & { return ebo_get<tag2>(*this); }

    // get const reference to U
    ASTL_NODISCARD constexpr auto m2() const & noexcept -> U const &
    {
        return ebo_get<tag2>(*this);
    }

    // get rvalue reference to U
    ASTL_NODISCARD constexpr auto m2() && noexcept -> U &&
    {
        return ebo_get<tag2>(std::move(*this));
    }

    // get const rvalue reference to U
    ASTL_NODISCARD constexpr auto m2() const && noexcept -> U const &&
    {
        return ebo_get<tag2>(std::move(*this));
    }

    // get reference to V
    ASTL_NODISCARD constexpr auto m3() & noexcept -> V & { return ebo_get<tag3>(*this); }

    // get const reference to T
    ASTL_NODISCARD constexpr auto m3() const & noexcept -> V const &
    {
        return ebo_get<tag3>(*this);
    }

    // get rvalue reference to V
    ASTL_NODISCARD constexpr auto m3() && noexcept -> V &&
    {
        return ebo_get<tag3>(std::move(*this));
    }

    // get const rvalue reference to V
    ASTL_NODISCARD constexpr auto m3() const && noexcept -> V const &&
    {
        return ebo_get<tag3>(std::move(*this));
    }
};

#if HAS_DEDUCTION_GUIDES

template <typename T, typename U, typename V> triple(T, U, V)->triple<T, U, V>;

#endif //#if HAS_DEDUCTION_GUIDES

template <typename T, typename U, typename V>
ASTL_NODISCARD constexpr auto make_triple(T &&x, U &&y, V &&z)
    -> triple<std::decay_t<T>, std::decay_t<U>, std::decay_t<V>>
{
    return triple<std::decay_t<T>, std::decay_t<U>, std::decay_t<V>>{
        static_cast<T &&>(x), static_cast<U &&>(y), static_cast<V &&>(z)};
}

/// Two triple of the same type are equal iff their members are equal.
template <typename T, typename U, typename V>
ASTL_NODISCARD constexpr auto operator==(triple<T, U, V> const &x,
                                         triple<T, U, V> const &y) noexcept -> bool
{
    return x.m1 == y.m1 && x.m2() == y.m2 && x.m3() == y.m3();
}

template <typename T, typename U, typename V>
ASTL_NODISCARD constexpr auto operator<(triple<T, U, V> const &x, triple<T, U, V> const &y) noexcept
    -> bool
{
    if (x.m1() < y.m1()) return true;
    if (y.m1() < x.m1()) return false;

    if (x.m2() < y.m2()) return true;
    if (y.m2() < x.m2()) return false;

    if (x.m3() < y.m3()) return true;
    if (y.m3() < x.m3()) return false;

    return false;
}

/// Uses @c operator== to find the result.
template <typename T, typename U, typename V>
ASTL_NODISCARD constexpr auto operator!=(triple<T, U, V> const &x,
                                         triple<T, U, V> const &y) noexcept -> bool
{
    return !(x == y);
}

/// Uses @c operator< to find the result.
template <typename T, typename U, typename V>
ASTL_NODISCARD constexpr auto operator>(triple<T, U, V> const &x, triple<T, U, V> const &y) noexcept
    -> bool
{
    return y < x;
}

/// Uses @c operator< to find the result.
template <typename T, typename U, typename V>
ASTL_NODISCARD constexpr auto operator<=(triple<T, U, V> const &x,
                                         triple<T, U, V> const &y) noexcept -> bool
{
    return !(y < x);
}

/// Uses @c operator< to find the result.
template <typename T, typename U, typename V>
ASTL_NODISCARD constexpr auto operator>=(triple<T, U, V> const &x,
                                         triple<T, U, V> const &y) noexcept -> bool
{
    return !(x < y);
}

namespace internal_triple
{
template <std::size_t Idx, typename Triple> auto triple_get(Triple &&t)
{
    if constexpr (Idx == 0) return static_cast<Triple &&>(t).m1();
    else if constexpr (Idx == 1)
        return static_cast<Triple &&>(t).m2();
    else if constexpr (Idx == 2)
        return static_cast<Triple &&>(t).m3();
    else {
        static_assert(Idx < 3, "triple index out of bounds");
        return t.m1();
    }
}

} // namespace internal_triple
} // namespace astl

namespace std
{
// TUPLE INTERFACE TO triple
template <typename T, typename U, typename V>
struct tuple_size<astl::triple<T, U, V>>: integral_constant<size_t, 3> { // size of triple
};

template <size_t Idx, typename T, typename U, typename V>
struct tuple_element<Idx, astl::triple<T, U, V>> { // struct to determine type
                                                   // of element Idx in triple
    static_assert(Idx < 3, "triple index out of bounds");

    using type = conditional_t<Idx == 1, U, V>;
};

template <typename T, typename U, typename V> struct tuple_element<0, astl::triple<T, U, V>> {
    using type = T;
};

} // namespace std

namespace astl
{
template <std::size_t Idx, typename T, typename U, typename V>
ASTL_NODISCARD constexpr auto get(triple<T, U, V> &t) noexcept
    -> std::tuple_element_t<Idx, triple<T, U, V>> &
{ // get reference to element at Idx in triple t
    return internal_triple::triple_get<Idx>(t);
}

template <typename T, typename U, typename V>
ASTL_NODISCARD constexpr auto get(triple<T, U, V> &t) noexcept -> T &
{ // get reference to element T in triple t
    return t.m1();
}

template <typename U, typename T, typename V>
ASTL_NODISCARD constexpr auto get(triple<T, U, V> &t) noexcept -> U &
{ // get reference to element U in triple t
    return t.m2();
}

template <typename V, typename U, typename T>
ASTL_NODISCARD constexpr auto get(triple<T, U, V> &t) noexcept -> V &
{ // get reference to element V in triple t
    return t.m3();
}

template <std::size_t Idx, typename T, typename U, typename V>
ASTL_NODISCARD constexpr auto get(triple<T, U, V> const &t) noexcept
    -> std::tuple_element_t<Idx, triple<T, U, V>> const &
{ // get const reference to element at Idx in triple t
    return internal_triple::triple_get<Idx>(t);
}

template <typename T, typename U, typename V>
ASTL_NODISCARD constexpr auto get(triple<T, U, V> const &t) noexcept -> T const &
{ // get const reference to element T in triple t
    return t.m1();
}

template <typename U, typename T, typename V>
ASTL_NODISCARD constexpr auto get(triple<T, U, V> const &t) noexcept -> U const &
{ // get const reference to element U in triple t
    return t.m2();
}

template <typename V, typename U, typename T>
ASTL_NODISCARD constexpr auto get(triple<T, U, V> const &t) noexcept -> V const &
{ // get const reference to element V in triple t
    return t.m3();
}

template <std::size_t Idx, typename T, typename U, typename V>
ASTL_NODISCARD constexpr auto get(triple<T, U, V> &&t) noexcept
    -> std::tuple_element_t<Idx, triple<T, U, V>> &&
{ // get rvalue reference to element at Idx in triple t
    return internal_triple::triple_get<Idx>(std::move(t));
}

template <typename T, typename U, typename V>
ASTL_NODISCARD constexpr auto get(triple<T, U, V> &&t) noexcept -> T &&
{ // get rvalue reference to element T in triple t
    return std::move(t).m1();
}

template <typename U, typename T, typename V>
ASTL_NODISCARD constexpr auto get(triple<T, U, V> &&t) noexcept -> U &&
{ // get rvalue reference to element U in triple t
    return std::move(t).m2();
}

template <typename V, typename U, typename T>
ASTL_NODISCARD constexpr auto get(triple<T, U, V> &&t) noexcept -> V &&
{ // get rvalue reference to element V in triple t
    return std::move(t).m3();
}

template <std::size_t Idx, typename T, typename U, typename V>
ASTL_NODISCARD constexpr auto get(triple<T, U, V> const &&t) noexcept
    -> std::tuple_element_t<Idx, triple<T, U, V>> const &&
{ // get const rvalue reference to element at Idx in triple t
    return internal_triple::triple_get<Idx>(std::move(t));
}

template <typename T, typename U, typename V>
ASTL_NODISCARD constexpr auto get(triple<T, U, V> const &&t) noexcept -> T const &&
{ // get const rvalue reference to element T in triple t
    return std::move(t).m1();
}

template <typename U, typename T, typename V>
ASTL_NODISCARD constexpr auto get(triple<T, U, V> const &&t) noexcept -> U const &&
{ // get const rvalue reference to element U in triple t
    return std::move(t).m2();
}

template <typename V, typename U, typename T>
ASTL_NODISCARD constexpr auto get(triple<T, U, V> const &&t) noexcept -> V const &&
{ // get const rvalue reference to element V in triple t
    return std::move(t).m3();
}

namespace internal_quad
{
template <int N> struct qid_tag {};
} // namespace internal_quad

template <typename T, typename U, typename V, typename Z>
struct quadriple: private ebo<internal_quad::qid_tag<1>, T>,
                  private ebo<internal_quad::qid_tag<2>, U>,
                  private ebo<internal_quad::qid_tag<3>, V>,
                  private ebo<internal_quad::qid_tag<4>, Z> {
private:
    using tag1 = internal_quad::qid_tag<1>;
    using tag2 = internal_quad::qid_tag<2>;
    using tag3 = internal_quad::qid_tag<3>;
    using tag4 = internal_quad::qid_tag<4>;

    using base1 = ebo<tag1, T>;
    using base2 = ebo<tag2, U>;
    using base3 = ebo<tag3, V>;
    using base4 = ebo<tag4, Z>;

    template <typename T1, typename U1, typename V1, typename Z1>
    using is_nt_construtible =
        std::integral_constant<bool,
                               std::is_nothrow_constructible<T, T1>::value
                                   && std::is_nothrow_constructible<U, U1>::value
                                   && std::is_nothrow_constructible<V, V1>::value
                                   && std::is_nothrow_constructible<Z, Z1>::value>;

    using is_nt_move_const =
        std::integral_constant<bool,
                               std::is_nothrow_move_constructible<T>::value
                                   && std::is_nothrow_move_constructible<U>::value
                                   && std::is_nothrow_move_constructible<V>::value
                                   && std::is_nothrow_move_constructible<Z>::value>;

    using is_nt_move_ass = std::integral_constant<bool,
                                                  std::is_nothrow_move_assignable<T>::value
                                                      && std::is_nothrow_move_assignable<U>::value
                                                      && std::is_nothrow_move_assignable<V>::value
                                                      && std::is_nothrow_move_assignable<Z>::value>;

public:
    using first_type = T;
    using second_type = U;
    using third_type = V;
    using fourth_type = Z;

    template <typename T1, typename U1, typename V1, typename Z1>
    quadriple(T1 &&x, U1 &&y, V1 &&z, Z1 &&z1) noexcept(is_nt_construtible<T1, U1, V1, Z1>::value)
        : base1(static_cast<T1 &&>(x)), base2(static_cast<U1 &&>(y)), base3(static_cast<V1 &&>(z)),
          base4(static_cast<Z1 &&>(z1))
    {}

    quadriple() = default;
    quadriple(quadriple const &) = default;
    quadriple(quadriple &&) noexcept(is_nt_move_const::value) = default;

    auto operator=(quadriple const &) -> quadriple & = default;
    auto operator=(quadriple &&) noexcept(is_nt_move_ass::value) -> quadriple & = default;

    // get reference to T
    ASTL_NODISCARD constexpr auto m1() & noexcept -> T & { return ebo_get<tag1>(*this); }

    // get const reference to T
    ASTL_NODISCARD constexpr auto m1() const & noexcept -> T const &
    {
        return ebo_get<tag1>(*this);
    }

    // get rvalue reference to T
    ASTL_NODISCARD constexpr auto m1() && noexcept -> T &&
    {
        return ebo_get<tag1>(std::move(*this));
    }

    // get const rvalue reference to T
    ASTL_NODISCARD constexpr auto m1() const && noexcept -> T const &&
    {
        return ebo_get<tag1>(std::move(*this));
    }

    // get reference to U
    ASTL_NODISCARD constexpr auto m2() & noexcept -> U & { return ebo_get<tag2>(*this); }

    // get const reference to U
    ASTL_NODISCARD constexpr auto m2() const & noexcept -> U const &
    {
        return ebo_get<tag2>(*this);
    }

    // get rvalue reference to U
    ASTL_NODISCARD constexpr auto m2() && noexcept -> U &&
    {
        return ebo_get<tag2>(std::move(*this));
    }

    // get const rvalue reference to U
    ASTL_NODISCARD constexpr auto m2() const && noexcept -> U const &&
    {
        return ebo_get<tag2>(std::move(*this));
    }

    // get reference to V
    ASTL_NODISCARD constexpr auto m3() & noexcept -> V & { return ebo_get<tag3>(*this); }

    // get const reference to V
    ASTL_NODISCARD constexpr auto m3() const & noexcept -> V const &
    {
        return ebo_get<tag3>(*this);
    }

    // get rvalue reference to V
    ASTL_NODISCARD constexpr auto m3() && noexcept -> V &&
    {
        return ebo_get<tag3>(std::move(*this));
    }

    // get const rvalue reference to V
    ASTL_NODISCARD constexpr auto m3() const && noexcept -> V const &&
    {
        return ebo_get<tag3>(std::move(*this));
    }

    // get reference to Z
    ASTL_NODISCARD constexpr auto m4() & noexcept -> Z & { return ebo_get<tag4>(*this); }

    // get const reference to Z
    ASTL_NODISCARD constexpr auto m4() const & noexcept -> Z const &
    {
        return ebo_get<tag4>(*this);
    }

    // get rvalue reference to Z
    ASTL_NODISCARD constexpr auto m4() && noexcept -> Z &&
    {
        return ebo_get<tag4>(std::move(*this));
    }

    // get const rvalue reference to Z
    ASTL_NODISCARD constexpr auto m4() const && noexcept -> Z const &&
    {
        return ebo_get<tag4>(std::move(*this));
    }
};

#if HAS_DEDUCTION_GUIDES

template <typename T, typename U, typename V, typename Z>
quadriple(T, U, V, Z)->quadriple<T, U, V, Z>;

#endif //#if HAS_DEDUCTION_GUIDES

/// Two triple of the same type are equal iff their members are equal.
template <typename T, typename U, typename V, typename Z>
ASTL_NODISCARD constexpr auto operator==(quadriple<T, U, V, Z> const &x,
                                         quadriple<T, U, V, Z> const &y) -> bool
{
    return x.m1 == y.m1 && x.m2() == y.m2 && x.m3() == y.m3() && x.m4() == y.m4();
}

template <typename T, typename U, typename V, typename Z>
ASTL_NODISCARD constexpr auto operator<(quadriple<T, U, V, Z> const &x,
                                        quadriple<T, U, V, Z> const &y) noexcept -> bool
{
    if (x.m1() < y.m1()) return true;
    if (y.m1() < x.m1()) return false;

    if (x.m2() < y.m2()) return true;
    if (y.m2() < x.m2()) return false;

    if (x.m3() < y.m3()) return true;
    if (y.m3() < x.m3()) return false;

    if (x.m4() < y.m4()) return true;
    if (y.m4() < x.m4()) return false;

    return false;
}

/// Uses @c operator== to find the result.
template <typename T, typename U, typename V, typename Z>
ASTL_NODISCARD constexpr auto operator!=(quadriple<T, U, V, Z> const &x,
                                         quadriple<T, U, V, Z> const &y) noexcept -> bool
{
    return !(x == y);
}

/// Uses @c operator< to find the result.
template <typename T, typename U, typename V, typename Z>
ASTL_NODISCARD constexpr auto operator>(quadriple<T, U, V, Z> const &x,
                                        quadriple<T, U, V, Z> const &y) noexcept -> bool
{
    return y < x;
}

/// Uses @c operator< to find the result.
template <typename T, typename U, typename V, typename Z>
ASTL_NODISCARD constexpr auto operator<=(quadriple<T, U, V, Z> const &x,
                                         quadriple<T, U, V, Z> const &y) noexcept -> bool
{
    return !(y < x);
}

/// Uses @c operator< to find the result.
template <typename T, typename U, typename V, typename Z>
ASTL_NODISCARD constexpr auto operator>=(quadriple<T, U, V, Z> const &x,
                                         quadriple<T, U, V, Z> const &y) noexcept -> bool
{
    return !(x < y);
}

namespace internal_quad
{
template <std::size_t Idx, typename Quadriple> auto quadriple_get(Quadriple &&q)
{
    if constexpr (Idx == 0) return static_cast<Quadriple &&>(q).m1();
    else if constexpr (Idx == 1)
        return static_cast<Quadriple &&>(q).m2();
    else if constexpr (Idx == 2)
        return static_cast<Quadriple &&>(q).m3();
    else if constexpr (Idx == 3)
        return static_cast<Quadriple &&>(q).m4();
    else {
        static_assert(Idx < 4, "quadriple index out of bounds");
        return q.m1();
    }
}

} // namespace internal_quad
} // namespace astl

namespace std
{
// TUPLE INTERFACE TO quadriple
template <typename T, typename U, typename V, typename Z>
struct tuple_size<astl::quadriple<T, U, V, Z>>: integral_constant<size_t, 4> { // size of quadriple
};

template <size_t Idx, typename T, typename U, typename V, typename Z>
struct tuple_element<Idx, astl::quadriple<T, U, V, Z>> {
    // struct to determine type of element Idx in quadriple
    static_assert(Idx < 4, "quadriple index out of bounds");

    using type = conditional_t<Idx == 2, V, Z>;
};

template <typename T, typename U, typename V, typename Z>
struct tuple_element<0, astl::quadriple<T, U, V, Z>> {
    using type = T;
};

template <typename T, typename U, typename V, typename Z>
struct tuple_element<1, astl::quadriple<T, U, V, Z>> {
    using type = U;
};

} // namespace std

namespace astl
{
template <std::size_t Idx, typename T, typename U, typename V, typename Z>
ASTL_NODISCARD constexpr auto get(quadriple<T, U, V, Z> &q) noexcept
    -> std::tuple_element_t<Idx, quadriple<T, U, V, Z>> &
{ // get reference to element at Idx in quadriple q
    return internal_quad::quadriple_get<Idx>(q);
}

template <typename T, typename U, typename V, typename Z>
ASTL_NODISCARD constexpr auto get(quadriple<T, U, V, Z> &q) noexcept -> T &
{ // get reference to element T in quadriple q
    return q.m1();
}

template <typename U, typename T, typename V, typename Z>
ASTL_NODISCARD constexpr auto get(quadriple<T, U, V, Z> &q) noexcept -> U &
{ // get reference to element U in quadriple q
    return q.m2();
}

template <typename V, typename U, typename T, typename Z>
ASTL_NODISCARD constexpr auto get(quadriple<T, U, V, Z> &q) noexcept -> V &
{ // get reference to element V in quadriple q
    return q.m3();
}

template <typename Z, typename V, typename U, typename T>
ASTL_NODISCARD constexpr auto get(quadriple<T, U, V, Z> &q) noexcept -> Z &
{ // get reference to element Z in quadriple q
    return q.m4();
}

template <std::size_t Idx, typename T, typename U, typename V, typename Z>
ASTL_NODISCARD constexpr auto get(quadriple<T, U, V, Z> const &q) noexcept
    -> std::tuple_element_t<Idx, quadriple<T, U, V, Z>> const &
{ // get const reference to element at Idx in quadriple q
    return internal_quad::quadriple_get<Idx>(q);
}

template <typename T, typename U, typename V, typename Z>
ASTL_NODISCARD constexpr auto get(quadriple<T, U, V, Z> const &q) noexcept -> T const &
{ // get const reference to element T in quadriple q
    return q.m1();
}

template <typename U, typename T, typename V, typename Z>
ASTL_NODISCARD constexpr auto get(quadriple<T, U, V, Z> const &q) noexcept -> U const &
{ // get const reference to element U in quadriple q
    return q.m2();
}

template <typename V, typename U, typename T, typename Z>
ASTL_NODISCARD constexpr auto get(quadriple<T, U, V, Z> const &q) noexcept -> V const &
{ // get const reference to element V in quadriple q
    return q.m3();
}

template <typename Z, typename V, typename U, typename T>
ASTL_NODISCARD constexpr auto get(quadriple<T, U, V, Z> const &q) noexcept -> Z const &
{ // get const reference to element Z in quadriple q
    return q.m4();
}

template <std::size_t Idx, typename T, typename U, typename V, typename Z>
ASTL_NODISCARD constexpr auto get(quadriple<T, U, V, Z> &&q) noexcept
    -> std::tuple_element_t<Idx, quadriple<T, U, V, Z>> &&
{ // get rvalue reference to element at Idx in quadriple q
    return internal_quad::quadriple_get<Idx>(std::move(q));
}

template <typename T, typename U, typename V, typename Z>
ASTL_NODISCARD constexpr auto get(quadriple<T, U, V, Z> &&q) noexcept -> T &&
{ // get rvalue reference to element T in quadriple q
    return std::move(q).m1();
}

template <typename U, typename T, typename V, typename Z>
ASTL_NODISCARD constexpr auto get(quadriple<T, U, V, Z> &&q) noexcept -> U &&
{ // get rvalue reference to element U in quadriple q
    return std::move(q).m2();
}

template <typename V, typename U, typename T, typename Z>
ASTL_NODISCARD constexpr auto get(quadriple<T, U, V, Z> &&q) noexcept -> V &&
{ // get rvalue reference to element V in quadriple q
    return std::move(q).m3();
}

template <typename Z, typename V, typename U, typename T>
ASTL_NODISCARD constexpr auto get(quadriple<T, U, V, Z> &&q) noexcept -> Z &&
{ // get rvalue reference to element V in quadriple q
    return std::move(q).m4();
}

template <std::size_t Idx, typename T, typename U, typename V, typename Z>
ASTL_NODISCARD constexpr auto get(quadriple<T, U, V, Z> const &&q) noexcept
    -> std::tuple_element_t<Idx, quadriple<T, U, V, Z>> const &&
{ // get const rvalue reference to element at Idx in quadriple q
    return internal_quad::quadriple_get<Idx>(std::move(q));
}

template <typename T, typename U, typename V, typename Z>
ASTL_NODISCARD constexpr auto get(quadriple<T, U, V, Z> const &&q) noexcept -> T const &&
{ // get const rvalue reference to element T in quadriple q
    return std::move(q).m1();
}

template <typename U, typename T, typename V, typename Z>
ASTL_NODISCARD constexpr auto get(quadriple<T, U, V, Z> const &&q) noexcept -> U const &&
{ // get const rvalue reference to element U in quadriple q
    return std::move(q).m2();
}

template <typename V, typename U, typename T, typename Z>
ASTL_NODISCARD constexpr auto get(quadriple<T, U, V, Z> const &&q) noexcept -> V const &&
{ // get const rvalue reference to element V in quadriple q
    return std::move(q).m3();
}

template <typename Z, typename V, typename U, typename T>
ASTL_NODISCARD constexpr auto get(quadriple<T, U, V, Z> const &&q) noexcept -> Z const &&
{ // get const rvalue reference to element Z in quadriple q
    return std::move(q).m4();
}

} // namespace astl

#endif // ASTL_INCLUDE_UTILITY_HPP
