//
// Created by Rijad on 04-Sep-18.
//

#ifndef ASTL_INCLUDE_ITERATOR_FUNCTION_ADAPTORS_HPP
#define ASTL_INCLUDE_ITERATOR_FUNCTION_ADAPTORS_HPP

#include <memory>
#include <type_traits>

#include "functional.hpp"

namespace astl
{
template <typename Comparator>
// requires Comparator, returns type convertible to bool, arguments two iterators
struct iter_comp_iter_t {
    Comparator _comp;

    iter_comp_iter_t() = default;

    explicit constexpr iter_comp_iter_t(Comparator comp) : _comp(std::move(comp)) {}

    template <typename I1, typename I2>
    // requires I1 Iterator
    // requires I2 Iterator
    ASTL_NODISCARD constexpr auto operator()(I1 i1, I2 i2) const noexcept -> bool
    {
        return bool(_comp(*i1, *i2));
    }
};

using iter_less_iter_t = iter_comp_iter_t<std::less<>>;

using iter_equal_to_iter_t = iter_comp_iter_t<std::equal_to<>>;

ASTL_NODISCARD constexpr auto iter_equal_to_iter() noexcept -> iter_equal_to_iter_t
{
    return iter_equal_to_iter_t{};
}

constexpr auto iter_less_iter() noexcept -> iter_less_iter_t { return iter_less_iter_t{}; }

template <typename Comparator>
// requires Comparator, returns type convertible to bool, arguments two iterators
ASTL_NODISCARD constexpr auto iter_comp_iter(Comparator comp) -> iter_comp_iter_t<Comparator>
{
    return iter_comp_iter_t<Comparator>{std::move(comp)};
}

template <typename Comparator>
// requires Comparator, returns type convertible to bool, arguments iterator and ...
struct iter_comp_val_t {
    Comparator _comp;

    iter_comp_val_t() = default;

    explicit constexpr iter_comp_val_t(Comparator comp) : _comp(std::move(comp)) {}

    explicit constexpr iter_comp_val_t(iter_comp_iter_t<Comparator> const &comp) noexcept(
        std::is_nothrow_copy_constructible<Comparator>::value)
        : _comp(comp._comp)
    {}

    explicit constexpr iter_comp_val_t(iter_comp_iter_t<Comparator> &&comp) noexcept(
        std::is_nothrow_move_constructible<Comparator>::value)
        : _comp(std::move(comp._comp))
    {}

    template <typename I, typename T>
    // requires I iterator
    // requires T ...
    ASTL_NODISCARD constexpr auto operator()(I i, T &&val) const noexcept -> bool
    {
        return bool(_comp(*i, val));
    }
};

using iter_equal_to_val_t = iter_comp_val_t<std::equal_to<>>;

ASTL_NODISCARD constexpr auto iter_equal_to_val() noexcept -> iter_equal_to_val_t
{
    return iter_equal_to_val_t{};
}

ASTL_NODISCARD constexpr auto iter_comp_val(iter_equal_to_iter_t) noexcept -> iter_equal_to_val_t
{
    return iter_equal_to_val_t{};
}

using iter_less_val_t = iter_comp_val_t<std::less<>>;

ASTL_NODISCARD constexpr auto iter_less_val() noexcept -> iter_less_val_t
{
    return iter_less_val_t{};
}

ASTL_NODISCARD constexpr auto iter_comp_val(iter_less_iter_t) noexcept -> iter_less_val_t
{
    return iter_less_val_t{};
}

template <typename Comparator>
// requires Comparator, returns type convertible to bool, arguments iterator and ...
ASTL_NODISCARD constexpr auto
iter_comp_val(Comparator comp) noexcept(std::is_nothrow_constructible<iter_comp_val_t<Comparator>, Comparator>::value)
    -> iter_comp_val_t<Comparator>
{
    return iter_comp_val_t<Comparator>{std::move(comp)};
}

template <typename Comparator>
ASTL_NODISCARD constexpr auto iter_comp_val(iter_comp_iter_t<Comparator> comp) -> iter_comp_val_t<Comparator>
{
    return iter_comp_val_t<Comparator>{std::move(comp)};
}

template <typename Comparator>
// requires Comparator, returns type convertible to bool, arguments iterator and ...
struct val_comp_iter_t {
    Comparator _comp;

    val_comp_iter_t() = default;

    explicit constexpr val_comp_iter_t(Comparator comp) : _comp(std::move(comp)) {}

    explicit constexpr val_comp_iter_t(iter_comp_iter_t<Comparator> const &comp) : _comp(comp._comp) {}

    explicit constexpr val_comp_iter_t(iter_comp_iter_t<Comparator> &&comp) : _comp(std::move(comp._comp))
    {}

    template <typename T, typename I>
    // requires T ...
    // requires I Iterator
    ASTL_NODISCARD constexpr auto operator()(T &val, I i) const noexcept -> bool
    {
        return bool(_comp(val, *i));
    }
};

using val_less_iter_t = val_comp_iter_t<std::less<>>;

template <typename Comparator>
// requires Comparator, returns type convertible to bool, arguments iterator and ...
ASTL_NODISCARD constexpr auto val_comp_iter(Comparator comp) -> val_comp_iter_t<Comparator>
{
    return val_comp_iter_t<Comparator>{std::move(comp)};
}

template <typename Comparator>
// requires Comparator, returns type convertible to bool, arguments iterator and ...
ASTL_NODISCARD constexpr auto val_comp_iter(iter_comp_iter_t<Comparator> comp) -> val_comp_iter_t<Comparator>
{
    return val_comp_iter_t<Comparator>{std::move(comp)};
}

ASTL_NODISCARD constexpr auto val_less_iter() noexcept -> val_less_iter_t
{
    return val_less_iter_t{};
}

ASTL_NODISCARD constexpr auto val_comp_iter(iter_less_iter_t) noexcept -> val_less_iter_t
{
    return val_less_iter_t{};
}

template <typename UnaryPredicate>
// requires UnaryPredicate, returns type convertible to bool, argument iterator
struct iter_pred_t {
    UnaryPredicate _pred;

    explicit constexpr iter_pred_t(UnaryPredicate pred) : _pred(std::move(pred)) {}

    template <typename I>
    // requires I Iterator
    ASTL_NODISCARD constexpr auto operator()(I i) const noexcept -> bool
    {
        return bool(_pred(*i));
    }
};

template <typename UnaryPredicate>
// requires UnaryPredicate, returns type convertible to bool, argument iterator
ASTL_NODISCARD constexpr auto pred_iter(UnaryPredicate pred) -> iter_pred_t<UnaryPredicate>
{
    return iter_pred_t<UnaryPredicate>{std::move(pred)};
}

template <typename Comparator, typename T>
// requires Comparator, returns type convertible to bool, arguments iterator and T
// requires T ...
struct iter_comp_to_val_t {
    Comparator _comp;
    T *_value;

    explicit constexpr iter_comp_to_val_t(T &val) noexcept : _comp(), _value(std::addressof(val)) {}

    constexpr iter_comp_to_val_t(Comparator comp, T &value)
        : _comp(std::move(comp)), _value(std::addressof(value))
    {}

    template <typename U>
    ASTL_NODISCARD constexpr auto operator()(U const &i) const noexcept -> bool
    {
        return bool(_comp(i, *_value));
    }
};

template <typename T> using iter_equals_val_t = iter_comp_to_val_t<std::equal_to<>, T>;

template <typename T>
ASTL_NODISCARD constexpr auto iter_equals_val(T &val) noexcept -> iter_equals_val_t<T>
{
    return iter_equals_val_t<T>{val};
}

template <typename Comparator, typename T>
// requires Comparator, returns type convertible to bool, arguments iterator and T
// requires T ...
ASTL_NODISCARD constexpr auto iter_comp_val(Comparator comp, T &val) -> iter_comp_to_val_t<Comparator, T>
{
    return iter_comp_to_val_t<Comparator, T>{std::move(comp), val};
}

template <typename Comparator, typename I1>
// requires Comparator, returns type convertible to bool, arguments iterator and
// value_type(I1) requires I1 Iterator
struct iter_comp_to_iter_t {
    Comparator _comp;
    I1 _it;

    explicit constexpr iter_comp_to_iter_t(I1 i1) : _comp(), _it(std::move(i1)) {}

    constexpr iter_comp_to_iter_t(Comparator comp, I1 i) : _comp(std::move(comp)), _it(std::move(i)) {}

    template <typename I2>
    // requires I2 iterator
    ASTL_NODISCARD constexpr auto operator()(I2 i2) const noexcept -> bool
    {
        return bool(_comp(*i2, *_it));
    }
};

template <typename I> using iter_equals_iter_t = iter_comp_to_iter_t<std::equal_to<>, I>;

template <typename I>
// requires I Iterator
ASTL_NODISCARD constexpr auto iter_comp_iter(iter_equal_to_iter_t, I i) -> iter_equals_iter_t<I>
{
    return iter_equals_iter_t<I>{i};
}

template <typename Comparator, typename I>
ASTL_NODISCARD constexpr auto iter_comp_iter(iter_comp_iter_t<Comparator> comp, I i)
    -> iter_comp_to_iter_t<Comparator, I>
{
    return iter_comp_to_iter_t<Comparator, I>{std::move(comp._comp), i};
}
} // namespace astl

#endif // ASTL_INCLUDE_ITERATOR_FUNCTION_ADAPTORS_HPP
