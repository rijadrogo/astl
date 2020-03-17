//
// Created by Rijad on 13-Aug-18.
//

#ifndef ASTL_INCLUDE_CONCAT_RANGES_HPP
#define ASTL_INCLUDE_CONCAT_RANGES_HPP

#include <cstdio>
#include <cstdlib>
#include <tuple>
#include <type_traits>

#include "iterator.hpp"
#include "iterator_range.hpp"
#include "range_access.hpp"

#define ZX_PANIC(fmt, ...)                                                                         \
    do {                                                                                           \
        std::printf(fmt, ##__VA_ARGS__);                                                           \
        std::abort();                                                                              \
    } while (0)

namespace astl
{
/// Iterator wrapper that concatenates sequences together.
///
/// This can concatenate different iterators, even with different types, into a
/// single iterator provided the value types of all the concatenated iterators
/// expose `reference` and `pointer` types that can be converted to `ValueT &`
/// and `ValueT *` respectively. It doesn't support more interesting/customized
/// pointer or reference types.
///
/// Currently this only supports forward or higher iterator categories as inputs
/// and always exposes a forward iterator interface.
template <typename ValueT, typename... IterTs>
struct concat_iterator
    : iterator_facade_base<concat_iterator<ValueT, IterTs...>, std::forward_iterator_tag, ValueT> {
private:
    using base_t = typename concat_iterator::iterator_facade_base;

    /// We store both the current and end iterators for each concatenated sequence
    /// in a tuple of pairs.
    ///
    /// Note that something like iterator_range seems nice at first here, but the
    /// range properties are of little benefit and end up getting in the way
    /// because we need to do mutation on the current iterators.
    std::tuple<IterTs...> begins;
    std::tuple<IterTs...> ends;

    /// Attempts to increment a specific iterator.
    ///
    /// Returns true if it was able to increment the iterator. Returns false if
    /// the iterator is already at the end iterator.
    template <std::size_t Index> constexpr auto increment_helper() -> bool
    {
        auto &&begin(std::get<Index>(begins));
        auto &&end(std::get<Index>(ends));

        if (begin == end) return false;

        ++begin;
        return true;
    }

    /// Increments the first non-end iterator.
    ///
    /// It is an error to call this with all iterators at the end.
    template <std::size_t... Ns> constexpr auto increment(std::index_sequence<Ns...>) -> void
    {
        // Build a sequence of functions to increment each iterator if
        // possible.
        bool (concat_iterator::*increment_helper_fns[])() = {
            &concat_iterator::template increment_helper<Ns>...};
        // Loop over them, and stop as soon as we succeed at
        // incrementing one.
        for (auto &increment_helper_fn : increment_helper_fns) {
            if ((this->*increment_helper_fn)()) return;
        }
        ZX_PANIC("Attempted to increment an end concat iterator!");
    }

    /// Returns null if the specified iterator is at the end. Otherwise,
    /// dereferences the iterator and returns the address of the resulting
    /// reference.
    template <std::size_t Index> constexpr auto get_helper() const noexcept -> ValueT *
    {
        auto &&begin(std::get<Index>(begins));
        auto &&end(std::get<Index>(ends));
        if (begin == end) return nullptr;

        return &*begin;
    }

    /// Finds the first non-end iterator, dereferences, and returns the resulting
    /// reference.
    ///
    /// It is an error to call this with all iterators at the end.
    // ReSharper disable once CppNotAllPathsReturnValue
    template <size_t... Ns>
    constexpr auto get(std::index_sequence<Ns...>) const noexcept -> ValueT &
    {
        // Build a sequence of functions to get from iterator if possible.
        ValueT *(concat_iterator::*get_helper_fns[])()
            const = {&concat_iterator::template get_helper<Ns>...};
        // Loop over them, and return the first result we find.
        for (auto &get_helper_fn : get_helper_fns)
            if (ValueT *p = (this->*get_helper_fn)()) return *p;

        ZX_PANIC("Attempted to get a pointer from an end concat iterator!");
    }

public:
    concat_iterator(concat_iterator const &cc) = default;

    constexpr concat_iterator(concat_iterator &cc) : begins(cc.begins), ends(cc.ends) {}

    concat_iterator(concat_iterator &&cc) noexcept(
        std::is_nothrow_move_constructible<decltype(begins)>::value
            &&std::is_nothrow_move_constructible<decltype(ends)>::value) = default;

    /// Constructs an iterator from a squence of ranges.
    ///
    /// We need the full range to know how to switch between each of the
    /// iterators.
    template <typename... RangeTs>
    explicit constexpr concat_iterator(RangeTs &&... ranges)
        : begins(adl::begin(ranges)...), ends(adl::end(ranges)...)
    {}

    using base_t::operator++;

    constexpr auto operator++() -> concat_iterator &
    {
        this->increment(std::index_sequence_for<IterTs...>());
        return *this;
    }

    constexpr auto operator*() const noexcept -> ValueT &
    {
        return this->get(std::index_sequence_for<IterTs...>());
    }

    ASTL_NODISCARD constexpr auto operator==(concat_iterator const &rhs) const noexcept -> bool
    {
        return begins == rhs.begins && ends == rhs.ends;
    }
};

namespace internal_concat
{
/// Helper to store a sequence of ranges being concatenated and access them.
///
/// This is designed to facilitate providing actual storage when temporaries are
/// passed into the constructor such that we can use it as part of range based
/// for loops.
template <typename ValueT, typename... RangeTs> struct concat_range {
    using iterator = concat_iterator<ValueT, begin_t<RangeTs &>...>;

private:
    std::tuple<RangeTs...> ranges;

    template <std::size_t... Ns> constexpr auto begin_impl(std::index_sequence<Ns...>) -> iterator
    {
        return iterator(std::get<Ns>(ranges)...);
    }

    template <std::size_t... Ns>
    constexpr auto begin_impl(std::index_sequence<Ns...>) const -> iterator
    {
        return iterator(std::get<Ns>(ranges)...);
    }

    template <std::size_t... Ns> constexpr auto end_impl(std::index_sequence<Ns...>) -> iterator
    {
        return iterator(
            astl::make_range(adl::end(std::get<Ns>(ranges)), adl::end(std::get<Ns>(ranges)))...);
    }

    template <std::size_t... Ns>
    constexpr auto end_impl(std::index_sequence<Ns...>) const -> iterator
    {
        return iterator(
            astl::make_range(adl::end(std::get<Ns>(ranges)), adl::end(std::get<Ns>(ranges)))...);
    }

public:
    constexpr explicit concat_range(RangeTs &&... ranges)
        : ranges(static_cast<RangeTs &&>(ranges)...)
    {}

    ASTL_NODISCARD constexpr auto begin() -> iterator
    {
        return this->begin_impl(std::index_sequence_for<RangeTs...>{});
    }

    ASTL_NODISCARD constexpr auto begin() const -> iterator
    {
        return this->begin_impl(std::index_sequence_for<RangeTs...>{});
    }

    ASTL_NODISCARD constexpr auto end() -> iterator
    {
        return this->end_impl(std::index_sequence_for<RangeTs...>{});
    }

    ASTL_NODISCARD constexpr auto end() const -> iterator
    {
        return this->end_impl(std::index_sequence_for<RangeTs...>{});
    }
};

template <typename... Types> struct first_type;

template <typename First, typename... Rest> struct first_type<First, Rest...> {
    using type = First;
};
}// namespace internal_concat

/// Concatenated range across two or more ranges.
///
/// The desired value type must be explicitly specified.
template <typename ValueT, typename... RangeTs>
ASTL_NODISCARD constexpr auto concat(RangeTs &&... ranges)
    -> internal_concat::concat_range<ValueT, RangeTs...>
{
    return internal_concat::concat_range<ValueT, RangeTs...>(static_cast<RangeTs &&>(ranges)...);
}

/// Concatenated range across two or more ranges.
///
/// The desired value type is value_type of first container.
template <typename... RangeTs,
          typename ValueT = typename std::remove_cv<typename std::remove_reference<range_value_type<
              typename internal_concat::first_type<RangeTs...>::type>>::type>::type>
ASTL_NODISCARD constexpr auto concat(RangeTs &&... ranges)
    -> internal_concat::concat_range<ValueT, RangeTs...>
{
    return internal_concat::concat_range<ValueT, RangeTs...>(static_cast<RangeTs &&>(ranges)...);
}
}// namespace astl

#undef ZX_PANIC

#endif// ASTL_INCLUDE_CONCAT_RANGES_HPP
