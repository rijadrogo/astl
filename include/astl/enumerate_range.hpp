//
// Created by Rijad on 13-Aug-18.
//

#ifndef ASTL_INCLUDE_ENUMERATE_RANGE_HPP
#define ASTL_INCLUDE_ENUMERATE_RANGE_HPP

#include <cassert>
#include <cstddef>
#include <limits>
#include <type_traits>

#include "iterator.hpp"
#include "iterator_range.hpp"
#include "range_access.hpp"

namespace astl
{
namespace internal_enumerate
{
template <typename Range> struct enumerator_iter;

template <typename Range> struct result_pair {
private:
    std::size_t curr_index = std::numeric_limits<std::size_t>::max();
    using iterator = iter_of_range<Range>;
    iterator iter;

public:
    friend struct enumerator_iter<Range>;

    constexpr result_pair() = default;

    constexpr result_pair(std::size_t const index, iterator iter) noexcept(
        std::is_nothrow_move_constructible<iterator>::value)
        : curr_index(index), iter(std::move(iter))
    {}

    constexpr auto operator=(result_pair<Range> const &other) -> result_pair<Range> & = default;

    ASTL_NODISCARD constexpr auto index() const noexcept -> std::size_t { return curr_index; }
    ASTL_NODISCARD constexpr auto value() const noexcept -> range_value_type<Range> const &
    {
        return *iter;
    }
    ASTL_NODISCARD constexpr auto value() noexcept -> range_value_type<Range> & { return *iter; }
};

// FIXME inherit from iterator_adaptor_base?
template <typename R>
struct enumerator_iter
    : iterator_facade_base<
          enumerator_iter<R>, typename std::iterator_traits<iter_of_range<R>>::iterator_category,
          result_pair<R>, typename std::iterator_traits<iter_of_range<R>>::difference_type,
          typename std::iterator_traits<iter_of_range<R>>::pointer,
          typename std::iterator_traits<iter_of_range<R>>::reference> {
private:
    using ResultType = result_pair<R>;
    ResultType result;

public:
    explicit constexpr enumerator_iter(iter_of_range<R> end_iter)
        : result(std::numeric_limits<std::size_t>::max(), std::move(end_iter))
    {}

    constexpr enumerator_iter(std::size_t const index, iter_of_range<R> iter)
        : result(index, std::move(iter))
    {}

    constexpr auto operator*() noexcept -> ResultType & { return result; }
    constexpr auto operator*() const noexcept -> ResultType const & { return result; }

    constexpr auto operator++() noexcept -> enumerator_iter<R> &
    {
        assert(result.curr_index != std::numeric_limits<size_t>::max());
        ++result.iter;
        ++result.curr_index;
        return *this;
    }

    ASTL_NODISCARD constexpr auto operator==(enumerator_iter<R> const &rhs) const noexcept -> bool
    {
        // Don't compare indices here, only iterators.  It's possible for an end
        // iterator to have different indices depending on whether it was created by
        // calling std::end() versus incrementing a valid iterator.
        return result.iter == rhs.result.iter;
    }

    ASTL_NODISCARD constexpr auto operator<(enumerator_iter<R> const &rhs) const noexcept -> bool
    {
        return result.iter < rhs.result.iter;
    }

    constexpr auto operator=(enumerator_iter<R> const &other) -> enumerator_iter<R> & = default;
};

template <typename Range> struct enumerator {
private:
    Range the_range;

public:
    using iterator = enumerator_iter<Range>;

    explicit constexpr enumerator(Range &&range) : the_range(static_cast<Range &&>(range)) {}

    ASTL_NODISCARD constexpr auto begin() -> enumerator_iter<Range>
    {
        return {0, adl::begin(the_range)};
    }

    ASTL_NODISCARD constexpr auto begin() const -> enumerator_iter<Range const>
    {
        return {0, adl::begin(the_range)};
    }

    ASTL_NODISCARD constexpr auto end() -> enumerator_iter<Range> { return {adl::end(the_range)}; }

    ASTL_NODISCARD constexpr auto end() const -> enumerator_iter<Range const>
    {
        return {adl::end(the_range)};
    }
};
}// namespace internal_enumerate

/// Given an input range, returns a new range whose values are are pair (A,B)
/// such that A is the 0-based index of the item in the sequence, and B is the
/// value from the original sequence. Example:
///
/// std::vector<char> Items = {'A', 'B', 'C', 'D'};
/// for (auto X : enumerate(Items)) {
///   printf("Item %d - %c\n", X.index(), X.value());
/// }
///
/// Output:
///   Item 0 - A
///   Item 1 - B
///   Item 2 - C
///   Item 3 - D
///

template <typename Range>
ASTL_NODISCARD constexpr auto enumerate(Range &&the_range) -> internal_enumerate::enumerator<Range>
{
    return internal_enumerate::enumerator<Range>(static_cast<Range &&>(the_range));
}

template <typename I, typename S>
ASTL_NODISCARD constexpr auto enumerate(I first, S last)
    -> internal_enumerate::enumerator<iterator_range<I, S>>
{
    return internal_enumerate::enumerator<iterator_range<I, S>>(
        astl::make_range(std::move(first), std::move(last)));
}
}// namespace astl

#endif// ASTL_INCLUDE_ENUMERATE_RANGE_HPP
