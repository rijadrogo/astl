//
// Created by Rijad on 13-Aug-18.
//

#ifndef ASTL_INCLUDE_COUNT_ITERATOR_HPP
#define ASTL_INCLUDE_COUNT_ITERATOR_HPP

#include <type_traits>

#include "iterator.hpp"
#include "iterator_range.hpp"
#include "range_access.hpp"

namespace astl
{
template <typename I, typename CountT = iter_diff_type<I>>
struct count_interator: iterator_adaptor_base<count_interator<I, CountT>, I> {
private:
    CountT index;
    using BaseT = iterator_adaptor_base<count_interator<I, CountT>, I>;

public:
    explicit constexpr count_interator(I i, CountT s = 0) : BaseT(std::move(i)), index(s) {}

    constexpr auto operator++() -> count_interator &
    {
        BaseT::operator++();
        ++index;
        return *this;
    }

    constexpr auto operator++(int) -> count_interator
    {
        count_interator tmp(*this);
        this->operator++();
        return tmp;
    }

    template <bool B = is_random_access_it_v<I>> constexpr auto operator--() -> count_interator &
    {
        BaseT::operator--();
        --index;
        return *this;
    }

    template <bool B = is_random_access_it_v<I>> constexpr auto operator--(int) -> count_interator
    {
        count_interator tmp(*this);
        BaseT::operator--();
        --index;
        return tmp;
    }

    template <bool B = is_random_access_it_v<I>>
    constexpr auto operator-=(iter_diff_type<I> n) -> count_interator &
    {
        index -= n;
        this->i -= n;
        return *this;
    }

    template <bool B = is_random_access_it_v<I>>
    constexpr auto operator+=(iter_diff_type<I> n) -> count_interator &
    {
        index += n;
        this->i += n;
        return *this;
    }

    template <bool B = is_random_access_it_v<I>>
    constexpr auto operator-(iter_diff_type<I> n) -> count_interator
    {
        return count_interator{this->base() - n, index - n};
    }

    template <bool B = is_random_access_it_v<I>>
    constexpr auto operator-(count_interator const &it) -> iter_diff_type<I>
    {
        return this->base() - it.base();
    }

    template <bool B = is_random_access_it_v<I>>
    constexpr auto operator+(iter_diff_type<I> n) -> count_interator
    {
        return count_interator{this->base() + n, index + n};
    }

    template <bool B = is_random_access_it_v<I>>
    constexpr auto operator+(count_interator const &it) -> count_interator
    {
        return count_interator{this->base() + it.base(), index + it.index};
    }

    ASTL_NODISCARD constexpr auto counter() const noexcept -> CountT { return index; }
};

template <typename I, typename CountT>
constexpr auto operator-(iter_diff_type<I> n, count_interator<I, CountT> const &i)
    -> count_interator<I, CountT>
{
    return count_interator<I, CountT>{n - i.base(), n - i.counter()};
}

template <typename I, typename CountT>
constexpr auto operator+(iter_diff_type<I> n, count_interator<I, CountT> const &i)
    -> count_interator<I, CountT>
{
    return count_interator<I, CountT>{n + i.base(), n + i.counter()};
}

template <typename I, typename CountT = iter_diff_type<I>>
ASTL_NODISCARD constexpr auto make_count_iterator(I i, CountT n = 0) -> count_interator<I, CountT>
{
    return count_interator<I, CountT>{std::move(i), std::move(n)};
}

template <typename R, typename CountT = range_diff_type<R>>
ASTL_NODISCARD constexpr auto make_count_range(R &&r, CountT n = 0)
{
    return astl::make_range(astl::make_count_iterator(adl::begin(r), n),
                            astl::make_count_iterator(adl::end(r), n));
}
} // namespace astl

#endif // ASTL_INCLUDE_COUNT_ITERATOR_HPP
