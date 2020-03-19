//
// Created by Rijad on 13-Aug-18.
//

#ifndef ASTL_INCLUDE_POINTER_ITERATOR_HPP
#define ASTL_INCLUDE_POINTER_ITERATOR_HPP

#include <type_traits>

#include "iterator.hpp"
#include "iterator_range.hpp"
#include "range_access.hpp"

namespace astl
{
template <typename WrappedIteratorT, typename T = decltype(&*std::declval<WrappedIteratorT>())>
struct pointer_iterator: iterator_adaptor_base<pointer_iterator<WrappedIteratorT, T>,
                                               WrappedIteratorT, iter_cat<WrappedIteratorT>, T> {
private:
    mutable T ptr;
    using BaseT = iterator_adaptor_base<pointer_iterator<WrappedIteratorT, T>, WrappedIteratorT,
                                        iter_cat<WrappedIteratorT>, T>;

public:
    pointer_iterator() = default;

    explicit constexpr pointer_iterator(WrappedIteratorT i)
        : pointer_iterator::iterator_adaptor_base(i)
    {}

    ASTL_NODISCARD constexpr auto operator*() noexcept -> T &
    {
        return ptr = std::addressof(*BaseT::base());
    }

    ASTL_NODISCARD constexpr auto operator*() const noexcept -> T const &
    {
        return ptr = &*BaseT::base();
    }
};

#if HAS_DEDUCTION_GUIDES

template <typename WrappedIteratorT>
pointer_iterator(WrappedIteratorT)->pointer_iterator<WrappedIteratorT>;

#endif // HAS_DEDUCTION_GUIDES

template <typename I>
ASTL_NODISCARD constexpr auto make_pointer_iterator(I i) -> pointer_iterator<I>
{
    return pointer_iterator<I>(i);
}

template <typename I, typename S, typename WrappedIteratorBeginT = I,
          typename WrappedIteratorEndT = S>
ASTL_NODISCARD constexpr auto make_pointer_range(I first, S last)
    -> iterator_range<pointer_iterator<WrappedIteratorBeginT>,
                      pointer_iterator<WrappedIteratorEndT>>
{
    return astl::make_range(pointer_iterator<WrappedIteratorBeginT>(first),
                            pointer_iterator<WrappedIteratorEndT>(last));
}

template <typename RangeT>
ASTL_NODISCARD constexpr auto make_pointer_range(RangeT &&r)
    -> iterator_range<pointer_iterator<begin_t<RangeT &&>>, pointer_iterator<end_t<RangeT &&>>>
{
    return astl::make_pointer_range(adl::begin(r), adl::end(r));
}
} // namespace astl

#endif // ASTL_INCLUDE_POINTER_ITERATOR_HPP
