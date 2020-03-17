//
// Created by Rijad on 13-Aug-18.
//

#ifndef ASTL_INCLUDE_POINTEE_ITERATOR_HPP
#define ASTL_INCLUDE_POINTEE_ITERATOR_HPP

#include <type_traits>

#include "iterator.hpp"
#include "iterator_range.hpp"
#include "range_access.hpp"

namespace astl
{
/// \brief An iterator type that allows iterating over the pointees via some
/// other iterator.
///
/// The typical usage of this is to expose a type that iterates over Ts, but
/// which is implemented with some iterator over T*s:
template <typename WrappedIteratorT,
          typename T =
              typename std::remove_reference<decltype(**std::declval<WrappedIteratorT>())>::type>
struct pointee_iterator
    : iterator_adaptor_base<pointee_iterator<WrappedIteratorT, T>, WrappedIteratorT,
                            typename std::iterator_traits<WrappedIteratorT>::iterator_category, T> {
private:
    using BaseT =
        iterator_adaptor_base<pointee_iterator<WrappedIteratorT, T>, WrappedIteratorT,
                              typename std::iterator_traits<WrappedIteratorT>::iterator_category,
                              T>;

public:
    pointee_iterator() = default;

    explicit constexpr pointee_iterator(WrappedIteratorT i) : BaseT(std::move(i)) {}

    ASTL_NODISCARD constexpr auto operator*() const noexcept -> T const &
    {
        return **BaseT::base();
    }
    ASTL_NODISCARD constexpr auto operator*() noexcept -> T & { return **BaseT::base(); }
};

#if HAS_DEDUCTION_GUIDES

template <typename WrappedIteratorT>
pointee_iterator(WrappedIteratorT)->pointee_iterator<WrappedIteratorT>;

#endif// HAS_DEDUCTION_GUIDES

template <typename I>
ASTL_NODISCARD constexpr auto make_pointee_iterator(I i) -> pointee_iterator<I>
{
    return pointee_iterator<I>{i};
}

template <typename I, typename S, typename WrappedIteratorBeginT = I,
          typename WrappedIteratorEndT = S>
ASTL_NODISCARD constexpr auto make_pointee_range(I first, S last)
    -> iterator_range<pointee_iterator<WrappedIteratorBeginT>,
                      pointee_iterator<WrappedIteratorEndT>>
{
    return astl::make_range(pointee_iterator<WrappedIteratorBeginT>(first),
                            pointee_iterator<WrappedIteratorEndT>(last));
}

template <typename RangeT>
ASTL_NODISCARD constexpr auto make_pointee_range(RangeT &&r)
    -> iterator_range<pointee_iterator<begin_t<RangeT &&>>, pointee_iterator<end_t<RangeT &&>>>
{
    return astl::make_pointee_range(adl::begin(r), adl::end(r));
}
}// namespace astl
#endif// ASTL_INCLUDE_POINTEE_ITERATOR_HPP
