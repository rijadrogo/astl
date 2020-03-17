//
// Created by Rijad on 13-Aug-18.
//

#ifndef ASTL_INCLUDE_MAP_ITERATOR_HPP
#define ASTL_INCLUDE_MAP_ITERATOR_HPP

#include <type_traits>// for decay, declval

#include "ebo_base.hpp"
#include "functional.hpp"
#include "iterator.hpp"
#include "iterator_range.hpp"
#include "range_access.hpp"

namespace astl
{
namespace internal_mi
{
template <int> struct mi_tag {};
}// namespace internal_mi

// mapped_iterator - This is a simple iterator adapter that causes a function to
// be applied whenever operator* is invoked on the iterator.
template <typename ItTy, typename FuncTy,
          typename FuncReturnTy = decltype(std::declval<FuncTy>()(*std::declval<ItTy>()))>
struct mapped_iterator
    : iterator_adaptor_base<mapped_iterator<ItTy, FuncTy>, ItTy,
                            typename std::iterator_traits<ItTy>::iterator_category,
                            typename std::remove_reference<FuncReturnTy>::type>,
      private ebo<internal_mi::mi_tag<0>, FuncTy> {
private:
    using BaseT = iterator_adaptor_base<mapped_iterator<ItTy, FuncTy>, ItTy,
                                        typename std::iterator_traits<ItTy>::iterator_category,
                                        typename std::remove_reference<FuncReturnTy>::type>;

    using BaseFun = ebo<internal_mi::mi_tag<0>, FuncTy>;

public:
    constexpr mapped_iterator(ItTy u, FuncTy f) : BaseT(std::move(u)), BaseFun(std::move(f)) {}

    ASTL_NODISCARD constexpr auto operator*() const -> FuncReturnTy
    {
        return invoke(ebo_get(*this), BaseT::base());
    }
};

// map_iterator - Provide a convenient way to create mapped_iterators, just like
// make_pair is useful for creating pairs...
template <typename ItTy, typename FuncTy>
ASTL_NODISCARD constexpr auto map_iterator(ItTy i, FuncTy f) -> mapped_iterator<ItTy, FuncTy>
{
    return mapped_iterator<ItTy, FuncTy>(std::move(i), std::move(f));
}

template <typename I, typename S, typename F>
ASTL_NODISCARD constexpr auto map_range(I first, S last, F &&fn)
    -> iterator_range<mapped_iterator<I, typename std::decay<F>::type>,
                      mapped_iterator<S, typename std::decay<F>::type>>
{
    return astl::make_range(astl::map_iterator(std::move(first), fn),
                            astl::map_iterator(std::move(last), fn));
}

template <typename R, typename F>
ASTL_NODISCARD constexpr auto map_range(R &&r, F &&fn)
    -> iterator_range<mapped_iterator<begin_t<R>, typename std::decay<F>::type>,
                      mapped_iterator<end_t<R>, typename std::decay<F>::type>>
{
    return astl::make_range(astl::map_iterator(adl::begin(r), fn),
                            astl::map_iterator(adl::end(r), fn));
}
}// namespace astl
#endif// ASTL_INCLUDE_MAP_ITERATOR_HPP
