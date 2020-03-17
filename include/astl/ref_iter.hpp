//
// Created by Rijad on 13-Aug-18.
//

#ifndef ASTL_INCLUDE_REF_ITER_HPP
#define ASTL_INCLUDE_REF_ITER_HPP

#include <type_traits>

#include "functional.hpp"
#include "iterator.hpp"

namespace astl
{
namespace internal_ri_adl_b
{
template <typename I>
struct ref_to_iter
    : iterator_facade_base<
          ref_to_iter<I>, iter_cat<I>, I &, iter_diff_type<I>,
          if_t<
              std::is_same<iter_value_type<I>, typename std::iterator_traits<I>::value_type>::value,
              typename std::iterator_traits<I>::pointer, iter_value_type<I> *>,
          if_t<
              std::is_same<iter_value_type<I>, typename std::iterator_traits<I>::value_type>::value,
              typename std::iterator_traits<I>::reference, iter_value_type<I> &>> {
private:
    I *p_iter;

public:
    explicit constexpr ref_to_iter(I &i) noexcept : p_iter(std::addressof(i)) {}

    ASTL_NODISCARD constexpr auto base() const noexcept -> I const & { return *p_iter; }

    ASTL_NODISCARD constexpr auto operator==(I const &i) const noexcept -> bool
    {
        return *p_iter == i;
    }
    ASTL_NODISCARD constexpr auto operator<(I const &i) const noexcept -> bool
    {
        return *p_iter < i;
    }

    template <typename It>
    ASTL_NODISCARD constexpr auto operator==(It const &i) const noexcept -> decltype(*p_iter == i)
    {
        return *p_iter == i;
    }

    template <typename It>
    ASTL_NODISCARD constexpr auto operator<(It const &i) const noexcept -> decltype(*p_iter < i)
    {
        return *p_iter < i;
    }

    constexpr auto operator++() -> ref_to_iter
    {
        ++(*p_iter);
        return *this;
    }

    constexpr auto operator--() -> ref_to_iter
    {
        --(*p_iter);
        return *this;
    }

    ASTL_NODISCARD constexpr auto operator*() const noexcept -> iter_value_type<I> const &
    {
        return **p_iter;
    }
    ASTL_NODISCARD constexpr auto operator*() noexcept -> iter_value_type<I> & { return **p_iter; }

    constexpr auto operator+=(iter_diff_type<I> n) -> ref_to_iter
    {
        *p_iter += n;
        return *this;
    }

    constexpr auto operator-=(iter_diff_type<I> n) -> ref_to_iter
    {
        *p_iter -= n;
        return *this;
    }

    ASTL_NODISCARD constexpr auto operator-(ref_to_iter<I> i) noexcept -> iter_diff_type<I>
    {
        return *p_iter - *(i.p_iter);
    }
};

template <typename I1, typename I2>
ASTL_NODISCARD constexpr auto operator==(I1 const &i, ref_to_iter<I2> ri) noexcept
    -> decltype(ri.operator==(i))
{
    return ri.operator==(i);
}

template <typename I1, typename I2>
ASTL_NODISCARD constexpr auto operator<(I1 const &i, ref_to_iter<I2> ri) noexcept
    -> decltype(i < ri.base())
{
    return i < ri.base();
}

template <typename I>
inline constexpr bool pass_by_value_treshold = sizeof(I) < 2 * sizeof(void *)
    && std::is_trivially_copy_constructible<I>::value &&std::is_trivially_destructible<I>::value;
} // namespace internal_ri_adl_b

using internal_ri_adl_b::ref_to_iter;

// if iterator is not cheap to copy use this to pass it by "reference",
// reference_wrapper adapted for iterators
template <typename I> ASTL_NODISCARD constexpr auto make_ref_iter(I &i) noexcept -> ref_to_iter<I>
{
    return internal_ri_adl_b::ref_to_iter<I>{i};
}

template <typename I>
ASTL_NODISCARD auto pass_iter(I i) noexcept ->
    typename std::enable_if<internal_ri_adl_b::pass_by_value_treshold<I>, I>::type
{
    return i;
}

template <typename I>
ASTL_NODISCARD auto pass_iter(I &i) noexcept ->
    typename std::enable_if<!astl::internal_ri_adl_b::pass_by_value_treshold<I>,
                            ref_to_iter<I>>::type
{
    return ref_to_iter<I>{i};
}

template <typename I> ASTL_NODISCARD auto pass_iter(ref_to_iter<I> i) noexcept -> ref_to_iter<I>
{
    return i;
}

} // namespace astl

#endif // ASTL_INCLUDE_REF_ITER_HPP
