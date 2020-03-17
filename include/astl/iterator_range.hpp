//
// Created by Rijad on 28-Aug-18.
//

//===------------------------------------------------------------------------------------------===//
/// \file
/// This provides a very simple, boring adaptor for a begin and end iterator
/// into a range type. This should be used to build range views that work well
/// with range based for loops and range based constructors.
///
/// Note that code here follows more standards-based coding conventions as it is
/// mirroring proposed interfaces for standardization.
///
//===------------------------------------------------------------------------------------------===//

#ifndef ASTL_INCLUDE_ITERATOR_RANGE_HPP
#define ASTL_INCLUDE_ITERATOR_RANGE_HPP

#include <type_traits>
#include <utility>

#include "functional.hpp"
#include "iterator.hpp"
#include "range_access.hpp"

namespace astl
{
namespace internal_constexpr_alg
{
template <typename I1, typename I2>
// requires I1 ForwardIterator
// requires I2 ForwardIterator
constexpr auto equal(I1 first1, I1 last1, I2 first2, I2 last2) -> bool
{
    if constexpr (is_random_access_it_v<I1, I2>) { // Random Access Iterators
        if (last1 - first1 != last2 - first2) return false;

        while (first1 != last1) {
            if (!(*first1 == *first2)) return false;

            ++first1;
            ++first2;
        }
        return true;
    }
    else { // Forward Iterators
        while (first1 != last1 && first2 != last2) {
            if (!(*first1 == *first2)) return false;

            ++first1;
            ++first2;
        }
        return first1 != last1 && first2 != last2;
    }
}

template <typename I1, typename I2>
// requires I1 ForwardIterator
// requires I2 ForwardIterator
constexpr auto lexicographical_compare(I1 first1, I1 last1, I2 first2, I2 last2) -> bool
{
    while (first1 != last1 && first2 != last2) {
        if (*first1 < *first2) return true;

        if (*first2 < *first1) return false;

        ++first1;
        ++first2;
    }
    return first1 == last1 && first2 != last2;
}
} // namespace internal_constexpr_alg

/// \brief A range adaptor for a pair of iterators.
///
/// This just wraps two iterators into a range-compatible interface.
template <typename IteratorT, typename Sentinel = IteratorT>
// requires IteratorT ForwardIterator
struct iterator_range {
private:
    IteratorT begin_iterator;
    Sentinel end_iterator;
    using difference_type = typename std::iterator_traits<IteratorT>::difference_type;

public:
    using iterator = IteratorT;

    iterator_range() = default;

    template <typename Container,
              typename std::enable_if<
                  std::is_constructible<IteratorT, astl::begin_t<Container>>::value, int>::type = 0>
    // requires Container ForwardIterator range
    // NOLINTNEXTLINE (bugprone-forwarding-reference-overload)
    constexpr explicit iterator_range(Container &&c)
        : begin_iterator(adl::begin(c)), end_iterator(adl::end(c))
    {}

    constexpr iterator_range(IteratorT first, Sentinel last)
        : begin_iterator(std::move(first)), end_iterator(std::move(last))
    {}

    constexpr iterator_range(IteratorT first, difference_type const &n)
        : begin_iterator(std::move(first)), end_iterator(astl::next(first, n))
    {}

    template <typename I, typename S>
    // requires I ForwardIterator
    // requires S ForwardIterator
    constexpr explicit iterator_range(std::pair<I, S> p)
        : begin_iterator(std::move(p).first), end_iterator(std::move(p).second)
    {}

    ASTL_NODISCARD constexpr auto begin() const -> IteratorT { return begin_iterator; }

    ASTL_NODISCARD constexpr auto begin() noexcept -> IteratorT & { return begin_iterator; }

    ASTL_NODISCARD constexpr auto end() const -> Sentinel { return end_iterator; }

    ASTL_NODISCARD constexpr auto end() noexcept -> Sentinel & { return end_iterator; }

    ASTL_NODISCARD constexpr auto empty() const noexcept -> bool
    {
        return begin_iterator == end_iterator;
    }

    // size is define only for random access iterators
    template <bool IsRandAccIter = is_randit<IteratorT>::value>
    ASTL_NODISCARD constexpr auto size() const noexcept ->
        typename std::enable_if<IsRandAccIter, difference_type>::type
    {
        return end_iterator - begin_iterator;
    }

    // NOLINTNEXTLINE(google-explicit-constructor)
    ASTL_NODISCARD constexpr operator std::pair<IteratorT, Sentinel>() const &
    {
        return std::make_pair(begin_iterator, end_iterator);
    }

    // NOLINTNEXTLINE(google-explicit-constructor)
    ASTL_NODISCARD constexpr operator std::pair<IteratorT, Sentinel>() &
    {
        return std::make_pair(begin_iterator, end_iterator);
    }

    // NOLINTNEXTLINE(google-explicit-constructor)
    ASTL_NODISCARD constexpr operator std::pair<IteratorT, Sentinel>() && //-V659
    {
        return std::make_pair(std::move(begin_iterator), std::move(end_iterator));
    }

    template <typename I1, typename S1>
    // requires I1 ForwardIterator, value_type(I1) equality comparable with
    // value_type(I1) requires S1 ForwardIterator
    ASTL_NODISCARD constexpr auto operator==(iterator_range<I1, S1> const &r) const noexcept -> bool
    {
        return internal_constexpr_alg::equal(this->begin(), this->end(), r.begin(), r.end());
    }

    template <
        typename T,
        decltype(*(std::declval<IteratorT>) == *(adl::begin(std::declval<T const &>()))) = true>
    // requires T ForwardIterator range, value_type(T) equality comparable with
    // value_type(IteratorT)
    ASTL_NODISCARD constexpr auto operator==(T const &r) const noexcept -> bool
    {
        return internal_constexpr_alg::equal(this->begin(), this->end(), adl::begin(r),
                                             adl::end(r));
    }

    template <typename I1, typename S1>
    // requires I1 ForwardIterator, value_type(I1) equality comparable with
    // value_type(IteratorT) requires S1 ForwardIterator
    ASTL_NODISCARD constexpr auto operator!=(iterator_range<I1, S1> const &r) const noexcept -> bool
    {
        return !(this->operator==(r));
    }

    template <typename T,
              decltype(*(std::declval<IteratorT>())
                       != *(adl::begin(std::declval<T const &>()))) = true>
    // requires T ForwardIterator range, value_type(T) equality comparable with
    // value_type(IteratorT)
    ASTL_NODISCARD constexpr auto operator!=(T const &r) const noexcept -> bool
    {
        return !(*this == r);
    }

    template <typename I1, typename S1>
    // requires I1 ForwardIterator, value_type(I1) less comparable with
    // value_type(I1) requires S1 ForwardIterator
    ASTL_NODISCARD constexpr auto operator<(iterator_range<I1, S1> const &r) const noexcept -> bool
    {
        return internal_constexpr_alg::lexicographical_compare(this->begin(), this->end(),
                                                               r.begin(), r.end());
    }

    template <typename T,
              decltype(*(std::declval<IteratorT>())
                       < *(adl::begin(std::declval<T const &>()))) = true>
    // requires T ForwardIterator range, value_type(T) less comparable with
    // value_type(IteratorT)
    ASTL_NODISCARD constexpr auto operator<(T const &r) const noexcept -> bool
    {
        return internal_constexpr_alg::lexicographical_compare(this->begin(), this->end(),
                                                               adl::begin(r), adl::end(r));
    }

    template <typename I1, typename S1>
    // requires I1 ForwardIterator, value_type(I1) less comparable with
    // value_type(I1) requires S1 ForwardIterator
    ASTL_NODISCARD constexpr auto operator<=(iterator_range<I1, S1> const &r) const noexcept -> bool
    {
        return !internal_constexpr_alg::lexicographical_compare(adl::begin(r), adl::end(r),
                                                                this->begin(), this->end());
    }

    template <typename T,
              decltype(*(std::declval<IteratorT>())
                       <= *(adl::begin(std::declval<T const &>()))) = true>
    // requires T ForwardIterator range, value_type(T) less comparable with
    // value_type(I1)
    ASTL_NODISCARD constexpr auto operator<=(T const &r) const noexcept -> bool
    {
        return !internal_constexpr_alg::lexicographical_compare(adl::begin(r), adl::end(r),
                                                                this->begin(), this->end());
    }

    template <typename I1, typename S1>
    // requires I1 ForwardIterator, value_type(I1) less comparable with
    // value_type(I1) requires S1 ForwardIterator
    ASTL_NODISCARD constexpr auto operator>(iterator_range<I1, S1> const &r) const noexcept -> bool
    {
        return internal_constexpr_alg::lexicographical_compare(adl::begin(r), adl::end(r),
                                                               this->begin(), this->end());
    }

    template <typename T,
              decltype(*(std::declval<IteratorT>())
                       > *(adl::begin(std::declval<T const &>()))) = true>
    // requires T ForwardIterator range, value_type(T) less comparable with
    // value_type(I1)
    ASTL_NODISCARD constexpr auto operator>(T const &r) const noexcept -> bool
    {
        return internal_constexpr_alg::lexicographical_compare(adl::begin(r), adl::end(r),
                                                               this->begin(), this->end());
    }

    template <typename I1, typename S1>
    // requires I1 ForwardIterator, value_type(I1) less comparable with
    // value_type(I1) requires S1 ForwardIterator
    ASTL_NODISCARD constexpr auto operator>=(iterator_range<I1, S1> const &r) const noexcept -> bool
    {
        return !internal_constexpr_alg::lexicographical_compare(this->begin(), this->end(),
                                                                adl::begin(r), adl::end(r));
    }

    template <typename T,
              decltype(*(std::declval<IteratorT>())
                       >= *(adl::begin(std::declval<T const &>()))) = true>
    // requires T ForwardIterator range, value_type(T) less comparable with
    // value_type(I1)
    ASTL_NODISCARD constexpr auto operator>=(T const &r) const noexcept -> bool
    {
        return !internal_constexpr_alg::lexicographical_compare(this->begin(), this->end(),
                                                                adl::begin(r), adl::end(r));
    }
};

template <typename I, typename S, typename T,
          decltype(*(std::declval<iterator_range<I, S> const &>().begin())
                   == *(adl::begin(std::declval<T const &>()))) = true>
ASTL_NODISCARD constexpr auto operator==(T const &r, iterator_range<I, S> const &ir) noexcept
    -> bool
{
    return ir.operator==(r);
}

template <typename I, typename S, typename T,
          decltype(*(std::declval<iterator_range<I, S> const &>().begin())
                   != *(adl::begin(std::declval<T const &>()))) = true>
ASTL_NODISCARD constexpr auto operator!=(T const &r, iterator_range<I, S> const &ir) noexcept
    -> bool
{
    return ir.operator!=(r);
}

template <typename I, typename S, typename T,
          decltype(*(std::declval<iterator_range<I, S> const &>().begin())
                   < *(adl::begin(std::declval<T const &>()))) = true>
ASTL_NODISCARD constexpr auto operator<(T const &r, iterator_range<I, S> const &ir) noexcept -> bool
{
    return ir.operator>=(r);
}

template <typename I, typename S, typename T,
          decltype(*(std::declval<iterator_range<I, S> const &>().begin())
                   <= *(adl::begin(std::declval<T const &>()))) = true>
ASTL_NODISCARD constexpr auto operator<=(T const &r, iterator_range<I, S> const &ir) noexcept
    -> bool
{
    return ir.operator>(r);
}

template <typename I, typename S, typename T,
          decltype(*(std::declval<iterator_range<I, S> const &>().begin())
                   > *(adl::begin(std::declval<T const &>()))) = true>
ASTL_NODISCARD constexpr auto operator>(T const &r, iterator_range<I, S> const &ir) noexcept -> bool
{
    return ir.operator<=(r);
}

template <typename I, typename S, typename T,
          decltype(*(std::declval<iterator_range<I, S> const &>().begin())
                   >= *(adl::begin(std::declval<T const &>()))) = true>
ASTL_NODISCARD constexpr auto operator>=(T const &r, iterator_range<I, S> const &ir) noexcept
    -> bool
{
    return ir.operator<(r);
}

#if HAS_DEDUCTION_GUIDES
template <typename I, typename S> iterator_range(std::pair<I, S>) -> iterator_range<I, S>;

template <typename R> iterator_range(R) -> iterator_range<astl::begin_t<R>, astl::end_t<R>>;
#endif

/// \brief Convenience function for iterating over sub-ranges.
///
/// This provides a bit of syntactic sugar to make using sub-ranges in for loops
/// a bit easier. Analogous to std::make_pair().
template <typename FwdIt, typename Sent>
// requires FwdIt ForwardIterator
ASTL_NODISCARD constexpr auto make_range(FwdIt x, Sent y) -> iterator_range<FwdIt, Sent>
{
    return iterator_range<FwdIt, Sent>(std::move(x), std::move(y));
}

template <typename FwdIt, typename Sent>
// requires FwdIt ForwardIterator
ASTL_NODISCARD constexpr auto make_range(std::pair<FwdIt, Sent> p) -> iterator_range<FwdIt, Sent>
{
    return iterator_range<FwdIt, Sent>(std::move(p).first, std::move(p).second);
}

template <typename FwdIt>
// requires FwdIt ForwardIterator
ASTL_NODISCARD constexpr auto make_range(FwdIt first, iter_diff_type<FwdIt> n)
    -> iterator_range<FwdIt, FwdIt>
{
    return iterator_range<FwdIt, FwdIt>(first, astl::next(first, n));
}

template <typename R>
// requires R ForwardIterator range
ASTL_NODISCARD constexpr auto drop_begin(R &&r, range_diff_type<R> n)
    -> iterator_range<astl::begin_t<R>, astl::end_t<R>>
{
    return iterator_range<astl::begin_t<R>, astl::end_t<R>>(astl::next(adl::begin(r), n),
                                                            adl::end(r));
}

template <typename I, typename S>
// requires I BidirectionalIterator
// requires S BidirectionalIterator
ASTL_NODISCARD auto reversed(I first, S last)
{
    return astl::make_range(std::make_reverse_iterator(last), std::make_reverse_iterator(first));
}

namespace internal_reve
{
template <typename R, typename = void> struct has_reverse_iterators: std::false_type {};

template <typename R1>
struct has_reverse_iterators<
    R1, void_t<decltype(astl::rbegin(std::declval<R1>()), astl::rbegin(std::declval<R1>()))>>
    : std::true_type {};

} // namespace internal_reve

template <typename R>
// requires R BidirectionalIterator range
ASTL_NODISCARD auto reversed(R &&r)
{
    if constexpr (internal_reve::has_reverse_iterators<R>::value)
        return astl::make_range(astl::rbegin(r), astl::rend(r));
    else
        return astl::reversed(adl::begin(r), adl::end(r));
}
} // namespace astl

#endif // ASTL_INCLUDE_ITERATOR_RANGE_HPP
