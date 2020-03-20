//
// Created by Rijad on 05-Aug-18.
//

#ifndef ASTL_INCLUDE_TRIM_HPP
#define ASTL_INCLUDE_TRIM_HPP

#include <algorithm>
#include <initializer_list>
#include <iterator>

#include "any_of.hpp"

#include "astl/functional.hpp"

namespace astl
{
namespace i
{

inline constexpr struct {
    template <typename FwdIt1, typename FwdIt2>
    // requires FwdIt ForwardIterator
    // requires FwdIt ForwardIterator
    ASTL_NODISCARD auto operator()(FwdIt1 first, FwdIt1 const last, FwdIt2 first_elem_to_trim,
                                   FwdIt2 last_elem_to_trim) const -> FwdIt1
    {
        while (first != last && i::any_of_equal(first_elem_to_trim, last_elem_to_trim, *first))
            ++first;

        return first;
    }

    template <typename FwdIt1, typename FwdIt2, typename P>
    ASTL_NODISCARD auto operator()(FwdIt1 first, FwdIt1 const last, FwdIt2 first_elem_to_trim,
                                   FwdIt2 last_elem_to_trim, P p) const -> FwdIt1
    {
        auto proj(astl::pass_fn(p));
        auto lam([&first, proj](auto &&el) { return el == invoke(proj, *first); });
        while (first != last && i::any_of(first_elem_to_trim, last_elem_to_trim, lam)) ++first;

        return first;
    }

    template <typename FwdIt, typename T>
    // requires FwdIt ForwardIterator
    // requires T equality comparable with value_type(FwdIt)
    ASTL_NODISCARD auto trim_left(FwdIt first, FwdIt last,
                                  std::initializer_list<T> trim_elements) const -> FwdIt
    {
        return (*this)(first, last, trim_elements.begin(), trim_elements.end());
    }

    template <typename FwdIt, typename T, typename P>
    ASTL_NODISCARD auto trim_left(FwdIt first, FwdIt last, std::initializer_list<T> trim_elements,
                                  P p) const -> FwdIt
    {
        return (*this)(first, last, trim_elements.begin(), trim_elements.end(), astl::pass_fn(p));
    }
} trim_left{};

inline constexpr struct {
    template <typename FwdIt, typename UnaryPredicate>
    ASTL_NODISCARD auto operator()(FwdIt first, FwdIt const last, UnaryPredicate pred) const
        -> FwdIt
    {
        while (first != last && pred(*first)) ++first;

        return first;
    }

    template <typename FwdIt, typename UnaryPredicate, typename P>
    ASTL_NODISCARD auto operator()(FwdIt first, FwdIt last, UnaryPredicate pred, P p) const -> FwdIt
    {
        return (*this)(first, last, astl::combine(astl::pass_fn(pred), astl::pass_fn(p)));
    }
} trim_left_if{};

inline constexpr struct {
    template <typename BidiIt, typename FwdIt>
    // requires BidiIt BidirectionalIterator
    // requires FwdIt ForwardIterator
    ASTL_NODISCARD auto operator()(BidiIt first, BidiIt last, FwdIt first_elem_to_trim,
                                   FwdIt last_elem_to_trim) const -> BidiIt
    {
        return i::trim_left(std::make_reverse_iterator(last), std::make_reverse_iterator(first),
                            first_elem_to_trim, last_elem_to_trim)
            .base();
    }

    template <typename BidiIt, typename FwdIt, typename P>
    ASTL_NODISCARD auto operator()(BidiIt first, BidiIt last, FwdIt first_elem_to_trim,
                                   FwdIt last_elem_to_trim, P p) const -> BidiIt
    {
        return i::trim_left(std::make_reverse_iterator(last), std::make_reverse_iterator(first),
                            first_elem_to_trim, last_elem_to_trim, astl::pass_fn(p))
            .base();
    }

    template <typename BidiIt, typename T>
    // requires BidiIt BidirectionalIterator
    // requires T equality comparable with value_type(BidiIt)
    ASTL_NODISCARD auto operator()(BidiIt first, BidiIt last,
                                   std::initializer_list<T> trim_elements) const -> BidiIt
    {
        return (*this)(first, last, trim_elements.begin(), trim_elements.end());
    }

    template <typename BidiIt, typename T, typename P>
    ASTL_NODISCARD auto operator()(BidiIt first, BidiIt last,
                                   std::initializer_list<T> trim_elements, P p) const -> BidiIt
    {
        return (*this)(first, last, trim_elements.begin(), trim_elements.end(), astl::pass_fn(p));
    }
} trim_right{};

inline constexpr struct {
    template <typename BidiIt, typename UnaryPredicate>
    ASTL_NODISCARD auto operator()(BidiIt first, BidiIt last, UnaryPredicate pred) const -> BidiIt
    {
        return i::trim_left_if(std::make_reverse_iterator(last), std::make_reverse_iterator(first),
                               astl::pass_fn(pred))
            .base();
    }

    template <typename BidiIt, typename UnaryPredicate, typename P>
    ASTL_NODISCARD auto operator()(BidiIt first, BidiIt last, UnaryPredicate pred, P p) const
        -> BidiIt
    {
        return (*this)(first, last, astl::combine(astl::pass_fn(pred), astl::pass_fn(p)));
    }
} trim_right_if{};

inline constexpr struct {
    template <typename BidiIt, typename FwdIt>
    // requires BidiIt BidirectionalIterator
    // requires FwdIt ForwardIterator
    ASTL_NODISCARD auto operator()(BidiIt first, BidiIt last, FwdIt first_elem_to_trim,
                                   FwdIt last_elem_to_trim) const -> iterator_range<BidiIt>
    {
        auto first1(i::trim_left(first, last, first_elem_to_trim, last_elem_to_trim));
        return astl::make_range(first1,
                                i::trim_right(first1, last, first_elem_to_trim, last_elem_to_trim));
    }

    template <typename BidiIt, typename FwdIt, typename P>
    ASTL_NODISCARD auto operator()(BidiIt first, BidiIt last, FwdIt first_elem_to_trim,
                                   FwdIt last_elem_to_trim, P p) const -> iterator_range<BidiIt>
    {
        auto proj(astl::pass_fn(p));
        auto first1(i::trim_left(first, last, first_elem_to_trim, last_elem_to_trim));
        return astl::make_range(
            first1, i::trim_right(first, last, first_elem_to_trim, last_elem_to_trim, proj));
    }

    template <typename BidiIt, typename T>
    // requires BidiIt BidirectionalIterator
    // requires T equality comparable with value_type(BidiIt)
    ASTL_NODISCARD auto operator()(BidiIt first, BidiIt last,
                                   std::initializer_list<T> trim_elements) const
        -> iterator_range<BidiIt>
    {
        return (*this)(first, last, trim_elements.begin(), trim_elements.end());
    }

    template <typename BidiIt, typename T, typename P>
    ASTL_NODISCARD auto operator()(BidiIt first, BidiIt last,
                                   std::initializer_list<T> trim_elements, P p) const
        -> iterator_range<BidiIt>
    {
        return (*this)(first, last, trim_elements.begin(), trim_elements.end(), astl::pass_fn(p));
    }
} trim{};

inline constexpr struct {
    template <typename BidiIt, typename UnaryPredicate, typename P>
    ASTL_NODISCARD auto operator()(BidiIt first, BidiIt last, UnaryPredicate pred) const
        -> iterator_range<BidiIt>
    {
        auto pred1(astl::pass_fn(pred));
        auto first1(i::trim_left_if(first, last, pred1));
        return astl::make_range(first1, i::trim_right_if(first1, last, pred1));
    }

    template <typename BidiIt, typename UnaryPredicate, typename P>
    ASTL_NODISCARD auto operator()(BidiIt first, BidiIt last, UnaryPredicate pred, P p) const
        -> iterator_range<BidiIt>
    {
        auto proj(astl::pass_fn(p));
        auto pred1(astl::pass_fn(pred));
        BidiIt first1(i::trim_left_if(first, last, pred1, proj));
        return astl::make_range(first1, i::trim_right_if(first1, last, pred1, proj));
    }
} trim_if{};

} // namespace i

namespace r
{

inline constexpr struct {
    template <typename R, typename TrimElements>
    // requires R ForwardIterator range
    // requires TrimElements ForwardIterator range
    ASTL_NODISCARD auto operator()(R &&r, TrimElements &&elems) const
        -> iterator_range<begin_t<R>, end_t<R>>
    {
        return astl::make_range(
            i::trim_left(adl::begin(r), adl::end(r), adl::begin(elems), adl::end(elems)),
            adl::end(r));
    }

    template <typename R, typename TrimElements, typename P>
    // requires R ForwardIterator range
    // requires TrimElements ForwardIterator range
    ASTL_NODISCARD auto operator()(R &&r, TrimElements &&elems, P p) const
        -> iterator_range<begin_t<R>, end_t<R>>
    {
        return astl::make_range(i::trim_left(adl::begin(r), adl::end(r), adl::begin(elems),
                                             adl::end(elems), astl::pass_fn(p)),
                                adl::end(r));
    }
} trim_left{};

inline constexpr struct {
    template <typename R, typename UnaryPredicate>
    // requires R ForwardIterator range
    ASTL_NODISCARD auto operator()(R &&r, UnaryPredicate pred) const
        -> iterator_range<begin_t<R>, end_t<R>>
    {
        return astl::make_range(i::trim_left_if(adl::begin(r), adl::end(r), astl::pass_fn(pred)),
                                adl::end(r));
    }

    template <typename R, typename UnaryPredicate, typename P>
    // requires R ForwardIterator range
    ASTL_NODISCARD auto operator()(R &&r, UnaryPredicate pred, P p) const
        -> iterator_range<begin_t<R>, end_t<R>>
    {
        return astl::make_range(
            i::trim_left(adl::begin(r), adl::end(r), astl::pass_fn(pred), astl::pass_fn(p)),
            adl::end(r));
    }
} trim_left_if{};

inline constexpr struct {
    template <typename R, typename TrimElements>
    // requires R BidirectionalIterator range
    // requires TrimElements ForwardIterator range
    ASTL_NODISCARD auto operator()(R &&r, TrimElements &&elems) const
        -> iterator_range<begin_t<R>, end_t<R>>
    {
        return astl::make_range(
            adl::begin(r),
            i::trim_right(adl::begin(r), adl::end(r), adl::begin(elems), adl::end(elems)));
    }

    template <typename R, typename TrimElements, typename P>
    // requires R BidirectionalIterator range
    // requires TrimElements ForwardIterator range
    ASTL_NODISCARD auto operator()(R &&r, TrimElements &&elems, P p) const
        -> iterator_range<begin_t<R>, end_t<R>>
    {
        return astl::make_range(adl::begin(r),
                                i::trim_right(adl::begin(r), adl::end(r), adl::begin(elems),
                                              adl::end(elems), astl::pass_fn(p)));
    }
} trim_right{};

inline constexpr struct {
    template <typename R, typename UnaryPredicate>
    // requires R BidirectionalIterator range
    ASTL_NODISCARD auto operator()(R &&r, UnaryPredicate pred) const
        -> iterator_range<begin_t<R>, end_t<R>>
    {
        return astl::make_range(adl::begin(r),
                                i::trim_right_if(adl::begin(r), adl::end(r), astl::pass_fn(pred)));
    }

    template <typename R, typename UnaryPredicate, typename P>
    // requires R BidirectionalIterator range
    ASTL_NODISCARD auto operator()(R &&r, UnaryPredicate pred, P p) const
        -> iterator_range<begin_t<R>, end_t<R>>
    {
        return astl::make_range(
            adl::begin(r),
            i::trim_right_if(adl::begin(r), adl::end(r), astl::pass_fn(pred), astl::pass_fn(p)));
    }
} trim_right_if{};

inline constexpr struct {
    template <typename R, typename TrimElements>
    // requires R BidirectionalIterator range
    // requires TrimElements ForwardIterator range
    ASTL_NODISCARD auto operator()(R &&r, TrimElements &&elems) const
        -> iterator_range<begin_t<R>, end_t<R>>
    {
        return i::trim(adl::begin(r), adl::end(r), adl::begin(elems), adl::end(elems));
    }

    template <typename R, typename TrimElements, typename P>
    // requires R BidirectionalIterator range
    // requires TrimElements ForwardIterator range
    ASTL_NODISCARD auto operator()(R &&r, TrimElements &&elems, P p) const
        -> iterator_range<begin_t<R>, end_t<R>>
    {
        return i::trim(adl::begin(r), adl::end(r), adl::begin(elems), adl::end(elems),
                       astl::pass_fn(p));
    }
} trim{};

inline constexpr struct {
    template <typename R, typename UnaryPredicate>
    // requires R BidirectionalIterator range
    ASTL_NODISCARD auto operator()(R &&r, UnaryPredicate pred) const
        -> iterator_range<begin_t<R>, end_t<R>>
    {
        return i::trim_if(adl::begin(r), adl::end(r), astl::pass_fn(pred));
    }

    template <typename R, typename UnaryPredicate, typename P>
    // requires R BidirectionalIterator range
    ASTL_NODISCARD auto operator()(R &&r, UnaryPredicate pred, P p) const
        -> iterator_range<begin_t<R>, end_t<R>>
    {
        return i::trim_if(adl::begin(r), adl::end(r), astl::pass_fn(pred), astl::pass_fn(p));
    }
} trim_if{};

} // namespace r
} // namespace astl

#endif // ASTL_INCLUDE_TRIM_HPP
