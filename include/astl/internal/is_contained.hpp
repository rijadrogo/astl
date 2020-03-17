//
// Created by Rijad on 04-Aug-18.
//

#ifndef ASTL_INCLUDE_IS_CONTAINED_HPP
#define ASTL_INCLUDE_IS_CONTAINED_HPP

#include <algorithm>

#include "find.hpp"

#include "astl/functional.hpp"
#include "astl/range_access.hpp"

namespace astl
{
namespace i
{
inline constexpr struct {
    template <typename FwdIt, typename E>
    // requires FwdIt ForwardIterator
    // requires E to be comparable with value_type(FwdIt)
    ASTL_NODISCARD auto operator()(FwdIt first, FwdIt last, E const &element) const -> bool
    {
        return std::find(first, last, element) != last;
    }

    template <typename FwdIt, typename E, typename P>
    ASTL_NODISCARD auto operator()(FwdIt first, FwdIt last, E const &element, P p) const -> bool
    {
        return i::find(first, last, element, astl::pass_fn(p)) != last;
    }

} is_contained{};

inline constexpr struct {
    template <typename FwdIt, typename UnaryPredicate>
    // requires FwdIt ForwardIterator
    // requires UnaryPredicate function returns bool, argument type value_type(FwdIt)
    ASTL_NODISCARD auto operator()(FwdIt first, FwdIt last, UnaryPredicate pred) const -> bool
    {
        return std::find_if(first, last, astl::pass_fn(pred)) != last;
    }

    template <typename FwdIt, typename UnaryPredicate, typename P>
    ASTL_NODISCARD auto operator()(FwdIt first, FwdIt last, UnaryPredicate pred, P p) const -> bool
    {
        return i::find_if(first, last, astl::pass_fn(pred), astl::pass_fn(p)) != last;
    }

} is_contained_if{};

inline constexpr struct {
    template <typename FwdIt, typename N, typename UnaryPredicate>
    // requires FwdIt ForwardIterator
    // requires N integral type
    // requires UnaryPredicate function returns bool, argument type value_type(FwdIt)
    ASTL_NODISCARD auto operator()(FwdIt first, N n, UnaryPredicate pred) const -> bool
    {
        return i::find_if_n(first, n, astl::pass_fn(pred)).second != n;
    }

    template <typename FwdIt, typename N, typename UnaryPredicate, typename P>
    ASTL_NODISCARD auto operator()(FwdIt first, N n, UnaryPredicate pred, P p) const -> bool
    {
        return i::find_if_n(first, n, astl::pass_fn(pred), astl::pass_fn(p)).second != n;
    }
} is_contained_if_n{};

inline constexpr struct {

    template <typename FwdIt, typename N, typename E>
    // requires FwdIt ForwardIterator
    // requires N integral type
    // requires E to be comparable with value_type(FwdIt)
    ASTL_NODISCARD auto operator()(FwdIt first, N n, E const &element) const -> bool
    {
        return i::find_n(first, n, element).second != n;
    }

    template <typename FwdIt, typename N, typename E, typename P>
    ASTL_NODISCARD auto operator()(FwdIt first, N n, E const &element, P p) const -> bool
    {
        return i::find_n(first, n, element, astl::pass_fn(p)).second != n;
    }
} is_contained_n{};

} // namespace i

namespace r
{

inline constexpr struct {
    template <typename R, typename E>
    // requires R ForwardIterator range
    // requires E to be comparable with value_type(FwdIt)
    ASTL_NODISCARD auto operator()(R &&r, E const &element) const -> bool
    {
        return r::find(r, element) != adl::end(r);
    }

    template <typename R, typename E, typename P>
    ASTL_NODISCARD auto operator()(R &&r, E const &element, P p) const -> bool
    {
        return i::is_contained(adl::begin(r), adl::end(r), element, astl::pass_fn(p));
    }
} is_contained{};

inline constexpr struct {

    template <typename R, typename UnaryPredicate>
    // requires R ForwardIterator range
    // requires UnaryPredicate function returns bool, argument type value_type(FwdIt)
    ASTL_NODISCARD auto operator()(R &&r, UnaryPredicate pred) const -> bool
    {
        return i::is_contained_if(adl::begin(r), adl::end(r), astl::pass_fn(pred));
    }

    template <typename R, typename UnaryPredicate, typename P>
    ASTL_NODISCARD auto operator()(R &&r, UnaryPredicate pred, P p) const -> bool
    {
        return i::is_contained_if(adl::begin(r), adl::end(r), astl::pass_fn(pred),
                                  astl::pass_fn(p));
    }

} is_contained_if{};

inline constexpr struct {

    template <typename R, typename N, typename UnaryPredicate>
    // requires R ForwardIterator range
    // requires N integral type
    // requires UnaryPredicate function returns bool, argument type value_type(FwdIt)
    ASTL_NODISCARD auto operator()(R &&r, N n, UnaryPredicate pred) const -> bool
    {
        return i::is_contained_if_n(adl::begin(r), n, astl::pass_fn(pred));
    }

    template <typename R, typename N, typename UnaryPredicate, typename P>
    ASTL_NODISCARD auto operator()(R &&r, N n, UnaryPredicate pred, P p) const -> bool
    {
        return i::is_contained_if_n(adl::begin(r), n, astl::pass_fn(pred), astl::pass_fn(p));
    }

} is_contained_if_n{};

inline constexpr struct {

    template <typename R, typename N, typename E>
    // requires R ForwardIterator range
    // requires N integral type
    // requires E to be comparable with value_type(FwdIt)
    ASTL_NODISCARD auto operator()(R &&r, N n, E const &element) const -> bool
    {
        return i::is_contained_n(adl::begin(r), n, element);
    }

    template <typename R, typename N, typename E, typename P>
    ASTL_NODISCARD auto operator()(R &&r, N n, E const &element, P p) const -> bool
    {
        return i::is_contained_n(adl::begin(r), n, element, astl::pass_fn(p));
    }

} is_contained_n{};

} // namespace r
} // namespace astl

#endif // ASTL_INCLUDE_IS_CONTAINED_HPP
