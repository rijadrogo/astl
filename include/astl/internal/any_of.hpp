//
// Created by Rijad on 29-Jul-18.
//

#ifndef ASTL_INCLUDE_ANY_OF_HPP
#define ASTL_INCLUDE_ANY_OF_HPP

#include <algorithm>
#include <type_traits>

#include "count.hpp"
#include "find.hpp"
#include "none_of.hpp"

#include "astl/functional.hpp"
#include "astl/range_access.hpp"

namespace astl
{
namespace i
{
inline constexpr struct {
    template <typename InIt>
    // requires InIt InputIterator
    ASTL_NODISCARD auto operator()(InIt first, InIt last) const -> bool
    {
        return std::find(first, last, true) != last;
    }

    template <typename InIt, typename UnaryPredicate>
    ASTL_NODISCARD auto operator()(InIt first, InIt last, UnaryPredicate pred) const -> bool
    {
        return std::any_of(first, last, astl::pass_fn(pred));
    }

    template <typename InIt, typename UnaryPredicate, typename P>
    ASTL_NODISCARD auto operator()(InIt first, InIt last, UnaryPredicate pred, P p) const -> bool
    {
        return std::any_of(first, last, astl::combine(astl::pass_fn(pred), astl::pass_fn(p)));
    }
} any_of{};

namespace internal_any_of
{

template <int N, bool> struct any_of_adjacent_t {

    template <typename FwdIt, typename NaryPred>
    ASTL_NODISCARD auto operator()(FwdIt first, FwdIt last, NaryPred pred) const ->
        typename std::enable_if<(N > 0), bool>::type
    {
        return !i::none_of_adjacent<N>(first, last, astl::pass_fn(pred));
    }

    template <typename FwdIt, typename NaryPred, typename P>
    ASTL_NODISCARD auto operator()(FwdIt first, FwdIt last, NaryPred pred, P p) const ->
        typename std::enable_if<(N > 0), bool>::type
    {
        return !i::none_of_adjacent<N>(first, last,
                                       astl::combine(astl::pass_fn(pred), astl::pass_fn(p)));
    }
};

template <int N> struct any_of_adjacent_t<N, true> {

    template <typename R, typename NaryPred>
    ASTL_NODISCARD auto operator()(R &&r, NaryPred pred) const ->
        typename std::enable_if<(N > 0), bool>::type
    {
        return !r::none_of_adjacent<N>(r, astl::pass_fn(pred));
    }

    template <typename R, typename NaryPred, typename P>
    ASTL_NODISCARD auto operator()(R &&r, NaryPred pred, P p) const ->
        typename std::enable_if<(N > 0), bool>::type
    {
        return !r::none_of_adjacent<N>(r, astl::pass_fn(pred), astl::pass_fn(p));
    }
};

} // namespace internal_any_of

template <int N>
inline constexpr auto any_of_adjacent = internal_any_of::any_of_adjacent_t<N, false>{};

inline constexpr struct {
    template <typename InIt, typename E>
    // requires InIt InputIterator
    // requires E quality comparable with value_type(InIt)
    ASTL_NODISCARD auto operator()(InIt first, InIt last, E const &e) const -> bool
    {
        return std::find(first, last, e) != last;
    }

    template <typename InIt, typename E, typename P>
    ASTL_NODISCARD auto operator()(InIt first, InIt last, E const &e, P p) const -> bool
    {
        return i::find(first, last, e, astl::pass_fn(p)) != last;
    }
} any_of_equal{};

inline constexpr struct {
    template <typename InIt, typename N, typename E>
    // requires InIt InputIterator
    // requires N integral type
    // requires E quality comparable with value_type(InIt)
    ASTL_NODISCARD auto operator()(InIt first, N n, E const &e) const -> bool
    {
        return !i::none_of_equal_n(first, n, e);
    }

    template <typename InIt, typename N, typename E, typename P>
    ASTL_NODISCARD auto operator()(InIt first, N n, E const &e, P p) const -> bool
    {
        return !i::none_of_equal_n(first, n, e, astl::pass_fn(p));
    }
} any_of_equal_n{};

inline constexpr struct {
    template <typename InIt, typename N>
    // requires InIt InputIterator
    // requires N integral type
    ASTL_NODISCARD auto operator()(InIt first, N n) const -> bool
    {
        return i::any_of_equal_n(first, n, true);
    }

    template <typename InIt, typename N, typename UnaryPredicate>
    // requires InIt InputIterator
    // requires N integral type
    // requires UnaryPredicate, returns bool, argument value_type(InIt)
    ASTL_NODISCARD auto operator()(InIt first, N n, UnaryPredicate pred) const -> bool
    {
        return !i::none_of_n(first, n, astl::pass_fn(pred));
    }

    template <typename InIt, typename N, typename UnaryPredicate, typename P>
    ASTL_NODISCARD auto operator()(InIt first, N n, UnaryPredicate pred, P p) const -> bool
    {
        return (*this)(first, n, astl::combine(astl::pass_fn(pred), astl::pass_fn(p)));
    }
} any_of_n{};

inline constexpr struct {
    template <typename InIt, typename UnaryPredicate>
    // requires InIt InputIterator
    // requires UnaryPredicate, returns bool argument value_type(InIt)
    ASTL_NODISCARD auto operator()(InIt first, InIt last, iter_diff_type<InIt> n,
                                   UnaryPredicate pred) const -> bool
    {
        if (n < 0 || first == last) return false;
        if constexpr (is_random_access_it_v<InIt>) {
            if (last - first < n) return false;
        }
        return i::count_if(first, last, astl::pass_fn(pred)) == n;
    }

    template <typename InIt, typename UnaryPredicate, typename P>
    ASTL_NODISCARD auto operator()(InIt first, InIt last, iter_diff_type<InIt> n,
                                   UnaryPredicate pred, P p) const -> bool
    {
        return (*this)(first, last, n, astl::combine(astl::pass_fn(pred), astl::pass_fn(p)));
    }
} has_n_items{};

inline constexpr struct {

    template <typename InIt, typename UnaryPredicate>
    // requires InIt InputIterator
    // requires UnaryPredicate, returns bool argument value_type(InIt)
    ASTL_NODISCARD auto operator()(InIt first, InIt last, iter_diff_type<InIt> n,
                                   UnaryPredicate pred) const -> bool
    {
        if (n < 0 || first == last) return false;
        if constexpr (is_random_access_it_v<InIt>) {
            if (last - first < n) return false;
        }
        return i::count_if(first, last, astl::pass_fn(pred)) >= n;
    }

    template <typename InIt, typename UnaryPredicate, typename P>
    ASTL_NODISCARD auto operator()(InIt first, InIt last, iter_diff_type<InIt> n,
                                   UnaryPredicate pred, P p) const -> bool
    {
        return (*this)(first, last, n, astl::combine(astl::pass_fn(pred), astl::pass_fn(p)));
    }

} has_n_items_or_more{};

inline constexpr struct {

    /// \fn one_of ( InputIterator first, InputIterator last, Predicate p )
    /// \return true if the predicate 'p' is true for exactly one item in [first,
    /// last).
    ///
    /// \param first The start of the input sequence
    /// \param last  One past the end of the input sequence
    /// \param pred     A predicate for testing the elements of the sequence
    ///
    template <typename InIt, typename UnaryPredicate>
    // requires InIt InputIterator
    // requires UnaryPredicate, returns bool, argument value_type(InIt)
    ASTL_NODISCARD auto operator()(InIt first, InIt last, UnaryPredicate pred) const -> bool
    {
        return i::find_if_unique(first, last, astl::pass_fn(pred)) != last;
    }

    template <typename InIt, typename UnaryPredicate, typename P>
    ASTL_NODISCARD auto operator()(InIt first, InIt last, UnaryPredicate pred, P p) const -> bool
    {
        return (*this)(first, last, astl::combine(astl::pass_fn(pred), astl::pass_fn(p)));
    }

} one_of{};

inline constexpr struct {
    template <typename InIt, typename N, typename UnaryPredicate>
    // requires InIt InputIterator
    // requires N integral type
    // requires UnaryPredicate, returns bool, argument value_type(InIt)
    ASTL_NODISCARD auto operator()(InIt first, N n, UnaryPredicate pred) const -> bool
    {
        return i::find_if_unique_n(first, n, astl::pass_fn(pred)).second != n;
    }

    template <typename InIt, typename N, typename UnaryPredicate, typename P>
    ASTL_NODISCARD auto operator()(InIt first, N n, UnaryPredicate pred, P p) const -> bool
    {
        return (*this)(first, n, astl::combine(astl::pass_fn(pred), astl::pass_fn(p)));
    }
} one_of_n{};

} // namespace i

namespace r
{

inline constexpr struct {

    template <typename R>
    // requires R InputIterator range
    ASTL_NODISCARD auto operator()(R &&r) const -> bool
    {
        return i::any_of(adl::begin(r), adl::end(r));
    }

    template <typename R, typename UnaryPredicate>
    ASTL_NODISCARD auto operator()(R &&r, UnaryPredicate pred) const -> bool
    {
        return i::any_of(adl::begin(r), adl::end(r), astl::pass_fn(pred));
    }

    template <typename R, typename UnaryPredicate, typename P>
    ASTL_NODISCARD auto operator()(R &&r, UnaryPredicate pred, P p) const -> bool
    {
        return i::any_of(adl::begin(r), adl::end(r), astl::pass_fn(pred), astl::pass_fn(p));
    }

} any_of{};

template <int N>
inline constexpr auto any_of_adjacent = i::internal_any_of::any_of_adjacent_t<N, true>{};

inline constexpr struct {
    template <typename R, typename E>
    // requires R InputIterator range
    // requires E quality comparable with value_type(InIt)
    ASTL_NODISCARD auto operator()(R &&r, E const &e) const -> bool
    {
        return i::any_of_equal(adl::begin(r), adl::end(r), e);
    }

    template <typename R, typename E, typename P>
    ASTL_NODISCARD auto operator()(R &&r, E const &e, P p) const -> bool
    {
        return i::any_of_equal(adl::begin(r), adl::end(r), e, astl::pass_fn(p));
    }
} any_of_equal{};

inline constexpr struct {
    template <typename R, typename N, typename E>
    // requires R InputIterator range
    // requires N integral type
    // requires E quality comparable with value_type(InIt)
    ASTL_NODISCARD auto operator()(R &&r, N n, E const &e) const -> bool
    {
        return i::any_of_equal_n(adl::begin(r), n, e);
    }

    template <typename R, typename N, typename E, typename P>
    ASTL_NODISCARD auto operator()(R &&r, N n, E const &e, P p) const -> bool
    {
        return i::any_of_equal_n(adl::begin(r), n, e, astl::pass_fn(p));
    }
} any_of_equal_n{};

inline constexpr struct {
    template <typename R, typename N>
    // requires R InputIterator range
    // requires N integral type
    ASTL_NODISCARD auto operator()(R &&r, N n) const -> bool
    {
        return i::any_of_n(adl::begin(r), n);
    }

    template <typename R, typename N, typename UnaryPredicate>
    // requires R InputIterator range
    // requires N integral type
    // requires UnaryPredicate, returns bool, argument value_type(InIt)
    ASTL_NODISCARD auto operator()(R &&r, N n, UnaryPredicate pred) const -> bool
    {
        return i::any_of_n(adl::begin(r), n, astl::pass_fn(pred));
    }

    template <typename R, typename N, typename UnaryPredicate, typename P>
    ASTL_NODISCARD auto operator()(R &&r, N n, UnaryPredicate pred, P p) const -> bool
    {
        return i::any_of_n(adl::begin(r), n, astl::pass_fn(pred), astl::pass_fn(p));
    }
} any_of_n{};

inline constexpr struct {
    template <typename R, typename UnaryPredicate>
    // requires R InputIterator range
    // requires UnaryPredicate, returns bool argument value_type(R)
    ASTL_NODISCARD auto operator()(R r, range_diff_type<R> n, UnaryPredicate pred) const -> bool
    {
        return i::has_n_items(adl::begin(r), adl::end(r), n, astl::pass_fn(pred));
    }

    template <typename R, typename UnaryPredicate, typename P>
    ASTL_NODISCARD auto operator()(R r, range_diff_type<R> n, UnaryPredicate pred, P p) const
        -> bool
    {
        return i::has_n_items(adl::begin(r), adl::end(r), n,
                              astl::combine(astl::pass_fn(pred), astl::pass_fn(p)));
    }
} has_n_items{};

inline constexpr struct {
    template <typename R, typename UnaryPredicate>
    // requires R InputIterator range
    // requires UnaryPredicate, returns bool argument value_type(R)
    ASTL_NODISCARD auto operator()(R r, range_diff_type<R> n, UnaryPredicate pred) const -> bool
    {
        return i::has_n_items(adl::begin(r), adl::end(r), n, astl::pass_fn(pred));
    }

    template <typename R, typename UnaryPredicate, typename P>
    ASTL_NODISCARD auto operator()(R r, range_diff_type<R> n, UnaryPredicate pred, P p) const
        -> bool
    {
        return i::has_n_items(adl::begin(r), adl::end(r), n,
                              astl::combine(astl::pass_fn(pred), astl::pass_fn(p)));
    }
} has_n_items_or_more{};

inline constexpr struct {
    template <typename R, typename UnaryPredicate>
    // requires InIt InputIterator
    // requires UnaryPredicate, returns bool, argument value_type(InIt)
    ASTL_NODISCARD auto operator()(R &&r, UnaryPredicate pred) const -> bool
    {
        return i::one_of(adl::begin(r), adl::end(r), astl::pass_fn(pred));
    }

    template <typename R, typename UnaryPredicate, typename P>
    ASTL_NODISCARD auto operator()(R &&r, UnaryPredicate pred, P p) const -> bool
    {
        return i::one_of(adl::begin(r), adl::end(r), astl::pass_fn(pred), astl::pass_fn(p));
    }
} one_of{};

inline constexpr struct {
    template <typename R, typename N, typename UnaryPredicate>
    // requires InIt InputIterator
    // requires N integral type
    // requires UnaryPredicate, returns bool, argument value_type(InIt)
    ASTL_NODISCARD auto operator()(R &&r, N n, UnaryPredicate pred) const -> bool
    {
        return i::one_of_n(adl::begin(r), n, astl::pass_fn(pred));
    }

    template <typename R, typename N, typename UnaryPredicate, typename P>
    ASTL_NODISCARD auto operator()(R &&r, N n, UnaryPredicate pred, P p) const -> bool
    {
        return i::one_of_n(adl::begin(r), n, astl::pass_fn(pred), astl::pass_fn(p));
    }
} one_of_n{};

} // namespace r
} // namespace astl

#endif // ASTL_INCLUDE_ANY_OF_HPP
