//
// Created by Rijad on 29-Jul-18.
//

#ifndef ASTL_INCLUDE_FOR_EACH_HPP
#define ASTL_INCLUDE_FOR_EACH_HPP

#include <algorithm>
#include <type_traits>

#include "astl/functional.hpp"
#include "astl/iterator.hpp"
#include "astl/range_access.hpp"

namespace astl
{
namespace internal_for_each
{
template <int N, typename FwdIt, typename NaryFunction, typename Size>
auto for_each_n1(FwdIt first, NaryFunction f, Size d) -> FwdIt
{
    std::make_integer_sequence<int, N> seq{};
    while (d >= N) {
        internal::for_each1(f, first, seq);
        std::advance(first, N);
        d -= N;
    }
    return first;
}

template <int N, typename FwdIt, typename NaryFunction, typename Size, typename P>
auto for_each_n1(FwdIt first, NaryFunction f, Size d, P p) -> FwdIt
{
    std::make_integer_sequence<int, N> seq{};
    while (d >= N) {
        internal::for_each1(f, first, p, seq);
        std::advance(first, N);
        d -= N;
    }
    return first;
}

template <int N, typename I, typename NaryFunction>
auto for_each_adjacent1(I first, NaryFunction f, iter_diff_type<I> d) -> NaryFunction
{
    std::make_integer_sequence<int, N> seq{};
    d = d - N + 1;
    constexpr iter_diff_type<I> loop_unroll_num(4);
    auto trip_count(d >> 2);
    while (trip_count > 0) {
        internal::for_each1(f, first, seq);
        ++first;

        internal::for_each1(f, first, seq);
        ++first;

        internal::for_each1(f, first, seq);
        ++first;

        internal::for_each1(f, first, seq);
        ++first;

        d -= loop_unroll_num;
        --trip_count;
    }

    switch (d) {
    case 3: internal::for_each1(f, first, seq); ++first;
    case 2: internal::for_each1(f, first, seq); ++first;
    case 1: internal::for_each1(f, first, seq);
    case 0:
    default: return f;
    }
}

template <int N, typename I, typename NaryPred, typename NaryFunction>
auto for_each_adjacent_if1(I first, NaryPred pred, NaryFunction f, iter_diff_type<I> d)
    -> NaryFunction
{
    std::make_integer_sequence<int, N> seq{};
    d = d - N + 1;
    constexpr iter_diff_type<I> loop_unroll_num(4);
    auto trip_count(d >> 2);
    while (trip_count > 0) {
        if (internal::for_each1(pred, first, seq)) internal::for_each1(f, first, seq);

        ++first;
        if (internal::for_each1(pred, first, seq)) internal::for_each1(f, first, seq);

        ++first;
        if (internal::for_each1(pred, first, seq)) internal::for_each1(f, first, seq);

        ++first;
        if (internal::for_each1(pred, first, seq)) internal::for_each1(f, first, seq);

        ++first;
        d -= loop_unroll_num;
        --trip_count;
    }

    switch (d) {
    case 3:
        if (internal::for_each1(pred, first, seq)) internal::for_each1(f, first, seq);

        ++first;
    case 2:
        if (internal::for_each1(pred, first, seq)) internal::for_each1(f, first, seq);

        ++first;
    case 1:
        if (internal::for_each1(pred, first, seq)) internal::for_each1(f, first, seq);

    case 0:
    default: return f;
    }
}

template <int N, bool> struct for_each_adjacent_t {
    template <typename FwdIt, typename NaryFunction>
    // requires FwdIt ForwardIterator
    // requires NaryFunction, N arguments of type value_type(FwdIt)
    auto operator()(FwdIt first, FwdIt last, NaryFunction f) const ->
        typename std::enable_if<(N > 0), NaryFunction>::type
    {
        internal_for_each::for_each_adjacent1<N>(first, astl::pass_fn(f),
                                                 astl::distance(first, last));
        return f;
    }

    template <typename FwdIt, typename NaryFunction, typename P>
    auto operator()(FwdIt first, FwdIt last, NaryFunction f, P p) const ->
        typename std::enable_if<(N > 0), NaryFunction>::type
    {
        internal_for_each::for_each_adjacent1<N>(
            first, astl::combine(astl::pass_fn(f), astl::pass_fn(p)), astl::distance(first, last));
        return f;
    }
};

template <int N> struct for_each_adjacent_t<N, true> {

    template <typename R, typename NaryFunction>
    auto operator()(R &&r, NaryFunction f) const ->
        typename std::enable_if<(N > 0), NaryFunction>::type
    {
        internal_for_each::for_each_adjacent1<N>(adl::begin(r), astl::pass_fn(f),
                                                 astl::size_or_distance(r));
        return f;
    }

    template <typename R, typename NaryFunction, typename P>
    auto operator()(R &&r, NaryFunction f, P p) const ->
        typename std::enable_if<(N > 0), NaryFunction>::type
    {
        internal_for_each::for_each_adjacent1<N>(adl::begin(r),
                                                 astl::combine(astl::pass_fn(f), astl::pass_fn(p)),
                                                 astl::size_or_distance(r));
        return f;
    }
};

template <int N, bool> struct for_each_adjacent_if_t {
    template <typename FwdIt, typename NaryPred, typename NaryFunction>
    // requires FwdIt ForwardIterator
    // requires NaryPred, return bool, N arguments of value_type(FwdIt)
    // requires NaryFunction, N arguments of value_type(FwdIt)
    auto operator()(FwdIt first, FwdIt last, NaryFunction f, NaryPred pred) const ->
        typename std::enable_if<(N > 0), NaryFunction>::type
    {
        internal_for_each::for_each_adjacent_if1<N>(first, astl::pass_fn(pred), astl::pass_fn(f),
                                                    astl::distance(first, last));
        return f;
    }

    template <typename FwdIt, typename NaryPred, typename NaryFunction, typename P>
    auto operator()(FwdIt first, FwdIt last, NaryFunction f, NaryPred pred, P p) const ->
        typename std::enable_if<(N > 0), NaryFunction>::type
    {
        internal_for_each::for_each_adjacent_if1<N>(
            first, astl::combine(astl::pass_fn(pred), astl::pass_fn(p)),
            astl::combine(astl::pass_fn(f), astl::pass_fn(p)), astl::distance(first, last));
        return f;
    }
};

template <int N> struct for_each_adjacent_if_t<N, true> {

    template <typename R, typename NaryPred, typename NaryFunction>
    auto operator()(R &&r, NaryFunction f, NaryPred pred) const ->
        typename std::enable_if<(N > 0), NaryFunction>::type
    {
        internal_for_each::for_each_adjacent_if1<N>(adl::begin(r), astl::pass_fn(pred),
                                                    astl::pass_fn(f), astl::size_or_distance(r));
        return f;
    }

    template <typename R, typename NaryPred, typename NaryFunction, typename P>
    auto operator()(R &&r, NaryFunction f, NaryPred pred, P p) const ->
        typename std::enable_if<(N > 0), NaryFunction>::type
    {
        internal_for_each::for_each_adjacent_if1<N>(
            adl::begin(r), astl::combine(astl::pass_fn(pred), astl::pass_fn(p)),
            astl::combine(astl::pass_fn(f), astl::pass_fn(p)), astl::size_or_distance(r));
        return f;
    }
};

} // namespace internal_for_each

namespace i
{

inline constexpr struct {
    template <typename BidIt, typename BinaryOp, typename A = identity>
    // requires BidIt Bidirectional Iterator
    // requires BinaryOp Binary Function on
    // iter_value_type<BidIt>,iter_value_type<BidIt> requires A Function on
    // iter_value_type<BidIt>
    auto operator()(BidIt first, BidIt last, BinaryOp op, A a = A{}) const
        -> std::tuple<BinaryOp, A, bool>
    {
        while (first != last) {
            --last;
            if (first == last) {
                (void) a(*first);
                return std::make_tuple(op, a, true);
            }
            (void) op(*first, *last);
            ++first;
        }
        return std::make_tuple(op, a, false);
    }

    template <typename BidIt, typename BinaryOp, typename A, typename P>
    auto operator()(BidIt first, BidIt last, BinaryOp op, A a, P p) const
        -> std::tuple<BinaryOp, A, bool>
    {
        while (first != last) {
            --last;
            if (first == last) {
                (void) a(invoke(p, *first));
                return std::make_tuple(op, a, true);
            }
            (void) op(invoke(p, *first), invoke(p, *last));
            ++first;
        }
        return std::make_tuple(op, a, false);
    }
} bidirectional_traversal{};

inline constexpr struct {
    template <typename FwdIt, typename BinaryOp>
    // requires FwdIt ForwardIterator
    // requires BinaryPredicate, two arguments value_type(FwdIt)
    auto operator()(FwdIt first, FwdIt last, BinaryOp op) const -> BinaryOp
    {
        if (first == last) return op;

        FwdIt prev(first);
        while (++first != last) {
            (void) op(*prev, *first);
            prev = first;
        }
        return op;
    }

    template <typename FwdIt, typename BinaryOp, typename P>
    auto operator()(FwdIt first, FwdIt last, BinaryOp op, P p) const -> BinaryOp
    {
        (*this)(first, last, astl::combine(astl::pass_fn(op), astl::pass_fn(p)));
        return op;
    }
} for_adjacent_pairs{};

inline constexpr struct {
    template <typename FwdIt, typename BinaryOp>
    // requires FwdIt ForwardIterator
    // requires BinaryOp, two arguments of type value_type(FwdIt)
    auto operator()(FwdIt first, FwdIt last, BinaryOp op) const -> BinaryOp
    {
        if (first == last) return op;

        FwdIt trailer(first);
        ++first;
        while (first != last) {
            FwdIt i(first);
            while (i != last) {
                (void) op(*trailer, *i);
                ++i;
            }
            ++first;
            ++trailer;
        }
        return op;
    }

    template <typename FwdIt, typename BinaryOp, typename P>
    auto operator()(FwdIt first, FwdIt last, BinaryOp op, P p) const -> BinaryOp
    {
        (*this)(first, last, astl::combine(astl::pass_fn(op), astl::pass_fn(p)));
        return op;
    }
} for_all_pairs{};

inline constexpr struct {
    template <typename FwdIt, typename N, typename BinaryOp>
    // requires FwdIt ForwardIterator
    // requires BinaryOp, two arguments of type value_type(FwdIt)
    auto operator()(FwdIt first, N n, BinaryOp op) const -> FwdIt
    {
        if (n != N(0)) {
            FwdIt trailer(first);
            ++first;
            while (--n != N(0)) {
                FwdIt i(first);
                N n2 = n;
                while (n2 != N(0)) {
                    (void) op(*trailer, *i);
                    ++i;
                    --n2;
                }
                ++first;
                ++trailer;
            }
        }
        return first;
    }

    template <typename FwdIt, typename N, typename BinaryOp, typename P>
    auto operator()(FwdIt first, N n, BinaryOp op, P p) const -> FwdIt
    {
        return (*this)(first, n, astl::combine(astl::pass_fn(op), astl::pass_fn(p)));
    }
} for_all_pairs_n{};

inline constexpr struct {

    template <typename FwdIt, typename UnaryOp>
    auto operator()(FwdIt first, FwdIt last, UnaryOp op) const -> UnaryOp
    {
        std::for_each(first, last, astl::pass_fn(op));
        return op;
    }

    template <typename FwdIt, typename UnaryOp, typename P>
    auto operator()(FwdIt first, FwdIt last, UnaryOp op, P p) const -> UnaryOp
    {
        while (first != last) {
            (void) op(invoke(p, *first));
            ++first;
        }
        return op;
    }
} for_each{};

template <int N = 2>
inline constexpr auto for_each_adjacent = internal_for_each::for_each_adjacent_t<N, false>{};

template <int N = 2>
inline constexpr auto for_each_adjacent_if = internal_for_each::for_each_adjacent_if_t<N, false>{};

inline constexpr struct {
    template <typename FwdIt, typename UnaryFunction, typename UnaryPredicate>
    // requires FwdIt ForwardIterator
    // requires UnaryFunction, argument of value_type(FwdIt)
    // requires UnaryPredicate, return bool, argument of value_type(FwdIt)
    auto operator()(FwdIt first, FwdIt last, UnaryFunction f, UnaryPredicate pred) const
        -> UnaryFunction
    {
        while (first != last) {
            if (pred(*first)) (void) f(*first);

            ++first;
        }
        return f;
    }

    template <typename FwdIt, typename UnaryPredicate, typename UnaryFunction, typename P>
    auto operator()(FwdIt first, FwdIt last, UnaryFunction f, UnaryPredicate pred, P p) const
        -> UnaryFunction
    {
        while (first != last) {
            if (pred(invoke(p, *first))) (void) f(invoke(p, *first));

            ++first;
        }
        return f;
    }
} for_each_if{};

inline constexpr struct {
    template <typename FwdIt, typename Size, typename UnaryFunction>
    // requires FwdIt ForwardIterator
    // requires Size integral type
    // requires UnaryFunction, unary function, argument of value_type(FwdIt)
    auto operator()(FwdIt first, Size d, UnaryFunction f) const -> FwdIt
    {
        while (d > 0) {
            (void) f(*first);
            --d;
            ++first;
        }
        return first;
    }

    template <typename FwdIt, typename Size, typename UnaryFunction, typename P>
    auto operator()(FwdIt first, Size d, UnaryFunction f, P p) const -> FwdIt
    {
        return (*this)(first, d, astl::combine(astl::pass_fn(f), astl::pass_fn(p)));
    }
} for_each_n{};

template <int N, typename FwdIt, typename NaryFunction>
// requires FwdIt ForwardIterator
// requires NaryFunction, N arguments of value_type(FwdIt)
auto for_each_N(FwdIt first, FwdIt last, NaryFunction f) ->
    typename std::enable_if<(N > 0), FwdIt>::type
{
    return internal_for_each::for_each_n1<N>(first, astl::pass_fn(f), astl::distance(first, last));
}

template <int N, typename FwdIt, typename NaryFunction, typename P>
auto for_each_N(FwdIt first, FwdIt last, NaryFunction f, P p) ->
    typename std::enable_if<(N > 0), FwdIt>::type
{
    return internal_for_each::for_each_n1<N>(first, astl::pass_fn(f), astl::distance(first, last),
                                             astl::pass_fn(p));
}

inline constexpr struct {
    template <typename FwdIt, typename Size, typename UnaryFunction, typename UnaryPredicate>
    // requires FwdIt ForwardIterator
    // requires Size integral type
    // requires UnaryFunction, unary function, argument of value_type(FwdIt)
    // requires UnaryPredicate, returns bool, argument of value_type(FwdIt)
    auto operator()(FwdIt first, Size d, UnaryFunction f, UnaryPredicate pred) const -> FwdIt
    {
        while (d > 0) {
            if (pred(*first)) (void) f(*first);

            --d;
            ++first;
        }
        return first;
    }

    template <typename FwdIt, typename Size, typename UnaryFunction, typename UnaryPredicate,
              typename P>
    auto operator()(FwdIt first, Size d, UnaryFunction f, UnaryPredicate pred, P p) const -> FwdIt
    {
        return i::for_each_n(astl::map_iterator(first, astl::pass_fn(p)), d, astl::pass_fn(f),
                             astl::pass_fn(pred))
            .base();
    }
} for_each_n_if{};

inline constexpr struct {
    template <typename FwdIt, typename UnaryFunc>
    auto operator()(FwdIt first, FwdIt last, UnaryFunc f) const -> UnaryFunc
    {
        while (first != last) {
            (void) f(first);
            ++first;
        }
        return f;
    }
} for_each_position{};

inline constexpr struct {
    template <typename FwdIt, typename T, typename UnaryFunction>
    auto operator()(FwdIt first, FwdIt last, T init, UnaryFunction f) const
        -> iter_value_type<FwdIt>
    {
        if (first == last) return init;

        while (first != last) {
            *first = init;
            init = f(std::move(init));
            ++first;
        }
        return init;
    }

    template <typename FwdIt, typename UnaryFunction>
    auto operator()(FwdIt first, FwdIt last, UnaryFunction f) const -> iter_value_type<FwdIt>
    {
        return (*this)(first, last, iter_value_type<FwdIt>{}, astl::pass_fn(f));
    }

    template <typename FwdIt, typename T, typename UnaryFunction, typename P>
    auto operator()(FwdIt first, FwdIt last, T init, UnaryFunction f, P p) const
        -> iter_value_type<FwdIt>
    {
        if (first == last) return init;

        while (first != last) {
            invoke(p, *first) = init;
            init = f(std::move(init));
            ++first;
        }
        return init;
    }
} iterate{};

inline constexpr struct {
    template <typename FwdIt, typename N, typename T, typename UnaryFunction>
    auto operator()(FwdIt first, N n, T init, UnaryFunction f) const
        -> std::pair<astl::iter_value_type<FwdIt>, FwdIt>
    {
        if (n == N(0)) return std::make_pair(init, first);

        while (n != N(0)) {
            *first = init;
            init = f(std::move(init));
            --n;
        }
        return std::make_pair(init, first);
    }

    template <typename FwdIt, typename N, typename T, typename UnaryFunction, typename P>
    auto operator()(FwdIt first, N n, T init, UnaryFunction f, P p) const
        -> std::pair<astl::iter_value_type<FwdIt>, FwdIt>
    {
        if (n == N(0)) return std::make_pair(init, first);

        while (n != N(0)) {
            invoke(p, *first) = init;
            init = f(std::move(init));
            --n;
        }
        return std::make_pair(init, first);
    }

    template <typename FwdIt, typename N, typename UnaryFunction>
    auto operator()(FwdIt first, N n, UnaryFunction f) const
        -> std::pair<astl::iter_value_type<FwdIt>, FwdIt>
    {
        return (*this)(first, n, iter_value_type<FwdIt>{}, astl::pass_fn(f));
    }
} iterate_n{};

} // namespace i

namespace r
{

inline constexpr struct {
    template <typename R, typename BinaryOp, typename A = identity>
    // requires R Bidirectional Iterator range
    // requires BinaryOp Binary Function on
    // iter_value_type<BidIt>,iter_value_type<BidIt> requires A Function on
    // iter_value_type<BidIt>
    auto operator()(R &&r, BinaryOp op, A a = A{}) const -> std::tuple<BinaryOp, A, bool>
    {
        return i::bidirectional_traversal(adl::begin(r), adl::end(r), std::move(op), std::move(a));
    }

    template <typename R, typename BinaryOp, typename A, typename P>
    auto operator()(R &&r, BinaryOp op, A a, P p) const -> std::tuple<BinaryOp, A, bool>
    {
        return i::bidirectional_traversal(adl::begin(r), adl::end(r), std::move(op), std::move(a),
                                          std::move(p));
    }
} bidirectional_traversal{};

inline constexpr struct {
    template <typename R, typename BinaryOp> auto operator()(R &&r, BinaryOp op) const -> BinaryOp
    {
        i::for_adjacent_pairs(adl::begin(r), adl::end(r), astl::pass_fn(op));
        return op;
    }

    template <typename R, typename BinaryOp, typename P>
    auto operator()(R &&r, BinaryOp op, P p) const -> BinaryOp
    {
        i::for_adjacent_pairs(adl::begin(r), adl::end(r), astl::pass_fn(op), astl::pass_fn(p));
        return op;
    }
} for_adjacent_pairs{};

inline constexpr struct {
    template <typename R, typename BinaryOp> auto operator()(R &&r, BinaryOp op) const -> BinaryOp
    {
        i::for_all_pairs(adl::begin(r), adl::end(r), astl::pass_fn(op));
        return op;
    }

    template <typename R, typename BinaryOp, typename P>
    auto operator()(R &&r, BinaryOp op, P p) const -> BinaryOp
    {
        i::for_all_pairs(adl::begin(r), adl::end(r), astl::pass_fn(op), astl::pass_fn(p));
        return op;
    }
} for_all_pairs{};

inline constexpr struct {
    template <typename R, typename UnaryFunction>
    auto operator()(R &&r, UnaryFunction f) const -> UnaryFunction
    {
        i::for_each(adl::begin(r), adl::end(r), astl::pass_fn(f));
        return f;
    }

    template <typename R, typename UnaryFunction, typename P>
    auto operator()(R &&r, UnaryFunction f, P p) const -> UnaryFunction
    {
        i::for_each(adl::begin(r), adl::end(r), astl::pass_fn(f), astl::pass_fn(p));
        return f;
    }
} for_each{};

template <int N = 2>
inline constexpr auto for_each_adjacent = internal_for_each::for_each_adjacent_t<N, true>{};

template <int N = 2>
inline constexpr auto for_each_adjacent_if = internal_for_each::for_each_adjacent_if_t<N, true>{};

inline constexpr struct {
    template <typename R, typename UnaryPredicate, typename UnaryFunction>
    auto operator()(R &&r, UnaryFunction f, UnaryPredicate pred) const -> UnaryFunction
    {
        i::for_each_if(adl::begin(r), adl::end(r), astl::pass_fn(pred), astl::pass_fn(f));
        return f;
    }

    template <typename R, typename UnaryPredicate, typename UnaryFunction, typename P>
    auto operator()(R &&r, UnaryFunction f, UnaryPredicate pred, P p) const -> UnaryFunction
    {
        i::for_each_if(adl::begin(r), adl::end(r), astl::pass_fn(pred), astl::pass_fn(f),
                       astl::pass_fn(p));
        return f;
    }
} for_each_if{};

inline constexpr struct {
    template <typename R, typename Size, typename UnaryFunction>
    auto operator()(R &&r, Size d, UnaryFunction f) const -> iter_of_range<R>
    {
        return i::for_each_n(adl::begin(r), d, astl::pass_fn(f));
    }

    template <typename R, typename Size, typename UnaryFunction, typename P>
    auto operator()(R &&r, Size d, UnaryFunction f, P p) const -> iter_of_range<R>
    {
        return i::for_each_n(adl::begin(r), d, astl::pass_fn(f), astl::pass_fn(p));
    }
} for_each_n{};

template <int N, typename R, typename NaryFunction>
auto for_each_N(R &&r, NaryFunction f) ->
    typename std::enable_if<(N > 0), astl::iter_of_range<R>>::type
{
    return internal_for_each::for_each_n1<N>(adl::begin(r), astl::pass_fn(f),
                                             astl::size_or_distance(r));
}

template <int N, typename R, typename NaryFunction, typename P>
auto for_each_N(R &&r, NaryFunction f, P p) ->
    typename std::enable_if<(N > 0), astl::iter_of_range<R>>::type
{
    return internal_for_each::for_each_n1<N>(adl::begin(r), astl::pass_fn(f),
                                             astl::size_or_distance(r), astl::pass_fn(p));
}

inline constexpr struct {
    template <typename R, typename Size, typename UnaryFunction, typename UnaryPredicate>
    auto operator()(R &&r, Size d, UnaryFunction f, UnaryPredicate pred) const -> iter_of_range<R>
    {
        return i::for_each_n_if(adl::begin(r), d, astl::pass_fn(f), astl::pass_fn(pred));
    }

    template <typename R, typename Size, typename UnaryFunction, typename UnaryPredicate,
              typename P>
    auto operator()(R &&r, Size d, UnaryFunction f, UnaryPredicate pred, P p) const
        -> iter_of_range<R>
    {
        return i::for_each_n_if(adl::begin(r), d, astl::pass_fn(f), astl::pass_fn(pred),
                                astl::pass_fn(p));
    }
} for_each_n_if{};

inline constexpr struct {
    template <typename R, typename UnaryPredicate>
    auto operator()(R &&r, UnaryPredicate pred) const -> UnaryPredicate
    {
        return i::for_each_position(adl::begin(r), adl::end(r), pred);
    }
} for_each_position{};

inline constexpr struct {
    template <typename R, typename T, typename UnaryFunction>
    auto iterate(R &&r, T init, UnaryFunction f) const -> range_value_type<R>
    {
        return i::iterate(adl::begin(r), adl::end(r), std::move(init), astl::pass_fn(f));
    }

    template <typename R, typename T, typename UnaryFunction, typename P>
    auto iterate(R &&r, T init, UnaryFunction f, P p) const -> range_value_type<R>
    {
        return i::iterate(adl::begin(r), adl::end(r), std::move(init), astl::pass_fn(f),
                          astl::pass_fn(p));
    }

    template <typename R, typename UnaryFunction>
    auto operator()(R &&r, UnaryFunction f) const -> range_value_type<R>
    {
        return i::iterate(adl::begin(r), adl::end(r), astl::pass_fn(f));
    }
} iterate{};

inline constexpr struct {
    template <typename R, typename N, typename T, typename UnaryFunction>
    auto operator()(R &&r, N n, T init, UnaryFunction f) const -> range_value_type<R>
    {
        return i::iterate_n(adl::begin(r), n, std::move(init), astl::pass_fn(f));
    }

    template <typename R, typename N, typename T, typename UnaryFunction, typename P>
    auto operator()(R &&r, N n, T init, UnaryFunction f, P p) const -> range_value_type<R>
    {
        return i::iterate_n(adl::begin(r), n, std::move(init), astl::pass_fn(f), astl::pass_fn(p));
    }

    template <typename R, typename N, typename UnaryFunction>
    auto operator()(R &&r, N n, UnaryFunction f) const -> range_value_type<R>
    {
        return i::iterate_n(adl::begin(r), n, astl::pass_fn(f));
    }
} iterate_n{};

} // namespace r
} // namespace astl

#endif // ASTL_INCLUDE_FOR_EACH_HPP
