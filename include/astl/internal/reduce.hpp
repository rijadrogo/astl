//
// Created by Rijad on 04-Aug-18.
//

#ifndef ASTL_INCLUDE_REDUCE_HPP
#define ASTL_INCLUDE_REDUCE_HPP

#include <functional>
#include <string>
#include <utility>

#include "find.hpp"

#include "astl/functional.hpp"
#include "astl/iterator.hpp"
#include "astl/range_access.hpp"
#include "astl/temporary_buffer.hpp"

namespace astl
{
namespace internal_reduce
{
template <typename Op> struct identity_element {
    /// the type of the identity is the same as the result type of the operation
    using result_type = typename Op::result_type;

    /// Yields the identity element
    // ReSharper disable once CppFunctionIsNotImplemented
    auto operator()() const -> result_type;
};

template <>
struct identity_element<std::plus<std::string>> // NOLINT(modernize-use-transparent-functors)
{
    /// the type of the identity is the same as the result type of the operation
    using result_type = std::string;

    /// Yields the identity element for addition
    auto operator()() const noexcept -> result_type { return std::string(); }
};

template <typename T>
struct identity_element<std::plus<T>> // NOLINT(modernize-use-transparent-functors)
{
    /// the type of the identity is the same as the result type of the operation
    using result_type = T;

    /// Yields the identity element for addition
    auto operator()() const -> result_type { return result_type(0); }
};

template <typename T>
struct identity_element<std::minus<T>> // NOLINT(modernize-use-transparent-functors)
{
    /// the type of the identity is the same as the result type of the operation
    using result_type = T;

    /// Yields the identity element for addition
    auto operator()() const -> result_type { return result_type(0); }
};

template <typename T>
struct identity_element<std::multiplies<T>> // NOLINT(modernize-use-transparent-functors)
{
    /// the type of the identity is the same as the result type of the operation
    using result_type = T;

    /// Yields the identity element for multiplication
    auto operator()() const -> result_type { return result_type(1); }
};

template <typename T>
struct identity_element<std::divides<T>> // NOLINT(modernize-use-transparent-functors)
{
    /// the type of the identity is the same as the result type of the operation
    using result_type = T;

    /// Yields the identity element for multiplication
    auto operator()() const -> result_type { return result_type(1); }
};
} // namespace internal_reduce

namespace i
{
template <typename FwdIt, typename BinaryOp>
// requires FwdIt ForwardIterator
// requires BinaryOp BinaryOperation
auto add_to_counter(FwdIt first, FwdIt const last, BinaryOp op, iter_value_type<FwdIt> value,
                    iter_value_type<FwdIt> const &zero) -> iter_value_type<FwdIt>
{
    if (value == zero) return zero;

    while (first != last) {
        if (*first != zero) {
            value = op(*first, value);
            *first = zero;
        }
        else {
            *first = value;
            return zero;
        }
        ++first;
    }
    return value;
}

template <typename FwdIt, typename BinaryOp>
// requires FwdIt ForwardIterator
// requires BinaryOp BinaryOperation
auto add_to_counter(FwdIt first, FwdIt last, BinaryOp op, iter_value_type<FwdIt> value)
    -> iter_value_type<FwdIt>
{
    return i::add_to_counter(first, last, astl::pass_fn(op), std::move(value),
                             internal_reduce::identity_element<BinaryOp>{}());
}

template <typename BidiIt, typename BinaryOp>
// requires BidiIt BidirectionalIterator
// requires BinaryOp BinaryOperation
auto add_to_counter_backward(BidiIt first, BidiIt last, BinaryOp op, iter_value_type<BidiIt> value,
                             iter_value_type<BidiIt> const &zero) -> iter_value_type<BidiIt>
{
    return i::add_to_counter(std::make_reverse_iterator(last), std::make_reverse_iterator(first),
                             astl::pass_fn(op), std::move(value), zero);
}

template <typename BidiIt, typename BinaryOp>
// requires BidiIt BidirectionalIterator
// requires BinaryOp BinaryOperation
auto add_to_counter_backward(BidiIt first, BidiIt last, BinaryOp op, iter_value_type<BidiIt> value)
    -> iter_value_type<BidiIt>
{
    return i::add_to_counter_backward(first, last, astl::pass_fn(op), std::move(value),
                                      internal_reduce::identity_element<BinaryOp>{}());
}

template <typename FwdIt, typename N, typename BinaryOp>
// requires FwdIt ForwardIterator
// requires N integral type
// requires BinaryOp BinaryOperation
auto add_to_counter_n(FwdIt first, N n, BinaryOp op, iter_value_type<FwdIt> value,
                      iter_value_type<FwdIt> const &zero)
    -> std::pair<astl::iter_value_type<FwdIt>, FwdIt>
{
    if (value == zero) return std::make_pair(zero, first);

    while (n != N(0)) {
        if (*first != zero) {
            value = op(*first, value);
            *first = zero;
        }
        else {
            *first = value;
            return zero;
        }
        ++first;
        --n;
    }
    return std::make_pair(value, first);
}

template <typename FwdIt, typename N, typename BinaryOp>
// requires FwdIt ForwardIterator
// requires N integral type
// requires BinaryOp BinaryOperation
auto add_to_counter_n(FwdIt first, N n, BinaryOp op, iter_value_type<FwdIt> value)
    -> std::pair<astl::iter_value_type<FwdIt>, FwdIt>
{
    return i::add_to_counter_n(first, n, astl::pass_fn(op), std::move(value),
                               internal_reduce::identity_element<BinaryOp>{}());
}

template <typename InIt, typename OutIt, typename BinaryOp>
// requires InIt InputIterator
// requires OutIt OutputIterator
// requires BinaryOp, two arguments value_type(InIt)
auto adjacent_reduce(InIt first, InIt last, OutIt dest, BinaryOp op) -> OutIt
{
    if (first == last) return dest;

    if constexpr (is_forward_it_v<InIt>) { // Forward Iterator
        using FwdIt = InIt;
        FwdIt trailer(first);
        while (++first != last) {
            *dest = op(*trailer, *first);
            trailer = first;
            ++dest;
        }
        return dest;
    }
    else { // Input Iterator
        auto prev_value(*first);
        while (++first != last) {
            *dest = op(prev_value, *first);
            prev_value = *first;
            ++dest;
        }
        return dest;
    }
}

template <typename FwdIt, typename OutIt, typename BinaryOp, typename P>
auto adjacent_reduce(FwdIt first, FwdIt last, OutIt dest, BinaryOp bin_op, P p) -> OutIt
{
    return i::adjacent_reduce(first, last, dest,
                              astl::combine(astl::pass_fn(bin_op), astl::pass_fn(p)));
}

template <typename InIt, typename T>
ASTL_NODISCARD auto reduce(InIt first, InIt const last, T init) -> T
{
    while (first != last) {
        init = std::move(init) + *first;
        ++first;
    }
    return init;
}

template <typename InIt, typename T, typename BinaryOp>
ASTL_NODISCARD auto reduce(InIt first, InIt const last, T init, BinaryOp op) -> T
{
    while (first != last) {
        init = op(std::move(init), *first);
        ++first;
    }
    return init;
}

template <typename InIt, typename T, typename BinaryOp, typename P>
ASTL_NODISCARD auto reduce(InIt first, InIt const last, T init, BinaryOp op, P p) -> T
{
    while (first != last) {
        init = op(std::move(init), invoke(p, *first));
        ++first;
    }
    return init;
}

template <typename InIt>
ASTL_NODISCARD auto reduce(InIt first, InIt const last) -> iter_value_type<InIt>
{
    auto init(*first);
    while (++first != last) init = std::move(init) + *first;

    return init;
}

template <typename InIt>
ASTL_NODISCARD auto reduce_backward(InIt first, InIt last) -> iter_value_type<InIt>
{
    return i::reduce(std::make_reverse_iterator(last), std::make_reverse_iterator(first));
}

template <typename InIt, typename T>
ASTL_NODISCARD auto reduce_backward(InIt first, InIt last, T init) -> T
{
    return i::reduce(std::make_reverse_iterator(last), std::make_reverse_iterator(first),
                     std::move(init));
}

template <typename InIt, typename T, typename BinaryOp>
ASTL_NODISCARD auto reduce_backward(InIt first, InIt last, T init, BinaryOp op) -> T
{
    return i::reduce(std::make_reverse_iterator(last), std::make_reverse_iterator(first),
                     std::move(init), astl::pass_fn(op));
}

template <typename InIt, typename T, typename BinaryOp, typename P>
ASTL_NODISCARD auto reduce_backward(InIt first, InIt last, T init, BinaryOp op, P p) -> T
{
    return i::reduce_backward(first, last, std::move(init),
                              astl::combine(astl::pass_fn(op), astl::pass_fn(p)));
}

template <typename InIt, typename N>
ASTL_NODISCARD auto reduce_n(InIt first, N n) -> std::pair<astl::iter_value_type<InIt>, InIt>
{
    auto init(*first);
    while (--n != N(0)) init = std::move(init) + *++first;

    return std::make_pair(init, first);
}

template <typename InIt, typename N, typename T>
ASTL_NODISCARD auto reduce_n(InIt first, N n, T init) -> std::pair<T, InIt>
{
    while (n != N(0)) {
        init = std::move(init) + *first;
        ++first;
        --n;
    }
    return std::make_pair(init, first);
}

template <typename InIt, typename N, typename T, typename BinaryOp>
ASTL_NODISCARD auto reduce_n(InIt first, N n, T init, BinaryOp op) -> std::pair<T, InIt>
{
    while (n != N(0)) {
        init = op(std::move(init), *first);
        ++first;
        --n;
    }
    return std::make_pair(init, first);
}

template <typename InIt, typename N, typename T, typename BinaryOp, typename P>
ASTL_NODISCARD auto reduce_n(InIt first, N n, T init, BinaryOp op, P p) -> std::pair<T, InIt>
{
    while (n != N(0)) {
        init = op(std::move(init), invoke(p, *first));
        ++first;
        --n;
    }
    return std::make_pair(init, first);
}

template <typename InIt, typename BinaryOp>
// requires InIt InputIterator
// requires BinaryOp BinaryOperation
ASTL_NODISCARD auto reduce_nonzeros(InIt first, InIt const last, BinaryOp op,
                                    iter_value_type<InIt> const &zero) -> iter_value_type<InIt>
{
    // skip zeros
    first = i::find_not(first, last, zero);
    if (first == last) return zero;

    auto result(*first);
    ++first;
    while (first != last) {
        if (*first != zero) result = op(result, *first);

        ++first;
    }
    return result;
}

template <typename InIt, typename BinaryOp>
// requires InIt InputIterator
// requires BinaryOp BinaryOperation
ASTL_NODISCARD auto reduce_nonzeros(InIt first, InIt last, BinaryOp op) -> iter_value_type<InIt>
{
    return i::reduce_nonzeros(first, last, astl::pass_fn(op),
                              internal_reduce::identity_element<BinaryOp>{}());
}

template <typename InIt>
// requires InIt InputIterator
ASTL_NODISCARD auto reduce_nonzeros(InIt first, InIt last) -> iter_value_type<InIt>
{
    using Op = std::plus<iter_value_type<InIt>>;
    return i::reduce_nonzeros(first, last, Op{}, internal_reduce::identity_element<Op>{}());
}

template <typename BidiIt, typename BinaryOp>
// requires BidiIt BidirectionalIterator
// requires BinaryOp BinaryOperation
ASTL_NODISCARD auto reduce_nonzeros_backward(BidiIt first, BidiIt last, BinaryOp op,
                                             iter_value_type<BidiIt> const &zero)
    -> iter_value_type<BidiIt>
{
    return i::reduce_nonzeros(std::make_reverse_iterator(last), std::make_reverse_iterator(first),
                              astl::pass_fn(op), std::move(zero));
}

template <typename BidiIt, typename BinaryOp>
// requires BidiIt BidirectionalIterator
// requires BinaryOp BinaryOperation
ASTL_NODISCARD auto reduce_nonzeros_backward(BidiIt first, BidiIt last, BinaryOp op)
    -> iter_value_type<BidiIt>
{
    return i::reduce_nonzeros_backward(first, last, astl::pass_fn(op),
                                       internal_reduce::identity_element<BinaryOp>{}());
}

template <typename BidiIt>
// requires BidiIt BidirectionalIterator
ASTL_NODISCARD auto reduce_nonzeros_backward(BidiIt first, BidiIt last) -> iter_value_type<BidiIt>
{
    using Op = std::plus<iter_value_type<BidiIt>>;
    return i::reduce_nonzeros_backward(first, last, Op{},
                                       internal_reduce::identity_element<Op>{}());
}

template <typename InIt, typename N, typename BinaryOp>
// requires InIt InputIterator
// requires N integral type
// requires BinaryOp BinaryOperation
ASTL_NODISCARD auto reduce_nonzeros_n(InIt first, N n, BinaryOp op,
                                      iter_value_type<InIt> const &zero)
    -> std::pair<astl::iter_value_type<InIt>, InIt>
{
    // skip zeros
    std::pair<InIt, N> p(i::find_not_n(first, n, zero));
    if (p.second == N(0)) return std::make_pair(zero, p.first);

    first = p.first;
    auto result(*first);
    ++first;
    while (--n != N(0)) {
        if (*first != zero) result = op(result, *first);

        ++first;
    }
    return std::make_pair(result, first);
}

template <typename InIt, typename N, typename BinaryOp>
// requires InIt InputIterator
// requires N integral type
// requires BinaryOp BinaryOperation
ASTL_NODISCARD auto reduce_nonzeros_n(InIt first, N n, BinaryOp op)
    -> std::pair<astl::iter_value_type<InIt>, InIt>
{
    return i::reduce_nonzeros_n(first, n, astl::pass_fn(op),
                                internal_reduce::identity_element<BinaryOp>{}());
}

template <typename InIt, typename N>
// requires InIt InputIterator
// requires N integral type
ASTL_NODISCARD auto reduce_nonzeros_n(InIt first, N n)
    -> std::pair<astl::iter_value_type<InIt>, InIt>
{
    using Op = std::plus<iter_value_type<InIt>>;
    return i::reduce_nonzeros_n(first, n, Op{}, internal_reduce::identity_element<Op>{}());
}

template <typename InIt, typename BinaryOp>
// requires InIt InputIterator
// requires BinaryOp BinaryOperation
ASTL_NODISCARD auto reduce_balanced(InIt first, InIt const last, BinaryOp op,
                                    iter_value_type<InIt> const &zero) -> iter_value_type<InIt>
{
    using T = iter_value_type<InIt>;
    inline_temporary_buffer<T> buffer(astl::distance(first, last), zero);
    auto buff(buffer.begin());
    auto op1(astl::pass_fn(op));
    while (first != last) {
        T tmp(i::add_to_counter(buffer.begin(), buff, op1, *first, zero));

        if (tmp != zero) {
            *buff = std::move(tmp);
            ++buff;
        }
        ++first;
    }
    return i::reduce_nonzeros(buffer.begin(), buff, astl::transpose(op1), zero);
}

template <typename InIt, typename BinaryOp>
// requires InIt InputIterator
// requires BinaryOp BinaryOperation
ASTL_NODISCARD auto reduce_balanced(InIt first, InIt last, BinaryOp op) -> iter_value_type<InIt>
{
    return i::reduce_balanced(first, last, astl::pass_fn(op),
                              internal_reduce::identity_element<BinaryOp>{}());
}

template <typename InIt>
// requires InIt InputIterator
ASTL_NODISCARD auto reduce_balanced(InIt first, InIt last) -> iter_value_type<InIt>
{
    using Op = std::plus<iter_value_type<InIt>>;
    return i::reduce_balanced(first, last, Op{}, internal_reduce::identity_element<Op>{}());
}

template <typename BidiIt, typename BinaryOp>
// requires BidiIt BidirectionalIterator
// requires BinaryOp BinaryOperation
ASTL_NODISCARD auto reduce_balanced_backward(BidiIt first, BidiIt last, BinaryOp op,
                                             iter_value_type<BidiIt> const &zero)
    -> iter_value_type<BidiIt>
{
    using T = iter_value_type<BidiIt>;
    inline_temporary_buffer<T> buffer(astl::distance(first, last), zero);
    auto buff(buffer.begin());
    auto op1(astl::pass_fn(op));
    if (first != last) {
        while (true) {
            --last;
            iter_value_type<BidiIt> tmp(
                i::add_to_counter_backward(buffer.begin(), buff, op1, *last, zero));

            if (tmp != zero) {
                *buff = std::move(tmp);
                ++buff;
            }

            if (last == first) break;
        }
    }
    return i::reduce_nonzeros_backward(buffer.begin(), buff, astl::transpose(op1), zero);
}

template <typename BidiIt, typename BinaryOp>
// requires BidiIt BidirectionalIterator
// requires BinaryOp BinaryOperation
ASTL_NODISCARD auto reduce_balanced_backward(BidiIt first, BidiIt last, BinaryOp op)
    -> iter_value_type<BidiIt>
{
    return i::reduce_balanced_backward(first, last, astl::pass_fn(op),
                                       internal_reduce::identity_element<BinaryOp>{}());
}

template <typename BidiIt>
// requires BidiIt BidirectionalIterator
ASTL_NODISCARD auto reduce_balanced_backward(BidiIt first, BidiIt last) -> iter_value_type<BidiIt>
{
    using Op = std::plus<iter_value_type<BidiIt>>;
    return i::reduce_balanced_backward(first, last, Op{},
                                       internal_reduce::identity_element<Op>{}());
}

template <typename InIt, typename N, typename BinaryOp>
// requires InIt InputIterator
// requires N integral type
// requires BinaryOp BinaryOperation
ASTL_NODISCARD auto reduce_balanced_n(InIt first, N n, BinaryOp op,
                                      iter_value_type<InIt> const &zero)
    -> std::pair<astl::iter_value_type<InIt>, InIt>
{
    using T = iter_value_type<InIt>;
    inline_temporary_buffer<T> buffer(n, zero);
    auto buff(buffer.begin());
    auto op1(astl::pass_fn(op));
    while (n != N(0)) {
        T tmp(i::add_to_counter(buffer.begin(), buff, op1, *first, zero));

        if (tmp != zero) {
            *buff = std::move(tmp);
            ++buff;
        }

        ++first;
        --n;
    }
    T tmp(i::reduce_nonzeros(buffer.begin(), buff, astl::transpose(op1), zero));
    return std::make_pair(std::move(tmp), first);
}

template <typename InIt, typename N, typename BinaryOp>
// requires InIt InputIterator
// requires N integral type
// requires BinaryOp BinaryOperation
ASTL_NODISCARD auto reduce_balanced_n(InIt first, N n, BinaryOp op)
    -> std::pair<astl::iter_value_type<InIt>, InIt>
{
    return i::reduce_balanced_n(first, n, astl::pass_fn(op),
                                internal_reduce::identity_element<BinaryOp>{}());
}

template <typename InIt, typename N>
// requires InIt InputIterator
// requires N integral type
// requires BinaryOp BinaryOperation
ASTL_NODISCARD auto reduce_balanced_n(InIt first, N n)
    -> std::pair<astl::iter_value_type<InIt>, InIt>
{
    using Op = std::plus<iter_value_type<InIt>>;
    return i::reduce_balanced_n(first, n, Op{}, internal_reduce::identity_element<Op>{}());
}
} // namespace i

namespace r
{
template <typename R, typename BinaryOp>
// requires R ForwardIterator range
// requires BinaryOp BinaryOperation
auto add_to_counter(R &&r, BinaryOp op, range_value_type<R> value, range_value_type<R> const &zero)
    -> range_value_type<R>
{
    return i::add_to_counter(adl::begin(r), adl::end(r), astl::pass_fn(op), std::move(value), zero);
}

template <typename R, typename BinaryOp>
// requires R ForwardIterator range
// requires BinaryOp BinaryOperation
auto add_to_counter(R &&r, BinaryOp op, range_value_type<R> value) -> range_value_type<R>
{
    return r::add_to_counter(r, astl::pass_fn(op), std::move(value),
                             internal_reduce::identity_element<BinaryOp>{}());
}

template <typename R, typename BinaryOp>
// requires R BidirectionalIterator range
// requires BinaryOp BinaryOperation
auto add_to_counter_backward(R &&r, BinaryOp op, range_value_type<R> value,
                             range_value_type<R> const &zero) -> range_value_type<R>
{
    return i::add_to_counter_backward(adl::begin(r), adl::end(r), astl::pass_fn(op),
                                      std::move(value), zero);
}

template <typename R, typename BinaryOp>
// requires R BidirectionalIterator range
// requires BinaryOp BinaryOperation
auto add_to_counter_backward(R &&r, BinaryOp op, range_value_type<R> value) -> range_value_type<R>
{
    return r::add_to_counter_backward(r, astl::pass_fn(op), std::move(value),
                                      internal_reduce::identity_element<BinaryOp>{}());
}

template <typename R, typename N, typename BinaryOp>
// requires R ForwardIterator range
// requires N integral type
// requires BinaryOp BinaryOperation
auto add_to_counter_n(R &&r, N n, BinaryOp op, range_value_type<R> value,
                      range_value_type<R> const &zero)
    -> std::pair<astl::range_value_type<R>, astl::iter_of_range<R>>
{
    return i::add_to_counter_n(adl::begin(r), n, astl::pass_fn(op), std::move(value), zero);
}

template <typename R, typename N, typename BinaryOp>
// requires R ForwardIterator range
// requires N integral type
// requires BinaryOp BinaryOperation
auto add_to_counter_n(R &&r, N n, BinaryOp op, range_value_type<R> value)
    -> std::pair<astl::range_value_type<R>, astl::iter_of_range<R>>
{
    return i::add_to_counter_n(adl::begin(r), n, astl::pass_fn(op), std::move(value),
                               internal_reduce::identity_element<BinaryOp>{}());
}

// NOTE for InputIterators this is gonna copy each element
template <typename R, typename OutIt, typename BinaryOp>
// requires R InputIterator range
// requires OutIt OutputIterator
// requires BinaryOp, two arguments value_type(R)
auto adjacent_reduce(R &&r, OutIt dest, BinaryOp binary_op) -> OutIt
{
    return i::adjacent_reduce(adl::begin(r), adl::end(r), dest, astl::pass_fn(binary_op));
}

// NOTE for InputIterators this is gonna copy each element
template <typename R, typename OutIt, typename BinaryOp, typename P>
auto adjacent_reduce(R &&r, OutIt dest, BinaryOp binary_op, P p) -> OutIt
{
    return i::adjacent_reduce(adl::begin(r), adl::end(r), dest, astl::pass_fn(binary_op),
                              astl::pass_fn(p));
}

template <typename R> ASTL_NODISCARD auto reduce(R &&r) -> range_value_type<R>
{
    return i::reduce(adl::begin(r), adl::end(r));
}

template <typename R, typename T> ASTL_NODISCARD auto reduce(R &&r, T init) -> T
{
    return i::reduce(adl::begin(r), adl::end(r), std::move(init));
}

template <typename R, typename T, typename BinaryOp>
ASTL_NODISCARD auto reduce(R &&r, T init, BinaryOp op) -> T
{
    return i::reduce(adl::begin(r), adl::end(r), std::move(init), astl::pass_fn(op));
}

template <typename R, typename T, typename BinaryOp, typename P>
ASTL_NODISCARD auto reduce(R &&r, T init, BinaryOp op, P p) -> T
{
    return i::reduce(adl::begin(r), adl::end(r), std::move(init), astl::pass_fn(op),
                     astl::pass_fn(p));
}

template <typename R, typename T, typename BinaryOp, typename P>
ASTL_NODISCARD auto reduce_backward(R &&r, T init, BinaryOp op, P p) -> T
{
    return i::reduce_backward(adl::begin(r), adl::end(r), std::move(init), astl::pass_fn(op),
                              astl::pass_fn(p));
}

template <typename R, typename T, typename BinaryOp>
ASTL_NODISCARD auto reduce_backward(R &&r, T init, BinaryOp op) -> T
{
    return i::reduce_backward(adl::begin(r), adl::end(r), std::move(init), astl::pass_fn(op));
}

template <typename R> ASTL_NODISCARD auto reduce_backward(R &&r) -> range_value_type<R>
{
    return i::reduce_backward(adl::begin(r), adl::end(r));
}

template <typename R, typename T> ASTL_NODISCARD auto reduce_backward(R &&r, T init) -> T
{
    return i::reduce_backward(adl::begin(r), adl::end(r), std::move(init));
}

template <typename R, typename N>
ASTL_NODISCARD auto reduce_n(R &&r, N n)
    -> std::pair<astl::range_value_type<R>, astl::iter_of_range<R>>
{
    return i::reduce_n(adl::begin(r), n);
}

template <typename R, typename N, typename T>
ASTL_NODISCARD auto reduce_n(R &&r, N n, T init) -> std::pair<T, astl::iter_of_range<R>>
{
    return i::reduce_n(adl::begin(r), n, std::move(init));
}

template <typename R, typename N, typename T, typename BinaryOp>
ASTL_NODISCARD auto reduce_n(R &&r, N n, T init, BinaryOp op)
    -> std::pair<T, astl::iter_of_range<R>>
{
    return i::reduce_n(adl::begin(r), n, std::move(init), astl::pass_fn(op));
}

template <typename R, typename N, typename T, typename BinaryOp, typename P>
ASTL_NODISCARD auto reduce_n(R &&r, N n, T init, BinaryOp op, P p)
    -> std::pair<T, astl::iter_of_range<R>>
{
    return i::reduce_n(adl::begin(r), n, std::move(init), astl::pass_fn(op), astl::pass_fn(p));
}

template <typename R, typename BinaryOp = std::plus<range_value_type<R>>>
// requires R InputIterator range
// requires BinaryOp BinaryOperation
ASTL_NODISCARD auto reduce_nonzeros(R &&r, BinaryOp op, range_value_type<R> const &zero)
    -> range_value_type<R>
{
    return i::reduce_nonzeros(adl::begin(r), adl::end(r), astl::pass_fn(op), zero);
}

template <typename R, typename BinaryOp>
// requires R InputIterator range
// requires BinaryOp BinaryOperation
ASTL_NODISCARD auto reduce_nonzeros(R &&r, BinaryOp op) -> range_value_type<R>
{
    return i::reduce_nonzeros(adl::begin(r), adl::end(r), astl::pass_fn(op),
                              internal_reduce::identity_element<BinaryOp>{}());
}

template <typename R>
// requires R InputIterator range
ASTL_NODISCARD auto reduce_nonzeros(R &&r) -> range_value_type<R>
{
    using Op = std::plus<range_value_type<R>>;
    return i::reduce_nonzeros(adl::begin(r), adl::end(r), Op{},
                              internal_reduce::identity_element<Op>{}());
}

template <typename R, typename BinaryOp>
// requires R BidirectionalIterator range
// requires BinaryOp BinaryOperation
ASTL_NODISCARD auto
reduce_nonzeros_backward(R &&r, BinaryOp op = BinaryOp{},
                         range_value_type<R> zero = internal_reduce::identity_element<BinaryOp>{}())
    -> range_value_type<R>
{
    return i::reduce_nonzeros_backward(adl::begin(r), adl::end(r), astl::pass_fn(op), zero);
}

template <typename R, typename BinaryOp>
// requires R BidirectionalIterator range
// requires BinaryOp BinaryOperation
ASTL_NODISCARD auto reduce_nonzeros_backward(R &&r, BinaryOp op) -> range_value_type<R>
{
    return i::reduce_nonzeros_backward(adl::begin(r), adl::end(r), astl::pass_fn(op),
                                       internal_reduce::identity_element<BinaryOp>{}());
}

template <typename R>
// requires R InputIterator range
ASTL_NODISCARD auto reduce_nonzeros_backward(R &&r) -> range_value_type<R>
{
    using Op = std::plus<range_value_type<R>>;
    return i::reduce_nonzeros_backward(adl::begin(r), adl::end(r), Op{},
                                       internal_reduce::identity_element<Op>{}());
}

template <typename R, typename N, typename BinaryOp>
// requires R InputIterator range
// requires N integral type
// requires BinaryOp BinaryOperation
ASTL_NODISCARD auto reduce_nonzeros_n(R &&r, N n, BinaryOp op, range_value_type<R> const &zero)
    -> std::pair<astl::range_value_type<R>, astl::iter_of_range<R>>
{
    return i::reduce_nonzeros_n(adl::begin(r), n, astl::pass_fn(op), zero);
}

template <typename R, typename N, typename BinaryOp>
// requires R InputIterator range
// requires N integral type
// requires BinaryOp BinaryOperation
ASTL_NODISCARD auto reduce_nonzeros_n(R &&r, N n, BinaryOp op)
    -> std::pair<astl::range_value_type<R>, astl::iter_of_range<R>>
{
    return i::reduce_nonzeros_n(adl::begin(r), n, astl::pass_fn(op),
                                internal_reduce::identity_element<BinaryOp>{}());
}

template <typename R, typename N>
// requires R InputIterator range
// requires N integral type
// requires BinaryOp BinaryOperation
ASTL_NODISCARD auto reduce_nonzeros_n(R &&r, N n)
    -> std::pair<astl::range_value_type<R>, astl::iter_of_range<R>>
{
    using Op = std::plus<range_value_type<R>>;
    return i::reduce_nonzeros_n(adl::begin(r), n, Op{}, internal_reduce::identity_element<Op>{}());
}

template <typename R, typename BinaryOp>
// requires R InputIterator range
// requires BinaryOp BinaryOperation
ASTL_NODISCARD auto reduce_balanced(R &&r, BinaryOp op, range_value_type<R> const &zero)
    -> range_value_type<R>
{
    return i::reduce_balanced(adl::begin(r), adl::end(r), astl::pass_fn(op), zero);
}

template <typename R, typename BinaryOp>
// requires R InputIterator range
// requires BinaryOp BinaryOperation
ASTL_NODISCARD auto reduce_balanced(R &&r, BinaryOp op) -> range_value_type<R>
{
    return i::reduce_balanced(adl::begin(r), adl::end(r), astl::pass_fn(op),
                              internal_reduce::identity_element<BinaryOp>{}());
}

template <typename R>
// requires R InputIterator range
ASTL_NODISCARD auto reduce_balanced(R &&r) -> range_value_type<R>
{
    using Op = std::plus<range_value_type<R>>;
    return i::reduce_balanced(adl::begin(r), adl::end(r), Op{},
                              internal_reduce::identity_element<Op>{}());
}

template <typename R, typename BinaryOp>
// requires R BidirectionalIterator range
// requires BinaryOp BinaryOperation
ASTL_NODISCARD auto reduce_balanced_backward(R &&r, BinaryOp op, range_value_type<R> const &zero)
    -> range_value_type<R>
{
    return i::reduce_balanced_backward(adl::begin(r), adl::end(r), astl::pass_fn(op), zero);
}

template <typename R, typename BinaryOp>
// requires R BidirectionalIterator range
// requires BinaryOp BinaryOperation
ASTL_NODISCARD auto reduce_balanced_backward(R &&r, BinaryOp op)
    -> std::pair<astl::range_value_type<R>, astl::iter_of_range<R>>
{
    return i::reduce_balanced_backward(adl::begin(r), adl::end(r), astl::pass_fn(op),
                                       internal_reduce::identity_element<BinaryOp>{}());
}

template <typename R>
// requires R BidirectionalIterator range
ASTL_NODISCARD auto reduce_balanced_backward(R &&r)
    -> std::pair<astl::range_value_type<R>, astl::iter_of_range<R>>
{
    using Op = std::plus<range_value_type<R>>;
    return i::reduce_balanced_backward(adl::begin(r), adl::end(r), Op{},
                                       internal_reduce::identity_element<Op>{}());
}

template <typename R, typename N, typename BinaryOp>
// requires R InputIterator range
// requires BinaryOp BinaryOperation
ASTL_NODISCARD auto reduce_balanced_n(R &&r, N n, BinaryOp op, range_value_type<R> const &zero)
    -> std::pair<astl::range_value_type<R>, astl::iter_of_range<R>>
{
    return i::reduce_balanced_n(adl::begin(r), n, astl::pass_fn(op), zero);
}

template <typename R, typename N, typename BinaryOp>
// requires R InputIterator range
// requires N integral type
// requires BinaryOp BinaryOperation
ASTL_NODISCARD auto reduce_balanced_n(R &&r, N n, BinaryOp op)
    -> std::pair<astl::range_value_type<R>, astl::iter_of_range<R>>
{
    return i::reduce_balanced_n(adl::begin(r), n, astl::pass_fn(op),
                                internal_reduce::identity_element<BinaryOp>{}());
}

template <typename R, typename N>
// requires R InputIterator range
// requires N integral type
// requires BinaryOp BinaryOperation
ASTL_NODISCARD auto reduce_balanced_n(R &&r, N n)
    -> std::pair<astl::range_value_type<R>, astl::iter_of_range<R>>
{
    using Op = std::plus<range_value_type<R>>;
    return i::reduce_balanced_n(adl::begin(r), n, Op{}, internal_reduce::identity_element<Op>{}());
}
} // namespace r

} // namespace astl

#endif // ASTL_INCLUDE_REDUCE_HPP
