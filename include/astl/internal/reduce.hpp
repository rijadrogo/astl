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

inline constexpr struct {
    template <typename FwdIt, typename BinaryOp>
    // requires FwdIt ForwardIterator
    // requires BinaryOp BinaryOperation
    auto operator()(FwdIt first, FwdIt const last, BinaryOp op, iter_value_type<FwdIt> value,
                    iter_value_type<FwdIt> const &zero) const -> iter_value_type<FwdIt>
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
    auto operator()(FwdIt first, FwdIt last, BinaryOp op, iter_value_type<FwdIt> value) const
        -> iter_value_type<FwdIt>
    {
        return (*this)(first, last, astl::pass_fn(op), std::move(value),
                       internal_reduce::identity_element<BinaryOp>{}());
    }
} add_to_counter{};

inline constexpr struct {
    template <typename BidiIt, typename BinaryOp>
    // requires BidiIt BidirectionalIterator
    // requires BinaryOp BinaryOperation
    auto operator()(BidiIt first, BidiIt last, BinaryOp op, iter_value_type<BidiIt> value,
                    iter_value_type<BidiIt> const &zero) const -> iter_value_type<BidiIt>
    {
        return i::add_to_counter(std::make_reverse_iterator(last),
                                 std::make_reverse_iterator(first), astl::pass_fn(op),
                                 std::move(value), zero);
    }

    template <typename BidiIt, typename BinaryOp>
    // requires BidiIt BidirectionalIterator
    // requires BinaryOp BinaryOperation
    auto operator()(BidiIt first, BidiIt last, BinaryOp op, iter_value_type<BidiIt> value) const
        -> iter_value_type<BidiIt>
    {
        return (*this)(first, last, astl::pass_fn(op), std::move(value),
                       internal_reduce::identity_element<BinaryOp>{}());
    }
} add_to_counter_backward{};

inline constexpr struct {
    template <typename FwdIt, typename N, typename BinaryOp>
    // requires FwdIt ForwardIterator
    // requires N integral type
    // requires BinaryOp BinaryOperation
    auto operator()(FwdIt first, N n, BinaryOp op, iter_value_type<FwdIt> value,
                    iter_value_type<FwdIt> const &zero) const
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
    auto operator()(FwdIt first, N n, BinaryOp op, iter_value_type<FwdIt> value)
        -> std::pair<astl::iter_value_type<FwdIt>, FwdIt>
    {
        return (*this)(first, n, astl::pass_fn(op), std::move(value),
                       internal_reduce::identity_element<BinaryOp>{}());
    }
} add_to_counter_n{};

inline constexpr struct {
    template <typename FwdIt, typename OutIt, typename BinaryOp>
    // requires FwdIt ForwardIterator
    // requires OutIt OutputIterator
    // requires BinaryOp, two arguments value_type(FwdIt)
    auto operator()(FwdIt first, FwdIt last, OutIt dest, BinaryOp op) const -> OutIt
    {
        if (first == last) return dest;

        FwdIt trailer(first);
        while (++first != last) {
            *dest = op(*trailer, *first);
            trailer = first;
            ++dest;
        }
        return dest;
    }

    template <typename FwdIt, typename OutIt, typename BinaryOp, typename P>
    auto operator()(FwdIt first, FwdIt last, OutIt dest, BinaryOp bin_op, P p) const -> OutIt
    {
        return (*this)(first, last, dest, astl::combine(astl::pass_fn(bin_op), astl::pass_fn(p)));
    }
} adjacent_reduce{};

inline constexpr struct {
    template <typename InIt, typename T>
    ASTL_NODISCARD auto operator()(InIt first, InIt const last, T init) const -> T
    {
        while (first != last) {
            init = std::move(init) + *first;
            ++first;
        }
        return init;
    }

    template <typename InIt, typename T, typename BinaryOp>
    ASTL_NODISCARD auto operator()(InIt first, InIt const last, T init, BinaryOp op) const -> T
    {
        while (first != last) {
            init = op(std::move(init), *first);
            ++first;
        }
        return init;
    }

    template <typename InIt, typename T, typename BinaryOp, typename P>
    ASTL_NODISCARD auto operator()(InIt first, InIt const last, T init, BinaryOp op, P p) const -> T
    {
        while (first != last) {
            init = op(std::move(init), invoke(p, *first));
            ++first;
        }
        return init;
    }

    template <typename InIt>
    ASTL_NODISCARD auto operator()(InIt first, InIt const last) const -> iter_value_type<InIt>
    {
        auto init(*first);
        while (++first != last) init = std::move(init) + *first;

        return init;
    }
} reduce{};

inline constexpr struct {
    template <typename InIt>
    ASTL_NODISCARD auto operator()(InIt first, InIt last) const -> iter_value_type<InIt>
    {
        return i::reduce(std::make_reverse_iterator(last), std::make_reverse_iterator(first));
    }

    template <typename InIt, typename T>
    ASTL_NODISCARD auto operator()(InIt first, InIt last, T init) const -> T
    {
        return i::reduce(std::make_reverse_iterator(last), std::make_reverse_iterator(first),
                         std::move(init));
    }

    template <typename InIt, typename T, typename BinaryOp>
    ASTL_NODISCARD auto operator()(InIt first, InIt last, T init, BinaryOp op) const -> T
    {
        return i::reduce(std::make_reverse_iterator(last), std::make_reverse_iterator(first),
                         std::move(init), astl::pass_fn(op));
    }

    template <typename InIt, typename T, typename BinaryOp, typename P>
    ASTL_NODISCARD auto operator()(InIt first, InIt last, T init, BinaryOp op, P p) const -> T
    {
        return (*this)(first, last, std::move(init),
                       astl::combine(astl::pass_fn(op), astl::pass_fn(p)));
    }
} reduce_backward{};

inline constexpr struct {
    template <typename InIt, typename N>
    ASTL_NODISCARD auto operator()(InIt first, N n) const
        -> std::pair<astl::iter_value_type<InIt>, InIt>
    {
        auto init(*first);
        while (--n != N(0)) init = std::move(init) + *++first;

        return std::make_pair(init, first);
    }

    template <typename InIt, typename N, typename T>
    ASTL_NODISCARD auto operator()(InIt first, N n, T init) const -> std::pair<T, InIt>
    {
        while (n != N(0)) {
            init = std::move(init) + *first;
            ++first;
            --n;
        }
        return std::make_pair(init, first);
    }

    template <typename InIt, typename N, typename T, typename BinaryOp>
    ASTL_NODISCARD auto operator()(InIt first, N n, T init, BinaryOp op) const -> std::pair<T, InIt>
    {
        while (n != N(0)) {
            init = op(std::move(init), *first);
            ++first;
            --n;
        }
        return std::make_pair(init, first);
    }

    template <typename InIt, typename N, typename T, typename BinaryOp, typename P>
    ASTL_NODISCARD auto operator()(InIt first, N n, T init, BinaryOp op, P p) const
        -> std::pair<T, InIt>
    {
        while (n != N(0)) {
            init = op(std::move(init), invoke(p, *first));
            ++first;
            --n;
        }
        return std::make_pair(init, first);
    }
} reduce_n{};

inline constexpr struct {
    template <typename InIt, typename BinaryOp>
    // requires InIt InputIterator
    // requires BinaryOp BinaryOperation
    ASTL_NODISCARD auto operator()(InIt first, InIt const last, BinaryOp op,
                                   iter_value_type<InIt> const &zero) const -> iter_value_type<InIt>
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
    ASTL_NODISCARD auto operator()(InIt first, InIt last, BinaryOp op) const
        -> iter_value_type<InIt>
    {
        return (*this)(first, last, astl::pass_fn(op),
                       internal_reduce::identity_element<BinaryOp>{}());
    }

    template <typename InIt>
    // requires InIt InputIterator
    ASTL_NODISCARD auto operator()(InIt first, InIt last) const -> iter_value_type<InIt>
    {
        using Op = std::plus<iter_value_type<InIt>>;
        return (*this)(first, last, Op{}, internal_reduce::identity_element<Op>{}());
    }
} reduce_nonzeros{};

inline constexpr struct {
    template <typename BidiIt, typename BinaryOp>
    // requires BidiIt BidirectionalIterator
    // requires BinaryOp BinaryOperation
    ASTL_NODISCARD auto operator()(BidiIt first, BidiIt last, BinaryOp op,
                                   iter_value_type<BidiIt> const &zero) const
        -> iter_value_type<BidiIt>
    {
        return i::reduce_nonzeros(std::make_reverse_iterator(last),
                                  std::make_reverse_iterator(first), astl::pass_fn(op),
                                  std::move(zero));
    }

    template <typename BidiIt, typename BinaryOp>
    // requires BidiIt BidirectionalIterator
    // requires BinaryOp BinaryOperation
    ASTL_NODISCARD auto operator()(BidiIt first, BidiIt last, BinaryOp op) const
        -> iter_value_type<BidiIt>
    {
        return (*this)(first, last, astl::pass_fn(op),
                       internal_reduce::identity_element<BinaryOp>{}());
    }

    template <typename BidiIt>
    // requires BidiIt BidirectionalIterator
    ASTL_NODISCARD auto operator()(BidiIt first, BidiIt last) const -> iter_value_type<BidiIt>
    {
        using Op = std::plus<iter_value_type<BidiIt>>;
        return (*this)(first, last, Op{}, internal_reduce::identity_element<Op>{}());
    }
} reduce_nonzeros_backward{};

inline constexpr struct {
    template <typename InIt, typename N, typename BinaryOp>
    // requires InIt InputIterator
    // requires N integral type
    // requires BinaryOp BinaryOperation
    ASTL_NODISCARD auto operator()(InIt first, N n, BinaryOp op,
                                   iter_value_type<InIt> const &zero) const
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
    ASTL_NODISCARD auto operator()(InIt first, N n, BinaryOp op) const
        -> std::pair<astl::iter_value_type<InIt>, InIt>
    {
        return (*this)(first, n, astl::pass_fn(op),
                       internal_reduce::identity_element<BinaryOp>{}());
    }

    template <typename InIt, typename N>
    // requires InIt InputIterator
    // requires N integral type
    ASTL_NODISCARD auto operator()(InIt first, N n) const
        -> std::pair<astl::iter_value_type<InIt>, InIt>
    {
        using Op = std::plus<iter_value_type<InIt>>;
        return (*this)(first, n, Op{}, internal_reduce::identity_element<Op>{}());
    }
} reduce_nonzeros_n{};

inline constexpr struct {
    template <typename InIt, typename BinaryOp>
    // requires InIt InputIterator
    // requires BinaryOp BinaryOperation
    ASTL_NODISCARD auto operator()(InIt first, InIt const last, BinaryOp op,
                                   iter_value_type<InIt> const &zero) const -> iter_value_type<InIt>
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
    ASTL_NODISCARD auto operator()(InIt first, InIt last, BinaryOp op) const
        -> iter_value_type<InIt>
    {
        return (*this)(first, last, astl::pass_fn(op),
                       internal_reduce::identity_element<BinaryOp>{}());
    }

    template <typename InIt>
    // requires InIt InputIterator
    ASTL_NODISCARD auto operator()(InIt first, InIt last) const -> iter_value_type<InIt>
    {
        using Op = std::plus<iter_value_type<InIt>>;
        return (*this)(first, last, Op{}, internal_reduce::identity_element<Op>{}());
    }
} reduce_balanced{};

inline constexpr struct {
    template <typename BidiIt, typename BinaryOp>
    // requires BidiIt BidirectionalIterator
    // requires BinaryOp BinaryOperation
    ASTL_NODISCARD auto operator()(BidiIt first, BidiIt last, BinaryOp op,
                                   iter_value_type<BidiIt> const &zero) const
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
    ASTL_NODISCARD auto operator()(BidiIt first, BidiIt last, BinaryOp op) const
        -> iter_value_type<BidiIt>
    {
        return (*this)(first, last, astl::pass_fn(op),
                       internal_reduce::identity_element<BinaryOp>{}());
    }

    template <typename BidiIt>
    // requires BidiIt BidirectionalIterator
    ASTL_NODISCARD auto operator()(BidiIt first, BidiIt last) const -> iter_value_type<BidiIt>
    {
        using Op = std::plus<iter_value_type<BidiIt>>;
        return (*this)(first, last, Op{}, internal_reduce::identity_element<Op>{}());
    }
} reduce_balanced_backward{};

inline constexpr struct {
    template <typename InIt, typename N, typename BinaryOp>
    // requires InIt InputIterator
    // requires N integral type
    // requires BinaryOp BinaryOperation
    ASTL_NODISCARD auto operator()(InIt first, N n, BinaryOp op,
                                   iter_value_type<InIt> const &zero) const
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
    ASTL_NODISCARD auto operator()(InIt first, N n, BinaryOp op) const
        -> std::pair<astl::iter_value_type<InIt>, InIt>
    {
        return (*this)(first, n, astl::pass_fn(op),
                       internal_reduce::identity_element<BinaryOp>{}());
    }

    template <typename InIt, typename N>
    // requires InIt InputIterator
    // requires N integral type
    // requires BinaryOp BinaryOperation
    ASTL_NODISCARD auto operator()(InIt first, N n) const
        -> std::pair<astl::iter_value_type<InIt>, InIt>
    {
        using Op = std::plus<iter_value_type<InIt>>;
        return (*this)(first, n, Op{}, internal_reduce::identity_element<Op>{}());
    }
} reduce_balanced_n{};

} // namespace i

namespace r
{

inline constexpr struct {
    template <typename R, typename BinaryOp>
    // requires R ForwardIterator range
    // requires BinaryOp BinaryOperation
    auto operator()(R &&r, BinaryOp op, range_value_type<R> value,
                    range_value_type<R> const &zero) const -> range_value_type<R>
    {
        return i::add_to_counter(adl::begin(r), adl::end(r), astl::pass_fn(op), std::move(value),
                                 zero);
    }

    template <typename R, typename BinaryOp>
    // requires R ForwardIterator range
    // requires BinaryOp BinaryOperation
    auto operator()(R &&r, BinaryOp op, range_value_type<R> value) const -> range_value_type<R>
    {
        return (*this)(r, astl::pass_fn(op), std::move(value),
                       internal_reduce::identity_element<BinaryOp>{}());
    }
} add_to_counter{};

inline constexpr struct {
    template <typename R, typename BinaryOp>
    // requires R BidirectionalIterator range
    // requires BinaryOp BinaryOperation
    auto operator()(R &&r, BinaryOp op, range_value_type<R> value,
                    range_value_type<R> const &zero) const -> range_value_type<R>
    {
        return i::add_to_counter_backward(adl::begin(r), adl::end(r), astl::pass_fn(op),
                                          std::move(value), zero);
    }

    template <typename R, typename BinaryOp>
    // requires R BidirectionalIterator range
    // requires BinaryOp BinaryOperation
    auto operator()(R &&r, BinaryOp op, range_value_type<R> value) const -> range_value_type<R>
    {
        return (*this)(r, astl::pass_fn(op), std::move(value),
                       internal_reduce::identity_element<BinaryOp>{}());
    }
} add_to_counter_backward{};

inline constexpr struct {
    template <typename R, typename N, typename BinaryOp>
    // requires R ForwardIterator range
    // requires N integral type
    // requires BinaryOp BinaryOperation
    auto operator()(R &&r, N n, BinaryOp op, range_value_type<R> value,
                    range_value_type<R> const &zero) const
        -> std::pair<astl::range_value_type<R>, astl::iter_of_range<R>>
    {
        return i::add_to_counter_n(adl::begin(r), n, astl::pass_fn(op), std::move(value), zero);
    }

    template <typename R, typename N, typename BinaryOp>
    // requires R ForwardIterator range
    // requires N integral type
    // requires BinaryOp BinaryOperation
    auto operator()(R &&r, N n, BinaryOp op, range_value_type<R> value) const
        -> std::pair<astl::range_value_type<R>, astl::iter_of_range<R>>
    {
        return i::add_to_counter_n(adl::begin(r), n, astl::pass_fn(op), std::move(value),
                                   internal_reduce::identity_element<BinaryOp>{}());
    }
} add_to_counter_n{};

inline constexpr struct {
    // NOTE for InputIterators this is gonna copy each element
    template <typename R, typename OutIt, typename BinaryOp>
    // requires R ForwardIterator range
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

} adjacent_reduce{};

inline constexpr struct {
    template <typename R> ASTL_NODISCARD auto operator()(R &&r) const -> range_value_type<R>
    {
        return i::reduce(adl::begin(r), adl::end(r));
    }

    template <typename R, typename T> ASTL_NODISCARD auto operator()(R &&r, T init) const -> T
    {
        return i::reduce(adl::begin(r), adl::end(r), std::move(init));
    }

    template <typename R, typename T, typename BinaryOp>
    ASTL_NODISCARD auto operator()(R &&r, T init, BinaryOp op) const -> T
    {
        return i::reduce(adl::begin(r), adl::end(r), std::move(init), astl::pass_fn(op));
    }

    template <typename R, typename T, typename BinaryOp, typename P>
    ASTL_NODISCARD auto operator()(R &&r, T init, BinaryOp op, P p) const -> T
    {
        return i::reduce(adl::begin(r), adl::end(r), std::move(init), astl::pass_fn(op),
                         astl::pass_fn(p));
    }
} reduce{};

inline constexpr struct {
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

    template <typename R> ASTL_NODISCARD auto operator()(R &&r) const -> range_value_type<R>
    {
        return i::reduce_backward(adl::begin(r), adl::end(r));
    }

    template <typename R, typename T> ASTL_NODISCARD auto operator()(R &&r, T init) const -> T
    {
        return i::reduce_backward(adl::begin(r), adl::end(r), std::move(init));
    }
} reduce_backward{};

inline constexpr struct {

    template <typename R, typename N>
    ASTL_NODISCARD auto operator()(R &&r, N n) const
        -> std::pair<astl::range_value_type<R>, astl::iter_of_range<R>>
    {
        return i::reduce_n(adl::begin(r), n);
    }

    template <typename R, typename N, typename T>
    ASTL_NODISCARD auto operator()(R &&r, N n, T init) const -> std::pair<T, astl::iter_of_range<R>>
    {
        return i::reduce_n(adl::begin(r), n, std::move(init));
    }

    template <typename R, typename N, typename T, typename BinaryOp>
    ASTL_NODISCARD auto operator()(R &&r, N n, T init, BinaryOp op) const
        -> std::pair<T, astl::iter_of_range<R>>
    {
        return i::reduce_n(adl::begin(r), n, std::move(init), astl::pass_fn(op));
    }

    template <typename R, typename N, typename T, typename BinaryOp, typename P>
    ASTL_NODISCARD auto operator()(R &&r, N n, T init, BinaryOp op, P p) const
        -> std::pair<T, astl::iter_of_range<R>>
    {
        return i::reduce_n(adl::begin(r), n, std::move(init), astl::pass_fn(op), astl::pass_fn(p));
    }

} reduce_n{};

inline constexpr struct {

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
    ASTL_NODISCARD auto operator()(R &&r, BinaryOp op) const -> range_value_type<R>
    {
        return i::reduce_nonzeros(adl::begin(r), adl::end(r), astl::pass_fn(op),
                                  internal_reduce::identity_element<BinaryOp>{}());
    }

    template <typename R>
    // requires R InputIterator range
    ASTL_NODISCARD auto operator()(R &&r) const -> range_value_type<R>
    {
        using Op = std::plus<range_value_type<R>>;
        return i::reduce_nonzeros(adl::begin(r), adl::end(r), Op{},
                                  internal_reduce::identity_element<Op>{}());
    }

} reduce_nonzeros{};

inline constexpr struct {

    template <typename R, typename BinaryOp>
    // requires R BidirectionalIterator range
    // requires BinaryOp BinaryOperation
    ASTL_NODISCARD auto
    operator()(R &&r, BinaryOp op = BinaryOp{},
               range_value_type<R> zero = internal_reduce::identity_element<BinaryOp>{}()) const
        -> range_value_type<R>
    {
        return i::reduce_nonzeros_backward(adl::begin(r), adl::end(r), astl::pass_fn(op), zero);
    }

    template <typename R, typename BinaryOp>
    // requires R BidirectionalIterator range
    // requires BinaryOp BinaryOperation
    ASTL_NODISCARD auto operator()(R &&r, BinaryOp op) const -> range_value_type<R>
    {
        return i::reduce_nonzeros_backward(adl::begin(r), adl::end(r), astl::pass_fn(op),
                                           internal_reduce::identity_element<BinaryOp>{}());
    }

    template <typename R>
    // requires R InputIterator range
    ASTL_NODISCARD auto operator()(R &&r) const -> range_value_type<R>
    {
        using Op = std::plus<range_value_type<R>>;
        return i::reduce_nonzeros_backward(adl::begin(r), adl::end(r), Op{},
                                           internal_reduce::identity_element<Op>{}());
    }

} reduce_nonzeros_backward{};

inline constexpr struct {

    template <typename R, typename N, typename BinaryOp>
    // requires R InputIterator range
    // requires N integral type
    // requires BinaryOp BinaryOperation
    ASTL_NODISCARD auto operator()(R &&r, N n, BinaryOp op, range_value_type<R> const &zero) const
        -> std::pair<astl::range_value_type<R>, astl::iter_of_range<R>>
    {
        return i::reduce_nonzeros_n(adl::begin(r), n, astl::pass_fn(op), zero);
    }

    template <typename R, typename N, typename BinaryOp>
    // requires R InputIterator range
    // requires N integral type
    // requires BinaryOp BinaryOperation
    ASTL_NODISCARD auto operator()(R &&r, N n, BinaryOp op) const
        -> std::pair<astl::range_value_type<R>, astl::iter_of_range<R>>
    {
        return i::reduce_nonzeros_n(adl::begin(r), n, astl::pass_fn(op),
                                    internal_reduce::identity_element<BinaryOp>{}());
    }

    template <typename R, typename N>
    // requires R InputIterator range
    // requires N integral type
    // requires BinaryOp BinaryOperation
    ASTL_NODISCARD auto operator()(R &&r, N n) const
        -> std::pair<astl::range_value_type<R>, astl::iter_of_range<R>>
    {
        using Op = std::plus<range_value_type<R>>;
        return i::reduce_nonzeros_n(adl::begin(r), n, Op{},
                                    internal_reduce::identity_element<Op>{}());
    }

} reduce_nonzeros_n{};

inline constexpr struct {

    template <typename R, typename BinaryOp>
    // requires R InputIterator range
    // requires BinaryOp BinaryOperation
    ASTL_NODISCARD auto operator()(R &&r, BinaryOp op, range_value_type<R> const &zero) const
        -> range_value_type<R>
    {
        return i::reduce_balanced(adl::begin(r), adl::end(r), astl::pass_fn(op), zero);
    }

    template <typename R, typename BinaryOp>
    // requires R InputIterator range
    // requires BinaryOp BinaryOperation
    ASTL_NODISCARD auto operator()(R &&r, BinaryOp op) const -> range_value_type<R>
    {
        return i::reduce_balanced(adl::begin(r), adl::end(r), astl::pass_fn(op),
                                  internal_reduce::identity_element<BinaryOp>{}());
    }

    template <typename R>
    // requires R InputIterator range
    ASTL_NODISCARD auto operator()(R &&r) const -> range_value_type<R>
    {
        using Op = std::plus<range_value_type<R>>;
        return i::reduce_balanced(adl::begin(r), adl::end(r), Op{},
                                  internal_reduce::identity_element<Op>{}());
    }

} reduce_balanced{};

inline constexpr struct {

    template <typename R, typename BinaryOp>
    // requires R BidirectionalIterator range
    // requires BinaryOp BinaryOperation
    ASTL_NODISCARD auto operator()(R &&r, BinaryOp op, range_value_type<R> const &zero) const
        -> range_value_type<R>
    {
        return i::reduce_balanced_backward(adl::begin(r), adl::end(r), astl::pass_fn(op), zero);
    }

    template <typename R, typename BinaryOp>
    // requires R BidirectionalIterator range
    // requires BinaryOp BinaryOperation
    ASTL_NODISCARD auto operator()(R &&r, BinaryOp op) const
        -> std::pair<astl::range_value_type<R>, astl::iter_of_range<R>>
    {
        return i::reduce_balanced_backward(adl::begin(r), adl::end(r), astl::pass_fn(op),
                                           internal_reduce::identity_element<BinaryOp>{}());
    }

    template <typename R>
    // requires R BidirectionalIterator range
    ASTL_NODISCARD auto operator()(R &&r) const
        -> std::pair<astl::range_value_type<R>, astl::iter_of_range<R>>
    {
        using Op = std::plus<range_value_type<R>>;
        return i::reduce_balanced_backward(adl::begin(r), adl::end(r), Op{},
                                           internal_reduce::identity_element<Op>{}());
    }

} reduce_balanced_backward{};

inline constexpr struct {

    template <typename R, typename N, typename BinaryOp>
    // requires R InputIterator range
    // requires BinaryOp BinaryOperation
    ASTL_NODISCARD auto operator()(R &&r, N n, BinaryOp op, range_value_type<R> const &zero) const
        -> std::pair<astl::range_value_type<R>, astl::iter_of_range<R>>
    {
        return i::reduce_balanced_n(adl::begin(r), n, astl::pass_fn(op), zero);
    }

    template <typename R, typename N, typename BinaryOp>
    // requires R InputIterator range
    // requires N integral type
    // requires BinaryOp BinaryOperation
    ASTL_NODISCARD auto operator()(R &&r, N n, BinaryOp op) const
        -> std::pair<astl::range_value_type<R>, astl::iter_of_range<R>>
    {
        return i::reduce_balanced_n(adl::begin(r), n, astl::pass_fn(op),
                                    internal_reduce::identity_element<BinaryOp>{}());
    }

    template <typename R, typename N>
    // requires R InputIterator range
    // requires N integral type
    // requires BinaryOp BinaryOperation
    ASTL_NODISCARD auto operator()(R &&r, N n) const
        -> std::pair<astl::range_value_type<R>, astl::iter_of_range<R>>
    {
        using Op = std::plus<range_value_type<R>>;
        return i::reduce_balanced_n(adl::begin(r), n, Op{},
                                    internal_reduce::identity_element<Op>{}());
    }
} reduce_balanced_n{};

} // namespace r

} // namespace astl

#endif // ASTL_INCLUDE_REDUCE_HPP
