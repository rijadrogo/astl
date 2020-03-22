//
// Created by Rijad on 04-Aug-18.
//

#ifndef ASTL_INCLUDE_TRANSFORM_HPP
#define ASTL_INCLUDE_TRANSFORM_HPP

#include <algorithm>
#include <type_traits>
#include <utility>

#include "astl/functional.hpp"
#include "astl/iterator.hpp"
#include "astl/map_iterator.hpp"
#include "astl/range_access.hpp"

namespace astl
{
namespace internal_transform
{
template <typename Type, template <class...> class Template>
constexpr bool is_specialization_v = false;

template <template <class...> class Template, typename... Types>
constexpr bool is_specialization_v<Template<Types...>, Template> = true;

template <typename InIt1, typename InIt2, typename T, typename BinOp1, typename BinOp2>
constexpr bool default_ops_transform_reduce_v = std::is_arithmetic<T>::value
    &&std::is_arithmetic<typename std::remove_pointer<InIt1>::type>::value
        &&std::is_arithmetic<typename std::remove_pointer<InIt2>::type>::value
            &&is_specialization_v<BinOp1, std::plus> &&is_specialization_v<BinOp2, std::multiplies>;

template <int N, typename FwdIt, typename T, typename BinOp, typename NaryOp, typename Size>
auto transform_reduce_each_n1(FwdIt first, T &val, BinOp reduce_op, NaryOp transform_op, Size d)
    -> std::pair<T, FwdIt>
{
    std::make_integer_sequence<int, N> seq{};
    while (d >= N) {
        val = reduce_op(std::move(val), internal::for_each1(transform_op, first, seq));
        std::advance(first, N);
        d -= N;
    }
    return std::move(val);
}

template <int N, typename FwdIt, typename T, typename BinOp, typename NaryOp, typename Size,
          typename P>
auto transform_reduce_each_n1(FwdIt first, T &val, BinOp reduce_op, NaryOp transform_op, Size d,
                              P p) -> std::pair<T, FwdIt>
{
    std::make_integer_sequence<int, N> seq{};
    while (d >= N) {
        val = reduce_op(std::move(val), internal::for_each1(transform_op, first, p, seq));
        std::advance(first, N);
        d -= N;
    }
    return std::make_pair(std::move(val), first);
}

template <int N, typename FwdIt, typename OutIt, typename NaryOp, typename Size>
auto transform_each_n1(FwdIt first, OutIt dest, NaryOp fn, Size d) -> std::pair<OutIt, FwdIt>
{
    std::make_integer_sequence<int, N> seq{};
    while (d >= N) {
        *dest = internal::for_each1(fn, first, seq);
        std::advance(first, N);
        d -= N;
        ++dest;
    }
    return std::make_pair(dest, first);
}

template <int N, typename FwdIt, typename OutIt, typename NaryOp, typename Size, typename P>
auto transform_each_n1(FwdIt first, OutIt dest, NaryOp fn, Size d, P p) -> std::pair<OutIt, FwdIt>
{
    std::make_integer_sequence<int, N> seq{};
    while (d >= N) {
        *dest = internal::for_each1(fn, first, p, seq);
        std::advance(first, N);
        d -= N;
        ++dest;
    }
    return std::make_pair(dest, first);
}
} // namespace internal_transform

namespace i
{
template <typename BidIt, typename OutIt, typename BinaryOp, typename A = identity>
// requires BidIt Bidirectional Iterator
// requires OutIt Output Iterator
// requires BinaryOp Binary Function on
// iter_value_type<BidIt>,iter_value_type<BidIt> requires A Function on
// iter_value_type<BidIt>
auto bidirectional_transform(BidIt first, BidIt last, OutIt dest, BinaryOp op, A a = A{})
    -> std::pair<OutIt, bool>
{
    while (first != last) {
        --last;
        if (first == last) {
            a(*first);
            return std::make_pair(dest, true);
        }
        *dest = op(*first, *last);
        ++dest;
        ++first;
    }
    return std::make_pair(dest, false);
}

template <typename BidIt, typename OutIt, typename BinaryOp, typename A, typename P>
auto bidirectional_transform(BidIt first, BidIt last, OutIt dest, BinaryOp op, A a, P p)
    -> std::pair<OutIt, bool>
{
    return i::bidirectional_transform(first, last, dest,
                                      astl::combine(astl::pass_fn(op), astl::pass_fn(p)),
                                      astl::combine(astl::pass_fn(a), astl::pass_fn(p)));
}

using std::transform; // NOLINT(misc-unused-using-decls)
template <typename InIt, typename OutIt, typename UnaryOp, typename P>
auto transform(InIt first, InIt last, OutIt dest, UnaryOp unary_op, P p) -> OutIt
{
    return std::transform(first, last, dest,
                          astl::combine(astl::pass_fn(unary_op), astl::pass_fn(p)));
}

template <typename InputIt1, typename InIt2, typename OutIt, typename BinaryOp, typename P1,
          typename P2>
auto transform(InputIt1 first1, InputIt1 last1, InIt2 first2, OutIt dest, BinaryOp binary_op, P1 p1,
               P2 p2) -> OutIt
{
    while (first1 != last1) {
        *dest = binary_op(invoke(p1, *first1), invoke(p2, *first2));
        ++first1;
        ++first2;
        ++dest;
    }
    return dest;
}

template <typename FwdIt, typename OutIt, typename BinaryOp>
// requires FwdIt ForwardIterator
// requires OutIt OutputIterator
// requires BinaryOp, two arguments value_type(FwdIt)
auto transform_adjacent(FwdIt first, FwdIt last, OutIt dest, BinaryOp op) -> OutIt
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
auto transform_adjacent(FwdIt first, FwdIt last, OutIt dest, BinaryOp op, P p) -> OutIt
{
    return i::transform_adjacent(first, last, dest,
                                 astl::combine(astl::pass_fn(op), astl::pass_fn(p)));
}

// Size of dest range should be at least = n * (n - 1) / 2; n = last - first
template <typename FwdIt, typename OutIt, typename BinaryOp>
// requires FwdIt ForwardIterator
// requires OutIt OutputIterator
// requires BinaryOp, returns value_type(dest), two arguments value_type(FwdIt)
auto transform_all_pairs(FwdIt first, FwdIt const last, OutIt dest, BinaryOp binary_op) -> OutIt
{
    if (first == last) return dest;

    FwdIt trailer(first);
    ++first;
    while (first != last) {
        FwdIt i(first);
        while (i != last) {
            *dest = binary_op(*trailer, *i);
            ++i;
            ++dest;
        }
        ++first;
        ++trailer;
    }

    return dest;
}

// Size of dest range should be at least = n * (n - 1) / 2; n = last - first
template <typename FwdIt, typename OutIt, typename BinaryOp, typename P>
auto transform_all_pairs(FwdIt first, FwdIt last, OutIt dest, BinaryOp binary_op, P p) -> OutIt
{
    return i::transform_all_pairs(first, last, dest,
                                  astl::combine(astl::pass_fn(binary_op), astl::pass_fn(p)));
}

// Size of dest range should be at least = n * (n - 1) / 2;
template <typename FwdIt, typename N, typename OutIt, typename BinaryOp>
// requires FwdIt ForwardIterator
// requires N integral type
// requires OutIt OutputIterator
// requires BinaryOp, returns value_type(dest), two arguments value_type(FwdIt)
auto transform_all_pairs_n(FwdIt first, N n, OutIt dest, BinaryOp binary_op)
    -> std::pair<OutIt, FwdIt>
{
    if (n != N(0)) {
        FwdIt trailer(first);
        ++first;
        while (--n != N(0)) {
            FwdIt it(first);
            N n1(n);
            while (n1 != N(0)) {
                *dest = binary_op(*trailer, *it);
                ++it;
                ++dest;
                --n1;
            }
            ++first;
            ++trailer;
        }
    }
    return std::make_pair(dest, first);
}

// Size of output range should be at least = n * (n - 1) / 2
template <typename FwdIt, typename N, typename OutIt, typename BinaryOp, typename P>
auto transform_all_pairs_n(FwdIt first, N n, OutIt dest, BinaryOp binary_op, P p)
    -> std::pair<OutIt, FwdIt>
{
    return i::transform_all_pairs_n(first, n, dest,
                                    astl::combine(astl::pass_fn(binary_op), astl::pass_fn(p)));
}

template <int N = 1, typename FwdIt, typename OutIt, typename NaryOp>
auto transform_each_n(FwdIt first, FwdIt last, OutIt dest, NaryOp fn) ->
    typename std::enable_if<(N >= 1), std::pair<OutIt, FwdIt>>::type
{
    return internal_transform::transform_each_n1<N>(first, dest, pass_fn(fn),
                                                    astl::distance(first, last));
}

template <int N = 1, typename FwdIt, typename OutIt, typename NaryOp, typename P>
auto transform_each_n(FwdIt first, FwdIt last, OutIt dest, NaryOp fn, P p) ->
    typename std::enable_if<(N >= 1), std::pair<OutIt, FwdIt>>::type
{
    return internal_transform::transform_each_n1<N>(first, dest, pass_fn(fn),
                                                    astl::distance(first, last), astl::pass_fn(p));
}

template <typename InIt, typename N, typename OutIt, typename UnaryOp>
// requires InIt InputIterator
// requires N integral type
// requires OutIt OutputIterator
// requires UnaryOp, returns value_type(dest), argument value_type(FwdIt)
auto transform_n(InIt first, N n, OutIt dest, UnaryOp unary_op) -> std::pair<OutIt, InIt>
{
    while (n != N(0)) {
        *dest = unary_op(*first);
        ++dest;
        ++first;
        --n;
    }
    return std::make_pair(dest, first);
}

template <typename InIt, typename N, typename OutIt, typename UnaryOp, typename P>
auto transform_n(InIt first, N n, OutIt dest, UnaryOp unary_op, P p) -> std::pair<OutIt, InIt>
{
    return i::transform_n(first, n, dest, astl::combine(astl::pass_fn(unary_op), astl::pass_fn(p)));
}

template <typename InIt, typename OutIt, typename T, typename BinOp, typename UnaryOp>
auto transform_exclusive_scan(InIt first, InIt const last, OutIt dest, T val, BinOp reduce_op,
                              UnaryOp transform_op) -> OutIt
{
    // set each value init [dest, dest + (last - first)) to the associative
    // reduction of transformed predecessors
    if (first == last) return dest;

    while (true) {
        T tmp(reduce_op(val, transform_op(*first)));
        *dest = std::move(val);
        ++dest;
        ++first;
        if (first == last) break;

        val = std::move(tmp); // Requirement missing from N4713
    }
    return dest;
}

template <typename InIt, typename OutIt, typename T, typename BinOp, typename UnaryOp, typename P>
auto transform_exclusive_scan(InIt first, InIt last, OutIt dest, T val, BinOp reduce_op,
                              UnaryOp transform_op, P p) -> OutIt
{
    auto fp(astl::pass_fn(p));
    return i::transform_exclusive_scan(astl::map_iterator(first, fp), astl::map_iterator(last, fp),
                                       dest, std::move(val), astl::pass_fn(reduce_op),
                                       astl::pass_fn(transform_op));
}

template <typename InIt, typename N, typename OutIt, typename T, typename BinOp, typename UnaryOp>
auto transform_exclusive_scan_n(InIt first, N n, OutIt dest, T val, BinOp reduce_op,
                                UnaryOp transform_op) -> std::pair<OutIt, InIt>
{
    // set each value init [dest, dest + n) to the associative reduction of
    // transformed predecessors
    if (n != N(0)) {
        while (true) {
            T tmp(reduce_op(val, transform_op(*first)));
            *dest = std::move(val);
            ++dest;
            ++first;
            if (--n != N(0)) break;

            val = std::move(tmp);
        }
    }
    return std::make_pair(dest, first);
}

template <typename InIt, typename N, typename OutIt, typename T, typename BinOp, typename UnaryOp,
          typename P>
auto transform_exclusive_scan_n(InIt first, N n, OutIt dest, T val, BinOp reduce_op,
                                UnaryOp transform_op, P p) -> std::pair<OutIt, InIt>
{
    auto pair(i::transform_exclusive_scan_n(astl::map_iterator(first, astl::pass_fn(p)), n, dest,
                                            std::move(val), astl::pass_fn(reduce_op),
                                            astl::pass_fn(transform_op)));
    return std::make_pair(pair.first, pair.second.base());
}

template <typename InIt, typename OutIt, typename UnaryOp, typename UnaryPredicate>
// requires InIt InputIterator
// requires OutIt OutputIterator
// requires UnaryOp, returns value_type(OutIt), argument value_type(InIt)
// requires UnaryPredicate, returns bool, argument value_type(InIt)
auto transform_if(InIt first, InIt const last, OutIt dest, UnaryOp op, UnaryPredicate pred) -> OutIt
{
    while (first != last) {
        if (pred(*first)) {
            *dest = op(*first);
            ++dest;
        }
        ++first;
    }
    return dest;
}

template <typename InIt, typename OutIt, typename UnaryOp, typename UnaryPredicate, typename Pop,
          typename Pup>
auto transform_if(InIt first, InIt last, OutIt dest, UnaryOp op, UnaryPredicate pred, Pop p1,
                  Pup p2) -> OutIt
{
    return i::transform_if(first, last, dest, astl::combine(astl::pass_fn(op), astl::pass_fn(p1)),
                           astl::combine(astl::pass_fn(pred), astl::pass_fn(p2)));
}

template <typename InIt1, typename InIt2, typename OutIt, typename BinaryOp,
          typename BinaryPredicate>
// requires InIt InputIterator
// requires OutIt OutputIterator
// requires UnaryOp, returns value_type(OutIt), two arguments value_type(InIt)
// requires UnaryPredicate, returns bool, two arguments value_type(InIt)
auto transform_if(InIt1 first1, InIt1 last1, InIt2 first2, OutIt dest, BinaryOp op,
                  BinaryPredicate pred) -> OutIt
{
    while (first1 != last1) {
        if (pred(*first1, *first2)) {
            *dest = op(*first1, *first2);
            ++dest;
        }
        ++first1;
    }
    return dest;
}

template <typename FwdIt1, typename FwdIt2, typename OutIt, typename BinaryOp,
          typename BinaryPredicate, typename Pbo, typename Pbu>
auto transform_if(FwdIt1 first1, FwdIt1 last1, FwdIt2 first2, OutIt dest, BinaryOp op,
                  BinaryPredicate pred, Pbo p1, Pbu p2) -> OutIt
{
    return i::transform_if(first1, last1, first2, dest,
                           astl::combine(astl::pass_fn(op), astl::pass_fn(p1)),
                           astl::combine(astl::pass_fn(pred), astl::pass_fn(p2)));
}

template <typename InIt, typename N, typename OutIt, typename UnaryOp, typename UnaryPredicate>
// requires InIt InputIterator
// requires N integral type
// requires OutIt OutputIterator
// requires UnaryOp, returns value_type(OutIt), argument value_type(InIt)
// requires UnaryPredicate, returns bool, argument value_type(InIt)
auto transform_if_n(InIt first, N n, OutIt dest, UnaryOp op, UnaryPredicate pred)
    -> std::pair<OutIt, InIt>
{
    while (n != N(0)) {
        if (pred(*first)) {
            *dest = op(*first);
            ++dest;
        }
        ++first;
    }
    return std::make_pair(dest, first);
}

template <typename InIt, typename N, typename OutIt, typename UnaryOp, typename UnaryPredicate,
          typename Pop, typename Pup>
auto transform_if_n(InIt first, N n, OutIt dest, UnaryOp op, UnaryPredicate pred, Pop p1, Pup p2)
    -> std::pair<OutIt, InIt>
{
    return i::transform_if_n(first, n, dest, astl::combine(astl::pass_fn(op), astl::pass_fn(p1)),
                             astl::combine(astl::pass_fn(pred), astl::pass_fn(p2)));
}

template <typename InIt, typename OutIt, typename T, typename BinOp, typename UnaryOp>
auto transform_inclusive_scan(InIt first, InIt const last, OutIt dest, BinOp reduce_op,
                              UnaryOp transform_op, T val) -> OutIt
{
    while (first != last) {
        val = reduce_op(std::move(val), transform_op(*first));
        *dest = val;
        ++first;
        ++dest;
    }
    return dest;
}

template <typename InIt, typename OutIt, typename T, typename BinOp, typename UnaryOp, typename P>
auto transform_inclusive_scan(InIt first, InIt last, OutIt dest, BinOp reduce_op,
                              UnaryOp transform_op, T val, P p) -> OutIt
{
    auto fp(astl::pass_fn(p));
    return i::transform_exclusive_scan(astl::map_iterator(first, fp), astl::map_iterator(last, fp),
                                       dest, val, astl::pass_fn(reduce_op),
                                       astl::pass_fn(transform_op), std::move(val));
}

template <typename InIt, typename N, typename OutIt, typename T, typename BinOp, typename UnaryOp>
auto transform_inclusive_scan_n(InIt first, N n, OutIt dest, BinOp reduce_op, UnaryOp transform_op,
                                T val) -> std::pair<OutIt, InIt>
{
    // compute partial non commutative and associative transformed reductions
    // including val into dest
    while (n != N(0)) {
        val = reduce_op(std::move(val), transform_op(*first));
        *dest = val;
        ++first;
        ++dest;
        --n;
    }
    return std::make_pair(dest, first);
}

template <typename InIt, typename N, typename OutIt, typename T, typename BinOp, typename UnaryOp,
          typename P>
auto transform_inclusive_scan_n(InIt first, N n, OutIt dest, BinOp reduce_op, UnaryOp transform_op,
                                T val, P p) -> std::pair<OutIt, InIt>
{
    auto pair(i::transform_exclusive_scan_n(astl::map_iterator(first, astl::pass_fn(p)), n, dest,
                                            val, astl::pass_fn(reduce_op),
                                            astl::pass_fn(transform_op), std::move(val)));
    return std::make_pair(pair.first, pair.second.base());
}

template <typename InIt1, typename InIt2, typename T, typename BinOp1, typename BinOp2>
ASTL_NODISCARD auto transform_reduce(InIt1 first1, InIt1 last1, InIt2 first2, T init,
                                     BinOp1 reduce_op, BinOp2 transform_op) -> T
{
    if constexpr (internal_transform::default_ops_transform_reduce_v<InIt1, InIt2, T, BinOp1,
                                                                     BinOp2>) {
        // return transform-reduction, default ops on contiguous arithmetic ranges case
        while (first1 != last1) {
            init += *first1 * *first2;
            ++first2;
            ++first1;
        }
        return init;
    }
    else {
        // return non commutative and non associative transform-reduction of sequences, general case
        while (first1 != last1) {
            init = reduce_op(std::move(init), transform_op(*first1, *first2));
            ++first1;
            ++first2;
        }
        return init;
    }
}

template <typename InIt1, typename InIt2, typename T>
ASTL_NODISCARD auto transform_reduce(InIt1 first1, InIt1 last1, InIt2 first2, T init) -> T
{
    return i::transform_reduce(first1, last1, first2, std::move(init), std::plus{},
                               std::multiplies{});
}

template <typename InIt, typename T, typename BinOp, typename UnaryOp>
ASTL_NODISCARD auto transform_reduce(InIt first, InIt last, T init, BinOp reduce_op,
                                     UnaryOp transform_op) -> T
{
    while (first != last) {
        init = reduce_op(std::move(init), transform_op(*first));
        ++first;
    }
    return init;
}

template <int N = 1, typename FwdIt, typename T, typename BinOp, typename NaryOp>
ASTL_NODISCARD auto transform_reduce_each_n(FwdIt first, FwdIt last, T val, BinOp reduce_op,
                                            NaryOp transform_op) ->
    typename std::enable_if<(N >= 1), std::pair<T, FwdIt>>::type
{
    return internal_transform::transform_reduce_each_n1<N>(
        first, std::move(val), astl::pass_fn(reduce_op), astl::pass_fn(transform_op),
        astl::distance(first, last));
}

template <int N = 1, typename FwdIt, typename T, typename BinOp, typename NaryOp, typename P>
ASTL_NODISCARD auto transform_reduce_each_n(FwdIt first, FwdIt last, T val, BinOp reduce_op,
                                            NaryOp transform_op, P p) ->
    typename std::enable_if<(N >= 1), std::pair<T, FwdIt>>::type
{
    return internal_transform::transform_reduce_each_n1<N>(
        first, std::move(val), astl::pass_fn(reduce_op), astl::pass_fn(transform_op),
        astl::distance(first, last), astl::pass_fn(p));
}

template <typename InIt1, typename N, typename InIt2, typename T, typename BinOp1, typename BinOp2>
ASTL_NODISCARD auto transform_reduce_n(InIt1 first1, N n, InIt2 first2, T init, BinOp1 reduce_op,
                                       BinOp2 transform_op) -> T
{
    if constexpr (internal_transform::default_ops_transform_reduce_v<InIt1, InIt2, T, BinOp1,
                                                                     BinOp2>) {
        // return transform-reduction, default ops on contiguous arithmetic ranges case
        while (n != N(0)) {
            init += *first1 * *first2;
            ++first2;
            ++first1;
            --n;
        }
        return init;
    }
    else {
        // return non commutative and non associative transform-reduction of sequences, general case
        while (n != N(0)) {
            init = reduce_op(std::move(init), transform_op(*first1, *first2));
            ++first1;
            ++first2;
            --n;
        }
        return init;
    }
}

template <typename InIt1, typename N, typename InIt2, typename T>
ASTL_NODISCARD auto transform_reduce_n(InIt1 first1, N n, InIt2 first2, T init) -> T
{
    return i::transform_reduce_n(first1, n, first2, std::move(init), std::plus{},
                                 std::multiplies{});
}

template <typename InIt, typename N, typename T, typename BinOp, typename UnaryOp>
ASTL_NODISCARD auto transform_reduce_n(InIt first, N n, T init, BinOp reduce_op,
                                       UnaryOp transform_op) -> T
{
    while (n != N(0)) {
        init = reduce_op(std::move(init), transform_op(*first));
        ++first;
        --n;
    }
    return init;
}

template <typename InIt, typename OutIt, typename UnaryOp, typename UnaryPredicate>
auto transform_while(InIt first, InIt const last, OutIt dest, UnaryOp op, UnaryPredicate pred)
    -> std::pair<OutIt, InIt>
{
    while (first != last && pred(*first)) {
        *dest = op(*first);
        ++first;
        ++dest;
    }
    return std::make_pair(dest, first);
}

template <typename InIt, typename OutIt, typename UnaryOp, typename UnaryPredicate, typename Pop,
          typename Pup>
auto transform_while(InIt first, InIt last, OutIt dest, UnaryOp op, UnaryPredicate pred, Pop p1,
                     Pup p2) -> std::pair<OutIt, InIt>
{
    return i::transform_while(first, last, dest,
                              astl::combine(astl::pass_fn(op), astl::pass_fn(p1)),
                              astl::combine(astl::pass_fn(pred), astl::pass_fn(p2)));
}

template <typename InIt, typename N, typename OutIt, typename UnaryOp, typename UnaryPredicate>
auto transform_while_n(InIt first, N n, OutIt dest, UnaryOp op, UnaryPredicate pred)
    -> std::pair<OutIt, InIt>
{
    while (n != N(0) && pred(*first)) {
        *dest = op(*first);
        ++first;
        ++dest;
    }
    return std::make_pair(dest, first);
}

template <typename InIt, typename N, typename OutIt, typename UnaryOp, typename UnaryPredicate,
          typename Pop, typename Pup>
auto transform_while_n(InIt first, N n, OutIt dest, UnaryOp op, UnaryPredicate pred, Pop p1, Pup p2)
    -> std::pair<OutIt, InIt>
{
    return i::transform_while_n(first, n, dest, astl::combine(astl::pass_fn(op), astl::pass_fn(p1)),
                                astl::combine(astl::pass_fn(pred), astl::pass_fn(p2)));
}

template <typename InIt1, typename InIt2, typename OutIt, typename BinaryOp,
          typename BinaryPredicate>
auto transform_while(InIt1 first1, InIt1 last1, InIt2 first2, OutIt dest, BinaryOp op,
                     BinaryPredicate pred) -> OutIt
{
    while (first1 != last1 && pred(*first1, *first2)) {
        *dest = op(*first1, *first2);
        ++first1;
        ++dest;
    }
    return dest;
}

template <typename InIt1, typename InIt2, typename OutIt, typename BinaryOp,
          typename BinaryPredicate, typename Pbo, typename Pbu>
auto transform_while(InIt1 first1, InIt1 last1, InIt2 first2, OutIt dest, BinaryOp op,
                     BinaryPredicate pred, Pbo p1, Pbu p2) -> OutIt
{
    return i::transform_while(first1, last1, first2, dest,
                              astl::combine(astl::pass_fn(op), astl::pass_fn(p1)),
                              astl::combine(astl::pass_fn(pred), astl::pass_fn(p2)));
}

template <typename InIt, typename OutIt, typename UnaryOp, typename UnaryPredicate>
auto transform_until(InIt first, InIt last, OutIt dest, UnaryOp op, UnaryPredicate pred)
    -> std::pair<OutIt, InIt>
{
    return i::transform_while(first, last, dest, astl::pass_fn(op),
                              astl::not_fn(astl::pass_fn(pred)));
}

template <typename InIt, typename OutIt, typename UnaryOp, typename UnaryPredicate, typename Pop,
          typename Pup>
auto transform_until(InIt first, InIt const last, OutIt dest, UnaryOp op, UnaryPredicate pred,
                     Pop p1, Pup p2) -> std::pair<OutIt, InIt>
{
    return i::transform_while(first, last, dest, astl::pass_fn(op), astl::pass_fn(p1),
                              astl::not_fn(astl::pass_fn(pred)), astl::pass_fn(p2));
}

template <typename InIt, typename N, typename OutIt, typename UnaryOp, typename UnaryPredicate>
auto transform_until_n(InIt first, N n, OutIt dest, UnaryOp op, UnaryPredicate pred)
    -> std::pair<OutIt, InIt>
{
    return i::transform_while_n(first, n, dest, astl::pass_fn(op),
                                astl::not_fn(astl::pass_fn(pred)));
}

template <typename InIt, typename N, typename OutIt, typename UnaryOp, typename UnaryPredicate,
          typename Pop, typename Pup>
auto transform_until_n(InIt first, N n, OutIt dest, UnaryOp op, UnaryPredicate pred, Pop p1, Pup p2)
    -> std::pair<OutIt, InIt>
{
    return i::transform_while_n(first, n, dest, astl::pass_fn(op), astl::pass_fn(p1),
                                astl::not_fn(astl::pass_fn(pred)), astl::pass_fn(p2));
}

template <typename InIt1, typename InIt2, typename OutIt, typename BinaryOp,
          typename BinaryPredicate>
auto transform_until(InIt1 first1, InIt1 last1, InIt2 first2, OutIt dest, BinaryOp op,
                     BinaryPredicate pred) -> OutIt
{
    return i::transform_while(first1, last1, first2, dest, astl::pass_fn(op),
                              astl::not_fn(astl::pass_fn(pred)));
}

template <typename InIt1, typename InIt2, typename OutIt, typename BinaryOp,
          typename BinaryPredicate, typename Pbo, typename Pbu>
auto transform_until(InIt1 first1, InIt1 last1, InIt2 first2, OutIt dest, BinaryOp op,
                     BinaryPredicate pred, Pbo p1, Pbu p2) -> OutIt
{
    return i::transform_while(first1, last1, first2, dest, astl::pass_fn(op), astl::pass_fn(p1),
                              astl::not_fn(astl::pass_fn(pred)), astl::pass_fn(p2));
}
} // namespace i

namespace r
{
template <typename R, typename BinaryOp, typename OutIt, typename A = identity>
// requires R Bidirectional Iterator range
// requires OutIt Output Iterator
// requires BinaryOp Binary Function on
// iter_value_type<BidIt>,iter_value_type<BidIt> requires A Function on
// iter_value_type<BidIt>
auto bidirectional_transform(R &&r, BinaryOp op, OutIt dest, A a = A{}) -> std::pair<OutIt, bool>
{
    return i::bidirectional_transform(adl::begin(r), adl::end(r), dest, std::move(op),
                                      std::move(a));
}

template <typename R, typename BinaryOp, typename OutIt, typename A, typename P>
auto bidirectional_transform(R &&r, BinaryOp op, OutIt dest, A a, P p) -> std::pair<OutIt, bool>
{
    return i::bidirectional_transform(adl::begin(r), adl::end(r), dest, std::move(op), std::move(a),
                                      std::move(p));
}

template <typename R, typename OutIt, typename UnaryOp, typename P>
auto transform(R &&r, OutIt dest, UnaryOp unary_op, P p) -> OutIt
{
    return i::transform(adl::begin(r), adl::end(r), dest, astl::pass_fn(unary_op),
                        astl::pass_fn(p));
}

template <typename R, typename OutIt, typename UnaryOp>
auto transform(R &&r, OutIt dest, UnaryOp unary_op) -> OutIt
{
    return i::transform(adl::begin(r), adl::end(r), dest, astl::pass_fn(unary_op));
}

template <typename R, typename InIt2, typename OutIt, typename BinaryOp, typename P1, typename P2>
auto transform(R &&r, InIt2 first2, OutIt dest, BinaryOp binary_op, P1 p1, P2 p2) -> OutIt
{
    return i::transform(adl::begin(r), adl::end(r), first2, dest, astl::pass_fn(binary_op),
                        astl::pass_fn(p1), astl::pass_fn(p2));
}

template <typename R, typename InIt2, typename OutIt, typename BinaryOp>
auto transform(R &&r, InIt2 first2, OutIt dest, BinaryOp binary_op) -> OutIt
{
    return i::transform(adl::begin(r), adl::end(r), first2, dest, astl::pass_fn(binary_op));
}

template <typename R, typename OutIt, typename BinaryOp>
// requires R ForwardIterator range
// requires OutIt OutputIterator
// requires BinaryOp, two arguments value_type(R)
auto transform_adjacent(R &&r, OutIt dest, BinaryOp binary_op) -> OutIt
{
    return i::transform_adjacent(adl::begin(r), adl::end(r), dest, astl::pass_fn(binary_op));
}

template <typename R, typename OutIt, typename BinaryOp, typename P>
auto transform_adjacent(R &&r, OutIt dest, BinaryOp binary_op, P p) -> OutIt
{
    return i::transform_adjacent(adl::begin(r), adl::end(r), dest, astl::pass_fn(binary_op),
                                 astl::pass_fn(p));
}

template <typename R, typename OutIt, typename BinaryOp>
auto transform_all_pairs(R &&r, OutIt dest, BinaryOp binary_op) -> OutIt
{
    return i::transform_all_pairs(adl::begin(r), adl::end(r), dest, astl::pass_fn(binary_op));
}

template <typename R, typename OutIt, typename BinaryOp, typename P>
auto transform_all_pairs(R &&r, OutIt dest, BinaryOp binary_op, P p) -> OutIt
{
    return i::transform_all_pairs(adl::begin(r), adl::end(r), dest, astl::pass_fn(binary_op),
                                  astl::pass_fn(p));
}

template <typename R, typename N, typename OutIt, typename BinaryOp, typename P>
auto transform_all_pairs_n(R &&r, N n, OutIt dest, BinaryOp binary_op, P p) -> OutIt
{
    return i::transform_all_pairs_n(adl::begin(r), n, dest, astl::pass_fn(binary_op),
                                    astl::pass_fn(p));
}

template <typename R, typename N, typename OutIt, typename BinaryOp>
auto transform_all_pairs_n(R &&r, N n, OutIt dest, BinaryOp binary_op) -> OutIt
{
    return i::transform_all_pairs_n(adl::begin(r), n, dest, astl::pass_fn(binary_op));
}

template <int N = 1, typename R, typename OutIt, typename NaryOp>
auto transform_each_n(R &&r, OutIt dest, NaryOp f) -> std::pair<OutIt, astl::iter_of_range<R>>
{
    return internal_transform::transform_each_n1<N>(adl::begin(r), dest, astl::pass_fn(f),
                                                    astl::size_or_distance(r));
}

template <int N = 1, typename R, typename OutIt, typename NaryOp, typename P>
auto transform_each_n(R &&r, OutIt dest, NaryOp f, P p) -> std::pair<OutIt, astl::iter_of_range<R>>
{
    return internal_transform::transform_each_n1<N>(adl::begin(r), dest, astl::pass_fn(f),
                                                    astl::size_or_distance(r), astl::pass_fn(p));
}

template <typename R, typename OutIt, typename T, typename BinOp, typename UnaryOp, typename P>
auto transform_exclusive_scan(R &&r, OutIt dest, T init, BinOp reduce_op, UnaryOp transform_op, P p)
    -> OutIt
{
    return i::transform_exclusive_scan(adl::begin(r), adl::end(r), dest, std::move(init),
                                       astl::pass_fn(reduce_op), astl::pass_fn(transform_op),
                                       astl::pass_fn(p));
}

template <typename R, typename OutIt, typename T, typename BinOp, typename UnaryOp>
auto transform_exclusive_scan(R &&r, OutIt dest, T init, BinOp reduce_op, UnaryOp transform_op)
    -> OutIt
{
    return i::transform_exclusive_scan(adl::begin(r), adl::end(r), dest, std::move(init),
                                       astl::pass_fn(reduce_op), astl::pass_fn(transform_op));
}

template <typename R, typename N, typename OutIt, typename T, typename BinOp, typename UnaryOp,
          typename P>
auto transform_exclusive_scan_n(R &&r, N n, OutIt dest, T init, BinOp reduce_op,
                                UnaryOp transform_op, P p)
    -> std::pair<OutIt, astl::iter_of_range<R>>
{
    return i::transform_exclusive_scan_n(adl::begin(r), n, dest, std::move(init),
                                         astl::pass_fn(reduce_op), astl::pass_fn(transform_op),
                                         astl::pass_fn(p));
}

template <typename R, typename N, typename OutIt, typename T, typename BinOp, typename UnaryOp>
auto transform_exclusive_scan_n(R &&r, N n, OutIt dest, T init, BinOp reduce_op,
                                UnaryOp transform_op) -> std::pair<OutIt, astl::iter_of_range<R>>
{
    return i::transform_exclusive_scan_n(adl::begin(r), n, dest, std::move(init),
                                         astl::pass_fn(reduce_op), astl::pass_fn(transform_op));
}

template <typename R, typename OutIt, typename UnaryOp, typename UnaryPredicate, typename Pop,
          typename Pup>
auto transform_if(R &&r, OutIt dest, UnaryOp op, UnaryPredicate pred, Pop p1, Pup p2) -> OutIt
{
    return i::transform_if(adl::begin(r), adl::end(r), dest, astl::pass_fn(op), astl::pass_fn(pred),
                           astl::pass_fn(p1), astl::pass_fn(p2));
}

template <typename R, typename OutIt, typename UnaryOp, typename UnaryPredicate>
auto transform_if(R &&r, OutIt dest, UnaryOp op, UnaryPredicate pred) -> OutIt
{
    return i::transform_if(adl::begin(r), adl::end(r), dest, astl::pass_fn(op),
                           astl::pass_fn(pred));
}

template <typename R, typename FwdIt, typename OutIt, typename BinaryOp, typename BinaryPredicate,
          typename Pbo, typename Pbu>
auto transform_if(R &&r, FwdIt first, OutIt dest, BinaryOp op, BinaryPredicate pred, Pbo p1, Pbu p2)
    -> OutIt
{
    return i::transform_if(adl::begin(r), adl::end(r), first, dest, astl::pass_fn(op),
                           astl::pass_fn(pred), astl::pass_fn(p1), astl::pass_fn(p2));
}

template <typename R, typename FwdIt, typename OutIt, typename BinaryOp, typename BinaryPredicate>
auto transform_if(R &&r, FwdIt first, OutIt dest, BinaryOp op, BinaryPredicate pred) -> OutIt
{
    return i::transform_if(adl::begin(r), adl::end(r), first, dest, astl::pass_fn(op),
                           astl::pass_fn(pred));
}

template <typename R, typename N, typename OutIt, typename UnaryOp, typename UnaryPredicate,
          typename Pop, typename Pup>
auto transform_if_n(R &&r, N n, OutIt dest, UnaryOp op, UnaryPredicate pred, Pop p1, Pup p2)
    -> std::pair<OutIt, astl::iter_of_range<R>>
{
    return i::transform_if_n(adl::begin(r), n, dest, astl::pass_fn(op), astl::pass_fn(pred),
                             astl::pass_fn(p1), astl::pass_fn(p2));
}

template <typename R, typename N, typename OutIt, typename UnaryOp, typename UnaryPredicate>
auto transform_if_n(R &&r, N n, OutIt dest, UnaryOp op, UnaryPredicate pred)
    -> std::pair<OutIt, astl::iter_of_range<R>>
{
    return i::transform_if_n(adl::begin(r), n, dest, astl::pass_fn(op), astl::pass_fn(pred));
}

template <typename R, typename OutIt, typename T, typename BinOp, typename UnaryOp>
auto transform_inclusive_scan(R &&r, OutIt dest, BinOp reduce_op, UnaryOp transform_op, T val)
    -> OutIt
{
    return i::transform_inclusive_scan(adl::begin(r), adl::end(r), dest, astl::pass_fn(reduce_op),
                                       astl::pass_fn(transform_op), std::move(val));
}

template <typename R, typename OutIt, typename T, typename BinOp, typename UnaryOp, typename P>
auto transform_inclusive_scan(R &&r, OutIt dest, BinOp reduce_op, UnaryOp transform_op, T val, P p)
    -> OutIt
{
    return i::transform_inclusive_scan(adl::begin(r), adl::end(r), dest, astl::pass_fn(reduce_op),
                                       astl::pass_fn(transform_op), std::move(val),
                                       astl::pass_fn(p));
}

template <typename R, typename N, typename OutIt, typename T, typename BinOp, typename UnaryOp>
auto transform_inclusive_scan_n(R &&r, N n, OutIt dest, BinOp reduce_op, UnaryOp transform_op,
                                T val) -> OutIt
{
    return i::transform_inclusive_scan_n(adl::begin(r), n, dest, astl::pass_fn(reduce_op),
                                         astl::pass_fn(transform_op), std::move(val));
}

template <typename R, typename N, typename OutIt, typename T, typename BinOp, typename UnaryOp,
          typename P>
auto transform_inclusive_scan_n(R &&r, N n, OutIt dest, BinOp reduce_op, UnaryOp transform_op,
                                T val, P p) -> OutIt
{
    return i::transform_inclusive_scan_n(adl::begin(r), n, dest, astl::pass_fn(reduce_op),
                                         astl::pass_fn(transform_op), std::move(val),
                                         astl::pass_fn(p));
}

template <typename R1, typename R2, typename T, typename BinOp1, typename BinOp2>
ASTL_NODISCARD auto transform_reduce(R1 &&c1, R2 &&c2, T init, BinOp1 reduce_op,
                                     BinOp2 transform_op) -> T
{
    return i::transform_reduce(adl::begin(c1), adl::end(c1), adl::begin(c2), std::move(init),
                               astl::pass_fn(reduce_op), astl::pass_fn(transform_op));
}

template <typename R, typename T, typename BinOp, typename UnaryOp>
ASTL_NODISCARD auto transform_reduce(R &&r, T init, BinOp reduce_op, UnaryOp transform_op) -> T
{
    return i::transform_reduce(adl::begin(r), adl::end(r), std::move(init),
                               astl::pass_fn(reduce_op), astl::pass_fn(transform_op));
}

template <typename R1, typename R2, typename T>
ASTL_NODISCARD auto transform_reduce(R1 &&r1, R2 &&r2, T init) -> T
{
    return i::transform_reduce(adl::begin(r1), adl::end(r1), adl::begin(r2), init, std::plus{},
                               std::multiplies{});
}

template <int N = 1, typename R, typename T, typename BinOp, typename NaryOp>
ASTL_NODISCARD auto transform_reduce_each_n(R &&r, T val, BinOp reduce_op, NaryOp transform_op)
    -> std::pair<T, astl::iter_of_range<R>>
{
    return internal_transform::transform_reduce_each_n1<N>(
        adl::begin(r), std::move(val), astl::pass_fn(reduce_op), astl::pass_fn(transform_op),
        astl::size_or_distance(r));
}

template <int N = 1, typename R, typename T, typename BinOp, typename NaryOp, typename P>
ASTL_NODISCARD auto transform_reduce_each_n(R &&r, T val, BinOp reduce_op, NaryOp transform_op, P p)
    -> std::pair<T, astl::iter_of_range<R>>
{
    return internal_transform::transform_reduce_each_n1<N>(
        adl::begin(r), std::move(val), astl::pass_fn(reduce_op), astl::pass_fn(transform_op),
        astl::size_or_distance(r), astl::pass_fn(p));
}

template <typename R1, typename N, typename R2, typename T, typename BinOp1, typename BinOp2>
ASTL_NODISCARD auto transform_reduce_n(R1 &&c1, N n, R2 &&c2, T init, BinOp1 reduce_op,
                                       BinOp2 transform_op) -> T
{
    return i::transform_reduce_n(adl::begin(c1), n, adl::begin(c2), std::move(init),
                                 astl::pass_fn(reduce_op), astl::pass_fn(transform_op));
}

template <typename R, typename N, typename T, typename BinOp, typename UnaryOp>
ASTL_NODISCARD auto transform_reduce_n(R &&r, N n, T init, BinOp reduce_op, UnaryOp transform_op)
    -> T
{
    return i::transform_reduce_n(adl::begin(r), n, std::move(init), astl::pass_fn(reduce_op),
                                 astl::pass_fn(transform_op));
}

template <typename R1, typename N, typename R2, typename T>
ASTL_NODISCARD auto transform_reduce_n(R1 &&r1, N n, R2 &&r2, T init) -> T
{
    return i::transform_reduce_n(adl::begin(r1), n, adl::begin(r2), init, std::plus{},
                                 std::multiplies{});
}

template <typename R, typename OutIt, typename UnaryOp, typename UnaryPredicate, typename Pop,
          typename Pup>
auto transform_until(R &&r, OutIt dest, UnaryOp op, UnaryPredicate pred, Pop p1, Pup p2)
    -> std::pair<OutIt, astl::iter_of_range<R>>
{
    return i::transform_until(adl::begin(r), adl::end(r), dest, astl::pass_fn(op),
                              astl::pass_fn(pred), astl::pass_fn(p1), astl::pass_fn(p2));
}

template <typename R, typename OutIt, typename UnaryOp, typename UnaryPredicate>
auto transform_until(R &&r, OutIt dest, UnaryOp op, UnaryPredicate pred)
    -> std::pair<OutIt, astl::iter_of_range<R>>
{
    return i::transform_until(adl::begin(r), adl::end(r), dest, astl::pass_fn(op),
                              astl::pass_fn(pred));
}

template <typename R, typename FwdIt, typename OutIt, typename BinaryOp, typename BinaryPredicate,
          typename Pbo, typename Pbu>
auto transform_until(R &&r, FwdIt first, OutIt dest, BinaryOp op, BinaryPredicate pred, Pbo p1,
                     Pbu p2) -> OutIt
{
    return i::transform_until(adl::begin(r), adl::end(r), first, dest, astl::pass_fn(op),
                              astl::pass_fn(pred), astl::pass_fn(p1), astl::pass_fn(p2));
}

template <typename R, typename FwdIt, typename OutIt, typename BinaryOp, typename BinaryPredicate>
auto transform_until(R &&r, FwdIt first, OutIt dest, BinaryOp op, BinaryPredicate pred) -> OutIt
{
    return i::transform_until(adl::begin(r), adl::end(r), first, dest, astl::pass_fn(op),
                              astl::pass_fn(pred));
}

template <typename R, typename N, typename OutIt, typename UnaryOp, typename UnaryPredicate,
          typename Pop, typename Pup>
auto transform_until_n(R &&r, N n, OutIt dest, UnaryOp op, UnaryPredicate pred, Pop p1, Pup p2)
    -> std::pair<OutIt, astl::iter_of_range<R>>
{
    return i::transform_until_n(adl::begin(r), n, dest, astl::pass_fn(op), astl::pass_fn(pred),
                                astl::pass_fn(p1), astl::pass_fn(p2));
}

template <typename R, typename N, typename OutIt, typename UnaryOp, typename UnaryPredicate>
auto transform_until_n(R &&r, N n, OutIt dest, UnaryOp op, UnaryPredicate pred)
    -> std::pair<OutIt, astl::iter_of_range<R>>
{
    return i::transform_until_n(adl::begin(r), n, dest, astl::pass_fn(op), astl::pass_fn(pred));
}

template <typename R, typename OutIt, typename UnaryOp, typename UnaryPredicate, typename Pop,
          typename Pup>
auto transform_while(R &&r, OutIt dest, UnaryOp op, UnaryPredicate pred, Pop p1, Pup p2)
    -> std::pair<OutIt, astl::iter_of_range<R>>
{
    return i::transform_while(adl::begin(r), adl::end(r), dest, astl::pass_fn(op),
                              astl::pass_fn(pred), astl::pass_fn(p1), astl::pass_fn(p2));
}

template <typename R, typename OutIt, typename UnaryOp, typename UnaryPredicate>
auto transform_while(R &&r, OutIt dest, UnaryOp op, UnaryPredicate pred)
    -> std::pair<OutIt, astl::iter_of_range<R>>
{
    return i::transform_while(adl::begin(r), adl::end(r), dest, astl::pass_fn(op),
                              astl::pass_fn(pred));
}

template <typename R, typename FwdIt, typename OutIt, typename BinaryOp, typename BinaryPredicate,
          typename Pbo, typename Pbu>
auto transform_while(R &&r, FwdIt first, OutIt dest, BinaryOp op, BinaryPredicate pred, Pbo p1,
                     Pbu p2) -> OutIt
{
    return i::transform_while(adl::begin(r), adl::end(r), first, dest, astl::pass_fn(op),
                              astl::pass_fn(pred), astl::pass_fn(p1), astl::pass_fn(p2));
}

template <typename R, typename FwdIt, typename OutIt, typename BinaryOp, typename BinaryPredicate>
auto transform_while(R &&r, FwdIt first, OutIt dest, BinaryOp op, BinaryPredicate pred) -> OutIt
{
    return i::transform_while(adl::begin(r), adl::end(r), first, dest, astl::pass_fn(op),
                              astl::pass_fn(pred));
}

template <typename R, typename N, typename OutIt, typename UnaryOp, typename UnaryPredicate,
          typename Pop, typename Pup>
auto transform_while_n(R &&r, N n, OutIt dest, UnaryOp op, UnaryPredicate pred, Pop p1, Pup p2)
    -> std::pair<OutIt, astl::iter_of_range<R>>
{
    return i::transform_while_n(adl::begin(r), n, dest, astl::pass_fn(op), astl::pass_fn(pred),
                                astl::pass_fn(p1), astl::pass_fn(p2));
}

template <typename R, typename N, typename OutIt, typename UnaryOp, typename UnaryPredicate>
auto transform_while_n(R &&r, N n, OutIt dest, UnaryOp op, UnaryPredicate pred)
    -> std::pair<OutIt, astl::iter_of_range<R>>
{
    return i::transform_while_n(adl::begin(r), n, dest, astl::pass_fn(op), astl::pass_fn(pred));
}
} // namespace r
} // namespace astl

#endif // ASTL_INCLUDE_TRANSFORM_HPP
