//
// Created by Rijad on 17-Aug-19.
//

#ifndef ASTL_INCLUDE_CLAMP_HPP
#define ASTL_INCLUDE_CLAMP_HPP

#include <utility>

#include "astl/functional.hpp"
#include "astl/iterator.hpp"
#include "astl/range_access.hpp"

namespace astl
{

inline constexpr struct {
    /**
 *  @brief  Returns the value clamped between lo and hi.
 *  @ingroup sorting_algorithms
 *  @param  val  A value of arbitrary type.
 *  @param  lo   A lower limit of arbitrary type.
 *  @param  hi   An upper limit of arbitrary type.
 *  @return max(val, lo) if val < hi or min(val, hi) otherwise.
 */
    template <typename T>
    ASTL_NODISCARD auto operator()(T const &val, T const &lo, T const &hi) noexcept -> T const &
    {
        return (val < lo) ? lo : (hi < val) ? hi : val;
    }

    /**
 *  @brief  Returns the value clamped between lo and hi.
 *  @ingroup sorting_algorithms
 *  @param  val   A value of arbitrary type.
 *  @param  lo    A lower limit of arbitrary type.
 *  @param  hi    An upper limit of arbitrary type.
 *  @param  comp  A comparison functor.
 *  @return max(val, lo, comp) if comp(val, hi) or min(val, hi, comp) otherwise.
 */
    template <typename T, typename Comparator>
    ASTL_NODISCARD auto operator()(T const &val, T const &lo, T const &hi, Comparator comp) const
        -> T const &
    {
        return comp(val, lo) ? lo : comp(hi, val) ? hi : val;
    }

    template <typename T, typename Comparator, typename P>
    ASTL_NODISCARD auto operator()(T const &val, T const &lo, T const &hi, Comparator comp,
                                   P p) const -> T const &
    {
        return comp(invoke(p, val), invoke(p, lo)) ? lo :
                                                     comp(invoke(p, hi), invoke(p, val)) ? hi : val;
    }
} clamp{};

namespace i
{
/// \return clamp the sequence of values [f, l) into [ lo, hi ]
///     using the comparison predicate p.
///
/// \param first The start of the range of values
/// \param last  One past the end of the range of input values
/// \param dest  An output iterator to write the clamped values into
/// \param lo    The lower bound of the range to be clamped to
/// \param hi    The upper bound of the range to be clamped to
/// \param pred     A predicate to use to compare the values.
///                 pred ( a, b ) returns a boolean.
///
inline constexpr struct {
    template <typename InIt, typename OutIt, typename BinaryPredicate = std::less<>>
    ASTL_NODISCARD auto operator()(InIt first, InIt last, OutIt dest,
                                   iter_value_type<InIt> const &lo, iter_value_type<InIt> const &hi,
                                   BinaryPredicate pred = BinaryPredicate{}) const -> OutIt
    {
        while (first != last) {
            *dest = astl::clamp(*first, lo, hi, pred);
            ++first;
            ++dest;
        }
        return dest;
    }

    template <typename InIt, typename OutIt, typename BinaryPredicate, typename LoT, typename HiT,
              typename P>
    ASTL_NODISCARD auto operator()(InIt first, InIt last, OutIt dest, LoT &&lo, HiT &&hi,
                                   BinaryPredicate pred, P p) const -> OutIt
    {
        while (first != last) {
            *dest = astl::clamp(*first, lo, hi, pred, p);
            ++first;
            ++dest;
        }
        return dest;
    }

} clamp_range{};

inline constexpr struct {

    template <typename InIt, typename N, typename OutIt, typename BinaryPredicate = std::less<>>
    ASTL_NODISCARD auto operator()(InIt first, N n, OutIt dest, iter_value_type<InIt> const &lo,
                                   iter_value_type<InIt> const &hi,
                                   BinaryPredicate pred = BinaryPredicate{}) const
        -> std::pair<OutIt, InIt>
    {
        while (n != N(0)) {
            *dest = astl::clamp(*first, lo, hi, pred);
            ++first;
            ++dest;
            --n;
        }
        return std::make_pair(dest, first);
    }

    template <typename InIt, typename N, typename OutIt, typename BinaryPredicate, typename LoT,
              typename HiT, typename P>
    ASTL_NODISCARD auto operator()(InIt first, N n, OutIt dest, LoT &&lo, HiT &&hi,
                                   BinaryPredicate pred, P p) const -> std::pair<OutIt, InIt>
    {
        while (n != N(0)) {
            *dest = astl::clamp(*first, lo, hi, pred, p);
            ++first;
            ++dest;
            --n;
        }
        return std::make_pair(dest, first);
    }

} clamp_range_n{};

} // namespace i
namespace r
{
inline constexpr struct {

    template <typename R, typename OutIt, typename BinaryPredicate = std::less<>>
    ASTL_NODISCARD auto operator()(R &&r, OutIt dest, range_value_type<R> const &lo,
                                   range_value_type<R> const &hi,
                                   BinaryPredicate pred = BinaryPredicate{}) const -> OutIt
    {
        return i::clamp_range(adl::begin(r), adl::end(r), dest, lo, hi, astl::pass_fn(pred));
    }

    template <typename R, typename OutIt, typename BinaryPredicate, typename LoT, typename HiT,
              typename P>
    ASTL_NODISCARD auto operator()(R &&r, OutIt dest, LoT &&lo, HiT &&hi, BinaryPredicate pred,
                                   P p) const -> OutIt
    {
        return i::clamp_range(adl::begin(r), adl::end(r), dest, lo, hi, astl::pass_fn(pred),
                              astl::pass_fn(p));
    }

} clamp_range{};

inline constexpr struct {
    template <typename R, typename N, typename OutIt, typename BinaryPredicate = std::less<>>
    ASTL_NODISCARD auto operator()(R &&r, N n, OutIt dest, range_value_type<R> const &lo,
                                   range_value_type<R> const &hi,
                                   BinaryPredicate pred = BinaryPredicate{}) const
        -> std::pair<OutIt, astl::iter_of_range<R>>
    {
        return i::clamp_range_n(adl::begin(r), n, dest, lo, hi, astl::pass_fn(pred));
    }

    template <typename R, typename N, typename OutIt, typename BinaryPredicate, typename LoT,
              typename HiT, typename P>
    ASTL_NODISCARD auto operator()(R &&r, N n, OutIt dest, LoT &&lo, HiT &&hi, BinaryPredicate pred,
                                   P p) const -> std::pair<OutIt, astl::iter_of_range<R>>
    {
        return i::clamp_range_n(adl::begin(r), n, dest, lo, hi, astl::pass_fn(pred),
                                astl::pass_fn(p));
    }
} clamp_range_n{};

} // namespace r
} // namespace astl

#endif // ASTL_INCLUDE_CLAMP_HPP
