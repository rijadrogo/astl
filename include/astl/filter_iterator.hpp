//
// Created by Rijad on 13-Aug-18.
//

#ifndef ASTL_INCLUDE_FILTER_ITERATOR_HPP
#define ASTL_INCLUDE_FILTER_ITERATOR_HPP

#include <type_traits>

#include "ebo_base.hpp"
#include "functional.hpp"
#include "iterator.hpp"
#include "iterator_range.hpp"
#include "range_access.hpp"

namespace astl
{
namespace internal_fib
{
template <int> struct fib_tag {};
} // namespace internal_fib
/// An iterator adaptor that filters the elements of given inner iterators.
///
/// The predicate parameter should be a callable object that accepts the wrapped
/// iterator's reference type and returns a bool. When incrementing or
/// decrementing the iterator, it will call the predicate on each element and
/// skip any where it returns false.
///
/// \code
///   int A[] = { 1, 2, 3, 4 };
///   auto R = make_filter_range(A, [](int N) { return N % 2 == 1; });
///   // R contains { 1, 3 }.
/// \endcode
///
/// Note: filter_iterator_base implements support for forward iteration.
/// filter_iterator_impl exists to provide support for bidirectional iteration,
/// conditional on whether the wrapped iterator supports it.
template <typename WrappedIteratorT, typename PredicateT, typename IterTag>
struct filter_iterator_base
    : iterator_adaptor_base<
          filter_iterator_base<WrappedIteratorT, PredicateT, IterTag>, WrappedIteratorT,
          typename std::common_type<
              IterTag, typename std::iterator_traits<WrappedIteratorT>::iterator_category>::type>,
      private ebo<internal_fib::fib_tag<0>, PredicateT> {
private:
    using BaseT = iterator_adaptor_base<
        filter_iterator_base<WrappedIteratorT, PredicateT, IterTag>, WrappedIteratorT,
        typename std::common_type<
            IterTag, typename std::iterator_traits<WrappedIteratorT>::iterator_category>::type>;

protected:
    using BasePred = ebo<internal_fib::fib_tag<0>, PredicateT>;

    WrappedIteratorT end;

    constexpr auto find_next_valid() -> void
    {
        while (this->i != end && !ebo_get(*this)(*this->i)) BaseT::operator++();
    }

    // Construct the iterator. The begin iterator needs to know where the end is,
    // so that it can properly stop when it gets there. The end iterator only
    // needs the predicate to support bidirectional iteration.
    constexpr filter_iterator_base(WrappedIteratorT first, WrappedIteratorT last, PredicateT pred)
        : BaseT(std::move(first)), BasePred(std::move(pred)), end(std::move(last))
    {
        find_next_valid();
    }

public:
    using BaseT::operator++;

    constexpr auto operator++() -> filter_iterator_base &
    {
        BaseT::operator++();
        find_next_valid();
        return *this;
    }
};

/// Specialization of filter_iterator_base for forward iteration only.
template <typename WrappedIteratorT, typename PredicateT,
          typename IterTag = std::forward_iterator_tag>
struct filter_iterator_impl: filter_iterator_base<WrappedIteratorT, PredicateT, IterTag> {
private:
    using BaseT = filter_iterator_base<WrappedIteratorT, PredicateT, IterTag>;

public:
    constexpr filter_iterator_impl(WrappedIteratorT first, WrappedIteratorT last, PredicateT pred)
        : BaseT(std::move(first), std::move(last), std::move(pred))
    {}
};

/// Specialization of filter_iterator_base for bidirectional iteration.
template <typename WrappedIteratorT, typename PredicateT>
struct filter_iterator_impl<WrappedIteratorT, PredicateT, std::bidirectional_iterator_tag>
    : filter_iterator_base<WrappedIteratorT, PredicateT, std::bidirectional_iterator_tag> {
private:
    using BaseT =
        filter_iterator_base<WrappedIteratorT, PredicateT, std::bidirectional_iterator_tag>;

    constexpr auto find_prev_valid() -> void
    {
        while (!ebo_get(*this)(*this->i)) BaseT::operator--();
    }

public:
    using BaseT::operator--;

    constexpr filter_iterator_impl(WrappedIteratorT first, WrappedIteratorT last, PredicateT pred)
        : BaseT(std::move(first), std::move(last), std::move(pred))
    {}

    constexpr auto operator--() -> filter_iterator_impl &
    {
        BaseT::operator--();
        find_prev_valid();
        return *this;
    }
};

namespace internal_filter
{
/// Helper which sets its type member to forward_iterator_tag if the category of
/// \p IterT does not derive from bidirectional_iterator_tag, and to
/// bidirectional_iterator_tag otherwise.
template <typename IterT> struct fwd_or_bidi_tag {
    using type = if_t<is_bidirectional_it_v<IterT>, std::bidirectional_iterator_tag,
                      std::forward_iterator_tag>;
};
} // namespace internal_filter

/// Defines filter_iterator to a suitable specialization of
/// filter_iterator_impl, based on the underlying iterator's category.

template <typename WrappedIteratorT, typename PredicateT>
using filter_iterator =
    filter_iterator_impl<WrappedIteratorT, PredicateT,
                         typename internal_filter::fwd_or_bidi_tag<WrappedIteratorT>::type>;

template <typename I, typename S, typename F>
ASTL_NODISCARD constexpr auto make_filter_iterator(I i, S last, F f) -> filter_iterator<I, F>
{
    return filter_iterator<I, F>{std::move(i), std::move(last), std::move(f)};
}

template <typename I, typename S, typename F>
ASTL_NODISCARD constexpr auto make_filterfalse_iterator(I i, S last, F f)
{
    return astl::make_filter_iterator(std::move(i), std::move(last), astl::not_fn(std::move(f)));
}

/// Convenience function that takes a range of elements and a predicate, and
/// return a new filter_iterator range.
template <typename I, typename PredicateT>
ASTL_NODISCARD constexpr auto make_filter_range(I first, I last, PredicateT pred)
    -> iterator_range<filter_iterator<I, PredicateT>>
{
    using FilterIteratorT = filter_iterator<I, PredicateT>;
    return astl::make_range(FilterIteratorT(first, last, pred), FilterIteratorT(last, last, pred));
}

/// FIXME: Currently if RangeT && is a rvalue reference to a temporary, the
/// lifetime of that temporary is not kept by the returned range object, and the
/// temporary is going to be dropped on the floor after the make_iterator_range
/// full expression that contains this function call.
template <typename RangeT, typename PredicateT>
ASTL_NODISCARD constexpr auto make_filter_range(RangeT &&range, PredicateT pred)
    -> iterator_range<filter_iterator<iter_of_range<RangeT>, PredicateT>>
{
    return astl::make_filter_range(adl::begin(range), adl::end(range), std::move(pred));
}

/// Convenience function that takes a range of elements and a predicate, and
/// return a new filter_iterator range.
template <typename I, typename PredicateT>
ASTL_NODISCARD constexpr auto make_filterfalse_range(I first, I last, PredicateT pred)
{
    return astl::make_range(astl::make_filterfalse_iterator(first, last, pred),
                            astl::make_filterfalse_iterator(last, last, pred));
}

/// FIXME: Currently if RangeT && is a rvalue reference to a temporary, the
/// lifetime of that temporary is not kept by the returned range object, and the
/// temporary is going to be dropped on the floor after the make_iterator_range
/// full expression that contains this function call.
template <typename RangeT, typename PredicateT>
ASTL_NODISCARD constexpr auto make_filterfalse_range(RangeT &&range, PredicateT pred)
{
    return astl::make_filterfalse_range(adl::begin(range), adl::end(range), std::move(pred));
}
} // namespace astl

#endif // ASTL_INCLUDE_FILTER_ITERATOR_HPP
