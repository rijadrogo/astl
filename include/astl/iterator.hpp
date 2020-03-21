//
// Created by Rijad on 28-Jul-18.
//

#ifndef ASTL_INCLUDE_ITERATOR_HPP
#define ASTL_INCLUDE_ITERATOR_HPP

#include <cstddef>
#include <iterator>
#include <type_traits>

#include "functional.hpp"
#include "range_access.hpp"

namespace astl
{
namespace internal_iter
{
template <typename I, typename = void> struct is_iterator1: std::false_type {};

template <typename I>
struct is_iterator1<I, void_t<decltype(typename I::iterator_category{})>>: std::true_type {};

} // namespace internal_iter

template <typename I> using is_iterator = typename internal_iter::is_iterator1<I>::value_type;

template <typename R> using iter_of_range = decltype(adl::begin(std::declval<R>()));

template <typename R> using begin_t = decltype(adl::begin(std::declval<R>()));

template <typename R> using end_t = decltype(adl::end(std::declval<R>()));

template <typename I> using iter_value_type = typename std::iterator_traits<I>::value_type;

template <typename I> using iter_diff_type = typename std::iterator_traits<I>::difference_type;

template <typename I> using iter_cat = typename std::iterator_traits<I>::iterator_category;

template <typename I> using iter_ref_t = typename std::iterator_traits<I>::reference;

template <typename I> using iter_pointer_t = typename std::iterator_traits<I>::pointer;

template <typename R>
using range_value_type = typename std::iterator_traits<iter_of_range<R>>::value_type;

template <typename R>
using range_diff_type = typename std::iterator_traits<iter_of_range<R>>::difference_type;

template <typename R>
using range_iter_cat = typename std::iterator_traits<iter_of_range<R>>::iterator_category;

template <typename I>
using range_ref_t = typename std::iterator_traits<iter_of_range<I>>::reference;

template <typename I>
using range_pointer_t = typename std::iterator_traits<iter_of_range<I>>::pointer;

template <typename I>
using is_randit = std::is_base_of<std::random_access_iterator_tag, iter_cat<I>>;

template <typename I>
using is_bidit = std::is_base_of<std::bidirectional_iterator_tag, iter_cat<I>>;

template <typename I> using is_fwdit = std::is_base_of<std::forward_iterator_tag, iter_cat<I>>;

template <typename I> using is_inputit = std::is_base_of<std::input_iterator_tag, iter_cat<I>>;

template <typename I> using is_outit = std::is_same<std::output_iterator_tag, iter_cat<I>>;

template <typename I> inline constexpr auto is_iterator_v = internal_iter::is_iterator1<I>::value;

template <typename... I> inline constexpr auto is_random_access_it_v = (... && is_randit<I>::value);

template <typename... I> inline constexpr auto is_bidirectional_it_v = (... && is_bidit<I>::value);

template <typename... I> inline constexpr auto is_forward_it_v = (... && is_fwdit<I>::value);

template <typename... I> inline constexpr auto is_input_it_v = (... && is_inputit<I>::value);

template <typename... I> inline constexpr auto is_output_it_v = (... && is_outit<I>::value);

template <typename I> ASTL_NODISCARD constexpr auto iterator_category(I) noexcept -> iter_cat<I>
{
    return {};
}

template <typename... I>
ASTL_NODISCARD constexpr auto iterator_category(I...) noexcept ->
    typename std::common_type<iter_cat<I>...>::type
{
    return {};
}

template <typename R>
ASTL_NODISCARD constexpr auto iterator_category(R &&) noexcept -> range_iter_cat<R>
{
    return {};
}

template <typename... R>
ASTL_NODISCARD constexpr auto iterator_category(R &&...) noexcept ->
    typename std::common_type<range_iter_cat<R>...>::type
{
    return {};
}

template <typename I>
ASTL_NODISCARD constexpr auto difference_type(I) noexcept -> iter_diff_type<I> *
{
    return nullptr;
}

template <typename... I>
ASTL_NODISCARD constexpr auto difference_type(I...) noexcept ->
    typename std::common_type<iter_diff_type<I>...>::type *
{
    return nullptr;
}

template <typename R>
ASTL_NODISCARD constexpr auto difference_type(R &&) noexcept -> range_diff_type<R> *
{
    return nullptr;
}

template <typename... R>
ASTL_NODISCARD constexpr auto difference_type(R &&...) noexcept ->
    typename std::common_type<range_diff_type<R>...>::type *
{
    return nullptr;
}

template <typename I> ASTL_NODISCARD constexpr auto value_type(I) noexcept -> iter_value_type<I> *
{
    return nullptr;
}

template <typename... I>
ASTL_NODISCARD constexpr auto value_type(I...) noexcept ->
    typename std::common_type<iter_value_type<I>...>::type *
{
    return nullptr;
}

template <typename R>
ASTL_NODISCARD constexpr auto value_type(R &&) noexcept -> range_value_type<R> *
{
    return nullptr;
}

template <typename... R>
ASTL_NODISCARD constexpr auto value_type(R &&...) noexcept ->
    typename std::common_type<range_value_type<R>...>::type *
{
    return nullptr;
}

inline constexpr struct {
    template <typename Iterator> auto operator()(Iterator i1, Iterator i2) const
    {
        return std::iter_swap(i1, i2);
    }

} iter_swap{};

inline constexpr struct {
    template <typename Iterator>
    auto operator()(Iterator i1, Iterator i2) -> decltype(*i1 = std::move(*i2)) const
    {
        return *i1 = std::move(*i2);
    }

    template <typename Iterator> auto operator()(Iterator i) -> decltype(std::move(*i)) const
    {
        return std::move(*i);
    }

} iter_move{};

inline constexpr struct {
    /// Increment (decrement for negative n) i |n| times or until i == bound
    /// whichever comes f. Returns n - the difference between i's final position and
    /// its initial position. (Note: "advance" has overloads with this behavior in
    /// the Ranges TS.)
    template <typename I, typename Distance, typename S>
    constexpr auto operator()(I &i, Distance n, S bound) const -> iter_diff_type<I>
    {
        if constexpr (is_random_access_it_v<I>) { // Random Access Iterator
            Distance it_diff(bound - i);
            if (n >= 0 ? n >= it_diff : n <= it_diff) {
                i = std::move(bound);
                return n - it_diff;
            }
            i += n;
            return 0;
        }
        else if constexpr (is_bidirectional_it_v<I>) { // Bidirectional Iterator
            while (n < Distance(0) && i != bound) {
                ++n;
                --i;
            }
            while (n > Distance(0) && i != bound) {
                --n;
                ++i;
            }
            return n;
        }
        else { // Input Iterator
            while (n > Distance(0) && i != bound) {
                --n;
                ++i;
            }
            return n;
        }
    }

    template <typename I, typename Distance> void operator()(I &iter, Distance n) const
    {
        std::advance(iter, n);
    }

} advance{};

inline constexpr struct {
    template <typename I, typename Distance, typename S>
    ASTL_NODISCARD constexpr auto operator()(I i, Distance n, S const bound) const -> I
    {
        astl::advance(i, n, bound);
        return i;
    }
    template <typename I, typename Distance>
    ASTL_NODISCARD auto operator()(I iter, Distance n) const -> I
    {
        return std::next(iter, n);
    }

    template <typename I> ASTL_NODISCARD auto operator()(I iter) const -> I
    {
        ++iter;
        return iter;
    }
} next{};

inline constexpr struct {
    template <typename I, typename S>
    ASTL_NODISCARD constexpr auto operator()(I i, S end) const -> I
    {
        return (i == end ? end : astl::next(i));
    }
} next_or_end{};

inline constexpr struct {

    template <typename BidiIt, typename Distance, typename S>
    ASTL_NODISCARD constexpr auto operator()(BidiIt i, Distance n, S const bound) const -> BidiIt
    {
        astl::advance(i, -static_cast<iter_diff_type<BidiIt>>(n), bound);
        return i;
    }
    template <typename BidiIter, typename Distance>
    ASTL_NODISCARD auto operator()(BidiIter iter, Distance n) const -> BidiIter
    {
        return std::prev(iter, n);
    }

    template <typename BidiIter> ASTL_NODISCARD auto operator()(BidiIter iter) const -> BidiIter
    {
        --iter;
        return iter;
    }
} prev{};

inline constexpr struct {

    template <typename I, typename S, typename Distance>
    constexpr auto operator()(I first, S last, Distance &d) const -> void
    {
        d += static_cast<Distance>(std::distance(first, last));
    }

    template <typename I, typename S>
    ASTL_NODISCARD constexpr auto operator()(I first, S last) const -> iter_diff_type<I>
    {
        return std::distance(first, last);
    }

} distance{};

inline constexpr struct {
    // Complexity O(n + n/2);  n = distance(first, last)
    template <typename I, typename S>
    ASTL_NODISCARD constexpr auto operator()(I first, S last) const -> I
    {
        return astl::next(first, astl::distance(first, last) >> 1);
    }
} middle{};

namespace internal_func_iter
{
template <typename R>
constexpr auto size_or_distance1(R &&r, internal_adl::rank<0>) -> range_diff_type<R>
{
    return astl::distance(adl::begin(r), adl::end(r));
}

template <typename R>
constexpr auto size_or_distance1(R &&r, internal_adl::rank<1>) -> decltype(adl::size(r))
{
    return adl::size(r);
}
} // namespace internal_func_iter

inline constexpr struct {
    template <typename R> ASTL_NODISCARD constexpr auto operator()(R &&c)
    {
        return internal_func_iter::size_or_distance1(c, internal_adl::rank<1>{});
    }
} size_or_distance{};

namespace int_adl_b
{
/// \brief CRTP base class which implements the entire standard iterator facade
/// in terms of a minimal subset of the interface.
///
/// Use this when it is reasonable to implement most of the iterator
/// functionality in terms of a core subset. If you need special behavior or
/// there are performance implications for this, you may want to override the
/// relevant members instead.
///
/// Note, one abstraction that this does *not* provide is implementing
/// subtraction in terms of addition by negating the difference. Negation isn't
/// always information preserving, and I can see very reasonable iterator
/// designs where this doesn't work well. It doesn't really force much added
/// boilerplate anyways.
///
/// Another abstraction that this doesn't provide is implementing increment in
/// terms of addition of one. These aren't equivalent for all iterator
/// categories, and respecting that adds a lot of complexity for little gain.
///
/// Classes wishing to use `iterator_facade_base` should implement the following
/// methods:
///
/// Forward Iterators:
///   (All of the following methods)
///   - DerivedT &operator=( DerivedT const&R);
///   - bool operator==( DerivedT const&R) const;
///   - const T &operator*() const;
///   - T &operator*();
///   - DerivedT &operator++();
///
/// Bidirectional Iterators:
///   (All methods of forward iterators, plus the following)
///   - DerivedT &operator--();
///
/// Random-access Iterators:
///   (All methods of bidirectional iterators excluding the following)
///   - DerivedT &operator++();
///   - DerivedT &operator--();
///   (and plus the following)
///   - bool operator<( DerivedT const&RHS) const;
///   - DifferenceTypeT operator-(DerivedT const&R) const;
///   - DerivedT &operator+=(DifferenceTypeT N);
///   - DerivedT &operator-=(DifferenceTypeT N);
///
template <typename DerivedT, typename IteratorCategoryT, typename T,
          typename DifferenceTypeT = std::ptrdiff_t, typename PointerT = T *,
          typename ReferenceT = T &>
struct iterator_facade_base {
    using iterator_category = IteratorCategoryT;
    using value_type = T;
    using difference_type = DifferenceTypeT;
    using pointer = PointerT;
    using reference = ReferenceT;

protected:
    static constexpr bool is_random_access =
        std::is_same_v<std::random_access_iterator_tag, IteratorCategoryT>;

    static constexpr bool is_bidirectional =
        std::is_same_v<std::bidirectional_iterator_tag, IteratorCategoryT>;

    /// A proxy object for computing a reference via indirecting a copy of an
    /// iterator. This is used in APIs which need to produce a reference via
    /// indirection but for which the iterator object might be a temporary. The
    /// proxy preserves the iterator internally and exposes the indirected
    /// reference via a conversion operator.
    struct reference_proxy {
    private:
        friend iterator_facade_base;

        DerivedT i;

        constexpr explicit reference_proxy(DerivedT it) : i(it) {}

    public:
        // NOLINTNEXTLINE(google-explicit-constructor)
        ASTL_NODISCARD constexpr operator ReferenceT() const { return *i; }
    };

public:
    ASTL_NODISCARD constexpr auto base() const -> DerivedT
    {
        return *static_cast<DerivedT const *>(this);
    }

    ASTL_NODISCARD constexpr auto operator+(DifferenceTypeT n) const -> DerivedT
    {
        static_assert(std::is_base_of<iterator_facade_base, DerivedT>::value,
                      "Must pass the derived type to this template!");
        static_assert(is_random_access,
                      "The '+' operator is only defined for random "
                      "access iterators.");
        DerivedT tmp(*static_cast<DerivedT const *>(this));
        tmp += n;
        return tmp;
    }

    ASTL_NODISCARD friend constexpr auto operator+(DifferenceTypeT n, DerivedT const &i) -> DerivedT
    {
        static_assert(is_random_access,
                      "The '+' operator is only defined for random "
                      "access iterators.");
        return i + n;
    }

    ASTL_NODISCARD constexpr auto operator-(DifferenceTypeT n) const -> DerivedT
    {
        static_assert(is_random_access,
                      "The '-' operator is only defined for random "
                      "access iterators.");
        DerivedT tmp(*static_cast<DerivedT const *>(this));
        tmp -= n;
        return tmp;
    }

    constexpr auto operator++() -> DerivedT &
    {
        static_assert(std::is_base_of<iterator_facade_base, DerivedT>::value,
                      "Must pass the derived type to this template!");
        return static_cast<DerivedT *>(this)->operator++();
    }

    constexpr auto operator++(int) -> DerivedT
    {
        DerivedT tmp(*static_cast<DerivedT *>(this));
        ++*static_cast<DerivedT *>(this);
        return tmp;
    }

    constexpr auto operator--() -> DerivedT &
    {
        static_assert(is_bidirectional,
                      "The decrement operator is only defined "
                      "for bidirectional iterators.");
        return static_cast<DerivedT *>(this)->operator--();
    }

    constexpr auto operator--(int) -> DerivedT
    {
        static_assert(is_bidirectional,
                      "The decrement operator is only defined "
                      "for bidirectional iterators.");
        DerivedT tmp(*static_cast<DerivedT *>(this));
        --*static_cast<DerivedT *>(this);
        return tmp;
    }

    ASTL_NODISCARD constexpr auto operator!=(DerivedT const &rhs) const noexcept -> bool
    {
        return !static_cast<DerivedT const *>(this)->operator==(rhs);
    }

    template <typename I> ASTL_NODISCARD constexpr auto operator!=(I const &rhs) const noexcept
    //        -> decltype(!(DerivedT::_op_equal(*static_cast<DerivedT
    //        const*>(this), rhs)))
    {
        return !(DerivedT::_op_equal(*static_cast<DerivedT const *>(this), rhs));
    }

    ASTL_NODISCARD constexpr auto operator>(DerivedT const &rhs) const noexcept -> bool
    {
        static_assert(is_random_access,
                      "Relational operators are only defined "
                      "for random access iterators.");
        return !static_cast<DerivedT const *>(this)->operator<(rhs)
            && !static_cast<DerivedT const *>(this)->operator==(rhs);
    }

    template <typename I> ASTL_NODISCARD constexpr auto operator>(I const &rhs) const noexcept
    {
        static_assert(is_random_access,
                      "Relational operators are only "
                      "defined for random access iterators.");
        return !(DerivedT::_op_less(*static_cast<DerivedT const *>(this), rhs))
            && !(DerivedT::_op_equal(*static_cast<DerivedT const *>(this), rhs));
    }

    ASTL_NODISCARD constexpr auto operator<=(DerivedT const &rhs) const noexcept -> bool
    {
        static_assert(is_random_access,
                      "Relational operators are only defined "
                      "for random access iterators.");
        return !static_cast<DerivedT const *>(this)->operator>(rhs);
    }

    template <typename I>
    ASTL_NODISCARD constexpr auto operator<=(I const &rhs) const noexcept -> bool
    {
        static_assert(is_random_access,
                      "Relational operators are only defined "
                      "for random access iterators.");
        return (DerivedT::_op_less(*static_cast<DerivedT const *>(this), rhs))
            || (DerivedT::_op_equal(*static_cast<DerivedT const *>(this), rhs));
        // return !(static_cast<DerivedT const*>(this)->operator>(rhs));
    }

    ASTL_NODISCARD constexpr auto operator>=(DerivedT const &rhs) const noexcept -> bool
    {
        static_assert(is_random_access,
                      "Relational operators are only defined "
                      "for random access iterators.");
        return !static_cast<DerivedT const *>(this)->operator<(rhs);
    }

    template <typename I>
    ASTL_NODISCARD constexpr auto operator>=(I const &rhs) const noexcept -> bool
    {
        static_assert(is_random_access,
                      "Relational operators are only defined "
                      "for random access iterators.");
        return !(DerivedT::_op_less(*static_cast<DerivedT const *>(this), rhs));
    }

    ASTL_NODISCARD constexpr auto operator-> () -> PointerT
    {
        return &static_cast<DerivedT *>(this)->operator*();
    }

    ASTL_NODISCARD constexpr auto operator-> () const -> PointerT
    {
        return &static_cast<DerivedT const *>(this)->operator*();
    }

    ASTL_NODISCARD constexpr auto operator[](DifferenceTypeT n) -> reference_proxy
    {
        static_assert(is_random_access,
                      "Subscripting is only defined "
                      "for random access iterators.");
        return reference_proxy(static_cast<DerivedT *>(this)->operator+(n));
    }

    ASTL_NODISCARD constexpr auto operator[](DifferenceTypeT n) const -> reference_proxy
    {
        static_assert(is_random_access,
                      "Subscripting is only defined "
                      "for random access iterators.");
        return reference_proxy(static_cast<DerivedT const *>(this)->operator+(n));
    }
    using _derived_t = DerivedT;
};

template <typename I1, typename I2>
ASTL_NODISCARD constexpr auto operator!=(I1 const &lhs, I2 const &rhs) noexcept
    -> decltype(!I2::_derived_t::_op_equal(lhs, rhs.base()))
{
    return !I2::_derived_t::_op_equal(lhs, rhs.base());
}

template <typename I1, typename I2>
ASTL_NODISCARD constexpr auto operator>(I1 const &lhs, I2 const &rhs) noexcept
    -> decltype(I2::_derived_t::_op_less(rhs.base(), lhs))
{
    return I2::_derived_t::_op_less(rhs.base(), lhs);
}

template <typename I1, typename I2>
ASTL_NODISCARD constexpr auto operator<=(I1 const &lhs, I2 const &rhs) noexcept
    -> decltype(I2::_derived_t::_op_less(lhs, rhs.base())
                && I2::_derived_t::_op_equal(lhs, rhs.base()))
{
    return I2::_derived_t::_op_less(lhs, rhs.base()) && I2::_derived_t::_op_equal(lhs, rhs.base());
}

template <typename I1, typename I2>
ASTL_NODISCARD constexpr auto operator>=(I1 const &lhs, I2 const &rhs) noexcept
    -> decltype(I2::_derived_t::_op_less(rhs.base(), lhs)
                || I2::_derived_t::_op_equal(lhs, rhs.base()))
{
    return I2::_derived_t::_op_less(rhs.base(), lhs) || I2::_derived_t::_op_equal(lhs, rhs.base());
}

/// \brief CRTP base class for adapting an iterator to a different type.
///
/// This class can be used through CRTP to adapt one iterator into another.
/// Typically this is done through providing in the derived class a custom \c
/// operator* implementation. Other methods can be overridden as well.
template <typename DerivedT, typename WrappedIteratorT,
          typename IteratorCategoryT = iter_cat<WrappedIteratorT>,
          typename ValueT = iter_value_type<WrappedIteratorT>,
          typename DifferenceTypeT = iter_diff_type<WrappedIteratorT>,
          typename PointerT = if_t<std::is_same<ValueT, iter_value_type<WrappedIteratorT>>::value,
                                   iter_pointer_t<WrappedIteratorT>, ValueT *>,
          typename ReferenceT = if_t<std::is_same<ValueT, iter_value_type<WrappedIteratorT>>::value,
                                     iter_ref_t<WrappedIteratorT>, ValueT &>,
          // Don't provide these, they are mostly to act as aliases below.
          typename WrappedTraitsT = std::iterator_traits<WrappedIteratorT>>
struct iterator_adaptor_base: iterator_facade_base<DerivedT, IteratorCategoryT, ValueT,
                                                   DifferenceTypeT, PointerT, ReferenceT> {
    template <typename DerivedT1, typename IteratorCategoryT1, typename T1,
              typename DifferenceTypeT1, typename PointerT1, typename ReferenceT1>
    friend struct iterator_facade_base;

protected:
    WrappedIteratorT i;

    iterator_adaptor_base() = default;

    explicit constexpr iterator_adaptor_base(WrappedIteratorT u) : i(u)
    {
        static_assert(std::is_base_of<iterator_adaptor_base, DerivedT>::value,
                      "Must pass the derived type to this template!");
    }

public:
    using _iterator_adaptor_base_tag = iterator_facade_base<DerivedT, IteratorCategoryT, ValueT,
                                                            DifferenceTypeT, PointerT, ReferenceT>;

    using base_t = typename iterator_adaptor_base::iterator_facade_base;
    using traits = WrappedTraitsT;

    // NOLINTNEXTLINE(readability-identifier-naming)
    static constexpr auto _op_equal(iterator_adaptor_base const &lhs, DerivedT const &rhs) noexcept
        -> bool
    {
        return lhs.base() == rhs.base();
    }

    template <typename I1, typename I2>
    // NOLINTNEXTLINE(readability-identifier-naming)
    static constexpr auto _op_equal(I1 const &lhs, I2 const &rhs) noexcept ->
        typename std::enable_if<std::is_base_of<_iterator_adaptor_base_tag, I1>::value
                                    && std::is_base_of<_iterator_adaptor_base_tag, I2>::value,
                                bool>::type
    {
        return lhs.base() == rhs.base();
    }

    template <typename I1, typename I2>
    // NOLINTNEXTLINE(readability-identifier-naming)
    static constexpr auto _op_equal(I1 const &lhs, I2 const &rhs) noexcept ->
        typename std::enable_if<std::is_base_of<_iterator_adaptor_base_tag, I1>::value
                                    && !std::is_base_of<_iterator_adaptor_base_tag, I2>::value,
                                bool>::type
    {
        return lhs.base() == rhs;
    }

    template <typename I1, typename I2>
    // NOLINTNEXTLINE(readability-identifier-naming)
    static constexpr auto _op_equal(I1 const &lhs, I2 const &rhs) noexcept ->
        typename std::enable_if<std::is_base_of<_iterator_adaptor_base_tag, I2>::value
                                    && !std::is_base_of<_iterator_adaptor_base_tag, I1>::value,
                                bool>::type
    {
        return lhs == rhs.base();
    }

    template <typename I1, typename I2>
    // NOLINTNEXTLINE(readability-identifier-naming)
    static constexpr auto _op_equal(I1 const &lhs, I2 const &rhs) noexcept ->
        typename std::enable_if<!std::is_base_of<_iterator_adaptor_base_tag, I1>::value
                                    && !std::is_base_of<_iterator_adaptor_base_tag, I2>::value,
                                decltype(rhs == lhs)>::type
    {
        return lhs == rhs;
    }
    // NOLINTNEXTLINE(readability-identifier-naming)
    static constexpr auto _op_less(iterator_adaptor_base const &lhs, DerivedT const &rhs) noexcept
        -> bool
    {
        return lhs.base() < rhs.base();
    }

    template <typename I1, typename I2>
    // NOLINTNEXTLINE(readability-identifier-naming)
    static constexpr auto _op_less(I1 const &lhs, I2 const &rhs) noexcept ->
        typename std::enable_if<std::is_base_of<_iterator_adaptor_base_tag, I1>::value
                                    && std::is_base_of<_iterator_adaptor_base_tag, I2>::value,
                                bool>::type
    {
        return lhs.base() < rhs.base();
    }

    template <typename I1, typename I2>
    // NOLINTNEXTLINE(readability-identifier-naming)
    static constexpr auto _op_less(I1 const &lhs, I2 const &rhs) noexcept ->
        typename std::enable_if<std::is_base_of<_iterator_adaptor_base_tag, I1>::value
                                    && !std::is_base_of<_iterator_adaptor_base_tag, I2>::value,
                                bool>::type
    {
        return lhs.base() < rhs;
    }

    template <typename I1, typename I2>
    // NOLINTNEXTLINE(readability-identifier-naming)
    static constexpr auto _op_less(I1 const &lhs, I2 const &rhs) noexcept ->
        typename std::enable_if<std::is_base_of<_iterator_adaptor_base_tag, I2>::value
                                    && !std::is_base_of<_iterator_adaptor_base_tag, I1>::value,
                                bool>::type
    {
        return lhs < rhs.base();
    }

    template <typename I1, typename I2>
    // NOLINTNEXTLINE(readability-identifier-naming)
    static constexpr auto _op_less(I1 const &lhs, I2 const &rhs) noexcept ->
        typename std::enable_if<!std::is_base_of<_iterator_adaptor_base_tag, I1>::value
                                    && !std::is_base_of<_iterator_adaptor_base_tag, I2>::value,
                                decltype(rhs == lhs)>::type
    {
        return lhs < rhs;
    }

    ASTL_NODISCARD constexpr auto base() const noexcept -> const WrappedIteratorT & { return i; }

    using difference_type = DifferenceTypeT;

    constexpr auto operator+=(difference_type const &n) -> DerivedT &
    {
        static_assert(base_t::is_random_access,
                      "The '+=' operator is only defined for random "
                      "access iterators.");
        i += n;
        return *static_cast<DerivedT *>(this);
    }

    constexpr auto operator-=(difference_type const &n) -> DerivedT &
    {
        static_assert(base_t::is_random_access,
                      "The '-=' operator is only defined for random "
                      "access iterators.");
        i -= n;
        return *static_cast<DerivedT *>(this);
    }

    using base_t::operator-;

    ASTL_NODISCARD constexpr auto operator-(DerivedT const &rhs) const -> difference_type
    {
        static_assert(base_t::is_random_access,
                      "The '-' operator is only defined for random "
                      "access iterators.");
        return i - rhs.i;
    }

    using base_t::operator++;

    constexpr auto operator++() -> DerivedT &
    {
        ++i;
        return *static_cast<DerivedT *>(this);
    }

    using base_t::operator--;

    constexpr auto operator--() -> DerivedT &
    {
        static_assert(base_t::is_bidirectional,
                      "The decrement operator is only defined for "
                      "bidirectional iterators.");
        --i;
        return *static_cast<DerivedT *>(this);
    }

    ASTL_NODISCARD constexpr auto operator==(DerivedT const &rhs) const noexcept -> bool
    {
        return i == rhs.i;
    }

    template <typename I>
    ASTL_NODISCARD constexpr auto operator==(I const &rhs) const noexcept
        -> decltype(iterator_adaptor_base::_op_equal(base(), rhs))
    {
        return iterator_adaptor_base::_op_equal(base(), rhs);
    }

    ASTL_NODISCARD constexpr auto operator<(DerivedT const &rhs) const noexcept -> bool
    {
        static_assert(base_t::is_random_access,
                      "Relational operators are only defined for random access "
                      "iterators.");
        return i < rhs.i;
    }

    template <typename I>
    ASTL_NODISCARD constexpr auto operator<(I const &rhs) const noexcept -> decltype(i < rhs.base())
    {
        static_assert(base_t::is_random_access,
                      "Relational operators are only defined for random access "
                      "iterators.");
        return i < rhs.base();
    }

    ASTL_NODISCARD constexpr auto operator*() const noexcept -> const typename base_t::value_type &
    {
        return *i;
    }
    ASTL_NODISCARD constexpr auto operator*() -> ReferenceT { return *i; }
};

template <typename I1, typename I2, typename = typename I2::_iterator_adaptor_base_tag>
ASTL_NODISCARD constexpr auto operator==(I1 const &lhs, I2 const &rhs) noexcept
    -> decltype(I2::_op_equal(lhs, rhs.base()))
{
    return I2::_op_equal(lhs, rhs.base());
}

template <typename I1, typename I2, typename = typename I2::_iterator_adaptor_base_tag>
ASTL_NODISCARD constexpr auto operator<(I1 const &lhs, I2 const &rhs) noexcept
    -> decltype(I2::_op_less(lhs, rhs.base()))
{
    return I2::_op_less(lhs, rhs.base());
}
} // namespace int_adl_b

using int_adl_b::iterator_adaptor_base;
using int_adl_b::iterator_facade_base;

namespace internal_move_only
{
struct move_only {
    move_only() = default;
    move_only(move_only &&) = default;
    auto operator=(move_only &&) -> move_only & = default;

    move_only(move_only const &) = delete;
    auto operator=(move_only const &) -> move_only & = delete;
};
} // namespace internal_move_only

template <typename I>
struct move_only_iterator: iterator_adaptor_base<move_only_iterator<I>, I>,
                           internal_move_only::move_only {
private:
    using BaseT = iterator_adaptor_base<move_only_iterator<I>, I>;

public:
    explicit constexpr move_only_iterator(I i)

        : BaseT(std::move(i)), move_only()
    {}
};

template <typename Iterator>
ASTL_NODISCARD constexpr auto make_move_only_iterator(Iterator i) -> move_only_iterator<Iterator>
{
    return move_only_iterator<Iterator>{std::move(i)};
}

//LWG-3390
template <typename Iterator>
ASTL_NODISCARD auto make_move_iterator(Iterator i) -> std::move_iterator<Iterator>
{
    return std::move_iterator<Iterator>(std::move(i));
}

} // namespace astl

#endif // ASTL_INCLUDE_ITERATOR_HPP
