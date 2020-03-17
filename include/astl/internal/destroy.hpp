//
// Created by Rijad on 30-Jul-18.
//

#ifndef ASTL_INCLUDE_DESTROY_HPP
#define ASTL_INCLUDE_DESTROY_HPP

#include <memory>
#include <type_traits>

#include "astl/iterator.hpp"
#include "astl/range_access.hpp"

namespace astl
{
namespace internal_destroy
{
template <typename Cont> auto clear(Cont &c, internal_adl::rank<1>) -> decltype(c.clear())
{

    return c.clear();
}

template <typename Cont> auto clear(Cont &, internal_adl::rank<0>) noexcept -> void {}

} // namespace internal_destroy

namespace i
{
inline constexpr struct {
    template <typename InIt>
    // requires InIt InputIterator
    auto operator()(InIt first, InIt last) const
        noexcept(std::is_nothrow_destructible<astl::iter_value_type<InIt>>::value) -> void
    {
        // precondition: value_type(InIt) pointer to something heap allocated with new
        while (first != last) {
            delete *first;
            ++first;
        }
    }

} delete_pointers{};

inline constexpr struct {
    template <typename InIt, typename N>
    // requires InIt InputIterator
    // requires N integral type
    auto operator()(InIt first, N n) const
        noexcept(std::is_nothrow_destructible<astl::iter_value_type<InIt>>::value) -> InIt
    {
        // precondition: value_type(InIt) pointer to something heap allocated with new
        while (n != N(0)) {
            delete *first;
            ++first;
            --n;
        }
        return first;
    }
} delete_pointers_n{};

inline constexpr struct {
    template <typename FwdIt>
    auto operator()(FwdIt first, FwdIt last) const
        noexcept(std::is_nothrow_destructible<astl::iter_value_type<FwdIt>>::value) -> void
    {
        constexpr bool trivial_dest = std::is_trivially_destructible<iter_value_type<FwdIt>>::value;
        if constexpr (!trivial_dest) {
            using T = iter_value_type<FwdIt>;
            while (first != last) {
                first->~T();
                ++first;
            }
        }
        else {
            (void) first;
            (void) last;
        }
    }

    template <typename FwdIt, typename Alloc>
    auto operator()(FwdIt first, FwdIt last, Alloc &alloc) const -> void
    {
        using Traits = std::allocator_traits<Alloc>;
        while (first != last) {
            Traits::destroy(alloc, std::addressof(*first));
            ++first;
        }
    }
} destroy{};

inline constexpr struct {
    template <typename T>
    auto operator()(T &location) const noexcept(std::is_nothrow_destructible<T>::value) -> void
    {
        location.~T();
    }

    template <typename T>
    auto operator()(T *const location) const noexcept(std::is_nothrow_destructible<T>::value)
        -> void
    {
        location->~T();
    }
} destroy_at{};

inline constexpr struct {
    template <typename FwdIt, typename N>
    auto operator()(FwdIt first, N n) const
        noexcept(std::is_nothrow_destructible<astl::iter_value_type<FwdIt>>::value) -> FwdIt
    {
        constexpr bool trivial_dest = std::is_trivially_destructible<iter_value_type<FwdIt>>::value;
        if constexpr (trivial_dest) { // nothing to do just advance iterator
            return astl::next(first, n);
        }
        else { // call destructor for all elements
            using T = typename std::iterator_traits<FwdIt>::value_type;
            while (n != N(0)) {
                first->~T();
                ++first;
                --n;
            }
            return first;
        }
    }

    template <typename FwdIt, typename N, typename Alloc>
    auto operator()(FwdIt first, N n, Alloc &alloc) const -> FwdIt
    {
        using Traits = std::allocator_traits<Alloc>;
        while (n != N(0)) {
            Traits::destroy(alloc, std::addressof(*first));
            ++first;
            --n;
        }
        return first;
    }
} destroy_n{};
} // namespace i

namespace r
{
inline constexpr struct {
    template <typename Cont>
    // requires Cont InputIterator range
    auto operator()(Cont &c) const
        noexcept(std::is_nothrow_destructible<astl::range_value_type<Cont>>::value) -> void
    {
        // precondition: value_type(Cont) pointer to something heap allocated with new
        for (auto v : c) delete v;

        internal_destroy::clear(c, internal_adl::rank<1>{});
    }
} delete_pointers{};

inline constexpr struct {
    template <typename Cont, typename N>
    // requires Cont InputIterator range
    auto operator()(Cont &c, N n) const
        noexcept(std::is_nothrow_destructible<astl::range_value_type<Cont>>::value)
            -> iter_of_range<Cont>
    {
        // precondition: value_type(Cont) pointer to something heap allocated with new
        return i::delete_pointers_n(adl::begin(c), n);
    }
} delete_pointers_n{};

inline constexpr struct {
    template <typename R>
    auto operator()(R &&r) const
        noexcept(std::is_nothrow_destructible<astl::range_value_type<R>>::value) -> void
    {
        i::destroy(adl::begin(r), adl::end(r));
    }

    template <typename R, typename Alloc> auto operator()(R &&r, Alloc &alloc) const -> void
    {
        i::destroy(adl::begin(r), adl::end(r), alloc);
    }

} destroy{};

inline constexpr struct {
    template <typename R, typename N>
    auto operator()(R &&r, N n) const
        noexcept(std::is_nothrow_destructible<astl::range_value_type<R>>::value) -> iter_of_range<R>
    {
        return i::destroy_n(adl::begin(r), n);
    }

    template <typename R, typename N, typename Alloc>
    auto operator()(R &&r, N n, Alloc &alloc) const -> iter_of_range<R>
    {
        return i::destroy_n(adl::begin(r), n, alloc);
    }
} destroy_n{};
} // namespace r
} // namespace astl

#endif // ASTL_INCLUDE_DESTROY_HPP
