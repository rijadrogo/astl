//
// Created by Rijad on 24-Aug-18.
//

#ifndef ASTL_INCLUDE_TEMPORARY_BUFFER_HPP
#define ASTL_INCLUDE_TEMPORARY_BUFFER_HPP

#include <algorithm>
#include <cstddef>
#include <new>
#include <type_traits>
#include <utility>

#include "internal/destroy.hpp"
#include "iterator.hpp"

namespace astl
{
namespace internal_tmp
{
template <typename T, typename... Ts>
auto construct_in_place(T &obj,
                        Ts &&... args) noexcept(std::is_nothrow_constructible<T, Ts...>::value)
    -> T *
{
    return ::new (const_cast<void *>(static_cast<void const volatile *>(std::addressof(obj))))
        T(static_cast<Ts &&>(args)...);
}

// STRUCT TEMPLATE uninitialized_backout
template <typename FwdIt>
// requires FwdIt ForwardIterator
struct uninitialized_backout {
    // struct to undo partially constructed ranges in uninitialized_xxx algorithms
    FwdIt _first;
    FwdIt _last;

    explicit uninitialized_backout(FwdIt dest) noexcept(
        std::is_nothrow_copy_constructible<FwdIt>::value)
        : _first(dest), _last(dest)
    {}

    uninitialized_backout(FwdIt first,
                          FwdIt last) noexcept(std::is_nothrow_move_constructible<FwdIt>::value)
        : _first(std::move(first)), _last(std::move(last))
    {}

    uninitialized_backout(uninitialized_backout const &) = delete;
    auto operator=(uninitialized_backout const &) -> uninitialized_backout & = delete;

    ~uninitialized_backout()
    { // destroy all elements guarded by this instance
        i::destroy(this->_first, this->_last);
    }

    template <typename... Ts>
    auto emplace_back(Ts &&... vals) noexcept(
        std::is_nothrow_constructible<iter_value_type<FwdIt>, Ts...>::value) -> void
    {
        // construct a new element at *_Last and increment
        internal_tmp::construct_in_place(*this->_last, static_cast<Ts &&>(vals)...);
        ++this->_last;
    }

    auto release() noexcept -> FwdIt
    { // suppress any exception handling backout and return last
        _first = _last;
        return _last;
    }
};

template <typename P, typename U>
// requires P ForwardIterator
// requires U, value_type(P) constructible from U
auto initialize_buffer(P data, std::ptrdiff_t const capacity, U const &val) noexcept(
    std::is_nothrow_constructible<iter_value_type<P>, U const &>::value) -> void
{
    if constexpr (std::is_trivially_constructible<U>::value) {
        std::fill(data, data + capacity, val);
    }
    else {
        uninitialized_backout<P> bc(data);
        std::ptrdiff_t i(0);
        while (i != capacity) {
            bc.emplace_back(val);
            ++i;
        }
        bc.release();
    }
}

template <typename U>
// requires U regular type
constexpr auto min_value(U const &left, U const &right) noexcept -> U
{
    // return smaller of _Left and _Right
    return (right < left ? right : left);
}

template <typename U>
// requires U regular type
constexpr auto max_value(U const &left, U const &right) noexcept -> U
{
    // return larger of left and right
    return (left < right ? right : left);
}

template <typename Diff>
// requires Diff integral type
constexpr auto temporary_buffer_size(Diff const value) noexcept -> std::ptrdiff_t
{
    // convert an iterator difference_type to a  std::ptrdiff_t for use in
    // temporary buffers
    using Ct = typename std::common_type<std::ptrdiff_t, Diff>::type;
    return static_cast<std::ptrdiff_t>(
        internal_tmp::min_value(static_cast<Ct>(PTRDIFF_MAX), static_cast<Ct>(value)));
}
} // namespace internal_tmp

// get raw temporary buffer of up to count elements
template <typename T>
ASTL_NODISCARD auto get_temporary_buffer(std::ptrdiff_t count) noexcept
    -> std::pair<T *, std::ptrdiff_t>
{
    if (static_cast<std::size_t>(count) <= static_cast<std::size_t>(-1) / sizeof(T)) {
        while (count > 0) {
            auto const new_size(static_cast<std::size_t>(count) * sizeof(T));
            void *pbuff(operator new(new_size, std::nothrow));
            if (pbuff != nullptr) return std::make_pair(static_cast<T *>(pbuff), count);

            count /= 2;
        }
    }

    return std::make_pair(nullptr, 0);
}

template <typename T> auto return_temporary_buffer(T *const pbuff) noexcept -> void
{
    ::operator delete(pbuff);
}

template <typename T> struct uninitilized_temporary_buffer {
private:
    T *buffer;
    std::ptrdiff_t len;
    std::ptrdiff_t original_len;

public:
    ASTL_NODISCARD auto size() const noexcept -> std::ptrdiff_t { return this->len; }
    ASTL_NODISCARD auto requested_size() const noexcept -> std::ptrdiff_t
    {
        return this->original_len;
    }
    ASTL_NODISCARD auto begin() noexcept -> T * { return this->buffer; }
    ASTL_NODISCARD auto end() noexcept -> T * { return this->buffer + this->len; }

    ASTL_NODISCARD auto begin() const noexcept -> T * { return this->buffer; }
    ASTL_NODISCARD auto end() const noexcept -> T * { return this->buffer + this->len; }

    explicit uninitilized_temporary_buffer(std::ptrdiff_t const requested_size) noexcept
        : original_len(requested_size)
    {
        std::pair<T *, std::ptrdiff_t> const buff(
            astl::get_temporary_buffer<T>(this->original_len));
        this->buffer = buff.first;
        this->len = buff.second;
    }

    template <typename FwdIt, typename Sent, typename = iter_value_type<FwdIt>>
    // requires FwdIt ForwardIterator
    // requires Sent ForwardIterator
    uninitilized_temporary_buffer(FwdIt first, Sent last) noexcept
        : uninitilized_temporary_buffer(astl::distance(first, last))
    {}

    ~uninitilized_temporary_buffer() noexcept { astl::return_temporary_buffer(this->buffer); }

    // Disable copy constructor and assignment operator.
    uninitilized_temporary_buffer(uninitilized_temporary_buffer const &) = delete;
    auto operator=(uninitilized_temporary_buffer const &)
        -> uninitilized_temporary_buffer & = delete;
};

#if HAS_DEDUCTION_GUIDES
template <typename I, typename S>
// requires I ForwardIterator
// requires S ForwardIterator
uninitilized_temporary_buffer(I, S)->uninitilized_temporary_buffer<iter_value_type<I>>;

#endif // HAS_DEDUCTION_GUIDES

template <typename T> struct temporary_buffer: private uninitilized_temporary_buffer<T> {
private:
    using Base = uninitilized_temporary_buffer<T>;

public:
    using Base::begin;
    using Base::end;
    using Base::requested_size;
    using Base::size;

    template <typename U>
    // requires U, T constructible from U
    temporary_buffer(std::ptrdiff_t const requested_size, U const &val) : Base(requested_size)
    {
        internal_tmp::initialize_buffer(this->begin(), this->size(), val);
    }

    template <typename FwdIt, typename Sent, typename = iter_value_type<FwdIt>>
    // requires FwdIt ForwardIterator
    // requires Sent ForwardIterator
    temporary_buffer(FwdIt first, Sent last) : temporary_buffer(astl::distance(first, last), *first)
    {}

    ~temporary_buffer() { i::destroy_n(this->begin(), this->size()); }

    // Disable copy constructor and assignment operator.
    temporary_buffer(temporary_buffer const &) = delete;
    auto operator=(temporary_buffer const &) -> temporary_buffer & = delete;
};

#if HAS_DEDUCTION_GUIDES
template <typename I, typename S>
// requires I ForwardIterator
// requires S ForwardIterator
temporary_buffer(I, S)->temporary_buffer<iter_value_type<I>>;

template <typename T> temporary_buffer(std::ptrdiff_t, T) -> temporary_buffer<T>;

#endif

template <typename T, std::size_t InlineBuffer = 4096>
struct uninitialized_inline_temporary_buffer { // temporary storage with
                                               // _alloca-like attempt
    static constexpr std::size_t optimistic_count =
        internal_tmp::max_value(static_cast<std::size_t>(1), InlineBuffer / sizeof(T));

private:
    std::aligned_union_t<0, T> stack_space[optimistic_count];
    T *data; // points to heap memory iff capacity > optimistic_count
    std::ptrdiff_t capacity;
    std::ptrdiff_t request_size;

public:
    template <typename Diff>
    // requires Diff integral type
    // requires U, T constructible from U
    explicit uninitialized_inline_temporary_buffer(Diff const requested_size) noexcept
        : request_size(requested_size)
    {
        if (static_cast<std::size_t>(requested_size) <= optimistic_count) {
            // unconditionally engage stack space
            this->data = reinterpret_cast<T *>(&this->stack_space[0]);
            this->capacity = static_cast<std::ptrdiff_t>(requested_size);
            return;
        }

        std::ptrdiff_t const attempt(internal_tmp::temporary_buffer_size(requested_size));
        std::pair<T *, std::ptrdiff_t> const buff(astl::get_temporary_buffer<T>(attempt));
        if (static_cast<std::size_t>(buff.second) > optimistic_count) {
            this->data = buff.first;
            this->capacity = buff.second;
            return;
        }

        astl::return_temporary_buffer(buff.first);
        this->data = reinterpret_cast<T *>(&this->stack_space[0]);
        this->capacity = optimistic_count;
    }

    template <typename FwdIt, typename Sent>
    // requires FwdIt ForwardIterator
    // requires Sent ForwardIterator
    uninitialized_inline_temporary_buffer(FwdIt first, Sent last)
        : uninitialized_inline_temporary_buffer(astl::distance(first, last))
    {}

    ~uninitialized_inline_temporary_buffer() noexcept
    { // return temporary
        // storage
        if (static_cast<std::size_t>(capacity) > optimistic_count)
            astl::return_temporary_buffer(this->data);
    }

    ASTL_NODISCARD auto begin() noexcept -> T * { return this->data; }
    ASTL_NODISCARD auto end() noexcept -> T * { return this->data + this->capacity; }

    ASTL_NODISCARD auto begin() const noexcept -> T const * { return this->data; }
    ASTL_NODISCARD auto end() const noexcept -> T const * { return this->data + this->capacity; }

    ASTL_NODISCARD auto size() const noexcept -> std::ptrdiff_t { return this->capacity; }
    ASTL_NODISCARD auto requested_size() const noexcept -> std::ptrdiff_t
    {
        return this->request_size;
    }

    uninitialized_inline_temporary_buffer(uninitialized_inline_temporary_buffer const &) = delete;
    auto operator=(uninitialized_inline_temporary_buffer const &)
        -> uninitialized_inline_temporary_buffer & = delete;
};

#if HAS_DEDUCTION_GUIDES
template <typename I, typename S>
// requires I ForwardIterator
// requires S ForwardIterator
uninitialized_inline_temporary_buffer(I, S)
    ->uninitialized_inline_temporary_buffer<iter_value_type<I>>;
#endif // HAS_DEDUCTION_GUIDES

template <typename T, std::size_t InlineBuffer = 4096>
struct inline_temporary_buffer: private uninitialized_inline_temporary_buffer<T, InlineBuffer> {
    // temporary storage with _alloca-like attempt
private:
    using base_t = uninitialized_inline_temporary_buffer<T, InlineBuffer>;

public:
    using base_t::begin;
    using base_t::end;
    using base_t::requested_size;
    using base_t::size;

    template <typename Diff, typename U>
    // requires Diff integral type
    // requires U, T constructible from U
    inline_temporary_buffer(Diff const requested_size, U const &val) : base_t(requested_size)
    {
        internal_tmp::initialize_buffer(this->begin(), this->size(), val);
    }

    template <typename FwdIt, typename Sent, typename = iter_cat<FwdIt>>
    // requires FwdIt ForwardIterator
    // requires Sent ForwardIterator
    inline_temporary_buffer(FwdIt first, Sent last)
        : inline_temporary_buffer(astl::distance(first, last), *first)
    {}

    ~inline_temporary_buffer() noexcept
    { // return temporary storage
        if (static_cast<std::size_t>(this->size()) > base_t::optimistic_count) {
            i::destroy_n(this->begin(), this->size());
        }
    }

    inline_temporary_buffer(inline_temporary_buffer const &) = delete;
    auto operator=(inline_temporary_buffer const &) -> inline_temporary_buffer & = delete;
};

#if HAS_DEDUCTION_GUIDES
template <typename I, typename S>
// requires I ForwardIterator
// requires S ForwardIterator
inline_temporary_buffer(I, S)->inline_temporary_buffer<iter_value_type<I>>;

template <typename T> inline_temporary_buffer(std::ptrdiff_t, T) -> inline_temporary_buffer<T>;

#endif // HAS_DEDUCTION_GUIDES

} // namespace astl

#endif // ASTL_INCLUDE_TEMPORARY_BUFFER_HPP
