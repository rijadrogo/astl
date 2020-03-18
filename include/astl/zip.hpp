//
// Created by Rijad on 14-Aug-19.
//

#ifndef ASTL_INCLUDE_ZIP_HPP
#define ASTL_INCLUDE_ZIP_HPP

#include <algorithm>
#include <initializer_list>
#include <tuple>
#include <type_traits>

#include "ebo_base.hpp"
#include "functional.hpp"
#include "internal/all_of.hpp"
#include "iterator.hpp"
#include "range_access.hpp"

namespace astl
{
namespace internal_zip
{
using std::declval;

template <typename... Ts> static auto min_el(Ts &&... conts) -> std::size_t
{
    return std::min({adl::size(conts)...});
}

// We have to alias this since inlining the actual type at the usage site in the
// parameter list of iterator_facade_base<> below ICEs MSVC 2017.
template <typename... Iters> struct zip_tuple_type {
    using type = std::tuple<decltype(*declval<Iters>())...>;
};

template <typename ZipType, typename... Iters>
using zip_traits = iterator_facade_base<
    ZipType,
    typename std::common_type<std::random_access_iterator_tag, astl::iter_cat<Iters>...>::type,
    typename zip_tuple_type<Iters...>::type,
    typename std::iterator_traits<
        typename std::tuple_element<0, std::tuple<Iters...>>::type>::difference_type,
    // ^ FIXME: This follows boost::make_zip_iterator's assumption that all
    // inner iterators have the same difference_type. It would fail if, for
    // instance, the second field's difference_type were non-numeric while the
    // first is.
    typename zip_tuple_type<Iters...>::type *, typename zip_tuple_type<Iters...>::type>;

template <typename ZipType, typename... Iters> struct zip_common: zip_traits<ZipType, Iters...> {
    using base = zip_traits<ZipType, Iters...>;
    using value_type = typename base::value_type;
    std::tuple<Iters...> _iterators;

protected:
    template <std::size_t... Ns>
    ASTL_NODISCARD constexpr auto deref(std::index_sequence<Ns...>) const -> value_type
    {
        return value_type(*std::get<Ns>(_iterators)...);
    }

    template <std::size_t... Ns>
    ASTL_NODISCARD constexpr auto tup_inc(std::index_sequence<Ns...>) const -> decltype(_iterators)
    {
        return std::tuple<Iters...>(astl::next(std::get<Ns>(_iterators))...);
    }

    template <std::size_t... Ns>
    ASTL_NODISCARD constexpr auto tup_dec(std::index_sequence<Ns...>) const -> decltype(_iterators)
    {
        return std::tuple<Iters...>(astl::prev(std::get<Ns>(_iterators))...);
    }

    template <std::size_t... Ns>
    ASTL_NODISCARD constexpr auto tup_plus_n(std::index_sequence<Ns...>,
                                             typename base::difference_type n) const
        -> decltype(_iterators)
    {
        return std::tuple<Iters...>(std::plus{}(std::get<Ns>(_iterators), n)...);
    }

    template <std::size_t... Ns>
    ASTL_NODISCARD constexpr auto it_minus_it(std::index_sequence<Ns...>,
                                              const zip_common &it) const ->
        typename base::difference_type
    {
        decltype(std::get<0>(_iterators) - std::get<0>(it._iterators)) arr[]{
            (std::get<Ns>(_iterators) - std::get<Ns>(it._iterators))...};
        return *std::min_element(arr, std::end(arr));
    }

public:
    constexpr explicit zip_common(Iters &&... ts) : _iterators(static_cast<Iters &&>(ts)...) {}

    ASTL_NODISCARD constexpr auto operator*() -> value_type
    {
        return this->deref(std::index_sequence_for<Iters...>{});
    }

    ASTL_NODISCARD constexpr auto operator*() const -> value_type
    {
        return this->deref(std::index_sequence_for<Iters...>{});
    }

    auto operator++() -> ZipType &
    {
        _iterators = this->tup_inc(std::index_sequence_for<Iters...>{});
        return *reinterpret_cast<ZipType *>(this);
    }

    auto operator--() -> ZipType &
    {
        static_assert(base::is_bidirectional,
                      "All inner iterators must be at least bidirectional.");
        _iterators = this->tup_dec(std::index_sequence_for<Iters...>{});
        return *reinterpret_cast<ZipType *>(this);
    }

    template <bool Bidi = false>
    ASTL_NODISCARD auto operator+(typename base::difference_type n) const -> ZipType
    {
        static_assert(base::is_random_access || Bidi,
                      "All inner iterators must random access iterator.");
        auto its(this->tup_plus_n(std::index_sequence_for<Iters...>{}, n));
        return *reinterpret_cast<ZipType *>(its);
    }

    template <bool Bidi = false>
    ASTL_NODISCARD auto operator-(typename base::difference_type n) const -> ZipType
    {
        static_assert(base::is_random_access || Bidi,
                      "All inner iterators must be random access iterator.");
        auto its(this->tup_plus_n(std::index_sequence_for<Iters...>{}, -n));
        return *reinterpret_cast<ZipType *>(its);
    }

    template <bool Bidi = false> auto operator+=(typename base::difference_type n) -> ZipType &
    {
        static_assert(base::is_random_access || Bidi,
                      "All inner iterators must be random access iterator.");
        _iterators = this->tup_plus_n(std::index_sequence_for<Iters...>{}, n);
        return *reinterpret_cast<ZipType *>(this);
    }

    template <bool Bidi = false> auto operator-=(typename base::difference_type n) -> ZipType &
    {
        static_assert(base::is_random_access || Bidi,
                      "All inner iterators must be random access iterator.");
        _iterators = this->tup_plus_n(std::index_sequence_for<Iters...>{}, -n);
        return *reinterpret_cast<ZipType *>(this);
    }

    template <bool IsRand = std::is_convertible<typename base::iterator_category,
                                                std::random_access_iterator_tag>::value>
    constexpr auto operator<(const zip_common &it) const noexcept -> bool
    {
        return _iterators < it._iterators;
    }

    template <bool IsRand = std::is_convertible<typename base::iterator_category,
                                                std::random_access_iterator_tag>::value>
    constexpr auto operator>(const zip_common &it) const noexcept -> bool
    {
        return _iterators > it._iterators;
    }

    template <bool IsRand = std::is_convertible<typename base::iterator_category,
                                                std::random_access_iterator_tag>::value>
    constexpr auto operator-(const zip_common &it) const -> typename base::difference_type
    {
        return this->it_minus_it(std::index_sequence_for<Iters...>{}, it);
    }
};

template <bool Val, typename... Iters>
struct zip_first: zip_common<zip_first<Val, Iters...>, Iters...> {
    using base = zip_common<zip_first<Val, Iters...>, Iters...>;

    ASTL_NODISCARD constexpr auto operator!=(const zip_first<Val, Iters...> &other) const noexcept
        -> bool
    {
        return std::get<0>(this->_iterators) != std::get<0>(other._iterators);
    }

    explicit zip_first(Iters &&... ts) : base(static_cast<Iters &&>(ts)...) {}
};

template <bool HasSize, typename... Iters>
struct zip_shortest: zip_common<zip_shortest<false, Iters...>, Iters...> {
private:
    template <std::size_t... Ns>
    constexpr auto test(const zip_shortest<false, Iters...> &other,
                        std::index_sequence<Ns...>) const noexcept -> bool
    {
        return r::all_of(std::initializer_list<bool>{std::get<Ns>(this->_iterators)
                                                     != std::get<Ns>(other._iterators)...});
    }

public:
    using base = zip_common<zip_shortest<false, Iters...>, Iters...>;

    constexpr explicit zip_shortest(Iters &&... ts) : base(static_cast<Iters &&>(ts)...) {}

    ASTL_NODISCARD constexpr auto operator!=(const zip_shortest<false, Iters...> &other) const
        noexcept -> bool
    {
        return this->test(other, std::index_sequence_for<Iters...>{});
    }
};

template <typename... Iters>
struct zip_shortest<true, Iters...>: zip_common<zip_shortest<true, Iters...>, Iters...> {
private:
    std::size_t s;

public:
    using base = zip_common<zip_shortest<true, Iters...>, Iters...>;

    constexpr explicit zip_shortest(std::size_t const s, Iters &&... ts)
        : base(static_cast<Iters &&>(ts)...), s(s)
    {}

    auto operator++() -> zip_shortest &
    {
        --s;
        base::operator++();
        return *this;
    }

    ASTL_NODISCARD constexpr auto operator!=(const zip_shortest<true, Iters...> &) const noexcept
        -> bool
    {
        return s > 0;
    }
};

template <bool, template <bool, typename...> class ItType, typename... Ts> struct zippy {
    using iterator = ItType<false, astl::begin_t<Ts>...>;
    using iterator_category = typename iterator::iterator_category;
    using value_type = typename iterator::value_type;
    using difference_type = typename iterator::difference_type;
    using pointer = typename iterator::pointer;
    using reference = typename iterator::reference;

private:
    std::tuple<Ts...> ts;

    template <std::size_t... Ns>
    ASTL_NODISCARD constexpr auto begin_impl(std::index_sequence<Ns...>) const -> iterator
    {
        return iterator(adl::begin(std::get<Ns>(ts))...);
    }

    template <std::size_t... Ns>
    ASTL_NODISCARD constexpr auto end_impl(std::index_sequence<Ns...>) const -> iterator
    {
        return iterator(adl::end(std::get<Ns>(ts))...);
    }

public:
    constexpr explicit zippy(Ts &&... ts) : ts(static_cast<Ts &&>(ts)...) {}

    ASTL_NODISCARD constexpr auto begin() const -> iterator
    {
        return this->begin_impl(std::index_sequence_for<Ts...>{});
    }

    ASTL_NODISCARD constexpr auto end() const -> iterator
    {
        return this->end_impl(std::index_sequence_for<Ts...>{});
    }
};

template <template <bool, typename...> class ItType, typename... Ts>
struct zippy<true, ItType, Ts...> {
    using iterator = ItType<true, astl::begin_t<Ts>...>;
    using iterator_category = typename iterator::iterator_category;
    using value_type = typename iterator::value_type;
    using difference_type = typename iterator::difference_type;
    using pointer = typename iterator::pointer;
    using reference = typename iterator::reference;

private:
    std::size_t s;
    std::tuple<Ts...> ts;

    template <std::size_t... Ns>
    ASTL_NODISCARD constexpr auto begin_impl(std::index_sequence<Ns...>) const -> iterator
    {
        return iterator(s, adl::begin(std::get<Ns>(ts))...);
    }

    template <std::size_t... Ns>
    ASTL_NODISCARD constexpr auto end_impl(std::index_sequence<Ns...>) const -> iterator
    {
        return iterator(s, adl::end(std::get<Ns>(ts))...);
    }

public:
    constexpr explicit zippy(Ts &&... ts)
        : s(internal_zip::min_el(ts...)), ts(static_cast<Ts &&>(ts)...)
    {}

    ASTL_NODISCARD constexpr auto begin() const -> iterator
    {
        return this->begin_impl(std::index_sequence_for<Ts...>{});
    }

    ASTL_NODISCARD constexpr auto end() const -> iterator
    {
        return this->end_impl(std::index_sequence_for<Ts...>{});
    }
};

template <typename, typename... Conts> struct has_all_size: std::false_type {};

template <typename... Conts>
struct has_all_size<astl::void_t<decltype(adl::size(std::declval<Conts>()))...>, Conts...>
    : std::true_type {};

template <typename F, typename ZipType, typename... Iters>
struct zip_common_imap: zip_traits<ZipType, Iters...>, F {
    using base = zip_traits<ZipType, Iters...>;
    using value_type = typename base::value_type;
    std::tuple<Iters...> _iterators;

protected:
    template <std::size_t... Ns>
    ASTL_NODISCARD constexpr auto deref(std::index_sequence<Ns...>) const -> value_type
    {
        return ebo_get(*this)(*std::get<Ns>(_iterators)...);
    }

    template <std::size_t... Ns>
    ASTL_NODISCARD constexpr auto deref(std::index_sequence<Ns...>) -> value_type
    {
        return ebo_get(*this)(*std::get<Ns>(_iterators)...);
    }

    template <std::size_t... Ns>
    ASTL_NODISCARD constexpr auto tup_inc(std::index_sequence<Ns...>) const -> decltype(_iterators)
    {
        return std::tuple<Iters...>(astl::next(std::get<Ns>(_iterators))...);
    }

    template <std::size_t... Ns>
    ASTL_NODISCARD constexpr auto tup_dec(std::index_sequence<Ns...>) const -> decltype(_iterators)
    {
        return std::tuple<Iters...>(astl::prev(std::get<Ns>(_iterators))...);
    }

    template <std::size_t... Ns>
    ASTL_NODISCARD constexpr auto tup_plus_n(std::index_sequence<Ns...>,
                                             typename base::difference_type n) const
        -> decltype(_iterators)
    {
        return std::tuple<Iters...>(std::plus{}(std::get<Ns>(_iterators), n)...);
    }

    template <std::size_t... Ns>
    ASTL_NODISCARD constexpr auto it_minus_it(std::index_sequence<Ns...>,
                                              const zip_common_imap &it) const ->
        typename base::difference_type
    {
        decltype(std::get<0>(_iterators) - std::get<0>(it._iterators)) arr[]{
            (std::get<Ns>(_iterators) - std::get<Ns>(it._iterators))...};
        return *std::min_element(arr, std::end(arr));
    }

public:
    template <typename Fn, typename... Is>
    constexpr explicit zip_common_imap(Fn f, Is &&... is)
        : F(static_cast<Fn &&>(f)), _iterators(static_cast<Is &&>(is)...)
    {}

    ASTL_NODISCARD constexpr auto operator*() -> value_type
    {
        return this->deref(std::index_sequence_for<Iters...>{});
    }

    ASTL_NODISCARD constexpr auto operator*() const -> value_type
    {
        return this->deref(std::index_sequence_for<Iters...>{});
    }

    auto operator++() -> ZipType &
    {
        _iterators = this->tup_inc(std::index_sequence_for<Iters...>{});
        return *reinterpret_cast<ZipType *>(this);
    }

    auto operator--() -> ZipType &
    {
        static_assert(base::is_bidirectional,
                      "All inner iterators must be at least bidirectional.");
        _iterators = this->tup_dec(std::index_sequence_for<Iters...>{});
        return *reinterpret_cast<ZipType *>(this);
    }

    template <bool Bidi = false>
    ASTL_NODISCARD auto operator+(typename base::difference_type n) const -> ZipType
    {
        static_assert(base::is_random_access || Bidi,
                      "All inner iterators must random access iterator.");
        auto its(this->tup_plus_n(std::index_sequence_for<Iters...>{}, n));
        return *reinterpret_cast<ZipType *>(its);
    }

    template <bool Bidi = false>
    ASTL_NODISCARD auto operator-(typename base::difference_type n) const -> ZipType
    {
        static_assert(base::is_random_access || Bidi,
                      "All inner iterators must be random access iterator.");
        auto its(this->tup_plus_n(std::index_sequence_for<Iters...>{}, -n));
        return *reinterpret_cast<ZipType *>(its);
    }

    template <bool Bidi = false> auto operator+=(typename base::difference_type n) -> ZipType &
    {
        static_assert(base::is_random_access || Bidi,
                      "All inner iterators must be random access iterator.");
        _iterators = this->tup_plus_n(std::index_sequence_for<Iters...>{}, n);
        return *reinterpret_cast<ZipType *>(this);
    }

    template <bool Bidi = false> auto operator-=(typename base::difference_type n) -> ZipType &
    {
        static_assert(base::is_random_access || Bidi,
                      "All inner iterators must be random access iterator.");
        _iterators = this->tup_plus_n(std::index_sequence_for<Iters...>{}, -n);
        return *reinterpret_cast<ZipType *>(this);
    }

    template <bool IsRand = std::is_convertible<typename base::iterator_category,
                                                std::random_access_iterator_tag>::value>
    constexpr auto operator<(const zip_common_imap &it) const noexcept -> bool
    {
        return _iterators < it._iterators;
    }

    template <bool IsRand = std::is_convertible<typename base::iterator_category,
                                                std::random_access_iterator_tag>::value>
    constexpr auto operator>(const zip_common_imap &it) const noexcept -> bool
    {
        return _iterators > it._iterators;
    }

    template <bool IsRand = std::is_convertible<typename base::iterator_category,
                                                std::random_access_iterator_tag>::value>
    constexpr auto operator-(const zip_common_imap &it) const -> typename base::difference_type
    {
        return this->it_minus_it(std::index_sequence_for<Iters...>{}, it);
    }
};

template <typename F, typename... Iters>
struct zip_imap: zip_common_imap<F, zip_imap<F, Iters...>, Iters...> {
    using base = zip_common_imap<F, zip_imap<F, Iters...>, Iters...>;

    ASTL_NODISCARD constexpr auto operator!=(zip_imap<F, Iters...> const &other) const noexcept
        -> bool
    {
        return std::get<0>(this->_iterators) != std::get<0>(other._iterators);
    }

    template <typename Fn, typename... Is>
    explicit zip_imap(Fn &&f, Is &&... ts)
        : base(static_cast<Fn &&>(f), static_cast<Iters &&>(ts)...)
    {}
};

template <int> struct zi_tag {};

template <typename F, typename... Ts> struct zippy_imap: ebo<zi_tag<0>, F> {
    using base_fun = ebo<zi_tag<0>, F>;
    using iterator = zip_imap<base_fun, astl::begin_t<Ts>...>;
    using iterator_category = typename iterator::iterator_category;
    using value_type = typename iterator::value_type;
    using difference_type = typename iterator::difference_type;
    using pointer = typename iterator::pointer;
    using reference = typename iterator::reference;

private:
    std::tuple<Ts...> ts;

    template <std::size_t... Ns>
    ASTL_NODISCARD constexpr auto begin_impl(std::index_sequence<Ns...>) const -> iterator
    {
        return iterator(ebo_get(*this), adl::begin(std::get<Ns>(ts))...);
    }

    template <std::size_t... Ns>
    ASTL_NODISCARD constexpr auto end_impl(std::index_sequence<Ns...>) const -> iterator
    {
        return iterator(ebo_get(*this), adl::end(std::get<Ns>(ts))...);
    }

public:
    template <typename Fn, typename... Is>
    constexpr explicit zippy_imap(Fn &&f, Is &&... is)
        : base_fun(static_cast<Fn &&>(f)), ts(static_cast<Is &&>(is)...)
    {}

    ASTL_NODISCARD constexpr auto begin() const -> iterator
    {
        return this->begin_impl(std::index_sequence_for<Ts...>{});
    }

    ASTL_NODISCARD constexpr auto end() const -> iterator
    {
        return this->end_impl(std::index_sequence_for<Ts...>{});
    }
};

}// namespace internal_zip

/// zip iterator for two or more iterable types.
template <typename R, typename... Rs,
          bool HasSize = internal_zip::has_all_size<void, R, Rs...>::value>
ASTL_NODISCARD auto zip(R &&t, Rs &&... args)
    -> internal_zip::zippy<HasSize, internal_zip::zip_shortest, R, Rs...>
{
    return internal_zip::zippy<HasSize, internal_zip::zip_shortest, R, Rs...>(
        static_cast<R &&>(t), static_cast<Rs &&>(args)...);
}

/// zip iterator that, for the sake of efficiency, assumes the first iteratee to
/// be the shortest.
template <typename R, typename... Rs>
ASTL_NODISCARD auto zip_first(R &&t, Rs &&... args)
    -> internal_zip::zippy<false, internal_zip::zip_first, R, Rs...>
{
    return internal_zip::zippy<false, internal_zip::zip_first, R, Rs...>(
        static_cast<R &&>(t), static_cast<Rs &&>(args)...);
}

template <typename F, typename T, typename... Ts>
ASTL_NODISCARD auto imap(F &&f, T &&t, Ts &&... args) -> internal_zip::zippy_imap<F, T, Ts...>
{
    return internal_zip::zippy_imap<F, T, Ts...>(static_cast<F &&>(f), static_cast<T &&>(t),
                                                 static_cast<Ts &&>(args)...);
}

}// namespace astl

#endif// ASTL_INCLUDE_ZIP_HPP
