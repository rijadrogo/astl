//
// Created by User on 11-Sep-19.
//

#ifndef ASTL_INCLUDE_FUNCTION_REF_HPP
#define ASTL_INCLUDE_FUNCTION_REF_HPP

#define TL_FUNCTION_REF_VERSION_MAJOR 1
#define TL_FUNCTION_REF_VERSION_MINOR 0
#define TL_FUNCTION_REF_VERSION_PATCH 0

#if (defined(_MSC_VER) && _MSC_VER == 1900)
/// \exclude
#define TL_FUNCTION_REF_MSVC2015
#endif

#if (defined(__GNUC__) && __GNUC__ == 4 && __GNUC_MINOR__ <= 9 && !defined(__clang__))
/// \exclude
#define TL_FUNCTION_REF_GCC49
#endif

#if (defined(__GNUC__) && __GNUC__ == 5 && __GNUC_MINOR__ <= 4 && !defined(__clang__))
/// \exclude
#define TL_FUNCTION_REF_GCC54
#endif

#if (defined(__GNUC__) && __GNUC__ == 4 && __GNUC_MINOR__ <= 9 && !defined(__clang__))
// GCC < 5 doesn't support overloading on const&& for member functions
/// \exclude
#define TL_FUNCTION_REF_NO_CONSTRR
#endif

#if __cplusplus > 201103L
/// \exclude
#define TL_FUNCTION_REF_CXX14
#endif

// constexpr implies const in C++11, not C++14
#if (__cplusplus == 201103L || defined(TL_FUNCTION_REF_MSVC2015)                                   \
     || defined(TL_FUNCTION_REF_GCC49))                                                            \
    && !defined(TL_FUNCTION_REF_GCC54)
/// \exclude
#define TL_FUNCTION_REF_11_CONSTEXPR
#else
/// \exclude
#define TL_FUNCTION_REF_11_CONSTEXPR constexpr
#endif

#include <functional>
#include <utility>

namespace tl
{
namespace detail
{
namespace fnref
{
// C++14-style aliases for brevity
template <class T> using remove_const_t = typename std::remove_const<T>::type;

template <class T> using remove_reference_t = typename std::remove_reference<T>::type;

template <class T> using decay_t = typename std::decay<T>::type;

template <bool E, class T = void> using enable_if_t = typename std::enable_if<E, T>::type;

template <bool B, class T, class F> using conditional_t = typename std::conditional<B, T, F>::type;

// std::invoke from C++17
// https://stackoverflow.com/questions/38288042/c11-14-invoke-workaround
template <typename Fn, typename... Args,
          typename = enable_if_t<std::is_member_pointer<decay_t<Fn>>::value>, int = 0>
constexpr auto
invoke(Fn &&f, Args &&... args) noexcept(noexcept(std::mem_fn(f)(std::forward<Args>(args)...)))
    -> decltype(std::mem_fn(f)(std::forward<Args>(args)...))
{
    return std::mem_fn(f)(std::forward<Args>(args)...);
}

template <typename Fn, typename... Args,
          typename = enable_if_t<!std::is_member_pointer<decay_t<Fn>>{}>>
constexpr auto
invoke(Fn &&f, Args &&... args) noexcept(noexcept(std::forward<Fn>(f)(std::forward<Args>(args)...)))
    -> decltype(std::forward<Fn>(f)(std::forward<Args>(args)...))
{
    return std::forward<Fn>(f)(std::forward<Args>(args)...);
}

// std::invoke_result from C++17
template <class F, class, class... Us> struct invoke_result_impl;

template <class F, class... Us>
struct invoke_result_impl<
    F, decltype(tl::detail::fnref::invoke(std::declval<F>(), std::declval<Us>()...), void()),
    Us...> {
    using type = decltype(tl::detail::fnref::invoke(std::declval<F>(), std::declval<Us>()...));
};

template <class F, class... Us> using invoke_result = invoke_result_impl<F, void, Us...>;

template <class F, class... Us> using invoke_result_t = typename invoke_result<F, Us...>::type;

template <class, class R, class F, class... Args> struct is_invocable_r_impl: std::false_type {};

template <class R, class F, class... Args>
struct is_invocable_r_impl<typename std::is_convertible<invoke_result_t<F, Args...>, R>::type, R, F,
                           Args...>: std::true_type {};

template <class R, class F, class... Args>
using is_invocable_r = is_invocable_r_impl<std::true_type, R, F, Args...>;

}// namespace fnref
}// namespace detail

/// A lightweight non-owning reference to a callable.
///
/// Example usage:
///
/// ```cpp
/// void foo (function_ref<int(int)> func) {
///     std::cout << "Result is " << func(21); //42
/// }
///
/// foo([](int i) { return i*2; });
template <class F> class function_ref;

/// Specialization for function types.
template <class R, class... Args> class function_ref<R(Args...)>
{
public:
    constexpr function_ref() noexcept = delete;

    /// Creates a `function_ref` which refers to the same callable as `rhs`.
    constexpr function_ref(const function_ref<R(Args...)> &rhs) noexcept = default;

    /// Constructs a `function_ref` referring to `f`.
    ///
    /// \synopsis template <typename F> constexpr function_ref(F &&f) noexcept
    template <typename F,
              detail::fnref::enable_if_t<
                  !std::is_same<detail::fnref::decay_t<F>, function_ref>::value
                  && detail::fnref::is_invocable_r<R, F &&, Args...>::value> * = nullptr>
    // NOLINTNEXTLINE(bugprone-forwarding-reference-overload,
    // google-explicit-constructor)
    TL_FUNCTION_REF_11_CONSTEXPR function_ref(F &&f) noexcept
        : obj_(const_cast<void *>(reinterpret_cast<const void *>(std::addressof(f))))
    {
        callback_ = [](void *obj, Args... args) -> R {
            return detail::fnref::invoke(*reinterpret_cast<typename std::add_pointer<F>::type>(obj),
                                         std::forward<Args>(args)...);
        };
    }

    /// Makes `*this` refer to the same callable as `rhs`.
    TL_FUNCTION_REF_11_CONSTEXPR auto operator=(const function_ref<R(Args...)> &rhs) noexcept
        -> function_ref<R(Args...)> & = default;

    /// Makes `*this` refer to `f`.
    ///
    /// \synopsis template <typename F> constexpr function_ref &operator=(F &&f)
    /// noexcept;
    template <typename F,
              detail::fnref::enable_if_t<detail::fnref::is_invocable_r<R, F &&, Args...>::value> * =
                  nullptr>
    TL_FUNCTION_REF_11_CONSTEXPR auto operator=(F &&f) noexcept -> function_ref<R(Args...)> &
    {
        obj_ = reinterpret_cast<void *>(std::addressof(f));
        callback_ = [](void *obj, Args... args) {
            return detail::fnref::invoke(*reinterpret_cast<typename std::add_pointer<F>::type>(obj),
                                         std::forward<Args>(args)...);
        };

        return *this;
    }

    /// Swaps the referred callables of `*this` and `rhs`.
    constexpr void swap(function_ref<R(Args...)> &rhs) noexcept
    {
        std::swap(obj_, rhs.obj_);
        std::swap(callback_, rhs.callback_);
    }

    /// Call the stored callable with the given arguments.
    auto operator()(Args... args) const -> R
    {
        return callback_(obj_, std::forward<Args>(args)...);
    }

private:
    void *obj_ = nullptr;
    R (*callback_)(void *, Args...) = nullptr;
};

/// Swaps the referred callables of `lhs` and `rhs`.
template <typename R, typename... Args>
constexpr void swap(function_ref<R(Args...)> &lhs, function_ref<R(Args...)> &rhs) noexcept
{
    lhs.swap(rhs);
}

#if __cplusplus >= 201703L

namespace detail
{
template <typename> struct function_guide_helper {};

template <typename R, typename T, bool Nx, typename... Ts>
struct function_guide_helper<R (T::*)(Ts...) noexcept(Nx)> {
    using type = R(Ts...);
};

template <typename R, typename T, bool Nx, typename... Ts>
struct function_guide_helper<R (T::*)(Ts...) & noexcept(Nx)> {
    using type = R(Ts...);
};

template <typename R, typename T, bool Nx, typename... Ts>
struct function_guide_helper<R (T::*)(Ts...) const noexcept(Nx)> {
    using type = R(Ts...);
};

template <typename R, typename T, bool Nx, typename... Ts>
struct function_guide_helper<R (T::*)(Ts...) const &noexcept(Nx)> {
    using type = R(Ts...);
};

}// namespace detail

template <typename R, typename... Args> function_ref(R (*)(Args...))->function_ref<R(Args...)>;

template <typename Functor,
          typename Signature =
              typename detail::function_guide_helper<decltype(&Functor::operator())>::type>
function_ref(Functor)->function_ref<Signature>;
#endif
}// namespace tl

namespace astl
{
using namespace tl;// NOLINT(google-build-using-namespace)
}// namespace astl

#endif// ASTL_INCLUDE_FUNCTION_REF_HPP
