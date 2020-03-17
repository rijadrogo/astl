//
// Created by Rijad on 29-Jul-18.
//

#ifndef ASTL_INCLUDE_FUNCTIONAL_HPP
#define ASTL_INCLUDE_FUNCTIONAL_HPP

#include <cassert>
#include <cstdlib>
#include <tuple>
#include <type_traits>

#include "ebo_base.hpp"

#define ZX_NOEX_DECL_RET(...)                                                                      \
    noexcept(noexcept(__VA_ARGS__))->decltype(__VA_ARGS__) { return __VA_ARGS__; }

namespace astl
{
template <typename...> using void_t = void;

namespace internal_callable
{
template <typename DecayedFp> struct member_pointer_class_type;

template <typename Ret, typename ClassType> struct member_pointer_class_type<Ret ClassType::*> {
    using type = ClassType;
};

// TEMPLATE FUNCTION invoke
struct invoker_pmf_object { // INVOKE a pointer to member function on an object
    template <typename Decayed, typename T, typename... Ts>
    static constexpr auto call(Decayed pmf, T &&obj, Ts &&... xs)
        ZX_NOEX_DECL_RET((static_cast<T &&>(obj).*pmf)(static_cast<Ts &&>(xs)...))
};

struct invoker_pmf_refwrap { // INVOKE a pointer to member function on a
                             // reference_wrapper
    template <typename Decayed, typename T, typename... Ts>
    static constexpr auto call(Decayed pmf, T &&ref_wrap, Ts &&... xs)
        ZX_NOEX_DECL_RET((static_cast<T &&>(ref_wrap).get().*pmf)(static_cast<Ts &&>(xs)...))
};

struct invoker_pmf_pointer { // INVOKE a pointer to member function on a [smart]
                             // pointer
    template <typename Decayed, typename T, typename... Ts>
    static constexpr auto call(Decayed pmf, T &&obj, Ts &&... xs)
        ZX_NOEX_DECL_RET(((*static_cast<T &&>(obj)).*pmf)(static_cast<Ts &&>(xs)...))
};

struct invoker_pmd_object { // INVOKE a pointer to member data on an object
    template <typename Decayed, typename T>
    static constexpr auto call(Decayed pmd, T &&obj) ZX_NOEX_DECL_RET(static_cast<T &&>(obj).*pmd)
};

struct invoker_pmd_refwrap { // INVOKE a pointer to member data on a
                             // reference_wrapper
    template <typename Decayed, typename T>
    static constexpr auto call(Decayed pmd, T &&ref_wrap)
        ZX_NOEX_DECL_RET(static_cast<T &&>(ref_wrap).get().*pmd)
};

struct invoker_pmd_pointer { // INVOKE a pointer to member data on a [smart]
                             // pointer
    template <typename Decayed, typename T>
    static constexpr auto call(Decayed pmd, T &&obj)
        ZX_NOEX_DECL_RET((*static_cast<T &&>(obj)).*pmd)
};

struct invoker_functor { // INVOKE a function / function object
    template <typename Callable, typename... Ts>
    static constexpr auto call(Callable &&f, Ts &&... xs)
        ZX_NOEX_DECL_RET(static_cast<Callable &&>(f)(static_cast<Ts &&>(xs)...))
};

template <bool B>
struct if_impl // NOLINT(readability-identifier-naming)
{
    template <typename True, typename /* False */> using type = True;
};

template <>
struct if_impl<false> // NOLINT(readability-identifier-naming)
{
    template <typename /* True */, typename False> using type = False;
};

template <bool Cond, typename TrueT, typename FalseT>
using conditional_t = typename if_impl<Cond>::template type<TrueT, FalseT>;

template <typename T, typename = void> struct is_dereferenceable: std::false_type {};

template <typename T>
struct is_dereferenceable<T, void_t<decltype(*std::declval<T>())>>: std::true_type {};

// function object
template <typename Callable, typename T, typename Decayed = typename std::decay<Callable>::type,
          bool IsPmf = std::is_member_function_pointer<Decayed>::value,
          bool IsPmd = std::is_member_object_pointer<Decayed>::value>
struct invoker1: invoker_functor /* <..., false, false> */
{};

// pointer to member function
template <typename Callable, typename T, typename Decayed>
struct invoker1<Callable, T, Decayed, true, false>
    : if_impl<std::is_base_of<typename member_pointer_class_type<Decayed>::type,
                              typename std::decay<T>::type>::value>::
          template type<invoker_pmf_object,
                        typename if_impl<is_dereferenceable<T>::value>::template type<
                            invoker_pmf_pointer, invoker_pmf_refwrap>> {};

// pointer to member data
template <typename Callable, typename T, typename Decayed>
struct invoker1<Callable, T, Decayed, false, true>
    : if_impl<std::is_base_of<typename member_pointer_class_type<Decayed>::type,
                              typename std::decay<T>::type>::value>::
          template type<invoker_pmd_object,
                        typename if_impl<is_dereferenceable<T>::value>::template type<
                            invoker_pmd_pointer, invoker_pmd_refwrap>> {};

template <typename Callable, typename... Ts> struct invoker;

// zero arguments
template <typename Callable> struct invoker<Callable>: invoker_functor {};

// one or more arguments
template <typename Callable, typename T, typename... Ts>
struct invoker<Callable, T, Ts...>: invoker1<Callable, T> {};
} // namespace internal_callable

using internal_callable::conditional_t;

template <bool Cond, typename TrueT, typename FalseT>
using if_t = typename internal_callable::if_impl<Cond>::template type<TrueT, FalseT>;

namespace internal_in
{
// ReSharper disable once CppFunctionIsNotImplemented
template <typename To> void implicitly_convert_to(To) noexcept;

template <typename From, typename To, bool = std::is_convertible<From, To>::value>
struct is_nothrow_convertible
    : std::integral_constant<bool,
                             noexcept(implicitly_convert_to<To>(
                                 std::declval<From>()))> { // determine whether From is
                                                           // nothrow-convertible to To
};

template <typename From, typename To>
struct is_nothrow_convertible<From, To, false>
    : std::false_type { // determine whether From is nothrow-convertible to To
};

template <typename Result, typename Ret, bool = std::is_void<Ret>::value>
struct is_invocable_r_impl: std::false_type {};

template <typename Result, typename Ret>
struct is_invocable_r_impl<Result, Ret, /* is_void<_Ret> = */ false> {
private:
    // ReSharper disable CppFunctionIsNotImplemented

    // The type of the INVOKE expression.
    // Unlike declval, this doesn't add_rvalue_reference.
    static auto get() -> Result;

    template <typename T> static auto conv(T) -> void;

    // This overload is viable if INVOKE(f, args...) can convert to T.
    template <typename T, typename = decltype(conv<T>(get()))>
    static auto test(int) -> std::true_type;

    template <typename T> static auto test(...) -> std::false_type;

    // ReSharper restore CppFunctionIsNotImplemented
public:
    using type = decltype(test<Ret>(1));
};

template <typename Void, typename... Ts>
struct invoke_traits { // selected when Callable isn't callable with Ts
    using is_invocable = std::false_type;
    using is_nothrow_invocable = std::false_type;
    template <typename /* Rx */> using is_invocable_r = std::false_type;
    template <typename /* Rx */> using is_nothrow_invocable_r = std::false_type;
};

template <typename... Ts>
struct invoke_traits<
    void_t<decltype(internal_callable::invoker<Ts...>::call(std::declval<Ts>()...))>,
    Ts...> { // selected when Callable is callable with Ts
    using type = decltype(internal_callable::invoker<Ts...>::call(std::declval<Ts>()...));

    using is_invocable = std::true_type;

    using is_nothrow_invocable = std::integral_constant<
        bool, noexcept(internal_callable::invoker<Ts...>::call(std::declval<Ts>()...))>;

    template <typename Rx> using is_invocable_r = typename is_invocable_r_impl<type, Rx>::type;

    template <typename Rx>
    using is_nothrow_invocable_r =
        std::integral_constant<bool,
                               is_nothrow_invocable::value
                                   && (std::is_void<Rx>::value
                                       || is_nothrow_convertible<type, Rx>::value)>;
};

template <typename Rx, typename... Ts>
using is_invocable_r_ = typename invoke_traits<void, Ts...>::template is_invocable_r<Rx>;

} // namespace internal_in

template <typename Callable, typename... Ts>
struct invoke_result: internal_in::invoke_traits<void, Callable,
                                                 Ts...> { // determine the result type of
                                                          // invoking Callable with Ts
};

template <typename... Ts>
using invoke_result_t = typename internal_in::invoke_traits<void, Ts...>::type;

// STRUCT TEMPLATE is_invocable
template <typename Callable, typename... Ts>
struct is_invocable
    : internal_in::invoke_traits<
          void, Callable, Ts...>::is_invocable { // determines whether Callable is callable with Ts
};

// STRUCT TEMPLATE is_nothrow_invocable
template <typename Callable, typename... Ts>
struct is_nothrow_invocable
    : internal_in::invoke_traits<void, Callable,
                                 Ts...>::is_nothrow_invocable { // determines whether Callable is
                                                                // nothrow-callable with Ts
};

// STRUCT TEMPLATE is_invocable_r
template <typename Rx, typename Callable, typename... Ts>
struct is_invocable_r
    : internal_in::is_invocable_r_<Rx, Callable, Ts...> { // determines whether Callable is callable
                                                          // with Ts and return type Rx
};

// STRUCT TEMPLATE is_nothrow_invocable_r
template <typename Rx, typename Callable, typename... Ts>
struct is_nothrow_invocable_r
    : internal_in::invoke_traits<void, Callable, Ts...>::template is_nothrow_invocable_r<
          Rx> { // determines whether Callable
                // is nothrow-callable with Ts
                // and return type Rx
};

// INVOKE function object
constexpr struct {
    template <typename Callable, typename... Ts>
    constexpr auto operator()(Callable &&f, Ts &&... xs) const
        noexcept(is_nothrow_invocable<Callable, Ts...>::value) -> invoke_result_t<Callable, Ts...>
    {
        return internal_callable::invoker<Callable, Ts...>::call(static_cast<Callable &&>(f),
                                                                 static_cast<Ts &&>(xs)...);
    }
} invoke{};

template <typename Callable>
ASTL_NODISCARD auto not_fn(Callable &&fn) noexcept(
    std::is_nothrow_constructible<std::decay_t<Callable>, Callable>::value)
{
    return [fn(static_cast<Callable &&>(fn))](auto &&... xs)
        ZX_NOEX_DECL_RET(!fn(static_cast<decltype(xs)>(xs)...));
}

namespace internal_fun
{
// FUNCTION TEMPLATE pass_fn
template <typename F> struct ref_fn { // pass function object by value as a reference
private:
    F &f;

public:
    using is_transparent = int;

    explicit constexpr ref_fn(F &f) noexcept : f(f) {}

    template <typename... Ts>
    constexpr auto operator()(Ts &&... xs) const ZX_NOEX_DECL_RET(f(static_cast<Ts &&>(xs)...))
};

template <typename F>
inline constexpr bool pass_by_value_treshold_v = sizeof(F) < 2 * sizeof(void *)
    && std::is_trivially_copy_constructible<F>::value &&std::is_trivially_destructible<F>::value;

template <typename F> using pass_fn_t = conditional_t<pass_by_value_treshold_v<F>, F, ref_fn<F>>;

} // namespace internal_fun

template <typename F>
ASTL_NODISCARD constexpr auto pass_fn(F f) noexcept ->
    typename std::enable_if<internal_fun::pass_by_value_treshold_v<F>, F>::type
{ // pass functor by value
    return f;
}

template <typename F>
ASTL_NODISCARD constexpr auto pass_fn(F &f) noexcept ->
    typename std::enable_if<!internal_fun::pass_by_value_treshold_v<F>,
                            internal_fun::ref_fn<F>>::type
{ // pass functor by
    // "reference"
    return {f};
}

/// Function object to check whether the first component of a pair compares less
/// than the first component of another pair.
struct less_first {
    template <typename T>
    ASTL_NODISCARD constexpr auto operator()(T const &lhs, T const &rhs) const
        ZX_NOEX_DECL_RET(lhs.first < rhs.first);

    template <typename T>
    ASTL_NODISCARD constexpr auto operator()(T const &lhs, T const &rhs) const
        ZX_NOEX_DECL_RET(lhs.first() < rhs.first());
};

/// Function object to check whether the second component of a pair compares
/// less than the second component of another pair.
struct less_second {
    template <typename T>
    ASTL_NODISCARD constexpr auto operator()(T const &lhs, T const &rhs) const
        ZX_NOEX_DECL_RET(lhs.second < rhs.second);

    template <typename T>
    ASTL_NODISCARD constexpr auto operator()(T const &lhs, T const &rhs) const
        ZX_NOEX_DECL_RET(lhs.second() < rhs.second());
};

struct free_deleter {
    auto operator()(void *p) const noexcept -> void { std::free(p); }
};

/// Functor that adapts to any other functor after dereferencing operands.
template <typename F> ASTL_NODISCARD auto deref(F &&f)
{
    return [f(static_cast<F &&>(f))](auto &&... xs)
        ZX_NOEX_DECL_RET(f(*static_cast<decltype(xs)>(xs)...));
}

template <typename F, typename G> ASTL_NODISCARD auto compose(F &&f, G &&g)
{
    return [f(static_cast<F &&>(f)), g(static_cast<G &&>(g))](auto &&... xs) -> decltype(auto) {
        return f(g(static_cast<decltype(xs)>(xs)...));
    };
}

template <typename F, typename G> ASTL_NODISCARD auto combine(F f, G g)
{
    return [f(std::move(f)), g(std::move(g))](auto &&... xs) -> decltype(auto) {
        return f(invoke(g, static_cast<decltype(xs)>(xs))...);
    };
}

template <typename F> ASTL_NODISCARD auto transpose(F &&f)
{
    return [f(static_cast<F &&>(f))](auto &&x, auto &&y, auto &&... z) -> decltype(auto) {
        return f(static_cast<decltype(y)>(y), static_cast<decltype(x)>(x),
                 static_cast<decltype(z)>(z)...);
    };
}

template <typename F> ASTL_NODISCARD auto lockstep(F &&f)
{
    return [f(static_cast<F &&>(f))](auto &&... gs) {
        return [f, gs...](auto &&... xs) -> decltype(auto) {
            return f(invoke(gs, static_cast<decltype(xs)>(xs))...);
        };
    };
}

template <typename F, typename... Gs> ASTL_NODISCARD auto lockstep(F &&f, Gs &&... gs)
{
    return [f(static_cast<F &&>(f)), gs...](auto &&... xs) -> decltype(auto) {
        return f(invoke(gs, static_cast<decltype(xs)>(xs))...);
    };
}

template <typename F, typename... Ts> ASTL_NODISCARD auto bind_front(F &&f, Ts &&... xs)
{
    return [f(static_cast<F &&>(f)), xs...](auto &&... ys) -> decltype(auto) {
        return f(xs..., static_cast<decltype(ys)>(ys)...);
    };
}

template <typename F, typename... Ts> ASTL_NODISCARD auto bind_back(F &&f, Ts &&... xs)
{
    return [f(static_cast<F &&>(f)), xs...](auto &&... ys) -> decltype(auto) {
        return f(static_cast<decltype(ys)>(ys)..., xs...);
    };
}

template <typename F, typename ArgT> ASTL_NODISCARD auto bind1st(F func, ArgT &&left)
{
    // return a binder1st functor adapter
    struct binder {
        ArgT _val;
    };
    return [func(std::move(func)), left(binder{static_cast<ArgT &&>(left)})](
               auto &&right) -> decltype(func(left, static_cast<decltype(right)>(right))) {
        return func(left._val, static_cast<decltype(right)>(right));
    };
}
// FUNCTION TEMPLATE bind2nd
template <typename F, typename ArgT> ASTL_NODISCARD auto bind2nd(F func, ArgT &&right)
{
    // return a binder2nd functor adapter
    struct binder {
        ArgT _val;
    };
    return [func(std::move(func)), right(binder{static_cast<ArgT &&>(right)})](
               auto &&left) -> decltype(func(static_cast<decltype(left)>(left), right)) {
        return func(static_cast<decltype(left)>(left), right._val);
    };
}

struct select1st {
    template <typename PairLike>
    ASTL_NODISCARD constexpr auto operator()(PairLike &&p) const noexcept
        -> decltype((static_cast<PairLike &&>(p).first))
    {
        return static_cast<PairLike &&>(p).first;
    }

    template <typename PairLike>
    ASTL_NODISCARD constexpr auto operator()(PairLike &&p) const
        ZX_NOEX_DECL_RET(static_cast<PairLike &&>(p).first());
};

struct select2nd {
    template <typename PairLike>
    ASTL_NODISCARD constexpr auto operator()(PairLike &&p) const noexcept
        -> decltype((static_cast<PairLike &&>(p).second))
    {
        return static_cast<PairLike &&>(p).second;
    }

    template <typename PairLike>
    ASTL_NODISCARD constexpr auto operator()(PairLike &&p) const
        ZX_NOEX_DECL_RET(static_cast<PairLike &&>(p).second());
};

struct project1st {
    template <typename T, typename... Ts>
    ASTL_NODISCARD constexpr auto operator()(T &&x, Ts &&...) const noexcept -> T &&
    {
        return static_cast<T &&>(x);
    }
};

struct project2nd {
    template <typename T1, typename T2, typename... Ts>
    ASTL_NODISCARD constexpr auto operator()(T1 &&, T2 &&x, Ts &&...) const noexcept -> T2 &&
    {
        return static_cast<T2 &&>(x);
    }
};

namespace internal_tuple
{
template <typename... Ts> struct cat_seq;

template <std::size_t... Is> struct cat_seq<std::index_sequence<Is...>> {
    using type = std::index_sequence<Is...>;
};

template <std::size_t... Is1, std::size_t... Is2>
struct cat_seq<std::index_sequence<Is1...>, std::index_sequence<Is2...>> {
    using type = std::index_sequence<Is1..., Is2...>;
};

template <std::size_t... Is1, std::size_t... Is2, std::size_t... Is3>
struct cat_seq<std::index_sequence<Is1...>, std::index_sequence<Is2...>,
               std::index_sequence<Is3...>> {
    using type = std::index_sequence<Is1..., Is2..., Is3...>;
};

template <std::size_t... Is1, std::size_t... Is2, std::size_t... Is3, std::size_t... Is4>
struct cat_seq<std::index_sequence<Is1...>, std::index_sequence<Is2...>,
               std::index_sequence<Is3...>, std::index_sequence<Is4...>> {
    using type = std::index_sequence<Is1..., Is2..., Is3..., Is4...>;
};

template <std::size_t... Is1, std::size_t... Is2, std::size_t... Is3, std::size_t... Is4,
          std::size_t... Is5>
struct cat_seq<std::index_sequence<Is1...>, std::index_sequence<Is2...>,
               std::index_sequence<Is3...>, std::index_sequence<Is4...>,
               std::index_sequence<Is5...>> {
    using type = std::index_sequence<Is1..., Is2..., Is3..., Is4..., Is5...>;
};

template <typename Is1, typename Is2, typename Is3, typename Is4, typename Is5, typename... Iss>
struct cat_seq<Is1, Is2, Is3, Is4, Is5, Iss...> {
    using type = typename cat_seq<typename cat_seq<Is1, Is2, Is3, Is4, Is5>::type, Iss...>::type;
};

template <typename... Iss> using cat_seq_t = typename cat_seq<Iss...>::type;

template <std::size_t N, typename T> struct gen_outter;

template <std::size_t N, std::size_t... Is> struct gen_outter<N, std::index_sequence<Is...>> {
    using type = std::index_sequence<(Is, N)...>;
};

template <typename T> using remove_cvref_t = std::remove_cv_t<std::remove_reference_t<T>>;

namespace adl
{
using std::get;
template <std::size_t I, typename T>
constexpr auto get(T &&t) -> decltype(get<I>(static_cast<T &&>(t)))
{
    return get<I>(static_cast<T &&>(t));
}
} // namespace adl

template <typename F, typename Tuple, std::size_t... Is>
constexpr auto apply1(F &&f, Tuple &&t, std::index_sequence<Is...>)
    -> decltype(invoke(static_cast<F &&>(f), std::get<Is>(static_cast<Tuple &&>(t))...))
{
    return invoke(static_cast<F &&>(f), std::get<Is>(static_cast<Tuple &&>(t))...);
}

template <typename F, typename Tuple, std::size_t... In, std::size_t... Ou>
constexpr auto apply1(F &&f, Tuple &&t, std::index_sequence<In...>, std::index_sequence<Ou...>)
    -> decltype(invoke(
        static_cast<F &&>(f),
        std::get<Ou>(std::get<In>(std::move(t)))...)) // NOLINT(bugprone-move-forwarding-reference)
{
    // t = std::forward_as_tuple(args...), this is known to be rvalue reference so
    // we move from it, NOLINTNEXTLINE(bugprone-move-forwarding-reference)
    return invoke(static_cast<F &&>(f), adl::get<Ou>(std::get<In>(std::move(t)))...);
}

template <typename Tuple, std::size_t... In, std::size_t... Ou,
          typename RetType = std::tuple<std::tuple_element_t<
              Ou, std::remove_reference_t<std::tuple_element_t<In, Tuple>>>...>>
constexpr auto tuple_cat1(Tuple &&t, std::index_sequence<In...>, std::index_sequence<Ou...>)
    -> RetType
{
    // t = std::forward_as_tuple(args...), this is known to be rvalue reference so
    // we move from it NOLINTNEXTLINE(bugprone-move-forwarding-reference)
    return RetType{adl::get<Ou>(std::get<In>(std::move(t)))...};
}

template <int> struct op_tag {};

template <int> struct of_tag {};

template <int> struct os_tag {};

template <typename T, typename... U> struct flatten_tuples;

template <std::size_t... Is, typename... Ts>
struct flatten_tuples<std::index_sequence<Is...>, Ts...> {
    using outter =
        cat_seq_t<std::make_index_sequence<std::tuple_size<remove_cvref_t<Ts>>::value>...>;
    using inner = cat_seq_t<typename gen_outter<
        Is, std::make_index_sequence<std::tuple_size<remove_cvref_t<Ts>>::value>>::type...>;
};

} // namespace internal_tuple

template <typename F, typename... Tuples,
          typename Indecies =
              internal_tuple::flatten_tuples<std::index_sequence_for<Tuples...>, Tuples...>>
constexpr auto apply(F &&f, Tuples &&... ts)
    -> decltype(internal_tuple::apply1(static_cast<F &&>(f),
                                       std::forward_as_tuple(static_cast<Tuples &&>(ts)...),
                                       typename Indecies::inner{}, typename Indecies::outter{}))
{
    return internal_tuple::apply1(static_cast<F &&>(f),
                                  std::forward_as_tuple(static_cast<Tuples &&>(ts)...),
                                  typename Indecies::inner{}, typename Indecies::outter{});
}

template <typename F, typename Tuple>
constexpr auto apply(F &&f, Tuple &&t) -> decltype(internal_tuple::apply1(
    static_cast<F &&>(f), static_cast<Tuple &&>(t),
    std::make_index_sequence<std::tuple_size<internal_tuple::remove_cvref_t<Tuple>>::value>{}))
{
    return internal_tuple::apply1(
        static_cast<F &&>(f), static_cast<Tuple &&>(t),
        std::make_index_sequence<std::tuple_size<internal_tuple::remove_cvref_t<Tuple>>::value>{});
}

/*
 * Difference between std::tuple_cat and astl::tuple_cat,
 * std::tuple_cat works with std::array, std::pair and std::tuple
 * astl::tuple_cat works with every type that has specialized
 * std::tuple_element, std::tuple_size and free function get<Index>(), found via adl
 */
template <typename... Tuples,
          typename Indecies =
              internal_tuple::flatten_tuples<std::index_sequence_for<Tuples...>, Tuples...>>
constexpr auto tuple_cat(Tuples &&... ts)
    -> decltype(internal_tuple::tuple_cat1(std::forward_as_tuple(static_cast<Tuples &&>(ts)...),
                                           typename Indecies::inner{}, typename Indecies::outter{}))
{
    return internal_tuple::tuple_cat1(std::forward_as_tuple(static_cast<Tuples &&>(ts)...),
                                      typename Indecies::inner{}, typename Indecies::outter{});
}

template <typename Tuple>
constexpr auto tuple_cat(Tuple &&t) -> internal_tuple::remove_cvref_t<Tuple>
{
    return static_cast<Tuple &&>(t);
}

template <typename F, typename... Ps>
constexpr auto on_first(F &&f, Ps &&... ps)
    -> decltype(static_cast<F &&>(f)(select1st{}(static_cast<Ps &&>(ps))...))
{
    return static_cast<F &&>(f)(select1st{}(static_cast<Ps &&>(ps))...);
}

template <typename F> struct on_first_t: private ebo<internal_tuple::of_tag<0>, F> {
private:
    using base_type = ebo<internal_tuple::of_tag<0>, F>;

public:
    using base_type::base_type;

    template <typename... Pairs>
    constexpr auto operator()(Pairs &&... ps) const
        -> decltype(astl::on_first(ebo_get(std::declval<base_type const &>()),
                                   static_cast<Pairs &&>(ps)...))
    {
        return astl::on_first(ebo_get(*this), static_cast<Pairs &&>(ps)...);
    }
};

#if HAS_DEDUCTION_GUIDES
template <typename F> on_first_t(F) -> on_first_t<F>;
#endif // HAS_DEDUCTION_GUIDES

template <typename F, typename... Ps>
constexpr auto on_second(F &&f, Ps &&... ps)
    -> decltype(static_cast<F &&>(f)(select2nd{}(static_cast<Ps &&>(ps))...))
{
    return static_cast<F &&>(f)(select2nd{}(static_cast<Ps &&>(ps))...);
}

template <typename F> struct on_second_t: private ebo<internal_tuple::os_tag<0>, F> {
private:
    using base_type = ebo<internal_tuple::os_tag<0>, F>;

public:
    using base_type::base_type;
    template <typename... PairLike>
    constexpr auto operator()(PairLike &&... p) const
        -> decltype(astl::on_second(ebo_get(std::declval<base_type const &>()),
                                    static_cast<PairLike &&>(p)...))
    {
        return astl::on_second(ebo_get(*this), static_cast<PairLike &&>(p)...);
    }
};

#if HAS_DEDUCTION_GUIDES
template <typename F> on_second_t(F) -> on_second_t<F>;
#endif

namespace internal
{
template <typename F, typename T, T... Is>
constexpr auto apply_index1(F &&f, std::integer_sequence<T, Is...>)
    -> decltype(f(std::integral_constant<int, Is>{}...))
{
    return f(std::integral_constant<int, Is>{}...);
}

template <typename I1, typename I2> struct ind_seq_split;

template <std::size_t... Idxs1, std::size_t... Idxs2>
struct ind_seq_split<std::index_sequence<Idxs1...>, std::index_sequence<Idxs2...>> {
    using type = std::index_sequence<(sizeof...(Idxs1) + Idxs2)..., Idxs1...>;
};

template <typename F, typename Tpl, std::size_t... Idxs>
constexpr auto rotate_impl(F &&f, Tpl &&tpl, std::index_sequence<Idxs...>)
    ZX_NOEX_DECL_RET(static_cast<F &&>(f)(
        std::get<Idxs>(std::move(tpl))...)); // NOLINT(bugprone-move-forwarding-reference)

template <std::size_t N, typename F, typename... Ts,
          typename Seq = ind_seq_split<std::make_index_sequence<N>,
                                       std::make_index_sequence<sizeof...(Ts) - N>>>
constexpr auto rotate1(std::integral_constant<std::size_t, N>, F &&f, Ts &&... xs)
    ZX_NOEX_DECL_RET(internal::rotate_impl(static_cast<F &&>(f),
                                           std::forward_as_tuple(static_cast<Ts &&>(xs)...),
                                           typename Seq::type{}));

template <typename F, typename T, typename... Ts>
constexpr auto rotate1(std::integral_constant<std::size_t, 1>, F &&f, T &&x, Ts &&... xs)
    ZX_NOEX_DECL_RET(static_cast<F &&>(f)(static_cast<Ts &&>(xs)..., static_cast<T &&>(x)));

template <typename F, typename... Ts>
constexpr auto rotate1(std::integral_constant<std::size_t, 0>, F &&f, Ts &&... xs)
    ZX_NOEX_DECL_RET(static_cast<F &&>(f)(static_cast<Ts &&>(xs)...));
} // namespace internal

template <int N, typename F>
constexpr auto apply_index(F &&f)
    -> decltype(internal::apply_index1(static_cast<F &&>(f), std::make_integer_sequence<int, N>{}))
{
    return internal::apply_index1(static_cast<F &&>(f), std::make_integer_sequence<int, N>{});
}

template <std::size_t N, typename F>
ASTL_NODISCARD auto
rotate(F &&f) noexcept(std::is_nothrow_constructible<typename std::decay<F>::type, F>::value)
{
    return [f(static_cast<F &&>(f))](auto &&... xs) -> decltype(internal::rotate1(
                                                        std::integral_constant<std::size_t, N>{},
                                                        static_cast<F &&>(f),
                                                        static_cast<decltype(xs)>(xs)...)) {
        return internal::rotate1(std::integral_constant<std::size_t, N>{}, static_cast<F &&>(f),
                                 static_cast<decltype(xs)>(xs)...);
    };
}

template <typename F>
ASTL_NODISCARD auto
rotate(F &&f) noexcept(std::is_nothrow_constructible<typename std::decay<F>::type, F>::value)
{
    return [f(static_cast<F &&>(f))](auto &&x,
                                     auto &&... xs) -> decltype(f(static_cast<decltype(xs)>(xs)...,
                                                                  static_cast<decltype(x)>(x))) {
        return f(static_cast<decltype(xs)>(xs)..., static_cast<decltype(x)>(x));
    };
}

template <typename F>
ASTL_NODISCARD auto
reverse(F &&f) noexcept(std::is_nothrow_constructible<std::decay_t<F>, F>::value)
{
    return [f(static_cast<F &&>(f))](auto &&... xs) -> decltype(auto) {
        constexpr std::size_t new_size(sizeof...(xs));
        return astl::apply_index<new_size>([new_size, &f, &xs...](auto... idxs) -> decltype(auto) {
            return f(std::get<new_size - idxs>(
                std::forward_as_tuple(static_cast<decltype(xs)>(xs)...))...);
        });
    };
}

namespace internal
{
template <typename Fn, typename Iter, int... Idxs>
constexpr auto for_each1(Fn f, Iter it, std::integer_sequence<int, Idxs...>)
    -> decltype(f(*std::next(it, Idxs)...))
{
    return f(*std::next(it, Idxs)...);
}

template <typename Fn, typename Iter, typename P, int... Idxs>
constexpr auto for_each1(Fn f, Iter it, P p, std::integer_sequence<int, Idxs...>)
    -> decltype(f(invoke(p, *std::next(it, Idxs))...))
{
    return f(invoke(p, *std::next(it, Idxs))...);
}

} // namespace internal

namespace internal_math
{
template <typename T> constexpr auto abs_integral(T val) -> T
{
    if constexpr (std::is_signed<T>::value) return val < 0 ? -val : val;
    else if (std::is_unsigned<T>::value) {
        return val;
    }
    else {
        static_assert(std::is_same<T, bool>::value, "type is neither signed or unsigned");
        return val;
    }
}

template <typename T> auto fast_mod(T const &input, T const &ceil) -> T
{
    // precondition: input > 0 && ceil > 0
    return input >= ceil ? input % ceil : input;
}

template <typename I> auto even(I n) -> bool { return n % I(2) == 0; }
template <typename I> auto odd(I n) -> bool { return n % I(2) != 0; }

template <typename T, typename I, typename Op>
auto power_accumulate_positive(T r, T a, I n, Op op) -> T
{
    // Precondition: associative(op) && positive(n)
    while (true) {
        if (internal_math::odd(n)) {
            r = op(r, a);
            if (n == I(1)) return r;
        }
        a = op(a, a);
        n = n >> I(1);
    }
}

template <typename T, typename I, typename Op>
auto power_accumulate_nonnegative(T r, T a, I n, Op op) -> T
{
    assert(n >= I(0));
    // Precondition: associative(op) && !negative(n)
    if (n == I(0)) return r;

    return internal_math::power_accumulate_positive(r, a, n, op);
}

} // namespace internal_math

template <typename T, typename U>
ASTL_NODISCARD constexpr auto gcd(T x, U y) -> typename std::common_type<T, U>::type
{
    static_assert(std::is_integral<T>::value, "gcd arguments are integers");
    static_assert(std::is_integral<U>::value, "gcd arguments are integers");

    static_assert(!std::is_same<std::remove_cv_t<T>, bool>::value, "gcd arguments are not bool's");
    static_assert(!std::is_same<std::remove_cv_t<U>, bool>::value, "gcd arguments are not bool's");

    using CommonT = typename std::common_type<T, U>::type;
    CommonT a(internal_math::abs_integral((CommonT) x));
    CommonT b(internal_math::abs_integral((CommonT) y));

    while (true) {
        if (b == CommonT(0)) return a;

        a = internal_math::fast_mod(a, b);
        if (a == CommonT(0)) return b;

        b = internal_math::fast_mod(b, a);
    }
}

template <typename T, typename U>
ASTL_NODISCARD constexpr auto lcm(T x, U y) -> typename std::common_type<T, U>::type
{
    return x == T(0) || y == U(0) ?
        0 :
        (internal_math::abs_integral(x) / astl::gcd(x, y)) * internal_math::abs_integral(y);
}

template <typename T, typename I, typename Op = std::multiplies<>>
auto power_positive(T a, I n, Op op = Op{}) -> T
{
    // Precondition: associative(op) && positive(n)
    assert(n > I(0));
    while (internal_math::even(n)) {
        a = op(a, a);
        n = n >> I(1);
    }
    n = n >> I(1);
    if (n == I(0)) return a;

    return internal_math::power_accumulate_positive(a, op(a, a), n, op);
}

template <typename T, typename I, typename Op = std::multiplies<>>
auto power_nonnegative(T a, I n, Op op = Op{}, T id = T(1)) -> T
{
    // Precondition: associative(op) && n >= 0
    if (n == I(0)) return id;

    return astl::power_positive(a, n, op);
}

struct identity {
    template <typename T> ASTL_NODISCARD constexpr auto operator()(T &&x) const noexcept -> T &&
    {
        return static_cast<T &&>(x);
    }

    using is_transparent = int;
};

} // namespace astl

#undef ZX_NOEX_DECL_RET

#endif // ASTL_INCLUDE_FUNCTIONAL_HPP
