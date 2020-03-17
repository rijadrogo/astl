//
// Created by Rijad on 07-Sep-19.
//

#ifndef ASTL_INCLUDE_SELECT_HPP
#define ASTL_INCLUDE_SELECT_HPP

#include <cassert>

#include "astl/functional.hpp"
/*
 * All functions are stable
 */
namespace astl
{
// ReSharper disable CppInconsistentNaming
template <typename T, typename R = std::less<>>
// requires T Regular
// requires R weak_ordering on T
ASTL_NODISCARD auto select_1st_2(T &a, T &b, R r = R{}) -> T &
{
    if (r(b, a)) return b;

    return a;
}

template <typename T, typename R = std::less<>>
// requires T Regular
// requires R weak_ordering on T
ASTL_NODISCARD auto select_1st_2(T const &a, T const &b, R r = R{}) -> T const &
{
    if (r(b, a)) return b;

    return a;
}

template <typename T, typename R = std::less<>>
// requires T Regular
// requires R weak_ordering on T
ASTL_NODISCARD auto select_2nd_2(T &a, T &b, R r = R{}) -> T &
{
    if (r(b, a)) return a;

    return b;
}

template <typename T, typename R = std::less<>>
// requires T Regular
// requires R weak_ordering on T
ASTL_NODISCARD auto select_2nd_2(T const &a, T const &b, R r = R{}) -> T const &
{
    if (r(b, a)) return a;

    return b;
}

template <typename T, typename R = std::less<>>
// requires T Regular
// requires R weak_ordering on T
ASTL_NODISCARD auto select_minmax_2(T &a, T &b, R r = R{}) -> std::pair<T &, T &>
{
    if (r(b, a)) return std::pair<T &, T &>{b, a};

    return std::pair<T &, T &>{a, b};
}

template <typename T, typename R = std::less<>>
// requires T Regular
// requires R weak_ordering on T
ASTL_NODISCARD auto select_minmax_2(T const &a, T const &b, R r = R{})
    -> std::pair<T const &, T const &>
{
    if (r(b, a)) return std::pair<T const &, T const &>{b, a};

    return std::pair<T const &, T const &>{a, b};
}

template <typename T, typename R = std::less<>>
// requires T Regular
// requires R weak_ordering on T
ASTL_NODISCARD auto select_1st_3(T &a, T &b, T &c, R r = R{}) -> T &
{
    return astl::select_1st_2(astl::select_1st_2(a, b, astl::pass_fn(r)), c, astl::pass_fn(r));
}

template <typename T, typename R = std::less<>>
// requires T Regular
// requires R weak_ordering on T
ASTL_NODISCARD auto select_1st_3(T const &a, T const &b, T const &c, R r = R{}) -> T const &
{
    return astl::select_1st_2(astl::select_1st_2(a, b, astl::pass_fn(r)), c, astl::pass_fn(r));
}

template <typename T, typename R = std::less<>>
// requires T Regular
// requires R weak_ordering on T
ASTL_NODISCARD auto select_2nd_3(T &a, T &b, T &c, R r = R{}) -> T &;

template <typename T, typename R = std::less<>>
// requires T Regular
// requires R weak_ordering on T
ASTL_NODISCARD auto select_2nd_3(T const &a, T const &b, T const &c, R r = R{}) -> T const &;

template <typename T, typename R = std::less<>>
// requires T Regular
// requires R weak_ordering on T
ASTL_NODISCARD auto select_1st_4(T &a, T &b, T &c, T &d, R r = R{}) -> T &;

template <typename T, typename R = std::less<>>
// requires T Regular
// requires R weak_ordering on T
ASTL_NODISCARD auto select_3rd_3(T &a, T &b, T &c, R r = R{}) -> T &;

template <typename T, typename R = std::less<>>
// requires T Regular
// requires R weak_ordering on T
ASTL_NODISCARD auto select_3rd_3(T const &a, T const &b, T const &c, R r = R{}) -> T const &;

template <typename T, typename R = std::less<>>
// requires T Regular
// requires R weak_ordering on T
ASTL_NODISCARD auto select_minmax_3(T &a, T &b, T &c, R r = R{}) -> std::pair<T &, T &>;

template <typename T, typename R = std::less<>>
// requires T Regular
// requires R weak_ordering on T
ASTL_NODISCARD auto select_minmax_3(T const &a, T const &b, T const &c, R r = R{})
    -> std::pair<T const &, T const &>;

template <typename T, typename R = std::less<>>
// requires T Regular
// requires R weak_ordering on T
ASTL_NODISCARD auto select_1st_4(T const &a, T const &b, T const &c, T const &d, R r = R{})
    -> T const &;

template <typename T, typename R = std::less<>>
// requires T Regular
// requires R weak_ordering on T
ASTL_NODISCARD auto select_2nd_4(T &a, T &b, T &c, T &d, R r = R{}) -> T &;

template <typename T, typename R = std::less<>>
// requires T Regular
// requires R weak_ordering on T
ASTL_NODISCARD auto select_2nd_4(T const &a, T const &b, T const &c, T const &d, R r = R{})
    -> T const &;

template <typename T, typename R = std::less<>>
// requires T Regular
// requires R weak_ordering on T
ASTL_NODISCARD auto select_3rd_4(T const &a, T const &b, T const &c, T const &d, R r = R{})
    -> T const &;

template <typename T, typename R = std::less<>>
// requires T Regular
// requires R weak_ordering on T
ASTL_NODISCARD auto select_3rd_4(T &a, T &b, T &c, T &d, R r = R{}) -> T &;

template <typename T, typename R = std::less<>>
// requires T Regular
// requires R weak_ordering on T
ASTL_NODISCARD auto select_4th_4(T &a, T &b, T &c, T &d, R r = R{}) -> T &;

template <typename T, typename R = std::less<>>
// requires T Regular
// requires R weak_ordering on T
ASTL_NODISCARD auto select_4th_4(T const &a, T const &b, T const &c, T const &d, R r = R{})
    -> T const &;

template <typename T, typename R = std::less<>>
// requires T Regular
// requires R weak_ordering on T
ASTL_NODISCARD auto select_minmax_4(T &a, T &b, T &c, T &d, R r = R{}) -> std::pair<T &, T &>;

template <typename T, typename R = std::less<>>
// requires T Regular
// requires R weak_ordering on T
ASTL_NODISCARD auto select_minmax_4(T const &a, T const &b, T const &c, T const &d, R r = R{})
    -> std::pair<T const &, T const &>;

namespace internal_select
{
template <typename T, typename R>
// requires T Regular
// requires R weak_ordering on T
ASTL_NODISCARD auto select_2nd_3_ab(T &a, T &b, T &c, R r) -> T &
{
    assert(!r(b, a) && "a and b must be non-decreasing"); //-V2528
    if (!r(c, b)) return b;                               // a,b,c are sorted

    return astl::select_2nd_2(a, c, r); // b is not the median
}

template <typename T, typename R>
// requires T Regular
// requires R weak_ordering on T
ASTL_NODISCARD auto select_2nd_3(T &a, T &b, T &c, R r) -> T &
{
    if (r(b, a)) return internal_select::select_2nd_3_ab(b, a, c, astl::pass_fn(r));

    return internal_select::select_2nd_3_ab(a, b, c, astl::pass_fn(r));
}

template <typename T, typename R>
// requires T Regular
// requires R weak_ordering on T
ASTL_NODISCARD auto select_2nd_4_ab_cd(T &a, T &b, T &c, T &d, R r) -> T &
{
    assert(!r(b, a) && !r(d, c) && //-V2528
           "a and b must be non-decreasing && c and d must be "
           "non-decreasing");
    if (r(c, a)) return astl::select_1st_2(a, d, r);

    return astl::select_1st_2(b, c, r);
}

template <bool Strict, typename R> struct compare_strict_or_reflexive;

template <typename R>
struct compare_strict_or_reflexive<true, R> // strict
{
    template <typename T> ASTL_NODISCARD auto operator()(T &a, T &b, R r) -> bool
    {
        return r(a, b);
    }
};

template <typename R>
struct compare_strict_or_reflexive<false, R> //  reflexive
{
    template <typename T> ASTL_NODISCARD auto operator()(T &a, T &b, R r) -> bool
    {
        return !r(b, a);
    }
};

template <int Ia, int Ib, typename R, typename T>
// requires T Regular
// requires R weak_ordering on T
ASTL_NODISCARD auto select_1st_2(T &a, T &b, R r) -> T &
{
    compare_strict_or_reflexive<(Ia < Ib), R> comp;
    if (comp(b, a, r)) return b;

    return a;
}

template <typename T, typename R /* = std::less<> */>
// requires T Regular
// requires R weak_ordering on T
ASTL_NODISCARD auto select_minmax_3(T &a, T &b, T &c, R r /* = R{} */) -> std::pair<T &, T &>
{
    if (r(b, a)) {
        if (r(c, b)) return std::pair<T &, T &>{c, a};

        return std::pair<T &, T &>{b, astl::select_2nd_2(a, c, astl::pass_fn(r))};
    }
    if (r(c, a)) return std::pair<T &, T &>{astl::select_1st_2(b, c, astl::pass_fn(r)), a};

    return std::pair<T &, T &>{a, astl::select_2nd_2(b, c, astl::pass_fn(r))};
}

template <int Ia, int Ib, int Ic, int Id, typename R, typename T>
// requires T Regular
// requires R weak_ordering on T
ASTL_NODISCARD auto select_2nd_4_ab_cd(T &a, T &b, T &c, T &d, R r) -> T &
{
    compare_strict_or_reflexive<(Ia < Ic), R> comp;
    assert(!comp(b, a, r) && !comp(d, c, r)
           && "a and b must be non-decreasing && c and d must be " //-V2528
              "non-decreasing");

    if (comp(c, a, r)) return internal_select::select_1st_2<Ia, Id>(a, d, r);

    return internal_select::select_1st_2<Ib, Ic>(b, c, r);
}

template <int Ia, int Ib, int Ic, int Id, typename R, typename T>
// requires T Regular
// requires R weak_ordering on T
ASTL_NODISCARD auto select_2nd_4_ab(T &a, T &b, T &c, T &d, R r) -> T &
{
    compare_strict_or_reflexive<(Ic < Id), R> comp;
    assert(!comp(b, a, r) && "a and b must be non-decreasing");
    if (comp(d, c, r))
        return internal_select::select_2nd_4_ab_cd<Ia, Ib, Id, Ic>(a, b, d, c,
                                                                   r); //-V764

    return internal_select::select_2nd_4_ab_cd<Ia, Ib, Ic, Id>(a, b, c, d, r);
}

template <int Ia, int Ib, int Ic, int Id, typename R, typename T>
// requires T Regular
// requires R weak_ordering on T
ASTL_NODISCARD auto select_2nd_4(T &a, T &b, T &c, T &d, R r) -> T &
{
    compare_strict_or_reflexive<(Ia < Ib), R> comp;
    if (comp(b, a, r)) return internal_select::select_2nd_4_ab<Ib, Ia, Ic, Id>(b, a, c, d, r);

    return internal_select::select_2nd_4_ab<Ia, Ib, Ic, Id>(a, b, c, d, r);
}

template <int Ia, int Ib, int Ic, int Id, typename R, typename T>
// requires T Regular
// requires R weak_ordering on T
ASTL_NODISCARD auto select_3rd_4_ab_cd(T &a, T &b, T &c, T &d, R r) -> T &
{
    compare_strict_or_reflexive<(Ia < Ic), R> comp;
    assert(!comp(b, a, r) && !comp(d, c, r)
           && "a and b must be non-decreasing && c and d must be "
              "non-decreasing");

    if (comp(c, a, r)) return astl::select_2nd_3(a, b, d, r);

    return astl::select_2nd_3(b, c, d, r);
}

template <int Ia, int Ib, int Ic, int Id, typename R, typename T>
// requires T Regular
// requires R weak_ordering on T
ASTL_NODISCARD auto select_3rd_4_ab(T &a, T &b, T &c, T &d, R r) -> T &
{
    compare_strict_or_reflexive<(Ic < Id), R> comp;
    assert(!comp(b, a, r) && "a and b must be non-decreasing");
    if (comp(d, c, r))
        return internal_select::select_3rd_4_ab_cd<Ia, Ib, Id, Ic>(a, b, d, c,
                                                                   r); //-V764

    return internal_select::select_3rd_4_ab_cd<Ia, Ib, Ic, Id>(a, b, c, d, r);
}

template <int Ia, int Ib, int Ic, int Id, typename R, typename T>
// requires T Regular
// requires R weak_ordering on T
ASTL_NODISCARD auto select_3rd_4(T &a, T &b, T &c, T &d, R r) -> T &
{
    compare_strict_or_reflexive<(Ia < Ib), R> comp;
    if (comp(b, a, r)) return internal_select::select_3rd_4_ab<Ib, Ia, Ic, Id>(b, a, c, d, r);

    return internal_select::select_3rd_4_ab<Ia, Ib, Ic, Id>(a, b, c, d, r);
}

template <int Ia, int Ib, int Ic, int Id, int Ie, typename T, typename R>
// requires T Regular
// requires R weak_ordering on T
ASTL_NODISCARD auto select_3rd_5_ab_cd(T &a, T &b, T &c, T &d, T &e, R r) -> T &
{
    compare_strict_or_reflexive<(Ia < Ic), R> comp;
    assert(!comp(b, a, r) && !comp(d, c, r)
           && "a and b must be non-decreasing && c and d must be "
              "non-decreasing");
    if (comp(c, a, r)) return internal_select::select_2nd_4_ab<Ia, Ib, Id, Ie>(a, b, d, e, r);

    return internal_select::select_2nd_4_ab<Ic, Id, Ib, Ie>(c, d, b, e, r);
}

template <int Ia, int Ib, int Ic, int Id, int Ie, typename T, typename R>
// requires T Regular
// requires R weak_ordering on T
ASTL_NODISCARD auto select_3rd_5_ab(T &a, T &b, T &c, T &d, T &e, R r) -> T &
{
    compare_strict_or_reflexive<(Ic < Id), R> comp;
    assert(!comp(b, a, r) && "a and b must be non-decreasing");
    if (comp(d, c, r))
        return internal_select::select_3rd_5_ab_cd<Ia, Ib, Id, Ic, Ie>(a, b, d, c, e, r); //-V764

    return internal_select::select_3rd_5_ab_cd<Ia, Ib, Ic, Id, Ie>(a, b, c, d, e, r);
}

template <int Ia, int Ib, int Ic, int Id, int Ie, typename T, typename R>
// requires T Regular
// requires R weak_ordering on T
ASTL_NODISCARD auto select_3rd_5(T &a, T &b, T &c, T &d, T &e, R r) -> T &
{
    compare_strict_or_reflexive<(Ia < Ib), R> comp;
    if (comp(b, a, r)) return internal_select::select_3rd_5_ab<Ib, Ia, Ic, Id, Ie>(b, a, c, d, e, r);

    return internal_select::select_3rd_5_ab<Ia, Ib, Ic, Id, Ie>(a, b, c, d, e, r);
}

template <typename T, typename R>
// requires T Regular
// requires R weak_ordering on T
ASTL_NODISCARD auto select_3rd_4_ab_cd(T &a, T &b, T &c, T &d, R r) -> T &
{
    assert(!r(b, a) && !r(d, c)
           && "a and b must be non-decreasing && c and d must be "
              "non-decreasing");
    if (r(c, a)) return astl::select_1st_2(a, d, r);

    return astl::select_1st_2(b, c, r);
}

template <typename T, typename R>
// requires T Regular
// requires R weak_ordering on T
ASTL_NODISCARD auto select_3rd_4_ab(T &a, T &b, T &c, T &d, R r) -> T &
{
    assert(!r(b, a) && "a and b must be non-decreasing");
    if (r(d, c)) return internal_select::select_3rd_4_ab_cd(a, b, d, c, r); //-V764

    return internal_select::select_3rd_4_ab_cd(a, b, c, d, r);
}

template <typename T, typename R>
// requires T Regular
// requires R weak_ordering on T
ASTL_NODISCARD auto select_minmax_4_ab_cd(T &a, T &b, T &c, T &d, R r) -> std::pair<T &, T &>
{
    assert(!r(b, a) && !r(d, c)
           && "a and b must be non-decreasing && c and d must be "
              "non-decreasing");
    if (r(c, a)) return std::pair<T &, T &>{c, astl::select_2nd_2(b, d, astl::pass_fn(r))};

    return std::pair<T &, T &>{a, astl::select_2nd_2(b, d, astl::pass_fn(r))};
}

template <typename T, typename R>
// requires T Regular
// requires R weak_ordering on T
ASTL_NODISCARD auto select_minmax_4_ab(T &a, T &b, T &c, T &d, R r) -> std::pair<T &, T &>
{
    assert(!r(b, a) && "a and b must be non-decreasing");
    if (r(d, c)) return select_minmax_4_ab_cd(a, b, d, c, astl::pass_fn(r));

    return select_minmax_4_ab_cd(a, b, c, d, astl::pass_fn(r));
}

template <typename T, typename R>
// requires T Regular
// requires R weak_ordering on T
ASTL_NODISCARD auto select_minmax_4(T &a, T &b, T &c, T &d, R r) -> std::pair<T &, T &>
{
    if (r(b, a)) return internal_select::select_minmax_4_ab(b, a, c, d, astl::pass_fn(r));

    return internal_select::select_minmax_4_ab(a, b, c, d, astl::pass_fn(r));
}

template <int Ia, int Ib, int Ic, int Id, int Ie, typename T, typename R>
// requires T Regular
// requires R weak_ordering on T
ASTL_NODISCARD auto select_2nd_5_ab_cd(T &a, T &b, T &c, T &d, T &e, R r) -> T &
{
    compare_strict_or_reflexive<(Ia < Ic), R> comp;
    assert(!comp(b, a, r) && !comp(d, c, r)
           && "a and b must be non-decreasing && c and d must be "
              "non-decreasing");
    if (comp(c, a, r)) return astl::select_1st_4(a, b, d, e, r);

    return astl::select_1st_4(c, d, b, e, r);
}

template <int Ia, int Ib, int Ic, int Id, int Ie, typename T, typename R>
// requires T Regular
// requires R weak_ordering on T
ASTL_NODISCARD auto select_2nd_5_ab(T &a, T &b, T &c, T &d, T &e, R r) -> T &
{
    compare_strict_or_reflexive<(Ic < Id), R> comp;
    assert(!comp(b, a, r) && "a and b must be non-decreasing");
    if (comp(d, c, r))
        return internal_select::select_2nd_5_ab_cd<Ia, Ib, Id, Ic, Ie>(a, b, d, c, e, r);

    return internal_select::select_2nd_5_ab_cd<Ia, Ib, Ic, Id, Ie>(a, b, c, d, e, r);
}

template <int Ia, int Ib, int Ic, int Id, int Ie, typename T, typename R>
// requires T Regular
// requires R weak_ordering on T
ASTL_NODISCARD auto select_2nd_5(T &a, T &b, T &c, T &d, T &e, R r) -> T &
{
    compare_strict_or_reflexive<(Ia < Ib), R> comp;
    if (comp(b, a, r)) return internal_select::select_2nd_5_ab<Ib, Ia, Ic, Id, Ie>(b, a, c, d, e, r);

    return internal_select::select_2nd_5_ab<Ia, Ib, Ic, Id, Ie>(a, b, c, d, e, r);
}

template <int Ia, int Ib, int Ic, int Id, int Ie, typename T, typename R>
// requires T Regular
// requires R weak_ordering on T
ASTL_NODISCARD auto select_4th_5_ab_cd(T &a, T &b, T &c, T &d, T &e, R r) -> T &
{
    compare_strict_or_reflexive<(Ia < Ic), R> comp;

    assert(!comp(b, a, r) && !comp(d, c, r)
           && "a and b must be non-decreasing && c and d must be "
              "non-decreasing");
    if (comp(c, a, r)) return astl::select_3rd_4(a, b, d, e, r);

    return astl::select_3rd_4(c, d, b, e, r);
}

template <int Ia, int Ib, int Ic, int Id, int Ie, typename T, typename R>
// requires T Regular
// requires R weak_ordering on T
ASTL_NODISCARD auto select_4th_5_ab(T &a, T &b, T &c, T &d, T &e, R r) -> T &
{
    compare_strict_or_reflexive<(Ic < Id), R> comp;

    assert(!comp(b, a, r) && "a and b must be non-decreasing");

    if (comp(d, c, r))
        return internal_select::select_4th_5_ab_cd<Ia, Ib, Id, Ic, Ie>(a, b, d, c, e, r); //-V764

    return internal_select::select_4th_5_ab_cd<Ia, Ib, Ic, Id, Ie>(a, b, c, d, e, r);
}
template <int Ia, int Ib, int Ic, int Id, int Ie, typename T, typename R>
// requires T Regular
// requires R weak_ordering on T
ASTL_NODISCARD auto select_4th_5(T &a, T &b, T &c, T &d, T &e, R r) -> T &
{
    compare_strict_or_reflexive<(Ia < Ib), R> comp;
    if (comp(b, a, r)) return internal_select::select_4th_5_ab<Ib, Ia, Ic, Id, Ie>(b, a, c, d, e, r);

    return internal_select::select_4th_5_ab<Ia, Ib, Ic, Id, Ie>(a, b, c, d, e, r);
}

template <typename T, typename R>
// requires T Regular
// requires R weak_ordering on T
ASTL_NODISCARD auto select_minmax_5_ab_cd(T &a, T &b, T &c, T &d, T &e, R r) -> std::pair<T &, T &>
{
    if (r(c, a)) {
        if (r(e, c)) return std::pair<T &, T &>{e, astl::select_2nd_2(b, d, astl::pass_fn(r))};

        return std::pair<T &, T &>{c, astl::select_3rd_3(b, d, e, astl::pass_fn(r))};
    }
    if (r(a, e)) return std::pair<T &, T &>{a, astl::select_3rd_3(b, d, e, astl::pass_fn(r))};

    return std::pair<T &, T &>{astl::select_1st_2(a, e, astl::pass_fn(r)),
                               astl::select_2nd_2(b, d, astl::pass_fn(r))};
}

template <typename T, typename R>
// requires T Regular
// requires R weak_ordering on T
ASTL_NODISCARD auto select_minmax_5_ab(T &a, T &b, T &c, T &d, T &e, R r) -> std::pair<T &, T &>
{
    if (r(d, c)) return internal_select::select_minmax_5_ab_cd(a, b, d, c, e, astl::pass_fn(r));

    return internal_select::select_minmax_5_ab_cd(a, b, c, d, e, astl::pass_fn(r));
}

template <typename T, typename R>
// requires T Regular
// requires R weak_ordering on T
ASTL_NODISCARD auto select_minmax_5(T &a, T &b, T &c, T &d, T &e, R r) -> std::pair<T &, T &>
{
    if (r(b, a)) return internal_select::select_minmax_5_ab(b, a, c, d, e, astl::pass_fn(r));

    return internal_select::select_minmax_5_ab(a, b, c, d, e, astl::pass_fn(r));
}

} // namespace internal_select

template <typename T, typename R /* = std::less<> */>
// requires T Regular
// requires R weak_ordering on T
ASTL_NODISCARD auto select_2nd_3(T &a, T &b, T &c, R r /* = R{} */) -> T &
{
    return internal_select::select_2nd_3(a, b, c, astl::pass_fn(r));
}

template <typename T, typename R /* = std::less<> */>
// requires T Regular
// requires R weak_ordering on T
ASTL_NODISCARD auto select_2nd_3(T const &a, T const &b, T const &c, R r /* = R{} */) -> T const &
{
    return internal_select::select_2nd_3(a, b, c, astl::pass_fn(r));
}

template <typename T, typename R /* = std::less<> */>
// requires T Regular
// requires R weak_ordering on T
ASTL_NODISCARD auto select_3rd_3(T &a, T &b, T &c, R r /* = R{} */) -> T &
{
    return astl::select_2nd_2(astl::select_2nd_2(a, b, r), c, astl::pass_fn(r));
}

template <typename T, typename R /* = std::less<> */>
// requires T Regular
// requires R weak_ordering on T
ASTL_NODISCARD auto select_3rd_3(T const &a, T const &b, T const &c, R r /* = R{} */) -> T const &
{
    return astl::select_2nd_2(astl::select_2nd_2(a, b, r), c, astl::pass_fn(r));
}

template <typename T, typename R /* = std::less<> */>
// requires T Regular
// requires R weak_ordering on T
ASTL_NODISCARD auto select_minmax_3(T &a, T &b, T &c, R r /* = R{} */) -> std::pair<T &, T &>
{
    return internal_select::select_minmax_3(a, b, c, astl::pass_fn(r));
}

template <typename T, typename R /* = std::less<> */>
// requires T Regular
// requires R weak_ordering on T
ASTL_NODISCARD auto select_minmax_3(T const &a, T const &b, T const &c, R r /* = R{} */)
    -> std::pair<T const &, T const &>
{
    return internal_select::select_minmax_3(a, b, c, astl::pass_fn(r));
}

template <typename T, typename R /* = std::less<> */>
ASTL_NODISCARD auto select_1st_4(T &a, T &b, T &c, T &d, R r /* = R{}*/) -> T &
{
    return astl::select_1st_2(a, astl::select_1st_3(b, c, d, astl::pass_fn(r)), astl::pass_fn(r));
}

template <typename T, typename R /* = std::less<> */>
// requires T Regular
// requires R weak_ordering on T
ASTL_NODISCARD auto select_1st_4(T const &a, T const &b, T const &c, T const &d, R r /* = R{} */)
    -> T const &
{
    return astl::select_1st_2(a, astl::select_1st_3(b, c, d, astl::pass_fn(r)), astl::pass_fn(r));
}

template <typename T, typename R /* = std::less<> */>
// requires T Regular
// requires R weak_ordering on T
ASTL_NODISCARD auto select_2nd_4(T &a, T &b, T &c, T &d, R r /* = R{} */) -> T &
{
    return internal_select::select_2nd_4<0, 1, 2, 3>(a, b, c, d, r);
}

template <typename T, typename R /* = std::less<> */>
// requires T Regular
// requires R weak_ordering on T
ASTL_NODISCARD auto select_2nd_4(T const &a, T const &b, T const &c, T const &d, R r /* = R{} */)
    -> T const &
{
    return internal_select::select_2nd_4<0, 1, 2, 3>(a, b, c, d, r);
}

template <typename T, typename R /* = std::less<> */>
// requires T Regular
// requires R weak_ordering on T
ASTL_NODISCARD auto select_3rd_4(T &a, T &b, T &c, T &d, R r /* = R{} */) -> T &
{
    return internal_select::select_3rd_4<0, 1, 2, 3>(a, b, c, d, astl::pass_fn(r));
}

template <typename T, typename R /* = std::less<> */>
// requires T Regular
// requires R weak_ordering on T
ASTL_NODISCARD auto select_3rd_4(T const &a, T const &b, T const &c, T const &d, R r /*= R{} */)
    -> T const &
{
    return internal_select::select_3rd_4<0, 1, 2, 3>(a, b, c, d, astl::pass_fn(r));
}

template <typename T, typename R /* = std::less<> */>
// requires T Regular
// requires R weak_ordering on T
ASTL_NODISCARD auto select_4th_4(T &a, T &b, T &c, T &d, R r /* = R{} */) -> T &
{
    return astl::select_2nd_2(a, astl::select_3rd_3(b, c, d, astl::pass_fn(r)), astl::pass_fn(r));
}

template <typename T, typename R /* = std::less<> */>
// requires T Regular
// requires R weak_ordering on T
ASTL_NODISCARD auto select_4th_4(T const &a, T const &b, T const &c, T const &d, R r /*= R{} */)
    -> T const &
{
    return astl::select_2nd_2(a, astl::select_3rd_3(b, c, d, astl::pass_fn(r)), astl::pass_fn(r));
}

template <typename T, typename R /* = std::less<> */>
// requires T Regular
// requires R weak_ordering on T
ASTL_NODISCARD auto select_minmax_4(T &a, T &b, T &c, T &d, R r /* = R{} */) -> std::pair<T &, T &>
{
    return internal_select::select_minmax_4(a, b, c, d, astl::pass_fn(r));
}

template <typename T, typename R /* = std::less<> */>
// requires T Regular
// requires R weak_ordering on T
ASTL_NODISCARD auto select_minmax_4(T const &a, T const &b, T const &c, T const &d, R r /* = R{} */)
    -> std::pair<T const &, T const &>
{
    return internal_select::select_minmax_4(a, b, c, d, astl::pass_fn(r));
}

template <typename T, typename R = std::less<>>
// requires T Regular
// requires R weak_ordering on T
ASTL_NODISCARD auto select_1st_5(T &a, T &b, T &c, T &d, T &e, R r = R{}) -> T &
{
    return astl::select_1st_2(a, astl::select_1st_4(b, c, d, e, astl::pass_fn(r)),
                              astl::pass_fn(r));
}

template <typename T, typename R = std::less<>>
// requires T Regular
// requires R weak_ordering on T
ASTL_NODISCARD auto select_1st_5(T const &a, T const &b, T const &c, T const &d, T const &e,
                                 R r = R{}) -> T const &
{
    return astl::select_1st_2(a, astl::select_1st_4(b, c, d, e, astl::pass_fn(r)),
                              astl::pass_fn(r));
}

template <typename T, typename R = std::less<>>
// requires T Regular
// requires R weak_ordering on T
ASTL_NODISCARD auto select_2nd_5(T &a, T &b, T &c, T &d, T &e, R r = R{}) -> T &
{
    return internal_select::select_2nd_5<0, 1, 2, 3, 4>(a, b, c, d, e, astl::pass_fn(r));
}

template <typename T, typename R = std::less<>>
// requires T Regular
// requires R weak_ordering on T
ASTL_NODISCARD auto select_2nd_5(T const &a, T const &b, T const &c, T const &d, T const &e,
                                 R r = R{}) -> T const &
{
    return internal_select::select_2nd_5<0, 1, 2, 3, 4>(a, b, c, d, e, astl::pass_fn(r));
}

template <typename T, typename R = std::less<>>
// requires T Regular
// requires R weak_ordering on T
ASTL_NODISCARD auto select_3rd_5(T &a, T &b, T &c, T &d, T &e, R r = R{}) -> T &
{
    return internal_select::select_3rd_5<0, 1, 2, 3, 4>(a, b, c, d, e, r);
}

template <typename T, typename R = std::less<>>
// requires T Regular
// requires R weak_ordering on T
ASTL_NODISCARD auto select_3rd_5(T const &a, T const &b, T const &c, T const &d, T const &e,
                                 R r = R{}) -> T const &
{
    return internal_select::select_3rd_5<0, 1, 2, 3, 4>(a, b, c, d, e, r);
}

template <typename T, typename R = std::less<>>
// requires T Regular
// requires R weak_ordering on T
ASTL_NODISCARD auto select_4th_5(T &a, T &b, T &c, T &d, T &e, R r = R{}) -> T &
{
    return internal_select::select_4th_5<0, 1, 2, 3, 4>(a, b, c, d, e, astl::pass_fn(r));
}

template <typename T, typename R = std::less<>>
// requires T Regular
// requires R weak_ordering on T
ASTL_NODISCARD auto select_4th_5(T const &a, T const &b, T const &c, T const &d, T const &e,
                                 R r = R{}) -> T const &
{
    return internal_select::select_4th_5<0, 1, 2, 3, 4>(a, b, c, d, e, astl::pass_fn(r));
}

template <typename T, typename R = std::less<>>
// requires T Regular
// requires R weak_ordering on T
ASTL_NODISCARD auto select_5th_5(T &a, T &b, T &c, T &d, T &e, R r = R{}) -> T &
{
    return astl::select_2nd_2(a, astl::select_4th_4(b, c, d, e, astl::pass_fn(r)),
                              astl::pass_fn(r));
}

template <typename T, typename R = std::less<>>
// requires T Regular
// requires R weak_ordering on T
ASTL_NODISCARD auto select_5th_5(T const &a, T const &b, T const &c, T const &d, T const &e,
                                 R r = R{}) -> T const &
{
    return astl::select_2nd_2(a, astl::select_4th_4(b, c, d, e, astl::pass_fn(r)),
                              astl::pass_fn(r));
}

template <typename T, typename R = std::less<>>
// requires T Regular
// requires R weak_ordering on T
ASTL_NODISCARD auto select_minmax_5(T &a, T &b, T &c, T &d, T &e, R r = R{}) -> std::pair<T &, T &>
{
    return internal_select::select_minmax_5(a, b, c, d, e, astl::pass_fn(r));
}

template <typename T, typename R = std::less<>>
// requires T Regular
// requires R weak_ordering on T
ASTL_NODISCARD auto select_minmax_5(T const &a, T const &b, T const &c, T const &d, T const &e,
                                    R r = R{}) -> std::pair<T const &, T const &>
{
    return internal_select::select_minmax_5(a, b, c, d, e, astl::pass_fn(r));
}

// ReSharper restore CppInconsistentNaming
} // namespace astl
#endif // ASTL_INCLUDE_SELECT_HPP
