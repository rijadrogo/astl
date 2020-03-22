//
// Created by Rijad on 04-Jan-20.
//

#ifndef ASTL_INCLUDE_PERMUTATION_COMBINATIONS_HPP
#define ASTL_INCLUDE_PERMUTATION_COMBINATIONS_HPP

//  (C) Copyright Howard Hinnant 2005-2011.
//  Use, modification and distribution are subject to the Boost Software
//  License, Version 1.0. (See accompanying file LICENSE_1_0.txt or copy at
//  http://www.boost.org/LICENSE_1_0.txt).
//
//  See http://www.boost.org/libs/type_traits for most recent version including
//  documentation.

//  Details are in namespace internal_pc.  Every effort has been made to make
//  combine_discontinuous and permute as fast as possible.  They minimize the
//  number of swaps that are performed. Everything else builds on these two
//  primitives. The most complicated algorithm is
//  for_each_reversible_permutation.  But it builds on combine_discontinuous and
//  permute and I believe represents a minimum number of swaps.  Without care,
//  algorithms such as for_each_reversible_permutation will take longer than
//  for_each_permutation instead of the intended half the time.

//  Speed is everything.  Lest you could just use std::next_permutation and
//  manually eliminate duplicate permutations.  If the implementation fails in
//  being orders of magnitude faster than that, then it has failed miserably.

// TODO add range overloads
// TODO make it work with forward iterators?

#include <algorithm>
#include <cstdint>
#include <iterator>
#include <limits>

#include "astl/iterator.hpp"
#include "astl/optional.hpp"

namespace astl
{
namespace internal_pc
{
// Rotates two discontinuous ranges to put *first2 where *first1 is.
//     If last1 == first2 this would be equivalent to rotate(first1, first2,
//     last2), but instead the rotate "jumps" over the discontinuity [last1,
//     first2) - which need not be a valid range. In order to make it faster,
//     the length of [first1, last1) is passed in as d1, and d2 must be the
//     length of [first2, last2).
//  In a perfect world the d1 > d2 case would have used swap_ranges and
//     reverse_iterator, but reverse_iterator is too inefficient.
template <typename BidiIter>
void rotate_discontinuous(BidiIter first1, BidiIter last1, iter_diff_type<BidiIter> d1,
                          BidiIter first2, BidiIter last2, iter_diff_type<BidiIter> d2)
{
    using std::swap;
    if (d1 <= d2) std::rotate(first2, std::swap_ranges(first1, last1, first2), last2);
    else {
        BidiIter i1 = last1;
        while (first2 != last2) swap(*--i1, *--last2);
        std::rotate(first1, i1, last1);
    }
}

// Rotates the three discontinuous ranges to put *first2 where *first1 is.
// Just like rotate_discontinuous, except the second range is now represented by
//    two discontinuous ranges: [first2, last2) + [first3, last3).
template <typename BidIt>
void rotate_discontinuous3(BidIt first1, BidIt last1, iter_diff_type<BidIt> d1, BidIt first2,
                           BidIt last2, iter_diff_type<BidIt> d2, BidIt first3, BidIt last3,
                           iter_diff_type<BidIt> d3)
{
    internal_pc::rotate_discontinuous(first1, last1, d1, first2, last2, d2);
    if (d1 <= d2)
        internal_pc::rotate_discontinuous(astl::next(first2, d2 - d1), last2, d1, first3, last3,
                                          d3);
    else {
        internal_pc::rotate_discontinuous(astl::next(first1, d2), last1, d1 - d2, first3, last3,
                                          d3);
        internal_pc::rotate_discontinuous(first2, last2, d2, first3, last3, d3);
    }
}

// Call f() for each combination of the elements [first1, last1) + [first2,
// last2)
//    swapped/rotated into the range [first1, last1).  As long as f() returns
//    false, continue for every combination and then return [first1, last1) and
//    [first2, last2) to their original state.  If f() returns true, return
//    immediately.
//  Does the absolute minimum amount of swapping to accomplish its task.
//  If f() always returns false it will be called (d1+d2)!/(d1!*d2!) times.
template <typename BidIt, typename Function>
auto combine_discontinuous(BidIt first1, BidIt last1, iter_diff_type<BidIt> d1, BidIt first2,
                           BidIt last2, iter_diff_type<BidIt> d2, Function &f,
                           iter_diff_type<BidIt> d = 0) -> bool
{
    using D = iter_diff_type<BidIt>;
    using std::swap;
    if (d1 == 0 || d2 == 0) return f();
    if (d1 == 1) {
        BidIt i2(first2);
        while (i2 != last2) {
            if (f()) return true;
            swap(*first1, *i2);
            ++i2;
        }
    }
    else {
        BidIt f1_p(astl::next(first1));
        BidIt i2(first2);
        D d22(d2);
        while (i2 != last2) {
            if (internal_pc::combine_discontinuous(f1_p, last1, d1 - 1, i2, last2, d22, f, d + 1))
                return true;
            swap(*first1, *i2);
            ++i2;
            --d22;
        }
    }
    if (f()) return true;
    if (d != 0)
        internal_pc::rotate_discontinuous(first1, last1, d1, astl::next(first2), last2, d2 - 1);
    else
        internal_pc::rotate_discontinuous(first1, last1, d1, first2, last2, d2);
    return false;
}

// A binder for binding arguments to call combine_discontinuous
template <typename Function, typename BidIt> struct call_combine_discontinuous {
private:
    using D = iter_diff_type<BidIt>;
    Function f;
    BidIt first1;
    BidIt last1;
    D d1;
    BidIt first2;
    BidIt last2;
    D d2;

public:
    call_combine_discontinuous(BidIt first1, BidIt last1, D d1, BidIt first2, BidIt last2, D d2,
                               Function &f)
        : f(f), first1(first1), last1(last1), d1(d1), first2(first2), last2(last2), d2(d2)
    {}

    auto operator()() -> bool
    {
        return internal_pc::combine_discontinuous(first1, last1, d1, first2, last2, d2, f);
    }
};

// See combine_discontinuous3
template <typename BidIt, typename Function>
auto combine_discontinuous3_1(BidIt first1, BidIt last1, iter_diff_type<BidIt> d1, BidIt first2,
                              BidIt last2, iter_diff_type<BidIt> d2, BidIt first3, BidIt last3,
                              iter_diff_type<BidIt> d3, Function &f, iter_diff_type<BidIt> d = 0)
    -> bool
{
    using D = iter_diff_type<BidIt>;
    using std::swap;
    if (d1 == 1) {
        BidIt i2(first2);
        while (i2 != last2) {
            if (f()) return true;
            swap(*first1, *i2);
            ++i2;
        }
        if (f()) return true;
        swap(*first1, *std::prev(last2));
        swap(*first1, *first3);
        i2 = astl::next(first3);
        while (i2 != last3) {
            if (f()) return true;
            swap(*first1, *i2);
            ++i2;
        }
    }
    else {
        BidIt f1_p(astl::next(first1));
        BidIt i2(first2);
        D d22(d2);
        while (i2 != last2) {
            if (internal_pc::combine_discontinuous3_1(f1_p, last1, d1 - 1, i2, last2, d22, first3,
                                                      last3, d3, f, d + 1))
                return true;

            swap(*first1, *i2);
            ++i2;
            --d22;
        }
        i2 = first3;
        d22 = d3;
        while (i2 != last3) {
            if (internal_pc::combine_discontinuous(f1_p, last1, d1 - 1, i2, last3, d22, f, d + 1))
                return true;

            swap(*first1, *i2);
            ++i2;
            --d22;
        }
    }
    if (f()) return true;
    if (d1 == 1) swap(*std::prev(last2), *first3);

    if (d != 0) {
        if (d2 > 1)
            internal_pc::rotate_discontinuous3(first1, last1, d1, astl::next(first2), last2, d2 - 1,
                                               first3, last3, d3);
        else
            internal_pc::rotate_discontinuous(first1, last1, d1, first3, last3, d3);
    }
    else
        internal_pc::rotate_discontinuous3(first1, last1, d1, first2, last2, d2, first3, last3, d3);
    return false;
}

// Like combine_discontinuous, but swaps/rotates each combination out of
//    [first1, last1) + [first2, last2) + [first3, last3) into [first1, last1).
//    If f() always returns false, it is called (d1+d2+d3)!/(d1!*(d2+d3)!)
//    times.
template <typename BidIt, typename Function>
auto combine_discontinuous3(BidIt first1, BidIt last1, iter_diff_type<BidIt> d1, BidIt first2,
                            BidIt last2, iter_diff_type<BidIt> d2, BidIt first3, BidIt last3,
                            iter_diff_type<BidIt> d3, Function &f) -> bool
{
    using F = call_combine_discontinuous<Function &, BidIt>;
    F fbc(first2, last2, d2, first3, last3, d3, f);// BC
    return internal_pc::combine_discontinuous3_1(first1, last1, d1, first2, last2, d2, first3,
                                                 last3, d3, fbc);
}

// See permute
template <typename BidIt, typename Function>
auto permute1(BidIt first1, BidIt last1, iter_diff_type<BidIt> d1, Function &f) -> bool
{
    using std::swap;
    switch (d1) {
    case 0:
    case 1: return f();
    case 2:
        if (f()) return true;
        swap(*first1, *astl::next(first1));
        return f();
    case 3: {
        if (f()) return true;
        BidIt f2(astl::next(first1));
        BidIt f3(astl::next(f2));
        swap(*f2, *f3);
        if (f()) return true;
        swap(*first1, *f3);
        swap(*f2, *f3);
        if (f()) return true;
        swap(*f2, *f3);
        if (f()) return true;
        swap(*first1, *f2);
        swap(*f2, *f3);
        if (f()) return true;
        swap(*f2, *f3);
        return f();
    }
    default:;
    }
    BidIt fp1(astl::next(first1));
    BidIt p(fp1);
    while (p != last1) {
        if (internal_pc::permute1(fp1, last1, d1 - 1, f)) return true;
        std::reverse(fp1, last1);
        swap(*first1, *p);
        ++p;
    }
    return internal_pc::permute1(fp1, last1, d1 - 1, f);
}

// Calls f() for each permutation of [first1, last1)
// Divided into permute and permute1 in a (perhaps futile) attempt to
//    squeeze a little more performance out of it.
template <typename BidIt, typename Function>
auto permute(BidIt first1, BidIt last1, iter_diff_type<BidIt> d1, Function &f) -> bool
{
    using std::swap;
    switch (d1) {
    case 0:
    case 1: return f();
    case 2: {
        if (f()) return true;
        BidIt i(astl::next(first1));
        swap(*first1, *i);
        if (f()) return true;
        swap(*first1, *i);
    } break;
    case 3: {
        if (f()) return true;
        BidIt f2(astl::next(first1));
        BidIt f3(astl::next(f2));
        swap(*f2, *f3);
        if (f()) return true;
        swap(*first1, *f3);
        swap(*f2, *f3);
        if (f()) return true;
        swap(*f2, *f3);
        if (f()) return true;
        swap(*first1, *f2);
        swap(*f2, *f3);
        if (f()) return true;
        swap(*f2, *f3);
        if (f()) return true;
        swap(*first1, *f3);
    } break;
    default:
        BidIt fp1(astl::next(first1));
        BidIt p(fp1);
        while (p != last1) {
            if (internal_pc::permute1(fp1, last1, d1 - 1, f)) return true;
            std::reverse(fp1, last1);
            swap(*first1, *p);
            ++p;
        }
        if (internal_pc::permute1(fp1, last1, d1 - 1, f)) return true;
        std::reverse(first1, last1);
        break;
    }
    return false;
}

// Creates a functor with no arguments which calls f(first, last).
//   Also has a variant that takes two It and ignores them.
template <typename Function, typename It> struct bound_range {
private:
    Function f;
    It first;
    It last;

public:
    bound_range(Function f, It first, It last) : f(f), first(first), last(last) {}

    auto operator()() -> bool { return f(first, last); }

    auto operator()(It, It) -> bool { return f(first, last); }
};

// A binder for binding arguments to call permute
template <typename Function, typename It> struct call_permute {
private:
    using D = typename std::iterator_traits<It>::difference_type;
    Function f;
    It first;
    It last;
    D d;

public:
    call_permute(Function f, It first, It last, D d) : f(f), first(first), last(last), d(d) {}

    auto operator()() -> bool { return permute(first, last, d, f); }
};

template <typename Int>
auto check_non_negative(Int, Int) ->
    typename std::enable_if<std::is_unsigned<Int>::value, bool>::type
{
    return true;
}

template <typename Int>
auto check_non_negative(Int d1, Int d2) ->
    typename std::enable_if<!std::is_unsigned<Int>::value, bool>::type
{
    if (d1 < Int(0) || d2 < Int(0)) return false;

    return true;
}

}// namespace internal_pc

namespace i
{
template <typename BidIt, typename Function>
auto for_each_combination(BidIt first, BidIt mid, BidIt last, Function f) -> Function
{
    internal_pc::bound_range<Function &, BidIt> wfunc(f, first, mid);
    internal_pc::combine_discontinuous(first, mid, std::distance(first, mid), mid, last,
                                       std::distance(mid, last), wfunc);
    return f;
}

template <typename UInt>
ASTL_NODISCARD auto count_each_combination(UInt d1, UInt d2) -> optional<UInt>
{
    if (!internal_pc::check_non_negative(d1, d2)) return nullopt;

    if (d2 < d1) std::swap(d1, d2);

    if (d1 == UInt()) return make_optional(1);

    if (d1 > std::numeric_limits<UInt>::max() - d2) return nullopt;

    UInt n(d1 + d2);
    UInt r(n);
    --n;
    UInt k(UInt(2));
    while (k <= d1) {
        // r = r * n / k, known to not not have truncation error
        UInt g(astl::gcd(r, k));
        r /= g;
        UInt t(n / (k / g));
        if (r > std::numeric_limits<UInt>::max() / t) return nullopt;
        r *= t;
        ++k;
        --n;
    }
    return astl::make_optional(r);
}

template <typename BidIt>
ASTL_NODISCARD auto count_each_combination(BidIt first, BidIt mid, BidIt last)
    -> optional<std::uintmax_t>
{
    return i::count_each_combination<std::uintmax_t>(std::distance(first, mid),
                                                     std::distance(mid, last));
}

// For each of the permutation algorithms, use for_each_combination (or
//    combine_discontinuous) to handle the "r out of N" part of the algorithm.
//    Thus each permutation algorithm has to deal only with an "N out of N"
//    problem.  I.e. For each combination of r out of N items, permute it
//    thusly.
template <typename BidIt, typename Function>
auto for_each_permutation(BidIt first, BidIt mid, BidIt last, Function f) -> Function
{
    using D = iter_diff_type<BidIt>;
    using Wf = internal_pc::bound_range<Function &, BidIt>;
    using Pf = internal_pc::call_permute<Wf, BidIt>;
    Wf wfunc(f, first, mid);
    D d1(std::distance(first, mid));
    Pf pf(wfunc, first, mid, d1);
    internal_pc::combine_discontinuous(first, mid, d1, mid, last, std::distance(mid, last), pf);
    return f;
}

// if function returns nullopt overflow happenes
template <typename UInt>
ASTL_NODISCARD auto count_each_permutation(UInt d1, UInt d2) -> optional<UInt>
{
    // return (d1+d2)!/d2!
    if (!internal_pc::check_non_negative(d1, d2) || d1 > std::numeric_limits<UInt>::max() - d2)
        return nullopt;

    UInt n(d1 + d2);
    UInt r(1);
    while (n > d2) {
        if (r > std::numeric_limits<UInt>::max() / n) return nullopt;

        r *= n;
        --n;
    }
    return astl::make_optional(r);
}

// if function returns nullopt overflow happenes
template <typename BidIt>
ASTL_NODISCARD auto count_each_permutation(BidIt first, BidIt mid, BidIt last)
    -> optional<std::uintmax_t>
{
    return i::count_each_permutation<std::uintmax_t>(std::distance(first, mid),
                                                     std::distance(mid, last));
}
}// namespace i
namespace internal_pc
{
// Adapt functor to permute over [first+1, last)
//   A circular permutation of N items is done by holding the first item and
//   permuting [first+1, last).
template <typename Function, typename BidIt> struct circular_permutation {
private:
    using D = iter_diff_type<BidIt>;

    Function fn;
    D s;

public:
    explicit circular_permutation(Function f, D s) : fn(f), s(s) {}

    auto operator()(BidIt first, BidIt last) -> bool
    {
        if (s <= 1) return fn(first, last);
        bound_range<Function, BidIt> f(fn, first, last);
        return internal_pc::permute(astl::next(first), last, s - 1, f);
    }
};

}// namespace internal_pc

namespace i
{
template <typename BidIt, typename Function>
auto for_each_circular_permutation(BidIt first, BidIt mid, BidIt last, Function f) -> Function
{
    i::for_each_combination(
        first, mid, last,
        internal_pc::circular_permutation<Function &, BidIt>(f, std::distance(first, mid)));
    return f;
}

template <typename UInt>
ASTL_NODISCARD auto count_each_circular_permutation(UInt d1, UInt d2) -> optional<UInt>
{
    // return d1 > 0 ? (d1+d2)!/(d1*d2!) : 1
    if (!internal_pc::check_non_negative(d1, d2)) return nullopt;

    if (d1 == UInt()) return make_optional(1);

    optional<UInt> op(i::count_each_combination(d1, d2));
    if (!op) return nullopt;

    UInt &r(*op);
    if (d1 <= d2) {
        --d1;
        while (d1 > UInt(1)) {
            if (r > std::numeric_limits<UInt>::max() / d1) return nullopt;

            r *= d1;
            --d1;
        }
    }
    else {// functionally equivalent but faster algorithm
        if (d1 > std::numeric_limits<UInt>::max() - d2) return nullopt;
        UInt n(d1 + d2);
        r = 1;
        while (n > d1) {
            if (r > std::numeric_limits<UInt>::max() / n) return nullopt;
            r *= n;
            --n;
        }
        --n;
        while (n > d2) {
            if (r > std::numeric_limits<UInt>::max() / n) return nullopt;
            r *= n;
            --n;
        }
    }
    return astl::make_optional(r);
}

template <typename BidIt>
ASTL_NODISCARD auto count_each_circular_permutation(BidIt first, BidIt mid, BidIt last)
    -> optional<std::uintmax_t>
{
    return i::count_each_circular_permutation<std::uintmax_t>(std::distance(first, mid),
                                                              std::distance(mid, last));
}
}// namespace i

namespace internal_pc
{
// Difficult!!!  See notes for operator().
template <typename Function, typename Size> struct reversible_permutation {
private:
    Function fn;
    Size s;

public:
    reversible_permutation(Function f, Size s) : fn(f), s(s) {}

    template <typename BidIt> auto operator()(BidIt first, BidIt last) -> bool;
};

// rev1 looks like call_permute
template <typename Function, typename BidIt> struct rev1 {
private:
    using D = iter_diff_type<BidIt>;

    Function f;
    BidIt first1;
    BidIt last1;
    D d1;

public:
    rev1(Function f, BidIt first, BidIt last, D d) : f(f), first1(first), last1(last), d1(d) {}

    auto operator()() -> bool { return internal_pc::permute(first1, last1, d1, f); }
};

// For each permutation in [first1, last1),
//     call f() for each permutation of [first2, last2).
template <typename Function, typename BidIt> struct rev2 {
private:
    using D = iter_diff_type<BidIt>;

    Function fn;
    BidIt first1;
    BidIt last1;
    D d1;
    BidIt first2;
    BidIt last2;
    D d2;

public:
    rev2(Function f, BidIt first1, BidIt last1, D d1, BidIt first2, BidIt last2, D d2)
        : fn(f), first1(first1), last1(last1), d1(d1), first2(first2), last2(last2), d2(d2)
    {}

    auto operator()() -> bool
    {
        call_permute<Function, BidIt> f(fn, first2, last2, d2);
        return internal_pc::permute(first1, last1, d1, f);
    }
};

// For each permutation in [first1, last1),
//     and for each permutation of [first2, last2)
//     call f() for each permutation of [first3, last3).
template <typename Function, typename BidIt> struct rev3 {
private:
    using D = iter_diff_type<BidIt>;

    Function fn;
    BidIt first1;
    BidIt last1;
    D d1;
    BidIt first2;
    BidIt last2;
    D d2;
    BidIt first3;
    BidIt last3;
    D d3;

public:
    rev3(Function f, BidIt first1, BidIt last1, D d1, BidIt first2, BidIt last2, D d2, BidIt first3,
         BidIt last3, D d3)
        : fn(f), first1(first1), last1(last1), d1(d1), first2(first2), last2(last2), d2(d2),
          first3(first3), last3(last3), d3(d3)
    {}

    auto operator()() -> bool
    {
        rev2<Function, BidIt> f(fn, first2, last2, d2, first3, last3, d3);
        return internal_pc::permute(first1, last1, d1, f);
    }
};

// There are simpler implementations.  I believe the simpler ones are far more
//     expensive.
template <typename Function, typename Size>
template <typename BidIt>
auto reversible_permutation<Function, Size>::operator()(BidIt first, BidIt last) -> bool
{
    // using difference_type = iter_diff_type<BidIt>;
    using F2 = rev2<bound_range<Function &, BidIt>, BidIt>;
    using F3 = rev3<bound_range<Function &, BidIt>, BidIt>;
    // When the range is 0 - 2, then this is just a combination of N out of N
    //   elements.
    if (s < 3) return fn(first, last);
    using std::swap;
    // Hold the first element steady and call f(first, last) for each
    //    permutation in [first+1, last).
    BidIt a(astl::next(first));
    bound_range<Function &, BidIt> f(fn, first, last);

    if (internal_pc::permute(a, last, s - 1, f)) return true;
    // Beginning with the first element, swap the previous element with the
    //    next element.  For each swap, call f(first, last) for each
    //    permutation of the discontinuous range:
    //    [prior to the original element] + [after the original element].
    Size s2(s / 2);
    BidIt am1(first);
    BidIt ap1(astl::next(a));
    Size i(1);
    while (i < s2) {
        swap(*am1, *a);
        F2 f2(f, first, a, i, ap1, last, s - i - 1);
        if (internal_pc::combine_discontinuous(first, a, i, ap1, last, s - i - 1, f2)) return true;

        ++i;
        ++am1;
        ++a;
        ++ap1;
    }
    // If [first, last) has an even number of elements, then fix it up to the
    //     original permutation.
    if (2 * s2 == s) { std::rotate(first, am1, a); }
    // else if the range has length 3, we need one more call and the fix is easy.
    else if (s == 3) {
        swap(*am1, *a);
        if (f(first, last)) return true;
        swap(*am1, *a);
    }
    // else the range is an odd number greater than 3.  We need to permute
    //     through exactly half of the permutations with the original element in
    //     the middle.
    else {
        // swap the original first element into the middle, and hold the current
        //   first element steady.  This creates a discontinuous range:
        //     [first+1, middle) + [middle+1, last).  Run through all permutations
        //     of that discontinuous range.
        swap(*am1, *a);
        BidIt b(first);
        BidIt bp1(astl::next(b));
        F2 f2(f, bp1, a, s2 - 1, ap1, last, s - s2 - 1);
        if (internal_pc::combine_discontinuous(bp1, a, s2 - 1, ap1, last, s - s2 - 1, f2))
            return true;
        // Swap the current first element into every place from first+1 to middle-1.
        //   For each location, hold it steady to create the following discontinuous
        //   range (made of 3 ranges): [first, b-1) + [b+1, middle) + [middle+1,
        //   last). For each b in [first+1, middle-1), run through all permutations
        //   of
        //      the discontinuous ranges.
        b = bp1;
        ++bp1;
        BidIt bm1(first);
        i = 1;
        while (i < s2 - 1) {
            swap(*bm1, *b);
            F3 f3(f, first, b, i, bp1, a, s2 - i - 1, ap1, last, s - s2 - 1);
            if (internal_pc::combine_discontinuous3(first, b, i, bp1, a, s2 - i - 1, ap1, last,
                                                    s - s2 - 1, f3))
                return true;

            ++i;
            ++bm1;
            ++b;
            ++bp1;
        }
        // swap b into into middle-1, creates a discontinuous range:
        //     [first, middle-1) + [middle+1, last).  Run through all permutations
        //     of that discontinuous range.
        swap(*bm1, *b);
        F2 f21(f, first, b, s2 - 1, ap1, last, s - s2 - 1);
        if (internal_pc::combine_discontinuous(first, b, s2 - 1, ap1, last, s - s2 - 1, f21))
            return true;
        // Revert [first, last) to original order
        std::reverse(first, b);
        std::reverse(first, ap1);
    }
    return false;
}

}// namespace internal_pc
namespace i
{
template <typename BidIt, typename Function>
auto for_each_reversible_permutation(BidIt first, BidIt mid, BidIt last, Function f) -> Function
{
    using D = iter_diff_type<BidIt>;
    i::for_each_combination(
        first, mid, last,
        internal_pc::reversible_permutation<Function &, D>(f, std::distance(first, mid)));
    return f;
}

template <typename UInt>
ASTL_NODISCARD auto count_each_reversible_permutation(UInt d1, UInt d2) -> optional<UInt>
{
    // return d1 > 1 ? (d1+d2)!/(2*d2!) : (d1+d2)!/d2!
    if (!internal_pc::check_non_negative(d1, d2) || d1 > std::numeric_limits<UInt>::max() - d2)
        return nullopt;

    UInt n(d1 + d2);
    UInt r(1);
    if (d1 > UInt(1)) {
        r = n;
        if ((n & UInt(1)) == UInt(0)) r /= UInt(2);
        --n;
        UInt t(n);
        if ((t & UInt(1)) == UInt(0)) t /= UInt(2);

        if (r > std::numeric_limits<UInt>::max() / t) return nullopt;
        r *= t;
        --n;
    }
    while (n > d2) {
        if (r > std::numeric_limits<UInt>::max() / n) return nullopt;
        r *= n;
        --n;
    }
    return astl::make_optional(r);
}

template <typename BidIt>
ASTL_NODISCARD auto count_each_reversible_permutation(BidIt first, BidIt mid, BidIt last)
    -> optional<std::uintmax_t>
{
    return i::count_each_reversible_permutation<std::uintmax_t>(std::distance(first, mid),
                                                                std::distance(mid, last));
}
}// namespace i

namespace internal_pc
{
// Adapt functor to permute over [first+1, last)
//   A reversible circular permutation of N items is done by holding the first
//   item and reverse-permuting [first+1, last).
template <typename Function, typename BidIt> struct reverse_circular_permutation {
private:
    using D = iter_diff_type<BidIt>;

    Function fn;
    D s;

public:
    explicit reverse_circular_permutation(Function f, D s) : fn(f), s(s) {}

    auto operator()(BidIt first, BidIt last) -> bool
    {
        if (s == 1) return fn(first, last);
        using D1 = iter_diff_type<BidIt>;
        using BoundFunc = bound_range<Function, BidIt>;
        BoundFunc f(fn, first, last);
        BidIt n(astl::next(first));
        return reversible_permutation<BoundFunc, D1>(f, std::distance(n, last))(n, last);
    }
};

}// namespace internal_pc

namespace i
{
template <typename BidIt, typename Function>
auto for_each_reversible_circular_permutation(BidIt first, BidIt mid, BidIt last, Function f)
    -> Function
{
    i::for_each_combination(
        first, mid, last,
        internal_pc::reverse_circular_permutation<Function &, BidIt>(f, std::distance(first, mid)));
    return f;
}

template <typename UInt>
ASTL_NODISCARD auto count_each_reversible_circular_permutation(UInt d1, UInt d2) -> optional<UInt>
{
    // return d1 == 0 ? 1 : d1 <= 2 ? (d1+d2)!/(d1*d2!) : (d1+d2)!/(2*d1*d2!)
    if (!internal_pc::check_non_negative(d1, d2) || d1 > std::numeric_limits<UInt>::max() - d2)
        return nullopt;

    optional<UInt> op(i::count_each_combination(d1, d2));
    if (!op) return nullopt;

    UInt &r(*op);

    if (d1 > UInt(3)) {
        --d1;
        while (d1 > UInt(2)) {
            if (r > std::numeric_limits<UInt>::max() / d1) return nullopt;
            r *= d1;
            --d1;
        }
    }
    return astl::make_optional(r);
}

template <typename BidIt>
ASTL_NODISCARD auto count_each_reversible_circular_permutation(BidIt first, BidIt mid, BidIt last)
    -> optional<std::uintmax_t>
{
    return i::count_each_reversible_circular_permutation<std::uintmax_t>(std::distance(first, mid),
                                                                         std::distance(mid, last));
}
}// namespace i
}// namespace astl

#endif// ASTL_INCLUDE_PERMUTATION_COMBINATIONS_HPP
