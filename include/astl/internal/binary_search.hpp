//
// Created by Rijad on 29-Jul-18.
//

#ifndef ASTL_INCLUDE_BINARY_SEARCH_HPP
#define ASTL_INCLUDE_BINARY_SEARCH_HPP

#include <algorithm>
#include <utility>

#include "lower_bound.hpp"

#include "astl/functional.hpp"
#include "astl/iterator.hpp"
#include "astl/range_access.hpp"

namespace astl
{
namespace i
{
template <typename FwdIt, typename T, typename Comparator>
// requires FwdIt ForwardIterator
// requires T to be comparable with value_type(FwdIt) via Comparator
// requires Comparator, returns bool, takes two arguments of type T, value_type(FwdIt)
ASTL_NODISCARD auto binary_find(FwdIt first, FwdIt last, T const &value, Comparator comp) -> FwdIt
{
    // Note: BOTH type T and the value_type(FwdIt) must be implicitly convertible
    // to BOTH Type1 and Type2, used in comp. This is stricter than lower_bound
    // requirement
    FwdIt i(std::lower_bound(first, last, value, astl::pass_fn(comp)));
    return i != last && !comp(value, *i) ? i : last;
}

template <typename FwdIt, typename T>
// requires FwdIt ForwardIterator
// requires T to be equality comparable with value_type(FwdIt)
ASTL_NODISCARD auto binary_find(FwdIt first, FwdIt last, T const &value) -> FwdIt
{
    // Note: BOTH type T and the value_type(FwdIt) must be implicitly convertible
    // to BOTH Type1 and Type2, used in comp. This is stricter than lower_bound
    // requirement
    return i::binary_find(first, last, value, std::less{});
}

template <typename FwdIt, typename T, typename Comparator, typename P>
ASTL_NODISCARD auto binary_find(FwdIt first, FwdIt last, T const &value, Comparator comp, P p)
    -> FwdIt
{
    // Note: BOTH type T and the value_type(FwdIt) must be implicitly convertible
    // to BOTH Type1 and Type2, used in comp. This is stricter than lower_bound
    // requirement
    FwdIt i(i::lower_bound(first, last, value, astl::pass_fn(comp), astl::pass_fn(p)));
    return i != last && !comp(value, invoke(p, *i)) ? i : last;
}

template <typename FwdIt, typename N, typename T, typename Comparator>
// requires FwdIt ForwardIterator
// requires N integral type
// requires T to be comparable with value_type(FwdIt) via Comparator
// requires Comparator, returns bool, takes two arguments of type T, value_type(FwdIt)
ASTL_NODISCARD auto binary_find_n(FwdIt first, N n, T const &value, Comparator comp)
    -> std::pair<FwdIt, N>
{
    // Note: BOTH type T and the value_type(FwdIt) must be implicitly convertible
    // to BOTH Type1 and Type2, used in comp. This is stricter than lower_bound
    // requirement
    std::pair<FwdIt, N> i(i::lower_bound_n(first, n, value, astl::pass_fn(comp)));
    if (i.second == n || comp(value, *i.first)) i.second = n;

    return i;
}

template <typename FwdIt, typename N, typename T>
// requires FwdIt ForwardIterator
// requires N integral type
// requires T to be equality comparable with value_type(FwdIt)
ASTL_NODISCARD auto binary_find_n(FwdIt first, N n, T const &value) -> std::pair<FwdIt, N>
{
    // Note: BOTH type T and the value_type(FwdIt) must be implicitly convertible
    // to BOTH Type1 and Type2, used in comp. This is stricter than lower_bound
    // requirement
    return i::binary_find_n(first, n, value, std::less{});
}

template <typename FwdIt, typename N, typename T, typename Comparator, typename P>
ASTL_NODISCARD auto binary_find_n(FwdIt first, N n, T const &value, Comparator comp, P p)
    -> std::pair<FwdIt, N>
{
    // Note: BOTH type T and the value_type(FwdIt) must be implicitly convertible
    // to BOTH Type1 and Type2, used in comp. This is stricter than lower_bound
    // requirement

    std::pair<FwdIt, N> i(i::lower_bound_n(first, n, value, astl::pass_fn(comp), astl::pass_fn(p)));
    if (i.second == n || comp(value, invoke(p, *i.first))) i.second = n;

    return i;
}

using std::binary_search; // NOLINT(misc-unused-using-decls)
template <typename FwdIt, typename T, typename Comparator, typename P>
ASTL_NODISCARD auto binary_search(FwdIt first, FwdIt last, T const &value, Comparator comp, P p)
    -> bool
{
    return std::binary_search(first, last, value,
                              astl::combine(astl::pass_fn(comp), astl::pass_fn(p)));
}

template <typename FwdIt, typename N, typename T, typename Comparator>
// requires FwdIt ForwardIterator
// requires N integral type
// requires T to be comparable with value_type(FwdIt) via Comparator
// requires Comparator, returns bool, takes two arguments of type T, value_type(FwdIt)
ASTL_NODISCARD auto binary_search_n(FwdIt first, N n, T const &value, Comparator comp) -> bool
{
    std::pair<FwdIt, N> i(i::lower_bound_n(first, n, value, astl::pass_fn(comp)));
    return i.second != n && !comp(value, *i.first);
}

template <typename FwdIt, typename N, typename T>
// requires FwdIt ForwardIterator
// requires N integral type
// requires T to be comparable(less) with value_type(FwdIt)
ASTL_NODISCARD auto binary_search_n(FwdIt first, N n, T const &value) -> bool
{
    return i::binary_search_n(first, n, value, std::less{});
}

template <typename FwdIt, typename N, typename T, typename Comparator, typename P>
ASTL_NODISCARD auto binary_search_n(FwdIt first, N n, T const &value, Comparator comp, P p) -> bool
{
    std::pair<FwdIt, N> i(i::lower_bound_n(first, n, value, astl::pass_fn(comp), astl::pass_fn(p)));
    return i.second != n && !comp(value, invoke(p, *i.first));
}

template <typename RandIt, typename T>
// requires RandIt RandomAccessIterator
// requires T to be equality comparable with value_type(RandIt)
ASTL_NODISCARD auto interpolation_search(RandIt first, RandIt const last, T const &value) -> RandIt
{
    static_assert(is_randit<RandIt>::value, "Iterators must be random access");
    iter_diff_type<RandIt> hi(last - first - 1);
    iter_diff_type<RandIt> lo(0);
    while (lo <= hi && value >= first[lo] && first[hi] >= value) {
        iter_diff_type<RandIt> pos(
            lo + ((static_cast<double>(hi - lo) / (first[hi] - first[lo])) * (value - first[lo])));

        if (first[pos] > value) hi = pos - 1;
        else if (first[pos] < value)
            lo = pos + 1;
        else
            return first + pos;
    }
    return last;
}

template <typename RandIt, typename T, typename Comparator>
// requires RandIt RandomAccessIterator
// requires T to be comparable with value_type(RandIt) via Comparator
// requires Comparator, returns bool, takes two arguments of type T,
// value_type(RandIt)
ASTL_NODISCARD auto interpolation_search(RandIt first, RandIt const last, T const &value,
                                         Comparator comp) -> RandIt
{
    static_assert(is_randit<RandIt>::value, "Iterators must be random access");
    iter_diff_type<RandIt> hi(last - first - 1);
    iter_diff_type<RandIt> lo(0);
    while (lo <= hi && !comp(value, first[lo]) && !comp(first[hi], value)) {
        iter_diff_type<RandIt> pos(
            lo + ((static_cast<double>(hi - lo) / (first[hi] - first[lo])) * (value - first[lo])));

        if (comp(value, first[pos])) hi = pos - 1;
        else if (comp(first[pos], value))
            lo = pos + 1;
        else
            return first + pos;
    }
    return last;
}

template <typename RandIt, typename N, typename T, typename Comparator>
// requires RandIt RandomAccessIterator
// requires N integral type
// requires T to be comparable with value_type(RandIt) via Comparator
// requires Comparator, returns bool, takes two arguments of type T,
// value_type(RandIt)
ASTL_NODISCARD auto interpolation_search_n(RandIt first, N n, T const &value, Comparator comp)
    -> std::pair<RandIt, N>
{
    RandIt last(first + n);
    RandIt i(i::interpolation_search(first, last, value, astl::pass_fn(comp)));
    return std::make_pair(i, last - i);
}

template <typename RandIt, typename N, typename T>
// requires RandIt RandomAccessIterator
// requires N integral type
// requires T to be equality comparable with value_type(RandIt)
ASTL_NODISCARD auto interpolation_search_n(RandIt first, N n, T const &value)
    -> std::pair<RandIt, N>
{
    RandIt last(first + n);
    RandIt i(i::interpolation_search(first, last, value));
    return std::make_pair(i, last - i);
}
} // namespace i

namespace r
{
template <typename R, typename T>
// requires R ForwardIterator range
// requires T to be comparable with value_type(R) via Comparator
ASTL_NODISCARD auto binary_find(R &&r, T const &value) -> iter_of_range<R>
{
    // Note: BOTH type T and the value_type(FwdIt) must be implicitly convertible
    // to BOTH Type1 and Type2, used in comp. This is stricter than lower_bound
    // requirement
    return i::binary_find(adl::begin(r), adl::end(r), value);
}

template <typename R, typename T, typename Comparator>
// requires R ForwardIterator range
// requires T to be comparable with value_type(R) via Comparator
// requires Comparator, returns bool, takes two arguments of type T, value_type(R)
ASTL_NODISCARD auto binary_find(R &&r, T const &value, Comparator comp) -> iter_of_range<R>
{
    // Note: BOTH type T and the value_type(R) must be implicitly convertible to
    // BOTH Type1 and Type2, used in comp. This is stricter than lower_bound
    // requirement
    return i::binary_find(adl::begin(r), adl::end(r), value, astl::pass_fn(comp));
}

template <typename R, typename T, typename Comparator, typename P>
ASTL_NODISCARD auto binary_find(R &&r, T const &value, Comparator comp, P p) -> iter_of_range<R>
{
    // Note: BOTH type T and the value_type(R) must be implicitly convertible to
    // BOTH Type1 and Type2, used in comp. This is stricter than lower_bound
    // requirement
    return i::binary_find(adl::begin(r), adl::end(r), value, astl::pass_fn(comp), astl::pass_fn(p));
}

template <typename R, typename N, typename T>
// requires R ForwardIterator range
// requires N integral type
// requires T to be comparable(less) with value_type(R)
ASTL_NODISCARD auto binary_find_n(R &&r, N n, T const &value) -> std::pair<iter_of_range<R>, N>
{
    return i::binary_find_n(adl::begin(r), n, value);
}

template <typename R, typename N, typename T, typename Comparator>
// requires R ForwardIterator range
// requires N integral type
// requires T to be comparable with value_type(R) via Comparator
// requires Comparator, returns bool, takes two arguments T and value_type(R)
ASTL_NODISCARD auto binary_find_n(R &&r, N n, T const &value, Comparator comp)
    -> std::pair<iter_of_range<R>, N>
{
    return i::binary_find_n(adl::begin(r), n, value, astl::pass_fn(comp));
}

template <typename R, typename N, typename T, typename Comparator, typename P>
ASTL_NODISCARD auto binary_find_n(R &&r, N n, T const &value, Comparator comp, P p)
    -> std::pair<iter_of_range<R>, N>
{
    return i::binary_find_n(adl::begin(r), n, value, astl::pass_fn(comp), astl::pass_fn(p));
}

template <typename R, typename T>
// requires R ForwardIterator range
// requires T to be comparable(less) with value_type(R)
ASTL_NODISCARD auto binary_search(R &&r, T const &value) -> bool
{
    return i::binary_search(adl::begin(r), adl::end(r), value);
}

template <typename R, typename T, typename Comparator>
// requires R ForwardIterator range
// requires T to be comparable with value_type(R) via Comparator
// requires Comparator, returns bool, takes two arguments T and value_type(R)
ASTL_NODISCARD auto binary_search(R &&r, T const &value, Comparator comp) -> bool
{
    return i::binary_search(adl::begin(r), adl::end(r), value, astl::pass_fn(comp));
}

template <typename R, typename T, typename Comparator, typename P>
ASTL_NODISCARD auto binary_search(R &&r, T const &value, Comparator comp, P p) -> bool
{
    return i::binary_search(adl::begin(r), adl::end(r), value, astl::pass_fn(comp),
                            astl::pass_fn(p));
}

template <typename R, typename N, typename T>
// requires R ForwardIterator range
// requires N integral type
// requires T to be comparable(less) with value_type(R)
ASTL_NODISCARD auto binary_search_n(R &&r, N n, T const &value) -> bool
{
    return i::binary_search_n(adl::begin(r), n, value);
}

template <typename R, typename N, typename T, typename Comparator>
// requires R ForwardIterator range
// requires N integral type
// requires T to be comparable with value_type(R) via Comparator
// requires Comparator, returns bool, takes two arguments T and value_type(R)
ASTL_NODISCARD auto binary_search_n(R &&r, N n, T const &value, Comparator comp) -> bool
{
    return i::binary_search_n(adl::begin(r), n, value, astl::pass_fn(comp));
}

template <typename R, typename N, typename T, typename Comparator, typename P>
ASTL_NODISCARD auto binary_search_n(R &&r, N n, T const &value, Comparator comp, P p) -> bool
{
    return i::binary_search_n(adl::begin(r), n, value, astl::pass_fn(comp), astl::pass_fn(p));
}

template <typename R, typename T>
// requires R RandomAccessIterator range
// requires T to be comparable with value_type(RandIt) via Comparator
ASTL_NODISCARD auto interpolation_search(R &&r, T const &value) -> iter_of_range<R>
{
    return i::interpolation_search(adl::begin(r), adl::end(r), value);
}

template <typename R, typename T, typename Comparator>
// requires R RandomAccessIterator range
// requires T to be comparable with value_type(RandIt) via Comparator
// requires Comparator, returns bool, takes two arguments T and value_type(RandIt)
ASTL_NODISCARD auto interpolation_search(R &&r, T const &value, Comparator comp) -> iter_of_range<R>
{
    return i::interpolation_search(adl::begin(r), adl::end(r), value, astl::pass_fn(comp));
}

template <typename R, typename N, typename T>
// requires R RandomAccessIterator range
// requires N integral type
// requires T to be comparable with value_type(RandIt) via Comparator
ASTL_NODISCARD auto interpolation_search_n(R &&r, N n, T const &value)
    -> std::pair<iter_of_range<R>, N>
{
    return i::interpolation_search_n(adl::begin(r), n, value);
}

template <typename R, typename N, typename T, typename Comparator>
// requires R RandomAccessIterator range
// requires N integral type
// requires T to be comparable with value_type(RandIt) via Comparator
// requires Comparator, returns bool, takes two arguments T and value_type(RandIt)
ASTL_NODISCARD auto interpolation_search_n(R &&r, N n, T const &value, Comparator comp)
    -> std::pair<iter_of_range<R>, N>
{
    return i::interpolation_search_n(adl::begin(r), n, value, astl::pass_fn(comp));
}
} // namespace r
} // namespace astl

#endif // ASTL_INCLUDE_BINARY_SEARCH_HPP
