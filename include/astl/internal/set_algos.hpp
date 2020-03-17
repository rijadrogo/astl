//
// Created by Rijad on 29-Jul-18.
//

#ifndef ASTL_INCLUDE_SET_ALGOS_HPP
#define ASTL_INCLUDE_SET_ALGOS_HPP

#include <algorithm>

#include "unique.hpp"

#include "astl/functional.hpp"
#include "astl/range_access.hpp"

namespace astl
{
namespace i
{
using std::includes; // NOLINT(misc-unused-using-decls)
template <typename InIt1, typename InIt2, typename Comparator, typename P1, typename P2>
ASTL_NODISCARD auto includes(InIt1 first1, InIt1 last1, InIt2 first2, InIt2 last2, Comparator comp,
                             P1 p1, P2 p2) -> bool
{
    return i::includes(first1, last1, first2, last2,
                       astl::lockstep(astl::pass_fn(comp), astl::pass_fn(p1), astl::pass_fn(p2)));
}

template <typename InIt1, typename N1, typename InIt2, typename N2, typename Comparator>
ASTL_NODISCARD auto includes_n(InIt1 first1, N1 n1, InIt2 first2, N2 n2, Comparator comp) -> bool
{
    while (n1 != N1(0) && n2 != N2(0)) {
        if (comp(*first2, *first1)) return false;

        if (comp(*first1, *first2)) {
            ++first1;
            --n1;
        }
        else {
            ++first1;
            --n1;
            ++first2;
            --n2;
        }
    }
    return n2 == N2(0);
}

template <typename InIt1, typename N1, typename InIt2, typename N2>
ASTL_NODISCARD auto includes_n(InIt1 first1, N1 n1, InIt2 first2, N2 n2) -> bool
{
    return i::includes_n(first1, n1, first2, n2, std::less{});
}

template <typename InIt1, typename N1, typename InIt2, typename N2, typename Comparator,
          typename P1, typename P2>
ASTL_NODISCARD auto includes_n(InIt1 first1, N1 n1, InIt2 first2, N2 n2, Comparator comp, P1 p1,
                               P2 p2) -> bool
{
    return i::includes_n(first1, n1, first2, n2,
                         astl::lockstep(astl::pass_fn(comp), astl::pass_fn(p1), astl::pass_fn(p2)));
}

using std::set_difference; // NOLINT(misc-unused-using-decls)
template <typename InIt1, typename InIt2, typename OutIt, typename Comparator, typename P1,
          typename P2>
auto set_difference(InIt1 first1, InIt1 last1, InIt2 first2, InIt2 last2, OutIt dest,
                    Comparator comp, P1 p1, P2 p2) -> OutIt
{
    while (first1 != last1 && first2 != last2) {
        if (comp(invoke(p1, *first1), invoke(p2, *first2))) {
            *dest = *first1;
            ++first1;
            ++dest;
        }
        else if (comp(invoke(p2, *first2), invoke(p1, *first1))) {
            ++first2;
        }
        else {
            ++first1;
            ++first2;
        }
    }
    return std::copy(first1, last1, dest);
}

template <typename InIt1, typename N1, typename InIt2, typename N2, typename OutIt,
          typename Comparator, typename P1, typename P2>
auto set_difference_n(InIt1 first1, N1 n1, InIt2 first2, N2 n2, OutIt dest, Comparator comp, P1 p1,
                      P2 p2) -> OutIt
{
    while (n1 != N1(0) && n2 != N2(0)) {
        if (comp(invoke(p1, *first1), invoke(p2, *first2))) {
            *dest = *first1;
            ++first1;
            --n1;
            ++dest;
        }
        else if (comp(invoke(p2, *first2), invoke(p1, *first1))) {
            ++first2;
            --n2;
        }
        else {
            ++first1;
            ++first2;
            --n1;
            --n2;
        }
    }
    return std::copy_n(first1, n1, dest).first;
}

template <typename InIt1, typename N1, typename InIt2, typename N2, typename OutIt,
          typename Comparator>
auto set_difference_n(InIt1 first1, N1 n1, InIt2 first2, N2 n2, OutIt dest, Comparator comp)
    -> OutIt
{
    while (n1 != N1(0) && n2 != N2(0)) {
        if (comp(*first1, *first2)) {
            *dest = *first1;
            ++first1;
            --n1;
            ++dest;
        }
        else if (comp(*first2, *first1)) {
            ++first2;
            --n2;
        }
        else {
            ++first1;
            ++first2;
            --n1;
            --n2;
        }
    }
    return std::copy_n(first1, n1, dest).first;
}

template <typename InIt1, typename N1, typename InIt2, typename N2, typename OutIt>
auto set_difference_n(InIt1 first1, N1 n1, InIt2 first2, N2 n2, OutIt dest) -> OutIt
{
    return i::set_difference_n(first1, n1, first2, n2, dest, std::less{});
}

template <typename InIt1, typename InIt2, typename OutIt, typename Comparator, typename Equal>
auto set_difference_unique(InIt1 first1, InIt1 last1, InIt2 first2, InIt2 last2, OutIt dest,
                           Comparator comp, Equal eq) -> OutIt
{
    while (first1 != last1 && first2 != last2) {
        if (comp(*first1, *first2)) {
            *dest = *first1;
            ++first1;
            break;
        }
        if (comp(*first2, *first1)) { ++first2; }
        else {
            ++first1;
            ++first2;
        }
    }

    while (first1 != last1 && first2 != last2) {
        if (comp(*first1, *first2)) {
            if (!eq(*first1, *dest)) { *++dest = *first1; }
            ++first1;
        }
        else if (comp(*first2, *first1)) {
            ++first2;
        }
        else {
            ++first1;
            ++first2;
        }
    }
    return std::unique_copy(first1, last1, dest, astl::pass_fn(eq));
}

template <typename InIt1, typename InIt2, typename OutIt, typename Comparator>
auto set_difference_unique(InIt1 first1, InIt1 last1, InIt2 first2, InIt2 last2, OutIt dest,
                           Comparator comp) -> OutIt
{
    return i::set_difference_unique(first1, last1, first2, last2, dest, astl::pass_fn(comp),
                                    std::equal_to{});
}

template <typename InIt1, typename InIt2, typename OutIt>
auto set_difference_unique(InIt1 first1, InIt1 last1, InIt2 first2, InIt2 last2, OutIt dest)
    -> OutIt
{
    return i::set_difference_unique(first1, last1, first2, last2, dest, std::less{},
                                    std::equal_to{});
}

template <typename InIt1, typename InIt2, typename OutIt, typename Comparator, typename Equal,
          typename P1, typename P2>
auto set_difference_unique(InIt1 first1, InIt1 last1, InIt2 first2, InIt2 last2, OutIt dest,
                           Comparator comp, Equal eq, P1 p1, P2 p2) -> OutIt
{
    while (first1 != last1 && first2 != last2) {
        if (comp(invoke(p1, *first1), invoke(p2, *first2))) {
            *dest = *first1;
            ++first1;
            break;
        }
        if (comp(invoke(p2, *first2), invoke(p1, *first1))) { ++first2; }
        else {
            ++first1;
            ++first2;
        }
    }

    while (first1 != last1 && first2 != last2) {
        if (comp(invoke(p1, *first1), invoke(p2, *first2))) {
            if (!eq(*first1, *dest)) { *++dest = *first1; }
            ++first1;
        }
        else if (comp(invoke(p2, *first2), invoke(p1, *first1))) {
            ++first2;
        }
        else {
            ++first1;
            ++first2;
        }
    }
    return std::unique_copy(first1, last1, dest, astl::pass_fn(eq));
}

template <typename InIt1, typename N1, typename InIt2, typename N2, typename OutIt,
          typename Comparator, typename Equal>
auto set_difference_unique_n(InIt1 first1, N1 n1, InIt2 first2, N2 n2, OutIt dest, Comparator comp,
                             Equal eq) -> OutIt
{
    while (n1 != N1(0) && n2 != N2(0)) {
        if (comp(*first1, *first2)) {
            *dest = *first1;
            ++first1;
            --n1;
            break;
        }
        if (comp(*first2, *first1)) {
            ++first2;
            --n2;
        }
        else {
            ++first1;
            --n1;
            ++first2;
            --n2;
        }
    }

    while (n1 != N1(0) && n2 != N2(0)) {
        if (comp(*first1, *first2)) {
            if (!eq(*first1, *dest)) { *++dest = *first1; }
            ++first1;
            --n1;
        }
        else if (comp(*first2, *first1)) {
            ++first2;
            --n2;
        }
        else {
            ++first1;
            --n1;
            ++first2;
            --n2;
        }
    }
    return i::unique_copy_n(first1, n1, dest, astl::pass_fn(eq)).first;
}
template <typename InIt1, typename N1, typename InIt2, typename N2, typename OutIt,
          typename Comparator>
auto set_difference_unique_n(InIt1 first1, N1 n1, InIt2 first2, N2 n2, OutIt dest, Comparator comp)
    -> OutIt
{
    return i::set_difference_unique_n(first1, n1, first2, n2, dest, astl::pass_fn(comp),
                                      std::equal_to{});
}

template <typename InIt1, typename N1, typename InIt2, typename N2, typename OutIt>
auto set_difference_unique_n(InIt1 first1, N1 n1, InIt2 first2, N2 n2, OutIt dest) -> OutIt
{
    return i::set_difference_unique_n(first1, n1, first2, n2, dest, std::less{}, std::equal_to{});
}

template <typename InIt1, typename N1, typename InIt2, typename N2, typename OutIt,
          typename Comparator, typename Equal, typename P1, typename P2>
auto set_difference_unique_n(InIt1 first1, N1 n1, InIt2 first2, N2 n2, OutIt dest, Comparator comp,
                             Equal eq, P1 p1, P2 p2) -> OutIt
{
    while (n1 != N1(0) && n2 != N2(0)) {
        if (comp(invoke(p1, *first1), invoke(p2, *first2))) {
            *dest = *first1;
            ++first1;
            --n1;
            break;
        }
        if (comp(invoke(p2, *first2), invoke(p1, *first1))) {
            ++first2;
            --n2;
        }
        else {
            ++first1;
            --n1;
            ++first2;
            --n2;
        }
    }

    while (n1 != N1(0) && n2 != N2(0)) {
        if (comp(invoke(p1, *first1), invoke(p2, *first2))) {
            if (!eq(*first1, *dest)) { *++dest = *first1; }
            ++first1;
            --n1;
        }
        else if (comp(invoke(p2, *first2), invoke(p1, *first1))) {
            ++first2;
            --n2;
        }
        else {
            ++first1;
            --n1;
            ++first2;
            --n2;
        }
    }
    return i::unique_copy_n(first1, n1, dest, astl::pass_fn(eq)).first;
}

using std::set_intersection; // NOLINT(misc-unused-using-decls)
template <typename InIt1, typename InIt2, typename OutIt, typename Comparator, typename P1,
          typename P2>
auto set_intersection(InIt1 first1, InIt1 last1, InIt2 first2, InIt2 last2, OutIt dest,
                      Comparator comp, P1 p1, P2 p2) -> OutIt
{
    while (first1 != last1 && first2 != last2) {
        if (comp(invoke(p1, *first1), invoke(p2, *first2))) { ++first1; }
        else if (comp(invoke(p2, *first2), invoke(p1, *first1))) {
            ++first2;
        }
        else {
            *dest = *first1;
            ++first1;
            ++first2;
            ++dest;
        }
    }
    return dest;
}

template <typename InIt1, typename N1, typename InIt2, typename N2, typename OutIt,
          typename Comparator>
auto set_intersection_n(InIt1 first1, N1 n1, InIt2 first2, N2 n2, OutIt dest, Comparator comp)
    -> OutIt
{
    while (n1 != N1(0) && n2 != N2(0)) {
        if (comp(*first1, *first2)) {
            ++first1;
            --n1;
        }
        else if (comp(*first2, *first1)) {
            ++first2;
            --n2;
        }
        else {
            *dest = *first1;
            ++first1;
            --n1;
            ++first2;
            --n2;
            ++dest;
        }
    }
    return dest;
}

template <typename InIt1, typename N1, typename InIt2, typename N2, typename OutIt>
auto set_intersection_n(InIt1 first1, N1 n1, InIt2 first2, N2 n2, OutIt dest) -> OutIt
{
    return i::set_intersection_n(first1, n1, first2, n2, dest, std::less{});
}

template <typename InIt1, typename N1, typename InIt2, typename N2, typename OutIt,
          typename Comparator, typename P1, typename P2>
auto set_intersection_n(InIt1 first1, N1 n1, InIt2 first2, N2 n2, OutIt dest, Comparator comp,
                        P1 p1, P2 p2) -> OutIt
{
    while (n1 != N1(0) && n2 != N2(0)) {
        if (comp(invoke(p1, *first1), invoke(p2, *first2))) {
            ++first1;
            --n1;
        }
        else if (comp(invoke(p2, *first2), invoke(p1, *first1))) {
            ++first2;
            --n2;
        }
        else {
            *dest = *first1;
            ++first1;
            --n1;
            ++first2;
            --n2;
            ++dest;
        }
    }
    return dest;
}

template <typename InIt1, typename InIt2, typename OutIt, typename Comparator, typename Equal>
auto set_intersection_unique(InIt1 first1, InIt1 last1, InIt2 first2, InIt2 last2, OutIt dest,
                             Comparator comp, Equal eq) -> OutIt
{
    while (first1 != last1 && first2 != last2) {
        if (comp(*first1, *first2)) { ++first1; }
        else if (comp(*first2, *first1)) {
            ++first2;
        }
        else {
            *dest = *first1;
            ++first1;
            ++first2;
            break;
        }
    }

    while (first1 != last1 && first2 != last2) {
        if (comp(*first1, *first2)) { ++first1; }
        else if (comp(*first2, *first1)) {
            ++first2;
        }
        else {
            if (!eq(*first1, *dest)) { *++dest = *first1; }
            ++first1;
            ++first2;
        }
    }
    return dest;
}

template <typename InIt1, typename InIt2, typename OutIt, typename Comparator>
auto set_intersection_unique(InIt1 first1, InIt1 last1, InIt2 first2, InIt2 last2, OutIt dest,
                             Comparator comp) -> OutIt
{
    return i::set_intersection_unique(first1, last1, first2, last2, dest, astl::pass_fn(comp),
                                      std::equal_to{});
}

template <typename InIt1, typename InIt2, typename OutIt>
auto set_intersection_unique(InIt1 first1, InIt1 last1, InIt2 first2, InIt2 last2, OutIt dest)
    -> OutIt
{
    return i::set_intersection_unique(first1, last1, first2, last2, dest, std::less{},
                                      std::equal_to{});
}

template <typename InIt1, typename InIt2, typename OutIt, typename Comparator, typename Equal,
          typename P1, typename P2>
auto set_intersection_unique(InIt1 first1, InIt1 last1, InIt2 first2, InIt2 last2, OutIt dest,
                             Comparator comp, Equal eq, P1 p1, P2 p2) -> OutIt
{
    while (first1 != last1 && first2 != last2) {
        if (comp(invoke(p1, *first1), invoke(p2, *first2))) { ++first1; }
        else if (comp(invoke(p2, *first2), invoke(p1, *first1))) {
            ++first2;
        }
        else {
            *dest = *first1;
            ++first1;
            ++first2;
            break;
        }
    }

    while (first1 != last1 && first2 != last2) {
        if (comp(invoke(p1, *first1), invoke(p2, *first2))) { ++first1; }
        else if (comp(invoke(p2, *first2), invoke(p1, *first1))) {
            ++first2;
        }
        else {
            if (!eq(*first1, *dest)) { *++dest = *first1; }
            ++first1;
            ++first2;
        }
    }
    return dest;
}

template <typename InIt1, typename N1, typename InIt2, typename N2, typename OutIt,
          typename Comparator, typename Equal>
auto set_intersection_unique_n(InIt1 first1, N1 n1, InIt2 first2, N2 n2, OutIt dest,
                               Comparator comp, Equal eq) -> OutIt
{
    while (n1 != N1(0) && n2 != N2(0)) {
        if (comp(*first1, *first2)) {
            ++first1;
            --n1;
        }
        else if (comp(*first2, *first1)) {
            ++first2;
            --n2;
        }
        else {
            *dest = *first1;
            ++first1;
            --n1;
            ++first2;
            --n2;
            break;
        }
    }

    while (n1 != N1(0) && n2 != N2(0)) {
        if (comp(*first1, *first2)) {
            ++first1;
            --n1;
        }
        else if (comp(*first2, *first1)) {
            ++first2;
            --n2;
        }
        else {
            if (!eq(*first1, *dest)) { *++dest = *first1; }
            ++first1;
            --n1;
            ++first2;
            --n2;
        }
    }
    return dest;
}

template <typename InIt1, typename N1, typename InIt2, typename N2, typename OutIt,
          typename Comparator>
auto set_intersection_unique_n(InIt1 first1, N1 n1, InIt2 first2, N2 n2, OutIt dest,
                               Comparator comp) -> OutIt
{
    return i::set_intersection_unique_n(first1, n1, first2, n2, dest, astl::pass_fn(comp),
                                        std::equal_to{});
}

template <typename InIt1, typename N1, typename InIt2, typename N2, typename OutIt>
auto set_intersection_unique_n(InIt1 first1, N1 n1, InIt2 first2, N2 n2, OutIt dest) -> OutIt
{
    return i::set_intersection_unique_n(first1, n1, first2, n2, dest, std::less{}, std::equal_to{});
}

template <typename InIt1, typename N1, typename InIt2, typename N2, typename OutIt,
          typename Comparator, typename Equal, typename P1, typename P2>
auto set_intersection_unique_n(InIt1 first1, N1 n1, InIt2 first2, N2 n2, OutIt dest,
                               Comparator comp, Equal eq, P1 p1, P2 p2) -> OutIt
{
    while (n1 != N1(0) && n2 != N2(0)) {
        if (comp(invoke(p1, *first1), invoke(p2, *first2))) {
            ++first1;
            --n1;
        }
        else if (comp(invoke(p2, *first2), invoke(p1, *first1))) {
            ++first2;
            --n2;
        }
        else {
            *dest = *first1;
            ++first1;
            --n1;
            ++first2;
            --n2;
            break;
        }
    }

    while (n1 != N1(0) && n2 != N2(0)) {
        if (comp(invoke(p1, *first1), invoke(p2, *first2))) {
            ++first1;
            --n1;
        }
        else if (comp(invoke(p2, *first2), invoke(p1, *first1))) {
            ++first2;
            --n2;
        }
        else {
            if (!eq(*first1, *dest)) { *++dest = *first1; }
            ++first1;
            --n1;
            ++first2;
            --n2;
        }
    }
    return dest;
}

using std::set_symmetric_difference; // NOLINT(misc-unused-using-decls)
template <typename InIt1, typename InIt2, typename OutIt, typename Comparator, typename P1,
          typename P2>
auto set_symmetric_difference(InIt1 first1, InIt1 last1, InIt2 first2, InIt2 last2, OutIt dest,
                              Comparator comp, P1 p1, P2 p2) -> OutIt
{
    while (first1 != last1 && first2 != last2) {
        if (comp(invoke(p1, *first1), invoke(p2, *first2))) {
            *dest = *first1;
            ++first1;
            ++dest;
        }
        else if (comp(invoke(p2, *first2), invoke(p1, *first1))) {
            *dest = *first2;
            ++first2;
            ++dest;
        }
        else {
            ++first1;
            ++first2;
        }
    }
    return std::copy(first2, last2, std::copy(first1, last1, dest));
}

template <typename InIt1, typename N1, typename InIt2, typename N2, typename OutIt,
          typename Comparator>
auto set_symmetric_difference_n(InIt1 first1, N1 n1, InIt2 first2, N2 n2, OutIt dest,
                                Comparator comp) -> OutIt
{
    while (n1 != N1(0) && n2 != N2(0)) {
        if (comp(*first1, *first2)) {
            *dest = *first1;
            ++first1;
            --n1;
            ++dest;
        }
        else if (comp(*first2, *first1)) {
            *dest = *first2;
            ++first2;
            --n2;
            ++dest;
        }
        else {
            ++first1;
            --n1;
            ++first2;
            --n2;
        }
    }
    return std::copy_n(first2, n2, std::copy_n(first1, n1, dest));
}

template <typename InIt1, typename N1, typename InIt2, typename N2, typename OutIt>
auto set_symmetric_difference_n(InIt1 first1, N1 n1, InIt2 first2, N2 n2, OutIt dest) -> OutIt
{
    return i ::set_symmetric_difference_n(first1, n1, first2, n2, dest, std::less{});
}

template <typename InIt1, typename N1, typename InIt2, typename N2, typename OutIt,
          typename Comparator, typename P1, typename P2>
auto set_symmetric_difference_n(InIt1 first1, N1 n1, InIt2 first2, N2 n2, OutIt dest,
                                Comparator comp, P1 p1, P2 p2) -> OutIt
{
    while (n1 != N1(0) && n2 != N2(0)) {
        if (comp(invoke(p1, *first1), invoke(p2, *first2))) {
            *dest = *first1;
            ++first1;
            --n1;
            ++dest;
        }
        else if (comp(invoke(p2, *first2), invoke(p1, *first1))) {
            *dest = *first2;
            ++first2;
            --n2;
            ++dest;
        }
        else {
            ++first1;
            --n1;
            ++first2;
            --n2;
        }
    }
    return std::copy_n(first2, n2, std::copy_n(first1, n1, dest));
}

template <typename InIt1, typename InIt2, typename OutIt, typename Comparator, typename Equal>
auto set_symmetric_difference_unique(InIt1 first1, InIt1 last1, InIt2 first2, InIt2 last2,
                                     OutIt dest, Comparator comp, Equal eq) -> OutIt
{
    while (first1 != last1 && first2 != last2) {
        if (comp(*first1, *first2)) {
            *dest = *first1;
            ++first1;
            break;
        }
        if (comp(*first2, *first1)) {
            *dest = *first2;
            ++first2;
            break;
        }
        ++first1;
        ++first2;
    }

    while (first1 != last1 && first2 != last2) {
        if (comp(*first1, *first2)) {
            if (!eq(*first1, *dest)) { *++dest = *first1; }
            ++first1;
        }
        else if (comp(*first2, *first1)) {
            if (!eq(*first2, *dest)) { *++dest = *first2; }
            ++first2;
        }
        else {
            ++first1;
            ++first2;
        }
    }
    auto eqf(astl::pass_fn(eq));
    return std::unique_copy(first2, last2, std::unique_copy(first1, last1, dest, eqf), eqf);
}

template <typename InIt1, typename InIt2, typename OutIt, typename Comparator>
auto set_symmetric_difference_unique(InIt1 first1, InIt1 last1, InIt2 first2, InIt2 last2,
                                     OutIt dest, Comparator comp) -> OutIt
{
    return i::set_symmetric_difference_unique(first1, last1, first2, last2, dest,
                                              astl::pass_fn(comp), std::equal_to{});
}

template <typename InIt1, typename InIt2, typename OutIt>
auto set_symmetric_difference_unique(InIt1 first1, InIt1 last1, InIt2 first2, InIt2 last2,
                                     OutIt dest) -> OutIt
{
    return i::set_symmetric_difference_unique(first1, last1, first2, last2, dest, std::less{},
                                              std::equal_to{});
}

template <typename InIt1, typename InIt2, typename OutIt, typename Comparator, typename Equal,
          typename P1, typename P2>
auto set_symmetric_difference_unique(InIt1 first1, InIt1 last1, InIt2 first2, InIt2 last2,
                                     OutIt dest, Comparator comp, Equal eq, P1 p1, P2 p2) -> OutIt
{
    while (first1 != last1 && first2 != last2) {
        if (comp(invoke(p1, *first1), invoke(p2, *first2))) {
            *dest = *first1;
            ++first1;
            break;
        }
        if (comp(invoke(p2, *first2), invoke(p1, *first1))) {
            *dest = *first2;
            ++first2;
            break;
        }
        ++first1;
        ++first2;
    }

    while (first1 != last1 && first2 != last2) {
        if (comp(invoke(p1, *first1), invoke(p2, *first2))) {
            if (!eq(*first1, *dest)) { *++dest = *first1; }
            ++first1;
        }
        else if (comp(invoke(p2, *first2), invoke(p1, *first1))) {
            if (!eq(*first2, *dest)) { *++dest = *first2; }
            ++first2;
        }
        else {
            ++first1;
            ++first2;
        }
    }
    auto eqf(astl::pass_fn(eq));
    return std::unique_copy(first2, last2, std::unique_copy(first1, last1, dest, eqf), eqf);
}

template <typename InIt1, typename N1, typename InIt2, typename N2, typename OutIt,
          typename Comparator, typename Equal, typename P1, typename P2>
auto set_symmetric_difference_unique_n(InIt1 first1, N1 n1, InIt2 first2, N2 n2, OutIt dest,
                                       Comparator comp, Equal eq, P1 p1, P2 p2) -> OutIt
{
    while (n1 != N1(0) && n2 != N2(0)) {
        if (comp(invoke(p1, *first1), invoke(p2, *first2))) {
            *dest = *first1;
            ++first1;
            --n1;
            break;
        }
        if (comp(invoke(p2, *first2), invoke(p1, *first1))) {
            *dest = *first2;
            ++first2;
            --n2;
            break;
        }
        ++first1;
        --n1;
        ++first2;
        --n2;
    }

    while (n1 != N1(0) && n2 != N2(0)) {
        if (comp(invoke(p1, *first1), invoke(p2, *first2))) {
            if (!eq(*first1, *dest)) { *++dest = *first1; }
            ++first1;
            --n1;
        }
        else if (comp(invoke(p2, *first2), invoke(p1, *first1))) {
            if (!eq(*first2, *dest)) { *++dest = *first2; }
            ++first2;
            --n2;
        }
        else {
            ++first1;
            --n1;
            ++first2;
            --n2;
        }
    }
    auto eqf(astl::pass_fn(eq));
    return i::unique_copy_n(first2, n2, i::unique_copy_n(first1, n1, dest, eqf).first, eqf).first;
}

template <typename InIt1, typename N1, typename InIt2, typename N2, typename OutIt,
          typename Comparator, typename Equal>
auto set_symmetric_difference_unique_n(InIt1 first1, N1 n1, InIt2 first2, N2 n2, OutIt dest,
                                       Comparator comp, Equal eq) -> OutIt
{
    while (n1 != N1(0) && n2 != N2(0)) {
        if (comp(*first1, *first2)) {
            *dest = *first1;
            ++first1;
            --n1;
            break;
        }
        if (comp(*first2, *first1)) {
            *dest = *first2;
            ++first2;
            --n2;
            break;
        }
        ++first1;
        --n1;
        ++first2;
        --n2;
    }

    while (n1 != N1(0) && n2 != N2(0)) {
        if (comp(*first1, *first2)) {
            if (!eq(*first1, *dest)) { *++dest = *first1; }
            ++first1;
            --n1;
        }
        else if (comp(*first2, *first1)) {
            if (!eq(*first2, *dest)) { *++dest = *first2; }
            ++first2;
            --n2;
        }
        else {
            ++first1;
            --n1;
            ++first2;
            --n2;
        }
    }
    auto eqf(astl::pass_fn(eq));
    return i::unique_copy_n(first2, n2, i::unique_copy_n(first1, n1, dest, eqf).first, eqf).first;
}

template <typename InIt1, typename N1, typename InIt2, typename N2, typename OutIt,
          typename Comparator>
auto set_symmetric_difference_unique_n(InIt1 first1, N1 n1, InIt2 first2, N2 n2, OutIt dest,
                                       Comparator comp) -> OutIt
{
    return i::set_symmetric_difference_unique_n(first1, n1, first2, n2, dest, astl::pass_fn(comp),
                                                std::equal_to{});
}

template <typename InIt1, typename N1, typename InIt2, typename N2, typename OutIt>
auto set_symmetric_difference_unique_n(InIt1 first1, N1 n1, InIt2 first2, N2 n2, OutIt dest)
    -> OutIt
{
    return i::set_symmetric_difference_unique_n(first1, n1, first2, n2, dest, std::less{},
                                                std::equal_to{});
}

using std::set_union; // NOLINT(misc-unused-using-decls)
template <typename InIt1, typename InIt2, typename OutIt, typename Comparator, typename P1,
          typename P2>
auto set_union(InIt1 first1, InIt1 last1, InIt2 first2, InIt2 last2, OutIt dest, Comparator comp,
               P1 p1, P2 p2) -> OutIt
{
    while (first1 != last1 && first2 != last2) {
        if (comp(invoke(p1, *first1), invoke(p2, *first2))) {
            *dest = *first1;
            ++first1;
        }
        else if (comp(invoke(p2, *first2), invoke(p1, *first1))) {
            *dest = *first2;
            ++first2;
        }
        else {
            *dest = *first1;
            ++first1;
            ++first2;
        }
        ++dest;
    }
    return std::copy(first2, last2, std::copy(first1, last1, dest));
}

template <typename InIt1, typename N1, typename InIt2, typename N2, typename OutIt,
          typename Comparator>
auto set_union_n(InIt1 first1, N1 n1, InIt2 first2, N2 n2, OutIt dest, Comparator comp) -> OutIt
{
    while (n1 != N1(0) && n2 != N2(0)) {
        if (comp(*first1, *first2)) {
            *dest = *first1;
            ++first1;
            --n1;
        }
        else if (comp(*first2, *first1)) {
            *dest = *first2;
            ++first2;
            --n2;
        }
        else {
            *dest = *first1;
            ++first1;
            --n1;
            ++first2;
            --n2;
        }
        ++dest;
    }
    return std::copy_n(first2, n2, std::copy_n(first1, n1, dest));
}

template <typename InIt1, typename N1, typename InIt2, typename N2, typename OutIt>
auto set_union_n(InIt1 first1, N1 n1, InIt2 first2, N2 n2, OutIt dest) -> OutIt
{
    return i::set_union_n(first1, n1, first2, n2, dest, std::less{});
}

template <typename InIt1, typename N1, typename InIt2, typename N2, typename OutIt,
          typename Comparator, typename P1, typename P2>
auto set_union_n(InIt1 first1, N1 n1, InIt2 first2, N2 n2, OutIt dest, Comparator comp, P1 p1,
                 P2 p2) -> OutIt
{
    while (n1 != N1(0) && n2 != N2(0)) {
        if (comp(invoke(p1, *first1), invoke(p2, *first2))) {
            *dest = *first1;
            ++first1;
            --n1;
        }
        else if (comp(invoke(p2, *first2), invoke(p1, *first1))) {
            *dest = *first2;
            ++first2;
            --n2;
        }
        else {
            *dest = *first1;
            ++first1;
            --n1;
            ++first2;
            --n2;
        }
        ++dest;
    }
    return std::copy_n(first2, n2, std::copy_n(first1, n1, dest));
}

template <typename I1, typename I2, typename OutIt, typename Comparator, typename Equal>
auto set_union_unique(I1 first1, I1 last1, I2 first2, I2 last2, OutIt dest, Comparator comp,
                      Equal eq) -> OutIt
{
    if (first1 != last1 && first2 != last2) {
        if (comp(*first1, *first2)) {
            *dest = *first1;
            ++first1;
        }
        else if (comp(*first2, *first1)) {
            *dest = *first2;
            ++first2;
        }
        else {
            *dest = *first1;
            ++first1;
            ++first2;
        }
    }

    while (first1 != last1 && first2 != last2) {
        if (comp(*first1, *first2)) {
            if (!eq(*first1, *dest)) {
                ++dest;
                *dest = *first1;
            }
            ++first1;
        }
        else if (comp(*first2, *first1)) {
            if (!eq(*first1, *dest)) { *++dest = *first2; }
            ++first2;
        }
        else {
            if (!eq(*dest, *first1)) { *++dest = *first1; }
            ++first1;
            ++first2;
        }
    }
    auto eqf(astl::pass_fn(eq));
    return std::unique_copy(first2, last2, std::unique_copy(first1, last1, dest, eqf), eqf);
}

template <typename I1, typename I2, typename OutIt, typename Comparator>
auto set_union_unique(I1 first1, I1 last1, I2 first2, I2 last2, OutIt dest, Comparator comp)
    -> OutIt
{
    return i::set_union_unique(first1, last1, first2, last2, dest, astl::pass_fn(comp),
                               std::equal_to{});
}

template <typename I1, typename I2, typename OutIt>
auto set_union_unique(I1 first1, I1 last1, I2 first2, I2 last2, OutIt dest) -> OutIt
{
    return i::set_union_unique(first1, last1, first2, last2, dest, std::less{}, std::equal_to{});
}

template <typename InIt1, typename InIt2, typename OutIt, typename Comparator, typename Equal,
          typename P1, typename P2>
auto set_union_unique(InIt1 first1, InIt1 last1, InIt2 first2, InIt2 last2, OutIt dest,
                      Comparator comp, Equal eq, P1 p1, P2 p2) -> OutIt
{
    if (first1 != last1 && first2 != last2) {
        if (comp(invoke(p1, *first1), invoke(p2, *first2))) {
            *dest = *first1;
            ++first1;
        }
        else if (comp(invoke(p2, *first2), invoke(p1, *first1))) {
            *dest = *first2;
            ++first2;
        }
        else {
            *dest = *first1;
            ++first1;
            ++first2;
        }
    }

    while (first1 != last1 && first2 != last2) {
        if (comp(invoke(p1, *first1), invoke(p2, *first2))) {
            if (!eq(*first1, *dest)) {
                ++dest;
                *dest = *first1;
            }
            ++first1;
        }
        else if (comp(invoke(p2, *first2), invoke(p1, *first1))) {
            if (!eq(*first1, *dest)) { *++dest = *first2; }
            ++first2;
        }
        else {
            if (!eq(*dest, *first1)) { *++dest = *first1; }
            ++first1;
            ++first2;
        }
    }
    auto eqf(astl::pass_fn(eq));
    return std::unique_copy(first2, last2, std::unique_copy(first1, last1, dest, eqf), eqf);
}

template <typename InIt1, typename N1, typename InIt2, typename N2, typename OutIt,
          typename Comparator, typename Equal>
auto set_union_unique_n(InIt1 first1, N1 n1, InIt2 first2, N2 n2, OutIt dest, Comparator comp,
                        Equal eq) -> OutIt
{
    if (n1 != N1(0) && n2 != N2(0)) {
        if (comp(*first1, *first2)) {
            *dest = *first1;
            ++first1;
            --n1;
        }
        else if (comp(*first2, *first1)) {
            *dest = *first2;
            ++first2;
            --n2;
        }
        else {
            *dest = *first1;
            ++first1;
            --n1;
            ++first2;
            --n2;
        }
    }

    while (n1 != N1(0) && n2 != N2(0)) {
        if (comp(*first1, *first2)) {
            if (!eq(*first1, *dest)) {
                ++dest;
                *dest = *first1;
            }
            ++first1;
            --n1;
        }
        else if (comp(*first2, *first1)) {
            if (!eq(*first1, *dest)) { *++dest = *first2; }
            ++first2;
            --n2;
        }
        else {
            if (!eq(*dest, *first1)) { *++dest = *first1; }
            ++first1;
            --n1;
            ++first2;
            --n2;
        }
    }
    auto eqf(astl::pass_fn(eq));
    return i::unique_copy_n(first2, n2, i::unique_copy_n(first1, n1, dest, eqf).first, eqf).first;
}

template <typename InIt1, typename N1, typename InIt2, typename N2, typename OutIt,
          typename Comparator>
auto set_union_unique_n(InIt1 first1, N1 n1, InIt2 first2, N2 n2, OutIt dest, Comparator comp)
    -> OutIt
{
    return i::set_union_unique_n(first1, n1, first2, n2, dest, astl::pass_fn(comp),
                                 std::equal_to{});
}

template <typename InIt1, typename N1, typename InIt2, typename N2, typename OutIt,
          typename Comparator, typename Equal, typename P1, typename P2>
auto set_union_unique_n(InIt1 first1, N1 n1, InIt2 first2, N2 n2, OutIt dest, Comparator comp,
                        Equal eq, P1 p1, P2 p2) -> OutIt
{
    if (n1 != N1(0) && n2 != N2(0)) {
        if (comp(invoke(p1, *first1), invoke(p2, *first2))) {
            *dest = *first1;
            ++first1;
            --n1;
        }
        else if (comp(invoke(p2, *first2), invoke(p1, *first1))) {
            *dest = *first2;
            ++first2;
            --n2;
        }
        else {
            *dest = *first1;
            ++first1;
            --n1;
            ++first2;
            --n2;
        }
    }

    while (n1 != N1(0) && n2 != N2(0)) {
        if (comp(invoke(p1, *first1), invoke(p2, *first2))) {
            if (!eq(*first1, *dest)) {
                ++dest;
                *dest = *first1;
            }
            ++first1;
            --n1;
        }
        else if (comp(invoke(p2, *first2), invoke(p1, *first1))) {
            if (!eq(*first1, *dest)) { *++dest = *first2; }
            ++first2;
            --n2;
        }
        else {
            if (!eq(*dest, *first1)) { *++dest = *first1; }
            ++first1;
            --n1;
            ++first2;
            --n2;
        }
    }
    auto eqf(astl::pass_fn(eq));
    return i::unique_copy_n(first2, n2, i::unique_copy_n(first1, n1, dest, eqf).first, eqf).first;
}
} // namespace i

namespace r
{
template <typename R1, typename R2, typename Comparator, typename P1, typename P2>
auto includes(R1 &&r1, R2 &&r2, Comparator comp, P1 p1, P2 p2) -> bool
{
    return i::includes(adl::begin(r1), adl::end(r1), adl::begin(r2), adl::end(r2),
                       astl::pass_fn(comp), astl::pass_fn(p1), astl::pass_fn(p2));
}

template <typename R1, typename R2, typename Comparator>
ASTL_NODISCARD auto includes(R1 &&r1, R2 &&r2, Comparator comp) -> bool
{
    return i::includes(adl::begin(r1), adl::end(r1), adl::begin(r2), adl::end(r2),
                       astl::pass_fn(comp));
}

template <typename R1, typename N1, typename R2, typename N2, typename Comparator, typename P1,
          typename P2>
ASTL_NODISCARD auto includes_n(R1 &&r1, N1 n1, R2 &&r2, N2 n2, Comparator comp, P1 p1, P2 p2)
    -> bool
{
    return i::includes_n(adl::begin(r1), n1, adl::begin(r2), n2, astl::pass_fn(comp),
                         astl::pass_fn(p1), astl::pass_fn(p2));
}

template <typename R1, typename N1, typename R2, typename N2, typename Comparator>
ASTL_NODISCARD auto includes_n(R1 &&r1, N1 n1, R2 &&r2, N2 n2, Comparator comp) -> bool
{
    return i::includes_n(adl::begin(r1), n1, adl::begin(r2), n2, astl::pass_fn(comp));
}

template <typename R1, typename N1, typename R2, typename N2>
ASTL_NODISCARD auto includes_n(R1 &&r1, N1 n1, R2 &&r2, N2 n2) -> bool
{
    return r::includes_n(r1, n1, r2, n2, std::less{});
}

template <typename R1, typename R2, typename OutIt, typename Comparator, typename P1, typename P2>
auto set_difference(R1 &&r1, R2 &&r2, OutIt dest, Comparator comp, P1 p1, P2 p2) -> OutIt
{
    return i::set_difference(adl::begin(r1), adl::end(r1), adl::begin(r2), adl::end(r2), dest,
                             astl::pass_fn(comp), astl::pass_fn(p1), astl::pass_fn(p2));
}

template <typename R1, typename R2, typename OutIt, typename Comparator>
auto set_difference(R1 &&r1, R2 &&r2, OutIt dest, Comparator comp) -> OutIt
{
    return i::set_difference(adl::begin(r1), adl::end(r1), adl::begin(r2), adl::end(r2), dest,
                             astl::pass_fn(comp));
}

template <typename R1, typename R2, typename OutIt>
auto set_difference(R1 &&r1, R2 &&r2, OutIt dest) -> OutIt
{
    return i::set_difference(adl::begin(r1), adl::end(r1), adl::begin(r2), adl::end(r2), dest);
}

template <typename R1, typename N1, typename R2, typename N2, typename OutIt, typename Comparator,
          typename P1, typename P2>
auto set_difference_n(R1 &&r1, N1 n1, R2 &&r2, N2 n2, OutIt dest, Comparator comp, P1 p1, P2 p2)
    -> OutIt
{
    return i::set_difference_n(adl::begin(r1), n1, adl::begin(r2), n2, dest, astl::pass_fn(comp),
                               astl::pass_fn(p1), astl::pass_fn(p2));
}

template <typename R1, typename N1, typename R2, typename N2, typename OutIt, typename Comparator>
auto set_difference_n(R1 &&r1, N1 n1, R2 &&r2, N2 n2, OutIt dest, Comparator comp) -> OutIt
{
    return i::set_difference_n(adl::begin(r1), n1, adl::begin(r2), n2, dest, astl::pass_fn(comp));
}

template <typename R1, typename N1, typename R2, typename N2, typename OutIt>
auto set_difference_n(R1 &&r1, N1 n1, R2 &&r2, N2 n2, OutIt dest) -> OutIt
{
    return r::set_difference_n(r1, n1, r2, n2, dest, std::less{});
}

template <typename R1, typename R2, typename OutIt, typename Comparator, typename Equal,
          typename P1, typename P2>
auto set_difference_unique(R1 &&r1, R2 &&r2, OutIt dest, Comparator comp, Equal eq, P1 p1, P2 p2)
    -> OutIt
{
    return i::set_difference_unique(adl::begin(r1), adl::end(r1), adl::begin(r2), adl::end(r2),
                                    dest, astl::pass_fn(comp), astl::pass_fn(eq), astl::pass_fn(p1),
                                    astl::pass_fn(p2));
}

template <typename R1, typename R2, typename OutIt, typename Comparator, typename Equal>
auto set_difference_unique(R1 &&r1, R2 &&r2, OutIt dest, Comparator comp, Equal eq) -> OutIt
{
    return i::set_difference_unique(adl::begin(r1), adl::end(r1), adl::begin(r2), adl::end(r2),
                                    dest, astl::pass_fn(comp), astl::pass_fn(eq));
}

template <typename R1, typename R2, typename OutIt, typename Comparator>
auto set_difference_unique(R1 &&r1, R2 &&r2, OutIt dest, Comparator comp) -> OutIt
{
    return r::set_difference_unique(r1, r2, dest, astl::pass_fn(comp), std::equal_to{});
}

template <typename R1, typename R2, typename OutIt>
auto set_difference_unique(R1 &&r1, R2 &&r2, OutIt dest) -> OutIt
{
    return r::set_difference_unique(r1, r2, dest, std::less{}, std::equal_to{});
}

template <typename R1, typename N1, typename R2, typename N2, typename OutIt, typename Comparator,
          typename Equal, typename P1, typename P2>
auto set_difference_unique_n(R1 &&r1, N1 n1, R2 &&r2, N2 n2, OutIt dest, Comparator comp, Equal eq,
                             P1 p1, P2 p2) -> OutIt
{
    return i::set_difference_unique_n(adl::begin(r1), n1, adl::begin(r2), n2, dest,
                                      astl::pass_fn(comp), astl::pass_fn(eq), astl::pass_fn(p1),
                                      astl::pass_fn(p2));
}

template <typename R1, typename N1, typename R2, typename N2, typename OutIt, typename Comparator,
          typename Equal>
auto set_difference_unique_n(R1 &&r1, N1 n1, R2 &&r2, N2 n2, OutIt dest, Comparator comp, Equal eq)
    -> OutIt
{
    return i::set_difference_unique_n(adl::begin(r1), n1, adl::begin(r2), n2, dest,
                                      astl::pass_fn(comp), astl::pass_fn(eq));
}

template <typename R1, typename N1, typename R2, typename N2, typename OutIt, typename Comparator>
auto set_difference_unique_n(R1 &&r1, N1 n1, R2 &&r2, N2 n2, OutIt dest, Comparator comp) -> OutIt
{
    return r::set_difference_unique_n(r1, n1, r2, n2, dest, astl::pass_fn(comp), std::equal_to{});
}

template <typename R1, typename N1, typename R2, typename N2, typename OutIt>
auto set_difference_unique_n(R1 &&r1, N1 n1, R2 &&r2, N2 n2, OutIt dest) -> OutIt
{
    return r::set_difference_unique_n(r1, n1, r2, n2, dest, std::less{}, std::equal_to{});
}

template <typename R1, typename R2, typename OutIt, typename Comparator, typename P1, typename P2>
auto set_intersection(R1 &&r1, R2 &&r2, OutIt dest, Comparator comp, P1 p1, P2 p2) -> OutIt
{
    return i::set_intersection(adl::begin(r1), adl::end(r1), adl::begin(r2), adl::end(r2), dest,
                               astl::pass_fn(comp), astl::pass_fn(p1), astl::pass_fn(p2));
}

template <typename R1, typename R2, typename OutIt, typename Comparator>
auto set_intersection(R1 &&r1, R2 &&r2, OutIt dest, Comparator comp) -> OutIt
{
    return i::set_intersection(adl::begin(r1), adl::end(r1), adl::begin(r2), adl::end(r2), dest,
                               astl::pass_fn(comp));
}

template <typename R1, typename R2, typename OutIt>
auto set_intersection(R1 &&r1, R2 &&r2, OutIt dest) -> OutIt
{
    return i::set_intersection(adl::begin(r1), adl::end(r1), adl::begin(r2), adl::end(r2), dest);
}

template <typename R1, typename N1, typename R2, typename N2, typename OutIt, typename Comparator,
          typename P1, typename P2>
auto set_intersection_n(R1 &&r1, N1 n1, R2 &&r2, N2 n2, OutIt dest, Comparator comp, P1 p1, P2 p2)
    -> OutIt
{
    return i::set_intersection_n(adl::begin(r1), n1, adl::begin(r2), n2, dest, astl::pass_fn(comp),
                                 astl::pass_fn(p1), astl::pass_fn(p2));
}

template <typename R1, typename N1, typename R2, typename N2, typename OutIt, typename Comparator>
auto set_intersection_n(R1 &&r1, N1 n1, R2 &&r2, N2 n2, OutIt dest, Comparator comp) -> OutIt
{
    return i::set_intersection_n(adl::begin(r1), n1, adl::begin(r2), n2, dest, astl::pass_fn(comp));
}

template <typename R1, typename N1, typename R2, typename N2, typename OutIt>
auto set_intersection_n(R1 &&r1, N1 n1, R2 &&r2, N2 n2, OutIt dest) -> OutIt
{
    return r::set_intersection_n(r1, n1, r2, n2, dest, std::less{});
}

template <typename R1, typename R2, typename OutIt, typename Comparator, typename Equal,
          typename P1, typename P2>
auto set_intersection_unique(R1 &&r1, R2 &&r2, OutIt dest, Comparator comp, Equal eq, P1 p1, P2 p2)
    -> OutIt
{
    return i::set_intersection_unique(adl::begin(r1), adl::end(r1), adl::begin(r2), adl::end(r2),
                                      dest, astl::pass_fn(comp), astl::pass_fn(eq),
                                      astl::pass_fn(p1), astl::pass_fn(p2));
}

template <typename R1, typename R2, typename OutIt, typename Comparator, typename Equal>
auto set_intersection_unique(R1 &&r1, R2 &&r2, OutIt dest, Comparator comp, Equal eq) -> OutIt
{
    return i::set_intersection_unique(adl::begin(r1), adl::end(r1), adl::begin(r2), adl::end(r2),
                                      dest, astl::pass_fn(comp), astl::pass_fn(eq));
}

template <typename R1, typename R2, typename OutIt, typename Comparator>
auto set_intersection_unique(R1 &&r1, R2 &&r2, OutIt dest, Comparator comp) -> OutIt
{
    return r::set_intersection_unique(r1, r2, dest, astl::pass_fn(comp), std::equal_to{});
}

template <typename R1, typename R2, typename OutIt>
auto set_intersection_unique(R1 &&r1, R2 &&r2, OutIt dest) -> OutIt
{
    return r::set_intersection_unique(r1, r2, dest, std::less{}, std::equal_to{});
}

template <typename R1, typename N1, typename R2, typename N2, typename OutIt, typename Comparator,
          typename Equal, typename P1, typename P2>
auto set_intersection_unique_n(R1 &&r1, N1 n1, R2 &&r2, N2 n2, OutIt dest, Comparator comp,
                               Equal eq, P1 p1, P2 p2) -> OutIt
{
    return i::set_intersection_unique_n(adl::begin(r1), n1, adl::begin(r2), n2, dest,
                                        astl::pass_fn(comp), astl::pass_fn(eq), astl::pass_fn(p1),
                                        astl::pass_fn(p2));
}

template <typename R1, typename N1, typename R2, typename N2, typename OutIt, typename Comparator,
          typename Equal>
auto set_intersection_unique_n(R1 &&r1, N1 n1, R2 &&r2, N2 n2, OutIt dest, Comparator comp,
                               Equal eq) -> OutIt
{
    return i::set_intersection_unique_n(adl::begin(r1), n1, adl::begin(r2), n2, dest,
                                        astl::pass_fn(comp), astl::pass_fn(eq));
}

template <typename R1, typename N1, typename R2, typename N2, typename OutIt, typename Comparator>
auto set_intersection_unique_n(R1 &&r1, N1 n1, R2 &&r2, N2 n2, OutIt dest, Comparator comp) -> OutIt
{
    return r::set_intersection_unique_n(r1, n1, r2, n2, dest, astl::pass_fn(comp), std::equal_to{});
}

template <typename R1, typename N1, typename R2, typename N2, typename OutIt>
auto set_intersection_unique_n(R1 &&r1, N1 n1, R2 &&r2, N2 n2, OutIt dest) -> OutIt
{
    return r::set_intersection_unique_n(r1, n1, r2, n2, dest, std::less{}, std::equal_to{});
}

template <typename R1, typename R2, typename OutIt, typename Comparator, typename P1, typename P2>
auto set_symmetric_difference(R1 &&r1, R2 &&r2, OutIt dest, Comparator comp, P1 p1, P2 p2) -> OutIt
{
    return i::set_symmetric_difference(adl::begin(r1), adl::end(r1), adl::begin(r2), adl::end(r2),
                                       dest, astl::pass_fn(comp), astl::pass_fn(p1),
                                       astl::pass_fn(p2));
}

template <typename R1, typename R2, typename OutIt, typename Comparator>
auto set_symmetric_difference(R1 &&r1, R2 &&r2, OutIt dest, Comparator comp) -> OutIt
{
    return i::set_symmetric_difference(adl::begin(r1), adl::end(r1), adl::begin(r2), adl::end(r2),
                                       dest, astl::pass_fn(comp));
}

template <typename R1, typename R2, typename OutIt>
auto set_symmetric_difference(R1 &&r1, R2 &&r2, OutIt dest) -> OutIt
{
    return i::set_symmetric_difference(adl::begin(r1), adl::end(r1), adl::begin(r2), adl::end(r2),
                                       dest);
}

template <typename R1, typename N1, typename R2, typename N2, typename OutIt, typename Comparator,
          typename P1, typename P2>
auto set_symmetric_difference_n(R1 &&r1, N1 n1, R2 &&r2, N2 n2, OutIt dest, Comparator comp, P1 p1,
                                P2 p2) -> OutIt
{
    return i::set_symmetric_difference_n(adl::begin(r1), n1, adl::begin(r2), n2, dest,
                                         astl::pass_fn(comp), astl::pass_fn(p1), astl::pass_fn(p2));
}

template <typename R1, typename N1, typename R2, typename N2, typename OutIt, typename Comparator>
auto set_symmetric_difference_n(R1 &&r1, N1 n1, R2 &&r2, N2 n2, OutIt dest, Comparator comp)
    -> OutIt
{
    return i::set_symmetric_difference_n(adl::begin(r1), n1, adl::begin(r2), n2, dest,
                                         astl::pass_fn(comp));
}

template <typename R1, typename N1, typename R2, typename N2, typename OutIt>
auto set_symmetric_difference_n(R1 &&r1, N1 n1, R2 &&r2, N2 n2, OutIt dest) -> OutIt
{
    return r::set_symmetric_difference_n(r1, n1, r2, n2, dest, std::less{});
}

template <typename R1, typename R2, typename OutIt, typename Comparator, typename Equal,
          typename P1, typename P2>
auto set_symmetric_difference_unique(R1 &&r1, R2 &&r2, OutIt dest, Comparator comp, Equal eq, P1 p1,
                                     P2 p2) -> OutIt
{
    return i::set_symmetric_difference_unique(
        adl::begin(r1), adl::end(r1), adl::begin(r2), adl::end(r2), dest, astl::pass_fn(comp),
        astl::pass_fn(eq), astl::pass_fn(p1), astl::pass_fn(p2));
}

template <typename R1, typename R2, typename OutIt, typename Comparator, typename Equal>
auto set_symmetric_difference_unique(R1 &&r1, R2 &&r2, OutIt dest, Comparator comp, Equal eq)
    -> OutIt
{
    return i::set_symmetric_difference_unique(adl::begin(r1), adl::end(r1), adl::begin(r2),
                                              adl::end(r2), dest, astl::pass_fn(comp),
                                              astl::pass_fn(eq));
}

template <typename R1, typename R2, typename OutIt, typename Comparator>
auto set_symmetric_difference_unique(R1 &&r1, R2 &&r2, OutIt dest, Comparator comp) -> OutIt
{
    return r::set_symmetric_difference_unique(r1, r2, dest, astl::pass_fn(comp), std::equal_to{});
}

template <typename R1, typename R2, typename OutIt>
auto set_symmetric_difference_unique(R1 &&r1, R2 &&r2, OutIt dest) -> OutIt
{
    return r::set_symmetric_difference_unique(r1, r2, dest, std::less{}, std::equal_to{});
}

template <typename R1, typename N1, typename R2, typename N2, typename OutIt, typename Comparator,
          typename Equal, typename P1, typename P2>
auto set_symmetric_difference_unique_n(R1 &&r1, N1 n1, R2 &&r2, N2 n2, OutIt dest, Comparator comp,
                                       Equal eq, P1 p1, P2 p2) -> OutIt
{
    return i::set_symmetric_difference_unique_n(adl::begin(r1), n1, adl::begin(r2), n2, dest,
                                                astl::pass_fn(comp), astl::pass_fn(eq),
                                                astl::pass_fn(p1), astl::pass_fn(p2));
}

template <typename R1, typename N1, typename R2, typename N2, typename OutIt, typename Comparator,
          typename Equal>
auto set_symmetric_difference_unique_n(R1 &&r1, N1 n1, R2 &&r2, N2 n2, OutIt dest, Comparator comp,
                                       Equal eq) -> OutIt
{
    return i::set_symmetric_difference_unique_n(adl::begin(r1), n1, adl::begin(r2), n2, dest,
                                                astl::pass_fn(comp), astl::pass_fn(eq));
}

template <typename R1, typename N1, typename R2, typename N2, typename OutIt, typename Comparator>
auto set_symmetric_difference_unique_n(R1 &&r1, N1 n1, R2 &&r2, N2 n2, OutIt dest, Comparator comp)
    -> OutIt
{
    return r::set_symmetric_difference_unique_n(r1, n1, r2, n2, dest, astl::pass_fn(comp),
                                                std::equal_to{});
}

template <typename R1, typename N1, typename R2, typename N2, typename OutIt>
auto set_symmetric_difference_unique_n(R1 &&r1, N1 n1, R2 &&r2, N2 n2, OutIt dest) -> OutIt
{
    return r::set_symmetric_difference_unique_n(r1, n1, r2, n2, dest, std::less{}, std::equal_to{});
}

template <typename R1, typename R2, typename OutIt, typename Comparator, typename P1, typename P2>
auto set_union(R1 &&r1, R2 &&r2, OutIt dest, Comparator comp, P1 p1, P2 p2) -> OutIt
{
    return i::set_union(adl::begin(r1), adl::end(r1), adl::begin(r2), adl::end(r2), dest,
                        astl::pass_fn(comp), astl::pass_fn(p1), astl::pass_fn(p2));
}

template <typename R1, typename R2, typename OutIt, typename Comparator>
auto set_union(R1 &&r1, R2 &&r2, OutIt dest, Comparator comp) -> OutIt
{
    return i::set_union(adl::begin(r1), adl::end(r1), adl::begin(r2), adl::end(r2), dest,
                        astl::pass_fn(comp));
}

template <typename R1, typename R2, typename OutIt>
auto set_union(R1 &&r1, R2 &&r2, OutIt dest) -> OutIt
{
    return i::set_union(adl::begin(r1), adl::end(r1), adl::begin(r2), adl::end(r2), dest);
}

template <typename R1, typename N1, typename R2, typename N2, typename OutIt, typename Comparator,
          typename P1, typename P2>
auto set_union_n(R1 &&r1, N1 n1, R2 &&r2, N2 n2, OutIt dest, Comparator comp, P1 p1, P2 p2) -> OutIt
{
    return i::set_union_n(adl::begin(r1), n1, adl::begin(r2), n2, dest, astl::pass_fn(comp),
                          astl::pass_fn(p1), astl::pass_fn(p2));
}

template <typename R1, typename N1, typename R2, typename N2, typename OutIt, typename Comparator>
auto set_union_n(R1 &&r1, N1 n1, R2 &&r2, N2 n2, OutIt dest, Comparator comp) -> OutIt
{
    return i::set_union_n(adl::begin(r1), n1, adl::begin(r2), n2, dest, astl::pass_fn(comp));
}

template <typename R1, typename N1, typename R2, typename N2, typename OutIt>
auto set_union_n(R1 &&r1, N1 n1, R2 &&r2, N2 n2, OutIt dest) -> OutIt
{
    return r::set_union_n(r1, n1, r2, n2, dest, std::less{});
}

template <typename R1, typename R2, typename OutIt, typename Comparator, typename Equal,
          typename P1, typename P2>
auto set_union_unique(R1 &&r1, R2 &&r2, OutIt dest, Comparator comp, Equal eq, P1 p1, P2 p2)
    -> OutIt
{
    return i::set_union_unique(adl::begin(r1), adl::end(r1), adl::begin(r2), adl::end(r2), dest,
                               astl::pass_fn(comp), astl::pass_fn(eq), astl::pass_fn(p1),
                               astl::pass_fn(p2));
}

template <typename R1, typename R2, typename OutIt, typename Comparator, typename Equal>
auto set_union_unique(R1 &&r1, R2 &&r2, OutIt dest, Comparator comp, Equal eq) -> OutIt
{
    return i::set_union_unique(adl::begin(r1), adl::end(r1), adl::begin(r2), adl::end(r2), dest,
                               astl::pass_fn(comp), astl::pass_fn(eq));
}

template <typename R1, typename R2, typename OutIt, typename Comparator>
auto set_union_unique(R1 &&r1, R2 &&r2, OutIt dest, Comparator comp) -> OutIt
{
    return r::set_union_unique(r1, r2, dest, astl::pass_fn(comp), std::equal_to{});
}

template <typename R1, typename R2, typename OutIt>
auto set_union_unique(R1 &&r1, R2 &&r2, OutIt dest) -> OutIt
{
    return r::set_union_unique(r1, r2, dest, std::less{}, std::equal_to{});
}

template <typename R1, typename N1, typename R2, typename N2, typename OutIt, typename Comparator,
          typename Equal, typename P1, typename P2>
auto set_union_unique_n(R1 &&r1, N1 n1, R2 &&r2, N2 n2, OutIt dest, Comparator comp, Equal eq,
                        P1 p1, P2 p2) -> OutIt
{
    return i::set_union_unique_n(adl::begin(r1), n1, adl::begin(r2), n2, dest, astl::pass_fn(comp),
                                 astl::pass_fn(eq), astl::pass_fn(p1), astl::pass_fn(p2));
}

template <typename R1, typename N1, typename R2, typename N2, typename OutIt, typename Comparator,
          typename Equal>
auto set_union_unique_n(R1 &&r1, N1 n1, R2 &&r2, N2 n2, OutIt dest, Comparator comp, Equal eq)
    -> OutIt
{
    return i::set_union_unique_n(adl::begin(r1), n1, adl::begin(r2), n2, dest, astl::pass_fn(comp),
                                 astl::pass_fn(eq));
}

template <typename R1, typename N1, typename R2, typename N2, typename OutIt, typename Comparator>
auto set_union_unique_n(R1 &&r1, N1 n1, R2 &&r2, N2 n2, OutIt dest, Comparator comp) -> OutIt
{
    return r::set_union_unique_n(r1, n1, r2, n2, dest, astl::pass_fn(comp), std::equal_to{});
}

template <typename R1, typename N1, typename R2, typename N2, typename OutIt>
auto set_union_unique_n(R1 &&r1, N1 n1, R2 &&r2, N2 n2, OutIt dest) -> OutIt
{
    return r::set_union_unique_n(r1, n1, r2, n2, dest, std::less{}, std::equal_to{});
}
} // namespace r
} // namespace astl

#endif // ASTL_INCLUDE_SET_ALGOS_HPP
