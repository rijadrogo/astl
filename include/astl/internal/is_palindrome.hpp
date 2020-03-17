//
// Created by Rijad on 04-Aug-18.
//

#ifndef ASTL_INCLUDE_IS_PALINDROME_HPP
#define ASTL_INCLUDE_IS_PALINDROME_HPP

#include <cstring>

#include "astl/functional.hpp"
#include "astl/iterator.hpp"
#include "astl/range_access.hpp"

namespace astl
{
namespace i
{

inline constexpr struct {

    template <typename BidiIt>
    // requires BidiIt BidirectionalIterator
    ASTL_NODISCARD auto operator()(BidiIt first, BidiIt last) const -> bool
    {
        if (first == last) return true;

        if constexpr (is_random_access_it_v<BidiIt>) // Random Access Iterator
        {
            while (first < --last && *first == *last) ++first;

            return !(first < last);
        }
        else { // Bidirectional Iterator
            while (--last != first) {
                if (*first != *last) return false;

                ++first;
                if (first == last) break;
            }
            return true;
        }
    }

    template <typename BidiIt, typename BinaryPredicate>
    // requires BidiIt BidirectionalIterator
    // requires BinaryPredicate function, returns bool, two arguments of type
    // value_type(RandIt)
    ASTL_NODISCARD auto operator()(BidiIt first, BidiIt last, BinaryPredicate pred) const -> bool
    {
        if (first == last) return true;

        if constexpr (is_random_access_it_v<BidiIt>) { // Random Access Iterator
            while (first < --last && pred(*first, *last)) ++first;

            return !(first < last);
        }
        else { // Bidirectional Iterator
            while (--last != first) {
                if (!pred(*first, *last)) return false;

                ++first;
                if (first == last) break;
            }
            return true;
        }
    }

    template <typename BidiIt, typename BinaryPredicate, typename P>
    ASTL_NODISCARD auto operator()(BidiIt first, BidiIt last, BinaryPredicate pred, P p) const
        -> bool
    {
        return (*this)(first, last, astl::combine(astl::pass_fn(pred), astl::pass_fn(p)));
    }
} is_palindrome{};

} // namespace i

namespace r
{

inline constexpr struct {

    ASTL_NODISCARD inline auto operator()(char const *str) const noexcept -> bool
    {
        // precondition: str must not be nullptr
        if (*str == '\0') return true;

        char const *first(str);
        char const *last(str + std::strlen(str)); //-V2513

        while (first < --last && *first == *last) ++first;

        return first >= last;
    }

    template <typename BinaryPredicate>
    ASTL_NODISCARD auto operator()(char const *str, BinaryPredicate pred) const -> bool
    {
        // precondition: str must not be nullptr
        if (*str == '\0') return true;

        char const *first(str);
        char const *last(str + std::strlen(str)); //-V2513

        while (first < --last && pred(*first, *last)) ++first;

        return first >= last;
    }

    template <typename R>
    // requires R Bidirectional range
    ASTL_NODISCARD auto operator()(R &&r) const -> bool
    {
        return i::is_palindrome(adl::begin(r), adl::end(r));
    }

    template <typename R, typename BinaryPredicate>
    // requires R Bidirectional range
    // requires BinaryPredicate function, returns bool, two arguments of type
    // value_type(R)
    ASTL_NODISCARD auto operator()(R &&r, BinaryPredicate pred) const -> bool
    {
        return i::is_palindrome(adl::begin(r), adl::end(r), astl::pass_fn(pred));
    }

    template <typename R, typename BinaryPredicate, typename P>
    ASTL_NODISCARD auto operator()(R &&r, BinaryPredicate pred, P p) const -> bool
    {
        return i::is_palindrome(adl::begin(r), adl::end(r), astl::pass_fn(pred), astl::pass_fn(p));
    }
} is_palindrome{};

} // namespace r
} // namespace astl

#endif // ASTL_INCLUDE_IS_PALINDROME_HPP
