//
// Created by Rijad on 30-Jul-18.
//

#ifndef ASTL_INCLUDE_REPLACE_HPP
#define ASTL_INCLUDE_REPLACE_HPP

#include <algorithm>
#include <utility>

#include "astl/functional.hpp"
#include "astl/iterator.hpp"
#include "astl/range_access.hpp"

namespace astl
{
namespace i
{

inline constexpr struct {
    template <typename FwdIt, typename T>
    auto operator()(FwdIt first, FwdIt const last, T const &old_value, T const &new_value) const
        -> void
    {
        std::replace(first, last, old_value, new_value);
    }

    template <typename FwdIt, typename T, typename P>
    auto operator()(FwdIt first, FwdIt const last, T const &old_value, T const &new_value,
                    P p) const -> void
    {
        while (first != last) {
            if (invoke(p, *first) == old_value) *first = new_value;

            ++first;
        }
    }
} replace{};

inline constexpr struct {
    template <typename InIt, typename OutIt, typename T>
    auto operator()(InIt first, InIt const last, OutIt dest, T const &old_value,
                    T const &new_value) const -> OutIt
    {
        return std::replace_copy(first, last, dest, old_value, new_value);
    }

    template <typename InIt, typename OutIt, typename T, typename P>
    auto operator()(InIt first, InIt const last, OutIt dest, T const &old_value, T const &new_value,
                    P p) const -> OutIt
    {
        while (first != last) {
            *dest = (invoke(p, *first) == old_value ? new_value : *first);
            ++first;
            ++dest;
        }
        return dest;
    }
} replace_copy{};

inline constexpr struct {
    template <typename InIt, typename OutIt, typename UnaryPredicate, typename T>
    auto operator()(InIt first, InIt last, OutIt dest, UnaryPredicate pred,
                    T const &new_value) const -> OutIt
    {
        return std::replace_copy_if(first, last, dest, astl::pass_fn(pred), new_value);
    }

    template <typename InIt, typename OutIt, typename UnaryPredicate, typename T, typename P>
    auto operator()(InIt first, InIt last, OutIt dest, UnaryPredicate pred, T const &new_value,
                    P p) const -> OutIt
    {
        return std::replace_copy_if(
            first, last, dest, astl::combine(astl::pass_fn(pred), astl::pass_fn(p)), new_value);
    }
} replace_copy_if{};

inline constexpr struct {

    template <typename InIt, typename N, typename OutIt, typename UnaryPredicate, typename U>
    auto operator()(InIt first, N n, OutIt dest, UnaryPredicate pred, U const &new_value) const
        -> std::pair<OutIt, InIt>
    {
        while (n != N(0)) {
            *dest = (pred(*first) ? new_value : *first);
            ++dest;
            ++first;
            --n;
        }
        return std::make_pair(dest, first);
    }

    template <typename InIt, typename N, typename OutIt, typename UnaryPredicate, typename U,
              typename P>
    auto operator()(InIt first, N n, OutIt dest, UnaryPredicate pred, U const &new_value, P p) const
        -> std::pair<OutIt, InIt>
    {
        return (*this)(first, n, dest, astl::combine(astl::pass_fn(pred), astl::pass_fn(p)),
                       new_value);
    }

} replace_copy_if_n{};

inline constexpr struct {
    template <typename InIt, typename N, typename OutIt, typename T>
    auto operator()(InIt first, N n, OutIt dest, T const &old_value, T const &new_value) const
        -> std::pair<OutIt, InIt>
    {
        while (n != N(0)) {
            *dest = (*first == old_value ? new_value : *first);
            ++first;
            ++dest;
            --n;
        }
        return std::make_pair(dest, first);
    }

    template <typename InIt, typename N, typename OutIt, typename T, typename P>
    auto operator()(InIt first, N n, OutIt dest, T const &old_value, T const &new_value, P p) const
        -> std::pair<OutIt, InIt>
    {
        while (n != N(0)) {
            *dest = (invoke(p, *first) == old_value ? new_value : *first);
            ++first;
            ++dest;
            --n;
        }
        return std::make_pair(dest, first);
    }
} replace_copy_n{};

inline constexpr struct {

    template <typename FwdIt, typename UnaryPredicate, typename T>
    auto operator()(FwdIt first, FwdIt last, UnaryPredicate pred, T const &new_value) const -> void
    {
        std::replace_if(first, last, astl::pass_fn(pred), new_value);
    }

    template <typename FwdIt, typename UnaryPredicate, typename T, typename P>
    auto operator()(FwdIt first, FwdIt last, UnaryPredicate pred, T const &new_value, P p) const
        -> void
    {
        (*this)(first, last, astl::combine(astl::pass_fn(pred), astl::pass_fn(p)), new_value);
    }
} replace_if{};

inline constexpr struct {
    template <typename FwdIt, typename N, typename UnaryPredicate, typename T>
    auto operator()(FwdIt first, N n, UnaryPredicate pred, T const &new_value) const -> FwdIt
    {
        while (n != N(0)) {
            if (pred(*first)) *first = new_value;

            ++first;
            --n;
        }
        return first;
    }

    template <typename FwdIt, typename N, typename UnaryPredicate, typename T, typename P>
    auto operator()(FwdIt first, N n, UnaryPredicate pred, T const &new_value, P p) const -> FwdIt
    {
        return (*this)(first, n, astl::combine(astl::pass_fn(pred), astl::pass_fn(p)), new_value);
    }
} replace_if_n{};

inline constexpr struct {
    template <typename InIt, typename OutIt, typename T>
    auto operator()(InIt first, InIt last, OutIt dest, T const &old_value, T const &new_value) const
        -> OutIt
    {
        return std::replace_copy(std::make_move_iterator(first), std::make_move_iterator(last),
                                 dest, old_value, new_value);
    }

    template <typename InIt, typename OutIt, typename T, typename P>
    auto operator()(InIt first, InIt const last, OutIt dest, T const &old_value, T const &new_value,
                    P p) const -> OutIt
    {
        while (first != last) {
            if (invoke(p, *first) == old_value) *dest = new_value;
            else
                *dest = std::move(*first);

            ++first;
            ++dest;
        }
        return dest;
    }
} replace_move{};

inline constexpr struct {
    template <typename InIt, typename OutIt, typename UnaryPredicate, typename T>
    auto operator()(InIt first, InIt last, OutIt dest, UnaryPredicate pred,
                    T const &new_value) const -> OutIt
    {
        return std::replace_copy_if(std::make_move_iterator(first), std::make_move_iterator(last),
                                    dest, astl::pass_fn(pred), new_value);
    }

    template <typename InIt, typename OutIt, typename UnaryPredicate, typename T, typename P>
    auto operator()(InIt first, InIt const last, OutIt dest, UnaryPredicate pred,
                    T const &new_value, P p) const -> OutIt
    {
        while (first != last) {
            if (pred(invoke(p, *first))) *dest = new_value;
            else
                *dest = std::move(*first);

            ++first;
            ++dest;
        }
        return dest;
    }
} replace_move_if{};

inline constexpr struct {
    template <typename InIt, typename N, typename OutIt, typename UnaryPredicate, typename T>
    auto operator()(InIt first, N n, OutIt dest, UnaryPredicate pred, T const &new_value) const
        -> std::pair<OutIt, InIt>
    {
        auto i(i::replace_copy_if_n(std::make_move_iterator(first), n, dest, astl::pass_fn(pred),
                                    new_value));
        return std::make_pair(i.first, i.second.base());
    }

    template <typename InIt, typename N, typename OutIt, typename UnaryPredicate, typename T,
              typename P>
    auto operator()(InIt first, N n, OutIt dest, UnaryPredicate pred, T const &new_value, P p) const
        -> std::pair<OutIt, InIt>
    {
        auto i(i::replace_copy_if_n(std::make_move_iterator(first), n, dest, astl::pass_fn(pred),
                                    new_value, astl::pass_fn(p)));
        return std::make_pair(i.first, i.second.base());
    }
} replace_move_if_n{};

inline constexpr struct {
    template <typename InIt, typename N, typename OutIt, typename T>
    auto operator()(InIt first, N n, OutIt dest, T const &old_value, T const &new_value) const
        -> std::pair<OutIt, InIt>
    {
        auto i(i::replace_copy_n(std::make_move_iterator(first), n, dest, old_value, new_value));
        return std::make_pair(i.first, i.second.base());
    }

    template <typename InIt, typename N, typename OutIt, typename T, typename P>
    auto operator()(InIt first, N n, OutIt dest, T const &old_value, T const &new_value, P p) const
        -> std::pair<OutIt, InIt>
    {
        auto i(i::replace_copy_n(std::make_move_iterator(first), n, dest, old_value, new_value,
                                 astl::pass_fn(p)));
        return std::make_pair(i.first, i.second.base());
    }
} replace_move_n{};

inline constexpr struct {
    template <typename FwdIt, typename N, typename T, typename U>
    auto operator()(FwdIt first, N n, T const &old_value, U const &new_value) const -> FwdIt
    {
        while (n != N(0)) {
            if (*first == old_value) *first = new_value;

            ++first;
            --n;
        }
        return first;
    }

    template <typename FwdIt, typename N, typename T, typename U, typename P>
    auto operator()(FwdIt first, N n, T const &old_value, U const &new_value, P p) const -> FwdIt
    {
        while (n != N(0)) {
            if (invoke(p, *first) == old_value) *first = new_value;

            ++first;
            --n;
        }
        return first;
    }
} replace_n{};

} // namespace i

namespace r
{

inline constexpr struct {
    template <typename R, typename T>
    auto operator()(R &&r, T const &old_value, T const &new_value) const -> void
    {
        i::replace(adl::begin(r), adl::end(r), old_value, new_value);
    }

    template <typename R, typename T, typename P>
    auto operator()(R &&r, T const &old_value, T const &new_value, P p) const -> void
    {
        i::replace(adl::begin(r), adl::end(r), old_value, new_value, astl::pass_fn(p));
    }

    /// Given a sequence container Cont, replace the range [cont_it, cont_end) with
    /// the range [val_it, val_end) (which is not from the same container).
    template <typename R, typename RandIt>
    auto operator()(R &cont, iter_of_range<R> cont_it, iter_of_range<R> cont_end, RandIt val_it,
                    RandIt val_end) const -> void
    {
        while (true) {
            if (val_it == val_end) {
                cont.erase(cont_it, cont_end);
                return;
            }
            if (cont_it == cont_end) {
                cont.insert(cont_it, val_it, val_end);
                return;
            }
            *cont_it = *val_it;
            ++cont_it;
            ++val_it;
        }
    }

    /// Given a sequence container Cont, replace the range [cont_it, cont_end) with
    /// the range R.
    template <typename R, typename Range = std::initializer_list<astl::range_value_type<R>>>
    void operator()(R &cont, iter_of_range<R> cont_it, iter_of_range<R> cont_end, Range r) const
    {
        (*this)(cont, cont_it, cont_end, r.begin(), r.end());
    }

} replace{};

inline constexpr struct {
    template <typename R, typename OutIt, typename T>
    auto operator()(R &&r, OutIt dest, T const &old_value, T const &new_value) const -> OutIt
    {
        return i::replace_copy(adl::begin(r), adl::end(r), dest, old_value, new_value);
    }

    template <typename R, typename OutIt, typename T, typename P>
    auto operator()(R &&r, OutIt dest, T const &old_value, T const &new_value, P p) const -> OutIt
    {
        return i::replace_copy(adl::begin(r), adl::end(r), dest, old_value, new_value,
                               astl::pass_fn(p));
    }
} replace_copy{};

inline constexpr struct {
    template <typename R, typename OutIt, typename UnaryPredicate, typename T>
    auto operator()(R &&r, OutIt dest, UnaryPredicate pred, T const &new_value) const -> OutIt
    {
        return i::replace_copy_if(adl::begin(r), adl::end(r), dest, astl::pass_fn(pred), new_value);
    }

    template <typename R, typename OutIt, typename UnaryPredicate, typename T, typename P>
    auto operator()(R &&r, OutIt dest, UnaryPredicate pred, T const &new_value, P p) const -> OutIt
    {
        return i::replace_copy_if(adl::begin(r), adl::end(r), dest, astl::pass_fn(pred), new_value,
                                  astl::pass_fn(p));
    }
} replace_copy_if{};

inline constexpr struct {
    template <typename R, typename N, typename OutIt, typename UnaryPredicate, typename T>
    auto operator()(R &&r, N n, OutIt dest, UnaryPredicate pred, T const &new_value) const
        -> std::pair<OutIt, astl::iter_of_range<R>>
    {
        return i::replace_copy_if_n(adl::begin(r), n, dest, astl::pass_fn(pred), new_value);
    }

    template <typename R, typename N, typename OutIt, typename UnaryPredicate, typename T,
              typename P>
    auto operator()(R &&r, N n, OutIt dest, UnaryPredicate pred, T const &new_value, P p) const
        -> std::pair<OutIt, astl::iter_of_range<R>>
    {
        return i::replace_copy_if_n(adl::begin(r), n, dest, astl::pass_fn(pred), new_value,
                                    astl::pass_fn(p));
    }
} replace_copy_if_n{};

inline constexpr struct {
    template <typename R, typename UnaryPredicate, typename T>
    auto operator()(R &&r, UnaryPredicate pred, T const &new_value) const -> void
    {
        i::replace_if(adl::begin(r), adl::end(r), astl::pass_fn(pred), new_value);
    }

    template <typename R, typename UnaryPredicate, typename T, typename P>
    auto operator()(R &&r, UnaryPredicate pred, T const &new_value, P p) const -> void
    {
        i::replace_if(adl::begin(r), adl::end(r), astl::pass_fn(pred), new_value, astl::pass_fn(p));
    }
} replace_if{};

inline constexpr struct {
    template <typename R, typename N, typename UnaryPredicate, typename T>
    auto operator()(R &&r, N n, UnaryPredicate pred, T const &new_value) const -> iter_of_range<R>
    {
        return i::replace_if_n(adl::begin(r), n, astl::pass_fn(pred), new_value);
    }

    template <typename R, typename N, typename UnaryPredicate, typename T, typename P>
    auto operator()(R &&r, N n, UnaryPredicate pred, T const &new_value, P p) const
        -> iter_of_range<R>
    {
        return i::replace_if_n(adl::begin(r), n, astl::pass_fn(pred), new_value, astl::pass_fn(p));
    }
} replace_if_n{};

inline constexpr struct {
    template <typename R, typename OutIt, typename T>
    auto operator()(R &&r, OutIt dest, T const &old_value, T const &new_value) const -> OutIt
    {
        return i::replace_move(adl::begin(r), adl::end(r), dest, old_value, new_value);
    }

    template <typename R, typename OutIt, typename T, typename P>
    auto operator()(R &&r, OutIt dest, T const &old_value, T const &new_value, P p) const -> OutIt
    {
        return i::replace_move(adl::begin(r), adl::end(r), dest, old_value, new_value,
                               astl::pass_fn(p));
    }
} replace_move{};

inline constexpr struct {
    template <typename R, typename OutIt, typename UnaryPredicate, typename T>
    auto operator()(R &&r, OutIt dest, UnaryPredicate pred, T const &new_value) const -> OutIt
    {
        return i::replace_move_if(adl::begin(r), adl::end(r), dest, astl::pass_fn(pred), new_value);
    }

    template <typename R, typename OutIt, typename UnaryPredicate, typename T, typename P>
    auto operator()(R &&r, OutIt dest, UnaryPredicate pred, T const &new_value, P p) const -> OutIt
    {
        return i::replace_move_if(adl::begin(r), adl::end(r), dest, astl::pass_fn(pred), new_value,
                                  astl::pass_fn(p));
    }
} replace_move_if{};

inline constexpr struct {
    template <typename R, typename N, typename OutIt, typename UnaryPredicate, typename T>
    auto operator()(R &&r, N n, OutIt dest, UnaryPredicate pred, T const &new_value) const
        -> std::pair<OutIt, astl::iter_of_range<R>>
    {
        return i::replace_move_if_n(adl::begin(r), n, dest, astl::pass_fn(pred), new_value);
    }

    template <typename R, typename N, typename OutIt, typename UnaryPredicate, typename T,
              typename P>
    auto operator()(R &&r, N n, OutIt dest, UnaryPredicate pred, T const &new_value, P p) const
        -> std::pair<OutIt, astl::iter_of_range<R>>
    {
        return i::replace_move_if_n(adl::begin(r), n, dest, astl::pass_fn(pred), new_value,
                                    astl::pass_fn(p));
    }
} replace_move_if_n{};

inline constexpr struct {
    template <typename R, typename N, typename OutIt, typename T>
    auto operator()(R &&r, N n, OutIt dest, T const &old_value, T const &new_value) const
        -> std::pair<OutIt, astl::iter_of_range<R>>
    {
        return i::replace_move_n(adl::begin(r), n, dest, old_value, new_value);
    }

    template <typename R, typename N, typename OutIt, typename T, typename P>
    auto operator()(R &&r, N n, OutIt dest, T const &old_value, T const &new_value, P p) const
        -> std::pair<OutIt, astl::iter_of_range<R>>
    {
        return i::replace_move_n(adl::begin(r), n, dest, old_value, new_value, astl::pass_fn(p));
    }
} replace_move_n{};

inline constexpr struct {
    template <typename R, typename N, typename T>
    auto operator()(R &&r, N n, T const &old_value, T const &new_value) const -> iter_of_range<R>
    {
        return i::replace(adl::begin(r), n, old_value, new_value);
    }

    template <typename R, typename N, typename T, typename P>
    auto operator()(R &&r, N n, T const &old_value, T const &new_value, P p) const
        -> iter_of_range<R>
    {
        return i::replace_n(adl::begin(r), n, old_value, new_value, astl::pass_fn(p));
    }
} replace_n{};

} // namespace r
} // namespace astl

#endif // ASTL_INCLUDE_REPLACE_HPP
