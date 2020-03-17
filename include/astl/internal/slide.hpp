//
// Created by Rijad on 04-Aug-18.
//

#ifndef ASTL_INCLUDE_SLIDE_HPP
#define ASTL_INCLUDE_SLIDE_HPP

#include <algorithm>
#include <utility>

namespace astl
{
namespace i
{
inline constexpr struct {
    template <typename RandIt>
    // requires RandIt RandomAccessIterator
    auto operator()(RandIt first, RandIt last, RandIt pos) const -> std::pair<RandIt, RandIt>
    {
        if (pos < first) return std::make_pair(pos, std::rotate(pos, first, last));

        if (last < pos) return std::make_pair(std::rotate(first, last, pos), pos);

        return std::make_pair(first, last);
    }
} slide{};

} // namespace i
} // namespace astl

#endif // ASTL_INCLUDE_SLIDE_HPP
