#include <iterator>

#include <algorithm>
#include <array>
#include <cassert>
#include <cmath>

#include "slot_map.hpp"
#include <functional>
#include <iostream>
#include <variant>

//template <typename T> void info() { std::cout << __PRETTY_FUNCTION__ << "\n\n"; }

#include "astl/algorithm.hpp"
#include "astl/iterator_adaptors.hpp"

template <typename I> void print_range(I f, I l)
{
    while (f != l) std::cout << *f++ << " ";
    std::cout << '\n';
}

namespace astl
{

namespace i
{

} // namespace i

namespace r
{

} // namespace r

} // namespace astl

constexpr auto is_even(const int &x) -> bool { return (x % 2 == 0); }

namespace loom = astl;

auto main() -> int
{

    stdext::slot_map<int> map;

    std::vector v{0, 1, 2, 3, 4, 5, 6, 1, 7, 8, 9, 10, 11};

    astl::i::none_of_adjacent(v.begin(), v.end(), [](auto x, auto y) { return true; });

    std::cout << *astl::i::first_repeating_element(v.begin(), v.end());
}
