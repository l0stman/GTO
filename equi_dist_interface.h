#ifndef GTO_EQUI_DIST_INTERFACE_H_
#define GTO_EQUI_DIST_INTERFACE_H_

#include <utility>
#include <vector>

#include "array.h"

namespace GTO {

// Interface representing the equity distribution between two players
// playing heads-up.
template<class Hand>
class EquiDistInterface {
public:
        // Return the equity of hero's hand against villain's or -1 if
        // it doesn't exist.
        virtual double Equity(const Hand& hero, const Hand& villain) const = 0;

        // Return a lookup table represented as an array such that
        // Array.get(i, j) == Equity(hands1[i], hands2[j]).
        virtual Array<double> LUT(
                const std::vector<Hand>& hand1,
                const std::vector<Hand>& hand2) const = 0;
};

template<class Hand>
using Pair = std::pair<Hand, Hand>;
} // namespace GTO

namespace std {

template<class Hand>
struct hash<GTO::Pair<Hand>> {
        // Use the function hash_combine from boost.
        void
        hash_combine(const Hand& c, size_t& seed) const
        {
                seed ^= hash<Hand>()(c) + 0x9e3779b9 + (seed << 6) +
                        (seed >> 2);
        }

        size_t operator()(const GTO::Pair<Hand>& p) const
        {
                size_t seed = 0;
                hash_combine(p.first, seed);
                hash_combine(p.second, seed);
                return seed;
        }
};
} // namespace std

#endif  // !GTO_EQUI_DIST_INTERFACE_H_
