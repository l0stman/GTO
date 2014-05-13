#ifndef GTO_EQUI_DIST_H_
#define GTO_EQUI_DIST_H_

#include "range.h"

#include <unordered_map>
#include <utility>

namespace GTO {

class EquiDist {
public:
        explicit EquiDist(const Range& hero,
                          const Range& villain,
                          const CardSet& board=CardSet());

        // Return the equity of hero's hand against villain's or -1 if
        // it doesn't exit.
        virtual double Equity(const CardSet& hero, const CardSet& villain);
private:
        void InitRiver(const Range& hero,
                       const Range& villain,
                       const CardSet& board);
        struct CSPairHash {
                // Use the function hash_combine from boost
                void
                hash_combine(const CardSet& c, size_t& seed) const
                {
                        seed ^= Range::CSHash()(c) + 0x9e3779b9 + (seed << 6) +
                                (seed >> 2);
                }
                size_t
                operator()(const std::pair<CardSet, CardSet>& p) const
                {
                        size_t seed = 0;
                        hash_combine(p.first, seed);
                        hash_combine(p.second, seed);
                        return seed;
                }
        };

        typedef std::unordered_map<std::pair<CardSet, CardSet>, double,
                                   CSPairHash> EQTable;
        EQTable equity_;
};
}

#endif // !GTO_EQUI_DIST_H_
