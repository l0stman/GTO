#ifndef GTO_EQUI_DIST_H_
#define GTO_EQUI_DIST_H_

#include <unordered_map>
#include <utility>
#include <vector>

#include "array.h"
#include "range.h"

namespace GTO {
typedef std::pair<Hand, Hand> PairHands;
}

namespace std {
template<>
struct hash<GTO::PairHands> {
        // Use the function hash_combine from boost.
        void
        hash_combine(const GTO::Hand& c, size_t& seed) const
        {
                seed ^= hash<GTO::Hand>()(c) + 0x9e3779b9 + (seed << 6) +
                        (seed >> 2);
        }

        size_t operator()(const GTO::PairHands& p) const
        {
                size_t seed = 0;
                hash_combine(p.first, seed);
                hash_combine(p.second, seed);
                return seed;
        }
};
} // namespace std

namespace GTO {

class EquiDist {
public:
        explicit EquiDist(const Range& hero,
                          const Range& villain,
                          const CardSet& board=CardSet());

        // Return the equity of hero's hand against villain's or -1 if
        // it doesn't exist.
        double Equity(const Hand& hero, const Hand& vill) const;

        // Return a lookup table represented as an array such that
        // Array.get(i, j) == Equity(hands1[i], hands2[j]).
        Array LUT(const std::vector<Hand>& hands1,
                  const std::vector<Hand>& hands2) const;
private:
        void InitRiver(const Range& hero,
                       const Range& villain,
                       const CardSet& board);
        void InitFlopOrTurn(const Range& hero,
                            const Range& villain,
                            const CardSet& board);
        void InitPreflop(const Range& hero, const Range& villain);
        void set_equity(const Hand& hero,
                        const Hand& villain,
                        const double& val)
        {
                equity_[PairHands(hero, villain)] = val;
        }

        std::unordered_map<PairHands, double> equity_;
        const char *preflop_file_ = "preflop-matchups.txt";
};
}

#endif // !GTO_EQUI_DIST_H_
