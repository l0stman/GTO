#ifndef GTO_EQUI_DIST_H_
#define GTO_EQUI_DIST_H_

#include "range.h"

#include <unordered_map>
#include <utility>

namespace GTO {

class EquiDist {
public:
        explicit EquiDist(const Range& hero, const Range& villain);
        explicit EquiDist(const Range& hero,
                          const Range& villain,
                          const CardSet& board);
        // Return the equity of the range for hero or villain.
        virtual double HeroEquity();
        virtual double VillEquity();

        // Return the equity of the hand or -1 if it doesn't exist.
        virtual double HeroEquity(const CardSet& hand);
        virtual double VillEquity(const CardSet& hand);

        // Return the equity of hand1 vs hand 2 or -1 if it doesn't exist.
        virtual double Equity(const CardSet& hand1, const CardSet& hand2);
private:
        static const size_t minrounds_ = 1000;
        static const size_t nsamples_ = 1000;
        static constexpr double threshold_ = 0.00000001;
        double hrange_equity_;

        struct CSPairHash {
                // Use the function hash_combine from boost.
                void
                hash_combine(size_t& seed, const CardSet& h) const
                {
                        seed ^= Range::CSHash()(h) + 0x9e3779b9 + (seed << 6) +
                                (seed >> 2);
                }

                size_t
                operator()(const std::pair<CardSet, CardSet>& p) const
                {
                        size_t seed = 0;
                        hash_combine(seed, p.first);
                        hash_combine(seed, p.second);
                        return seed;
                }
        };

        typedef std::unordered_map<CardSet, double, Range::CSHash> CSTable;
        typedef std::unordered_map<std::pair<CardSet, CardSet>, double,
                                   CSPairHash> CSPairTable;

        inline bool
        CSCmp(const CardSet& h, const CardSet& v)
        {
                return h.mask() < v.mask();
        }

        inline void
        Inc(const CardSet& h, const CardSet& v, CSPairTable& T, double val)
        {
                std::pair<CardSet, CardSet> p;
                if (CSCmp(h, v)) {
                        p.first = h;
                        p.second = v;
                } else {
                        p.first = v;
                        p.second = h;
                }
                if (T.count(p) > 0)
                        T[p] += val;
                else
                        T[p] = val;
        }

        CSTable hero_equity_;
        CSTable vill_equity_;
        CSPairTable equity_;
};
}

#endif // !GTO_EQUI_DIST_H_
