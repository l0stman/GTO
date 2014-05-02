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
private:
        static const size_t minrounds_ = 1000;
        static const size_t nsamples_ = 1000;
        static constexpr double threshold_ = 0.00000001;
        double hrange_equity_;

        typedef std::unordered_map<CardSet, double, Range::CSHash> CSTable;
        CSTable hero_equity_;
        CSTable vill_equity_;
};
}

#endif // !GTO_EQUI_DIST_H_
