#ifndef GTO_EQUI_DIST_H_
#define GTO_EQUI_DIST_H_

#include <unordered_map>

#include "equi_dist_interface.h"
#include "range.h"

namespace GTO {

// Implementation of post-flop equity distribution.
class EquiDist : public EquiDistInterface<Hand> {
public:
        explicit EquiDist(const Range& hero,
                          const Range& villain,
                          const CardSet& board=CardSet());

        virtual double
        Equity(const Hand& hero, const Hand& villain) const
        {
                Pair<Hand> p(hero, villain);

                return equity_.count(p) > 0 ? equity_.at(p) : -1;
        }

        virtual Array<double> LUT(
                const std::vector<Hand>& hands1,
                const std::vector<Hand>& hands2) const;
private:
        std::unordered_map<Pair<Hand>, double> equity_;
};
}

#endif // !GTO_EQUI_DIST_H_
