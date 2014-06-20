#ifndef GTO_PREFLOP_EQUI_DIST_H_
#define GTO_PREFLOP_EQUI_DIST_H_

#include <unordered_map>

#include "equi_dist_interface.h"
#include "preflop_range.h"

namespace GTO {

// Represents the equity distribution preflop.
class PreflopEquiDist : public EquiDistInterface<PreflopHand> {
public:
        PreflopEquiDist();
        ~PreflopEquiDist() {}

        virtual double
        Equity(const PreflopHand& hero, const PreflopHand& villain) const
        {
                Pair<PreflopHand> p(hero, villain);

                return equity_.count(p) > 0 ? equity_.at(p) : -1;
        }

        virtual Array LUT(const std::vector<PreflopHand>& hands1,
                          const std::vector<PreflopHand>& hands2) const;
private:
        void
        set_equity(const char *h, const char *v, const double& EQ)
        {
                equity_[Pair<PreflopHand>(PreflopHand(h),PreflopHand(v))] = EQ;
        }

        const char *preflop_file_ = "preflop-matchups.txt";
        std::unordered_map<Pair<PreflopHand>, double> equity_;
};

} // namespace GTO

#endif  // !GTO_PREFLOP_EQUI_DIST_H_
