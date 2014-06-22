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

        virtual Array<double> LUT(
                const std::vector<PreflopHand>& hands1,
                const std::vector<PreflopHand>& hands2) const;
private:
        void
        set_equity(const char *h, const char *v, const double& EQ)
        {
                equity_[Pair<PreflopHand>(PreflopHand(h),PreflopHand(v))] = EQ;
        }

        const char *preflop_equity_file_ = "preflop-matchups.txt";
        std::unordered_map<Pair<PreflopHand>, double> equity_;
};

// Represents the number of match-ups suit combos between two preflop
// hands.
class SuitCombos {
public:
        SuitCombos();
        ~SuitCombos() {}

        // Return the number of possible match-ups between a hand
        // represented as "hero" and all the hands represented as
        // "villain" in preflop if we take into account their suits.
        short NumCombos(const PreflopHand& hero,
                        const PreflopHand& villain) const
        {
                return combos_.at(Pair<PreflopHand>(hero, villain));
        }

        // Return a lookup table represented as an array such that
        // Array.get(i, j) == NumCombos(hands1[i], hands2[j]).
        Array<short> LUT(
                const std::vector<PreflopHand>& hands1,
                const std::vector<PreflopHand>& hands2) const;
private:
        void
        set_combos(const char *h, const char *v, const double& n)
        {
                combos_[Pair<PreflopHand>(PreflopHand(h), PreflopHand(v))] = n;
        }

        const char *preflop_combos_file_ = "preflop-combos.txt";
        std::unordered_map<Pair<PreflopHand>, short> combos_;
};

} // namespace GTO

#endif  // !GTO_PREFLOP_EQUI_DIST_H_
