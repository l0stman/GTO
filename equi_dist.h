#ifndef GTO_EQUI_DIST_H_
#define GTO_EQUI_DIST_H_

#include <unordered_map>

#include "equi_dist_interface.h"
#include "range.h"

namespace GTO {

// Represents the equity distribution postflop.
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

        virtual Array LUT(const std::vector<Hand>& hands1,
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
                equity_[Pair<Hand>(hero, villain)] = val;
        }

        const char *preflop_file_ = "preflop-matchups.txt";
        std::unordered_map<Pair<Hand>, double> equity_;
};
}

#endif // !GTO_EQUI_DIST_H_
