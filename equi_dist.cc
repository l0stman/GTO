#include "equi_dist.h"

#include <cstdio>
#include <cstdlib>
#include <vector>

#include <pokerstove/peval/Card.h>
#include <pokerstove/peval/PokerHandEvaluator.h>

namespace GTO {
using std::vector;
using pokerstove::PokerHandEvaluator;
using pokerstove::PokerEvaluation;

EquiDist::EquiDist(const Range& hero,
                   const Range& villain,
                   const CardSet& init_board)
{
        switch (init_board.size()) {
        case 5:
                InitRiver(hero, villain, init_board);
                break;
        default:
                fprintf(stderr, "Unsupported initial board size: %s\n",
                        init_board.str().c_str());
                exit(1);
        }
}

void
EquiDist::InitRiver(const Range& hero,
                    const Range& villain,
                    const CardSet& board)
{
        boost::shared_ptr<PokerHandEvaluator> E(PokerHandEvaluator::alloc("h"));
        PokerEvaluation he, ve;
        std::pair<CardSet, CardSet> p;

        for (auto hit = hero.begin(); hit != hero.end(); ++hit) {
                if (hit->intersects(board))
                        continue;
                for (auto vit = villain.begin(); vit != villain.end(); ++vit) {
                        if (vit->intersects(board) || vit->intersects(*hit))
                                continue;
                        he = E->evaluateHand(*hit, board).high();
                        ve = E->evaluateHand(*vit, board).high();
                        p.first = *hit;
                        p.second = *vit;
                        if (he == ve)
                                equity_[p] = 0.5;
                        else if (he > ve)
                                equity_[p] = 1;
                        else
                                equity_[p] = 0;
                }
        }
}

double EquiDist::Equity(const CardSet& hero, const CardSet& villain)
{
        std::pair<CardSet, CardSet> p(hero, villain);

        return equity_.count(p) > 0 ? equity_[p] : -1;
}
}
