#include "equi_dist.h"

#include <cstdio>
#include <cstdlib>
#include <vector>

#include <pokerstove/peval/Card.h>
#include <pokerstove/peval/PokerHandEvaluator.h>
#include <pokerstove/util/combinations.h>

namespace GTO {
using std::vector;
using pokerstove::PokerHandEvaluator;
using pokerstove::PokerEvaluation;
using pokerstove::Card;
using pokerstove::combinations;

EquiDist::EquiDist(const Range& hero,
                   const Range& villain,
                   const CardSet& board)
{
        switch (board.size()) {
        case 3:                 // FALLTHROUGH
        case 4:
                InitFlopOrTurn(hero, villain, board);
                break;
        case 5:
                InitRiver(hero, villain, board);
                break;
        default:
                fprintf(stderr, "Unsupported initial board size: %s\n",
                        board.str().c_str());
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

void
EquiDist::InitFlopOrTurn(const Range& hero,
                         const Range& villain,
                         const CardSet& init_board)
{
        boost::shared_ptr<PokerHandEvaluator> E(PokerHandEvaluator::alloc("h"));
        PokerEvaluation he, ve;
        uint64_t all_mask = ~(0xffffffffffffffff<<CardSet::STANDARD_DECK_SIZE);

        for (auto hit = hero.begin(); hit != hero.end(); ++hit) {
                if (hit->intersects(init_board))
                        continue;
                for (auto vit = villain.begin(); vit != villain.end(); ++vit) {
                        if (vit->intersects(init_board) ||
                            vit->intersects(*hit))
                                continue;
                        if (Equity(*hit, *vit) >= 0)
                                continue;
                        CardSet dead(init_board);
                        dead |= *hit;
                        dead |= *vit;
                        double shares = 0;
                        size_t total = 0;
                        short N = static_cast<short>(5-init_board.size());
                        CardSet all(all_mask);
                        vector<Card> deck = all.remove(dead).cards();
                        combinations boards(deck.size(), N);
                        do {
                                CardSet board(init_board);
                                for (short i=0; i<N; i++)
                                        board.insert(deck[boards[i]]);
                                he = E->evaluateHand(*hit, board).high();
                                ve = E->evaluateHand(*vit, board).high();
                                if (he == ve)
                                        shares += 0.5;
                                else if (he > ve)
                                        shares += 1;
                                ++total;
                        } while (boards.next());
                        if (N > 0) {
                                SetEquity(*hit, *vit, shares/total);
                                SetEquity(*vit, *hit, 1-shares/total);
                        }
                }
        }
}

double EquiDist::Equity(const CardSet& hero, const CardSet& villain)
{
        std::pair<CardSet, CardSet> p(hero, villain);

        return equity_.count(p) > 0 ? equity_[p] : -1;
}
}
