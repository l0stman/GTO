#include "equi_dist.h"

#include <cstdio>
#include <cstdlib>
#include <vector>

#include <pokerstove/peval/Card.h>
#include <pokerstove/peval/PokerHandEvaluator.h>
#include <pokerstove/util/combinations.h>

#include "err.h"

namespace {

using std::vector;
using pokerstove::PokerHandEvaluator;
using pokerstove::PokerEvaluation;
using pokerstove::Card;
using pokerstove::CardSet;
using pokerstove::combinations;

typedef std::unordered_map<GTO::Pair<GTO::Hand>, double> Table;

inline void
set(const GTO::Hand& hero,
    const GTO::Hand& villain,
    const double& val,
    Table& equity)
{
        equity[GTO::Pair<GTO::Hand>(hero, villain)] = val;
}

void
InitRiver(const GTO::Range& hero,
          const GTO::Range& villain,
          const CardSet& board,
          Table& equity)
{
        boost::shared_ptr<PokerHandEvaluator> E(PokerHandEvaluator::alloc("h"));
        PokerEvaluation he, ve;

        for (auto h : hero) {
                if (h.intersects(board))
                        continue;
                for (auto v : villain) {
                        if (v.intersects(board) || v.intersects(h))
                                continue;
                        he = E->evaluateHand(h, board).high();
                        ve = E->evaluateHand(v, board).high();
                        if (he == ve)
                                set(h, v, 0.5, equity);
                        else if (he > ve)
                                set(h, v, 1, equity);
                        else
                                set(h, v, 0, equity);
                }
        }
}

void
InitFlopOrTurn(const GTO::Range& hero,
               const GTO::Range& villain,
               const CardSet& init_board,
               Table& equity)
{
        boost::shared_ptr<PokerHandEvaluator> E(PokerHandEvaluator::alloc("h"));
        PokerEvaluation he, ve;
        CardSet all;

        for (auto h : hero) {
                if (h.intersects(init_board))
                        continue;
                for (auto v : villain) {
                        if (v.intersects(init_board) ||
                            v.intersects(h) ||
                            equity.count(GTO::Pair<GTO::Hand>(h, v)) > 0)
                                continue;
                        CardSet dead(init_board);
                        dead |= h;
                        dead |= v;
                        double shares = 0;
                        size_t total = 0;
                        short N = static_cast<short>(5-init_board.size());
                        all.fill();
                        vector<Card> deck = all.remove(dead).cards();
                        combinations boards(deck.size(), N);
                        do {
                                CardSet board(init_board);
                                for (short i=0; i<N; i++)
                                        board.insert(deck[boards[i]]);
                                he = E->evaluateHand(h, board).high();
                                ve = E->evaluateHand(v, board).high();
                                if (he == ve)
                                        shares += 0.5;
                                else if (he > ve)
                                        shares += 1;
                                ++total;
                        } while (boards.next());
                        if (N > 0) {
                                set(h, v, shares/total, equity);
                                set(v, h, 1-shares/total, equity);
                        }
                }
        }
}
} // namespace

namespace GTO {

EquiDist::EquiDist(const Range& hero,
                   const Range& villain,
                   const CardSet& board)
{
        switch (board.size()) {
        case 0:
                err::quit("Not supported. Use PreflopEquiDist instead.");
                break;
        case 3:
        case 4:
                InitFlopOrTurn(hero, villain, board, equity_);
                break;
        case 5:
                InitRiver(hero, villain, board, equity_);
                break;
        default:
                err::quit("Unsupported initial board size: %s.",
                          board.str().c_str());
        }
}

Array<double>
EquiDist::LUT(const std::vector<Hand>& hands1,
              const std::vector<Hand>& hands2) const
{
        Array<double> equity(hands1.size(), hands2.size());
        for (size_t i = 0; i < hands1.size(); i++)
                for (size_t j = 0; j < hands2.size(); j++)
                        equity.set(i, j, Equity(hands1[i], hands2[j]));
        return equity;
}
}
