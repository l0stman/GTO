#include "equi_dist.h"

#include <cstdio>
#include <cstdlib>
#include <vector>

#include <pokerstove/peval/Card.h>
#include <pokerstove/peval/PokerHandEvaluator.h>
#include <pokerstove/util/combinations.h>

#include "err.h"

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
        case 0:
                InitPreflop(hero, villain);
                break;
        case 3:
        case 4:
                InitFlopOrTurn(hero, villain, board);
                break;
        case 5:
                InitRiver(hero, villain, board);
                break;
        default:
                err::quit("Unsupported initial board size: %s.",
                          board.str().c_str());
        }
}

void
EquiDist::InitRiver(const Range& hero,
                    const Range& villain,
                    const CardSet& board)
{
        boost::shared_ptr<PokerHandEvaluator> E(PokerHandEvaluator::alloc("h"));
        PokerEvaluation he, ve;
        Pair<Hand> p;

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
        CardSet all;

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
                        all.fill();
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
                                set_equity(*hit, *vit, shares/total);
                                set_equity(*vit, *hit, 1-shares/total);
                        }
                }
        }
}

void
EquiDist::InitPreflop(const Range& hero, const Range& villain)
{
        FILE *fp;
        char h[4];
        char v[4];
        double EQh = 0;
        double EQv = 0;
        std::unordered_map<Pair<Hand>, double> equity;

        if ((fp = fopen(preflop_file_, "r")) == NULL)
                err::sys("Can't open %s", preflop_file_);
        while (fscanf(fp, "%s vs. %s : %lf vs. %lf", h, v, &EQh, &EQv)!=EOF) {
                Range hr(h);
                Range vr(v);
                Hand hc = *hr.begin();
                Hand vc = *vr.begin();
                hc.canonize();
                vc.canonize();
                equity[Pair<Hand>(hc, vc)] = EQh;
                equity[Pair<Hand>(vc, hc)] = EQv;
        }
        fclose(fp);

        for (auto hit = hero.begin(); hit != hero.end(); ++hit)
                for (auto vit = villain.begin(); vit != villain.end(); ++vit)
                        if (hit->disjoint(*vit)) {
                                Hand hc = *hit;
                                Hand vc = *vit;
                                hit->canonize();
                                vit->canonize();
                                if (hc == vc)
                                        set_equity(*hit, *vit, 0.5);
                                else
                                        set_equity(*hit,
                                                   *vit,
                                                   equity[Pair<Hand>(hc, vc)]);

                        }
}

Array
EquiDist::LUT(const std::vector<Hand>& hands1,
              const std::vector<Hand>& hands2) const
{
        Array equity(hands1.size(), hands2.size());
        for (size_t i = 0; i < hands1.size(); i++)
                for (size_t j = 0; j < hands2.size(); j++)
                        equity.set(i, j, Equity(hands1[i], hands2[j]));
        return equity;
}
}
