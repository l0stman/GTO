#include "equi_dist.h"

#include <cstdio>
#include <cstdlib>
#include <vector>

#include <pokerstove/peval/Card.h>
#include <pokerstove/peval/PokerHandEvaluator.h>

namespace {
using std::string;
using pokerstove::CardSet;
using pokerstove::Card;

const string kRanks = "23456789TJQKA";
const string kSuits = "cdhs";

inline int
RandInt(int n)
{
        return static_cast<int>(static_cast<double>(rand())/RAND_MAX*n);
}

inline char
RandElt(const string& s)
{
        return s[RandInt(s.length())];
}

void
ExpandBoard(const CardSet& init_board,
            const CardSet& dead_cards,
            CardSet& final_board)
{
        CardSet dc(dead_cards);
        string s(2, 'x');
        Card c;

        final_board.clear();
        final_board.insert(init_board);
        for (int i = init_board.size(); i < 5; i++) {
                do {
                        s[0] = RandElt(kRanks);
                        s[1] = RandElt(kSuits);
                        c = Card(s);
                } while (dc.contains(c));
                dc.insert(c);
                final_board.insert(c);
        }
}

void
CheckRangesOrDie(const GTO::Range& h,
                 const GTO::Range& v,
                 const CardSet& b)
{
        bool ok = false;
        for (auto hit = h.begin(); !ok && hit != h.end(); hit++)
                if (hit->disjoint(b))
                        for (auto vit = v.begin(); vit != v.end(); vit++)
                                if (vit->disjoint(b) && vit->disjoint(*hit)) {
                                        ok = true;
                                        break;
                                }
        if (!ok) {
                fprintf(stderr, "The ranges and board conflict.\n\
Hero\t: %s\nVillain\t: %s\nBoard\t: %s\n", h.str().c_str(), v.str().c_str(),
                        b.str().c_str());
                exit(1);
        }
}

size_t
AddRange(const GTO::Range& r,
         const CardSet& board,
         std::vector<CardSet>& hands)
{
        size_t size = 0;
        for (auto it = r.begin(); it != r.end(); it++)
                if (board.disjoint(*it)) {
                        size++;
                        hands.push_back(*it);
                }
        return size;
}
}

namespace GTO {
using std::vector;
using pokerstove::PokerHandEvaluator;

EquiDist::EquiDist(const Range& hero, const Range& villain)
{
        EquiDist(hero, villain, CardSet());
}

EquiDist::EquiDist(const Range& hero,
                   const Range& villain,
                   const CardSet& init_board)
{
        CheckRangesOrDie(hero, villain, init_board);
        vector<CardSet> hands;
        size_t hsiz = AddRange(hero, init_board, hands);
        size_t vsiz = AddRange(villain, init_board, hands);
        vector<double> shares(hsiz+vsiz, 0);
        vector<size_t> total(hsiz+vsiz, 0);
        vector<double> equity(hsiz+vsiz, -1);
        boost::shared_ptr<PokerHandEvaluator> E(PokerHandEvaluator::alloc("h"));
        pokerstove::PokerEvaluation he, ve;
        CardSet board, dead_cards;
        bool stop = false;
        size_t nrounds = 0;

        srand(time(0));
        while (!stop) {
                nrounds++;
                for (size_t i = 0; i < nsamples_; i++) {
                        int h, v;
                        dead_cards.clear();
                        dead_cards.insert(init_board);
                        do {
                                h = RandInt(hsiz);
                                v = hsiz + RandInt(vsiz);
                        } while (dead_cards.intersects(hands[h]) ||
                                 dead_cards.intersects(hands[v]) ||
                                 hands[h].intersects(hands[v]));
                        total[h]++;
                        total[v]++;
                        dead_cards.insert(hands[h]);
                        dead_cards.insert(hands[v]);
                        ExpandBoard(init_board, dead_cards, board);
                        he = E->evaluateHand(hands[h], board).high();
                        ve = E->evaluateHand(hands[v], board).high();
                        if (he == ve) {
                                shares[h] += 0.5;
                                shares[v] += 0.5;
                        } else if (he > ve)
                                shares[h]++;
                        else
                                shares[v]++;

                }
                if (nrounds < minrounds_)
                        continue;
                double err = 0.0;
                for (size_t i = 0; i < shares.size(); i++) {
                        if (equity[i] >= 0) {
                                double d = equity[i]-shares[i]/total[i];
                                err += d*d;
                        } else if (total[i] > 0)
                                goto update;
                }
                if (err < threshold_)
                        stop = true;
        update:
                for (size_t i = 0; i < shares.size(); i++)
                        if (total[i] > 0)
                                equity[i] = shares[i]/total[i];
        }
        hrange_equity_ = 0;
        size_t htotal = 0;
        for (size_t i = 0; i < hsiz; i++)
                if (total[i] > 0) {
                        hrange_equity_ += shares[i];
                        htotal += total[i];
                        hero_equity_[hands[i]] = equity[i];
                }
        hrange_equity_ /= htotal;
        for (size_t i = hsiz; i < vsiz+hsiz; i++)
                if (total[i] > 0)
                        vill_equity_[hands[i]] = equity[i];
}

double
EquiDist::HeroEquity()
{
        return hrange_equity_;
}

double
EquiDist::VillEquity()
{
        return 1-hrange_equity_;
}

double
EquiDist::HeroEquity(const CardSet& hand)
{
        return hero_equity_.count(hand) > 0 ? hero_equity_[hand] : -1;
}

double
EquiDist::VillEquity(const CardSet& hand)
{
        return vill_equity_.count(hand) > 0 ? vill_equity_[hand] : -1;
}
}
