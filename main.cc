#include "range.h"

#include <cstdio>
#include <utility>
#include <vector>

#include <pokerstove/peval/Card.h>
#include <pokerstove/peval/PokerHandEvaluator.h>

using GTO::Range;
using pokerstove::CardSet;
using pokerstove::PokerHandEvaluator;
using std::vector;

int
main(int argc, char *argv[])
{
        Range hero("AcJc,KcJc,QcJc,JcTc,5c4c,KcJh,QcJh,KdJc,QdJc,5d4d,KdJh,QdJh,KhJc,QhJc,Ah2h,Ah5h,Ah8h,AhJh,KhJh,QhJh,JhTh,AsTc,KsJc,QsJc,AsTd,AsTh,KsJh,QsJh,Qs6s,QsTs,Ts7s,9s7s,8s7s,3d3c,3h3c,3h3d,6s6c,6d6c,6d6s,AsAc,AdAc,AhAc");
        Range vill("44,55,77,88,99,TT,JJ,A3s,A6s,K6s,KJs,Q6s,QJs,J2s,J3s,J4s,J5s,J6s,J7s,J8s,J9s,JTs,62s,63s,64s,A6o,K6o,Q6o,J4o,J5o,J6o,J7o,J8o,J9o,JTo,T6o,96o,86o,76o,65o,Ts4s,Ts5s,Ts6s,Ts7s,Ts8s,Ts9s,9s4s,9s5s,9s6s,9s7s,9s8s,8s4s,8s5s,8s6s,8s7s,7s6s,6s5s,3d3c,6d6c,QsQc,QdQc,QhQc,KsKc,KdKc,KhKc,AsAc,AdAc,AhAc");
        CardSet board("Js6h3sJd2d");
        size_t hsiz = hero.size();
        size_t vsiz = vill.size();
        vector<CardSet> hands(hsiz*vsiz);
        vector<double> equity(hsiz*vsiz, -1);
        boost::shared_ptr<PokerHandEvaluator> E(PokerHandEvaluator::alloc("h"));
        pokerstove::PokerEvaluation he, ve;
        const double pot = 53;
        const double stack = 48.5;

        fprintf(stderr, "Hero: %d hands, Villain: %d hands.\n", hsiz, vsiz);
        auto hit = hero.begin();
        for (size_t i = 0; i < hsiz; i++)
                hands[i] = *hit++;
        auto vit = vill.begin();
        for (size_t i = 0; i < vsiz; i++)
                hands[hsiz+i] = *vit++;

        // Compute equity.
        for (size_t i = 0; i < hsiz; i++)
                if (hands[i].disjoint(board))
                        for (size_t j = 0; j < vsiz; j++)
                                if (hands[hsiz+j].disjoint(board) &&
                                    hands[hsiz+j].disjoint(hands[i])) {
                                        size_t idx = i*vsiz+j;
                                        he = E->evaluateHand(
                                                hands[i], board).high();
                                        ve = E->evaluateHand(
                                                hands[hsiz+j], board).high();
                                        if (he == ve)
                                                equity[idx] = 0.5;
                                        else if (he > ve)
                                                equity[idx] = 1;
                                        else
                                                equity[idx] = 0;
                                }
        return 0;
}
