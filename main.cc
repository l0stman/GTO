#include "range.h"
#include "equi_dist.h"

#include <cstdio>
#include <cstdlib>
#include <utility>
#include <vector>

#include <pokerstove/peval/Card.h>
#include <pokerstove/peval/PokerHandEvaluator.h>

using GTO::Range;
using pokerstove::CardSet;
using pokerstove::PokerHandEvaluator;
using std::vector;

inline double
Rand()
{
        return static_cast<double>(rand())/RAND_MAX;
}

inline void
RandInit(vector<double>& V)
{
        for (size_t i = 0; i < V.size(); i++)
                V[i] = Rand() < 0.5 ? 0 : 1;
}

size_t
AddRange(const Range& r, const CardSet& board, vector<CardSet>& hands)
{
        size_t s = 0;

        for (auto it = r.begin(); it != r.end(); it++)
                if (board.disjoint(*it)) {
                        s++;
                        hands.push_back(*it);
                }
        return s;
}

void
ComputeEquity(const vector<CardSet>& hands,
              const size_t& hsiz,
              const Range& hero,
              const Range& vill,
              const CardSet& board,
              vector<double>& equity)
{
        GTO::EquiDist eq(hero, vill, board);
        size_t vsiz = hands.size()-hsiz;

        for (size_t i = 0; i < hsiz; i++)
                for (size_t j = 0; j < vsiz; j++) {
                        size_t idx = i*vsiz+j;
                        equity[idx] = eq.Equity(hands[i], hands[hsiz+j]);
                }
}

// Return the EV for hero when he bets a hand.
double
EVHeroBets(const size_t& h,
           const vector<double>& equity,
           const vector<double>& call_prob,
           const double& stack,
           const double& pot,
           const double& bet)
{
        size_t vsiz = call_prob.size();
        double EV = 0;
        size_t N = 0;

        for (size_t v = 0; v < vsiz; v++) {
                size_t idx = h*vsiz+v;
                if (equity[idx] >= 0) {
                        N++;
                        EV += (1-call_prob[v])*(pot+bet) +
                                call_prob[v]*equity[idx]*(pot+2*bet);
                }
        }
        return N > 0 ? stack-bet + EV/N : -1;
}

// Return the EV for hero's when he and villain play the given
// strategies.
double
EVHero(const vector<double>& equity,
       const vector<double>& bet_prob,
       const vector<double>& call_prob,
       const double& stack,
       const double& pot,
       const double& bet)
{
        size_t hsiz = bet_prob.size();
        size_t vsiz = call_prob.size();
        double EV = 0;
        size_t N = 0;

        for (size_t h = 0; h < hsiz; h++)
                for (size_t v = 0; v < vsiz; v++) {
                        size_t idx = h*vsiz+v;
                        if (equity[idx] == -1)
                                continue;
                        N++;
                        EV += bet_prob[h]*(
                                stack-bet + (1-call_prob[v])*(pot+bet) +
                                call_prob[v]*equity[idx]*(pot+2*bet));
                        EV += (1-bet_prob[h])*stack;
                }
        return N > 0 ? EV/N : -1;
}

// Return the EV for villain when he calls hero's bet with a
// particular hand.
double
EVVillCalls(const size_t& v,
            const vector<double>& equity,
            const vector<double>& bet_prob,
            const double& stack,
            const double& pot,
            const double& bet)
{
        size_t hsiz = bet_prob.size();
        size_t vsiz = equity.size()/hsiz;
        double EV = 0;
        double N = 0;

        for (size_t h = 0; h < hsiz; h++) {
                size_t idx = h*vsiz+v;
                if (equity[idx] >= 0) {
                        N += bet_prob[h];
                        EV += bet_prob[h]*
                                (stack-bet+(1-equity[idx])*(pot + 2*bet));
                }
        }
        return N > 0 ? EV/N : -1;
}

void
UpdateHeroStrategy(const vector<double>& equity,
                   const size_t& nsamples,
                   const vector<double>& call_prob,
                   const double& stack,
                   const double& pot,
                   const double& bet,
                   vector<double>& bet_prob)
{
        size_t hsiz = bet_prob.size();

        for (size_t h = 0; h < hsiz; h++) {
                double EV_bets = EVHeroBets(h, equity, call_prob, stack, pot,
                                            bet);
                double d = 0;
                if (stack < EV_bets ||
                    (stack == EV_bets && Rand() < 0.5))
                        d = 1;
                bet_prob[h] = ((nsamples-1)*bet_prob[h] + d)/nsamples;
        }
}

void
UpdateVillStrategy(const vector<double>& equity,
                   const size_t& nsamples,
                   const vector<double>& bet_prob,
                   const double& stack,
                   const double& pot,
                   const double& bet,
                   vector<double>& call_prob)
{
        for (size_t v = 0; v < call_prob.size(); v++) {
                double EV = EVVillCalls(v, equity, bet_prob, stack, pot, bet);
                double d = 0;

                if (stack < EV || (stack == EV && Rand() < 0.5))
                        d = 1;
                call_prob[v] = ((nsamples-1)*call_prob[v] + d)/nsamples;
        }
}

int
main(int argc, char *argv[])
{
        Range hero("AcJc,KcJc,QcJc,JcTc,5c4c,KcJh,QcJh,KdJc,QdJc,5d4d,KdJh,QdJh,KhJc,QhJc,Ah2h,Ah5h,Ah8h,AhJh,KhJh,QhJh,JhTh,AsTc,KsJc,QsJc,AsTd,AsTh,KsJh,QsJh,Qs6s-QsTs,Ts7s,9s7s,8s7s,3d3c,3h3c,3h3d,6s6c,6d6c,6d6s,AsAc,AdAc,AhAc");
        Range vill("44,55,77-JJ,A3s,A6s,K6s,KJs,Q6s,QJs,J2s+,62s-64s,A6o,K6o,Q6o,J4o+,T6o,96o,86o,76o,65o,Ts4s+,9s4s+,8s4s+,7s6s,6s5s,3d3c,6d6c,QsQc,QdQc,QhQc,KsKc,KdKc,KhKc,AsAc,AdAc,AhAc");
        CardSet board("Js6h3sJd2d");
        vector<CardSet> hands;
        size_t hsiz = AddRange(hero, board, hands);
        size_t vsiz = AddRange(vill, board, hands);
        vector<double> equity(hsiz*vsiz, -1);
        vector<double> bet_prob(hsiz, 0);
        vector<double> call_prob(vsiz, 0);
        const double pot = 53;
        const double stack = 48.5;

        fprintf(stderr, "Hero: %d hands, Villain: %d hands.\n", hsiz, vsiz);
        ComputeEquity(hands, hsiz, hero, vill, board, equity);

        srand(time(0));
        RandInit(bet_prob);
        RandInit(call_prob);
        for (size_t nsamples = 2; nsamples < 50000; nsamples++) {
                UpdateVillStrategy(equity, nsamples, bet_prob, stack, pot, stack, call_prob);
                UpdateHeroStrategy(equity, nsamples, call_prob, stack, pot, stack, bet_prob);
        }
        double EV = EVHero(equity, bet_prob, call_prob, stack, pot, stack);
        fprintf(stderr, "Hero EV = %.4f\n", EV);
        fprintf(stderr, "Hero:\n");
        for (size_t h = 0; h < hsiz; h++)
                if (bet_prob[h] >= 0.05) {
                fprintf(stderr, "%s: %.4f, EV = %.4f\n", hands[h].str().c_str(), bet_prob[h], bet_prob[h]*EVHeroBets(h, equity, call_prob, stack, pot, stack)+(1-bet_prob[h])*stack);
        }
        fprintf(stderr, "Villain:\n");
        for (size_t v = 0; v < vsiz; v++)
                if (call_prob[v] >= 0.05) {
                fprintf(stderr, "%s: %.4f, EV = %.4f\n", hands[hsiz+v].str().c_str(), call_prob[v], call_prob[v]*EVVillCalls(v, equity, bet_prob, stack, pot, stack)+(1-call_prob[v])*stack);
        }
        return 0;
}
