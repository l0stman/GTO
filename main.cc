#include <algorithm>
#include <cstdio>
#include <cassert>
#include <cstdlib>
#include <vector>

#include "range.h"
#include "equi_dist.h"

namespace {
using std::vector;
using pokerstove::CardSet;
using std::string;

void
AddRange(const GTO::Range& r, const CardSet& board, vector<CardSet>& hands)
{
        for (auto it = r.begin(); it != r.end(); it++)
                if (board.disjoint(*it))
                        hands.push_back(*it);
}

class EquiLUT {
public:
        explicit EquiLUT(const vector<CardSet>& hero,
                         const vector<CardSet>& vill,
                         const GTO::EquiDist& ED)
                : hsize_(hero.size()), vsize_(vill.size())
        {
                equity_.reserve(hsize_*vsize_);
                for (size_t i = 0; i < hsize_; ++i)
                        for (size_t j = 0; j < vsize_; ++j) {
                                size_t idx = i*vsize_+j;
                                double EQ = ED.Equity(hero[i], vill[j]);
                                if (EQ == -1) {
                                        EQ = ED.Equity(vill[j], hero[i]);
                                        if (EQ >= 0)
                                                EQ = 1-EQ;
                                }
                                equity_[idx] = EQ;
                        }
        }

        size_t NumRows() const { return hsize_; }
        size_t NumCols() const { return vsize_; }

        double Val(size_t hero, size_t vill) const
        {
                assert(hero < hsize_ && vill < vsize_);
                return equity_[hero*vsize_+vill];
        }

private:
        const double hsize_;
        const double vsize_;
        vector<double> equity_;
};

enum ActionType {
        VILL_OPEN_FOLD = 0,
        VILL_RAISE_FOLD,
        VILL_4BET_FOLD,
        VILL_4BET_CALL,
        HERO_FOLD = 0,
        HERO_FLAT_CALL,
        HERO_3BET_FOLD,
        HERO_5BET,
};

class Strategy;

class Action {
public:
        virtual string Name() const = 0;
        virtual double EV(const size_t& hand,
                          const Strategy& opponent,
                          const EquiLUT& equity) const = 0;
};

class Strategy {
public:
        explicit Strategy(const vector<Action *>& hero_actions,
                          const vector<CardSet>& hero_hands,
                          const vector<CardSet>& vill_hands,
                          const GTO::EquiDist& ED)
                : nhands_(hero_hands.size()),
                  nactions_(hero_actions.size()),
                  nsamples_(nactions_),
                  actions_(vector<Action *>(hero_actions)),
                  equity_(EquiLUT(hero_hands, vill_hands, ED))
        {
                assert(nactions_ > 0 && nhands_ > 0);
                probs_.assign(nhands_*nactions_,
                              1/static_cast<double>(nactions_));
                ignore_masks_.assign(nhands_*nactions_, 0);
        }

        double Prob(const size_t& hand, const ActionType& type) const
        {
                assert(hand < nhands_ && type < nactions_);
                return probs_[hand*nactions_+type];
        }

        void IgnoreAction(const size_t& hand, const ActionType& type)
        {
                assert(hand < nhands_ && type < nactions_);
                ignore_masks_[hand*nactions_+type] |= 1 << type;
        }

        void Update(const Strategy& opponent)
        {
                vector<size_t> best(nactions_);

                srand(time(0));
                for (size_t h = 0; h < nhands_; ++h) {
                        double bestEV = -1;
                        double nties = 0;
                        for (size_t a = 0; a < nactions_; ++a) {
                                if (ignore_masks_[h*nactions_+a] & (1<<a))
                                        continue;
                                double EV = actions_[a]->EV(h,opponent,equity_);
                                if (EV == bestEV)
                                        best[++nties] = a;
                                else if (EV > bestEV) {
                                        bestEV = EV;
                                        nties = 0;
                                        best[0] = a;
                                }
                        }
                        Inc(h, RandAction(best, nties+1));
                }
                ++nsamples_;
        }

        const EquiLUT& LUT() const
        {
                return equity_;
        }

private:
        inline ActionType
        RandAction(const vector<size_t> A, const size_t& size)
        {
                size_t i = static_cast<size_t>(
                        static_cast<double>(rand())/RAND_MAX*size);
                return static_cast<ActionType>(A[i]);
        }

        void Inc(const size_t& hand, const ActionType& type)
        {
                assert(hand < nhands_ && type < nactions_);
                for (size_t T = 0; T<nactions_; T++) {
                        double d = (static_cast<ActionType>(T) == type) ? 1 : 0;
                        size_t idx = hand*nactions_+T;
                        probs_[idx] = (nsamples_*probs_[idx]+d)/(nsamples_+1);
                }
        }

        const size_t nhands_;
        const size_t nactions_;
        size_t nsamples_;
        const vector<Action *> actions_;
        const EquiLUT equity_;
        vector<double> probs_;
        vector<char> ignore_masks_;
};

class VillOpenFold : public Action {
public:
        VillOpenFold(const string& name, const double& stack)
                : name_(name), stack_(stack)
        {}

        string Name() const { return name_; }

        double EV(const size_t& hand,
                  const Strategy& hero,
                  const EquiLUT& equity) const
        {
                return stack_;
        }
private:
        const string name_;
        const double stack_;
};

class VillRaiseFold : public Action {
public:
        VillRaiseFold(const string& name,
                      const double& stack,
                      const double& blinds,
                      const double& raise)
                : name_(name),
                  stack_(stack),
                  blinds_(blinds),
                  raise_(raise)
        {}

        string Name() const { return name_; }

        double EV(const size_t& hand,
                  const Strategy& hero,
                  const EquiLUT& equity) const
        {
                double EV = 0;
                size_t N = 0;

                for (size_t h = 0; h < equity.NumCols(); ++h) {
                        double EQ = equity.Val(hand, h);
                        if (EQ == -1)
                                continue;
                        ++N;
                        double pot = blinds_+2*raise_;
                        double bet = 2*pot/3;
                        EV += hero.Prob(h, HERO_FOLD)*(stack_+blinds_) +
                                hero.Prob(h, HERO_FLAT_CALL)*(
                                        stack_-raise_-bet+EQ*(pot+2*bet)) +
                                (hero.Prob(h, HERO_3BET_FOLD) +
                                 hero.Prob(h, HERO_5BET))*(stack_-raise_);
                }
                return N > 0 ? EV/N : -1;
        }
private:
        const string name_;
        const double stack_;
        const double blinds_;
        const double raise_;
};

class Vill4betFold : public Action {
public:
        Vill4betFold(const string& name,
                     const double& stack,
                     const double& blinds,
                     const double& raise,
                     const double& three_bet,
                     const double& four_bet)
                : name_(name),
                  stack_(stack),
                  blinds_(blinds),
                  raise_(raise),
                  three_bet_(three_bet),
                  four_bet_(four_bet)
        {}

        string Name() const { return name_; }

        double EV(const size_t& hand,
                  const Strategy& hero,
                  const EquiLUT& equity) const
        {
                double EV = 0;
                size_t N = 0;

                for (size_t h = 0; h < equity.NumCols(); ++h) {
                        double EQ = equity.Val(hand, h);
                        if (EQ == -1)
                                continue;
                        ++N;
                        double pot = blinds_+2*raise_;
                        double bet = 2*pot/3;
                        EV += hero.Prob(h, HERO_FOLD)*(stack_+blinds_) +
                                hero.Prob(h, HERO_FLAT_CALL) *
                                (stack_-raise_-bet+EQ*(pot+2*bet)) +
                                hero.Prob(h, HERO_3BET_FOLD) *
                                (stack_+blinds_+three_bet_) +
                                hero.Prob(h, HERO_5BET)*(stack_-four_bet_);
                }
                return N > 0 ? EV/N : -1;
        }
private:
        const string name_;
        const double stack_;
        const double blinds_;
        const double raise_;
        const double three_bet_;
        const double four_bet_;
};

class Vill4betCall : public Action {
public:
        Vill4betCall(const string& name,
                     const double& stack,
                     const double& blinds,
                     const double& raise,
                     const double& three_bet)
                : name_(name),
                  stack_(stack),
                  blinds_(blinds),
                  raise_(raise),
                  three_bet_(three_bet)
        {}

        string Name() const { return name_; }

        double EV(const size_t& hand,
                  const Strategy& hero,
                  const EquiLUT& equity) const
        {
                double EV = 0;
                size_t N = 0;

                for (size_t h = 0; h < equity.NumCols(); ++h) {
                        double EQ = equity.Val(hand, h);
                        if (EQ == -1)
                                continue;
                        ++N;
                        double pot = blinds_+2*raise_;
                        double bet = 2*pot/3;
                        EV += hero.Prob(h, HERO_FOLD)*(stack_+blinds_) +
                                hero.Prob(h, HERO_FLAT_CALL) *
                                (stack_-raise_-bet+EQ*(pot+2*bet)) +
                                hero.Prob(h, HERO_3BET_FOLD) *
                                (stack_ + blinds_ + three_bet_) +
                                hero.Prob(h, HERO_5BET)*EQ*(blinds_ + 2*stack_);
                }
                return N > 0 ? EV/N : -1;
        }
private:
        const string name_;
        const double stack_;
        const double blinds_;
        const double raise_;
        const double three_bet_;
};

class HeroFold : public Action {
public:
        HeroFold(const string& name,
                 const double& stack)
                : name_(name),
                  stack_(stack)
        {}

        string Name() const { return name_; }

        double EV(const size_t& hand,
                  const Strategy& vill,
                  const EquiLUT& equity) const
        {
                return stack_;
        }
private:
        const string name_;
        const double stack_;
};

class HeroFlatCall : public Action {
public:
        HeroFlatCall(const string& name,
                     const double& stack,
                     const double& blinds,
                     const double& raise)
                : name_(name),
                  stack_(stack),
                  blinds_(blinds),
                  raise_(raise)
        {}

        string Name() const { return name_; }

        double EV(const size_t& hand,
                  const Strategy& vill,
                  const EquiLUT& equity) const
        {
                double EV = 0;
                size_t N = 0;

                for (size_t v = 0; v < equity.NumCols(); ++v) {
                        double EQ = equity.Val(hand, v);
                        if (EQ == -1)
                                continue;
                        double pot = blinds_+2*raise_;
                        double bet = 2*pot/3;
                        double p = vill.Prob(v, VILL_OPEN_FOLD);
                        ++N;
                        EV += p*(stack_+blinds_) +
                                (1-p)*(stack_-raise_-bet + EQ*(pot+2*bet));
                }
                return N > 0 ? EV/N : -1;
        }
private:
        const string name_;
        const double stack_;
        const double blinds_;
        const double raise_;
};

class Hero3betFold : public Action {
public:
        Hero3betFold(const string& name,
                     const double& stack,
                     const double& blinds,
                     const double& raise,
                     const double& three_bet)
                : name_(name),
                  stack_(stack),
                  blinds_(blinds),
                  raise_(raise),
                  three_bet_(three_bet)
        {}

        string Name() const { return name_; }

        double EV(const size_t& hand,
                  const Strategy& vill,
                  const EquiLUT& equity) const
        {
                double EV = 0;
                size_t N = 0;

                for (size_t v = 0; v < equity.NumCols(); ++v) {
                        if (equity.Val(hand, v) == -1)
                                continue;
                        ++N;
                        EV += vill.Prob(v, VILL_OPEN_FOLD)*(stack_+blinds_) +
                                vill.Prob(v, VILL_RAISE_FOLD) *
                                (stack_ + blinds_ + raise_) +
                                (vill.Prob(v, VILL_4BET_FOLD) +
                                 vill.Prob(v, VILL_4BET_CALL)) *
                                (stack_ - three_bet_);
                }
                return N > 0 ? EV/N : -1;
        }
private:
        const string name_;
        const double stack_;
        const double blinds_;
        const double raise_;
        const double three_bet_;
};

class Hero5bet : public Action {
public:
        Hero5bet(const string& name,
                 const double& stack,
                 const double& blinds,
                 const double& raise,
                 const double& four_bet)
                : name_(name),
                  stack_(stack),
                  blinds_(blinds),
                  raise_(raise),
                  four_bet_(four_bet)
        {}

        string Name() const { return name_; }

        double EV(const size_t& hand,
                  const Strategy& vill,
                  const EquiLUT& equity) const
        {
                double EV = 0;
                size_t N = 0;

                for (size_t v = 0; v < equity.NumCols(); ++v) {
                        double EQh = equity.Val(hand, v);
                        if (EQh == -1)
                                continue;
                        ++N;
                        EV += vill.Prob(v, VILL_OPEN_FOLD)*(stack_+blinds_) +
                                vill.Prob(v, VILL_RAISE_FOLD) *
                                (stack_ + blinds_ + raise_) +
                                vill.Prob(v, VILL_4BET_FOLD) *
                                (stack_ + blinds_ + four_bet_) +
                                vill.Prob(v, VILL_4BET_CALL) *
                                EQh*(blinds_ + 2*stack_);
                }
                return N > 0 ? EV/N : -1;
        }
private:
        const string name_;
        const double stack_;
        const double blinds_;
        const double raise_;
        const double four_bet_;
};

double
HeroEV(const Strategy& hero,
       const Strategy& vill,
       const double& stack,
       const double& blinds,
       const double& raise,
       const double& three_bet,
       const double& four_bet)
{
        double EV = 0;
        double N = 0;
        const EquiLUT& equity = hero.LUT();

        for (size_t h = 0; h < equity.NumRows(); ++h)
                for (size_t v = 0; v < equity.NumCols(); ++v) {
                        double EQ = equity.Val(h, v);
                        if (EQ == -1)
                                continue;
                        ++N;
                        double pot = blinds+2*raise;
                        double bet = 2*pot/3;
                        EV += hero.Prob(h, HERO_FOLD)*stack;
                        EV += hero.Prob(h, HERO_FLAT_CALL)*(
                                vill.Prob(v, VILL_OPEN_FOLD)*(stack+blinds) +
                                (1-vill.Prob(v, VILL_OPEN_FOLD))*(
                                        stack-raise-bet+EQ*(pot+2*bet)));
                        EV += hero.Prob(h, HERO_3BET_FOLD)*(
                                vill.Prob(v, VILL_OPEN_FOLD)*(stack+blinds) +
                                vill.Prob(v, VILL_RAISE_FOLD)*(
                                        stack+blinds+raise) +
                                (vill.Prob(v, VILL_4BET_FOLD) +
                                 vill.Prob(v, VILL_4BET_CALL))*(
                                         stack-three_bet));
                        EV += hero.Prob(h, HERO_5BET)*(
                                vill.Prob(v, VILL_OPEN_FOLD)*(stack+blinds) +
                                vill.Prob(v, VILL_RAISE_FOLD)*(
                                        stack+blinds+raise) +
                                vill.Prob(v, VILL_4BET_FOLD)*(
                                        stack+blinds+four_bet) +
                                vill.Prob(v, VILL_4BET_CALL)*EQ*(
                                        blinds+2*stack));
                }
        return N > 0 ? EV/N : -1;
}

struct Record {
        size_t hand;
        double prob;
        double EV;

        Record(const size_t& hand,
               const double& prob,
               const double& EV)
                : hand(hand), prob(prob), EV(EV)
        {}

        bool operator<(const Record& rhs) const
        {
                return rhs.prob < prob ||
                                  (rhs.prob == prob && rhs.EV < EV);
        }
};

void
PrintResults(const vector<CardSet>& hands,
             const Strategy& hero,
             const Strategy& vill,
             const vector<Action *>& actions)
{
        size_t size = hands.size();
        vector<Record> R;

        for (size_t a = 0; a < actions.size(); ++a) {
                size_t N = 0;
                double total = 0;
                ActionType T = static_cast<ActionType>(a);
                R.clear();
                R.reserve(size);
                for (size_t h = 0; h < size; ++h) {
                        double p = hero.Prob(h, T);
                        if (p < 0.05)
                                continue;
                        ++N;
                        total += p;
                        R.push_back(
                                Record(h,p,actions[T]->EV(h,vill,hero.LUT())));
                }
                printf("%s range: %.2f hand%c\n", actions[a]->Name().c_str(),
                       total, total > 1 ? 's' : ' ');
                printf("Hand\tProb\tEV\n");
                sort(R.begin(), R.begin()+N);
                for (size_t i = 0; i < N; ++i)
                        printf("%s\t%.4f\t%.4f\n",
                               hands[R[i].hand].str().c_str(),
                               R[i].prob,
                               R[i].EV);
                printf("\n");
        }
}
}

using GTO::Range;
using pokerstove::CardSet;

int
main(int argc, char *argv[])
{
        Range hero("22+,A2s+,K6s+,Q8s+,J8s+,T7s+,96s+,85s+,74s+,63s+,52s+,42s+,32s,A8o+,K9o+,QTo+,JTo");
        Range vill("77+,A7s+,K9s+,QTs+,JTs,ATo+,KTo+,QJo");
        CardSet board;
        vector<CardSet> hhands;
        vector<CardSet> vhands;
        double stack = 100;
        double blinds = 1.5;
        double raise = 3;
        double three_bet = 9;
        double four_bet = 27;

//        hero.Fill();
//        vill.Fill();
        AddRange(hero, board, hhands);
        AddRange(vill, board, vhands);
        GTO::EquiDist ED(hero, vill, board);
        vector<Action *> vactions(4);
        vector<Action *> hactions(4);

        vactions[VILL_OPEN_FOLD] = new VillOpenFold("Open fold", stack);
        vactions[VILL_RAISE_FOLD] = new VillRaiseFold(
                "Open raise bluff", stack, blinds, raise);
        vactions[VILL_4BET_FOLD] = new Vill4betFold(
                "4-bet bluff", stack, blinds, raise, three_bet, four_bet);
        vactions[VILL_4BET_CALL] = new Vill4betCall(
                "4-bet call", stack, blinds, raise, three_bet);
        hactions[HERO_FOLD] = new HeroFold("Fold", stack);
        hactions[HERO_FLAT_CALL] = new HeroFlatCall(
                "Flat call", stack, blinds, raise);
        hactions[HERO_3BET_FOLD] = new Hero3betFold(
                "3-bet bluff", stack, blinds, raise, three_bet);
        hactions[HERO_5BET] = new Hero5bet(
                "5-bet", stack, blinds, raise, four_bet);

        Strategy hstrategy(hactions, hhands, vhands, ED);
        Strategy vstrategy(vactions, vhands, hhands, ED);

        double EV = 0;
        for (size_t i = 0; i < 10000; ++i) {
                hstrategy.Update(vstrategy);
                vstrategy.Update(hstrategy);
                if (i % 100 == 0) {
                        EV = HeroEV(hstrategy,
                                    vstrategy,
                                    stack,
                                    blinds,
                                    raise,
                                    three_bet,
                                    four_bet);
                        fprintf(stderr, "Iteration %d: EV(EP) = %.6f, \
EV(BTN) = %.6f\n", i, blinds+2*stack-EV, EV);
                }
        }
        printf("UTG: EV = %.4f\n", blinds+2*stack-EV);
        PrintResults(vhands, vstrategy, hstrategy, vactions);
        printf("\nBTN: EV = %.4f\n", EV);
        PrintResults(hhands, hstrategy, vstrategy, hactions);

        return 0;
}
