// This is the implementation of 5-bet shove game that takes place in
// pre-flop.  The UTG player open raise.  We suppose that it's folded
// to the BTN that could fold, flat calls or 3-bet.  We assume that
// the blinds always get out of the way.  If he flat calls, he's going
// to call another bet equal to 2/3 of the pot on the flop.  If he
// 3-bets, the UTG player could fold or 4-bet.  The button could then
// decide to fold or move all-in.  And UTG could either fold or call
// the shove.  UTG's opening range and the various bet size should be
// supplied and a GTO strategy is computed for each player.
// Usage
// $ 5bet -h
// $ 5bet -f -N <5bet_example.txt

#include <libgen.h>
#include <limits.h>
#include <unistd.h>

#include <cstdio>
#include <cstdlib>
#include <cstring>

#include <random>

#include "cfr-inl.h"
#include "dealer_interface.h"
#include "err.h"
#include "input.h"
#include "preflop_equi_dist.h"

namespace {
using std::vector;
using std::string;

struct GameInfo {
        const double stack;
        const double blinds;
        const double raise;
        const double three_bet;
        const double four_bet;
        const vector<GTO::PreflopHand> vill_hands;
        const vector<GTO::PreflopHand> hero_hands;
        const GTO::Array<double> equity;
        const GTO::Array<unsigned short> suit_combos;

        explicit GameInfo(double stack,
                          double blinds,
                          double raise,
                          double three_bet,
                          double four_bet,
                          const GTO::PreflopRange& vill,
                          const GTO::PreflopRange& hero)
                : stack(stack),
                  blinds(blinds),
                  raise(raise),
                  three_bet(three_bet),
                  four_bet(four_bet),
                  vill_hands(vill.ToVector()),
                  hero_hands(hero.ToVector()),
                  equity(GTO::PreflopEquiDist().LUT(vill_hands, hero_hands)),
                  suit_combos(GTO::SuitCombos().LUT(vill_hands, hero_hands))
        {}
};

class VillFourBetFold : public GTO::Leaf {
public:
        explicit VillFourBetFold(const string& name, const GameInfo& info)
                : GTO::Leaf(name), info_(info)
        {}

        virtual double
        Utility(Player player, size_t pid, size_t oid) const
        {
                double EV = 0;

                switch (player) {
                case GTO::Node::VILLAIN:
                        EV = info_.stack - info_.four_bet;
                        break;
                case GTO::Node::HERO:
                        EV = info_.stack + info_.blinds + info_.four_bet;
                        break;
                default:
                        GTO::UtilError(player, name());
                        break;
                }
                return EV;
        }
private:
        const GameInfo& info_;
};

class VillFourBetCall : public GTO::Leaf {
public:
        explicit VillFourBetCall(const string& name, const GameInfo& info)
                : GTO::Leaf(name), info_(info)
        {}

        virtual double
        Utility(Player player, size_t pid, size_t oid) const
        {
                double EQ = 0;

                switch (player) {
                case GTO::Node::HERO:
                        EQ = info_.equity.get(oid, pid);
                        assert(EQ >= 0);
                        EQ = 1-EQ;
                        break;
                case GTO::Node::VILLAIN:
                        EQ = info_.equity.get(pid, oid);
                        assert(EQ >= 0);
                        break;
                default:
                        GTO::UtilError(player, name());
                        break;
                }

                return EQ*(2*info_.stack + info_.blinds);
        }
private:
        const GameInfo& info_;
};

class HeroThreeBetFold : public GTO::Leaf {
public:
        explicit HeroThreeBetFold(const string& name, const GameInfo& info)
                : GTO::Leaf(name), info_(info)
        {}

        virtual double
        Utility(Player player, size_t pid, size_t oid) const
        {
                double EV = 0;

                switch (player) {
                case GTO::Node::VILLAIN:
                        EV = info_.stack + info_.blinds + info_.three_bet;
                        break;
                case GTO::Node::HERO:
                        EV = info_.stack - info_.three_bet;
                        break;
                default:
                        GTO::UtilError(player, name());
                        break;
                }
                return EV;
        }
private:
        const GameInfo& info_;
};

class VillRaiseFold : public GTO::Leaf {
public:
        explicit VillRaiseFold(const string& name, const GameInfo& info)
                : GTO::Leaf(name), info_(info)
        {}

        virtual double
        Utility(Player player, size_t pid, size_t oid) const
        {
                double EV = 0;

                switch (player) {
                case GTO::Node::VILLAIN:
                        EV = info_.stack - info_.raise;
                        break;
                case GTO::Node::HERO:
                        EV = info_.stack + info_.blinds + info_.raise;
                        break;
                default:
                        GTO::UtilError(player, name());
                        break;
                }
                return EV;
        }
private:
        const GameInfo& info_;
};

class HeroFlatCall : public GTO::Leaf {
public:
        explicit HeroFlatCall(const string& name, const GameInfo& info)
                : GTO::Leaf(name), info_(info)
        {}

        virtual double
        Utility(Player player, size_t pid, size_t oid) const
        {
                double flop_pot = info_.blinds + 2*info_.raise;
                double flop_bet = 2.0/3*flop_pot;
                double EQ = 0;

                switch (player) {
                case GTO::Node::HERO:
                        EQ = info_.equity.get(oid, pid);
                        assert(EQ >= 0);
                        EQ = 1-EQ;
                        break;
                case GTO::Node::VILLAIN:
                        EQ = info_.equity.get(pid, oid);
                        assert(EQ >= 0);
                        break;
                default:
                        GTO::UtilError(player, name());
                        break;
                }

                return info_.stack-info_.raise-flop_bet +
                        EQ*(flop_pot + 2*flop_bet);
        }

private:
        const GameInfo& info_;
};

class HeroFold : public GTO::Leaf {
public:
        explicit HeroFold(const string& name, const GameInfo& info)
                : GTO::Leaf(name), info_(info)
        {}

        virtual double
        Utility(Player player, size_t pid, size_t oid) const
        {
                double EV = 0;

                switch (player) {
                case GTO::Node::VILLAIN:
                        EV = info_.stack + info_.blinds;
                        break;
                case GTO::Node::HERO:
                        EV = info_.stack;
                        break;
                default:
                        GTO::UtilError(player, name());
                        break;
                }
                return EV;
        }

private:
        const GameInfo& info_;
};

std::mt19937 generator((std::random_device())());
std::uniform_real_distribution<double> prob_dist(0, 1.0);

class Dealer : public GTO::DealerInterface {
public:
        explicit Dealer(const GameInfo& info)
                : vill_hands_(info.vill_hands),
                  suit_combos_(info.suit_combos),
                  hero_weights_(InitWeights(info.hero_hands)),
                  vill_weights_(InitWeights(info.vill_hands))
        {}
        ~Dealer() {}

        virtual void
        Deal(size_t& hero_id, size_t& vill_id)
        {
                for (;;) {
                        hero_id = Sample(hero_weights_);
                        vill_id = Sample(vill_weights_);
                        unsigned short m = suit_combos_.get(vill_id, hero_id);
                        unsigned short s = vill_hands_[vill_id].suit_combos();
                        if (m == s)
                                return; // there is no possible conflict
                        // We choose a random hand from those
                        // represented by vill_hands_[vill_id].  If
                        // it's one of the possible match-ups, then
                        // we're good.
                        std::uniform_int_distribution<unsigned short> d(0, s-1);
                        if (d(generator) < m)
                                return;
                }
        }
private:
        vector<double>
        InitWeights(const vector<GTO::PreflopHand>& hands)
        {
                vector<double> W(hands.size(), 0.0);
                double total = 0;

                for (size_t i = 0; i < W.size(); i++) {
                        total += hands[i].suit_combos();
                        W[i] = total;
                }
                if (total > 0)
                        for (size_t i = 0; i < W.size(); i++)
                                W[i] /= total;
                return W;
        }

        size_t
        Sample(const vector<double>& weights) const
        {
                double r = prob_dist(generator);
                int min = 0;
                int max = weights.size()-1;
                while (min <= max) {
                        size_t mid = min + ((max-min) >> 1);
                        if (r < weights[mid])
                                max = mid-1;
                        else if (r < weights[mid+1])
                                return mid+1;
                        else
                                min = mid+1;
                }
                return min;
        }

        const vector<GTO::PreflopHand>& vill_hands_;
        const GTO::Array<unsigned short>& suit_combos_;
        const vector<double> hero_weights_;
        const vector<double> vill_weights_;
};

void
Usage()
{
        fprintf(stderr, "usage: %s [-h] [-f] [-N] [-n iter]\n", err::progname);
        fprintf(stderr, "  -h\t\t-- print this help message\n");
        fprintf(stderr, "  -f\t\t-- allow the BTN to have a flat calling range\
\n");
        fprintf(stderr, "  -N\t\t-- non-interactive mode, don't prompt the user\
for parameters\n");
        fprintf(stderr, "  -n niter\t-- number of iterations for the simulation\
\n");
        exit(EXIT_FAILURE);
}

size_t num_iter = 100000000;
bool fflag = false;
bool iflag = true;

} // namespace

int
main(int argc, char *argv[])
{
        int ch;
        double stack, raise, three_bet, four_bet;

        err::progname = strdup(basename(argv[0]));
        while ((ch = getopt(argc, argv, "hfNn:")) != -1) {
                switch (ch) {
                case 'n':
                        num_iter = strtol(optarg, NULL, 10);
                        break;
                case 'N':
                        iflag = false;
                        break;
                case 'f':
                        fflag = true;
                        break;
                case 'h':
                        // fall through
                default:
                        Usage();
                        break;
                }
        }
        if (iflag)
                fprintf(stderr, "Enter the starting stack size: ");
        input::ScanfOrDie("%lf", &stack);
        if (iflag)
                fprintf(stderr, "Enter the initial raise size: ");
        input::ScanfOrDie("%lf", &raise);
        if (iflag)
                fprintf(stderr, "Enter the 3-bet size: ");
        input::ScanfOrDie("%lf", &three_bet);
        if (iflag)
                fprintf(stderr, "Enter the 4-bet size: ");
        input::ScanfOrDie("%lf", &four_bet);
        if (iflag)
                fprintf(stderr, "Enter UTG's opening range: ");
        GTO::PreflopRange vill(input::ReadLineOrDie());
        GTO::PreflopRange hero;
        hero.Fill();
        GameInfo info(stack, 1.5, raise, three_bet, four_bet, vill, hero);
        size_t vsize = info.vill_hands.size();
        size_t hsize = info.hero_hands.size();
        vector<GTO::Node *> five_bet_children = {
                new VillFourBetFold("4-bet bluffing", info),
                new VillFourBetCall("4-bet calling", info)
        };
        vector<GTO::Node *> four_bet_children = {
                new HeroThreeBetFold("3-bet bluffing", info),
                new GTO::ParentNode("5-bet",
                                    GTO::Node::VILLAIN,
                                    vsize,
                                    five_bet_children)
        };
        vector<GTO::Node *> three_bet_children = {
                new VillRaiseFold("Open raise bluffing", info),
                new GTO::ParentNode("4-bet",
                                    GTO::Node::HERO,
                                    hsize,
                                    four_bet_children)
        };
        vector<GTO::Node *> root_children;
        if (fflag)
                root_children = {
                        new HeroFold("Folding", info),
                        new HeroFlatCall("Flat calling", info),
                        new GTO::ParentNode("3-bet",
                                            GTO::Node::VILLAIN,
                                            vsize,
                                            three_bet_children)
                };
        else
                root_children = {
                        new HeroFold("Folding", info),
                        new GTO::ParentNode("3-bet",
                                            GTO::Node::VILLAIN,
                                            vsize,
                                            three_bet_children)
                };
        GTO::ParentNode root("root", GTO::Node::HERO, hsize, root_children);
        Dealer dealer(info);
        printf("Stack\t: %.2f\nRaise\t: %.2f\n3-bet\t: %.2f\n4-bet\t: %.2f\n\n",
               stack, raise, three_bet, four_bet);
        GTO::Train(num_iter,
                   info.hero_hands,
                   info.vill_hands,
                   "BTN",
                   "UTG",
                   dealer,
                   root);
        free(const_cast<char *>(err::progname));
        return 0;
}
