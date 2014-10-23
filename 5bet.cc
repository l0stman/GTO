// This is an implementation of a 5-bet shove game that takes place in
// pre-flop.  The UTG player open-raises.  We suppose that it's folded
// to the BTN that could fold, flat call or 3-bet.  We assume that the
// blinds always get out of the way.  If he flat calls, he's going to
// call another bet equal to 2/3 of the pot on the flop.  If he
// 3-bets, the UTG player could flat-call, fold or 4-bet.  If UTG
// flat calls, then BTN will bet 2/3 of the pot on the flop too.
// Otherwise, the button could then decide to fold or move all-in.
// And UTG could either fold or call the shove.  UTG's opening range
// and the various bet size should be supplied and a GTO strategy is
// computed for each player.
// Usage:
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
#include "range.h"

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

class VillRaiseCall : public GTO::Leaf {
public:
        explicit VillRaiseCall(const string& name, const GameInfo& info)
                : GTO::Leaf(name), info_(info)
        {}

        virtual double
        Utility(Player player, size_t pid, size_t oid) const
        {
                double flop_pot = info_.blinds + 2*info_.three_bet;
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

                return info_.stack-info_.three_bet-flop_bet +
                        EQ*(flop_pot + 2*flop_bet);
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
        typedef std::uniform_int_distribution<size_t> UniDist;

        explicit Dealer(const GameInfo& info)
                : hero_hands_(InitHands(info.hero_hands)),
                  vill_hands_(InitHands(info.vill_hands)),
                  hero_ids_(InitIds(info.hero_hands)),
                  vill_ids_(InitIds(info.vill_hands)),
                  hero_dist_(UniDist(0, hero_hands_.size()-1)),
                  vill_dist_(UniDist(0, vill_hands_.size()-1))
        {}
        ~Dealer() {}

        virtual void
        Deal(size_t& hero_id, size_t& vill_id)
        {
                size_t h = 0;
                size_t v = 0;

                for (;;) {
                        h = hero_dist_(generator);
                        v = vill_dist_(generator);
                        if (hero_hands_[h].disjoint(vill_hands_[v])) {
                                hero_id = hero_ids_[h];
                                vill_id = vill_ids_[v];
                                return;
                        }
                }
        }

private:
        vector<GTO::Hand> InitHands(const vector<GTO::PreflopHand>& hands);
        vector<size_t> InitIds(const vector<GTO::PreflopHand>& hands);

        const vector<GTO::Hand> hero_hands_;
        const vector<GTO::Hand> vill_hands_;
        const vector<size_t> hero_ids_;
        const vector<size_t> vill_ids_;
        UniDist hero_dist_;
        UniDist vill_dist_;
};

vector<GTO::Hand>
Dealer::InitHands(const vector<GTO::PreflopHand>& preflop_hands)
{
        vector<GTO::Hand> hands;

        for (auto preflop_hand : preflop_hands)
                for (auto hand : GTO::Range(preflop_hand.ToString()))
                        hands.push_back(hand);
        return hands;
}

vector<size_t>
Dealer::InitIds(const vector<GTO::PreflopHand>& preflop_hands)
{
        vector<size_t> ids;
        size_t id = 0;

        for (auto preflop_hand : preflop_hands) {
                for (unsigned short i = 0; i < preflop_hand.suit_combos(); i++)
                        ids.push_back(id);
                id++;
        }
        return ids;
}

void
Usage()
{
        fprintf(stderr, "usage: %s [-h] [-f] [-N] [-n iter]\n", err::progname);
        fprintf(stderr, "  -h\t\t-- print this help message\n");
        fprintf(stderr, "  -f\t\t-- allow BTN to have a flat calling range \
facing a raise\n");
        fprintf(stderr, "  -F\t\t-- allow UTG to have a flat calling range \
facing a 3-bet\n");
        fprintf(stderr, "  -N\t\t-- non-interactive mode, don't prompt the user\
 for parameters\n");
        fprintf(stderr, "  -n niter\t-- number of iterations for the simulation\
\n");
        exit(EXIT_FAILURE);
}

} // namespace

int
main(int argc, char *argv[])
{
        int ch;
        double stack, raise, three_bet, four_bet;
        size_t num_iter = 100000000;
        bool fflag = false;     // with flat calling range for BTN?
        bool Fflag = false;     // with flat calling range for UTG?
        bool iflag = true;      // interactive mode?

        err::progname = strdup(basename(argv[0]));
        while ((ch = getopt(argc, argv, "hfFNn:")) != -1) {
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
                case 'F':
                        Fflag = true;
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
        vector<GTO::Node *> three_bet_children;
        if (Fflag)
                three_bet_children = {
                        new VillRaiseFold("Open raise bluffing", info),
                        new VillRaiseCall("Open raise calling", info),
                        new GTO::ParentNode("4-bet",
                                            GTO::Node::HERO,
                                            hsize,
                                            four_bet_children)
                };
        else
                three_bet_children = {
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
