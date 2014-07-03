// This a 3-bet shove game on the flop in an unraised pot.  The SB
// leads out by betting a certain amount, it's folded to a player in
// CO who can fold, raise, or flat call.  If he flat calls, he's going
// to call another bet equal to 2/3 of the pot on the turn.  If he
// raises, the SB can fold or move all-in.  Then, the CO can call or
// fold.  The SB betting range and the CO limping range should be
// supplied as other parameters such as the bet size.  And a GTO
// strategy is computed for each player.
// Usage:
// $ 3bet -h
// $ 3bet -f -N <3bet_example.txt

#include <libgen.h>
#include <limits.h>
#include <unistd.h>

#include <cstdlib>
#include <cstring>
#include <random>
#include <vector>

#include "cfr.h"
#include "cfr-inl.h"
#include "dealer_interface.h"
#include "err.h"
#include "equi_dist.h"
#include "range.h"

namespace {
using std::vector;
using std::string;

struct GameInfo {
        const double stack;
        const double pot;
        const double bet;
        const double raise;
        const vector<GTO::Hand> vill_hands;
        const vector<GTO::Hand> hero_hands;
        const GTO::Array<double> equity;

        explicit GameInfo(double stack,
                          double pot,
                          double bet,
                          double raise,
                          const pokerstove::CardSet& board,
                          const GTO::Range& vill,
                          const GTO::Range& hero)
                : stack(stack),
                  pot(pot),
                  bet(bet),
                  raise(raise),
                  vill_hands(vill.ToVector(board)),
                  hero_hands(hero.ToVector(board)),
                  equity(GTO::EquiDist(
                                 vill, hero, board).LUT(vill_hands,hero_hands))
        {}
};

class HeroRaiseFold : public GTO::Leaf {
public:
        explicit HeroRaiseFold(const string& name, const GameInfo& info)
                : GTO::Leaf(name), info_(info)
        {}

        virtual double
        Utility(Player player, size_t pid, size_t oid) const
        {
                double EV = 0;

                switch (player) {
                case GTO::Node::HERO:
                        EV = info_.stack-info_.raise;
                        break;
                case GTO::Node::VILLAIN:
                        EV = info_.stack + info_.pot + info_.raise;
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


class HeroRaiseCall : public GTO::Leaf {
public:
        explicit HeroRaiseCall(const string& name, const GameInfo& info)
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
                return EQ*(2*info_.stack+info_.pot);
        }

private:
        const GameInfo& info_;
};

class VillBetFold : public GTO::Leaf {
public:
        explicit VillBetFold(const string& name, const GameInfo& info)
                : GTO::Leaf(name), info_(info)
        {}

        virtual double
        Utility(Player player, size_t pid, size_t oid) const
        {
                double EV = 0;

                switch (player) {
                case GTO::Node::HERO:
                        EV = info_.stack + info_.pot;
                        break;
                case GTO::Node::VILLAIN:
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
                case GTO::Node::HERO:
                        EV = info_.stack;
                        break;
                case GTO::Node::VILLAIN:
                        EV = info_.stack + info_.pot;
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

        virtual
        double Utility(Player player, size_t pid, size_t oid) const
        {
                double turn_pot = info_.pot + 2*info_.bet;
                double turn_bet = 2.0/3*turn_pot;
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
                return info_.stack-info_.bet-turn_bet +
                        EQ*(turn_pot + 2*turn_bet);
        }
private:
        const GameInfo& info_;
};

std::mt19937 generator((std::random_device())());

class Dealer : public GTO::DealerInterface {
public:
        typedef std::uniform_int_distribution<size_t> UniDist;

        explicit Dealer(const vector<GTO::Hand>& hero_hands,
                        const vector<GTO::Hand>& vill_hands)
                : hero_hands_(hero_hands),
                  vill_hands_(vill_hands),
                  hero_dist_(UniDist(0, hero_hands.size()-1)),
                  vill_dist_(UniDist(0, vill_hands.size()-1))
        {}

        ~Dealer() {}

        virtual void
        Deal(size_t& hero_id, size_t& vill_id)
        {
                for (;;) {
                        hero_id = hero_dist_(generator);
                        vill_id = vill_dist_(generator);
                        if (hero_hands_[hero_id].disjoint(vill_hands_[vill_id]))
                                return;
                }
        }

private:
        const vector<GTO::Hand>& hero_hands_;
        const vector<GTO::Hand>& vill_hands_;
        UniDist hero_dist_;
        UniDist vill_dist_;
};

void
Usage()
{
        fprintf(stderr, "usage: %s [-h] [-f] [-N] [-n niter]\n", err::progname);
        fprintf(stderr, "  -h\t\t-- print this help message\n");
        fprintf(stderr, "  -f\t\t-- allow the CO to have a flat calling range\
\n");
        fprintf(stderr, "  -N\t\t-- non-interactive mode, don't prompt the user\
 for parameters\n");
        fprintf(stderr, "  -n niter\t-- number of iterations for the simulation\
\n");
        exit(1);
}

size_t num_iter = 200000000;
bool fflag = false;
bool iflag = true;

// Return a string on the next non-empty line read from "stdin" and
// drop the newline.
char *
ReadLine()
{
        static char buf[BUFSIZ];
        for (;;) {
                if (fgets(buf, sizeof(buf), stdin) == NULL) {
                        if (feof(stdin))
                                exit(1);
                        else
                                err::sys("Read");
                }
                size_t len = strlen(buf)-1;
                if (len == 0)
                        continue; // empty line
                if (buf[len] == '\n') {
                        buf[len] = '\0';
                        break;
                }

        }
        return buf;
}

} // namespace

int
main(int argc, char *argv[])
{
        int ch;
        double stack, pot, bet, raise;

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
        fscanf(stdin, "%lf\n", &stack);
        if (iflag)
                fprintf(stderr, "Enter SB's bet size: ");
        fscanf(stdin, "%lf\n", &bet);
        if (iflag)
                fprintf(stderr, "Enter the pot size after SB bets: ");
        fscanf(stdin, "%lf\n", &pot);
        if (iflag)
                fprintf(stderr, "Enter CO's raise size: ");
        fscanf(stdin, "%lf\n", &raise);
        if (iflag)
                fprintf(stderr, "Enter the board: ");
        pokerstove::CardSet board(ReadLine());
        if (iflag)
                fprintf(stderr, "Enter the SB's range: ");
        GTO::Range vill(ReadLine());
        if (iflag)
                fprintf(stderr, "Enter the CO's range: ");
        GTO::Range hero(ReadLine());
        GameInfo info(stack, pot, bet, raise, board, vill, hero);
        size_t vsize = info.vill_hands.size();
        size_t hsize = info.hero_hands.size();
        vector<GTO::Node *> shove_children = {
                new HeroRaiseFold("Bluff raising", info),
                new HeroRaiseCall("Calling", info)
        };
        vector<GTO::Node *> raise_children = {
                new VillBetFold("Bet folding", info),
                new GTO::ParentNode("Shoving",
                                    GTO::Node::HERO,
                                    hsize,
                                    shove_children)
        };
        vector<GTO::Node *> root_children;
        if (fflag)
                root_children = {
                        new HeroFold("Folding", info),
                        new HeroFlatCall("Flat calling", info),
                        new GTO::ParentNode("Raising",
                                            GTO::Node::VILLAIN,
                                            vsize,
                                            raise_children)
                };
        else
                root_children = {
                        new HeroFold("Folding", info),
                        new GTO::ParentNode("Raising",
                                            GTO::Node::VILLAIN,
                                            vsize,
                                            raise_children)
                };

        GTO::ParentNode root("root", GTO::Node::HERO, hsize, root_children);
        Dealer dealer(info.hero_hands, info.vill_hands);
        printf("Stack\t: %.2f\nPot\t: %.2f\nBet\t: %.2f\nRaise\t: %.2f\n\
Board\t: %s\n\n", info.stack, info.pot, info.bet, info.raise,
               board.str().c_str());
        GTO::Train(num_iter,
                   info.hero_hands,
                   info.vill_hands,
                   "CO",
                   "SB",
                   dealer,
                   root);
        free(const_cast<char *>(err::progname));
        return 0;
}
