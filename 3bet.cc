#include <libgen.h>

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

        explicit GameInfo(const double& stack,
                          const double& pot,
                          const double& bet,
                          const double& raise,
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
        Utility(const Player& player,
                const size_t& pid,
                const size_t& oid) const
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
        Utility(const Player& player,
                const size_t& pid,
                const size_t& oid) const
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
        Utility(const Player& player,
                const size_t& pid,
                const size_t& oid) const
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
        Utility(const Player& player,
                const size_t& pid,
                const size_t& oid) const
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
        double Utility(const Player& player,
                       const size_t& pid,
                       const size_t& oid) const
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

} // namespace

int
main(int argc, char *argv[])
{
        err::progname = strdup(basename(argv[0]));
        GTO::Range vill("74,75,54,6d5d,77,44,55,88,63,86,Ad7h,Ad7c,Ad7s,Kd7h,Kd7c,Kd7s,Ad6h,Ad6c,Ad6s,Kd6h,Kd6c,Kd6s,3d2d,6d2d,9d6d,Td6d,Jd6d,Qd6d,Kd6d,Ad6d,Ad8d,Kd8d,Ad3d,Kd3d");
        GTO::Range hero("77-22,ATs-A2s,K2s+,Q7s+,J8s+,T8s+,97s+,86s+,75s+,64s+,53s+,42s+,32s,ATo-A8o,K9o+,QTo+,JTo");
        GameInfo info(4135, 550, 250, 500, pokerstove::CardSet("7d4d5h"), vill,
                      hero);
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
        vector<GTO::Node *> root_children = {
                new HeroFold("Folding", info),
                new HeroFlatCall("Flat calling", info),
                new GTO::ParentNode("Raising",
                                    GTO::Node::VILLAIN,
                                    vsize,
                                    raise_children)
        };
        GTO::ParentNode root("root", GTO::Node::HERO, hsize, root_children);
        Dealer dealer(info.hero_hands, info.vill_hands);
        Train(40000000,
              info.hero_hands,
              info.vill_hands,
              "CO",
              "SB",
              dealer,
              root);
        free(const_cast<char *>(err::progname));
        return 0;
}
