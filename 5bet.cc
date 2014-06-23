#include <libgen.h>

#include <cstdio>
#include <cstdlib>
#include <cstring>

#include <random>

#include "cfr-inl.h"
#include "dealer_interface.h"
#include "err.h"
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
        const GTO::Array<short> suit_combos;

        explicit GameInfo(const double& stack,
                          const double& blinds,
                          const double& raise,
                          const double& three_bet,
                          const double& four_bet,
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
        Utility(const Player& player,
                const size_t& pid,
                const size_t& oid) const
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
        Utility(const Player& player,
                const size_t& pid,
                const size_t& oid) const
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
        Utility(const Player& player,
                const size_t& pid,
                const size_t& oid) const
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
        Utility(const Player& player,
                const size_t& pid,
                const size_t& oid) const
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
        Utility(const Player& player,
                const size_t& pid,
                const size_t& oid) const
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
                        short m = suit_combos_.get(vill_id, hero_id);
                        short s = vill_hands_[vill_id].suit_combos();
                        if (m == s)
                                return;
                        std::uniform_int_distribution<size_t> dist(0, s-1);
                        if (dist(generator) < m)
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
        const GTO::Array<short>& suit_combos_;
        const vector<double> hero_weights_;
        const vector<double> vill_weights_;
};

} // namespace

int
main(int argc, char *argv[])
{
        err::progname = strdup(basename(argv[0]));
        GTO::PreflopRange vill("77+,A7s+,K9s+,QTs+,JTs,ATo+,KTo+,QJo");
        GTO::PreflopRange hero;

        hero.Fill();
        GameInfo info(100, 1.5, 3, 9, 27, vill, hero);
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
        vector<GTO::Node *> root_children = {
                new HeroFold("Folding", info),
                new HeroFlatCall("Flat calling", info),
                new GTO::ParentNode("3-bet",
                                    GTO::Node::VILLAIN,
                                    vsize,
                                    three_bet_children)
        };
        GTO::ParentNode root("root", GTO::Node::HERO, hsize, root_children);
        Dealer dealer(info);
        GTO::Train(100000000,
                   info.hero_hands,
                   info.vill_hands,
                   "BTN",
                   "UTG",
                   dealer,
                   root);
        free(const_cast<char *>(err::progname));
        return 0;
}
