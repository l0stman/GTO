#include <libgen.h>

#include <cstdlib>
#include <cstring>
#include <random>
#include <vector>

#include "cfr.h"
#include "cfr-inl.h"
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
        GTO::Array equity;

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

void
UtilError(const GTO::Node::Player& player, const string& name)
{
        err::quit("Don't have utility for %d at the node %s.", player,
                  name.c_str());
}

class HeroRaiseFold : public GTO::Leaf {
public:
        explicit HeroRaiseFold(const string& name, const GameInfo& info)
                : GTO::Leaf(name), info_(info)
        {}

        double Utility(const Player& player,
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
                        UtilError(player, name());
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

        double Utility(const Player& player,
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
                        UtilError(player, name());
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

        double Utility(const Player& player,
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
                        UtilError(player, name());
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

        double Utility(const Player& player,
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
                        UtilError(player, name());
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
                        UtilError(player, name());
                        break;
                }
                return info_.stack-info_.bet-turn_bet +
                        EQ*(turn_pot + 2*turn_bet);
        }
private:
        const GameInfo& info_;
};

void
LeafNames(const GTO::Node& node,
          vector<string>& hnames,
          vector<string>& vnames)
{
        if (node.isleaf())
                return;
        bool isterm = true;
        for (auto it = node.children().begin();
             it != node.children().end();
             ++it) {
                if ((*it)->isleaf())
                        if (node.active_player() == GTO::Node::HERO)
                                hnames.push_back((*it)->name());
                        else
                                vnames.push_back((*it)->name());
                else
                        isterm = false;
                LeafNames(**it, hnames, vnames);
        }
        if (isterm) {
                if (node.active_player() == GTO::Node::HERO)
                        vnames.push_back(node.name());
                else
                        hnames.push_back(node.name());
        }
}

void
ProbArrayIter(const GTO::Node& node,
              const size_t& id,
              const GTO::Node::Player& player,
              const double& p,
              size_t& idx,
              GTO::Array& probs)
{
        if (node.isleaf())
                return;
        GTO::Array strat = node.AverageStrategy();
        bool isactive = node.active_player() == player;
        bool isterm = true;

        for (size_t a = 0; a < node.children().size(); a++) {
                GTO::Node* c = node.children()[a];
                if (c->isleaf()) {
                        if (isactive)
                                probs.set(id, idx++, p*strat.get(id, a));

                } else
                        isterm = false;
                ProbArrayIter(*c,
                              id,
                              player,
                              isactive ? p*strat.get(id, a) : p,
                              idx,
                              probs);
        }
        if (isterm && !isactive)
                probs.set(id, idx++, p);
}

void
ProbArray(const GTO::Node& node,
          const GTO::Node::Player& player,
          GTO::Array& probs)
{
        for (size_t id = 0; id < probs.num_rows(); id++) {
                size_t idx = 0;
                ProbArrayIter(node, id, player, 1.0, idx, probs);
        }
}

struct Record {
        string hand;
        double prob;

        explicit Record(const string& hand, const double& prob)
                : hand(hand), prob(prob)
        {}

        bool operator<(const Record& rhs) const
        {
                return rhs.prob < prob;
        }
};

void
PrintProbs(const string& player,
           const vector<GTO::Hand>& hands,
           const GTO::Array& probs,
           const vector<string>& names,
           const double& util)
{
        vector<Record> records;
        double total = 0.0;

        printf("%s: %.4f\n", player.c_str(), util);
        for (size_t n = 0; n < names.size(); n++) {
                total = 0.0;
                records.clear();
                records.reserve(hands.size());
                for (size_t id = 0; id < hands.size(); id++) {
                        if (probs.get(id, n) >= 0.05) {
                                records.push_back(Record(hands[id].str(),
                                                         probs.get(id, n)));
                                total += probs.get(id, n);
                        }
                }
                sort(records.begin(), records.end());
                printf("%s range: %.2f hand%c\n", names[n].c_str(), total,
                       total == 1 ? ' ' : 's');
                printf("Hand\tProb\n");
                for (auto it = records.begin(); it != records.end(); ++it)
                        printf("%s\t%.4f\n", it->hand.c_str(), it->prob);
        }
}

void
PrintNode(const GTO::Node& root,
          const double& hutil,
          const double& vutil,
          const vector<GTO::Hand>& hhands,
          const vector<GTO::Hand>& vhands)
{
        vector<string> hnames;
        vector<string> vnames;
        LeafNames(root, hnames, vnames);
        GTO::Array hprobs(hhands.size(), hnames.size());
        GTO::Array vprobs(vhands.size(), vnames.size());
        ProbArray(root, GTO::Node::HERO, hprobs);
        ProbArray(root, GTO::Node::VILLAIN, vprobs);
        PrintProbs("SB", vhands, vprobs, vnames, vutil);
        putchar('\n');
        PrintProbs("CO", hhands, hprobs, hnames, hutil);
}

void
Deal(const GameInfo& info,
     std::mt19937& generator,
     std::uniform_int_distribution<size_t>& hdist,
     std::uniform_int_distribution<size_t>& vdist,
     size_t& hero_id,
     size_t& vill_id)
{
        for (;;) {
                hero_id = hdist(generator);
                vill_id = vdist(generator);
                if (info.hero_hands[hero_id].disjoint(info.vill_hands[vill_id]))
                        return;
        }
}

void
Simulate(const double& stack,
         const double& pot,
         const double& bet,
         const double& raise,
         const pokerstove::CardSet& board,
         const GTO::Range& vill,
         const GTO::Range& hero)
{
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
        vector<GTO::Node *> root_children = {
                new HeroFold("Folding", info),
                new HeroFlatCall("Flat calling", info),
                new GTO::ParentNode("Raising",
                                    GTO::Node::VILLAIN,
                                    vsize,
                                    raise_children)
        };
        GTO::ParentNode root("root",
                             GTO::Node::HERO,
                             hsize,
                             root_children);
        double vutil = 0.0;
        double hutil = 0.0;
        size_t niter = 40000000;
        size_t hero_id = 0;
        size_t vill_id = 0;
        std::random_device rd;
        std::mt19937 generator(rd());
        std::uniform_int_distribution<size_t> hdist(0, hsize-1);
        std::uniform_int_distribution<size_t> vdist(0, vsize-1);
        for (size_t i = 0; i < niter; i++) {
                Deal(info, generator, hdist, vdist, hero_id, vill_id);
                vutil += root.CFR(GTO::Node::VILLAIN, vill_id, hero_id);
                Deal(info, generator, hdist, vdist, hero_id, vill_id);
                hutil += root.CFR(GTO::Node::HERO, hero_id, vill_id);
                if (i % 1000000 == 0 && i > 0)
                        fprintf(stderr, "%d Villain: %.8f, Hero: %.8f\n",
                                i, vutil/i, hutil/i);
        }
        PrintNode(root,
                  hutil/niter,
                  vutil/niter,
                  info.hero_hands,
                  info.vill_hands);
}
} // namespace

int
main(int argc, char *argv[])
{
        err::progname = strdup(basename(argv[0]));
        GTO::Range vill("74,75,54,6d5d,77,44,55,88,63,86,Ad7h,Ad7c,Ad7s,Kd7h,Kd7c,Kd7s,Ad6h,Ad6c,Ad6s,Kd6h,Kd6c,Kd6s,3d2d,6d2d,9d6d,Td6d,Jd6d,Qd6d,Kd6d,Ad6d,Ad8d,Kd8d,Ad3d,Kd3d");
        GTO::Range hero("77-22,ATs-A2s,K2s+,Q7s+,J8s+,T8s+,97s+,86s+,75s+,64s+,53s+,42s+,32s,ATo-A8o,K9o+,QTo+,JTo");
        Simulate(4135, 550, 250, 825, pokerstove::CardSet("7d4d5h"), vill, hero);
        free(const_cast<char *>(err::progname));
        return 0;
}
