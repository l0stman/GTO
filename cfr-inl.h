#ifndef GTO_CFR_INL_H_
#define GTO_CFR_INL_H_

#include <algorithm>
#include <string>
#include <vector>

#include "cfr.h"
#include "dealer_interface.h"

namespace GTO {

// Print all the strategies of each node under "node" for
// "player". "states" is a vector containing all the possible states
// for the given player.  Thus, the state id varies between 0 and
// states.size()-1.
// REQUIRES: the State class should implement StateInterface.
template<class State>
void
TreePrint(const Node& node,
          const Node::Player& player,
          const std::vector<State>& states)
{
        if (node.isleaf())
                return;
        if (node.active_player() == player) {
                Array<double> strat = node.AverageStrategy();
                printf("%s", State::Name().c_str());
                for (auto c : node.children())
                        printf(" | %s", c->name().c_str());
                putchar('\n');
                for (size_t s = 0; s < strat.num_rows(); s++) {
                        printf("%s", states[s].ToString().c_str());
                        for (size_t a = 0; a < strat.num_cols(); a++)
                                printf("\t%.4f", strat.get(s, a));
                        putchar('\n');
                }
        }
        for (auto c : node.children())
                TreePrint(*c, player, states);
}

struct Record {
        string state;
        double prob;

        explicit Record(const string& state, const double& prob)
                : state(state), prob(prob)
        {}

        bool operator<(const Record& rhs) const
        {
                return rhs.prob < prob;
        }
};

// For each node under "root" where "player" is last active, print the
// probability that "player" would take the given action if he was in
// a given state. "states" is a vector containing all the possible
// states of "player" during the game. "names" is vector of nodes
// returned by GetFinalActionNames for the player.
// REQUIRES: the State class should implement StateInterface.
template<class State>
void
FlatPrint(const Node& root,
          const Node::Player& player,
          const vector<State>& states,
          const vector<string>& names)
{
        Array<double> probs(states.size(), names.size());
        vector<Record> records;
        double total = 0.0;

        Node::GetFinalActionProbs(root, player, probs);
        for (size_t n = 0; n < names.size(); n++) {
                total = 0.0;
                records.clear();
                records.reserve(names.size());
                for (size_t id = 0; id < states.size(); id++) {
                        if (probs.get(id, n) >= 0.05) {
                                records.push_back(Record(states[id].ToString(),
                                                         probs.get(id, n)));
                                total += states[id].NumCombos()*probs.get(id,n);
                        }
                }
                sort(records.begin(), records.end());
                printf("%s range: %.2f %s%c\n", names[n].c_str(), total,
                       State::Name().c_str(), total == 1 ? ' ' : 's');
                printf("%s\tProb\n", State::Name().c_str());
                for (auto r : records)
                        printf("%s\t%.4f\n", r.state.c_str(), r.prob);
        }
}

inline void
UtilError(const Node::Player& player, const string& name)
{
        err::quit("Don't have utility for %s at the node %s.",
                  Node::player_names[player], name.c_str());
}

// Use CFR to update "node" by playing repeatedly "Node::VILLAIN"
// against "Node::HERO" during "num_iter" iterations then print the
// result to the standard output at the end. "hero_states" and
// "vill_states" are vectors containing all the possible states of
// "Node::HERO" and "Node::VILLAIN" during the game respectively.  And
// "dealer" is the dealer of the game.
// REQUIRES: the State class should implement StateInterface.
template<class State>
void
Train(const size_t& num_iter,
      const vector<State>& hero_states,
      const vector<State>& vill_states,
      const string& hero_name,
      const string& vill_name,
      DealerInterface& dealer,
      Node& node)
{
        double vutil = 0.0;
        double hutil = 0.0;
        size_t hero_id = 0;
        size_t vill_id = 0;

        for (size_t i = 1; i <= num_iter; i++) {
                dealer.Deal(hero_id, vill_id);
                vutil += node.CFR(Node::VILLAIN, vill_id, hero_id);
                dealer.Deal(hero_id, vill_id);
                hutil += node.CFR(Node::HERO, hero_id, vill_id);
                if (i % 1000000 == 0)
                        fprintf(stderr, "%u Villain: %.8f, Hero: %.8f\n",
                                i, vutil/i, hutil/i);
        }
        hutil /= num_iter;
        vutil /= num_iter;
        vector<string> hnames;
        vector<string> vnames;
        Node::GetFinalActionNames(node, hnames, vnames);
        printf("%s: %.4f\n", vill_name.c_str(), vutil);
        FlatPrint(node, Node::VILLAIN, vill_states, vnames);
        printf("\n%s: %.4f\n", hero_name.c_str() , hutil);
        FlatPrint(node, Node::HERO, hero_states, hnames);
}

} // namespace GTO

#endif  // !GTO_CFR_INL_H_
