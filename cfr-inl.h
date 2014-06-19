#ifndef GTO_CFR_INL_H_
#define GTO_CFR_INL_H_

#include <string>
#include <vector>

#include "cfr.h"

namespace GTO {

// Print all the strategies of each node under "node" for
// "player". "states" is a vector containing all the possible states
// for the given player.  Thus, the state id varies between 0 and
// states.size()-1.  "state_name" is the chosen name of each the state.
// REQUIRES: the State class should have a str() method that returns a
// string representing it.
template<class State>
void
TreePrint(const Node& node,
          const Node::Player& player,
          const std::vector<State>& states,
          const std::string& state_name)
{
        if (node.isleaf())
                return;
        const std::vector<Node *>& children = node.children();
        if (node.active_player() == player) {
                GTO::Array strat = node.AverageStrategy();
                size_t nstates = strat.num_rows();
                size_t nactions = strat.num_cols();
                printf("%s", state_name.c_str());
                for (size_t a = 0; a < nactions; a++)
                        printf(" | %s", children[a]->name().c_str());
                putchar('\n');
                for (size_t s = 0; s < nstates; s++) {
                        printf("%s", states[s].str().c_str());
                        for (size_t a = 0; a < nactions; a++)
                                printf(" %.4f", strat.get(s, a));
                        putchar('\n');
                }
        }
        for (auto it = children.begin(); it != children.end(); ++it)
                TreePrint<State>(**it, player, states);
}

// Thin wrapper around the other TreePrint function.
template<class State>
inline void
TreePrint(const Node& node,
          const Node::Player& player,
          const std::vector<State>& states)
{
        TreePrint(node, player, states, "State");
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
// states of "player" during the game. "names" is vector nodes
// returned by GetFinalActionNames for the player. "state_name" is the
// generic name of each state.
// REQUIRES: the State class should have a str() method that returns a
// string representing it.
template<class State>
void
FlatPrint(const Node& root,
          const Node::Player& player,
          const vector<State>& states,
          const vector<string>& names,
          const std::string& state_name)
{
        Array probs(states.size(), names.size());
        vector<Record> records;
        double total = 0.0;

        Node::GetFinalActionProbs(root, player, probs);
        for (size_t n = 0; n < names.size(); n++) {
                total = 0.0;
                records.clear();
                records.reserve(names.size());
                for (size_t id = 0; id < states.size(); id++) {
                        if (probs.get(id, n) >= 0.05) {
                                records.push_back(Record(states[id].str(),
                                                         probs.get(id, n)));
                                total += probs.get(id, n);
                        }
                }
                sort(records.begin(), records.end());
                printf("%s range: %.2f %s%c\n", names[n].c_str(), total,
                       state_name.c_str(), total == 1 ? ' ' : 's');
                printf("%s\tProb\n", state_name.c_str());
                for (auto it = records.begin(); it != records.end(); ++it)
                        printf("%s\t%.4f\n", it->state.c_str(), it->prob);
        }
}

// This is a thin wrapper around the other FlatPrint function.
template<class State>
inline void
FlatPrint(const Node& root,
          const Node::Player& player,
          const vector<State>& states,
          const vector<string>& names)
{
        FlatPrint(root, player, states, names, "State");
}

} // namespace GTO

#endif  // !GTO_CFR_INL_H_
