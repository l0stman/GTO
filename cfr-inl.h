#ifndef GTO_CFR_INL_H_
#define GTO_CFR_INL_H_

#include <string>
#include <vector>

#include "cfr.h"

namespace GTO {

// Print all the strategies of each node under NODE for PLAYER. STATES
// is a vector containing all the possible states for the given
// player.  Thus, the state id varies between 0 and STATES.size()-1.
// The State class should have a str() method that returns a string
// representing it.  STATE_NAME is the chosen name of the state.
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

// Thin wrapper around the other function.
template<class State>
inline void
TreePrint(const Node& node,
          const Node::Player& player,
          const std::vector<State>& states)
{
        TreePrint(node, player, states, "State");
}
} // namespace GTO

#endif  // !GTO_CFR_INL_H_
