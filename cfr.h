#ifndef GTO_CFR_H_
#define GTO_CFR_H_

#include <cstdio>
#include <cstdlib>
#include <string>
#include <vector>

#include "array.h"
#include "err.h"

namespace GTO {
using std::string;
using std::vector;

class Node {
public:
        // Represents the players in the game.
        enum Player {
                HERO,
                VILLAIN,
                NOBODY
        };

        const string& name() const { return name_; }
        const Player& active_player() const { return active_player_; }
        const vector<Node *>& children() const { return children_; }

        // Return the utility of "player" at a leaf node. "pid" is the
        // stade id of "player" and "oid" the state id of his opponent.
        virtual double Utility(const Player& player,
                               const size_t& pid,
                               const size_t& oid) const = 0;

        // Return the average info set mixed strategy across all
        // iterations.  The returned value is an Array.  The rows
        // correspond to the states id of the active player and the
        // columns to the actions taken at the node.  The value at a
        // given row and column is the probability that the current
        // active player takes the given action if he's in the given
        // state.
        Array AverageStrategy() const;

        // Test if the node is a leaf.
        bool isleaf() const
        {
                return children_.empty();
        }

        // Return the utility at the current node of a given "player"
        // using the counterfactual regret minimization algorithm to
        // update the game tree under node.  "pid" is the state id of
        // "player" and "oid" the state id of his opponent.
        double CFR(const Player& player, const size_t& pid, const size_t& oid)
        {
                return CFR(this, player, pid, oid, 1.0, 1.0);
        }

        // For each player, get the names of the nodes under "node"
        // where the player is last active before the game terminates.
        // Those corresponding to HERO are appended to "hero_names"
        // and those corresponding to villain to "vill_hands".
        static void
        GetFinalActionNames(const Node& node,
                            vector<string>& hero_names,
                            vector<string>& vill_names);

        // Get the probabilities of taking each possible final action
        // for "player" under "node".  The result is stored in "probs"
        // which is Array of size N x M where N is the number of
        // possible states for "player" and M is the number of
        // possible final actions returned by GetFinalActionNames
        // taken by the player under "node".  Thus probs.get(i, j) is
        // the probability that "player" would take the final action
        // number j if his state id is i.
        static void
        GetFinalActionProbs(const Node& node,
                            const Player& player,
                            Array& probs);

protected:
        explicit Node(const string& name,
                      const Player& active_player,
                      const vector<Node *>& children,
                      const Array& regret_sum,
                      const Array& strategy_sum,
                      const Array& strategy,
                      const vector<double>& utils)
                : name_(name),
                  active_player_(active_player),
                  children_(children),
                  regret_sum_(regret_sum),
                  strategy_sum_(strategy_sum),
                  strategy_(strategy),
                  utils_(utils)
        {}

        virtual ~Node()
        {
                for (auto it = children_.begin(); it != children_.end(); ++it)
                        delete *it;
        }

private:
        // The public method is just a thin wrapper around this one.
        // "pprob" and "oprob" are the reaching probabilities of "node" for
        // "player" and his opponent respectively.
        double CFR(Node* node,
                   const Player& player,
                   const size_t& pid,
                   const size_t& oid,
                   const double& pprob,
                   const double& oprob);

        const string name_;           // Name of the node.
        const Player active_player_;  // Player that should play at the node.
        vector<Node *> children_;     // Children of the node.
        Array regret_sum_;            // Sum of regrets of each action.
        Array strategy_sum_;          // Sum of all previous strategies.
        Array strategy_;              // Probabilities of taking each action.
        vector<double> utils_;        // Utilities of each action.
};

class ParentNode : public Node {
public:
        // Create a new Node with name "name" and an active player
        // "active_player". "children" is a non-empty vector of nodes that
        // are the children of the current one.  "num_states" is the
        // number of possible states of the active player at the given
        // node.  Each state is referred to by an unique id between 0
        // and "num_states"-1.
        explicit ParentNode(const string& name,
                            const Player& active_player,
                            const size_t& nstates,
                            const vector<Node *>& children)
                : Node(name,
                       active_player,
                       children,
                       Array(nstates, children.size()),
                       Array(nstates, children.size()),
                       Array(nstates, children.size(), 1.0/children.size()),
                       vector<double>(children.size()))
        {
                if (children.size() == 0)
                        err::quit("children of %s should be a non-empty vector \
of nodes.", name.c_str());
        }

        ~ParentNode() {}

        // A non-terminal node doesn't need this method.
        double Utility(const Player& player,
                       const size_t& pid,
                       const size_t& oid) const
        {
                assert(false);
                return 0;
        }
};

class Leaf : public Node {
public:
        // Create a leaf node with name "name". The node should
        // implement the Utility method.
        explicit Leaf(const string& name)
                : Node(name,
                       NOBODY,
                       vector<Node*>(),
                       Array(),
                       Array(),
                       Array(),
                       vector<double>())
        {}

        ~Leaf() {}
};

}

#endif  // !GTO_RANGE_H_
