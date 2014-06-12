#ifndef GTO_CFR_H_
#define GTO_CFR_H_

#include <cassert>
#include <cstdio>
#include <cstdlib>
#include <string>
#include <vector>

namespace GTO {
using std::string;
using std::vector;

// Represents a two-dimensional array of doubles.
class Array {
public:
        Array() : nrows_(0), ncols_(0) {}

        Array(size_t nrows, size_t ncols, double init=0.0)
                : vect_(vector<double>(nrows*ncols, init)),
                  nrows_(nrows),
                  ncols_(ncols)
        {}

        size_t NumRows() const { return nrows_; }
        size_t NumCols() const { return ncols_; }

        double Get(size_t row, size_t col) const
        {
                assert(row < nrows_ && col < ncols_);
                return vect_[row*ncols_+col];
        }

        void Set(size_t row, size_t col, double val)
        {
                assert(row < nrows_ && col < ncols_);
                vect_[row*ncols_+col] = val;
        }

        void Inc(size_t row, size_t col, double val)
        {
                assert(row < nrows_ && col < ncols_);
                vect_[row*ncols_+col] += val;
        }
private:
        vector<double> vect_;
        const size_t nrows_;
        const size_t ncols_;
};

class Node {
public:
        // Represents the players in the game.
        enum Player {
                HERO,
                VILLAIN,
                NOBODY
        };

        const string& Name() const { return name_; }
        const Player& ActivePlayer() const { return active_player_; }
        const vector<Node *>& Children() const { return children_; }

        // Returns the utility of PLAYER at a leaf node. PID is the
        // stade id of PLAYER and OID the state id of his opponent.
        virtual double Utility(const Player& player,
                               const size_t& pid,
                               const size_t& oid) const = 0;

        // Return the average info set mixed strategy across all
        // iterations.  The returned value is an Array.  The rows
        // correspond the states id of the active player and the
        // columns to the actions taken at the node.
        Array AverageStrategy() const;

        // Test if the node is a leaf.
        bool IsLeaf() const
        {
                return children_.empty();
        }

        // Return the utility at the current node of a given PLAYER
        // and the counterfactual regret minimization algorithm to
        // update the game tree under node.  PID is the state id of
        // PLAYER and OID the state id of his opponent.
        double CFR(const Player& player, const size_t& pid, const size_t& oid)
        {
                return CFR(this, player, pid, oid, 1.0, 1.0);
        }

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
        // PPROB and OPROB are the reaching probabilities of NODE for
        // PLAYER and his opponent respectively.
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
        // Create a new Node with name NAME and an active player
        // ACTIVE_PLAYER. CHILDREN is a non-empty vector of nodes that
        // are the children of the current one.  NUM_STATES is the
        // number of possible states of the active player at the given
        // node.  Each state is referred to by an unique id between 0
        // and NUM_STATES-1.
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
                if (children.size() == 0) {
                        fprintf(stderr, "CHILDREN should be a non-empty vector\
 of nodes\n");
                        exit(1);
                }
        }

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
        // Create a leaf node with name NAME. The node should
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
