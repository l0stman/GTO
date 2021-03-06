#include "cfr.h"

namespace {

void
GetProbsAux(const GTO::Node& node,
            size_t id,
            GTO::Node::Player player,
            double p,
            size_t& idx,
            GTO::Array<double>& probs)
{
        if (node.isleaf())
                return;
        GTO::Array<double> strat = node.AverageStrategy();
        bool isactive = node.active_player() == player;
        bool isterm = true;

        for (size_t a = 0; a < node.children().size(); a++) {
                GTO::Node* c = node.children()[a];
                if (c->isleaf()) {
                        if (isactive)
                                probs.set(id, idx++, p*strat.get(id, a));

                } else
                        isterm = false;
                GetProbsAux(*c,
                            id,
                            player,
                            isactive ? p*strat.get(id, a) : p,
                            idx,
                            probs);
        }
        if (isterm && !isactive)
                probs.set(id, idx++, p);
}

} // namespace

namespace GTO {

#define X(a, b) b,
const char *Node::player_names[] = { PLAYERS };
#undef X

Array<double>
Node::AverageStrategy() const
{
        if (isleaf())
                return Array<double>();
        Array<double> avg(strategy_sum_.num_rows(), strategy_sum_.num_cols());
        for (size_t s=0; s<avg.num_rows(); s++) {
                double norm = 0;
                for (size_t a=0; a<avg.num_cols(); a++)
                        norm += strategy_sum_.get(s, a);
                for (size_t a=0; a<avg.num_cols(); a++)
                        if (norm > 0)
                                avg.set(s, a, strategy_sum_.get(s, a)/norm);
                        else
                                avg.set(s, a, 1.0/avg.num_cols());
                        }
        return avg;
}

double
Node::CFR(Node* node,
          Player player,
          size_t pid,
          size_t oid,
          double pprob,
          double oprob)
{
        size_t len = node->children_.size();
        vector<double>& utils = node->utils_;
        double util = 0.0;
        bool isactive = node->active_player_ == player;
        size_t id = isactive ? pid : oid;
        Array<double>& regsum = node->regret_sum_;
        Array<double>& stratsum = node->strategy_sum_;
        Array<double>& strat = node->strategy_;

        for (size_t a=0; a<len; a++) {
                if (node->children_[a]->isleaf())
                        utils[a] = node->children_[a]->Utility(player, pid,oid);
                else if (isactive)
                        utils[a] = CFR(node->children_[a],
                                       player,
                                       pid,
                                       oid,
                                       pprob*strat.get(pid, a),
                                       oprob);
                else
                        utils[a] = CFR(node->children_[a],
                                       player,
                                       pid,
                                       oid,
                                       pprob,
                                       oprob*strat.get(oid, a));
                util += utils[a] * strat.get(id, a);
        }
        if (isactive) {
                for (size_t a=0; a<len; a++) {
                        regsum.inc(pid, a, oprob*(utils[a]-util));
                        stratsum.inc(pid, a, pprob*strat.get(pid, a));
                }
                double norm = 0.0;
                for (size_t a=0; a<len; a++) {
                        double rs = regsum.get(pid, a);
                        strat.set(pid, a, rs > 0 ? rs : 0.0);
                        norm += strat.get(pid, a);
                }
                for (size_t a=0; a<len; a++)
                        strat.set(pid,
                                  a,
                                  norm > 0 ? strat.get(pid, a)/norm : 1.0/len);
        }
        return util;
}

void
Node::GetFinalActionNames(const Node& node,
                          vector<string>& hero_names,
                          vector<string>& vill_names)
{
        if (node.isleaf())
                return;
        bool isterm = true;
        for (auto c : node.children()) {
                if (c->isleaf())
                        if (node.active_player() == HERO)
                                hero_names.push_back(c->name());
                        else
                                vill_names.push_back(c->name());
                else
                        isterm = false;
                GetFinalActionNames(*c, hero_names, vill_names);
        }
        if (isterm) {
                if (node.active_player() == HERO)
                        vill_names.push_back(node.name());
                else
                        hero_names.push_back(node.name());
        }
}

void
Node::GetFinalActionProbs(const Node& node,
                          Player player,
                          Array<double>& probs)
{
        for (size_t id = 0; id < probs.num_rows(); id++) {
                size_t idx = 0;
                GetProbsAux(node, id, player, 1.0, idx, probs);
        }
}

} // namespace GTO

