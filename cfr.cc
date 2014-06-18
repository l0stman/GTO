#include "cfr.h"

namespace GTO {
Array
Node::AverageStrategy() const
{
        if (isleaf())
                return Array();
        Array avg(strategy_sum_.num_rows(), strategy_sum_.num_cols());
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
          const Player& player,
          const size_t& pid,
          const size_t& oid,
          const double& pprob,
          const double& oprob)
{
        if (node->isleaf())
                return node->Utility(player, pid, oid);
        size_t len = node->children_.size();
        vector<double>& utils = node->utils_;
        double util = 0.0;
        bool isactive = node->active_player_ == player;
        size_t id = isactive ? pid : oid;
        Array& regsum = node->regret_sum_;
        Array& stratsum = node->strategy_sum_;
        Array& strat = node->strategy_;

        for (size_t a=0; a<len; a++) {
                utils[a] = isactive ?
                        CFR(node->children_[a],
                            player,
                            pid,
                            oid,
                            pprob*strat.get(pid, a),
                            oprob) :
                        CFR(node->children_[a],
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
} // namespace GTO

