#include "cfr.h"

namespace GTO {
Array Node::AverageStrategy() const
{
        if (IsLeaf())
                return Array();
        Array avg(strategy_sum_.NumRows(), strategy_sum_.NumCols());
        for (size_t s=0; s<avg.NumRows(); s++) {
                double norm = 0;
                for (size_t a=0; a<avg.NumCols(); a++)
                        norm += strategy_sum_.Get(s, a);
                for (size_t a=0; a<avg.NumCols(); a++)
                        if (norm > 0)
                                avg.Set(s, a, strategy_sum_.Get(s, a)/norm);
                        else
                                avg.Set(s, a, 1.0/avg.NumCols());
                        }
        return avg;
}

double Node::CFR(Node* node,
                 const Player& player,
                 const size_t& pid,
                 const size_t& oid,
                 const double& pprob,
                 const double& oprob)
{
        if (node->IsLeaf())
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
                            pprob*strat.Get(pid, a),
                            oprob) :
                        CFR(node->children_[a],
                            player,
                            pid,
                            oid,
                            pprob,
                            oprob*strat.Get(oid, a));
                util += utils[a] * strat.Get(id, a);
        }
        if (isactive) {
                for (size_t a=0; a<len; a++) {
                        regsum.Inc(pid, a, oprob*(utils[a]-util));
                        stratsum.Inc(pid, a, pprob*strat.Get(pid, a));
                }
                double norm = 0.0;
                for (size_t a=0; a<len; a++) {
                        double rs = regsum.Get(pid, a);
                        strat.Set(pid, a, rs > 0 ? rs : 0.0);
                        norm += strat.Get(pid, a);
                }
                for (size_t a=0; a<len; a++)
                        strat.Set(pid,
                                  a,
                                  norm > 0 ? strat.Get(pid, a)/norm : 1.0/len);
        }
        return util;
}
}
