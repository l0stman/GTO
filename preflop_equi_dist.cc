#include "err.h"
#include "preflop_equi_dist.h"

namespace GTO {

PreflopEquiDist::PreflopEquiDist()
{
        FILE *fp;
        char h[4];
        char v[4];
        double EQh = 0;
        double EQv = 0;

        if ((fp = fopen(preflop_equity_file_, "r")) == NULL)
                err::sys("Can't open %s", preflop_equity_file_);
        while (fscanf(fp, "%s vs. %s : %lf vs. %lf", h, v, &EQh, &EQv)!=EOF) {
                set_equity(h, v, EQh);
                set_equity(v, h, EQv);
                set_equity(h, h, 0.5);
                set_equity(v, v, 0.5);
        }
        fclose(fp);
}

Array<double>
PreflopEquiDist::LUT(const std::vector<PreflopHand>& hands1,
                     const std::vector<PreflopHand>& hands2) const
{
        Array<double> equity(hands1.size(), hands2.size());
        for (size_t i = 0; i < hands1.size(); i++)
                for (size_t j = 0; j < hands2.size(); j++)
                        equity.set(i, j, Equity(hands1[i], hands2[j]));
        return equity;
}

SuitCombos::SuitCombos()
{
        FILE *fp;
        char h[4];
        char v[4];
        short num;

        if ((fp = fopen(preflop_combos_file_, "r")) == NULL)
                err::sys("Can't open %s", preflop_combos_file_);
        while (fscanf(fp, "%s %s %hd", h, v, &num) != EOF)
                set_combos(h, v, num);
}

Array<short>
SuitCombos::LUT(const std::vector<PreflopHand>& hands1,
                const std::vector<PreflopHand>& hands2) const
{
        Array<short> combos(hands1.size(), hands2.size());
        for (size_t i = 0; i < hands1.size(); i++)
                for (size_t j = 0; j < hands2.size(); j++)
                        combos.set(i, j, NumCombos(hands1[i], hands2[j]));
        return combos;
}
} // namespace GTO
