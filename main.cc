#include "range.h"
#include "equi_dist.h"

#include <cstdio>
#include <utility>

int
main(int argc, char *argv[])
{
        GTO::Range hero(argv[1]);
        GTO::Range villain(argv[2]);
        pokerstove::CardSet board(argc > 3 ? argv[3] : "");
        GTO::EquiDist eq(hero, villain, board);

        fprintf(stderr, "board: %s\n", board.str().c_str());
        fprintf(stderr, "Hero distribution:\n");
        for (auto it = hero.begin(); it != hero.end(); it++) {
                double e = eq.HeroEquity(*it);
                if (e > 0)
                        fprintf(stderr, "%s: %.3f\n", it->str().c_str(), e);
        }
        fprintf(stderr, "Villain distribution:\n");
        for (auto it = villain.begin(); it != villain.end(); it++) {
                double e = eq.VillEquity(*it);
                if (e > 0)
                        fprintf(stderr, "%s: %.3f\n", it->str().c_str(), e);
        }
        fprintf(stderr, "Hero: %.3f, Villain: %.3f\n", eq.HeroEquity(),
                eq.VillEquity());
        for (auto hit = hero.begin(); hit != hero.end(); hit++)
                for (auto vit = villain.begin(); vit != villain.end(); vit++) {
                        double e = eq.Equity(*hit, *vit);
                        if (e > 0)
                                fprintf(stderr, "%s vs %s: %.3f\n",
                                        hit->str().c_str(),
                                        vit->str().c_str(),
                                        e);
                }
        return 0;
}
