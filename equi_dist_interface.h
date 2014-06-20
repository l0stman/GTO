#ifndef GTO_EQUI_DIST_INTERFACE_H_
#define GTO_EQUI_DIST_INTERFACE_H_

#include <vector>
#include "array.h"

namespace GTO {

// Interface representing the equity distribution between two players
// playing heads-up.
template<class Hand>
class EquiDistInterface {
public:
        // Return the equity of hero's hand against villain's or -1 if
        // it doesn't exist.
        virtual double Equity(const Hand& hero, const Hand& villain) const = 0;

        // Return a lookup table represented as an array such that
        // Array.get(i, j) == Equity(hands1[i], hands2[j]).
        virtual Array LUT(const std::vector<Hand>& hand1,
                          const std::vector<Hand>& hand2) const = 0;
};
} // namespace GTO

#endif  // !GTO_EQUI_DIST_INTERFACE_H_
