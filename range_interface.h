#ifndef GTO_RANGE_INTERFACE_H_
#define GTO_RANGE_INTERFACE_H_

#include <functional>
#include <string>

#include <pokerstove/peval/Card.h>
#include <pokerstove/peval/CardSet.h>

#include "err.h"

namespace GTO {
using pokerstove::CardSet;
using std::vector;

// Represent hole cards.
class Hand : public CardSet {
public:
        Hand() : CardSet() {}

        explicit Hand(const std::string& s)
                : CardSet(s)
        {
                if (size() != 2)
                        err::quit("Hand should contain two cards: %s.",
                                  s.c_str());
        }

        bool operator<(const Hand& rhs) const
        {
                vector<pokerstove::Card> c1 = cards();
                vector<pokerstove::Card> c2 = rhs.cards();
                int t1 = (c1[0] < c1[1]) ? 1 : 0;
                int t2 = (c2[0] < c2[1]) ? 1 : 0;

                return c1[t1] < c2[t2] ||
                                (c1[t1] == c2[t2] && c1[1-t1]<c2[1-t2]);
        }

        std::string ToString() const { return str(); }
};

class RangeInterface {
public:
        // Test if "hand" is a member of the range.
        virtual bool IsMember(const Hand& hand) const = 0;

        // Add "hand" to the range.
        virtual void Add(const Hand& hand) = 0;

        // Remove "hand" from the range.
        virtual void Remove(const Hand& hand) = 0;

        // Add all possible hands to the range except those that
        // conflict with "dead_cards".
        virtual void Fill(const CardSet& dead_cards=CardSet()) = 0;

        // Return a vector of all the hands in the range that don't
        // conflict with "dead_cards".
        virtual vector<Hand> ToVector(
                const CardSet& dead_cards=CardSet()) const = 0;

        // Return the number of hands in the range.
        virtual size_t Size() const = 0;

        // Return a string representing the range.
        virtual std::string ToString() const = 0;
};
} // namespace GTO

namespace std {
template<>
struct hash<GTO::Hand> {
        size_t
        operator()(const GTO::Hand& h) const
        {
                return hash<uint64_t>()(h.mask());
        }
};
} // namespace std

#endif  // !GTO_RANGE_INTERFACE_H_
