#ifndef GTO_RANGE_INTERFACE_H_
#define GTO_RANGE_INTERFACE_H_

#include <string>
#include <pokerstove/peval/CardSet.h>

namespace GTO {
using pokerstove::CardSet;

// Interface representing the range of hands a player can hold.
template<class Hand>
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
        virtual std::vector<Hand> ToVector(
                const CardSet& dead_cards=CardSet()) const = 0;

        // Return the number of hands in the range.
        virtual size_t Size() const = 0;

        // Return a string representing the range.
        virtual std::string ToString() const = 0;
};
} // namespace GTO

#endif  // !GTO_RANGE_INTERFACE_H_
