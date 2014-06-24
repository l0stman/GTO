#ifndef GTO_RANGE_H_
#define GTO_RANGE_H_

#include <functional>
#include <string>
#include <unordered_set>
#include <vector>

#include <pokerstove/peval/Card.h>

#include "err.h"
#include "range_interface.h"
#include "state_interface.h"

namespace GTO {

// Represent hole cards.
class Hand : public CardSet, public StateInterface {
public:
        Hand() : CardSet() {}

        explicit Hand(const std::string& s)
                : CardSet(s)
        {
                if (size() != 2)
                        err::quit("Hand should contain two cards: %s.",
                                  s.c_str());
        }

        explicit Hand(const CardSet& c)
                : CardSet(c)
        {
                if (size() != 2)
                        err::quit("Hand should contain two cards: %s.",
                                  c.str().c_str());
        }

        bool operator<(const Hand& rhs) const
        {
                std::vector<pokerstove::Card> c1 = cards();
                std::vector<pokerstove::Card> c2 = rhs.cards();
                int t1 = (c1[0] < c1[1]) ? 1 : 0;
                int t2 = (c2[0] < c2[1]) ? 1 : 0;

                return c1[t1] < c2[t2] ||
                                (c1[t1] == c2[t2] && c1[1-t1]<c2[1-t2]);
        }

        static std::string Name() { return "Hand"; }
        virtual std::string ToString() const { return str(); }
        virtual short NumCombos() const { return 1; }
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

namespace GTO {
using std::string;
using std::vector;

class Range : public RangeInterface<Hand> {
public:
        typedef std::unordered_set<Hand>::const_iterator const_iterator;

        Range() {};
        explicit Range(const string& s);

        virtual  bool
        IsMember(const Hand& hand) const
        {
                return range_.count(hand) > 0;
        }

        virtual void Add(const Hand& hand) { range_.insert(hand); }
        virtual void Remove(const Hand& hand) { range_.erase(hand); }
        virtual void Fill(const CardSet& dead_cards=CardSet());
        virtual vector<Hand> ToVector(
                const CardSet& dead_cards=CardSet()) const;
        virtual string ToString() const;
        virtual size_t Size() const { return range_.size(); }

        const_iterator begin() const { return range_.begin();};
        const_iterator end() const { return range_.end();};
private:
        std::unordered_set<Hand> range_;
};
} // namespace GTO

#endif // !GTO_RANGE_H_
