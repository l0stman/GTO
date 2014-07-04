#ifndef GTO_PREFLOP_RANGE_H_
#define GTO_PREFLOP_RANGE_H_

#include <functional>
#include <unordered_set>

#include "range_interface.h"
#include "state_interface.h"

namespace GTO {

// Represents a pocket hand in pre-flop.  Suit doesn't matter so the
// number of hand combination is reduced from 1329 to 169.  We
// distinguish only between pocket pairs, offsuit hand and suited
// hand.
class PreflopHand : public StateInterface {
public:
        explicit PreflopHand(const std::string& s)
                : hand_(Init(s)),
                  suit_combos_(s.size() == 2 ? 6 : (s[2] == 's' ? 4 : 12))
        {}
        ~PreflopHand() {}

        // Return the number of suit combos represented by the hand.
        unsigned short suit_combos() const { return suit_combos_; }

        bool
        operator==(const GTO::PreflopHand& rhs) const
        {
                return hand_ == rhs.hand_;
        }

        static std::string Name() { return "Hand"; }
        virtual std::string ToString() const { return hand_; }
        virtual unsigned short NumCombos() const { return suit_combos_; }
private:
        std::string Init(const std::string& s);
        const std::string hand_;
        const short suit_combos_;
};

} // namespace GTO

namespace std {

template<>
struct hash<GTO::PreflopHand> {
        size_t
        operator()(const GTO::PreflopHand& h) const
        {
                return hash<string>()(h.ToString());
        }
};

} // namespace std

namespace GTO {
using pokerstove::CardSet;

// Implements a range of hands in pre-flop.
class PreflopRange : RangeInterface<PreflopHand> {
public:
        typedef std::unordered_set<PreflopHand>::const_iterator const_iterator;

        PreflopRange() {}
        explicit PreflopRange(const std::string& s);
        ~PreflopRange() {}

        virtual bool
        IsMember(const PreflopHand& hand) const
        {
                return range_.count(hand) > 0;
        }

        virtual void Add(const PreflopHand& hand) { range_.insert(hand); }
        virtual void Remove(const PreflopHand& hand) { range_.insert(hand); }
        virtual void Fill(const CardSet& dead_cards=CardSet());
        virtual std::vector<PreflopHand> ToVector(
                const CardSet& dead_cards=CardSet()) const;
        virtual std::string ToString() const;
        virtual size_t Size() const { return range_.size(); }

        const_iterator begin() const { return range_.begin(); }
        const_iterator end() const { return range_.end(); }

private:
        std::unordered_set<PreflopHand> range_;
};

}

#endif  // !GTO_PREFLOP_RANGE_H_
