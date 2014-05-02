#ifndef GTO_RANGE_H_
#define GTO_RANGE_H_

#include <functional>
#include <string>
#include <unordered_set>
#include <vector>

#include <pokerstove/peval/Card.h>
#include <pokerstove/peval/CardSet.h>

namespace GTO {
using pokerstove::CardSet;
using std::unordered_set;
using std::string;

class Range {
public:
        explicit Range(const string& s);
        // Build a range from the hands between [min, max).
        explicit Range(const std::vector<CardSet>& hands,
                       const size_t& min,
                       const size_t& max);
        virtual bool IsMember(const CardSet& hand) const;
        virtual void Add(const CardSet& hand);
        virtual void Remove(const CardSet& hand);

        string str() const;
        size_t size() const { return range_.size();};

        struct CSCmp {
                bool
                operator()(const CardSet& h1, const CardSet& h2) const
                {
                        std::vector<pokerstove::Card> c1 = h1.cards();
                        std::vector<pokerstove::Card> c2 = h2.cards();
                        int t1 = (c1[0] < c1[1]) ? 1 : 0;
                        int t2 = (c2[0] < c2[1]) ? 1 : 0;

                        return c1[t1] < c2[t2] ||
                                        (c1[t1] == c2[t2] && c1[1-t1]<c2[1-t2]);
                }
        };

        struct CSHash {
                size_t
                operator()(const CardSet& h) const
                {
                        return std::hash<uint64_t>()(h.mask());
                }
        };

        typedef unordered_set<CardSet, CSHash>::const_iterator const_iterator;
        const_iterator begin() const { return range_.begin();};
        const_iterator end() const { return range_.end();};
private:
        void AddSuited(const string& s, size_t pos);
        void AddOffsuit(const string& s, size_t pos);

        const string kRanks_ = "23456789TJQKA";
        const string kSuits_ = "cdhs";
        unordered_set<CardSet, CSHash> range_;
};
}

#endif // !GTO_RANGE_H_
