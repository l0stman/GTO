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
using std::vector;

class Range {
public:
        Range() {};
        explicit Range(const string& s);
        // Build a range from the hands between [min, max).
        explicit Range(const vector<CardSet>& hands,
                       const size_t& min,
                       const size_t& max);
        virtual bool IsMember(const CardSet& hand) const;
        virtual void Add(const CardSet& hand);
        virtual void Remove(const CardSet& hand);
        virtual void Fill();
        virtual vector<CardSet> ToVector(const CardSet& board=CardSet()) const;

        string Str() const;
        size_t Size() const { return range_.size();};

        struct CSCmp {
                bool
                operator()(const CardSet& h1, const CardSet& h2) const
                {
                        vector<pokerstove::Card> c1 = h1.cards();
                        vector<pokerstove::Card> c2 = h2.cards();
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
        void AddSuited(const string& s, const size_t& pos);
        void AddOffsuit(const string& s, const size_t& pos);
        void AddSuitedPlus(const string& s, const size_t& pos);
        void AddOffsuitPlus(const string& s, const size_t& pos);
        void AddSuitedRange(const string& s, const size_t& pos);
        void AddOffsuitRange(const string& s, const size_t& pos);
        void AddPairsRange(const string& s, const size_t& pos);
        void AddPairsPlus(const string& s, const size_t& pos);
        void AddSingleSuitRange(const string& s, const size_t& pos);
        void AddSingleSuitPlus(const string& s, const size_t& pos);
        unordered_set<CardSet, CSHash> range_;
};
}

#endif // !GTO_RANGE_H_
