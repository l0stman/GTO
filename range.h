#ifndef GTO_RANGE_H_
#define GTO_RANGE_H_

#include <string>
#include <unordered_set>
#include <vector>

#include "range_interface.h"

namespace GTO {
using std::string;

class Range : public RangeInterface {
public:
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
        virtual string Str() const;
        virtual size_t Size() const { return range_.size(); }

        typedef std::unordered_set<Hand>::const_iterator const_iterator;
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
        std::unordered_set<Hand> range_;
};
}

#endif // !GTO_RANGE_H_
