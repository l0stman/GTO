#include "range.h"

#include <cctype>
#include <cstdio>
#include <cstdlib>
#include <set>

namespace {
using std::string;

const string kRanks_ = "23456789TJQKA";
const string kSuits_ = "cdhs";

void
FmtError(const string& s)
{
        fprintf(stderr, "Unknown range format: %s\n", s.c_str());
        exit(1);
}

inline size_t
GetRank(const char& c)
{
        size_t r = kRanks_.find(c);
        if (r == string::npos) {
                fprintf(stderr, "Unknown rank: %c\n", c);
                exit(1);
        }
        return r;
}
}

namespace GTO {
void
Range::AddSuited(const string& s, const size_t& pos)
{
        string h(4, 'x');
        h[0] = s[pos];
        h[2] = s[pos+1];

        for (size_t i = 0; i < kSuits_.length(); i++) {
                h[1] = kSuits_[i];
                h[3] = kSuits_[i];
                range_.insert(CardSet(h));
        }
}

void
Range::AddOffsuit(const string& s, const size_t& pos)
{
        string h(4, 'x');
        h[0] = s[pos];
        h[2] = s[pos+1];

        for (size_t i = 0; i < kSuits_.length(); i++)
                for (size_t j = 0; j < kSuits_.length(); j++)
                        if (i != j) {
                                h[1] = kSuits_[i];
                                h[3] = kSuits_[j];
                                range_.insert(CardSet(h));
                        }
}

void
Range::AddSuitedPlus(const string& s, const size_t& pos)
{
        string h(4, 'x');
        size_t r1 = GetRank(s[pos]);
        size_t r2 = GetRank(s[pos+1]);
        size_t min = r1 < r2 ? r1 : r2;
        size_t max = r1 < r2 ? r2 : r1;

        h[0] = kRanks_[max];
        for (r1 = min; r1 < max; ++r1) {
                h[2] = kRanks_[r1];
                for (r2 = 0; r2 < kSuits_.length(); ++r2) {
                        h[1] = kSuits_[r2];
                        h[3] = kSuits_[r2];
                        range_.insert(CardSet(h));
                }
        }
}

Range::Range(const string& in)
{
        string s(in);
        size_t pos = 0;
        // Delete spaces from the input string.
        for (size_t i = 0; i < s.length(); i++)
                if (!isspace(s[i]))
                        s[pos++] = s[i];
        size_t first = 0;
        for (size_t last = 0; last <= pos; last++)
                if ((s[last] == ',' || last == pos) && first < last) {
                        size_t len = last-first;
                        switch (len) {
                        case 2:
                                if (s[first] != s[first+1])
                                        AddSuited(s, first);
                                AddOffsuit(s, first);
                                break;
                        case 3:
                                switch (s[first+2]) {
                                case 's':
                                        AddSuited(s, first);
                                        break;
                                case 'o':
                                        AddOffsuit(s, first);
                                        break;
                                default:
                                        FmtError(s.substr(first, len));
                                }
                                break;
                        case 4:
                                if (s[first+2] == 's' && s[first+3] == '+')
                                        AddSuitedPlus(s, first);
                                else
                                        range_.insert(
                                                CardSet(s.substr(first, len)));
                                break;
                        default:
                                FmtError(s.substr(first, len));
                        }
                        first = last+1;
                }
}

Range::Range(const std::vector<CardSet>& hands,
             const size_t& min,
             const size_t& max)
{
        for (size_t i = min; i < max; i++)
                range_.insert(hands[i]);
}

bool
Range::IsMember(const CardSet& hand) const
{
        return range_.count(hand) > 0;
}

void
Range::Add(const CardSet& hand)
{
        range_.insert(hand);
}

void
Range::Remove(const CardSet& hand)
{
        range_.erase(hand);
}

void
Range::Fill()
{
        string c1(2, 'x');
        string c2(2, 'x');

        for (size_t r1 = 0; r1 < kRanks_.length(); r1++)
                for (size_t s1 = 0; s1 < kSuits_.length(); s1++) {
                        c1[0] = kRanks_[r1];
                        c1[1] = kSuits_[s1];
                        for (size_t r2 = 0; r2 < kRanks_.length(); r2++)
                                for (size_t s2 = 0; s2<kSuits_.length(); s2++) {
                                        CardSet hand(c1);
                                        c2[0] = kRanks_[r2];
                                        c2[1] = kSuits_[s2];
                                        CardSet C(c2);

                                        if (hand.disjoint(C)) {
                                                hand.insert(C);
                                                Add(hand);
                                         }
                                }
                }
}

string
Range::str() const
{
        string s;
        std::set<CardSet,CSCmp> r(range_.begin(), range_.end());
        auto it = r.begin();

        if (it != r.end())
                for (;;) {
                        s += it->str();
                        if (++it == r.end())
                                break;
                        s += ",";
                }
        return s;
}
}
