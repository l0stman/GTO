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

inline void
ParseRanks(const string& s,
           const size_t& pos,
           size_t& min,
           size_t& max)
{
        size_t r1 = GetRank(s[pos]);
        size_t r2 = GetRank(s[pos+1]);
        min = std::min(r1, r2);
        max = std::max(r1, r2);
}
}

namespace GTO {
void
Range::AddSuited(const string& s, const size_t& pos)
{
        string h(4, 'x');
        h[0] = s[pos];
        h[2] = s[pos+1];

        if (s[pos] == s[pos+1])
                FmtError(s.substr(pos, 3));
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
        size_t min = 0;
        size_t max = 0;

        ParseRanks(s, pos, min, max);
        h[0] = kRanks_[max];
        for (size_t i = min; i < max; ++i) {
                h[2] = kRanks_[i];
                for (size_t j = 0; j < kSuits_.length(); ++j) {
                        h[1] = kSuits_[j];
                        h[3] = kSuits_[j];
                        range_.insert(CardSet(h));
                }
        }
}

void
Range::AddOffsuitPlus(const string& s, const size_t& pos)
{
        string h(4, 'x');
        size_t min = 0;
        size_t max = 0;

        ParseRanks(s, pos, min, max);
        h[0] = kRanks_[max];
        for (size_t i = min; i < max; ++i) {
                h[2] = kRanks_[i];
                for (size_t j = 0; j < kSuits_.length(); ++j)
                        for (size_t k = 0; k < kSuits_.length(); ++k)
                                if (j != k) {
                                        h[1] = kSuits_[j];
                                        h[3] = kSuits_[k];
                                        range_.insert(CardSet(h));
                                }
        }
}

void
Range::AddSuitedRange(const string& s, const size_t& pos)
{
        string h(4, 'x');
        size_t min1, min2, max1, max2;

        ParseRanks(s, pos, min1, max1);
        ParseRanks(s, pos+4, min2, max2);
        if (max1 != max2)
                FmtError(s.substr(pos, 7));
        h[0] = kRanks_[max1];
        max1 = std::min(min1, min2);
        max2 = std::max(min1, min2);
        for (size_t i = max1; i<=max2; ++i) {
                h[2] = kRanks_[i];
                for (size_t j = 0; j < kSuits_.length(); ++j) {
                        h[1] = kSuits_[j];
                        h[3] = kSuits_[j];
                        range_.insert(CardSet(h));
                }
        }
}

void
Range::AddOffsuitRange(const string& s, const size_t& pos)
{
        string h(4, 'x');
        size_t min1, min2, max1, max2;

        ParseRanks(s, pos, min1, max1);
        ParseRanks(s, pos+4, min2, max2);
        if (max1 != max2)
                FmtError(s.substr(pos, 7));
        h[0] = kRanks_[max1];
        max1 = std::min(min1, min2);
        max2 = std::max(min1, min2);
        for (size_t i = max1; i<=max2; ++i) {
                h[2] = kRanks_[i];
                for (size_t j = 0; j < kSuits_.length(); ++j)
                        for (size_t k = 0; k < kSuits_.length(); ++k)
                                if (j != k) {
                                        h[1] = kSuits_[j];
                                        h[3] = kSuits_[k];
                                        range_.insert(CardSet(h));
                                }
        }
}

void
Range::AddPocketsRange(const string& s, const size_t& pos)
{
        string h(2, 'x');
        size_t r1 = GetRank(s[pos]);
        size_t r2 = GetRank(s[pos+3]);
        size_t min = std::min(r1, r2);
        size_t max = std::max(r1, r2);

        for (size_t i = min; i < max; i++) {
                h[0] = kRanks_[i];
                h[1] = kRanks_[i];
                AddOffsuit(h, 0);
        }
}

void
Range::AddSingleSuitRange(const string& s, const size_t& pos)
{
        string h(4, 'x');
        size_t r1 = GetRank(s[pos]);
        size_t r2 = GetRank(s[pos+2]);
        size_t r3 = GetRank(s[pos+5]);
        size_t r4 = GetRank(s[pos+7]);
        size_t max = std::max(r1, r2);
        size_t min1 = std::min(std::min(r1, r2), std::min(r3, r4));
        size_t min2 = std::max(std::min(r1, r2), std::min(r3, r4));

        if (max != std::max(r3, r4))
                FmtError(s.substr(pos, 9));
        h[0] = kRanks_[max];
        h[1] = s[1];
        h[3] = s[1];
        for (size_t i = min1; i<=min2; ++i) {
                h[2] = kRanks_[i];
                range_.insert(h);
        }
}

void
Range::AddSingleSuitPlus(const string& s, const size_t& pos)
{
        string h(4, 'x');
        size_t r1 = GetRank(s[pos]);
        size_t r2 = GetRank(s[pos+2]);
        size_t min = std::min(r1, r2);
        size_t max = std::max(r1, r2);

        h[0] = kRanks_[max];
        h[1] = s[pos+1];
        h[3] = s[pos+1];
        for (size_t i = min; i < max; ++i) {
                h[2] = kRanks_[i];
                range_.insert(h);
        }
}

Range::Range(const string& in)
{
        string s(in);
        size_t pos = 0;
        char c;
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
                                else if (s[first+2] == 'o' && s[first+3] == '+')
                                        AddOffsuitPlus(s, first);
                                else
                                        range_.insert(
                                                CardSet(s.substr(first, len)));
                                break;
                        case 5:
                                if (s[first] == s[first+1] &&
                                    s[first+2] == '-' &&
                                    s[first+3] == s[first+4])
                                        AddPocketsRange(s, first);
                                else if (s[first+4] == '+') {
                                        c = s[first+1];
                                        if ((c != 'c' && c != 's' &&
                                             c != 'd' && c != 'h') ||
                                            (c != s[first+3]))
                                                FmtError(s.substr(first, len));
                                        AddSingleSuitPlus(s, first);
                                } else
                                        FmtError(s.substr(first, len));
                                break;
                        case 7:
                                if (s[first+2] == 's' && s[first+3] == '-' &&
                                    s[first+6] == 's')
                                        AddSuitedRange(s, first);
                                else if (s[first+2] == 'o' &&
                                         s[first+3] == '-' &&
                                         s[first+6] == 'o')
                                        AddOffsuitRange(s, first);
                                else
                                        FmtError(s.substr(first, len));
                                break;
                        case 9:
                                c = s[first+1];
                                if (c != 'c' && c != 'h' && c != 'd' && c !='s')
                                        FmtError(s.substr(first, len));
                                if (s[first+3] == c && s[first+6] == c &&
                                    s[first+8] == c && s[first+4] == '-')
                                        AddSingleSuitRange(s, first);
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
