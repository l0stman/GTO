#include "range.h"

#include <algorithm>
#include <cctype>
#include <cstdio>
#include <cstdlib>
#include <set>

#include "range_utils.h"

namespace {

using std::string;
typedef std::unordered_set<GTO::Hand> Table;

void
AddSuited(const string& s, const size_t& pos, Table& range)
{
        string h(4, 'x');
        h[0] = s[pos];
        h[2] = s[pos+1];

        if (s[pos] == s[pos+1])
                GTO::FmtError(s.substr(pos, 3));
        for (auto s : GTO::kSuits) {
                h[1] = s;
                h[3] = s;
                range.insert(GTO::Hand(h));
        }
}

void
AddOffsuit(const string& s, const size_t& pos, Table& range)
{
        string h(4, 'x');
        h[0] = s[pos];
        h[2] = s[pos+1];

        for (auto s1 : GTO::kSuits)
                for (auto s2 : GTO::kSuits)
                        if (s1 != s2) {
                                h[1] = s1;
                                h[3] = s2;
                                range.insert(GTO::Hand(h));
                        }
}

void
AddSuitedPlus(const string& s, const size_t& pos, Table& range)
{
        string h(4, 'x');
        size_t min = 0;
        size_t max = 0;

        GTO::ParseRanks(s, pos, min, max);
        h[0] = GTO::kRanks[max];
        for (size_t i = min; i < max; ++i) {
                h[2] = GTO::kRanks[i];
                for (auto s : GTO::kSuits) {
                        h[1] = s;
                        h[3] = s;
                        range.insert(GTO::Hand(h));
                }
        }
}

void
AddOffsuitPlus(const string& s, const size_t& pos, Table& range)
{
        string h(4, 'x');
        size_t min = 0;
        size_t max = 0;

        GTO::ParseRanks(s, pos, min, max);
        h[0] = GTO::kRanks[max];
        for (size_t i = min; i < max; ++i) {
                h[2] = GTO::kRanks[i];
                for (auto s1 : GTO::kSuits)
                        for (auto s2 : GTO::kSuits)
                                if (s1 != s2) {
                                        h[1] = s1;
                                        h[3] = s2;
                                        range.insert(GTO::Hand(h));
                                }
        }
}

void
AddSuitedRange(const string& s, const size_t& pos, Table& range)
{
        string h(4, 'x');
        size_t min1, min2, max1, max2;

        GTO::ParseRanks(s, pos, min1, max1);
        GTO::ParseRanks(s, pos+4, min2, max2);
        if (max1 != max2)
                GTO::FmtError(s.substr(pos, 7));
        h[0] = GTO::kRanks[max1];
        std::pair<size_t,size_t> p = std::minmax(min1, min2);
        for (size_t i = p.first; i<=p.second; ++i) {
                h[2] = GTO::kRanks[i];
                for (auto s : GTO::kSuits) {
                        h[1] = s;
                        h[3] = s;
                        range.insert(GTO::Hand(h));
                }
        }
}

void
AddOffsuitRange(const string& s, const size_t& pos, Table& range)
{
        string h(4, 'x');
        size_t min1, min2, max1, max2;

        GTO::ParseRanks(s, pos, min1, max1);
        GTO::ParseRanks(s, pos+4, min2, max2);
        if (max1 != max2)
                GTO::FmtError(s.substr(pos, 7));
        h[0] = GTO::kRanks[max1];
        std::pair<size_t,size_t> p = std::minmax(min1, min2);
        for (size_t i = p.first; i<=p.second; ++i) {
                h[2] = GTO::kRanks[i];
                for (auto s1 : GTO::kSuits)
                        for (auto s2 : GTO::kSuits)
                                if (s1 != s2) {
                                        h[1] = s1;
                                        h[3] = s2;
                                        range.insert(GTO::Hand(h));
                                }
        }
}

void
AddPairsRange(const string& s, const size_t& pos, Table& range)
{
        string h(2, 'x');
        std::pair<size_t,size_t> p = std::minmax(
                GTO::GetRank(s[pos]), GTO::GetRank(s[pos+3]));

        for (size_t i = p.first; i <= p.second; i++) {
                h[0] = GTO::kRanks[i];
                h[1] = GTO::kRanks[i];
                AddOffsuit(h, 0, range);
        }
}

void
AddPairsPlus(const string& s, const size_t& pos, Table& range)
{
        string h(2, 'x');
        size_t r = GTO::GetRank('A');

        for (size_t i = GTO::GetRank(s[pos]); i<=r; ++i) {
                h[0] = GTO::kRanks[i];
                h[1] = GTO::kRanks[i];
                AddOffsuit(h, 0, range);
        }
}

void
AddSingleSuitRange(const string& s, const size_t& pos, Table& range)
{
        string h(4, 'x');
        size_t r1 = GTO::GetRank(s[pos]);
        size_t r2 = GTO::GetRank(s[pos+2]);
        size_t r3 = GTO::GetRank(s[pos+5]);
        size_t r4 = GTO::GetRank(s[pos+7]);
        size_t max = std::max(r1, r2);
        std::pair<size_t,size_t> p = std::minmax(
                std::min(r1, r2), std::min(r3, r4));

        if (max != std::max(r3, r4))
                GTO::FmtError(s.substr(pos, 9));
        h[0] = GTO::kRanks[max];
        h[1] = s[1];
        h[3] = s[1];
        for (size_t i = p.first; i<=p.second; ++i) {
                h[2] = GTO::kRanks[i];
                range.insert(GTO::Hand(h));
        }
}

void
AddSingleSuitPlus(const string& s, const size_t& pos, Table& range)
{
        string h(4, 'x');
        std::pair<size_t,size_t> p = std::minmax(
                GTO::GetRank(s[pos]), GTO::GetRank(s[pos+2]));

        h[0] = GTO::kRanks[p.second];
        h[1] = s[pos+1];
        h[3] = s[pos+1];
        for (size_t i = p.first; i < p.second; ++i) {
                h[2] = GTO::kRanks[i];
                range.insert(GTO::Hand(h));
        }
}
} // namespace

namespace GTO {

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
                if ((last == pos || s[last] == ',') && first < last) {
                        size_t len = last-first;
                        switch (len) {
                        case 2:
                                if (!IsHand(s, first))
                                        FmtError(s.substr(first, len));
                                if (s[first] != s[first+1])
                                        AddSuited(s, first, range_);
                                AddOffsuit(s, first, range_);
                                break;
                        case 3:
                                if (!IsHand(s, first))
                                        FmtError(s.substr(first, len));
                                switch (s[first+2]) {
                                case 's':
                                        AddSuited(s, first, range_);
                                        break;
                                case 'o':
                                        AddOffsuit(s, first, range_);
                                        break;
                                case '+':
                                        if (s[first] == s[first+1])
                                                AddPairsPlus(s, first, range_);
                                        else
                                                FmtError(s.substr(first, len));
                                        break;
                                default:
                                        FmtError(s.substr(first, len));
                                }
                                break;
                        case 4:
                                if (!IsRank(s[first]))
                                        FmtError(s.substr(first, len));
                                if (s[first+2] == 's' && s[first+3] == '+' &&
                                    IsRank(s[first+1]))
                                        AddSuitedPlus(s, first, range_);
                                else if (s[first+2] == 'o' &&
                                         s[first+3] =='+' &&
                                         IsRank(s[first+1]))
                                        AddOffsuitPlus(s, first, range_);
                                else if (IsRank(s[first+2]) &&
                                         IsSuit(s[first+1]) &&
                                         IsSuit(s[first+3]))
                                        range_.insert(
                                                Hand(s.substr(first, len)));
                                else
                                        FmtError(s.substr(first, len));
                                break;
                        case 5:
                                if (IsRank(s[first]) &&
                                    s[first] == s[first+1] &&
                                    s[first+2] == '-' &&
                                    IsRank(s[first+3]) &&
                                    s[first+3] == s[first+4])
                                        AddPairsRange(s, first, range_);
                                else if (IsRank(s[first]) &&
                                         IsRank(s[first+2]) &&
                                         IsSuit(s[first+1]) &&
                                         s[first+1] == s[first+3] &&
                                         s[first+4] == '+')
                                        AddSingleSuitPlus(s, first, range_);
                                else
                                        FmtError(s.substr(first, len));
                                break;
                        case 7:
                                if (!IsHand(s, first) || !IsHand(s, first+4))
                                        FmtError(s.substr(first, len));
                                if (s[first+2] == 's' && s[first+3] == '-' &&
                                    s[first+6] == 's')
                                        AddSuitedRange(s, first, range_);
                                else if (s[first+2] == 'o' &&
                                         s[first+3] == '-' &&
                                         s[first+6] == 'o')
                                        AddOffsuitRange(s, first, range_);
                                else
                                        FmtError(s.substr(first, len));
                                break;
                        case 9:
                                c = s[first+1];
                                if (IsRank(s[first]) && IsRank(s[first+2]) &&
                                    IsRank(s[first+5]) && IsRank(s[first+7]) &&
                                    IsSuit(c) && s[first+3] == c &&
                                    s[first+6] == c && s[first+8] == c &&
                                    s[first+4] == '-')
                                        AddSingleSuitRange(s, first, range_);
                                else
                                        FmtError(s.substr(first, len));
                                break;
                        default:
                                FmtError(s.substr(first, len));
                        }
                        first = last+1;
                }
}

void
Range::Fill(const CardSet& dead_cards)
{
        string c1(2, 'x');
        string c2(2, 'x');

        for (auto r1 : kRanks)
                for (auto s1 : kSuits) {
                        c1[0] = r1;
                        c1[1] = s1;
                        for (auto r2 : kRanks)
                                for (auto s2 : kSuits) {
                                        Hand hand(c1);
                                        c2[0] = r2;
                                        c2[1] = s2;
                                        CardSet C(c2);

                                        if (hand.disjoint(C)) {
                                                hand.insert(C);
                                                if (hand.disjoint(dead_cards))
                                                        Add(hand);
                                         }
                                }
                }
}

vector<Hand>
Range::ToVector(const CardSet& board) const
{
        vector<Hand> hands;
        hands.reserve(Size());
        for (auto hand : *this)
                if (board.disjoint(hand))
                        hands.push_back(hand);
        return hands;
}

string
Range::ToString() const
{
        string s;
        std::set<Hand> r(range_.begin(), range_.end());
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
} // namespace GTO
