#include "preflop_range.h"

#include <cassert>
#include <cctype>

#include <algorithm>
#include <string>
#include <utility>

#include "err.h"
#include "range_utils.h"

namespace GTO {
using std::string;

string
PreflopHand::Init(const string& s)
{
        if (s.size() < 2 || s.size() > 3 || !IsRank(s[0]) || !IsRank(s[1]) ||
            (s.size() == 2 && s[0] != s[1]) ||
            (s.size() == 3 && s[2] != 's' && s[2] != 'o'))
                err::quit("Unknown preflop hand: %s.", s.c_str());
        return s;
}

PreflopRange::PreflopRange(const string& in)
{
        string s(in);
        size_t pos = 0;
        std::pair<size_t, size_t> p;

        // Delete spaces from the input string.
        for (char c : s)
                if (!isspace(c))
                        s[pos++] = c;
        size_t first = 0;
        for (size_t last = 0; last <= pos; last++)
                if ((last == pos || s[last] == ',') && first < last) {
                        size_t len = last-first;
                        string fmt = s.substr(first, len);
                        if (!IsHand(fmt, 0))
                                FmtError(fmt);
                        switch (len) {
                        case 2:
                                if (fmt[0] == fmt[1])
                                        range_.insert(PreflopHand(fmt));
                                else {
                                        fmt.push_back('s');
                                        range_.insert(PreflopHand(fmt));
                                        fmt[2] = 'o';
                                        range_.insert(PreflopHand(fmt));
                                }
                                break;
                        case 3:
                                switch (fmt[2]) {
                                case 's':
                                case 'o':
                                        if (fmt[0] == fmt[1])
                                                FmtError(fmt);
                                        range_.insert(PreflopHand(fmt));
                                        break;
                                case '+':
                                        if (fmt[0] != fmt[1])
                                                FmtError(fmt);
                                        fmt.pop_back();
                                        for (size_t r = GetRank(fmt[0]);
                                             r <= GetRank('A');
                                             r++) {
                                                fmt[0] = kRanks[r];
                                                fmt[1] = kRanks[r];
                                                range_.insert(PreflopHand(fmt));
                                        }
                                        break;
                                default:
                                        FmtError(fmt);
                                        break;
                                }
                                break;
                        case 4:
                                if (fmt[3] != '+' ||
                                    (fmt[2] != 's' && fmt[2] != 'o') ||
                                    fmt[0] == fmt[1])
                                        FmtError(fmt);
                                fmt.pop_back();
                                size_t min, max;
                                ParseRanks(fmt, 0, min, max);
                                fmt[0] = kRanks[max];
                                for (size_t r = min; r < max; r++) {
                                        fmt[1] = kRanks[r];
                                        range_.insert(PreflopHand(fmt));
                                }
                                break;
                        case 5:
                                if (!IsHand(fmt, 3) || fmt[2] != '-' ||
                                    fmt[0] != fmt[1] || fmt[3] != fmt[4])
                                        FmtError(fmt);
                                p = std::minmax(GetRank(fmt[0]),
                                                GetRank(fmt[3]));
                                fmt.erase(2, 3);
                                for (size_t r = p.first; r <= p.second; r++) {
                                        fmt[0] = kRanks[r];
                                        fmt[1] = kRanks[r];
                                        range_.insert(PreflopHand(fmt));
                                }
                                break;
                        case 7:
                                if (!IsHand(fmt, 4) || fmt[2] != fmt[6] ||
                                    fmt[0] == fmt[1] || fmt[4] == fmt[5] ||
                                    (fmt[2] != 's' && fmt[2] != 'o') ||
                                    fmt[3] != '-')
                                        FmtError(fmt);
                                size_t min1, max1, min2, max2;
                                ParseRanks(fmt, 0, min1, max1);
                                ParseRanks(fmt, 4, min2, max2);
                                if (max1 != max2)
                                        FmtError(fmt);
                                p = std::minmax(min1, min2);
                                fmt.erase(3, 4);
                                fmt[0] = kRanks[max1];
                                for (size_t r = p.first; r <= p.second; r++) {
                                        fmt[1] = kRanks[r];
                                        range_.insert(PreflopHand(fmt));
                                }
                                break;
                        default:
                                FmtError(fmt);
                                break;
                        }
                        first = last+1;
                }
}

void
PreflopRange::Fill(const CardSet& dead_cards)
{
        string h(3, 'x');

        assert(dead_cards.size() == 0);
        for (size_t r1 = 0; r1 < kRanks.length()-1; r1++)
                for (size_t r2 = r1+1; r2 < kRanks.length(); r2++) {
                        h[0] = kRanks[r2];
                        h[1] = kRanks[r1];
                        h[2] = 's';
                        range_.insert(PreflopHand(h));
                        h[2] = 'o';
                        range_.insert(PreflopHand(h));
                }
        h.pop_back();
        for (auto r : kRanks) {
                h[0] = r;
                h[1] = r;
                range_.insert(PreflopHand(h));
        }
}

std::vector<PreflopHand>
PreflopRange::ToVector(const CardSet& dead_cards) const
{
        assert(dead_cards.size() == 0);
        return std::vector<PreflopHand>(range_.begin(), range_.end());
}

string
PreflopRange::ToString() const
{
        string s;

        if (range_.size() > 0) {
                auto it = range_.begin();
                s += it++->ToString();
                while (it != range_.end())
                        s += "," + it++->ToString();
        }
        return s;
}
} // namespace GTO

