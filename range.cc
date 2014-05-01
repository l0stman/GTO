#include "range.h"

#include <cctype>
#include <cstdio>
#include <cstdlib>
#include <set>

namespace {
void
FmtError(const std::string& s)
{
        fprintf(stderr, "Unknown range format: %s\n", s.c_str());
        exit(1);
}
}

namespace GTO {
void
Range::AddSuited(const string& s, size_t pos)
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
Range::AddOffsuit(const string& s, size_t pos)
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
                                range_.insert(CardSet(s.substr(first, len)));
                                break;
                        default:
                                FmtError(s.substr(first, len));
                        }
                        first = last+1;
                }
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
