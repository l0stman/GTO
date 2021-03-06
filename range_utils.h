#ifndef GTO_RANGE_UTILS_H_
#define GTO_RANGE_UTILS_H_

#include <algorithm>

#include "err.h"

namespace GTO {

using std::string;

const string kRanks = "23456789TJQKA";
const string kSuits = "cdhs";

inline void
FmtError(const string& s)
{
        err::quit("Unknown range format: %s.", s.c_str());
}

inline size_t
GetRank(char c)
{
        return kRanks.find(c);
}

inline void
ParseRanks(const string& s, size_t pos, size_t& min, size_t& max)
{
        std::pair<size_t,size_t> p = std::minmax(
                GetRank(s[pos]), GetRank(s[pos+1]));
        min = p.first;
        max = p.second;
}

inline bool
IsSuit(char c)
{
        return c == 'c' || c == 'd' || c == 'h' || c == 's';
}

inline bool
IsRank(char c)
{
        return GetRank(c) != string::npos;
}

inline bool
IsHand(const string& s, size_t pos)
{
        return IsRank(s[pos]) && IsRank(s[pos+1]);
}

} // namespace GTO

#endif  // !GTO_RANGE_UTILS_H_
