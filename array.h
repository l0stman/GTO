#ifndef GTO_ARRAY_H_
#define GTO_ARRAY_H_

#include <cassert>
#include <vector>

namespace GTO {
using std::vector;

// Represents a two-dimensional array of doubles.
class Array {
public:
        Array() : vect_(vector<double>(0)), nrows_(0), ncols_(0) {}

        Array(size_t nrows, size_t ncols, double init=0.0)
                : vect_(vector<double>(nrows*ncols, init)),
                  nrows_(nrows),
                  ncols_(ncols)
        {}

        size_t NumRows() const { return nrows_; }
        size_t NumCols() const { return ncols_; }

        double Get(size_t row, size_t col) const
        {
                assert(row < nrows_ && col < ncols_);
                return vect_[row*ncols_+col];
        }

        void Set(size_t row, size_t col, double val)
        {
                assert(row < nrows_ && col < ncols_);
                vect_[row*ncols_+col] = val;
        }

        void Inc(size_t row, size_t col, double val)
        {
                assert(row < nrows_ && col < ncols_);
                vect_[row*ncols_+col] += val;
        }
private:
        vector<double> vect_;
        const size_t nrows_;
        const size_t ncols_;
};
} // namespace GTO

#endif                          // !GTO_ARRAY_H_
