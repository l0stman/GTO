#ifndef GTO_ARRAY_H_
#define GTO_ARRAY_H_

#include <cassert>
#include <vector>

namespace GTO {
using std::size_t;
using std::vector;

// Represents a two-dimensional array of numbers.
template<class Number>
class Array {
public:
        Array() : vect_(vector<Number>(0)), nrows_(0), ncols_(0) {}

        Array(const size_t& nrows, const size_t& ncols, const Number& init=0.0)
                : vect_(vector<Number>(nrows*ncols, init)),
                  nrows_(nrows),
                  ncols_(ncols)
        {}

        size_t num_rows() const { return nrows_; }
        size_t num_cols() const { return ncols_; }

        Number get(const size_t& row, const size_t& col) const
        {
                assert(row < nrows_ && col < ncols_);
                return vect_[row*ncols_+col];
        }

        void set(const size_t& row, const size_t& col, const Number& val)
        {
                assert(row < nrows_ && col < ncols_);
                vect_[row*ncols_+col] = val;
        }

        void inc(const size_t& row, const size_t& col, const Number& val)
        {
                assert(row < nrows_ && col < ncols_);
                vect_[row*ncols_+col] += val;
        }
private:
        vector<Number> vect_;
        const size_t nrows_;
        const size_t ncols_;
};
} // namespace GTO

#endif                          // !GTO_ARRAY_H_
