#pragma once

#include <vector>

#include "ideal.h"

struct DifferenceHistogram {
  DifferenceHistogram() {
    diff_count[0] = 0;
    diff_count[1] = 0;
    diff_count[2] = 0;
    diff_count[3] = 0;
    diff_count[4] = 0;
    total = 0;
  }
  int diff_count[5];
  int total;
};

/// Assume both sequences are sorted in ascending order.
/// Returns the number of elements that needed to be added to src
/// so that dst is a subset of src.
/// NOTE: Tail recursion optimization makes this non-recursive.
template<class Iter1, class Iter2>
inline int difference(Iter1 src, Iter1 srcend,
		      Iter2 dst, Iter2 dstend,
		      const int acc = 0) {
  if (dst == dstend) return acc;
  if (src == srcend) return difference(src, srcend, ++dst, dstend, acc+1);
  if (*src > *dst) return difference(src, srcend, ++dst, dstend, acc+1);
  if (*src < *dst) return difference(++src, srcend, dst, dstend, acc);
  return difference(++src, srcend, ++dst, dstend, acc);
}


/// Returns the average difference between the factors of any two ideals.
/// The idea is to assume that we know the factors of one ideal and compute
/// how many factors we need to add to know the factorization of some
/// other ideal.
double avgDifference(const std::vector<Ideal>& group);

/// Compute the maximum difference between the factorization of the order
/// of two ideals in the group.
int maxDifference(const std::vector<Ideal>& group);

/// Combine function for a fold counting the difference between ideals.
/// NOTE: Modifies the accumulator in place.
DifferenceHistogram& histogramCombine(DifferenceHistogram& hist,
				      const std::vector<Ideal>& group);
