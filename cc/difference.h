#pragma once

#include <vector>

#include "ideal.h"

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

/// For pairs of ideal classes, compute the probability that knowing
/// the order of one ideal class is 0, 1, 2, etc factors away from the
/// order of another ideal class.
void runDifference();
