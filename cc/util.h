#pragma once

#include <limits>

/// Compute the average over a sequence.
template<class R, class Iter>
R average(Iter iter, Iter end) {
  R sum = 0;
  R count = 0;
  for (; iter != end; ++iter) {
    sum += *iter;
    count ++;
  }
  return sum / count;
}

