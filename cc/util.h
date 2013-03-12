#pragma once

#include <limits>
#include <sstream>
#include <string>

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

/// Return the ideal file associated with i-bits.
std::string idealFilename(const int i) {
  std::stringstream ss;
  //    if (i % 8 == 0) {
  ss << "/home/max/Desktop/masters/ideals/ideal-" << i << ".txt";
  //    } else {
  //  ss << "../ideals-small-sample/ideal-" << i << ".txt";
  //    }
  return ss.str();
}

