#pragma once

#include <limits>
#include <sstream>
#include <string>
#include <vector>

#include "string_integer.h"

/// True if 3^a, 5^b, 7^c, and 11^d are <= t for a, b, c, d occuring
/// in the factorization of the order and if x^e has e <= 1 for all x > 11.
inline bool validFactors(const std::vector<StringInteger>& factors,
			 const int t) {
  int threes = 1;
  int fives = 1;
  int sevens = 1;
  int elevens = 1;
  StringInteger last = 0;
  for (const StringInteger& x : factors) {
    if (x == 3) threes *= 3;
    else if (x == 5) fives *= 5;
    else if (x == 7) sevens *= 7;
    else if (x == 11) elevens *= 11;
    else if (x != 2 && x == last) return false;
    last = x;
  }
  return threes <= t && fives <= t && sevens <= t;
}

/// Compute the average over a sequence.
template<class R, class Iter>
inline R average(Iter iter, Iter end) {
  R sum = 0;
  R count = 0;
  for (; iter != end; ++iter) {
    sum += *iter;
    count ++;
  }
  return sum / count;
}

/// Return the ideal file associated with i-bits.
inline std::string bigIdealFilename(const int i) {
  std::stringstream ss;
  ss << "/home/max/Desktop/masters/ideals/ideal-" << i << ".txt";
  return ss.str();
}

/// Return the small ideal file associated with i-bits.
inline std::string smallIdealFilename(const int i) {
  std::stringstream ss;
  ss << "/home/max/Desktop/masters/ideals-small-sample/ideal-"
     << i << ".txt";
  return ss.str();
}
