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
