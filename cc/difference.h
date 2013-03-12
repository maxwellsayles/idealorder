#pragma once

#include <vector>

#include "ideal.h"

/// Returns the average difference between the factors of any two ideals.
/// The idea is to assume that we know the factors of one ideal and compute
/// how many factors we need to add to know the factorization of some
/// other ideal.
double avgDifference(const std::vector<Ideal>& group);

/// Compute the maximum difference between the factorization of the order
/// of two ideals in the group.
int maxDifference(const std::vector<Ideal>& group);

