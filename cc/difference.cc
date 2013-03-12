#include "difference.h"

#include <algorithm>
#include <vector>

#include <assert.h>

#include "string_integer.h"

using namespace std;

/// Assume both sequences are sorted in ascending order.
/// Returns the number of elements that needed to be added to src
/// so that dst is a subset of src.
template<class Iter1, class Iter2>
int difference(Iter1 src, Iter1 srcend,
	       Iter2 dst, Iter2 dstend,
	       const int acc = 0) {
  if (dst == dstend) return acc;
  if (src == srcend) return difference(src, srcend, ++dst, dstend, acc+1);
  if (*src > *dst) return difference(src, srcend, ++dst, dstend, acc+1);
  if (*src < *dst) return difference(++src, srcend, dst, dstend, acc);
  return difference(++src, srcend, ++dst, dstend, acc);
}

double avgDifference(const vector<Ideal>& group) {
  int n = group.size();

  vector<vector<StringInteger>::const_iterator> starts;
  for (int i = 0; i < n; i++) {
    starts.push_back(upper_bound(group[i].factors.cbegin(),
				 group[i].factors.cend(),
				 StringInteger(7)));
  }
  assert(starts.size() == group.size());

  double res = 0;
  int count = 0;
  for (int i = 0; i < n; i++) {
    for (int j = 0; j < n; j++) {
      if (i != j) {
	count ++;
	res += difference(starts[i], group[i].factors.cend(),
			  starts[j], group[j].factors.cend());
      }
    }
  }
  assert(count == n * n - n);
  if (count == 0) return 0;
  return res / (n * n - n);
}

/// Compute the maximum difference between the factorization of the order
/// of two ideals in the group.
int maxDifference(const vector<Ideal>& group) {
  int n = group.size();
  vector<vector<StringInteger>::const_iterator> starts;
  for (int i = 0; i < n; i++) {
    starts.push_back(upper_bound(group[i].factors.cbegin(),
				 group[i].factors.cend(),
				 StringInteger(7)));
  }
  assert(starts.size() == group.size());

  int res = 0;
  for (int i = 0; i < n; i++) {
    for (int j = 0; j < n; j++) {
      if (i != j) {
	int s = difference(starts[i], group[i].factors.cend(),
			   starts[j], group[j].factors.cend());
	if (s > res) res = s;
      }
    }
  }
  return res;
}


DifferenceHistogram& histogramCombine(DifferenceHistogram& hist,
				      const std::vector<Ideal>& group) {
  int n = group.size();
  vector<vector<StringInteger>::const_iterator> starts;
  for (int i = 0; i < n; i++) {
    starts.push_back(upper_bound(group[i].factors.cbegin(),
				 group[i].factors.cend(),
				 StringInteger(7)));
  }
  assert(starts.size() == group.size());

  for (int i = 0; i < n; i++) {
    for (int j = 0; j < n; j++) {
      if (i != j) {
	int s = difference(starts[i], group[i].factors.cend(),
			   starts[j], group[j].factors.cend());
	if (s <= 4) hist.diff_count[s]++;
	hist.total++;
      }
    }
  }
  return hist;
}
