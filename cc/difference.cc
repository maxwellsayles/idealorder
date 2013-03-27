/**
 * Fix the number of bits.
 * For each pair of ideal classes, assume h=ord(a) is known and count
 * the number of prime factors of h'=ord(b^h).  If h'=1 then the number
 * of prime factors is 0.  Build a histogram of this.
 */
#include "difference.h"

#include <algorithm>
#include <fstream>
#include <iomanip>
#include <iostream>
#include <vector>

#include <assert.h>

#include "group_by_k.h"
#include "ideal.h"
#include "string_integer.h"
#include "util.h"

using namespace std;

/// Returns the average difference between the factors of any two ideals.
/// The idea is to assume that we know the factors of one ideal and compute
/// how many factors we need to add to know the factorization of some
/// other ideal.
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

/// Represents a histogram of the differences between the factorization
/// of orders.
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

/// Combine function for a fold counting the difference between ideals.
/// NOTE: Modifies the accumulator in place.
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

void runDifference() {
  ofstream out("difference.dat");
  out << fixed << setprecision(5);
  cout << fixed << setprecision(5);
  for (int i = 32; i <= 80; i += 8) {
    string filename = bigIdealFilename(i);
    list<Ideal> ideals;
    loadIdeals(filename, ideals);

    DifferenceHistogram hist;
    ideal_list_group group(ideals.cbegin(), ideals.cend());
    hist = accumulate(group.cbegin(), group.cend(),
		      hist, histogramCombine);
    cout << i << " :";
    out << i;
    for (int i = 0; i < 5; i ++) {
      double d = static_cast<double>(hist.diff_count[i]) /
                     static_cast<double>(hist.total);
      cout << ' ' << 100*d;
      out << ", " << d;
    }
    cout << endl;
    out << endl;
  }
}
