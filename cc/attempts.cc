#include <algorithm>
#include <array>
#include <fstream>
#include <list>
#include <iomanip>
#include <iostream>
#include <string>
#include <utility>

#include <assert.h>

#include "difference.h"
#include "group_by_k.h"
#include "ideal.h"
#include "util.h"

using namespace std;

struct attempts_t {
  attempts_t() {
    n1.fill(make_pair(0, 0));
    n3.fill(make_pair(0, 0));
  }
  array<pair<int, int>, 11> n1;
  array<pair<int, int>, 11> n3;
};

const int multipliers[] = {1, 2, 3, 5, 6, 7, 10};

/// Compute the number of attempts to factor N in this group if
/// the order of the first element is known.
/// NOTE: Modifies acc in place.
attempts_t& combine(attempts_t& acc, const vector<Ideal>& group) {
  if (group.empty()) {
    return acc;
  }

  // Assume we know the factors of the order.
  const vector<StringInteger>& factors = group[0].factors;
  int nmod4 = group[0].n.mod4();
  assert(nmod4 == 1 || nmod4 == 3);
  array<pair<int, int>, 11>& ns = nmod4 == 1 ? acc.n1 : acc.n3;
  ns[group[0].k].second++;

  // Count the number of ideals tried in this group.
  for (auto ideal : group) {
    assert(ideal.k == group[0].k);
    assert(ideal.n.mod4() == nmod4);
    if (validFactors(ideal.factors, 49) &&
	difference(factors.cbegin(), factors.cend(),
		   ideal.factors.cbegin(),
		   ideal.factors.cend()) == 0 &&
	ideal.success) {
      break;
    }
    ns[ideal.k].first++;
  }
  return acc;
}

void runAttempts() {
  cout << fixed << setprecision(5);
  ofstream out("attempts.dat");
  out << fixed << setprecision(5);
  for (int i = 32; i <= 80; i += 8) {
    string filename = bigIdealFilename(i);
    list<Ideal> ideals;
    loadIdeals(filename, ideals);

    ideal_list_group group(ideals.cbegin(), ideals.cend());
    attempts_t avg;
    avg = accumulate(group.cbegin(), group.cend(),
		     avg, combine);
    out << i;
    for (int k : multipliers) {
      double avg1 = (double)avg.n1[k].first / (double)avg.n1[k].second;
      double avg3 = (double)avg.n3[k].first / (double)avg.n3[k].second;
      cout << "n = 1, " << i << ", " << k << ", " << avg1 << endl;
      cout << "n = 3, " << i << ", " << k << ", " << avg3 << endl;
      out << ", " << avg1 << ", " << avg3;
    }
    out << endl;
    cout << endl;
  }
}
