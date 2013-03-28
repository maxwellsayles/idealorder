
#include "reusable.h"

#include <algorithm>
#include <fstream>
#include <iomanip>
#include <iostream>
#include <vector>

#include <assert.h>

#include "difference.h"
#include "group_by_k.h"
#include "ideal.h"
#include "string_integer.h"
#include "util.h"

using namespace std;

const int primes[] = {2, 3, 5, 7, 11};

struct Histogram {
  Histogram() {
    prime[0] = 0;
    prime[1] = 0;
    prime[2] = 0;
    prime[3] = 0;
    prime[4] = 0;
    total = 0;
  }
  int prime[5];
  int total;
};

/// NOTE: Modifies the accumulator in place.
Histogram& histogramCombine(Histogram& hist,
			    const std::vector<Ideal>& group) {
  int n = group.size();
  hist.total += n * (n-1);

  for (int k = 0; k < 5; k++) {
    // Assume an exponent removing all primes <= primes[k] is known.
    vector<vector<StringInteger>::const_iterator> starts;
    for (int i = 0; i < n; i++) {
      starts.push_back(upper_bound(group[i].factors.cbegin(),
				   group[i].factors.cend(),
				   StringInteger(primes[k])));
    }
    assert(starts.size() == group.size());
    
    for (int i = 0; i < n; i++) {
      for (int j = 0; j < n; j++) {
	if (i != j) {
	  int s = difference(starts[i], group[i].factors.cend(),
			     starts[j], group[j].factors.cend());
	  if (s == 0) hist.prime[k]++;
	}
      }
    }
  }
  return hist;
}

void runReusable() {
  ofstream out("reusable.dat");
  out << fixed << setprecision(5);
  cout << fixed << setprecision(5);
  for (int i = 32; i <= 80; i += 8) {
    string filename = bigIdealFilename(i);
    list<Ideal> ideals;
    loadIdeals(filename, ideals);

    Histogram hist;
    ideal_list_group group(ideals.cbegin(), ideals.cend());
    hist = accumulate(group.cbegin(), group.cend(),
		      hist, histogramCombine);
    cout << i << " :";
    out << i;
    for (int i = 0; i < 5; i ++) {
      double d = static_cast<double>(hist.prime[i]) /
                     static_cast<double>(hist.total);
      cout << ' ' << 100*d;
      out << ", " << d;
    }
    cout << endl;
    out << endl;
  }
}
