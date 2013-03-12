#include <algorithm>
#include <fstream>
#include <functional>
#include <iomanip>
#include <ios>
#include <iostream>
#include <list>
#include <numeric>
#include <string>
#include <vector>

#include <assert.h>
#include <sys/resource.h>

#include "attempts.h"
#include "ideal.h"
#include "difference.h"
#include "group_by_k.h"
#include "string_integer.h"
#include "util.h"

using namespace std;

const int multipliers[] = {1, 2, 3, 5, 6, 7, 10};

/// Filters the input list by multiplier k.
void filterByMultiplier(const int k, const list<Ideal>& ideals,
			list<Ideal>& output) {
  output.clear();
  copy_if(ideals.begin(), ideals.end(), back_inserter(output),
	  [k](const Ideal& ideal) { return ideal.k == k; });
}

/// Compute the number of ideals where the factorization of its order
/// does not violate validFactors() above.
template<class R, class Iter>
R idealsRatio(Iter first, Iter last, const int t) {
  R c1 = 0;
  R c2 = 0;
  for (; first != last; first++) {
    c1 ++;
    if (validFactors(first->factors, t)) c2 ++;
  }
  return c2 / c1;
}

// Computes the probability that the order has no prime power
// factors greater than a set of target prime power bounds.
vector<double> probWithPower(const int i) {
  vector<double> sums(8, 0);
  const int targets[] = {1, 9, 25, 27, 49, 81, 121, 125};
  string filename = bigIdealFilename(i);
  cout << "Processing file \"" << filename << "\"" << endl;
  cout << fixed << setprecision(5);

  // Load ideals
  list<Ideal> ideals;
  loadIdeals(filename, ideals);

  // Compute ratio for each target
  for (int i = 0; i < 8; i++) {
    int target = targets[i];
    double ratio = idealsRatio<double>(ideals.begin(), ideals.end(),
				       target);
    cout << "Ratio after culling for target " << target
	 << ": " << ratio << endl;

    stringstream outfile;
    outfile << "results-" << target << ".txt";
    ofstream out;
    out.open(outfile.str().c_str(), ios_base::out | ios_base::app);
    out << fixed << setprecision(5) << i << ", " << ratio << endl;
    out.close();
    
    sums[i] += ratio;
  }

  cout << endl;
  return sums;
}

void runProbWithPower() {
  vector<double> sums;
  for (int i = 32; i <= 80; i++) {
    sums = probWithPower(i);
  }
  for (int i = 0; i < 8; i++) {
    cout << "Average: " << (sums[i] / (80-32+1)) << endl;
  }
}

int main(int argc, char** argv) {
  // Make sure the program doesn't go too crazy!
  struct rlimit l = {7*1024ULL*1024ULL*1024ULL, 7*1024ULL*1024ULL*1024ULL};
  setrlimit(RLIMIT_AS, &l);

  //  runProbWithPower();
  //  runDifference();
  runAttempts();

  return 0;
}

