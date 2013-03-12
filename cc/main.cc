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

#include "ideal.h"
#include "difference.h"
#include "string_integer.h"
#include "util.h"

using namespace std;

const int multipliers[] = {1, 2, 3, 5, 6, 7, 10};

/// Return the ideal file associated with i-bits.
string idealFilename(const int i) {
  stringstream ss;
  //    if (i % 8 == 0) {
  ss << "/home/max/Desktop/masters/ideals/ideal-" << i << ".txt";
  //    } else {
  //  ss << "../ideals-small-sample/ideal-" << i << ".txt";
  //    }
  return ss.str();
}

/// Group the list by Ideal::n and then call map on the group.
template<class T, class F, class Iter>
void groupMap(Iter iter, Iter end,
	      F f,
	      list<T>& out) {
  out.clear();
  vector<Ideal> group;
  StringInteger last;
  for (; iter != end; ++iter) {
    if (iter->n != last) {
      if (!group.empty()) {
	out.push_back(f(group));
      }
      group.clear();
      last = iter->n;
    }
    group.push_back(*iter);
  }
  if (!group.empty()) {
    out.push_back(f(group));
  }
}

/// Group the list by Ideal::n and Ideal::k and then call map on the group.
template<class T, class F, class Iter>
void group2Map(Iter iter, Iter end,
	       F f,
	       list<T>& out) {
  out.clear();
  vector<Ideal> group;
  StringInteger last_n;
  int last_k = 0;
  for (; iter != end; ++iter) {
    if (iter->n != last_n || iter->k != last_k) {
      if (!group.empty()) {
	out.push_back(f(group));
      }
      group.clear();
      last_n = iter->n;
      last_k = iter->k;
    }
    group.push_back(*iter);
  }
  if (!group.empty()) {
    out.push_back(f(group));
  }
}

/// Fold-left after grouping the list by Ideal::n and Ideal::k.
/// The combine function takes the accumulator and
/// a vector of ideals for each group.
template<class R, class F, class Iter>
R group2Foldl(Iter iter, Iter end, R z0, F combine) {
  R acc = z0;
  vector<Ideal> group;
  StringInteger last_n;
  int last_k = 0;
  for (; iter != end; ++iter) {
    if (iter->n != last_n || iter->k != last_k) {
      if (!group.empty()) {
	acc = combine(acc, group);
      }
      group.clear();
      last_n = iter->n;
      last_k = iter->k;
    }
    group.push_back(*iter);
  }
  if (!group.empty()) {
    acc = combine(acc, group);
  }
  return acc;
}
		 

/// Filters the input list by multiplier k.
void filterByMultiplier(const int k, const list<Ideal>& ideals,
			list<Ideal>& output) {
  output.clear();
  copy_if(ideals.begin(), ideals.end(), back_inserter(output),
	  [k](const Ideal& ideal) { return ideal.k == k; });
}


/// True if 3^a, 5^b, 7^c, and 11^d are <= t for a, b, c, d occuring
/// in the factorization of the order and if x^e has e <= 1 for all x > 11.
bool validFactors(const vector<StringInteger>& factors, const int t) {
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
  string filename = idealFilename(i);
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

void runSimilarity() {
  for (int i = 32; i <= 80; i += 8) {
    string filename = idealFilename(i);
    list<Ideal> ideals;
    loadIdeals(filename, ideals);

    list<double> results;
    group2Map(ideals.begin(), ideals.end(), avgDifference, results);
    cout << i << ", "
	 << average<double>(results.begin(), results.end()) << endl;
    //    group2Map(ideals.begin(), ideals.end(), maxDifference, results);
    //    cout << i << ", "
    //	 << *max_element(results.begin(), results.end()) << endl;
  }
}

int main(int argc, char** argv) {
  // Make sure the program doesn't go too crazy!
  struct rlimit l = {7*1024ULL*1024ULL*1024ULL, 7*1024ULL*1024ULL*1024ULL};
  setrlimit(RLIMIT_AS, &l);

  //  runProbWithPower();
  runSimilarity();

  return 0;
}

