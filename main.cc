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
#include "string_integer.h"

using namespace std;

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


void filterByMultiplier(const int k, const list<Ideal>& ideals,
			list<Ideal>& output) {
  output.clear();
  copy_if(ideals.begin(), ideals.end(), back_inserter(output),
	  [k](const Ideal& ideal) { return ideal.k == k; });
}

/// Assume both sequences are sorted in ascending order.
/// Returns the number of elements that needed to be added to src
/// so that dst is a subset of src.
template<class Iter1, class Iter2>
int similarity(Iter1 src, Iter1 srcend,
	       Iter2 dst, Iter2 dstend,
	       const int acc = 0) {
  if (dst == dstend) return acc;
  if (src == srcend) return similarity(src, srcend, ++dst, dstend, acc+1);
  if (*src > *dst) return similarity(src, srcend, ++dst, dstend, acc+1);
  if (*src < *dst) return similarity(++src, srcend, dst, dstend, acc);
  return similarity(++src, srcend, ++dst, dstend, acc);
}

/// Returns the average similarity between any two elements in the group.
/// The idea is to assume that we know the factors of one ideal and compute
/// how many factors we need to add to know the factorization of some
/// other ideal.
double groupSimilarity(const vector<Ideal>& group) {
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
	res += similarity(starts[i], group[i].factors.cend(),
			  starts[j], group[j].factors.cend());
      }
    }
  }
  assert(count == n * n - n);
  if (count == 0) return 0;
  return res / (n * n - n);
}

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

template<class Iter>
double idealsRatio(Iter first, Iter last, const int t) {
  double c1 = 0;
  double c2 = 0;
  for (; first != last; first++) {
    c1 ++;
    if (validFactors(first->factors, t)) c2 ++;
  }
  return c2 / c1;
}

string idealFilename(const int i) {
  stringstream ss;
  //    if (i % 8 == 0) {
  //  ss << "ideal-" << i << ".txt";
  //    } else {
  ss << "../ideals-small-sample/ideal-" << i << ".txt";
  //    }
  return ss.str();  
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
    double ratio = idealsRatio(ideals.begin(), ideals.end(), target);
    cout << "Ratio after culling for target " << target
	 << ": " << ratio << endl;

    /*
    stringstream outfile;
    outfile << "results-" << target << ".txt";
    ofstream out;
    out.open(outfile.str().c_str(), ios_base::out | ios_base::app);
    out << fixed << setprecision(5) << i << ", " << ratio << endl;
    out.close();
    */
    
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

StringInteger maxFactor(const vector<Ideal>& group) {
  StringInteger res;
  for (const Ideal& ideal : group) {
    const vector<StringInteger>& factors = ideal.factors;
    if (!factors.empty()) {
      StringInteger x = factors[factors.size()-1];
      if (x > res) res = x;
    }
  }
  return res;
}

const int multipliers[] = {1, 2, 3, 5, 6, 7, 10};

int main(int argc, char** argv) {
  struct rlimit l = {7*1024ULL*1024ULL*1024ULL, 7*1024ULL*1024ULL*1024ULL};
  setrlimit(RLIMIT_AS, &l);

  //  runProbWithPower();

  for (int i = 32; i <= 80; i += 1) {
    string filename = idealFilename(i);
    //    cout << "Processing file \"" << filename << "\"" << endl;
    list<Ideal> ideals;
    loadIdeals(filename, ideals);

    list<double> results;
    group2Map(ideals.begin(), ideals.end(), groupSimilarity, results);
    cout << i << ", "
	 << average<double>(results.begin(), results.end()) << endl;
    /*
    for (int k : multipliers) {
      list<Ideal> ideals2;
      filterByMultiplier(k, ideals, ideals2);
      list<double> results;
      groupMap(ideals2.begin(), ideals2.end(), groupSimilarity, results);
      cout << i << ", "
	   << average<double>(results.begin(), results.end()) << endl;
    }
    */
  }

  return 0;
}

