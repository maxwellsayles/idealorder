#include "prime_powers.h"

#include <fstream>
#include <iomanip>
#include <iostream>
#include <list>
#include <sstream>
#include <string>
#include <vector>

#include "ideal.h"
#include "util.h"

using namespace std;

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

void runPrimePowers() {
  vector<double> sums;
  for (int i = 32; i <= 80; i++) {
    sums = probWithPower(i);
  }
  for (int i = 0; i < 8; i++) {
    cout << "Average: " << (sums[i] / (80-32+1)) << endl;
  }
}

