#include "prime_powers.h"

#include <fstream>
#include <iomanip>
#include <iostream>
#include <list>
#include <string>
#include <vector>

#include "ideal.h"
#include "util.h"

using namespace std;

const int targets[] = {1, 9, 25, 27, 49, 81, 121, 125};

// Computes the probability that the order has no prime power
// factors greater than a set of target prime power bounds.
void runPrimePowers() {
  ofstream out("prime-powers.dat");
  for (int i = 32; i <= 80; i += 8) {
    string filename = bigIdealFilename(i);
    cout << "Processing file \"" << filename << "\"" << endl;
    cout << fixed << setprecision(8);

    // Load ideals
    list<Ideal> ideals;
    loadIdeals(filename, ideals);

    out << fixed << setprecision(8);
    out << i;

    // Compute ratio for each target
    for (int target : targets) {
      double ratio = validIdealFactorsRatio<double>(ideals.begin(),
						    ideals.end(),
						    target);
      cout << "Ratio after culling for target " << target
	   << ": " << ratio << endl;
      out << ", " << ratio;
    }

    cout << endl;
    out << endl;
  }
}

