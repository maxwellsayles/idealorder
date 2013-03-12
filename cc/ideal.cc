#include "ideal.h"

#include <fstream>
#include <istream>
#include <sstream>
#include <vector>

#include <assert.h>

#include "string_integer.h"

using namespace std;

/// Read a list, e.g. [1,2,3,4].
istream& operator>>(istream& stream, vector<StringInteger>& res) {
  res.clear();
  string tmp;
  char c;
  if (!(stream >> c)) {
    return stream;
  }
  assert(c == '[');
  while (stream >> c && c != ']') {
    if (c == ',') {
      res.push_back(tmp);
      tmp = "";
    } else if (c != ' ') {
      tmp += c;
    }
  }
  if (tmp != "") {
    res.push_back(tmp);
  }
  return stream;
}

istream& operator>>(istream& stream, Ideal& ideal) {
  stream >> ideal.n;
  stream >> ideal.k;
  stream >> ideal.p;
  stream >> ideal.order;
  stream >> ideal.success;
  stream >> ideal.factors;
  return stream;
}

void loadIdeals(const string& filename, list<Ideal>& ideals) {
  ideals.clear();
  ifstream f(filename);
  Ideal ideal;
  while (f >> ideal) {
    ideals.push_back(ideal);
  }
}
