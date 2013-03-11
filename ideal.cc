#include "ideal.h"

#include <fstream>
#include <istream>
#include <sstream>
#include <vector>

#include <assert.h>

#include "string_integer.h"

using namespace std;

/// Read [1,2,3,4]
vector<StringInteger> parseList(istream& stream) {
  vector<StringInteger> res;
  string tmp;
  char c;
  if (!(stream >> c)) {
    return res;
  }
  assert(c == '[');
  while (stream >> c && c != ']') {
    if (c == ',') {
      res.push_back(tmp);
      tmp = "";
    } else {
      tmp += c;
    }
  }
  if (tmp != "") {
    res.push_back(tmp);
  }
  return res;
}

istream& operator>>(istream& stream, Ideal& ideal) {
  stream >> ideal.n;
  stream >> ideal.k;
  stream >> ideal.p;
  stream >> ideal.order;
  int i;
  stream >> i;
  ideal.success = i == 1;
  ideal.factors = parseList(stream);
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
