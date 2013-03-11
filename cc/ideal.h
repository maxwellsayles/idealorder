#pragma once

#include <istream>
#include <list>
#include <string>
#include <vector>

#include "string_integer.h"

struct Ideal {
  StringInteger n;
  int k;
  int p;
  StringInteger order;
  bool success;
  std::vector<StringInteger> factors;
};

std::istream& operator>>(std::istream& stream, Ideal& ideal);

void loadIdeals(const std::string& filename, std::list<Ideal>& ideals);
