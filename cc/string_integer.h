/**
 * Represents unbound positive integers using strings.
 * This class only supports comparison.
 */
#pragma once

#include <iostream>
#include <sstream>
#include <string>

class StringInteger {
 public:
  StringInteger() : i() {}
  StringInteger(const int x) { std::stringstream ss; ss << x; i = ss.str(); }
  StringInteger(const std::string& x) : i(x) {}
  StringInteger(std::string&& x) : i() { i.swap(x); }
  StringInteger(const StringInteger& x) : i(x.i) {}
  StringInteger(StringInteger&& x) : i() { i.swap(x.i); }

  StringInteger& operator=(const std::string& x) {
    i = x;
    return *this;
  }

  StringInteger& operator=(std::string&& x) {
    i.swap(x);
    return *this;
  }

  StringInteger& operator=(const StringInteger& x) {
    i = x.i;
    return *this;
  }

  StringInteger& operator=(StringInteger&& x) {
    i.swap(x.i);
    return *this;
  }

  void swap(StringInteger& x) {
    i.swap(x.i);
  }
  
  bool operator==(const StringInteger& x) const {
    return i == x.i;
  }

  bool operator!=(const StringInteger& x) const {
    return i != x.i;
  }

  bool operator<(const StringInteger& x) const {
    if (i.length() < x.i.length()) return true;
    if (i.length() > x.i.length()) return false;
    return i < x.i;
  }

  bool operator<=(const StringInteger& x) const {
    return operator==(x) || operator<(x);
  }

  bool operator>(const StringInteger& x) const {
    if (i.length() > x.i.length()) return true;
    if (i.length() < x.i.length()) return false;
    return i > x.i;
  }

  bool operator>=(const StringInteger& x) const {
    return operator==(x) || operator>(x);
  }

  std::string toString() const {
    return i;
  }

  /// Compute i mod 4.
  /// Thanks to Myles Maxfield for pointing out that 100%4 == 0.
  /// Therefore, we only need the last two digits, n = 10x + y.
  /// Since 10%4 = 2, we have n = (2x + y) % 4.
  int mod4() const {
    if (i == "") return 0;
    if (i.length() == 1) return (i[0] - '0') & 3;
    int x = i[i.length() - 2];
    int y = i[i.length() - 1];
    // 2 * (x - '0') + y - '0' = 2 * x + y - 3 * '0' = 2 * x + y + '0'
    return (2 * x + y + '0') & 3;
  }

 private:
  std::string i;
};

inline std::istream& operator>>(std::istream& stream, StringInteger& x) {
  std::string y;
  stream >> y;
  x = y;
  return stream;
}

inline std::ostream& operator<<(std::ostream& stream,
				const StringInteger& x) {
  stream << x.toString();
  return stream;
}

