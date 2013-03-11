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

