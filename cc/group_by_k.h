/**
 * A minimal implementation that transforms a sequence of ideals
 * into a sequence of some push_back container grouped by equality
 * on the k member of each ideal.
 */
#pragma once

#include "ideal.h"

// Internal class.  Use GroupByK below instead.
template<class PushBackContainer, class Iter>
class GroupByKIterator_ {
 private:
  Iter end_;
  Iter iter_;
  //  std::vector<Ideal> group_;
  PushBackContainer group_;
  int last_k_;

  void nextGroup() {
    group_.clear();
    if (iter_ == end_) {
      return;
    }
    last_k_ = iter_->k;
    for (; iter_ != end_ && iter_->k == last_k_; ++iter_) {
      group_.push_back(*iter_);
    }
  }
  
 public:
  GroupByKIterator_(Iter end)
      : end_(end)
      , iter_(end)
      , group_()
      , last_k_(0) {
  }

  GroupByKIterator_(Iter begin, Iter end)
      : end_(end)
      , iter_(begin)
      , group_()
      , last_k_(0) {
    nextGroup();
  }

  bool operator==(const GroupByKIterator_& that) const {
    return iter_ == that.iter_;
  }

  bool operator!=(const GroupByKIterator_& that) const {
    return iter_ != that.iter_;
  }

  //  const std::vector<Ideal>& operator*() const {
  const PushBackContainer& operator*() const {
    return group_;
  }

  GroupByKIterator_& operator++() {
    nextGroup();
    return *this;
  }

  GroupByKIterator_& operator++(int) {
    nextGroup();
    return *this;
  }
};

template <class PushBackContainer, class Iter>
class GroupByK {
 private:
  Iter begin_;
  Iter end_;
 public:
  GroupByK(Iter begin, Iter end)
      : begin_(begin)
      , end_(end) {
  }

  GroupByKIterator_<PushBackContainer, Iter> cbegin() const {
    return GroupByKIterator_<PushBackContainer, Iter>(begin_, end_);
  }

  GroupByKIterator_<PushBackContainer, Iter> cend() const {
    return GroupByKIterator_<PushBackContainer, Iter>(end_, end_);
  }
};


