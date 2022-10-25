#include <R.h>
#include <cpp11.hpp>
#include <cpp11/doubles.hpp>
#include <math.h>
#include <testthat.h>

const bool isSmallerThen(const double &x, const double theta = 0.000001){
  return x < theta;
}

const bool isDifferenceSmallerThen(const double &x, const double &y, const double theta = 0.000001){
  return isSmallerThen(abs(x - y), theta);
}

const double distance(const cpp11::doubles& a, const cpp11::doubles& b);

context("internal C++ 'distance' function") {
  test_that("distance") {
    cpp11::writable::doubles a = {0, 0};
    cpp11::writable::doubles b = {2, 2};
    double dist = distance(a, b);
    expect_true(isDifferenceSmallerThen(dist, 2.828427));

    cpp11::writable::doubles c = {1, 1};
    cpp11::writable::doubles d = {1, 1};
    dist = distance(a, b);
    expect_true(dist == 0);
  }
}
