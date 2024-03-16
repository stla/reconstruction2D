#include "reconstruction2D.h"

PointMassList makePoints(Rcpp::NumericMatrix Pts, Rcpp::NumericVector masses) {

  int n = Pts.ncol();

  PointMassList points;
  points.reserve(n);

  for(int i = 0; i < n; i++) {
    Rcpp::NumericVector pt = Pts(Rcpp::_, i);
    Point point(pt(0), pt(1));
    points.emplace_back(std::make_pair(point, masses(i)));
  }

  return points;
}