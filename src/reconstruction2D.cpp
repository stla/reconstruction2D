#include "reconstruction2D.h"

// -------------------------------------------------------------------------- //
// -------------------------------------------------------------------------- //
void Message(std::string msg) {
  SEXP rmsg = Rcpp::wrap(msg);
  Rcpp::message(rmsg);
}

// -------------------------------------------------------------------------- //
// -------------------------------------------------------------------------- //
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

// -------------------------------------------------------------------------- //
// -------------------------------------------------------------------------- //
// [[Rcpp::export]]
Rcpp::List runOTR(
  Rcpp::NumericMatrix Pts, Rcpp::NumericVector masses, 
  unsigned int steps, std::size_t np, double tolerance
) {
  PointMassList points = makePoints(Pts, masses);
  PointMap pointMap;
  MassMap  massMap;
  Otr otr(points, pointMap, massMap);
  if(steps == 0 && np == 0) {
    otr.run_under_wasserstein_tolerance(tolerance);    
  } else {
    bool success;
    if(steps == 0) {
      success = otr.run_until(np);
    } else {
      success = otr.run(steps);
    }
    if(!success) {
      Message("Premature end.");
    }
  }
  std::vector<Point> isolatedVertices;
  std::vector<Segment> edges;
  otr.list_output(
    std::back_inserter(isolatedVertices), std::back_inserter(edges)
  );
  int nvertices = isolatedVertices.size();
  int nedges = edges.size();
  Rcpp::NumericMatrix Vertices(2, nvertices);
  Rcpp::NumericMatrix Edges(4, nedges);
  for(int i = 0; i < nvertices; i++) {
    Point point = isolatedVertices[i];
    Rcpp::NumericVector vertex = 
      Rcpp::NumericVector::create(point.x(), point.y());
    Vertices(Rcpp::_, i) = vertex;
  }
  for(int i = 0; i < nedges; i++) {
    Segment segment = edges[i];
    Point vx1 = segment.source();
    Point vx2 = segment.target();
    Rcpp::NumericVector edge = 
      Rcpp::NumericVector::create(vx1.x(), vx1.y(), vx2.x(), vx2.y());
    Edges(Rcpp::_, i) = edge;
  }
  Rcpp::rownames(Vertices) = Rcpp::CharacterVector::create("x", "y");
  Rcpp::rownames(Edges) = 
    Rcpp::CharacterVector::create("x0", "y0", "x1", "y1");
  return Rcpp::List::create(Rcpp::Named("vertices") = Rcpp::transpose(Vertices),
                            Rcpp::Named("segments") = Rcpp::transpose(Edges));
}