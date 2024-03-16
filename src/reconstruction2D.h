#ifndef __HEADER__
#define __HEADER__

#include <Rcpp.h>

#define CGAL_EIGEN3_ENABLED 1

#include <CGAL/Exact_predicates_inexact_constructions_kernel.h>
#include <CGAL/Optimal_transportation_reconstruction_2.h>
#include <CGAL/property_map.h>

typedef CGAL::Exact_predicates_inexact_constructions_kernel                 K;
typedef K::FT                                                               FT;
typedef K::Point_2                                                          Point;
typedef K::Segment_2                                                        Segment;
typedef std::pair<Point, FT>                                                PointMassPair;
typedef std::vector<PointMassPair>                                          PointMassList;
typedef CGAL::First_of_pair_property_map <PointMassPair>                    PointMap;
typedef CGAL::Second_of_pair_property_map <PointMassPair>                   MassMap;
typedef CGAL::Optimal_transportation_reconstruction_2<K, PointMap, MassMap> Otr;

#endif
