// source of the inspiration for the function
// https://github.com/qgis/QGIS/blob/b3d2619976a69d7fb67b884492da491dfaba287c/src/analysis/raster/qgskde.cpp
#include <R.h>
#include <cpp11/R.hpp>
#include <cpp11/doubles.hpp>
#include <cpp11/matrix.hpp>
#include <math.h>

// uniform kernel
double uniformKernel(double d, double bw, bool scaled){

  if (scaled) {
    double k = 2./(M_PI*bw);
    return k * (0.5/bw);
  }
  else {
    return 1.;
  }
}

//quarticKernel
double quarticKernel(double d, double bw, bool scaled){

  if (scaled) {
    double k = 116./(5.*M_PI*pow(bw, 2));
    return k*(15./16.)*pow(1.-pow(d/bw, 2),2);
  }
  else {
    return pow(1.-pow(d/bw, 2), 2);
  }
}

//triweightKernel
double triweightKernel(double d, double bw, bool scaled){

  if (scaled) {
    double k = 128./(35.*M_PI*pow(bw, 2));
    return k * (35./32.)*pow(1.-pow(d/bw, 2), 3);
  }
  else {
    return pow(1.-pow(d/bw, 2), 3);
  }
}

//epanechnikovKernel
double epanechnikovKernel(double d, double bw, bool scaled){

  if (scaled) {
    double k = 8./(3.*M_PI*pow(bw, 2));
    return k*(3./4.)*(1.-pow(d/bw, 2));
  }
  else {
    return (1.-pow(d/bw, 2));
  }
}

double triangularKernel(double d, double bw, bool scaled, double decay){

  if (scaled) {
    if (decay  >= 0){
      double k = 3./((1.+2.*decay)*M_PI*pow(bw,2));
      // Derived from Wand and Jones (1995), p. 175 (with addition of decay parameter)
      return k*(1.-(1.-decay)*(d/bw));
    }
    else {
      // Non-standard or mathematically valid negative decay ("coolmap")
      return (1.-(1.-decay)*(d/bw));
    }
  }
  else {
    return(1.-(1.-decay)*(d/bw));
  }
}

double kde_element(double d, double bw, std::string kernel, bool scaled, double decay){
  if (d <= bw) {
    if (kernel == "uniform") {
      return uniformKernel(d, bw, scaled);
    }
    else if(kernel == "quartic") {
      return quarticKernel(d, bw, scaled);
    }
    else if(kernel == "triweight") {
      return triweightKernel(d, bw, scaled);
    }
    else if(kernel == "epanechnikov") {
      return epanechnikovKernel(d, bw, scaled);
    }
    else if(kernel == "triangular") {
      return triangularKernel(d, bw, scaled, decay);
    }
    // default is uniform kernel
    else{
      return uniformKernel(d, bw, scaled);
    }
  } else {
    return 0;
  }
}

double distance(cpp11::doubles a, cpp11::doubles b) {
  double dist = 0;
  for (int i=0; i < a.size(); i++){
    dist += std::pow(a[i] - b[i], 2);
  }
  return std::sqrt(dist);
}

[[cpp11::register]]
cpp11::writable::doubles kde_estimate(cpp11::doubles_matrix<cpp11::by_row> fishnet,
                                      cpp11::doubles_matrix<cpp11::by_row> points,
                                      double bw,
                                      std::string kernel,
                                      bool scaled,
                                      double decay,
                                      cpp11::doubles weights) {

  cpp11::writable::doubles out(fishnet.nrow());


  for (int i = 0; i < fishnet.nrow(); i++) {

    double result = 0;

    auto fishnet_row_auto = fishnet[i];

    cpp11::writable::doubles fishnet_row(fishnet.ncol());

    int k = 0;
    for (auto value : fishnet_row_auto) {
      fishnet_row[k] = value;
      k++;
    }

    double w = 1;

    for (int j = 0; j < points.nrow(); j++) {

      auto points_row_auto = points[j];
      cpp11::writable::doubles points_row(points.ncol());

      int k = 0;
      for (auto value : points_row_auto) {
        points_row[k] = value;
        k++;
      }

      if (weights.size() != 0) {
        w = weights[j];
      }

      result += (w * kde_element(distance(fishnet_row, points_row),
                                bw,
                                kernel,
                                scaled,
                                decay));
    }

    out[i] = result;
    cpp11::check_user_interrupt();
  }

  return out;
}
