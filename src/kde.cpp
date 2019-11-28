// source of the inspiration for the function
// https://github.com/qgis/QGIS/blob/b3d2619976a69d7fb67b884492da491dfaba287c/src/analysis/raster/qgskde.cpp
#include <Rcpp.h>
#include <math.h>

using namespace Rcpp;

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

// TODO include the triangularKernel

double kde_element(double d, double bw, String kernel, bool scaled){
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
    // default is uniform kernel
    else{
      return uniformKernel(d, bw, scaled);
    }
  } else {
    return 0;
  }
}

// [[Rcpp::export]]
NumericVector kde_estimate(NumericMatrix fishnet,
                           NumericMatrix points,
                           double bw,
                           String kernel,
                           bool scaled = false) {

  int nrow = fishnet.nrow();

  NumericVector out(nrow);

  for (int i = 0; i < nrow; i++) {

    double d = 0;

    for (int j = 0; j < points.nrow(); j++) {

      NumericVector v1 = fishnet.row(i);
      NumericVector v2 = points.row(j);

      NumericVector v3 = v1-v2;

      d += kde_element(sqrt(sum(pow(v3, 2.0))),
                       bw,
                       kernel,
                       scaled);

    }

    out(i) = d;
    Rcpp::checkUserInterrupt();
  }

  return out;
}
