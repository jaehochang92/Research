#include <Rcpp.h>
using namespace Rcpp ;

// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp 
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//

// [[Rcpp::export()]]

NumericVector fh (NumericVector x, NumericVector X, float h) {
  int J = x.length() ; int n = X.length() ;
  NumericVector ret(J) ;
  
  for (int j = 0 ; j < J ; j++) {
    float sum = 0 ;
    for (int i = 0 ; i < n ; i++) {
      if (std::abs((x[j]-X[i])/h)<=1) {
        sum += 3.0/4.0 * (1-std::pow((x[j]-X[i])/h,2)) ;
      } else {
        sum += 0 ;
      }
    }
    ret(j) = sum/(n*h) ;
  }
  
  return ret ;
}