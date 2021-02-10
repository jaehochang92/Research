#include <Rcpp.h>
using namespace Rcpp ;

// [[Rcpp::export]]
NumericVector fh (NumericVector x, NumericVector X, float h) {
  int J = x.length() ; int n = X.length() ;
  NumericVector ret(J) ;
  
  for (int j = 0 ; j < J ; j++) {
    float sum = 0 ;
    for (int i = 0 ; i < n ; i++) {
      if (std::abs((x[j]-X[i])/h)<=1) {
        sum += 3.0/4.0 * (1-std::pow((x[j]-X[i])/h,2)) ;
      }
    }
    ret(j) = sum/(n*h) ;
  }
  
  return ret ;
}
