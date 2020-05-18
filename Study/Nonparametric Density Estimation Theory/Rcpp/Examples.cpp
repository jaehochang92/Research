#include <Rcpp.h>
using namespace Rcpp ;

// [[Rcpp::export]]
NumericVector NestedLoop(int x) {
  NumericMatrix ret(x,x) ;
  for (int i = 0 ; i < x ; i++) {
    for (int j = 0 ; j < x ; j++) {
      ret(i,j) = i+1 + j+1 ;
    }
  }
  return ret ;
}
