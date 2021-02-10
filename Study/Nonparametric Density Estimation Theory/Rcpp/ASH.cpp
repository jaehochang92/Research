#include <Rcpp.h>
using namespace Rcpp ;

// [[Rcpp::export]]
NumericVector Cfx (NumericVector x, NumericVector data, int K, int m) {
    int J = x.length() ; int n = data.length() ;
    double h = (max(data) - min(data))/K ;
    double eps = h ; NumericVector ret(J) ;
    
    for (int j = 0 ; j < J ; j++){
      double sum = 0.0 ;
      for (int i = 0 ; i < n ; i++) {
        for (int k = 1 ; k < (K*m) ; k++) {
          for (int l = (1 - m) ; l < (m - 1) ; l++) {
            if (k == 1) {
              sum += ((1 - std::fabs(l)/m)*((k - 1)*h/m - eps <= x(j) & x(j) < k*h/m) *
                ((k + l - 1)*h/m <= data(i) & data(i) < (k + l)*h/m)) ;
            }else if (k == K*m) {
              sum += ((1 - std::fabs(l)/m)*((k - 1)*h/m <= x(j) & x(j) < k*h/m) *
                ((k + l - 1)*h/m <= data(i) & data(i) < (k + l)*h/m + eps)) ;
            }else {
              sum += ((1 - std::fabs(l)/m)*((k - 1)*h/m <= x(j) & x(j) < k*h/m) *
                ((k + l - 1)*h/m <= data(i) & data(i) < (k + l)*h/m)) ;
            }
          }
        }
      }
      ret(j) = sum ;
    }
    return ret/(n*h) ;
}
