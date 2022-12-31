#include <iostream>
#include <cmath>
#include <RcppArmadillo.h>
#include <Rmath.h>
#include <Rdefines.h>
#include <algorithm>
#include <iterator>
// [[Rcpp::depends(RcppArmadillo)]]


// arma::mat rwish(const double& v, const arma::mat& S) {
//   int p = S.n_rows;
//   arma::mat R = arma::chol(S);
//   arma::mat A(p,p);
//   // std::generate(A.begin(), A.end(), ::norm_rand);
//   for (int i = 0; i < p-1; ++i) {
//     for (int j = i+1; j < p; ++j) {
//       A(j,i) = ::norm_rand();
//     }
//   }
//   // A = arma::trimatl(A);
  
//   for (int i = 0; i < p; ++i) {
//     A(i,i) = std::sqrt(::Rf_rchisq(v-(double)i));
//   }
//   return R.t() * A * A.t() * R;
// }


// [[Rcpp::export]]
arma::mat test(int &n) {
    // arma::mat I(n, n, arma::fill::eye);
    // return arma::wishrnd(I, 10.0);
    double df = 10.0;
    return arma::chi2rnd(df, 3, 3);
}
