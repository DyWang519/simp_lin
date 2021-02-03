#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;
using namespace arma;

// [[Rcpp::export]]
Rcpp::List simp_lin_cpp(const arma::vec & x, const arma::vec & y) {

  int n = x.size();
  double ndbl = (double)n;
  arma::vec intercept(n, arma::fill::ones);

  arma::mat X(n, 2);
  X.col(0) = intercept;
  X.col(1) = x;

  arma::colvec betahat = arma::solve(X, y);
  arma::colvec preds = X*betahat;
  arma::colvec residuals = y - preds;
  double RSS = arma::as_scalar(arma::trans(residuals)*residuals);
  double sigma2 = RSS/(n-2);
  arma::colvec std_error = arma::sqrt(sigma2*arma::diagvec(arma::inv(arma::trans(X)*X)));

  double quant = R::qt(0.975, ndbl-2, true, false);

  arma::colvec moe = quant*std_error;
  arma::colvec lb = betahat - moe;
  arma::colvec ub = betahat + moe;

  arma::mat ci(betahat.size(), 2);
  ci.col(0) = lb;
  ci.col(1) = ub;

  return Rcpp::List::create(
    Rcpp::Named("coef") = betahat,
    Rcpp::Named("std_error") = std_error,
    Rcpp::Named("conf_interval") = ci,
    Rcpp::Named("residuals") = residuals,
    Rcpp::Named("predicted") = preds);
}
