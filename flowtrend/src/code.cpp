#include <RcppArmadillo.h>
#include <Rcpp.h>
using namespace Rcpp;
// Generated from _main.Rmd: do not edit by hand
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::depends(RcppEigen)]]
#include <RcppEigen.h>
#include <numeric>

using namespace arma;
using namespace Eigen;

using Eigen::Map;                       // 'maps' rather than copies
using Eigen::MatrixXd;                  // variable size matrix, double precision

//' Solve "barebones" sylvester equation that takes upper triangular matrices as coefficients.
//'
//' @param TA Upper-triangular matrix
//' @param TB Upper-triangular matrix
//' @param C matrix
//' @export
// [[Rcpp::export]]
Eigen::MatrixXd matrix_function_solve_triangular_sylvester_barebonesC2(const Eigen::MatrixXd & TA, 
								     const Eigen::MatrixXd & TB,
								     const Eigen::MatrixXd & C){
  // Eigen::eigen_assert(TA.rows() == TA.cols());
  // Eigen::eigen_assert(TA.Eigen::isUpperTriangular());
  // Eigen::eigen_assert(TB.rows() == TB.cols());
  // Eigen::eigen_assert(TB.Eigen::isUpperTriangular());
  // Eigen::eigen_assert(C.rows() == TA.rows());
  // Eigen::eigen_assert(C.cols() == TB.rows());

  // typedef typename MatrixType::Index Index; 
  // typedef typename MatrixType::Scalar Scalar;

  int m = TA.rows();
  int n = TB.rows();
  Eigen::MatrixXd X(m, n);

  for (int i = m - 1; i >= 0; --i) {
    for (int j = 0; j < n; ++j) {

      // Compute T_A X = \sum_{k=i+1}^m T_A_{ik} X_{kj}
      double TAX;
      if (i == m - 1) {
      	TAX = 0;
      } else {
	MatrixXd TAXmatrix = TA.row(i).tail(m-1-i) * X.col(j).tail(m-1-i);
      	TAX = TAXmatrix(0,0);
      }

      // Compute X T_B = \sum_{k=1}^{j-1} X_{ik} T_B_{kj}
      double XTB;
      if (j == 0) {
      	XTB = 0;
      } else {
      	MatrixXd XTBmatrix = X.row(i).head(j) * TB.col(j).head(j);
      	XTB = XTBmatrix(0,0);
      }

      X(i,j) = (C(i,j) - TAX - XTB) / (TA(i,i) + TB(j,j));
    }
  }
  return X;
}

// [[Rcpp::depends(RcppArmadillo)]]

static double const log2pi = std::log(2.0 * M_PI);

/* C++ version of the dtrmv BLAS function */
void inplace_tri_mat_mult(arma::rowvec &x, arma::mat const &trimat){
  arma::uword const n = trimat.n_cols;

  for(unsigned j = n; j-- > 0;){
    double tmp(0.);
    for(unsigned i = 0; i <= j; ++i)
      tmp += trimat.at(i, j) * x[i];
    x[j] = tmp;
  }
}


// [[Rcpp::export]]
arma::vec dmvnorm_arma_fast(arma::mat const &x,
                           arma::rowvec const &mean,
                           arma::mat const &sigma,
                           bool const logd = false) {
    using arma::uword;
    uword const n = x.n_rows,
             xdim = x.n_cols;
    arma::vec out(n);
    arma::mat const rooti = arma::inv(trimatu(arma::chol(sigma)));
    double const rootisum = arma::sum(log(rooti.diag())),
                constants = -(double)xdim/2.0 * log2pi,
              other_terms = rootisum + constants;

    arma::rowvec z;
    for (uword i = 0; i < n; i++) {
        z = (x.row(i) - mean);
        inplace_tri_mat_mult(z, rooti);
        out(i) = other_terms - 0.5 * arma::dot(z, z);
    }

    if (logd)
      return out;
    return exp(out);
}

// All credit goes to https://gallery.rcpp.org/articles/dmvnorm_arma/
