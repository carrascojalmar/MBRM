#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix Kappas(IntegerVector y) {
  int n = y.size();
  IntegerVector ksizes(n);
  for (int i = 0; i < n; i++) {
    ksizes[i] = y[i] + 1;
  }

  int total = 1;
  for (int i = 0; i < n; i++) {
    total *= ksizes[i];
  }

  NumericMatrix out(total, n);
  for (int col = 0; col < n; col++) {
    int repeat_len = 1;
    for (int k = col + 1; k < n; k++) repeat_len *= ksizes[k];
    int block = 1;
    for (int k = 0; k < col; k++) block *= ksizes[k];
    for (int i = 0; i < block; i++) {
      for (int val = 0; val < ksizes[col]; val++) {
        for (int j = 0; j < repeat_len; j++) {
          int index = i * ksizes[col] * repeat_len + val * repeat_len + j;
          out(index, col) = val;
        }
      }
    }
  }
  return out;
}

// [[Rcpp::export]]
double dBer_LGG(NumericVector theta,
                    NumericMatrix X,
                    IntegerVector y,
                    bool return_log = true) {

  double lambda = theta[0];
  double phi = 1.0/(lambda * lambda);

  NumericVector beta(theta.begin()+1,theta.end());

  int p = X.nrow();
  int q = X.ncol();
  NumericVector u(p);

  for (int i = 0; i < p; i++) {
    double xb = 0.0;
    for (int j = 0; j < q; j++) {
      xb += X(i, j) * beta[j];
    }
    u[i] = std::exp(xb);
  }

  NumericMatrix kappas = Kappas(y);
  int nk = kappas.nrow();
  int mi = y.size();

  NumericVector kappa_sum(nk);
  for (int l = 0; l < nk; l++) {
    double s = 0;
    for (int j = 0; j < mi; j++) {
      s += kappas(l, j);
    }
    kappa_sum[l] = s;
  }

  double u_sum = sum(u);

  NumericVector aux0(nk);

  for (int l = 0; l < nk; l++) {
    double sum_diff = 0;
    for (int j = 0; j < mi; j++) {
      sum_diff += u[j]*(kappas(l,j)-y[j]);
    }

    double aux2 = pow(sum_diff + u_sum + phi, -phi);
    double aux3 = (static_cast<int>(kappa_sum[l]) % 2 == 0) ? 1.0 : -1.0;
    aux0[l] = aux2 * aux3;
  }

  double ld = pow(phi, phi)*sum(aux0);
  if (!return_log) return ld;
  if (ld <= 0.0 || !R_finite(ld)) return -1e10;
  return log(ld);
}

// [[Rcpp::export]]
double lvero(NumericVector theta, List X_list, List y_list){

  int n = y_list.size();
  double total = 0.0;
  for (int i = 0; i < n; i++) {
    NumericMatrix Xi = X_list[i];
    IntegerVector yi = y_list[i];
    double logli = dBer_LGG(theta, Xi, yi, true);
    total += logli;
  }
  return -total;
}

