#include <Rcpp.h>
using namespace Rcpp;

//' @name CLap
//' @title Laplace sample by Rcpp
//' @description Generate the Laplace sample by Rcpp,using the normal distribution
//' @param sigma the SD of Normal distribution
//' @param x0 the first data
//' @param N the sample size
//' @return a list,x stands for random sample of Laplace sample,k is the acceptance number
//' @examples
//' \dontrun{
//' res <- CLap(sigma,x0,N)
//' }
//' @export
//[[Rcpp::export]]
List CLap(double sigma, double x0, int N) {
  NumericVector x(N);
  x[0]=x0;
  NumericVector u(N);
  u=runif(N);
  int k=0;
  double r;
  for(int i=1; i<N; i++){
    double y;
    y=rnorm(1,x[i-1],sigma)[0];
    r = exp(abs(x[i - 1]) - abs(y));
    if(u[i]<=r)
      x[i]=y;
    else{
      x[i]=x[i-1];
      k++;
    }
  }
  List out;
  out["x"]=x;
  out["k"]=k;
  return out;
  //return x;
}

