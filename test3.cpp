#include <Rcpp.h>

using namespace Rcpp;

//Rows with zero gene expression give no real insight
//Basic program below removes rows which wholly contain zeroes

//Gene Expressions >=0, therefore sum determines a zero row
//[[Rcpp::export]]
bool zeroRow(NumericVector x){
	int s=std::accumulate(x.begin(), x.end(), 0);
	if (s == 0){
		return TRUE;
	}else{
		return FALSE;
	}
}

//Transform each row into column vector to allow the above function to be applied
//[[Rcpp::export]]
NumericVector pickRowI(NumericMatrix x, int i){
  int cols = 200;
  NumericVector y(cols);
  for (int j=0; j < cols; ++j){
    y(j)=x(i,j);
  }
  return y;
}

//Final deletion of zero rows
//[[Rcpp::export]]
NumericMatrix rowDelete(NumericMatrix x){
  n=x.nrow(); m=x.ncol();
  int k = 0;
  for(int i = 0; i < n; ++i)
    if (zerorow(pickRowI(x,i))=TRUE){
      ++k; 
      for(int j=0; j < m; ++j){
        
      }
    }
}
  
  
  
  
