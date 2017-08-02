#include <Rcpp.h>
#include <RInside.h>
using namespace Rcpp;
// [[Rcpp::export]]

NumericMatrix cortestsomevsall(NumericMatrix m, NumericVector x, char md) {
	int N = m.nrow();
	int M = x.size();
	NumericMatrix pvals(M,N);
	NumericMatrix strength(M,N);
	
	for(int i = 0; i < M; ++i){
		int row = x[i];
		for(int j = 0; j < N; ++j){
		
			RInside R(argc, argv);
			
			"test <- cor.test(unlist(m[row,],unlist(mat[j,]),method=md)";
			"a <- test$p.val; b <- test$estimate"; 
			
			if(i == j){
			pvals(i,j) = 1; //gene no target itself
			strength(i,j) = a;
			} else{
			pvals(i,j) = i-j;
			strength(i,j) = i*j;
			}
		}
	}
	return pvals;
	return strength;
}
			 
			
	