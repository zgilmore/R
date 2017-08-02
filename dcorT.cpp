#include <Rcpp.h>

//implementation of the distance correlation t-test taken from the R source code

using namespace Rcpp

//[[Rcpp::export]]
NumericVector rowMeans(NumericMatrix d){
	//returns the mean values of the matrix for each row
	int n=d.nrow();
	for (int i = 0; i<n; ++i){
		NumericVector m(i) = sum(d[i]);
	}
	return m
}

//[[Rcpp::export]]
NumericMatrix sweep(NumericMatrix d, int n, NumericVector x){
	/*simple rework of the R function
	sole use to aid calculate the modified doubly centred matrix
	d=distance matrix, n==margin, x=row/col means */
	if (n>2 && n<1){
	std::cout << 'Margin must be defines as either 1 or 2' <<std:endl;
	}else{
		if(n==1){
		int row=d.nrow();
			for(int i=0; i<row; ++i){
			NumericMatrix a[i][:]=d[i][:]-transpose(x(:)); //need to rework the C++ code here though
			}
			return a;
		}
		if (n==2){
		int col=d.ncol();
			for(int j=0; j<col; ++j){
			NumericMatrix b[:][i]=d[:][i]-x(;); //need to rework C++ code
			}
			return b;
		}
	} 
}

//[[Rcpp::export]]
/* rework of the R function
d is a distance matrix or distance object
modified or corrected doubly centered distance matrice denotes A* (or B*)
as per the JMVA t-test paper (2013)*/

NumericMatrix Astar(NumericMatrix d){
	int n=d.ncols;
	if (n != int n2=d.nrow;){
	std::cout << "Argument d should be distance Matrix" << std::endl;
	return 0;
	} else{
	NumericVector m = rowMeans(d);
	double M = mean(d);
	NumericMatrix a = sweep(d,1,m);
	NumericMatrix b = sweep(a,2,m);
	NumericMatrix A = b+M;
	//correction to get A^* as per the paper
	NumericMatrix As = A - d/n;
	diag(A) = m-M; //Need to ensure this is in sugar 
	return NumericMatrix AS = (n/(n-1))*A;
	}
}

//[[Rcpp::export]]
/* rework of the R function
compute the bias corrected distance correlation
attempt to check if the distance flag is valid */

NumericMatrix BCDCOR(NumericMatrix x, NumericMatrix y, 