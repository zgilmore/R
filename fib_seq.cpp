#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
int fib_seq (int n) {
	int result;
	if (n < 2) {
		result = n;
	} else {
		int a, b;
		a = fib_seq (n-1);
		b = fib_seq (n-2);
		result = a + b;
		}
	return result;
}

int fib_par (int n) {
	int result;
	if (n < 2){
		result = n;
		} else {
		int a, b;
		fork2([&] {
			a = fib_par(n-1);
		}, [&] {
			b = fib_prar(n-2);
		});
		result = a + b;
	}
	return result;
}