#include <Rcpp.h>
using namespace Rcpp;

NumericVector partial_dcor(NumericMatrix Dx, NumericMatrix Dy, NumericMatrix Dz);
double        partial_dcov(NumericMatrix Dx, NumericMatrix Dy, NumericMatrix Dz);

NumericMatrix U_center(NumericMatrix);
double        U_product(NumericMatrix U, NumericMatrix V);
NumericMatrix projection(NumericMatrix Dx, NumericMatrix Dz);

// [[Rcpp::export]]
NumericVector partial_dcor(NumericMatrix Dx, NumericMatrix Dy, NumericMatrix Dz) {
  /*  partial distance correlation, second formulation
  Dx, Dy, Dz are symmetric distance or dissimilarity matrices with zero diagonals
  partial_dcor  : vector length 4, partial_dcor[0] is pdcor
  partial_dcor returns vector [Rxyz, Rxy, Rxz, Ryz] starred versions
  */
  int    n = Dx.nrow();
  NumericMatrix A(n, n), B(n, n), C(n, n);
  double Rxy=0.0, Rxz=0.0, Ryz=0.0, Rxyz=0.0, den;
  double AB, AC, BC, AA, BB, CC, pDCOV;
  double eps = std::numeric_limits<double>::epsilon();  //machine epsilon

  A = U_center(Dx);           /* U-centering to get A^U etc. */
  B = U_center(Dy);
  C = U_center(Dz);

  AB = U_product(A, B);
  AC = U_product(A, C);
  BC = U_product(B, C);
  AA = U_product(A, A);
  BB = U_product(B, B);
  CC = U_product(C, C);
  pDCOV = U_product(projection(Dx, Dz), projection(Dy, Dz));

  den = sqrt(AA*BB);
  if (den > eps)
    Rxy = AB / den;
  den = sqrt(AA*CC);
  if (den > eps)
    Rxz = AC / den;
  den = sqrt(BB*CC);
  if (den > eps)
    Ryz = BC / den;
  den = sqrt(1 - Rxz*Rxz) * sqrt(1 - Ryz * Ryz);

  if (den > eps)
    Rxyz = (Rxy - Rxz * Ryz) / den;
  else {
    Rxyz = 0.0;
  }

  return NumericVector::create(
    _["pdcor"] = Rxyz,
    _["pdcov"] = pDCOV,
    _["Rxy"] = Rxy,
    _["Rxz"] = Rxz,
    _["Ryz"] = Ryz
  );
}


//[[Rcpp::export]]
double partial_dcov(NumericMatrix Dx, NumericMatrix Dy, NumericMatrix Dz) {
  /* pdcov following the definition via projections
  Dx, Dy, Dz are symmetric distance or dissimilarity matrices with zero diagonals
  returns pdcov sample coefficient
  */
  int    n = Dx.nrow();
  int    i, j;
  NumericMatrix A(n, n), B(n, n), C(n, n), Pxz(n, n), Pyz(n, n);
  double AC, BC, CC, c1, c2;
  double eps = std::numeric_limits<double>::epsilon();  //machine epsilon

  A = U_center(Dx);           /* U-centering to get A^U etc. */
  B = U_center(Dy);
  C = U_center(Dz);

  AC = U_product(A, C);
  BC = U_product(B, C);
  CC = U_product(C, C);

  c1 = c2 = 0.0;
  // if (C,C)==0 then C=0 and both (A,C)=0 and (B,C)=0
  if (fabs(CC) > eps) {
    c1 = AC / CC;
    c2 = BC / CC;
  }

  for (i=0; i<n; i++)
    for (j=0; j<n; j++) {
      Pxz(i, j) = A(i, j) - c1 * C(i, j);
      Pyz(i, j) = B(i, j) - c2 * C(i, j);
    }

    return U_product(Pxz, Pyz);
}

//[[Rcpp::export]]
int main(NumericMatrix Adj, NumericVector gene1, NumericVector gene2,
		double gene1_rows, double gene2_rows, double gene1_targets, double gene2_targets, int t)
		
	dcor_1t	
		
{

}

//R-code

		dcor_1t = Adj[(Adj[,1] == gene1 & Adj[,2] == t),]$strength
		pdcor1t_2 = pdcor(unlist(Mat[rownames(Mat)==gene1,]), unlist(Mat[rownames(Mat)==t,]),unlist(Mat[rownames(Mat)==gene2,]))
		dcor_2t = Adj[(Adj[,1] == gene2 & Adj[,2] == t),]$strength
		pdcor2t_1 = pdcor(unlist(Mat[rownames(Mat)==gene2,]), unlist(Mat[rownames(Mat)==t,]),unlist(Mat[rownames(Mat)==gene1,]))
		if (dcor_1t+margin < pdcor1t_2 & dcor_2t+margin < pdcor2t_1) {
			# INTERACTION
			interact_targets = c(interact_targets,t);
#		} else  if (pdcor2t_1 >= dcor_2t-margin & pdcor1t_2 <= 0+margin & pdcor1t_2 < dcor_1t-margin) {
		} else  if (pdcor2t_1 >= dcor_2t-margin & pdcor1t_2 < dcor_1t-margin) {
                        # gene1 -> gene2 -> target
                       	pathway_targets = c(pathway_targets,t);
                        if (seen) {
                                if( first != gene1 ) {seen=2} # disagreement over direction
                        } else {
                                seen = 1;
                        }
                        first <- gene1; second <- gene2;
#		} else	if (pdcor1t_2 >= dcor_1t-margin & pdcor2t_1 <= 0+margin & pdcor2t_1 < dcor_2t-margin) {
		} else	if (pdcor1t_2 >= dcor_1t-margin & pdcor2t_1 < dcor_2t-margin) {
			# gene2 -> gene1 -> target
                        interact_targets = c(interact_targets,t);
                        if (seen) {
                                if( first != gene2 ) {seen=2} # disagreement over direction
                        } else {
                                seen = 1;
                        }
                        first <- gene2; second <- gene1;
                }

