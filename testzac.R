zac.test <-function(m,n){
	library('doParallel')
	n_cores<-detectCores()-1;
	cl<-makeCluster(n_cores);
	registerDoParallel(cl)
	
x <- foreach(i=1:m, .combine='rbind') %:%
   foreach(j=1:n, .combine='c') %dopar% {
     i * j   
   }

	stopCluster(cl)
	print(x)
}

zac.test2 <- function(n,m){
	A=matrix(NA,nrow=n,ncol=m);
	f_outer<-function(i){		
	f_inner<-function(j){
		return(i*j)
	}
		A[i,1:m]<-sapply(1:m, f_inner)
	}
	A[1:n,]<-sapply(1:n, f_outer)
	print(A)
}

zac.test3 <- function(n,m){
	A=matrix(NA,nrow=(2*n),ncol=m);
	B=matrix(0,nrow=(2*n),ncol=m);
	library('snow');
	library('parallel');
	n_cores<-detectCores()-1;
	cl<-makeCluster(n_cores);
	f_outer<-function(i){	
	f_inner<-function(j){
			if(i == j){return(0)}
			else{ if(i>n){
					return((i-n)*j)}
					else{
						return(i*j)
						}
						}
		}
		A[i,1:m]<-sapply(1:m, f_inner)
	}
	A[1:n,]<-parSapply(cl,1:n, f_outer)
	A[(n+1):(2*n),]<-parSapply(cl,(n+1):(2*n), f_outer)
	B[1:(2*n),]<-parSapply(cl,1:(2*n), f_outer)
	stopCluster(cl)
	print(A)
	print(B)
}


zac.test4<- function(n,m){
	A=matrix(0,nrow=n,ncol=m);
	library('snow');
	library('parallel');
	#Calcuate the number of cores required
	no_cores<-detectCores()-1;
	#Initiate cluster
	cl<-makeCluster(no_cores);
	parLapply(cl, 1:n, function(i){
		lapply(1:m, function(j){
			A[i,j]=i*j;
		})
	})
	stopCluster(cl);
	print(A)
}

lowfunc=function(x){
			y=ceiling(length(x)/4);
			z<-sort(x);
			if(z[y]==0){return(unlist(0))}
			else{return(unlist(quantile(x,c(0.25))))}
		}
		
		highfunc=function(x){
			y=ceiling(3*length(x)/4);
			z<-sort(x);
			if(z[y]==0){return(0)}
			else{return(unlist(quantile(x,c(0.75))))}
		}
		
		
cor.test.somevsall <-function(mat,rows,method){
	N=length(mat[,1]);
	M=length(rows);
	pvals = matrix(rep(NA,times=N*M),nrow=M,ncol=N);
	strength = matrix(rep(NA,times=N*M),nrow=M,ncol=N);
	library('snow');
	library('parallel');
	n_cores<-detectCores()-1;
	cl<-makeCluster(n_cores);
	f_outer1<-function(j){
		row=rows[j];
		f_inner1=function(i){
			if(i == j){return(1)} #Gene no target itself
else{ test=cor.test(unlist(mat[row,]),unlist(mat[i,]),method=method)
			return(test$p.val)}
			}		
		pvals[j,1:N]<-sapply(1:N, f_inner1)

	}
	pvals[1:M,]<-parSapply(cl,1:M, f_outer1);

	f_outer2<-function(k){
		row=rows[k];
		f_inner2=function(l){test=cor.test(unlist(mat[row,]),unlist(mat[l,]),method=method)
			return(test$estimate)			
			}

		strength[k,1:N]<-sapply(1:N, f_inner2)
					}
	strength[1:M,]<-parSapply(cl,1:M, f_outer2);		
	stopCluster(cl);	
			output=list();
			output$pvals=pvals;
			output$strength=strength;
			colnames(output$strength)=rownames(mat)
			rownames(output$strength)=rownames(mat)[rows]
			colnames(output$pvals)=rownames(mat)
			rownames(output$pvals)=rownames(mat)[rows]
			output
}

cor.test.somevsall2 <- function(mat, rows, method) {
        N = length(mat[,1]);
        M = length(rows);
	library('doParallel');
	n_cores<-detectCores()-1;
	cl<-makeCluster(n_cores);
	registerDoParallel(cl)
 
 x<-    foreach (i=1:M, .combine='rbind', .packages='foreach') %dopar% {
                row = rows[i];
        foreach (j=1:N, .combine='c') %dopar% {
                test <- cor.test(unlist(mat[row,]),unlist(mat[j,]), method=method)
                if (i == j){data.frame(pv=1, st=test$estimate)} # Gene no target itself
                else{data.frame(pv=test$p.val, st=test$estimate)}
        }
        }
        stopCluster(cl)
        pvals<-x[,grep("pv",colnames(x))]
        strength<-x[,grep("st",colnames(x))]
        output = list();
        output$pvals=pvals;
		output$strength=strength;
        row.names(output$pvals) <- rownames(mat)[rows]
        colnames(output$pvals) <- rownames(mat)
        row.names(output$strength) <- rownames(mat)[rows]
        colnames(output$strength) <- rownames(mat)       
        output
}


applyKernel <-function(newX, FUN, d2, d.call, dn.call=NULL, ...){
	foreach(x=iter(newX, by='col')) %dopar%
		FUN(array(x, d.call, dn.call),...)
	}

deleterow <- function(mat){
	N=length(mat[,1]);
	O=matrix(0,N,1);
	library('snow');
	library('parallel')
	n_cores<-detectCores()-1
	cl<-makeCluster(n_cores)
	
	zerorow=function(i){if( sum(mat[i,]) == 0) {return(0)}
	else{return(1)}}
	
	O[1:N,]=parSapply(cl,1:N,zerorow);
	stopCluster(cl)
	P= which(O !=0, arr.ind=T);	
	return(mat[P[,1],])
}

#prevent t-test errors from crashing
my.t.test <- function(...) {
    obj<-try(t.test(...), silent=TRUE)
    if (is(obj, "try-error")) {
		obj$p.value=NA
		return(obj) 
	}else{ 
		return(obj)
	}
}


dcor.test.somevsall <- function(mat2, rows) {
        mat=deleterow(mat2);
        N = length(mat[,1]);
        M = length(rows);
        library('doParallel');
		n_cores<-detectCores()-1;
		cl<-makeCluster(n_cores);
		registerDoParallel(cl)
x <-	        foreach(i=1:M, .combine='rbind', .packages='foreach') %dopar% {
        row = rows[i];
        
		y1=ceiling(N/4);
		y2=ceiling(3*N/4);
		z<-sort(mat[row,]);
		if(z[y1] == 0){low <- 0}
		else {low <- unlist(quantile(mat[row,],c(0.25)))}
			
		if(z[y2] == 0){high <- 0}
		else {high <- unlist(quantile(mat[row,],c(0.75)))}

        foreach(j=1:N, .combine='c') %dopar% {
        require("energy")
        require("pdcor2")
        test = dcor.ttest(unlist(mat[row,]),unlist(mat[j,]))
        	
        	my.t.test <- function(...) {
    			obj<-try(t.test(...), silent=TRUE)
    				if (is(obj, "try-error")) {
						obj$p.value=NA
						return(obj) 
					}else{ 
						return(obj)
					}
				}

        
		test_dir = my.t.test(mat[j,(mat[row,]<=low)],mat[j,(mat[row,]>=high)])
		if (j == row) {

		if (is.na(test_dir$p.value)) {
			data.frame(pv=1,st=test$estimate,d=0);
		} else {
			if (low != high & test_dir$p.value < 0.05) {
				if (test_dir$estimate[1] < test_dir$estimate[2]) {
					data.frame(pv=1,st=test$estimate,d=1);
				} else {
					data.frame(pv=1,st=test$estimate,d=-1);
				}
			} else {
				data.frame(pv=1,st=test$estimate,d=0);
			}
		}
        }# Gene no target itself.
        else{
        	if (is.na(test_dir$p.value)) {
			data.frame(pv=test$p.val,st=test$estimate,d=0);
		} else {
			if (low != high & test_dir$p.value < 0.05) {
				if (test_dir$estimate[1] < test_dir$estimate[2]) {
					data.frame(pv=test$p.val,st=test$estimate,d=1);
				} else {
					data.frame(pv=test$p.val,st=test$estimate,d=-1);
				}
			} else {
				data.frame(pv=test$p.val,st=test$estimate,d=0);
			}
		}
		}
        	
        }
        }
        stopCluster(cl)
        pvals<-x[,grep("pv",colnames(x))]
        strength<-x[,grep("st",colnames(x))]
        direction<-x[,grep("d",colnames(x))]
        output = list();
        output$pvals = pvals;
        output$strength = strength;
	output$dir = direction
        colnames(output$strength) = rownames(mat)
        rownames(output$strength) = rownames(mat)[rows]
        colnames(output$pvals) = rownames(mat)
        rownames(output$pvals) = rownames(mat)[rows]
        colnames(output$dir) = rownames(mat)
        rownames(output$dir) = rownames(mat)[rows]
        output
}

library(Rcpp)

cppFunction('NumericVector rowSumsC(NumericMatrix x) {
	int nrow = x.nrow(), ncol = x.ncol();
	NumericVector out(nrow);
	
	for (int i=0; i < nrow; i++){
		double total = 0;
		for (int j = 0; j < ncol; j++) {
			total += x(i,j );
		}
		out[i] = total;	
	}
	return out;
}')