
assetportfolio_optm <- function(dataset,expected_return_target,weights,diag=NULL,value_desired){
   #the function returns the optimal weights after the quadratic minimization problem wehre:
   # inputs:
   # - dataset: matrix containing the prices of the assets we want to include in our portfolio
   # - expected_return_target: numeric object which represent the desired expected return of our portfolio 
   # - weights: vector containing the starting weights
   # - diag: numeric object representing the number of assets we have in our portfolio, it is used in case we want to constraint 
   #the weights to be greater to the "value desired". default = NULL
   # - value_desired: numeric object which represent the minimum value of our weights
   mydata <- dataset
 
#how to get the returns from our dataset
   myreturns <- matrix(nrow=length(row(mydata[1]))-1, ncol =length(mydata),
                     dimnames=list(1:(length(row(mydata[1]))-1),symbols))
   for (j in 1:length(mydata)){
     for (i in 2:length(row(mydata[j]))){
      
      myreturns[sum(i-1),j]<- matrix(((mydata[i,j] - mydata[i-1,j])/mydata[i-1,j]))
     }
   }
  myreturns <- data.frame(myreturns)
#variance-covariance matrix 
  var_cov = var(myreturns)
#for loop that returns a vector of mean for our dataset 
  mean <-matrix(nrow=1,ncol=length(myreturns))
  for (i in 1:length(myreturns)){
  mean[[i]]<- c(mean(myreturns[[i]]))
  
  }
  
#quadratic optimization problem
  library(quadprog)
  Dmat = 0.5*var_cov #our objective function
#construction A*x = b matrix
  dvec=  matrix(mean) #matrix containing the mean of our dataset
  A.Equality <- matrix(rep(1,length(mydata)), ncol=1) #matrix formed by all ones 
  if (is.null(diag) == TRUE){
     Amat = cbind(dvec,A.Equality,diag(length(mydata))) #matrix returning the dvec and A.Equality matrix
     bvec= c(expected_return_target,1,rep(0,length(mydata)))#the b vector
     } else{
#adding the non negativity constraint which is the main diagonal of ones (with length: n. of assets we want)
   Amat = cbind(dvec,A.Equality,diag(length(mydata)),diag(diag)) 
   bvec= c(expected_return_target,1,rep(0,length(mydata)),rep((value_desired),length(mydata))) 
}

#results
  x=solve.QP(Dmat,dvec,Amat,bvec=bvec, meq=2)$solution #optimal weights for our parameter
#check return: x*mean
  return_portfolio <- sum(x*mean)
  results =list(expected_return_target=expected_return_target, returns = myreturns, variance_covariance_matrix = var_cov, 
              expected_return_each_asset = mean, optimal_weights=x,return_portfolio_check = return_portfolio)
  
  return(results)
  }


assetportfolio_optm_2 <- function(dataset,variance_desired,diag=NULL,value){
   #the function returns the optimal weights after the linear programming problem wehre:
   # inputs:
   # - dataset: matrix containing the prices of the assets we want to include in our portfolio
   # - variance_desired: numeric object which represent the desired variance of our portfolio 
   # - weights: vector containing the starting weights
   # - diag: numeric object representing the number of assets we have in our portfolio, it is used in case we want to constraint 
   #the weights to be greater to the "value desired". default = NULL
   # - value: numeric object which represent the minimum value of our weights
   mydata <- dataset
#how to get the returns from our dataset
   myreturns <- matrix(nrow=length(row(mydata[1]))-1, ncol =length(mydata),
                       dimnames=list(1:(length(row(mydata[1]))-1),symbols))
   for (j in 1:length(mydata)){
      for (i in 2:length(row(mydata[j]))){
         
         myreturns[sum(i-1),j]<- matrix(((mydata[i,j] - mydata[i-1,j])/mydata[i-1,j]))
      }
   }
   myreturns <- data.frame(myreturns)
#variance-covariance matrix 
   var_cov = var(myreturns)
#for loop that returns a vector of mean for our dataset 
   mean <-matrix(nrow=1,ncol=length(myreturns))
   for (i in 1:length(myreturns)){
      mean[[i]]<- c(mean(myreturns[[i]]))
   }
   var = as.vector(diag(var_cov))
#optimization problem
#we require the mean vector in order to construct the objective function u*w
   f.obj <- mean #c_t * w  objective function
   if (is.null(diag) == TRUE){
      f.con <- matrix(c(rep(1,length(mydata)),var),nrow=2,ncol=length(mydata),byrow=TRUE) #linear system byrow
      f.dir <- c("=","=")
      f.rhs <-c(1,variance_desired)
      w=lp(direction="max", f.obj, f.con, f.dir, f.rhs)$solution #this function gives us the optimal weights 
    }else{
# we now compute the linear system byrow
   f.con <- matrix(c(rep(1,length(mydata)),var,diag(diag)),nrow=2+diag,ncol=length(mydata),byrow=TRUE) #linear system byrow
   f.dir <- c("=","=",rep(">",diag))
   f.rhs <-c(1,variance_desired,rep(value,diag))
   w=lp(direction="max", f.obj, f.con, f.dir, f.rhs)$solution}
# maximization problem results
   wealth=sum(w*mean)#wealth we are able to reach if we invest on the number of assets using the weights
   results =list(variance_desired = variance_desired, returns = myreturns, variance_covariance_matrix = var_cov, 
                 expected_return_each_asset = mean,variances=var, optimal_weights=w,return_optimization = wealth)
   
   return(results)
}

