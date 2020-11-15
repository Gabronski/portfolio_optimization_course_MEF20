
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
#for loop that returns a vector of mean for our dataset  chiamare mean in un altro modo
  mean_1 <-matrix(nrow=1,ncol=dim(myreturns)[2])
  for (i in 1:dim(myreturns)[2]){
  mean_1[[i]]<- c(mean(myreturns[[i]]))
  
  }
  
#quadratic optimization problem
  library(quadprog)
  Dmat = 0.5*var_cov #our objective function
#construction A*x = b matrix
  #dvec=  matrix(mean) #matrix containing the mean of our dataset
  dvec = t(rep(0, dim(myreturns)[2]))
  A.Equality <- matrix(rep(1,length(mydata)), ncol=1) #matrix formed by all ones 
  if (is.null(diag) == TRUE){
     Amat = cbind(t(mean_1),A.Equality) #matrix returning the dvec and A.Equality matrix
     bvec= c(expected_return_target,1)#the b vector
     } else{
#adding the non negativity constraint which is the main diagonal of ones (with length: n. of assets we want)
   Amat = cbind(t(mean_1),A.Equality,diag(length(mydata)),diag(diag)) 
   bvec= c(expected_return_target,1,rep(0,length(mydata)),rep((value_desired),length(mydata))) 
}

#results
  x=solve.QP(Dmat,dvec,Amat,bvec=bvec, meq=2)$solution #optimal weights for our parameter

  #check return: x*mean
  return_portfolio <- sum(x*mean_1)
  results =list(expected_return_target=expected_return_target, returns = myreturns, variance_covariance_matrix = var_cov, 
              expected_return_each_asset = mean_1, optimal_weights=x,return_portfolio_check = return_portfolio)
  
  return(results)
  }

#mean-variance frontier construction
mean_var_front <- function(myminimizationproblem,expected_return_target,value_max=NULL,value_min=NULL,interval=NULL){
   expected_return_target <- expected_return_target
   port_optmized <- Myminimization_problem1          #myminimizationproblem
   #vector of expected returns from the minimization problem
   mu = c(port_optmized$expected_return_each_asset)
   mu_t = t(t(mu))
   var = port_optmized$variance_covariance_matrix
   #the inverse of our variance covariance matrix
   v_1 = solve(port_optmized$variance_covariance_matrix)
  
   ones=rep(1,length(mu))
   ones_t = t(t(ones))
   #now find A,B,C,D
   A = as.vector(ones%*%v_1%*%mu_t)
   B = as.vector(mu%*%v_1%*%mu_t)
   C = as.vector(ones%*%v_1%*%ones_t)
   D = as.vector((B*C) - A^2)
   # I need now h and g to be found in order to construct my frontier equation w = return_portfolio*h + g
   h = C/D*v_1%*%mu_t - A/D*v_1%*%ones
   g=B/D*v_1%*%ones - A/D*v_1%*%mu_t
  #minimum variance portfolio
   mu_p <- A/C 
   sigma_sq <- 1/C
   sigma <- sqrt(sigma_sq)
   min_port <- c(sigma=sigma,mu_p=mu_p)
   min_port <- data.frame(min_port)
   #our efficient portfolio, which is the same of the minimization problem
   efficient_portfolio_var = (C/D)*((expected_return_target - A/C)^2) + 1/C
   efficient_portfolio_sigma = sqrt(efficient_portfolio_var)
   efficient_portfolio_point = data.frame(sigma=efficient_portfolio_sigma,exp_r_port_eff=expected_return_target)
   #if we do not personalize our domain, the function do it automatically 
   if (is.null(value_min) == TRUE & is.null(value_max) == TRUE & is.null(interval) == TRUE){
      interval = 0.00001
      vector_exp_ret <- seq(from=mu_p-0.0001,to= expected_return_target + 0.0001,by=interval)
      }else{
      vector_exp_ret <- seq(from=value_min, to=value_max, by=interval)
      #if the value max is lower than the expected_return_target, we cannot build the line connecting all the points until a general expected return target
      if (value_max <=expected_return_target )  warning('value_max should be greater than the expected_return_target')
      
      }
   # for loop used to construct the values of my efficient frontier
   myfrontier<- function(vector_exp_ret){
      sigma2p <-c()
      for (i in 1:length(vector_exp_ret)){
         sigma2p[i] <- c((C/D*((vector_exp_ret[i]-A/C)^2) + 1/C))}
         sigma_port <- c()
      for (i in 1:length(sigma2p)){
         sigma_port[i] <- c(sqrt(sigma2p[i]))}
         mean_var_values <- data.frame(cbind("sigma_port"=sigma_port,"exp_r_port"=vector_exp_ret))
      return(mean_var_values)
   }
   
   mean_variance_front <- myfrontier(vector_exp_ret)
   mean_variance_front 
   mean_variance_front <- data.frame(mean_variance_front)
      x=ggplot(data= mean_variance_front, aes(x = sigma_port, y=exp_r_port))  +
            geom_hline(yintercept = mu_p) +
      geom_point(data = mean_variance_front, aes(x = sigma_port,y = exp_r_port), color = "grey" ) +
      geom_point(data = min_port,aes(x =sigma, 
                                           y =mu_p), color = "red", size = 3, shape = 18) +
      geom_point(data = efficient_portfolio_point, aes( x=sigma,y=exp_r_port_eff),color="blue",size=3,shape=18)+
      theme_bw() + ggtitle("Mean Variance Portfolio") +
      xlab("Standard Deviations") + ylab("Expected Returns") 
   results=list(final_results=x,mean_variance_frontier = mean_variance_front, minimum_portfolio=min_port,
                efficient_portfolio = efficient_portfolio_point)
   return(results)}

