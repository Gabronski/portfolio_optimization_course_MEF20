setwd("/Users/gabrielepiergallini/R")

# binomial_model: this function estimates a uniperiodal binomial model with non-arbitrage constaints and applies a maximization problem on the expected
# utility using the Martingale Approach
# inputs:
# dataset: object containing the prices of the asset you want
# u,d,pu: objects of class numeric containing the initial values of the upward, downward movements and the real probability of upward movement
# interest_rate: object of class numeric containing the yearly interest rate
# initial_price: object of class numeric containing the initial price of your risky asset
# type: object of class character where you decide if the interest rate is:
#      -  NULL : if the interest rate is computed on a yearly basis
#      -"semiannual" : if the interest rate is computed on a semi-annual basis
#      -"quarterly" : if the interest rate is computed on a quarterly basis
#      -"monthly" : if the interest rate is computed on a monthly basis")
# symbols : object of class character containing a vector of tickets of your risky assets
binomial_model <- function(dataset,u,d,pu,interest_rate, wealth,initial_price,type = NULL){
  
  mydata <- dataset #your dataset
  s0 <- initial_price   #initial price
  u <- u  #upward movement
  d <- d  #downward movement
  pu <- pu #probability of the upward movement
  interest_rate <- interest_rate    #yearly interest rate
  wealth <- wealth   #initial wealth
  w0 <- c(u,d,pu)    #your control variables in order to find the optimal values of u,d,pu and consequently 1-pu
  pd <- 1-pu     
  
  
  if (is.null(type) == TRUE){ #by default yearly interest rate
    interest_rate <- interest_rate
  }else{
    if (type == "monthly"){
      interest_rate <- (1+interest_rate)^(1/12)
    }else if (type == "weekly"){
      interest_rate <- (1+interest_rate)^(1/52)
    }else if (type == "daily"){
      interest_rate <- (1+interest_rate)^(1/365)
    }
  }
  #in case the data inputs do not satisfy the entry requirements of the function:
  if ( is.null(type) ==FALSE && (type !="monthly") && (type !="weekly") && (type != "daily"))
    stop ("the variable 'type' should be:
             
             -  NULL : if the interest rate is computed on a yearly basis
             - 'monthly' :  if the interest rate is computed on a monthly basis
             - 'weekly' : if the interest rate is computed on a weekly basis
             - 'daily' : if the interest rate is computed on a daily basis")
  
  
  #how to get the returns from our dataset
  myreturns <- matrix(nrow=length(row(mydata[1]))-1, ncol =length(mydata),
                      dimnames=list(1:(length(row(mydata[1]))-1),symbols))
  for (j in 1:length(mydata)){
    for (i in 2:length(row(mydata[j]))){
      
      myreturns[sum(i-1),j]<- matrix(((mydata[i,j] - mydata[i-1,j])/mydata[i-1,j]))
    }
  }
  
  myreturns2 <- myreturns^2
  myreturns3 <- myreturns^3
  myreturns4 <- myreturns^4
  mean_myreturns <- mean(myreturns)
  mean_myreturns2 <- mean(myreturns2)
  mean_myreturns3 <- mean(myreturns3)
  mean_myreturns4 <- mean(myreturns4)
  exp_m1 <- (u-1)*pu + (d-1)*(1-pu)
  exp_m2 <- exp_m1^2
  exp_m3 <- exp_m1^3
  exp_m4 <- exp_m1^4
  sq_er1 <- (exp_m1 - mean_myreturns)^2
  sq_er2 <- (exp_m2 - mean_myreturns2)^2  
  sq_er3 <- (exp_m3 - mean_myreturns3)^2
  sq_er4 <- (exp_m4 - mean_myreturns4)^2
  
  #minimization problem
  argument <- NULL  
  eval_f <- function(x_1){ 
    for (n in 1:4){
      argument = sum(cbind(argument, ((x_1[1]-1)^n*x_1[3]+(x_1[2]-1)^n*(1-x_1[3])-mean(myreturns^n))^2))
    }
    return(argument)
  }
  
  
  eval_g_grad <- function( x_1){
    return("gradient"= c(8*x_1[3]*(x_1[1] - 1)^3*(x_1[3]*(x_1[1] - 1)^4+(1 - x_1[3])*(x_1[2] - 1)^4 - mean_myreturns4)+6*x_1[3]*(x_1[1] - 1)^2*(x_1[3]*(x_1[1] - 1)^3+
                                                                                                                                                  (1 - x_1[3])*(x_1[2]-1)^3 - mean_myreturns3)+ 2*x_1[3]*(2*x_1[1] -2)*(x_1[3]*(x_1[1] - 1)^2+(1 - x_1[3])*(x_1[2] - 1)^2 - mean_myreturns2)+
                           2*x_1[3]*(x_1[3]*(x_1[1] - 1)+(1 - x_1[3])*(x_1[2] - 1) - mean_myreturns),
                         #2nd
                         8*(1 - x_1[3])*(x_1[2]-1)^3*(x_1[3]*(x_1[1]-1)^4+(1-x_1[3])*(x_1[2]-1)^4-mean_myreturns4)+6*(1-x_1[3])*(x_1[2]-1)^2*(x_1[3]*(x_1[1]-1)^3
                                                                                                                                              +(1-x_1[3])*(x_1[2]-1)^3-mean_myreturns3)+2*(1-x_1[3])*(2*x_1[2]-2)*(x_1[3]*(x_1[1]-1)^2+(1-x_1[3])*(x_1[2]-1)^2-mean_myreturns2)+
                           (2-2*x_1[3])*(x_1[3]*(x_1[1]-1)+(1-x_1[3])*(x_1[2]-1)-mean_myreturns),
                         #3rd
                         (2*x_1[1] - 2*x_1[2])*(x_1[3]*(x_1[1] - 1)+(1 - x_1[3])*(x_1[2] - 1) - mean_myreturns) +  (2*(x_1[1] - 1)^2-2*(x_1[2]-1)^2)*(x_1[3]*(x_1[1]-1)^2
                                                                                                                                                      #(2*x_1[1]-2*x_1[2])*(x_1[3]*(x_1[1]-1)+(1-x_1[3])*(x_1[2]-1)-mean_myreturns)
                                                                                                                                                      #                 +(2*(x_1[1]-1)^2-2*(x_1[2]-1)^2)*(x_1[3]*(x_1[1]-1))^2
                                                                                                                                                      +(1-x_1[3])*(x_1[2]-1)^2-mean_myreturns2)+(2*(x_1[1]-1)^3-2*(x_1[2]-1)^3)*(x_1[3]*(x_1[1]-1)^3+(1-x_1[3])*(x_1[2]-1)^3-mean_myreturns3)
                         +(2*(x_1[1]-1)^4-2*(x_1[2]-1)^4)*(x_1[3]*(x_1[1]-1)^4+(1-x_1[3])*(x_1[2]-1)^4-mean_myreturns4)
    ))}
  
  
  local_opts <- list( "algorithm" = "NLOPT_LD_SLSQP",  #NLOPT_LN_AUGLAG
                      "xtol_rel" = 1.0e-10,     "maxeval" = 10000)
  
  
  opts <- list( "algorithm" = "NLOPT_LD_SLSQP", #NLOPT_LN_AUGLAG
                "xtol_rel" = 1.0e-10,
                "maxeval" = 10000,
                "local_opts" = local_opts)
  
  res <- nloptr( x0=w0,
                 eval_f=eval_f,
                 eval_grad_f = eval_g_grad,
                 #eval_g_ineq = eval_g_inequal,
                 #eval_jac_g_ineq = eval_jac_g,
                 opts=opts,
                 ub = c(Inf,interest_rate,1),
                 lb = c(interest_rate,-Inf,0)
  )
  
  results1 <- c(u = res$solution[1], d = res$solution[2], pu = res$solution[3], pd = 1-res$solution[3])
  
  #so results are 
  myvalues <- res$solution
  u <- res$solution[1]
  d <- res$solution[2]
  pu <- res$solution[3]
  pd = 1-pu
  myvalues <- c(u=u,d=d,pu=pu,pd=pd)
  #now we maximize utility
  b0 = 1/interest_rate
  Vu=4000
  Vd=4000
  x_4<- c(Vu,Vd)
  q_u= (interest_rate - d)/(u-d)
  q_d= (u-interest_rate)/(u-d)
  martingale_measures <- c(q_u =q_u,q_d = q_d)
  
  eval_f <- function(x_5){ 
    return( "argument" = -log(x_5[1])*pu - log(x_5[2])*(1-pu)     )
  }
  
  eval_g_grad <- function(x_5){
    return("gradient" = c((-pu)/x_5[1], (pu-1)/x_5[2]))}
  
  
  #constraint functions
  eval_g_eq <- function( x_5 ){
    return("constraint" =  b0*(x_5[1]*q_u +x_5[2]*q_d) -  wealth)
  }
  
  eval_jac_g <- function( x_5 ){
    return("gradient"= c(b0*q_u,b0*q_d))}
  
  local_opts <- list( "algorithm" = "NLOPT_LD_SLSQP",  
                      "xtol_rel" = 1.0e-14,     "maxeval" = 100000)
  
  
  opts <- list( "algorithm" = "NLOPT_LD_SLSQP",
                "xtol_rel" = 1.0e-14,
                "maxeval" = 100000,
                "local_opts" = local_opts)
  
  
  
  
  res1 <- nloptr( x0=x_4,
                  eval_f=eval_f,
                  eval_grad_f = eval_g_grad,
                  eval_g_eq = eval_g_eq,
                  eval_jac_g_eq = eval_jac_g,
                  opts=opts)
  
  #replication strategy
  
  A = cbind(c(1,1),c(u*s0,d*s0))
  
  A_1= solve(A)
  
  alpha= (A_1%*%res1$solution)
  
  optimal_weights1= alpha[1]*b0/wealth
  
  optimal_weights2= alpha[2]*s0/wealth
  optimal_weights=c(optimal_weights1,optimal_weights2)
  
  results2 <- list("martingale_measures" = martingale_measures, "final_values"=c(V_up=res1$solution[1],V_down=res1$solution[2]),"optimal_units"=c(alpha_0 = alpha[1],alpha_1 = alpha[2]),
                   "optimal_weights"= c(w_0 = optimal_weights[1],w_1 = optimal_weights[2]))
  #point4
  #so results are 
  myvalues <- res$solution
  u <- res$solution[1]
  d <- res$solution[2]
  pu <- res$solution[3]
  pd = 1-pu
  myvalues <- c(u=u,d=d,pu=pu,pd=pd)
  #now we maximize utility
  x_2 <- c(2000,4000)
  
  eval_f <- function(x_3){ 
    return( "argument" = sum(-log(x_3[1]+x_3[2]*s0*u)*pu,-log(x_3[1]+x_3[2]*s0*d)*pd)     )
  }
  
  eval_g_grad <- function(x_3){
    return("gradient" = c(
      
      ((-(pu)/(x_3[1]+s0*u*x_3[2])) +  ((pu-1)/(x_3[1]+s0*d*x_3[2]))),
      ((s0*d*(pu-1))/(s0*d*x_3[2]+x_3[1]) - (s0*pu*u)/(s0*u*x_3[2]+x_3[1]))
    ))}
  
  
  #constraint functions
  eval_g_eq <- function( x_3 ){
    return("constraint" =  x_3[1]*b0 + x_3[2]*s0 -  wealth)
  }
  
  eval_jac_g <- function( x_3 ){
    return("gradient"= c(b0,s0))}
  
  local_opts <- list( "algorithm" = "NLOPT_LD_SLSQP",  
                      "xtol_rel" = 1.0e-10,     "maxeval" = 10000)
  
  
  opts <- list( "algorithm" = "NLOPT_LD_SLSQP",
                "xtol_rel" = 1.0e-10,
                "maxeval" = 10000,
                "local_opts" = local_opts)
  
  res2 <- nloptr( x0=x_2,
                  eval_f=eval_f,
                  eval_grad_f = eval_g_grad,
                  eval_g_eq = eval_g_eq,
                  eval_jac_g_eq = eval_jac_g,
                  opts=opts, lb = c(0,-Inf))
  
  sol <- (res2$solution)
  sol <- c(alpha0 = sol[1], alpha1 = sol[2])
  optimal_weightsa= sol[1]*b0/wealth
  optimal_weightsb= sol[2]*s0/wealth
  optimal_weights_1=c(w0 = optimal_weightsa,w1 = optimal_weightsb)
  
  results3 <- list("myvalues"=myvalues,"optimal_units"=sol,"optimal_weights"= optimal_weights_1)
  print(list(optimal_measures = results1, martingale_results = results2, standard_approach_results = results3 ))
  checks <- c("check_real_prob" = sum(results1[3],results1[4]), "check_martingale_measures" = sum(results2$martingale_measures), "check_weights" = sum(optimal_weights_1))
  return(list(optimal_measures = results1, martingale_results = results2, standard_approach_results = results3, check_conditions = checks ))
}

# cluster_function: function that applies a cluster analysis in order to find the real probabilities and martingale measures in each state, then it
# it finds the optimal expected return using the martingale approach
# inputs:
# dataset: object containing the prices of the assets you want to include in your maximization problem
# interest_rate: object of class numeric containing the annual interest_rate 
# returns_matrix: object of class numeric containing the matrix of the starting points of the returns in each state
# it should be n(number of states)Xn-1(number of assets) 
# initial_prices: object of class numeric containing the vector of initial prices of your risky assets
# type: object of class character where you decide if the interest rate is:
#      -  NULL : if the interest rate is computed on a yearly basis
#      -"semiannual" : if the interest rate is computed on a semi-annual basis
#      -"quarterly" : if the interest rate is computed on a quarterly basis
#      -"monthly" : if the interest rate is computed on a monthly basis")
# symbols : object of class character containing a vector of tickets of your risky assets
cluster_function <- function(dataset, interest_rate, returns_matrix, initial_prices, initial_wealth, type = NULL, symbols ){
  mydata <- data.frame(dataset)
  A <- returns_matrix
  initial_prices <- initial_prices#vector of the risky assets' initial prices
  w0 <- initial_wealth
  symbols <- symbols
  if (is.null(type) == TRUE){ #by default yearly interest rate
    interest_rate <- interest_rate
  }else{
    if (type == "monthly"){
      interest_rate <- (1+interest_rate)^(1/12)-1
    }else if (type == "weekly"){
      interest_rate <- (1+interest_rate)^(1/52)-1
    }else if (type == "daily"){
      interest_rate <- (1+interest_rate)^(1/365)-1
    }
  }
  #in case the data inputs do not satisfy the entry requirements of the function:
  if ( is.null(type) ==FALSE && (type !="monthly") && (type !="weekly") && (type != "daily"))
    stop ("the variable 'type' should be:
             
             -  NULL : if the interest rate is computed on a yearly basis
             - 'monthly' :  if the interest rate is computed on a monthly basis
             - 'weekly' : if the interest rate is computed on a weekly basis
             - 'daily' : if the interest rate is computed on a daily basis")
  
  A <- cbind(rep(interest_rate,nrow(A)),A)
  myreturns <- matrix(nrow=length(row(mydata[1]))-1, ncol =length(mydata),
                      dimnames=list(1:(length(row(mydata[1]))-1),symbols))
  for (j in 1:length(mydata)){
    for (i in 2:length(row(mydata[j]))){
      
      myreturns[sum(i-1),j]<- matrix(((mydata[i,j] - mydata[i-1,j])/mydata[i-1,j]))
    }
  }
  
  price0 <- c(bond = 1/(1+interest_rate), initial_prices)
  
  cluster1 <- matrix(nrow=nrow(myreturns),ncol=ncol(A))
  distance <- c()
  for (j in 1:ncol(A)){
    for (i in 1:nrow(myreturns)){
      cluster1[i,j] <- matrix(sum((myreturns[i,] - A[j,-1])^2))
      distance[i] <- c(min(cluster1[i,]))
    }
  }
  
  #minimization problem
  distance2 <- NULL
  #distance <- c()
  eval_f <- function(x){ 
    cluster1 <- matrix(nrow=nrow(myreturns),ncol=ncol(A))
    distance1 <- c()
    for (j in 1:ncol(A)){
      for (i in 1:nrow(myreturns)){
        cluster1[i,j] <- matrix(sum((myreturns[i,] - matrix(x,nrow = nrow(A),ncol=ncol(A[,-1]))[j,])^2))
        distance1[i] <- c((min(cluster1[i,])))
      }
    }
    distance2 <- sum(distance1)
    return(distance2)
  }
  
  local_opts <- list( "algorithm" = "NLOPT_LN_NELDERMEAD", 
                      "xtol_rel" = 1.0e-14,     "maxeval" = 10000)
  
  opts <- list( "algorithm" = "NLOPT_LN_NELDERMEAD", 
                "xtol_rel" = 1.0e-14,
                "maxeval" = 10000,
                "local_opts" = local_opts)
  
  res <- nloptr( x0=A[,-1],
                 eval_f=eval_f,
                 opts=opts)
  
  optimal_returns <- matrix(res$solution,nrow=nrow(A),ncol=length(initial_prices))
  
  A_new <- matrix(nrow=nrow(A), ncol=length(initial_prices))
  for (j in 1:ncol(A[,-1])){
    for (i in 1:nrow(A))
      A_new[i,j] <- matrix(price0[-1][j]*(1+optimal_returns[i,j]))
  }
  
  A_new <- as.matrix(cbind(price0[1]*(1+interest_rate),A_new))
  
  cluster_0_1 <- matrix(nrow=nrow(cluster1),ncol=ncol(cluster1))
  for (j in 1:ncol(cluster1)){
    for (i in 1:nrow(cluster1)){
      if (cluster1[i,j] == distance[i]){
        cluster_0_1[i,j] <- 1
      }else{
        cluster_0_1[i,j] <- 0
      }
    }
  }
  
  prob <- c()
  for (j in 1:ncol(cluster_0_1)){
    prob[j] <- c(sum((cluster_0_1[,j]))/nrow(cluster_0_1))
  }
  names(prob)[1:length(prob)] <- sprintf("prob%d",1:length(prob))
  q <- c((1+interest_rate)*solve(t(A_new))%*%t(t(price0)))
  names(q)[1:length(q)] <- sprintf("q%d",1:length(q))
  V_1 <- c()
  
  for (i in 1:length(q)){
    V_1 <- c(rep(500,i))
  }
  
  objective <- NULL
  eval_f <- function(v_opt){ 
    for (i in 1:length(q)){
      objective = sum(cbind(objective,-exp(-0.5*v_opt[i])*prob[i]))
    }
    return( "argument" = objective)
  }
  
  eval_g_grad <- function(v_opt){
    gradient <- c()
    for (i in 1:length(q)){
      gradient[i] <- c((0.5*exp(-0.5*v_opt[i])*prob[i]))
    }
    return("gradient" = gradient)}
  
  
  #constraint functions
  martingale_c <-  NULL
  eval_g_eq <- function( v_opt ){
    for (i in 1:length(q)){
      martingale_c <- sum(cbind(martingale_c,price0[1]*sum(v_opt[i]*q[i])))
    }
    martingale_c1 <- martingale_c-w0
    return("constraint" =  martingale_c1)
  }
  
  eval_jac_g <- function(v_opt){
    jacobiana <- c()
    for ( i in 1:length(q)){
      jacobiana[i] <- c(q[i]*price0[1])
    }
    return("gradient"= jacobiana)}
  
  local_opts <- list( "algorithm" = "NLOPT_LD_SLSQP",  
                      "xtol_rel" = 1.0e-07,     "maxeval" = 100000)
  
  opts <- list( "algorithm" = "NLOPT_LD_SLSQP",
                "xtol_rel" = 1.0e-07,
                "maxeval" = 100000,
                "local_opts" = local_opts)
  
  res1 <- nloptr( x0=V_1,
                  eval_f=eval_f,
                  eval_grad_f = eval_g_grad,
                  eval_g_eq = eval_g_eq,
                  eval_jac_g_eq = eval_jac_g,
                  opts=opts)
  
  
  final_values <- c(res1$solution)
  names(final_values)[1:length(final_values)] <- sprintf(c("RISK_FREE_ASSET",symbols),1:length(final_values))
  check <- NULL
  for (i in 1:length(q)){
    check <- sum(cbind(check,price0[1]*sum(final_values[i]*q[i])))
  }
  
  #replication strategy
  A_inverse= solve(A_new)
  
  alpha= c(A_inverse%*%res1$solution)
  names(alpha)[1:length(alpha)] <- sprintf(c("RISK_FREE_ASSET",symbols),1:length(alpha))
  optimal_weights= c(alpha*price0/w0)
  names(optimal_weights)[1:length(optimal_weights)] <- sprintf(c("RISK_FREE_ASSET",symbols),1:length(optimal_weights))
  check_weights <- sum(optimal_weights)
  checks <- c("check_real_prob" = sum(prob), "check_martingale_measures" = sum(q),"check_martingale_condition" = check, "check_weights" = check_weights)
  results1 <- list("real_probabilities" = prob,"martingale_measures" = q, "final_values"= final_values,"optimal_units"= alpha,
                   "optimal_weights"= optimal_weights, "checks_optimal_values" = checks)
  print(results1)
  
  results2 <- list("myreturns"=myreturns,"clusters"=cluster1,"distance"=t(t(distance)),"optimal_returns" = optimal_returns,"real_probabilities" = prob,"martingale_measures" = q, "final_values"= final_values,"optimal_units"= alpha,
                   "optimal_weights"= optimal_weights, checks_optimal_values = checks)
  
  return(results2)
}

















