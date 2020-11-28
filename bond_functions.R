#bonds' prices (present values)
Bprice <- function(cash_flow, maturities, interest_rate){
  
  # the function returns the price of a bond under the assumption of a constant interest rate for each maturity
  # inputs: 
  # cash flow: is a numeric object with dimension the number of cash flows
  # maturities: is a numeric object containing all maturities and it has the same dimension of cash flow
  # interest rate: is a numeric object that represents the term structure of the zero rates (no forward).
  # If interest rate is a scalar then automatically we have a constant term structure.
  # interest rate 
  
  Discounted_Cash_Flow = cash_flow/(1+interest_rate)^maturities
  Price <- sum(Discounted_Cash_Flow)
  results <- list(Price=Price,Discounted_Cash_Flow=Discounted_Cash_Flow)
  print(results)
  return(results)
}

#bonds' durations
Bduration <- function(cash_flow, maturities, interest_rate){
  
  # infoPrice: we recall the function Bprice because we need the present value of each cash flow and the total price of the bond in order
  # to determine the weights we need
  # weights: is a numeric objectrequired to compute the duration representing the fraction of the total price given by each cash flow
  # Duration: total sum of the product between the maturities and the weights, it must be between the smallest and the highest maturity
  # in order to guarantee the internality condition 
  
  infoPrice <- Bprice(cash_flow, maturities, interest_rate)
  weights <- infoPrice[[2]]/infoPrice[[1]]
  tw <- weights*maturities
  Duration <- sum(tw)
  results <- list(Duration = Duration,Weights = weights)
  print(results)
  return(results)
}

#bonds' convexities
Bconvexity <- function(cash_flow, maturities, interest_rate) {
  
  # infoPrice: we use it to recall the weights in order to compute the first part of the convexity formula given by the product 
  # between the maturities in the power of 2 and the weights
  # infoDuration: we require the Bduration function in order to use the duration value in order to find the convexity
  # check_internality: I use this formula in order to check if the internality condition is satisfied
  
  infoPrice <- Bprice(cash_flow, maturities, interest_rate)
  infoDuration <- Bduration(cash_flow, maturities, interest_rate)
  t2w <- sum((maturities)^2*infoDuration[[2]])
  Convexity <- t2w + infoDuration[[1]]
  check_internality <- maturities^2 + maturities
  results <- list(Convexity_total = Convexity, check_internality = check_internality)
  print(results)
  return(results)
}

# bonds' pricing

Bpricing <- function(face_value,maturities, coupon, interest_rate){
  
  #function created to embed all the sensitivity measures and data required in order to compute 
  # present value, duration and convexity of a bond
  #richiamare funzione elementare
  cash_flow=c(c(rep(coupon,length(maturities)-1),face_value+coupon))
  Discounted_Cash_Flow = cash_flow/(1+interest_rate)^maturities
  Price <- sum(Discounted_Cash_Flow)
  weights <- Discounted_Cash_Flow/Price
  tw <- weights*maturities
  Duration <- sum(tw)
  t2w <- sum((maturities)^2*weights)
  Convexity <- t2w + Duration
  
  
  matrix1 <- matrix(c(maturities,cash_flow,Discounted_Cash_Flow,weights),nrow=length(maturities),ncol=4, 
                 dimnames=list(c(1:length(maturities)),c("maturities","cash_flow","discounted_cash_flows","weights")))
  matrix <- t((matrix(c(interest_rate,Price,Duration, Convexity),nrow=1,ncol=4,
                  dimnames=list("total value",c("interest_rate","present_value","duration","convexity")))))
  
  results <- list(cash_flows=matrix1,total_pricing_bond=matrix)
  print(results)
  return(results)
  
}

maximization_problem <- function(MyBondPricing,MyBond2Pricing,MyBond3Pricing,Duration_desired,initial_wealth) {
  
  # Bond_Dur: for each Bond's pricing we pick the duration
  # Bond_Conv: for each Bond's pricing we pick the convexity
  # Durations: we build a vector containing all the bonds' durations
  # Convexities: we build a vector containing all the bonds' convexities
  
  Bond1_Price <- MyBondPricing$total_pricing_bond[2]
  Bond1_Conv <- MyBondPricing$total_pricing_bond[4]
  Bond1_Dur  <- MyBondPricing$total_pricing_bond[3]
  Bond2_Price <- MyBond2Pricing$total_pricing_bond[2]
  Bond2_Dur  <- MyBond2Pricing$total_pricing_bond[3]
  Bond2_Conv <- MyBond2Pricing$total_pricing_bond[4]
  Bond3_Price <- MyBond3Pricing$total_pricing_bond[2]
  Bond3_Dur  <- MyBond3Pricing$total_pricing_bond[3]
  Bond3_Conv <- MyBond3Pricing$total_pricing_bond[4]
  Present_values <- c(Bond1_Price,Bond2_Price,Bond3_Price)
  Durations <- c(Bond1_Dur,Bond2_Dur,Bond3_Dur)
  Convexities <- c(Bond1_Conv,Bond2_Conv,Bond3_Conv)
  #maximization problem
  C_t <- t(Convexities) #we require the convexity vector transposed
  f.obj <- C_t  #objective function <- C*w
  #f.con, f.dir and f.rhs are used to construct the linear system of constraints
  f.con <- matrix(c(1,1,1,Bond1_Dur*1,Bond2_Dur*1,Bond3_Dur*1),nrow=2, byrow=TRUE) #linear system byrow
  f.dir <- c("=","=")
  f.rhs <-c(1,Duration_desired)
  w <- lp (direction="max", f.obj, f.con, f.dir, f.rhs)$solution
  w #optimal weights determined by the maximization problem
  #how much units we should buy
   optimal_units <- w*Present_values/initial_wealth 
  
  results <- list(Present_values=Present_values,Convexities=Convexities, Durations=Durations,optimal_weights=w,optimal_units=optimal_units )
   print(results)
   return(results)
}
  
#maximization problem using Bpricing function n times

maximization_problem2 <- function(...,Duration_desired,initial_wealth) {
  # in this case we try to run the function without limiting the number of Bonds' pricing. In this way we can construct a 
  #portfolio with n bonds
  # x: we construct a vector from the lsit of Bonds we have
  # Convexities: using the "for" loop we are able to construct a vector having the convexity of each bond
  # Durations: using the "for" loop we are able to construct a vector having the durations of each bond
  # Bonds_prices: using the "for" loop we are able to construct a vector having the prices(present values) of each bond
  Duration_desired = Duration_desired
  Bonds_prices <<- c()
  Durations <<- c()
  Convexities <<-c()
  for (i in x){
    n = length(x)
    Durations<- c(Durations,i$total_pricing_bond[3])
    Convexities <- c(Convexities,i$total_pricing_bond[4])
    Bonds_prices <- c(Bonds_prices, i$total_pricing_bond[[2]])
    sensitivity_measures <<- list(Durations=Durations,Convexities=Convexities,Bonds_prices=Bonds_prices)
    sensitivity_measures
  }
  #maximization problem
  C_t= sensitivity_measures$Convexities #we require the convexity vector in order to construct the objective function C*w
  f.obj <- C_t #c_t * w  objective function
  # we now compute the linear system byrow
  f.con <- matrix(c(rep(1,n),sensitivity_measures$Durations[1:n]*1),nrow=2,ncol=n, byrow=TRUE) #linear system byrow
  f.dir <- c("=","=")
  f.rhs <-c(1,Duration_desired)
  w=lp (direction="max", f.obj, f.con, f.dir, f.rhs)$solution #this function gives us the optimal weights according to the
  # maximization problem
  #how much units we should buy
  optimal_units = w*initial_wealth/Bonds_prices 
  
  results <- list(Bonds_prices=Bonds_prices,Durations=Durations,Convexities=Convexities,optimal_weights=w,optimal_units=optimal_units)
  print(results)
  return(results)
}

#Bpricing with dataset


Bpricing2 <- function(dataset,interest_rate){
  
  dataset <- Mydataset
  discounted_cf_year <- c()
  discounted_cf_day <- c()
  cash_flows <- c()
  Price_yearly <- c()
  Price_daily <- c()
  tw_year <- c()
  tw_day <- c()
  Duration_daily <- c()
  Duration_yearly  <- c()
  t2w_daily <-  c()
  t2w_yearly  <- c()
  Convexity_daily <- c()
  Convexity_yearly <-  c()
  for (i in 1:length(sheets)){
    #still working on measurement of cash flows
    cash_flows[[i]] = c(dataset[[i]]$Interest + dataset[[i]]$Principal)
    discounted_cf_day[[i]]= c(cash_flows[[i]]/((1+interest_rate)^dataset[[i]]$TTM_DAY))
    discounted_cf_year[[i]] = c(cash_flows[[i]]/(1+interest_rate)^dataset[[i]]$TTM_YB)
    Price_daily[[i]] <- c(sum(discounted_cf_day[[i]]))
    Price_yearly[[i]] <- c(sum(discounted_cf_year[[i]]))
    tw_day[[i]] <- c(dataset[[i]]$weights*dataset[[i]]$TTM_DAY)
    tw_year[[i]] <- c(dataset[[i]]$weights*dataset[[i]]$TTM_YB)
    Duration_daily[[i]] <- c((sum(tw_day[[i]]))*(sum(dataset[[i]]$weights)))
    Duration_yearly[[i]] <- c((sum(tw_year[[i]]))*(sum(dataset[[i]]$weights)))
    t2w_daily[[i]] <- c(sum(((dataset[[i]]$TTM_DAY)^2)*dataset[[i]]$weights))
    t2w_yearly[[i]] <- c(sum(((dataset[[i]]$TTM_YB)^2)*dataset[[i]]$weights))
    Convexity_daily[[i]] <- c(t2w_daily[[i]] + Duration_daily[[i]])
    Convexity_yearly[[i]] <- c(t2w_yearly[[i]] + Duration_yearly[[i]])
  
  } 
  
  matrix1 <- c()
  for (i in 1: length(sheets)){
    
    matrix1[[i]] = matrix(c(cash_flows[[i]],dataset[[i]]$weights,tw_day[[i]],discounted_cf_day[[i]],
                            discounted_cf_year[[i]],tw_year[[i]]),
                          ncol=6, dimnames = list(c(1:(nrow(dataset[[i]]))),c("cash_flows","weights","tw_day","discounted_cf_day", 
                                                                              "discounted_cf_year", "tw_year")))
  }
  
  matrix2 = c()
  
  for (i in 1:length(sheets)){
    
    matrix2[[i]] = matrix(c(Price_daily[i],Price_yearly[i],sum(dataset[[i]]$weights),Duration_daily[i],Duration_yearly[i],
                            Convexity_daily[i],Convexity_yearly[i]),nrow=1,ncol=7,
                          dimnames = list("final result",c("Price_day","Price_year","check_weights","Duration_day","Duration_year",
                                                           "Convexity_day","Convexity_year")))
  } 
  
  total <- list(values_for_each_maturity=matrix1,total_values=matrix2)
  print(total)
  return(total) 
}

#maximization problem with datataset

maximization_with_dataset <- function(...,Duration_desired_daily,Duration_desired_yearly,initial_wealth_day,initial_wealth_year) {
  
  #dataset: variable which include all the inputs we give to the function and we name it 
  #price_day: vector which includes all data of our dataset for each sheet in position [1]
  #price_year and so on: same concept until Convexity_year
  
  dataset <- c(...)
  price_day <- c() 
  price_year <- c()
  Duration_day <- c()
  Duration_year <- c()
  Convexity_day <- c()
  Convexity_year <- c()
  for (i in 1:length(sheets)){
    price_day[[i]] <- c(dataset$total_values[[i]][1])
    price_year[[i]] <- c(dataset$total_values[[i]][2])
    Duration_day[[i]] <- c(dataset$total_values[[i]][4])
    Duration_year[[i]] <- c(dataset$total_values[[i]][[5]])
    Convexity_day[[i]] <- c(dataset$total_values[[i]][6])
    Convexity_year[[i]] <- c(dataset$total_values[[i]][7])
    price_day <- c(unlist(price_day))
    price_year <- c(unlist(price_year))
    Duration_day <- c(unlist(Duration_day))
    Duration_year <- c(unlist(Duration_year))
    Convexity_day <- c(unlist(Convexity_day))
    Convexity_year <- c(unlist(Convexity_year))
  }
  
  
  sensitivity_measures <<- list(Price_day=price_day,Duration_day=Duration_day, Convexity_day=Convexity_day,
                                Price_year=price_year,Duration_year=Duration_year,Convexity_year=Convexity_year)
  sensitivity_measures
  
  #maximization problem daily
  C_t <- sensitivity_measures$Convexity_day #we require the convexity vector in order to construct the objective function C*w
  f.obj <- C_t #c_t * w  objective function
  # we now compute the linear system byrow
  f.con <- matrix(c(rep(1,length(sheets)),sensitivity_measures$Duration_day[1:length(sheets)]),nrow=2,ncol=length(sheets), byrow=TRUE) #linear system byrow
  f.con
  f.dir <- c("=","=")
  f.rhs <-c(1,Duration_desired_daily)
  w <- lp (direction="max", f.obj, f.con, f.dir, f.rhs)$solution #this function gives us the optimal weights according to the
  # maximization problem considering daily data
  w
  optimal_units <- w*price_day/initial_wealth_day 
  daily_results <- list(Bonds_prices_daily=price_day,Durations_daily=Duration_day,Convexities_daily=Convexity_day,
                       optimal_weights=w, optimal_units=optimal_units)

  #maximization problem using yearly data
  C_t1 <- sensitivity_measures$Convexity_year #we require the convexity vector in order to construct the objective function C*w
  f.obj1 <- C_t1 #c_t * w  objective function
  # we now compute the linear system byrow
  f.con1 <- matrix(c(rep(1,length(sheets)),sensitivity_measures$Duration_year[1:length(sheets)]),nrow=2,ncol=length(sheets), byrow=TRUE) #linear system byrow
  f.con1
  f.dir1 <- c("=","=")
  f.rhs1 <-c(1,Duration_desired_yearly)
  w1 <- lp (direction="max", f.obj1, f.con1, f.dir1, f.rhs1)$solution #this function gives us the optimal weights according to the
  # maximization problem with yearly data
  optimal_units_y <- w*price_day/initial_wealth_year 
  yearly_results <- list(Bonds_price_yearly = price_year, Duration_yearly = Duration_year, Convexity_yearly = Convexity_year,
                        optimal_weights = w1, optimal_units_yearly=optimal_units_y)
  #vector of lists that represents the daily results and yearly results
  results <- c(daily_results, yearly_results)
  print(results)
  return(results)
  
}

#we now build a function in the case where we already have convexity price and duration of the bonds

maximization_problem_given <- function(..., Duration_desired,initial_wealth){
  
  #dataset: variable which include the data input used by the user
  dataset <- c(...)
  Convexities <- dataset$CONVEXITY
  Durations <- dataset$DURATION
  Price <- dataset$PRICE
  Bonds <- dataset$BOND
  n <-length(Bonds)
  
  #maximization problem
  C_t <- t(Convexities) #we require the convexity vector in order to construct the objective function C*w
  f.obj <- C_t #c_t * w  objective function
  # we now compute the linear system byrow
  f.con <- matrix(c(rep(1,n),Durations[1:n]*1), nrow=2,ncol=n, byrow=TRUE) #linear system byrow
  f.con
  f.dir <- c("=","=")
  f.rhs <-c(1,Duration_desired)
  w<-lp (direction="max", f.obj, f.con, f.dir, f.rhs)$solution #this function gives us the optimal weights according to the
  optimal_units <- w*Price/initial_wealth
  results <- list(Price=Price,Durations=Durations,Convexities=Convexities,optimal_weights=w,optimal_units=optimal_units)
  print(results)
  return(results)
}




# functions solved for the assignment
# 1st function : find duration, convexity and present value
# inputs :
# face_value : object of class scalar containing the face value of your bond
# date_of_evaluation : object of class date containing the day you evaluate the bond
# maturities : object of class date containing the payments date
# coupon : object of class vector containing the cash flows of your bonds
# interest_rate : object of class scalar containing the interest rate
# ttm: object of class character where you decide your type of time to maturity:
#     - default : time to maturity is computed daily
#     - "year" : if the time to maturity is computed by day
#     - "semiannual" : if the time to maturity is computed by semester
#     - "month" : if the time to maturity is computed by month"
B_pricing <- function(face_value,date_of_evaluation,maturities, coupon, interest_rate, ttm = NULL){
  
  face_value <- face_value
  coupon <- coupon
  interest_rate <- interest_rate
  maturities <- maturities #here you can put your time to maturity
  date_of_evaluation <- date_of_evaluation
  
  if( is.null(ttm) == FALSE && (ttm !="year") && (ttm!="semiannual") && (ttm != "month"))
    stop (" the variable 'ttm' should be:
             -  NULL : by default it is computed daily
             - 'year': if the time to maturity is computed by day
             - 'semiannual': if the time to maturity is computed by semester
             - 'month': if the time to maturity is computed by month")
  
  TTM <- c()
  for (i in 1:length(maturities)){
    
    TTM[i] <- c(rep(maturities[i]-date_of_evaluation))
    
  }
  
  
  if (is.null(ttm) == TRUE){
    TTM <- TTM
  }else{
    if ( ttm == "year"){
      TTM <- TTM/365
    }else if ( ttm == "semiannual"){
      TTM <- TTM/183
    }else if ( ttm == "month"){
      TTM <- TTM/30
    }
  }
  
  Discounted_Cash_Flow = cash_flow/(1+interest_rate)^TTM
  Price <- sum(Discounted_Cash_Flow)
  weights <- Discounted_Cash_Flow/Price
  tw <- weights*TTM
  Duration <- sum(tw)
  t2w <- sum((TTM)^2*weights)
  Convexity <- t2w + Duration

  matrix = t((matrix(c(Price,Duration, Convexity),nrow=1,ncol=3,
                     dimnames=list("total value",c("present_value","duration","convexity")))))
  print(matrix)
  return(matrix)
  
}

# 2nd function: maximization problem where you maximize the convexity, given a specific target duration
# inputs:
# coupon_m : object of class matrix containing the cash flows of your bonds
# payment_dates : object of class date containing the payments date
# date_of_evaluation: object of class date containing the day you evaluate the bonds
# face_value: object of class vector containing the face values of your bonds
# w0: object of class scalar containing your initial wealth
# interest_rate: object of class scalar containing the interest rate
# type: object of class character where you decide if the interest rate is:
#      -  NULL : if the interest rate is computed on a yearly basis
#      -"semiannual" : if the interest rate is computed on a semi-annual basis
#      -"quarterly" : if the interest rate is computed on a quarterly basis
#      -"monthly" : if the interest rate is computed on a monthly basis")
  
# ttm: object of class character where you decide your type of time to maturity:
#     - default : it is computed daily
#     - "year" : if the time to maturity is computed by day
#     - "semiannual" : if the time to maturity is computed by semester
#     - "month" : if the time to maturity is computed by month"
Bondport_optm <- function(coupon_m,payment_dates,date_of_evaluation,face_value,w0,interest_rate,
                          target_duration,type=NULL,ttm=NULL){
  
  prova2 <- coupon_m
  payment_dates <- payment_dates
  face_value <- face_value 
  date_of_evaluation <- date_of_evaluation
  w0 <- w0
  Duration_desired <- target_duration
  
  if (is.null(type) == TRUE){ #by default yearly interest rate
    interest_rate <- interest_rate
  }else{
    if (type == "semiannual"){
      interest_rate <- sqrt(1+interest_rate)-1
    }else if (type == "quarterly"){
      interest_rate <- (1+interest_rate)^(1/4)-1
    }else if (type == "monthly"){
      interest_rate <- (1+interest_rate)^(1/12)-1
    }
  }
  
  #in case the data inputs do not satisfy the entry requirements of the function:
  if ( is.null(type) ==FALSE && (type !="semiannual") && (type !="quarterly") && (type != "monthly"))
    stop ("the variable 'type' should be:
             
             -  NULL : if the interest rate is computed on a yearly basis
             - 'semiannual' : if the interest rate is computed on a semi-annual basis
             - 'quarterly' : if the interest rate is computed on a quarterly basis
             - 'monthly' : if the interest rate is computed on a monthly basis")
  
  if (is.matrix(coupon_m) == FALSE) stop  ("coupon_m must be an object of class matrix containing the cash flows of the bonds")
  if (length(face_value) != ncol(coupon_m)) stop ("length face_value is different from the number of bonds of coupon_m")
  if (length(payment_dates) != nrow(coupon_m)) stop ("the length of date_of_evaluation is different from the cash flows of your bonds")
  
  if( is.null(ttm) == FALSE && (ttm !="year") && (ttm!="semiannual") && (ttm != "month"))
    stop (" the variable 'ttm' should be:
             -  NULL : by default it is computed daily
             - 'year': if the time to maturity is computed by day
             - 'semiannual': if the time to maturity is computed by semester
             - 'month': if the time to maturity is computed by month")
  
  TTM <- c()
  
  for (i in 1:length(my_payments_dates)){
    
    TTM[i] <- c(rep(my_payments_dates[i]-date_of_evaluation))
    
  }
  
  
  if (is.null(ttm) == TRUE){
    TTM <- TTM
  }else{
    if ( ttm == "year"){
      TTM <- TTM/365
    }else if ( ttm == "semiannual"){
      TTM <- TTM/183
    }else if ( ttm == "month"){
      TTM <- TTM/30
    }
  }
  
  
  Discounted_Cash_Flow <- matrix(nrow = nrow(prova2),ncol=ncol(prova2[,-4]))
  #dimnames = list(payments_dates,c(1:ncol(coupon_m_1))))
  Price <- t(matrix(ncol=ncol(prova2[,-4])))
  weights <- matrix(nrow=nrow(prova2),ncol=ncol(prova2[,-4]))
  tw <- matrix(nrow=nrow(weights),ncol=ncol(weights))
  t2w <- matrix()
  check <- c()
  Duration <- c()
  t2w <- matrix(nrow=nrow(weights),ncol=ncol(weights))
  Convexity <- c()
  for (j in 1:ncol(prova2[,-4])){
    for (i in 1:nrow(prova2)){
      
      #cash_flow[i,j]=c(c(rep(coupon[i,j],length(maturities)-1),face_value+coupon))
      Discounted_Cash_Flow[i,j] <- matrix((prova2[,-4][i,j]/(1+interest_rate)^TTM[i]))
      Price[j] <- c(sum(Discounted_Cash_Flow[,j]))
      
    }
    
    for (i in 1:nrow(Discounted_Cash_Flow)){
      
      weights[i,j] <- matrix((Discounted_Cash_Flow[i,j]/t(Price)[,j]))
      check[j] <- c(sum(weights[,j]))
    }
    
    for (i in 1:nrow(weights)){
      
      tw[i,j] <- matrix((weights[i,j]%*%TTM[i]))
      Duration[j] <- sum(tw[,j])
      t2w[i,j] <- matrix((sum((TTM[i])^2%*%weights[i,j])))
      Convexity[j] <- c(sum(t2w[,j]) + Duration[j])
      
    }
  }
  
  #maximization problem, primal
  sensitivity_measures <<- list(Durations=Duration,Convexities=Convexity,Bonds_prices=Price)
  sensitivity_measures
  n = ncol(prova2)
  #maximization problem
  C_t= sensitivity_measures$Convexities #we require the convexity vector in order to construct the objective function C*w
  f.obj <- C_t #c_t * w  objective function
  # we now compute the linear system byrow
  f.con <- matrix(c(rep(1,n),sensitivity_measures$Durations[1:n]*1),nrow=2,ncol=n, byrow=TRUE) #linear system byrow
  f.dir <- c("=","<=")
  f.rhs <-c(1,Duration_desired)
  w <- lp(direction="max", f.obj, f.con, f.dir, f.rhs)$solution #this function gives us the optimal weights according to the
  #maximization problem
  #how much units we should buy
  optimal_units <- w*w0/Price
  
  #minimization problem, dual
  #f.rhs_t %*% y
  
  f.obj2 <- t(f.rhs)
  f.con2 <- t(f.con)
  f.dir2 <- c(">=",">=",">=")
  f.rhs2 <- t(t(Convexity))
  w2 <- lp(direction="min", f.obj2, f.con2, f.dir2, f.rhs2)$solution 
  #optimal_units2 <- w2*w0/Price
  
  #primal_problem <- list(optimal_weights = w, optimal_units= optimal_units)
  #dual_problem <- list(optimal_weights = w2)
  #return <- list("primal_problem"=primal_problem,"dual_problem"=dual_problem)
  
  if (Duration_desired < min(sensitivity_measures$Durations) | Duration_desired > max(sensitivity_measures$Durations)){
    
    primal_problem <- list(Durations = sensitivity_measures$Durations)
    
    dual_problem <- list(optimal_weights = w2) #??
    return <- list("primal_problem"=primal_problem,"dual_problem"=dual_problem)
    warning ("Duration must satisfy the internality condition")
    if ( round(sum(w),digits=10) != 1) warning ("the sum of the weights is not one")
    print(return)
    return(return)
  }else{
    primal_problem <- list(optimal_weights = w, optimal_units= optimal_units)
    dual_problem <- list(optimal_weights = w2)
    return1 <- list("primal_problem"=primal_problem,"dual_problem"=dual_problem)
    print(return1)
    return(return1)
  }
  
  
}
