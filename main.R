setwd("/Users/gabrielepiergallini/R")


### PART 1: BUILD MAXIMIZATION PROBLEM STARTING BY "PRICING" EACH BOND:

#we find the present value(price), duration and convexity of each bond.
# With these values we will try to construct a maximization problem as the one we saw during the classes



source('bond_functions.R') #using "source" we can call the file "bond_functions.R" without opening it
#pay attention! Make sure that this file is saved in the same directory of "bond_functions.R", otherwise it will
#not work

#BOND1
coupon=2.5
face_value=100
maturities = c(seq(from=0.5, to=20, by=0.5)) #semesters maturity: 10y
#if you want to have semester payments, as the BTP (italian bond) just change the values inside "maturities"
# e.g maturities = c(seq(from=0.5, to=20, by=0.5))
# from=0.5 <-- it means from the first trimester, to=20 <-- to the 20th year, by=0.5 <-- it means by
#semester to semester.
interest_rate= 0.02

cash_flow = c(rep(coupon,length(maturities)-1),face_value+coupon)
MyBond <- Bprice(cash_flow,maturities,interest_rate)[[1]] #we find the price of a bond
MyDuration <- Bduration(cash_flow,maturities,interest_rate) #we find the duration of a bond
MyConvexity <- Bconvexity(cash_flow,maturities,interest_rate)# we find the convexity of a bond
MyBondPricing <- Bpricing(face_value,maturities, coupon, interest_rate) #we find all these measures in one function


#BOND2 #we do the same for the BOND2 - BOND3 - BOND4
coupon=2.7
face_value=100
maturities = c(seq(from=0.5, to=8, by=0.5))
maturities
interest_rate= 0.02
cash_flow = c(rep(coupon,length(maturities)-1),face_value+coupon)
MyBond2Pricing <- Bpricing(face_value,maturities,coupon,interest_rate)


#BOND3
coupon =3.6
face_value =100
maturities = c(seq(from=1, to=50,by=1))
cash_flow=c(rep(coupon,length(maturities)-1),face_value+coupon)
interest_rate=0.02
MyBond3Pricing<- Bpricing(face_value,maturities,coupon,interest_rate)
MyBond3Pricing

#BOND4
coupon = 2.5
face_value = 100
maturities = c(seq(from=0.5, to=20,by=0.5))
interest_rate=0.02

MyBond4Pricing <- Bpricing(face_value,maturities,coupon,interest_rate)

###PART 2: maximization problem without dataset, we just use the pricing we have found previously
#the maximization_problem function can be used in this way:
myfunction <- maximization_problem(MyBondPricing,MyBond2Pricing,MyBond3Pricing,Duration_desired=14.27,initial_wealth=1000)
#in this case you cannot change the number of bonds so we need to create a different function
install.packages("lpSolve")
library(lpSolve)
x=list(MyBondPricing,MyBond2Pricing,MyBond3Pricing,MyBond4Pricing)

optimal_values<- maximization_problem2(x,Duration_desired=14.27,initial_wealth=1000) #the maximization_problem2 works also if we plug
#manually the bonds pricing into the function




###PART 3:maximization problem with dataset: we have a dataset with all the main info regarding a bond
#case when we do not have durations and convexities of the bonds
install.packages("readxl") #we require this package in order to read the excel file
library(readxl)


sheets = c("BTP","SPAIN","PORT") #the number of sheets containing the informations on the bonds we want
#in our portfolio, and the name of these sheets in our excel file.
# Pay attention: name inside variable "sheets" = name inside excel file
Mydataset <- c()
for (i in 1:length(sheets)){
  Mydataset[[i]] <- read_excel("/Users/gabrielepiergallini/R/BONDSPV.xlsx", col_names = TRUE, sheet = sheets[[i]])
}
interest_rate = 0.2 #the interest rate we find, e.g EURIBOR + spread CDS BTP Italy
View(Mydataset)
Myfunction <- Bpricing2(Mydataset,interest_rate) #function which price all the bonds we have in our dataset


#function which maximize our portfolio
Mymaximizationproblem_dataset <-  maximization_with_dataset(Myfunction,Duration_desired_daily = 11*365,
                                                            Duration_desired_yearly = 11,initial_wealth_day = 100,initial_wealth_year = 1000)

### PART4: Maximization problem where present value, duration and convexity of our bonds are given by an excel file

Mydataset_given <- read_excel("/Users/gabrielepiergallini/R/BONDS_PROVA1.xlsx", col_names = TRUE)
Mymaxprobgiven<- maximization_problem_given(Mydataset_given,Duration_desired = 10,initial_wealth = 1000)

#in case we have more sheets containing same info as the previous exercise but for multiple days
sheets = c("week1","week2") #the number of sheets containing the informations on the bonds we want
#in our portfolio, and the name of these sheets in our excel file.
# Pay attention: name inside variable "sheets" = name inside excel file
Mydataset <- c()
for (i in 1:length(sheets)){
  Mydataset[[i]] <- read_excel("/Users/gabrielepiergallini/R/BONDS_PROVA1.xlsx", col_names = TRUE, sheet = sheets[[i]])
}
interest_rate = 0.2 #

Mymaxprobgiven_dataset <- list()
optimal_units_periods <- list()
for (i in 1:length(sheets)){
  
  Mymaxprobgiven_dataset[[i]]<- list(maximization_problem_given(Mydataset[[i]],Duration_desired = 10,initial_wealth = 1000))
   optimal_units_periods[[i]] <- list("optimal_units"=Mymaxprobgiven[i]$optimal_units)
  }




#aissigment Portfolio Optimization 


###### first question, first exercise ###########
# date_of_evaluation = Date of evaluation: a scalar expressed as an object of class date.
# maturities = Payment Dates: a vector expressed as an object of class date.
# coupon = coupons: a vector expressed as an object of class numeric.
# maturities = Payment Dates: a vector expressed as an object of class date.
# face_value = Notional Amount: a scalar expressed as an object of class numeric.
# interest_rate = Interest rate on yearly basis: a scalar expressed as an object of class numeric.

install.packages("lpSolve")
library(lpSolve)


date_of_evaluation <- as.Date("2020-11-23")


#BOND1
maturities <- seq(as.Date("2020-11-30"), by="12 months", length.out=10)
coupon=2.5
face_value=100
#maturities = c(seq(from=0.5, to=20, by=0.5)) #semesters maturity: 10y
interest_rate= 0.02
cash_flow = c(rep(coupon,length(payments_dates)-1),face_value+coupon)
Bond1pricing <- B_pricing(face_value,date_of_evaluation,maturities,coupon,interest_rate,ttm="year")


#check with an other bond
#BOND2 #we do the same for the BOND2 - BOND3 - BOND4
payments_dates <- seq( as.Date("2020-12-03"), by="6 months", length.out=30)
#here we have to change the annual interest rate then:
interest_rate <- sqrt(1+interest_rate) - 1
coupon=2.7
face_value=100
#maturities = c(seq(from=0.5, to=8, by=0.5))
interest_rate= 0.04
#cash_flow2 = c(rep(coupon,length(payments_dates)-1),face_value+coupon)
Bond2pricing <- B_pricing(face_value,date_of_evaluation,payments_dates,coupon,interest_rate,ttm="year")


###########################################
#second question first exercise

#coupons
#notional amounts
#payment dates
#date of evaluation
#Interest rates
#initial wealth
#target duration

#It returns a list containing:
#The optimal weights, expressed as percentage of the initial wealth, that maximize the portfolio convexity with a target duration.
#The optimal units.

date_of_evaluation <- as.numeric(as.Date("2020-11-23"))

#BOND1
coupon1=2.5
face_value=100
pay_dates_1 <- seq(as.Date("2020-11-30"),by="12 months",length.out =37)
cash_flow1 = c(rep(c(0,coupon1),length(pay_dates_1)-4),0,face_value+coupon1,rep(0,((length(pay_dates_1)-(length(pay_dates_1)-5)))))

#BOND2 #we do the same for the BOND2 - BOND3 - BOND4
coupon2=2.7
face_value=100
pay_dates_2 <- seq(as.Date("2020-11-30"),by="6 months",length.out = 73)
cash_flow2 = c(rep(coupon2,length(pay_dates_2)-1),face_value+coupon2)


#BOND3
coupon3 =3.6
face_value =100
#maturities = c(seq(from=1, to=50,by=1))
pay_dates_3 <- seq(as.Date("2020-11-30"),by="24 months",length.out = 19)
cash_flow3=c(rep(c(0,0,0,coupon3),length(pay_dates_3)-4),0,0,0,face_value+coupon3, rep(0,3*((length(pay_dates_3)-(length(pay_dates_3)-3)))))

my_payments_dates <- seq(as.Date("2020-11-30",origin = "2056-11-30"),by="6 months",length.out = 73)



cash_flows_matrix <- matrix(c(cash_flow1,cash_flow2,cash_flow3),nrow = length(my_payments_dates), ncol=3,
                            dimnames = list(c(my_payments_dates), c("cash_flow1","cash_flow2","cash_flow3")))

face_value <- c(bond1=100,bond2=100,bond3=99)

interest_rate = 0.05

w0 = 1000 
duration_t<- 40

#undebug(Bondport_optm)

myfunction <- Bondport_optm(cash_flows_matrix,my_payments_dates,date_of_evaluation,
                            face_value,w0,interest_rate,duration_t)
myfunction <- Bondport_optm(cash_flows_matrix,my_payments_dates,date_of_evaluation,face_value,w0,interest_rate,duration_t,type="semiannual",ttm="month")
