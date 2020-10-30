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
maturities
#if you want to have semester payments, as the BTP (italian bond) just change the values inside "maturities"
# e.g maturities = c(seq(from=0.5, to=20, by=0.5))
# from=0.5 <-- it means from the first trimester, to=20 <-- to the 20th year, by=0.5 <-- it means by
#semester to semester.
maturities
interest_rate= 0.02
cash_flow = c(rep(coupon,length(maturities)-1),face_value+coupon)
MyBond <- Bprice(cash_flow,maturities,interest_rate)[[1]] #we find the price of a bond
MyBond
MyDuration <- Bduration(cash_flow,maturities,interest_rate) #we find the duration of a bond
MyDuration
MyConvexity <- Bconvexity(cash_flow,maturities,interest_rate)# we find the convexity of a bond
MyConvexity
undebug(Bpricing)
MyBondPricing <- Bpricing(face_value,maturities, coupon, interest_rate) #we find all these measures in one function
MyBondPricing

#BOND2 #we do the same for the BOND2 - BOND3 - BOND4
coupon=2.7
face_value=100
maturities = c(seq(from=0.5, to=8, by=0.5))
maturities
interest_rate= 0.02
cash_flow = c(rep(coupon,length(maturities)-1),face_value+coupon)
# debug(Bpricing)
# undebug(Bpricing)
MyBond2Pricing <- Bpricing(face_value,maturities,coupon,interest_rate)
MyBond2Pricing
Bond2Convexity <- Bconvexity(cash_flow,maturities,interest_rate)
Bond2Convexity

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
MyBond4Pricing

###PART 2: maximization problem without dataset, we just use the pricing we have found previously

install.packages("lpSolve")
library(lpSolve)
x=list(MyBondPricing,MyBond2Pricing,MyBond3Pricing,MyBond4Pricing)
optimal_values<- maximization_problem2(x,Duration_desired=14.27) #the maximization_problem2 works also if we plug
#manually the bonds pricing into the function
optimal_values #results


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
Myfunction
undebug(Bpricing2)

#function which maximize our portfolio
undebug(maximization_with_dataset)
Mymaximizationproblem_dataset <-  maximization_with_dataset(Myfunction,Duration_desired_daily = 5500,
                                                            Duration_desired_yearly = 11)
Mymaximizationproblem_dataset

### PART4: Maximization problem where present value, duration and convexity of our bonds are given by an excel file

Mydataset_given <- read_excel("/Users/gabrielepiergallini/R/BONDS_PROVA1.xlsx", col_names = TRUE)
Mydataset_given
dataset1 <- data.frame(Mydataset_given)
dataset <- list(dataset1,dataset2)
dataset







