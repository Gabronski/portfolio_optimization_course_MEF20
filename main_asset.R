setwd("/Users/gabrielepiergallini/R") #your path
source('asset_functions.R')

# This file aims to construct a portfolio minimization and maximization problem using only assets. 
# The first dataset has the same data of the Prof. Mercuri's file :"MVFrontierAndFiniteStateMarketModel.xlsx" first sheet. You can check if the
# results are the same with the ones on excel
# We then:
# - minimize the variance of our portfolio (constrained by the sum of the weights = 1 and a target expected return),
#   with the assetportfolio_optm function

# - maximize the expected return of our portfolio (constrained by the sum of the weights = 1 and a target variance),
#   with the assetportfolio_optm_2

# The same problems are applied using data downloaded from yahoo finance. You can try to construct your optimization problem
# using the assets listed in yahoo finance: https://finance.yahoo.com

# In the functions I made, you can add an other constraint: 
# adding "diag" and value_desired" for the assetportfolio_optm function and "diag" and "value" in the assetportfolio_optm_2 
# function, you are constraining the weights of the portfolio to be greater than a certain parameter. 

# e.g. with the first dataset and the function assetportfolio_optm, using the term diag = number of assets (in this case 4)
# and value_desired = 0.1, you are telling the function to return the maximization problem using a minimum value for your
# weights of 0.1 = 10% of your portfolio. 
# You can see this on the lines 38 and 43


install.packages("quantmod")
library(quantmod)
install.packages("lpSolve")
library(lpSolve)
library(readxl)
dataset_excel <- read_xlsx("~/R/dati_prof_assets.xlsx")
dataset_excel
dataset_excel <- data.frame(dataset_excel[2:5])
dataset_excel
symbols <- c("Eni","Fiat","Unicredit","TIM")

weights <- c(0.25,0.25,0.25,0.25) 


Myminimization_problem1 <- assetportfolio_optm(dataset_excel,expected_return_target = 0.0002,weights = weights)
Myminimization_problem1
#we use this function in case we want to constraint also the minimum value of our weights
Myminimization_problem2 <- assetportfolio_optm(dataset_excel,expected_return_target = 0.0002,weights = weights,diag=4,value_desired = 0.1)
Myminimization_problem2


#maximization problem: the objective function is weights*expected returns with a targeted variacne
Mymaximizationproblem1 <- assetportfolio_optm_2(dataset_excel,variance_desired = 0.0003)
Mymaximizationproblem1
#maximization problem with weights greater than 0.1
Mymaximizationproblem2 <- assetportfolio_optm_2(dataset_excel,variance_desired = 0.0003,value = 0.1,diag = 4)
Mymaximizationproblem2

# if you want to use data from yahoo finance and pick whatever asset you want
# I picked: American Airlines, Apple, Microsofr, Vanguard ETF Emerging Markets, FCA (US), Unicredit (FTSEMIB), TIM (FTSEMIB)
symbols <- c("AAL","AAPL","MSFT","VWO","E","FCAU","UCG.MI","TIT.MI")
# I use monthly data because, using daily data and choosing US and EU assets, we end with different days and therefore
# different rows.
dataset <- getSymbols.yahoo(symbols, from = "2016-01-01", to="2019-06-06",periodicity = "monthly",env = (.GlobalEnv))
# I decided to select adjusted prices but you can chose also close price
dataset<- data.frame(AAL$AAL.Adjusted,AAPL$AAPL.Adjusted,MSFT$MSFT.Adjusted,VWO$VWO.Adjusted,E$E.Adjusted,
                     FCAU$FCAU.Adjusted,UCG.MI$UCG.MI.Adjusted,TIT.MI$TIT.MI.Adjusted)
dataset
weights <- c(0.1,0.2,0.1,0.1,0.2,0.1,0.1,0.1) 
#we use this in case we want to constraint also the minimum value of our weights
Myminimizationproblem3 <- assetportfolio_optm(dataset,expected_return_target = 0.0002,weights = weights,diag=8,value_desired = 0.05)

Myminimizationproblem4 <- assetportfolio_optm(dataset,expected_return_target = 0.002,weights = weights)

Mymaximizationproblem3 <- assetportfolio_optm_2(dataset,variance_desired = 0.03)

Mymaximizationproblem4 <- assetportfolio_optm_2(dataset,variance_desired = 0.003,diag= 8 ,value = 0.001)#funzione creata

#example of for loop in case you have just a bond. This is were I started the construction of my functions

#myreturns <- list()
#i=row(dataset)
#for (i in 1:length(row(dataset))){
# myreturns[i] <- c(((dataset[sum(i+1)]-dataset[i])/dataset[i]))
# myreturns <- na.omit(myreturns)
#myreturns <- matrix(myreturns)}