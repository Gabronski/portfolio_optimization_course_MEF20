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
library(readxl)
#exercise with professor's excel data
dataset_excel <- read_xlsx("~/R/dati_prof_assets.xlsx")
dataset_excel
dataset_excel <- data.frame(dataset_excel[2:5])
dataset_excel
symbols <- c("Eni","Fiat","Unicredit","TIM")

weights <- c(0.25,0.25,0.25,0.25) 
Myminimization_problem1 <- assetportfolio_optm(dataset_excel,expected_return_target = 0.0002,weights = weights)


install.packages("ggplot2")
library(ggplot2)

undebug(mean_var_front)
#the function mean_var_front helps you in building the mean-variance frontier
mean_var_frontier <- mean_var_front(Myminimization_problem1, expected_return_target = 0.0002)
#you can also set the interval where you want your function plotted
mean_var_frontier<- mean_var_front(Myminimization_problem1,expected_return_target = 0.0002,value_min = -0.001,value_max=0.0005,interval=0.00001) 
#we use this function in case we want to constraint also the minimum value of our weights
Myminimization_problem2 <- assetportfolio_optm(dataset_excel,expected_return_target = 0.0002,weights = weights,diag=4,value_desired = 0.1)


# if you want to use data from yahoo finance and pick whatever asset you want
# I picked: American Airlines, Apple, Microsoft, Vanguard ETF Emerging Markets, FCA (US), Unicredit (FTSEMIB), TIM (FTSEMIB)
symbols <- c("AAL","AAPL","MSFT","VWO","E","FCAU","UCG.MI","TIT.MI")
# I use monthly data because, using daily data and choosing US and EU assets, we end with different days and therefore
# different rows.
dataset <- getSymbols.yahoo(symbols, from = "2016-01-01", to="2019-06-06",periodicity = "monthly",env = (.GlobalEnv))
# I decided to select adjusted prices but you can chose also close price
#ha senso close price, adjusted = aggiustato con decuratazione dividend yield
dataset<- data.frame(AAL$AAL.Adjusted,AAPL$AAPL.Adjusted,MSFT$MSFT.Adjusted,VWO$VWO.Adjusted,E$E.Adjusted,
                     FCAU$FCAU.Adjusted,UCG.MI$UCG.MI.Adjusted,TIT.MI$TIT.MI.Adjusted)

weights <- c(0.1,0.2,0.1,0.1,0.2,0.1,0.1,0.1) 
#we use this in case we want to constraint also the minimum value of our weights
Myminimizationproblem3 <- assetportfolio_optm(dataset,expected_return_target = 0.0002,weights = weights,diag=8,value_desired = 0.05)

Myminimizationproblem4 <- assetportfolio_optm(dataset,expected_return_target = 0.002,weights = weights)








#analytical procedure to find the optimal portfolio

mu = c(port_optmized$expected_return_each_asset)
mu_t = t(t(mu))
var = port_optmized$variance_covariance_matrix
v_1 = solve(port_optmized$variance_covariance_matrix)
ones=rep(1,length(mu))
ones_t = t(t(ones))
#now find A,B,C,D
A = as.vector(ones%*%v_1%*%mu_t)
B = as.vector(mu%*%v_1%*%mu_t)
C = as.vector(ones%*%v_1%*%ones_t)
D = as.vector((B*C) - A^2)
# I need now h and g to be found in order to construct my frontier equation w = return_portfolio*h + g
h = C/D*v_1%*%mu_t - A/D*v_1%*%ones #it should be equal to 0 or near 0
g=B/D*v_1%*%ones - A/D*v_1%*%mu_t #it should be equal to 1
#minimum-variance portfolio
mu_p <- A/C 
sigma <- 1/C 
minimum_portfolio <- c(sigma=sigma,min_mu=mu_p)
#optimal weights when expected return of the portfolio is 0.02
exp_ret = 0.02
sigma_2 <- C/D*(exp_ret - A/C)^2 + 1/C
sigma <- sqrt(sigma_2)
optimal_weights <- 0.0002*h + g
optimal_weights #weights should be equal to the weights found in the minimization problem
port_optmized <- Myminimization_problem1        #myminimizationproblem
port_optmized$optimal_weights
