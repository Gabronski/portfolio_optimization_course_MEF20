setwd("/Users/gabrielepiergallini/R")
source("binomial_model_functions.R")

# this file list 2 exercises in order to find:
# 1) a uniperiodal binomial model and applying a maximization of the expected utily on it
# 2) a finite state market model without arbitrage opportunities using cluster analysis and then applying a maximization of the expected utility on it 


#################
# EXERCISE 1:
##################

symbols <- c("FCA.MI")
dataset <- getSymbols.yahoo(symbols, from = "2019-08-30", to="2020-08-30",periodicity = "weekly",env = (.GlobalEnv))
dataset<- data.frame(FCA.MI$FCA.MI.Close)
u = 4 #upward movement starting point
d = 0.8 #downward movement starting point
pu = 0.6 #upward movement probability starting point
int = 0.01 #interest rate
s0 = 10.47 #initial price of your asset
mywealth = 1000 #your initial wealth

# binomial_model: this function estimates a uniperiodal binomial model with non-arbitrage constaints and maximizes the expected utility of wealth 
# at the final time horizon based on the Martingale Approach
prova_binomial <- binomial_model(dataset,u,d,pu,int,mywealth, s0,type = "weekly")

################
# EXERCISE 2:
################
# complete finite state market model without arbitrage opportunities using cluster analysis and
# maximization of the expected utility problem with that
# first and second attempt: replication of the results of professor's data
# using quantmod some prices are downloaded differently so I created a file in excel with professor's data
symbols <- c("FCA.MI","ENI.MI","UCG.MI")
dataset <- getSymbols.yahoo(symbols, from = "2015-12-01", to="2020-10-01",periodicity = "monthly",env = (.GlobalEnv))
dataset<- data.frame(FCA.MI$FCA.MI.Close,ENI.MI$ENI.MI.Close, UCG.MI$UCG.MI.Close) 
starting_returns<- cbind(c(0.05,0.01,0,0.00001),c(0,0.05,0.04,0.001),c(0.001,0.007,0.0006,0.0001)) 
price_t0  <- c(fiat = 12.22, eni = 7.678, unicr = 8.14)
interest_rate = 0.01
wealth = 10000
# in the cluster function we use type in order to adjust the interest rate to the frequence of prices you want
# e.g. we use type = "monthly" thus the interest rate is determined by 1/(1+interest_rate)^1/52,
# indeed the data downloaded with quantmod are monthly data

# cluster_function: function that applies a cluster analysis in order to find the real probabilities and martingale measures in each state, then it
# it finds the optimal expected return using the martingale approach
f_claster1 <- cluster_function(dataset, interest_rate,starting_returns, price_t0 , wealth, type = "monthly",symbols)
# attempt with professor's data
library(readxl) #dataset excel professor
symbols <- c("FCA.MI","ENI.MI","UCG.MI")
dataset <- read_excel("prof_cluster.xlsx",col_names = TRUE)
interest_rate = 0.01
price_t0 <- c(fiat = 12.22, eni = 7.678, unicr = 8.14)
wealth = 10000
f_claster2 <- cluster_function(dataset, interest_rate,starting_returns, price_t0, wealth, type = "monthly",symbols)

# problem using n assets, the more assets you use the more you have to wait for the results
symbols1 <- c("VOO","AAPL","TWTR","E","AAL","TSLA")
dataset1 <- getSymbols.yahoo(symbols1, from = "2015-12-01", to="2020-10-01",periodicity = "monthly",env = (.GlobalEnv))
dataset1<- data.frame(VOO$VOO.Close,AAPL$AAPL.Close,TWTR$TWTR.Close,E$E.Close,AAL$AAL.Close,TSLA$TSLA.Close)
interest_rate = 0.01
price_t01 <- c(VOO = 334.14 , AAPL = 116.49, TWTR = 46.59,E= 17,AAL = 14.98, TSLA = 585.76)
wealth1 = 10000
# pay attention on calibrating the starting point, if the function return negatives values it means you have to change the starting points
# by increasing a bit the numbers
starting_returns1 <- cbind(c(0.05,0.01,0,0.0001,0.0001,0.01,0.01),c(0.01,0.01,0,0.05,0.04,0.003,0.002),
                           c(0.01,0.01,0.05,0.001,0.00001,0.01,0.001),c(0.01,0.01,0.01,00001,0.01,0.0001,0.01),
                           c(0.01,0.01,0.01,00001,0.01,0.0001,0.01),c(0.01,0.01,0.01,00001,0.01,0.0001,0.01)) 
f_claster3 <- cluster_function(dataset1, interest_rate,starting_returns1, price_t01, wealth1, type = "monthly",symbols1)
