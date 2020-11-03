# portfolio_optimization_course_MEF20
This repository is an attempt to create and collect functions in order to apply portfolio optimization problems studied during the lessons of Portfolio Optimization in MEF Mater's degree offered by University of Milan.

This is an initiative carried out by a student, it is unofficial. I am working also on a package for all these functions.

Before the  download of the file, make sure files are stored in the same working directory. 
Before running the code use setwd("/yourpath") in case you want work in the same directory.

# main_attempt file:
It is divided in 4 different approaches to the maximization of a bond portfolio problem, given a target duration:

1) Function for the estimation of the present value and one for each sensitivity measures (duration and convexity);

2) Function in order to estimate all the previous parameters for each bond you want in your portfolio,
  
   - maximization problem: maximize the convexity of each bond by its weight on the portfolio, given a target duration;

3) Function in case of real data coming from excel file,in this case we still do not have the duration and the convexity of our bonds, the maximization problem has the same scope as the previous;

4) Function in case of real data coming from excel where we have the end price, convexity and duration on a specific day, the maximization problem has the same scope as the previous.

# bond_functions file:
It contains all the functions we want to run. 

# main_asset.R:
script were I try to run the functions created in the "asset_functions.R" file. Use "dati_prof_assets.xlsx" in the first example in order to check if the results are the same of those used by the professor

# asset_functions.R (work in progress):
file containing 2 functions:
  1-) assetportfolio_optm which run a quadratic programming problem and minimizes the variance of our portfolio, given a target expected return;
  2-) asssetportfolio_optm_2 solve a linear programming problem and maximize the expected return of our portfolio, given a target variance.
  
 In both cases you can select an other constraint on weights of your portfolio picking a minimum value for the weights.
# this repository is a work in progress, the files may be changed in order to modify errors or add functions.
Right now I am working on Asset Portfolio Allocation in a static context (e.g. Markowitz portfolio theory and the efficient frontier)

I do not know if there are mistakes, in case write me on whatsapp.


  
