# portfolio_optimization_course_MEF20
This repository is an attempt to collect all the functions created in order to apply portfolio optimization problems, studied during the lessons of MEF course.
This is an initiative carried out by a student, it is unofficial. I am working also on a package for all these functions.

Before the  download of the file, make sure that "main.R" and "bond_functions.R" are stored in the same working directory. 
Before running the code use setwd("/yourpath") in case you want work in the same directory.

I do not know if there are mistakes, in case write me on whatsapp.

# main_attempt file:
It is divided in 4 different approaches to the maximization problem:

1) Function for the estimation of the present value and one for each sensitivity measures (duration and convexity).

2) Function in order to estimate all the previous parameters for each bond you want in your portfolio
  
   - maximization problem: maximize the convexity of each bond by its weight on the portfolio, given a target duration.

3) Function in case of real data coming from excel file,in this case we still do not have the duration and the convexity of our bonds, the maximization problem has the same scope as the previous

4) Function in case of real data coming from excel where we have the end price, convexity and duration on a specific day, the maximization problem has the same scope as the previous.

# bond_functions file:
It contains all the functions we want to run. 

# this repository is a work in progress, the files may be changed in order to modify errors or add functions. 
