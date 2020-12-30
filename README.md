UNDER DEVELOPMENT 

# Goal of package
This package aims to 
- define an R-class for inhomogeneous discrete markov chains with payoffs depending on the state the markov chain is in
- functions to ease the construction of such objects
- functions to evaluate the expected value, higher moments and the distribution of the markov chain
- functionalities to evaluate the multiple such markov chains as in computing the present value of expected claims for a portfolio of insurance contracts
- supply sample data for generic markov chains in the insurance sector

# Installation of package
The package can be installed via executing the following code in R

    install.packages("devtools")
    devtools::install_github("manalysis/MarkovThiele")
    
R will then install the package devtools from cran, with which you can install the package from this site (github.com/manalysis/MarkovThiele)

# Contributions 
Feel free to provide improvements of the code or extend functionalities. The following is a list of open TO-Dos from my point of view:
- define an function to check the validity of a defined markov chain
- create a function that computes the higher moments
- find a set-up to include homogeneous markov chains
