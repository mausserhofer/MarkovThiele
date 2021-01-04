UNDER DEVELOPMENT 

# Introduction
This package deals with cashflow processes that are describable with a discrete time markov chain and cashflows that depend on the state of underlying the markov chain. We'll call such a process a *Markov-Thiele-chain (MTC)*.

A Markov-Thiele-chain is described by the following:
1. permissible states s_1, s_2, s_3, ..., s_n
2. transition probabilities <img src="https://render.githubusercontent.com/render/math?math=P(s_i, s_j, t)">
3. cashflows that are triggered if the markov chain is in a specific state at a given time: <img src="https://render.githubusercontent.com/render/math?math=payoffPre(s_i, t)">, and 
4. cashflows that are triggerd if the markov chain changes its state at a given time step <img src="https://render.githubusercontent.com/render/math?math=payoffPost(s_i, s_j, t)">
5. interest rate term structures, this is used for discounting in calculating the expected value of cashflows and etc., see below.

Points 1 and 2 form a regular markov chain, points 3 and 4 add the cashflow structure. Point 5 is regarded as part of the Markov-Thiele-chain for practical purposes. 

# Application of Markov-Thiele-Chains
MTCs can represent almost any life and health insurance contract. They can represent financial instruments such as vanilla options or structured notes. 

What people want to know about such objects are the following:
- what is the present value of the expected cashflows?
- what is the variance and higher moments of the present value of expected cashflows?
- what is value-at-risk for different levels of alpha?
- what is the distribution of the present value of cashflows?

# Goals of package

- define an R-class for inhomogeneous discrete markov chains with payoffs that depend on the state of the underlying markov chain
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
