UNDER DEVELOPMENT 

# Introduction
This package deals with cashflow processes that are describable with a discrete time markov chain and cashflows that depend on the state of underlying the markov chain. We'll call such a process a *Markov-Thiele-chain (MTC)*.

A Markov-Thiele-chain is described by the following:
1. permissible states  <img src="https://render.githubusercontent.com/render/math?math=s_1, s_2, s_3, ..., s_n">
2. transition probabilities <img src="https://render.githubusercontent.com/render/math?math=P(s_i, s_j, t)">
3. cashflows that are triggered if the markov chain is in a specific state at a given time: <img src="https://render.githubusercontent.com/render/math?math=\text{payoffPre}(s_i, t)">, and 
4. cashflows that are triggerd if the markov chain changes its state at a given time step <img src="https://render.githubusercontent.com/render/math?math=\text{payoffPost}(s_i, s_j, t)">
5. interest rate term structures, this is used for discounting in calculating the expected value of cashflows and etc., see below.
6. Terminal values of the MTC, typically a fixed cashflow at the terminal time of the MTC.

Points 1 and 2 form a regular markov chain, points 3 and 4 add the cashflow structure. Point 5 is regarded as part of the Markov-Thiele-chain for practical purposes. 

## Thiele difference equation
Core of the package is the Thiele-Difference Equation. It allows the stepwise evaluation of the present value of expected cashflows at time t, using the present value of expected cashflows at time t+1, transition probabilities from time t to t+1 and the cashflows occuring at time t+1.

Similar equations for stepwise evaluation exist for higher moments as well as for the distribution of cashflows.

# Application of Markov-Thiele-Chains

MTCs can represent almost any life and health insurance contract. They can represent financial instruments such as vanilla options or structured notes. 

What people want to know about such objects are the following:
- what is the present value of the expected cashflows?
- what is the variance and higher moments of the present value of expected cashflows?
- what is value-at-risk for different levels of alpha?
- what is the distribution of the present value of cashflows?

# Usage of package

- a markoThieleChain can be generated with invoking markovThieleChain with admissible parameters. The interest rate term structure can be defined with one number, being the annual interest. States that do not possess outgoing probabilities, i.e. a state *s_1* where for time *t* there exist no transition probabilites P(s_1, s_j, t) for any state *s_j*, are assumed to be terminal states, i.e. the MTC ends if such a state is reached and no further cashflows occur.
- the present value for all times and all states can be computed with the function 'completeV'
- the distribution of a MTC can be calculated with the function 'completeDist', this is using approximations. For getting more exact results, increase the parameter 'granularity'.
- the probability that the present value of future cashflows is smaller or equal to a value *u* can be computed with the function forwardDist. This function is not using any approximations, however the output is just the probability for this one state, time and *u*. This function can be used to check whether the granularity used in completeDist is sufficiently small.


# Further goals of this package
- functions to evaluate the higher moments of MTCs
- functionalities to evaluate a set of MTCs as in computing the present value of expected claims for a portfolio of insurance contracts
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
