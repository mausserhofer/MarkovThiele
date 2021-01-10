library(dplyr)
library(openxlsx)
library(data.table)
library(ggplot2)

rm(list=ls())
setwd("C:/dev/R/MarkovThiele")

source("R/prepareDataFunctions.R")


al <- 100
el <- -10
#state
state = c("alive", "dead")
#interest rates
i = 0.01
disc <- 1/(1+i)
# transition probabilities
trans <- createTransition(file="sterbetafel/statAustria.xlsx", sheet="2010_2012 zusammen" )
horizon <- max(trans$time) + 1
#terminal condition
W <- createV()
#cashflow
cashflowPost <- createPayoffPost(al, horizon)
cashflowPre  <- createPayoffPre(el, horizon)

# define class
mc <- markovThieleChain(trans=trans,
                        cashflowPre=cashflowPre,
                        cashflowPost=cashflowPost,
                        i=0,
                        W=W)
print(mc)

completeV(mc)
dist <- completeDist(mc, granularity = 0.1)

plotData <- dist[time %in% c(30) & state=="alive"]
ggplot(data=plotData, aes(x=u, y=Prob)) + geom_line(aes(color=time))
source("R/forwardDist.R")



# exact <-
  forwardDist(u=10, state="alive", time=firstAge, trans, cashflowPost, cashflowPre, i)

Dist <-
  completeDist(granularity, sim_min, sim_max, trans, cashflowPost, cashflowPre, i, state)

Dist[time==firstAge & u==10]

# Dist <- results[[as.character(granularity)]]
ggplot(data = Dist[time==firstAge], aes(x=u, y=Prob)) +
  geom_line(aes(color=state))

max(Dist[time==firstAge&state=="alive", .(Prob)])

