library(tidyr)
library(dplyr)
library(openxlsx)
library(data.table)
library(ggplot2)

rm(list=ls())
setwd("C:/Users/marku/Google Drive/Aktuar_/Personenv/R")
source("markovFunctions.R")
source("prepareDataFunctions.R")
al <- 0
el <- 1
#state
state = c("alive", "dead")
#interest rates
i = 0.01
disc <- 1/(1+i)
# transition probabilities
trans <- createTransition(file="sterbetafel/statAustria.xlsx", sheet="2010_2012 zusammen" )
horizon <- max(trans$time) + 1
#terminal condition
# V <- createV()
#payoff
payoffPost <- createPayoffPost(al, horizon)
payoffPre  <- createPayoffPre(el, horizon)

# berechne verteilung
granularity <- 0.1
sim_min <- -granularity
sim_max <- 11
firstAge <- 90
source("markovFunctions.R")
source("forwardDist.R")

# exact <-
  forwardDist(u=10, state="alive", time=firstAge, trans, payoffPost, payoffPre, i)

Dist <-
  completeDist(granularity, sim_min, sim_max, trans, payoffPost, payoffPre, i, state)
Dist[time==firstAge & u==10]

# Dist <- results[[as.character(granularity)]]
ggplot(data = Dist[time==firstAge], aes(x=u, y=Prob)) +
  geom_line(aes(color=state))

max(Dist[time==firstAge&state=="alive", .(Prob)])

