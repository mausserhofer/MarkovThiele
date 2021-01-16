#' Example data for a livelong life risk insurance with payoff 100
#' in case of death
#' 
#' @format A data.table with columns time, from, to, amount:
#' \describe{
#' \item{time} {the time step the cashflow applies to}
#' \item{from} {the state at time step t}
#' \item{to} {the state at time step t + 1}
#' \item{amount} {the amount of cashflow}
#' }
"cashflowPost"

#' Example data for a livelong life risk insurance contract
#' with premium 4 to be paid when the policy holder is alive
#' 
#' @format A data.table with columns time, state, amount:
#' \describe{
#' \item{time} {the time step the cashflow applies to}
#' \item{state} {the state for which the cashflow arises}
#' \item{amount} {the amount of cashflow}
#' }
#' 
"cashflowPre"


#' Example data from Statistik Oesterreich
#' 
#' The data.table contains yearly mortality data 
#' 
#' @format A data.table with columns time, from, to, p
#' \describe{
#' \item{time} {the time step in question}
#' \item{from} {the state at time t}
#' \item{to} {the state at time t+1}
#' \item{p}{the probability for the change to happen}
#' }
#' 
#' @source table Ausfuehrliche, allgemeine und ausgeglichene Sterbetafeln 1868/71 bis 2010/12 nach dem Geschlecht
#'  \url{https://www.statistik.at/web_de/statistiken/menschen_und_gesellschaft/bevoelkerung/sterbetafeln/index.html}
"trans"