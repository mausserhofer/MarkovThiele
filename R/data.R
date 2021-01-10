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
#' with premium 10 to be paid when the policy holder is alive
#' 
#' @format A data.table with columns time, state, amount:
#' \describe{
#' \item{time} {the time step the cashflow applies to}
#' \item{state} {the state for which the cashflow arises}
#' \item{amount} {the amount of cashflow}
#' }
#' 
"cashflowPre"