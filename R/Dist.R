#' A function to compute the distrubtion of the present value of
#' future cashflows
#' @param granularity indicates the amount of precision to be used in the distribution
#'
#' @export
Dist <- function(mc, granularity = 0.1){
  # check whether there are terminal conditions, if so, function cannot be applied
  trans        <- mc[["trans"]]
  cashflowPre  <- mc[["cashflowPre"]]
  cashflowPost <- mc[["cashflowPost"]]
  states       <- mc[["states"]]
  disc         <- mc[["disc"]]
  W            <- mc[["W"]]
  firstAge     <- mc[["firstAge"]]
  lastAge      <- mc[["lastAge"]]

  if (nrow(W)>0)
    if (nrow(W[v!=0])>0) 
      stop("The Markov-Thiele-Chain uses terminal condtions. The function cannot be applied. Either change the terminal                       conditions to cashflows or contribute to improve this function.")

  # heuristics for maximal and minimal values of u - To-Do: to be improved
  sim_min <- cashflowPre[amount<0, sum(amount)] + cashflowPost[amount<0, sum(amount)] - granularity
  sim_max <- cashflowPre[amount>0, sum(amount)] + cashflowPost[amount>0, sum(amount)]

  trans[ , toTime := time + 1]

  # 1 erstelle Dist
  temp <- merge(data.table::data.table(time=1:lastAge, ones=rep(1, lastAge)),
                 data.table::data.table(state=states, ones=rep(1, length(states))),
                 by=("ones"), allow.cartesian = TRUE)
  # konvertie richtige stutzstellen zu nummerierten
  u_min <- floor(sim_min / granularity)
  u_max <- ceiling(sim_max/granularity)
  stutz = u_min:u_max
  Dist <- merge(temp,
                data.table::data.table(ones=rep(1,length(stutz)), u=stutz),
                by=("ones"), allow.cartesian=TRUE)
  Dist$ones <- NULL

  # generate data.table with all points to be evaluated and their 'successor'
  Dist <- merge(Dist, trans,
                by.x=c("time", "state"),
                by.y=c("time", "from"),
                all.x = TRUE, allow.cartesian=TRUE) %>%
    merge(cashflowPre, by.x = c("time", "state"),
          by.y=c("time", "state"), all.x = TRUE) %>%
    rename("preAmount"="amount", "toState"="to") %>%
    merge(cashflowPost, by.x=c("state", "toState", "time"),
          by.y = c("from", "to", "time"), all.x = TRUE) %>%
    rename("postAmount"="amount")

  Dist[is.na(postAmount), postAmount := 0]
  Dist[is.na(preAmount), preAmount   := 0]
  Dist[ ,year := time]
  Dist[year<lastAge , 
       comp := disc[,pv][match(year, disc$time)] / disc[,pv][match(year+1, disc$time)]]
  Dist[, u_new :=
         roundDist(comp*(u*granularity - preAmount - postAmount)/granularity,
                   sim_min, sim_max, granularity)]
  # View(Dist)
  Dist$preAmount <- Dist$postAmount <- Dist$year <- NULL
  
  Result <- Dist[time==lastAge,.(time, u, state)][, Prob := as.numeric(0<=u*granularity)]
  # View(Result)
  for (year in (lastAge-1):firstAge){
    # möchten vektor mit Probs für die time == ursprünglicher toState
    # und time ursprünglich to Time

    temp <- Dist[time==year][ , index := match(concat(toState, toTime, u_new),
                                               Result[time==(year+1)][, concat(state, time, u)])]
    temp[ , Prob := Result[time==(year+1), Prob][index]]
    temp <- temp[ , Prob := sum(p * Prob), by=.(state, u, time)]
    temp[is.na(Prob), Prob := as.numeric(0<=u)]
    temp <- temp[ , c("state", "u", "time", "Prob")]
    tempResult <- unique(temp, by=c("state", "u", "time", "Prob"))

    Result <- rbind(Result, tempResult)
  }
  Result[,u:=u*granularity]
  return (Result)
}
