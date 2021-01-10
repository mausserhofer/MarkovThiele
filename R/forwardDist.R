#' evaluate the distribution function for one given state, timee and value
#'
#' This function constructs a tree of points that are to be stepwise
#' evaluated for higher time-steps.
#'
#' @param u value for which to evalute the distribution function
#' @param state state for which to evalute the distribution function
#' @param time time for which to evalute the distribution function
#' @export
forwardDist <- function(mc, u, state, time){
  trans        <- mc[["trans"]]
  cashflowPre  <- mc[["cashflowPre"]]
  cashflowPost <- mc[["cashflowPost"]]
  states       <- mc[["states"]]
  disc         <- mc[["disc"]]
  W            <- mc[["W"]]
  firstAge     <- mc[["firstAge"]]
  lastAge      <- mc[["lastAge"]]

  if (nrow(W[v!=0])>0) stop("The Markov-Thiele-Chain uses terminal condtions.
                      The function cannot be applied. Either change the terminal
                      conditions to cashflows or contribute to improve this function.")

   trans[ , toTime := time + 1]

  # finde benotigte u's
  gridPoints <- data.table(u=u, state=state, time=time)

  step <- function(points, mc, year){
    trans        <- mc[["trans"]]
    cashflowPre  <- mc[["cashflowPre"]]
    cashflowPost <- mc[["cashflowPost"]]
    disc         <- mc[["disc"]]

    #compounding
    comp <- disc[time==year, pv]/disc[time==year+1, pv]

    points <- points[!is.na(state)][,.(time, state, u)]
    points <- merge(points, trans,
                  by.x=c("time", "state"),
                  by.y=c("time", "from"),
                  allow.cartesian=TRUE,
                  all.x = TRUE) %>%
      merge(cashflowPre, by.x = c("time", "state"),
            by.y=c("time", "state"), all.x = TRUE) %>%
      rename("preAmount"="amount", "toState"="to") %>%
      merge(cashflowPost, by.x=c("state", "toState", "time"),
            by.y = c("from", "to", "time"), all.x = TRUE) %>%
      rename("postAmount"="amount")
    points[is.na(postAmount), postAmount := 0]
    points[is.na(preAmount), preAmount   := 0]
    points[, u_new := (u - preAmount - postAmount)*comp]

    dt <- points[,.(time, state, u, p, toTime, toState, u_new)]
    return (dt)
  }

  #gridPoints <- step(startPoints, mc, time)

  # erstelle punkte pro jahr (forward from time to lastAge)
  for (year in time:horizon){
    temp <- rename(gridPoints[time==year, .(toTime, toState, u_new)],
                   "time"="toTime", "state"="toState", "u"="u_new")
    gridPoints <- rbind(gridPoints,
                        step(temp, mc, year),
                        fill=TRUE
    )
  }

  # werte punkte von hinten aus (backward from lastAge to time)
  Result <- gridPoints[toTime==horizon,.(toTime, u_new, toState)] %>%
    rename("state"="toState", "time"="toTime", "u"="u_new")
  Result[, Prob := as.numeric(0<=u)]

  for (year in (lastAge-1):time){
    temp <- gridPoints[time==year][ , index := match(concat(toState, toTime, round(u_new, digits=4)),
                                               Result[time==(year+1)][, concat(state, time, round(u, digits=4))])]
    temp[ , Prob := Result[time==(year+1), Prob][index]]
    if (nrow(temp[is.na(Prob) & !is.na(toState)])>0)
      warning("error in matching data")
    temp[is.na(Prob), Prob := as.numeric(0<=u)]
    temp[is.na(p), p:=1] # these are transitions from 'dead' states, i.e. no outward transition probability
    temp <- temp[ , Prob := sum(p * Prob), by=.(state, u, time)]
    temp <- temp[ , c("state", "u", "time", "Prob")]
    tempResult <- unique(temp, by=c("time", "u", "state", "Prob"))

    Result <- rbind(Result, tempResult)
  }
  year <- year - 1
  t <- time
  return (Result[time==t])
}
