#' The function completeV creates the expected value of future cashflows
#' for all possible states and times.
#' @export
completeV <- function(mc){
  trans        <- mc[["trans"]]
  cashflowPre  <- mc[["cashflowPre"]]
  cashflowPost <- mc[["cashflowPost"]]
  states       <- mc[["states"]]
  disc         <- mc[["disc"]]
  V            <- mc[["W"]]
  lastAge      <- mc[["lastAge"]]
  firstAge     <- mc[["firstAge"]]

  trans[ , toTime := time + 1]

  # 1 erstelle st?tzpunkte f?r W
  stutz <- data.table::merge.data.table(data.table::data.table(time=firstAge:lastAge, ones=rep(1, lastAge-firstAge+1)),
                 data.table::data.table(state=states, ones=rep(1, length(states))),
                 by=("ones"), allow.cartesian = TRUE)
  V <- data.table::merge.data.table(stutz, V, by=c("time", "state"), all.x=TRUE)
  V$ones <- NULL

  # 2 loop through W
  for ( year in (lastAge-1):firstAge){
    V <- V[ time==year & is.na(v),
            v := aPre(state, time) +
              disc[time==year+1, pv]/disc[time==year, pv] * nextPeriod(state, time, V)]
  }
  return (V)
}

aPre <- function(selState, selTime){
  foundAmount <- unlist(cashflowPre[state==selState & time==selTime, "amount"])
  # print (foundAmount)
  if (length(foundAmount)>0) return (foundAmount)
  else return (0)
}

nextPeriod <- function(selState, selTime, W){
  temp <- data.table::merge.data.table(trans[from==selState & time==selTime],
                W,
                all.x=TRUE, by.x=c("to", "toTime"), by.y=c("state", "time"))
  temp <- data.table::merge.data.table(temp,
                cashflowPost,
                all.x=TRUE, by=c("from", "to", "time"))
  temp[is.na(amount), amount:=0]
  temp[ , v_add := p * (amount+v)]

  # print (temp)
  return (temp[,sum(v_add)])
}
