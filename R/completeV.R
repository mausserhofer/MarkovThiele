#' @export
completeV <- function(mc){
  trans      <- mc[["trans"]]
  payoffPre  <- mc[["payoffPre"]]
  payoffPost <- mc[["payoffPost"]]
  states     <- mc[["states"]]
  disc       <- mc[["disc"]]
  V          <- mc[["W"]]
  lastAge    <- mc[["lastAge"]]
  firstAge   <- mc[["firstAge"]]

  trans[ , toTime := time + 1]

  # 1 erstelle st?tzpunkte f?r W
  maxTime <- max(V$time)
  stutz <- data.table::merge.data.table(data.table::data.table(time=1:maxTime, ones=rep(1, maxTime)),
                 data.table::data.table(state=states, ones=rep(1, length(states))),
                 by=("ones"), allow.cartesian = TRUE)
  V <- data.table::merge.data.table(stutz, V, by=c("time", "state"), all.x=TRUE)
  V$ones <- NULL

  # 2 loop through W
  for ( year in (maxTime-1):1){
    V <- V[ time==year & is.na(v),
            v := aPre(state, time) + disc[time]/disc[time+1] *nextPeriod(state, time, V)]
  }


  mc[["V"]] <- V
  return (mc)
}

aPre <- function(selState, selTime){
  foundAmount <- unlist(payoffPre[state==selState & time==selTime, "amount"])
  # print (foundAmount)
  if (length(foundAmount)>0) return (foundAmount)
  else return (0)
}

nextPeriod <- function(selState, selTime, W){
  temp <- data.table::merge.data.table(trans[from==selState & time==selTime],
                W,
                all.x=TRUE, by.x=c("to", "toTime"), by.y=c("state", "time"))
  temp <- data.table::merge.data.table(temp,
                payoffPost,
                all.x=TRUE, by=c("from", "to", "time"))
  temp[is.na(amount), amount:=0]
  temp[ , v_add := p * (amount+v)]

  # print (temp)
  return (temp[,sum(v_add)])
}
