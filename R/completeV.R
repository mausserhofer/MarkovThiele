#' @export
completeV <- function(W, trans, payoffPost, payoffPre, i, state){
  disc <- (1+i)^(-1)
  trans[ , toTime := time + 1]

  if (DQcheck(W, trans, payoffPost, payoffPre, i)=="failed") return (NULL)

  # 1 erstelle st?tzpunkte f?r W
  maxTime <- max(W$time)
  stutz <- data.table::merge(data.table::data.table(time=1:maxTime, ones=rep(1, maxTime)),
                 data.table::data.table(state=state, ones=rep(1, length(state))),
                 by=("ones"), allow.cartesian = TRUE)
  W <- data.table::merge(stutz, W, by=c("time", "state"), all.x=TRUE)

  # 2 loop through W
  for ( year in (maxTime-1):1){
    W <- W[ time==year & is.na(v),
            v := aPre(state, time) + disc*nextPeriod(state, time, W)]
  }

  return (W)
}

aPre <- function(selState, selTime){
  foundAmount <- unlist(payoffPre[state==selState & time==selTime, "amount"])
  # print (foundAmount)
  if (length(foundAmount)>0) return (foundAmount)
  else return (0)
}
nextPeriod <- function(selState, selTime, W){
  temp <- data.table::merge(trans[from==selState & time==selTime],
                W,
                all.x=TRUE, by.x=c("to", "toTime"), by.y=c("state", "time"))
  temp <- data.table::merge(temp,
                payoffPost,
                all.x=TRUE, by=c("from", "to", "time"))
  temp[is.na(amount), amount:=0]
  temp[ , v_add := p * (amount+v)]

  # print (temp)
  return (temp[,sum(v_add)])
}
