# u <- 9
# state <- "alive"
# time <- 90

forwardDist <- function(u, state, time, trans, payoffPost, payoffPre, i){
  upsc <- (1+i)
  trans[ , toTime := time + 1]
  
  # finde benotigte u's
  startPoints <- data.table(u=u, state=state, time=time)
  
  step <- function(points, trans, payoffPre, payoffPost, upsc){
    points <- points[!is.na(state)][,.(time, state, u)]
    points <- merge(points, trans,
                  by.x=c("time", "state"),
                  by.y=c("time", "from"),
                  allow.cartesian=TRUE,
                  all.x = TRUE) %>%
      merge(payoffPre, by.x = c("time", "state"), 
            by.y=c("time", "state"), all.x = TRUE) %>% 
      rename("preAmount"="amount", "toState"="to") %>% 
      merge(payoffPost, by.x=c("state", "toState", "time"),
            by.y = c("from", "to", "time"), all.x = TRUE) %>%
      rename("postAmount"="amount")
    points[is.na(postAmount), postAmount := 0]
    points[is.na(preAmount), preAmount   := 0]
    points[, u_new := (u - preAmount - postAmount)*upsc]
    
    dt <- points[,.(time, state, u, p, toTime, toState, u_new)]
    return (dt)
  }
  
  gridPoints <- step(startPoints, trans, payoffPre, payoffPost, upsc)
  # erstelle punkte pro jahr
  for (year in time:horizon){
    temp <- rename(gridPoints[time==year, .(toTime, toState, u_new)],
                   "time"="toTime", "state"="toState", "u"="u_new")
    gridPoints <- rbind(gridPoints, 
                        step(temp, 
                             trans, payoffPre, payoffPost, upsc),
                        fill=TRUE
    )
  }
  
  # werte punkte von hinten aus
  Result <- gridPoints[toTime==horizon,.(toTime, u_new, toState)] %>%
    rename("state"="toState", "time"="toTime", "u"="u_new")
  Result[, Prob := as.numeric(0<=u)]
  
  for (year in (horizon-1):time){
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
