#' evaluate the distribution function for one given state, timee and value
#'
#' This function constructs a tree of points that are to be stepwise
#' evaluated for higher time-steps.
#'
#' @param u value for which to evalute the distribution function
#' @param state state for which to evalute the distribution function
#' @param time time for which to evalute the distribution function
#' @export
PointDist <- function(mc, u, state, time){
  trans        <- mc[["trans"]]
  cashflowPre  <- mc[["cashflowPre"]]
  cashflowPost <- mc[["cashflowPost"]]
  states       <- mc[["states"]]
  disc         <- mc[["disc"]]
  W            <- mc[["W"]]
  firstAge     <- mc[["firstAge"]]
  lastAge      <- mc[["lastAge"]]

  if (nrow(W)>0)
    if (nrow(W[v!=0])>0) stop("The Markov-Thiele-Chain uses terminal condtions.
                      The function cannot be applied. Either change the terminal
                      conditions to cashflows or contribute to improve this function.")

   trans[ , toTime := time + 1]

  
   # - function step takes toState, toTime and toU of previous step
   #   and identifies the state and new U 
  
  step <- function(points, mc, year){
    # adds accesible points for next time step
    trans        <- mc[["trans"]]
    cashflowPre  <- mc[["cashflowPre"]]
    cashflowPost <- mc[["cashflowPost"]]
    disc         <- mc[["disc"]]

    # compounding
    comp <- disc[time==year, pv]/disc[time==year+1, pv]
  
    # make step and add cashflowPre and cashflowPost
    points <- points[!is.na(state)&toTime==year][,.(toTime, toState, toU)] %>% 
      rename("time"="toTime", "state"="toState", "u"="toU")
    
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
    points[ , toU := (u - preAmount - postAmount)*comp]
    points[ , toTime := time + 1]
    
    # states without outogoing probabiliites need not to be evaluated
    points <- points[!is.na(toState)] 
    
    dt <- points[,.(time, state, u, p, toTime, toState, toU)]
    return (dt)
  }
  
  # finde benotigte u's
  startPoints <- data.table::data.table(toU=u, toState=state, toTime=time)
  gridPoints <- step(startPoints, mc, time)
  
  # erstelle punkte pro jahr (forward from time to lastAge)
  for (year in (time+1):(lastAge-1)){
    gridPoints <- rbind(step(gridPoints, mc, year),
                        gridPoints)
  }

  # werte punkte von hinten aus (backward from lastAge to time)
  Result <- gridPoints[toTime==lastAge,.(toTime, toState, toU)][, Prob := as.numeric(0<=toU)] %>%
    rename("time"="toTime", "state"="toState", "u"="toU")

  for (year in (lastAge-1):time){
    temp <- gridPoints[time==year]
    temp[ , index := match(concat(toState, year+1, round(toU, digits=6)),
                     Result[time==(year+1)][, concat(state, time, round(u, digits=6))])]
    temp[ , Prob := Result[time==(year+1), Prob][index]]
    # if (nrow(temp[is.na(Prob) & !is.na(state)])>0)
    #   warning("error in matching data")
    temp[is.na(Prob), Prob := as.numeric(0<=toU)]
    # temp[is.na(p), p:=1] # these are transitions from 'dead' states, i.e. no outward transition probability
    temp <- temp[ , Prob := sum(p * Prob), by=.(state, u, time)]
    temp <- temp[ , c("state", "u", "time", "Prob")]
    tempResult <- unique(temp, by=c("time", "u", "state", "Prob"))

    Result <- rbind(Result, tempResult)
  }
  t <- time
  return (Result[time==t])
}
