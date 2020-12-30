completeDist <- function(mc){
  trans      <- mc[["trans"]]
  payoffPre  <- mc[["payoffPre"]]
  payoffPost <- mc[["payoffPost"]]
  states     <- mc[["states"]]
  disc       <- mc[["disc"]]
  V          <- mc[["W"]]
  # berechne verteilung
  granularity <- 0.1
  sim_min <- -granularity
  sim_max <- 11
  firstAge <- 90

  disc <- (1+i)^(-1)
  upsc <- (1+i)^1
  trans[ , toTime := time + 1]

  # 1 erstelle Dist
  temp <- merge(data.table(time=1:horizon, ones=rep(1, horizon)),
                 data.table(state=state, ones=rep(1, length(state))),
                 by=("ones"), allow.cartesian = TRUE)
  # konvertie richtige stutzstellen zu nummerierten
  u_min <- floor(sim_min / granularity)
  u_max <- ceiling(sim_max/granularity)
  stutz = u_min:u_max
  Dist <- merge(temp,
                data.table(ones=rep(1,length(stutz)), u=stutz),
                by=("ones"), allow.cartesian=TRUE)
  Dist$ones <- NULL

  # try vectorized algorithm
  Dist <- merge(Dist, trans,
                by.x=c("time", "state"),
                by.y=c("time", "from"),
                all.x = TRUE, allow.cartesian=TRUE) %>%
    merge(payoffPre, by.x = c("time", "state"),
          by.y=c("time", "state"), all.x = TRUE) %>%
    rename("preAmount"="amount", "toState"="to") %>%
    merge(payoffPost, by.x=c("state", "toState", "time"),
          by.y = c("from", "to", "time"), all.x = TRUE) %>%
    rename("postAmount"="amount")

  Dist[is.na(postAmount), postAmount := 0]
  Dist[is.na(preAmount), preAmount   := 0]
  Dist[, u_new :=
         roundDist((u*granularity - preAmount - postAmount)/granularity*upsc, sim_min, sim_max, granularity)]
  # View(Dist)
  Dist$preAmount <- Dist$postAmount <- NULL
  Result <- Dist[time==horizon,.(time, u, state)][, Prob := as.numeric(0<=u*granularity)]
  # View(Result)
  for (year in (horizon-1):firstAge){
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
