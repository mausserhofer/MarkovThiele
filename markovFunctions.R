DQcheck <- function(V, trans, payoffPost, payoffPre, i){
  #TO-DO: data quality checks einbauen
  return ("passed")
}

findV <- function(selTime, selState){
  return (unlist(V[state==selState & time==selTime, "v"]))
}

nextPeriod <- function(selState, selTime, W){
  temp <- merge(trans[from==selState & time==selTime], 
                W, 
                all.x=TRUE, by.x=c("to", "toTime"), by.y=c("state", "time"))
  temp <- merge(temp, 
                payoffPost, 
                all.x=TRUE, by=c("from", "to", "time"))
  temp[is.na(amount), amount:=0]
  temp[ , v_add := p * (amount+v)]
  
  # print (temp)
  return (temp[,sum(v_add)])
}

aPre <- function(selState, selTime){
  foundAmount <- unlist(payoffPre[state==selState & time==selTime, "amount"])
  # print (foundAmount)
  if (length(foundAmount)>0) return (foundAmount)
  else return (0)
  }

completeV <- function(W, trans, payoffPost, payoffPre, i, state){
  disc <- (1+i)^(-1)
  trans[ , toTime := time + 1]
  
  if (DQcheck(W, trans, payoffPost, payoffPre, i)=="failed") return (NULL)
  
  # 1 erstelle st?tzpunkte f?r W
  maxTime <- max(W$time)
  stutz <- merge(data.table(time=1:maxTime, ones=rep(1, maxTime)), 
                 data.table(state=state, ones=rep(1, length(state))), 
                 by=("ones"), allow.cartesian = TRUE)
  W <- merge(stutz, W, by=c("time", "state"), all.x=TRUE)
  
  # 2 loop through W
  for ( year in (maxTime-1):1){
    # W <- W[ time==year & is.na(v),
    #         pre := aPre(state, time) ]
    # W <- W[ time==year & is.na(v),
    #         post :=  disc*nextPeriod(state, time)]
    W <- W[ time==year & is.na(v),
            v := aPre(state, time) + disc*nextPeriod(state, time, W)]
  }
  
  return (W)
}

recDist <- function(selTime, selState, u){ #probability DCF < u
  temp <- trans[from==selState & time==selTime]
  if (selTime==horizon | nrow(temp)==0) {
    ans <- as.numeric(0<=u)
    return (ans)
  }

  temp <- merge(temp, 
                payoffPost, 
                all.x=TRUE, by=c("from", "to", "time")) %>% 
    rename("post"="amount")
  temp <- merge(temp, payoffPre,
                all.x = TRUE, 
                by.x=c("from", "time"), 
                by.y=c("state", "time")) %>% 
          rename("pre"="amount")
  temp[is.na(post), post:=0]
  temp[is.na(pre), pre:=0]
  temp[ , u_new := disc * (u - pre - post)]
  temp[ , dist := mapply(recDist, selTime = toTime, selState = to, u=u_new)]
  temp[ , sum := p * dist] 
    
  
  return (temp[,sum(sum)])
  
}

nextPeriodProb <- function(selState, selTime, u, table){
  # finde stützpunkt für zeitschritt später
  temp <- trans[time==selTime & from==selState]
  if (nrow(temp) == 0) return (as.numeric(0<u))
  
  temp <- merge(temp, 
                payoffPost, 
                all.x=TRUE, by=c("from", "to", "time")) %>%
          rename("post"="amount")
  temp[is.na(post), post:=0]
  temp <- merge(temp,
                payoffPre,
                all.x=TRUE, by.x = c("from", "time"), by.y=c("state", "time")) %>% 
          rename("pre"="amount")
  temp[, u_new := disc*(u - pre - post)]
  temp[, u_app := roundDist(u_new)]
  
  # add probability of one time step later
  temp <- merge(temp, 
                table, 
                by.x=c("toTime", "to", "u_app"), 
                by.y=c("time", "state", "u"),
                all.x = TRUE)
  temp[ , sum := p * Prob]
  
  ans <- temp[,sum(sum)]
  if (is.na(ans)){
    print("keine passenden u's gefunden")
    print(temp)
    ans <- 0
  } 
  
  # print(temp)
  return (ans)
}

roundDist <- function(x){
  u_min <- floor(sim_min / granularity)
  u_max <- floor(sim_max / granularity) + 1
  return (as.integer(pmin(pmax(u_min, round(x)), u_max)))
}

concat <- function(...){
  args <- list(...)
  narg <- length(args)
  if (narg < 1L) return ("")
  else if (narg == 1L) return (args[[1]])
  else{
    key <- args[[1]]
    for (i in 2:(narg)){
      key <- paste(key, args[[i]], sep=".")
    }
  } 
  return (key)
}

completeDist <- function(granularity, sim_min, sim_max, trans, payoffPost, payoffPre, i, state){
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
         roundDist((u*granularity - preAmount - postAmount)/granularity*upsc)]
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

filterDist <- function(selState, selTime, selU){
  ans <- Dist[time==selTime & state==selState & u < selU + eps & u > selU - eps, .(Prob)]
  if (length(ans)==0) ans <- as.numeric(0<selU)
  return (ans)
}

