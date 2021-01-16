#' The function completeV creates the expected value of future cashflows
#' for all possible states and times.
#' @export
EPV <- function(mc){
  trans        <- mc[["trans"]]
  cashflowPre  <- mc[["cashflowPre"]]
  cashflowPost <- mc[["cashflowPost"]]
  states       <- mc[["states"]]
  disc         <- mc[["disc"]]
  V            <- mc[["W"]]
  lastAge      <- mc[["lastAge"]]
  firstAge     <- mc[["firstAge"]]

  trans[ , toTime := time + 1]

  # 0 überprüfe, ob Endbedingungen vorliegen, wenn nicht, setze diese zu lastAge = 0
  if (nrow(V)==0) #d.h. keine Endbedingungen
    V <- data.table::data.table(state=states,
                                time =rep(lastAge, length(states)),
                                v    =rep(0, length(states)))
  
  # 1 erstelle st?tzpunkte f?r W
  stutz <- data.table::merge.data.table(data.table::data.table(time=firstAge:lastAge, ones=rep(1, lastAge-firstAge+1)),
                 data.table::data.table(state=states, ones=rep(1, length(states))),
                 by=("ones"), allow.cartesian = TRUE)
  V <- data.table::merge.data.table(stutz, V, by=c("time", "state"), all.x=TRUE)
  V$ones <- NULL

  # 2 loop through W
  for ( year in (lastAge-1):firstAge){
    # test
    V[ time==year & is.na(v)]
    #pre
    V <- V[ time==year & is.na(v),
            pre := getCashflowPre(state, year, cashflowPre) ]
    #post
    V <- V[ time==year & is.na(v),
            post := disc[time==year+1, pv]/disc[time==year, pv] * getPost(state, year, trans, V)]
    
    V <- V[ time==year & is.na(v),
            v := getCashflowPre(state, year, cashflowPre) +
              disc[time==year+1, pv]/disc[time==year, pv] * getPost(state, year, trans, V)]
  }
  return (V)
}

getCashflowPre <- function(selState, selTime, cashflowPre){
  # selState..vector, selTime..number, cashflowPre..data.table
  
  # filter cashflowPre for time, 
  # sum over all entries (in case of double entries) and make unique
  tempCF <- cashflowPre[state %in% selState & time==selTime][
    , amount := sum(amount), by=state] %>%
    unique()
  
  # merge with given states (better alternative use match)
  foundAmount <- data.table::merge.data.table(
     data.table::data.table(state=selState),
     tempCF,
     all.x = TRUE)
  foundAmount[is.na(amount), amount:=0]
  return (foundAmount$amount)
}

getPost <- function(selState, selTime, trans, W){
  # selState..vector, selTime..number, cashflowPre..data.table
  # get EPV at time + 1 and cashflow at end of period
  
  # start with selected States
  temp <- data.table::merge.data.table(
    data.table::data.table(from=selState),
    trans[from %in% selState & time==selTime],
    all.x = TRUE
  )
  
  # add EPV at time + 1 
  temp <- data.table::merge.data.table(
                temp,
                W[time==(selTime+1)],
                all.x=TRUE, by.x=c("to", "toTime"), by.y=c("state", "time"))
  
  # preprocess cashflowPost
  tempCF <- cashflowPost[from %in% selState & time==selTime][
    , amount := sum(amount), by=.(from, to)] %>%
    unique()
  
  # add cashflows at end of period
  temp <- data.table::merge.data.table(temp,
                tempCF,
                all.x=TRUE, by=c("from", "to", "time"))
  temp[is.na(amount), amount:=0]
  temp[is.na(v), v:=0]
  temp[is.na(p), p:=0]
  post <- temp[ , sum(p * (amount+v)), by=from]

  # print (temp)
  return (post$V1)
}
