# horizon is the maximum age subjects can attain
# payoffs at age horizon and later are zero
# 
# 

createTransition <- function(file, sheet){
  st <- read.xlsx(file, 
                  sheet=sheet, 
                  startRow = 6) %>%
    data.table()
  st[ , p := 1 - q]
  horizon <- max(st$x)+1
  
  
  trans = data.table(from=c(), to=c(), time=c(), p=c())
  trans = rbind(trans, data.table(time=st$x,
                                  from=rep("alive", horizon), 
                                  to=rep("dead", horizon), 
                                  p=st$q))
  trans = rbind(trans, data.table(time=st$x,
                                  from=rep("alive", horizon), 
                                  to=rep("alive", horizon), 
                                  p=st$p ))
  return (trans)
}

createV <- function() {
  V <- data.table(state=c(), time=c(), v=c())
  V <- rbind(V, data.table(state=rep("dead", horizon), time=1:horizon, v=0))
  V <- rbind(V, data.table(state="alive", time=horizon, v=0))
  V <- V[order(time, decreasing=TRUE)]
  return (V)
}

createPayoffPost <- function(al, horizon){
  max_active <- horizon - 1
  payoffPost <- data.table(from=c(), to=c(), time=c(), amount=c())
  payoffPost <- rbind(payoffPost, data.table(from=rep("alive", max_active), 
                                             to=rep("dead", max_active), 
                                             time=1:max_active, 
                                             amount=rep(al,max_active))
  )
  return (payoffPost)
}

createPayoffPre <- function(el, horizon) {
  max_active <- horizon - 1
  payoffPre <- data.table(state=c(), time=c(), amount=c())
  payoffPre <- rbind(payoffPre, data.table(state="alive", 
                                           time=1:max_active, 
                                           amount=rep(el, max_active)))
  return (payoffPre)
}
