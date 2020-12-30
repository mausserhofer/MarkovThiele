#' @export
markovChain <- function(trans, payoffPre, payoffPost, i=0, W=data.table()){
  #check whether input is of usable type
  if (!("data.frame" %in% intersect(intersect(class(trans), class(payoffPre)),
                                              class(payoffPost))))
      stop("inputs must be data.frame, data.table or tibbles")

  # check whether input contains needed columns
  if(!(dplyr::setequal(names(trans), c("from", "to", "time", "p"))))
      stop("trans must contain columns from, to, time, and p")
  if(!(dplyr::setequal(names(payoffPre), c("state", "time", "amount"))))
    stop("payoffPre must contain columns state, time, and amount")
  if(!(dplyr::setequal(names(payoffPost), c("from", "to", "time", "amount"))))
    stop("payoffPost must contain from, to, time and amount")

  # check whether final conditions are ok
  if (nrow(W)>0){
    if (!(setequal(c("state", "time", "v"), names(W))))
      stop("W must contain columns state, time and v")
  }

  # check whether sum of outgoing-probabilites equals one for all states and times
  out_prob <- trans[,sum(p), by=.(time, from)]$V1
  if (max(out_prob) > 1 | min(out_prob) < 1)
    stop("transition probabilities do not equal 1")

  # compute parameter and limits of markov chain
  firstAge <- min(trans$time)
  lastAge  <- max(trans$time + 1)
  states   <- unique(c(trans$from, trans$to))

  # create discount process
  if (length(i) == 1 & is.numeric(i)){
    v <- (1+i)^-(firstAge:lastAge)
  } else{
    if (!(setequal(c("time", "pv"), names(i))))
          stop("i must be either a scalar representing a fixed interest rate or
               a table consisting of columns time and pv giving the present value of a zero bound
               at given future time")
    v <- c()
    for (j in 1:nrow(i))
      v[i$time[j]] <- i$pv[j]
  }
  if (sum(is.na(v[firstAge:lastAge]))>0)
    stop("present value for at least one time between first age and last age missing")

  # create object
  structure(list(trans=trans,
                 payoffPre=payoffPre,
                 payoffPost=payoffPost,
                 firstAge=firstAge,
                 lastAge=lastAge,
                 states=states,
                 disc=v,
                 W=W),
            class = "markovChain")
}

#' @export
print.markovChain <- function(mc){
  for (st in mc[["states"]]){
    if (st==mc[["states"]][1]) str_states <- st
    else {
      if (st==mc[["states"]][length(mc[["states"]])])
        str_states <- paste0(str_states, ", and ", st)
      else
        str_states <- paste(str_states, st, sep=", ")
    }
  }
  print (paste0("Markov Chain containing states ", str_states,
                " spanning from time ", mc[["firstAge"]], " to time ", mc[["lastAge"]]))
  print ("transition probabilities: ")
  print (head(mc[["trans"]]))
  print ("payoff at beginning of period")
  print (head(mc[["payoffPre"]]))
  print ("payoff at end of period")
  print (head(mc[["payoffPost"]]))
  print ("they interest rate term structure is")
  print (head(mc[["disc"]][mc[["firstAge"]]:mc[["lastAge"]]]))
  if (nrow(mc[["W"]]>0)) {
    print ("terminal conditions")
    print (head(mc[["W"]]))
  } else {
    print ("there are no terminal conditions defined")
  }
}

