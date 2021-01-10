
roundDist <- function(x, sim_min, sim_max, granularity){
  u_min <- floor(sim_min / granularity)
  u_max <- ceiling(sim_max / granularity)
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

DQcheck <- function(V, trans, payoffPost, payoffPre, i){
  #TO-DO: data quality checks einbauen
  return ("passed")
}
