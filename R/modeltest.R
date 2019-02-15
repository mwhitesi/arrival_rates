modeltest__makeQ <- function(ar2, st2, levels=c(seq(0.009, 0.001, by=-0.002), seq(0.0009, 0.0001, by=-0.0002))) {
  #' Make multiple Queue models with a range of service levels
  #' 
  #' 
  
  # Parameters
  ar.lag = NULL 
  st.lag = NULL
  load.threshold = 35
  duration.in.min = 15
  
  
  # Generate models
  models <- list()
  for(sl in levels) {
    models[[paste0('p',round(sl,4))]] = queuemodel__model(ar2, st2, duration.in.min, sl, ar.lag, st.lag, load.threshold, do.plot=FALSE)
  }
  
  return(models)
}

modeltest__makeR <- function(demand, p=c(seq(.95, .99, by=0.02), seq(.991, .999, by=0.002))) {
  #' Make multiple regression models with a range of prediction intervals
  #' 
  #' 
  
  # Parameters
  duration.in.min = 15
  
  
  # Generate models
  models <- list()
  for(pi in p) {
    #pi = signif(pi,4)
    print(pi)
    models[[paste0('p',pi)]] = tsregression__fitFFTModel(demand, duration.in.min, pi)
  }
  
  return(models)
}