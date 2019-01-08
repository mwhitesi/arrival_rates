# External packages
source("R/packages.R") 


# Work flow functions
source("R/forecast_targets.R")
source("R/historical_targets.R")

dur.m = 15

# Load required data
# st=drake::readd(st2)
# ar=drake::readd(ar2)
# dt = readd(dt1)


utl = list()

experiment <- function(dmnd) {
  
  # No alteration to the queue model inputs
  servs = modelTarget(ar, st, duration.in.min=dur.m, ar.lag=0, st.lag=0, do.plot=FALSE)
  utl[['NoLag']] = utilizationTarget(dmnd, servs, duration.in.min=dur.m, do.plot=FALSE)

  # # Average arrival rate lagged by mean service rate
  # servs = modelTarget(ar, st, duration.in.min=dur.m, ar.lag=NULL, st.lag=0, do.plot=FALSE)
  # utl[['ARLagAve']] = utilizationTarget(dmnd, servs, duration.in.min=dur.m, do.plot=FALSE)
  # 
  # Average arrival rate & service lagged by mean service rate
  servs = modelTarget(ar, st, duration.in.min=dur.m, ar.lag=NULL, st.lag=NULL, do.plot=FALSE)
  utl[['LagAve']] = utilizationTarget(dmnd, servs, duration.in.min=dur.m, do.plot=FALSE)
  # 
  # # Average arrival rate & service lagged by preceding period
  # servs = modelTarget(ar, st, duration.in.min=dur.m, ar.lag=1, st.lag=1, do.plot=FALSE)
  # utl[['Lag1']] = utilizationTarget(dmnd, servs, duration.in.min=dur.m, do.plot=FALSE)
  # 
  # # Average arrival rate lagged by mean service rate, service lagged by preceding period
  # servs = modelTarget(ar, st, duration.in.min=dur.m, ar.lag=NULL, st.lag=0, do.plot=FALSE)
  # utl[['ARLagAveSTLag1']] = utilizationTarget(dmnd, servs, duration.in.min=dur.m, do.plot=FALSE)
  # 
  # # Average arrival rate lagged by mean service rate, service lagged by preceding period
  # servs = modelTarget(ar, st, duration.in.min=dur.m, ar.lag=10, st.lag=10, do.plot=FALSE)
  # utl[['LagAve10']] = utilizationTarget(dmnd, servs, duration.in.min=dur.m, do.plot=FALSE)
  
  # Average arrival rate lagged by mean service rate, service lagged by preceding period
  servs = modelTarget(ar, st, duration.in.min=dur.m, ar.lag=NULL, st.lag=NULL, load.threshold=35, do.plot=FALSE)
  utl[['LagAveLoad']] = utilizationTarget(dmnd, servs, duration.in.min=dur.m, do.plot=FALSE)
  
  
  return(utl)
}

dmnd = demandTarget(dt, duration.in.min=dur.m, do.plot=FALSE)

utl=experiment(dmnd)

utl.df = data.frame(matrix(unlist(utl), nrow=length(utl), byrow=T), stringsAsFactors=FALSE)
colnames(utl.df) = names(utl[[1]])
rownames(utl.df) = names(utl)
