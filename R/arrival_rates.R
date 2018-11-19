# Name: timesutils.R
# Desc: Functions for binning events/calls into time intervals
# Author: Matt Whiteside
# Date: Nov 7, 2018

library(lubridate)
library(data.table)

arrival <- new.env()

# Get various yearly,monthly,daily metrics for each Day of Week and hour
arrival$arrival_rates <- function(dt, f) {
  tmp = dt[,.(nevents=.N), .(startnperiod, startnday, startbin)]
  ave = tmp[, f(nevents), .(startnday, startbin)]
  keycol = c("startnday","startbin")
  setorderv(ave, keycol)
  return(ave)
}

arrival$summary.fun <- function(x) { return(list('mean'=mean(x),'sd'=sd(x),
                                                 'p50'=quantile(x, .50), 'p25'=quantile(x, .25), 'p75'=quantile(x, .75),
                                                 'p90'=quantile(x, .90),
                                                 'min'=min(x), 'max'=max(x))) }



