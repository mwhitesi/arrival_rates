# Name: timesutils.R
# Desc: Functions for binning events/calls into time intervals
# Author: Matt Whiteside
# Date: Nov 7, 2018

library(lubridate)
library(data.table)

timeutils <- new.env()

timeutils$bin_timestamp <- function(timestamp, duration) {
  minuteval = hour(timestamp) * 60 + minute(timestamp)
  minuteval %/% duration
}

timeutils$bin_events_by_time <- function(dt, startcol, duration, period="week", endcol=NULL) {
  
  datetimes <- dt[,.(start=get(startcol))]
  if (!any(is.Date(datetimes$start))) {
    datetimes[,start := as_datetime(start)]
  }
  
  if(is.null(endcol)) {
    # Treat single timepoints as zero length intervals
    endcol = startcol
  }
  datetimes <- cbind(datetimes, dt[,.(end=get(endcol))])
  if (!any(is.Date(datetimes$end))) {
    datetimes[,end := as_datetime(end)]
  }
  
  # Add column with time period
  if(period == "week") {
    firstweek = floor_date(datetimes[,min(start)], unit = "weeks", week_start=1)
    datetimes[,`:=`(startnday=wday(start, week_start=1)%%7)]
    datetimes[,`:=`(endnday=wday(end, week_start=1)%%7)]
    datetimes[,`:=`(startnperiod=(start - firstweek)%/%dweeks(1))]
    datetimes[,`:=`(endnperiod=(start - firstweek)%/%dweeks(1))]
  } else {
    stop(c("period='",period,"' not implemented"))
  }
  
  intervals = datetimes[,interval(start=start, end=end)]
  datetimes$interval_length = as.numeric(intervals,units="secs")
  
  datetimes[,`:=`(startbin=timeutils$bin_timestamp(start, duration), endbin=timeutils$bin_timestamp(end, duration))]
  datetimes <- cbind(dt, datetimes)
  
  setorder(datetimes, start)
  
  return(datetimes)
}

timeutils$add_interval_bins <- function(dt, startbin="startbin", endbin="endbin", duration=60, period=7) {
  nb = round(24*60/duration)
  
  dt[, intervalbins := mapply(timeutils$span, startnday, startbin, startnperiod, endnday, endbin, endnperiod,
                              nbins=nb, ndays=period)]
  
}

timeutils$span <- function(nday1, bin1, period1, nday2, bin2, period2, nbins, ndays) {
  # Assumes events that span a reporting period occur in the following period. I.e. no event can span longer than
  # a single period
  
  periodspan = period2-period1
  dayspan= periodspan * ndays + (nday2-nday1)
  binspan = dayspan * nbins + (bin2 - bin1)
  
  if(periodspan < 0) {
    
    cat(paste(period1,period2,periodspan,"\n"))
    cat(paste(dayspan,"\n"))
    cat(paste(binspan,"\n"))
    stop()
  }
  
  
  bins = sapply(seq(bin1,bin1+binspan), 
                function(x){
                  d = (x %/% nbins + nday1)
                  p = (d %/% ndays + period1)
                  return(list(nperiod=p,
                              nday=d %% ndays, 
                              bin=x %% nbins))}
                )
  return(bins)
}


timeutils$concurrent_counts <- function(dt, duration=60, period=7) {
  # concurrent_counts = data.table(
  #   nday=seq(0,period-1),
  #   bin=rep(seq(0,nd-1),period)
  # )
}

# Get various yearly,monthly,daily metrics for each Day of Week and hour
timeutils$arrival_rates <- function(dt, f) {
  tmp = dt[,.(nevents=.N), .(startnperiod, startnday, startbin)]
  ave = tmp[, f(nevents), .(startnday, startbin)]
  keycol = c("startnday","startbin")
  setorderv(ave, keycol)
  return(ave)
}

timeutils$summary.fun <- function(x) { return(list('mean'=mean(x), 'p50'=quantile(x, .50), 'p90'=quantile(x, .90), 'sd'=sd(x),
                                         'min'=min(x), 'max'=max(x))) }

timeutils$daylabel <- function(d) {
  i = d+1
  labels = c("Sun","Mon","Tue", "Wed", "Thu", "Fri", "Sat")
  return(labels[i])
}

