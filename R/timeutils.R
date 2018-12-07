# Name: timesutils.R
# Desc: Functions for binning events/calls into time intervals
# Author: Matt Whiteside
# Date: Nov 7, 2018

library(lubridate)
library(data.table)

timeutils <- new.env()

timeutils$format_time <- function(dt, startcol, endcol=NULL) {
  
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
  
  intervals = datetimes[,interval(start=start, end=end)]
  datetimes$interval_length = as.numeric(intervals,units="secs")
  
  datetimes <- cbind(dt, datetimes)
  setorder(datetimes, start)
  
  return(datetimes)
}

timeutils$bin_time <- function(dt, duration.in.min) {
 
  u=paste(duration.in.min,"mins")
  dt[,start.window:=floor_date(start,unit=u)]
  dt[,end.window:=floor_date(end,unit=u)]
  dt[,interval.windows:=mapply(seq, from=start.window, to=end.window, MoreArgs=list(by=u))]
  
  invisible(dt)
}

timeutils$concurrent_windows <- function(dt) {
  
  histories=as.data.table(dt[,as.POSIXct(unlist(interval.windows), tz="UTC", origin="1970-01-01 UTC")])
  names(histories) <-c("window") 
  setkey(histories, window)
  
  return(histories[,.(count = .N), window])
}

# timeutils$bin_timestamp <- function(timestamp, duration) {
#   minuteval = hour(timestamp) * 60 + minute(timestamp)
#   minuteval %/% duration
# }
# 
# timeutils$add_interval_bins <- function(dt, startbin="startbin", endbin="endbin", duration=60, period=7) {
#   nb = floor(24*60/duration)
#   
#   dt[, intervalbins := mapply(timeutils$span, startnday, startbin, startnperiod, endnday, endbin, endnperiod,
#                               nbins=nb, ndays=period)]
#   
#   invisible(dt)
# }
# 
# timeutils$span <- function(nday1, bin1, period1, nday2, bin2, period2, nbins, ndays) {
#   # Assumes events that span a reporting period occur in the following period. I.e. no event can span longer than
#   # a single period
#   
#   periodspan = period2-period1
#   dayspan= periodspan * ndays + (nday2-nday1)
#   binspan = dayspan * nbins + (bin2 - bin1)
#   
#   if(periodspan < 0) {
#     cat(paste(period1,period2,periodspan,"\n"))
#     cat(paste(dayspan,"\n"))
#     cat(paste(binspan,"\n"))
#     stop("Invalid period inputs")
#   }
#   
#   bins = sapply(seq(bin1,bin1+binspan), 
#                 function(x){
#                   d = (x %/% nbins + nday1)
#                   p = (d %/% ndays + period1)
#                   return(list(nperiod=p,
#                               nday=d %% ndays, 
#                               bin=x %% nbins))}
#                 )
#   return(bins)
# }
# 
# 
# timeutils$concurrent_counts <- function(dt, duration=60, period=7) {
#   
#   maxperiod <- max(dt[,endnperiod])
#   nb <- floor(24*60/duration)
#   units <- data.table(expand.grid(bin=seq(0,nb), day=seq(0,6), period=seq(0,maxperiod)))
#   units[,count:=0]
#   setkey(units, bin, day, period)
#   
#   dt[, {l_ply(intervalbins,
#                         function(l) {
#                           a_ply(l, 2, 
#                                 function(c) {
#                                   units[bin==c[1] & day==c[2] & period==c[3], count := count+1 ]
#                                   return(NULL)
#                                 })
#                         })
#             }]
#   
#   return(units)
# }
# 
# timeutils$concurrent_counts_group_by <- function(dt) {
#   
#   records<-data.table(matrix(dt[,unlist(intervalbins)], ncol=3, byrow=TRUE))
#   names(records) <-c("period", "day", "bin") 
#   setkey(records, period, day, bin)
#   
#   return(records[,.(count = .N), .(period, day, bin)])
# }
# 
# 
# 
# 
# # Get various yearly,monthly,daily metrics for each Day of Week and hour
# timeutils$arrival_rates <- function(dt, f) {
#   tmp = dt[,.(nevents=.N), .(startnperiod, startnday, startbin)]
#   ave = tmp[, f(nevents), .(startnday, startbin)]
#   keycol = c("startnday","startbin")
#   setorderv(ave, keycol)
#   return(ave)
# }
# 
# timeutils$summary.fun <- function(x) { return(list('mean'=mean(x), 'p50'=quantile(x, .50), 'p90'=quantile(x, .90), 'sd'=sd(x),
#                                          'min'=min(x), 'max'=max(x))) }
# 
# timeutils$daylabel <- function(d) {
#   i = d+1
#   labels = c("Sun","Mon","Tue", "Wed", "Thu", "Fri", "Sat")
#   return(labels[i])
# }

