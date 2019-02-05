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



