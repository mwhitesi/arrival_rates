# Name: tsregression.R
# Desc: Compute expected units required using regression. Plot figures
# Author: Matt Whiteside
# Date: Feb 11, 2019

tsregression__fitFFTModel <- function(hd, duration.in.min=15, prediction.interval=.995, do.plot=TRUE) {
  
  setkey(hd, window)
  
  # Start and end on sunday
  startday = min(hd[weekly.bin == 1,window])
  endday = max(hd[weekly.bin == 7,window])
  d = hd[window >= startday & window <= endday]
  
  # Fill in empty values
  dur.m = paste(duration.in.min, "min")
  periods = seq(from=startday,to=endday,by=dur.m)
  empty = data.table(window=periods, count=0)
  setkey(empty, window)
  d2 = empty[d][, count:=ifelse(is.na(i.count), count, i.count)]
  
  # Create time series object
  # the seasonalities that we are interested are sub-weekly (trying to only fit one week)
  hourly=60/duration.in.min
  yms = msts(d2[,count], seasonal.periods = c(24*hourly, 168*hourly))
  
  # Use AICc to find the optimal number of fast-fourier terms to model seasonality
  # Num of fourier terms were selected using AICc
  fit <- tslm(yms ~ fourier(yms, K = c(7,6)))
  
  
  # Lots of uncaptured patterns/correlation in residuals, but at least residuals are centered around 0
  # normality is plasuible - slight fat tails
  # Not sure how this affects a fourier model
  # checkresiduals(fit)
  # qqnorm(residuals(fit))
  # qqline(residuals(fit))
  
  pred <- forecast(fit, 
                   data.frame(fourier(yms, K = c(7,6), h=168*60/duration.in.min)),
                   level=c(prediction.interval))
  
  date.range = as.POSIXct(startday) + as.numeric(time(pred$upper))*7*24*60*60
  starts = queuemodel__addTimeColumns(data.table(start=date.range))
  
  reqd = data.table(starts, pred$upper)
  names(reqd) = c("start", "weekday", "ts", "group", "s")
  
  reqd = reqd[,.(group,ts,s)]
  
  
  return(reqd)
}
