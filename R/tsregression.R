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
  
  reqd = reqd %>% mutate(tp=as.integer(as.factor(reqd$ts)), s=ceiling(s)) %>% arrange(group, tp)
  
  return(reqd)
}

tsregression__fitFFTModel_threshold <- function(hd, duration.in.min=15, weekly.min=252000, use.box.cox=TRUE, do.plot=TRUE) {
  
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
  
  lambda = BoxCox.lambda(d2[,count])
  yms.bc = msts(BoxCox(d2[,count], seasonal.periods = c(24*hourly, 168*hourly))
  
  # Parameters originally selected using AICc
  fit <- tslm(yms ~ fourier(yms, K = c(7,6)))
  
  # Predict a current estimate and adjust 
  pi=.9
  pred <- forecast(fit, 
                   data.frame(fourier(yms, K = c(7,6), h=168*60/duration.in.min)),
                   level=c(pi))
  
  # Mean scheduled unit mins
  lambda = 0.777
  mean.mins = sum(pred$mean * duration.in.min)
  
  # Compute the standard error portion of the prediction interval
  np = length(pred$upper)
  tval = qt((1 - pi)/2, fit$df.residual)
  se = BoxCox(pred$upper, lambda) - BoxCox(pred$mean, lambda)/tval
  
  BoxCox(pred$mean, lambda) - tval*se
  
  prop = BoxCox(weekly.mins-mean.mins, lambda)/sum(se)
  
  quant = pt(prob, fit$df.residual)
  
  lev = 1 - quant*2
  
  date.range = as.POSIXct(startday) + as.numeric(time(pred$upper))*7*24*60*60
  starts = queuemodel__addTimeColumns(data.table(start=date.range))
  
  reqd = data.table(starts, pred$upper)
  names(reqd) = c("start", "weekday", "ts", "group", "s")
  
  reqd = reqd[,.(group,ts,s)]
  
  reqd = reqd %>% mutate(tp=as.integer(as.factor(reqd$ts)), s=ceiling(s)) %>% arrange(group, tp)
  
  return(reqd)
}

tsregression__zeroin_fit <- function()

tsregression__FFTModelAICc <- function(hd, duration.in.min=15, prediction.interval=.995, do.plot=TRUE) {
  
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
  
  bestfit <- list(aicc=Inf)
  for(i in 5:7) {
    for(j in 5:7) {
      fit <- auto.arima(yms, xreg=fourier(yms, K=c(i, j)), seasonal=FALSE, parallel=TRUE, num.cores=8, stepwise=FALSE)
      if(fit$aicc < bestfit$aicc)
        bestfit <- fit
      else break;
      print(sprintf("i: %i, j: %i AICc: %f", i, j, fit$aicc))
    }
  }
  
  
  
  # Lots of uncaptured patterns/correlation in residuals, but at least residuals are centered around 0
  # normality is plasuible - slight fat tails
  # Not sure how this affects a fourier model
  # checkresiduals(fit)
  # qqnorm(residuals(fit))
  # qqline(residuals(fit))
  
  pred <- forecast(bestfit, 
                   data.frame(fourier(yms, K = c(i,j), h=168*60/duration.in.min)),
                   level=c(prediction.interval))
  
  date.range = as.POSIXct(startday) + as.numeric(time(pred$upper))*7*24*60*60
  starts = queuemodel__addTimeColumns(data.table(start=date.range))
  
  reqd = data.table(starts, pred$upper)
  names(reqd) = c("start", "weekday", "ts", "group", "s")
  
  reqd = reqd[,.(group,ts,s)]
  
  reqd = reqd %>% mutate(tp=as.integer(as.factor(reqd$ts)), s=ceiling(s)) %>% arrange(group, tp)
  
  return(list(model=bestfit, estimated=reqd))
}
