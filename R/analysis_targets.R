# Name: analysis_targets.R
# Desc: Compute hourly rates. Plot figures
# Author: Matt Whiteside
# Date: Nov 20, 2018


arrivalSummaryTarget <- function(ar, file.out) {
  
  rt = ar %>% group_by(weekday=wday(start, label=T), hour=sprintf("%02d:00", hour(start))) %>% 
    summarise(n=sum(arrival_rate), 
              mean=mean(arrival_rate),
              sd=sd(arrival_rate),
              p50=quantile(arrival_rate,.5),
              p25=quantile(arrival_rate,.25),
              p75=quantile(arrival_rate,.75)
              )
  rt %>% write.csv(file = file.out)
  
  return(rt)
}

serviceTimeSummaryTarget <- function(st, file.out) {
  rt = st %>% mutate(interval_length = interval_length/60) %>% group_by(weekday=wday(start, label=T), hour=sprintf("%02d:00", hour(start))) %>% 
    summarise(n=sum(interval_length), 
              mean=mean(interval_length),
              sd=sd(interval_length),
              p50=quantile(interval_length,.5),
              p25=quantile(interval_length,.25),
              p75=quantile(interval_length,.75)
    )
  rt %>% write.csv(file = file.out)
  
  return(rt)
}