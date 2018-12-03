# Name: forecast_targets.R
# Desc: Try different methods to predict ST and AR 
# Author: Matt Whiteside
# Date: Nov 27, 2018


trendsTarget <- function(ar, st) {
  
  
  ems911 = st %>% filter(grepl('911', Stream))
  
  fit_exp = fitdist(ems911$interval_length/60, distr="exp", method="mle")
  fit_ln = fitdist(ems911$interval_length/60, distr="lnorm", method="mle")
  fit_gamma = fitdist(ems911$interval_length/60, distr="gamma", method="mle")
  
  par(mfrow=c(2,2))
  plot.legend <- c("exponential", "lognormal", "gamma")
  denscomp(list(fit_exp, fit_ln, fit_gamma), legendtext = plot.legend)
  cdfcomp (list(fit_exp, fit_ln, fit_gamma), legendtext = plot.legend)
  qqcomp  (list(fit_exp, fit_ln, fit_gamma), legendtext = plot.legend)
  ppcomp  (list(fit_exp, fit_ln, fit_gamma), legendtext = plot.legend)
  
  gofstat(list(fit_exp, fit_ln, fit_gamma), fitnames = plot.legend)
  
  ems911.transported = st %>% filter(grepl('911-T', Stream))
  
  fit_exp = fitdist(ems911.transported$interval_length/60, distr="exp", method="mle")
  fit_ln = fitdist(ems911.transported$interval_length/60, distr="lnorm", method="mle")
  fit_gamma = fitdist(ems911.transported$interval_length/60, distr="gamma", method="mle")
  
  par(mfrow=c(2,2))
  plot.legend <- c("exponential", "lognormal", "gamma")
  denscomp(list(fit_exp, fit_ln, fit_gamma), legendtext = plot.legend)
  cdfcomp (list(fit_exp, fit_ln, fit_gamma), legendtext = plot.legend)
  qqcomp  (list(fit_exp, fit_ln, fit_gamma), legendtext = plot.legend)
  ppcomp  (list(fit_exp, fit_ln, fit_gamma), legendtext = plot.legend)
  
  gofstat(list(fit_exp, fit_ln, fit_gamma), fitnames = plot.legend)
  
  ems911.notransport = st %>% filter(grepl('911-N', Stream))
  
  fit_exp = fitdist(ems911.notransport$interval_length/60, distr="exp", method="mle")
  fit_ln = fitdist(ems911.notransport$interval_length/60, distr="lnorm", method="mle")
  fit_gamma = fitdist(ems911.notransport$interval_length/60, distr="gamma", method="mle")
  
  par(mfrow=c(2,2))
  plot.legend <- c("exponential", "lognormal", "gamma")
  denscomp(list(fit_exp, fit_ln, fit_gamma), legendtext = plot.legend)
  cdfcomp (list(fit_exp, fit_ln, fit_gamma), legendtext = plot.legend)
  qqcomp  (list(fit_exp, fit_ln, fit_gamma), legendtext = plot.legend)
  ppcomp  (list(fit_exp, fit_ln, fit_gamma), legendtext = plot.legend)
  
  gofstat(list(fit_exp, fit_ln, fit_gamma), fitnames = plot.legend)
  
  fit_exp = fitdist(ar$arrival_rate, distr="exp", method="mle")
  fit_ln = fitdist(ar$arrival_rate, distr="lnorm", method="mle")
  fit_gamma = fitdist(ar$arrival_rate, distr="gamma", method="mle")
  
  par(mfrow=c(2,2))
  plot.legend <- c("exponential", "lognormal", "gamma")
  denscomp(list(fit_exp, fit_ln, fit_gamma), legendtext = plot.legend)
  cdfcomp (list(fit_exp, fit_ln, fit_gamma), legendtext = plot.legend)
  qqcomp  (list(fit_exp, fit_ln, fit_gamma), legendtext = plot.legend)
  ppcomp  (list(fit_exp, fit_ln, fit_gamma), legendtext = plot.legend)
  
  gofstat(list(fit_exp, fit_ln, fit_gamma), fitnames = plot.legend)
  
  
  ggplot(ems911, aes(x=interval_length)) + 
    labs(x = "", y = "")+ 
    ggtitle("Exponential Distribution") +
    geom_histogram(aes(y=..density..), binwidth=60, colour="black", fill="white") + 
    geom_vline(aes(xintercept=1/fit1$estimate), color="blue", linetype="solid", size=1) +
    stat_function(fun=dexp, geom="line", size=1, col="red", args=(mean=fit1$estimate))
  
  ggplot(ems911, aes(x=interval_length)) + 
    stat_function(fun=dexp, geom="line", size=1, col="red", args=(mean=fit1$estimate))
  
 
  ems911 %>% ggplot(aes(x=interval_length)) +
    geom_histogram(binwidth = 60)
  
  st.freq = as.tibble(table(sort(ems911$interval_length%/%60)))
  st.freq %<>% rename(service.time=Var1,frequency=n) %<>% mutate(service.time = as.integer(service.time))
  
  
  # Compare daily trends for AR and ST
  st.tbl = st %>% mutate(weekday=wday(start, label=T), hour=sprintf("%02d:00", hour(start)))
  
  
  
  # Weekly totals for the year with a monthly smoothing line
  # plotLongTermRollingTrend(ar %>% filter(Stream == "911-Transported"), "arrival_rate")
  
  
 
  return(rt)
}

serviceTimeDistribution <- function(st) {
  
}


dayAnalysis <- function(rt, d="Wed") {
  # Are individual hour service times exponential or something else?
  daily = rt %>% filter(weekday == d)
  
  hourly = daily %>% filter(hour == "00:00")
  
}

plotLongTermRollingTrend <- function(rt, ylab="Arrival Rate", b=168) {
  
  # Aggregate and smooth data to make it easier to visualize
  agg.rt = rollapply(rt$arrival_rate, b, by=b, FUN = sum, align="right")
  agg.ts = rt[seq(b,nrow(rt),by=b),"start"]
  d = agg.ts %>% add_column(arrival_rate=agg.rt)
  
  ma = d %>% mutate(
    monthly_median = rollmedian(arrival_rate, k=5, fill=NA)
  )
  ma %>% gather(metric, value, arrival_rate:monthly_median) %>%
    ggplot(aes(start, value, linetype=metric)) +
    geom_line()+
    scale_color_grey()+
    theme(panel.border = element_blank(),
          panel.background = element_blank(),
          panel.grid.minor = element_line(colour = "grey90"),
          panel.grid.major = element_line(colour = "grey90"),
          panel.grid.major.x = element_line(colour = "grey90"),
          axis.text = element_text(size = 10),
          axis.title = element_text(size = 12, face = "bold"),
          strip.text = element_text(size = 12, face = "bold")) +
    labs(x = "Date", y = "Weekly Arrival Rate")
}

plotMAD <- function(rt, val, ylab="Weekly Arrival Rate") {
  
}
