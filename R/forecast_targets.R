# Name: forecast_targets.R
# Desc: Try different methods to predict ST and AR 
# Author: Matt Whiteside
# Date: Nov 27, 2018


trendsTarget <- function(ar, st) {
  
  
  st.911 = st %>% filter(grepl('911', Stream))
  ar.911 = ar %>% filter(grepl('911', Stream))
  
  # Compare days
  st.daily = st.911 %>% mutate(weekday=wday(start,label=T)) %>% group_by(weekday) %>% summarise(values=list(interval_length/60))
  st.showdown = st.daily %>% expand(weekday, weekday) %>% dplyr::inner_join(st.daily, by="weekday") %>% 
    dplyr::inner_join(st.daily,by = c("weekday1" = "weekday")) %>% filter(weekday != weekday1)
  
  st.showdown = st.showdown %>% rowwise() %>% mutate(ks=ks.test(values.x, values.y)$p.value) %>% 
    mutate(kolmogorov.smirnov=ks.test(values.x, values.y)$p.value) %>%
    mutate(t.test=t.test(values.x, values.y)$p.value) %>%
    mutate(mann.whitney=wilcox.test(values.x, values.y)$p.value)
  
  # Display p-values into heatmap to visualize
  p.stpvals = st.showdown %>% gather(metric, value, kolmogorov.smirnov:mann.whitney) %>%
    ggplot(aes(weekday, weekday1, fill = value)) +
    geom_tile() +
    scale_fill_gradient2(low = "blue", mid = "white", high = "red",
                         midpoint = 0.04, space = "Lab", na.value = "grey50", guide = "colourbar") + 
    facet_wrap(. ~ metric) +
    theme(panel.border = element_blank(),
          panel.background = element_blank(),
          panel.grid.minor = element_line(colour = "grey90"),
          panel.grid.major = element_line(colour = "grey90"),
          panel.grid.major.x = element_line(colour = "grey90"),
          axis.text = element_text(size = 10),
          axis.title = element_text(size = 12, face = "bold"),
          strip.text = element_text(size = 12, face = "bold")) +
    ggtitle('P-values Comparing Service Time Distributions')
  
  ggsave(paste0("data/interim/plot_service_time_distribution_pvalues.pdf"), p.stpvals, dev="pdf")
  
  
  # Weekend 
  st.daily = st.911 %>% mutate(weekday=wday(start,label=T)) %>% group_by(weekday) %>% summarise(values=list(interval_length/60))
  st.showdown = st.daily %>% expand(weekday, weekday) %>% dplyr::inner_join(st.daily, by="weekday") %>% 
    dplyr::inner_join(st.daily,by = c("weekday1" = "weekday")) %>% filter(weekday != weekday1)
  
    
  
  
  
  # Fit distributions
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


modelTarget <- function(ar, st) {
  
  
  # Time periods
  
  
  weekend <- c(6,0)
  
  # Use appropriate streams
  st.911 = st %>% filter(grepl('911', Stream))
  ar.911 = ar %>% filter(grepl('911', Stream))
  
  # Combine transport/non-transport AR 
  ar.911 = ar.911 %>% group_by(start) %>% summarise(arrival_rate=sum(arrival_rate))
  
  st.911 = st.911 %>% mutate(weekday = wday(start)) %>% mutate(group=ifelse(match(weekday, weekend, 0L), "weekend", "weekday"))
  ar.911 = ar.911 %>% mutate(weekday = wday(start)) %>% mutate(group=ifelse(match(weekday, weekend, 0L), "weekend", "weekday"))
  
  # Talking hours here
  st.911 = st.911 %>% mutate(service.time=interval_length/60/60)
  
  # Aggregate
  ar.values = ar.911 %>% mutate(weekday = wday(start)) %>% mutate(group=ifelse(match(weekday, weekend, 0L), "weekend", "weekday")) %>%
    mutate(hour=hour(start)) %>% group_by(group,hour) %>% summarise(values=list(arrival_rate))
  
  st.values = st.911 %>% mutate(weekday = wday(start)) %>% mutate(group=ifelse(match(weekday, weekend, 0L), "weekend", "weekday")) %>%
    mutate(hour=hour(start)) %>% group_by(group,hour) %>% summarise(values=list(service.time))
  
  # Fit a model
  
  
}

fitQueueModel <- function(st.list, ar.list, st.list.prev, ar.list.prev) {
  
  # Check service distribution
  fit_exp = fitdist(st.list, distr="exp", method="mle")
  fit_ln = fitdist(st.list, distr="lnorm", method="mle")
  fit_gamma = fitdist(st.list, distr="gamma", method="mle")
  
  par(mfrow=c(2,2))
  plot.legend <- c("exponential", "lognormal", "gamma")
  denscomp(list(fit_exp, fit_ln, fit_gamma), legendtext = plot.legend)
  cdfcomp (list(fit_exp, fit_ln, fit_gamma), legendtext = plot.legend)
  qqcomp  (list(fit_exp, fit_ln, fit_gamma), legendtext = plot.legend)
  ppcomp  (list(fit_exp, fit_ln, fit_gamma), legendtext = plot.legend)
  
  gofstat(list(fit_exp, fit_ln, fit_gamma), fitnames = plot.legend)
  
  # squared coefficient of variation for service time should be < 2, for exponential distribution to be appropriate
  # See Green et al, Coping with Time-Varying Demand When Setting Staffing Requirements for a Service System, 2007
  st.sd = var(st.list)
  st.mean = mean(st.list)
  scv.service = st.sd/st.mean^2
  
  # Adjust arrival rate? Service time?
  ar.res = rate.summary(ar.list)
  
  # Populate model
  lambda = ar.res$mean
  mu = 1/st.mean
  
  rho = lambda/mu
  
  
  # Search space
  # Lower bound
  s.lowerbound = round(rho/.9) # Make sure utilization proportion is less than 1 to make this work
  
  pw = 1
  service.level = 0.0001
  s=s.lowerbound
  pw = erlang_c_wait_probability(s,rho)
  while(pw > service.level) {
    s=s+1
    pw = erlang_c_wait_probability(s,rho)
  }
  
  # Plot queue performance for different number of servers
  mmcs = tibble(s=s.lowerbound:(s+5), lambda=lambda, mu=mu)
  mmcs %<>% rowwise() %>% do(queue.performance(s=.$s, lambda=.$lambda, mu=.$mu))
  
  p.prob = mmcs %>% 
    ggplot(aes(x=s,y=C)) +
    geom_line() +
    theme(panel.border = element_blank(),
          panel.background = element_blank(),
          panel.grid.minor = element_line(colour = "grey90"),
          panel.grid.major = element_line(colour = "grey90"),
          panel.grid.major.x = element_line(colour = "grey90"),
          axis.text = element_text(size = 10),
          axis.title = element_text(size = 12, face = "bold"),
          strip.text = element_text(size = 12, face = "bold")) +
    ggtitle('Probability No Unit Available (Erlang C model)') +
    ylab('Probability') +
    xlab('Number of Units Staffed')
  
  p.W = mmcs %>% 
    ggplot(aes(x=s,y=W)) +
    geom_line() +
    theme(panel.border = element_blank(),
          panel.background = element_blank(),
          panel.grid.minor = element_line(colour = "grey90"),
          panel.grid.major = element_line(colour = "grey90"),
          panel.grid.major.x = element_line(colour = "grey90"),
          axis.text = element_text(size = 10),
          axis.title = element_text(size = 12, face = "bold"),
          strip.text = element_text(size = 12, face = "bold")) +
    ggtitle('Estimated Time in System') +
    ylab('Hours') +
    xlab('Number of Units Staffed')
    
  
  
}

erlang_b_wait_probability <- function(s,rho) {
  # Probability of delay when new event arrives
  sigma = sum(sapply(0:s, function(i) rho^i/factorial(i)))
  B = (rho^s)/factorial(s)
  
  prob.wait = B / sigma
  
  return(prob.wait)
}


erlang_c_wait_probability <- function(s,rho) {
  # Probability of delay when new event arrives
  sigma = sum(sapply(0:(s-1), function(i) rho^i/factorial(i)))
  C = (rho^s)/(factorial(s-1)*(s-rho))
  
  prob.wait = C / (sigma + C)
  
  return(prob.wait)
}

queue.performance <- function(s,lambda,mu) {
  params.mmc = NewInput.MMC(lambda = lambda, mu = mu , n=1, method = 0 , c=s)
  mmc=QueueingModel(params.mmc)
  
  C=erlang_c_wait_probability(s,lambda/mu)
  B=erlang_b_wait_probability(s, lambda/mu)
 
  return(data.frame(s=s, C=C, B=B, Lq=mmc$Lq, L=mmc$L, W=mmc$W, Wq=mmc$Wq, P0=mmc$Pn[1]))
}

rate.summary <- function(l) {
  return(list("mean"=mean(l), "sd"=sd(l), 
              "p50"=quantile(l,.5), "p90"=quantile(l,.9)))
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


