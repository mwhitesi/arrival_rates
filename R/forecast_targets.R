# Name: forecast_targets.R
# Desc: Estimate required crews for each time period using Queueing models
# Author: Matt Whiteside
# Date: Nov 27, 2018


trendsTarget <- function(ar, st) {
  # Compare daily trends to identify similar days than can use the same shifts
  
  st.911 = st
  ar.911 = ar
  
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
  
  ggsave(paste0("data/interim/plot_service_time_distribution_pvalues.pdf"), p.stpvals, dev="pdf", height=8.5, width=11)
  
  
  ar.911 = ar.911 %>% mutate(weekday = factor(wday(start), levels=1:7)) %>% mutate(hour = sprintf('%02d:00', hour(start)))
  
  p.ar = ar.911 %>% group_by(weekday,hour) %>% dplyr::summarise(mean=mean(arrival_rate)) %>%
    ggplot(aes(x=hour,y=mean,group=weekday,color=weekday)) +
    geom_line() +
    ylab('Average Arrival Rate') +
    xlab('Hour of Day') +
    theme(panel.border = element_blank(),
      panel.background = element_blank(),
      panel.grid.minor = element_line(colour = "grey90"),
      panel.grid.major = element_line(colour = "grey90"),
      panel.grid.major.x = element_line(colour = "grey90"),
      axis.text = element_text(size = 10),
      axis.text.x = element_text(angle = 45, hjust = 1, size=10),
      axis.title = element_text(size = 12, face = "bold"),
      strip.text = element_text(size = 12, face = "bold"))
  
  ggsave(paste0("data/interim/plot_daily_arrival_rate_trends.pdf"), p.ar, dev="pdf", height=8.5, width=11)
  
  
  # Weekly totals for the year with a monthly smoothing line
  # plotLongTermRollingTrend(ar.911, "arrival_rate")
  
}


modelTarget <- function(ar, st) {
  
  
  # Breakdown of weekly data into similar periods
  # Weekend/Weekday
  weekend <- c(1,7)
  
  st.911 = st
  ar.911 = ar
  
  st.911 = st.911 %>% mutate(weekday = wday(start)) %>% mutate(group=ifelse(match(weekday, weekend, 0L), "weekend", "weekday")) %>% mutate(hour=hour(start))
  ar.911 = ar.911 %>% mutate(weekday = wday(start)) %>% mutate(group=ifelse(match(weekday, weekend, 0L), "weekend", "weekday")) %>% mutate(hour=hour(start))
  
  # Talking hours here
  st.911 = st.911 %>% mutate(service.time=interval_length/60/60)
  
  # Aggregate
  ar.values = ar.911 %>% group_by(group,hour) %>% summarise(values=list(arrival_rate))
  st.values = st.911 %>% group_by(group,hour) %>% summarise(values=list(service.time))
  
  # Fit a model
  groups = st.values %>% distinct(group) %>% arrange(group) %>% pull(group)
  hours = st.values %>% distinct(hour) %>% arrange(hour) %>% pull(hour)
  
  stopifnot(length(hours) == 24)
  stopifnot(nrow(ar.values %>% distinct(hour)) == 24)
  
  required.servers = as.tibble(expand.grid(groups,hours,0))
  names(required.servers) = c('group','hour','s')
  
  for(g in groups) {
    for(h in hours) {
      prev_h = ifelse((h-1)>0, h-1, max(hours))
      stlist = unlist(st.values %>% filter(group == g & hour == h) %>% select('values')) 
      arlist = unlist(ar.values %>% filter(group == g & hour == h) %>% select('values'))
      stlist.prev = unlist(st.values %>% filter(group == g & hour == prev_h) %>% select('values')) 
      arlist.prev = unlist(ar.values %>% filter(group == g & hour == prev_h) %>% select('values'))
      est.servers = fitQueueModel(stlist, arlist, stlist.prev, arlist.prev, g, h, TRUE)$servers
      
      print(sprintf("Group: %s, hour: %i, servers: %i", g, h, est.servers))
      
      required.servers %<>% mutate(s = replace(s, group == g & hour == h, est.servers))
    }
  }
  
 p = required.servers %>% mutate(hour2 = sprintf('%02d:00', hour)) %>%
   ggplot(aes(x=hour2, y=s, group=group)) +
     facet_wrap(. ~ group) +
     geom_line() +
     ylab('Estimated Number of Units') +
     xlab('Hour of Day') +
     theme(panel.border = element_blank(),
           panel.background = element_blank(),
           panel.grid.minor = element_line(colour = "grey90"),
           panel.grid.major = element_line(colour = "grey90"),
           panel.grid.major.x = element_line(colour = "grey90"),
           axis.text = element_text(size = 10),
           axis.text.x = element_text(angle = 45, hjust = 1, size=10),
           axis.title = element_text(size = 12, face = "bold"),
           strip.text = element_text(size = 12, face = "bold"))
 
  ggsave("data/interim/plot_required_crews.pdf", p, height=8.5, width=11)
     
    
  return(required.servers)
  
}

fitQueueModel <- function(st.list, ar.list, st.list.prev, ar.list.prev, group.label, hour, do.plots=TRUE) {
  
  path = file.path('data/interim', group.label)
  if(do.plots) {
    
    dir.create(path, showWarnings = F)
    
    # Check service distribution
    fit_exp = fitdist(st.list, distr="exp", method="mle")
    fit_ln = fitdist(st.list, distr="lnorm", method="mle")
    fit_gamma = fitdist(st.list, distr="gamma", method="mle")
    

    # squared coefficient of variation for service time should be < 2, for exponential distribution to be appropriate
    # See Green et al, Coping with Time-Varying Demand When Setting Staffing Requirements for a Service System, 2007
    st.sd = var(st.list)
    st.mean = mean(st.list)
    scv.service = st.sd/st.mean^2
    scv.result = sprintf("Squared coefficent of variation is indicator\nof suitability of exp distn for service time\n Is SCV less than 2?\n\t%s (%f)", 
                         ifelse(scv.service, "Yes", "No"), scv.service)
    gof.result = capture.output(print(gofstat(list(fit_exp, fit_ln, fit_gamma))))
    
    pdf(file.path(path,paste0('plots_service_time_gof__hour',hour,'.pdf')),width=11.5,height=8)
    par(mfrow=c(2,3))
    plot.legend <- c("exponential", "lognormal", "gamma")
    denscomp(list(fit_exp, fit_ln, fit_gamma), legendtext = plot.legend)
    cdfcomp (list(fit_exp, fit_ln, fit_gamma), legendtext = plot.legend)
    qqcomp  (list(fit_exp, fit_ln, fit_gamma), legendtext = plot.legend)
    ppcomp  (list(fit_exp, fit_ln, fit_gamma), legendtext = plot.legend)
    plot(NA,NA,axes=F,xlim=c(0,10),ylim=c(0,10),xlab="",ylab="")
    text(5,5,labels=scv.result)
    plot(NA,NA,axes=F,xlim=c(0,10),ylim=c(0,10),xlab="",ylab="")
    text(4,1,labels=paste(gof.result,collapse="\n"), pos=3, cex=0.8)
    dev.off()
  }
  
  ar.res = rate.summary(ar.list)
  
  # Populate model
  lambda = ar.res$mean
  lambda_prev = rate.summary(ar.list.prev)$mean
  mu = 1/st.mean
  
  rho = lambda/mu
  
  # Adjust arrival rate? Service time?
  
  # Previous research indicated a difference of 20% would impact the current state performance
  # Lots of different approaches for adjusting arrival rates to account for preexisting load in the queue
  # LAG - use arrival rate for period shifted back by L units (average service time)
  # Mix - In periods where rates are increasing, use existing rate, otherwise, use 1.2 average (avoids understaffing)
  # Adaptive - If difference is > 20% between preceding and current, take average
  
  perc.change = (lambda_prev - lambda)/lambda
  if(abs(perc.change) > .2) {
    warning(sprintf("Significant change in arrival rate. Current Rate: %f, previous period rate: %f (%f change)", lambda, lambda_prev,
                    perc.change))
  }
  
  # Search space
  # Lower bound
  s.lowerbound = round(rho/.9) # Make sure utilization proportion is less than 1 to make this work
  
  pw = 1
  service.level = 0.001
  s=s.lowerbound
  pw = erlang_c_wait_probability(s,rho)
  while(pw > service.level) {
    s=s+1
    pw = erlang_c_wait_probability(s,rho)
  }
  
  
  if(do.plots) {
    
    # Plot queue performance for different number of servers
    mmcs = tibble(s=s.lowerbound:(s+5), lambda=lambda, mu=mu)
    mmcs %<>% rowwise() %>% do(queue.performance(s=.$s, lambda=.$lambda, mu=.$mu))
    
    plot.width=30
    
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
      ggtitle(wrapper('Probability No Unit Available (Erlang C model)', plot.width)) +
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
      ggtitle(wrapper('Estimated Event Time', plot.width)) +
      ylab('Hours') +
      xlab('Number of Units Staffed')
    
    p.Wq = mmcs %>% 
      ggplot(aes(x=s,y=Wq)) +
      geom_line() +
      theme(panel.border = element_blank(),
            panel.background = element_blank(),
            panel.grid.minor = element_line(colour = "grey90"),
            panel.grid.major = element_line(colour = "grey90"),
            panel.grid.major.x = element_line(colour = "grey90"),
            axis.text = element_text(size = 10),
            axis.title = element_text(size = 12, face = "bold"),
            strip.text = element_text(size = 12, face = "bold")) +
      ggtitle(wrapper('Estimated Time Waiting for Available Unit', plot.width)) +
      ylab('Hours') +
      xlab('Number of Units Staffed')
    
    p.L = mmcs %>% 
      ggplot(aes(x=s,y=L)) +
      geom_line() +
      theme(panel.border = element_blank(),
            panel.background = element_blank(),
            panel.grid.minor = element_line(colour = "grey90"),
            panel.grid.major = element_line(colour = "grey90"),
            panel.grid.major.x = element_line(colour = "grey90"),
            axis.text = element_text(size = 10),
            axis.title = element_text(size = 12, face = "bold"),
            strip.text = element_text(size = 12, face = "bold")) +
      ggtitle(wrapper('Estimated Number of Ongoing Events', plot.width)) +
      ylab('Number of Events') +
      xlab('Number of Units Staffed')
    
    p.Lq = mmcs %>% 
      ggplot(aes(x=s,y=Lq)) +
      geom_line() +
      theme(panel.border = element_blank(),
            panel.background = element_blank(),
            panel.grid.minor = element_line(colour = "grey90"),
            panel.grid.major = element_line(colour = "grey90"),
            panel.grid.major.x = element_line(colour = "grey90"),
            axis.text = element_text(size = 10),
            axis.title = element_text(size = 12, face = "bold"),
            strip.text = element_text(size = 12, face = "bold")) +
      ggtitle(wrapper('Estimated Number of Events Waiting for Available Unit', plot.width)) +
      ylab('Number of Events') +
      xlab('Number of Units Staffed')
    
    p.full = grid.arrange(p.prob, p.W, p.Wq, p.L, p.Lq, ncol=3)
    ggsave(file.path(path,paste0('plot_queue_model_performance__hour',hour,'.pdf')), plot=p.full, device='pdf', width=11, height=8.5)
    
  }
  
  return(list(servers=s))
}

wrapper <- function(x, ...) {
  paste(strwrap(x, ...), collapse = "\n")
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


