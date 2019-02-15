# Name: queuemodel.R
# Desc: Compute hourly rates. Plot figures
# Author: Matt Whiteside
# Date: Nov 20, 2018


queuemodel__addTimeColumns <- function(tbl) {
  tmp = tbl %>% mutate(weekday=wday(start, label=T), ts=strftime(start, format="%H:%M", tz="UTC"),
                       wd=wday(start, label=F))
  
  return(tmp)
}

queuemodel__convertToTbl <- function(ts) {
  cols <- c("Event.Number", "start", "interval_length", "Event.Datetime")
  events_tbl = ts[,mget(cols)] %>% as_tibble() %>% mutate(start = as_datetime(start)) %>% 
    as_tbl_time(start) %>% arrange(start)
  
  return(events_tbl)
}

queuemodel__computeArrivalRates <- function(tbl, start.date) {
  ar_tbl = tbl %>% collapse_by("15 minutes", start_date=start.date, clean=T, side="start") %>% 
    group_by(start) %>% summarise('arrival_rate'=n())
  return(ar_tbl)
}

queuemodel__computeServiceTime <- function(tbl, start.date) {
  tt_tbl = tbl %>% collapse_by("15 minutes", start_date=start.date, clean=T, side="start")
  return(tt_tbl)
}

queuemodel__findAnomalies <- function(tbl, col) {
  # Find outliers using sliding window on time series data
  # Adds columns to tbl
  anomly_tbl <- tbl %>%
    time_decompose(col, 
                   frequency='7 days',
                   trend='3 months',
                   merge = TRUE) %>%
    anomalize(remainder) %>%
    time_recompose()
  
  return(anomly_tbl)
}

queuemodel__findOutliers <- function(dt, metric, start.date='2017-01-01') {
  
  tbl <- queuemodel__convertToTbl(dt)
  
  if(metric == "arrival rates") {
    rt <- queuemodel__computeArrivalRates(tbl, start.date)
    label <- 'arrival_rates'
    colnm <- 'arrival_rate'
  } else if(metric == "service time") {
    rt <- queuemodel__computeServiceTime(tbl, start.date)
    label <- 'service_time'
    colnm <- 'interval_length'
  } else {
    stop(paste('Unknown metric:',metric))
  }
  
  anoms <- queuemodel__findAnomalies(rt, colnm)
  
  # Plot anomalies
  p.decomp = anoms %>% plot_anomalies()
  ggsave(paste0("data/interim/queuemodel/plot_",label,"__anomalies.pdf"), p.decomp, dev="pdf")
  
  # Filter out anomalies
  final = anoms %>% filter(anomaly == "No") %>% select(start, colnm)
  
  
  return(final)
}

queuemodel__arrivalSummary <- function(ar, file.out) {
  
  rt = ar %>% group_by(weekday, ts) %>% 
    summarise(n=sum(arrival_rate), 
              mean=mean(arrival_rate),
              sd=sd(arrival_rate),
              p50=quantile(arrival_rate,.5),
              p25=quantile(arrival_rate,.25),
              p75=quantile(arrival_rate,.75)
              )
  rt %>% write.csv(file = file.out)
  
  queuemodel__makeHistogram(ar, 'arrival_rate', "arrival_rate", "Arrival Rate (# events per 15 min)", "Arrival Rate", 1)
  
  queuemodel__makePlots(ar, rt, 'arrival_rate', "arrival_rate", "Arrival Rate (# events per 15 min)", "Arrival Rate")
  
  return(rt)
}

queuemodel__makeHistogram <-function(tbl, val, filename, xl, hist.title, bw=5) {
  p = tbl %>% 
    ggplot(aes_string(x=val)) + 
    geom_histogram(binwidth=bw) +
    xlab(xl) +
    ggtitle(paste(hist.title, "for Edmonton Events November 2017 - 2018"))
  
  ggsave(paste0("data/interim/queuemodel/",filename,"__histogram.pdf"), p, dev="pdf")
}

queuemodel__makePlots <- function(tbl, rt, val, filename, y.label, plot.title) {
  
  for(d in wday(1:7, label=TRUE)) {
    
    p1 = rt %>% filter(weekday == d) %>% 
      ggplot(aes(y = p50, x = factor(ts))) +
        geom_line() +
        geom_point(size=2) +
        geom_errorbar(aes(ymax=p75,ymin=p25),width=.5, lwd=.4) +
        theme(
          text = element_text(size=10),
          axis.text.x = element_text(angle = 45, hjust = 1, size=6))+
        ylab(y.label) +
        xlab("Time Period") +
        theme(panel.border = element_blank(),
              panel.background = element_blank(),
              panel.grid.minor = element_line(colour = "grey90"),
              panel.grid.major = element_line(colour = "grey90"),
              panel.grid.major.x = element_line(colour = "grey90")) +
        ggtitle(paste(plot.title, "for Edmonton Events November 2017 - 2018"))
    
    ggsave(paste0("data/interim/queuemodel/",filename,"_",d,"__lineplot.pdf"), p1, dev="pdf", width=11, height=8.5)
    
  }
}

queuemodel__serviceTimeSummary <- function(st, file.out) {
  
  tmp = st %>% mutate(interval_length = interval_length/60)
  rt = tmp %>% group_by(weekday, ts) %>% 
    summarise(n=sum(interval_length), 
              mean=mean(interval_length),
              sd=sd(interval_length),
              p50=quantile(interval_length,.5),
              p25=quantile(interval_length,.25),
              p75=quantile(interval_length,.75)
    )
  rt %>% write.csv(file = file.out)
  
  queuemodel__makeHistogram(tmp, 'interval_length', "service_time", "Service Time (minutes)", "Service Time", 10)
  
  queuemodel__makePlots(tmp, rt, 'interval_length', "service_time", "Service Time (minutes)", "Service Time")
  
  return(rt)
}

queuemodel__correlationSummary <- function(st_rates, ar_rates) {
  rates = inner_join(ar_rates, st_rates, by=c('weekday', 'ts'), suffix=c('.ar', '.st'))
  
  p5 = ggplot(data=rates, aes(x=p50.ar, y=p50.st)) +
    geom_point(size=1) +
    geom_smooth(method = "lm") +
    xlab("Median Arrival Rate") +
    ylab("Median Time on Task") +
    ggtitle("Comparison of Time on Task and Number of Events for Each Period\nin a Given Day of Week")
  ggsave("data/interim/queuemodel/time_on_task_vs_arrival_rates_plot.pdf", p5, dev="pdf")
  
  return(rates)
}

queuemodel__trends <- function(ar, st) {
  # Compare daily arrival rates and service times to identify days that have similar demand patterns.
  
  st.911 = st
  ar.911 = ar
  
  # Compare days
  st.daily = st.911 %>% group_by(weekday) %>% summarise(values = list(interval_length/60))
  st.showdown = st.daily %>% expand(weekday, weekday) %>% dplyr::inner_join(st.daily, by="weekday") %>% 
    dplyr::inner_join(st.daily,by = c("weekday1" = "weekday")) %>% filter(weekday != weekday1)
  
  st.showdown = st.showdown %>% rowwise() %>% mutate(ks=ks.test(values.x, values.y)$p.value) %>% 
    mutate(kolmogorov.smirnov = ks.test(values.x, values.y)$p.value) %>%
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
  
  ggsave(paste0("data/interim/queuemodel/plot_service_time_distribution_pvalues.pdf"), p.stpvals, dev="pdf", height=8.5, width=11)
  
  
  p.ar = ar.911 %>% group_by(weekday,ts) %>% dplyr::summarise(mean=mean(arrival_rate)) %>%
    ggplot(aes(x=ts,y=mean,group=weekday,color=weekday)) +
    geom_line() +
    ylab('Average Arrival Rate') +
    xlab('Time of Day') +
    theme(panel.border = element_blank(),
          panel.background = element_blank(),
          panel.grid.minor = element_line(colour = "grey90"),
          panel.grid.major = element_line(colour = "grey90"),
          panel.grid.major.x = element_line(colour = "grey90"),
          axis.text = element_text(size = 10),
          axis.text.x = element_text(angle = 45, hjust = 1, size=10),
          axis.title = element_text(size = 12, face = "bold"),
          strip.text = element_text(size = 12, face = "bold"))
  
  ggsave(paste0("data/interim/queuemodel/plot_daily_arrival_rate_trends.pdf"), p.ar, dev="pdf", height=8.5, width=11)
  
  
  return(st.showdown)
  
}

queuemodel__model <- function(ar, st, duration.in.min=15, service.level=0.001, ar.lag=NULL, st.lag=NULL, load.threshold=0, do.plot=TRUE) {
  
  st.911 = st
  ar.911 = ar
  
  # Convert to minutes
  st.911 = st.911 %>% mutate(service.time=interval_length/60)
  ar.911 = ar.911 %>% mutate(arrival_rate=arrival_rate/duration.in.min)
  
  # Aggregate
  ar.values = ar.911 %>% group_by(wd,ts) %>% summarise(values=list(arrival_rate))
  st.values = st.911 %>% group_by(wd,ts) %>% summarise(values=list(service.time))
  
  # Fit a model
  groups = st.values %>% distinct(wd) %>% arrange(wd) %>% pull(wd)
  hours = st.values %>% distinct(ts) %>% arrange(ts) %>% pull(ts)
  
  stopifnot(nrow(ar.values %>% distinct(ts)) == length(hours))
  
  hlen = length(hours)
  required.servers = as.tibble(expand.grid(groups,hours,0))
  names(required.servers) = c('group','ts','s')
  
  # Averge Lag
  if(is.null(ar.lag)) {
    ar.lag=round(mean(st.911$service.time)/duration.in.min)-1
  }
  if(is.null(st.lag)) {
    st.lag=round(mean(st.911$service.time)/duration.in.min)-1
  }
  
  #lag=1
  
  for(g in groups) {
    for(h in 0:(hlen-1)) {
      
      ar.lagvals = h:(h-ar.lag) %% hlen + 1
      ar.hlabels = hours[ar.lagvals]
      st.lagvals = h:(h-st.lag) %% hlen + 1
      st.hlabels = hours[st.lagvals]
      hval = h+1
      
      stlists = st.values %>% filter(wd == g & ts %in% st.hlabels) %>% select('values')
      arlists = ar.values %>% filter(wd == g & ts %in% ar.hlabels) %>% select('values')
      est.servers = queuemodel__fitQueueModel(stlists, arlists, g, hval, load.threshold, service.level, do.plot)$servers
      
      print(sprintf("Group: %s, ts: %i, servers: %i", g, hval, est.servers))
      
      required.servers %<>% mutate(s = replace(s, group == g & ts == ar.hlabels[1], est.servers))
    }
  }
  
  required.servers = required.servers %>% mutate(tp=as.integer(ts)) %>% arrange(group, tp)
  
  if(do.plot) {
    tl = apply(required.servers, 1, 
               function(r) {
                 wd=wday(as.integer(r['group']), label=T)
                 return(sprintf('%s %s', wd, r['ts']))
               })
    breakpoints = seq(1,length(tl),by=12)
    p = required.servers %>% mutate(i=row_number()) %>% 
      ggplot(aes(x=i, y=s)) +
      geom_line() +
      ylab('Estimated Number of Units') +
      xlab('Time Period') +
      scale_x_continuous(labels=tl[breakpoints], breaks=breakpoints) +
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
  }
  
  
  return(required.servers)
  
}


queuemodel__fitQueueModel <- function(st.lists, ar.lists, group.label, hour, load.threshold, service.level, do.plots=TRUE) {
  
  st.list = unlist(st.lists)
  ar.list = unlist(ar.lists)
  
  st.sd = var(st.list)
  st.mean = mean(st.list)
  scv.service = (st.sd/st.mean)^2
  
  path = file.path('data/interim', group.label)
  if(do.plots) {
    
    dir.create(path, showWarnings = F)
    
    # Check service distribution
    fit_exp = fitdist(st.list, distr="exp", method="mle")
    fit_ln = fitdist(st.list, distr="lnorm", method="mle")
    fit_gamma = fitdist(st.list, distr="gamma", method="mle")
    
    
    # squared coefficient of variation for service time should be < 2, for exponential distribution to be appropriate
    # See Green et al, Coping with Time-Varying Demand When Setting Staffing Requirements for a Service System, 2007
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
  
  ar.res = queuemodel__rate_summary(ar.list)
  st.mean = mean(st.list)
  
  # Populate model
  lambda = ar.res$mean
  
  mu = 1/st.mean
  
  rho = lambda/mu
  rho.fast = rho/1.05
  
  # Search space
  # Lower bound
  s.lowerbound = round(rho/.8) # Make sure utilization proportion is less than 1 to make this work
  
  pw = 1
  s=s.lowerbound
  pw = queuemodel__erlang_c_wait_probability(s,rho)
  while(pw > service.level) {
    s=s+1
    pw = queuemodel__erlang_c_wait_probability(s,rho)
  }
  
  # Try again with faster service times reflecting low system load
  if(s < load.threshold) {
    pw = pa = 1
    s=s.lowerbound
    pw = queuemodel__erlang_c_wait_probability(s,rho.fast)
    while(pw > service.level) {
      s=s+1
      pw = queuemodel__erlang_c_wait_probability(s,rho.fast)
    }
  }
  
  
  if(do.plots) {
    
    # Plot queue performance for different number of servers
    mmcs = tibble(s=s.lowerbound:(s+5), lambda=lambda, mu=mu)
    mmcs %<>% rowwise() %>% do(queuemodel__queue_performance(s=.$s, lambda=.$lambda, mu=.$mu))
    
    plot.width=30
    
    p.prob = mmcs %>% 
      ggplot(aes(x=s,y=1-C)) +
      geom_line() +
      theme(panel.border = element_blank(),
            panel.background = element_blank(),
            panel.grid.minor = element_line(colour = "grey90"),
            panel.grid.major = element_line(colour = "grey90"),
            panel.grid.major.x = element_line(colour = "grey90"),
            axis.text = element_text(size = 10),
            axis.title = element_text(size = 12, face = "bold"),
            strip.text = element_text(size = 12, face = "bold")) +
      ggtitle(queuemodel__wrapper('Probability Unit Available (Erlang C model)', plot.width)) +
      ylab('Probability') +
      xlab('Number of Units Staffed')
    
    # p.W = mmcs %>% 
    #   ggplot(aes(x=s,y=W)) +
    #   geom_line() +
    #   theme(panel.border = element_blank(),
    #         panel.background = element_blank(),
    #         panel.grid.minor = element_line(colour = "grey90"),
    #         panel.grid.major = element_line(colour = "grey90"),
    #         panel.grid.major.x = element_line(colour = "grey90"),
    #         axis.text = element_text(size = 10),
    #         axis.title = element_text(size = 12, face = "bold"),
    #         strip.text = element_text(size = 12, face = "bold")) +
    #   ggtitle(queuemodel__wrapper('Estimated Event Time', plot.width)) +
    #   ylab('Time Period') +
    #   xlab('Number of Units Staffed')
    # 
    # p.Wq = mmcs %>% 
    #   ggplot(aes(x=s,y=Wq)) +
    #   geom_line() +
    #   theme(panel.border = element_blank(),
    #         panel.background = element_blank(),
    #         panel.grid.minor = element_line(colour = "grey90"),
    #         panel.grid.major = element_line(colour = "grey90"),
    #         panel.grid.major.x = element_line(colour = "grey90"),
    #         axis.text = element_text(size = 10),
    #         axis.title = element_text(size = 12, face = "bold"),
    #         strip.text = element_text(size = 12, face = "bold")) +
    #   ggtitle(queuemodel__wrapper('Estimated Time Waiting for Available Unit', plot.width)) +
    #   ylab('Time Period') +
    #   xlab('Number of Units Staffed')
    # 
    # p.L = mmcs %>% 
    #   ggplot(aes(x=s,y=L)) +
    #   geom_line() +
    #   theme(panel.border = element_blank(),
    #         panel.background = element_blank(),
    #         panel.grid.minor = element_line(colour = "grey90"),
    #         panel.grid.major = element_line(colour = "grey90"),
    #         panel.grid.major.x = element_line(colour = "grey90"),
    #         axis.text = element_text(size = 10),
    #         axis.title = element_text(size = 12, face = "bold"),
    #         strip.text = element_text(size = 12, face = "bold")) +
    #   ggtitle(queuemodel__wrapper('Estimated Number of Ongoing Events', plot.width)) +
    #   ylab('Number of Events') +
    #   xlab('Number of Units Staffed')
    # 
    # p.Lq = mmcs %>% 
    #   ggplot(aes(x=s,y=Lq)) +
    #   geom_line() +
    #   theme(panel.border = element_blank(),
    #         panel.background = element_blank(),
    #         panel.grid.minor = element_line(colour = "grey90"),
    #         panel.grid.major = element_line(colour = "grey90"),
    #         panel.grid.major.x = element_line(colour = "grey90"),
    #         axis.text = element_text(size = 10),
    #         axis.title = element_text(size = 12, face = "bold"),
    #         strip.text = element_text(size = 12, face = "bold")) +
    #   ggtitle(queuemodel__wrapper('Estimated Number of Events Waiting for Available Unit', plot.width)) +
    #   ylab('Number of Events') +
    #   xlab('Number of Units Staffed')
    # 
    # p.full = grid.arrange(p.prob, p.W, p.Wq, p.L, p.Lq, ncol=3)
    ggsave(file.path(path,paste0('plot_queue_model_performance__hour',hour,'.pdf')), plot=p.prob, device='pdf', width=11, height=8.5)
    
  }
  
  return(list(servers=s))
}

queuemodel__wrapper <- function(x, ...) {
  paste(strwrap(x, ...), collapse = "\n")
}

queuemodel__erlang_b_wait_probability <- function(s,rho) {
  # Probability of delay when new event arrives
  sigma = sum(sapply(0:s, function(i) rho^i/factorial(i)))
  B = (rho^s)/factorial(s)
  
  prob.wait = B / sigma
  
  return(prob.wait)
}


queuemodel__erlang_c_wait_probability <- function(s,rho) {
  # Probability of delay when new event arrives
  sigma = sum(sapply(0:(s-1), function(i) rho^i/factorial(i)))
  C = (rho^s)/(factorial(s-1)*(s-rho))
  
  prob.wait = C / (sigma + C)
  
  return(prob.wait)
}

queuemodel__queue_performance <- function(s,lambda,mu) {
  params.mmc = NewInput.MMC(lambda = lambda, mu = mu , n=1, method = 0 , c=s)
  mmc=QueueingModel(params.mmc)
  
  C=queuemodel__erlang_c_wait_probability(s,lambda/mu)
  B=queuemodel__erlang_b_wait_probability(s, lambda/mu)
  
  return(data.frame(s=s, C=C, B=B, Lq=mmc$Lq, L=mmc$L, W=mmc$W, Wq=mmc$Wq, P0=mmc$Pn[1]))
}

queuemodel__rate_summary <- function(l) {
  return(list("mean"=mean(l), "sd"=sd(l), 
              "p50"=quantile(l,.5), "p90"=quantile(l,.9)))
}

# plotLongTermRollingTrend <- function(rt, ylab="Arrival Rate", b=168) {
#   
#   # Aggregate and smooth data to make it easier to visualize
#   agg.rt = rollapply(rt$arrival_rate, b, by=b, FUN = sum, align="right")
#   agg.ts = rt[seq(b,nrow(rt),by=b),"start"]
#   d = agg.ts %>% add_column(arrival_rate=agg.rt)
#   
#   ma = d %>% mutate(
#     monthly_median = rollmedian(arrival_rate, k=5, fill=NA)
#   )
#   ma %>% gather(metric, value, arrival_rate:monthly_median) %>%
#     ggplot(aes(start, value, linetype=metric)) +
#     geom_line()+
#     scale_color_grey()+
#     theme(panel.border = element_blank(),
#           panel.background = element_blank(),
#           panel.grid.minor = element_line(colour = "grey90"),
#           panel.grid.major = element_line(colour = "grey90"),
#           panel.grid.major.x = element_line(colour = "grey90"),
#           axis.text = element_text(size = 10),
#           axis.title = element_text(size = 12, face = "bold"),
#           strip.text = element_text(size = 12, face = "bold")) +
#     labs(x = "Date", y = "Weekly Arrival Rate")
# }

queuemodel__convert <- function(dt, myorigin = "2018-12-9 00:00:00") {
  
  myorigin = as.POSIXct(myorigin, tz="UTC")
  dt %<>% arrange(group, ts) %>% mutate(window = myorigin + hms(paste0(ts, ':00')) + (group-1)*24*60*60 )
  
  dt = data.table(dt %>% select(window, count=s))
  return(dt)
}
