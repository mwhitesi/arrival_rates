# Name: historical_targets.R
# Desc: Identify number of units needed in each time window
# Author: Matt Whiteside
# Date: Dec 4, 2018

demandTarget <- function(datetimes, duration.in.min=15) {
  
  # Assign to bins (labels represent begining of time window)
  datetimes = timeutils$bin_time(datetimes, duration.in.min)
  window.counts = timeutils$concurrent_windows(datetimes)
  
  # Plot overall trend
  window.counts[,`:=`(daily.median=rollmedian(count, k=95, fill=NA), weekly.median=rollmedian(count, k=671, fill=NA))]
  q95=window.counts[,quantile(count,.95)]
  x.min = min(window.counts[,window])

  p = window.counts %>% gather(metric,value,count:weekly.median) %>%
    ggplot(aes(x=window, y=value, linetype=metric, color=metric, size=metric)) +
    geom_line()+
    scale_size_manual(values=c(1,1.2,1.5))+
    scale_linetype_manual(values=c("dashed","solid","solid"))+
    scale_color_manual(values=c("grey80","grey40","black"))+
    geom_hline(yintercept=q95, linetype="dashed", color="red", size1.2)+
    annotate("text", x = x.min, y = q95, label = "95th Percentile", hjust=0, vjust=0, color="red")+
    theme(panel.border = element_blank(),
          panel.background = element_blank(),
          panel.grid.minor = element_line(colour = "grey90"),
          panel.grid.major = element_line(colour = "grey90"),
          panel.grid.major.x = element_line(colour = "grey90"),
          axis.text = element_text(size = 10),
          axis.title = element_text(size = 12, face = "bold"),
          strip.text = element_text(size = 12, face = "bold"))
  
  ggsave(paste0("data/interim/plot_total_period_median_demand.pdf"), p, dev="pdf", height=8.5, width=11)

  # Plot weekly trend
  window.counts[,`:=`(
    weekly.bin=wday(window), 
    daily.bin=as.numeric(window-floor_date(window,"day")) / (duration.in.min*60)
    )]
  setkey(window.counts, weekly.bin, daily.bin)
  
  weekly.counts <- window.counts[,.(
      n=sum(count), 
      mean=mean(count),
      sd=sd(count),
      p50=quantile(count,.5),
      p99.9=quantile(count,.999)
    ),
    .(weekly.bin, daily.bin)
  ]
  setorder(weekly.counts, weekly.bin, daily.bin)
  weekly.counts[,id:=1:.N]
  
  
  # Add time labels
  tl = apply(weekly.counts, 1, 
             function(r) {
               wd=wday(r["weekly.bin"], label=T)
               hr=seconds_to_period(r["daily.bin"]*duration.in.min*60)
               return(sprintf('%s %02d:%02d', wd, hour(hr), minute(hr)))
             })
  
  breakpoints <- seq(1, length(weekly.counts$id), 12)
  p.weekly <- weekly.counts %>% gather(metric,value,p50:p99.9) %>%
    ggplot(aes(x=id, y=value, group=desc(metric))) +
    geom_line(aes(color=metric), size=1.1) +
    scale_color_manual(values=c("black", "grey65"))+
    scale_x_continuous(labels=tl[breakpoints], breaks=breakpoints)+
    ylab("Number of Active Units")+
    xlab("Time Period")+
    theme(panel.border = element_blank(),
          panel.background = element_blank(),
          panel.grid.minor = element_line(colour = "grey90"),
          panel.grid.major = element_line(colour = "grey90"),
          panel.grid.major.x = element_line(colour = "grey90"),
          axis.text.x = element_text(angle = 45, hjust = 1, size=10),
          axis.title = element_text(size = 12, face = "bold"),
          strip.text = element_text(size = 12, face = "bold"))
  
  ggsave(paste0("data/interim/plot_weekly_median_demand.pdf"), p.weekly, dev="pdf", height=8.5, width=11)
  
  return(window.counts)
}


utilizationTarget <- function(window.counts, required.servers, st, ar, duration.in.min=15) {
  
  window.counts %<>% arrange(window)
  
  # Iterate over each week
  d = calcUtilization(window.counts, required.servers)
  
  d %>% mutate(daily.mean=rollmean(x=uz,48,fill=NA),
               weekly.mean=rollmean(x=uz,672,fill=NA),
               monthly.mean=rollmean(x=uz,672*4,fill=NA)) %>%
    na.omit() %>%
  ggplot(aes(window)) +
    geom_line(aes(y=daily.mean), color='grey40', alpha=.8) +
    geom_line(aes(y=weekly.mean), color='black') +
    geom_line(aes(y=monthly.mean), color='red') +
    ylab("Mean Utilization") +
    xlab("Timestamp") +
    theme(panel.border = element_blank(),
          panel.background = element_blank(),
          panel.grid.minor = element_line(colour = "grey90"),
          panel.grid.major = element_line(colour = "grey90"),
          panel.grid.major.x = element_line(colour = "grey90"),
          axis.text.x = element_text(angle = 45, hjust = 1, size=10),
          axis.title = element_text(size = 12, face = "bold"),
          strip.text = element_text(size = 12, face = "bold"))
  
  
  # window.counts %<>% arrange(window)
  # st.counts = dplyr::left_join(st, window.counts, by=c('start'='window')) 
  # st.counts %>% mutate(bin=cut(count,12)) %>% 
  #   group_by(bin) %>% filter(n() > 10) %>% dplyr::summarise(mean=mean(interval_length), 
  #                                                           se = 1.96*(sd(interval_length, na.rm=T)/sqrt(n()))) %>%
  #   ggplot(aes(x=bin, y=mean)) +
  #   geom_point() +
  #   geom_line() +
  #   geom_errorbar(aes(x=bin, ymin=mean-se, ymax=mean+se)) +
  #   ylab("Mean Service Time (s)")+
  #   xlab("N Concurrent Units")+
  #   theme(panel.border = element_blank(),
  #         panel.background = element_blank(),
  #         panel.grid.minor = element_line(colour = "grey90"),
  #         panel.grid.major = element_line(colour = "grey90"),
  #         panel.grid.major.x = element_line(colour = "grey90"),
  #         axis.text.x = element_text(angle = 45, hjust = 1, size=10),
  #         axis.title = element_text(size = 12, face = "bold"),
  #         strip.text = element_text(size = 12, face = "bold"))
  # 
  # 
  # increasing.windows = window.counts %>% filter
  
  utilizationSummary(d)
  
}

calcUtilization <- function(weekly.demand, required.servers) {
  
  
  d = weekly.demand %>% mutate(wd=wday(window), ts=strftime(window, format="%H:%M", tz="UTC")) %>%
    rowwise() %>%
    mutate(estimate = required.servers$s[wd == required.servers$group & ts == required.servers$ts])
  
  # Drop first and last day, since first few periods will not count preceding/post events that extend beyond analysis
  firstday = min(d$window)
  lastday = max(d$window)
  d %<>% filter(date(window) != date(firstday) & date(window) != date(lastday))
  
  d %<>% mutate(uz=count/estimate)
  
  d %<>% ungroup()
  
  # d %>% 
  #   ggplot(aes(x=window)) +
  #   geom_line(aes(y=count), color='black') +
  #   geom_line(aes(y=estimate), color='red')
  
  return(d)
}

utilizationSummary <-function(d) {
  uz = d %>% pull(uz)
  
  res = list(
    max=max(uz),
    min=min(uz),
    median=median(uz),
    p50=quantile(uz,.5),
    p90=quantile(uz,.9),
    mean=mean(uz),
    sd=sd(uz),
    overcapacity.periods=sum(uz > 1),
    undercapacity.periods=sum(uz < .5)
  )
  
  return(res)
}

weeklyUtilization <- function(d) {
  
  tbl = d %>% group_by(weekly.bin, daily.bin) %>% dplyr::summarise(mean=mean(uz)) %>% ungroup() %>%
    mutate(id=1:n())
  tl = apply(tbl, 1, 
             function(r) {
               wd=wday(as.integer(r["weekly.bin"]), label=T)
               hr=seconds_to_period(as.integer((r["daily.bin"]))*duration.in.min*60)
               return(sprintf('%s %02d:%02d', wd, hour(hr), minute(hr)))
             })
  breakpoints <- seq(1, length(tl), 12)
  
  tbl %>%
    ggplot(aes(x=id, y=mean)) +
    geom_line() +
    scale_x_continuous(labels=tl[breakpoints], breaks=breakpoints)+
    ylab("Average Utilization")+
    xlab("Time Period")+
    theme(panel.border = element_blank(),
          panel.background = element_blank(),
          panel.grid.minor = element_line(colour = "grey90"),
          panel.grid.major = element_line(colour = "grey90"),
          panel.grid.major.x = element_line(colour = "grey90"),
          axis.text.x = element_text(angle = 45, hjust = 1, size=10),
          axis.title = element_text(size = 12, face = "bold"),
          strip.text = element_text(size = 12, face = "bold"))
 
  
  
}

  
  
  
  
  
  
  
