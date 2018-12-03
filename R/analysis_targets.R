# Name: analysis_targets.R
# Desc: Compute hourly rates. Plot figures
# Author: Matt Whiteside
# Date: Nov 20, 2018


arrivalSummaryTarget <- function(ar, file.out) {
  
  rt = ar %>% group_by(Stream, weekday=wday(start, label=T), hour=sprintf("%02d:00", hour(start))) %>% 
    summarise(n=sum(arrival_rate), 
              mean=mean(arrival_rate),
              sd=sd(arrival_rate),
              p50=quantile(arrival_rate,.5),
              p25=quantile(arrival_rate,.25),
              p75=quantile(arrival_rate,.75)
              )
  rt %>% write.csv(file = file.out)
  
  makeHistogram(ar, 'arrival_rate', "arrival_rate","Arrival Rate (# events per hour)", "Arrival Rate", 2)
  
  makePlots(ar, rt, 'arrival_rate', "arrival_rate","Arrival Rate (# events per hour)", "Arrival Rate", 15)
  
  return(rt)
}

makeHistogram <-function(tbl, val, filename, xl, hist.title, bw=5) {
  p = tbl %>% 
    ggplot(aes_string(x=val, fill="Stream")) + 
    geom_histogram(binwidth=bw, position="dodge") +
    xlab(xl) +
    ggtitle(paste(hist.title, "for Edmonton Events November 2017 - 2018"))
  
  ggsave(paste0("data/interim/",filename,"__histogram.pdf"), p, dev="pdf")
}

makePlots <- function(tbl, rt, val, filename, y.label, plot.title, y.lim=100) {
  
  jogged_labels = rep("", 24)
  jogged_labels[seq(1,25,by=3)] = sprintf("%02d:00", seq(0,25,by=3))
  
  # binned_tbl = tbl %>% mutate(weekday=wday(start, label=TRUE), 
  #                             hour=factor(sprintf("%02d:00", hour(start))))
  # 
  # # Box plot 
  # p = binned_tbl %>% 
  #   ggplot(aes_string(y=val, 
  #                     x="hour")) +
  #   stat_boxplot(geom="boxplot",
  #                #outlier.shape=NA,
  #                outlier.size=.2,
  #                lwd=.2,
  #                width=0.8
  #   ) +
  #   facet_grid(Stream~weekday) +
  #   theme(
  #     text = element_text(size=10),
  #     axis.text.x = element_text(angle = 45, hjust = 1, size=8))+
  #   ylab(y.label) +
  #   xlab("Time Period") +
  #   scale_x_discrete(labels=jogged_labels) +
  #   scale_y_continuous(limits=c(0,y.lim)) +
  #   ggtitle(paste(plot.title, "for Edmonton Events November 2017 - 2018"))
  #  
  # ggsave(paste0("data/interim/",filename,"__boxplot.pdf"), p, dev="pdf")
  
  # Strip out IFT for this next part
  nonift = rt %>% filter(grepl("911", Stream))
  p1 = ggplot(data=nonift, aes(y = p50, 
                      x = factor(hour),
                      group = Stream,
                      color = Stream)) +
    geom_line() +
    geom_point(size=2, aes(shape=Stream)) +
    geom_errorbar(aes(ymax=p75,ymin=p25),width=.5, lwd=.4) +
    facet_grid(.~weekday) +
    theme(
      text = element_text(size=10),
      axis.text.x = element_text(angle = 45, hjust = 1, size=6))+
    ylab(y.label) +
    xlab("Time Period") +
    scale_x_discrete(labels=jogged_labels) +
    #scale_y_continuous(limits=c(0,y.lim)) +
    ggtitle(paste(plot.title, "for Edmonton Events November 2017 - 2018"))
  
  ggsave(paste0("data/interim/",filename,"__lineplot.pdf"), p1, dev="pdf")
  
}

serviceTimeSummaryTarget <- function(st, file.out) {
  tmp = st %>% mutate(interval_length = interval_length/60)
  rt = tmp %>% group_by(Stream, weekday=wday(start, label=T), hour=sprintf("%02d:00", hour(start))) %>% 
    summarise(n=sum(interval_length), 
              mean=mean(interval_length),
              sd=sd(interval_length),
              p50=quantile(interval_length,.5),
              p25=quantile(interval_length,.25),
              p75=quantile(interval_length,.75)
    )
  rt %>% write.csv(file = file.out)
  
  makeHistogram(tmp, 'interval_length', "service_time", "Service Time (minutes)", "Service Time", 10)
  
  makePlots(tmp, rt, 'interval_length', "service_time", "Service Time (minutes)", "Service Time", 300)
  
  return(rt)
}

correlationSummaryTarget <- function(st_rates, ar_rates) {
  rates = inner_join(ar_rates, st_rates, by=c('Stream', 'weekday', 'hour'), suffix=c('.ar', '.st'))
  
  # IFT is a distraction
  rates911 = rates %>% filter(grepl("911", Stream))
  
  p5 = ggplot(data=rates911, aes(x=p50.ar, y=p50.st, group=Stream)) +
    geom_point(size=2, aes(shape=Stream))+
    xlab("Median Arrival Rate")+
    ylab("Median Time on Task")+
    ggtitle("Comparison of Time on Task and Number of Events for Each Hour Period\nin a Given Day of Week")
  p5
  ggsave("data/interim/time_on_task_vs_arrival_rates_plot.pdf", p5, dev="pdf")
  
  return(rates)
}

