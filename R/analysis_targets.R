# Name: analysis_targets.R
# Desc: Compute hourly rates. Plot figures
# Author: Matt Whiteside
# Date: Nov 20, 2018


addTimeColumns <- function(tbl) {
  tmp = tbl %>% mutate(weekday=wday(start, label=T), ts=strftime(start, format="%H:%M", tz="UTC"),
                       wd=wday(start, label=F))
  
  return(tmp)
}

arrivalSummaryTarget <- function(ar, file.out) {
  
  rt = ar %>% group_by(weekday, ts) %>% 
    summarise(n=sum(arrival_rate), 
              mean=mean(arrival_rate),
              sd=sd(arrival_rate),
              p50=quantile(arrival_rate,.5),
              p25=quantile(arrival_rate,.25),
              p75=quantile(arrival_rate,.75)
              )
  rt %>% write.csv(file = file.out)
  
  makeHistogram(ar, 'arrival_rate', "arrival_rate", "Arrival Rate (# events per 15 min)", "Arrival Rate", 1)
  
  makePlots(ar, rt, 'arrival_rate', "arrival_rate", "Arrival Rate (# events per 15 min)", "Arrival Rate")
  
  return(rt)
}

makeHistogram <-function(tbl, val, filename, xl, hist.title, bw=5) {
  p = tbl %>% 
    ggplot(aes_string(x=val)) + 
    geom_histogram(binwidth=bw) +
    xlab(xl) +
    ggtitle(paste(hist.title, "for Edmonton Events November 2017 - 2018"))
  
  ggsave(paste0("data/interim/",filename,"__histogram.pdf"), p, dev="pdf")
}

makePlots <- function(tbl, rt, val, filename, y.label, plot.title) {
  
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
    
    ggsave(paste0("data/interim/",filename,"_",d,"__lineplot.pdf"), p1, dev="pdf", width=11, height=8.5)
    
  }
}

serviceTimeSummaryTarget <- function(st, file.out) {
  
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
  
  makeHistogram(tmp, 'interval_length', "service_time", "Service Time (minutes)", "Service Time", 10)
  
  makePlots(tmp, rt, 'interval_length', "service_time", "Service Time (minutes)", "Service Time")
  
  return(rt)
}

correlationSummaryTarget <- function(st_rates, ar_rates) {
  rates = inner_join(ar_rates, st_rates, by=c('weekday', 'ts'), suffix=c('.ar', '.st'))
  
  p5 = ggplot(data=rates, aes(x=p50.ar, y=p50.st)) +
    geom_point(size=1) +
    geom_smooth(method = "lm") +
    xlab("Median Arrival Rate") +
    ylab("Median Time on Task") +
    ggtitle("Comparison of Time on Task and Number of Events for Each Period\nin a Given Day of Week")
  ggsave("data/interim/time_on_task_vs_arrival_rates_plot.pdf", p5, dev="pdf")
  
  return(rates)
}

