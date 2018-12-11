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
      p25=quantile(count,.25),
      p75=quantile(count,.75),
      p05=quantile(count,.05),
      p95=quantile(count,.95)
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
               return(sprintf('%s %02d:%02d:%02d', wd, hour(hr), minute(hr), second(hr)))
             })
  
  breakpoints <- seq(1, length(weekly.counts$id), 12)
  p.weekly <- weekly.counts %>% gather(metric,value,p50:p95) %>%
    ggplot(aes(x=id, y=value, group=desc(metric))) +
    geom_line(aes(color=metric, size=metric)) +
    scale_size_manual(values=c(.9,.9,1.5,.9,.9))+
    scale_color_manual(values=c("grey85","grey65","black","grey65","grey85"))+
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
  
  
  
  
  
  
  
