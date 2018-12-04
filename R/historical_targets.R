# Name: historical_targets.R
# Desc: Identify number of units needed in each time window
# Author: Matt Whiteside
# Date: Dec 4, 2018

demandTarget <- function(datetimes, duration=15) {
  
  # Update minute bin assignment to match desired duration
  
  # The bin_timestamp function updates only the minute bins,
  # bin_events_by_time function sets the day and week bins
  datetimes[,`:=`(startbin=timeutils$bin_timestamp(start, duration), endbin=timeutils$bin_timestamp(end, duration))]
  
  # Assign each unit deployment to time window bins
  timeutils$add_interval_bins(datetimes, duration=duration)
  
  # Count number of active units in each bin
  demand = timeutils$concurrent_counts_group_by(datetimes)
  
  # Visualise yearly & weekly demand patterns
  u=paste(duration, "mins")
  startdate = floor_date(min(dt[,start]), unit=u)
  enddate = ceiling_date(max(dt[,end]), unit=u)
  timeseries = seq(startdate, enddate, by=u)
  
  demand %>% 
    ggplot(aes())""

  
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