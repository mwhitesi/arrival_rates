# Name: historical_targets.R
# Desc: Identify number of units needed in each time window
# Author: Matt Whiteside
# Date: Dec 4, 2018

demandTarget <- function(datetimes, duration.in.min=15, do.plot=TRUE) {
  #'Extract number of units on event in each duration
  #'
  #'Drake Target
  #'
  #'@param datetimes data.table of unit events with start and end timestamp columns
  #'@param duration.in.min Number of minutes to use for binning period
  #'@param do.plot Boolean to turn off plots
  #'
  #'@returns window.counts data.table of number of units on events in each period
  
  # Assign bins (labels represent beginning of time window)
  datetimes = timeutils$bin_time(datetimes, duration.in.min)
  window.counts = timeutils$concurrent_windows(datetimes)
  
  window.counts[,`:=`(
    weekly.bin=wday(window), 
    daily.bin=as.numeric(window-floor_date(window,"day")) / (duration.in.min*60)
  )]
  setkey(window.counts, weekly.bin, daily.bin)
  
  if(do.plot) {
    
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
    
  }
  
  return(window.counts)
}

availabilityTarget <- 


utilizationTarget <- function(window.counts, required.servers, duration.in.min=15, do.plot=TRUE) {
  
  window.counts %<>% arrange(window)
  
  # Iterate over each week
  d = calcUtilization(window.counts, required.servers)
  
  if(do.plot) {
    d %>% mutate(daily.mean=rollmean(x=ut,48,fill=NA),
                 weekly.mean=rollmean(x=ut,672,fill=NA),
                 monthly.mean=rollmean(x=ut,672*4,fill=NA)) %>%
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
  }
 
  
  s=utilizationSummary(d)
  
  return(s)
}




errorSummary <- function(d) {
  
  e = list(
    'rmse'=rmse(d$count, d$estimate),
    'asym.rmse'=asymmetricRMSE(d$count, d$estimate, .25)
  )
  
  return(e)
}

asymmetricLoss <- function(actual, predicted, gamma=0.5) {
  w = ifelse((actual-predicted)<0, 1-gamma, gamma)
  aae = w*ae(actual-predicted)
  
  return(aae)
}

asymmetricQuadraticLoss <- function(actual, predicted, gamma=0.5) {
  w = ifelse((actual-predicted)<0, 1-gamma, gamma)
  ase = w*se(actual,predicted)
  
  return(ase)
}

asymmetricMSE <- function(actual, predicted, gamma=0.5) {
  return(mean(asymmetricQuadraticLoss(actual, predicted, gamma)))
}

asymmetricMAE <- function(actual, predicted, gamma=0.5) {
  return(mean(asymmetricLoss(actual, predicted, gamma)))
}

asymmetricRMSE <- function(actual, predicted, gamma=0.5) {
  return(sqrt(asymmetricMSE(actual, predicted, gamma)))
}
  

calculateAvailability <- function(demand, units, unroll=TRUE) {
  
  if(unroll) {
    # Only a weeks worth of scheduled units
    # Match weekly units for each day in dataset
    d = demand %>% mutate(wd=wday(window), ts=strftime(window, format="%H:%M", tz="UTC")) %>%
      rowwise() %>%
      mutate(units = units$s[wd == units$group & ts == units$ts])
    d %<>% ungroup()
  } else {
    # Match units logged on for given day to demand table
    setkey(demand, window)
    setkey(units, window)
    d = merge(demand, units)
    d = d[,.(window, count.x, count.y)]
    setnames(d, c("count.x", "count.y"), c("count", "units"))
  }
  
  # Drop first and last day, since first few periods will not count preceding/post events that extend beyond analysis
  firstday = min(d$window)
  lastday = max(d$window)
  d %<>% filter(date(window) != date(firstday) & date(window) != date(lastday))
  
  d %<>% mutate(ut=count/units, av=units-count)
  
  
  # d %>% 
  #   ggplot(aes(x=window)) +
  #   geom_line(aes(y=count), color='black') +
  #   geom_line(aes(y=estimate), color='red')
  
  return(d)
}

utilizationSummary <-function(d) {
  ut = d %>% pull(ut)
  reqd = d %>% pull(estimate)
  
  res = list(
    max=max(ut),
    min=min(ut),
    p50=quantile(ut,.5),
    p90=quantile(ut,.9),
    mean=mean(ut),
    sd=sd(ut),
    overcapacity.periods=sum(ut > 1),
    undercapacity.periods=sum(ut < .5),
    d.total=sum(reqd),
    d.sum=mean(reqd)
  )
  
  return(res)
}

weeklyUtilization <- function(d) {
  
  tbl = d %>% group_by(weekly.bin, daily.bin) %>% dplyr::summarise(mean=mean(ut)) %>% ungroup() %>%
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

loadUN_WKLOAD <- function(file, duration.in.min=15, do.plot=TRUE) {
  #'Load data download from EDMO_UNIT_Workload_By_Date.sql query
  #'
  #'Converts UN_WKLOAD data into a count of logged on units in each period
  #'
  #'@param file filepath to csv file
  #'@param duration.in.min Number of minutes to use for binning period
  #'@param do.plot Boolean to turn off plots
  #'@param logfile path to file that records log_messages
  
  dt = data.table::fread(file, check.names = TRUE)
  
  # Unit IDs 1,2,3,6
  log_message('Step: filtering specialty units')
  log_message('Rule: 6th character in Unit Name is 1,2,3,6')
  nr = nrow(dt)
  log_message(sprintf('Before: %d', nr))
  dt = dt[substr(Unit, 6,6) %in% c(1,2,3,6)]
  log_message(sprintf('After: %d. Removed: %d', nrow(dt), nr-nrow(dt)))
  
  # Shift time booked for event
  log_message('Step: filtering special event booked shifts')
  log_message('Rule: Remove LoggedOn >= 4hrs, OnEvent/LoggedOn > 0.95')
  nr = nrow(dt)
  log_message(sprintf('Before: %d', nr))
  rows = dt$LoggedOn >= 4*60*60 & dt$OnEvent/dt$LoggedOn > .95
  rem = dt[rows]
  dt = dt[!rows]
  log_message(sprintf('After: %d. Removed: %d', nrow(dt), nrow(rem)))
  log_message("\n--START-REMOVED--")
  log_table(rem)
  log_message("\n--END-REMOVED--\n\n")
  
  # Short orphan shifts
  log_message('Step: filtering short orphan shifts')
  log_message('Rule: Remove LoggedOn <= 1hrs and no >= 4hr LoggedOn record within 90 mins that has same unit ID and DGROUP')
  dt = dt[, `:=`(start = as.POSIXct(Shift_Start_TS, tz="UTC"), end=as.POSIXct(Shift_End_TS, tz="UTC"))]
  long = dt[LoggedOn > 3600]
  short = dt[LoggedOn <= 3600]
  setkeyv(long, c('Unit', 'start'))
  orphans = short %>% rowwise %>% do(x=has_continuing_shift(., long)) %>% unnest(x) %>% mutate(orphan = x != 1) %>% pull(orphan)
  dt=rbind(long,short[!orphans])
  log_message(sprintf('After: %d. Removed: %d', nrow(dt), sum(orphans)))
  log_message("\n--START-REMOVED--")
  log_table(short[orphans, .(Unit, Vehicle, Shift_Start_TS, Shift_End_TS, LoggedOn, OnEvent)])
  log_message("\n--END-REMOVED--\n\n")
  
  
  # Floor timestamps to bin into equal periods
  dt = timeutils$bin_time(dt, duration.in.min)
  window.counts = timeutils$concurrent_windows(dt)
  
  window.counts[,`:=`(
    weekly.bin=wday(window), 
    daily.bin=as.numeric(window-floor_date(window,"day")) / (duration.in.min*60)
  )]
  setkey(window.counts, weekly.bin, daily.bin)
  
  if(do.plot) {
    
    # A random sunday -- just need a starting point to create artifical days of the week
    myorigin = "2018-12-9 00:00:00"
    
    # Plot weekly trend
    weekly.counts <- window.counts[,.(
      n=sum(count), 
      mean=mean(count),
      sd=sd(count),
      p50=quantile(count,.5),
      p99=quantile(count,.99)
    ),
    .(weekly.bin, daily.bin)
    ]
    setorder(weekly.counts, weekly.bin, daily.bin)
    weekly.counts[,id:=1:.N]
    
    # Add a timestamp matching each hour/day of week
    np = max(weekly.counts$id)
    weekly.counts[,ts:=as.POSIXct(0:(np-1)*duration.in.min*60, origin=myorigin, tz="UTC")]
    lims = as.POSIXct(c(0,(np*duration.in.min*60)), origin=myorigin, tz="UTC")
    
    p.weekly <- weekly.counts %>% gather(metric,value,p50:p99) %>%
      ggplot(aes(x=ts, y=value, group=desc(metric))) +
      geom_line(aes(color=metric), size=1.1) +
      scale_color_manual(values=c("black", "grey60"))+
      scale_x_datetime(date_labels = "%A %H:%M", date_breaks='6 hour', limits=lims) +
      ylab("Number of Logged On Units")+
      xlab("Time Period")+
      theme(panel.border = element_blank(),
            panel.background = element_blank(),
            panel.grid.minor = element_line(colour = "grey90"),
            panel.grid.major = element_line(colour = "grey90"),
            panel.grid.major.x = element_line(colour = "grey90"),
            axis.text.x = element_text(angle = 45, hjust = 1, size=10),
            axis.title = element_text(size = 12, face = "bold"),
            strip.text = element_text(size = 12, face = "bold"))
    
    ggsave(paste0("data/interim/historical/plot_historical_weekly_logged_on_units.pdf"), p.weekly, dev="pdf", height=8.5, width=11)
    
  }
  
 
  return(window.counts)
  
}

capture.output(loadUN_WKLOAD(file, do.plot=FALSE), file='data/interim/UNIT_WKLOAD.log', split=FALSE, type = 'message')

log_message <- function(msg) {
  message(msg)
}

log_table <- function(tbl) {
  apply(tbl, 1, function(r) log_message(paste0(r, '\t')))
}

has_continuing_shift <- function(shortrow, long) {
 
  match = long[
    substr(Unit,1,5) == substr(shortrow$Unit,1,5) &    # Same DGROUP
    substring(Unit,8) == substring(shortrow$Unit,8) &  # Same Unit Number
    start > shortrow$end &                             # Starts after
    start <= (shortrow$end + minutes(90)) &            # Starts within 90 mins
    LoggedOn > 4*60*60                                 # Is at least 4 hrs
  ]
  
  return(nrow(match))
}

shifts$plot_weekly_availability_line <- function(historical, funded, modelled) {
  
  lims = c(min(dt$window), max(dt$window))
  
  p = shifts %>%
    ggplot(aes(x=window, y=required)) +
    geom_line(aes(linetype='Required')) +
    geom_line(aes(x=window, y=staffed, linetype='Staffed')) +
    geom_ribbon(aes(ymin = required, ymax = pmin(staffed, required), fill = "Overcapacity "), alpha=0.5) +
    geom_ribbon(aes(ymin = staffed, ymax = pmin(staffed, required), fill = "Undercapacity "), alpha=0.5) +
    scale_fill_manual(values = c("grey", "red")) +
    scale_x_datetime(date_labels = "%A %H:%M", date_breaks='6 hour', limits=lims) +
    labs(y="# Units", x=NULL) + 
    theme(axis.text.x=element_text(angle=45, hjust=1), axis.text.y=element_text(margin=margin(0,20,0,20)), legend.title=element_blank()) +
    theme(panel.background = element_rect(fill="#ffffff", colour=NA),
          axis.title.x = element_text(vjust=-0.2), axis.title.y=element_text(vjust=1.5),
          title = element_text(vjust=1.2),
          panel.border = element_blank(),
          axis.line =element_line(size=0.5, colour="grey80"),
          panel.grid.minor = element_blank(),
          panel.grid.major.x = element_line(size=0.5, colour="grey80"),
          panel.grid.major.y = element_line(size=0.5, colour="grey80"),
          axis.ticks = element_blank(),
          legend.position = "bottom", 
          axis.title = element_text(size=rel(0.8)),
          strip.text = element_text(size=rel(1)),
          strip.background=element_rect(fill="#ffffff", colour=NA),
          panel.spacing.y = unit(1.5, "lines"),
          legend.key = element_blank()) +
    coord_cartesian(xlim=lims,expand=FALSE) +
    ggtitle('Staffed Units vs Required Units')
  
  return(p)
}

shifts$plot_weekly_availability_bar <- function() {
  
}

shifts$plot_weekly_utilization_bar <- function() {
  
}

shifts$plot_availability_distribution <- function() {
  
}

loadProfile <- function(file, duration.in.min=15, do.plot=TRUE) {
  #'Load data csv data from Provincial profile query
  #'
  #'Converts Profile shift data into a count of scheduled units in each period
  #'
  #'@param file filepath to csv file
  #'@param duration.in.min Number of minutes to use for binning period
  #'@param do.plot Boolean to turn off plots
  
  dt = data.table::fread(file, check.names = TRUE)
  
  
  # Filter EDMO 911 ALS/BLS units
  dt = dt[(Level == "BLS" | Level == "ALS") & Type == "Ambulance"]
  dt = dt[Category == "911"]
  





  
  
  
  
  
  
  
