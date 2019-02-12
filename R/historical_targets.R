# Name: historical_targets.R
# Desc: Identify number of units needed in each time window
# Author: Matt Whiteside
# Date: Dec 4, 2018



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


plot_weekly_availability_line <- function(historical, funded, modelled) {
  
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

plot_weekly_availability_bar <- function() {
  
}

plot_weekly_utilization_bar <- function() {
  
}

plot_availability_distribution <- function() {
  
}


  
  
  
  
  
  
  
