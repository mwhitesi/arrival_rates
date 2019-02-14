performance__plot_weekly_capacity_line <- function(dt) {
  
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
  
  return(p)
}


performance__plot_weekly_multi_metric_line <- function(weekly, metrics, title='Availability', ylabel='# Units') {
  
  lims = c(min(weekly$window), max(weekly$window))
  
  p = weekly %>% gather(model,value,metrics) %>%
    ggplot(aes(x=window, y=value, group=model)) +
    geom_line(aes(linetype=model, color=model)) +
    geom_point(aes(shape=model), size=1) +
    scale_x_datetime(date_labels = "%A %H:%M", date_breaks='6 hour', limits=lims) +
    labs(y=ylabel, x=NULL) + 
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
    coord_cartesian(xlim=lims, expand=FALSE) +
    ggtitle(title)
  
  return(p)
  
}

performance__plot_weekly_multi_metric_bar <- function(weekly, metrics, title='Availability', ylabel='# Units') {
  
  lims = c(min(weekly$window), max(weekly$window))
  
  p = weekly %>% gather(model,value,metrics) %>%
    ggplot(aes(x=window, y=value, fill=model)) +
    geom_bar(stat='identity', position="dodge2") +
    scale_x_datetime(date_labels = "%A %H:%M", date_breaks='3 hour', limits=lims) +
    labs(y=ylabel, x=NULL) +
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
    coord_cartesian(xlim=lims, expand=FALSE) +
    ggtitle(title)
  
  return(p)
  
}


performance__plot_hour_histogram <- function(dt, metrics, duration.in.min, av.range=c(NA,NA)) {
  
  dt1 = melt(dt, measure.vars = metrics)
  
  counts = dt1[, .N, by=.(variable, value)]
  
  lwr = av.range[1]
  if(is.na(lwr)) {
    lwr = min(counts[,value])
  }
  
  upr = av.range[2]
  if(is.na(upr)) {
    upr = max(counts[,value])
  }
  
  counts = counts[value >= lwr & value <= upr]
  counts[, N := N * (duration.in.min / 60)]
  
  p = counts %>% ggplot(aes(x=value, y=N, group=variable)) +
    geom_bar(stat='identity', aes(fill=variable), position='dodge') +
    labs(y="Total Hours", x="# Units Available") +
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
          legend.key = element_blank(),
          legend.title=element_blank())
  
  return(p)
}

performance__plot_availability_cumulative_distribution <- function(dt, metrics, av.range=c(NA,NA)) {
  
  dt1 = melt(dt, measure.vars = metrics)
  
  lwr = av.range[1]
  if(is.na(lwr)) {
    lwr = min(dt1[,value])
  }
  
  upr = av.range[2]
  if(is.na(upr)) {
    upr = max(dt1[,value])
  }
  
  p = dt1 %>% ggplot(aes(x=value, group=variable)) +
    stat_ecdf(aes(color=variable, linetype=variable)) +
    labs(y="Proportion", x="# Units Available") +
    scale_x_continuous(limits=c(lwr,upr)) +
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
          legend.key = element_blank(),
          legend.title=element_blank())
  
  return(p)
}

performance__table_availability_cumulative_distribution <- function(dt) {
  
  tot=nrow(dt)
  av.perc = dt[,.(perc = round((.N/tot)*100, 1)), by=av]
  av.ecdf = ecdf(dt[,av])
  
  avcdf  = data.table(av=seq(min(dt[,av]), max(dt[,av])))
  setkey(avcdf, av)
  
  setkey(av.perc, av)
  avcdf = av.perc[avcdf]
  avcdf[is.na(perc), perc:=0]
  
  avcdf[,cdf:=round(av.ecdf(av),2)]
  
  wknd = dt[wday(window) %in% c(1,7)]
  wkdy = dt[wday(window) %in% c(2:6)]
  
  wknd.tot = nrow(wknd)
  wkdy.tot = nrow(wkdy)
  wknd.perc = wknd[,.(weekend = round((.N/wknd.tot)*100, 1)), by=av]
  wkdy.perc = wkdy[,.(weekday = round((.N/wkdy.tot)*100, 1)), by=av]
  setkey(wknd.perc, av)
  setkey(wkdy.perc, av)
  
  avcdf = wknd.perc[avcdf]
  avcdf = wkdy.perc[avcdf]
  
  avcdf[is.na(weekend), weekend:=0]
  avcdf[is.na(weekday), weekday:=0]
  
  return(avcdf)
}

