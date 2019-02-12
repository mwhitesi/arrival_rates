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

# plot_weekly_utilization_bar <- function() {
#   
# }
# 
# plot_availability_distribution <- function() {
#   
# }