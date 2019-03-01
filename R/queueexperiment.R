# External packages
source("R/packages.R") 


# Work flow functions
source("R/queuemodel.R")


dur.m = 15

# Load required data
st=drake::readd(st2)
ar=drake::readd(ar2)
demand = drake::readd(demand)


utl = list()

experiment <- function(dmnd) {
  
  # No alteration to the queue model inputs
  servs = queuemodel_model(ar, st, duration.in.min=dur.m, ar.lag=0, st.lag=0, do.plot=FALSE)
  utl[['NoLag']] = experiment_results(dmnd, servs, duration.in.min=dur.m, do.plot=FALSE)

  # Average arrival rate & service lagged by mean service rate
  servs = queuemodel_model(ar, st, duration.in.min=dur.m, ar.lag=NULL, st.lag=NULL, do.plot=FALSE)
  utl[['LagAve']] = experiment_results(dmnd, servs, duration.in.min=dur.m, do.plot=FALSE)
 
  # Average arrival rate lagged by mean service rate, service lagged by preceding period
  servs = modelTarget(ar, st, duration.in.min=dur.m, ar.lag=NULL, st.lag=NULL, load.threshold=35, do.plot=FALSE)
  utl[['LagAveLoad']] = experiment_results(dmnd, servs, duration.in.min=dur.m, do.plot=FALSE)
  
  
  return(utl)
}

experiment_results <- function(window.counts, required.servers, duration.in.min=15, do.plot=TRUE) {
  
  window.counts %<>% arrange(window)
  
  # Iterate over each week
  d = calculateAvailability(window.counts, required.servers)
  
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
  }
  
  
  s=utilizationSummary(d)
  
  return(s)
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

utl=experiment(dmnd)

utl.df = data.frame(matrix(unlist(utl), nrow=length(utl), byrow=T), stringsAsFactors=FALSE)
colnames(utl.df) = names(utl[[1]])
rownames(utl.df) = names(utl)
