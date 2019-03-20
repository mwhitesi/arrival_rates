# Name: shifts.R
# Desc: Functions for optimizing shifts given daily requirments
# Author: Matt Whiteside
# Date: Nov 7, 2018


library(data.table)

shifts <- new.env(parent=emptyenv())

shifts$shift_options <- function(shift.types, period.stagger=15, period.days=7, daily.sc.window=c(1,1440)) {
  # Build matrix of possible shift arrangements and the daily periods that each shift covers
  
  sfts = list()
  changeovers = list()
  costs = list()
  types = list()
  
  i = 1
  for (s in shift.types) {
   
    tmp = shifts$encode_shift(s, period.stagger=period.stagger, period.days=period.days, daily.sc.window=daily.sc.window)

    sfts[[i]] = tmp$shifts
    changeovers[[i]] = tmp$changeover
    costs[[i]] = tmp$cost
    types[[i]] = tmp$type 
      
    i = i + 1
  }
  
  return(list(shifts=do.call(rbind, sfts), 
              changeovers=do.call(rbind, changeovers),
              costs=unlist(costs),
              types=unlist(types)))
  
}

shifts$encode_shift <- function(shift.type, period.stagger=15, period.days=7, daily.sc.window) {
  
  switch(shift.type,
         'week_247' = {
           sfts = shifts$encode_247(period.stagger, period.days, daily.sc.window)
         },
         'week_12hr' = {
           sfts = shifts$encode_week(12*60, period.stagger, period.days, daily.sc.window)
         },
         'week_10.5hr' = {
           sfts = shifts$encode_week(10.5*60, period.stagger, period.days, daily.sc.window)
         },
         'week_8.4hr' = {
           sfts = shifts$encode_week(8.4*60, period.stagger, period.days, daily.sc.window)
         },
         '4day_10.5hr' = {
           sfts = shifts$encode_contiguous_days(10.5*60, 4, period.stagger, period.days, daily.sc.window)
         },
         '4day_12hr' = {
           sfts = shifts$encode_contiguous_days(12*60, 4, period.stagger, period.days, daily.sc.window)
         },
         '2day_10.5hr' = {
           sfts = shifts$encode_contiguous_days(10.5*60, 2, period.stagger, period.days, daily.sc.window)
         },
         '2day_12hr' = {
           sfts = shifts$encode_contiguous_days(12*60, 2, period.stagger, period.days, daily.sc.window)
         },
         '4day_8.4hr' = {
           sfts = shifts$encode_contiguous_days(8.4*60, 4, period.stagger, period.days, daily.sc.window)
         },
         '2day_8.4hr' = {
           sfts = shifts$encode_contiguous_days(8.4*60, 2, period.stagger, period.days, daily.sc.window)
         },
         '10.5hr' = {
           sfts = shifts$encode_contiguous_days(10.5*60, 1, period.stagger, period.days, daily.sc.window)
         },
         '12hr' = {
           sfts = shifts$encode_contiguous_days(12*60, 1, period.stagger, period.days, daily.sc.window)
         },
         '8.4hr' = {
           sfts = shifts$encode_contiguous_days(8.4*60, 1, period.stagger, period.days, daily.sc.window)
         },
         stop(paste('Unrecognized shift type: ',shift.type))
  )
  
  return(list(shifts=sfts$shifts, changeover=sfts$changeover, cost=sfts$cost, type=rep(shift.type, length(sfts$cost))))
}

shifts$possible_daily_starts <- function(period.stagger, daily.sc.window) {
  
  return(seq(ceiling(daily.sc.window[1]/period.stagger), ceiling(daily.sc.window[2]/period.stagger)))
}

shifts$possible_period_sc <- function(period.stagger, period.days, daily.sc.window, shift.duration) {

  daily = shifts$possible_daily_starts(period.stagger, daily.sc.window)
  daily.shift = 24*60/period.stagger
  max.period = daily.shift * period.days
  shift.periods = shift.duration/period.stagger
  l = length(daily)
  
  starts = rep(0, l*period.days)
  ends = rep(0, l*period.days)
  
  for (d in 1:period.days) {
    i = d - 1
    j = i * l + 1
    k = j + l - 1
    
    s = daily + i*daily.shift
    e = sapply(s, function(i) ifelse(i + shift.periods > max.period, (i + shift.periods) %% max.period, i + shift.periods))
    
    starts[j:k] = s
    ends[j:k] = e
    
  }
  
  return(list(start=starts, end=ends))
}

shifts$encode_247 <- function(period.stagger=15, period.days=7, daily.sc.window=c(1,1440)) {
  # 24/7 shift - in reality two 12 hour shifts running continuously
  
  nperiods = 24*60*period.days/period.stagger
  
  daily.starts = shifts$possible_daily_starts(period.stagger, daily.sc.window)
  nshifts = length(daily.starts)
  sfts = matrix(1, ncol=nperiods, nrow=nshifts)
  changeovers = matrix(0, ncol=nperiods, nrow=nshifts)
 
  sc = shifts$possible_period_sc(period.stagger, period.days, daily.sc.window, 12*60)
  idx1 = matrix(c(rep_len(1:nshifts, length(sc$start)), sc$start), byrow=FALSE, ncol=2)
  idx2 = matrix(c(rep_len(1:nshifts, length(sc$start)), sc$end), byrow=FALSE, ncol=2)
  changeovers[idx1] = changeovers[idx1] + 2  # 2 Units swapping
  changeovers[idx2] = changeovers[idx2] + 2   # 2 Units swapping
  
  colnames(sfts) <- seq(1,nperiods)
  colnames(changeovers) <- seq(1,nperiods)
  
  return(list(shifts=sfts, changeover=changeovers, cost=rep(24*period.days, nshifts)))
}

shifts$encode_week <- function(sft.len, period.stagger=15, period.days=7, daily.sc.window=c(1,1440)) {
  # Start same time each day
  
  nperiods = 24*60*period.days/period.stagger
  period.len = sft.len / period.stagger
  sft.periods = shifts$possible_daily_starts(period.stagger, daily.sc.window)
  day.stagger = 24*60/period.stagger
  
  nshifts = length(sft.periods)
  sfts = changeovers = matrix(0, ncol=nperiods, nrow=nshifts)
  
  for (i in 1:nshifts) {
    p = sft.periods[i]
    
    for (d in 0:(period.days - 1)) {
      start = p + (d*day.stagger)
      
      if (start > nperiods) {
        start = start %% nperiods
      }
      
      end = start + period.len - 1
      
      if (end > nperiods) {
        end2 = end %% nperiods
        sfts[i, 1:end2] = 1
        end = nperiods
      }
      
      sfts[i, start:end] = 1
    }

  }
  
  sc = shifts$possible_period_sc(period.stagger, period.days, daily.sc.window, 12*60)
  idx1 = matrix(c(rep_len(1:nshifts, length(sc$start)), sc$start), byrow=FALSE, ncol=2)
  idx2 = matrix(c(rep_len(1:nshifts, length(sc$start)), sc$end), byrow=FALSE, ncol=2)
  changeovers[idx1] = changeovers[idx1] + 1  
  changeovers[idx2] = changeovers[idx2] + 1
  
  colnames(sfts) <- seq(1,nperiods)
  colnames(changeovers) <- seq(1,nperiods)
  
  return(list(shifts=sfts, changeover=changeovers, cost=rep(12*period.days,nshifts)))
}

shifts$encode_contiguous_days <- function(sft.len, sft.days, period.stagger=15, period.days=7, daily.sc.window=c(1,1440)) {
  # Start same time every day for d consecutive days
  
  stopifnot(sft.days <= period.days)

  nperiods = 24*60*period.days/period.stagger
  sft.nperiods = sft.len / period.stagger
 
  possible.sfts = shifts$possible_period_sc(period.stagger, period.days, daily.sc.window, sft.len)
 
  nshifts = length(possible.sfts[['start']])
  day.stagger = 24*60/period.stagger
  
  sfts = changeovers = matrix(0, ncol=nperiods, nrow=nshifts)

  for (i in 1:nshifts) {
    p = possible.sfts$start[i]
    
    for (d in 0:(sft.days - 1)) {
      start = p + (d*day.stagger)
      
      if (start > nperiods) {
        start = start %% nperiods
      }
      
      end = start + sft.nperiods - 1

      if (end > nperiods) {
        end2 = end %% nperiods
        sfts[i, 1:end2] = 1
        end = nperiods
      }

      sfts[i, start:end] = 1
    }
  }
  
  idx1 = matrix(c(rep_len(1:nshifts, length(possible.sfts$start)), possible.sfts$start), byrow=FALSE, ncol=2)
  idx2 = matrix(c(rep_len(1:nshifts, length(possible.sfts$start)), possible.sfts$end), byrow=FALSE, ncol=2)
  changeovers[idx1] = changeovers[idx1] + 1  
  changeovers[idx2] = changeovers[idx2] + 1
  
  colnames(sfts) <- seq(1,nperiods)
  colnames(changeovers) <- seq(1,nperiods)
  
  return(list(shifts=sfts, changeover=changeovers, cost=rep(sft.len/60*sft.days,nshifts)))

}


shifts$plot_weekly_shift_gantt <- function(shift.summary, shift.matrix, period.stagger, myorigin = "2018-12-9 00:00:00") {
  
  
  # Convert to long format
  shifts.list = list()
  i = 1
  for (r in 1:nrow(shift.summary)) {
    
    res = shifts$extract_start_ends(shift.matrix[r,])
    se = res$se
    first = res$f
    
    new.shifts = lapply(seq_along(se), function(j) {
      x <- se[[j]]
      x['type'] = shift.summary[r,type]
      x['shift'] = i
      x['length'] = x$end - x$start
      x['p'] = j
      x['first'] = first
      return(x)
    })
    i <- i + 1
    
    shifts.list = append(shifts.list, new.shifts)
    
  }
  
  label_maker <- function(shift, first, type) {
    l = NULL
    if(type == 'week_247') {
      l = as.character(shift)
    } else if(type == 'week_12hr') {
      l = paste0(shift, ' - ', strftime(first, format="%H:%M", tz="UTC"))
    } else {
      l = paste0(shift, ' - ', strftime(first, format="%a %H:%M", tz="UTC"))
    }
    l
  }
  
  shifts.long = bind_rows(shifts.list)
  shift.order = shifts.long %>% group_by(shift) %>% summarise(l=sum(length), s=min(start)) %>% arrange(desc(l), s, shift) %>% pull(shift)
  xlabs = shifts.long %>% group_by(shift, type) %>% 
    summarise(f=as.POSIXct(min(first)*period.stagger*60, origin=myorigin, tz="UTC")) %>% rowwise() %>%
    mutate(f2=label_maker(shift, f, type)) %>% pull(f2)
  xlabs = xlabs[shift.order]

  nperiods = max(shifts.long$end)
  x.breaks = seq(0.5,nperiods,by=1)
  
  lims = as.POSIXct(c(0,(nperiods*period.stagger*60)), origin=myorigin, tz="UTC")
  
  sl = shifts.long %>%
    mutate(shift.group=paste0(shift,'_period',p)) %>% mutate(shift=factor(shift, levels=shift.order)) %>%
    mutate(start=as.POSIXct(start*period.stagger*60, origin=myorigin, tz="UTC"),
           end=as.POSIXct(end*period.stagger*60, origin=myorigin, tz="UTC"))
  
  sl %<>% gather(date.type,date.time,start:end) %>% arrange(date.time)
  
  
  p = sl %>%
  ggplot(aes(x=shift,y=date.time,group=shift.group)) +
    geom_line(aes(color=type), size=2, alpha=.4) +
    geom_vline(xintercept=x.breaks, colour="grey80", linetype="dotted") + 
    labs(y=NULL, x=NULL) + coord_flip(ylim=lims, expand=FALSE, clip="on") +
    #labs(y=NULL, x=NULL) + coord_flip(ylim=lims, expand=FALSE, clip="on") +
    scale_x_discrete(labels=xlabs) + 
    scale_y_datetime(date_labels = "%A %H:%M", date_breaks='6 hour', limits=lims) +
    #scale_color_brewer(palette="Dark2") +
    scale_color_viridis(discrete=TRUE) +
    shifts$ggplot2_theme() + theme(axis.text.x=element_text(angle=45, hjust=1), axis.text.y=element_text(margin=margin(0,20,0,20))) +
    ggtitle('Weekly Shift Gantt Chart')
    
  # Count starting units
  counts = tibble(period=1:nperiods)
  counts = shifts.long %>% count(start) %>% rename(period=start, start=n) %>% right_join(counts, by="period")
  counts = shifts.long %>% count(end) %>% rename(period=end, end=n) %>% right_join(counts, by="period")
  counts = counts %>% replace_na(list(start=0, end=0))
  
  # For the plot, shifts that span the beyond the end week will have an artifical start/end on the first 
  # and last period so that the block gets split properly
  # I assumes that no shift actually starts or ends at the first or last period in the week - a simiplifying assumption so i do
  # not need to identify which shifts wrap, currently safe since origin is midnight and shift window is 5-22hrs.
  counts = counts %>% mutate(start = case_when(period == 1 | period == max(period) ~ 0, TRUE ~ start)) %>% 
    mutate(end = case_when(period == 1 | period == max(period) ~ 0, TRUE ~ end))
  counts = counts %>% mutate(dt=strftime(as.POSIXct(period*period.stagger*60, origin=myorigin, tz="UTC"), format = "%a %H:%M"))

  
  
  num.units = max(apply(crews.bl$shift.matrix, 2, sum))
  
  text <- paste0("Number of units required to implement shift: ", num.units, sep = " ")
  text.p <- ggparagraph(text = text, size = 11, color = "black")
  
  # Arrange the plots on the same page
  ggarrange(p, text.p, 
            ncol = 1, nrow = 2,
            heights = c(1, .1))
  
  
  return(p)
}

shifts$ggplot2_theme <- function(base_size=11) {
    ret <- theme_bw(base_size) %+replace%
    theme(panel.background = element_rect(fill="#ffffff", colour=NA),
          axis.title.x = element_text(vjust=-0.2), axis.title.y=element_text(vjust=1.5),
          title = element_text(vjust=1.2),
          panel.border = element_blank(), axis.line = element_blank(),
          panel.grid.minor = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.major.x = element_line(size=0.5, colour="grey80"),
          axis.ticks = element_blank(),
          legend.position = "bottom", 
          axis.title = element_text(size=rel(0.8)),
          strip.text = element_text(size=rel(1)),
          strip.background=element_rect(fill="#ffffff", colour=NA),
          panel.spacing.y = unit(1.5, "lines"),
          legend.key = element_blank())
  
  ret
}

shifts$extract_start_ends <- function(arow) {
  runs = rle(arow)
  
  start.ends <- list()
  s = 1
  i = 1
  for (r in 1:length(runs$lengths)) {
    if (runs$values[r] == 1) {
      # Add daily shift
      start.ends[[i]] = list(start=s, end=s+runs$lengths[r])
      i = i + 1
    }
    
    s = s + runs$lengths[r]
  }
  
  if(any(runs$values == 0)) {
    first = which.max(runs$lengths[runs$values == 0])
    if(start.ends[[1]]$start == 1) {
      first = first + 1
    }
    if(first > length(start.ends)) {
      first = 1
    } 
  } else {
    first = 1
  }
  
  return(list(se=start.ends, f=as.numeric(start.ends[[first]]$start)))
}

shifts$plot_shifts_vs_demand <- function(shift.summary, shift.matrix, period.stagger, demand.table, myorigin = "2018-12-9 00:00:00") {
  
  np = dim(shift.matrix)[2]
  est = apply(shift.matrix, 2, sum)
  req = demand.table$s
  
  stopifnot(length(req) == np)
  
 
  shifts = data.table(p=1:np, ts=as.POSIXct(0:(np-1)*period.stagger*60, origin=myorigin, tz="UTC"), 
                      Scheduled=est, Required=req)
  
  lims = as.POSIXct(c(0,(np*period.stagger*60)), origin=myorigin, tz="UTC")
  
  p = shifts %>%
    ggplot(aes(x=ts, y=Required)) +
    geom_line(aes(linetype='Required')) +
    geom_line(aes(x=ts, y=Scheduled, linetype='Scheduled')) +
    geom_ribbon(aes(ymin = Required, ymax = pmin(Scheduled, Required), fill = "Overcapacity "), alpha=0.5) +
    geom_ribbon(aes(ymin = Scheduled, ymax = pmin(Scheduled, Required), fill = "Undercapacity "), alpha=0.5) +
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
    ggtitle('Scheduled Units vs Required Units')
  
  return(p)
}

shifts$evaluationSummary <- function(shift.matrix, period.stagger, demand.table) {
  
  np = dim(shift.matrix)[2]
  est = apply(shift.matrix, 2, sum)
  req = demand.table$s
  diff = est-req
  overcap = diff < 0
  hrs = period.stagger / 60
  
  results = list()
  results[['Total']] = sum(est) * hrs
  results[['O.Total']] = sum(diff[overcap]) * hrs
  results[['O.Mean']] = mean(diff[overcap]) * hrs
  results[['O.RMSE']] = sqrt(mean(diff[overcap]^2)) * hrs
  results[['U.Total']] = sum(diff[!overcap]) * hrs
  results[['U.Mean']] = mean(diff[!overcap]) * hrs
  results[['U.RMSE']] = sqrt(mean(diff[!overcap]^2)) * hrs
  
  return(results)
}

shifts$trim <- function(dt) {
  #'Remove first and last day from dataset
  #'
  #'@param dt data.table with columns:
  #'  \describe{
  #'    \item{window}{POSIXct}
  #'@returns data.table
  
  firstday = min(dt$window)
  lastday = max(dt$window)
  dt = dt[as.IDate(window) != as.IDate(firstday) & as.IDate(window) != as.IDate(lastday)]
}

shifts$performance <- function(dt) {
  #'Compute performance metrics for a time series consisting of staffed units (staffed) vs required units (required)
  #'
  #'@param dt data.table with columns:
  #'  \describe{
  #'    \item{ts}{POSIXct}
  #'    \item{staffed}{double}
  #'    \item{required}{double}
  #'  }
  #'@returns list
  
  hrs = as.double(difftime(dt$window[2], dt$window[1], units='hours'))
  dt[,`:=`(ut=required/staffed, av=staffed-required)]
  delta = dt$av
  overcap = delta < 0
  
  mymetrics <- function(v){
    return(list(
      max=max(v),
      min=min(v),
      p50=quantile(v,.5),
      p90=quantile(v,.9),
      mean=mean(v),
      sd=sd(v),
      tot=sum(v),
      rmse=sqrt(mean(v^2))
    ))
  }
  
  results = list()
  results[['StaffedTotalHrs']] = sum(dt$staffed) * hrs
  results[['RequiredTotalHrs']] = sum(dt$required) * hrs
  results[['UnitAvailableTotalHrs']] = sum(!overcap) * hrs
  results[['UnitAvailableProportion']] = sum(!overcap) / length(overcap)
  results[['Availability']] = mymetrics(delta)
  results[['OvercapacityUnitHrs']] = mymetrics(abs(delta[overcap])*hrs)
  results[['UndercapacityUnitHrs']] = mymetrics(delta[!overcap]*hrs)
  results[['Utilization']] = mymetrics(dt$ut)
  
  return(results)
}

shifts$weekly_bin <- function(dt, duration.in.min) {
  #' Assign grouping bins based on day of week and period in day
  #' 
  #' @param dt data.table with window column (POSIXct)
  #' @param duration.in.min Number of minutes in measurement period
  #' @return data.table with numeric weekly.bin and daily.bin columns

  
  dt[,`:=`(
    weekly.bin=wday(window), 
    daily.bin=as.numeric(window-floor_date(window,"day")) %/% (duration.in.min*60)
  )]
}

shifts$weekly_aggregate <- function(dt, duration.in.min) {
  #' Compute weekly average for all metrics in timeseries data.table
  #' 
  #' @param dt data.table with window column (POSIXct)
  #' @param duration.in.min Number of minutes in measurement period
  #' @return data.table with numeric weekly.bin and daily.bin columns
  
  cn = colnames(dt)
  dt = data.table::copy(dt)
  dt =shifts$weekly_bin(dt, duration.in.min)
  
  
  val.cols = cn[!cn %in% c('window', 'weekly.bin', 'daily.bin')]
  
  dtnew <- dt[,(val.cols) := lapply(.SD, as.double), .SDcols=val.cols]
  
  colnms = paste(val.cols, 'mean', sep='.')
  dtnew[, (colnms) := lapply(.SD, mean), by=.(weekly.bin, daily.bin), .SDcols=val.cols]
  
  colnms = paste(val.cols, 'p50', sep='.')
  dtnew[, (colnms) := lapply(.SD, median), by=.(weekly.bin, daily.bin), .SDcols=val.cols]
  
  colnms = paste(val.cols, 'p90', sep='.')
  dtnew[, (colnms) := lapply(.SD, quantile, .9), by=.(weekly.bin, daily.bin), .SDcols=val.cols]
  
  colnms = paste(val.cols, 'sd', sep='.')
  dtnew[, (colnms) := lapply(.SD, sd), by=.(weekly.bin, daily.bin), .SDcols=val.cols]
  
  dtnew[, UnitAvailableProportion := sum(av>0)/.N, by=.(weekly.bin, daily.bin)]
  
  # Pick a Sunday start point
  st = min(dtnew$window[ lubridate::wday(dtnew$window) == 1 &
                         lubridate::hour(dtnew$window) == 0 &
                         lubridate::minute(dtnew$window) == 0])
  en = st + lubridate::days(7)
  
  weekly = dtnew[window >= st & window < en]
  setkeyv(weekly, c("weekly.bin", "daily.bin"))
  
  return(weekly)
}



shifts$shift_join <- function(shifts, demand) {
  #' Take a shift count matrix and right joins it with the demand matrix
  #' 
  #' Fills in missing values between max and min timestamps on demand matrix.
  #' 
  #' @param shifts data.table with window (POSIXct) and count (numeric) columns
  #' @param demand data.table with window and count columns
  #' @return a data.table with window (POSIXct), staffed (numeric), and required (numeric) columns
  
  ts.min = min(demand$window)
  ts.max = max(demand$window)
  delta = difftime(demand$window[2], demand$window[1], units='mins')
  
  ts=seq(ts.min, ts.max, by=delta)
  
  dt = data.table(window=ts)
  setkey(dt, window)
  
  dt = merge(dt, demand, all.x=TRUE)
  dt = dt[,.(window, required=count)]
  dt[is.na(required)] = 0
  
  dt = merge(dt, shifts, all.x=TRUE)
  dt = dt[,.(window, required, staffed=count)]
  dt[is.na(staffed)] = 0
  
  return(dt)
}

shifts$fill <- function(dt, start, end) {
  #' Take weekly data and expand to timeseries defined by start and end
  #' 
  #' 
  #' @param dt data.table with window (POSIXct) and count (numeric)
  #' @param start POSIXct start timestamp
  #' @param end POSIXct end timestamp
  #' @return data.table with window and count columns
  
  
  delta = difftime(dt$window[2], dt$window[1], units='mins')
  stopifnot(nrow(dt) == 7*24*60/as.numeric(delta))
  ts=seq(start, end, by=delta)
  
  setorder(dt, window)
  n = nrow(dt)
  
  i = dt[wday(window) == wday(start) & as.ITime(window) == as.ITime(start), which=TRUE]
  stopifnot(i < n & i > 1)
  neworder = c(i:n, 1:(i-1))
  
  dt = dt[neworder]
  
  res = data.table(window=ts, count=rep_len(dt$count, length(ts)))
  
  return(res)
}

shifts$convert_shift_matrix <- function(sm, origin, duration.in.min) {
  #' Convert the `$shift.matrix` format to a data.table of concurrent unit counts
  #' 
  #' @param sm matrix with 1/0 in each column period that unit is active
  #' @param origin POSIXct object indicating the datetime of the first column
  #' @param duration.in.in Integer indicating the length in minutes for each period
  
  
  
  np = ncol(sm)
  periods = as.POSIXct(seq(0,(np*duration.in.min*60), by=duration.in.min*60), origin=origin, tz="UTC")
  periods = periods[1:(length(periods)-1)]
  counts = apply(sm, 2, sum)
  
  stopifnot(length(periods) == length(counts))
  
  return(data.table(window=periods, counts=counts))
  
}

