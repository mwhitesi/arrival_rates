# Name: shifts.R
# Desc: Functions for optimizing shifts given daily requirments
# Author: Matt Whiteside
# Date: Nov 7, 2018


library(data.table)

shifts <- new.env()

shifts$shift_options <- function(shift.types, period.stagger=15, period.days=7) {
  # Build matrix of possible shift arrangements and the daily periods that each shift covers
  
  sfts = list()
  sft.info = list()
  i=1
  for(s in shift.types) {
   
    tmp = shifts$encode_shift(s, period.stagger=period.stagger, period.days=period.days)

    sfts[[i]] = tmp$shifts
    sft.info[[i]] = tmp$shift.info
    i = i+1
  }
  
  sfts = do.call(rbind, sfts)
  sft.info = rbindlist(sft.info)
  
  return(list(shifts=sfts, shift.info=sft.info))
  
}

shifts$encode_shift <- function(shift.type, period.stagger=15, period.days=7) {
  
  switch(shift.type,
         'week_247'={
           sfts = shifts$encode_247(period.stagger, period.days)
         },
         'week_12hr'={
           sfts = shifts$encode_week(12*60, period.stagger, period.days)
         },
         '4day_10.5hr'={
           sfts = shifts$encode_contiguous_days(10.5*60, 4, period.stagger, period.days)
         },
         stop('Unrecognized shift type')
  )
  
  sft.info = data.table(start=sfts$starts, costs=sfts$costs)
  sft.info[,`:=`(type=shift.type)]
  
  #print(sft.info)
  
  return(list(shifts=sfts$shifts, shift.info=sft.info))
}

shifts$encode_247 <- function(period.stagger=15, period.days=7) {
  # 24/7 shift
  nperiods = 24*60*period.days/period.stagger
  
  sfts = matrix(1,ncol=nperiods,nrow=1)
  colnames(sfts) <- seq(1,nperiods)
  
  return(list(shifts=sfts, starts=c(1), costs=c(24)))

}

shifts$encode_week <- function(sft.len, period.stagger=15, period.days=7) {
  # Start same time each day
  
  nperiods = 24*60*period.days/period.stagger
  period.len = sft.len / period.stagger
  sft.periods = seq(1,(24*60/period.stagger))
  day.stagger = 24*60/period.stagger
  
  sfts = matrix(0, ncol=nperiods, nrow=length(sft.periods))
  
  for(p in sft.periods) {
    
    for(d in 0:6) {
      start = p+(d*day.stagger)
      
      if(start > nperiods) {
        start = start %% nperiods
      }
      
      end = start+period.len-1
      
      if(end > nperiods) {
        end2 = end %% nperiods
        sfts[p, 1:end2] = 1
        end = nperiods
      }
      
      sfts[p, start:end] = 1
    }

  }
  
  colnames(sfts) <- seq(1,nperiods)
  starts=sft.periods
  
  return(list(shifts=sfts, starts=starts, costs=rep(12,length(starts))))
}

shifts$encode_contiguous_days <- function(sft.len, sft.days, period.stagger=15, period.days=7) {
  # Start same time every day for d consecutive days
  
  stopifnot(sft.days < period.days)

  nperiods = 24*60*period.days/period.stagger
  period.len = sft.len / period.stagger
  sft.periods = seq(1,nperiods)
  day.stagger = 24*60/period.stagger
  
  sfts = matrix(0, ncol=nperiods, nrow=length(sft.periods))

  for(p in sft.periods) {
    
    for(d in 0:(sft.days-1)) {
      start = p+(d*day.stagger)
      
      if(start > nperiods) {
        start = start %% nperiods
      }
      
      end = start+period.len-1

      if(end > nperiods) {
        end2 = end %% nperiods
        sfts[p, 1:end2] = 1
        end = nperiods
      }

      sfts[p, start:end] = 1
    }
  }
  
  colnames(sfts) <- seq(1,nperiods)
  starts=sft.periods
  
  return(list(shifts=sfts, starts=starts, costs=rep(10.5,length(starts))))

}


shifts$plot_weekly_shifts <- function(shift.summary, shift.periods, period.stagger, myorigin = "2018-12-9 00:00:00") {
  
  
  # Convert to long format
  shifts.long = list()
  i = 1
  for(r in 1:nrow(shift.summary)) {
    n=shift.summary[r,n]
    for(j in 1:n) {
      se = shifts$extract_start_ends(shift.periods[r,])
      
      new.shifts = lapply(se, function(x) {
        x['type']=shift.summary[r,type]
        x['shift']=i
        return(x)
      })
      i <- i+1
      
      shifts.long = append(shifts.long, new.shifts)
    }
  }
  
  shifts.long = bind_rows(shifts.long)
  shifts.long %<>% arrange(start)
  
  nperiods = max(shifts.long$end)
  x.breaks = seq(0.5,nperiods,by=1)
  
  lims = as.POSIXct(c(0,(nperiods*period.stagger*60)), origin=myorigin, tz="UTC")
  
  sl = shifts.long %>% mutate(start.ts=seconds_to_period(start*period.stagger*60)) %>% mutate(day=start.ts@day) %>% 
    mutate(shift.group=paste0(shift,'_day',day)) %>% mutate(shift=as.factor(shift)) %>%
    mutate(start=as.POSIXct(start*period.stagger*60, origin=myorigin, tz="UTC"),
           end=as.POSIXct(end*period.stagger*60, origin=myorigin, tz="UTC"))
  
  sl %>% gather(date.type,date.time,start:end) %>%
  p = ggplot(aes(x=shift,y=date.time,group=shift.group)) +
    geom_line(size=4, alpha=.4) +
    geom_vline(xintercept=x.breaks, colour="grey80", linetype="dotted") + 
    labs(y=NULL, x=NULL) + coord_flip() +
    scale_y_datetime(date_labels = "%A %H:%M", date_breaks='6 hour', limits=lims) +
    shifts$ggplot2_theme() + theme(axis.text.x=element_text(angle=45, hjust=1))
    
  
  return(p)
}

shifts$ggplot2_theme <- function(base_size=11) {
    ret <- theme_bw(base_size) %+replace%
    theme(panel.background = element_rect(fill="#ffffff", colour=NA),
          axis.title.x=element_text(vjust=-0.2), axis.title.y=element_text(vjust=1.5),
          title=element_text(vjust=1.2),
          panel.border = element_blank(), axis.line=element_blank(),
          panel.grid.minor=element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.major.x = element_line(size=0.5, colour="grey80"),
          axis.ticks=element_blank(),
          legend.position="bottom", 
          axis.title=element_text(size=rel(0.8)),
          strip.text=element_text(size=rel(1)),
          strip.background=element_rect(fill="#ffffff", colour=NA),
          panel.spacing.y=unit(1.5, "lines"),
          legend.key = element_blank())
  
  ret
}

shifts$extract_start_ends <- function(arow) {
  runs = rle(arow)
  
  start.ends <- list()
  s=0
  i=1
  for(r in 1:length(runs$lengths)) {
    if(runs$values[r] == 1) {
      # Add daily shift
      start.ends[[i]] = list(start=s, end=s+runs$lengths[r])
      i = i+1
    }
    
    s = s+runs$lengths[r]
  }
  
  start.ends
}

shifts$plot_units_vs_demand <- function(shift.summary, shift.periods, period.stagger, demand.table, myorigin = "2018-12-9 00:00:00") {
  
  shifts.long = list()
  i = 1
  for(r in 1:nrow(shift.summary)) {
    n=shift.summary[r,n]
    for(j in 1:n) {
      se = shifts$extract_start_ends(shift.periods[r,])
      shifts.long = append(shifts.long, se)
    }
  }
  
  shifts.long = bind_rows(shifts.long)
  shifts.long %<>% arrange(start)
  
  
  
  nperiods = max(shifts.long$end)
  
}

