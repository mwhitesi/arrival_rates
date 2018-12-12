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
