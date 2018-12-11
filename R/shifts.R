# Name: shifts.R
# Desc: Functions for optimizing shifts given daily requirments
# Author: Matt Whiteside
# Date: Nov 7, 2018


library(data.table)

shifts <- new.env()

shifts$shift_options <- function(shift.types, period.stagger=15, period.days=7) {
  # Build matrix of possible shift arrangements and the daily periods that each shift covers
  
  shifts = list()
  shift.info = list()
  i=1
  for(s in shift.types) {
    tmp = shifts$encode_shift(s, period.stagger, period.days)
    
    shifts[i] = tmp$shifts
    shift.info[i] = tmp$shift.info
    i = i+1
  }
  
  shifts = do.call(rbind, shifts)
  shift.info = rbindlist(shift.info)
  
  return(list(shifts=shifts, shift.info=shift.info))
  
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
  
  sft.info = as.data.table(sfts$starts)
  sft.info[,`:=`(type=shift.type)]
  
  return(list(shifts=sfts$shifts, shift.info=shift.info))
}

shifts$encode_247 <- function(period.stagger=15, period.days=7) {
  # 24/7 shift
  nperiods = 24*60*period.days/period.stagger
  
  sfts = matrix(1,ncol=nperiods,nrow=1)
  colnames(sfts) <- seq(1,nperiods)
  
  return(list(shifts=sfts, starts=c(1)))

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
  starts=rep(1,sft.periods)
  
  return(list(shifts=sfts, starts=starts))
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
  starts=rep(1,sft.periods)
  
  return(list(shifts=sfts, starts=starts))

}

