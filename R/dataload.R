# Name: dataload_targets.R
# Desc: Load and clean data. Add utility columns
# Author: Matt Whiteside
# Date: Nov 19, 2018

## Data Munging

dataload <- new.env(parent=emptyenv())

dataload$loadEventData <- function(file.in) {
  
  # Load data
  dt = data.table::fread(file.in, check.names = T)
 
  # Filter unusable data rows
  
  # Keep only 911 events
  dt = dt[grepl("911", Event.Stream)]

  # Don't count units that are not dispatched
  dt = dt[Unit.Dispatch.TS != ""]
  
  # Don't count units that never clear (usually these have no Even.Close.TS)
  dt = dt[Unit.Clear.TS != ""]
  
  # Remove anything but ALS and BLS units
  dt = dt[Unit.Type == "ALS" | Unit.Type == "BLS"]
  
  # Units must have at least one of these actionable TS
  dt = dt[Unit.Onscene.TS != "" | "Unit.Transport.TS" != ""]
  
  # Add time columns
  timed_data = timeutils$format_time(dt, "Event.Datetime", endcol="Unit.Clear.TS")
  
  return(timed_data)
}

dataload$log_message <- function(msg) {
  message(msg)
}

dataload$log_table <- function(tbl) {
  apply(tbl, 1, function(r) dataload$log_message(paste0(r, '\t')))
}

dataload$has_continuing_shift <- function(shortrow, long) {
  
  match = long[
    substr(Unit,1,5) == substr(shortrow$Unit,1,5) &    # Same DGROUP
      substring(Unit,8) == substring(shortrow$Unit,8) &  # Same Unit Number
      start > shortrow$end &                             # Starts after
      start <= (shortrow$end + minutes(90)) &            # Starts within 90 mins
      LoggedOn > 4*60*60                                 # Is at least 4 hrs
    ]
  
  return(nrow(match))
}

dataload$loadUNIT_WKLOAD <- function(file, duration.in.min=15, do.plot=TRUE) {
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
  dataload$log_message('Step: filtering specialty units')
  dataload$log_message('Rule: 6th character in Unit Name is 1,2,3,6')
  nr = nrow(dt)
  dataload$log_message(sprintf('Before: %d', nr))
  dt = dt[substr(Unit, 6,6) %in% c(1,2,3,6)]
  dataload$log_message(sprintf('After: %d. Removed: %d', nrow(dt), nr-nrow(dt)))
  
  # Shift time booked for event
  dataload$log_message('Step: filtering special event booked shifts')
  dataload$log_message('Rule: Remove LoggedOn >= 4hrs, OnEvent/LoggedOn > 0.95')
  nr = nrow(dt)
  dataload$log_message(sprintf('Before: %d', nr))
  rows = dt$LoggedOn >= 4*60*60 & dt$OnEvent/dt$LoggedOn > .95
  rem = dt[rows]
  dt = dt[!rows]
  dataload$log_message(sprintf('After: %d. Removed: %d', nrow(dt), nrow(rem)))
  dataload$log_message("\n--START-REMOVED--")
  dataload$log_table(rem)
  dataload$log_message("\n--END-REMOVED--\n\n")
  
  # Short orphan shifts
  dataload$log_message('Step: filtering short orphan shifts')
  dataload$log_message('Rule: Remove LoggedOn <= 1hrs and no >= 4hr LoggedOn record within 90 mins that has same unit ID and DGROUP')
  dt = dt[, `:=`(start = as.POSIXct(Shift_Start_TS, tz="UTC"), end=as.POSIXct(Shift_End_TS, tz="UTC"))]
  long = dt[LoggedOn > 3600]
  short = dt[LoggedOn <= 3600]
  setkeyv(long, c('Unit', 'start'))
  orphans = short %>% rowwise %>% do(x=dataload$has_continuing_shift(., long)) %>% unnest(x) %>% mutate(orphan = x != 1) %>% pull(orphan)
  dt=rbind(long,short[!orphans])
  dataload$log_message(sprintf('After: %d. Removed: %d', nrow(dt), sum(orphans)))
  dataload$log_message("\n--START-REMOVED--")
  dataload$log_table(short[orphans, .(Unit, Shift_Start_TS, Shift_End_TS, LoggedOn, OnEvent)])
  dataload$log_message("\n--END-REMOVED--\n\n")
  
  
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
    
    ggsave(paste0("data/interim/dataload/plot_historical_weekly_logged_on_units.pdf"), p.weekly, dev="pdf", height=8.5, width=11)
    
  }
  
  
  return(window.counts)
}


dataload$loadProvProfile <- function(file, duration.in.min=15, do.plot=TRUE) {
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
  dt = dt[Ops.Zone == "Edmonton" & Town == "Edmonton"]
  
  # Not setup to handle seasonal
  stopifnot(all(is.na(dt[,Seasonal])))
  
  # Extract hours
  mins = seq(0,45,by=15)
  hrs = seq(0,23)
  times = sprintf("%02d.%02d", rep(hrs,each=length(mins)), mins)
  days = c('Sun', 'Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat')
  shift.cols = c(paste(rep(days, each=length(times)), times, sep='.'))
  
  shifts = as.matrix(dt[,shift.cols, with=FALSE])
  
  # Check no gaps in offsets
  offsets = apply(shifts,1,function(x){any(x < 1 & x > 0)})
  noffsets = sum(offsets)
  
  stopifnot(all(dt[offsets, Peak.Split.OFFSET] == 1))
  stopifnot(all(table(dt[offsets, Offset]) == noffsets/2))
  
  
  # Sum coverage
  # Starts on a Sunday at midnight
  myorigin = "2018-12-9 00:00:00"
  ts = seq(as.POSIXct(myorigin), as.POSIXct(myorigin)+7*24*60*60, by="15 mins")
  window.counts = data.table(window=ts[1:length(ts)-1], count=apply(shifts, 2, sum))
  
  stopifnot(all((window.counts$count - floor(window.counts$count)) == 0))
  
  return(window.counts)
}


