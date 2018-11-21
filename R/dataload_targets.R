# Name: dataload_targets.R
# Desc: Load and clean data. Add utility columns
# Author: Matt Whiteside
# Date: Nov 19, 2018

## Data Munging

loadDataTarget <- function(file.in) {
  
  # Load data
  dt = fread(file.in, check.names = T)
 
  # Filter unusable data rows
  
  # Remove odd event categories such as "Training". These use the term "Other"
  dt = dt[!grepl("Other", Event.Stream)]

  # Don't count units that are not dispatched
  dt = dt[Unit.Dispatch.TS != ""]
  
  # Don't count units that never clear (usually these have no Even.Close.TS)
  dt = dt[Unit.Clear.TS != ""]
  
  # Remove anything but ALS and BLS units
  dt = dt[Unit.Type == "ALS" | Unit.Type == "BLS"]
  
  # Units must have at least one of these actionable TS
  dt = dt[Unit.Onscene.TS != "" | "Unit.Transport.TS" != ""]
  
  # Add time columns
  timed_data = timeutils$bin_events_by_time(dt, "Event.Datetime", duration=60, period="week", endcol="Unit.Clear.TS")
  
  return(timed_data)
}


## Outliers

convertToTbl <- function(ts) {
  cols <- c("Event.Number","start","interval_length", "Event.Datetime", 
            "Stream")
  tmp = ts[,'Stream' := paste0(Event.Stream, '-', ifelse(Unit.Transported.Flag == 'N', "No_Transport", "Transported"))]
  events_tbl = tmp[,mget(cols)] %>% as_tibble() %>% mutate(start = as_datetime(start)) %>% 
    as_tbl_time(start) %>% arrange(start)
  
  return(events_tbl)
}

computeArrivalRates <- function(tbl) {
  ar_tbl = tbl %>% collapse_by("hourly", start_date="2017-01-01", clean=T, side="start") %>% 
    group_by(Stream, start) %>% summarise('arrival_rate'=n())
  return(ar_tbl)
}

computeServiceTime <- function(tbl) {
  tt_tbl = tbl %>% collapse_by("hourly", start_date="2017-01-01", clean=T, side="start")
  return(tt_tbl)
}

findAnomalies <- function(tbl, col) {
  # Find outliers using sliding window on time series data
  # Adds columns to tbl
  anomly_tbl <- tbl %>%
    time_decompose(col, 
                   frequency='1 week',
                   trend='1 month',
                   merge = TRUE) %>%
    anomalize(remainder) %>%
    time_recompose()
  
  return(anomly_tbl)
}

findOutliersTarget <- function(dt, metric) {
  
  tbl <- convertToTbl(dt)
  
  if(metric == "arrival rates") {
    rt <- computeArrivalRates(tbl)
    label <- 'arrival_rates'
    colnm <- 'arrival_rate'
  } else if(metric == "service time") {
    rt <- computeServiceTime(tbl)
    label <- 'service_time'
    colnm <- 'interval_length'
  } else {
    stop(paste('Unknown metric:',metric))
  }
  
  anoms <- findAnomalies(rt, colnm)
  
  # Plot anomalies
  p.decomp = anoms %>% plot_anomalies()
  ggsave(paste0("data/interim/plot_",label,"__anomalies.pdf"), p.decomp, dev="pdf")
 
  # Filter out anomalies
  final = anoms %>% filter(anomaly == "No") %>% select(Stream, start, colnm)
  
  
  return(final)
}


