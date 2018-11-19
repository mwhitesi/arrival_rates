# Name: data_prep.R
# Desc: Load and clean data. Add utility columns
# Author: Matt Whiteside
# Date: Nov 19, 2018

cleanData <- function(dt) {
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
  
  return(dt)
}


# Steps to find outliers:

convertToTbl <- function(dt) {
  cols <- c("Event.Number","start","interval_length", "Event.Datetime")
  events_tbl = ts[,mget(cols)] %>% as_tibble() %>% mutate(start = as_datetime(start)) %>% 
    as_tbl_time(start) %>% arrange(start)
  
  return(events_tbl)
}

computeArrivalRates <- function(tbl) {
  ar_tbl = tbl %>% collapse_by("hourly", start_date="2017-01-01", clean=T, side="start") %>% 
    group_by(start) %>% summarise('arrival_rate'=n())
  return(ar_tbl)
}

computeServiceTime <- function(tbl) {
  tt_tbl = tbl %>% collapse_by("hourly", start_date="2017-01-01", clean=T, side="start")
  return(tt_tbl)
}

findTSAnomalies <- function(tbl) {
  # Find outliers using sliding window on time series data
  # Adds columns to tbl
  anomly_tbl <- tbl %>%
    time_decompose(tbl, 
                   frequency='1 week',
                   trend='1 month',
                   merge = TRUE) %>%
    anomalize(remainder) %>%
    time_recompose()
  
  return(anomly_tbl)
}

outlier_plan <- drake_plan(
  tbl = convertToTbl(dataset__),
  arrival_rates = computeArrivalRates(tbl),
  service_time = computeServiceTime(tbl),
  st_outliers = findSTAnomalies(service_time),
  ar_outliers = findSTAnomalies(arrival_rates),
  ar_outlier_plot = ar_outliers %>% plot_anomaly_decomposition(),
  st_outlier_plot = st_outliers %>% plot_anomaly_decomposition()
  
)

