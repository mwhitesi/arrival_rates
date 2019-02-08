# Name: plan.R
# Desc: Arrival rates plan
# Author: Matt Whiteside
# Date: Nov 19, 2018


start.date = '2017-11-01'
duration.in.min = 15

# Load historical data
dir.create('data/interim/dataload/', showWarnings=FALSE)
data_plan <- drake_plan(
  resp = dataload$loadEventData(file_in("data/raw/EDMO_Metro_Response_Extract/Report 1.csv")),
  histU = dataload$loadUNIT_WKLOAD(file_in("data/raw/EDMO_unit_workload.csv"), duration.in.min, do.plot=TRUE),
  fundU = dataload$loadProvProfile(file_in("data/raw/Profile_2019-01-25.csv"), duration.in.min, do.plot=TRUE)
)

# Queue model
dir.create('data/interim/queuemodel/', showWarnings=FALSE)
service.level = 0.001  # probability of waiting for unit to become available for new events
  
queuemodel_plan <- drake_plan(
  ar = queuemodel$findOutliers(resp, "arrival rates", start.date),
  st = queuemodel$findOutliers(resp, "service time", start.date),
  ar2 = queuemodel$addTimeColumns(ar),
  st2 = queuemodel$addTimeColumns(st),
  ar_summary = queuemodel$arrivalSummary(ar2, file_out("data/processed/EDMO_arrival_rates__2017-11-01_2018-11-01.csv")),
  st_summary = queuemodel$serviceTimeSummary(st2, file_out("data/processed/EDMO_service_time__2017-11-01_2018-11-01.csv")),
  st_ar_corr = queuemodel$correlationSummary(st_summary, ar_summary),
  st_pvals = queuemodel$trends(ar2, st2)
  #servs = modelTarget(ar2, st2)
  
)

whole_plan <- bind_plans(data_plan, queuemodel_plan)



# forecast_targets.R
forecast_plan <- drake_plan(
  
)

# historical_targets.R
historical_plan <- drake_plan(
  hd = demandTarget(dt1),
  hu = loadUN_WKLOAD('data/raw/EDMO_unit_workload.csv', duration.in.min)
)

# shift_targets.R
# Allowable shift types
shift.setup <- list(
  shift.types = c('week_12hr', '4day_12hr', '4day_10.5hr', '2day_12hr', '2day_10.5hr'),
  period.stagger = duration.in.min,
  period.days = 7,
  daily.sc.window = c(5*60,20*60),
  max.sc = 4
)
solver <- 'symphony'
shifts_plan <- drake_plan(
  crews = optimumTarget(servs, shift.setup, solver)
  
)


whole_plan <- bind_plans(data_plan, analysis_plan, forecast_plan, historical_plan, shifts_plan)
