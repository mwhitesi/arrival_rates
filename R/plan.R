# Name: plan.R
# Desc: Arrival rates plan
# Author: Matt Whiteside
# Date: Nov 19, 2018

# dataload_targets.R
data_plan <- drake_plan(
  dt1 = loadDataTarget(file_in("data/raw/EDMO_Metro_Response_Extract/Report 1.csv")),
  ar = findOutliersTarget(dt1, "arrival rates"),
  st = findOutliersTarget(dt1, "service time")
)

# analysis_targets.R
analysis_plan <- drake_plan(
  ar_rates = arrivalSummaryTarget(ar, file_out("data/processed/EDMO_arrival_rates__2017-11-01_2018-11-01.csv")),
  st_rates = serviceTimeSummaryTarget(st, file_out("data/processed/EDMO_service_time__2017-11-01_2018-11-01.csv")),
  rates = correlationSummaryTarget(ar_rates, st_rates)
)

# historical_targets.R
historical_plan <- drake_plan(
  hd = demandTarget(dt1)
)

# forecast_targets.R
forecast_plan <- drake_plan(
  trendsTarget(ar, st),
  servs = modelTarget(ar, st)
)

# shift_targets.R
# Allowable shift types
shift.types <- c('week_247', 'week_12hr', '4day_10.5hr')
shifts_plan <- drake_plan(
  optimumTarget(servs, shift.types)
)




whole_plan <- bind_plans(data_plan, analysis_plan, historical_plan, forecast_plan)
