# Name: plan.R
# Desc: Arrival rates plan
# Author: Matt Whiteside
# Date: Nov 19, 2018

# dataload_targets.R
start.date='2017-11-01'
data_plan <- drake_plan(
  dt1 = loadDataTarget(file_in("data/raw/EDMO_Metro_Response_Extract/Report 1.csv")),
  ar = findOutliersTarget(dt1, "arrival rates", start.date),
  st = findOutliersTarget(dt1, "service time", start.date)
)

# analysis_targets.R
analysis_plan <- drake_plan(
  ar2 = addTimeColumns(ar),
  st2 = addTimeColumns(st),
  ar_rates = arrivalSummaryTarget(ar2, file_out("data/processed/EDMO_arrival_rates__2017-11-01_2018-11-01.csv")),
  st_rates = serviceTimeSummaryTarget(st2, file_out("data/processed/EDMO_service_time__2017-11-01_2018-11-01.csv")),
  rates = correlationSummaryTarget(st_rates, ar_rates)
  
)

# historical_targets.R
historical_plan <- drake_plan(
  hd = demandTarget(dt1)
)

# forecast_targets.R
forecast_plan <- drake_plan(
  st_pvals = trendsTarget(ar2, st2),
  servs = modelTarget(ar2, st2)
)

# shift_targets.R
# Allowable shift types
shift.setup <- list(
  shift.types = c('week_247', 'week_12hr', '4day_10.5hr'),
  period.stagger = 15,
  period.days = 7,
  daily.sc.window = c(5*60,20*60)
)
solver <- 'glpk'
shifts_plan <- drake_plan(
  crews = optimumTarget(servs, shift.setup, solver)
  
)


whole_plan <- bind_plans(data_plan, analysis_plan, forecast_plan, historical_plan, shifts_plan)
