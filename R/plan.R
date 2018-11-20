# Name: plan.R
# Desc: Arrival rates plan
# Author: Matt Whiteside
# Date: Nov 19, 2018

# Data prep plans

# dataload_targets
data_plan <- drake_plan(
  dt1 = loadDataTarget(file_in("data/raw/EDMO_Metro_Response_Extract/Report 1.csv")),
  ar = findOutliersTarget(dt1, "arrival rates"),
  st = findOutliersTarget(dt1, "service time")
)

analysis_plan <- drake_plan(
  ar_rates = arrivalSummaryTarget(ar, file_out("data/processed/EDMO_arrival_rates__2017-11-01_2018-11-01.csv")),
  st_rates = serviceTimeSummaryTarget(st, file_out("data/processed/EDMO_service_time__2017-11-01_2018-11-01.csv"))
)




whole_plan <- bind_plans(data_plan, analysis_plan)
