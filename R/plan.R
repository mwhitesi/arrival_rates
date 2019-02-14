# Name: plan.R
# Desc: Arrival rates plan
# Author: Matt Whiteside
# Date: Nov 19, 2018


start.date = '2017-11-01'
duration.in.min = 15

# Load historical data
dir.create('data/interim/dataload/', showWarnings=FALSE)
data_plan <- drake_plan(
  resp = dataload__loadEventData(file_in("data/raw/EDMO_Metro_Response_Extract/Report 1.csv")),
  histU = dataload__loadUNIT_WKLOAD(file_in("data/raw/EDMO_unit_workload.csv"), duration.in.min, do.plot=TRUE),
  fundU = dataload__loadProvProfile(file_in("data/raw/Profile_2019-01-25.csv"), duration.in.min, do.plot=TRUE),
  demand = dataload__demand(resp, duration.in.min)
)

# Queue model
dir.create('data/interim/queuemodel/', showWarnings=FALSE)
service.level = 0.001  # probability of having to wait for unit to become available for new events
ar.lag=NULL # Set arrival rate lag to default: average service time
st.lag=NULL # Set service time lag to default: average service time
load.threshold=35 # When number of required units drops below this value, a 5% increase is applied to service rate (number of units / duration)
  
queuemodel_plan <- drake_plan(
  ar = queuemodel__findOutliers(resp, "arrival rates", start.date),
  st = queuemodel__findOutliers(resp, "service time", start.date),
  ar2 = queuemodel__addTimeColumns(ar),
  st2 = queuemodel__addTimeColumns(st),
  ar_summary = queuemodel__arrivalSummary(ar2, file_out("data/processed/EDMO_arrival_rates__2017-11-01_2018-11-01.csv")),
  st_summary = queuemodel__serviceTimeSummary(st2, file_out("data/processed/EDMO_service_time__2017-11-01_2018-11-01.csv")),
  st_ar_corr = queuemodel__correlationSummary(st_summary, ar_summary),
  st_pvals = queuemodel__trends(ar2, st2),
  qmodU = queuemodel__model(ar2, st2, duration.in.min, service.level, ar.lag, st.lag, load.threshold, do.plot=TRUE)
)

pred.int = .991 # Prediction interval to consider when predicting required number of units
regressionmodel_plan <- drake_plan(
  
  rmodU = tsregression__fitFFTModel(demand, duration.in.min, pred.int)
)

# Compare required shift estimates - TS regression vs Queue model
dir.create('data/interim/comparemodels/', showWarnings=FALSE)
comparemodels_plan <- drake_plan(
  modelreport = rmarkdown::render(
    knitr_in("R/comparemodels.Rmd"),
    output_file = file_out("R/comparemodels/unit_requirement_modelling.html"),
    quiet = TRUE
  )
)


# # Allowable shift types
# blockshift.setup <- list(
#   shift.types = c('week_12hr', '4day_10.5hr', '2day_10.5hr'),
#   period.stagger = duration.in.min,
#   period.days = 7,
#   daily.sc.window = c(5*60,20*60),
#   max.sc = 4
# )
# solver <- 'symphony'
# shifts_plan <- drake_plan(
#   crews = (servs, shift.setup, solver)
#   
# )

whole_plan <- bind_plans(data_plan, queuemodel_plan, regressionmodel_plan, comparemodels_plan)

drake_cache_log_file(file = "drake_cache_log.txt")


script.dir <- getwd()
ff <- file.path(script.dir, "data/interim/comparemodels/unit_requirement_modelling.html")
rmarkdown::render(
  "R/comparemodels.Rmd",
  output_file = ff
)

