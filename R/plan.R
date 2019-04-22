# Name: plan.R
# Desc: Arrival rates plan
# Author: Matt Whiteside
# Date: Nov 19, 2018

# Load sources
source('R/packages.R')
source('R/timeutils.R')
source('R/dataload.R')
source('R/queuemodel.R')
source('R/modeltest.R')
source('R/tsregression.R')
source('R/performance.R')
source('R/shifts.R')
source('R/shiftopt.R')


start.date = '2017-11-01'
duration.in.min = 15

# Load historical data
dir.create('data/interim/dataload/', showWarnings=FALSE)

data_plan <- drake_plan(
  resp = dataload__loadEventData("data/raw/EDMO_Metro_Response_Extract/Report 1.csv"),
  histU = dataload__loadUNIT_WKLOAD("data/raw/EDMO_unit_workload.csv", duration.in.min, do.plot=TRUE),
  fundU = dataload__loadProvProfile("data/raw/Profile_2019-01-25.csv", duration.in.min, do.plot=TRUE),
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
  ar_summary = queuemodel__arrivalSummary(ar2, "data/processed/EDMO_arrival_rates__2017-11-01_2018-11-01.csv"),
  st_summary = queuemodel__serviceTimeSummary(st2, "data/processed/EDMO_service_time__2017-11-01_2018-11-01.csv"),
  st_ar_corr = queuemodel__correlationSummary(st_summary, ar_summary),
  st_pvals = queuemodel__trends(ar2, st2),
  qmodU = queuemodel__model(ar2, st2, duration.in.min, service.level, ar.lag, st.lag, load.threshold, do.plot=FALSE)
)

pred.int = .991 # Prediction interval to consider when predicting required number of units
regressionmodel_plan <- drake_plan(
  rmodU = tsregression__fitFFTModel(demand, duration.in.min, pred.int),
  experiment.mod1 = tsregression__fitFFTModel(demand, duration.in.min, 0.95735),
  experiment.mod2 = tsregression__fitFFTModel(demand, duration.in.min, 0.97311)
)

# Compare required shift estimates - TS regression vs Queue model
dir.create('data/interim/comparemodels/', showWarnings=FALSE)
comparemodels_plan <- drake_plan(
  modelreport = rmarkdown::render(
    knitr_in("R/comparemodels.Rmd"),
    output_file = "data/interim/comparemodels/unit_requirement_modelling_v1.html",
    quiet = TRUE
  )
)


# Allowable shift types
blockshift.setup <- list(
  shift.types = c('week_12hr', '4day_10.5hr', '2day_10.5hr'), # Allowable shift types
  period.stagger = duration.in.min, # Time increment that shifts can start
  period.days = 7, # Planning period length in days
  daily.sc.window = c(5*60,20*60), # Window that a shift change can occur in minutes
  max.sc = 4 # Max number of start/stops in any time block
)
freeshift.setup <- list(
  shift.types = c('12hr', '10.5hr', '8.4hr'),
  period.stagger = duration.in.min,
  period.days = 7,
  daily.sc.window = c(5*60,20*60),
  max.sc = 4
)
solver <- 'symphony'
shifts_plan <- drake_plan(
  crewsQmBl = shiftopt__optimize(qmodU, blockshift.setup, solver),
  crewsQmFr = shiftopt__optimize(qmodU, freeshift.setup, solver),
  crewsRmBl = shiftopt__optimize(rmodU, blockshift.setup, solver),
  crewsRmFr = shiftopt__optimize(rmodU, freeshift.setup, solver),
  crewsExBl = shiftopt__optimize(experiment.mod1, blockshift.setup, solver),
  crewsExFr = shiftopt__optimize(experiment.mod2, freeshift.setup, solver)
)




# Compare optimized shifts vs existing shift patterns
dir.create('data/interim/compareshifts/', showWarnings=FALSE)
# comparemodels_plan <- drake_plan(
#   modelreport = rmarkdown::render(
#     knitr_in("R/comparemodels.Rmd"),
#     output_file = file_out("data/interim/compare/unit_requirement_modelling.html"),
#     quiet = TRUE
#   )
# )

whole_plan <- bind_plans(data_plan, queuemodel_plan, regressionmodel_plan, comparemodels_plan, shifts_plan)

#drake_cache_log_file(file = "drake_cache_log.txt")


script.dir <- getwd()
ff <- file.path(script.dir, "data/interim/compareshifts/shift_comparison_v1.html")
rmarkdown::render(
  "R/compareshifts.Rmd",
  output_file = ff
)

# Save shift matrices
myorigin = '2017-10-01 00:00:00'
fundDT = dataload__loadProvProfileMatrix("data/raw/Profile_2019-01-25.csv", myorigin)
blockResult = drake::readd(crewsRmBl)
ts = seq(as.POSIXct(myorigin, tz="UTC"), as.POSIXct(myorigin, tz="UTC")+7*24*60*60, by="15 mins")
ts = ts[-length(ts)]
blSM = blockResult$shift.matrix
blDT = data.table(seq(1:nrow(blSM)), blSM)
setnames(blDT, c("",as.character(ts)))

fwrite(fundDT, file='data/interim/funded_unit_shift_matrix.csv')
fwrite(blDT, file='data/interim/optimized_block_shift_matrix.csv')
