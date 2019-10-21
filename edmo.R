# Name: plan.R
# Desc: Arrival rates plan
# Author: Matt Whiteside
# Date: Nov 19, 2018

# Load sources
source('R/packages.R')
source('R/timeutils.R')
source('R/dataload.R')
source('R/modeltest.R')
source('R/queuemodel.R')
source('R/tsregression.R')
source('R/performance.R')
source('R/shifts.R')
source('R/shiftopt.R')


start.date = '2017-11-01'
duration.in.min = 15
myorigin = '2017-10-01 00:00:00'

# Load historical data
data_plan <- drake_plan(
  resp = dataload__loadEventData("data/raw/EDMO_Metro_Response_Extract/Report 1.csv"),
  demand = dataload__demand(resp, duration.in.min),
  fundDT = dataload__loadProvProfileMatrix("data/raw/Profile_2019-01-25.csv", myorigin)
)

# Build demand model
intv.t = .991
intv.b = .8 
# Prediction interval to considers when building optimum number of units: target and baseline
# Prediction interval interpretation: proportion of historical events for which a unit will be available for a given time of day and day of week.
# If problem is infeasible, most likely there are not enough scheduled unit hours to meet the baseline level of service
regressionmodel_plan <- drake_plan(
  tmod = tsregression__fitFFTModel(demand, duration.in.min, intv.t),
  bmod = tsregression__fitFFTModel(demand, duration.in.min, intv.b)
)

# Calculate shifts
freeshift.setup <- list(
  shift.types = c('12hr', '10.5hr', '8.4hr'),
  period.stagger = duration.in.min,
  period.days = 7,
  daily.sc.window = c(5*60,20*60),
  max.sc = 4
)
solver <- 'symphony'
tot.p = sum(fundDT[,-1])
shifts_plan <- drake_plan(
  crews = shiftopt__optimize_with_constraints(tmod, bmod, freeshift.setup, tot.p, solver)
)


whole_plan <- bind_plans(data_plan, regressionmodel_plan, shifts_plan)

drake_cache_log_file(file = "drake_cache_log.txt")


# Save shift matrices
ts = seq(as.POSIXct(myorigin, tz="UTC"), as.POSIXct(myorigin, tz="UTC")+7*24*60*60, by="15 mins")
ts = ts[-length(ts)]
sm = crews$shift.matrix
smDT = data.table(seq(1:nrow(sm)), sm)
setnames(smDT, c("",as.character(ts)))

fwrite(fundDT, file='data/interim/funded_unit_shift_matrix.csv')
fwrite(smDT, file='data/interim/optimized_shift_matrix.csv')
