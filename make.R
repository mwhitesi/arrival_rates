# See the full tutorial at
# https://ropenscilabs.github.io/drake-manual/gsp.html.

source("R/packages.R")  # Load all the packages you need.

# My general purpose functions
source("R/timeutils.R")
source("R/shifts.R")

# Work flow functions
source("R/dataload.R")
source("R/analysis_targets.R")
source("R/forecast_targets.R")
source("R/historical_targets.R")
source("R/shift_targets.R")

# Plan building script
source("R/plan.R")      # Build your workflow plan data frame.


# Optionally plot the graph of your workflow.
# config <- drake_config(whole_plan) # nolint
# vis_drake_graph(config)            # nolint

# Run project
#make(whole_plan)
make(whole_plan, jobs=4)
