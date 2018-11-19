# See the full tutorial at
# https://ropenscilabs.github.io/drake-manual/gsp.html.

source("R/packages.R")  # Load all the packages you need.

# My general purpose functions
source("R/timeutils.R")

# Work flow functions
source("R/dataprep.R") # Load all the functions into your environment.

# Plan building script
source("R/plan.R")      # Build your workflow plan data frame.

# Now, your functions and workflow plan should be in your environment.
ls()

# Optionally plot the graph of your workflow.
# config <- drake_config(whole_plan) # nolint
# vis_drake_graph(config)            # nolint

# Run project
make(whole_plan)
progress()
