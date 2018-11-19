# Name: plan.R
# Desc: Arrival rates plan
# Author: Matt Whiteside
# Date: Nov 19, 2018

# Data prep plans

# Load & filter
data_plan <- drake_plan(
  
  raw_data = fread(file_in("data/raw/EDMO_Metro_Response_Extract/Report 1.csv"), check.names = T),
  clean_data = cleanData(raw_data),
  timed_data1 = timeutils$bin_events_by_time(clean_data, "Event.Datetime", duration=60, period="week", endcol="Unit.Clear.TS")
)

my_outliers = evaluate_plan(
  outlier_plan,
  wildcard = "dataset__",
  values = c("timed_data1")
)


whole_plan <- bind_plans(data_plan, my_outliers)
