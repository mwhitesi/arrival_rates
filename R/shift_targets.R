# Name: shift_targets.R
# Desc: Pick shifts to meet demand
# Author: Matt Whiteside
# Date: Dec 11, 2018

optimumTarget <- function(required.servers, shift.types) {
  
  # Available shifts
  a = shifts$shift_options(shift.types)
  
  # Requirements
  r = NULL
  
}