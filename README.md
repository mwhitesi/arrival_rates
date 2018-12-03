# Edmonton Arrival Rates and Time on Task Analysis

## Prereqs:
1. `data/` directory with required raw directory input files.

## Running:
To run the drake plan `source('make.R')`. 

To run jobs in parallel, uncomment:
```
make(whole_plan)
# make(whole_plan, jobs=4)
```
to

```
#make(whole_plan)
make(whole_plan, jobs=4)
```

Calling `source('clean.R')` will empty the drake cache and force all targets to be re-run.


