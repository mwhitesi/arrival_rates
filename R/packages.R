# Here, load the packages you need for your workflow.

library(drake)
library(MASS)
library(purrr)
library(plyr)
library(ggplot2)
library(data.table)
library(lubridate)
library(tidyr)
library(tidyverse)
library(anomalize)
library(tibbletime)
library(zoo)
library(magrittr)
library(fitdistrplus)
library(queueing)
library(ROI)
library(ROI.plugin.glpk)
library(ompr)
library(ompr.roi)
library(gridExtra)



pkgconfig::set_config("drake::strings_in_dots" = "literals")
