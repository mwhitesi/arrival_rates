# Here, load the packages you need for your workflow.

library(drake)
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
library(MASS)


pkgconfig::set_config("drake::strings_in_dots" = "literals")
