# Here, load the packages you need for your workflow.

library(drake)
library(ggplot2)
library(data.table)
library(lubridate)
library(tidyr)
library(tidyverse)
library(anomalize)
library(tibbletime)


pkgconfig::set_config("drake::strings_in_dots" = "literals")
