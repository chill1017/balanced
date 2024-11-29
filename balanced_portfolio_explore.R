# write a portfolio balancer

library(tidyquant)
library(tidyverse)
library(progress)
library(reshape2)
require("quadprogpp")
setwd("/Users/calebhill/Documents/misc_coding/quant")


asset <- tq_get(x="GOOG")


sim_and_plot(asset, shares_to_start=1, prop=0.5, thresh=1)
