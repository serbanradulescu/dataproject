source("importdata.R")
library(tidyr)
library(dplyr)
### Principal component analysis for the behaviour data
##First step: Converting the dataframe to numeric
behaviour1 <- behaviour
row.names(behaviour1) <- paste(behaviour1$sample, behaviour1$treatment,
                               behaviour1$ï..exp_round, sep = "_")
beh <- select(behaviour1, S:M)
