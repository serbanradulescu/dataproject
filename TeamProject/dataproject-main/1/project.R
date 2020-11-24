library(ggplot2)
##Importing the data from CSV files.
weight <- data.frame(read.csv("raw_data\\Weight.csv"))
survival <- data.frame(read.csv("raw_data\\Survival.csv"))
behaviour <- data.frame(read.csv("raw_data\\behaviour.csv"))
behaviour[is.na(behaviour)] = 0

