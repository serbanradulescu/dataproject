library(ggplot2)
##Importing the data from CSV files.
weightData <- data.frame(read.csv("raw_data\\Weight.csv"))
survivaldata <- data.frame(read.csv("raw_data\\Survival.csv"))
behaviourdata <- data.frame(read.csv("raw_data\\behaviour.csv"))
behaviourdata[is.na(behaviourdata)] = 0

