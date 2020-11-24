library(ggplot2)
##Importing the data from CSV files.
WeightData <- data.frame(read.csv("raw_data\\Weight.csv"))
Survivaldata <- data.frame(read.csv("raw_data\\Survival.csv"))
Behaviourdata <- data.frame(read.csv("raw_data\\behaviour.csv"))
Behaviourdata[is.na(Behaviourdata)] = 0

