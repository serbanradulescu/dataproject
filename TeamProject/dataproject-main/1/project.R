##Packages
install.packages("ggplot2")
library(ggplot2)
##Importing the data from CSV files.
WeightData <- data.frame(read.csv("C:\\Users\\madha\\Desktop\\Data Science\\TeamProject\\Weight.csv"))
Survivaldata <- data.frame(read.csv("C:\\Users\\madha\\Desktop\\Data Science\\TeamProject\\Survival.csv"))
Behaviourdata <- data.frame(read.csv("C:\\Users\\madha\\Desktop\\Data Science\\TeamProject\\behaviour.csv"))
Behaviourdata[is.na(Behaviourdata)] = 0
