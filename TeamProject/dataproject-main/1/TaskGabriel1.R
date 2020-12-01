### This Gabriel`s work, please, for our sake, do not alter without consent.

source("importdata.R")

## Analyze the effect of treatments on larvae weight

### Hypothesis:

#H0: Larval weight does not depend upon the concentration of insecticide it is exposed to. 

#H1: Larval weight depends upon the concentration of insecticide it is exposed to. 


### I am only using the histogram to check normality, and based on the histogram,
### the weight does not follow a normal distribution.

hist(weight$Weight..g.)

summary(weight)
str(weight)


## The treatment, FIL and Time were integer, so i needed to convert them to a factor for easier manipulation.
weight$Treatment <- as.factor(weight$Treatment)
weight$FIL <- as.factor(weight$FIL)
weight$Time <- as.factor(weight$Time)
str(weight)

#One-way ANOVA to determine the effect of thiamethoxam on pupal weight
pupweight <- aov(Weight..g. ~ Treatment, data = weight)
summary(pupweight)


