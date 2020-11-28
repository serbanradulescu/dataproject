### This Gabriel`s work, please, for our sake, do not alter without consent.

source("importdata.R")

## Analyze the effect of treatments on larvae weight

### Hypothesis:

#H0: Larval weight does not depend upon the concentration of insecticide it is exposed to. 

#H1: Larval weight depends upon the concentration of insecticide it is exposed to. 


### I am only using the histogram to check normality, and based on the histogram,
### the weight does not follow a normal distribution.

hist(weight$Weight..g.)
