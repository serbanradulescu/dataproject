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
##Boxplot to compare differences between Treatments
boxplot(Weight..g. ~ Treatment, data = weight)

##Based on the boxplot, it is obvious that the Treatment 500 is different from other treatments


## The treatment, FIL and Time were integer, so i needed to convert them to a factor for easier manipulation.
weight$Treatment <- as.factor(weight$Treatment)
weight$FIL <- as.factor(weight$FIL)
weight$Time <- as.factor(weight$Time)
str(weight)

#One-way ANOVA to determine the effect of thiamethoxam on pupal weight
pupweight <- aov(Weight..g. ~ Treatment, data = weight)
summary(pupweight)
### the ANOVA is suggesting there is no significance between the treatment.

##LeveneTest for equality of Variance
#Hypothesis
#H0: The variance for all groups are equal
#H1: At least one variance is not equal

library(car)
leveneTest(Weight..g. ~ Treatment, data = weight)
#Based on the Levenetest, the variance for all the groups are equal

#Kruskal Wallis to to determine the effect of thiamethoxam on pupal weight
library(agricolae)
kruz <- kruskal(weight$Weight..g., trt = weight$Treatment)
print(kruz)

boxplot(Weight..g. ~ Treatment, data = weight, ylim = c(0.0, 0.6))
text(x = 1:6 , y = 0.5, c("a", "a", "a", "a", "a", "b"))


#Test for normality using the Shapiro–Wilk statistic
shapiro.test(pupweight$residuals)

#Fisher`s test between Treatment and FIL
#First I create a new table

Tfil <- table(weight$FIL, weight$Treatment)
Tfil
fisher.test(Tfil)
