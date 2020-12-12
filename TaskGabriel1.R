### This Gabriel`s work, please, for our sake, do not alter without consent.

source("importdata.R")

## Analyze the effect of treatments on larvae weight(working with the dataframe weight)

### Hypothesis:

#H0: Larval weight does not depend upon the concentration of insecticide it is exposed to. 

#H1: Larval weight depends upon the concentration of insecticide it is exposed to. 

lavalweight <- weight
View(lavalweight)

### Make a new data.frame from pupweight for when Time was 4 for optimum analysis
library(dplyr)
lavalwgt <- filter(lavalweight, Time == 4)
data.frame(lavalwgt)

##Check the structure of pupwgt
str(lavalwgt)

## The treatment, FIL and Time were integer, so i needed to convert them to a factor for easier manipulation.
lavalwgt$Treatment <- as.factor(lavalwgt$Treatment)
lavalwgt$FIL <- as.factor(lavalwgt$FIL)
lavalwgt$Time <- as.factor(lavalwgt$Time)
str(lavalwgt)

#One-way ANOVA to determine the effect of thiamethoxam on pupal weight
lw <- aov(Weight..g. ~ Treatment, data = lavalwgt)
summary(lw)

###Based on the anova we do not have sufficeint argument to accept the H1


#Test for normality using the Shapiro–Wilk statistic.Making a Q-Q plot  
##This can only be done after ANOVA

shapiro.test(lw$residuals)
qqnorm(lw$residuals)
qqline(lw$residuals)

##LeveneTest for equality of Variance
#Hypothesis
#H0: The variance for all groups are equal
#H1: At least one variance is not equal

library(car)
leveneTest(Weight..g. ~ Treatment, data = lavalwgt)
#Based on the Levenetest, the variance for all the groups are equal

#Kruskal Wallis to to determine the effect of thiamethoxam on pupal weight
library(agricolae)
kruz <- kruskal(lavalwgt$Weight..g., trt = lavalwgt$Treatment)
print(kruz)

boxplot(Weight..g. ~ Treatment, data = weight, ylim = c(0.0, 0.6))
text(x = 1:6 , y = 0.5, c("a", "a", "a", "a", "a", "a"))

wt <- sqrt(lavalwgt$Weight..g.)
lw2 <- aov(wt ~ Treatment, data = lavalwgt)
lw2


Tfil <- table(lavalwgt$FIL, lavalwgt$Treatment)
Tfil
fisher.test(Tfil)

ASD1 <- HSD.test(lw, "Treatment", console = TRUE)
ASD2 <- HSD.test(lw2, "Treatment", console = TRUE)
