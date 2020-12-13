### This Gabriel`s work, please, for our sake, do not alter without consent.

source("01_import-data.R")
library(dplyr)
library(car)
library(agricolae)
library(ggplot2)

## Analyze the effect of treatments on larvae weight(working with the dataframe weight)

### Hypothesis:

#H0: Larval weight does not depend upon the concentration of insecticide it is exposed to. 

#H1: Larval weight depends upon the concentration of insecticide it is exposed to. 
wgt <- weig
lavalweight <- wgt
View(lavalweight)

### Make new data.frames from lavalweight for the initial and final time (1 & 4)
#for optimum analysis

Initialwgt <- filter(lavalweight, Time == 1)
Finalwgt <- filter(lavalweight, Time == 4)
data.frame(Initialwgt)
data.frame(Finalwgt)

###Changing names of columns to differentiate initial and final time and weights
colnames(Initialwgt)[colnames(Initialwgt) %in% c("Time", "weight")] <- 
  c("InitialTime", "Initialweight")

colnames(Finalwgt)[colnames(Finalwgt) %in% c("Time", "weight")] <- 
  c("FinalTime", "Finalweight")

##Check the structure of newly created dataframes
str(Initialwgt)
str(Finalwgt)

## The treatment, FIL and Time were integer, so i needed to convert them to a factor for easier manipulation.
Initialwgt$Treatment <- as.factor(Initialwgt$Treatment)
Initialwgt$FIL <- as.factor(Initialwgt$FIL)
Initialwgt$InitialTime <- as.factor(Initialwgt$InitialTime)
str(Initialwgt)

Finalwgt$Treatment <- as.factor(Finalwgt$Treatment)
Finalwgt$FIL <- as.factor(Finalwgt$FIL)
Finalwgt$FinalTime <- as.factor(Finalwgt$FinalTime)
str(Finalwgt)

###Checking normality with ggplot
wgt %>%
  ggplot(aes(x = Treatment, group = Treatment, y = weight)) +
  geom_boxplot() + 
  theme_classic() +
  xlab("Treatment") + 
  ylab("Weight")

### it is obvious that Treatment500 is different from other treatments

Initialwgt %>%
  ggplot(aes(x = Treatment, group = Treatment, y = Initialweight)) +
  geom_boxplot() + 
  theme_classic() +
  xlab("Treatment") + 
  ylab("Weight")


Finalwgt %>%
  ggplot(aes(x = Treatment, group = Treatment, y = Finalweight)) +
  geom_boxplot() + 
  theme_classic() +
  xlab("Treatment") + 
  ylab("Weight")


#One-way ANOVA to determine the effect of thiamethoxam on initial weight and Final weight
IW <- aov(Initialweight ~ Treatment, data = Initialwgt)
summary(IW)

FW <- aov(Finalweight ~ Treatment, data = Finalwgt)
summary(FW)

###Based on the two ANOVA we do not have sufficient argument to accept the H1



#Test for normality using the Shapiro–Wilk statistic.Making a Q-Q plot  
##This can only be done after ANOVA

shapiro.test(IW$residuals)
qqnorm(IW$residuals,  main = "Initial Weight")
qqline(IW$residuals)

shapiro.test(FW$residuals)
qqnorm(FW$residuals,  main = "Final Weigt")
qqline(FW$residuals)

##LeveneTest for equality of Variance
#Hypothesis
#H0: The variance for all groups are equal
#H1: At least one variance is not equal

#Based on the LeveneTest, the variance for all the groups are equal
leveneTest(Initialweight ~ Treatment, data = Initialwgt)
leveneTest(Finalweight ~ Treatment, data = Finalwgt)

#Kruskal Wallis to to determine the effect of thiamethoxam on pupal weight

kruz <- kruskal(Initialwgt$Initialweight, trt = Initialwgt$Treatment)
print(kruz)

KR <- kruskal(Finalwgt$Finalweight, trt = Finalwgt$Treatment)
KR

###Fisher's test
Tfil <- table(wgt$FIL, wgt$Treatment)
Tfil
fisher.test(Tfil)

INFish <- table(Initialwgt$FIL, Initialwgt$Treatment)
INFish
fisher.test(INFish)

FNFish <- table(Finalwgt$FIL, Finalwgt$Treatment)
FNFish
fisher.test(FNFish)
###########

