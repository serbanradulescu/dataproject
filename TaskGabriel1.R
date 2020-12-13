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

lavalweight <- weig
View(lavalweight)

### Make new data.frames from lavalweight for the initial and final time (1 & 4)
#for optimum analysis

Initialwgt <- filter(lavalweight, Time == 1)
Finalwgt <- filter(lavalweight, Time == 4)



###Find the difference between the final and initial weight. There is a difference
## in the number of row between the newly created data.frames, because some lavae
#escaped before the final weight and some immediately after the first weight, so 
#there is a need to get those reps that have both the final and the initial weight 
#to get the difference in weight.

Fordifference <- filter(lavalweight, Time == 4)


Initialweight <- replicate(153, 0)
Fordifference <- data.frame(Fordifference, Initialweight)

for(i in 1:nrow(Fordifference)){
  for(j in 1:nrow(Initialwgt)){
  if(Fordifference$Rep[i] == Initialwgt$Rep[j]){
    Fordifference$Initialweight[i] <- Initialwgt$weight[j]
  }}}

###Change the weight to final weight to differentiate between initial and final weight
colnames(Fordifference)[colnames(Fordifference) == "weight"] <- "Finalweight"

##Create another column for difference between final and initial weight
Fordifference$Difference <- (Fordifference$Finalweight - Fordifference$Initialweight)

##Remove the column time from the data.frame
Fordifference <-  select(Fordifference, -Time)

##Arrange the column of the data.frame in the order below
Fordifference <- Fordifference[c("Rep", "Treatment", "FIL", "Initialweight", 
                                 "Finalweight", "Difference")]

##Check the structure of newly created dataframes
str(Fordifference)

## The treatment, FIL and Time were integer, so i needed to convert them to a 
##factor for easier manipulation.
Fordifference$Treatment <- as.factor(Fordifference$Treatment)
Fordifference$FIL <- as.factor(Fordifference$FIL)

str(Fordifference)


#One-way ANOVA to determine the effect of thiamethoxam on weight
res.aov<- aov(Difference ~ Treatment, data = Fordifference)
summary(res.aov)

#Test for normality using the Shapiro–Wilk statistic.Making a Q-Q plot  
##This can only be done after ANOVA

shapiro.test(res.aov$residuals)
qqnorm(res.aov$residuals)
qqline(res.aov$residuals)



##LeveneTest for equality of Variance
#Hypothesis
#H0: The variance for all groups are equal
#H1: At least one variance is not equal


leveneTest(Difference ~ Treatment, data = Fordifference)

#Based on the LeveneTest, the variance for all the groups are equal


#Kruskal Wallis to to determine the effect of thiamethoxam on pupal weight

kruz <- kruskal(Fordifference$Difference, trt = Fordifference$Treatment)
print(kruz)

##Make a plot for visual representation of the analysis
Fordifference %>%
  ggplot(aes(x = Treatment, group = Treatment, y = Difference, fill = Treatment)) +
  geom_violin(trim = FALSE) + 
  geom_boxplot(width = 0.2) +
  scale_y_continuous(limits = c(0.15, 0.50)) +
  theme_gray() +
  xlab("Treatment") + 
  ylab("Weight")
  

 ###Fisher's test
fil <- table(Fordifference$FIL, Fordifference$Treatment)
fil
fisher.test(fil)



###########

