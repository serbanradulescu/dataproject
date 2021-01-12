### This Gabriel`s work, please, for our sake, do not alter without consent.

source("01_import-data.R")
library(dplyr)
library(car)
library(agricolae)
library(ggplot2)
library(ggpubr)

## The essence of this part is to analyze the effect of treatments on larvae weight(working with the dataframe weight)

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
colnames(Fordifference)[colnames(Fordifference) == "Rep"] <- "RepID"

##Create another column for difference between final and initial weight
Fordifference$Difference <- (Fordifference$Finalweight - Fordifference$Initialweight)

##Remove the column time from the data.frame
Fordifference <-  select(Fordifference, -Time)

##Arrange the column of the data.frame in the order below
Fordifference <- Fordifference[c("RepID", "Treatment", "FIL", "Initialweight", 
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

## Based on the shapiro.test, we can conclude that the distribution of the treatment
## effect was significantly different from normal distribution


##LeveneTest for equality of Variance
#Hypothesis
#H0: The variance for all groups are equal
#H1: At least one variance is not equal


leveneTest(Difference ~ Treatment, data = Fordifference)

#Based on the LeveneTest, the variance for all the groups are equal


#Kruskal Wallis to determine the effect of thiamethoxam on pupal weight

kruz <- kruskal(Fordifference$Difference, trt = Fordifference$Treatment)
print(kruz)

##Based on the Kruskal Wallis test, there is no significant difference between 
## treatments

##Make a plot for visual representation of the analysis and comparison between treatments and control
my_comparisons <- list(c("0", "5"), c("0", "15"),c("0", "50"),c("0", "100"),
                       c("0", "500"))
Fordifference %>%
  ggplot(aes(x = Treatment, group = Treatment, y = Difference, color = Treatment)) +
  geom_violin(trim = F, width = 0.65, draw_quantiles = c(0.25, 0.5, 0.75)) +
  theme_classic()+
  scale_y_continuous(limits = c(0.15, 0.70)) +
  xlab("Treatment") + 
  ylab("Weight")+
  ggtitle("Comparison of treatment effect on larval weight")+
  stat_compare_means(method = "kruskal.test")+
  stat_compare_means(comparisons = my_comparisons,label.y = c(0.43,0.46,0.49,
                                                              0.52,0.55))+
  stat_compare_means(label.y = 70)
  


 ###Fisher's test
##Hypothesis : H0 :  the treatment has no effect on FIL
              #H1 :  the treatment has effect on FIL
fisher.test(Fordifference$FIL, Fordifference$Treatment)



 #p-value 0.3198 is greater than 0.05, therefore, 
##there is no sufficient evidence to accept the alternative hypothesis

###########






