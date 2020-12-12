#This is Serban's task, please do not interfere with this code unless 
#you know you know better


#Hyp1:
#H0: Larval survival does not depend on the quantity of applied insecticide.
#H1: Larval survival does depend on the quantity of applied insecticide

#Step 1: Explain the data: a dataset with 6 column:
#1.the ID of repetition
#2.exp_round - different blocks, with 8 repetitions
#3.treatment - Control (0),T1= 15, T2= 50, T3= 100, T4= 500
#4.daysinxp  - the amount of days it needs to reach pupae and so to be considered
#survived (5 days to time it forms a pupae)
#5.time to pup - if it never reached adulthood, daysinxp are considered 0
#6.dead - mortality (yes or no) 

library(ggplot2)
library(agricolae)
library( RVAideMemoire)
source("importdata.R")

#For the mortality rate (survival$dead)
#Step 1: We plot it in order to see if:
#-there are differences between the blocks
#-there are differences between the treatments


survival$treatment <- as.factor(survival$treatment)
survival$exp_round <- as.factor(survival$exp_round)
ggplot(survival, aes(x=treatment, y=dead, fill=exp_round)) + 
  geom_bar(stat="identity")

#It is obvious that the treatment500 is different from the others.
#It is hard to observe if there are differences between blocks, we proced to do a linear model with the blocks as median

lm1 <- lm(dead ~ treatment + exp_round, data= survival)
summary(lm1)

#P value is bigger than 0.05 for: treatment500. The other treatments and the blocks show a p_value smaller than 0.5 so no significant effect.
#We proceed to study the interaction betwenn treatments and blocks.

lm1 <- lm(dead ~ treatment + exp_round +
            I(as.numeric(treatment) * as.numeric(exp_round)),
          data= survival)
summary(lm1)

#No interaction between treatments and blocks. (p_value = 0.3738)
#We proceed with the fisher test, as the data is categorical.

t1 <- table(survival$treatment, survival$dead)
fisher.test(t1, simulate.p.value=TRUE)

#A simple fisher test tells us that there are significant differences between treatments.
#We proceed with a multi-comparation fisher test.

fisher.multcomp(t1)

#500 has significant differences with all of them, between them there is no significant difference

#Part 2 - Analysis on time to pupate
#ANOVA

plot(y=survival$timetopup, x= survival$treatment)

#We know from previous analysis that treatment 500 is different. We proceed to study the differences between blocks and treatments for the time to pupate

lm1 <- lm(timetopup ~ treatment + exp_round, data= survival)
summary(lm1)

#P value is bigger than 0.05 for: treatment500. The other treatments and the blocks show a p_value smaller than 0.5 so no significant effect.
#We proceed to study the interaction betwenn treatments and blocks.

lm1 <- lm(timetopup ~ treatment + exp_round +
            I(as.numeric(treatment) * as.numeric(exp_round)),
          data= survival)
summary(lm1)

#No interaction, p = 0.27, do the anova

anv <- aov( lm(survival$timetopup ~ survival$treatment) )
summary(anv)

#Anova shows significant differences between treatments. We proceed with a HSD test.

agri <- HSD.test(anv, 'survival$treatment', alpha = 0.05, group=TRUE, main = "HSD Test")
plot(agri, main = "Observing statistical differences with agricola package", ylab = "Insects counted", xlab = "Type of spray", sub = "two groups of effects: a and b")


#Conclusions:
