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
#It is hard to observe if there are differences between blocks, we procced to do a linear model with the blocks as median

lm1 <- lm(dead ~ treatment + exp_round, data= survival)
summary(lm1)



lm1 <- lm(dead ~ treatment + exp_round +
            I(as.numeric(treatment) * as.numeric(exp_round)),
          data= survival)
summary(lm1)

t1 <- table(survival$treatment, survival$dead)
fisher.test(t1, simulate.p.value=TRUE)

fisher.multcomp(t1)
#500 has significant differences with all of them, between them there is no significant difference

#ANOVA

lm1 <- lm(timetopup ~ treatment + exp_round, data= survival)
summary(lm1)

lm1 <- lm(timetopup ~ treatment + exp_round +
            I(as.numeric(treatment) * as.numeric(exp_round)),
          data= survival)
summary(lm1)

#No interaction, p = 0.27, do the anova

anv <- aov( lm(survival$timetopup ~ survival$treatment) )
summary(anv)

plot(y=survival$timetopup, x= survival$treatment)

agri <- HSD.test(anv, 'survival$treatment', alpha = 0.05, group=TRUE, main = "HSD Test")
plot(agri, main = "Observing statistical differences with agricola package", ylab = "Insects counted", xlab = "Type of spray", sub = "two groups of effects: a and b")


#Conclusions:
