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
library(stringr)
library(tidyverse)
source("01_import-data.R")

#for the package RVAideMemoire, use the  install_github("mixOmicsTeam/mixOmics") command before

#For the mortality rate (survival$dead)
#Step 1: We plot it in order to see if:
#-there are differences between the blocks
#-there are differences between the treatments

survival <- surv

survival$treatment <- as.factor(survival$treatment)
survival$exp_round <- as.factor(survival$exp_round)
ggplot(survival, aes(x=treatment, y=dead, fill=exp_round)) + 
  geom_bar(stat="identity")

#It is obvious that the treatment500 is different from the others.
#It is hard to observe if there are differences between blocks, we proced to do a linear model with the blocks as median

lm1 <- lm(dead ~ treatment + exp_round, data= survival)

lms <- summary(lm1)
lms <- as.data.frame(lms$coefficients)
lms$`Pr(>|t|)`[which(row.names(lms)== 'treatment500')]



#P value is smaller than 0.05 for: treatment500. The other treatments and the blocks show a p_value smaller than 0.5 so no significant effect.
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

tab <- fisher.multcomp(t1)

let <- character()
let[1] <- ' '

for(i in 1:length(tab$p.value[,1])) {
  let[i+1] <- if(tab$p.value[i,1] > 0.01) 
                  {let[i+1] <- ' '} 
               else {let[i+1] <- '*'}}

surs <- data.frame(let=let, treatment=levels(survival$treatment))
ggplot() + 
  geom_bar(data=survival, aes(x=treatment, y=dead, fill=exp_round),stat="identity") +
  geom_text(data=surs, 
    aes(label = let,x=1:6, y = 33.5),
    # position = position_dodge(0.9),
    vjust = 0
  )
#Put a legend



#Treatment 500 has significant differences with all of them, between the others there is no significant difference


#Study of survival

library(survival)
library(survminer)
library(dplyr)

#we create the surv_object, which is a compiled version of the daysinexp and dead columns that can be interpreted using the survfit function
surv_object <- Surv(time = survival$daysinexp, event = survival$dead)
#Now we do the Kaplan-Meier curves
fit1 <- survfit(surv_object ~ treatment, data = survival)
summary(fit1)
#We plot the Kaplan-Meier curves so we can see better
ggsurvplot(fit1, data = survival, pval = TRUE)


#Now we do the same with time to pupate. We found it already hard to read, because of the numerous treatments so we decided to include the time to pupate in another ggplot.
surv_object <- Surv(time = survival$timetopup, event = survival$dead)
fit2 <- survfit(surv_object ~ treatment, data = survival)
summary(fit2)
ggsurvplot(fit2, data = survival, pval = TRUE)

#As many insects die while on treatment 500, the plot for time to pupate isn't very conclusive.


#Hypothesis 6
#Redefine survival and redo the analyses

#Step 1: We get the data from behaviour in order to redefine survival. In order to do this, we create survival2, a data frame with the insects that survived.

numextract <- function(string){
  str_extract(string, "\\-*\\d+\\.*\\d*")
}
behaviour <- beha
ID <- numextract(behaviour$sample)
survival2 <- data.frame(ID, behaviour$exp_round, behaviour$treatment)

names(survival2)[names(survival2) == "ID"] <- "RepID"
names(survival2)[names(survival2) == "behaviour.exp_round"] <- "exp_round"
names(survival2)[names(survival2) == "behaviour.treatment"] <- "treatment"

#Step 2: We observed that we can not corelate the data from behaviour with the one from survival, as the notations changed (we have sample 29 for exp_round 1)
#We find a work around: merge the data with the survival one, but deleting the days in exp and time to pupate, as those can not be corelated. We just keep the number of individuals that survived and the number of individual that were initially in the experience.

survival <- surv
survival[4:5] <- list(NULL)

redefdead <- replicate(213, 1)
survival <- data.frame(survival, redefdead)

i <- 1
j<- 1

for(i in 1:nrow(survival)){
  while(survival$treatment[i] == survival2$treatment[j] && 
     survival$exp_round[i] == survival2$exp_round[j]){
        survival$redefdead[i] <- 0
        j<- j+1
        i <- i+1
        }}

#Step 1: We plot it in order to see if:
#-there are differences between the blocks
#-there are differences between the treatments


survival$treatment <- as.factor(survival$treatment)
survival$exp_round <- as.factor(survival$exp_round)
ggplot(survival, aes(x=treatment, y=redefdead, fill=exp_round)) + 
  geom_bar(stat="identity")

#It is obvious that the treatment500 is different from the others.
#It is hard to observe if there are differences between blocks, we proced to do a linear model with the blocks as median

lm1 <- lm(redefdead ~ treatment + exp_round, data= survival)
summary(lm1)

#P value is smaller than 0.05 for: treatment500. The other treatments and the blocks show a p_value smaller than 0.5 so no significant effect.
#We proceed to study the interaction betwenn treatments and blocks.

lm1 <- lm(redefdead ~ treatment + exp_round +
            I(as.numeric(treatment) * as.numeric(exp_round)),
          data= survival)
summary(lm1)

#No interaction between treatments and blocks. (p_value = 0.1551)
#We proceed with the fisher test, as the data is categorical.

t1 <- table(survival$treatment, survival$redefdead)
fisher.test(t1, simulate.p.value=TRUE)

#A simple fisher test tells us that there are significant differences between treatments.
#We proceed with a multi-comparation fisher test.

fisher.multcomp(t1)

#Treatment500 has significant differences with all of them, between them there is no significant difference




#Part 2 - Analysis on time to pupate on the original defined survival, as for the redefined we weren't able to identify the individuals
#ANOVA

plot(y=survival$timetopup, x= survival$treatment, xlab="Time to pupate" , ylab="Treatment")

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
#Treatment 500 is the only one that shows significant differences in comparison with the control.


#Conclusions:
#We couldn't do the time to pupate analysis of the redefined survival, as the values from behaviour table couldn't be correlated with the survival.
#Even with the redefined survival, the only treatment that had a significant effect was the treatment500.
