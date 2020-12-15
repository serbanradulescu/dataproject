source("01_import-data.R")
library(stringr)

#We get the data from behaviour in order to redefine survival. In order to do this, we create survival2, a data frame with the insects that survived.

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
        survival$exp_round[i] == survival2$exp_round[j] && j < nrow(survival2)){
    survival$redefdead[i] <- 0
    j<- j+1
    i <- i+1
  }}

