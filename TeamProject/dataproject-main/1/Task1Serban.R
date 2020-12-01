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


source("importdata.R")