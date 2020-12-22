#Maya s code hands down

source("01_import-data.R")
library(tidyr)
library(dplyr)
library(ggplot2)
library(ggfortify)
library(factoextra)
library(cluster)
library(corrplot)
## Explanation of data
##The behaviour data records the time spent by 7 days old adult hoverflies.
##Seven adult behaviours were studied for 10 minutes (600 seconds) and
##the time spent on each behaviour was recorded in seconds. 
##The behaviours studied are - 
#S- stationary
#GR- grooming
#W- walking
#F- flying
#PR- probing through the cage netting with their proboscis
#N- feeding on nectar, pollen, and water
#M- moving, which includes making small jerking motion while being stationary

#Effect of treatment on behavior 

##First step: Converting the dataframe to numeric
behaviour1 <- beha
behaviour1 <- as.data.frame(behaviour1)
row.names(behaviour1) <- paste(behaviour1$sample,
                               behaviour1$exp_round, sep = "_")
b1 <- select(behaviour1, S:M)
b2 <- data.frame(as.factor(beha$treatment), b1)
colnames(b2)[colnames(b2) == "as.factor.beha.treatment."] <- "treatment"


#PCA 

#install.packages("factoextra")
pca1 <- prcomp(b1, scale = TRUE) 
summary(pca1)
##Eigen values
eig.val <- get_eigenvalue(pca1)

##Plotting
##Scree plot
fviz_eig(pca1, addlabels = TRUE)


##From the Eigen Values and the Scree plot we that the first two principle 
## components explain 46.4% of the variation.


var <- get_pca_var(pca1)
var
head(var$coord)
head(var$cos2)
head(var$contrib)


##Heat map
autoplot(var$cor)
##Plotting the correlation circle
fviz_pca_var(pca1, col.var = "black")


##Individuals PCA
autoplot(pca1, data = b2, colour = 'treatment', frame = TRUE, frame.type = 'norm')

#To determine the effect of treatments in behaviour we can apply Kruskal Wallis for each behaviour
#to do this first we have to do anova then shapiro for residuals 

anovS<-aov(S~treatment, data= beha)
summary(anovS)
anovGR<-aov(GR~treatment, data= beha)
summary(anovGR)
anovW<-aov(W~treatment, data= beha)
summary(anovW)
anovF<-aov(F~treatment, data= beha)
summary(anovF)
anovPR<-aov(PR~treatment, data= beha)
summary(anovPR)
anovN<-aov(N~treatment, data= beha)
summary(anovN)
anovM<-aov(M~treatment, data= beha)
summary(anovM)

#shapiro for residuals
shapiro.test(anovS$residuals)
shapiro.test(anovGR$residuals)
shapiro.test(anovW$residuals)
shapiro.test(anovF$residuals)
shapiro.test(anovPR$residuals)
shapiro.test(anovN$residuals)
shapiro.test(anovM$residuals)

#Kruskal Wallis 
krusS<-kruskal(beha$S, trt = beha$treatment, alpha= 0.01)
print(krusS)
krusGR<-kruskal(beha$GR, trt= beha$treatment, aalpha = 0.01)
print(krusGR)
krusW<-kruskal(W ~ treatment, data = beha)
print(krusW)
krusF<-kruskal(F ~ treatment, data = beha)
print(krusF)
krusPR<-kruskal(PR ~ treatment, data = beha)
print(krusPR)
krusN<-kruskal(N ~ treatment, data = beha)
print(krusN)
krusM<-kruskal(M ~ treatment, data = beha)
print(krusM)

