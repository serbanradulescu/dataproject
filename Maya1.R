#Maya s code hands down

source("01_import-data.R")
library(tidyr)
library(dplyr)
library(ggplot2)
library(ggfortify)
library(factoextra)
library(cluster)
library(corrplot)
library(agricolae)
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
#From the Eigen values and the Scree plot of our PCA we understand that Principal Component 1 and Principal Component 2 explain 46.4% of the variance in the data.

##From the Eigen Values and the Scree plot we that the first two principle 
## components explain 46.4% of the variation.


var <- get_pca_var(pca1)
var
head(var$coord)
head(var$cos2)
head(var$contrib)


##Heat map
autoplot(var$cor)

#From the heat map we can visualize that the first two dimensions explain a largest degree of variance in our data. The third and fourth compnent also explain a large degree of the variance. The fourth dimension is a good fit for the probing behaviour (PR). 


##Plotting the correlation circle
fviz_pca_var(pca1, col.var = "black")

# This plot of the correlation circle visualizes the correlation between the first two principal components which explain the highest variance which are respectively 27.6 % and 18.8%. We can conclude that Flying and walking are correlated together (the arrow are grouped), stationary and  grooming are correlated together and the arrow which reprents feeding on nectar, pollen, and water is in the opposite direction and not correlated with others behaviours.

##Individuals PCA
autoplot(pca1, data = b2, colour = 'treatment', frame = TRUE, frame.type = 'norm')

#This plot with ellipses shows the distribution of each behaviour following each treatment. From this plot we can conclude that there is no significant difference between the effect of treatment on

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
krusS<-kruskal(beha$S, trt = beha$treatment, alpha= 0.05)

print(krusS)
krusGR<-kruskal(beha$GR, trt= beha$treatment, alpha = 0.05)
print(krusGR)
krusW<-kruskal(beha$W, trt = beha$treatment, alpha= 0.05)
print(krusW)
krusF<-kruskal(beha$F, trt = beha$treatment, alpha= 0.05)
print(krusF)
krusPR<-kruskal(beha$PR , trt =  beha$treatment, alpha= 0.05)
print(krusPR)
krusN<-kruskal(beha$N, trt= beha$treatment, alpha= 0.05)
print(krusN)
krusM<-kruskal(beha$M , trt =  beha$treatment, alpha= 0.05)
print(krusM)


### Comparing the effect of weight on behaviour
Weight1 <- filter(weig, FIL == 0, Time == 4)
AvgWeights <- as.numeric()
for(i in levels(as.factor(Weight1$Treatment))){
  AvgWeights <-c(AvgWeights,mean(Weight1$weight[which(Weight1$Treatment == i)]))
}
AvgWeights

M <- filter(Weight1, Treatment == 15, Time == 4)
mean(M$weight)
##Calculating the average time spent by adults of each treatment group
##For each behaviour

##Avg S
AvgS <- as.numeric()
for(i in levels(as.factor(behaviour1$treatment))){
  AvgS <- c(AvgS, mean(behaviour1$S[which(behaviour1$treatment == i)]))
}
AvgS

##Avg GR
AvgGR <- as.numeric()
for(i in levels(as.factor(behaviour1$treatment))){
  AvgGR <- c(AvgGR, mean(behaviour1$GR[which(behaviour1$treatment == i)]))
}
AvgGR

##Avg W
AvgW <- as.numeric()
for(i in levels(as.factor(behaviour1$treatment))){
  AvgW <- c(AvgW, mean(behaviour1$W[which(behaviour1$treatment == i)]))
}
AvgW

##Avg F
AvgF <- as.numeric()
for(i in levels(as.factor(behaviour1$treatment))){
  AvgF <- c(AvgF, mean(behaviour1$F[which(behaviour1$treatment == i)]))
}
AvgF

##Avg PR
AvgPR <- as.numeric()
for(i in levels(as.factor(behaviour1$treatment))){
  AvgPR <- c(AvgPR, mean(behaviour1$PR[which(behaviour1$treatment == i)]))
}
AvgPR

##Avg N
AvgN <- as.numeric()
for(i in levels(as.factor(behaviour1$treatment))){
  AvgN <- c(AvgN, mean(behaviour1$N[which(behaviour1$treatment == i)]))
}
AvgN

##Avg M
AvgM <- as.numeric()
for(i in levels(as.factor(behaviour1$treatment))){
  AvgM <- c(AvgM, mean(behaviour1$M[which(behaviour1$treatment == i)]))
}
AvgM
Treatments <- c(0, 5, 15, 50, 100, 500)
WeiBeha <- data.frame(Treatments, AvgWeights, AvgS, AvgGR, AvgW, AvgF, AvgPR, AvgN, AvgM)

WeiBeha2 <- WeiBeha %>%
  pivot_longer(!c(Treatments,AvgWeights), names_to = "Behaviour", values_to = "seconds")

WeiBeha2$Treatments <- as.factor(WeiBeha2$Treatments)

G1 <- ggplot(data=WeiBeha2, aes(x= Treatments, y=seconds, fill = Behaviour)) +
  geom_bar(stat="identity", position=position_dodge())+
  ggtitle("Relationship between weight and behaviour")

G1 + scale_x_discrete(labels=c("0" = "0 (0.327g)", "5" = "5 (0.324g)",
                               "15" = "15 (0.324g)", "50" = "50 (0.327g)",
                               "100" = "100 (0.319g)", "500" = "500 (0.312g)"))
#Madhab figured out this method 


