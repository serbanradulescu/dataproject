source("01_import-data.R")
library(tidyr)
library(dplyr)
library(ggplot2)
library(ggfortify)
library(factoextra)
library(cluster)
library(corrplot)
library(agricolae)
library(grid)
library(gridExtra)

##The behaviour data records the time spent by 7 day old adult hoverflies.
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


### Principal component analysis for the behaviour data
##First step: Converting the dataframe to numeric
behaviour1 <- beha  ## making a copy of the originaml dataframe
behaviour1 <- as.data.frame(behaviour1)
##making a numeric dataframe
row.names(behaviour1) <- paste(behaviour1$sample,
                               behaviour1$exp_round, sep = "_")
b1 <-select(behaviour1, S:M)
b2 <- data.frame(as.factor(beha$treatment), b1)
colnames(b2)[colnames(b2) == "as.factor.beha.treatment."] <- "treatment"


##Prcomp for Principal component analysis
##scale is a logical value indicating whether the variables should be scaled to 
##have unit variance before the analysis takes place.
##The default is FALSE for consistency with S, but in general scaling is advisable.
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
autoplot(var$cor, main = "Heat map")
##Plotting the correlation circle
fviz_pca_var(pca1, col.var = "black")


##Individuals PCA
autoplot(pca1, data = b2, colour = 'treatment', frame = TRUE, frame.type = 'norm')

###Anova, Shapiro Wilk and Kruskal Wallis

##Anovas for individual behaviours
beh.aovS <- aov(S ~ treatment, data = b2)
summary.aov(beh.aovS)
beh.aovGR<- aov(GR ~ treatment, data = b2)
summary.aov(beh.aovGR)
beh.aovW <- aov(W ~ treatment, data = b2)
summary.aov(beh.aovW)
beh.aovF <- aov(F ~ treatment, data = b2)
summary.aov(beh.aovF)
beh.aovPR <- aov(PR ~ treatment, data = b2)
summary.aov(beh.aovPR)
beh.aovN <- aov(N ~ treatment, data = b2)
summary.aov(beh.aovN)
beh.aovM <- aov(M ~ treatment, data = b2)
summary(beh.aovM)


##Shapiro Wilk test for normality for each behaviour
shapiro.test(beh.aovS$residuals)
shapiro.test(beh.aovGR$residuals)
shapiro.test(beh.aovW$residuals)
shapiro.test(beh.aovF$residuals)
shapiro.test(beh.aovPR$residuals)
shapiro.test(beh.aovN$residuals)
shapiro.test(beh.aovM$residuals)

##From the p values of the Shapiro Wilk statistic we can conclude that 
##none of the behaviours follow a normal distribution. 

##Kruskal Wallis test
krusS <- kruskal(b2$S, trt = b2$treatment, alpha = 0.05)
print(krusS)
krusGR <- kruskal(b2$GR, trt = b2$treatment, alpha = 0.05)
print(krusGR)
krusW <- kruskal(b2$W, trt = b2$treatment, alpha = 0.05)
print(krusW)
krusF <- kruskal(b2$F, trt = b2$treatment, alpha = 0.05)
print(krusF)
krusPR <- kruskal(b2$PR, trt = b2$treatment, alpha = 0.05)
print(krusPR)
krusN <- kruskal(b2$N, trt = b2$treatment, alpha = 0.05)
print(krusN)
krusM <- kruskal(b2$M, trt = b2$treatment, alpha = 0.05)
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
##For each behavious

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
  geom_bar(stat="identity", position=position_dodge())

G1 + scale_x_discrete(labels=c("0" = "0 (0.327g)", "5" = "5 (0.324g)",
                              "15" = "15 (0.324g)", "50" = "50 (0.327g)",
                              "100" = "100 (0.319g)", "500" = "500 (0.312g)"))
