source("01_import-data.R")
library(tidyr)
library(dplyr)
library(ggplot2)
library(ggfortify)
library(factoextra)
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
##Prcomping for Principal component analysis
##a logical value indicating whether the variables should be scaled to 
##have unit variance before the analysis takes place.
##The default is FALSE for consistency with S, but in general scaling is advisable.
pca1 <- prcomp(b1, scale = TRUE) 
summary(pca1)
##Plotting
##Scree plot
fviz_eig(pca1, addlabels = TRUE)

##Individuals PCA
autoplot(pca1, data = b2, colour = 'treatment')
