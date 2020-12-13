source("importdata.R")
library(tidyr)
library(dplyr)
library(ggplot2)
library(factoextra)
### Principal component analysis for the behaviour data
##First step: Converting the dataframe to numeric
behaviour1 <- behaviour  ## making a copy of the originaml dataframe
##Renaming columns
colnames(behaviour1)
names(behaviour1)[names(behaviour1) == "ï..exp_round"] <- "er"
##making a numeric dataframe
behaviour1
row.names(behaviour1) <- paste(behaviour1$sample, behaviour1$treatment,
                               behaviour1$er, sep = "_")
b1 <- select(behaviour1, S:M)

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
##Plot A
fviz_pca_ind(pca1, col.ind = "cos2", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)

##PlotB
fviz_pca_ind(pca1, col.ind = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             label = "none",
             repel = TRUE
)