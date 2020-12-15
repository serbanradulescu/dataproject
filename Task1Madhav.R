source("01_import-data.R")
library(tidyr)
library(dplyr)
library(ggplot2)
library(ggfortify)
library(factoextra)
library(cluster)
library(corrplot)

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

##Plotting the correlation circle
fviz_pca_var(pca1, col.var = "black")
autoplot(var$cor)

##Individuals PCA
autoplot(pca1, data = b2, colour = 'treatment')
autoplot(pam(b2, 7),frame = TRUE, frame.type = 'norm')
autoplot(pam(b2, 7),colour = "treatment", frame = TRUE, frame.type = 'norm')
autoplot(pca1, data = b2, colour = 'treatment', frame = TRUE, frame.type = 'norm')
