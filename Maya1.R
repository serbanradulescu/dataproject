#Maya s code hands down

source("importdata.R")

#Effect of treatment on behavior 


library(tidyr)
library(dplyr)

##First step: Converting the dataframe to numeric
behaviour1 <- behaviour
row.names(behaviour1) <- paste(behaviour1$sample, behaviour1$treatment,
                               behaviour1$ï..exp_round, sep = "_")
b1 <- select(behaviour1, S:M)

#PCA 

install.packages("factoextra")
library(factoextra)

groups <- as.factor(decathlon2$Competition[1:23])
fviz_pca_ind(res.pca,
             col.ind = groups, # color by groups
             palette = c("#00AFBB",  "#FC4E07"),
             addEllipses = TRUE, # Concentration ellipses
             ellipse.type = "confidence",
             legend.title = "Groups",
             repel = TRUE
)

