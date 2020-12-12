############# Import data #############################
# 2020-12-12 Created by Alfonso Garmendia
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
### This script imports the three data frames
library(readxl)

### files names
file1 <- "data/behaviour.xlsx"
file2 <- "data/survival.xlsx"
file3 <- "data/weight.xlsx"

legend.behaviour <- paste(
  "Each line represents one individual and the behaviours recorded in a 10",
  "minute (600 seconds) period:",
  "(S) stationary, (GR) grooming, (W) walking, (F) flying,",
  "(PR) probing through the cage netting with their proboscis, (N) feeding on",
  "nectar, pollen or water (grouped together as feeding) and (M) moving which",
  "involved remaining stationary whilst making small jerking motions of their",
  "body.")

beha <- read_excel(file1)
surv <- read_excel(file2)
weig <- read_excel(file3)

### Fix names -----------------
names(beha)[which(names(beha) == "...12")] <- "total.seconds"
names(weig)[which(names(weig) == "Weight (g)")] <- "weight"

rm(file1, file2, file3)

