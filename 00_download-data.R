############# Download data #############################
# 2020-12-12 Created by Alfonso Garmendia
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
### This script donwloads the data from the article web
### page and saves the three xlsx files into data folder

urls <- c( # URLs to the data
  # larval weight data
  "https://dfzljdn9uc3pi.cloudfront.net/2018/4258/1/Weight_PJ.xlsx", 
  # Development time and survival
  "https://dfzljdn9uc3pi.cloudfront.net/2018/4258/1/Survival_PJ.xlsx",
  # Behavioural observations of the E. tenax after larval thiamethoxam exposure.
  "https://dfzljdn9uc3pi.cloudfront.net/2018/4258/1/behavobvs_PeerJ.xlsx"
  )
file.names <- c(  # names for the three files
    # larval weight data
  "weight.xlsx", 
  # Development time and survival
  "survival.xlsx",
  # Behavioural observations of the E. tenax after larval thiamethoxam exposure.
  "behaviour.xlsx"
)

folder <- "data"  # The data folder

if(!dir.exists(folder)) dir.create(folder)  # Creates data folder if necessary

download.file(urls, 
  paste0(folder, "/", file.names))  # Donwload files into folder

rm(urls, file.names, folder)  # Clean environment