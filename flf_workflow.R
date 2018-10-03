# Load packages
library(dplyr)
library(grDevices)
require(ggplot2)
library(tidyverse)
library(lubridate)
require(lubridate)
require(tidyverse)

# call function
setwd("~/Github/GEMcarbon.R")
source("~/Github/GEMcarbon.R/functions.r")
#source("~/Github/GEMcarbon.R/flf_2018.R")
setwd("~/Github/gemcarbon_data/raw_data_ingemdb_forELDS")
# read in data
xx_old <- read.csv("flf_20180523.csv", sep=",", header=T, stringsAsFactors=FALSE) 
xx <- read.csv("flf_20180723.csv", sep=",", header=T, stringsAsFactors=FALSE) 
data_flf = subset(xx, plot_code %in% c("WAY-01", "TAM-06", "TAM-05", "ESP-01", "KEN-02", "SPD-02", "KEN-01", "ALP-11", "ALP-12", "SPD-01", 
                                       "SAF-02", "SAF-03", "MLA-01", "SAF-01", "SAF-05", "MLA-02", "SAF-04", "IVI-01", "IVI-02", "ACJ-01", 
                                       "PAN-02", "PAN-03", "TRU-04", "DAN-04", "DAN-05", "TAM-05", "ANK-01", "ANK-02", "ANK-03", "BOB-01", 
                                       "BOB-02", "BOB-03", "BOB-04", "BOB-05", "BOB-06", "KOG-02", "KOG-03", "KOG-04", "KOG-05", "KOG-06", 
                                       "BLZ-22", "BLZ-21", "BLZ-12", "BLZ-11", "TAM-09", "NXV-01", "NXV-02", "STB-12", "STB-08", "STD-10", 
                                       "STD-11", "STD-05", "STJ-01", "STJ-04", "STJ-05", "STL-10", "STL-09", "STN-02", "STN-03", "STN-04", 
                                       "STN-06", "STN-09", "STO-03", "STO-06", "STO-07", "STQ-11", "STQ-08", "JEN-11", "JEN-12", "REQ-14", 
                                       "AGU-01", "LPG-01", "LPG-02", "MNG-03", "MNG-04"))



#plot_code



data_flf$collectiondate = as.Date(paste(data_flf$year, data_flf$month, data_flf$day, sep="-"), format="%Y-%m-%d")
data_flf %>% group_by(plot_code) %>%
  mutate(collectiondate = as.POSIXct(collectiondate)) %>% 
  ggplot(data=., aes(collectiondate, leaves_g_per_trap)) + geom_point() +
  #ylim(0,300) +
  facet_wrap(~plot_code)

write.csv(plotit, file="fidele_flf_MgC_ha_mo_20180727.csv") 

