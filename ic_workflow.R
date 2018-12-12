
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
#source("~/Github/GEMcarbon.R/ingrowth_cores_20180213.R")
setwd("~/Github/gemcarbon_data/raw_data_ingemdb_forELDS")

# read in data
xx <- read.csv("ic_raw_20181016.csv", sep=",", header=T, stringsAsFactors=FALSE) 
datafile = subset(xx, plot_code %in% c( "BLZ-21", "BLZ-22", "KEN-01", "KEN-02", "ALP-11", "ALP-12", "BOB-01", "BOB-02", "BOB-03", "BOB-04", "BOB-05", "BOB-06", "KOG-02", "KOG-03", "KOG-04",
                                        "KOG-05", "KOG-06", "IVI-01", "IVI-02", "DAN-04", "DAN-05", "MLA-01", "MLA-02", "OP",     "SAF-01", "SAF-02", "SAF-03", "SAF-04", "SAF-05", "NXV-02",
                                        "NXV-01", "ACJ-01", "TAM-05", "TAM-06", "TAM-09", "ESP-01", "SPD-01", "SPD-02", "WAY-01", "PAN-03", "PAN-02", "TRU-04", "JEN-03", "JEN-11", "JEN-12",
                                        "STQ-08", "STQ-11", "STB-08", "STB-12", "STJ-01", "STO-03", "STJ-05", "STO-06", "STN-06", "STN-09", "STD-05", "STD-11", "STN-02", "STO-07", "STJ-04",
                                        "STL-10", "STL-09", "STD-10", "STN-03", "STN-04", "ANK-01", "ANK-02", "ANK-03", "TAN-01", "TAN-02", "BLZ-11", "BLZ-12", "LPG-01", "LPG-02", "MNG-03",
                                        "MNG-04"))
  
# ATTENTION !!!!!!!!!!!!!!!!!!!!!!Define IC diameter!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! Which plots have ic diameter = 14??
datafile = datafile %>% mutate(tubed = if plot = c(,,,,,) then 11.5, else 14)

# Not working. Need to re-run these plots:
"ESP-01", "SPD-02", "SPD-01", "WAY-01"

# Values are too high, we have disguarded these measurements:
# ALP-12 20/03/2011 - The data are collected in a suspicious way: the first two timesteps are almost equal, and add up to verhy high values. We can't figure out what is going on, but it doesn't look right. 
# ANK-03 03/04/2013 - These values go up to 3g but only a handful of values are above 1. Values seem to high, but we are not sure why? 
# JEN-11 20/07/14 - Quality control "not sure" and comments saying that the values may be too high. The values go from 0.1g to 7g fairly consistently. Jhon mentionned that he didn't trust these because they were not adequately oven dried. They may still contain moisture. 
# JEN-11 24/10/13 -  Quality control "not sure" and comments saying that the values may be too high. The values are consistently high. Jhon mentionned that he didn't trust these because they were not adequately oven dried. They may still contain moisture.
# BLZ-12 07/05/16 - The first time step was always very high (~20g) but it would be strange if it's stock since it's the third sampling occasion that year. 
# KEN-01 22/11/17 - Values high throughout, but they seem too high. Values between 0.1g and 4.1g. 
# TAM-09 10/12/11 - Some very high numbers (>16g) in the first time steps. 

head(data4)
data4 %>% group_by(plot_code) %>%
  ggplot(data=., aes(month, monthlyNPProot)) + geom_point() +
  ylim(0,3) +
  facet_wrap(~plot_code)

# Save results
write.csv(data4, file="ic_MgC_ha_mo_20180525.csv")

