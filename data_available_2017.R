

library(zoo)
library(sqldf)
library(dplyr)
library(plyr)
library(grDevices)
library(RColorBrewer)
require(ggplot2)
require(gridExtra)

# revalue plot_codes
#data$plot_code <- revalue(data$plot_code, c("Tower" = "SAF-05", "E" = "SAF-03", "B South" = "SAF-01", "B North" = "SAF-02", "Seraya" = "MLA-02", "Belian" = "MLA-01", "LF" = "SAF-04", "Danum Carbon 1" = "DAN-04", "Danum Carbon 2" = "DAN-05", "BZ11" = "BLZ-11", "BZ12" = "BLZ-12", "BZ22" = "BLZ-22", "BZ21" = "BLZ-21", "Ank-02" = "ANK-02"))


# R SOIL

# raw soil respiration
raw_totsrA   <- read.table("~/Github/gemcarbon_data/raw_data_ingembd/eltr_rsoil_total_feb17.csv", sep=",", header=T)
unique(raw_totsrA$plot_code)
raw_totsrA$date <- as.Date(paste(raw_totsrA$year, raw_totsrA$month, raw_totsrA$day, sep="."), format="%Y.%m.%d") 

a <- subset(raw_totsrA, plot_code == "WAY-01")
b <- subset(raw_totsrA, plot_code == "ESP-01")
c <- subset(raw_totsrA, plot_code == "ACJ-01")
d <- subset(raw_totsrA, plot_code == "PAN-02")
e <- subset(raw_totsrA, plot_code == "PAN-03")
f <- subset(raw_totsrA, plot_code == "SPD-01")
g <- subset(raw_totsrA, plot_code == "SPD-02")
h <- subset(raw_totsrA, plot_code == "TRU-04")
i <- subset(raw_totsrA, plot_code == "TAM-05")
j <- subset(raw_totsrA, plot_code == "TAM-06")
k <- subset(raw_totsrA, plot_code == "TAM-09")

aa <- ggplot(a) + geom_point(aes(x=date, y=co2ref_ppm_sec, colour=factor(year))) + xlim(min(j$date), max(j$date)) + ggtitle("WAY-01") + theme(legend.position="none")
bb <- ggplot(b) + geom_point(aes(x=date, y=co2ref_ppm_sec, colour=factor(year))) + xlim(min(j$date), max(j$date)) + ggtitle("ESP-01") + theme(legend.position="none")
cc <- ggplot(c) + geom_point(aes(x=date, y=co2ref_ppm_sec, colour=factor(year))) + xlim(min(j$date), max(j$date)) + ggtitle("ACJ-01") + theme(legend.position="none")
dd <- ggplot(d) + geom_point(aes(x=date, y=co2ref_ppm_sec, colour=factor(year))) + xlim(min(j$date), max(j$date)) + ggtitle("PAN-02") + theme(legend.position="none")
ee <- ggplot(e) + geom_point(aes(x=date, y=co2ref_ppm_sec, colour=factor(year))) + xlim(min(j$date), max(j$date)) + ggtitle("PAN-03") + theme(legend.position="none")
ff <- ggplot(f) + geom_point(aes(x=date, y=co2ref_ppm_sec, colour=factor(year))) + xlim(min(j$date), max(j$date)) + ggtitle("SPD-01") + theme(legend.position="none")
gg <- ggplot(g) + geom_point(aes(x=date, y=co2ref_ppm_sec, colour=factor(year))) + xlim(min(j$date), max(j$date)) + ggtitle("SPD-02") + theme(legend.position="none")
hh <- ggplot(h) + geom_point(aes(x=date, y=co2ref_ppm_sec, colour=factor(year))) + xlim(min(j$date), max(j$date)) + ggtitle("TRU-04") + theme(legend.position="none")
ii <- ggplot(i) + geom_point(aes(x=date, y=co2ref_ppm_sec, colour=factor(year))) + xlim(min(j$date), max(j$date)) + ggtitle("TAM-05") + theme(legend.position="none")
jj <- ggplot(j) + geom_point(aes(x=date, y=co2ref_ppm_sec, colour=factor(year))) + xlim(min(j$date), max(j$date)) + ggtitle("TAM-06") + theme(legend.position="none")
kk <- ggplot(k) + geom_point(aes(x=date, y=co2ref_ppm_sec, colour=factor(year))) + xlim(min(j$date), max(j$date)) + ggtitle("TAM-09") + theme(legend.position="none")

fig1 <- grid.arrange(aa, bb, cc, dd, ee, ff, gg, hh, ii, jj, kk, ncol=3, nrow=4) 
fig1

# Soil respiration partitioning

setwd("~/Github/gemcarbon_data/processed_ts_2017/ts_soil_respiration")

ACJ01   <- read.table("ts_ACJ01_Rs_part_2017.csv", sep=",", header=T)
ESP01   <- read.table("ts_ESP01_Rs_part_2017.csv", sep=",", header=T) 
PAN02   <- read.table("ts_PAN02_Rs_part_2017.csv", sep=",", header=T) 
PAN03   <- read.table("ts_PAN03_Rs_part_2017.csv", sep=",", header=T) 
SPD01   <- read.table("ts_SPD01_Rs_part_2017.csv", sep=",", header=T) 
SPD02   <- read.table("ts_SPD02_Rs_part_2017.csv", sep=",", header=T) 
TAM05   <- read.table("ts_TAM05_Rs_part_2017.csv", sep=",", header=T) ############################## TO DO
TAM06   <- read.table("ts_TAM06_Rs_part_2017.csv", sep=",", header=T) 
TAM09   <- read.table("ts_TAM09_Rs_part_2017.csv", sep=",", header=T) 
TRU04   <- read.table("ts_TRU04_Rs_part_2017.csv", sep=",", header=T)       
WAY01   <- read.table("ts_WAY01_Rs_part_2017.csv", sep=",", header=T) 

# SOMETHING IS WRONG WITH MAT!!! GO BACK TO THE CODE soil_respiration_persubplot_2017

tacj1   <- mean(ACJ01$soil_temp_c_out, na.rm=T)
tesp1   <- mean(ESP01$soil_temp_c_out, na.rm=T) 
tpan2   <- mean(PAN02$soil_temp_c_out, na.rm=T) 
tpan3   <- mean(PAN03$soil_temp_c_out, na.rm=T) 
tspd1   <- mean(SPD01$soil_temp_c_out, na.rm=T)
tspd2   <- mean(SPD02$soil_temp_c_out, na.rm=T)
ttam5   <- mean(TAM05$soil_temp_c_out, na.rm=T) ############################## TO DO
ttam6   <- mean(TAM06$soil_temp_c_out, na.rm=T) 
ttam9   <- mean(TAM09$soil_temp_c_out, na.rm=T)
ttru4   <- mean(TRU04$soil_temp_c_out, na.rm=T)    
tway1   <- mean(WAY01$soil_temp_c_out, na.rm=T)

scale_colour_brewer(palette="Paired", name = "Rsoil component") +

aa <- ggplot(ACJ01, aes(x = date, y = Rs_root_MgC_ha_mo, na.rm = T)) +
      geom_point(data = ACJ01, aes(x = date, y = Rs_total_MgC_ha_mo), size = 2, colour = "darkgrey", na.rm=T) +
      geom_point(data = ACJ01, aes(x = date, y = Rs_root_MgC_ha_mo), size = 2, colour = "blue", na.rm=T) +
      geom_point(data = ACJ01, aes(x = date, y = Rs_het_MgC_ha_mo), size = 2, colour = "red", na.rm=T) +
      ggtitle("ACJ01")

bb <- ggplot(ESP01, aes(x = date, y = Rs_root_MgC_ha_mo, na.rm = T)) +
      geom_point(data = ESP01, aes(x = date, y = Rs_total_MgC_ha_mo), size = 2, colour = "darkgrey", na.rm=T) +
      geom_point(data = ESP01, aes(x = date, y = Rs_root_MgC_ha_mo), size = 2, colour = "blue", na.rm=T) +
      geom_point(data = ESP01, aes(x = date, y = Rs_het_MgC_ha_mo), size = 2, colour = "red", na.rm=T) +
      ggtitle("ESP01")

cc <- ggplot(PAN02, aes(x = date, y = Rs_root_MgC_ha_mo, na.rm = T)) +
      geom_point(data = PAN02, aes(x = date, y = Rs_total_MgC_ha_mo), size = 2, colour = "darkgrey", na.rm=T) +
      geom_point(data = PAN02, aes(x = date, y = Rs_root_MgC_ha_mo), size = 2, colour = "blue", na.rm=T) +
      geom_point(data = PAN02, aes(x = date, y = Rs_het_MgC_ha_mo), size = 2, colour = "red", na.rm=T) +
      ggtitle("PAN02")

dd <- ggplot(PAN03, aes(x = date, y = Rs_root_MgC_ha_mo, na.rm = T)) +
      geom_point(data = ESP01, aes(x = date, y = Rs_total_MgC_ha_mo), size = 2, colour = "darkgrey", na.rm=T) +
      geom_point(data = ESP01, aes(x = date, y = Rs_root_MgC_ha_mo), size = 2, colour = "blue", na.rm=T) +
      geom_point(data = ESP01, aes(x = date, y = Rs_het_MgC_ha_mo), size = 2, colour = "red", na.rm=T) +
      ggtitle("PAN03")

ee <- ggplot(SPD01, aes(x = date, y = Rs_root_MgC_ha_mo, na.rm = T)) +
      geom_point(data = SPD01, aes(x = date, y = Rs_total_MgC_ha_mo), size = 2, colour = "darkgrey", na.rm=T) +
      geom_point(data = SPD01, aes(x = date, y = Rs_root_MgC_ha_mo), size = 2, colour = "blue", na.rm=T) +
      geom_point(data = SPD01, aes(x = date, y = Rs_het_MgC_ha_mo), size = 2, colour = "red", na.rm=T) +
      ggtitle("SPD01")

ff <- ggplot(SPD02, aes(x = date, y = Rs_root_MgC_ha_mo, na.rm = T)) +
      geom_point(data = SPD02, aes(x = date, y = Rs_total_MgC_ha_mo), size = 2, colour = "darkgrey", na.rm=T) +
      geom_point(data = SPD02, aes(x = date, y = Rs_root_MgC_ha_mo), size = 2, colour = "blue", na.rm=T) +
      geom_point(data = SPD02, aes(x = date, y = Rs_het_MgC_ha_mo), size = 2, colour = "red", na.rm=T) +
      ggtitle("SPD02")

gg <- ggplot(TAM06, aes(x = date, y = Rs_root_MgC_ha_mo, na.rm = T)) +
      geom_point(data = TAM06, aes(x = date, y = Rs_total_MgC_ha_mo), size = 2, colour = "darkgrey", na.rm=T) +
      geom_point(data = TAM06, aes(x = date, y = Rs_root_MgC_ha_mo), size = 2, colour = "blue", na.rm=T) +
      geom_point(data = TAM06, aes(x = date, y = Rs_het_MgC_ha_mo), size = 2, colour = "red", na.rm=T) +
      ggtitle("TAM06")

hh <- ggplot(TAM09, aes(x = date, y = Rs_root_MgC_ha_mo, na.rm = T)) +
      geom_point(data = TAM09, aes(x = date, y = Rs_total_MgC_ha_mo), size = 2, colour = "darkgrey", na.rm=T) +
      geom_point(data = TAM09, aes(x = date, y = Rs_root_MgC_ha_mo), size = 2, colour = "blue", na.rm=T) +
      geom_point(data = TAM09, aes(x = date, y = Rs_het_MgC_ha_mo), size = 2, colour = "red", na.rm=T) +
      ggtitle("TAM09")

ii <- ggplot(TRU04, aes(x = date, y = Rs_root_MgC_ha_mo, na.rm = T)) +
      geom_point(data = TRU04, aes(x = date, y = Rs_total_MgC_ha_mo), size = 2, colour = "darkgrey", na.rm=T) +
      geom_point(data = TRU04, aes(x = date, y = Rs_root_MgC_ha_mo), size = 2, colour = "blue", na.rm=T) +
      geom_point(data = TRU04, aes(x = date, y = Rs_het_MgC_ha_mo), size = 2, colour = "red", na.rm=T) +
      ggtitle("TRU04")

jj <- ggplot(WAY01, aes(x = date, y = Rs_root_MgC_ha_mo, na.rm = T)) +
      geom_point(data = WAY01, aes(x = date, y = Rs_total_MgC_ha_mo), size = 2, colour = "darkgrey", na.rm=T) +
      geom_point(data = WAY01, aes(x = date, y = Rs_root_MgC_ha_mo), size = 2, colour = "blue", na.rm=T) +
      geom_point(data = WAY01, aes(x = date, y = Rs_het_MgC_ha_mo), size = 2, colour = "red", na.rm=T) +
      ggtitle("WAY01")


fig2 <- grid.arrange(aa, bb, cc, dd, ee, ff, gg, hh, ii, jj, ncol=3, nrow=4) 
fig2


# R STEM

aa <- ggplot(a, aes(x = date, y = flux_MgChamonth, na.rm = T)) +
      geom_point(data = a, aes(x = date, y = flux_MgChamonth), size = 2, colour = a$year, na.rm=T) +
      ggtitle(plotname)



# FLF

setwd("~/Github/GEMcarbon.R")
source("~/Github/GEMcarbon.R/flf_2016.R")
data_flf <- read.table("~/Github/gemcarbon_data/raw_data_ingembd/flf_all_4April.csv", sep=",", header=T)

ts_flf_2017_afr <- flf(subset(data_flf, plot_code %in% c("KOG-02", "KOG-03", "KOG-04", "KOG-05", "KOG-06", "BOB-01", "BOB-02", "BOB-03", "BOB-04", "BOB-05", "BOB-06", "ANK-01", "ANK-02", "ANK-03", "LPG-01", "LPG-02", "IVI-01", "IVI-02")), plotit = F) 
ts_flf_2017_sea <- flf(subset(data_flf, plot_code %in% c("SAF-01", "SAF-02", "SAF-03", "SAF-04", "SAF-05", "MLA-01", "MLA-02", "DAN-04", "DAN-05", "MNG-03", "MNG-04")), plotit = F) 
ts_flf_2017_sa  <- flf(subset(data_flf, plot_code %in% c("KEN-02", "SPD-02", "WAY-01", "ESP-01", "ALP-11", "ALP-12", "KEN-01", "SPD-01", "TAM-05", "TAM-06", "TAM-09", "ACJ-01", "PAN-02", "PAN-03", "TRU-04", "BLZ-11", "BLZ-12", "BLZ-21", "BLZ-22")), plotit = F) 

# I tested these individually, they work:
#SA: "KEN-02", "SPD-02", "WAY-01", "ALP-11", "ALP-12", "KEN-01", "SPD-01", "TAM-05", "TAM-06", "BLZ-11", "BLZ-12", "BLZ-21", "BLZ-22",
#SEA: "SAF-02", "SAF-01", "SAF-04"
#AFR: "KOG-06", "KOG-05", "KOG-04", "KOG-03", "KOG-02", "BOB-01", "BOB-02", "BOB-03", "BOB-04", "BOB-05", "BOB-06", "ANK-03"

# Not working (mainly when plotit = T)
# TAM-09 ESP-01 
# SAF-03 MLA-01 SAF-05 
# LPG-01 LPG-02 "ANK-02"
# We only have total for: TRU-04

unique(data_flf$plot_code)
plotname <- "ANK-03"

str(ts_flf_2017) 

setwd("~/Github/gemcarbon_data/processed_ts_2017")
write.csv(ts_flf_2017_afr, file="ts_flf_2017_afr.csv")
write.csv(ts_flf_2017_sea, file="ts_flf_2017_sea.csv")
write.csv(ts_flf_2017_sa, file="ts_flf_2017_sa.csv")

# http://www.r-graph-gallery.com/ 
# Is it possible to combine two palettes (Paired, Set3)??
colourCount = length(unique(ts_flf_2017_sa$plot))
getPalette = colorRampPalette(brewer.pal(12, "Paired"))

mindate <- min(ts_flf_2017_sa$date)
  
afr <- ggplot(ts_flf_2017_afr, aes(date, leavesflf_MgC_ha_month, colour = factor(ts_flf_2017_afr$plot))) + 
             geom_point() +
             scale_colour_manual(values=rep(brewer.pal(12,"Paired"), times=2), name = "") +
             theme(legend.position = "bottom") +
             xlab("") + ylab(expression(paste("Leaf litterfall (MgC ",ha^-1, mo^-1, ")", sep=""))) +
             xlim(mindate, max(ts_flf_2017_afr$date)) +
             ggtitle("Africa") 
afr

sa <- ggplot(ts_flf_2017_sa, aes(date, leavesflf_MgC_ha_month, colour = factor(ts_flf_2017_sa$plot))) + 
             geom_point() +
             scale_colour_manual(values=rep(brewer.pal(12,"Paired"), times=2), name = "") +
             scale_shape_manual(values=rep(c(15,16,17,18,19), each=5)) +
             # scale_colour_brewer(palette="Paired", name = "plot code") +
             # scale_fill_manual(values = getPalette(colourCount)) + 
             theme(legend.position = "bottom") +
             xlab("") + ylab(expression(paste("Leaf litterfall (MgC ",ha^-1, mo^-1, ")", sep=""))) +
             #ylim(0, 1.5) + xlim(mindate, max(ts_flf_2017_sa$date)) +# ATTENTION!!!! CHECK OUT THE OUTLIERS IN TAMBOPATA.
             ggtitle("South America") 
sa

sea <- ggplot(ts_flf_2017_sea, aes(date, leavesflf_MgC_ha_month, colour = factor(ts_flf_2017_sea$plot))) + 
              geom_point() +
              scale_colour_brewer(palette="Paired", name = "") +
              theme(legend.position = "bottom") +
              xlab("") + ylab(expression(paste("Leaf litterfall (MgC ",ha^-1, mo^-1, ")", sep=""))) +
              xlim(mindate, max(ts_flf_2017_sea$date)) +
              ggtitle("South East Asia") 
sea


fig3 <- grid.arrange(sa, afr, sea, ncol=1, nrow=3) 
fig3

# IC

setwd("~/Github/GEMcarbon.R")
source("~/Github/GEMcarbon.R/NPProot_2015.R")
data_ic <- read.table("~/Github/gemcarbon_data/raw_data_ingembd/ic_all_11April.csv", sep=",", header=T)
#data_ic$plot_code <- revalue(data_ic$plot_code, c("DC1" = "DAN-04", "DC2" = "DAN-05", "BZ11" = "BLZ-11", "BZ12" = "BLZ-12", "BZ22" = "BLZ-22", "BZ21" = "BLZ-21", "OP" = "OP"))

ts_ic_2017_test <- NPProot_ic(subset(data_ic, plot_code %in% c("ACJ-01", "ESP-01"), ret_type = "list")) 

ts_ic_2017_test[["three_monthly"]]
# ACJ-01 ESP-01 WAY-01 ALP-11 ALP-12 PAN-02 PAN-03 SPD-01 SPD-02 TAM-05 TAM-06 TAM-09 TAN-01 TAN-02 TRU-4 PAN-01 KEN-01 KEN-02 
# MNG-03 MNG-04 LPG-01 LPG-02 IVI-01 IVI-02 ANK-01 ANK-02 ANK-03 BOB-01 BOB-02 BOB-03 BOB-04 BOB-05 BOB-06 KOG-02 KOG-03 KOG-04 KOG-05 KOG-06
# SAF-05 SAF-03 MLA-01 SAF-01 SAF-02 MLA-02 SAF-04     
# DC1 DC2 BZ11 BZ12 BZ21 BZ22 OP


