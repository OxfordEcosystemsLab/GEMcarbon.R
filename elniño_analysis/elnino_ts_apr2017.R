

library(zoo)
library(sqldf)
library(dplyr)
library(plyr)
library(grDevices)
library(RColorBrewer)
require(ggplot2)
require(gridExtra)

# revalue plot_codes
rsoil_sea$Plot <- revalue(rsoil_sea$Plot, c("Tower" = "SAF-05", "E" = "SAF-03", "B South" = "SAF-01", "B North" = "SAF-02", "Seraya" = "MLA-02", "Belian" = "MLA-01", "LF" = "SAF-04", "Danum Carbon 1" = "DAN-04", "Danum Carbon 2" = "DAN-05", "BZ11" = "BLZ-11", "BZ12" = "BLZ-12", "BZ22" = "BLZ-22", "BZ21" = "BLZ-21", "Ank-02" = "ANK-02"))


### R SOIL ###

setwd("~/Github/gemcarbon_data/processed_ts_2017/ts_soil_respiration")

# SA

ACJ01   <- read.table("ts_ACJ01_Rs_part_2017.csv", sep=",", header=T)
ESP01   <- read.table("ts_ESP01_Rs_part_2017.csv", sep=",", header=T) 
PAN02   <- read.table("ts_PAN02_Rs_part_2017.csv", sep=",", header=T) 
PAN03   <- read.table("ts_PAN03_Rs_part_2017.csv", sep=",", header=T) 
SPD01   <- read.table("ts_SPD01_Rs_part_2017.csv", sep=",", header=T) 
SPD02   <- read.table("ts_SPD02_Rs_part_2017.csv", sep=",", header=T) 
TAM05   <- read.table("ts_TAM05_Rs_part_2017.csv", sep=",", header=T) ############################## TO DO
TAM06   <- read.table("ts_TAM06_Rs_part_mar17.csv", sep=",", header=T) 
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

# AFR

## AFRICA RHET

# read in data
raw_con   <- read.table("~/Github/gemcarbon_data/raw_data_ingembd/soil_respiration/XXX.csv", sep=",", header=T)
raw_tot   <- read.table("~/Github/gemcarbon_data/raw_data_ingembd/soil_respiration/Afr_Rtotal/ ## This is not the right file. Cecilia is working on Total Resp Ghana. GhanaTotalSoilRespUploadv4.csv", sep=",", header=T)
raw_tot$soil_temp_c = 25
raw_tot$soil_vwc_per = 20
raw_tot$ch_fill = 7

weather      <- read.table("~/Github/gemcarbon_data/raw_data_ingembd/soil_respiration/XXX.csv", sep=",", header=T) 

#raw_parLPG   <- read.table("~/Github/gemcarbon_data/raw_data_ingembd/soil_respiration/LPG_PART.csv", sep=",", header=T)
raw_parBOB    <- read.table("~/Github/gemcarbon_data/raw_data_ingembd/soil_respiration/BOB_PART.csv", sep=",", header=T)
#raw_parKOG   <- read.table("~/Github/gemcarbon_data/raw_data_ingembd/soil_respiration/XXX.csv", sep=",", header=T)
raw_par       <- raw_parBOB #rbind(raw_parsrLPG, raw_parsrBOB, raw_parsrKOG)

# Call function Rflux. This simply estimates the flux for each measurement.
setwd("~/Github/GEMcarbon.R")
source("~/Github/GEMcarbon.R/EGM_fluxfunction_2017.R")

# Temp data for Africa 
#raw_parsr$air_temp_c <- 25
#raw_parsr$ch_fill <- 5

test <- Rflux(raw_tot, ret="Res", "KOG-04")

bob01 <- Rflux(raw_parsrBOB, ret="Res", "BOB-01")
bob02 <- Rflux(raw_parsrBOB, ret="Res", "BOB-02")
bob03 <- Rflux(raw_parsrBOB, ret="Res", "BOB-03")
bob04 <- Rflux(raw_parsrBOB, ret="Res", "BOB-04")
bob05 <- Rflux(raw_parsrBOB, ret="Res", "BOB-05")
bob06 <- Rflux(raw_parsrBOB, ret="Res", "BOB-06")

bob   <- data.frame(rbind(bob01, bob02, bob03, bob04, bob05, bob06))

# remove outliers (> 3 SD) and NAs:
bob$Rflux_MgC_ha_mo1 <- rm.flux.outlier(bob$Rflux_MgC_ha_mo, sd_interval = 2)                     # Discuss sd_interval with team: it makes a big difference to the data if you use 2 sd or 3 sd.

ctrl                <- subset(bob, part_code=="con_no_lit")
ctrl$id             <- as.factor(paste(ctrl$plot_code, ctrl$replica, ctrl$day, ctrl$month, ctrl$year, sep="."))
avgctrl             <- data.frame(tapply(ctrl$Rflux_MgC_ha_mo, ctrl$id, mean, na.rm=T))
avgctrl$id          <- rownames(avgctrl)
het                 <- subset(bob, part_code=="so_no_lit") ## Check this is the right way to do this.
het$id              <- as.factor(paste(het$plot_code, het$replica, het$day, het$month, het$year, sep="."))
avghet              <- data.frame(tapply(het$Rflux_MgC_ha_mo, het$id, mean, na.rm=T))
avghet$id           <- rownames(avghet)
partbob           <- merge(avgctrl, avghet, by = "id") 
colnames(partbob) <- c("id", "rctrl", "rhet")
partbob$aut       <- partbob$rhet - partbob$rctrl

temp = (strsplit(partbob$id, "[.]")) 
partbob$plot_code = unlist(lapply(temp, `[[`, 1))
partbob$replica = unlist(lapply(temp, `[[`, 2))
partbob$day = unlist(lapply(temp, `[[`, 3))
partbob$month = unlist(lapply(temp, `[[`, 4))
partbob$year = unlist(lapply(temp, `[[`, 5))

partbob$date <- strptime(paste(as.character(partbob$year), as.character(partbob$month), as.character(partbob$day), sep="-"), format="%Y-%m-%d")  


# Plot results

bob01 <- subset(partbob, plot_code=="BOB-01")
bob02 <- subset(partbob, plot_code=="BOB-02")
bob03 <- subset(partbob, plot_code=="BOB-03")
bob04 <- subset(partbob, plot_code=="BOB-04")
bob05 <- subset(partbob, plot_code=="BOB-05")
bob06 <- subset(partbob, plot_code=="BOB-06")


aa <- ggplot(bob01, aes(x = date, y = rctrl, na.rm = T)) +
             geom_point(data = bob01, aes(x = date, y = rctrl), size = 2, colour = "orange", na.rm=T) +
             geom_point(data = bob01, aes(x = date, y = rhet), size = 2, colour = "grey", na.rm=T) +
             geom_point(data = bob01, aes(x = date, y = aut), size = 2, colour = "turquoise", na.rm=T) +
             ggtitle("Rsoil partitionning BOB-01")

bb <- ggplot(bob02, aes(x = date, y = rctrl, na.rm = T)) +
  geom_point(data = bob02, aes(x = date, y = rctrl), size = 2, colour = "orange", na.rm=T) +
  geom_point(data = bob02, aes(x = date, y = rhet), size = 2, colour = "grey", na.rm=T) +
  geom_point(data = bob02, aes(x = date, y = aut), size = 2, colour = "turquoise", na.rm=T) +
  ggtitle("Rsoil partitionning BOB-02")

cc <- ggplot(bob03, aes(x = date, y = rctrl, na.rm = T)) +
  geom_point(data = bob03, aes(x = date, y = rctrl), size = 2, colour = "orange", na.rm=T) +
  geom_point(data = bob03, aes(x = date, y = rhet), size = 2, colour = "grey", na.rm=T) +
  geom_point(data = bob03, aes(x = date, y = aut), size = 2, colour = "turquoise", na.rm=T) +
  ggtitle("Rsoil partitionning BOB-03")

dd <- ggplot(bob04, aes(x = date, y = rctrl, na.rm = T)) +
  geom_point(data = bob04, aes(x = date, y = rctrl), size = 2, colour = "orange", na.rm=T) +
  geom_point(data = bob04, aes(x = date, y = rhet), size = 2, colour = "grey", na.rm=T) +
  geom_point(data = bob04, aes(x = date, y = aut), size = 2, colour = "turquoise", na.rm=T) +
  ggtitle("Rsoil partitionning BOB-04")

ee <- ggplot(bob05, aes(x = date, y = rctrl, na.rm = T)) +
  geom_point(data = bob05, aes(x = date, y = rctrl), size = 2, colour = "orange", na.rm=T) +
  geom_point(data = bob05, aes(x = date, y = rhet), size = 2, colour = "grey", na.rm=T) +
  geom_point(data = bob05, aes(x = date, y = aut), size = 2, colour = "turquoise", na.rm=T) +
  ggtitle("Rsoil partitionning BOB-05")

ff <- ggplot(bob06, aes(x = date, y = rctrl, na.rm = T)) +
  geom_point(data = bob06, aes(x = date, y = rctrl), size = 2, colour = "orange", na.rm=T) +
  geom_point(data = bob06, aes(x = date, y = rhet), size = 2, colour = "grey", na.rm=T) +
  geom_point(data = bob06, aes(x = date, y = aut), size = 2, colour = "turquoise", na.rm=T) +
  ggtitle("Rsoil partitionning BOB-06")

#################################################################################################################
#################################################################################################################

# Total soil respiration

datafile <- subset(raw_tot, plot_code=="BOB-03")
plotname="BOB-03"
datafile$soil_temp_c = 25
datafile$soil_vwc_per = 20
datafile$ch_fill = 7

bob01 <- Rflux(raw_tot, ret="Res", "BOB-01")
bob02 <- Rflux(raw_tot, ret="Res", "BOB-02")
bob03 <- Rflux(raw_tot, ret="Res", "BOB-03")
bob04 <- Rflux(raw_tot, ret="Res", "BOB-04")
bob05 <- Rflux(raw_tot, ret="Res", "BOB-05")
bob06 <- Rflux(raw_tot, ret="Res", "BOB-06")
kog02 <- Rflux(raw_tot, ret="Res", "KOG-02")
kog03 <- Rflux(raw_tot, ret="Res", "KOG-03")
kog04 <- Rflux(raw_tot, ret="Res", "KOG-04")


bob   <- data.frame(rbind(bob01, bob02, bob03, bob04, bob05, bob06, kog02, kog03, kog04))

# remove outliers (> 3 SD) and NAs:
bob$Rflux_MgC_ha_mo1 <- rm.flux.outlier(bob$Rflux_MgC_ha_mo, sd_interval = 2)                     # Discuss sd_interval with team: it makes a big difference to the data if you use 2 sd or 3 sd.
bob$date <- strptime(paste(as.character(bob$year), as.character(bob$month), as.character(bob$day), sep="-"), format="%Y-%m-%d")  

aa <- ggplot(bob, aes(x = date, y = Rflux_MgC_ha_mo, na.rm = T)) +
             geom_point(data = bob, aes(x = date, y = Rflux_MgC_ha_mo), size = 2, colour = "green", na.rm=T) +
             ggtitle("Total Rsoil Bobiri")
aa


bob %>% summary
bob$month %>% summary

# Save to directory
setwd("~/Github/gemcarbon_data/")
write.csv(partbob, file="BOB_PART.csv")
write.csv(bob, file="ts_Rs_total_Ghana.csv")

# SEA
rsoilsea   <- read.table("~/Github/gemcarbon_data/raw_data_ingembd/soil_respiration/SAFE_SoilRespiration_Data_toCecile3.csv", sep=",", header=T, fill = TRUE)
# revalue plot_codes
rsoilsea$Plot <- revalue(rsoilsea$Plot, c("Tower" = "SAF-05", "E" = "SAF-03", "B South" = "SAF-01", "B North" = "SAF-02", "Seraya" = "MLA-02", "Belian" = "MLA-01", "LF" = "SAF-04", "DC1" = "DAN-04", "DC2" = "DAN-05"))


# SOUTH AMERICA

aa <- ggplot(ACJ01, aes(x = date, y = Rs_root_MgC_ha_mo, na.rm = T)) +
      #geom_point(data = ACJ01, aes(x = date, y = Rs_total_MgC_ha_mo), size = 2, colour = "darkgrey", na.rm=T) +
      #geom_point(data = ACJ01, aes(x = date, y = Rs_root_MgC_ha_mo), size = 2, colour = "blue", na.rm=T) +
      geom_point(data = ACJ01, aes(x = date, y = Rs_het_MgC_ha_mo), size = 2, colour = "darkgrey", na.rm=T) +
      ggtitle("ACJ01")

bb <- ggplot(ESP01, aes(x = date, y = Rs_root_MgC_ha_mo, na.rm = T)) +
      #geom_point(data = ESP01, aes(x = date, y = Rs_total_MgC_ha_mo), size = 2, colour = "darkgrey", na.rm=T) +
      #geom_point(data = ESP01, aes(x = date, y = Rs_root_MgC_ha_mo), size = 2, colour = "blue", na.rm=T) +
      geom_point(data = ESP01, aes(x = date, y = Rs_het_MgC_ha_mo), size = 2, colour = "darkgrey", na.rm=T) +
      ggtitle("ESP01")

cc <- ggplot(PAN02, aes(x = date, y = Rs_root_MgC_ha_mo, na.rm = T)) +
      #geom_point(data = PAN02, aes(x = date, y = Rs_total_MgC_ha_mo), size = 2, colour = "darkgrey", na.rm=T) +
      #geom_point(data = PAN02, aes(x = date, y = Rs_root_MgC_ha_mo), size = 2, colour = "blue", na.rm=T) +
      geom_point(data = PAN02, aes(x = date, y = Rs_het_MgC_ha_mo), size = 2, colour = "darkgrey", na.rm=T) +
      ggtitle("PAN02")

dd <- ggplot(PAN03, aes(x = date, y = Rs_root_MgC_ha_mo, na.rm = T)) +
      #geom_point(data = ESP01, aes(x = date, y = Rs_total_MgC_ha_mo), size = 2, colour = "darkgrey", na.rm=T) +
      #geom_point(data = ESP01, aes(x = date, y = Rs_root_MgC_ha_mo), size = 2, colour = "blue", na.rm=T) +
      geom_point(data = ESP01, aes(x = date, y = Rs_het_MgC_ha_mo), size = 2, colour = "darkgrey", na.rm=T) +
      ggtitle("PAN03")

ee <- ggplot(SPD01, aes(x = date, y = Rs_root_MgC_ha_mo, na.rm = T)) +
      #geom_point(data = SPD01, aes(x = date, y = Rs_total_MgC_ha_mo), size = 2, colour = "darkgrey", na.rm=T) +
      #geom_point(data = SPD01, aes(x = date, y = Rs_root_MgC_ha_mo), size = 2, colour = "blue", na.rm=T) +
      geom_point(data = SPD01, aes(x = date, y = Rs_het_MgC_ha_mo), size = 2, colour = "darkgrey", na.rm=T) +
      ggtitle("SPD01")

ff <- ggplot(SPD02, aes(x = date, y = Rs_root_MgC_ha_mo, na.rm = T)) +
      #geom_point(data = SPD02, aes(x = date, y = Rs_total_MgC_ha_mo), size = 2, colour = "darkgrey", na.rm=T) +
      #geom_point(data = SPD02, aes(x = date, y = Rs_root_MgC_ha_mo), size = 2, colour = "blue", na.rm=T) +
      geom_point(data = SPD02, aes(x = date, y = Rs_het_MgC_ha_mo), size = 2, colour = "darkgrey", na.rm=T) +
      ggtitle("SPD02")

gg <- ggplot(TAM06, aes(x = date, y = Rs_root_MgC_ha_mo, na.rm = T)) +
      #geom_point(data = TAM06, aes(x = date, y = Rs_total_MgC_ha_mo), size = 2, colour = "darkgrey", na.rm=T) +
      #geom_point(data = TAM06, aes(x = date, y = Rs_root_MgC_ha_mo), size = 2, colour = "blue", na.rm=T) +
      geom_point(data = TAM06, aes(x = date, y = Rs_het_MgC_ha_mo), size = 2, colour = "darkgrey", na.rm=T) +
      ggtitle("TAM06")

hh <- ggplot(TAM09, aes(x = date, y = Rs_root_MgC_ha_mo, na.rm = T)) +
      #geom_point(data = TAM09, aes(x = date, y = Rs_total_MgC_ha_mo), size = 2, colour = "darkgrey", na.rm=T) +
      #geom_point(data = TAM09, aes(x = date, y = Rs_root_MgC_ha_mo), size = 2, colour = "blue", na.rm=T) +
      geom_point(data = TAM09, aes(x = date, y = Rs_het_MgC_ha_mo), size = 2, colour = "darkgrey", na.rm=T) +
      ggtitle("TAM09")

ii <- ggplot(TRU04, aes(x = date, y = Rs_root_MgC_ha_mo, na.rm = T)) +
      #geom_point(data = TRU04, aes(x = date, y = Rs_total_MgC_ha_mo), size = 2, colour = "darkgrey", na.rm=T) +
      #geom_point(data = TRU04, aes(x = date, y = Rs_root_MgC_ha_mo), size = 2, colour = "blue", na.rm=T) +
      geom_point(data = TRU04, aes(x = date, y = Rs_het_MgC_ha_mo), size = 2, colour = "darkgrey", na.rm=T) +
      ggtitle("TRU04")

jj <- ggplot(WAY01, aes(x = date, y = Rs_root_MgC_ha_mo, na.rm = T)) +
      #geom_point(data = WAY01, aes(x = date, y = Rs_total_MgC_ha_mo), size = 2, colour = "darkgrey", na.rm=T) +
      #geom_point(data = WAY01, aes(x = date, y = Rs_root_MgC_ha_mo), size = 2, colour = "blue", na.rm=T) +
      geom_point(data = WAY01, aes(x = date, y = Rs_het_MgC_ha_mo), size = 2, colour = "darkgrey", na.rm=T) +
      ggtitle("WAY01")

fig2 <- grid.arrange(aa, bb, cc, dd, ee, ff, gg, hh, ii, jj, ncol=3, nrow=4) 
fig2

#  AFRICA

afr_aa <- ggplot(LPG01, aes(x = date, y = Rs_root_MgC_ha_mo, na.rm = T)) +
  #geom_point(data = WAY01, aes(x = date, y = Rs_total_MgC_ha_mo), size = 2, colour = "darkgrey", na.rm=T) +
  #geom_point(data = WAY01, aes(x = date, y = Rs_root_MgC_ha_mo), size = 2, colour = "blue", na.rm=T) +
  geom_point(data = WAY01, aes(x = date, y = Rs_het_MgC_ha_mo), size = 2, colour = "darkgrey", na.rm=T) +
  ggtitle("LPG01")

# SOUTH EAST ASIA

totsea <- subset(rsoilsea, Quality == 1 & Collar_type %in% c("Total"))
hetsea <- subset(rsoilsea, Quality == 1 & Collar_type %in% c("C3"))
totsea$id <- paste(totsea$Plot, totsea$Subplot, totsea$year, totsea$month, totsea$day, sep=".")
hetsea$id <- paste(hetsea$Plot, hetsea$Subplot, hetsea$year, hetsea$month, hetsea$day, sep=".")
rsoil_sea <- merge(totsea, hetsea, by = "id") # all - Only rows with data from both x and y are included in the output
rsoil_sea$raut_MgC_ha_month <- rsoil_sea$Flux_MgC_ha_month.x - rsoil_sea$Flux_MgC_ha_month.y
rsoil_sea$date <- as.Date(paste(rsoil_sea$year.x, rsoil_sea$month.x, rsoil_sea$day.x, sep="."), format="%Y.%m.%d") 

saf01 <- subset(rsoil_sea, Plot.x %in% c("SAF-01"))
saf02 <- subset(rsoil_sea, Plot.x %in% c("SAF-02"))
saf03 <- subset(rsoil_sea, Plot.x %in% c("SAF-03"))
saf04 <- subset(rsoil_sea, Plot.x %in% c("SAF-04"))
saf05 <- subset(rsoil_sea, Plot.x %in% c("SAF-05"))
saf06 <- subset(rsoil_sea, Plot.x %in% c("SAF-06"))



aa <- ggplot(saf01, aes(x = date, y = Flux_MgC_ha_month.x, na.rm = T)) +
             geom_point(data = saf01, aes(x = date, y = Flux_MgC_ha_month.x), size = 2, colour = "orange", na.rm=T) +
             geom_point(data = saf01, aes(x = date, y = Flux_MgC_ha_month.y), size = 2, colour = "turquoise", na.rm=T) +
             geom_point(data = saf01, aes(x = date, y = raut_MgC_ha_month), size = 2, shape=79, colour = "navy", na.rm=T) +
             ggtitle("SAF-01 (B South)")

bb <- ggplot(saf02, aes(x = date, y = Flux_MgC_ha_month.x, na.rm = T)) +
             geom_point(data = saf02, aes(x = date, y = Flux_MgC_ha_month.x), size = 2, colour = "orange", na.rm=T) +
             geom_point(data = saf02, aes(x = date, y = Flux_MgC_ha_month.y), size = 2, colour = "turquoise", na.rm=T) +
             geom_point(data = saf02, aes(x = date, y = raut_MgC_ha_month), size = 2, shape=79, colour = "navy", na.rm=T) +
             ggtitle("SAF-02 (B North)")

cc <- ggplot(saf03, aes(x = date, y = Flux_MgC_ha_month.x, na.rm = T)) +
             geom_point(data = saf03, aes(x = date, y = Flux_MgC_ha_month.x), size = 2, colour = "orange", na.rm=T) +
             geom_point(data = saf03, aes(x = date, y = Flux_MgC_ha_month.y), size = 2, colour = "turquoise", na.rm=T) +
             geom_point(data = saf03, aes(x = date, y = raut_MgC_ha_month), size = 2, shape=79, colour = "navy", na.rm=T) +
             ggtitle("SAF-03 (E)")

dd <- ggplot(saf04, aes(x = date, y = Flux_MgC_ha_month.x, na.rm = T)) +
             geom_point(data = saf04, aes(x = date, y = Flux_MgC_ha_month.x), size = 2, colour = "orange", na.rm=T) +
             geom_point(data = saf04, aes(x = date, y = Flux_MgC_ha_month.y), size = 2, colour = "turquoise", na.rm=T) +
             geom_point(data = saf04, aes(x = date, y = raut_MgC_ha_month), size = 2, shape=79, colour = "navy", na.rm=T) +
             ggtitle("SAF-04 (LF)")

ee <- ggplot(saf05, aes(x = date, y = Flux_MgC_ha_month.x, na.rm = T)) +
             geom_point(data = saf05, aes(x = date, y = Flux_MgC_ha_month.x), size = 2, colour = "orange", na.rm=T) +
             geom_point(data = saf05, aes(x = date, y = Flux_MgC_ha_month.y), size = 2, colour = "turquoise", na.rm=T) +
             geom_point(data = saf05, aes(x = date, y = raut_MgC_ha_month), size = 2, shape=79, colour = "navy", na.rm=T) +
             ggtitle("SAF-05 (Tower)")

fig3 <- grid.arrange(aa, bb, cc, dd, ee, ncol=1, nrow=5) 
fig3

# Save results
setwd("~/Github/gemcarbon_data/processed_ts_2017/")
write.csv(rsoil_sea, file="ts_rsoil_part_sea_June2017.csv")

# Rsoil NXV
setwd("~/Github/GEMcarbon.R")
source("~/Github/GEMcarbon.R/EGM_fluxfunction_2017.R")

setwd("~/Github/gemcarbon_data/raw_data_ingembd/soil_respiration/NXV")
rsnxv <- read.table("total_rsoil_NXV_2017.csv", sep=",", header=T)

# plot_codes 
rsnxv$plot_code  <- gsub("CDB","NXV-02", rsnxv$plot_code, ignore.case=T)
rsnxv$plot_code  <- gsub("CTB","NXV-01", rsnxv$plot_code, ignore.case=T)
rsnxv$plot_code  <- gsub("PTB","NXV-xx1", rsnxv$plot_code, ignore.case=T)
rsnxv$plot_code  <- gsub("RGB","NXV-xx2", rsnxv$plot_code, ignore.case=T)

# Temp data for NXV 
rsnxv$air_temp_c <- rsnxv$soil_temp_degc # ATTENTION!! It's soil temp.
rsnxv$ch_fill <- 7                       # ATTENTION!! We need real values.

# Testing NXV soil resp data with the function
datafile = rsnxv 
ret="Res"
plotname = "NXV-02"

nxvCDB <- Rflux(rsnxv, ret="Res", "NXV-01")
nxvCTB <- Rflux(rsnxv, ret="Res", "NXV-02")
nxvPTB <- Rflux(rsnxv, ret="Res", "NXV-xx1")
nxvRGB <- Rflux(rsnxv, ret="Res", "NXV-xx2")
 
# dates
nxvCDB$date <- as.Date(paste(nxvCDB$year, nxvCDB$month, nxvCDB$day, sep="."), format="%Y.%m.%d") 
nxvCTB$date <- as.Date(paste(nxvCTB$year, nxvCTB$month, nxvCTB$day, sep="."), format="%Y.%m.%d") 
nxvPTB$date <- as.Date(paste(nxvPTB$year, nxvPTB$month, nxvPTB$day, sep="."), format="%Y.%m.%d") 
nxvRGB$date <- as.Date(paste(nxvRGB$year, nxvRGB$month, nxvRGB$day, sep="."), format="%Y.%m.%d") 

avg_nxvCDB %<%
avg_nxvCTB

avg_nxvCDB = nxvCDB %>% group_by(year, month) %>% 
                        dplyr::summarize(avg = mean(Rflux_MgC_ha_mo, na.rm = T), 
                                         sd = sd(Rflux_MgC_ha_mo, na.rm = T), 
                                         date = max(date))
avg_nxvCDB <- data.frame(avg_nxvCDB)

avg_nxvCTB = nxvCTB %>% group_by(year, month) %>% 
                        dplyr::summarize(avg = mean(Rflux_MgC_ha_mo, na.rm = T), 
                                         sd = sd(Rflux_MgC_ha_mo, na.rm = T), 
                                         date = max(date))
avg_nxvCTB <- data.frame(avg_nxvCTB)


limits <- aes(ymax = avg_nxvCDB$avg + avg_nxvCDB$sd, ymin = avg_nxvCDB$avg - avg_nxvCDB$sd)

aa <- ggplot(nxvCDB, aes(x = nxvCDB$date, y = nxvCDB$Rflux_MgC_ha_mo, na.rm = T)) +
             geom_point(data = nxvCDB, aes(x = date, y = Rflux_MgC_ha_mo), size = 2, colour = "turquoise", na.rm=T) +
             geom_point(data = avg_nxvCDB, aes(x = date, y = avg), size = 2, colour = "red", na.rm=T) +
             #geom_errorbar(data = avg_nxvCDB, aes(ymax = avg + sd, ymin = avg - sd), colour="red", width=.1) +
             ggtitle("NXV-01 total soil respiration (MgC ha-1 mo-1)")
aa

bb <- ggplot(nxvCTB, aes(x = date, y = nxvCTB$Rflux_MgC_ha_mo, na.rm = T)) +
             geom_point(data = nxvCTB, aes(x = date, y = nxvCTB$Rflux_MgC_ha_mo), size = 2, colour = "turquoise", na.rm=T) +
             geom_point(data = avg_nxvCTB, aes(x = date, y = avg), size = 2, colour = "red", na.rm=T) +
             #geom_errorbar(data = avg_nxvCTB, aes(ymax = avg + sd, ymin = avg - sd), colour="red", width=.1) +
             ggtitle("NXV-02 total soil respiration (MgC ha-1 mo-1)")
bb

fig <- grid.arrange(aa, bb, ncol=1, nrow=2) 
fig

# Save results
rsoil_nxv   <- data.frame(rbind(avg_nxvCTB, avg_nxvCDB))
setwd("~/Github/gemcarbon_data/processed_ts_2017/")
write.csv(rsoil_nxv, file="ts_rsoil_tot_nxv_July2017.csv")


### FLF ###

setwd("~/Github/GEMcarbon.R")
source("~/Github/GEMcarbon.R/flf_2016.R")
data_flf <- read.table("~/Github/gemcarbon_data/raw_data_ingembd/flf_all_15July.csv", sep=",", header=T)
#data_flf <- subset(data_flf, plot_code %in% c("TAM-05"))

# PROBLEM: we need to replace missing days by days 1 or 15. Assuming 0 = 1 & 1 = 15
w = which((data_flf$day == 0 & data_flf$year == 2005) | (data_flf$day == 0 & data_flf$year == 2006) | (data_flf$day == 0 & data_flf$year == 2007) | (data_flf$day == 0 & data_flf$year == 2008) | (data_flf$day == 0 & data_flf$year == 2009) | (data_flf$day == 0 & data_flf$year == 2010) | (data_flf$day == 0 & data_flf$year == 2011))
data_flf$day[w] = 2    
v = which((data_flf$day == 1 & data_flf$year == 2005) | (data_flf$day == 1 & data_flf$year == 2006) | (data_flf$day == 1 & data_flf$year == 2007) | (data_flf$day == 1 & data_flf$year == 2008) | (data_flf$day == 1 & data_flf$year == 2009) | (data_flf$day == 1 & data_flf$year == 2010) | (data_flf$day == 1 & data_flf$year == 2011))
data_flf$day[v] = 16    

ts_flf_2017_afr <- flf(subset(data_flf, plot_code %in% c("KOG-02", "KOG-03", "KOG-04", "KOG-05", "KOG-06", "BOB-01", "BOB-02", "BOB-03", "BOB-04", "BOB-05", "BOB-06", "ANK-01", "ANK-02", "ANK-03", "LPG-01", "LPG-02", "IVI-01", "IVI-02")), plotit = F) #, "MNG-03", "MNG-04"
ts_flf_2017_sea <- flf(subset(data_flf, plot_code %in% c("SAF-01", "SAF-02", "SAF-03", "SAF-04", "SAF-05", "MLA-01", "MLA-02", "DAN-04", "DAN-05")), plotit = F) 
ts_flf_2017_sa  <- flf(subset(data_flf, plot_code %in% c("KEN-02", "SPD-02", "WAY-01", "ESP-01", "ALP-11", "ALP-12", "KEN-01", "SPD-01", "TAM-05", "TAM-06", "TAM-09", "ACJ-01", "PAN-02", "PAN-03", "TRU-04", "BLZ-11", "BLZ-12", "BLZ-21", "BLZ-22", "NXV-01", "NXV-02")), plotit = F) 

ts_flf_2017_tam  <- flf(subset(data_flf, plot_code %in% c("TAM-05", "TAM-06", "TAM-09")), plotit = F, ret="monthly.means.ts") 

afr <- data.frame(ts_flf_2017_afr)
sea <- data.frame(ts_flf_2017_sea)
sa  <- data.frame(ts_flf_2017_sa)

# Testing the code

data2$idchar <- as.character(data2$id)
data2$year <- substr(data2$idchar, 10, 13)

test <- ggplot(data_flf2, aes(year, total)) + geom_point() 
test

aa <- subset(ts_flf_2017_tam, plot %in% c("TAM-05"))
bb <- subset(ts_flf_2017_tam, plot %in% c("TAM-06"))
cc <- subset(ts_flf_2017_tam, plot %in% c("TAM-09"))

tam5 <- ggplot(aa, aes(year, totalflf_MgC_ha_month)) + 
        geom_point(colour = "navy") 

tam6 <- ggplot(bb, aes(year, totalflf_MgC_ha_month)) + 
        geom_point(colour = "red2") 

tam9 <- ggplot(cc, aes(year, totalflf_MgC_ha_month)) + 
        geom_point(colour = "turquoise4") 

fig3 <- grid.arrange(tam5, tam6, tam9, ncol=1, nrow=3) 
fig3

tam_raw <- subset(data_flf, plot_code %in% c("TAM-05", "TAM-06", "TAM-09"))

aa <- subset(tam_raw, plot_code %in% c("TAM-05"))
bb <- subset(tam_raw, plot_code %in% c("TAM-06"))
cc <- subset(tam_raw, plot_code %in% c("TAM-09"))

tam5r <- ggplot(aa, aes(year, total_litter_g_per_trap)) + 
               geom_point(colour = "navy") 
tam5r

tam6r <- ggplot(bb, aes(year, leaves_g_per_trap)) + 
               geom_point(colour = "red2") 

tam9r <- ggplot(cc, aes(year, total_litter_g_per_trap)) + 
               geom_point(colour = "turquoise4") 

fig3r <- grid.arrange(tam5r, tam6r, tam9r, ncol=1, nrow=3) 
fig3r

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
plotname <- "TAM-06"

setwd("~/Github/gemcarbon_data/processed_ts_2017/ts_flf")
write.csv(ts_flf_2017_afr, file="ts_flf_2017_afr.csv")
write.csv(ts_flf_2017_sea, file="ts_flf_2017_sea.csv")
write.csv(ts_flf_2017_sa, file="ts_flf_2017_sa.csv")

ts_flf_2017_afr <- read.table("~/Github/gemcarbon_data/processed_ts_2017/ts_flf/ts_flf_2017_afr.csv", sep=",", header=T)
ts_flf_2017_sea <- read.table("~/Github/gemcarbon_data/processed_ts_2017/ts_flf/ts_flf_2017_sea.csv", sep=",", header=T)
ts_flf_2017_sa <- read.table("~/Github/gemcarbon_data/processed_ts_2017/ts_flf/ts_flf_2017_sa.csv", sep=",", header=T)

# Dates
ts_flf_2017_afr$date      <- strptime(paste(as.character(ts_flf_2017_afr$year), as.character(ts_flf_2017_afr$month), as.character(15), sep="-"), format="%Y-%m-%d")
ts_flf_2017_sa$date      <- strptime(paste(as.character(ts_flf_2017_sa$year), as.character(ts_flf_2017_sa$month), as.character(15), sep="-"), format="%Y-%m-%d")
ts_flf_2017_sea$date      <- strptime(paste(as.character(ts_flf_2017_sea$year), as.character(ts_flf_2017_sea$month), as.character(15), sep="-"), format="%Y-%m-%d")

# http://www.r-graph-gallery.com/ 
colourCount = length(unique(ts_flf_2017_sa$plot))
getPalette = colorRampPalette(brewer.pal(12, "Paired")) # ATTENTION: colours are repeated twice.

mindate <- min(ts_flf_2017_afr$date, na.rm=T)

afr <- ggplot(ts_flf_2017_afr, aes(date, leavesflf_MgC_ha_month, colour = factor(ts_flf_2017_afr$plot))) + 
             geom_point() +
             scale_colour_manual(values=rep(brewer.pal(12,"Paired"), times=2), name = "") +
             theme(legend.position = "bottom") +
             xlab("") + ylab(expression(paste("Leaf litterfall (MgC ",ha^-1, mo^-1, ")", sep=""))) +
             #xlim(mindate, max(ts_flf_2017_afr$date)) +
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
             ylim(0, 1.5) + xlim(mindate, max(ts_flf_2017_sa$date)) +# ATTENTION!!!! CHECK OUT THE OUTLIERS IN TAMBOPATA.
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

# Plots per region
# SEA
dsaf = (subset(ts_flf_2017_sea, plot %in% c("SAF-01", "SAF-02", "SAF-03", "SAF-04", "SAF-05"))) 
ddan = (subset(ts_flf_2017_sea, plot %in% c("DAN-04", "DAN-05"))) 
dmla = (subset(ts_flf_2017_sea, plot %in% c("MLA-01", "MLA-02"))) 
  
saf <- ggplot(dsaf, aes(date, leavesflf_MgC_ha_month, colour = factor(dsaf$plot))) + 
  geom_point() +
  scale_colour_manual(values=rep(brewer.pal(12,"Paired"), times=2), name = "") +
  theme(legend.position = "bottom") +
  xlab("") + ylab(expression(paste("Leaf litterfall (MgC ",ha^-1, mo^-1, ")", sep=""))) +
  #xlim(mindate, max(dsaf$date)) +
  ggtitle("SAF") 

dan <- ggplot(ddan, aes(date, leavesflf_MgC_ha_month, colour = factor(ddan$plot))) + 
  geom_point() +
  scale_colour_manual(values=rep(brewer.pal(12,"Paired"), times=2), name = "") +
  scale_shape_manual(values=rep(c(15,16,17,18,19), each=5)) +
  # scale_colour_brewer(palette="Paired", name = "plot code") +
  # scale_fill_manual(values = getPalette(colourCount)) + 
  theme(legend.position = "bottom") +
  xlab("") + ylab(expression(paste("Leaf litterfall (MgC ",ha^-1, mo^-1, ")", sep=""))) +
  ylim(0, 1.5) + xlim(mindate, max(ddan$date)) +# ATTENTION!!!! CHECK OUT THE OUTLIERS IN TAMBOPATA.
  ggtitle("DAN") 

mla <- ggplot(dmla, aes(date, leavesflf_MgC_ha_month, colour = factor(dmla$plot))) + 
  geom_point() +
  scale_colour_brewer(palette="Paired", name = "") +
  theme(legend.position = "bottom") +
  xlab("") + ylab(expression(paste("Leaf litterfall (MgC ",ha^-1, mo^-1, ")", sep=""))) +
  xlim(mindate, max(dmla$date)) +
  ggtitle("MLA") 

fig3a <- grid.arrange(saf, dan, mla, ncol=1, nrow=3) 
fig3a


# SA
dtam  = (subset(ts_flf_2017_sa, plot %in% c("TAM-05", "TAM-06", "TAM-09"))) 
dspd  = (subset(ts_flf_2017_sa, plot %in% c("SPD-01", "SPD-02"))) 
dway = (subset(ts_flf_2017_sa, plot %in% c("ESP-01", "WAY-01"))) 
dken = (subset(ts_flf_2017_sa, plot %in% c("KEN-01", "KEN-02")))

tam <- ggplot(dtam, aes(date, leavesflf_MgC_ha_month, colour = factor(dtam$plot))) + 
  geom_point() +
  scale_colour_manual(values=rep(brewer.pal(12,"Paired"), times=2), name = "") +
  theme(legend.position = "bottom") +
  xlab("") + ylab(expression(paste("Leaf litterfall (MgC ",ha^-1, mo^-1, ")", sep=""))) +
  #ylim(0, 1.5) + # xlim(mindate, max(dtam$date)) +  # ATTENTION!!!! CHECK OUT THE OUTLIERS IN TAMBOPATA.
  ggtitle("TAM") 
tam

spd <- ggplot(dspd, aes(date, leavesflf_MgC_ha_month, colour = factor(dspd$plot))) + 
  geom_point() +
  scale_colour_manual(values=rep(brewer.pal(12,"Paired"), times=2), name = "") +
  scale_shape_manual(values=rep(c(15,16,17,18,19), each=5)) +
  # scale_colour_brewer(palette="Paired", name = "plot code") +
  # scale_fill_manual(values = getPalette(colourCount)) + 
  theme(legend.position = "bottom") +
  xlab("") + ylab(expression(paste("Leaf litterfall (MgC ",ha^-1, mo^-1, ")", sep=""))) +
  ylim(0, 1.5) + #xlim(mindate, max(dspd$date)) +
  ggtitle("SPD") 

way <- ggplot(dway, aes(date, leavesflf_MgC_ha_month, colour = factor(dway$plot))) + 
  geom_point() +
  scale_colour_brewer(palette="Paired", name = "") +
  theme(legend.position = "bottom") +
  xlab("") + ylab(expression(paste("Leaf litterfall (MgC ",ha^-1, mo^-1, ")", sep=""))) +
  ylim(0, 1.5) + #xlim(mindate, max(dway$date)) +
  ggtitle("WAY & ESP") 

ken <- ggplot(dken, aes(date, leavesflf_MgC_ha_month, colour = factor(dken$plot))) + 
  geom_point() +
  scale_colour_manual(values=rep(brewer.pal(12,"Paired"), times=2), name = "") +
  theme(legend.position = "bottom") +
  xlab("") + ylab(expression(paste("Leaf litterfall (MgC ",ha^-1, mo^-1, ")", sep=""))) +
  #ylim(0, 1.5) + # xlim(mindate, max(dken$date)) +  
  ggtitle("KEN") 

fig3b <- grid.arrange(tam, spd, way, ken, ncol=1, nrow=4) 
fig3b

# AFR
dlpg = (subset(ts_flf_2017_afr, plot %in% c("LPG-01", "LPG-02", "SAF-03", "SAF-04", "SAF-05"))) 
dbob = (subset(ts_flf_2017_afr, plot %in% c("BOB-01", "BOB-02", "BOB-03", "BOB-04", "BOB-05", "BOB-06"))) 
dkog = (subset(ts_flf_2017_afr, plot %in% c("KOG-02", "KOG-03", "KOG-04", "KOG-05", "KOG-06"))) 

lpg <- ggplot(dlpg, aes(date, leavesflf_MgC_ha_month, colour = factor(dlpg$plot))) + 
  geom_point() +
  scale_colour_manual(values=rep(brewer.pal(12,"Paired"), times=2), name = "") +
  theme(legend.position = "bottom") +
  xlab("") + ylab(expression(paste("Leaf litterfall (MgC ",ha^-1, mo^-1, ")", sep=""))) +
  #xlim(mindate, max(dlpg$date)) +
  ggtitle("LPG") 

bob <- ggplot(dbob, aes(date, leavesflf_MgC_ha_month, colour = factor(dbob$plot))) + 
  geom_point() +
  scale_colour_manual(values=rep(brewer.pal(12,"Paired"), times=2), name = "") +
  scale_shape_manual(values=rep(c(15,16,17,18,19), each=5)) +
  # scale_colour_brewer(palette="Paired", name = "plot code") +
  # scale_fill_manual(values = getPalette(colourCount)) + 
  theme(legend.position = "bottom") +
  xlab("") + ylab(expression(paste("Leaf litterfall (MgC ",ha^-1, mo^-1, ")", sep=""))) +
  ylim(0, 1.5) + xlim(mindate, max(dbob$date)) +
  ggtitle("BOB") 

kog <- ggplot(dkog, aes(date, leavesflf_MgC_ha_month, colour = factor(dkog$plot))) + 
  geom_point() +
  scale_colour_brewer(palette="Paired", name = "") +
  theme(legend.position = "bottom") +
  xlab("") + ylab(expression(paste("Leaf litterfall (MgC ",ha^-1, mo^-1, ")", sep=""))) +
  xlim(mindate, max(dkog$date)) +
  ggtitle("KOG") 

fig3c <- grid.arrange(lpg, bob, kog, ncol=1, nrow=3) 
fig3c

### IC ###

setwd("~/Github/GEMcarbon.R")
source("~/Github/GEMcarbon.R/NPProot_2015.R")
rawic1 <- read.table("~/Github/gemcarbon_data/raw_data_ingembd/ic_all_10July.csv", sep=",", header=T)

# rename plots
rawic1$plot_code <- revalue(rawic1$plot_code, c("TRU-4" = "TRU-04", "DC1" = "DAN-04", "DC2" = "DAN-05", "BZ11" = "BLZ-11", "BZ12" = "BLZ-12", "BZ22" = "BLZ-22", "BZ21" = "BLZ-21", "OP" = "OP"))

# clean NA
rawic1[rawic1 == 'NA'] <- NA
#remove stock measurements 
w = which(rawic1$is_stock == "y")
rawic = rawic1[-w,]

# ATTENTION! SOMETHING IS WRONG WITH THE FUNCTION> IT MUST BE SOMETHING TO DO with the way "tx" is dealt with in nexted functions.

datafile <- rawic
plotname <- "NXV-02"
logmodel = T
fine_root_cor = "Default" 
tubed = 0.07 
remove_stock_meas = T 
ret = "monthly.means.ts"
ret_type = "list" 

datafile = set_df_coltypes(datafile, ic_column_types)
  
ts_ic_2017_sa <- NPProot_ic(subset(datafile, plot_code %in% c("TAM-05", "ESP-01", "WAY-01", "ACJ-01", "TRU-4", "PAN-02", "PAN-03", "KEN-01", "KEN-02", "SPD-01", "SPD-02", "TAM-05", "TAM-06", "TAM-09", "BLZ-11", "BLZ-12", "BLZ-21", "BLZ-22", "NXV-02", "NXV-01"), ret_type = "list")) 
# These don't work: 
# "TAN-01", "TAN-02" - time_step_minutes = 5, 10, 15 
# "PAN-01" - time_step_minutes = 10, 20, 30
# "ALP-11", "ALP-12" 

ts_ic_2017_afr <- NPProot_ic(subset(datafile, plot_code %in% c("KOG-02", "KOG-03", "KOG-04", "KOG-05", "KOG-06", "LPG-01", "LPG-02", "ANK-01", "ANK-02", "ANK-03", "BOB-01", "BOB-02", "BOB-03", "BOB-04", "BOB-05", "BOB-06", "MNG-03", "MNG-04"), ret_type = "list"))
#  (stocks?) "LPG-01", "LPG-02", "ANK-01", "ANK-02", "ANK-03", "BOB-01", "BOB-02", "BOB-03", "BOB-04", "BOB-05", "BOB-06"
# not working: "IVI-01", "IVI-02" - time_step_minutes = 1, 2, 3, 4
 
ts_ic_2017_sea <- NPProot_ic(subset(datafile, plot_code %in% c("SAF-05", "SAF-01", "SAF-02", "DAN-04", "DAN-05", "MLA-01", "MLA-02", "OP" , "MLA-01", "MLA-02", "OP", "SAF-05", "SAF-01", "SAF-02"), ret_type = "list", tx = "txc"))
# working: "SAF-05", "SAF-01", "SAF-02", "DAN-04", "DAN-05", "MLA-01", "MLA-02", "OP" , "MLA-01", "MLA-02", "OP", "SAF-05", "SAF-01", "SAF-02"
# not working: "SAF-03", "SAF-04" 
# For all of sea, check - time_step = 1, 1, 1 & time_step_minutes = 10, 10, 10

# Testing
ic_test <- NPProot_ic(subset(datafile, plot_code %in% c("BOB-02"), ret_type = "list"))


sa <- ts_ic_2017_sa[["three_monthly"]]
sa <- data.frame(sa)
head(sa)

afr <- ts_ic_2017_afr[["three_monthly"]]
afr <- data.frame(afr)
head(afr)

sea <- ts_ic_2017_sea[["three_monthly"]]
sea <- data.frame(sea)
head(sea)

icsa <- ggplot(sa, aes(date, threemonthlyNPProot, colour = factor(sa$plot_code))) + 
                geom_point() +
                scale_colour_manual(values=rep(brewer.pal(12,"Paired"), times=5)) +
                theme(legend.position = "bottom") +
                xlab("") + ylab(expression(paste("Ingrowth Core (MgC ",ha^-1, threemonths^-1, ")", sep=""))) +
                ylim(0, 5) +
                ggtitle("South America") 
icsa

icafr <- ggplot(afr, aes(date, threemonthlyNPProot, colour = factor(afr$plot_code))) + 
                geom_point() +
                scale_colour_manual(values=rep(brewer.pal(12,"Paired"), times=5)) +
                theme(legend.position = "bottom") +
                xlab("") + ylab(expression(paste("Ingrowth Core (MgC ",ha^-1, threemonths^-1, ")", sep=""))) +
                ylim(0, 5) +
                ggtitle("Africa") 
icafr

icsea <- ggplot(sea, aes(date, threemonthlyNPProot, colour = factor(sea$plot_code))) + 
                geom_point() +
                scale_colour_manual(values=rep(brewer.pal(12,"Paired"), times=5)) +
                theme(legend.position = "bottom") +
                xlab("") + ylab(expression(paste("Ingrowth Core (MgC ",ha^-1, threemonths^-1, ")", sep=""))) +
                ylim(0, 5) +
                ggtitle("South East Asia") 
icsea


fig4 <- grid.arrange(icsa, icafr, icsea, ncol=1, nrow=3) 
fig4


# all ic data available 
sa = subset(rawic, plot_code %in% c("TAM-05", "ESP-01", "WAY-01", "ACJ-01", "TRU-4", "PAN-02", "PAN-03", "KEN-01", "KEN-02", "SPD-01", "SPD-02", "TAM-05", "TAM-06", "TAM-09", "BLZ-11", "BLZ-12", "BLZ-21", "BLZ-22", "TAN-01", "TAN-02", "PAN-01", "NXV-02", "NXV-01"))
afr = subset(rawic, plot_code %in% c("SAF-02")) 
sea = subset(rawic, plot_code %in% c("SAF-02"))
  
icraw <- ggplot(rawic , aes(year, month, colour = factor(rawic$plot_code))) + 
         geom_point() +
         scale_colour_manual(values=rep(brewer.pal(12,"Paired"), times=5)) +
         scale_shape_manual(values=rep(c(15,16,17,18,19), each=11)) 
         #theme(legend.position = "bottom")         
icraw


## Save to processed_ts

setwd("~/Github/gemcarbon_data/processed_ts_2017/")
write.csv(sa, file="ts_ic_sa_July2017.csv")
write.csv(afr, file="ts_ic_afr_July2017.csv")
write.csv(sea, file="ts_ic_sea_July2017.csv")

write.csv(data4, file="ts_ic_BOB01.csv")
write.csv(data4, file="ts_ic_BOB02.csv")
write.csv(data4, file="ts_ic_BOB03.csv")
write.csv(data4, file="ts_ic_BOB04.csv")
write.csv(data4, file="ts_ic_BOB05.csv")
write.csv(data4, file="ts_ic_BOB06.csv")
write.csv(data4, file="ts_ic_SAF01.csv")
write.csv(data4, file="ts_ic_SAF02.csv")
write.csv(data4, file="ts_ic_SAF03.csv")
write.csv(data4, file="ts_ic_SAF04.csv")
write.csv(data4, file="ts_ic_SAF05.csv")

### R STEM ###

setwd("~/Github/GEMcarbon.R")
source("~/Github/GEMcarbon.R/stem_respiration_percollar_2017.R")

stem_resp <- read.table("~/Github/gemcarbon_data/raw_data_ingembd/stem_respiration/stem_resp_13June.csv", sep=",", header=T)
stem_resp <- subset(stem_resp, select=c(1:20))

stem_resp$co2ref_ppm_sec <- as.numeric(as.character(stem_resp$co2ref_ppm_sec)) # NAs introduced by coercion 
stem_resp$time <- as.numeric(as.character(stem_resp$time))  # NAs introduced by coercion 

#stem_resp$plot_code <- revalue(stem_resp$plot_code, c("DC2" = "DAN-05", "DC1" = "DAN-04", "BZ11" = "BLZ-11", "BZ12" = "BLZ-12"))
#unique(stem_resp$plot_code) 
# sort out plot_codes

# SEA plot_codes from Terhi 03/04/2017
#B North               SAF-02
#B South               SAF-01
#E                     SAF-03
#LF                    SAF-04
#Tower                 SAF-05
#Belian (in Maliau)    MLA-01
#Seraya (in Maliau)    MLA-02
#Danum Carbon 1  (DC1) DAN-04
#Danum Carbon 2  (DC2) DAN-05

# Mean annual soil temperature

matALP11 <- 25.2  
matALP30 <- 25.2	
matTAM   <- 24.4 # TAM-05 -06 -09	
matTON01 <- 20.7	
matSPD02 <- 18.8	
matSPD01 <- 17.4	
matTRU07 <- 17.4	
matTRU08 <- 18	
matTRU04 <- 13.5	
matTRU03 <- 11.8
matWAY01 <- 11.8
matESP01 <- 13.1


# Test
plotname = "BOB-02"
collardiameter = 12
collarheight = 5
T_ambient = "Default"

TAM-09 TAM-06 TAM-05 SPD-02 SPD-01 ESP-01 WAY-01
PAN-01 PAN-02 PAN-03 TAM-09 TAM-06 TAM-05 SPD-02 SPD-01 ACJ-01 ESP-01 TRU-04 WAY-01 KEN-01 KEN-02 BLZ-11 BLZ-12 
LPG-01 LPG-02 MNG-03 MNG-04 IVI-01 IVI-02 KOG-02 KOG-03 KOG-04 KOG-05 KOG-06 BOB-01 BOB-02 BOB-03 BOB-04 BOB-05 BOB-06 
SAF-05 SAF-03 SAF-01 SAF-02 MLA-02 MLA-01 SAF-04 DAN-05 DAN-04 

# SA
tam09 <- stemrespiration(stem_resp, "TAM-09", ret="monthly.ts.percollar", collardiameter=12, collarheight=5, pressure="Default", elevation="Default", T_ambient=24.4, plotit=T)
tam06 <- stemrespiration(stem_resp, "TAM-06", ret="monthly.ts.percollar", collardiameter=12, collarheight=5, pressure="Default", elevation="Default", T_ambient=24.4, plotit=T)
tam05 <- stemrespiration(stem_resp, "TAM-05", ret="monthly.ts.percollar", collardiameter=12, collarheight=5, pressure="Default", elevation="Default", T_ambient=24.4, plotit=T)
spd02 <- stemrespiration(stem_resp, "SPD-02", ret="monthly.ts.percollar", collardiameter=12, collarheight=5, pressure="Default", elevation="Default", T_ambient=18.8, plotit=T)
spd01 <- stemrespiration(stem_resp, "SPD-01", ret="monthly.ts.percollar", collardiameter=12, collarheight=5, pressure="Default", elevation="Default", T_ambient=17.4, plotit=T)
esp01 <- stemrespiration(stem_resp, "ESP-01", ret="monthly.ts.percollar", collardiameter=12, collarheight=5, pressure="Default", elevation="Default", T_ambient=13.1, plotit=T)
way01 <- stemrespiration(stem_resp, "WAY-01", ret="monthly.ts.percollar", collardiameter=12, collarheight=5, pressure="Default", elevation="Default", T_ambient=11.8, plotit=T)
# not working: blz11 <- stemrespiration(stem_resp, "BLZ-11", ret="monthly.means.ts", collardiameter=12, collarheight=5, pressure="Default", elevation="Default", T_ambient=24.4, plotit=T)
# not working: blz12 <- stemrespiration(stem_resp, "BLZ-12", ret="monthly.means.ts", collardiameter=12, collarheight=5, pressure="Default", elevation="Default", T_ambient=24.4, plotit=T)

rstemsa <- rbind(tam09, tam06, tam05) # , spd02, spd01, esp01, way01
rstemsa$codew <- NULL
# ATTTENTION!! LOOK INTO OUTLIERS
w = which(rstemsa$flux_MgC_ha_month > 5) 
rstemsa$flux_MgC_ha_month[w] = 0/0 
  
  avg_rs_sa = rstemsa %>% group_by(plot_code, year, month) %>% 
                          dplyr::summarize(plotavg_MgC_ha_month = mean(flux_MgC_ha_month, na.rm = T), 
                                           plotavg_MgC_ha_month_sd = sd(flux_MgC_ha_month, na.rm = T), 
                                           date = max(date))
  avg_rs_sa = data.frame(avg_rs_sa)


# AFR
lpg01 <- stemrespiration(stem_resp, "LPG-01", ret="monthly.ts.percollar", collardiameter=12, collarheight=5, pressure="Default", elevation="Default", T_ambient=25, plotit=T)
lpg02 <- stemrespiration(stem_resp, "LPG-02", ret="monthly.ts.percollar", collardiameter=12, collarheight=5, pressure="Default", elevation="Default", T_ambient=25, plotit=T)
mng03 <- stemrespiration(stem_resp, "MNG-03", ret="monthly.ts.percollar", collardiameter=12, collarheight=5, pressure="Default", elevation="Default", T_ambient=25, plotit=T)
mng04 <- stemrespiration(stem_resp, "MNG-04", ret="monthly.ts.percollar", collardiameter=12, collarheight=5, pressure="Default", elevation="Default", T_ambient=25, plotit=T)
ivi01 <- stemrespiration(stem_resp, "IVI-01", ret="monthly.ts.percollar", collardiameter=12, collarheight=5, pressure="Default", elevation="Default", T_ambient=25, plotit=T)
ivi02 <- stemrespiration(stem_resp, "IVI-02", ret="monthly.ts.percollar", collardiameter=12, collarheight=5, pressure="Default", elevation="Default", T_ambient=25, plotit=T)
kog02 <- stemrespiration(stem_resp, "KOG-02", ret="monthly.ts.percollar", collardiameter=12, collarheight=5, pressure="Default", elevation="Default", T_ambient=25, plotit=T)
kog03 <- stemrespiration(stem_resp, "KOG-03", ret="monthly.ts.percollar", collardiameter=12, collarheight=5, pressure="Default", elevation="Default", T_ambient=25, plotit=T)
kog04 <- stemrespiration(stem_resp, "KOG-04", ret="monthly.ts.percollar", collardiameter=12, collarheight=5, pressure="Default", elevation="Default", T_ambient=25, plotit=T)
kog05 <- stemrespiration(stem_resp, "KOG-05", ret="monthly.ts.percollar", collardiameter=12, collarheight=5, pressure="Default", elevation="Default", T_ambient=25, plotit=T)
kog06 <- stemrespiration(stem_resp, "KOG-06", ret="monthly.ts.percollar", collardiameter=12, collarheight=5, pressure="Default", elevation="Default", T_ambient=25, plotit=T)
bob01 <- stemrespiration(stem_resp, "BOB-01", ret="monthly.ts.percollar", collardiameter=12, collarheight=5, pressure="Default", elevation="Default", T_ambient=25, plotit=T)
bob02 <- stemrespiration(stem_resp, "BOB-02", ret="monthly.ts.percollar", collardiameter=12, collarheight=5, pressure="Default", elevation="Default", T_ambient=25, plotit=T)
bob03 <- stemrespiration(stem_resp, "BOB-03", ret="monthly.ts.percollar", collardiameter=12, collarheight=5, pressure="Default", elevation="Default", T_ambient=25, plotit=T)
bob04 <- stemrespiration(stem_resp, "BOB-04", ret="monthly.ts.percollar", collardiameter=12, collarheight=5, pressure="Default", elevation="Default", T_ambient=25, plotit=T)
bob05 <- stemrespiration(stem_resp, "BOB-05", ret="monthly.ts.percollar", collardiameter=12, collarheight=5, pressure="Default", elevation="Default", T_ambient=25, plotit=T)
bob06 <- stemrespiration(stem_resp, "BOB-06", ret="monthly.ts.percollar", collardiameter=12, collarheight=5, pressure="Default", elevation="Default", T_ambient=25, plotit=T)

rstemafr <- rbind(lpg01, lpg02, mng03, mng04, ivi01, ivi02, kog02, kog03, kog04, kog05, kog06, bob01, bob02, bob03, bob04, bob05, bob06)


bob01 <- stemrespiration(stem_resp, "BOB-01", ret="monthly.means.ts", collardiameter=12, collarheight=5, pressure="Default", elevation="Default", T_ambient=25, plotit=T)
bob02 <- stemrespiration(stem_resp, "BOB-02", ret="monthly.means.ts", collardiameter=12, collarheight=5, pressure="Default", elevation="Default", T_ambient=25, plotit=T)
bob03 <- stemrespiration(stem_resp, "BOB-03", ret="monthly.means.ts", collardiameter=12, collarheight=5, pressure="Default", elevation="Default", T_ambient=25, plotit=T)
bob04 <- stemrespiration(stem_resp, "BOB-04", ret="monthly.means.ts", collardiameter=12, collarheight=5, pressure="Default", elevation="Default", T_ambient=25, plotit=T)
bob05 <- stemrespiration(stem_resp, "BOB-05", ret="monthly.means.ts", collardiameter=12, collarheight=5, pressure="Default", elevation="Default", T_ambient=25, plotit=T)
bob06 <- stemrespiration(stem_resp, "BOB-06", ret="monthly.means.ts", collardiameter=12, collarheight=5, pressure="Default", elevation="Default", T_ambient=25, plotit=T)
rstembob <- rbind(bob01, bob02, bob03, bob04, bob05, bob06)


rstemafr$codew <- NULL
# ATTTENTION!! LOOK INTO OUTLIERS
w = which(rstemafr$flux_MgC_ha_month > 5) 
rstemafr$flux_MgC_ha_month[w] = 0/0 

avg_rs_afr = rstemafr %>% group_by(plot_code, year, month) %>% 
                          dplyr::summarize(plotavg_MgC_ha_month = mean(flux_MgC_ha_month, na.rm = T), 
                                           plotavg_MgC_ha_month_sd = sd(flux_MgC_ha_month, na.rm = T), 
                                           date = max(date))
avg_rs_afr = data.frame(avg_rs_afr)


# SEA

temp = (subset(stem_resp, plot_code %in% c("SAF-01", "SAF-02", "SAF-03", "SAF-04", "SAF-05", "DAN-04", "DAN-05", "MLA-01", "MLA-02"))) 
rstemsea  = (subset(temp, quality_code == 1)) 
rstemsea$slope = as.numeric(as.character(rstemsea$InputF))

#################### calculate flux in MgC ha-1 month-1 : we need to re-do this using the correct weather data
rstemsea$soil_temp     = 26
rstemsea$atmp          = 999

rstemsea$codew   <- paste(rstemsea$plot_code, rstemsea$sub_plot, rstemsea$tree_tag, rstemsea$day, rstemsea$month, rstemsea$year, sep=".")

# get unique identifyer for each measurement
uid <- unique(rstemsea$codew)
xx <- c()
yy <- c()
zz <- c()

#for (i in uid) {
for (i in 1:length(uid)) {
  sub      <- subset(rstemsea, subset=(rstemsea$codew == uid[i]))
  id       <- tail(sub$codew, n=1) 
  co2slope <- sub$slope     
  P        <- tail(sub$atmp, n=1)                                                  # ambient pressure at t10 (mb)
  Ta       <- tail(sub$soil_temp, n=1)                                             # air temp at t10 (deg C)
  Vd       <- 0.0012287                                                            # m3 (constant)
  A        <- 0.00950                                                              # m2 (constant)
  ch       <- 5
  Va       <- A*(ch/100)                                                           # additional volume m3
  Ru       <- 8.31432                                                              # J mol-1 K-1 (constant)
  fl       <- co2slope * (P/(Ta + 273.15))*(Vd/A)*((44.01*0.36)/Ru)                # CO2 efflux (g CO2 m-2 h-1)
  fl2      <- (fl*A/Vd*(Va+Vd)/A)*6.312                                            # Convert to umol m-2 s-1. Correct for collar height.
  tempcorr <- exp(-0.0695*(1))                                                     # add a temperature correction from Sotta et al 2004 Q10=1.8 and k=0.0613
  convert  <- (2592000*10000*12)/(1000000*1000000)                                 # Convert units umol m2 s-1 to MgC ha month = 1mo=2592000sec, 10000m2=1ha, 1000000umol = 1 mol, 1mol = 12 g, 1000000g=1Mg
  flux     <- fl2*convert*tempcorr
  xx       <- rbind(xx, id)
  yy       <- rbind(yy, flux)
  print(yy)
}
Res <- data.frame(cbind(xx, yy), row.names=NULL)
colnames(Res) <- c("codew", "flux_MgC_ha_mo")

Res$flux_MgC_ha_mo <- as.numeric(as.character(Res$flux_MgC_ha_mo))

rstemsea <- merge(Res, rstemsea, all.x=T)
rstemsea$date <- strptime(paste(as.character(rstemsea$year), as.character(rstemsea$month), as.character(rstemsea$day), sep="-"), format="%Y-%m-%d")
  


minx <- min(rstemsea$flux_MgC_ha_mo, rm.na=T)
maxx <- max(rstemsea$flux_MgC_ha_mo, rm.na=T)
####################


## Save results
setwd("~/Github/gemcarbon_data/processed_ts_2017/")
write.csv(rstembob, file="ts_rstem_BOB.csv")

setwd("~/Github/gemcarbon_data/processed_ts_2017/ts_stem_respiration")
write.csv(rstemsa, file="ts_rstem_may17_sa.csv")
write.csv(rstemafr, file="ts_rstem_may17_afr.csv")
write.csv(rstemsea, file="ts_rstem_may17_sea.csv")

# plot all stem respiration measurements

samin  <- min(tam05$date, na.rm=T)
samax  <- max(tam05$date, na.rm=T)
afrmin <- min(lpg01$date, na.rm=T)
afrmax <- max(lpg01$date, na.rm=T)
seamin <- min(rstemsea$date, na.rm=T)
seamax <- max(rstemsea$date, na.rm=T)

plot_rs_sa <- ggplot(rstemsa, aes(date, flux_MgC_ha_month, colour = factor(rstemsa$plot_code))) + 
                     geom_point() +
                     geom_point(data = avg_rs_sa, aes(avg_rs_sa$date, avg_rs_sa$plotavg_MgC_ha_month, colour = factor(avg_rs_sa$plot_code))) + 
                     #geom_errorbar(data = avg_rs_sa, aes(x=date, y=plotavg_MgC_ha_month, ymin=(plotavg_MgC_ha_month-plotavg_MgC_ha_month_sd), ymax=(plotavg_MgC_ha_month+plotavg_MgC_ha_month_sd)), width = 0.5) +
                     geom_line(data = avg_rs_sa, aes(date, plotavg_MgC_ha_month, colour = factor(avg_rs_sa$plot_code))) +
                     scale_colour_manual(values=rep(brewer.pal(3,"Paired"))) +
                     theme(legend.position = "bottom") + 
                     xlab("") + ylab(expression(paste("Stem respiration per collar (MgC ",ha^-1, month^-1, ")", sep=""))) +
                     xlim(samin, samax) + #ylim(0, 5) +
                     ggtitle("South America") 
plot_rs_sa


plot_rs_afr <- ggplot(rstemafr, aes(date, flux_MgC_ha_month, colour = factor(rstemafr$plot_code))) + 
                     geom_point() +
                     geom_point(data = avg_rs_afr, aes(avg_rs_afr$date, avg_rs_afr$plotavg_MgC_ha_month, colour = factor(avg_rs_afr$plot_code))) + 
                     #geom_errorbar(data = avg_rs_afr, aes(x=date, y=plotavg_MgC_ha_month, ymin=(plotavg_MgC_ha_month-plotavg_MgC_ha_month_sd), ymax=(plotavg_MgC_ha_month+plotavg_MgC_ha_month_sd)), width = 0.5) +
                     geom_line(data = avg_rs_afr, aes(date, plotavg_MgC_ha_month, colour = factor(avg_rs_afr$plot_code))) +
                     #scale_colour_manual(values=rep(brewer.pal(17,"Paired"))) +
                     theme(legend.position = "bottom") + 
                     xlab("") + ylab(expression(paste("Stem respiration per collar (MgC ",ha^-1, month^-1, ")", sep=""))) +
                     #xlim(afrmin, afrmax) + ylim(0, 5) +
                     ggtitle("Africa") 
plot_rs_afr

plot_rs_sea <- ggplot(rstemsea, aes(date, flux_MgC_ha_mo, colour = factor(rstemsea$plot_code))) + 
                      geom_point() +
                      #geom_point(data = avg_rs_sea, aes(avg_rs_sea$date, avg_rs_sea$plotavg_MgC_ha_month, colour = factor(avg_rs_sea$plot_code))) + 
                      #geom_errorbar(data = avg_rs_sea, aes(x=date, y=plotavg_MgC_ha_month, ymin=(plotavg_MgC_ha_month-plotavg_MgC_ha_month_sd), ymax=(plotavg_MgC_ha_month+plotavg_MgC_ha_month_sd)), width = 0.5) +
                      #geom_line(data = avg_rs_sea, aes(date, plotavg_MgC_ha_month, colour = factor(avg_rs_sea$plot_code))) +
                      #scale_colour_manual(values=rep(brewer.pal(12,"Paired"))) +
                      theme(legend.position = "bottom") + 
                      xlab("") + ylab(expression(paste("Stem respiration per collar (MgC ",ha^-1, month^-1, ")", sep=""))) +
                      ylim((-1), 10) + #
                      ggtitle("South East Asia") 
plot_rs_sea

fig5 <- grid.arrange(plot_rs_sa, plot_rs_afr, plot_rs_sea, ncol=1, nrow=3) 
fig5


# Plots per region
# SA
dtam = (subset(rstemsa, plot_code %in% c("TAM-05", "TAM-06", "TAM-09"))) 
dspd = (subset(rstemsa, plot_code %in% c("SPD-01", "SPD-02"))) 
dway = (subset(rstemsa, plot_code %in% c("ESP-01", "WAY-01"))) 
dken = (subset(rstemsa, plot_code %in% c("KEN-01", "KEN-02")))

# AFR
dlpg = (subset(rstemafr, plot_code %in% c("LPG-01", "LPG-02", "SAF-03", "SAF-04", "SAF-05"))) 
dbob = (subset(rstemafr, plot_code %in% c("BOB-01", "BOB-02", "BOB-03", "BOB-04", "BOB-05", "BOB-06"))) 
dkog = (subset(rstemafr, plot_code %in% c("KOG-02", "KOG-03", "KOG-04", "KOG-05", "KOG-06"))) 

# SEA
dsaf = (subset(rstemsea, plot_code %in% c("SAF-01", "SAF-02", "SAF-03", "SAF-04", "SAF-05"))) 
ddan = (subset(rstemsea, plot_code %in% c("DAN-04", "DAN-05"))) 
dmla = (subset(rstemsea, plot_code %in% c("MLA-01", "MLA-02"))) 



#######################################
########## NPPstem CENSUS #############
#######################################

# NPPacw_census() : a function to calculate annual NPPacw from census data for trees >10 cm.

require(ggplot2)
require(sqldf)
require(lubridate)
require(dplyr)

setwd("/Users/cecile/Dropbox/GEMcarbondb/db_csv/db_csv_2015/readyforupload_db")
census_TRU04A <- read.table("andesplots_WFR_nov2014_noroots.csv", header=TRUE, sep=",", na.strings=c("NA", "NaN", ""), dec=".", strip.white=TRUE)
setwd("/Users/cecile/Dropbox/GEMcarbondb/db_csv/db_csv_2015/readyforupload_db/acj_pan_2015")
census_ACJ01 <- read.table("census_ACJ_01.csv", header=TRUE, sep=",", na.strings=c("NA", "NaN", ""), dec=".", strip.white=TRUE)
census_PAN02 <- read.table("census_PAN_02.csv", header=TRUE, sep=",", na.strings=c("NA", "NaN", ""), dec=".", strip.white=TRUE)
census_PAN03 <- read.table("census_PAN_03.csv", header=TRUE, sep=",", na.strings=c("NA", "NaN", ""), dec=".", strip.white=TRUE)
wd_chave      <- read.table("wsg.txt", header=TRUE, sep=",", na.strings=c("NA", "NaN", ""), dec=".", strip.white=TRUE)
wsg           <- census_TRU04A

#(...)

# get functions
setwd("/Users/cecile/GitHub/GEMcarbon.R") 
dir()
Sys.setlocale('LC_ALL','C') 
source("NPPacw_census_function_2015.R")
source("allometric_equations_2014.R")

# Run the function for the plot level census. Default allometric equation is Chave et al. 2005 
tam05a  <- NPPacw_census(census, plotname="ACJ-01", allometric_option="Default", height_correction_option="Default", census1_year=2013, census2_year=2014)
tam05b  <- NPPacw_census(census, plotname="ACJ-01", allometric_option="Default", height_correction_option="Default", census1_year=2014, census2_year=2015)


#######################################
########## NPPdendrometers ############
#######################################
setwd("/Users/cecile/GitHub/GEMcarbon.R/") 
source("~/Github/GEMcarbon.R/allometric_equations_2014.R")
#source("~/Github/GEMcarbon.R/NPPacw_census_function_2015.R")
source("~/Github/GEMcarbon.R/NPPacw_dendro_function_2015.r")

setwd("/Users/cecile/Github/gemcarbon_data/raw_data_ingembd/stem_npp")
NPPdend_all <- read.table("dendro_all_14June.csv", header=TRUE, sep=",", na.strings=c("NA", "NaN", ""), dec=".", strip.white=TRUE)
dbh_census  <- read.table("BOB01_treelist.csv", header=TRUE, sep=",", na.strings=c("NA", "NaN", ""), dec=".", strip.white=TRUE)
  
# STEP 1. for each plot: run census data cleaning function above first, to get the dataframe "census". You need to do this for each plot separately.
# STEP 2. then, select the parameters below for each plot, and start running the code "NPPacw_dendro_function_2014.R". Work on one plot at a time. 
# STEP 3. at the end of "NPPacw_dendro_function_2014.R", you use the function "NPPacw_census" to get an annual value of NPPACW from census data. L 198. Make sure you have the correct parameters in that function (plot name & census years)

#TAM-05

dendrometer = NPPdend_all # %>% filter(year<=2012)
census = dbh_census
plotname = "BOB-01"     
allometric_option = "Default"
height_correction_option = "Default"
census_year = 2012
plotit=F
dtam05 <- NPPacw_dendro(census, dendrometer, plotname = "BOB-02", allometric_option="Default", height_correction_option="Default", census_year = 2005)

# save dendrometer data in MgC / tree / day
setwd("~/Github/gemcarbon_data/processed_ts_2017/")
write.csv(npp_tree, file="ts_dendrometers_tam06_16June17.csv")


