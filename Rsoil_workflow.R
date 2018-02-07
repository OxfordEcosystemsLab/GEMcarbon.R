# Rsoil workflow
# This script is the workflow for estimating R soil - total and partitionning- from the data saved from the EGM-4.

library(dplyr)
library(grDevices)
require(ggplot2)
## require(gridExtra) !! CHECK THIS.
library(tidyverse)
library(lubridate)
require(lubridate)
require(tidyverse)


## NOTES FROM YM
# for each experiment, get f= Rhet/Rtot - check these look right, get rid of the dodgy ones.
# work out an average value for the plot
# then use the Rtot from 25 ploints to get plot level R aut r = Rtot*f, Raut = 1-r



# load functions
# function EGM_fluxfunction_20171205.R simply estimates the flux for each measurement recorded by the EGM.
setwd("~/Github/GEMcarbon.R")
source("~/Github/GEMcarbon.R/EGM_fluxfunction_20171205.R")

# load data

setwd("~/Github/gemcarbon_data/raw_data_ingemdb_forELDS/soil_respiration")
#eltr        = read.table("eltr_rsoil_total_mar17.csv", sep=",", header=T)

rtot        = read.table("tot_soil_resp_20180131.csv", sep=",", header=T)

#> unique(rtot$plot_code)
#ACJ-01 ESP-01 TRU-04 WAY-01
#KEN-01 KEN-02 
#NXV-01 NXV-02 
#PAN-02 PAN-03 
#SPD-01 SPD-02 
#TAM-05 TAM-06 TAM-09  
#LPG-01 LPG-02        
#ANK-01 ANK-02 ANK-03 
#BOB-01 BOB-03 BOB-02 BOB-04 BOB-05 BOB-06 
#KOG-02 KOG-03 KOG-04 KOG-05 KOG-06

temp_vwc_ch = read.table("eltr_rsoil_temp_vwc_ch_mar17.csv", sep=",", header=T)

#rpart      = read.table("part_soil_resp_20171205.csv", sep=",", header=T)
#rpart_test = rpart %>% select(plot_code, sub_plot, plot_corner_code, collar_number, measurement_code, treatment_code_partitioning, disturbance_code_CTRL, litter_code, replica, year, egm_measurement, recno, day, month, co2ref_ppm_sec, time, atmp_mb) %>%
                       filter(plot_code == "TAM-05")

# If we do have air temperature and collar height, merge the two datasets.
rtot$uid = mutate(rtot, code = paste(plot_code, replica, year, month, day, sep = '_'))
temp_vwc_ch$uid = mutate(temp_vwc_ch, code = paste(plot_code, replica, year, month, day, sep = '_'))
datafile = left_join(rtot, temp_vwc_ch, by = "uid")

# run EGM_fluxfunction_20171205.R
datafile = rtot
datafile = rpart_test 

# If you don't have air temperature, relative humidity, and collar height, you need to add the columns, EGM_fluxfunction_20171205.R replaces NAs with default values
datafile$air_temp_c = NA
datafile$ch_fill = NA

# Either run this function plot by plot:
rsoil_tot_persubplot <- Rflux(datafile, ret="Res", "TAM-05")
rsoil_tot_persubplot

# Or go through the code step by step and run the whole dataset. That gives you Rflux_MgC_ha_mo per subplot
# So this is to average per plot:
avg_rtot = Res %>% group_by(plot_code, measurement_code, year, month) %>% 
           dplyr::summarize(avg = mean(Rflux_MgC_ha_mo, na.rm = T), 
                    sd = sd(Rflux_MgC_ha_mo, na.rm = T))

Res2 = data.frame(avg_rtot)

# Save output: this is in GEMcarbon.R
setwd("~/Github/gemcarbon_data/raw_data_ingemdb_forELDS")
write.csv(Res, file="rsoil_tot_persubplot_20180204.csv")
write.csv(Res2, file="rsoil_tot_MgChamo_20180204.csv")

Res2$date = strptime(paste(as.character(Res2$year), as.character(Res2$month), as.character("15"), sep="-"), format="%Y-%m-%d")  
unique(Res2$plot_code)
# "ANK-01" "ANK-02" "ANK-03" "BOB-01" "BOB-02" "BOB-03" "BOB-04" "BOB-05" "BOB-06" 
# "IVI-01" "IVI-02" "KEN-01" "KEN-02" "KOG-02" "KOG-03" "KOG-04" "KOG-05" "KOG-06" "LPG-01" "LPG-02" "MNG-03" "MNG-04" 
# "NXV-01" "NXV-02" 
# "ACJ-01" "ESP-01" "SPD-02" "TAM-05" "TAM-06" "TAM-09" "TRU-04" "WAY-01" "PAN-02" "PAN-03" "SPD-01"


# Plot it
sub = subset(Res2, plot_code = "ANK-01")
sub = sub[-1,]
aa <- ggplot(sub, aes(x = date, y = avg, na.rm = T)) +
             geom_point(data = sub, aes(x = date, y = avg), size = 2, colour = "orange", na.rm=T) +
             #geom_ribbon(data = sub, aes(x = date, max = avg+sd, min = avg-sd), colour = "grey", na.rm=T) +
             ggtitle("ANK-01")


sub = subset(Res2, plot_code = "ANK-02")
sub = sub[-1,]
bb <- ggplot(sub, aes(x = date, y = avg, na.rm = T)) +
             geom_point(data = sub, aes(x = date, y = avg), size = 2, colour = "orange", na.rm=T) +
             #geom_ribbon(data = sub, aes(x = date, max = avg+sd, min = avg-sd), colour = "grey", na.rm=T) +
             ggtitle("ANK-02")


sub = subset(Res2, plot_code = "ANK-03")
sub = sub[-1,]
cc <- ggplot(sub, aes(x = date, y = avg, na.rm = T)) +
             geom_point(data = sub, aes(x = date, y = avg), size = 2, colour = "orange", na.rm=T) +
             #geom_ribbon(data = sub, aes(x = date, max = avg+sd, min = avg-sd), colour = "grey", na.rm=T) +
             ggtitle("ANK-03")

sub = subset(Res2, plot_code = "BOB-01")
sub = sub[-1,]
dd <- ggplot(sub, aes(x = date, y = avg, na.rm = T)) +
  geom_point(data = sub, aes(x = date, y = avg), size = 2, colour = "orange", na.rm=T) +
  #geom_ribbon(data = sub, aes(x = date, max = avg+sd, min = avg-sd), colour = "grey", na.rm=T) +
  ggtitle("BOB-01")

sub = subset(Res2, plot_code = "BOB-02")
sub = sub[-1,]
ee <- ggplot(sub, aes(x = date, y = avg, na.rm = T)) +
  geom_point(data = sub, aes(x = date, y = avg), size = 2, colour = "orange", na.rm=T) +
  #geom_ribbon(data = sub, aes(x = date, max = avg+sd, min = avg-sd), colour = "grey", na.rm=T) +
  ggtitle("BOB-02")

sub = subset(Res2, plot_code = "BOB-03")
sub = sub[-1,]
ff <- ggplot(sub, aes(x = date, y = avg, na.rm = T)) +
  geom_point(data = sub, aes(x = date, y = avg), size = 2, colour = "orange", na.rm=T) +
  #geom_ribbon(data = sub, aes(x = date, max = avg+sd, min = avg-sd), colour = "grey", na.rm=T) +
  ggtitle("BOB-03")

sub = subset(Res2, plot_code = "BOB-04")
sub = sub[-1,]
gg <- ggplot(sub, aes(x = date, y = avg, na.rm = T)) +
  geom_point(data = sub, aes(x = date, y = avg), size = 2, colour = "orange", na.rm=T) +
  #geom_ribbon(data = sub, aes(x = date, max = avg+sd, min = avg-sd), colour = "grey", na.rm=T) +
  ggtitle("BOB-04")

sub = subset(Res2, plot_code = "BOB-05")
sub = sub[-1,]
hh <- ggplot(sub, aes(x = date, y = avg, na.rm = T)) +
  geom_point(data = sub, aes(x = date, y = avg), size = 2, colour = "orange", na.rm=T) +
  #geom_ribbon(data = sub, aes(x = date, max = avg+sd, min = avg-sd), colour = "grey", na.rm=T) +
  ggtitle("BOB-05")

sub = subset(Res2, plot_code = "BOB-06")
sub = sub[-1,]
ii <- ggplot(sub, aes(x = date, y = avg, na.rm = T)) +
  geom_point(data = sub, aes(x = date, y = avg), size = 2, colour = "orange", na.rm=T) +
  #geom_ribbon(data = sub, aes(x = date, max = avg+sd, min = avg-sd), colour = "grey", na.rm=T) +
  ggtitle("BOB-06")

fig1 <- grid.arrange(aa, bb, cc, dd, ee, ff, gg, hh, ii, ncol=3, nrow=3) 
fig1

#write.csv(Res, file="rsoil_part_tam05_20171220.csv")

totflux = read.table("rsoil_tot_persubplot_20171213.csv", sep=",", header=T)
partflux = read.table("rsoil_part_persubplot_20171219_2.csv", sep=",", header=T)

# Add date to totflux
totflux = totflux %>% mutate(date = as.Date(paste(year, month, day, sep="."), format="%Y.%m.%d"))


# calculate Raut:

#> unique(partflux$part_code)
#[1] so_no_lit          ml_nor_lit         my_doub_lit        <NA>               so_doub_lit        my_nor_lit         so_nor_lit        
#[8] con_nor_lit        my_no_lit          con_no_lit         ml_no_lit          con_doub_lit       so_no_it           all               
#[15] all_no_lit         all_no_lit_root    all_no_lit_root_my

# KEN: What are the part_code NA values??? there are a lot of them! But no "con_nor_lit" - could they be the control normal litter??
# KEN: sub_plot must be collar_number? The sub_plot numbers don't match from Rhet and Rtot. We can only do this analysis on a plot level.
# Group per plot_code, day, month, year, measurement_code, part_code


Rtot = partflux %>% select(plot_code, sub_plot, collar_number, replica, day, month, year, measurement_code, treatment_code_partitioning, Rflux_MgC_ha_mo) %>%
                    group_by(plot_code, year, month, day, measurement_code, treatment_code_partitioning) %>%
                    mutate(id = paste(plot_code, sub_plot, day, month, year, sep = '_')) %>%
                    filter(plot_code == "TAM-05" & treatment_code_partitioning == "con_nor_lit")

Rtotdf = data.frame(Rtot)

Rhet = partflux %>% select(plot_code, sub_plot, collar_number, replica, day, month, year, measurement_code, treatment_code_partitioning, Rflux_MgC_ha_mo) %>%
                    group_by(plot_code, year, month, day, measurement_code, treatment_code_partitioning) %>%
                    mutate(id = paste(plot_code, sub_plot, day, month, year, sep = '_')) %>%  
                    filter(plot_code == "TAM-05" & treatment_code_partitioning == "so_nor_lit")

Rhetdf = data.frame(Rhet)

# this assumes they're collected on the same day and links the replicas
# Try not being so restrictive on the join

Rpartflux = Rtot %>% left_join(Rhet, by = "id") %>%
                     mutate(date = as.Date(paste(year.x, month.x, day.x, sep="."), format="%Y.%m.%d"))


Rpartfluxdf = data.frame(Rpartflux)
head(Rpartfluxdf, 10)

Rpartfluxdf = Rpartfluxdf %>% mutate(Raut = Rflux_MgC_ha_mo.x - Rflux_MgC_ha_mo.y)



# Plot KEN-01 & KEN-02
# R total
ken01_tot25 = totflux %>% select(plot_code, sub_plot, collar_number, replica, day, month, year, part_code, Rflux_MgC_ha_mo) %>%
                          filter(plot_code == "KEN-01" & Rflux_MgC_ha_mo >= 0 & Rflux_MgC_ha_mo <= 20) %>%
                          mutate(date = as.Date(paste(year, month, day, sep="."), format="%Y.%m.%d"))

ken02_tot25 = totflux %>% select(plot_code, sub_plot, collar_number, replica, day, month, year, part_code, Rflux_MgC_ha_mo) %>%
                          filter(plot_code == "KEN-02" & Rflux_MgC_ha_mo >= 0 & Rflux_MgC_ha_mo <= 20) %>%
                          mutate(date = as.Date(paste(year, month, day, sep="."), format="%Y.%m.%d"))

# R part 

# Plots
plot1 =  ggplot(ken01_tot25, aes(date, Rflux_MgC_ha_mo)) + 
                geom_point(size = 1, colour = "dark grey", na.rm=T) +
               #geom_point(data = Rpartflux, aes(x = date, y = Rflux_MgC_ha_mo.y), size = 1, colour = "orange", na.rm=T) +
               #geom_point(data = Rpartflux, aes(x = date, y = Rflux_MgC_ha_mo.y), size = 0.25, colour = "red", na.rm=T) +
                xlab("") + 
                ylab(expression(paste("Rsoil (MgC ",ha^-1, month^-1, ")", sep=""))) +
                ggtitle("KEN-01, Rsoil total 25 sub plots, Rpart") 
plot1

plot2 =  ggplot(ken02_tot25, aes(date, Rflux_MgC_ha_mo)) + 
                geom_point(size = 1, colour = "dark grey", na.rm=T) +
               #geom_point(data = Rpartflux, aes(x = date, y = Rflux_MgC_ha_mo.y), size = 1, colour = "orange", na.rm=T) +
               #geom_point(data = Rpartflux, aes(x = date, y = Rflux_MgC_ha_mo.y), size = 0.25, colour = "red", na.rm=T) +
                xlab("") + 
                ylab(expression(paste("Rsoil (MgC ",ha^-1, month^-1, ")", sep=""))) +
                ggtitle("KEN-02, Rsoil total 25 sub plots, Rpart") 
plot2









#plot total and heterotrophic and autotrophic on the same plot for TAN / TAM / BOB
Rplot =  ggplot(Rpartfluxdf, aes(date, Rflux_MgC_ha_mo.x)) + 
               geom_point(size = 0.75, colour = "dark grey", na.rm=T) +
               geom_point(data = Rpartfluxdf, aes(x = date, y = Rflux_MgC_ha_mo.y), size = 0.5, colour = "orange", na.rm=T) +
               geom_point(data = Rpartfluxdf, aes(x = date, y = Raut), size = 0.25, colour = "red", na.rm=T) +
               xlab("") + ylab(expression(paste("Rsoil (MgC ",ha^-1, month^-1, ")", sep=""))) +
               ggtitle("TAM-05: grey = Rtot, orange = Rhet, red = Raut ") 
Rplot


total = totflux %>% select(plot_code, day, month, year, Rflux_MgC_ha_mo) %>%
                    filter(Rflux_MgC_ha_mo >= -5 & Rflux_MgC_ha_mo <= 20) %>%
                    mutate(date = as.Date(paste(year, month, day, sep="."), format="%Y.%m.%d"))

Rplot =  ggplot(total, aes(date, Rflux_MgC_ha_mo, colour = factor(total$plot_code))) + 
                geom_point(size = 0.75, na.rm=T) +
                xlab("") + ylab(expression(paste("Rsoil (MgC ",ha^-1, month^-1, ")", sep=""))) +
                theme(legend.position = "bottom") +
                ggtitle("Total R soil for all plots") 
Rplot



# Average per plot
# avg_rsoil_tot = rsoilsea_new %>% group_by(Plot, Collar_type, year, month) %>% 
#                         dplyr::summarize(avg = mean(Flux_MgC_ha_month, na.rm = T), 
#                                  sd = sd(Flux_MgC_ha_month, na.rm = T))


# To do for soil resp
# save calculated sea & str data separately in ELDS
# save calculated Santarem in ELDS 




