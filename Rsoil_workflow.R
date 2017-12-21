# Rsoil workflow
# This script is the workflow for estimating R soil - total and partitionning- from the data saved from the EGM-4.

library(dplyr)
library(grDevices)
require(ggplot2)
library(tidyverse)
library(lubridate)
require(lubridate)
require(tidyverse)

# load functions
# function EGM_fluxfunction_20171205.R simply estimates the flux for each measurement recorded by the EGM.
setwd("~/Github/GEMcarbon.R")
source("~/Github/GEMcarbon.R/EGM_fluxfunction_20171205.R")

# load data

setwd("~/Github/gemcarbon_data/raw_data_ingemdb_forELDS/soil_respiration")
#eltr        = read.table("eltr_rsoil_total_mar17.csv", sep=",", header=T)

rtot        = read.table("tot_soil_resp_20171212.csv", sep=",", header=T)
rtot_test = rtot %>% select(plot_code, sub_plot, plot_corner_code, collar_number, measurement_code, treatment_code_partitioning, litter_code, replica, year, egm_measurement, recno, day, month, co2ref_ppm_sec, time, atmp_mb) %>%
                     filter(plot_code == "TAM-05")

#> unique(rtot$plot_code)
#[1] ACJ-01 ESP-01 KEN-01 KEN-02 NXV-01 NXV-02 PAN-02 PAN-03 SPD-01 SPD-02 STB-08 STB-12 STD-05 STD-10 STD-11 STJ-01 STJ-04 STJ-05 STL-09
#[20] STL-10 STN-02 STN-03 STN-04 STN-06 STN-09 STO-03 STO-06 STO-07 STQ-08 STQ-11 TAM-05 TAM-06 TAM-09 TRU-04 WAY-01 LPG-01 LPG-02

temp_vwc_ch = read.table("eltr_rsoil_temp_vwc_ch_mar17.csv", sep=",", header=T)

rpart      = read.table("part_soil_resp_20171205.csv", sep=",", header=T)
rpart_test = rpart %>% select(plot_code, sub_plot, plot_corner_code, collar_number, measurement_code, treatment_code_partitioning, disturbance_code_CTRL, litter_code, replica, year, egm_measurement, recno, day, month, co2ref_ppm_sec, time, atmp_mb) %>%
                       filter(plot_code == "TAM-05")

# If we do have air temperature and collar height, merge the two datasets.

#datafile = left_join(rtot, temp_vwc_ch, by = "uid")

# run EGM_fluxfunction_20171205.R
datafile = rpart_test 

# If you don't have air temperature, relative humidity, and collar height, you need to add the columns, EGM_fluxfunction_20171205.R replaces NAs with default values
datafile$air_temp_c = NA
datafile$ch_fill = NA

# Either run this function plot by plot:
rsoil_tot_persubplot <- Rflux(datafile, ret="Res", "TAM-05")
rsoil_tot_persubplot

# Or go through the code step by step and run the whole dataset.

# Save output: this is in GEMcarbon.R
setwd("~/Github/gemcarbon_data/raw_data_ingemdb_forELDS")
write.csv(Res, file="rsoil_tot_persubplot_20171213.csv")
write.csv(Res, file="rsoil_part_tam05_20171220.csv")

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




