# October 2016
# Cécile Girardin: I use the following script to re-format raw xl sheets that come from the field. 

# load packages
library(sqldf)
library(plyr)

## Soil respiration

setwd("~/Desktop/data_sorting/rsoil/Rs_db_csv")
list.files()
rcontrol_kos        <- read.table("~/Desktop/data_sorting/rsoil/Rs_db_csv/clean_controlresp_kos_Dec2014.csv", sep=",", header=T)
names(rcontrol_kos) <- sub("^treatment_code_partitionning$", "treatment_code_partitioning", names(rcontrol_kos)) # ACJ-01 ESP-01 PAN-02 PAN-03 SPD-01 SPD-02 TRU-04 WAY-01 & 2011 2009 2010 2012 2013 2014
wcontrol_kos        <- read.table("~/Desktop/data_sorting/rsoil/Rs_db_csv/clean_conweather_kos_Dec2014.csv", sep=",", header=T) # ACJ-01 ESP-01 PAN-02 PAN-03 SPD-01 SPD-02 TRU-04 WAY-01 & 2011 2012 2013 2014 2008 2009 2010
rpart_kosA          <- read.table("~/Desktop/data_sorting/rsoil/Rs_db_csv/clean_partresp_kos_Dec2014.csv", sep=",", header=T) # ACJ-01 ESP-01 PAN-02 PAN-03 SPD-01 SPD-02 TRU-04 WAY-01 &  2011 2010 2009 2012 2013
wpart_kos           <- read.table("~/Desktop/data_sorting/rsoil/Rs_db_csv/clean_partweather_kos_Dec2014.csv", sep=",", header=T) # ACJ-01 ESP-01 PAN-01 PAN-02 PAN-03 SPD-01 SPD-02 TRU-04 WAY-01 & 2011 2013 2014 2008 2009 2010 2012
rtotal_kosA         <- read.table("~/Desktop/data_sorting/rsoil/Rs_db_csv/clean_totalresp_kos_Dec2014.csv", sep=",", header=T) # ACJ-01 ESP-01 PAN-02 PAN-03 SPD-01 SPD-02 TRU-04 WAY-01 & 2011 2010 2009 2012 2013 2014
wtotal_kos          <- read.table("~/Desktop/data_sorting/rsoil/Rs_db_csv/kos_total_weather_2009_2014.csv", sep=",", header=T) #  ACJ-01 ESP-01 PAN-02 PAN-03 SPD-01 SPD-02 TRU-04 WAY-01 & 2009 2011 2008 2010 2012 2013 2014   NA


# KOSNIPATA
# partitioning

rpart_kos <- c()

rpart_kos$plot_code                   <- rpart_kosA$plot_code    
rpart_kos                             <- data.frame(rpart_kos)
rpart_kos$sub_plot                    <- rpart_kosA$subplot # unique: 1 5 21 25 26 32 36 33 4 16 10 51 52 53 54 55 56 57 58 59 60 61 62 63 64 65 66 67 68 69 70 71 72 73 74 75 76 77 78 79 80 81 82 83 84 85 86 87 88 89 90       
rpart_kos$plot_corner_code            <- NA
rpart_kos$collar_num                  <- rpart_kosA$tube
rpart_kos$measurement_code            <- "PART"
rpart_kos$treatment_code_partitioning <- rpart_kosA$treatment_code
rpart_kos$disturbance_code_control    <- NA       
	
# get litter_code info
rpart_kosA$xx  <- rpart_kosA$treatment_code
rpart_kosA$xx  <- gsub("con_nor_lit","normal", rpart_kosA$xx, ignore.case=T)
rpart_kosA$xx  <- gsub("con_no_lit","none", rpart_kosA$xx, ignore.case=T)
rpart_kosA$xx  <- gsub("con_doub_lit","double", rpart_kosA$xx, ignore.case=T)
rpart_kosA$xx  <- gsub("my_nor_lit","normal", rpart_kosA$xx, ignore.case=T)
rpart_kosA$xx  <- gsub("my_no_lit","none", rpart_kosA$xx, ignore.case=T)
rpart_kosA$xx  <- gsub("my_doub_lit","double", rpart_kosA$xx, ignore.case=T)
rpart_kosA$xx  <- gsub("so_nor_lit","normal", rpart_kosA$xx, ignore.case=T)
rpart_kosA$xx  <- gsub("so_no_lit","none", rpart_kosA$xx, ignore.case=T)
rpart_kosA$xx  <- gsub("so_doub_lit","double", rpart_kosA$xx, ignore.case=T)
rpart_kosA$xx  <- gsub("ml","NA", rpart_kosA$xx, ignore.case=T)

rpart_kos$litter_code     <- rpart_kosA$xx
rpart_kos$replica         <- rpart_kosA$replica
rpart_kos$year            <- rpart_kosA$year                  
rpart_kos$egm_measurement <- rpart_kosA$egm_measurement    
rpart_kos$recno           <- rpart_kosA$recno                      
rpart_kos$day             <- rpart_kosA$day                        
rpart_kos$month           <- rpart_kosA$month                   
rpart_kos$hour            <- rpart_kosA$hour                            
rpart_kos$min             <- rpart_kosA$min     	              
rpart_kos$co2ref_ppm_sec  <- rpart_kosA$co2ref 	      
rpart_kos$InputD          <- rpart_kosA$InputD                       
rpart_kos$time            <- rpart_kosA$time                   
rpart_kos$InputF          <- rpart_kosA$InputF  
rpart_kos$atmp_mb         <- rpart_kosA$atmp                   
rpart_kos$probe_type      <- rpart_kosA$probe_type                 
rpart_kos$quality_code    <- "good"                
rpart_kos$comments        <- "check numbers of sub_plot"             
rpart_kos$InputF          <- as.numeric(as.character(rpart_kos$InputF))


# total 
rtotal_kos <- c()

rtotal_kos$plot_code                   <- rtotal_kosA$plot_code    
rtotal_kos                             <- data.frame(rtotal_kos)
rtotal_kos$sub_plot                    <- rtotal_kosA$subplot # unique:  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38       
rtotal_kos$plot_corner_code            <- NA
rtotal_kos$collar_num                  <- NA
rtotal_kos$measurement_code            <- "TOT"
rtotal_kos$treatment_code_partitioning <- NA
rtotal_kos$disturbance_code_control    <- NA       
rtotal_kos$litter_code                 <- NA
rtotal_kos$replica                     <- rtotal_kosA$replica
rtotal_kos$year                        <- rtotal_kosA$year                  
rtotal_kos$egm_measurement             <- rtotal_kosA$egm_measurement    
rtotal_kos$recno                       <- rtotal_kosA$recno                      
rtotal_kos$day                         <- rtotal_kosA$day                        
rtotal_kos$month                       <- rtotal_kosA$month                   
rtotal_kos$hour                        <- rtotal_kosA$hour                            
rtotal_kos$min                         <- rtotal_kosA$min     	              
rtotal_kos$co2ref_ppm_sec              <- rtotal_kosA$co2ref 	      
rtotal_kos$InputD                      <- rtotal_kosA$InputD                       
rtotal_kos$time                        <- rtotal_kosA$time                   
rtotal_kos$InputF                      <- rtotal_kosA$InputF  
rtotal_kos$atmp_mb                     <- rtotal_kosA$atmp                   
rtotal_kos$probe_type                  <- rtotal_kosA$probe_type                 
rtotal_kos$quality_code                <- "good"                
rtotal_kos$comments                    <- "check numbers of sub_plot"              
rtotal_kos$InputD                     <- as.numeric(as.character(rtotal_kos$InputD))


# TAMBOPATA

setwd("~/Desktop/data_sorting/rsoil")
list.files()


#partitioning
tam_part_11         <- read.table("~/Desktop/data_sorting/rsoil/2011 TAM Partitioning.csv", sep=",", header=T)
tam_part_12         <- read.table("~/Desktop/data_sorting/rsoil/2012 TAM Partitioning.csv", sep=",", header=T) # TAM2012PART.csv
tam_part_13         <- read.table("~/Desktop/data_sorting/rsoil/2013 TAM Partitioning.csv", sep=",", header=T) 
tam_part_14         <- read.table("~/Desktop/data_sorting/rsoil/2014 TAM Partitioning.csv", sep=",", header=T) # 2014 TAM Partitioning missing.csv
tam_partconttot_15  <- read.table("~/Desktop/data_sorting/rsoil/Soil_component_respiration_data_2015.csv", sep=",", header=T) #TAM-05-TAM-06, 2015-2016
colnames(tam_partconttot_15) <- c("plot_code", "sub_plot", "plot_corner_code", "collar_number", "measurement_code", "treatment_code_partitioning", "disturbance_code_control", "litter_code", "replica", "year", "egm_measurement", "RecNo", "Day", "Month", "Hour", "Min", "co2ref_ppm_sec", "Input.D", "time", "Input.F", "atmp_mb", "probe_type", "manually_collected_final_flux_ppm_sec", "quality_code", "comments")

rpart_tam <- rbind(tam_part_11, tam_part_12, tam_part_13, tam_part_14, tam_partconttot_15) 

names(rpart_tam) <- sub("^collar_number$", "collar_num", names(rpart_tam))
rpart_tam$manually_collected_final_flux_ppm_sec <- NULL
names(rpart_tam) <- sub("^RecNo$", "recno", names(rpart_tam))
names(rpart_tam) <- sub("^Day$", "day", names(rpart_tam))
names(rpart_tam) <- sub("^Month$", "month", names(rpart_tam))
names(rpart_tam) <- sub("^Hour$", "hour", names(rpart_tam))
names(rpart_tam) <- sub("^Min$", "min", names(rpart_tam))
names(rpart_tam) <- sub("^Input.D$", "InputD", names(rpart_tam))
names(rpart_tam) <- sub("^Input.F$", "InputF", names(rpart_tam))

# Rename plots

rpart_tam$plot_code  <- gsub("TAM-03","TAM-05", rpart_tam$plot_code, ignore.case=T)
rpart_tam$plot_code  <- gsub("TAM-04","TAM-06", rpart_tam$plot_code, ignore.case=T)

# TAM-03 = TAM-05
# TAM-04 = TAM-06
# TAM-09 = TAM-09

# control  
tam_ctrl_12  <- read.table("~/Desktop/data_sorting/rsoil/2012 Tam Ctrl.csv", sep=",", header=T) #2012 TAM CTRL 20150928.csv 
tam_ctrl_13  <- read.table("~/Desktop/data_sorting/rsoil/2013 TAM CTRL 20150928.csv", sep=",", header=T)
tam_ctrl_14  <- read.table("~/Desktop/data_sorting/rsoil/TAM 2014 Control.csv", sep=",", header=T)

rcontrol_tam        <- rbind(tam_ctrl_12, tam_ctrl_13, tam_ctrl_14)
names(rcontrol_tam) <- sub("^RecNo$", "recno", names(rcontrol_tam))
names(rcontrol_tam) <- sub("^Day$", "day", names(rcontrol_tam))
names(rcontrol_tam) <- sub("^Month$", "month", names(rcontrol_tam))
names(rcontrol_tam) <- sub("^Hour$", "hour", names(rcontrol_tam))
names(rcontrol_tam) <- sub("^Min$", "min", names(rcontrol_tam))
names(rcontrol_tam) <- sub("^Input.D$", "InputD", names(rcontrol_tam))
names(rcontrol_tam) <- sub("^Input.F$", "InputF", names(rcontrol_tam))

# Are these in 2012 Tam Ctrl.csv?
#TAM3_CTRL_190912.csv #TAM3_CTRL_250612.csv #TAM3_CTRL_260812.csv #TAM4_CTRL_220912.csv #TAM4_CTRL_230612.csv #TAM9_CTRL_200912.csv #TAM9_CTRL_260612.csv #TAM9_CTRL_270812.csv 
#20151005 TAM files v2.csv

# total
tam_tot_05   <- read.table("~/Desktop/data_sorting/rsoil/TAM TOT 2005.csv", sep=",", header=T)
tam_tot_06   <- read.table("~/Desktop/data_sorting/rsoil/TAM TOT 2006.csv", sep=",", header=T)
tam_tot_07   <- read.table("~/Desktop/data_sorting/rsoil/TAM TOT 2007.csv", sep=",", header=T)
tam_tot_08   <- read.table("~/Desktop/data_sorting/rsoil/TAM TOT 2008.csv", sep=",", header=T)
tam_tot_09   <- read.table("~/Desktop/data_sorting/rsoil/TAM TOT 2009.csv", sep=",", header=T)
tam_tot_10   <- read.table("~/Desktop/data_sorting/rsoil/TAM TOT 2010.csv", sep=",", header=T)
tam_tot_11a  <- read.table("~/Desktop/data_sorting/rsoil/TAM TOT 2011a.csv", sep=",", header=T)
tam_tot_11b  <- read.table("~/Desktop/data_sorting/rsoil/TAM TOT 2011b.csv", sep=",", header=T)
tam_tot_12   <- read.table("~/Desktop/data_sorting/rsoil/TAM TOT 2012.csv", sep=",", header=T)
tam_tot_13   <- read.table("~/Desktop/data_sorting/rsoil/TAM TOT 2013.csv", sep=",", header=T)
tam_tot_14   <- read.table("~/Desktop/data_sorting/rsoil/TAM TOT 2014.csv", sep=",", header=T)

rtotal_tam  <-  rbind(tam_tot_05, tam_tot_06, tam_tot_07, tam_tot_08, tam_tot_09, tam_tot_10, tam_tot_11a, tam_tot_11b, tam_tot_12, tam_tot_13, tam_tot_14)

names(rtotal_tam) <- sub("^collar_number$", "collar_num", names(rtotal_tam))
rtotal_tam$manually_collected_final_flux_ppm_sec <- NULL
names(rtotal_tam) <- sub("^RecNo$", "recno", names(rtotal_tam))
names(rtotal_tam) <- sub("^Day$", "day", names(rtotal_tam))
names(rtotal_tam) <- sub("^Month$", "month", names(rtotal_tam))
names(rtotal_tam) <- sub("^Hour$", "hour", names(rtotal_tam))
names(rtotal_tam) <- sub("^Min$", "min", names(rtotal_tam))
names(rtotal_tam) <- sub("^Input.D$", "InputD", names(rtotal_tam))
names(rtotal_tam) <- sub("^Input.F$", "InputF", names(rtotal_tam))
  
# The following files are included in the files above. Keep these as backup.
# ACJ-01
# acj01_ctrl_13  <- read.table("~/Desktop/data_sorting/rsoil/ACJ_2013_ctrl_resp.csv", sep=",", header=T)
# acj01_ctrl_14  <- read.table("~/Desktop/data_sorting/rsoil/ACJ_2014_crtl_resp.csv", sep=",", header=T)
# TRU-04
# tru04_ctrl_13  <- read.table("~/Desktop/data_sorting/rsoil/TU4_2013_ctrl_resp.csv", sep=",", header=T)
# tru04_ctrl_13  <- read.table("~/Desktop/data_sorting/rsoil/TU4_2014_crtl_resp.csv", sep=",", header=T)
# PAN-02
# pan02_ctrl_13  <- read.table("~/Desktop/data_sorting/rsoil/PAN02_2013_crtl_resp.csv", sep=",", header=T)
# pan02_ctrl_14  <- read.table("~/Desktop/data_sorting/rsoil/PAN02_2014_crtl_resp.csv", sep=",", header=T)
# pan03_ctrl_13  <- read.table("~/Desktop/data_sorting/rsoil/PAN03_2013_ctrl_resp.csv", sep=",", header=T)
# pan03_ctrl_14  <- read.table("~/Desktop/data_sorting/rsoil/PAN03_2014_crtl_resp.csv", sep=",", header=T)
# ESP-01
# esp01_ctrl_13  <- read.table("~/Desktop/data_sorting/rsoil/ESP_2013_ctrl_resp.csv", sep=",", header=T)
# esp01_ctrl_14  <- read.table("~/Desktop/data_sorting/rsoil/ESP_2014_crtl_resp.csv", sep=",", header=T)
# WAY-01
# WAY_2013_ctrl_resp.csv
# SPD-02
# SP1500_2013_ctrl_resp.csv
# SPD-01
# SP1750_2013_ctrl_resp.csv 
# rpart_09to11  <- read.table("~/Desktop/data_sorting/rsoil/kos_partitionning_2009_2011.csv", sep=",", header=T)
# rpart_12to14  <- read.table("~/Desktop/data_sorting/rsoil/kos_partitionning_2012_2014.csv", sep=",", header=T)
# rctrl_11      <- read.table("~/Desktop/data_sorting/rsoil/KOS.CONTROL.FLUX.2011.csv", sep=",", header=T)



## CHECK this may come in handy: rtot_kos_ts   <- read.table("~/Desktop/data_sorting/rsoil/Totres_Kos_timeseries_2012.xlsx", sep=",", header=T)



# WEATHER & COLAR DEPTH

wrespar_tam   <- read.table("~/Desktop/data_sorting/rsoil/ResParTam_weather.csv", sep=",", header=T)
wrescon_tam   <- read.table("~/Desktop/data_sorting/rsoil/ResconTam_weather.csv", sep=",", header=T)
wrestot_tam   <- read.table("~/Desktop/data_sorting/rsoil/TotresTam_weather.csv", sep=",", header=T)
wres_tam_1113 <- read.table("~/Desktop/data_sorting/rsoil/TAM_Resp_suelo_Tem_Humed_Profundidad-2011-2013.csv", sep=",", header=T)
wres_tam_1516 <- read.table("~/Desktop/data_sorting/rsoil/Soil_respiration_component_ingrowthcore_weather_data_2015.csv", sep=",", header=T) # Weather for 2015 & 2016

wrestot_eltr <- read.table("~/Desktop/data_sorting/rsoil/TotresEltr_weather.csv", sep=",", header=T)
wrescon_eltr <- read.table("~/Desktop/data_sorting/rsoil/ResconEltr_weather.csv", sep=",", header=T)
wrespar_eltr <- read.table("~/Desktop/data_sorting/rsoil/ResParEltr_weather.csv", sep=",", header=T) 

wres_eltr_1314 <- read.table("~/Desktop/data_sorting/rsoil/Soil_respiration_component_weather_data_Kosnipata_2013_2014.csv", sep=",", header=T) 


# Tambopata

## wrescon_tam  

wrescon_tam$yr      <- wrescon_tam$year + 2000
wrescon_tam$s       <- paste("TAM-0", wrescon_tam$site, sep="")
wrescon_tam$col_num <- substring(wrescon_tam$num,1,1)
wrescon_tam$rep     <- substring(wrescon_tam$num,3,3)

aa <- c()

aa$plot_code                    <- wrescon_tam$s
aa$sub_plot                     <- NA
aa$year                         <- wrescon_tam$yr                       	
aa$month                        <- wrescon_tam$month                  
aa$day                          <- NA

aa                              <- data.frame(aa)
               
aa$measurement_code             <- "CTRL" 
aa$treatment_code_partitioning  <- NA   
aa$disturbance_code_control     <- NA
aa$ingrowth_core                <- NA            
aa$litter_code                  <- NA       
aa$collar_number                   <- as.numeric(as.character(wrescon_tam$col_num))              
aa$replica                      <- 1            
aa$vwc_percent_in               <- NA             
aa$vwc_percent_out              <- wrescon_tam$VWC..             
aa$soil_temp_c_in               <- NA           
aa$soil_temp_c_out              <- NA           
aa$air_temp_c                   <- wrescon_tam$Temp          
aa$collar_height_cm             <- wrescon_tam$Depth           
aa$quality_code                 <- "not_sure"           
aa$comments                     <- NA         

head(aa)
str(aa)

## wrespar_tam

wrespar_tam$yr      <- wrespar_tam$year + 2000
wrespar_tam$s       <- paste("TAM-0", wrespar_tam$site, sep="")
wrespar_tam$col_num <- substring(wrespar_tam$num,1,1)
wrespar_tam$rep     <- substring(wrespar_tam$num,3,3)

bb <- c()

bb$plot_code                   <- wrespar_tam$s 
bb$sub_plot                    <- NA
bb$year                        <- wrespar_tam$yr   	
bb$month                       <- wrespar_tam$month
bb                             <- data.frame(bb)
bb$day                         <- NA    
bb$measurement_code            <- "PART"  
bb$treatment_code_partitioning <- NA  
bb$disturbance_code_control    <- NA 
bb$ingrowth_core               <- NA
bb$litter_code                 <- NA
bb$collar_number                  <- as.numeric(as.character(wrespar_tam$col_num))             
bb$replica                     <- as.numeric(as.character(wrespar_tam$rep))               
bb$vwc_percent_in              <- NA   
bb$vwc_percent_out             <- wrespar_tam$VWC..  
bb$soil_temp_c_in              <- NA   
bb$soil_temp_c_out             <- NA
bb$air_temp_c                  <- wrespar_tam$Temp  
bb$collar_height_cm            <- wrespar_tam$Depth 
bb$quality_code                <- "not_sure"
bb$comments                    <- NA

head(bb)
str(bb)

## wrestot_tam

wrestot_tam$yr      <- wrestot_tam$year + 2000
wrestot_tam$s       <- paste("TAM-0", wrestot_tam$site, sep="")
wrestot_tam$col_num <- substring(wrestot_tam$num,1,1)
wrestot_tam$rep     <- substring(wrestot_tam$num,3,3)

cc <- c()

cc$plot_code                    <- wrestot_tam$s 
cc$sub_plot                     <- as.numeric(as.character(wrestot_tam$col_num))
cc$year                         <- wrestot_tam$yr              	
cc$month                        <- wrestot_tam$month  
cc                              <- data.frame(cc)
cc$day                          <- NA         
cc$measurement_code             <- "TOTAL"
cc$treatment_code_partitioning  <- NA 
cc$disturbance_code_control     <- NA
cc$ingrowth_core                <- NA
cc$litter_code                  <- NA
cc$collar_number                   <- as.numeric(as.character(wrestot_tam$col_num))
cc$replica                      <- as.numeric(as.character(wrestot_tam$rep)) 
cc$vwc_percent_in               <- NA   
cc$vwc_percent_out              <- wrestot_tam$VWC..   
cc$soil_temp_c_in               <- NA
cc$soil_temp_c_out              <- NA 
cc$air_temp_c                   <- wrestot_tam$Temp
cc$collar_height_cm             <- wrestot_tam$Depth
cc$quality_code                 <- NA 
cc$comments                     <- NA

head(cc)
str(cc)

head(wres_tam_1113)
hh <- c()

hh$plot_code                    <- wres_tam_1113$Plot  
hh$sub_plot                     <- wres_tam_1113$SubPlot
hh$year                         <- wres_tam_1113$YY                            
hh$month                        <- wres_tam_1113$MM           
hh$day                          <- wres_tam_1113$DD
hh                              <- data.frame(hh)
hh$measurement_code             <- wres_tam_1113$TypeMeas 
hh$treatment_code_partitioning  <- NA 
hh$disturbance_code_control     <- wres_tam_1113$Treat # CD D SD UD
hh$ingrowth_core                <- NA
hh$litter_code                  <- NA
hh$collar_number                   <- as.numeric(as.character(wres_tam_1113$CoNum))
hh$replica                      <- as.numeric(as.character(wres_tam_1113$Meas))  
hh$vwc_percent_in               <- NA 
hh$vwc_percent_out              <- wres_tam_1113$VWC 
hh$soil_temp_c_in               <- NA 
hh$soil_temp_c_out              <- wres_tam_1113$SoilTemp
hh$air_temp_c                   <- wres_tam_1113$AirTemp
hh$collar_height_cm             <- wres_tam_1113$CoHeight 
hh$quality_code                 <- "good" 
hh$comments                     <- wres_tam_1113$Notes


# TAM weather data 2015 & 2016 
head(wres_tam_1516)

ii <- c()

ii$plot_code                    <- wres_tam_1516$plot_code   
ii$sub_plot                     <- wres_tam_1516$sub_plot
ii$year                         <- wres_tam_1516$year                            
ii$month                        <- wres_tam_1516$month           
ii$day                          <- wres_tam_1516$day
ii                              <- data.frame(ii)
ii$measurement_code             <- wres_tam_1516$measurement_code 
ii$treatment_code_partitioning  <- wres_tam_1516$treatment_code_partitionning 
ii$disturbance_code_control     <- wres_tam_1516$disturbance_code_control
ii$ingrowth_core                <- wres_tam_1516$ingrowth_core
ii$litter_code                  <- wres_tam_1516$litter_code
ii$collar_number                   <- as.numeric(as.character(wres_tam_1516$collar_number))
ii$replica                      <- as.numeric(as.character(wres_tam_1516$replica))  
ii$vwc_percent_in               <- NA 
ii$vwc_percent_out              <- wres_tam_1516$vwc_pcnt 
ii$soil_temp_c_in               <- NA 
ii$soil_temp_c_out              <- wres_tam_1516$soil_temp_c
ii$air_temp_c                   <- wres_tam_1516$air_temp_c
ii$collar_height_cm             <- wres_tam_1516$collar_depth_cm 
ii$quality_code                 <- wres_tam_1516$quality_code 
ii$comments                     <- wres_tam_1516$comments


# TAM-03 = TAM-05
# TAM-04 = TAM-06
# TAM-09 = TAM-09



# Elevation transect
#wrestot_eltr

wrestot_eltr$yr      <- wrestot_eltr$year + 2000
wrestot_eltr$s       <- # WHAT ARE THE PLOT NUMBERS / CODES?
wrestot_eltr$col_num <- substring(wrestot_eltr$Plot,1,1)
wrestot_eltr$rep     <- substring(wrestot_eltr$Plot,3,3)

dd <- c()

dd$plot_code                    <- wrestot_eltr$s    
dd$sub_plot                     <- as.numeric(as.character(wrestot_eltr$col_num))
dd$year                         <- wrestot_eltr$yr              
dd$month                        <- wrestot_eltr$month 
dd                              <- data.frame(dd)
dd$day                          <- NA                
dd$measurement_code             <- "TOTAL"          
dd$treatment_code_partitioning  <- NA   
dd$disturbance_code_control     <- NA
dd$ingrowth_core                <- NA
dd$litter_code                  <- NA
dd$collar_number                   <- as.numeric(as.character(wrestot_eltr$col_num))
dd$replica                      <- as.numeric(as.character(wrestot_eltr$rep))  
dd$vwc_percent_in               <- NA    
dd$vwc_percent_out              <- wrestot_eltr$VWC  
dd$soil_temp_c_in               <- NA
dd$soil_temp_c_out              <- NA
dd$air_temp_c                   <- wrestot_eltr$Temperatura.Amb
dd$collar_height_cm             <- wrestot_eltr$Profunidad
dd$quality_code                 <- "not_sure"       
dd$comments                     <- NA                     

head(dd)
str(dd)

#wrescon_eltr

wrescon_eltr$yr      <- wrescon_eltr$year + 2000
wrescon_eltr$col_num <- substring(wrescon_eltr$Plot,1,1)
wrescon_eltr$rep     <- substring(wrescon_eltr$Plot,3,3)
wrescon_eltr$d       <- as.character(wrescon_eltr$dis..1.not.dis..2.dis.) 
wrescon_eltr$d       <- gsub("1","N", wrescon_eltr$d, ignore.case=T)
wrescon_eltr$d       <- gsub("2","Y", wrescon_eltr$d, ignore.case=T)

ee <- c()

ee$plot_code                       <- wrescon_eltr$plot  
ee$sub_plot                        <- NA
ee$year                            <- wrescon_eltr$yr                         	
ee$month                           <- wrescon_eltr$month
ee                                 <- data.frame(ee)
ee$day                             <- NA
ee$measurement_code                <- "CTRL"
ee$treatment_code_partitioning     <- NA
ee$disturbance_code_control        <- wrescon_eltr$d
ee$ingrowth_core                   <- NA
ee$litter_code                     <- NA
ee$collar_number                      <- as.numeric(as.character(wrescon_eltr$col_num))
ee$replica                         <- 1  
ee$vwc_percent_in                  <- NA
ee$vwc_percent_out                 <- wrescon_eltr$VWC 
ee$soil_temp_c_in                  <- NA
ee$soil_temp_c_out                 <- NA
ee$air_temp_c                      <- wrescon_eltr$Temperatura.Amb
ee$collar_height_cm                <- wrescon_eltr$Profunidad
ee$quality_code                    <- "not_sure"
ee$comments                        <- NA

head(ee)
str(ee)

# wrespar_eltr

wrespar_eltr$yr      <- wrespar_eltr$year + 2000
wrespar_eltr$s       <- # WHAT ARE THE PLOT NUMBERS / CODES?
wrespar_eltr$col_num <- substring(wrespar_eltr$X.Plot,1,1)
wrespar_eltr$rep     <- substring(wrespar_eltr$X.Plot,3,3)

ff <- c()

ff$plot_code                    <- wrespar_eltr$plot    
ff$sub_plot                     <- NA
ff$year                         <- wrespar_eltr$yr                          	
ff$month                        <- wrespar_eltr$month           
ff$day                          <- NA
ff                              <- data.frame(ff)
ff$measurement_code             <- "PART" 
ff$treatment_code_partitioning  <- NA 
ff$disturbance_code_control     <- NA
ff$ingrowth_core                <- NA
ff$litter_code                  <- NA
ff$collar_number                   <- as.numeric(as.character(wrespar_eltr$col_num))
ff$replica                      <- as.numeric(as.character(wrespar_eltr$rep))  
ff$vwc_percent_in               <- NA 
ff$vwc_percent_out              <- NA 
ff$soil_temp_c_in               <- NA 
ff$soil_temp_c_out              <- NA
ff$air_temp_c                   <- wrespar_eltr$Temperatura.Amb
ff$collar_height_cm             <- wrespar_eltr$Profunidad 
ff$quality_code                 <- "not_sure" 
ff$comments                     <- NA


#wres_eltr_1314
gg <- c()

gg$plot_code                    <- wres_eltr_1314$plot_code 
gg$sub_plot                     <- wres_eltr_1314$sub_plot
gg$year                         <- wres_eltr_1314$year                            
gg$month                        <- wres_eltr_1314$month           
gg$day                          <- wres_eltr_1314$day
gg                              <- data.frame(gg)
gg$measurement_code             <- wres_eltr_1314$measurement_code 
gg$treatment_code_partitioning  <- wres_eltr_1314$treatment_code_partitionning 
gg$disturbance_code_control     <- wres_eltr_1314$disturbance_code_control
gg$ingrowth_core                <- wres_eltr_1314$ingrowth_core
gg$litter_code                  <- wres_eltr_1314$litter_code
gg$collar_number                <- as.numeric(as.character(wres_eltr_1314$collar_number))
gg$replica                      <- as.numeric(as.character(wres_eltr_1314$replica))  
gg$vwc_percent_in               <- NA 
gg$vwc_percent_out              <- wres_eltr_1314$vwc_pcnt 
gg$soil_temp_c_in               <- NA 
gg$soil_temp_c_out              <- wres_eltr_1314$soil_temp_c
gg$air_temp_c                   <- wres_eltr_1314$air_temp_c
gg$collar_height_cm             <- wres_eltr_1314$collar_depth_cm 
gg$quality_code                 <- wres_eltr_1314$quality_code 
gg$comments                     <- wres_eltr_1314$comments


weather_09_14 <- rbind(aa, bb, cc, hh, ii, dd, ee, ff, gg)


"ResconEltr_weather.xlsx"                                            
"ResconTam_weather.xlsx"                                                                                     
"ResParTam_weather.xlsx"   
"ResParEltr_weather.xlsx"   
"S.comp.clima.Tam05.2013.csv"                                                                                
"S.comp.clima.Tam09.2013.csv" 
"S.comp.clima.Tam06.2013.csv"
"S.ctrl.clima.Tam09.2013.csv"
"S.ctrl.clima.Tam05.2013.csv"                                        
"S.ctrl.clima.Tam06.2013.csv"                                                                                
"Soil_component_respiration_data_2015.csv"                                                                     
"SP1750_2013_ctrl_resp.csv"                                                                                
"TAM 2014 Control.csv"                                                                                                
"sresp_Tam_weather_to2011.xlsx"                                                                                               
"TAM_Resp_suelo_Tem_Humed_Profund_desde_Jun07_nuevo (3).xlsx"                                                                                                          
"TotresTam_weather.xlsx"                                                                                           
"TotresEltr_weather.xlsx"                                            
"TotresTam_wea.xlsx"                                                                                            
"weather_for_total_par_control_ic_respiration_Jan_to_Aug_2015...xlsx"                                                                                     
                                         
 

# Clean weather_09_14 by standardising codes

# unique(weather_09_14$plot_code)
# Levels: TAM-05 TAM-06 TAM-07 TAM-09 1 2 3 4 5 6 7 8 9 N ACJ-01 ESP-01 PAN-02 PAN-03 SPD-01 SPD-02 TRU-04 WAY-01

revalue(weather_09_14$plot_code, c("1" = "WAY-01", "2" = "ESP-01", "3" = "SPD-01", "4" = "SPD-02")) #, "5" = "TAM-05", "6" = "TAM-06", "7" = "TAM-09", "N" = "NA"))
# WAY-01=1, ESP-01=2, SPD-01=3, SPD-02=4, TAM-05=5, TAM-06=6 and TAM-09=7. 
# TAM-03 = TAM-05
# TAM-04 = TAM-06
# TAM-09 = TAM-09

# disturbance_code_control
weather_09_14$disturbance_code_control <- revalue(weather_09_14$disturbance_code_control, c("SD" = "N", "CD" = "Y", "UD" = "N", "D" = "Y", "NaN" = "NA", "n" = "N", "y" = "Y"))

weather_09_14$litter_code <- revalue(weather_09_14$litter_code, c("nonne" = "none", "NaN" = "NA"))

# Check this: unique((weather_09_14$quality_code))

# unique((weather_09_14$treatment_code_partitioning))
# [1] NA              "con_nor_lit"   "con_no_lit"    "con_doub_lit"  "my_nor_lit"    "my_no_lit"     "my_doub_lit"   "so_nor_lit"    "so_no_lit"     "so_doub_lit"   "NaN"           "mlayer_no_lit"
# [13] "UD"            "U" 
weather_09_14$treatment_code_partitioning <- revalue(weather_09_14$treatment_code_partitioning, c("UD" = "NA", "U" = "NA", "NaN" = "NA"))

# CHECK THIS IS NOT A PROBLEM:
# ;Plot = treatment_code
# ;Plot is component  soil respiration (nine collars of PVC)
# 1.1  con_nor_lit # 2.1 con_no_lit # 3.1 con_doub_lit # 4.1 my_nor_lit # 5.1 my_no_lit # 6.1 my_doub_lit # 7.1 so_nor_lit # 8.1 so_no_lit # 9.1 so_doub_lit

head(weather_09_14)

#
# Problem in weather: "8_2_11"     "9_2_11"     "10_2_11"  (...) "7_12_12"    "8_12_12"    "9_12_12"    "10_12_12" (...) "3_3_13"     "4_3_13"     "5_3_13"     "6_3_13"
#


# ELEVATION TRANSECT

# TAM-03 = TAM-05
# TAM-04 = TAM-06
# TAM-09 = TAM-09

rsoil_control_eltr              <- rbind(rcontrol_kos, rcontrol_tam)  # c <- rcontrol_kos[, which(colnames(rcontrol_kos)%in% colnames(rcontrol_tam))] # rsoil_control_eltr$recno <- as.numeric(as.character(rsoil_control_eltr$recno))

rsoil_total_eltr                <- rbind(rtotal_kos, rtotal_tam)
rsoil_total_eltr$plot_code      <- toupper(rsoil_total_eltr$plot_code)

rsoil_part_eltr                 <- rbind(rpart_kos, rpart_tam)

# one last thing...
rsoil_part_eltr$egm_measurement <- as.integer(as.character(rsoil_part_eltr$egm_measurement))
XXX             <- sqldf("select rsoil_part_eltr.* from rsoil_part_eltr where rsoil_part_eltr.month = '0'")
w <- which(rsoil_part_eltr$month == '0')
rsoil_part_eltr$month[w] <- 12


# save files
write.csv(rsoil_control_eltr, file="eltr_rsoil_control_dec16.csv") 

write.csv(rsoil_part_eltr, file="eltr_rsoil_part_dec16.csv")                     

write.csv(rsoil_total_eltr, file="eltr_rsoil_total_dec16.csv") 

write.csv(weather_09_14, file="eltr_rsoil_weather_dec16.csv") 

