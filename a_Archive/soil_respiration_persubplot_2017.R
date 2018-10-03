 ### Function Soil respiration:
# This function calculates soil respiration and uses input data specified in the RAINFOR-GEM manual.
# Based on matlab code developed by Chris Doughty, 2011.
# Last edited: Cecile Girardin, 10.09.2015

### Required Data:
# dataframe of total soil respiration
# dataframe of partition respiration 
# dataframe of control respiration 
# plotname: specify plot_code of the plot you are working with (eg. WAY-01)
# ret: data-format of return values: "monthly.means.ts" or "monthly.means.matrix"
# plotit: logical (T/F), plot a quick graphical summary of the data?
# User has to specify either elevation or pressure.

## TO DO: have a switch, so that you can either choose to get het / aut respiration per subplot, based on Rs_total from each subplot (i.e. the way it is estimated here), 
# OR just spit out het / aut respitation for the subplots we measure them in. No scaling.


#load packages
library(sqldf)
require(ggplot2)


### read data (option 1):

# South America data. These are flux data from the PED papers. INFO MISSING. GO BACK TO RAW DATA? 
# plot_code: 1=BolA, 2=BolB, 3=Iq1, 4=Iq2, 5=TangA, 6=TangB
setwd("~/Github/gemcarbon_data/processed_ts_2017") #/soil_respiration_flux
dataresc  <- read.table("Resconall_cd.txt", sep="", header=T) 
colnames(dataresc) <- c("year", "month", "plot_code", "area", "replica", "CO2", "t_air", "vwc", "depth_cm", "DCO2")
dataresp  <- read.table("Resparall_cd.txt", sep="", header=T)  
colnames(dataresp) <- c("year", "month", "plot_code", "area", "replica", "CO2", "t_air", "vwc", "depth_cm", "DCO2")
datarest  <- read.table("Restotall_cd.txt", sep="", header=T)  
colnames(datarest) <- c("year", "month", "plot_code", "area", "replica", "CO2", "t_air", "vwc", "depth_cm", "DCO2")
revalue(datarest$plot_code, c("1" = "KEN-01", "2" = "KEN-02", "3" = "ALP-01", "4" = "ALP-30", "5" = "TAN-01", "6" = "TAN-02")) 

# info we need: codew  flux_umolm2sec	plot_code	sub_plot	measurement_code	treatment_code_partitioning	egm_measurement	day	month	year	hour	soil_temp_c_out	vwc_percent_out	collar_height_cm

# Elevation Transect data
# SPD-02 SPD-01 ESP-01 WAY-01 ACJ-01 PAN-02 PAN-03 TRU-04 TAM-05 TAM-06 TAM-09
setwd("~/Github/gemcarbon_data/processed_ts_2017/soil_respiration_flux")
# total
dat1  <- read.table("flux_total_ACJ01.csv", sep=",", header=T)  
dat2  <- read.table("flux_total_ESP01.csv", sep=",", header=T) 
dat3  <- read.table("flux_total_PAN02.csv", sep=",", header=T)  
dat4  <- read.table("flux_total_PAN03.csv", sep=",", header=T) 
dat5  <- read.table("flux_total_SPD01.csv", sep=",", header=T) 
dat6  <- read.table("flux_total_SPD02.csv", sep=",", header=T)  
dat7  <- read.table("flux_total_TAM05_mar17.csv", sep=",", header=T) 
dat8  <- read.table("flux_total_TAM06_mar17.csv", sep=",", header=T)  
dat9  <- read.table("flux_total_TAM09_mar17.csv", sep=",", header=T)  
dat10 <- read.table("flux_total_TRU04.csv", sep=",", header=T)  
dat11 <- read.table("flux_total_WAY01.csv", sep=",", header=T) 
datarest <- dat9 #rbind(dat1, dat2, dat3, dat4, dat5, dat6, dat7, dat8, dat9, dat10, dat11) 

# control
dat1  <- read.table("flux_control_ACJ01.csv", sep=",", header=T)  
dat2  <- read.table("flux_control_ESP01.csv", sep=",", header=T) 
dat3  <- read.table("flux_control_PAN02.csv", sep=",", header=T)  
dat4  <- read.table("flux_control_PAN03.csv", sep=",", header=T) 
dat5  <- read.table("flux_control_SPD01.csv", sep=",", header=T) 
dat6  <- read.table("flux_control_SPD02.csv", sep=",", header=T)  
dat7  <- read.table("flux_control_TAM05_mar17.csv", sep=",", header=T)  
dat8  <- read.table("flux_control_TAM06_mar17.csv", sep=",", header=T)  
dat9  <- read.table("flux_control_TAM09_mar17.csv", sep=",", header=T)  
dat10 <- read.table("flux_control_TRU04.csv", sep=",", header=T)  
dat11 <- read.table("flux_control_WAY01.csv", sep=",", header=T) 
dataresc <- dat9 #rbind(dat1, dat2, dat3, dat4, dat5, dat6, dat7, dat8, dat9, dat10, dat11) 
discor1  <-  read.table("disturbance_perplot_2017.csv", sep=",", header=T)

# partitioning
dat1  <- read.table("flux_part_ACJ01.csv", sep=",", header=T)  
dat2  <- read.table("flux_part_ESP01.csv", sep=",", header=T) 
dat3  <- read.table("flux_part_PAN02.csv", sep=",", header=T)  
dat4  <- read.table("flux_part_PAN03.csv", sep=",", header=T) 
dat5  <- read.table("flux_part_SPD01.csv", sep=",", header=T) 
dat6  <- read.table("flux_part_SPD02.csv", sep=",", header=T)  
dat7  <- read.table("flux_part_TAM05_mar17.csv", sep=",", header=T)  
dat8  <- read.table("flux_part_TAM06_mar17.csv", sep=",", header=T)  
dat9  <- read.table("flux_part_TAM09_mar17.csv", sep=",", header=T)  
dat10 <- read.table("flux_part_TRU04.csv", sep=",", header=T)  
dat11 <- read.table("flux_part_WAY01.csv", sep=",", header=T) 
dataresp <- dat9 #rbind(dat1, dat2, dat3, dat4, dat5, dat6, dat7, dat8, dat9, dat10, dat11) 

# dataresp$collar_height_cm has a lot of NAs, I am replacing NAs by mean(dataresp$collar_height_cm, na.rm=T)
# dataresp$collar_height_cm[is.na(dataresp$collar_height_cm)] <- mean(dataresp$collar_height_cm, na.rm=T)

# Define function parameters
pressure = 1013.25
plotname = "TAM-09"
partitioningoption = 1
elevation = "Default"
T_ambient="Default"
plotit=T

### read data for option 2 (Africa).
#setwd("/Users/Cecile/Dropbox/Carbon_Use_Efficieny_R/testing/soilresp")
#dataresc <- read.table("Resconallsam.csv", sep=",", header=T)
#dataresp <- read.table("Resparallsam.csv", sep=",", header=T)
#datarest <- read.table("Restotallsam.csv", sep=",", header=T)
#pressure = 1013.25
#plotname = 1.1
#partitioningoption = 2
#elevation = "Default"
#pressure="Default"
#T_ambient="Default"

# read correction functions:
setwd("~/Github/GEMcarbon.R")
source("~/Github/GEMcarbon.R/soilrespiration_auxfunctions.r")

script.dir <- dirname(sys.frame(1)$ofile)
source(paste0(script.dir,"/soilrespiration_auxfunctions.R"))

soilrespiration <- function(datarest,dataresp,dataresc, plotname, ret="monthly.means.ts", # Add tube radius as a parameter, change A to A <- pi*(rad^2) 
                            partitioningoption="Default",
                            pressure="Default", elevation="Default", T_ambient="Default",
                            plotit=F) {
  
  
  ## Defaults for chamber volume and tube area:
  # The CPY-2 defaults are Volume = 2465 cm3  Area = 170 cm2 and V/A = 1450
  Vd = 1171/1000000    # chamber volume m3
  A = 0.0078           # tube area m2
  
  ## Corrections and conversions
  # add a temperature correction from Sotta et al 2004 Q10=1.8 and k=0.0613
  corrsresA = exp(-0.0695*(1))
  # Convert units umol m2 s-1 to MgC ha month = 1mo=2592000sec, 10000m2=1ha, 1000000umol = 1 mol, 1mol = 12 g, 1000000g=1Mg
  convert = (2592000*10000*12)/(1000000*1000000)
  
  
  ## TOTAL SOIL RESPIRATION per subplot, Mg C / ha / yr.

  datarest <- subset(datarest, plot_code == plotname)
  # remove outliers and NAs: Fluxes based on overall correction, ## Temperature and chamber correction: Temperature and Chamber height (see functions!)
  # Note: the choice of the sd_interval changes things.
  
  # Initialise dataframe for timeseries (ts)
  tst <- data.frame(datarest$plot_code, datarest$sub_plot, datarest$plot_corner_code, datarest$collar_number, datarest$measurement_code, datarest$replica, datarest$year, datarest$egm_measurement, datarest$recno, datarest$day, datarest$month, datarest$hour, datarest$air_temp_c, datarest$ch_fill, datarest$flux)
  colnames(tst) <- c("plot_code", "sub_plot", "plot_corner_code", "collar_number", "measurement_code", "replica", "year", "egm_measurement", "recno", "day", "month", "hour", "soil_temp_c_out", "collar_height_cm", "flux")  
  
  tst$fluxt <- rm.flux.outlier(tst$flux, 4) 
  tst$tempt <- rm.temp.outlier(tst$soil_temp_c_out, tst$month) 
  
  tst$date <- as.Date(paste(tst$year, tst$month, tst$day, sep="."), format="%Y.%m.%d") 
  tst = tst[order(tst$sub_plot,tst$date),]

  # Group by replica  
  ts_total <- sqldf("SELECT tst.plot_code, tst.sub_plot, year, day, month, hour, AVG(soil_temp_c_out), AVG(collar_height_cm), AVG(fluxt), STDEV(fluxt) FROM tst GROUP BY year, month, sub_plot")
  colnames(ts_total) <- c("plot_code", "sub_plot", "year", "day", "month", "hour", "soil_temp_c_out", "collar_height_cm", "Rs_total", "Rs_total_std")  
  ts_total$date <- as.Date(paste(ts_total$year, ts_total$month, ts_total$day, sep="."), format="%Y.%m.%d") 
  ts_total$soil_temp_c_out <- as.numeric(as.character(ts_total$soil_temp_c_out))
  ts_total$Rs_total <- as.numeric(as.character(ts_total$Rs_total))
  
  ts_total$Rs_total_MgC_ha_mo = ts_total$Rs_total*convert*corrsresA
  ts_total$Rs_total_MgC_ha_mo_std = ts_total$Rs_total_std*convert*corrsresA
  
  plot <- ggplot(ts_total, aes(x = date, y = Rs_total_MgC_ha_mo, na.rm = T)) +
          geom_point(data = ts_total, aes(x = date, y = Rs_total_MgC_ha_mo), size = 2, colour = ts_total$sub_plot, na.rm=T) +
          ggtitle(plotname)
  plot

  ts_total$Rs_total  <- NULL
  ts_total$Rs_total_std <- NULL
  
  #ts_total1 <- subset(ts_total, sub_plot == 1 | sub_plot == 2 | sub_plot == 3 | sub_plot == 4 | sub_plot == 5)
  #setwd("~/Github/gemcarbon_data/processed_ts_2017/ts_soil_respiration")
  #write.csv(ts_total, file="ts_TAM06_Rs_total_2017.csv") 

 
  
  ### CONTROL SOIL RESPIRATION
  
  # remove outliers (> 3 SD) and NAs:
  dataresc$fluxc <- rm.flux.outlier(dataresc$flux, sd_interval = 4)                     # Discuss sd_interval with team: it makes a big difference to the data if you use 2 sd or 3 sd.
  dataresc$tempc <- rm.temp.outlier(dataresc$soil_temp_c_out, dataresc$month)
  
  # Corrections and conversions
  
  dataresc$Rs_control_MgC_ha_mo = dataresc$flux_umolm2sec*convert*corrsresA
  dataresc$Rs_control_std = 0/0
  
  
  control_d                 <- subset(dataresc, disturbance_code_control == "Y", select = c(plot_code, year, Rs_control_MgC_ha_mo))
  control_ud                <- subset(dataresc, disturbance_code_control == "N", select = c(plot_code, year, Rs_control_MgC_ha_mo))
  avg_d_yr                  <- sqldf("SELECT control_d.plot_code, AVG(Rs_control_MgC_ha_mo), STDEV(Rs_control_MgC_ha_mo) FROM control_d GROUP BY plot_code")
  colnames(avg_d_yr)        <- c("plot_code", "disturbed_control", "disturbed_control_std")
  avg_ud_yr                 <- sqldf("SELECT control_ud.plot_code, AVG(Rs_control_MgC_ha_mo), STDEV(Rs_control_MgC_ha_mo) FROM control_ud GROUP BY plot_code")
  colnames(avg_ud_yr)       <- c("plot_code", "undisturbed_control", "undisturbed_control_std")
  disturbance               <- merge(avg_d_yr, avg_ud_yr, by = "plot_code")
  colnames(disturbance)     <- c("plot_code", "disturbed_control_MgC_ha_mo", "disturbed_control_std", "undisturbed_control_MgC_ha_mo", "undisturbed_control_std")
  disturbance$dist_yr       <- as.numeric(as.character(disturbance$undisturbed_control_MgC_ha_mo)) - as.numeric(as.character(disturbance$disturbed_control_MgC_ha_mo))
  disturbance$dist_yr_std   <- sqrt(as.numeric(as.character(disturbance$undisturbed_control_std))^2 + as.numeric(as.character(disturbance$disturbed_control_std))^2)
  
  print(disturbance)
  
  #setwd("~/Github/gemcarbon_data/processed_ts_2017/ts_soil_respiration")
  #write.csv(disturbance, file="disturbance_perplot_2017.csv")
  
  
  
  #### PARTITIONING SOIL RESPIRATION
  
  # Partitioning option 
  if (partitioningoption=="Default") {
    print("Warning! No partitioning option (see RAINFOR manual, p. 56) was specified.")
    print("Please specify the variable 'partitioningoption' in the function call.")
    partitioningoption=1
  }
  
  if (partitioningoption==2) {
    print("Code is running on partitioning option 2 (RAINFOR-GEM manual, p. 56).")
  }
  
  if (partitioningoption==1) {
    print("Code is running on partitioning option 1 (RAINFOR-GEM manual, p. 56).")
  }
  
  if (pressure=="Default" & elevation=="Default" & T_ambient=="Default") {
    print("WARNING! Neither ambient pressure nor site elevation was specified")
    print("Calculations will be based on p=1013.25 hPa (sea level) and temperature-independent barometric equation.")
    pressure <- 1013.25
  }
  
  if (pressure!="Default") {
    print("Ambient pressure was specified and will be used for flux correction.")
  }
  
  if (pressure=="Default" & elevation!="Default" & T_ambient=="Default") {
    print("Ambient pressure and temperature was not specified. Ambient pressure for flux correction is calculated from elevation
          using the temperature-independent barometric equation.")
    pressure <- barometric_equation(elevation)
  }
  
  if (pressure=="Default" & elevation!="Default" & T_ambient!="Default") {
    print("Ambient pressure was not specified. Ambient pressure for flux correction is calculated from elevation
           and ambient temperature (in 0C) using the temperature-dependent barometric equation.")
    pressure <- barometric_equation_T(elevation, T_ambient)
  }
  
    
  # remove outliers and NAs: Flux (sd > 3), Temperature and Chamber height (see soilrespiration_aux-functions.R)
  dataresp$date <- as.Date(paste(dataresp$year, dataresp$month, dataresp$day, sep="."), format="%Y.%m.%d") 
  dataresp = dataresp[order(dataresp$sub_plot,dataresp$date),]
  
  dataresp$fluxp <- rm.flux.outlier(dataresp$flux, sd_interval=4) 
  dataresp$tempp <- rm.temp.outlier(dataresp$soil_temp_c_out, dataresp$month) 
 
  
  # Corrections and conversions

  dataresp$Rs_part_MgC_ha_mo = dataresp$fluxp*convert*corrsresA
  dataresp$Rs_part_std = 0/0
  
  dataresp$id   <- paste(dataresp$sub_plot, dataresp$day, dataresp$month, dataresp$year, sep=".")
  
  discor2 <- subset(discor1, plot_code == plotname)
  discor  <- abs(discor2$dist_yr)                       ####################### QUESTION: SOMETIMES DISTURBANCE CORRECTION IS NEGATIVE. DO WE WANT THE ABSOLUTE VALUE IN THAT CASE?
  
  ### Calculate respiration values in each year and month for the three different treatments:
  
  # Partitioning: initialize matrices:
  
  if (partitioningoption == 1) {
    
    con1  <- subset(dataresp, treatment_code_partitioning == "con_nor_lit", select = c(id, Rs_part_MgC_ha_mo))
    con2  <- subset(dataresp, treatment_code_partitioning == "con_no_lit", select = c(id, Rs_part_MgC_ha_mo))
    con3  <- subset(dataresp, treatment_code_partitioning == "con_doub_lit", select = c(id, Rs_part_MgC_ha_mo))
    my1   <- subset(dataresp, treatment_code_partitioning == "my_nor_lit", select = c(id, Rs_part_MgC_ha_mo))
    my2   <- subset(dataresp, treatment_code_partitioning == "my_no_lit", select = c(id, Rs_part_MgC_ha_mo))
    my3   <- subset(dataresp, treatment_code_partitioning == "my_doub_lit", select = c(id, Rs_part_MgC_ha_mo))
    so1   <- subset(dataresp, treatment_code_partitioning == "so_nor_lit", select = c(id, Rs_part_MgC_ha_mo))
    so2   <- subset(dataresp, treatment_code_partitioning == "so_no_lit", select = c(id, Rs_part_MgC_ha_mo))
    so3   <- subset(dataresp, treatment_code_partitioning == "so_doub_lit", select = c(id, Rs_part_MgC_ha_mo))
    
    # build new dataframe
    tempdata = merge(con1, con2, by='id', all=T)
    tempdata = merge(tempdata, con3, by='id', all=T)
    colnames(tempdata) <- c("id", "con_nor_lit_MgC_ha_mo", "con_no_lit_MgC_ha_mo", "con_doub_lit_MgC_ha_mo")
    tempdata = merge(tempdata, my1, by='id', all=T)
    tempdata = merge(tempdata, my2, by='id', all=T)
    tempdata = merge(tempdata, my3, by='id', all=T)
    colnames(tempdata) <- c("id",  "con_nor_lit_MgC_ha_mo", "con_no_lit_MgC_ha_mo", "con_doub_lit_MgC_ha_mo", "my_nor_lit_MgC_ha_mo", "my_no_lit_MgC_ha_mo", "my_doub_lit_MgC_ha_mo")
    tempdata = merge(tempdata, so1, by='id', all=T)
    tempdata = merge(tempdata, so2, by='id', all=T)
    tempdata = merge(tempdata, so3, by='id', all=T)
    colnames(tempdata) <- c("id", "con_nor_lit_MgC_ha_mo", "con_no_lit_MgC_ha_mo", "con_doub_lit_MgC_ha_mo", "my_nor_lit_MgC_ha_mo", "my_no_lit_MgC_ha_mo", "my_doub_lit_MgC_ha_mo", "so_nor_lit_MgC_ha_mo", "so_no_lit_MgC_ha_mo", "so_doub_lit_MgC_ha_mo")
    
  } else if (partitioningoption == 2) {
    
    con_lit  <- 
    S1       <- 
    S2       <- 
    S3       <- 
    S1std    <-  
    S2std    <- 
    S3std    <- 
    
    tempdata = 
    .
    .
    .
    colnames(tempdata) <-
    
  }
  
  # merge tempdata and dataresp
  tempresp  <- sqldf("SELECT dataresp.id, dataresp.plot_code, dataresp.sub_plot, dataresp.day, dataresp.month, dataresp.year, dataresp.hour, dataresp.tempp, dataresp.collar_height_cm, dataresp.vwc_percent_out FROM dataresp GROUP BY dataresp.id")
  tsp       <- merge(tempdata, tempresp, by = 'id', all.x = TRUE) 
  
  # estimate rhizosphere respiration
  # Fraction allocated to root respiration under three different treatments: control, no litter and double litter.
  if (partitioningoption == 1) {

    tsp$rr1 = ((tsp$con_no_lit_MgC_ha_mo - (tsp$so_no_lit_MgC_ha_mo + (-discor))) / (tsp$con_no_lit_MgC_ha_mo))
    test <- tsp$rr1[which(tsp$rr1>1)]
    test
    tsp$rr1[which(tsp$rr1>1)] = 0/0
    
    tsp$rr2 = ((tsp$con_nor_lit_MgC_ha_mo - (tsp$so_nor_lit_MgC_ha_mo + discor)) / (tsp$con_nor_lit_MgC_ha_mo))
    tsp$rr2[which(tsp$rr2>1)] = 0/0
    
    tsp$rr3 = ((tsp$con_doub_lit_MgC_ha_mo - (tsp$so_doub_lit_MgC_ha_mo + discor)) / (tsp$con_doub_lit_MgC_ha_mo))
    tsp$rr3[which(tsp$rr3>1)] = 0/0
    
    tsp$rr = (tsp$rr1 + tsp$rr2 + tsp$rr3)/3
    is.na(tsp$rr) <- !is.finite(tsp$rr) 
    tsp$rr[which(tsp$rr>1)] = 0/0
    r <- abs(tsp$rr)
    rr <- mean(r, na.rm=T)
    
    tsp$rr_std = sd(r, na.rm=T)   
    
    
  } else if (partitioningoption == 2) {
    
    tsp$rr = ((S1-(S3+discor))/S1) # Check 
    ...
  
  }
  

  ## autotrophic root respiration:
  ts_total$Rs_root_MgC_ha_mo     <- ts_total$Rs_total_MgC_ha_mo*rr
  ts_total$Rs_root_MgC_ha_mo_std <- (ts_total$Rs_total_MgC_ha_mo_std*rr)/sqrt(length(ts_total$Rs_total_MgC_ha_mo_std))
  
  ## heterotrophic respiration:
  ts_total$Rs_het_MgC_ha_mo      <- ts_total$Rs_total_MgC_ha_mo*(1-rr)
  ts_total$Rs_het_MgC_ha_mo_std  <- (ts_total$Rs_total_MgC_ha_mo_std*(1-rr))/sqrt(length(ts_total$Rs_total_MgC_ha_mo_std))
  
 plota <- ggplot(ts_total, aes(x = date, y = Rs_het_MgC_ha_mo, na.rm = T)) +
   #geom_point(data = ts_total, aes(x = date, y = Rs_total_MgC_ha_mo), size = 2, colour = "darkgrey", na.rm=T) +
   #geom_point(data = ts_total, aes(x = date, y = Rs_root_MgC_ha_mo), size = 2, colour = "blue", na.rm=T) + #ts_total$sub_plot
   geom_point(data = ts_total, aes(x = date, y = Rs_het_MgC_ha_mo), size = 2, colour = "darkgreen", na.rm=T) +
   ylim(0, 2.5) + 
   ggtitle(plotname)
 plota
 
 # Save file
 setwd("~/Github/gemcarbon_data/processed_ts_2017/ts_soil_respiration")
 write.csv(ts_total, file="ts_TAM09_Rs_part_may17.csv")
 
  # Return a timeseries
 return(ts_total)

 
}
