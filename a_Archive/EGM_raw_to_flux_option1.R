## Code to go from EGM-4 raw data files to the csv files we need for the soilrespiration_Aug.R code
# Cecile Girardin 28.02.2014
# Last edited: Cecile Girardin 15.10.2014

# Notes:
## Need to define the disturbance and partitioning codes for each plot - search "get disturbance code" and "get partitioning code". Does this fit your data?

# Column names needed for this code:
# plot_code : this is the rainfor code of your ploy (e.g. TAM-05)
# disturbance_code :  1 = no disturbance, 2 = disturbed. This is only needed for the control file.
# partitionning code : [Sam, please specify what 1,2,3,4 stand for here. Maybe we should replace them by character strings. Let's discuss with the team.]
# year: add the year to your dataset
# Keep all the columns you get from the EGM: ;Plot, RecNo, Day, Month, Hour, Min, CO2 Ref, mb Ref, mbr Temp, InputA, InputB, InputC, InputD, InputE, InputF, InputG, InputH, atmp, Probe Type

# simple steps:
# 1. organise respiration data into three files: total, partitionning and control. Save your files as .csv
# 2. change directory in line 25 and replace "T_S_Resp_Lp1_01.07.2013.csv" with the name of the file you want to run this code through (same for control and partitionning)
# 3. Do the same for the temperature and vwc files. 
# 4, Change COLLAR DIAMETER (cm) in the code, for each section; total, control and partitionning.
# 5. run code
# 6. your new .csv files will be in the directory you specify in lines 111, 180 & 256
# 7. Run the soilrespiration.R function



# load packages
  library(sqldf)
# read in soil respiration auxillary functions from GitHub
  setwd("~/Github/GEMcarbon.R")
  source("~/Github/GEMcarbon.R/soilrespiration_auxfunctions.r")

# read in data 
  setwd("~/Desktop/data_sorting/rsoil")
  raw_consr   <- read.table("~/Desktop/data_sorting/rsoil/rsoil_control_eltr_nov16.csv", sep=",", header=T)
  raw_parsr   <- read.table("~/Desktop/data_sorting/rsoil/rsoil_part_eltr_nov16.csv", sep=",", header=T) 
  raw_totsrA   <- read.table("~/Desktop/data_sorting/rsoil/rsoil_total_eltr_nov16.csv", sep=",", header=T)
  weather     <- read.table("~/Desktop/data_sorting/rsoil/rsoil_weather_eltr.csv", sep=",", header=T) 
  collar_avg  <- read.table("~/Desktop/data_sorting/rsoil/collar_height_avg.csv", sep=",", header=T) 
  
  setwd("C:/Users/Cecile/Dropbox/GEMcarbondb/RawData_SA/Kos?ipata/2014_Respiration/Control_soil_respiration_2013")
  raw_totsrA        <- read.table("eltr_tot.csv", header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE)
  raw_parsr         <- read.table("eltr_part.csv", header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE)
  raw_consr         <- read.table("eltr_ctrl.csv", header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE)
  wea_tot           <- read.table("eltr_tot_temp.csv", header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE)  # column names: plot, day, month, year, subplot, temp, vwc  
  wea_con           <- read.table("eltr_ctrl_temp.csv", header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE)  # column names: plot, day, month, year, measurement, disturb, temp, vwc. Note: "measurement" is the measurement number (e.g. 21-35) 
  wea_par           <- read.table("eltr_part_temp.csv", header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE)  # column names: plot, day, month, year, measurement, temp, vwc

# Functions
  mgsub <- function(pattern, replacement, x, ...) {
    if (length(pattern)!=length(replacement)) {
      stop("pattern and replacement do not have the same length.")
    }
    result <- x
    for (i in 1:length(pattern)) {
      result <- gsub(pattern[i], replacement[i], result, ...)
    }
    result
  }
  
  
  
## Total soil respiration 
# select a plot
  raw_totsrA$plot_code <- toupper(raw_totsrA$plot_code) ########## Attention!! delete this.
  
  raw_totsrA            <- subset(raw_totsrA, plot_code=="ESP-01")
  raw_totsrA$collar_num <- raw_totsrA$sub_plot   ########## Attention!! We don't always need this.
  wea_tot               <- subset(weather, plot_code=="ESP-01" | measurement_code=="TOTAL")
  collar                <- subset(collar_avg, plot_code=="ESP-01")
  
# Add air temperature (temp), volumetric water content (vwc), and chamber depth (depth) to the raw control data (raw_consr)
  wea_tot$code1     <- paste(wea_tot$collar_num, wea_tot$month, wea_tot$year, sep=".") # wea_tot$day, wea_tot$month, wea_tot$year, sep=".") # only use code 1 to merge wea_tot and raw_totsr (subplot.day.month.year is not a unique identifier).
  raw_totsrA$code1   <- paste(raw_totsrA$collar_num, raw_totsrA$month, raw_totsrA$year, sep=".") # raw_totsrA$day, raw_totsrA$month, raw_totsrA$year, sep=".")
  raw_totsrB        <- sqldf("SELECT raw_totsrA.*, wea_tot.soil_temp_c_out, wea_tot.vwc_percent_out, wea_tot.collar_height_cm FROM  raw_totsrA JOIN  wea_tot  ON raw_totsrA.code1 = wea_tot.code1")
  
  {is.na(raw_totsrB$collar_height_cm)
  }  
  
  # placeholder: use this until we get the complete weather & collar_height dataset from Walter:
  
  
  ###################################### I am here : raw_totsrA.code1 = wea_tot.code1 dates don't coincide
  ###################################### I am here : raw_totsrA.code1 = wea_tot.code1 dates don't coincide
  
  
  # code1 for collar_num.day.month.year, but then use egm_measurement for codew
  raw_totsr$codew   <- paste(raw_totsr$egm_measurement, raw_totsr$day, raw_totsr$month, raw_totsr$year, sep=".")
  
# Collar diameter (cm)
  raw_totsr$collar_diam <- 12
  
# Estimate missing pressure whith temperature-dependent version of the barometric equation (see soilrespiration_aux-functions)
  t <- mean(as.numeric(wea_tot$temp), na.rm=T)
  w <- which(is.na(raw_totsr$atmp))
  raw_totsr$atmp[w] <- barometric_equation_T(elevation=0, temp=t )
         
## estimate flux for each measurement
  ## Sanity checks. Plot each flux batch to check data.
 
  #### TO DO: this only shows the last plot. What's wrong? 
  op <- par(ask=TRUE) # http://stackoverflow.com/questions/6031093/making-a-series-of-plots-that-proceed-by-a-click
  for (i in 1:length(raw_totsr$codew)){
    aa <- subset(raw_totsr, codew == codew[i], select = c(codew, co2ref, time))
    colnames(aa) <- c("codew", "co2", "time")
    plot(aa$co2, aa$time, main = paste("code:", head(aa$codew, 1)), xlab="time", ylab="CO2 (micro mol s-1? ppm?)")
    par(op)
  }

  ## Linear fit, estimate r2 quality check. 
  umea <- unique(raw_totsr$codew)
  xx <- c()
  yy <- c()
  zz <- c()
  
  for (i in 1:length(umea)) {
    sub  <- subset(raw_totsr, subset=(raw_totsr$codew == umea[i]))
    fit  <- lm(sub$time ~ sub$co2ref) 
    r    <- summary(fit)$r.squared 
    p    <- anova(fit)$'Pr(>F)'[1] # or p <- summary(fit)$coefficients[,4]
    u    <- head(sub$codew, 1)
    xx   <- rbind(xx, r) 
    yy   <- rbind(yy, p)
    zz   <- rbind(zz, u)  
  }
  table <- data.frame(cbind(xx, yy, zz))
  colnames(table) <- c("r2", "pvalue", "unique_code") # notification: small r2 doesn't mean bad flux - small flux = small r2
  
# get unique identifyer for each measurement
  uid <- unique(raw_totsr$codew)
  xx <- c()
  yy <- c()
  
  
  #for (i in uid) {
  for (i in 1:length(uid)) {
    sub      <- subset(raw_totsr, subset=(raw_totsr$codew == uid[i]))
    id       <- tail(sub$codew, n=1) 
    ten_co2  <- tail(sub$co2ref, n=10)   # !!!!!!!!! Drop first two values rather than only keep last 10 values.
    ten_time <- tail(sub$time, n=10)       
    fit      <- lm(ten_co2~ten_time)
    Co2slope <- fit$coefficients[2]      # USE SLOPE RATHER THAN DIFFERENCE OF C10-C1
    C10      <- tail(ten_co2, n=1)                                                   # last CO2 measurement of last 10 measurements
    C1       <- head(ten_co2, n=1)                                                   # first CO2 measurement of last 10 measurements
    t10      <- tail(ten_time, n=1)                                                  # last time step of 10 last measurements
    t1       <- head(ten_time, n=1)                                                  # first time step of 10 last measurements
    P        <- tail(sub$atmp, n=1)                                                  # ambient pressure at t10 (mb)
    Ta       <- tail(sub$soil_temp, n=1)                                             # air temp at t10 (deg C)
    Vd       <- 0.0012287                                                            # m3 (constant)
    A        <- 0.00950                                                              # m2 (constant)
    Ru       <- 8.31432                                                              # J mol-1 K-1 (constant)
    flux     <- ((C10 - C1)/(t10 - t1)) * (P/(Ta + 273.15))*(Vd/A)*((44.01*0.36)/Ru) # CO2 efflux (g CO2 m-2 h-1)
    flux2     <- Co2slope * (P/(Ta + 273.15))*(Vd/A)*((44.01*0.36)/Ru) # CO2 efflux (g CO2 m-2 h-1)
    xx       <- rbind(xx, id)
    yy       <- rbind(yy, flux)
  }
  Res <- data.frame(cbind(xx, yy))
  colnames(Res) <- c("codew", "flux")
  
# TO DO: have a look at how Khoon calculates this. 


# build the new data frame
  Restot <- sqldf("SELECT Res.*, MAX(raw_totsr.plot_code), MAX(raw_totsr.measurement_code), MAX(raw_totsr.egm_measurement), MAX(raw_totsr.day), MAX(raw_totsr.month), MAX(raw_totsr.year), MAX(raw_totsr.hour), MAX(raw_totsr.soil_temp), MAX(raw_totsr.vwc), MAX(raw_totsr.collar_depth) FROM Res JOIN raw_totsr ON Res.codew = raw_totsr.codew GROUP BY codew")
  colnames(Restot)  <- c("code", "flux_umolco2_m2_s", "plot_code", "measurement_code", "egm_measurement", "day", "month", "year", "hour", "soil_temp_degC", "vwc_percent", "collar_depth_cm") 
  
  
# save to current directory  
  setwd("C:/Users/Cecile/Documents/GitHub/GEMcarbon.R/example files/outputs")
  write.csv(Restot, file="Res_total_test1.csv")

    
## SOIL RESPIRATION CONTROL

  ## contol soil respiration 
# select a plot
  raw_consr <- subset(raw_consr, plot_code=="ACJ")
  wea_con   <- subset(data.frame(wea_con), plot_code=="ACJ")
  
# Add air temperature (temp), volumetric water content (vwc), and chamber depth (depth) to the raw control data (raw_consr)
  wea_con$code1     <- paste(wea_con$subplot, wea_con$collar_number, wea_con$day, wea_con$month, wea_con$year, sep=".") 
  raw_consr$code1   <- paste(raw_consr$subplot, wea_con$collar_number, raw_consr$day, raw_consr$month, raw_consr$year, sep=".")
  raw_consr         <- sqldf("SELECT raw_consr.*, wea_con.soil_temp, wea_con.vwc, wea_con.collar_depth FROM  raw_consr JOIN  wea_con  ON raw_consr.code1 = wea_con.code1")
# code1 for subplot.collar_measurement.day.month.year, but then use egm_measurement for codew
  raw_consr$codew   <- paste(raw_consr$egm_measurement, raw_consr$day, raw_consr$month, raw_consr$year, sep=".")
  
# Collar diameter (cm)
  raw_consr$collar_diam <- 12
  
  # Estimate missing pressure whith temperature-dependent version of the barometric equation (see soilrespiration_aux-functions)
  t <- mean(as.numeric(wea_con$temp), na.rm=T)
  w <- which(is.na(raw_consr$atmp))
  raw_consr$atmp[w] <- barometric_equation_T(elevation=0, temp=t )
  
  ## estimate flux for each measurement
  ## Sanity checks. Plot each flux batch to check data.
  
  #### TO DO: this only shows the last plot. What's wrong? 
  op <- par(ask=TRUE) # http://stackoverflow.com/questions/6031093/making-a-series-of-plots-that-proceed-by-a-click
  for (i in 1:length(raw_consr$codew)){
    aa <- subset(raw_consr, codew == codew[i], select = c(codew, co2ref, time))
    colnames(aa) <- c("codew", "co2", "time")
    plot(aa$co2, aa$time, main = paste("code:", head(aa$codew, 1)), xlab="time", ylab="CO2 (micro mol s-1? ppm?)")
    par(op)
  }
  
  
  ## Linear fit, estimate r2 quality check. ####   umea <- unique(raw_consr$codew)
  xx <- c()
  yy <- c()
  zz <- c()
  
  for (i in 1:length(umea)) {
    sub  <- subset(raw_consr, subset=(raw_consr$codew == umea[i]))
    fit  <- lm(sub$time ~ sub$co2ref) 
    r    <- summary(fit)$r.squared 
    p    <- anova(fit)$'Pr(>F)'[1] # or p <- summary(fit)$coefficients[,4]
    u    <- head(sub$codew, 1)
    xx   <- rbind(xx, r) 
    yy   <- rbind(yy, p)
    zz   <- rbind(zz, u)  
  }
  table <- data.frame(cbind(xx, yy, zz))
  colnames(table) <- c("r2", "pvalue", "unique_code") 
  
  # get unique identifyer for each measurement
  uid <- unique(raw_consr$codew)
  xx <- c()
  yy <- c()
  
  #for (i in uid) {
  for (i in 1:length(uid)) {
    sub      <- subset(raw_consr, subset=(raw_consr$codew == uid[i]))
    id       <- tail(sub$codew, n=1) 
    ten_co2  <- tail(sub$co2ref, n=10) 
    ten_time <- tail(sub$time, n=10)
    C10      <- tail(ten_co2, n=1)                                                   # last CO2 measurement of last 10 measurements
    C1       <- head(ten_co2, n=1)                                                   # first CO2 measurement of last 10 measurements
    t10      <- tail(ten_time, n=1)                                                  # last time step of 10 last measurements
    t1       <- head(ten_time, n=1)                                                  # first time step of 10 last measurements
    P        <- tail(sub$atmp, n=1)                                                  # ambient pressure at t10 (mb)
    Ta       <- tail(sub$soil_temp, n=1)                                                  # air temp at t10 (deg C)
    Vd       <- 0.0012287                                                            # m3 (constant)
    A        <- 0.00950                                                              # m2 (constant)
    Ru       <- 8.31432                                                              # J mol-1 K-1 (constant)
    flux     <- ((C10 - C1)/(t10 - t1)) * (P/(Ta + 273.15))*(Vd/A)*((44.01*0.36)/Ru) # CO2 efflux (g CO2 m-2 h-1)
    xx       <- rbind(xx, id)
    yy       <- rbind(yy, flux)
  }
  Res2 <- data.frame(cbind(xx, yy))
  colnames(Res2) <- c("codew", "flux")
  
  # TO DO: have a look at how Khoon calculates this. 
  
  # build the new data frame
  Rescon <- sqldf("SELECT Res2.*, MAX(raw_consr.plot_code), MAX(raw_consr.measurement_code), MAX(raw_consr.disturbance_code), MAX(raw_consr.egm_measurement), MAX(raw_consr.day), MAX(raw_consr.month), MAX(raw_consr.year), MAX(raw_consr.hour), MAX(raw_consr.soil_temp), MAX(raw_consr.vwc), MAX(raw_consr.collar_depth) FROM Res2 JOIN raw_consr ON Res2.codew = raw_consr.codew GROUP BY codew")
  colnames(Rescon)  <- c("code", "flux_umolco2_m2_s", "plot_code", "measurement_code", "disturbance_code", "egm_measurement", "day", "month", "year", "hour", "soil_temp_degC", "vwc_percent", "collar_depth_cm") 
    

  # save to current directory  
  setwd("C:/Users/Cecile/Documents/GitHub/GEMcarbon.R/example files/outputs")
  write.csv(Rescon, file="Res_control_test1.csv")
  
   
## SOIL RESPIRATION PARTITIONNING
  ## change treatment codes to the numbers we have in the database
  
  # Total respiration normal litterfall - CNH = 1
  # Total respiration without litterfall - CSH = 2
  # Total respiration double litterfall - C2H = 3
  # Mycorrizal respiration normal litterfall - MNH = 4
  # Mycorrizal respiration without litterfall - MSH = 5
  # Mycorrizal respiration double litterfall - M2H = 6
  # Soil matter respiration normal litterfall - SNH = 7
  # Soil matter respiration without litterfal - SSH = 8
  # Soil matter respiration double litterfall - S2H = 9
  # Mineral layer respiration - ML = 10
  
  raw_parsr$treatment_code <- mgsub(c("CNH","CSH","C2H","MNH","MSH","M2H","SNH","SSH","S2H","M"), c("1","2","3","4","5","6","7","8","9", "10"), raw_parsr$treatment_code)
  wea_par$treatment_code <- mgsub(c("CNH","CSH","C2H","MNH","MSH","M2H","SNH","SSH","S2H","M"), c("1","2","3","4","5","6","7","8","9", "10"), wea_par$treatment_code)
  
  # select a plot
  raw_parsr <- subset(raw_parsr, plot_code=="ACJ")
  wea_par <- subset(data.frame(wea_par), plot_code=="ACJ")
  
  # Add air temperature (temp), volumetric water content (vwc), and chamber depth (depth) to the raw control data (raw_consr)
  wea_par$code1     <- paste(wea_par$subplot, wea_par$treatment_code, wea_par$day, wea_par$month, wea_par$year, sep=".") # only use code 1 to merge wea_par and raw_parsr (subplot.day.month.year is not a unique identifier).
  raw_parsr$code1   <- paste(raw_parsr$subplot, raw_parsr$treatment_code, raw_parsr$day, raw_parsr$month, raw_parsr$year, sep=".")
  raw_parsr         <- sqldf("SELECT raw_parsr.*, wea_par.soil_temp, wea_par.vwc, wea_par.collar_depth FROM  raw_parsr JOIN  wea_par  ON raw_parsr.code1 = wea_par.code1")
 
  # code1 for subplot.day.month.year, but then use egm_measurement for codew
  raw_parsr$codew   <- paste(raw_parsr$egm_measurement, raw_parsr$day, raw_parsr$month, raw_parsr$year, sep=".")
   
  # Collar diameter (cm)
  #raw_parsr$collar_diam <- 12
  
  # Estimate missing pressure whith temperature-dependent version of the barometric equation (see soilrespiration_aux-functions)
  t <- mean(as.numeric(wea_par$temp), na.rm=T)
  w <- which(is.na(raw_parsr$atmp))
  raw_parsr$atmp[w] <- barometric_equation_T(elevation=0, temp=t )
  
  ## estimate flux for each measurement
  ## Sanity checks. Plot each flux batch to check data.
  
  #### TO DO: this only shows the last plot. What's wrong? 
  op <- par(ask=TRUE) # http://stackoverflow.com/questions/6031093/making-a-series-of-plots-that-proceed-by-a-click
  for (i in 1:length(raw_parsr$codew)){
    aa <- subset(raw_parsr, codew == codew[i], select = c(codew, co2ref, time))
    colnames(aa) <- c("codew", "co2", "time")
    plot(aa$co2, aa$time, main = paste("code:", head(aa$codew, 1)), xlab="time", ylab="CO2 (micro mol s-1? ppm?)")
    par(op)
  }
  
  
  ## Linear fit, estimate r2 quality check. 
  umea <- unique(raw_parsr$codew)
  xx <- c()
  yy <- c()
  zz <- c()
  
  for (i in 1:length(umea)) {
    sub  <- subset(raw_parsr, subset=(raw_parsr$codew == umea[i]))
    fit  <- lm(sub$time ~ sub$co2ref) 
    r    <- summary(fit)$r.squared 
    p    <- anova(fit)$'Pr(>F)'[1] # or p <- summary(fit)$coefficients[,4]
    u    <- head(sub$codew, 1)
    xx   <- rbind(xx, r) 
    yy   <- rbind(yy, p)
    zz   <- rbind(zz, u)  
  }
  table <- data.frame(cbind(xx, yy, zz))
  colnames(table) <- c("r2", "pvalue", "unique_code") 
  
  # get unique identifyer for each measurement
  uid <- unique(raw_parsr$codew)
  xx <- c()
  yy <- c()
  
  #for (i in uid) {
  for (i in 1:length(uid)) {
    sub      <- subset(raw_parsr, subset=(raw_parsr$codew == uid[i]))
    id       <- tail(sub$codew, n=1) 
    ten_co2  <- tail(sub$co2ref, n=10) 
    ten_time <- tail(sub$time, n=10)
    C10      <- tail(ten_co2, n=1)                                                   # last CO2 measurement of last 10 measurements
    C1       <- head(ten_co2, n=1)                                                   # first CO2 measurement of last 10 measurements
    t10      <- tail(ten_time, n=1)                                                  # last time step of 10 last measurements
    t1       <- head(ten_time, n=1)                                                  # first time step of 10 last measurements
    P        <- tail(sub$atmp, n=1)                                                  # ambient pressure at t10 (mb)
    Ta       <- tail(sub$soil_temp, n=1)                                                  # air temp at t10 (deg C)
    Vd       <- 0.0012287                                                            # m3 (constant)
    A        <- 0.00950                                                              # m2 (constant)
    Ru       <- 8.31432                                                              # J mol-1 K-1 (constant)
    flux     <- ((C10 - C1)/(t10 - t1)) * (P/(Ta + 273.15))*(Vd/A)*((44.01*0.36)/Ru) # CO2 efflux (g CO2 m-2 h-1)
    xx       <- rbind(xx, id)
    yy       <- rbind(yy, flux)
  }
  Res <- data.frame(cbind(xx, yy))
  colnames(Res) <- c("codew", "flux")
  
  # TO DO: have a look at how Khoon calculates this.

# build the new data frame
  Respar <- sqldf("SELECT Res.*, MAX(raw_parsr.plot_code), MAX(raw_parsr.measurement_code), MAX(raw_parsr.treatment_code), MAX(raw_parsr.egm_measurement), MAX(raw_parsr.day), MAX(raw_parsr.month), MAX(raw_parsr.year), MAX(raw_parsr.hour), MAX(raw_parsr.soil_temp), MAX(raw_parsr.vwc), MAX(raw_parsr.collar_depth) FROM Res JOIN raw_parsr ON Res.codew = raw_parsr.codew GROUP BY codew")
  colnames(Respar)  <- c("code", "flux_umolco2_m2_s", "plot_code", "measurement_code", "treatment_code", "egm_measurement", "day", "month", "year", "hour", "soil_temp_degC", "vwc_percent", "collar_depth_cm") 
  
# save to current directory  
  setwd("C:/Users/Cecile/Documents/GitHub/GEMcarbon.R/example files/outputs")
  write.csv(Respar, file="Res_partitionning_test1.csv") 
  
  ## style guide on how to lay out R code: http://google-styleguide.googlecode.com/svn/trunk/Rguide.xml#indentation 