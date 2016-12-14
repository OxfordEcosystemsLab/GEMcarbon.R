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
  require(ggplot2)
  
# read in soil respiration auxillary functions from GitHub
  setwd("~/Github/GEMcarbon.R")
  source("~/Github/GEMcarbon.R/soilrespiration_auxfunctions.r")

# read in data 
  raw_consrA   <- read.table("~/Desktop/data_sorting/eltr_rsoil_control_dec16.csv", sep=",", header=T)
  raw_parsrA   <- read.table("~/Desktop/data_sorting/eltr_rsoil_part_dec16.csv", sep=",", header=T) 
  raw_totsrA   <- read.table("~/Desktop/data_sorting/eltr_rsoil_total_dec16.csv", sep=",", header=T)
  weather      <- read.table("~/Desktop/data_sorting/eltr_rsoil_weather_dec16.csv", sep=",", header=T) 
  

## TOTAL SOIL RESPIRATION
  #ACJ-01 ESP-01 PAN-02 PAN-03 SPD-01 SPD-02 TAM-03 TAM-04 TAM-05 TAM-06 TAM-09 TRU-04 WAY-01
  # TAM-03 = TAM-05
  # TAM-04 = TAM-06
  # TAM-09 = TAM-09
  
# select a plot
  raw_totsrA            <- subset(raw_totsrA, plot_code=="WAY-01")
  raw_totsrA$collar_num <- raw_totsrA$sub_plot                       ## Attention!! We don't always need this.
  wea_tot               <- subset(weather, plot_code=="WAY-01" | measurement_code=="TOTAL")
  
# Add air temperature (temp), volumetric water content (vwc), and chamber depth (depth) to the raw control data (raw_consr)
  wea_tot$code1     <- paste(wea_tot$collar_num, wea_tot$replica, wea_tot$month, wea_tot$year, sep="_") # wea_tot$day, wea_tot$month, wea_tot$year, sep=".") # only use code 1 to merge wea_tot and raw_totsr (subplot.day.month.year is not a unique identifier).
  raw_totsrA$code1  <- paste(raw_totsrA$collar_num, raw_totsrA$replica, raw_totsrA$month, raw_totsrA$year, sep="_") # raw_totsrA$day, raw_totsrA$month, raw_totsrA$year, sep=".")
  
  wea_tot_avg       <- sqldf("SELECT AVG(wea_tot.vwc_percent_in), AVG(wea_tot.vwc_percent_out), AVG(wea_tot.soil_temp_c_in), AVG(wea_tot.soil_temp_c_out), AVG(wea_tot.air_temp_c), AVG(wea_tot.collar_height_cm), wea_tot.code1 FROM wea_tot GROUP BY code1")
  colnames(wea_tot_avg) <- c("vwc_percent_in","vwc_percent_out","soil_temp_c_in", "soil_temp_c_out", "air_temp_c", "collar_height_cm", "code1")
  test1             <- sqldf("select wea_tot_avg.* from wea_tot_avg where wea_tot_avg.code1 = '9_2_4_2011'")
  
  raw_totsrB        <- sqldf("SELECT raw_totsrA.*, wea_tot_avg.vwc_percent_in, wea_tot_avg.vwc_percent_out, wea_tot_avg.soil_temp_c_in, wea_tot_avg.soil_temp_c_out, wea_tot_avg.air_temp_c, wea_tot_avg.collar_height_cm FROM raw_totsrA LEFT JOIN wea_tot_avg ON raw_totsrA.code1 = wea_tot_avg.code1")
  test2             <- sqldf("select raw_totsrB.* from raw_totsrB where raw_totsrB.code1 = '9_2_4_2011'")
  
  # Replace missing collar height data with the average collar height of that collar. Above, we are grouping by collar number, replica and date. Here, we get the average over all previous measurements.
  collar_h           <- sqldf("SELECT wea_tot.collar_number, AVG(wea_tot.collar_height_cm) FROM  wea_tot GROUP BY collar_number")
  colnames(collar_h) <- c("collar_number","avg_collar_height")
  raw_totsr          <- sqldf("SELECT raw_totsrB.*, collar_h.avg_collar_height FROM raw_totsrB LEFT JOIN collar_h ON raw_totsrB.collar_number = collar_h.collar_number")

  w <- which(is.na(raw_totsr$collar_height_cm))
  raw_totsr$collar_height_cm[w] <- raw_totsr$avg_collar_height
  #  TO DO: check this warning: number of items to replace is not a multiple of replacement length
  
  # code1 for collar_num.day.month.year, but then use collar_num and replica for codew
  # too many NAs in egm_measurement: raw_totsr$codew   <- paste(raw_totsr$egm_measurement, raw_totsr$day, raw_totsr$month, raw_totsr$year, sep=".")
  raw_totsr$codew   <- paste(raw_totsr$collar_num, raw_totsr$replica, raw_totsr$day, raw_totsr$month, raw_totsr$year, sep=".")
  
# Collar diameter (cm)
  raw_totsr$collar_diam <- 12
  
# Estimate missing pressure whith temperature-dependent version of the barometric equation (see soilrespiration_aux-functions)
  t <- mean(as.numeric(wea_tot$air_temp_c), na.rm=T) 
  w <- which(is.na(raw_totsr$atmp_mb))
  raw_totsr$atmp_mb[w] <- barometric_equation_T(elevation=0, temp=t )
  
# Estimate missing temperature as average temperature for the plot
  w <- which(is.na(raw_totsr$air_temp_c))
  raw_totsr$air_temp_c[w] <- t
         
## estimate flux for each measurement
  ## Sanity checks. Plot each flux batch to check data.
 
  #### TO DO: this only shows the last plot. What's wrong? 
  #op <- par(ask=TRUE) # http://stackoverflow.com/questions/6031093/making-a-series-of-plots-that-proceed-by-a-click
  #for (i in 1:length(raw_totsr$codew)){
  #  aa <- subset(raw_totsr, codew == codew[i], select = c(codew, co2ref, time))
  #  colnames(aa) <- c("codew", "co2", "time")
  #  plot(aa$co2, aa$time, main = paste("code:", head(aa$codew, 1)), xlab="time", ylab="CO2 (micro mol s-1? ppm?)")
  #  par(op)
  #}

  ## Linear fit, estimate r2 quality check. 
  #umea <- unique(raw_totsr$codew)
  #xx <- c()
  #yy <- c()
  #zz <- c()
  
  #for (i in 1:length(umea)) {
  #  sub  <- subset(raw_totsr, subset=(raw_totsr$codew == umea[i]))
  #  fit  <- lm(sub$time ~ sub$co2ref) 
  #  r    <- summary(fit)$r.squared 
  #  p    <- anova(fit)$'Pr(>F)'[1] # or p <- summary(fit)$coefficients[,4]
  #  u    <- head(sub$codew, 1)
  #  xx   <- rbind(xx, r) 
  #  yy   <- rbind(yy, p)
  #  zz   <- rbind(zz, u)  
  #}
  #table <- data.frame(cbind(xx, yy, zz))
  #colnames(table) <- c("r2", "pvalue", "unique_code") # note: small r2 doesn't mean bad flux - small flux = small r2
  
# get unique identifyer for each measurement
  uid <- unique(raw_totsr$codew)
  xx <- c()
  yy <- c()
  
  for (i in 1:length(uid)) {
    sub      <- subset(raw_totsr, subset=(raw_totsr$codew == uid[i])) 
    id       <- tail(sub$codew, n=1) 
    ten_co2  <- tail(sub$co2ref_ppm_sec, n=10)   # Drop first two values rather than only keep last 10 values.
    ten_time <- tail(sub$time, n=10)       
    fit      <- lm(ten_co2~ten_time)
    Co2slope <- fit$coefficients[2]      # USE SLOPE RATHER THAN DIFFERENCE OF C10-C1
    C10      <- tail(ten_co2, n=1)                                                   # last CO2 measurement of last 10 measurements
    C1       <- head(ten_co2, n=1)                                                   # first CO2 measurement of last 10 measurements
    t10      <- tail(ten_time, n=1)                                                  # last time step of 10 last measurements
    t1       <- head(ten_time, n=1)                                                  # first time step of 10 last measurements
    P        <- tail(sub$atmp, n=1)                                                  # ambient pressure at t10 (mb)
    Ta       <- tail(sub$air_temp_c, n=1)                                            # air temp at t10 (deg C)
    Vd       <- 0.0012287                                                            # m3 (constant)
    A        <- 0.00950                                                              # m2 (constant)
    Ru       <- 8.31432                                                              # J mol-1 K-1 (constant)
    flux     <- ((C10 - C1)/(t10 - t1)) * (P/(Ta + 273.15))*(Vd/A)*((44.01*0.36)/Ru) # CO2 efflux (g CO2 m-2 h-1)
    flux2    <- Co2slope * (P/(Ta + 273.15))*(Vd/A)*((44.01*0.36)/Ru)                # CO2 efflux (g CO2 m-2 h-1)
    xx       <- rbind(xx, id)
    yy       <- rbind(yy, flux)
  }
  rownames(xx) <- NULL
  rownames(yy) <- NULL
  
  Res <- data.frame(cbind(xx, yy))
  colnames(Res) <- c("codew", "flux")
  
# TO DO: have a look at how Khoon calculates this. 
  
# Check for duplicates
  any(duplicated(Res$codew))
  Res$codew[which(duplicated(Res$codew))]
  
# build the new data frame
  
  Restot       <- sqldf("SELECT Res.*, raw_totsr.* FROM Res LEFT JOIN raw_totsr ON Res.codew = raw_totsr.codew GROUP BY Res.codew")
  Restot$X     <- NULL
  Restot$atmp  <- NULL 
  Restot$code1 <- NULL
  
# save to current directory  
  setwd("~/Desktop/data_sorting/Rflux")
  write.csv(Restot, file="flux_total_WAY-01_09to14.csv")
  
    
## SOIL RESPIRATION CONTROL
  raw_consrA   <- read.table("~/Desktop/data_sorting/eltr_rsoil_control_dec16.csv", sep=",", header=T)

  # ACJ-01 ESP-01 PAN-02 PAN-03 SPD-01 SPD-02 TAM-05 TAM-06 TAM-09 TRU-04 WAY-01
  
  ## contol soil respiration 
# select a plot
  raw_consrAA  <- subset(raw_consrA, plot_code == "WAY-01") 
  raw_consrBB  <- subset(raw_consrAA, measurement_code=="CTRL")
  X            <- subset(weather, plot_code == "WAY-01")
  wea_con      <- subset(X, measurement_code=="CTRL")
  
# Define unique identifiers
  wea_con$code1      <- paste(wea_con$collar_number, wea_con$month, wea_con$year, sep="_") 
  raw_consrBB$code1   <- paste(raw_consrBB$collar_number, raw_consrBB$month, raw_consrBB$year, sep="_")
  raw_consrBB$codew   <- paste(raw_consrBB$collar_num, raw_consrBB$replica, raw_consrBB$day, raw_consrBB$month, raw_consrBB$year, sep="_")
  
# Add air temperature (temp), volumetric water content (vwc), and chamber depth (depth) to the raw control data (raw_consr)
  
  wea_con_avg           <- sqldf("SELECT AVG(wea_con.vwc_percent_in), AVG(wea_con.vwc_percent_out), AVG(wea_con.soil_temp_c_in), AVG(wea_con.soil_temp_c_out), AVG(wea_con.air_temp_c), AVG(wea_con.collar_height_cm), wea_con.code1 FROM wea_con GROUP BY code1")
  colnames(wea_con_avg) <- c("vwc_percent_in","vwc_percent_out","soil_temp_c_in", "soil_temp_c_out", "air_temp_c", "collar_height_cm", "code1")
  test1                 <- sqldf("select wea_con_avg.* from wea_con_avg where wea_con_avg.code1 = '10_1_2014'")

  raw_consrB        <- sqldf("SELECT raw_consrBB.*, wea_con_avg.vwc_percent_in, wea_con_avg.vwc_percent_out, wea_con_avg.soil_temp_c_in, wea_con_avg.soil_temp_c_out, wea_con_avg.air_temp_c, wea_con_avg.collar_height_cm FROM raw_consrBB LEFT JOIN wea_con_avg ON raw_consrBB.code1 = wea_con_avg.code1")
  test2             <- sqldf("select raw_consrB.* from raw_consrB where raw_consrB.code1 = '10_1_2014'")
  
  
  # Replace missing collar height data with the average collar height of that collar. Above, we are grouping by collar number, replica and date. Here, we get the average over all previous measurements.
  collar_h            <- sqldf("SELECT wea_con.collar_number, AVG(wea_con.collar_height_cm) FROM wea_con GROUP BY collar_number")
  colnames(collar_h)  <- c("collar_number","avg_collar_height")
  raw_consrC          <- merge(raw_consrB, collar_h,  by.x = "collar_number", by.y = "collar_number", all.x=T)
  test3               <- sqldf("select raw_consrC.* from raw_consrC where raw_consrC.code1 = '10_1_2014'")

  w <- which(is.na(raw_consrC$collar_height_cm))
  raw_consrC$collar_height_cm[w] <- raw_consrC$avg_collar_height[w]
  
  # code1 for subplot.collar_measurement.day.month.year, but then use egm_measurement for codew
  
  
  # Collar diameter (cm)
  raw_consrC$collar_diam <- 12
  
  # Estimate missing pressure whith temperature-dependent version of the barometric equation (see soilrespiration_aux-functions)
  t <- mean(as.numeric(wea_con$air_temp_c), na.rm=T) 
  w <- which(is.na(raw_consrC$atmp_mb))
  raw_consrC$atmp_mb[w] <- barometric_equation_T(elevation=0, temp=t )
  
  # Estimate missing temperature as average temperature for the plot
  w <- which(is.na(raw_consrC$air_temp_c))
  raw_consrC$air_temp_c[w] <- t
    
  ## estimate flux for each measurement
  ## Sanity checks. Plot each flux batch to check data.
  
  #### TO DO: this only shows the last plot. What's wrong? 
  #op <- par(ask=TRUE) # http://stackoverflow.com/questions/6031093/making-a-series-of-plots-that-proceed-by-a-click
  #for (i in 1:length(raw_consr$codew)){
  #  aa <- subset(raw_consr, codew == codew[i], select = c(codew, co2ref, time))
  #  colnames(aa) <- c("codew", "co2", "time")
  #  plot(aa$co2, aa$time, main = paste("code:", head(aa$codew, 1)), xlab="time", ylab="CO2 (micro mol s-1? ppm?)")
  #  par(op)
  #}
  
  
  ## Linear fit, estimate r2 quality check. ####   umea <- unique(raw_consr$codew)
  #xx <- c()
  #yy <- c()
  #zz <- c()
  
  #for (i in 1:length(umea)) {
  #  sub  <- subset(raw_consr, subset=(raw_consr$codew == umea[i]))
  #  fit  <- lm(sub$time ~ sub$co2ref) 
  #  r    <- summary(fit)$r.squared 
  #  p    <- anova(fit)$'Pr(>F)'[1] # or p <- summary(fit)$coefficients[,4]
  #  u    <- head(sub$codew, 1)
  #  xx   <- rbind(xx, r) 
  #  yy   <- rbind(yy, p)
  #  zz   <- rbind(zz, u)  
  #}
  #table <- data.frame(cbind(xx, yy, zz))
  #colnames(table) <- c("r2", "pvalue", "unique_code") 
  
  # Order by codew (unique identifyer for each measurement)
  raw_consr <- sqldf("SELECT * FROM raw_consrC ORDER BY raw_consrC.codew")
  
  # get unique identifyer for each measurement
  uid <- unique(raw_consr$codew)
  xx <- c()
  yy <- c()
  
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
    Ta       <- tail(sub$air_temp_c, n=1)                                            # air temp at t10 (deg C)
    Vd       <- 0.0012287                                                            # m3 (constant)
    A        <- 0.00950                                                              # m2 (constant)
    Ru       <- 8.31432                                                              # J mol-1 K-1 (constant)
    flux     <- ((C10 - C1)/(t10 - t1)) * (P/(Ta + 273.15))*(Vd/A)*((44.01*0.36)/Ru) # CO2 efflux (g CO2 m-2 h-1)
    xx       <- rbind(xx, id)
    yy       <- rbind(yy, flux)
  }
  
  rownames(xx) <- NULL
  rownames(yy) <- NULL
  
  Res2 <- data.frame(cbind(xx, yy))
  colnames(Res2) <- c("codew", "flux")
  
  ResCON         <- sqldf("SELECT Res2.*, MAX(raw_consr.plot_code), MAX(raw_consr.sub_plot), MAX(raw_consr.plot_corner_code),
                          MAX(raw_consr.collar_number), MAX(raw_consr.replica), MAX(raw_consr.egm_measurement), MAX(raw_consr.disturbance_code_control), MAX(raw_consr.litter_code), 
                          MAX(raw_consr.year), MAX(raw_consr.day), MAX(raw_consr.month), MAX(raw_consr.hour), MAX(raw_consr.atmp_mb), MAX(raw_consr.quality_code), 
                          MAX(raw_consr.vwc_percent_in), MAX(raw_consr.vwc_percent_out), MAX(raw_consr.soil_temp_c_in), MAX(raw_consr.soil_temp_c_out), MAX(raw_consr.air_temp_c), MAX(raw_consr.collar_height_cm), MAX(raw_consr.collar_diam)
                          FROM Res2 LEFT JOIN raw_consr ON Res2.codew = raw_consr.codew GROUP BY Res2.codew")
  
  colnames(ResCON) <- c("codew", "flux", "plot_code", "raw_consr.sub_plot", "raw_consr.plot_corner_code",
                        "collar_number", "replica", "egm_measurement", "disturbance_code_control", "litter_code", 
                        "year", "day", "month", "hour", "atmp_mb", "quality_code", 
                        "vwc_percent_in", "vwc_percent_out", "soil_temp_c_in", "soil_temp_c_out", "air_temp_c", "collar_height_cm", "collar_diam")

  # Check for duplicates
  any(duplicated(ResCON$codew))
  ResCON$codew[which(duplicated(ResCON$codew))]
  
  plot <- ggplot(ResCON) + geom_point(aes(x=year, y=flux, colour=factor(collar_number)))
  plot 
  
  # save to current directory 
  setwd("~/Desktop/data_sorting/Rflux")
  write.csv(ResCON, file="flux_control_WAY01_09to14.csv")
  
   
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