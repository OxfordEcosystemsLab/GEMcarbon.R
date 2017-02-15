## Code to go from EGM-4 raw data files to the csv files you need to run the code soil_respiration_2015.r.
# Cecile Girardin 28.02.2014
# Last edited: Cecile Girardin 15.01.2017


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
  require(plyr)
  
# read in soil respiration auxillary functions from GitHub
  setwd("~/Github/GEMcarbon.R")
  source("~/Github/GEMcarbon.R/soilrespiration_auxfunctions.r")

# read in data 
  raw_consrA   <- read.table("~/Desktop/data_sorting/rsoil/eltr_rsoil_control_dec16.csv", sep=",", header=T)
  raw_parsrA   <- read.table("~/Desktop/data_sorting/rsoil/eltr_rsoil_part_dec16.csv", sep=",", header=T) 
  raw_totsrA   <- read.table("~/Github/gemcarbon_data/raw_data_ingembd/eltr_rsoil_total_feb17.csv", sep=",", header=T)
  weather      <- read.table("~/Github/gemcarbon_data/raw_data_ingembd/eltr_rsoil_weather_feb17.csv", sep=",", header=T) 
  weather$air_temp_c <- as.numeric(as.character(weather$air_temp_c)) 
  # Replace missing collar height data 
  # with the previous or subsequent collar height, or an average of the two if they are both provided.
  weather$ch_new <- fill.na(weather$collar_height_cm)
  
## TOTAL SOIL RESPIRATION
  
  
  ##########################
  #########################
  ######################## THERE IS A PROBLEM WITH THE FILL.NA!!!!!!!!!!!!
  ######### Notes to me: this is working fine, you can run all the datasets to estimate TOTAL soil respiration. Still a lot of NAs, could it be an issue with the collar height?
  
  # unique(raw_totsrA$plot_code)
  # SPD-02 SPD-01 ESP-01 WAY-01 ACJ-01 PAN-02 PAN-03 TRU-04 TAM-05 TAM-06 TAM-09
    
# select a plot
  raw_totsrA            <- subset(raw_totsrA, plot_code=="SPD-01")
  raw_totsrA$collar_num <- raw_totsrA$sub_plot                                              #### Attention!! We don't always need this. ####
  wea_tot               <- subset(weather, plot_code=="SPD-01" | measurement_code=="TOTAL")
  
# Add air temperature (temp), volumetric water content (vwc), and chamber depth (depth) to the raw control data (raw_consr)
  wea_tot$code1     <- paste(wea_tot$collar_num, wea_tot$replica, wea_tot$month, wea_tot$year, sep="_") # wea_tot$day, wea_tot$month, wea_tot$year, sep=".") # only use code 1 to merge wea_tot and raw_totsr (subplot.day.month.year is not a unique identifier).
  raw_totsrA$code1  <- paste(raw_totsrA$collar_num, raw_totsrA$replica, raw_totsrA$month, raw_totsrA$year, sep="_") # raw_totsrA$day, raw_totsrA$month, raw_totsrA$year, sep=".")
  
  wea_tot_avg       <- sqldf("SELECT AVG(wea_tot.vwc_percent_in), AVG(wea_tot.vwc_percent_out), AVG(wea_tot.soil_temp_c_in), AVG(wea_tot.soil_temp_c_out), AVG(wea_tot.air_temp_c), AVG(wea_tot.ch_new), wea_tot.code1 FROM wea_tot GROUP BY code1")
  colnames(wea_tot_avg) <- c("vwc_percent_in","vwc_percent_out","soil_temp_c_in", "soil_temp_c_out", "air_temp_c", "ch_gapfilled", "code1")
  test1             <- sqldf("select wea_tot_avg.* from wea_tot_avg where wea_tot_avg.code1 = '9_2_4_2011'")
  test1
  raw_totsr         <- sqldf("SELECT raw_totsrA.*, wea_tot_avg.vwc_percent_in, wea_tot_avg.vwc_percent_out, wea_tot_avg.soil_temp_c_in, wea_tot_avg.soil_temp_c_out, wea_tot_avg.air_temp_c, wea_tot_avg.ch_gapfilled FROM raw_totsrA LEFT JOIN wea_tot_avg ON raw_totsrA.code1 = wea_tot_avg.code1")
  test2             <- sqldf("select raw_totsr.* from raw_totsr where raw_totsr.code1 = '9_2_4_2011'")
  test2
  
  
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
  # Replace missing air temp with soil temp.
  raw_totsr$air_temp_c[w] <- raw_totsr$soil_temp_c_out[w]
  # If data are still missing, replace with average air temp. #ATTENTION!! THIS IS A HACK! WHAT SHOULD WE DO WHEN WE DON"T HAVE AIR TEMP?
  w <- which(is.na(raw_totsr$air_temp_c))
  raw_totsr$air_temp_c[w] <- t
  raw_totsr$air_temp_c <- as.numeric(as.character(raw_totsr$air_temp_c)) 
  
  maat <- sqldf("SELECT raw_totsr.plot_code, AVG(raw_totsr.air_temp_c) FROM raw_totsr GROUP BY plot_code")

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

  ## estimate flux for each measurement

# get unique identifyer for each measurement
  uid <- unique(raw_totsr$codew)
  xx <- c()
  yy <- c()
  
  for (i in 1:length(uid)) {
    sub      <- subset(raw_totsr, subset=(raw_totsr$codew == uid[i])) 
    id       <- tail(sub$codew, n=1) 
    ten_co2  <- tail(sub$co2ref_ppm_sec, n=10)                                       # Drop first two values rather than only keep last 10 values.
    ten_time <- tail(sub$time, n=10)       
    #fit      <- lm(ten_co2~ten_time)
    #Co2slope <- fit$coefficients[2]                                                 # USE SLOPE RATHER THAN DIFFERENCE OF C10-C1
    C10      <- tail(ten_co2, n=1)                                                   # last CO2 measurement of last 10 measurements
    C1       <- head(ten_co2, n=1)                                                   # first CO2 measurement of last 10 measurements
    t10      <- tail(ten_time, n=1)                                                  # last time step of 10 last measurements
    t1       <- head(ten_time, n=1)                                                  # first time step of 10 last measurements
    P        <- tail(sub$atmp, n=1)                                                  # ambient pressure at t10 (mb)
    Ta       <- tail(sub$air_temp_c, n=1)                                            # air temp at t10 (deg C)
    ch       <- tail(sub$ch_gapfilled, n=1)                                          # see gap filling function fill.na() in soilrespiration_auxfinctions.r
    Vd       <- 0.0012287                                                            # m3 (constant)
    A        <- 0.00950                                                              # m2 (constant)
    Ru       <- 8.31432                                                              # J mol-1 K-1 (constant)
    Va       <- A*(ch/100)                                             # additional volume m3
    fl       <- ((C10 - C1)/(t10 - t1)) * (P/(Ta + 273.15))*(Vd/A)*((44.01*0.36)/Ru) # CO2 efflux (g CO2 m-2 h-1)
    #flux2    <- Co2slope * (P/(Ta + 273.15))*(Vd/A)*((44.01*0.36)/Ru)               # CO2 efflux (g CO2 m-2 h-1)
    flux     <- (fl*A/Vd*(Va+Vd)/A)*6.312                                            # Convert to umol m-2 s-1. Correct for collar height.
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
  setwd("~/Github/gemcarbon_data/raw_data_ingembd")
  write.csv(Restot, file="flux_total_SPD01.csv")
  
    
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
  
  
  ################################### 
  # ATTENTION! This should be previous collar height. & we do this in the next step. ditch these lines. 
  
  # Replace missing collar height data with the average collar height of that collar. Above, we are grouping by collar number, replica and date. Here, we get the average over all previous measurements.
  collar_h            <- sqldf("SELECT wea_con.collar_number, AVG(wea_con.collar_height_cm) FROM wea_con GROUP BY collar_number")
  colnames(collar_h)  <- c("collar_number","avg_collar_height")
  
  raw_consrC          <- merge(raw_consrB, collar_h,  by.x = "collar_number", by.y = "collar_number", all.x=T)
  test3               <- sqldf("select raw_consrC.* from raw_consrC where raw_consrC.code1 = '10_1_2014'")

  w <- which(is.na(raw_consrC$collar_height_cm))
  raw_consrC$collar_height_cm[w] <- raw_consrC$avg_collar_height[w]
  
  ################################### 
  
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
   
    ############################# Chamber volume correction.
    #############################
    
    ## Perform chamber and flux correction (Metcalfe 2009) 
    # chamber volume correction according to Metcalfe et al (2009): Rainfor Manual Appendix II, page 75
    # flux correction function is based on Appendix 2, RAINFOR manual
    # see p. 75, RAINFOR manual
    
    fluxcorr <- function(flux, temp, ch, Vd, A, pressure) {
      Va = A*(ch/100)        # additional volume m3
      # initialize Variables for the for-loop: (variables are the flux variables and to specify plotname)
      RucA <- numeric(length=length(temp))
      RcA <- numeric(length=length(temp))
      # correct for the new tube volume
      for (i in 1:length(temp)) {
        RucA[i]=(flux[i])*(pressure/1000)*(273/(temp[i]+273))*(44.01/22.41)*(Vd/A)/1000*3600
        RcA[i]= (RucA[i]*A/Vd*(Va[i]+Vd)/A)*6.312  # convert to umol m-2 s-1 !!!!!!!!!!!!!!!!!!!!!!!!!!! Make sure our result is in this unit
      }
      return(RcA)
    }
    
    #############################
    #############################
    
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
  
   
  # SOIL RESPIRATION PARTITIONNING
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
  
  raw_parsrA$treatment_code <- mapvalues(raw_parsrA$treatment_code, from=c("SSN","M2C","ml"), to=c("so_no_lit","my_doub_lit","ml_nor_lit"))
  weather$treatment_code <- mapvalues(weather$treatment_code, from=c("mlayer_no_lit"), to=c("ml_no_lit"))
  
  # select a plot
  # TAM-03 TAM-04 TAM-05 TAM-06 TAM-09 
  raw_parsrA <- subset(raw_parsrA, plot_code=="TAM-09")
  wea_part   <- subset(weather, plot_code=="TAM-09" | measurement_code=="PART")
  
  # Add air temperature (temp), volumetric water content (vwc), and chamber depth (depth) to the raw control data (raw_consr)
  wea_part$code1     <- paste(wea_part$sub_plot, wea_part$treatment_code_partitioning, wea_part$day, wea_part$month, wea_part$year, sep=".") # only use code 1 to merge weather and raw_parsr (subplot.day.month.year is not a unique identifier).
  raw_parsrA$code1  <- paste(raw_parsrA$sub_plot, raw_parsrA$treatment_code_partitioning, raw_parsrA$day, raw_parsrA$month, raw_parsrA$year, sep=".")
    
  wea_part_avg       <- sqldf("SELECT AVG(wea_part.vwc_percent_in), AVG(wea_part.vwc_percent_out), AVG(wea_part.soil_temp_c_in), AVG(wea_part.soil_temp_c_out), AVG(wea_part.air_temp_c), AVG(wea_part.collar_height_cm), wea_part.code1 FROM wea_part GROUP BY code1")
  colnames(wea_part_avg) <- c("vwc_percent_in","vwc_percent_out","soil_temp_c_in", "soil_temp_c_out", "air_temp_c", "collar_height_cm", "code1")
  test1             <- sqldf("select wea_part_avg.* from wea_part_avg where wea_part_avg.code1 = '4.my_doub_lit.27.8.2013'")
  
  raw_parsr        <- sqldf("SELECT raw_parsrA.*, wea_part_avg.vwc_percent_in, wea_part_avg.vwc_percent_out, wea_part_avg.soil_temp_c_in, wea_part_avg.soil_temp_c_out, wea_part_avg.air_temp_c, wea_part_avg.collar_height_cm FROM raw_parsrA LEFT JOIN wea_part_avg ON raw_parsrA.code1 = wea_part_avg.code1")
  test2             <- sqldf("select raw_parsr.* from raw_parsr where raw_parsr.code1 = '4.my_doub_lit.27.8.2013'")
  
  # code1 for subplot.day.month.year, but then use egm_measurement for codew
  raw_parsr$codew   <- paste(raw_parsr$egm_measurement, raw_parsr$day, raw_parsr$month, raw_parsr$year, sep=".")
   
  # Collar diameter (cm)
  #raw_parsr$collar_diam <- 12
  
  # Estimate missing pressure whith temperature-dependent version of the barometric equation (see soilrespiration_aux-functions)
  t <- mean(as.numeric(weather$soil_temp_c_out), na.rm=T) # most of our data is soil temp here. We should use air temp, but it is not available.
  w <- which(is.na(raw_parsr$atmp))
  raw_parsr$atmp[w] <- barometric_equation_T(elevation=0, temp=t )
  
  # Estimate missing temperature as average temperature for the plot
  w <- which(is.na(raw_parsr$soil_temp_c_out))
  raw_parsr$soil_temp_c_out[w] <- t
  
  
  ## Estimate flux for each measurement
  
  ## Sanity checks. Plot each flux batch to check data.
  #### TO DO: this only shows the last plot. What's wrong? 
  #op <- par(ask=TRUE) # http://stackoverflow.com/questions/6031093/making-a-series-of-plots-that-proceed-by-a-click
  #for (i in 1:length(raw_parsr$codew)){
  #  aa <- subset(raw_parsr, codew == codew[i], select = c(codew, co2ref, time))
  #  colnames(aa) <- c("codew", "co2", "time")
  #  plot(aa$co2, aa$time, main = paste("code:", head(aa$codew, 1)), xlab="time", ylab="CO2 (micro mol s-1? ppm?)")
  #  par(op)
  #}
  
  
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
  table
  
  # get unique identifyer for each measurement
  uid <- unique(raw_parsr$codew)
  xx <- c()
  yy <- c()
  
  #for (i in uid) {
  for (i in 1:length(uid)) {
    sub      <- subset(raw_parsr, subset=(raw_parsr$codew == uid[i]))
    id       <- tail(sub$codew, n=1) 
    ten_co2  <- tail(sub$co2ref, n=10)                                               # this should be more than last 10 measurements
    ten_time <- tail(sub$time, n=10)
    C10      <- tail(ten_co2, n=1)                                                   # last CO2 measurement of last 10 measurements
    C1       <- head(ten_co2, n=1)                                                   # first CO2 measurement of last 10 measurements
    t10      <- tail(ten_time, n=1)                                                  # last time step of 10 last measurements
    t1       <- head(ten_time, n=1)                                                  # first time step of 10 last measurements
    P        <- tail(sub$atmp, n=1)                                                  # ambient pressure at t10 (mb)
    Ta       <- tail(sub$soil_temp_c_out, n=1)                                       # air temp at t10 (deg C)
    Vd       <- 0.0012287                                                            # m3 (constant)
    A        <- 0.00950                                                              # m2 (constant)
    Ru       <- 8.31432                                                              # J mol-1 K-1 (constant)
    flux     <- ((C10 - C1)/(t10 - t1)) * (P/(Ta + 273.15))*(Vd/A)*((44.01*0.36)/Ru) # CO2 efflux (g CO2 m-2 h-1)
    xx       <- rbind(xx, id)
    yy       <- rbind(yy, flux)
  }
  Res <- data.frame(cbind(xx, yy))
  
  colnames(Res) <- c("codew", "flux")
  Res$codew <- as.character(Res$codew)
  
  test3             <- sqldf("select raw_parsr.* from raw_parsr where raw_parsr.codew = '65.9.2.2013'")
  

  
# build the new data frame 
  
  xx <- sqldf("SELECT raw_parsr.codew, raw_parsr.plot_code, raw_parsr.sub_plot, raw_parsr.measurement_code, raw_parsr.treatment_code_partitioning, MAX(raw_parsr.egm_measurement), MAX(raw_parsr.day), MAX(raw_parsr.month), MAX(raw_parsr.year), MAX(raw_parsr.hour), MAX(raw_parsr.soil_temp_c_out), MAX(raw_parsr.vwc_percent_out), MAX(raw_parsr.collar_height_cm) FROM raw_parsr GROUP BY raw_parsr.codew")
  colnames(xx) <- c("codew", "plot_code", "sub_plot", "measurement_code", "treatment_code_partitioning", "egm_measurement", "day", "month", "year", "hour", "soil_temp_c_out", "vwc_percent_out", "collar_height_cm")
  ResPAR <- merge(Res, xx,  by.x = "codew", by.y = "codew", all.x=TRUE)
  head(ResPAR)
  
# save to current directory  
  setwd("~/Desktop/data_sorting/Rflux")
  write.csv(ResPAR, file="flux_part_TAM_09_2013.csv") 
  
  ## style guide on how to lay out R code: http://google-styleguide.googlecode.com/svn/trunk/Rguide.xml#indentation 