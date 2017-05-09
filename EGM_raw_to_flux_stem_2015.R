## Code to go from EGM-4 raw data files to the csv files we need for the stem_respiration_2015.R code
# Cecile Girardin August 2015
# Last edited: Cecile Girardin September 2015

# Notes:

# simple steps:
# 1. organise respiration data into one file. Save your files as .csv
# 2. change directory in line XXXXXXXXX and replace "XXXXXXXXX.csv" with the name of the file you want to run this code through.
# 3. Do the same for the temperature and vwc files. 
# 4. Change COLLAR DIAMETER (cm) in the code.
# 5. run code
# 6. your new .csv files will be in the directory you specify in lines XXXXXXXXX
# 7. Run the stem_respiration_2015.R function

# column names
# plot_code
# measurement_code: this is the partitionning code (e.g. TOT, CON, DLF, etc - see manual for codes)
# subplot
# year
# egm_measurement: this is ;Plot from the EGM
# recno, day, month, hour, min, CO2ref, inputD, atmp, probe_type: from EGM file

# column names for temperature files:
# plot_code
# measurement_code: this is the partitionning code (e.g. TOT, CON, DLF, etc - see manual for codes)
# subplot
# day
# month
# year
# temp
# vwc
# collar_depth
# collar_diam

# read in data 
#setwd("C:/Users/Cecile/Documents/GitHub/GEMcarbon.R/example files")
setwd("/Users/cecile/Dropbox/GEMcarbondb/db_csv/db_csv_2015/readyforupload_db/acj_pan_2015/")
Rstem_flux <- read.table("Rstem_flux_PAN03_2013_2014.csv", header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE)
Rstem_temp <- read.table("Rstem_temp_PAN03_2013_2014.csv", header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE)

# load packages
  library(sqldf)
# read in soil respiration auxillary functions from GitHub
  source("/Users/cecile/Documents/GitHub/GEMcarbon.R/soilrespiration_auxfunctions.R")

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
  raw <- subset(Rstem_flux, plot_code=="PAN-03")
  wea <- subset(data.frame(Rstem_temp), plot_code=="PAN-03")
  
# Add missing columns
  wea$vwc <- NA
  
# Collar diameter (cm)
  raw$collar_diam <- 12

# Remove rows where co2ref = NA
raw <- raw[!(is.na(raw$co2ref_ppm)),]

# Add air temperature (temp), volumetric water content (vwc), and chamber depth (depth) to the raw control data (raw_consr)
  wea$code1   <- paste(wea$sub_plot, wea$day, wea$month, wea$year, sep=".") # only use code 1 to merge wea and raw (subplot.day.month.year is not a unique identifier).
  raw$code1   <- paste(raw$sub_plot, raw$day, raw$month, (2000+raw$year), sep=".")
  raw         <- sqldf("SELECT raw.*, wea.soil_temp, wea.vwc, wea.collar_depth FROM raw JOIN  wea  ON raw.code1 = wea.code1")
 
# code1 for subplot.day.month.year, but then use egm_measurement for codew
  raw$codew   <- paste(raw$egm_measurement, raw$day, raw$month, raw$year, sep=".")
  
# Estimate missing pressure whith temperature-dependent version of the barometric equation (see soilrespiration_aux-functions)
  t <- mean(as.numeric(wea$temp), na.rm=T)
  w <- which(is.na(raw$atmp))
  raw$atmp[w] <- barometric_equation_T(elevation=0, temp=t )
         
## estimate flux for each measurement
  ## Sanity checks. Plot each flux batch to check data.
 
  #### TO DO: this only shows the last plot. What's wrong? 
  op <- par(ask=TRUE) # http://stackoverflow.com/questions/6031093/making-a-series-of-plots-that-proceed-by-a-click
  for (i in 1:length(raw$codew)){
    aa <- subset(raw, codew == codew[i], select = c(codew, co2ref, time))
    colnames(aa) <- c("codew", "co2", "time")
    plot(aa$co2, aa$time, main = paste("code:", head(aa$codew, 1)), xlab="time", ylab="CO2 (micro mol s-1? ppm?)")
    par(op)
  }

  ## Linear fit, estimate r2 quality check. 
  umea <- unique(raw$codew)
  xx <- c()
  yy <- c()
  zz <- c()
  
  for (i in 1:length(umea)) {
    sub  <- subset(raw, subset=(raw$codew == umea[i]))
    fit  <- lm(sub$time ~ sub$co2ref_ppm) 
    r    <- summary(fit)$r.squared 
    p    <- anova(fit)$'Pr(>F)'[1] # or p <- summary(fit)$coefficients[,4]
    u    <- head(sub$codew, 1)
    xx   <- rbind(xx, r) 
    yy   <- rbind(yy, p)
    zz   <- rbind(zz, u)  
  }
  table <- data.frame(cbind(xx, yy, zz), row.names=NULL)
  colnames(table) <- c("r2", "pvalue", "unique_code") # notification: small r2 doesn't mean bad flux - small flux = small r2
  
# get unique identifyer for each measurement
  uid <- unique(raw$codew)
  xx <- c()
  yy <- c()
  zz <- c()
  
  #for (i in uid) {
  for (i in 1:length(uid)) {
    sub      <- subset(raw, subset=(raw$codew == uid[i]))
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
    zz       <- rbind(zz, Co2slope)
  }
  Res <- data.frame(cbind(xx, yy, zz), row.names=NULL)
  colnames(Res) <- c("codew", "flux", "co2slope")
  
# build the new data frame
  Restot <- sqldf("SELECT Res.*, MAX(raw.plot_code), MAX(sub_plot), MAX(raw.egm_measurement), MAX(raw.day), MAX(raw.month), MAX(raw.year), MAX(raw.hour), MAX(raw.soil_temp), MAX(raw.vwc), MAX(raw.collar_depth) FROM Res JOIN raw ON Res.codew = raw.codew GROUP BY Res.codew")
  colnames(Restot)  <- c("code", "flux_umolco2_m2_s", "co2slope", "plot_code", "sub_plot", "egm_measurement", "day", "month", "year", "hour", "soil_temp_degC", "vwc_percent", "collar_depth_cm") 
    
# save to current directory  
  write.csv(Restot, file="Rstem_PAN03_co2slope.csv")
