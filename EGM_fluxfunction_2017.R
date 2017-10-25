## Code to go from EGM-4 raw data files to estimate flux in umol m-2 sec-1.
# Cecile Girardin 26.06.2017

Rflux <- function(datafile, ret="Res", plotname, collardiameter) {
  
# load packages
  library(sqldf)
  require(plyr)
  
# read in soil respiration auxillary functions from GitHub
  setwd("~/Github/GEMcarbon.R")
  source("~/Github/GEMcarbon.R/soilrespiration_auxfunctions.r")

    
    ## estimate flux for each measurement
    data = subset(datafile, plot_code==plotname)
    
#  TO DO: DEFAULT 
    collardiameter = 10 # 12 cm in Africa & Peru

# Replace missing temperature and swc with default values.
#w <- which(is.na(data$soil_temp_c))
#data$soil_temp_c[w] = 25
#w <- which(is.na(data$air_temp_c))
#data$air_temp_c[w] = 25
#w <- which(is.na(data$soil_vwc_per))
#data$soil_vwc_per[w] = 20

    ## Corrections and conversions
    # add a temperature correction from Sotta et al 2004 Q10=1.8 and k=0.0613
    corrsresA = exp(-0.0695*(1))
    # Convert units umol m2 s-1 to MgC ha month = 1mo=2592000sec, 10000m2=1ha, 1000000umol = 1 mol, 1mol = 12 g, 1000000g=1Mg
    convert = (2592000*10000*12)/(1000000*1000000)

    # Define unique id for each measurement
    data$codew   <- paste(data$sub_plot, data$collar_number, data$replica, data$day, data$month, data$year, sep=".") # In most cases you will need to add the data$replica to this unique id.

    # Order by codew and time
    data <- data[order(data$codew, data$time),]

    # get unique identifyer for each measurement
    uid <- unique(data$codew)
    xx <- c()
    yy <- c()
    zz <- c()
    zzz <- c()
    
    for (i in 1:length(uid)) {
      sub      <- subset(data, subset=(data$codew == uid[i])) 
      id       <- tail(sub$codew, n=1) 
      ten_co2  <- tail(sub$co2ref_ppm_sec, n=10)                                       # Drop first two values rather than only keep last 10 values.
      ten_time <- tail(sub$time, n=10)       
      fit      <- lm(ten_co2~ten_time)
      Co2slope <- fit$coefficients[2]                                                  # USE SLOPE RATHER THAN DIFFERENCE OF C10-C1
      C10      <- tail(ten_co2, n=1)                                                   # last CO2 measurement of last 10 measurements
      C1       <- head(ten_co2, n=1)                                                   # first CO2 measurement of last 10 measurements
      t10      <- tail(ten_time, n=1)                                                  # last time step of 10 last measurements
      t1       <- head(ten_time, n=1)                                                  # first time step of 10 last measurements
      P        <- tail(sub$atmp, n=1)                                                  # ambient pressure at t10 (mb)
      Ta       <- tail(sub$air_temp_c, n=1)                                            # air temp at t10 (deg C)
      ch       <- tail(sub$ch_fill, n=1)                                               # see gap filling function fill.na() in soilrespiration_auxfinctions.r
      codep    <- tail(sub$treatment_code_partitioning, n=1)
      plot     <- tail(sub$plot_code, n=1)
      Vd       <- 0.0012287                                                            # m3 (constant)
      A        <- 0.00950                                                              # m2 (constant)
      Ru       <- 8.31432                                                              # J mol-1 K-1 (constant)
      Va       <- A*(ch/100)                                             # additional volume m3
      fl       <- ((C10 - C1)/(t10 - t1)) * (P/(Ta + 273.15))*(Vd/A)*((44.01*0.36)/Ru) # CO2 efflux (g CO2 m-2 h-1)
      #flux2    <- Co2slope * (P/(Ta + 273.15))*(Vd/A)*((44.01*0.36)/Ru)               # CO2 efflux (g CO2 m-2 h-1)
      flux     <- (fl*A/Vd*(Va+Vd)/A)*6.312                                            # Convert to umol m-2 s-1. Correct for collar height.
      
      xx       <- rbind(xx, id)
      yy       <- rbind(yy, flux)
      zz       <- rbind(zz, as.character(codep))
      zzz       <- rbind(zzz, as.character(plot))
      print(yy)
    }
    rownames(xx) <- NULL
    rownames(yy) <- NULL
    rownames(zz) <- NULL
    rownames(zzz) <- NULL
    
    Res <- data.frame(cbind(xx, yy, zz, zzz))
    colnames(Res) <- c("codew", "Rflux_umolm2sec", "part_code", "plot_code")
    Res$codew <- as.character(Res$codew)
    Res$Rflux_umolm2sec <- as.numeric(as.character(Res$Rflux_umolm2sec))
    Res$Rflux_MgC_ha_mo = Res$Rflux_umolm2sec*convert*corrsresA # see correction factors above.

    temp = (strsplit(Res$codew, "[.]"))
    Res$sub_plot = unlist(lapply(temp, `[[`, 1))
    Res$collar_number = unlist(lapply(temp, `[[`, 2)) 
    Res$replica = unlist(lapply(temp, `[[`, 3))
    Res$day = unlist(lapply(temp, `[[`, 4))
    Res$month = unlist(lapply(temp, `[[`, 5))
    Res$year = unlist(lapply(temp, `[[`, 6))


    return(Res)
    
  }
  

