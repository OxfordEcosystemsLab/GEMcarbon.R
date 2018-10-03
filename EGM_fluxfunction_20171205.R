## Code to go from EGM-4 raw data files to estimate flux in umol m-2 sec-1.
# Cecile Girardin 26.06.2017

Rflux <- function(datafile, ret="Res", plotname, collardiameter) {
  
# load packages
  library(dplyr)
  library(tidyverse)
  library(lubridate)
  
# read in soil respiration auxillary functions from GitHub
  setwd("~/Github/GEMcarbon.R")
  source("~/Github/GEMcarbon.R/soilrespiration_auxfunctions.r")

## subset datafile to estimate measurements per plot, or run it for all the plots (data = datafile)
  data = datafile #subset(datafile, plot_code==plotname)
    
#  TO DO: set a DEFAULT collar diameter
  collardiameter = 12 # 12 cm in Africa & Andes & TAM, 10.6 cm in Malaysia, 10.143 in JEN 

data$air_temp_c = NA
data$ch_fill = NA

# Replace missing temperature and collar height with default values.
  w = which(is.na(data$air_temp_c))
  data$air_temp_c[w] = 25
  w = which(is.na(data$ch_fill))
  data$ch_fill[w] = 7
  
    ## Corrections and conversions
    # add a temperature correction from Sotta et al. 2004 Q10=1.8 and k=0.0613
    corrsresA = exp(-0.0695*(1))
    # Convert units umol m2 s-1 to MgC ha month = 1mo=2592000sec, 10000m2=1ha, 1000000umol = 1 mol, 1mol = 12 g, 1000000g=1Mg
    convert = (2592000*10000*12)/(1000000*1000000)

# !! It would be better to Convert to umol m2 s-1 here, from g CO2 m-2 h-1.

    # Define unique id for each measurement
    data$codew = paste(data$plot_code, data$sub_plot, data$collar_number, data$replica, data$day, data$month, data$year, data$measurement_code, data$treatment_code_partitioning, sep=".") # In most cases you will need to add the data$replica to this unique id.

    # Order by codew and time
    data = data[order(data$codew, data$time),]

    # get unique identifyer for each measurement
    uid = unique(data$codew)
    xx  = c()
    yy  = c()
    zz  = c()
    zzz = c()
    
    for (i in 1:length(uid)) {
      sub      = subset(data, subset=(data$codew == uid[i]))       # Drop first four measurements: & data$time >= 20
      id       = tail(sub$codew, n=1) 
      ten_co2  = tail(sub$co2ref_ppm_sec, n=10)                                                   
      ten_time = tail(sub$time, n=10)         
      
      P        = tail(sub$atmp_mb, n=1)*100                                           # ambient pressure at t10 (Pa)
      Ta       = tail(sub$air_temp_c, n=1)                                            # air temp at t10 (deg C)
      ch       = tail(sub$ch_fill, n=1)                                               # see gap filling function fill.na() in soilrespiration_auxfinctions.r
      codep    = tail(sub$treatment_code_partitioning, n=1)
      plot     = tail(sub$plot_code, n=1)
      A_collar = (pi*((collardiameter/2)/100)^2)                                      # Area of the collar (m2)
      Vd       = 0.0012287                                                            # m3 (constant)
      A        = 0.00950                                                              # m2 (constant)
      Ru       = 8.3144                                                               # J mol-1 K-1 (constant)
      Va       = A*(ch/100)                                                           # additional volume m3
      Vtot     = Vd+Va
      
      # This is the equation from the GEM manual, we have replaced it with the equation below.
      #C10      = tail(ten_co2, n=1)                                                   # last CO2 measurement of last 10 measurements
      #C1       = head(ten_co2, n=1)                                                   # first CO2 measurement of last 10 measurements
      #t10      = tail(ten_time, n=1)                                                  # last time step of 10 last measurements
      #t1       = head(ten_time, n=1)                                                  # first time step of 10 last measurements
      #fl       = ((C10 - C1)/(t10 - t1)) * (P/(Ta + 273.15))*(Vd/A)*((44.01*0.36)/Ru) # CO2 efflux (g CO2 m-2 h-1). This is the equation we have in the GEM manual. We need to update the manal.
      #flux     = (fl*A/Vd*(Va+Vd)/A)*6.312                                           # Convert to umol m-2 s-1, and correct for collar height.
      
      
      if (sum(is.na(ten_co2)) < 7 & length(ten_co2) >= 7){                   # if at least 7 of the 10 values are different from NA, we apply a linear a regression to get CO2 flux
        
        ten_co2[!is.na(ten_co2)]
        ten_time[!is.na(ten_time)]
        fit      = lm(ten_co2~ten_time)
        Co2slope = fit$coefficients[2]                                       # ["ten_time"]
       
        flux    = Co2slope * P * Vtot / (Ru * (Ta + 273.15)) / A_collar                 # output is in umol m-2 s-1. This equation was provided by Terhi Riutta, January 2018.
       
        
      }else{flux = NA}
      
      xx        = rbind(xx, id)
      yy        = rbind(yy, flux)
      zz        = rbind(zz, as.character(codep))
      zzz       = rbind(zzz, as.character(plot))
      print(xx)
    }

    rownames(xx)  = NULL
    rownames(yy)  = NULL
    rownames(zz)  = NULL
    rownames(zzz) = NULL
    
    Res = data.frame(cbind(xx, yy, zzz)) #zz, 
    colnames(Res) = c("codew", "Rflux_umolm2sec", "part_code", "plot_code") 
    Res$fluxnum = as.numeric(levels(Res$Rflux_umolm2sec))[Res$Rflux_umolm2sec] # Make sure flux is numeric

# Filter outlyers
## Deleting negative values and values above 15 umol m^-2 s^-1 because they don't make sense for soil respiration
Res$fluxnum[Res$fluxnum <= 0 | Res$fluxnum >= 15] <- NA

# We are not deleting values on the tails of the remaining distribution here. We need to explore these values first.
#res$flux_umolm2sec <- rm.flux.outlier(res$flux_umolm2sec, 2.5)
#hist(as.numeric(res$flux_umolm2sec), breaks = 5)

    Res$codew = as.character(Res$codew)
    Res$Rflux_MgC_ha_mo = Res$fluxnum*convert*corrsresA          # see convertion and correction factors above.

    temp              = (strsplit(Res$codew, "[.]"))                     # split codew into the information we need (plot_code, sub_plot, collar_number, replica, day, month, year).
    Res$plot_code     = unlist(lapply(temp, `[[`, 1))
    Res$sub_plot      = unlist(lapply(temp, `[[`, 2))
    Res$collar_number = unlist(lapply(temp, `[[`, 3)) 
    Res$replica       = unlist(lapply(temp, `[[`, 4))
    Res$day           = unlist(lapply(temp, `[[`, 5))
    Res$month         = unlist(lapply(temp, `[[`, 6))
    Res$year          = unlist(lapply(temp, `[[`, 7))
    Res$collection    = as.Date(paste(Res$year, Res$month, Res$day, sep="."), format="%Y.%m.%d") 
    Res$measurement_code            = unlist(lapply(temp, `[[`, 8)) 
    Res$treatment_code_partitioning = unlist(lapply(temp, `[[`, 9))
    
    # convert character to numeric variables !! CHECK THIS: NAs introduced by coercion
    names <- c(7:12)
    Res[,names] <- sapply(Res[,names],as.numeric)

    # Res - average per collar (average replicas!)
    Res1 = Res %>% group_by(plot_code, measurement_code, 
                            treatment_code_partitioning, #for partitioning
                            #disturbance_code_control, #for distubance
                            #litter_code, #for IC
                            #cwd_num, #for cwd
                            year, month, sub_plot, collar_number) %>% 
      dplyr::summarise(collectiondate = min(collection),
                       N = length(Rflux_MgC_ha_mo[!is.na(Rflux_MgC_ha_mo)]),
                       collar_fluxnum_umolm2sec = mean(fluxnum, na.rm = T),
                       collar_Rflux_MgC_ha_mo = mean(Rflux_MgC_ha_mo, na.rm = T)) %>% 
      arrange(plot_code, year, month, sub_plot, collar_number) %>% data.frame(.)
    
# Average over the whole plot: one value per plot per month
    Res2 = Res1 %>% group_by(plot_code, measurement_code, treatment_code_partitioning, year, month) %>% 
                   dplyr::summarize(avg = mean(collar_Rflux_MgC_ha_mo, na.rm = T), 
                                    sd = sd(collar_Rflux_MgC_ha_mo, na.rm = T),
                                    collection_date = max(collectiondate)) %>% data.frame(.)


Res2 %>% group_by(plot_code) %>%
  ggplot(data=., aes(month, avg, colour=year)) + geom_point() +
  facet_wrap(~plot_code)

#Res2$date = strptime(paste(as.character(Res2$year), as.character(Res2$month), as.character("15"), sep="-"), format="%Y-%m-%d")  


    return(Res1)
    return(Res2)
 
  }
  

