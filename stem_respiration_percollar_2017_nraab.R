# This code estimates stem respiration per tree collar. It does not scale to the whole surface of the tree yet.
# Cecile Girardin 02/05/2017.

rm(list = ls())
#library(DescTools)

library(sqldf)
library(dplyr)
setwd("/Users/nicolas/Dropbox/Oxford/ElNino/StemRespiration/")

source("allometric_equations_2014.R")
source("soilrespiration_auxfunctions.R")

#setwd("~/Github/GEMcarbon.R")
#source("~/Github/GEMcarbon.R/soilrespiration_auxfunctions.r")

#setwd("~/Github/GEMcarbon.R") 
#source("allometric_equations_2014.R")

stem_resp <- read.table("all_stem_resp_26Sep.csv", sep = ",",header = T)
#stem_resp <- read.table("~/ ... stem_resp_13June.csv", sep=",", header=T)


#Reading Census data
#census <- read.csv("forestplots_download_oct17.csv", sep = ",",header = T)
census<- read.csv("forestplots_18oct17.csv", sep = ",", header = TRUE)


print("Remember to change site elevation (L.132(aprox))")

###########################################################################################
############################# Declare Constants ###########################################
###########################################################################################

# Convert units umol m-2 s-1 to MgC ha month = 1mo=2592000sec, 10000m2=1ha, 1000000umol = 1 mol, 1mol = 12 g, 1000000g=1Mg
#convert  = (2592000*10000*12)/(1000000*1000000)    #[MgC ha^-1 month^-1]
convert = ((2592000*12))/(1000000*1000000)          #[MgC  m^-2 month^-1] 

## Corrections and conversions
# add a temperature correction from Sotta et al 2004 Q10=1.8 and k=0.0613
tempcorr = exp(-0.0695*(1))   #I do not know where to use this

collardiameter = 12   #[cm] collar diameter
collarheight = 5      #[cm] collar height

Vd       <- 0.0012287                                                            # EGM chamber volumme [m3] (constant)
A        <- 0.00950                                                              # God knows [m2] (constant)
Ru       <- 8.31432                                                              # [J mol^-1 K^-1] (constant)
T_kel    <- 273.15                                                               # to convert from Kelvin to ºC
A_collar <- (pi*((collardiameter/2)/100)^2)                                      # [m^2] Collar Area
V_total  <-  Vd + A_collar*(collarheight/100)                                    # [m^3] EGM chamber volume + collar volume



#### Function that we'll create only for Tambopata where they use another kind of chamber with 3 different heads (2; 3 and 4)    

Tambopata_chamber =  function(collar){
  
  V_total <- numeric(length(collar))
  A_collar <- numeric(length(collar))
  
  for(i in 1:length(collar)){
    
    if(is.na(collar)[i] == TRUE){
      V_total[i] <- mean(0.245, 0.55, 0.67)/1000    
      A_collar[i] <- mean(.0107, .0173, .0161)  
    }else{
      if(collar[i] == 2){
        V_total[i] <- .245/1000
        A_collar[i] <- .0107}
      
      if(collar[i] == 3){
        V_total[i] <- .55/1000
        A_collar[i] <- .0173}
      
      if(collar[i] == 4){
        V_total[i] <- .67/1000
        A_collar[i] <- .0161
      }
      else{
        V_total[i] <- mean(.245, .55, .67)/1000    
        A_collar[i] <- mean(.0107, .0173, .0161)  
      }
    }
  }
  return(data.frame(V_total, A_collar)) 
}

#######################################################################################    
#######################################################################################    
#######################################################################################


plotNames = as.character(unique(stem_resp$plot_code))

stem_resp$Plot_3_letters <- as.character(substr(stem_resp$plot_code,1,3))

data0 = subset(stem_resp, Plot_3_letters == "ANK")


###############################################################################################################
################## Weather Data to get air temperature for each Site ##########################################
###############################################################################################################

#Temp <-  read.table("TAM_gapfill_erai_monthly_2005_2016.csv", sep = ",",header = TRUE) 
#Temp <- read.table("kenia_gapfill_erai_monthly_2005_2016.csv",sep = ",",header = TRUE)
#Temp <- read.table("BOB_gapfill_erai_monthly_2005_2016.csv", sep = ",", header = TRUE)
#Temp <- read.table("kenia_gapfill_erai_monthly_2005_2016.csv", sep = ",", header = TRUE)
#Temp <- read.table("KOG_gapfill_erai_daily_2005_2016.csv", sep = ",", header = TRUE)
#Temp <- read.table("Tmean_KOG.csv", sep = ",", header = TRUE)
Temp <- read.table("NXV_gapfill_erai_monthly_2005_2016.csv", sep = ",", header = TRUE)
Temp$Serial_date_temp <- lubridate::decimal_date(lubridate::ymd(paste(Temp$year, Temp$month, 15, sep = "-") ))
#Temp$Serial_date_temp <- lubridate::decimal_date(lubridate::ymd(paste(Temp$Year, Temp$Month, 15, sep = "-") ))

###############################################################################################################
###############################################################################################################
###############################################################################################################


# SELECT A PLOT
plotname <- as.character(unique(data0$plot_code))

compendium <- c()
All_Sites <- c()

for(m in 1:length(plotname)){
  
  
  data1 = subset(stem_resp, plot_code==plotname[m])
  
  data1$V_total <- V_total
  data1$A_collar <- A_collar
  
  # These parameters should be numeric:
  data1$co2ref_ppm_sec <- as.numeric(as.character(data1$co2ref_ppm_sec))
  data1$time           <- as.numeric(as.character(data1$time))
  data1$atmp_mb        <- as.numeric(as.character(data1$atmp_mb))

  # unique identifyer for each measurement: tree_tag, replica, date 
  data1$codew <- paste(data1$tree_tag, data1$replica, data1$day, data1$month, data1$year, sep=".")
  data1$Decimal_Date <- lubridate::decimal_date(as.Date(paste(data1$year, data1$month, data1$day, sep = '-')))
  
  
  ### Routine to get air temperature from the re-analysis since we don't have air temperature from the instrument         
  for(i in 1:length(data1$Decimal_Date)){
    
    if(is.na(data1$Decimal_Date[i]) == FALSE){
      data1$air_temp_c[i] = Temp$Tmean[which.min(abs(data1$Decimal_Date[i]- Temp$Serial_date_temp))]
    }
    else{data1$air_temp_c[i] = 25}
  }
  
  
  # Estimate missing atmospheric pressure whith temperature-dependent version of the barometric equation (see soilrespiration_auxfunctions)
  t = mean(as.numeric(data1$air_temp_c), na.rm=TRUE) 
  w = which(is.na(data1$atmp_mb))
  
  data1$atmp_mb[w] = barometric_equation_T(elevation=223, temp=t)  #[mb] Barometric equation 
  
  ## estimate flux [umol CO2 m^-2 s^-1] for each measurement
  
  # get unique identifyer for each measurement
  uid <- unique(data1$codew)
  xx  <- c()
  yy  <- c()
  yy2 <- c()
  yy3 <- c()
  zz <- c()
  ll <- c()
  
  
  for (i in 1:length(uid)){
    
    sub      <- subset(data1, subset=(data1$codew == uid[i])) 
    id       <- tail(sub$codew, n=1) 
    decimal_date <- tail(sub$Decimal_Date, n=1) 
    ten_co2  <- tail(sub$co2ref_ppm_sec, n=10)                                       # Drop first two values rather than only keep last 10 values.
    ten_time <- tail(sub$time, n=10)       
    PlotCode <- as.character(tail(sub$plot_code, n=1))
    V_total <- as.numeric(tail(sub$V_total, n=1))
    A_collar <- as.numeric(tail(sub$A_collar, n=1))
    
    if (sum(is.na(ten_co2)) < 7 & length(ten_co2) >=7){                   #if at least 7 of the 10 values are different from NA, we apply a linear a regression to get CO2 flux
      ten_co2[!is.na(ten_co2)]
      ten_time[!is.na(ten_time)]
      fit      <- lm(ten_co2~ten_time)
      Co2slope <- fit$coefficients["ten_time"]                                       # USE SLOPE RATHER THAN DIFFERENCE OF C10-C1.
      P        <- tail(sub$atmp_mb, n=1)*100                                       # ambient pressure at t10 (Pa)
      Ta       <- tail(sub$air_temp_c, n=1)                                          # air temp at t10 (deg C)

      flux2  <- Co2slope*P*V_total/(Ru*(Ta+ 273.15))/A_collar                        # [umol CO2 m^-2 s^-1] EGM flux from the collar Area
      
    }else{flux2 = NA}
    yy2      <- rbind(yy2, flux2)
    
    C10      <- tail(ten_co2, n=1)                                                   # last CO2 measurement of last 10 measurements
    C1       <- head(ten_co2, n=1)                                                   # first CO2 measurement of last 10 measurements
    t10      <- tail(ten_time, n=1)                                                  # last time step of 10 last measurements
    t1       <- head(ten_time, n=1)                                                  # first time step of 10 last measurements
    P        <- tail(sub$atmp_Pa, n=1)                                                  # ambient pressure at t10 (mb)
    Ta       <- tail(sub$air_temp_c, n=1)                                            # air temp at t10 (deg C)

    flux2  <- ((C10 - C1)/(t10 - t1))*P*V_total/(Ru*(Ta+ 273.15))/A_collar                     # [umol CO2 m^-2 s^-1] EGM flux from the collar Area
    
    zz       <- rbind(zz, decimal_date) 
    xx       <- rbind(xx, id)
    yy       <- rbind(yy, flux2)
    ll       <- rbind(ll, PlotCode)
   
  }
  
  
  rownames(xx)  <- NULL
  rownames(yy)  <- NULL
  rownames(yy2) <- NULL
  rownames(zz) <- NULL
  rownames(ll) <- NULL
  
  yy            <- as.numeric(as.character(yy)) ## WHY IS THIS AS FACTOR??
  yy2           <- as.numeric(as.character(yy2))
  
  res <- cbind.data.frame(ll,xx, zz, yy2)
  
  colnames(res)   <- c("plot_code","codew","Serial_Date","flux_umolm2sec")
  
  
  ##Deleting negative values and values above 5 umol m^-2 s^-1 because they don't make sense for stem respiration
  res$flux_umolm2sec[res$flux_umolm2sec < 0 | res$flux_umolm2sec > 5] <- NA
  
  
  ##Deleting values on the tails of the remaining distribution (2.5 std positive from the mean according to rm.flux.outlier f(x))
  res$flux_umolm2sec <- rm.flux.outlier(res$flux_umolm2sec, 2.5)
  hist(as.numeric(res$flux_umolm2sec), breaks = 5)
  mean_flux_umolm2sec = mean(res$flux_umolm2sec, na.rm = TRUE)
  
  
  ##############################################################
  ######## Scale to stem surface area of the plot. #############
  ##############################################################    
  
  #1-Subset for the plot of interest
  #data_census = subset(census, plot_code==plotname[m])
  data_census = subset(census, Plot.Code==plotname[m])
  
  #2-Extract Date census
  #year_from_census <- lubridate::year(lubridate::date_decimal(data_census$census_date)) 
  year_from_census <- lubridate::year(lubridate::date_decimal(data_census$Census.Date)) 
  #month_from_census <- lubridate::month(lubridate::date_decimal(data_census$census_date))
  month_from_census <- lubridate::month(lubridate::date_decimal(data_census$Census.Date))
  #day_from_census <- lubridate::day(lubridate::date_decimal(data_census$census_date))
  day_from_census <- lubridate::day(lubridate::date_decimal(data_census$Census.Date))
  #be careful with NA....
  data_census$codew <- paste(data_census$tree_tag,NA,day_from_census,month_from_census,year_from_census,sep = '.')
  
  #3-Match collar respiration based on measurement code, with same tree tag and closest date to get tree DBH
  
  # extracting tree tags from res$codew    
  res$tree_tags_res <- c()
  res$DBH <-c()
  
  for (i in 1:length(res$codew)){
    points_tags_res = which(strsplit(as.character(res$codew[i]), "")[[1]]==".")
    res$tree_tags_res[i] = (substr(as.character(res$codew[i]), 1, points_tags_res[1]-1)) 
  }
  
  #### Matching the tree tag from respiration measurements with the closest census' (by date) DBH
  
  for(i in 1:length(res$tree_tags_res)){
    
    if(is.na(match(res$tree_tags_res[i], data_census$Tag.Number)) == FALSE){
      
      try(subset_census <- subset(data_census, data_census$Tag.Number == res$tree_tags_res[i]))
      
      if(is.na(res$Serial_Date[i]) == FALSE){
        res$DBH[i] <- subset_census$D1[which.min(abs(res$Serial_Date[i]-subset_census$Census.Date))]
        
      }else{res$DBH[i <- NA]}
      
      
    }else{res$DBH[i] <- NA}
    
  }
  
  
  #Res$DBH[Res$DBH == 0] <- NA     #Because values with DBH = 0; don't make sense
  
  data_census$D2[data_census$D2 > 1000] <- NA      #Too Big trees
  data_census$D2[data_census$D2 < 100] <- NA       #Too small trees
  plot(data_census$D2)
  
  #### Separating Respiration measurements by day 
  Stem_Respiration_ha_month_MgCHa <- c()
  Stem_Respiration_ha_month_MgCHa_sorted <- c()
  

  
  for(i in 1:length(unique(res$Serial_Date))){
    
    subset_respiration <- subset(res, res$Serial_Date == unique(res$Serial_Date)[i])
    
    if(nrow(subset_respiration) > 5){ 

      closest_census_date <-  unique(data_census$Census.Date)[which.min(abs(unique(subset_respiration$Serial_Date) - unique(data_census$Census.Date)))]
      
      subset_census_date <- subset(data_census, closest_census_date == data_census$Census.Date)
      
      subset_census_date$stem_area <- Chambers2004_surfaceArea(subset_census_date$D2/10)   #[m^2] Stem Area per tree, , input dbh must be in [cm]!!!!!!  
      
      mean_collar_flux <- mean(subset_respiration$flux_umolm2sec, na.rm = TRUE)  #[umol stem^-1 s^-1]
      
      sd_collar_flux <- sd(subset_respiration$flux_umolm2sec, na.rm = TRUE)  #[umol stem^-1 s^-1]
      
      subset_census_date$stem_respiration_umolstemsec <- subset_census_date$stem_area * mean_collar_flux  #Respiration per stem [umolCO2 stem^-1 s^-1]
      
      subset_census_date$stem_respiration_MgCStemMonth <- subset_census_date$stem_respiration_umolstemsec*convert         #[MgC stem^-1 month^-1]
      
      
    }else{#closest_census_date <-  unique(data_census$census_date)[which.min(abs(unique(subset_respiration$Serial_Date) - unique(data_census$census_date)))]
      
      #subset_census_date <- subset(data_census, closest_census_date == data_census$census_date)
      
      subset_census_date$stem_area <- Chambers2004_surfaceArea(subset_census_date$D2/10)   #[m^2] Stem Area per tree, input dbh must be in [cm]!!!!!! 
      
      mean_collar_flux <- NA
      
      sd_collar_flux <- NA
      
      subset_census_date$stem_respiration_umolstemsec <- NA  
      
      subset_census_date$stem_respiration_umolstemsec <- NA
      
      subset_census_date$stem_respiration_MgCStemMonth <- NA
    }        
    
    subset_census_date$stem_respiration_MgCStemMonth[!is.finite(subset_census_date$stem_respiration_MgCStemMonth)] <- NA   #Getting rid of Inf values for stem Area
    Stem_area_plot <- sum(subset_census_date$stem_area, na.rm = TRUE)
    number_trees_plot <- length(subset_census_date$stem_area)
    
    Total_StemResp_ha <- sum(subset_census_date$stem_respiration_MgCStemMonth, na.rm = TRUE)
    Stem_Respiration_ha_month_MgCHa[[i]] <- data.frame(res$plot_code[i],unique(res$Serial_Date)[i],lubridate::year(lubridate::date_decimal(unique(res$Serial_Date)[i])), lubridate::month(lubridate::date_decimal(unique(res$Serial_Date)[i])), lubridate::day(lubridate::date_decimal(unique(res$Serial_Date)[i])), mean_collar_flux, sd_collar_flux, number_trees_plot, Stem_area_plot, Total_StemResp_ha)   
    
  }  
  
  compendium[[m]] <- do.call(rbind,Stem_Respiration_ha_month_MgCHa)  
  colnames(compendium[[m]]) <- c("A", "Serial_Date", "C", "D", "E", "F", "G", "H", "I", "J")    #Fantasy names for the columns, then we'll assign them real ones
  
  compendium[[m]] <- dplyr::arrange(compendium[[m]], compendium[[m]]$Serial_Date)
  
} #end of 'for' from line 76


All_Sites <- do.call(rbind, compendium)
colnames(All_Sites) <- c("Plot Code", "Serial_Date", "Year", "Month", "Day", "Collar mean Flux [umol m^-2 s^-1]", "Collar sd Flux [umol m^-2 s^-1]","Number of Trees", "Total Stem Area [m^2]", "Total Stem Respiration [MgC Ha^-1 month^-1]")    


#Writing 
write.csv(All_Sites, file = "./Outputs/LPG")
