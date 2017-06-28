
# This is a temporary file. Do not use. I just need to plot heterotrophic respiration from key plots for the 10th May El Nino meeting.

# load packages
  library(sqldf)
  require(ggplot2)
  require(plyr)
  require(gridExtra)
  
# read in soil respiration auxillary functions from GitHub
  setwd("~/Github/GEMcarbon.R")
  source("~/Github/GEMcarbon.R/soilrespiration_auxfunctions.r")
  
## SA RHET  
  # read in data
  # SA
  setwd("~/Github/gemcarbon_data/processed_ts_2017/ts_soil_respiration")
  dir()
  
  sa1   <- read.table("ts_ACJ01_Rs_part_2017.csv", sep=",", header=T)
  sa2   <- read.table("ts_ESP01_Rs_part_2017.csv" , sep=",", header=T)    
  sa3   <- read.table("ts_PAN02_Rs_part_2017.csv", sep=",", header=T)   
  sa4   <- read.table("ts_PAN03_Rs_part_2017.csv", sep=",", header=T)    
  sa5   <- read.table("ts_SPD01_Rs_part_2017.csv", sep=",", header=T)   
  sa6   <- read.table("ts_SPD02_Rs_part_2017.csv", sep=",", header=T)    
  sa7   <- read.table("ts_TAM-06_Rs_part_2017.csv", sep=",", header=T)     
  sa8   <- read.table("ts_TAM06_Rs_part_2017.csv", sep=",", header=T)    
  sa9   <- read.table("ts_TAM09_Rs_part_2017.csv", sep=",", header=T)   
  sa10  <- read.table("ts_TRU04_Rs_part_2017.csv" , sep=",", header=T)   
  sa11  <- read.table("ts_WAY01_Rs_part_2017.csv" , sep=",", header=T)
  
  sa1avg = sa1 %>% group_by(year, month) %>% 
    dplyr::summarize(avgRhet = mean(Rs_het_MgC_ha_mo, na.rm = T), 
                     stdRhet = sd(Rs_het_MgC_ha_mo, na.rm = T), 
                     date = max(as.Date(date)))
    
  psa1 <- ggplot(sa1, aes(x = date, y = sa1$Rs_het_MgC_ha_mo, na.rm = T)) +
    geom_point(data = sa1, aes(x = date, y = sa1$Rs_het_MgC_ha_mo), size = 2, colour = "darkblue", na.rm=T) +
    geom_line(data = sa1avg, aes(x = date, y = avgRhet), size = 2, colour = "darkblue", na.rm=T) +
    geom_ribbon(data = sa1avg, aes(ymin = avgRhet - stdRhet, ymax = avgRhet + stdRhet), alpha=0.2) +
    #ylim(0, 3) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    ggtitle("ACJ-01")
  psa1
  
  psa2 <- ggplot(sa2, aes(x = date, y = Rs_het_MgC_ha_mo, na.rm = T)) +
    geom_point(data = sa2, aes(x = date, y = Rs_het_MgC_ha_mo), size = 2, colour = "darkblue", na.rm=T) +
    #ylim(0, 3) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    ggtitle("ESP-01")
  
  psa3 <- ggplot(sa3, aes(x = date, y = Rs_het_MgC_ha_mo, na.rm = T)) +
    geom_point(data = sa3, aes(x = date, y = Rs_het_MgC_ha_mo), size = 2, colour = "darkblue", na.rm=T) +
    #ylim(0, 3) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    ggtitle("PAN-02")
  
  psa4 <- ggplot(sa4, aes(x = date, y = Rs_het_MgC_ha_mo, na.rm = T)) +
    geom_point(data = sa4, aes(x = date, y = Rs_het_MgC_ha_mo), size = 2, colour = "darkblue", na.rm=T) +
    #ylim(0, 3) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    ggtitle("PAN-03")
  
  psa5 <- ggplot(sa5, aes(x = date, y = Rs_het_MgC_ha_mo, na.rm = T)) +
    geom_point(data = sa5, aes(x = date, y = Rs_het_MgC_ha_mo), size = 2, colour = "darkblue", na.rm=T) +
    #ylim(0, 3) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    ggtitle("SPD-01")
  
  psa6 <- ggplot(sa6, aes(x = date, y = Rs_het_MgC_ha_mo, na.rm = T)) +
    geom_point(data = sa6, aes(x = date, y = Rs_het_MgC_ha_mo), size = 2, colour = "darkblue", na.rm=T) +
    #ylim(0, 3) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    ggtitle("SPD-02")
  
  psa8 <- ggplot(sa8, aes(x = date, y = Rs_het_MgC_ha_mo, na.rm = T)) +
    geom_point(data = sa8, aes(x = date, y = Rs_het_MgC_ha_mo), size = 2, colour = "darkblue", na.rm=T) +
    #ylim(0, 3) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    ggtitle("TAM-06")
  
  psa9 <- ggplot(sa9, aes(x = date, y = Rs_het_MgC_ha_mo, na.rm = T)) +
    geom_point(data = sa9, aes(x = date, y = Rs_het_MgC_ha_mo), size = 2, colour = "darkblue", na.rm=T) +
    #ylim(0, 3) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    ggtitle("TAM-09")
  
  psa10 <- ggplot(sa10, aes(x = date, y = Rs_het_MgC_ha_mo, na.rm = T)) +
    geom_point(data = sa10, aes(x = date, y = Rs_het_MgC_ha_mo), size = 2, colour = "darkblue", na.rm=T) +
    #ylim(0, 3) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    ggtitle("TRU-04")
  
  psa11 <- ggplot(sa11, aes(x = date, y = Rs_het_MgC_ha_mo, na.rm = T)) +
    geom_point(data = sa11, aes(x = date, y = Rs_het_MgC_ha_mo), size = 2, colour = "darkblue", na.rm=T) +
    #ylim(0, 3) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    ggtitle("WAY-01")
  
  fig3 <- grid.arrange(psa11, psa1, psa10, psa2, psa3, psa4, psa5, psa6, psa8, psa9, ncol=2, nrow=5) 
  fig3
  
  fig3a <- grid.arrange(psa11, psa2, psa10, psa1, psa3, psa4, ncol=2, nrow=3) 
  fig3a
  
  fig3b <- grid.arrange(psa5, psa6, psa8, psa9, ncol=2, nrow=2) 
  fig3b
  
  # Plot it
  
  subaa = subset(Res, plot_code=="LPG-01")
  subbb = subset(Res, plot_code=="LPG-02")
  
  aa <- ggplot(subaa, aes(x = date, y = Rs_het_MgC_ha_mo, na.rm = T)) +
    geom_point(data = subaa, aes(x = date, y = Rs_het_MgC_ha_mo), size = 2, colour = "darkblue", na.rm=T) +
    #ylim(0, 3) +
    ggtitle("LPG-01")
  aa
  
  bb <- ggplot(subbb, aes(x = date, y = Rs_het_MgC_ha_mo, na.rm = T)) +
    geom_point(data = subbb, aes(x = date, y = Rs_het_MgC_ha_mo), size = 2, colour = "darkblue", na.rm=T) +
    #ylim(0, 3) +
    ggtitle("LPG-02")
  bb
  
  fig4 <- grid.arrange(aa, bb, ncol=1, nrow=2) 
  fig4
  
## AFRICA RHET
  
  # read in data
  raw_consrA   <- read.table("~/Github/gemcarbon_data/raw_data_ingembd/soil respiration/XXX.csv", sep=",", header=T)
  raw_totsrA   <- read.table("~/Github/gemcarbon_data/raw_data_ingembd/soil respiration/LPG_TOT.csv", sep=",", header=T)
  weather      <- read.table("~/Github/gemcarbon_data/raw_data_ingembd/soil respiration/XXX.csv", sep=",", header=T) 
  
  raw_parsrLPG   <- read.table("~/Github/gemcarbon_data/raw_data_ingembd/soil respiration/LPG_PART.csv", sep=",", header=T)
  raw_parsrBOB   <- read.table("~/Github/gemcarbon_data/raw_data_ingembd/soil respiration/LPG_PART.csv", sep=",", header=T)
  raw_parsrKOG   <- read.table("~/Github/gemcarbon_data/raw_data_ingembd/soil respiration/LPG_PART.csv", sep=",", header=T)
  raw_parsrA     <- raw_parsrLPG #rbind(raw_parsrLPG, raw_parsrBOB, raw_parsrKOG)
  
  # Collar diameter (cm)
  raw_parsr <- c()
  raw_parsr$collar_diam <- 12
  
  # TEMP DATA FOR AFRICA 
  raw_parsr <- subset(raw_parsrA, treatment_code_partitioning=="all_no_lit_root_my")
  raw_parsr$codew <- paste(raw_parsr$plot_code, raw_parsr$collar_number, raw_parsr$litter_code, raw_parsr$replica, raw_parsr$Day, raw_parsr$Month, raw_parsr$year, sep="_")
  raw_parsr$air_temp_c <- 25
  raw_parsr$ch_fill <- 5
 
  
  # get unique identifyer for each measurement
  uid <- unique(raw_parsr$codew)
  xx <- c()
  yy <- c()
  
  for (i in 1:length(uid)) {
    sub      <- subset(raw_parsr, subset=(raw_parsr$codew == uid[i])) 
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
    ch       <- tail(sub$ch_fill, n=1)                                          # see gap filling function fill.na() in soilrespiration_auxfinctions.r
    Vd       <- 0.0012287                                                            # m3 (constant)
    A        <- 0.00950                                                              # m2 (constant)
    Ru       <- 8.31432                                                              # J mol-1 K-1 (constant)
    Va       <- A*(ch/100)                                             # additional volume m3
    fl       <- ((C10 - C1)/(t10 - t1)) * (P/(Ta + 273.15))*(Vd/A)*((44.01*0.36)/Ru) # CO2 efflux (g CO2 m-2 h-1)
    #flux2    <- Co2slope * (P/(Ta + 273.15))*(Vd/A)*((44.01*0.36)/Ru)               # CO2 efflux (g CO2 m-2 h-1)
    flux     <- (fl*A/Vd*(Va+Vd)/A)*6.312                                            # Convert to umol m-2 s-1. Correct for collar height.
    xx       <- rbind(xx, id)
    yy       <- rbind(yy, flux)
    print(yy)
  }
  rownames(xx) <- NULL
  rownames(yy) <- NULL
  
  Res <- data.frame(cbind(xx, yy))
  
  colnames(Res) <- c("codew", "flux_umolm2sec")
  Res$codew <- as.character(Res$codew)
  head(Res)
  

  Res$plot_code = str_split_fixed(Res$codew, "_", 7)[,1] 
  Res$collar_number = str_split_fixed(Res$codew, "_", 7)[,2]
  Res$litter_code = str_split_fixed(Res$codew, "_", 7)[,3]
  Res$replica = str_split_fixed(Res$codew, "_", 7)[,4]
  Res$Day = str_split_fixed(Res$codew, "_", 7)[,5]
  Res$Month = str_split_fixed(Res$codew, "_", 7)[,6]
  Res$year = str_split_fixed(Res$codew, "_", 7)[,7]
  Res$date = as.Date(paste(Res$year, Res$Month, Res$Day, sep="."), format="%Y.%m.%d")
  
  # corrections and conversions
  # remove outlyers
  Res$flux_umolm2sec <- rm.flux.outlier(Res$flux_umolm2sec, sd_interval=4) 
  ## Defaults for chamber volume and tube area:
  # The CPY-2 defaults are Volume = 2465 cm3  Area = 170 cm2 and V/A = 1450
  Vd = 1171/1000000    # chamber volume m3
  A = 0.0078           # tube area m2
  
  ## Corrections and conversions
  # add a temperature correction from Sotta et al 2004 Q10=1.8 and k=0.0613
  corrsresA = exp(-0.0695*(1))
  # Convert units umol m2 s-1 to MgC ha month = 1mo=2592000sec, 10000m2=1ha, 1000000umol = 1 mol, 1mol = 12 g, 1000000g=1Mg
  convert = (2592000*10000*12)/(1000000*1000000)
  Res$Rs_het_MgC_ha_mo = as.numeric(as.character(Res$flux_umolm2sec))*convert*corrsresA
  
  # Plot it
  
  subaa = subset(Res, plot_code=="LPG-01")
  subbb = subset(Res, plot_code=="LPG-02")
  
  aa <- ggplot(subaa, aes(x = date, y = Rs_het_MgC_ha_mo, na.rm = T)) +
               geom_point(data = subaa, aes(x = date, y = Rs_het_MgC_ha_mo), size = 2, colour = "darkblue", na.rm=T) +
               #ylim(0, 3) +
               ggtitle("LPG-01")
  aa

  bb <- ggplot(subbb, aes(x = date, y = Rs_het_MgC_ha_mo, na.rm = T)) +
               geom_point(data = subbb, aes(x = date, y = Rs_het_MgC_ha_mo), size = 2, colour = "darkblue", na.rm=T) +
               #ylim(0, 3) +
               ggtitle("LPG-02")
  bb
  
  fig3 <- grid.arrange(aa, bb, ncol=1, nrow=2) 
  fig3
  
  # save to current directory  
  setwd("~/Github/gemcarbon_data/processed_ts_2017/ts_soil_respiration")
  write.csv(Res, file="temp_rhet_LPG.csv")
  
  setwd("~/Github/gemcarbon_data/raw_data_ingembd/soil respiration/")
  
  # SEA RHET
  soil_resp_sea  <- read.table("~/Github/gemcarbon_data/raw_data_ingembd/soil_respiration/SAFE_SoilRespiration_Data_toCecile3.csv", sep=",", header=T, fill = TRUE)
  Res <- subset(soil_resp_sea, soil_resp_sea$Quality == 1 & soil_resp_sea$Collar_type == c("C3", "Total"))
  Res$plot_code <- Res$Plot
  Res$plot_code <- revalue(Res$plot_code, c("Tower" = "SAF-05","E" = "SAF-03","B South" = "SAF-01","B North" = "SAF-02","Belian" =  "MLA-01","Seraya" = "MLA-02","LF" = "SAF-04","DC1" = "DAN-04","DC2" = "DAN-05"))
  write.csv(Res, file="ts_rhet_sea_may17.csv")
  
  # date 
  #Res$day = str_split_fixed(Res$Date, "/", 3)[,1] 
  #Res$month = str_split_fixed(Res$Date, "/", 3)[,2]
  #Res$yr = str_split_fixed(Res$Date, "/", 3)[,3]
  #Res$year = as.numeric(as.character(Res$yr)) + 2000
  Res$date <- as.Date(paste(Res$year, Res$month, Res$day, sep="."), format="%Y.%m.%d")
  Res$Flux_Mg_C_ha_month <- as.numeric(as.character(Res$Flux_MgC_ha_month))
  
  # Plot it
  
  aa = subset(Res, plot_code %in% c("SAF-01", "SAF-02", "SAF-03", "SAF-04", "SAF-05"))
  bb = subset(Res, plot_code %in% c("MLA-01", "MLA-02"))
  cc = subset(Res, plot_code %in% c("DAN-04", "DAN-05"))
  dd = subset(Res, plot_code=="OP")
  
  xmin = min(aa$date, na.rm=T)
  xmax = max(aa$date, na.rm=T)
  
  paa <- ggplot(aa, aes(x = date, y = Flux_Mg_C_ha_month, colour = factor(aa$plot_code), na.rm = T)) +
                geom_point() +
                scale_colour_manual(values=rep(brewer.pal(6,"Paired"))) +
                theme(legend.position = "bottom") + 
                theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
                xlab("") + ylab(expression(paste("Heterotrophic Rsoil (MgC ",ha^-1,mo^-1, ")", sep=""))) +
                ylim(0, 3) 
                
  
  pbb <- ggplot(bb, aes(x = date, y = Flux_Mg_C_ha_month, colour = factor(bb$plot_code), na.rm = T)) +
                geom_point() +
                scale_colour_manual(values=rep(brewer.pal(6,"Paired"))) +
                theme(legend.position = "bottom") + 
                theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
                xlab("") + ylab(expression(paste("Heterotrophic Rsoil (MgC ",ha^-1,mo^-1, ")", sep=""))) +
                ylim(0, 3) + xlim(xmin, xmax)
  
  pcc <- ggplot(cc, aes(x = date, y = Flux_Mg_C_ha_month, colour = factor(cc$plot_code), na.rm = T)) +
                geom_point() +
                scale_colour_manual(values=rep(brewer.pal(6,"Paired"))) +
                theme(legend.position = "bottom") + 
                theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
                xlab("") + ylab(expression(paste("Heterotrophic Rsoil (MgC ",ha^-1,mo^-1, ")", sep=""))) +
                ylim(0, 3) + xlim(xmin, xmax)
  
  
  fig3 <- grid.arrange(paa, pbb, pcc, ncol=1, nrow=3) 
  fig3