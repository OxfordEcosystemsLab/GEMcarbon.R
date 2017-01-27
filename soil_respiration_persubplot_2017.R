### Function Soil respiration:
# This function calculates soil respiration and uses input data specified in the RAINFOR-GEM manual.
# Based on matlab code developed by Chris Doughty, 2011.
# Last edited: Cecile Girardin, 10.09.2015

### Required Data:
# data.frame of total soil respiration
# data.frame of partition respiration 
# data.frame of control respiration 
# plotname: specify plot_code of the plot you are working with (eg. WAY-01)
# ret: data-format of return values: "monthly.means.ts" or "monthly.means.matrix"
# plotit: logical (T/F), plot a quick graphical summary of the data?
# User has to specify either elevation or pressure.


#load packages
library(sqldf)
require(ggplot2)


### read data for option 1:
setwd("~/Github/gemcarbon_data/processed_data/soil_respiration_flux")
data.resc <- read.table("flux_control_ESP01_09to14.csv", sep=",", header=T)
data.resp <- read.table("flux_part_ESP_01_2013.csv", sep=",", header=T)
data.rest <- read.table("flux_total_ESP01_09to14.csv", sep=",", header=T)

# data.resp$collar_height_cm has a lot of NAs, I am replacing NAs by mean(data.resp$collar_height_cm, na.rm=T)
data.resp$collar_height_cm[is.na(data.resp$collar_height_cm)] <- mean(data.resp$collar_height_cm, na.rm=T)

pressure = 1013.25
plotname = "ESP-01"
partitioningoption = 1
elevation = "Default"
T_ambient="Default"
plotit=T

### read data for option 2:
#setwd("/Users/Cecile/Dropbox/Carbon_Use_Efficieny_R/testing/soilresp")

#data.resc <- read.table("Resconallsam.csv", sep=",", header=T)
#data.resp <- read.table("Resparallsam.csv", sep=",", header=T)
#data.rest <- read.table("Restotallsam.csv", sep=",", header=T)
#pressure = 1013.25
#plotname = 1.1
#partitioningoption = 2
#elevation = "Default"
#pressure="Default"
#T_ambient="Default"

# read correction functions:
source("/Users/cecile/Documents/GitHub/GEMcarbon.R/soilrespiration_auxfunctions.R")


soilrespiration <- function(data.rest,data.resp,data.resc, plotname, ret="monthly.means.ts", # Add tube radius as a parameter, change A to A <- pi*(rad^2) 
                            partitioningoption="Default",
                            pressure="Default", elevation="Default", T_ambient="Default",
                            plotit=F) {
  
  # Partitionning option 
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
  
  ######################################################## TO DO #############################################################################
  ############################################################################################################################################
  ############################################################################################################################################
  
  ### Defaults for chamber volume and tube area:
  # The CPY-2 defaults are Volume = 2465 cm3  Area = 170 cm2 and V/A = 1450
  Vd = 1171/1000000    # chamber volume m3
  A = 0.0078           # tube area m2
  
  ## TOTAL SOIL RESPIRATION per subplot, Mg C / ha / yr.

  # remove outliers and NAs: Fluxes based on overall correction, ## Temperature and chamber correction: Temperature and Chamber height (see functions!)
  # Note: the choice of the sd_interval changes things.
  # Note from CG, 22 Sept 2015: check the functions from soilrespiration_auxfunctions. They are from Chris's Matlab code.
  
  # Initialise dataframe for timeseries (ts)
  ts <- data.frame(data.rest$plot_code, data.rest$sub_plot, data.rest$plot_corner_code, data.rest$collar_number, data.rest$measurement_code, data.rest$replica, data.rest$year, data.rest$egm_measurement, data.rest$recno, data.rest$day, data.rest$month, data.rest$hour, data.rest$soil_temp_c_out, data.rest$collar_height_cm, data.rest$flux)
  colnames(ts) <- c("plot_code", "sub_plot", "plot_corner_code", "collar_number", "measurement_code", "replica", "year", "egm_measurement", "recno", "day", "month", "hour", "soil_temp_c_out", "collar_height_cm", "flux")  
  
  ts$fluxt1 <- ts$flux
  ts$fluxt <- rm.flux.outlier(ts$fluxt1, sd_interval=5) 
  ts$tempt1 <- ts$soil_temp_c_out
  ts$tempt <- rm.temp.outlier(ts$tempt1, ts$month) 
  ts$cht1  <- ts$collar_height_cm
  ts$cht   <- fill.ch.na(ts$cht1, ts$month)    # !!INTERPOLATION: WORKING ON THIS WITH ALLIE
  
  ## Perform chamber and flux correction (Metcalfe 2009), see function fluxcorr. 
  # chamber volume correction according to Metcalfe et al (2009): Rainfor Manual Appendix II, page 75. 
  ts$Rs_total <- fluxcorr(flux=ts$fluxt, temp=ts$tempt, ch=ts$cht, Vd=Vd, A=A, pressure=pressure)
  
  plot1 <- ggplot(ts, aes(x=year, y=Rs_total, na.rm=T)) +
    geom_point(data = ts, aes(x=year, y=Rs_total), size=2, colour='orange', na.rm=T) + 
    #theme(text = element_text(size=17), legend.title = element_text(colour = 'black', size = 17, hjust = 3, vjust = 7, face="plain")) +
    xlab("") + ylab(expression(paste(Total R[soil] flux, "(Mg C ", ha^-1, mo^-1, ")", sep=""))) +    
  plot1
  
  # convert from umol m-2 s-1 to MgC ha month
  # convert units umol m2s-1 to MgC ha month = 1mo=2592000sec, 10000m2=1ha,
  # 1000000umol = 1 mol, 1mol = 12 g, 1000000g=1Mg
  convert = (2592000*10000*12)/(1000000*1000000)
  totresAc = resAt*convert*corrsresA
  totresAcstd = resAtstd*convert*corrsresA
  
  
 
  ### CONTROL SOIL RESPIRATION
  
  # remove outliers (> 3 SD) and NAs:
  fluxc <- rm.flux.outlier(fluxc, sd_interval=2) # Discuss with team: it makes a big difference to the data if you use 2 sd or 3 sd.
  tempc <- rm.temp.outlier(temp = tempc, month=monthc)
  chc   <- rm.ch.outlier(ch=chc)
  
  ## Flux correction according to Metcalfe, RAINFOR Manual, Appendix 2, p. 75 
  RccA <- fluxcorr(flux=fluxc, temp=tempc, ch=chc, Vd=Vd, A=A, pressure=pressure) 
  
  #### PARTITIONING SOIL RESPIRATION
  
  # remove outliers and NAs: Flux (sd > 3), Temperature and Chamber height (see soilrespiration_aux-functions.R)
  fluxp <- rm.flux.outlier(fluxp, sd_interval=2)
  tempp <- rm.temp.outlier(temp=tempp, month=monthp)
  chp   <- rm.ch.outlier(ch=chp)
  
  # flux and chamber volume correction, see function fluxcorr
  RcpA <- fluxcorr(flux=fluxp, temp=tempp, ch=chp, Vd=Vd, A=A, pressure=pressure)
  
  ### Calculate respiration values in each year and month for the three different treatments:
  
  ### This section is to differentiate between different forms of partitioning experiments:
  #  Control - normal litterfall
  #  Control - no litterfall
  #  Control - double litterfall
  #  Mycorrhizae - normal litterfall
  #  Mycorrhizae - no litterfall
  #  Mycorrhizae - double litterfall
  #  Soil - normal litterfall
  #  Soil - no litterfall
  #  Soil - double litterfall
  
  ############################################
  ############################################ TO DO: plot data per tube & treatment to identify outlyers.
  ############################################
  
  
  # collar height correction - done?
  # disturbance correction
  # estimate rhisosphere respiration for each tube (or subplot?) (e.g. control_not_litter - so_nor_litter)?
  
  fluxp <- rm.flux.outlier(fluxp, sd_interval=2)
  tempp <- rm.temp.outlier(temp=tempp, month=monthp)
  chp   <- rm.ch.outlier(ch=chp)
  
  # flux and chamber volume correction, see function fluxcorr
  RcpA <- fluxcorr(flux=fluxp, temp=tempp, ch=chp, Vd=Vd, A=A, pressure=pressure)
  
  ############################################
  ############################################ 
  ############################################
  
  
  # Total Respiration initialize matrixes:
  resAt     <- matrix(data=NA, nrow=12, ncol=fir_yeare-fir_year+1, dimnames=list(c(month.name),fir_year:fir_yeare))
  resAtstd  <- matrix(data=NA, nrow=12, ncol=fir_yeare-fir_year+1, dimnames=list(c(month.name),fir_year:fir_yeare))
  
  # Control: initialize matrixes:
  rcanotper <- matrix(data=NA, nrow=12, ncol=fir_yeare-fir_year+1, dimnames=list(c(month.name),fir_year:fir_yeare))
  rcaper    <- matrix(data=NA, nrow=12, ncol=fir_yeare-fir_year+1, dimnames=list(c(month.name),fir_year:fir_yeare))
  DCdAstd   <- matrix(data=NA, nrow=12, ncol=fir_yeare-fir_year+1, dimnames=list(c(month.name),fir_year:fir_yeare))
  
  # Partitioning: initialize matrices:
  
  if (partitioningoption == 1) {
    
    con_nor_litA  <- matrix(data=NA, nrow=12, ncol=fir_yeare-fir_year+1, dimnames=list(c(month.name),fir_year:fir_yeare))
    con_no_litA   <- matrix(data=NA, nrow=12, ncol=fir_yeare-fir_year+1, dimnames=list(c(month.name),fir_year:fir_yeare))
    con_doub_litA <- matrix(data=NA, nrow=12, ncol=fir_yeare-fir_year+1, dimnames=list(c(month.name),fir_year:fir_yeare))
    My_nor_litA   <- matrix(data=NA, nrow=12, ncol=fir_yeare-fir_year+1, dimnames=list(c(month.name),fir_year:fir_yeare))
    My_no_litA    <- matrix(data=NA, nrow=12, ncol=fir_yeare-fir_year+1, dimnames=list(c(month.name),fir_year:fir_yeare))
    My_doub_litA  <- matrix(data=NA, nrow=12, ncol=fir_yeare-fir_year+1, dimnames=list(c(month.name),fir_year:fir_yeare))
    So_nor_litA   <- matrix(data=NA, nrow=12, ncol=fir_yeare-fir_year+1, dimnames=list(c(month.name),fir_year:fir_yeare))
    So_no_litA    <- matrix(data=NA, nrow=12, ncol=fir_yeare-fir_year+1, dimnames=list(c(month.name),fir_year:fir_yeare))
    So_doub_litA  <- matrix(data=NA, nrow=12, ncol=fir_yeare-fir_year+1, dimnames=list(c(month.name),fir_year:fir_yeare))
    
    con_nor_litAstd  <- matrix(data=NA, nrow=12, ncol=fir_yeare-fir_year+1, dimnames=list(c(month.name),fir_year:fir_yeare))
    con_no_litAstd   <- matrix(data=NA, nrow=12, ncol=fir_yeare-fir_year+1, dimnames=list(c(month.name),fir_year:fir_yeare))
    con_doub_litAstd <- matrix(data=NA, nrow=12, ncol=fir_yeare-fir_year+1, dimnames=list(c(month.name),fir_year:fir_yeare))
    My_nor_litAstd   <- matrix(data=NA, nrow=12, ncol=fir_yeare-fir_year+1, dimnames=list(c(month.name),fir_year:fir_yeare))
    My_no_litAstd    <- matrix(data=NA, nrow=12, ncol=fir_yeare-fir_year+1, dimnames=list(c(month.name),fir_year:fir_yeare))
    My_doub_litAstd  <- matrix(data=NA, nrow=12, ncol=fir_yeare-fir_year+1, dimnames=list(c(month.name),fir_year:fir_yeare))
    So_nor_litAstd   <- matrix(data=NA, nrow=12, ncol=fir_yeare-fir_year+1, dimnames=list(c(month.name),fir_year:fir_yeare))
    So_no_litAstd    <- matrix(data=NA, nrow=12, ncol=fir_yeare-fir_year+1, dimnames=list(c(month.name),fir_year:fir_yeare))
    So_doub_litAstd  <- matrix(data=NA, nrow=12, ncol=fir_yeare-fir_year+1, dimnames=list(c(month.name),fir_year:fir_yeare))
    
  } else if (partitioningoption == 2) {
    
    con_lit  <- matrix(data=NA, nrow=12, ncol=fir_yeare-fir_year+1, dimnames=list(c(month.name),fir_year:fir_yeare))
    S1       <- matrix(data=NA, nrow=12, ncol=fir_yeare-fir_year+1, dimnames=list(c(month.name),fir_year:fir_yeare))
    S2       <- matrix(data=NA, nrow=12, ncol=fir_yeare-fir_year+1, dimnames=list(c(month.name),fir_year:fir_yeare))
    S3       <- matrix(data=NA, nrow=12, ncol=fir_yeare-fir_year+1, dimnames=list(c(month.name),fir_year:fir_yeare))
    S1std    <- matrix(data=NA, nrow=12, ncol=fir_yeare-fir_year+1, dimnames=list(c(month.name),fir_year:fir_yeare)) 
    S2std    <- matrix(data=NA, nrow=12, ncol=fir_yeare-fir_year+1, dimnames=list(c(month.name),fir_year:fir_yeare))
    S3std    <- matrix(data=NA, nrow=12, ncol=fir_yeare-fir_year+1, dimnames=list(c(month.name),fir_year:fir_yeare))
    
  }
  
  
  # average fluxes by month for total soil respiration
  n=1
  for (j in fir_year:fir_yeare) {
    m=1
    for (i in 1:12) { 
      
      # calculate average fluxes by month for total soil respiration:
      ind = which(montht==i & yeart==j)
      resAt[m,n] = mean(RcAt[ind],na.rm=T) # averages the 25 vales for that month and that year.
      resAtstd[m,n] = sd(RcAt[ind],na.rm=T)
      
      # calculate average fluxes by month for control soil respiration: (for disturbed and non-disturbed plots)
      im = which(monthc==i & yearc==j)
      xx = RccA[im]                      # All current flux data we have is allocated to this new variable, xx
      if (partitioningoption==1) {
        rcanotper[m,n] <- mean(xx[which(distc[im]=="UD")], na.rm=T)  ## not disturbed
        rcaper[m,n]    <- mean(xx[which(distc[im]=="D")], na.rm=T)   ## disturbed
        DCdAstd[m,n]   <- sd(RccA[im], na.rm=T)
        
      }
      else if (partitioningoption==2) {
        # Here comes the data by disturbed or not: (i.e. rca not perturbed or rca perturbed?) 
        rcanotper[m,n] <- mean(xx[which(distc[im]==1)], na.rm=T)  # disturbance == 1 is undisturbed # just change to "D" and "U" in database and code becomes rcanotper[m,n] <- mean(xx[which(distc=="U")],na.rm=T)
        rcaper[m,n] <- mean(xx[which(distc[im]==2)], na.rm=T)  # disturbance == 2 is disturbed
      }
      
      # calculate average fluxes by month for different partitioning treatments: 
      
      imp = which(monthp == i & yearp == j) # imp is an index to get current year & current month
      
      if (partitioningoption == 1) {
        xx = RcpA[imp]                
        nx = treatmentp[imp]
        if (sum(xx, na.rm = T) > 0) {
          
          ix1 = which(nx == "con_nor_lit")
          ix2 = which(nx == "con_no_lit")
          ix3 = which(nx == "con_doub_lit")
          ix4 = which(nx == "my_nor_lit")
          ix5 = which(nx == "my_no_lit")
          ix6 = which(nx == "my_doub_lit")
          ix7 = which(nx == "so_nor_lit")
          ix8 = which(nx == "so_no_lit")
          ix9 = which(nx == "so_doub_lit")
          
          con_nor_litA[m,n]  = mean(xx[ix1], na.rm=T) # Control - normal litterfall
          con_no_litA[m,n]   = mean(xx[ix2], na.rm=T) # Control - no litterfall
          con_doub_litA[m,n] = mean(xx[ix3], na.rm=T) # Control - double litterfall
          My_nor_litA[m,n]   = mean(xx[ix4], na.rm=T) # Mycorrhizae - normal litterfall
          My_no_litA[m,n]    = mean(xx[ix5], na.rm=T) # Mycorrhizae - no litterfall
          My_doub_litA[m,n]  = mean(xx[ix6], na.rm=T) # Mycorrhizae - double litterfall
          So_nor_litA[m,n]   = mean(xx[ix7], na.rm=T) # Soil - normal litterfall
          So_no_litA[m,n]    = mean(xx[ix8], na.rm=T) # Soil - no litterfall
          So_doub_litA[m,n]  = mean(xx[ix9], na.rm=T) # Soil - double litterfall
          
          con_nor_litAstd[m,n]  = sd(xx[ix1], na.rm=T)
          con_no_litAstd[m,n]   = sd(xx[ix2], na.rm=T)  
          con_doub_litAstd[m,n] = sd(xx[ix3], na.rm=T)
          My_nor_litAstd[m,n]   = sd(xx[ix4], na.rm=T)  
          My_no_litAstd[m,n]    = sd(xx[ix5], na.rm=T)  
          My_doub_litAstd[m,n]  = sd(xx[ix6], na.rm=T) 
          So_nor_litAstd[m,n]   = sd(xx[ix7], na.rm=T)  
          So_no_litAstd[m,n]    = sd(xx[ix8], na.rm=T)   
          So_doub_litAstd[m,n]  = sd(xx[ix9], na.rm=T) 
          
        } 
        else {
          
          #  con_nor_litA[m,n]  = NA
          #  con_no_litA[m,n]   = NA
          #  con_doub_litA[m,n] = NA
          #  My_nor_litA[m,n]   = NA
          #  My_no_litA[m,n]    = NA
          #  My_doub_litA[m,n]  = NA
          #  So_nor_litA[m,n]   = NA
          #  So_no_litA[m,n]    = NA
          #  So_doub_litA[m,n]  = NA
          
          #  con_nor_litAstd[m,n]  = NA
          #  con_no_litAstd[m,n]   = NA   
          #  con_doub_litAstd[m,n] = NA 
          #  My_nor_litAstd[m,n]   = NA  
          #  My_no_litAstd[m,n]    = NA  
          #  My_doub_litAstd[m,n]  = NA  
          #  So_nor_litAstd[m,n]   = NA  
          #  So_no_litAstd[m,n]    = NA  
          #  So_doub_litAstd[m,n]  = NA
        }
      }
      else if (partitioningoption == 2) {
        xx = RcpA[imp]           
        nx = treatmentp[imp]
        if (sum(xx, na.rm = T) > 0) {
          ix1 = which(nx == 1)  # Control
          ix2 = which(nx == 2)  # No Litter
          ix3 = which(nx == 3)  # No Litter/ No Roots
          ix4 = which(nx == 4)  # No Litter/ Root + Mycorrhizae Exclusion
          
          con_lit[m,n] = mean(xx[ix1],na.rm = T) # Control
          S1[m,n] = mean(xx[ix2], na.rm=T) # No Litter
          S2[m,n] = mean(xx[ix3], na.rm=T) # No Litter/No Roots
          S3[m,n] = mean(xx[ix4], na.rm=T) # No Litter/ Root + Mycorrhizae Exclusion
          
        }
        else {
          
          con_lit[m,n] = NA # Control
          S1[m,n] = NA # No Litter
          S2[m,n] = NA # No Litter/No Roots
          S3[m,n] = NA # No Litter/ Root + Mycorrhizae Exclusion
          
          ############################################################
          ############ TO DO: Add sd for these too.###################
          ############################################################
        }
      }
      m=m+1
    }
    n=n+1
  }
  
  
  ### Unit conversions:
  # convert from umol m-2 s-1 to MgC ha month
  # convert units umol m2s-1 to MgC ha month = 1mo=2592000sec, 10000m2=1ha,
  # 1000000umol = 1 mol, 1mol = 12 g, 1000000g=1Mg
  convert = (2592000*10000*12)/(1000000*1000000)
  DCdA = (rcaper)*convert     # average disturbed soil cores
  DCudA = (rcanotper)*convert # average undisturbed soil cores
  
  
  #  estimation of the relative contributions of (1) surface organic litter, (2) roots, (3) mycorrhizae and (4) soil organic matter to total soil respiration
  # add a temperature correction from Sotta et al 2004 Q10=1.8 and k=0.0613
  corrsresA = exp(-0.0695*(1))
  
  
  
  
  # correct for disturbance of soil: First look at yearly trend to see if the
  # disturbance effect persists.  If so average for all three years
  ## ORIGINAL VERSION:
  #discorA = mean(colMeans(DCudA[,2:(dim(DCudA)[2])] - DCdA[,2:(dim(DCudA)[2])],na.rm=T),na.rm=T)
  #discorAstd = mean(colMeans(DCdAstd[,2:(dim(DCdAstd)[2])],na.rm=T),na.rm=T);
  
  discorA = mean(colMeans(DCudA - DCdA, na.rm=T), na.rm=T)
  discorAstd = sqrt(DCudA^2 + DCdAstd^2) # !!!!! GET DCudAstd !!!!!!!!!!!
  
  
  if (partitioningoption == 1) {
    ## respiration under three different treatments: control, no litter and double litter.
    rrA1 = ((con_no_litA-(So_no_litA+discorA))/(con_no_litA))
    #rrA1[which(rrA1 < 0)] <- NA
    rrA1[which(rrA1>1)] = NA
    
    rrA2 = ((con_nor_litA-(So_nor_litA+discorA))/(con_nor_litA))
    #rrA2[which(rrA2 < 0)] <- NA
    rrA2[which(rrA2>1)] = NA
    
    rrA3 = ((con_doub_litA-(So_doub_litA+discorA))/(con_doub_litA))
    #rrA3[which(rrA3<0)] <- NA
    rrA3[which(rrA3>1)] <- NA
    
    rrxc = dim(rrA1)
    rrA    <- matrix(data = NA, nrow = rrxc[1], ncol = rrxc[2])
    rrAstd <- matrix(data = NA, nrow = rrxc[1], ncol = rrxc[2])
    for (i in 1:rrxc[1]) {
      for (j in 1:rrxc[2]) {
        rrA[i,j]    = mean(c(rrA1[i,j],rrA2[i,j],rrA3[i,j]),na.rm=T)    
        rrAstd[i,j] = sd(c(rrA1[i,j],rrA2[i,j],rrA3[i,j]),na.rm=T)    
      }
    } 
    rrA[which(rrA>1)] = NA
  } else if (partitioningoption == 2) {
    rrA = ((S1-(S3+discorA))/S1) # Check that this is what the new code does.
    #rrA[which(rrA > 1)] = NA
    # rrAstd = ....
  }
  
  
  ## autotrophic root respiration:
  rrtotresAc    = totresAc*rrA
  rrtotresAcstd = (totresAcstd*rrA)/sqrt(25) # should be more generic - length(subplot)
  
  ## heterotrophic respiration:
  hrtotresAc    = totresAc*(1-rrA)
  hrtotresAcstd = (totresAcstd*(1-rrA))/sqrt(25)
  
  ### NEW BIT FOR DAN's PAPER ################## I AM HERE##################### 
  
  # root respiration Ts ??? (Tdm ??? D)
  #rrA1 <- ((con_nor_litA + con_no_litA + con_doub_litA) / 3) - (((My_nor_litA + My_no_litA + My_doub_litA) / 3) + discorA)
  #rrA1std <- sqrt(((con_nor_litAstd^2) + (con_no_litAstd^2) + (con_doub_litAstd^2)) / 3 + ((My_nor_litAstd^2) + (My_no_litAstd^2) + (My_doub_litAstd^2)) / 3 + (discorAstd^2))
  
  # set thresholds
  rrA1[which(rrA1 < (-0.5))] = NA ### CHECK THIS THRESHOLD IS RIGHT?
  rrA1[which(rrA1 > 3)] = NA
  
  # Mycorrhizal respiration = (Tdm + D) ??? (Tds + D)
  MrA1 <- (((My_nor_litA + My_no_litA + My_doub_litA) / 3) + discorA) - (((So_nor_litA + So_no_litA + So_doub_litA)/3) + discorA)
  MrA1std <- sqrt((My_nor_litAstd^2) + (My_no_litAstd^2) + (My_doub_litAstd^2) + (discorAstd^2) + (So_nor_litAstd^2) + (So_no_litAstd^2) + (So_doub_litAstd^2) + (discorAstd^2))
  
  # set thresholds
  MrA1[which(MrA1 < (-0.5))] = NA
  MrA1[which(MrA1 > 3)] = NA
  
  # Soil organic matter respiration = Tds + D
  OMrA1 <- (((So_nor_litA + So_no_litA + So_doub_litA) / 3) + discorA)
  OMrA1std <- sqrt(((So_nor_litAstd^2) + (So_no_litAstd^2) + (So_doub_litAstd^2)) / 3 + (discorAstd^2))
  
  # set thresholds
  OMrA1[which(OMrA1 < (-0.5))] = NA
  OMrA1[which(OMrA1 > 3)] = NA
  
  # Effects of litter addition on litter respiration are assessed by calculating this component twice ??? from double # litter (Rldl) and no-litter (Rlnl) compared to ambient litter tubes:
  # Rlnl = (Tdsal + D) ??? (Tdsnl + D)
  RlnlA <- (So_nor_litA + discorA) - (So_no_litA + discorA)
  RlnlAstd <- sqrt((So_nor_litAstd^2) + (So_no_litAstd^2) + (discorAstd^2))
  
  # Rldl = (Tdsdl + D) ??? (Tdsal + D)
  RldlA <- (So_doub_litA + discorA) - (So_nor_litA + discorA)
  RldlAstd <- sqrt((So_nor_litAstd^2) + (So_no_litAstd^2) + (discorAstd^2))
  
  ###################################################################################################
  
  # fill gaps
  rrAfg = colMeans(t(rrA), na.rm=T)
  
  for (i in 1:12) {
    for (j in 1:(fir_yeare-fir_year+1)) {
      if (is.na(rrA[i,j]) & !is.na(totresAc[i,j])) {
        if (!is.na(rrAfg[i])) {
          rrtotresAc[i,j]    = totresAc[i,j]*rrAfg[i]
          rrtotresAcstd[i,j] = (totresAcstd[i,j]*rrAfg[i])
          hrtotresAc[i,j]    = totresAc[i,j]*(1-rrAfg[i])
          hrtotresAcstd[i,j] = totresAcstd[i,j]*(1-rrAfg[i])
        } else if (i == 12) {
          rrtotresAc[i,j]    = totresAc[i,j]*rrAfg[i-1]
          rrtotresAcstd[i,j] = (totresAcstd[i,j]*rrAfg[i-1])
          hrtotresAc[i,j]    = totresAc[i,j]*(1-rrAfg[i-1])
          hrtotresAcstd[i,j] = totresAcstd[i,j]*(1-rrAfg[i-1])
        } else {
          rrtotresAc[i,j]    = totresAc[i,j]*rrAfg[i+1]
          rrtotresAcstd[i,j] = (totresAcstd[i,j]*rrAfg[i+1])
          hrtotresAc[i,j]    = totresAc[i,j]*(1-rrAfg[i+1])
          hrtotresAcstd[i,j] = totresAcstd[i,j]*(1-rrAfg[i+1])
        }
      }    
    }
  }
  
  
  ### Relevant Data Output: totres, hrtotres
  ###  Build data frame with time series structure
  
  ##Restructure the data (according to time series structure):
  Year  <- NULL
  Month <- NULL
  Day   <- NULL
  
  for (i in 1:dim(totresAc)[2]) {
    Year[((i-1)*12+1):((i-1)*12+12)]  <- (rep(c(fir_year:fir_yeare)[i],12))
    Month[((i-1)*12+1):((i-1)*12+12)] <- (1:12)
    Day[((i-1)*12+1):((i-1)*12+12)]   <- rep(NA,12)
  }
  
  soilresp.data.monthly.ts <- data.frame(Year,Month,Day,
                                         c(rrtotresAc), c(rrtotresAcstd),
                                         c(hrtotresAc), c(hrtotresAcstd))
                                         #c(rrA1), 
                                         #c(MrA1), 
                                         #c(OMrA1), 
                                         #c(RlnlA), 
                                         #c(RldlA))
  
  colnames(soilresp.data.monthly.ts) <- c("year","month","day",  
                                          "auto_totres_MgC_ha_mo","auto_totres_std",
                                          "hetero_totres_MgC_ha_mo","hetero_totres_std")
                                          #"root_res_MgC_ha_mo",
                                          #"mycorrhizal_res_MgC_ha_mo", 
                                          #"soil_organic_matter_res_MgC_ha_mo", 
                                          #"som_nolitter_res_MgC_ha_mo", 
                                          #"som_doublelitter_res_MgC_ha_mo")
  
  
  ## Plotroutine, triggered by argument 'plotit=T'
  if (plotit == T) {
    ## Time representation of Dates as character in the vector 'dates':
    dates <- c()
    for (i in 1:length(Year)) {
      dates[i] <- as.character(strptime(paste(as.character(Year[i]),as.character(Month[i]),as.character(15),sep="-"),
                                        format="%Y-%m-%d"))
    }
    
    x11()
    par(mfrow = c(2,1))
    par(mar = c(4,4,0.5,0.5))
    plot(x = strptime(dates, format = "%Y-%m-%d"), y = soilresp.data.monthly.ts$rrtotresAc, 
         type = 'l',lwd = 2,
         xlab = "Years", ylab = "Total Respiration [Units]")
    
    plot(x = strptime(dates, format = "%Y-%m-%d"), y = soilresp.data.monthly.ts$hrtotresAc, 
         type = 'l',lty=1,
         xlab = "Years", ylab="Heterotrophic Soil Respiration [Units]")
  }
  
  # Get values for each tube rather than average
  
  # Return either monthly means (ret="monthly.means") as time series or matrix  
  switch(ret,
         monthly.means.matrix = {return(soilresp.data.monthly.matrix)},
         monthly.means.ts     = {return(soilresp.data.monthly.ts)}
  )
  
}
