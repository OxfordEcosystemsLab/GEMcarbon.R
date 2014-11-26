### Function Soil respiration:
# This function calculates soil respiration and uses input data specified in the RAINFOR-GEM manual.
# Last edited: Cecile Girardin, 01.07.2014 

### Required Data:
# data.frame of total soil respiration
# data.frame of partition respiration 
# data.frame of control respiration 
# plotname: specify plotname of which plot should be calculated (eg. 1,2,etc)
# ret: data-format of return values: "monthly.means.ts" or "monthly.means.matrix"
# plotit: logical, plot a quick graphical summary of the data?

### COMMENT BY SEBASTIAN: I changed the order of the elements of the code:
## In this version, first the assignment of variables is done, in order to check for the lowest year value (in the
## overall data), in order to find start and end years

### read data for option 1:
setwd("C:/Users/Cecile/Documents/GitHub/GEMcarbon.R/example files/outputs")
data.resc <- read.table("Res_control_test1.csv", sep=",", header=T)
data.resp <- read.table("Res_partitionning_test1.csv", sep=",", header=T)
data.rest <- read.table("Res_total_test1.csv", sep=",", header=T)
pressure = 1013.25
plotname = "ACJ"
partitioningoption = 1
elevation = "default"
T_ambient="Default"

### read data for option 2:
setwd("C:/Users/Cecile/Dropbox/Carbon_Use_Efficieny_R/testing/soilresp")

data.resc <- read.table("Resconallsam.csv", sep=",", header=T)
data.resp <- read.table("Resparallsam.csv", sep=",", header=T)
data.rest <- read.table("Restotallsam.csv", sep=",", header=T)
pressure = 1013.25
plotname = 1.1
partitioningoption = 2
elevation = "Default"
pressure="Default"
T_ambient="Default"

# read correction functions:
source("C:/Users/Cecile/Documents/GitHub/GEMcarbon.R/soilrespiration_auxfunctions.R")


soilrespiration <- function(data.rest,data.resp,data.resc, plotname, ret="monthly.means.ts", # Add tube radius as a parameter, change A to A <- pi*(rad^2) 
                            partitioningoption="Default",
                            pressure="Default", elevation="Default", T_ambient="Default",
                                                        plotit=F) {
# User has to specify either elevation or pressure. 
### CONTROL FLAGS
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
  
  
{
  
### Total soil respiration
### 25 tubes measured every month, record soil moisture and temp in every tube
## Column names:
  #  plot (1=BolA, 2=BolB, 3=Iq1, 4=Iq2, 5=TangA, 6=TangB)  
  #  year  
  #  month  
  #  temperature : temperature in C	
  #  depth: chamber height	
  #  fluxt 
  
plott = data.rest$plot_code   # 1=BolA, 2=BolB, 3=Iq1, 4=Iq2, 5=TangA, 6=TangB  
yeart = data.rest$year[which(plott==plotname)]
montht = data.rest$month[which(plott==plotname)]
tempt = data.rest$soil_temp_degC[which(plott==plotname)]
cht = data.rest$collar_depth_cm[which(plott==plotname)]     ## depth (cm)
fluxt = data.rest$flux_umolco2_m2_s[which(plott==plotname)]    ## This is to determine overall outliers

# Control

# insert 5 40cm tubes in the soil, excavate soil cores
# 13cm diameter by 35cm depth, mix soil but don't remove roots

#  plot 
#  year  
#  month  
#  temperature : temperature in C
#  depth
#  flux                  

plotc = data.resc$plot_code   
yearc = data.resc$year[which(plotc==plotname)]
monthc = data.resc$month[which(plotc==plotname)]
tempc = data.resc$soil_temp_degC[which(plotc==plotname)]
chc = data.resc$collar_depth_cm[which(plotc==plotname)] 
fluxc = data.resc$flux_umolco2_m2_s[which(plotc==plotname)]
distc = data.resc$disturbance_code[which(plotc==plotname)]

#  Partitioning components of soil respiration
#  4 groups of 9 tubes per plot (36 tubes in total per plot).
#  plot
#  year
#  month	
#  nump: option 1, partitioning treatment
#  area: option 2, partitioning treatment ("area")
#  temperature : temperature in C
#  depth: chamber_height
#  fluxp

plotp = data.resp$plot_code 
yearp = data.resp$year[which(plotp==plotname)]
monthp = data.resp$month[which(plotp==plotname)]
treatmentp = data.resp$treatment_code[which(plotp==plotname)] # CHECK THIS IS RIGHT?
tempp = data.resp$soil_temp_degC[which(plotp==plotname)]
chp = data.resp$collar_depth_cm[which(plotp==plotname)]  
fluxp = data.resp$flux_umolco2_m2_s[which(plotp==plotname)] 

### Defaults for chamber volume and tube area:
# The CPY-2 defaults are Volume = 2465 cm3  Area = 170 cm2 and V/A = 1450
Vd = 1171/1000000    # chamber volume m3
A = 0.0078           # tube area m2
fir_year = min(c(yeart,yearp,yearc),na.rm=T)  # to know from when to when the data should be calculated for.
fir_yeare = max(c(yeart,yearp,yearc),na.rm=T) # fir_yeare means last year.
}

########################################## Should we have this in EGM_to_db code rather than here?

## Total Soil respiration Calculation: 
    # remove outliers and NAs: Fluxes based on overall correction, ## Temperature and chamber correction: Temperature and Chamber height (see functions!)
    # the choice of the sd_interval changes things!    

    fluxt <- rm.flux.outlier(fluxt, sd_interval=2) # IS this really defining SD? check in aux-functions
##########################################
##########################################  PROBLEM HERE !!!              tempt <- rm.temp.outlier(temp=tempt, month=montht) ############################
##########################################
    cht   <- rm.ch.outlier(ch=cht) # this doesn't remove outlyers, it replaces missing data with the average of chamber deapth measurements for the whole plot.
    
    ## Perform chamber and flux correction (Metcalfe 2009), see function fluxcorr. 
    # chamber volume correction according to Metcalfe et al (2009): Rainfor Manual Appendix II, page 75. 
    RcAt <- fluxcorr(flux=fluxt, temp=tempt, ch=cht, Vd=Vd, A=A, pressure=pressure)

### Control Measurements 

    # remove outliers (> 3 SD) and NAs:
    fluxc <- rm.flux.outlier(fluxc, sd_interval=2) # Discuss with team: it makes a big difference to the data if you use 2 sd or 3sd.
    tempc <- rm.temp.outlier(temp = tempc, month=monthc)
    chc   <- rm.ch.outlier(ch=chc)
    
    ## Flux correction according to Metcalfe, RAINFOR Manual, Appendix 2, p. 75
    RccA <- fluxcorr(flux=fluxc, temp=tempc, ch=chc, Vd=Vd, A=A, pressure=pressure)


#### Partitioning Soil Repiration calculation 
    # remove outliers and NAs: Flux (sd > 3), Temperature and Chamber height (see soilrespiration_aux-functions.R)
    fluxp <- rm.flux.outlier(fluxp, sd_interval=2)
    tempp <- rm.temp.outlier(temp=tempp, month=monthp)
    chc   <- rm.ch.outlier(ch=chc)
    
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

################################
###### TO DO: plot data per tube & treatment to identify outlyers.
################################

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
    
    # calculate for total soil respiration:
      ind = which(montht==i & yeart==j)  
      resAt[m,n] = mean(RcAt[ind],na.rm=T) # averages the 25 vales for that month and that year.
      resAtstd[m,n] = sd(RcAt[ind],na.rm=T)

    # calculate for control soil respiration: (for disturbed and non-disturbed plots)
       im = which(monthc==i & yearc==j)
        xx = RccA[im]                      # All current flux data we have is allocated to this new variable, xx
      if (partitioningoption==1) {
           if (length(xx)==10) { # this only works with data that is structured like this, but we need to change this to a logical, as is done in partitionning option 2.
            rcanotper[m,n] = mean(c(xx[1],xx[3], xx[5], xx[7], xx[9]),na.rm=T)  ## not disturbed
            rcaper[m,n] = mean(c(xx[2],xx[4], xx[6], xx[8], xx[10]),na.rm=T)    ## disturbed
            DCdAstd[m,n] = sd(c(xx[1]-xx[2],xx[3]-xx[4],xx[5]-xx[6],xx[7]-xx[8],xx[9]-xx[10]),na.rm=T)
          } else {
            rcanotper[m,n] = NA
            rcaper[m,n] = NA
          }
      } else if (partitioningoption==2) {
        # Here comes the data by disturbed or not: (i.e. rca not perturbed or rca perturbed?) 
        rcanotper[m,n] <- mean(xx[which(distc[im]==1)],na.rm=T)  # disturbance == 1 is undisturbed # just change to "D" and "U" in database and code becomes rcanotper[m,n] <- mean(xx[which(distc=="U")],na.rm=T)
        rcaper[m,n] <- mean(xx[which(distc[im]==2)], na.rm=T)  # disturbance == 2 is disturbed
      }
    
    # calculate for different partitioning treatments:
      imp = which(monthp == i & yearp == j) # imp is an index to get current year & current month
      if (partitioningoption == 1) {
          xx = RcpA[imp]                
          nx = treatmentp[imp]
                              
          if (sum(xx, na.rm = T) > 0) {
            ix1 = which(nx == 1)
            ix2 = which(nx == 2)
            ix3 = which(nx == 3)
            ix4 = which(nx == 4)
            ix5 = which(nx == 5)
            ix6 = which(nx == 6)
            ix7 = which(nx == 7)
            ix8 = which(nx == 8)
            ix9 = which(nx == 9)
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
          } else {
            con_nor_litA[m,n]  = NA
            con_no_litA[m,n]   = NA
            con_doub_litA[m,n] = NA
            My_nor_litA[m,n]   = NA
            My_no_litA[m,n]    = NA
            My_doub_litA[m,n]  = NA
            So_nor_litA[m,n]   = NA
            So_no_litA[m,n]    = NA
            So_doub_litA[m,n]  = NA
            
            con_nor_litAstd[m,n]  = NA
            con_no_litAstd[m,n]   = NA   
            con_doub_litAstd[m,n] = NA 
            My_nor_litAstd[m,n]   = NA  
            My_no_litAstd[m,n]    = NA  
            My_doub_litAstd[m,n]  = NA  
            So_nor_litAstd[m,n]   = NA  
            So_no_litAstd[m,n]    = NA  
            So_doub_litAstd[m,n]  = NA  
          }
      } else if (partitioningoption == 2) {
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
              
            } else {
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


    # convert from umol m-2 s-1 to MgC ha month
    # convert units umol m2s-1 to MgC ha month = 1mo=2592000sec, 10000m2=1ha,
    # 1000000umol = 1 mol, 1mol = 12 g, 1000000g=1Mg
    convert = (2592000*10000*12)/(1000000*1000000)
    totresAc = resAt*convert*corrsresA
    totresAcstd = resAtstd*convert*corrsresA


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

################################ NEW BIT I AM WORKING ON ##########################################

# root respiration Ts ??? (Tdm ??? D)
rrA1 <- ((con_nor_litA + con_no_litA + con_doub_litA) / 3) - (((My_nor_litA + My_no_litA + My_doub_litA) / 3) + discorA)
rrA1std <- sqrt(((con_nor_litAstd^2) + (con_no_litAstd^2) + (con_doub_litAstd^2)) / 3 + ((My_nor_litAstd^2) + (My_no_litAstd^2) + (My_doub_litAstd^2)) / 3 + (discorAstd^2))

  
################## I AM HERE: STD FOR EACH NEW PARAMETER #####################
  
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
{
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
                                    c(hrtotresAc), c(hrtotresAcstd),
                                    c(rrA1), c(),
                                    c(MrA1), C(),
                                    c(OMrA1), c(),
                                    c(RlnlA), c(),
                                    c(RldlA), c())
  
  "root_res_MgC_ha_mo", 
  "mycorrhizal_res_MgC_ha_mo", 
  "soil_organic_matter_res_MgC_ha_mo", 
  "som_nolitter_res_MgC_ha_mo", 
  "som_doublelitter_res_MgC_ha_mo",
    
  colnames(soilresp.data.monthly.ts) <- c("year","month","day",  
                                     "auto_totres_MgC_ha_mo","auto_totres_std",
                                     "hetero_totres_MgC_ha_mo","hetero_totres_std")
}
 

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