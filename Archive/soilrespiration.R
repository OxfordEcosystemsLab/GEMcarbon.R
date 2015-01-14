### Function Soil respiration:
# This function calculates soil respiration and uses input data specified in the RAINFOR-GEM manual.
# Last edited: Cécile Girardin, 01.07.2014 

### Required Data:
# data.frame of total soil respiration
# data.frame of partition respiration 
# data.frame of control respiration 
# plotname: specify the plot_code you need your estimates for.
# ret: specify the data-format of return values this is either "monthly.means.ts" or "monthly.means.matrix"
# plotit: logical, plotit=T to plot a quick graphical summary of the data. 

### read data for option 2:
setwd("C:/Users/Cecile/Dropbox/Carbon_Use_Efficieny_R/testing/soilresp")

data.resc <- read.table("Resconallsam.csv", sep=",", header=T)
data.resp <- read.table("Resparallsam.csv", sep=",", header=T)
data.rest <- read.table("Restotallsam.csv", sep=",", header=T)

# read correction functions:
#source("D:/Dokumente/My Dropbox/Carbon_Use_Efficieny_R/R-testscripts(v4)/soilrespiration_aux-functions.R")
source("C:/Users/Cecile/Dropbox/Carbon_Use_Efficieny_R/R-testscripts(v4)/soilrespiration_aux-functions.R")

## here comes a script to test the inclusion of partitioning option 2:

print("WARNING! Neither ambient pressure nor site elevation was specified")
print("Calculations will be based on p=1013.25 hPa (sea level) and
        temperature-independent barometric equation!")
pressure <- 1013.25
plotname = 1.1
partitioningoption = 2
elevation = "default"


### data for option 1:
library(R.matlab)
setwd("C:/Users/Cecile/Dropbox/Carbon_Use_Efficieny_R/Original-matlab-code/Ken_Iq-Tang")
#setwd("D:/Dokumente/My Dropbox/Carbon_Use_Efficieny_R/Original-matlab-code/Ken_Iq-Tang")
dir()
Dataall <- readMat(con="Dataall.mat")
pressure <- 1013.25
plotname = 1
partitioningoption = 1

## required data:
data.rest = data.frame(Dataall$Restotall)   # total respiration
data.resp = data.frame(Dataall$Resparall)   # partion respiration 
data.resc = data.frame(Dataall$Resconall)   # control respiration
## adjust colnames for Chris' matlab data:
colnames(data.rest) <- c("year", "month", "plot", "area","co2","temperature","water_content","depth","flux")
colnames(data.resc) <- c("year", "month", "plot", "area", "disturbance", "co2", "temperature", "water_content", "depth", "flux")
colnames(data.resp) <- c("year", "month", "plot", "area", "co2", "temperature", "water_content", "depth", "flux")

#Activate this code to use as a function:
soilrespiration <- function(data.rest,data.resp,data.resc, plotname, ret="monthly.means.ts", # Add tube radius as a parameter, change A to A <- pi*(rad^2) 
                            partitioningoption="Default",
                            pressure="Default", elevation="Default", T_ambient="Default",
                                                        plotit=F) {
# User has to specify either elevation or pressure. 

### CONTROL FLAGS
  if (partitioningoption=="Default") {
    print("Warning! No partitioning option (see RAINFOR manual, p. 56) was specified!")
    print("Please specify the variable 'partitioningoption' in the function call!")
    partitioningoption=1
  }
  
  if (partitioningoption==2) {
    print("Code is running on partitioning option 2!")
  }
  
  if (partitioningoption==1) {
    print("Code is running on partitioning option 1 (see RAINFOR manual, p. 56")
  }
  
  if (pressure=="Default" & elevation=="Default" & T_ambient=="Default") {
    print("WARNING! Neither ambient pressure nor site elevation was specified")
    print("Calculations will be based on p=1013.25 hPa (sea level) and
          temperature-independent barometric equation!")
    pressure <- 1013.25
  }
  
  if (pressure!="Default") {
    print("Ambient pressure was specified and will be used for flux correction")
  }
  
  if (pressure=="Default" & elevation!="Default" & T_ambient=="Default") {
    print("Ambient pressure and temperature was not specified. Ambient pressure for flux correction is calculated from elevation
          using the temperature-independent barometric equation.")
    pressure <- barometric_equation(elevation)
  }
  
  if (pressure=="Default" & elevation!="Default" & T_ambient!="Default") {
    print("Ambient pressure was not specified. Ambient pressure for flux correction is calculated from elevation
           and ambient temperature (in °C!) using the temperature-dependent barometric equation.")
    pressure <- barometric_equation_T(elevation, T_ambient)
  }
  
  
  #### Data READ-IN: line 20 - line 98; and fix constants (first and last year; Vd And A)
{
### Total soil respiration
### 25 tubes measured every month, record soil moisture and temp in every tube
## Column names are the following:
  
  #  plot (1=BolA, 2=BolB, 3=Iq1, 4=Iq2, 5=TangA, 6=TangB)  
  #  year  
  #  month  
  #  temperature : temperature in C	
  #  depth: chamber height	
  #  fluxt #  data.rest should be a dataframe

plott = data.rest$plot   # 1=BolA, 2=BolB, 3=Iq1, 4=Iq2, 5=TangA, 6=TangB  
yeart = data.rest$year[which(plott==plotname)]
montht = data.rest$month[which(plott==plotname)]
tempt = data.rest$temperature[which(plott==plotname)]
cht = data.rest$depth[which(plott==plotname)]     ## depth (cm)
fluxt = data.rest$flux[which(plott==plotname)]    ## This is to determine overall outliers

# Control

# insert 5 40cm tubes in the soil, excavate soil cores
# 13cm diameter by 35cm depth, mix soil but don't remove roots

#  plot 
#  year  
#  month  
#  temperature : temperature in C
#  depth
#  flux                  

plotc = data.resc$plot   
yearc = data.resc$year[which(plotc==plotname)]
monthc = data.resc$month[which(plotc==plotname)]
tempc = data.resc$temperature[which(plotc==plotname)]
chc = data.resc$depth[which(plotc==plotname)] ############# WHAT is chc??? depth??? check this!!!!
fluxc = data.resc$flux[which(plotc==plotname)]
distc = data.resc$disturbance[which(plotc==plotname)]

#  Partitioning components of soil respiration
#  4 groups of 9 tubes per plot (36 tubes in total per plot).
#  
#  plot
#  year
#  month	
#  nump: option 1, partitioning treatment
#  area: option 2, partitioning treatment ("area")
#  temperature : temperature in C
#  depth: chamber_height
#  fluxp

plotp = data.resp$plot 
yearp = data.resp$year[which(plotp==plotname)]
monthp = data.resp$month[which(plotp==plotname)]
treatmentp = data.resp$area[which(plotp==plotname)]#A=1 B=2
tempp = data.resp$temperature[which(plotp==plotname)]
chp = data.resp$depth[which(plotp==plotname)]  # ch is height of the collar - rename?
fluxp = data.resp$flux[which(plotp==plotname)] # overall fluxes

### Defaults for chamber volume and tube area:
# The CPY-2 defaults are Volume = 2465 cm3  Area = 170 cm2 and V/A = 1450
Vd = 1171/1000000    # chamber volume m3
A = 0.0078           # tube area m2
fir_year = min(c(yeart,yearp,yearc),na.rm=T)  # to know from when to when the data should be calculated for.
fir_yeare = max(c(yeart,yearp,yearc),na.rm=T) # fir_yeare means last year.
}


## Total Soil respiration Calculation:
    # remove outliers and NAs: Fluxes based on overall correction, ## Temperature and chamber correction: Temperature and Chamber height (see functions!)
    # the choice of the sd_interval changes things!    

    fluxt <- rm.flux.outlier(fluxt, sd_interval=2) # IS this really defining SD? check in aux-functions
    tempt <- rm.temp.outlier(temp=tempt, month=montht)
    cht   <- rm.ch.outlier(ch=cht)
    
    ## Perform chamber and flux correction (Metcalfe 2009), see function fluxcorr
    # chamber volume correction according to Metcalfe et al (2009): Rainfor Manual Appendix II, page 75
    RcAt <- fluxcorr(flux=fluxt, temp=tempt, ch=cht, Vd=Vd, A=A, pressure=pressure)


### Control Measurements (data which has been assigned above!)

    # remove outliers (> 3 SD) and NAs:
    fluxc <- rm.flux.outlier(fluxc, sd_interval=2) # Discuss with team: it makes a big difference to the data if you use 2 sd or 3sd.
    tempc <- rm.temp.outlier(temp = tempc, month=monthc)
    chc   <- rm.ch.outlier(ch=chc)
    
    ## Flux correction according to Metcalfe, RAINFOR Manual, Appendix 2, p. 75
    RccA <- fluxcorr(flux=fluxc, temp=tempc, ch=chc, Vd=Vd, A=A, pressure=pressure)


#### Partitioning Soil Repiration calculation (data was assigned to vectors above)
    # remove outliers and NAs: Flux (sd > 3), Temperature and Chamber height (see soilrespiration_aux-functions.R)
    fluxp <- rm.flux.outlier(fluxp, sd_interval=2)
    tempp <- rm.temp.outlier(temp=tempp, month=monthp)
    chc <- rm.ch.outlier(ch=chc)
    
    # flux and chamber volume correction, see function fluxcorr
    RcpA <- fluxcorr(flux=fluxp, temp=tempp, ch=chp, Vd=Vd, A=A, pressure=pressure)


### Calculate respiration values in each year and month for the three different treatments:

# Total Respiration initialize matrixes:
  resAt     <- matrix(data=NA, nrow=12, ncol=fir_yeare-fir_year+1, dimnames=list(c(month.name),fir_year:fir_yeare))
  resAtstd <- matrix(data=NA, nrow=12, ncol=fir_yeare-fir_year+1, dimnames=list(c(month.name),fir_year:fir_yeare))

# Control: initialize matrixes:
  rcanotper <- matrix(data=NA, nrow=12, ncol=fir_yeare-fir_year+1, dimnames=list(c(month.name),fir_year:fir_yeare))
  rcaper <- matrix(data=NA, nrow=12, ncol=fir_yeare-fir_year+1, dimnames=list(c(month.name),fir_year:fir_yeare))
  DCdAstd <- matrix(data=NA, nrow=12, ncol=fir_yeare-fir_year+1, dimnames=list(c(month.name),fir_year:fir_yeare))

  if (partitioningoption==1) {
#### Partitioning: Initialize matrices: (option 1)
### This section is to differentiate between different forms of partitioning experiments:
#  Controle-normal liteira
#  Controle-sem liteira
#  Controle-duplo liteira
#  Mycorrhizae-normal liteira
#  Mycorrhizae-sem liteira
#  Mycorrhizae-duplo liteira
#  Solo-normal liteira
#  Solo-sem liteira
#  Solo-duplo liteira

    con_nor_litA  <- matrix(data=NA, nrow=12, ncol=fir_yeare-fir_year+1, dimnames=list(c(month.name),fir_year:fir_yeare))
    con_no_litA  <- matrix(data=NA, nrow=12, ncol=fir_yeare-fir_year+1, dimnames=list(c(month.name),fir_year:fir_yeare))
    con_doub_litA  <- matrix(data=NA, nrow=12, ncol=fir_yeare-fir_year+1, dimnames=list(c(month.name),fir_year:fir_yeare))
    My_nor_litA  <- matrix(data=NA, nrow=12, ncol=fir_yeare-fir_year+1, dimnames=list(c(month.name),fir_year:fir_yeare))
    My_no_litA  <- matrix(data=NA, nrow=12, ncol=fir_yeare-fir_year+1, dimnames=list(c(month.name),fir_year:fir_yeare))
    My_doub_litA  <- matrix(data=NA, nrow=12, ncol=fir_yeare-fir_year+1, dimnames=list(c(month.name),fir_year:fir_yeare))
    So_nor_litA  <- matrix(data=NA, nrow=12, ncol=fir_yeare-fir_year+1, dimnames=list(c(month.name),fir_year:fir_yeare))
    So_no_litA  <- matrix(data=NA, nrow=12, ncol=fir_yeare-fir_year+1, dimnames=list(c(month.name),fir_year:fir_yeare))
    So_doub_litA  <- matrix(data=NA, nrow=12, ncol=fir_yeare-fir_year+1, dimnames=list(c(month.name),fir_year:fir_yeare))
    
    con_nor_litAstd  <- matrix(data=NA, nrow=12, ncol=fir_yeare-fir_year+1, dimnames=list(c(month.name),fir_year:fir_yeare))
    con_no_litAstd  <- matrix(data=NA, nrow=12, ncol=fir_yeare-fir_year+1, dimnames=list(c(month.name),fir_year:fir_yeare))
    con_doub_litAstd  <- matrix(data=NA, nrow=12, ncol=fir_yeare-fir_year+1, dimnames=list(c(month.name),fir_year:fir_yeare))
    My_nor_litAstd  <- matrix(data=NA, nrow=12, ncol=fir_yeare-fir_year+1, dimnames=list(c(month.name),fir_year:fir_yeare))
    My_no_litAstd  <- matrix(data=NA, nrow=12, ncol=fir_yeare-fir_year+1, dimnames=list(c(month.name),fir_year:fir_yeare))
    My_doub_litAstd  <- matrix(data=NA, nrow=12, ncol=fir_yeare-fir_year+1, dimnames=list(c(month.name),fir_year:fir_yeare))
    So_nor_litAstd  <- matrix(data=NA, nrow=12, ncol=fir_yeare-fir_year+1, dimnames=list(c(month.name),fir_year:fir_yeare))
    So_no_litAstd  <- matrix(data=NA, nrow=12, ncol=fir_yeare-fir_year+1, dimnames=list(c(month.name),fir_year:fir_yeare))
    So_doub_litAstd  <- matrix(data=NA, nrow=12, ncol=fir_yeare-fir_year+1, dimnames=list(c(month.name),fir_year:fir_yeare))

  } else if (partitioningoption==2) {
    con_lit  <- matrix(data=NA, nrow=12, ncol=fir_yeare-fir_year+1, dimnames=list(c(month.name),fir_year:fir_yeare))
    S1  <- matrix(data=NA, nrow=12, ncol=fir_yeare-fir_year+1, dimnames=list(c(month.name),fir_year:fir_yeare))
    S2  <- matrix(data=NA, nrow=12, ncol=fir_yeare-fir_year+1, dimnames=list(c(month.name),fir_year:fir_yeare))
    S3  <- matrix(data=NA, nrow=12, ncol=fir_yeare-fir_year+1, dimnames=list(c(month.name),fir_year:fir_yeare))
    
    ############################################################
    ############ TO DO: Add sd for these too.###################
    ############################################################
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
      imp = which(monthp==i & yearp==j) # imp is an index to get current year & current month
      if (partitioningoption==1) {
          xx = RcpA[imp]                
          nx=treatmentp[imp]
          
          if (sum(xx, na.rm=T) > 0) {
            ix1=which(nx==1)
            ix2=which(nx==2)
            ix3=which(nx==3)
            ix4=which(nx==4)
            ix5=which(nx==5)
            ix6=which(nx==6)
            ix7=which(nx==7)
            ix8=which(nx==8)
            ix9=which(nx==9)
            con_nor_litA[m,n] = mean(xx[ix1],na.rm=T)
            con_no_litA[m,n] = mean(xx[ix2],na.rm=T)   # 1b Controle-sem liteira
            con_doub_litA[m,n] = mean(xx[ix3],na.rm=T) # 1cControle-duplo liteira
            My_nor_litA[m,n] = mean(xx[ix4],na.rm=T)   # 2aMycorrhizae-normal liteira
            My_no_litA[m,n] = mean(xx[ix5],na.rm=T)    # 2b Mycorrhizae-sem liteira
            My_doub_litA[m,n] = mean(xx[ix6],na.rm=T)  # 2cMycorrhizae-duplo liteira
            So_nor_litA[m,n] = mean(xx[ix7],na.rm=T)   # 3a Solo-normal liteira
            So_no_litA[m,n] = mean(xx[ix8],na.rm=T)    # 3bSolo-sem liteira
            So_doub_litA[m,n] = mean(xx[ix9],na.rm=T)  # 3cSolo-duplo liteira 
            
            con_nor_litAstd[m,n] = sd(xx[ix1],na.rm=T)
            con_no_litAstd[m,n] = sd(xx[ix2],na.rm=T)  # 1b Controle-sem liteira
            con_doub_litAstd[m,n] = sd(xx[ix3],na.rm=T)# 1cControle-duplo liteira
            My_nor_litAstd[m,n] = sd(xx[ix4],na.rm=T)  # 2aMycorrhizae-normal liteira
            My_no_litAstd[m,n] = sd(xx[ix5],na.rm=T)   # 2b Mycorrhizae-sem liteira
            My_doub_litAstd[m,n] = sd(xx[ix6],na.rm=T) # 2cMycorrhizae-duplo liteira
            So_nor_litAstd[m,n] = sd(xx[ix7],na.rm=T)  # 3a Solo-normal liteira
            So_no_litAstd[m,n] = sd(xx[ix8],na.rm=T)   # 3bSolo-sem liteira
            So_doub_litAstd[m,n] = sd(xx[ix9],na.rm=T) # 3cSolo-duplo liteira       
          } else {
            con_nor_litA[m,n] = NA
            con_no_litA[m,n] = NA
            con_doub_litA[m,n] = NA
            My_nor_litA[m,n] = NA
            My_no_litA[m,n] = NA
            My_doub_litA[m,n] = NA
            So_nor_litA[m,n] = NA
            So_no_litA[m,n] = NA
            So_doub_litA[m,n] = NA
            
            con_nor_litAstd[m,n] = NA
            con_no_litAstd[m,n] = NA   # 1b Controle-sem liteira
            con_doub_litAstd[m,n] = NA # 1cControle-duplo liteira
            My_nor_litAstd[m,n] = NA   # 2aMycorrhizae-normal liteira
            My_no_litAstd[m,n] = NA    # 2b Mycorrhizae-sem liteira
            My_doub_litAstd[m,n] = NA  # 2cMycorrhizae-duplo liteira
            So_nor_litAstd[m,n] = NA   # 3a Solo-normal liteira
            So_no_litAstd[m,n] = NA    # 3bSolo-sem liteira
            So_doub_litAstd[m,n] = NA  # 3cSolo-duplo liteira 
          }
      } else if (partitioningoption==2) {
        xx = RcpA[imp]           
        nx=treatmentp[imp]
            if (sum(xx, na.rm=T) > 0) {
              ix1=which(nx==1)  # Control
              ix2=which(nx==2)  # No Litter
              ix3=which(nx==3)  # No Litter/No Roots
              ix4=which(nx==4)  # No Litter/ Root + Mycorrhizae Exclusion
              
              con_lit[m,n] = mean(xx[ix1],na.rm=T) # Control
              S1[m,n] = mean(xx[ix2],na.rm=T) # No Litter
              S2[m,n] = mean(xx[ix3],na.rm=T) # No Litter/No Roots
              S3[m,n] = mean(xx[ix4],na.rm=T) # No Litter/ Root + Mycorrhizae Exclusion
              
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


    #  estimation of the relative contributions of (1) surface organic
    #  litter, (2) roots, (3) mycorrhizae and (4) soil organic matter to total soil respiration
    # add a temperature correction from Sotta et al 2004 Q10=1.8 and k=0.0613
    corrsresA=exp(-0.0695*(1))


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
  
  discorA = mean(colMeans(DCudA - DCdA,na.rm=T),na.rm=T)
  #discorAstd = mean(colMeans(DCdAstd,na.rm=T),na.rm=T)

if (partitioningoption==1) {
    ## respiration under three different treatments: control, no litter and double litter.
    rrA1 = ((con_no_litA-(So_no_litA+discorA))/(con_no_litA))
    #rrA1[which(rrA1 < 0)] <- NA
    rrA1[which(rrA1>1)] <- NA
    
    rrA2 = ((con_nor_litA-(So_nor_litA+discorA))/(con_nor_litA))
    #rrA2[which(rrA2 < 0)] <- NA
    rrA2[which(rrA2>1)] <- NA
    
    rrA3 = ((con_doub_litA-(So_doub_litA+discorA))/(con_doub_litA))
    #rrA3[which(rrA3<0)] <- NA
    rrA3[which(rrA3>1)] <- NA
    
    rrxc = dim(rrA1)
    rrA <- matrix(data=NA, nrow=rrxc[1], ncol=rrxc[2])
    rrAstd <- matrix(data=NA, nrow=rrxc[1], ncol=rrxc[2])
    for (i in 1:rrxc[1]) {
      for (j in 1:rrxc[2]) {
        rrA[i,j] = mean(c(rrA1[i,j],rrA2[i,j],rrA3[i,j]),na.rm=T)    
        rrAstd[i,j] = sd(c(rrA1[i,j],rrA2[i,j],rrA3[i,j]),na.rm=T)    
      }
    } 
    rrA[which(rrA>1)] <- NA
} else if (partitioningoption==2) {
  rrA = ((S1-(S3+discorA))/S1) # Chris should check this equation. Should have 0<rrA<1.
  #rrA[which(rrA>1)] <- NA
}


## autotrophic root respiration:
rrtotresAc = totresAc*rrA
rrtotresAcstd = (totresAcstd*rrA)/sqrt(25) # should this be more generic? length(subplot)

## heterotrophic respiration:
hrtotresAc = totresAc*(1-rrA)
hrtotresAcstd = (totresAcstd*(1-rrA))/sqrt(25)

# fill gaps
rrAfg = colMeans(t(rrA),na.rm=T)

for (i in 1:12) {
  for (j in 1:(fir_yeare-fir_year+1)) {
    if (is.na(rrA[i,j]) & !is.na(totresAc[i,j])) {
      if (!is.na(rrAfg[i])) {
        rrtotresAc[i,j] = totresAc[i,j]*rrAfg[i]
        rrtotresAcstd[i,j] = (totresAcstd[i,j]*rrAfg[i])
        hrtotresAc[i,j] = totresAc[i,j]*(1-rrAfg[i])
        hrtotresAcstd[i,j] = totresAcstd[i,j]*(1-rrAfg[i])
      } else if (i==12) {
        rrtotresAc[i,j] = totresAc[i,j]*rrAfg[i-1]
        rrtotresAcstd[i,j] = (totresAcstd[i,j]*rrAfg[i-1])
        hrtotresAc[i,j] = totresAc[i,j]*(1-rrAfg[i-1])
        hrtotresAcstd[i,j] = totresAcstd[i,j]*(1-rrAfg[i-1])
      } else {
        rrtotresAc[i,j] = totresAc[i,j]*rrAfg[i+1]
        rrtotresAcstd[i,j] = (totresAcstd[i,j]*rrAfg[i+1])
        hrtotresAc[i,j] = totresAc[i,j]*(1-rrAfg[i+1])
        hrtotresAcstd[i,j] = totresAcstd[i,j]*(1-rrAfg[i+1])
      }
    }    
  }
}


### Relevant Data Output: totres, hrtotres
## Build list with matrices that contain monthly values:
{
  soilresp.data.monthly.matrix <- list(
    (rrtotresAc),(rrtotresAcstd),
    (hrtotresAc),(hrtotresAcstd))
  
  names(soilresp.data.monthly.matrix) <- c("rrtotresAc","rrtotresAcstd",
                                      "hrtotresAc","hrtotresAcstd")
}

###  Build data frame with time series structure
{
  ##Restructure the data (according to time series structure):
  Year <- NULL
  Month <- NULL
  Day <- NULL
  
  for (i in 1:dim(totresAc)[2]) {
    Year[((i-1)*12+1):((i-1)*12+12)] <- (rep(c(fir_year:fir_yeare)[i],12))
    Month[((i-1)*12+1):((i-1)*12+12)] <- (1:12)
    Day[((i-1)*12+1):((i-1)*12+12)] <- rep(NA,12)
  }
  
  soilresp.data.monthly.ts <- data.frame(Year,Month,Day,
                                    c(rrtotresAc),c(rrtotresAcstd),
                                    c(hrtotresAc),c(hrtotresAcstd))
  
  colnames(soilresp.data.monthly.ts) <- c("Year","Month","Day",  
                                     "rrtotresAc","rrtotresAcstd",
                                     "hrtotresAc","hrtotresAcstd")
}
 

## Plotroutine, triggered by argument 'plotit=T'
if (plotit==T) {
  ## Time representation of Dates as character in the vector 'dates':
  dates <- c()
  for (i in 1:length(Year)) {
    dates[i] <- as.character(strptime(paste(as.character(Year[i]),as.character(Month[i]),as.character(15),sep="-"),
                                      format="%Y-%m-%d"))
  }
  
  x11()
  par(mfrow=c(2,1))
  par(mar=c(4,4,0.5,0.5))
  plot(x=strptime(dates, format="%Y-%m-%d"), y=soilresp.data.monthly.ts$rrtotresAc, 
       type='l',lwd=2,
       xlab="Years", ylab="Total Respiration [Units]")
  
  plot(x=strptime(dates, format="%Y-%m-%d"), y=soilresp.data.monthly.ts$hrtotresAc, 
       type='l',lty=1,
       xlab="Years", ylab="Heterotrophic Soil Respiration [Units]")
  }

# Return either monthly means (ret="monthly.means") as time series or matrix  
switch(ret,
       monthly.means.matrix = {return(soilresp.data.monthly.matrix)},
       monthly.means.ts = {return(soilresp.data.monthly.ts)}
)
}