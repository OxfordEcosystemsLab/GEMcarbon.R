# Function/Script: fine root biomsss v3
## This function calculates Root biomass (fine root productivity)
## option = 1: this means the time steps 10,20,30,40 minutes are chosen
## option 2: this means the time steps 5, 10, 15 minutes are chosen


### Read test data:

## open Boldata:
library(R.matlab)
setwd("C:/Users/Cecile/Dropbox/Carbon_Use_Efficieny_R/Original-matlab-code/Ken_Iq-Tang")
#setwd("D:/Dokumente/My Dropbox/Carbon_Use_Efficieny_R/Original-matlab-code/Ken_Iq-Tang")
dir()
Dataall <- readMat(con="Dataall.mat")

setwd("C:/Users/Cecile/Dropbox/Carbon_Use_Efficieny_R/R-functions(v3)")
#setwd("D:/Dokumente/My Dropbox/Carbon_Use_Efficieny_R/R-functions(v3)")

# root biomass:
data.ic <- data.frame(Dataall$ICall)   #;%ingrowthcore data

names(data.ic) <- c("year","month","plot","section","humidity","temperature_C",
                    "diameter","altura","time",
                    "soil_layer_less_2_mm","soil_layer_2_5_mm","soil_layer_greater_2_mm")

## adjust options:
plotname = 1
option = 1
logtransform = T
fine_root_cor <- "Default"
tubed=0.07  ## diameter of tube


### adjust options, with time step option 2:
plotname=5
option = 2
logtransform = T
fine_root_cor <- "Default"
tubed=0.07  ## diameter of tube




#### ____________________________

## Use Sam's data and adjust the options:
#setwd("C:/Users/Cecile/Dropbox/NPPflux/db_csv")
#data.ic <- read.table("D:/Dokumente/My Dropbox/Carbon_Use_Efficieny_R/testing/ICEltr_samtest.csv", sep=",", header=T)
data.ic <- read.table("C:/Users/Cecile/Dropbox/Carbon_Use_Efficieny_R/testing/ICEltr_samtest.csv", sep=",", header=T)

colnames(data.ic) <-  c("plot","year","month","day","soil_humidity_pcnt", "soil_temperature_c", "section", "time_step", "soil_layer_less_2_mm","soil_layer_2_5_mm", "comments")
## adjust options:
plotname = 1
option = 1
logtransform = T
fine_root_cor <- "Default" # Toby wanted to add this as a variable
tubed = 0.07  ## diameter of tube



#data.ic=read.table("ICEltr.csv", header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE)
#esp <- subset(data.ic, select=c(year, month, plot_code, "pointic", ingrowth_core_num, time_step, ol_under_2mm_g, ml_under_2mm_g, "ML2_ic"), subset=(Plot == "ESP-01"))
#way <- subset(data.ic, subset=(Plot == "WAY-01"))




#rootbiomass <- function(data.ic, plotname, option = 1
#                        logtransform=T, fine_root_cor="Default", tubed=0.07,
#                                     ret="monthly.means.ts", plotit=F) {
  
plotic = data.ic$plot #A=1 B=2
yearic = data.ic$year[which(plotic==plotname)] 
monthic = data.ic$month[which(plotic==plotname)] 
timeic= data.ic$time[which(plotic==plotname)] 
OLic = data.ic$soil_layer_less_2_mm[which(plotic==plotname)]    # change OLic to less2, etc.
MLic = data.ic$soil_layer_2_5_mm[which(plotic==plotname)] 
ML2ic = data.ic$soil_layer_greater_2_mm[which(plotic==plotname)] 

## here should come some sanity check of the input Data!

# Find NA's
OLic[is.na(OLic)] <- 0
MLic[is.na(MLic)] <- 0
ML2ic[is.na(ML2ic)] <- 0

if (option == 1) {
    
    ## All timeic==1 values are investigated (time index that equal 1, i.e. for 10 min time step)
    ix = which(timeic==1)                            # looking for timestep 1
    olx <- matrix(data=NA, nrow=length(ix), ncol=4)  ## build matrix for organic layer # should be: build matrix for less than 2mm
    mlx <- matrix(data=NA, nrow=length(ix), ncol=4)  ## build matrix for mineral layer 1
    ml2x <- matrix(data=NA, nrow=length(ix), ncol=4) ## build matrix for mineral layer 1
    yearic1 <- NULL
    monthic1 <- NULL
    plotic1 <- NULL
    
    ## Loop in which relevant data from OLIC, MLIC, MLIC2 are selected (according to their relevant time step!):
    # Make sure that 1st timestep is always followed by timesteps 2,3,4 in that order
    n=1
    for (i in (1:length(ix))) {
        olx[n,] = c(OLic[ix[i]],OLic[ix[i]+1],OLic[ix[i]+2],OLic[ix[i]+3])
        mlx[n,] = c(MLic[ix[i]],MLic[ix[i]+1],MLic[ix[i]+2],MLic[ix[i]+3])
        ml2x[n,] = c(ML2ic[ix[i]],ML2ic[ix[i]+1],ML2ic[ix[i]+2],ML2ic[ix[i]+3])
        yearic1[n] = yearic[ix[i]]
        monthic1[n] = monthic[ix[i]]
        plotic1[n] = plotic[ix[i]]
        n=n+1;
    }
    
    
    tx = c(10,20,30,40) 
    ## Those are the 10 minute timesteps given in Araujo-Murakamie et al (NPP (fineroots))
    ## and used in Metcalfe et al (2007)
    
    # Initialize variables for loop:
    # cuma, cumb, cumc are cumulative roots in each soil layer and for each sample
    # oltot, mltot and ml2tot are (hypothetical) roots encountered after 100minutes (i.e. fitted model)
    cuma <- NULL
    cumb <- NULL
    cumc <- NULL
    oltot <- NULL
    mltot <- NULL
    ml2tot <- NULL
    oltot_log <- NULL
    mltot_log <- NULL
    ml2tot_log <- NULL
    
    ## Fits power models to the data (10 min. time steps!):
    for (i in 1:max(dim(olx))) {
        if  (!any(is.na(olx[i,])) & sum(olx[i,]) > 0) { # makes sure there are no NAs, so only estimates powerlaw if there is data in the four timesteps.
            cumol = cumsum(sort(olx[i,],decreasing=T))
            cuma[i] = cumol[length(cumol)]    ## cuma is added the last element of cumol; i.e. cumulative roots for current iteration
            if (logtransform==F) {
            P = nls(cumol ~ a * tx^b, start=list(a=1, b=1), 
                    control=nls.control(maxiter=1000,warnOnly=T))
            oltot[i] = coef(P)[1] * (100)^(coef(P)[2]) 
            } else {
            P_log = lm(log(cumol) ~ log(tx))
            oltot_log[i] <- exp(coefficients(P_log)[1]) * (100)^(coefficients(P_log)[2]) }
            } else {
            oltot[i] <- NA 
            oltot_log[i] <- NA
            cuma[i] <- NA
        }
          if  (!any(is.na(mlx[i,])) & sum(mlx[i,]) > 0) { # same as above, for root size 2 to 5 mm
            cumol = cumsum(sort(mlx[i,],decreasing=T))
            cumb[i] = cumol[length(cumol)]
            if (logtransform==F) {
            P = nls(cumol ~ a * tx^b, start=list(a=1, b=1), 
                    control=nls.control(maxiter=1000,warnOnly=T))
            mltot[i] = coef(P)[1] * (100)^(coef(P)[2]) 
            } else {
            P_log = lm(log(cumol) ~ log(tx))
            mltot_log[i] <- exp(coefficients(P_log)[1]) * (100)^(coefficients(P_log)[2]) }
          } else {
            mltot[i] <- NA 
            mltot_log[i] <- NA
            cumb[i] <- NA
          }
        if  (!any(is.na(ml2x[i,])) & sum(ml2x[i,]) > 0) { # same as above, for root size larger than 5 mm
          cumol = cumsum(sort(ml2x[i,],decreasing=T))
          cumc[i] = cumol[length(cumol)]
          if (logtransform==F) {
          P = nls(cumol ~ a * tx^b, start=list(a=1, b=1), 
                  control=nls.control(maxiter=1000,warnOnly=T))
          ml2tot[i] = coef(P)[1] * (100)^(coef(P)[2])
          } else {
          P_log = lm(log(cumol) ~ log(tx))
          ml2tot_log[i] <- exp(coefficients(P_log)[1]) * (100)^(coefficients(P_log)[2]) }
        } else {
          ml2tot[i] <- NA
          ml2tot_log[i] <- NA
          cumc[i] <- NA
        }
    }
    
    ## if log-transformation is used:
    if (logtransform==T) {
    oltot <- oltot_log
    mltot <- mltot_log
    ml2tot <- ml2tot_log
    }
    
    ## rootztot is the sum of roots in the three different soil layers.
    rootztot = c(colSums(t(matrix(data=c(oltot,mltot,ml2tot), ncol=3)),na.rm=T)) # sum of three different root size classes after 100 min.
    rootztota = c(colSums(t(matrix(data=c(cuma,cumb,cumc), ncol=3)),na.rm=T))    # sum of three different root size classes for 4 timesteps, so after 40 min rather than 100 min.
    yearic = c(yearic1)
    monthic = c(monthic1)
    plotic = c(plotic1)

} else if (option==2) {
  
  
  ## All timeic==1 values are investigated (time index that equal 1, i.e. for 10 min time step)
  ix = which(timeic==1) # THis is timestep number, not the minutesd (i.e. timestep 1, not 5 min).
  
  olx <- matrix(data=NA, nrow=length(ix), ncol=3)  ## build matrix for organic layer
  yearic1 <- NULL
  monthic1 <- NULL
  plotic1 <- NULL
  
  ## Loop in which relevant data from OLIC, MLIC, MLIC2 are selected (according to their relevant time step!):
  n=1;
  for (i in (1:length(ix))) {
    olx[n,] = c(OLic[ix[i]],OLic[ix[i]+1],OLic[ix[i]+2])
    yearic1[n] = yearic[ix[i]]
    monthic1[n] = monthic[ix[i]]
    plotic1[n] = plotic[ix[i]]
    n=n+1
  }
  
  
  tx = c(5,10,15) 
  ## alternative time steps
  
  # Initialize variables for loop:
  # cuma, cumb, cumc are cumulative roots in each soil layer and for each sample
  # oltot, mltot and ml2tot are (hypothetical) roots encountered after 100minutes (i.e. fitted model)
  cuma <- NULL
  oltot <- NULL
  oltot_log <- NULL
  
  ## Fits power models to the data (10 min. time steps!):
  for (i in 1:max(dim(olx))) {
    if  (!any(is.na(olx[i,])) & sum(olx[i,]) > 0) {
      cumol = cumsum(sort(olx[i,],decreasing=T))
      cuma[i] = cumol[length(cumol)]    ## cuma is added the last element of cumol; i.e. cumulative roots for current iteration
      if (logtransform==F) {
        P = nls(cumol ~ a * tx^b, start=list(a=1, b=1), 
                control=nls.control(maxiter=1000,warnOnly=T))
        oltot[i] = coef(P)[1] * (100)^(coef(P)[2]) 
      } else {
        P_log = lm(log(cumol) ~ log(tx))
        oltot_log[i] <- exp(coefficients(P_log)[1]) * (100)^(coefficients(P_log)[2]) }
    } else {
      oltot[i] <- NA 
      oltot_log[i] <- NA
      cuma[i] <- NA
    }
  }
  
  ## if log-transformation is used:
  if (logtransform==T) {
    oltot <- oltot_log
  }
  
  
  ## rootztot is the sum of roots in the one soil layers.
  rootztot = c(colSums(t(matrix(data=c(oltot), ncol=1)),na.rm=T))
  rootztota = c(colSums(t(matrix(data=c(cuma), ncol=1)),na.rm=T))
  yearic = c(yearic1)
  monthic = c(monthic1)
  plotic = c(plotic1)
  
}

if (fine_root_cor=="Default") {
tubeh=0
# depth profile of roots
depic = (30-tubeh/10)/100
dzz = 0.5*(exp(-7.344*depic)+exp(-1.303*depic)) #correction for fine root productivity below 30cm
} else {
  dzz <- fine_root_cor
}

#in Davids study, 37% of the fine roots (<2mm) were below 30cm, this is
#close to 39% found by this equation
### Please note there is a discrepancy here to the RAINFOR manual (2.3, pp. 47), 
## because the assumption there is 45% more than the rootbiomass in the top 30 cm of the soil.
# Sebastian, 10.12.2013, based on Toby's comment.
# Sebastian, 17.12.2013: dzz can be specified in fine_root_cor


tubed = tubed  ## tube diameter is introduced
#sum total carbon from roots
#(diameter ~ 14cm, depth ~ 30cm)
ciric = (3.14*tubed^2) #surface area m2
volic = ciric*depic
rootztot[is.na(rootztot)]=0 # Chris: why do we need this? can we delete?
totaic = rootztot / (1-dzz)   # roots plus roots growing below 30cm
icalA = (totaic/ciric)*10000/(2.1097*1000*1000)  # Mg roots per ha (10000m2=1ha, 1Mg=1000000g divide by 2 for carbon
                                                                   
rooiA = (rootztot-rootztota)/rootztot
monthicA = monthic
yearicA = yearic
                                                                   
fir_mon = 1;
fir_mone = 12;
fir_year = min(yearicA,na.rm=T)
fir_yeare = max(yearicA,na.rm=T)

toticalA <- matrix(data=NA, nrow=12, ncol=(fir_yeare-fir_year+1), dimnames=list(c(month.name),fir_year:fir_yeare))
toticalAstd <- matrix(data=NA, nrow=12, ncol=(fir_yeare-fir_year+1), dimnames=list(c(month.name),fir_year:fir_yeare))

#convert to MgC ha month
n=1;
for (j in fir_year:fir_yeare) {
    m=1;
    for (i in fir_mon:12) {
        ind = which(monthicA==i & yearicA==j)
        toticalA[m,n] = mean(icalA[ind],na.rm=T);
        #convert to se
        toticalAstd[m,n] = (sd(icalA[ind],na.rm=T))/sqrt(16);
        m=m+1;
    }
    n=n+1;
}


### Build list that contains matrices with monthly values
{
  rootbio.data.monthly.matrix <- list(toticalA,toticalAstd)
  names(rootbio.data.monthly.matrix) <- c("toticalA","toticalAstd")
}

### Build data.frame with rootbio time series data:
{
### Restructure the data (according to time series structure):
Year <- NULL
Month <- NULL
Day <- NULL

for (i in 1:(max(yearicA,na.rm=T)-min(yearicA,na.rm=T)+1)) {
  Year[((i-1)*12+1):((i-1)*12+12)] <- (rep(c(min(yearicA,na.rm=T):max(yearicA,na.rm=T))[i],12))
  Month[((i-1)*12+1):((i-1)*12+12)] <- (1:12)
  Day[((i-1)*12+1):((i-1)*12+12)] <- rep(NA,12)
}

rootbio.data.monthly.ts <- data.frame(Year,Month,Day,
                               c(toticalA),c(toticalAstd))
colnames(rootbio.data.monthly.ts) <- c("Year","Month","Day",  
                                "toticalA","toticalAstd")
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
  par(mfrow=c(1,1))
  par(mar=c(4,4,0.5,0.5))
  plot(x=strptime(dates, format="%Y-%m-%d"), y=rootbio.data.monthly.ts$toticalA, 
       type='p',lwd=2,
       xlab="Years", ylab="Fine root NPP (MgC ha-1 yr-1)")
  legend("topleft", c("Fine root NPP"), lty=c(1), bty='n')
}

# Return either monthly means (ret="monthly.means")   
switch(ret,
       monthly.means.matrix = {return(rootbio.data.monthly.matrix)},
       monthly.means.ts = {return(rootbio.data.monthly.ts)}
)

}