## These functions are required for running the soilrespiration-program!
# Sebastian Sippel, 06.12.2013
# Last edited: Cicile Girardin, 01.03.2015 

## remove flux outliers: CURRENTLY NOT IMPLEMENTED; DUE TO OVERALL FLUX CORRECTION (see below)!
#rm.flux.outlier <- function(flux, sd_interval=3) {
#  outliers = mean(flux,na.rm=T)+sd_interval*sd(flux,na.rm=T);
#  flux[which(flux < 0)] <- 0;
#  flux[which(flux > outliers)] <- NA
#  return(flux)
#}

# Add a SPIKE DETECTION FUNCTION
# Get the SD of mean of 2 points before and 2 points after, to detect spikes in data.
# Add SD per subplot / measurement

## new flux correction based on overall fluxes:
rm.flux.outlier <- function(flux_overall, sd_interval=3) {
  outliers = mean(flux_overall,na.rm=T) + 3*sd(flux_overall,na.rm=T)
  flux_overall[which(flux_overall < 0)] <- 0
  flux_overall[which(flux_overall > outliers)] <- NA
  return(flux_overall)
}

# Add a spike detection function

## remove temperature outlier:
rm.temp.outlier <- function(temp, month) {
  temp[which(temp<5)]=NA   
  temp[which(temp>50)]=NA   
  xtemp <- NULL
  ## run through 12 months to find xtemp:
  for (i in 1:12) {
    xtemp[i] <-mean(temp[which(month == i)],na.rm=T)
    temp[which(is.na(temp) & month == i)] <- xtemp[i]
  }
  return(temp)
}

## remove ch outlier:

library(zoo)

# zoo function
# date column
# na.approx

# sort by collar
# sort by date

fill.ch.na <- function(ch, sub_plot, month) {
  xxch <- NULL
  xch <- NULL
  for (i in 1:25) {
    for (j in 1:12) {
      xxch[j] <-ch[which(month == j-1)]
    }
    xch[i] <-mean(ch[which(sub_plot == i)], na.rm=T)
    ch[which(is.na(ch) & sub_plot == i)] <- xch[i]
  }
  return(ch)
  
  #xch=mean(ch,na.rm=T)  ## mean of ch over all plot! This should be the ch for that collar for the month before.
  #ch[which(is.na(ch))] <- xch
  #return(ch)
}

# ch=ch-1 in cm. This eliminates the overlap between the collar and the adaptor.

## Perform chamber and flux correction (Metcalfe 2009) 
# chamber volume correction according to Metcalfe et al (2009): Rainfor Manual Appendix II, page 75
## flux correction function is based on Appendix 2, RAINFOR manual
# see p. 75, RAINFOR manual
## this correction is used for total and partitioning

# Chamber volume correction.

fluxcorr <- function(flux, temp, ch, Vd, A, pressure) {
  Va = A*(ch/100)        # additional volume m3
  # initialize Variables for the for-loop: (variables are the flux variables and to specify plotname)
  RucA <- numeric(length=length(temp))
  RcA <- numeric(length=length(temp))
  # correct for the new tube volume
  for (i in 1:length(temp)) {
    RucA[i]=(flux[i])*(pressure/1000)*(273/(temp[i]+273))*(44.01/22.41)*(Vd/A)/1000*3600
    RcA[i]= (RucA[i]*A/Vd*(Va[i]+Vd)/A)*6.312  # convert to umol m-2 s-1
  }
  return(RcA)
}


## Temperature-dependent version of the barometric equation: 
# see Berberan-Santos et al. (1997)
barometric_equation_T <- function(elevation, temp) {
  ## temp in 0C
  xtemp = temp+273.13
  Ru=8.31432                    # Universal gas constant in J/molK
  molarMd=28.9644               # Molar mass (molecular weight/mass) of dry air in g/mol
  Rd=1000*Ru/molarMd            # Specific gas constant for dry air in J/kgK (approx. 287.04 J/kgK)
  P=1013.25*exp(-9.81*(elevation)/(Rd*(xtemp)))
  return(P)
}


## Temperature-independent version of the barometric equation (see e.g. Wikipedia):
barometric_equation <- function(elevation) {
  P=1013.25*(1-0.0065*elevation/288.15)^5.255  # in hPa Adapt lapse rate to your region.
  return(P)
}


