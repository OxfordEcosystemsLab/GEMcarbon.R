## These functions are required for running the soilrespiration-program!
# Sebastian Sippel, 06.12.2013
# Last edited: Cécile Girardin, 01.07.2014 

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
  temp[which(temp<15)]=NA   # DEFINABLE VARIABLE
  temp[which(temp>35)]=NA   # DEFINABLE VARIABLE
  
  xtemp <- NULL
  ## run through 12 months to find xtemp:
  for (i in 1:12) {
    xtemp[i] <-mean(temp[which(month == i)],na.rm=T)
    temp[which(is.na(temp) & month == i)] <- xtemp[i]
  }
  #%if temp data are missing fill in the values with these data -> WHY?
  # This was before I changed the error correction:
  #xtemp= t(matrix(data=c(23.5768518518518,23.5444444444444,24.1574074074074,23.8675925925926,20.1416666666667,22.7888888888889,20.1111111111111,21.2144444444444,23.4574074074074,24.3486111111111,24.3129629629630,24.3324074074074,23.7212962962963,23.5629629629630,24.3620370370370,24.3703703703704,22.4324074074074,22.7194444444444,18.4259259259259,21.8000000000000,24.7944444444444,24.6347222222222,24.7742592592593,24.1861111111111,23.8083590509375,23.8083590509375,25.0081202046036,23.5112903225806,24.7426680672269,25.1614285714286,23.6415492957747,24.8514084507042,24.7034439513795,26.1714285714286,23.8083590509375,23.8083590509375,23.8083590509375,23.8083590509375,24.6732394366197,24.4428571428572,23.8083590509375,25.8693635014982,23.5798507462687,24.8459611546023,24.6394366197183,26.3195085470086,23.8083590509375,23.8083590509375,25.3805555555556,24.1027777777778,24.1736111111111,23.6175925925926,23.9555555555556,22.1527777777778,21.5527777777778,21.6125000000000,23.5277777777778,24.0222222222222,24.5375000000000,24.0166666666667,25.0629629629630,25.0629629629630,24.8013888888889,26.4185185185185,24.1972222222222,22.9416666666667,22.1652777777778,22.3138888888889,24.1083333333333,24.7944444444444,24.5000000000000,24.2944444444444),nrow=12,ncol=6))
  return(temp)
}

## remove ch outlier:
rm.ch.outlier <- function(ch) {
  xch=mean(ch,na.rm=T)  ## mean of ch over all plots!
  ch[which(is.na(ch))] <- xch
  return(ch)
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
  ## temp in °C
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


