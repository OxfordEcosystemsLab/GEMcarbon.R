#
# Functions required for running the soil_respiration_2015 & soil_respiration_persubplot_2017 scripts.
# Sebastian Sippel, 06.12.2013
# Last edited: Cecile Girardin, 01.02.2017


# TO DO: Add a SPIKE DETECTION FUNCTION
# Get the SD of mean of 2 points before and 2 points after, to detect spikes in data.
# Add SD per subplot / measurement

## new flux correction based on overall fluxes:
rm.flux.outlier <- function(flux_overall, sd_interval=3) {
  outliers = mean(flux_overall,na.rm=T) + 3*sd(flux_overall,na.rm=T)
  flux_overall[which(flux_overall < 0)] <- 0
  flux_overall[which(flux_overall > outliers)] <- NA
  return(flux_overall)
}

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



## The following function is used to fill missing collar heights.
# Replace NAs in a vector with the average of the surrounding values.
# In the special cases of NAs at the start or end, simply use the prev/next value.

fill.na <- function(vect) {

  prev_num <- 0/0                                            # Keep track of the previous CH to use for averages after an NA block
  na_count <- 0                                              # Count how many NAs we have in an NA block
  length_vect <- length(vect)
  
  for (i in 1:length_vect) {
  
    current_num <- vect[i]                                    # Get the current CH value
    
    if (is.na(current_num)) {                                # If CH is NA
      na_count <- na_count + 1                              # If the current CH is an NA then simply increment the NA counter and move to the next row
    } else {                                                # CH is not NA
      if (na_count > 0) {                                   # The 'if' block below is the special bit that effectively replaces the NAs with averages
        if (is.na(prev_num)) {
          prev_num <- current_num                             # The special case where we have NAs to start
        }
        average_num <- (prev_num + current_num) / 2          # Get the average of the CHs before and after the NA block
        for (j in 1:na_count) {
          vect[i-j] <- average_num
        }
        na_count <- 0                                       # Reset the NA counter
      }
      prev_num <- current_num                                 # Keep track of the CH that comes before an NA block
    }
  }
  if (na_count > 0) {
    for (i in 1:na_count) {
      vect[length_vect-i+1] <- prev_num
    }
  }
  return(vect)
}

#xch=mean(ch,na.rm=T)  ## mean of ch over all plot! This should be the ch for that collar for the month before.
#ch[which(is.na(ch))] <- xch
#return(ch)

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
  if (na_count > 0) {                               # Take into account a block of NAs at the end of the data
    for (j in 1:na_count) {
      vect[length_vect-j+1] <- prev_num
    }
  }
  return(vect)
}
}   

# TO DO: flag up if difference between measurements > 3cm
# TO DO: ch=ch-1 in cm. This eliminates the overlap between the collar and the adaptor.


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
