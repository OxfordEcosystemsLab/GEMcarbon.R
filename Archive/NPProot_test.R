
## This function calculates fine root productivity
# Based on Matlab code by C. Doughty (2013)
# Translated to R by S. Sippel (2014)
# Updated and adapted to GEM plots by C. Girardin (2015)

## option = 1: this means the time steps 10,20,30,40 minutes are chosen
## option = 2: this means the time steps 5, 10, 15 minutes are chosen
# ATTENTION!! Make sure that 1st timestep is always followed by timesteps 2,3,4 in that order.

# colnames in database:
# plot_code                  
# year                        
# month                         
# day                         
# ingrowth_core_num           
# is_stock_yn                 
# ingrowth_core_litterfall_g  
# soil_humidity_pcnt          
# soil_temperature_c          		
# ol_layer_depth_cm	    
# ml_layer_depth_cm	    
# time_step                   
# time_step_minutes           
# ol_under_2mm_g                         
# ml_under_2mm_g                           
# ol_2to3_mm_g	            
# ml_2to3_mm_g	            
# ol_3to4_mm_g	            
# ml_3to4_mm_g	           
# ol_4to5_mm_g	
# ml_4to5_mm_g	           
# ol_above_5mm_g	            
# ml_above_5mm_g	           
# quality_code                
# comments   

### Read test data:
#library(R.matlab)
#setwd("C:/Users/Cecile/Dropbox/Carbon_Use_Efficieny_R/Original-matlab-code/Ken_Iq-Tang")
#setwd("D:/Dokumente/My Dropbox/Carbon_Use_Efficieny_R/Original-matlab-code/Ken_Iq-Tang")
#dir()
#Dataall <- readMat(con="Dataall.mat")

### Read test data:
#setwd("C:/Users/Cecile/Dropbox/Carbon_Use_Efficieny_R/R-functions(v3)")
#data.ic <- data.frame(Dataall$ICall)   #%ingrowthcore data
#names(data.ic) <- c("year","month","plot","section","humidity","temperature_C",
#                    "diameter","altura","time",
#                    "soil_layer_less_2_mm","soil_layer_2_5_mm","soil_layer_greater_2_mm")

### Read test data:
#setwd("C:/Users/Cecile/Dropbox/NPPflux/db_csv")
#data.ic <- read.table("C:/Users/Cecile/Dropbox/Carbon_Use_Efficieny_R/testing/ICEltr_samtest.csv", sep=",", header=T)

### Read test data:
#data.ic=read.table("ICEltr.csv", header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE)
#esp <- subset(data.ic, select=c(year, month, plot_code, "pointic", ingrowth_core_num, time_step, ol_under_2mm_g, ml_under_2mm_g, "ML2_ic"), subset=(Plot == "ESP-01"))
#way <- subset(data.ic, subset=(Plot == "WAY-01"))

## adjust options:
plotname = 1
option = 1
logtransform = T
fine_root_cor <- "Default" # Toby suggested to add this as a variable
tubed=0.07  ## diameter of tube

### adjust options, with time step option 2:
plotname=5
option = 2
logtransform = T
fine_root_cor <- "Default"
tubed=0.07  ## diameter of tube


# The function starts here.
NPProot_ic <- function(data.ic, plotname, option = 1, logtransform=T, fine_root_cor="Default", tubed=0.07, ret="monthly.means.ts", plotit=F) {
  
plotic    = data.ic$plot_code 
yearic    = data.ic$year[which(plotic==plotname)] 
monthic   = data.ic$month[which(plotic==plotname)] 
timeic    = data.ic$time_step[which(plotic==plotname)] 

OL_under2 = data.ic$ol_under_2mm_g[which(plotic==plotname)] 
OL_2to3   = data.ic$ol_2to3_mm_g[which(plotic==plotname)]   
OL_3to4   = data.ic$ol_3to4_mm_g[which(plotic==plotname)]
OL_4to5   = data.ic$ol_4to5_mm_g[which(plotic==plotname)]  
OL_above5 = data.ic$ol_above_5mm_g[which(plotic==plotname)]

ML_under2 = data.ic$ml_under_2mm_g[which(plotic==plotname)]                                 
ML_2to3   = data.ic$ml_2to3_mm_g[which(plotic==plotname)]	                        
ML_3to4   = data.ic$ml_3to4_mm_g[which(plotic==plotname)]	           
ML_4to5   = data.ic$ml_4to5_mm_g[which(plotic==plotname)]	                    
ML_above5 = data.ic$ml_above_5mm_g[which(plotic==plotname)]

## TO DO: We should add sanity checks for the input data here.

# Find NA's

OL_under2[is.na(OL_under2)] <- 0
OL_2to3[is.na(OL_2to3)]     <- 0
OL_3to4[is.na(OL_3to4)]     <- 0
OL_4to5[is.na(OL_4to5)]     <- 0
OL_above5[is.na(OL_above5)] <- 0

ML_under2[is.na(ML_under2)] <- 0                        
ML_2to3[is.na(ML_2to3)]     <- 0                       
ML_3to4[is.na(ML_3to4)]     <- 0           
ML_4to5[is.na(ML_4to5)]     <- 0                  
ML_above5[is.na(ML_above5)] <- 0


if (option == 1) {
    
    ## All timeic == 1 values are selected (time index that equal 1, i.e. for 10 min time step).
    ix = which(timeic==1)    # looking for timestep 1
    olunder2  <- matrix(data=NA, nrow=length(ix), ncol=4)  ## build matrix for organic layer 1, roots under 2 mm
    ol2to3    <- matrix(data=NA, nrow=length(ix), ncol=4)  ## build matrix for organic layer 1, roots between 2 and 3 mm
    ol3to4    <- matrix(data=NA, nrow=length(ix), ncol=4)  ## build matrix for organic layer 1, roots between 3 and 4 mm
    ol4to5    <- matrix(data=NA, nrow=length(ix), ncol=4)  
    olabove5  <- matrix(data=NA, nrow=length(ix), ncol=4)  
    mlunder2  <- matrix(data=NA, nrow=length(ix), ncol=4)  
    ml2to3    <- matrix(data=NA, nrow=length(ix), ncol=4)  
    ml3to4    <- matrix(data=NA, nrow=length(ix), ncol=4)  
    ml4to5    <- matrix(data=NA, nrow=length(ix), ncol=4)  
    mlabove5  <- matrix(data=NA, nrow=length(ix), ncol=4)  
    yearic1   <- NULL
    monthic1  <- NULL
    plotic1   <- NULL
    
    ## Loop in which relevant data from olunder2, ol2to3 ... mlabove5 are selected (according to their relevant time step):
    # ATTENTION!! Make sure that 1st timestep is always followed by timesteps 2,3,4 in that order.
        
    n = 1
    for (i in (1:length(ix))) {
      olunder2[n,]  <- c(OL_under2[ix[i]], OL_under2[ix[i]+1], OL_under2[ix[i]+2], OL_under2[ix[i]+3])
      ol2to3[n,]    <- c(OL_2to3[ix[i]], OL_2to3[ix[i]+1], OL_2to3[ix[i]+2], OL_2to3[ix[i]+3])
      ol3to4[n,]    <- c(OL_3to4[ix[i]], OL_3to4[ix[i]+1], OL_3to4[ix[i]+2], OL_3to4[ix[i]+3])
      ol4to5[n,]    <- c(OL_4to5[ix[i]], OL_4to5[ix[i]+1], OL_4to5[ix[i]+2], OL_4to5[ix[i]+3])
      olabove5[n,]  <- c(OL_above5[ix[i]], OL_above5[ix[i]+1], OL_above5[ix[i]+2], OL_above5[ix[i]+3])
      mlunder2[n,]  <- c(ML_under2[ix[i]], ML_under2[ix[i]+1], ML_under2[ix[i]+2], ML_under2[ix[i]+3]) 
      ml2to3[n,]    <- c(ML_2to3[ix[i]], ML_2to3[ix[i]+1], ML_2to3[ix[i]+2], ML_2to3[ix[i]+3]) 
      ml3to4[n,]    <- c(ML_3to4[ix[i]], ML_3to4[ix[i]+1], ML_3to4[ix[i]+2], ML_3to4[ix[i]+3])
      ml4to5[n,]    <- c(ML_4to5[ix[i]], ML_4to5[ix[i]+1], ML_4to5[ix[i]+2], ML_4to5[ix[i]+3])
      mlabove5[n,]  <- c(ML_above5[ix[i]], ML_above5[ix[i]+1], ML_above5[ix[i]+2], ML_above5[ix[i]+3]) 
      
      yearic1[n] = yearic[ix[i]]
      monthic1[n] = monthic[ix[i]]
      plotic1[n] = plotic[ix[i]]
      n=n+1
    }
    
    
    tx = c(10,20,30,40) ## These are the timesteps used in Metcalfe et al. (2007)
    
    # Initialize variables for loop:
    # cum_ol and cum_ml are cumulative roots (e.g. 4 timesteps) in each soil sample. 
    # oltot, mltot and ml2tot are (hypothetical) roots encountered after 100minutes (i.e. fitted model).
    
    cum_olunder2  <- NULL
    cum_ol2to3    <- NULL
    cum_ol3to4    <- NULL
    cum_ol4to5    <- NULL
    cum_olabove5  <- NULL
    cum_mlunder2  <- NULL
    cum_ml2to3    <- NULL
    cum_ml3to4    <- NULL
    cum_ml4to5    <- NULL
    cum_mlabove5  <- NULL
    
    oltot_olunder2 <- NULL
    oltot_ol2to3   <- NULL
    oltot_ol3to4   <- NULL
    oltot_ol4to5   <- NULL
    oltot_olabove5 <- NULL
    mltot_mlunder2 <- NULL
    mltot_ml2to3   <- NULL
    mltot_ml3to4   <- NULL
    mltot_ml4to5   <- NULL
    mltot_mlabove5 <- NULL
    
    oltot_log_olunder2  <- NULL
    oltot_log_ol2to3    <- NULL
    oltot_log_ol3to4    <- NULL
    oltot_log_ol4to5    <- NULL
    oltot_log_olabove5  <- NULL
    mltot_log_mlunder2  <- NULL
    mltot_log_ml2to3    <- NULL
    mltot_log_ml3to4    <- NULL
    mltot_log_ml4to5    <- NULL
    mltot_log_mlabove5  <- NULL
    
    cuma <- NULL
    cumb <- NULL
    cumc <- NULL
    cumd <- NULL
    cume <- NULL
    cumf <- NULL
    cumg <- NULL
    cumh <- NULL
    cumi <- NULL
    cumj <- NULL
    
    ## Fits power models to the data (10 min. time steps!):
    for (i in 1:max(dim(olunder2))) { 
      # cum_olunder2  
        if  (!any(is.na(olunder2[i,])) & sum(olunder2[i,]) > 0) { # makes sure there are no NAs, so only estimates powerlaw if there is data in the four timesteps.
            cum_olunder2 = cumsum(sort(olunder2[i,],decreasing=T))
            cuma[i] = cum_olunder2[length(cum_olunder2)]          # cuma is added the last element of cum_olunder2; i.e. cumulative roots for current iteration
            if (logtransform==F) {
            P = nls(cum_olunder2 ~ a * tx^b, start=list(a=1, b=1), 
                    control=nls.control(maxiter=1000,warnOnly=T))
            oltot_olunder2[i] = coef(P)[1] * (100)^(coef(P)[2]) 
            } else {
            P_log = lm(log(cum_olunder2) ~ log(tx))
            oltot_log_olunder2[i] <- exp(coefficients(P_log)[1]) * (100)^(coefficients(P_log)[2]) }
            } else {
            oltot_olunder2[i] <- NA 
            oltot_log_olunder2[i] <- NA
            cuma[i] <- NA
            }
        
      # cum_ol2to3 
        if  (!any(is.na(ol2to3[i,])) & sum(ol2to3[i,]) > 0) { # same as above, for root size 2 to 5 mm
            cum_ol2to3 = cumsum(sort(ol2to3[i,],decreasing=T))
            cumb[i] = cum_ol2to3[length(cum_ol2to3)]
          if (logtransform==F) {
            P = nls(cum_ol2to3 ~ a * tx^b, start=list(a=1, b=1), 
                    control=nls.control(maxiter=1000,warnOnly=T))
            oltot_ol2to3[i] = coef(P)[1] * (100)^(coef(P)[2]) 
            } else {
            P_log = lm(log(cum_ol2to3) ~ log(tx))
            oltot_log_ol2to3[i] <- exp(coefficients(P_log)[1]) * (100)^(coefficients(P_log)[2]) }
            } else {
            oltot_ol2to3[i] <- NA 
            oltot_log_ol2to3[i] <- NA
            cumb[i] <- NA
            }
      
      # ol3to4
        if  (!any(is.na(ol3to4[i,])) & sum(ol3to4[i,]) > 0) { # same as above, for root size 3 to 4 mm
            cum_ol2to3 = cumsum(sort(ol3to4[i,],decreasing=T))
            cumc[i] = cum_ol3to4[length(cum_ol3to4)]
          if (logtransform==F) {
            P = nls(cum_ol3to4 ~ a * tx^b, start=list(a=1, b=1), 
                   control=nls.control(maxiter=1000,warnOnly=T))
            oltot_ol3to4[i] = coef(P)[1] * (100)^(coef(P)[2]) 
            } else {
            P_log = lm(log(cum_ol3to4) ~ log(tx))
            oltot_log_ol3to4[i] <- exp(coefficients(P_log)[1]) * (100)^(coefficients(P_log)[2]) }
            } else {
            oltot_ol3to4[i] <- NA 
            oltot_log_ol3to4[i] <- NA
            cumc[i] <- NA
            }
      
      # ol4to5
      if  (!any(is.na(ol4to5[i,])) & sum(ol4to5[i,]) > 0) { # same as above, for root size 3 to 4 mm
          cum_ol4to5 = cumsum(sort(ol4to5[i,],decreasing=T))
          cumd[i] = cum_ol4to5[length(cum_ol3to4)]
        if (logtransform==F) {
          P = nls(cum_ol4to5 ~ a * tx^b, start=list(a=1, b=1), 
                  control=nls.control(maxiter=1000,warnOnly=T))
          oltot_ol4to5[i] = coef(P)[1] * (100)^(coef(P)[2]) 
          } else {
          P_log = lm(log(cum_ol4to5) ~ log(tx))
          oltot_log_ol4to5[i] <- exp(coefficients(P_log)[1]) * (100)^(coefficients(P_log)[2]) }
          } else {
          oltot_ol4to5[i] <- NA 
          oltot_log_ol4to5[i] <- NA
          cumd[i] <- NA
          }
      
      # olabove5
        if  (!any(is.na(olabove5[i,])) & sum(olabove5[i,]) > 0) { # same as above, for root size above 5 mm
            cum_olabove5 = cumsum(sort(olabove5[i,],decreasing=T))
            cume[i] = cum_olabove5[length(cum_olabove5)]
          if (logtransform==F) {
            P = nls(cum_olabove5 ~ a * tx^b, start=list(a=1, b=1), 
                   control=nls.control(maxiter=1000,warnOnly=T))
            oltot_olabove5[i] = coef(P)[1] * (100)^(coef(P)[2]) 
            } else {
            P_log = lm(log(cum_olabove5) ~ log(tx))
            oltot_log_olabove5[i] <- exp(coefficients(P_log)[1]) * (100)^(coefficients(P_log)[2]) }
            } else {
            oltot_olabove5[i] <- NA 
            oltot_log_olabove5[i] <- NA
            cume[i] <- NA
            }
      
      # mlunder2
        if  (!any(is.na(mlunder2[i,])) & sum(mlunder2[i,]) > 0) { 
            cum_mlunder2 = cumsum(sort(mlunder2[i,], decreasing=T))
            cumf[i] = cum_mlunder2[length(cum_mlunder2)]    
         if (logtransform==F) {
            P = nls(cum_mlunder2 ~ a * tx^b, start=list(a=1, b=1), 
                   control=nls.control(maxiter=1000,warnOnly=T))
            mltot_mlunder2[i] = coef(P)[1] * (100)^(coef(P)[2])        # ACJ-01: length(mltot_mlunder2) is 63, all the others are 64. WHY????
            } else {
            P_log = lm(log(cum_mlunder2) ~ log(tx))
            mltot_log_mlunder2[i] <- exp(coefficients(P_log)[1]) * (100)^(coefficients(P_log)[2]) }
            } else {
            mltot_mlunder2[i] <- NA 
            mltot_log_mlunder2[i] <- NA
            cumf[i] <- NA
            }
      
      # ml2to3
      if  (!any(is.na(ml2to3[i,])) & sum(ml2to3[i,]) > 0) { 
          cum_ml2to3 = cumsum(sort(ml2to3[i,],decreasing=T))
          cumg[i] = cum_ml2to3[length(cum_ml2to3)]    
        if (logtransform==F) {
          P = nls(cum_ml2to3 ~ a * tx^b, start=list(a=1, b=1), 
                  control=nls.control(maxiter=1000,warnOnly=T))
          mltot_ml2to3[i] = coef(P)[1] * (100)^(coef(P)[2]) 
          } else {
          P_log = lm(log(cum_olunder2) ~ log(tx))
          mltot_log_ml2to3[i] <- exp(coefficients(P_log)[1]) * (100)^(coefficients(P_log)[2]) }
          } else {
          mltot_ml2to3[i] <- NA 
          mltot_log_ml2to3[i] <- NA
          cumg[i] <- NA
          }
      
      # ml3to4
      if  (!any(is.na(ml3to4[i,])) & sum(ml3to4[i,]) > 0) { 
          cum_ml3to4 = cumsum(sort(ml3to4[i,],decreasing=T))
          cumh[i] = cum_ml3to4[length(cum_ml3to4)]    
        if (logtransform==F) {
          P = nls(cum_ml3to4 ~ a * tx^b, start=list(a=1, b=1), 
                  control=nls.control(maxiter=1000,warnOnly=T))
          mltot_ml3to4[i] = coef(P)[1] * (100)^(coef(P)[2]) 
          } else {
          P_log = lm(log(cum_ml3to4) ~ log(tx))
          mltot_log_ml3to4[i] <- exp(coefficients(P_log)[1]) * (100)^(coefficients(P_log)[2]) }
          } else {
          mltot_ml3to4[i] <- NA 
          mltot_log_ml3to4[i] <- NA
          cumh[i] <- NA
          }
      
      # ml4to5
      if  (!any(is.na(ml4to5[i,])) & sum(ml4to5[i,]) > 0) { # makes sure there are no NAs, so only estimates powerlaw if there is data in the four timesteps.
          cum_ml4to5 = cumsum(sort(ml4to5[i,],decreasing=T))
          cumi[i] = cum_ml4to5[length(cum_ml4to5)]    ## cuma is added the last element of cum_olunder2; i.e. cumulative roots for current iteration
        if (logtransform==F) {
          P = nls(cum_ml4to5 ~ a * tx^b, start=list(a=1, b=1), 
                  control=nls.control(maxiter=1000,warnOnly=T))
          mltot_ml4to5[i] = coef(P)[1] * (100)^(coef(P)[2]) 
          } else {
          P_log = lm(log(cum_ml4to5) ~ log(tx))
          mltot_log_ml4to5[i] <- exp(coefficients(P_log)[1]) * (100)^(coefficients(P_log)[2]) }
          } else {
          mltot_ml4to5[i] <- NA 
          mltot_log_ml4to5[i] <- NA
          cumi[i] <- NA
          }
      
      # mlabove5
      if  (!any(is.na(mlabove5[i,])) & sum(mlabove5[i,]) > 0) { # makes sure there are no NAs, so only estimates powerlaw if there is data in the four timesteps.
          cum_mlabove5 = cumsum(sort(mlabove5[i,],decreasing=T))
          cumj[i] = cum_mlabove5[length(cum_mlabove5)]    ## cuma is added the last element of cum_olunder2; i.e. cumulative roots for current iteration
        if (logtransform==F) {
          P = nls(cum_mlabove5 ~ a * tx^b, start=list(a=1, b=1), 
                  control=nls.control(maxiter=1000,warnOnly=T))
          oltot_mlabove5[i] = coef(P)[1] * (100)^(coef(P)[2]) 
          } else {
          P_log = lm(log(cum_mlabove5) ~ log(tx))
          mltot_log_mlabove5[i] <- exp(coefficients(P_log)[1]) * (100)^(coefficients(P_log)[2]) }
          } else {
          mltot_mlabove5[i] <- NA 
          mltot_log_mlabove5[i] <- NA
          cumj[i] <- NA
          }
    }
    
    ## if log-transformation is used:
    if (logtransform==T) {
      oltot_olunder2 <- oltot_log_olunder2 
      oltot_ol2to3   <- oltot_log_ol2to3 
      oltot_ol3to4   <- oltot_log_ol3to4 
      oltot_ol4to5   <- oltot_log_ol4to5    
      oltot_olabove5 <- oltot_log_olabove5  
      mltot_mlunder2 <- mltot_log_mlunder2 
      mltot_ml2to3   <- mltot_log_ml2to3
      mltot_ml3to4   <- mltot_log_ml3to4  
      mltot_ml4to5   <- mltot_log_ml4to5
      mltot_mlabove5 <- mltot_log_mlabove5
    }
    
    ## rootztot is the sum of roots from all soil layers and root diameter class. ATTENTION!!!! ncol=10 is a problem. Can't sort it out.
    rootztot = c(colSums(t(matrix(data=c(oltot_olunder2, oltot_ol2to3, oltot_ol3to4, oltot_ol4to5, oltot_olabove5, mltot_mlunder2, mltot_ml2to3, mltot_ml3to4, mltot_ml4to5, mltot_mlabove5), ncol=10)),na.rm=T)) # sum of all different root size classes after 100 min.
    rootztota = c(colSums(t(matrix(data=c(cuma, cumb, cumc, cumd, cume, cumf, cumg, cumh, cumi, cumj), ncol=10)),na.rm=T))                                                                                        # sum of different root size classes for 4 timesteps, so after 40 min rather than 100 min.
    yearic = c(yearic1)
    monthic = c(monthic1)
    plotic = c(plotic1)

} else if (option==2) {
  
  ## All timeic==1 values are investigated (time index that equal 1, i.e. for 10 min time step)
  ix = which(timeic==1) # This is timestep number, not the minutes (i.e. timestep 1, not 5 min).
  
  olx <- matrix(data=NA, nrow=length(ix), ncol=3)  ## build matrix for organic layer
  yearic1  <- NULL
  monthic1 <- NULL
  plotic1  <- NULL
  
  ## Loop in which relevant data from olabove2mm, ol2to3mm, etc. are selected (according to their relevant time step!):
  n = 1
  for (i in (1:length(ix))) {
    olx[n,]     = c(OLic[ix[i]],OLic[ix[i]+1],OLic[ix[i]+2])
    yearic1[n]  = yearic[ix[i]]
    monthic1[n] = monthic[ix[i]]
    plotic1[n]  = plotic[ix[i]]
    n = n+1
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

# dzz is the correction for fine root productivity below 30cm. dzz can be specified in fine_root_cor.

if (fine_root_cor=="Default") {
tubeh=0
# depth profile of roots
depic = (30-tubeh/10)/100
dzz = 0.5*(exp(-7.344*depic)+exp(-1.303*depic)) 
} else {
  dzz <- fine_root_cor 
}

# In David's study, 37% of the fine roots (<2mm) were below 30cm, this is close to 39% found by this equation.
# Please note: there is a discrepancy between here and the RAINFOR manual (2.3, pp. 47), because the assumption there is 45% in the top 30 cm of the soil.

# Introduce tube diameter
tubed = tubed  
#sum total carbon from roots (diameter ~ 14cm, depth ~ 30cm)
ciric = (3.14*tubed^2) # surface area m2
volic = ciric*depic
rootztot[is.na(rootztot)] = 0 # Chris: why do we need to replace NA by 0 here? can we delete?
totaic = rootztot / (1-dzz)   # total roots estimated by extrapolating timesteps, plus roots growing below 30cm estimated with the correction factor dzz.
icalA = (totaic/ciric)*10000/(2.1097*1000*1000)  # Mg roots per ha (10000m2 = 1ha, 1Mg = 1000000g divide by 2 for carbon)
                                                                   
rooiA = (rootztot-rootztota)/rootztot
monthicA = monthic
yearicA = yearic
                                                                   
fir_mon = 1
fir_mone = 12
fir_year = min(yearicA,na.rm=T)
fir_yeare = max(yearicA,na.rm=T)

toticalA <- matrix(data=NA, nrow=12, ncol=(fir_yeare-fir_year+1), dimnames=list(c(month.name),fir_year:fir_yeare))
toticalAstd <- matrix(data=NA, nrow=12, ncol=(fir_yeare-fir_year+1), dimnames=list(c(month.name),fir_year:fir_yeare))

#convert to MgC ha month
n=1
for (j in fir_year:fir_yeare) {
    m=1
    for (i in fir_mon:12) {
        ind = which(monthicA==i & yearicA==j)
        toticalA[m,n] = mean(icalA[ind],na.rm=T)
        #convert to se
        toticalAstd[m,n] = (sd(icalA[ind],na.rm=T))/sqrt(16)
        m=m+1
    }
    n=n+1
}


### Build list that contains matrices with monthly values
{
  rootbio.data.monthly.matrix <- list(toticalA,toticalAstd)
  names(rootbio.data.monthly.matrix) <- c("toticalA","toticalAstd")
}

### Restructure the data (according to time series structure). Build data.frame with NPProot time series data:
{
Year <- NULL
Month <- NULL
Day <- NULL

for (i in 1:(max(yearicA,na.rm=T)-min(yearicA,na.rm=T)+1)) {
  Year[((i-1)*12+1):((i-1)*12+12)] <- (rep(c(min(yearicA,na.rm=T):max(yearicA,na.rm=T))[i],12))
  Month[((i-1)*12+1):((i-1)*12+12)] <- (1:12)
  Day[((i-1)*12+1):((i-1)*12+12)] <- rep(NA,12)
}

npproot.data.monthly.ts <- data.frame(Year, Month, Day, c(toticalA), c(toticalAstd))
colnames(npproot.data.monthly.ts) <- c("Year", "Month", "Day", "toticalA", "toticalAstd")
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
       monthly.means.matrix = {return(npproot.data.monthly.matrix)},
       monthly.means.ts = {return(npproot.data.monthly.ts)}
)

}

x <- mean(npproot.data.monthly.ts$toticalA, na.rm=T)
NPProot_MgCHaYr <- x*12

y <- mean(npproot.data.monthly.ts$toticalAstd, na.rm=T)
NPProot_MgCHaYr_sd <- y*12