
## This function calculates fine root productivity
# Based on Matlab code by C. Doughty (2013)
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
#setwd("C:/Users/Cecile/Dropbox/NPPflux/db_csv")
#data.ic <- read.table("C:/Users/Cecile/Dropbox/Carbon_Use_Efficieny_R/testing/ICEltr_samtest.csv", sep=",", header=T)

## adjust options:
plotname = "ESP-01"
option = 1
logtransform = T
fine_root_cor <- "Default" 
tubed = 0.07  ## diameter of tube

### adjust options, with time step option 2:
plotname = "try Sam's plots"
option = 2
logtransform = T
fine_root_cor <- "Default"
tubed = 0.07  ## diameter of tube


# The function starts here.
NPProot_ic <- function(data.ic, plotname, option = 1, logtransform=T, fine_root_cor="Default", tubed=0.07, ret="monthly.means.ts", plotit=F) {
  
# load packages
library(sqldf)
require(ggplot2)
library(scales)
library(nlme)
  
data <- subset(subset(data.ic, plot_code == plotname))

# re-name columns rather than building new ones?
data$ol_under2 <- data$ol_under_2mm_g   
data$ol_2to3   <- data$ol_2to3_mm_g 
data$ol_3to4   <- data$ol_3to4_mm_g 
data$ol_4to5   <- data$ol_4to5_mm_g   
data$ol_above5 <- data$ol_above_5mm_g 
data$ml_under2 <- data$ml_under_2mm_g                                
data$ml_2to3   <- data$ml_2to3_mm_g 	                        
data$ml_3to4   <- data$ml_3to4_mm_g 	           
data$ml_4to5   <- data$ml_4to5_mm_g 	                    
data$ml_above5 <- data$ml_above_5mm_g 
data$time_step_cum <- data$time_step*data$time_step_minutes # get cumulative time step

## TO DO: We should add sanity checks for the input data here.

# Replace NAs by 0

data$ol_under2[is.na(data$ol_under2)] <- 0
data$ol_2to3[is.na(data$ol_2to3)]     <- 0
data$ol_3to4[is.na(data$ol_3to4)]     <- 0
data$ol_4to5[is.na(data$ol_4to5)]     <- 0
data$ol_above5[is.na(data$ol_above5)] <- 0
data$ml_under2[is.na(data$ml_under2)] <- 0                        
data$ml_2to3[is.na(data$ml_2to3)]     <- 0                       
data$ml_3to4[is.na(data$ml_3to4)]     <- 0           
data$ml_4to5[is.na(data$ml_4to5)]     <- 0                  
data$ml_above5[is.na(data$ml_above5)] <- 0

data$this_core <- (paste(as.character(data$year),as.character(data$month),as.character(data$day),as.character(data$ingrowth_core_num), sep="-"))
dim(data)

# OR exclude missing values from the cum data?
# data <- na.exclude(data)

if (option == 1) {
  
  uid <- unique(data$this_core)
  xx <- c()
  aa <- c()
  bb <- c() 
  cc <- c() 
  dd <- c()
  ee <- c()
  ff <- c()
  gg <- c()
  hh <- c()
  ii <- c()
  jj <- c()
  
  for (i in 1:length(uid)) {
    sub          <- subset(data, subset=(data$this_core == uid[i]))
    id           <- tail(sub$this_core, n=1) 
    
    #ol_under2
    if  (!any(is.na(sub$ol_under2)) & sum(sub$ol_under2) > 0) {
    cumdata      <- tail(sub$ol_under2, n=4) # cumulative values for that diameter class
    tx           <- c(10,20,30,40)           # cumulative time steps in minutes WE SHOULD USE data.ic$time_step_cum
    cum1         <- cumsum(cumdata)
    if(logtransform==F){ 
      P            <- nls(cum1 ~ a * tx^b, start=list(a=1, b=1), control = nls.control(maxiter=1000, warnOnly=T))
      tot_olunder2 <- coef(P)[1] * (100)^(coef(P)[2])  # Chris used 100 mins and power law, but here we use 120 min (Khoon & Terhi).
    }else{
      P_log        <- lm(log(cum1) ~ log(tx))
      tot_olunder2 <- exp(coefficients(P_log)[1]) * (100)^(coefficients(P_log)[2])  # We use the same method as Khoon & Terhi (120 mins and log-curve as default).
    }
    } else {
    tot_olunder2 <- NA 
      #cum1 <- NA
    }
    
    #ol_2to3
    if  (!any(is.na(sub$ol_2to3)) & sum(sub$ol_2to3) > 0) {
    cumdata      <- tail(sub$ol_2to3, n=4) # cumulative values for that diameter class
    tx           <- c(10,20,30,40)           # cumulative time steps in minutes
    cum2         <- cumsum(cumdata)
    if(logtransform==F){ 
      P            <- nls(cum2 ~ a * tx^b, start=list(a=1, b=1), control = nls.control(maxiter=1000, warnOnly=T))
      tot_ol_2to3 <- coef(P)[1] * (100)^(coef(P)[2]) 
    }else{
      P_log        <- lm(log(cum2) ~ log(tx))
      tot_ol_2to3 <- exp(coefficients(P_log)[1]) * (100)^(coefficients(P_log)[2])
    }
    } else {
      tot_ol_2to3 <- NA 
      #cum2 <- NA
    }
    
    #ol_3to4
   if  (!any(is.na(sub$ol_3to4)) & sum(sub$ol_3to4) > 0) {
    cumdata      <- tail(sub$ol_3to4, n=4) # cumulative values for that diameter class
    tx           <- c(10,20,30,40)           # cumulative time steps in minutes
    cum3         <- cumsum(cumdata)
    if(logtransform==F){ 
      P            <- nls(cum1 ~ a * tx^b, start=list(a=1, b=1), control = nls.control(maxiter=1000, warnOnly=T))
      tot_ol_3to4 <- coef(P)[1] * (100)^(coef(P)[2]) 
    }else{
      P_log        <- lm(log(cum3) ~ log(tx))
      tot_ol_3to4 <- exp(coefficients(P_log)[1]) * (100)^(coefficients(P_log)[2])
    }
    } else {
     tot_ol_3to4 <- NA 
     #cum3 <- NA
    }
   
    #ol_4to5
    if  (!any(is.na(sub$ol_4to5)) & sum(sub$ol_4to5) > 0) {
    cumdata      <- tail(sub$ol_4to5, n=4) # cumulative values for that diameter class
    tx           <- c(10,20,30,40)           # cumulative time steps in minutes
    cum4         <- cumsum(cumdata)
    if(logtransform==F){ 
      P            <- nls(cum4 ~ a * tx^b, start=list(a=1, b=1), control = nls.control(maxiter=1000, warnOnly=T))
      tot_ol_4to5 <- coef(P)[1] * (100)^(coef(P)[2]) 
    }else{
      P_log        <- lm(log(cum4) ~ log(tx))
      tot_ol_4to5 <- exp(coefficients(P_log)[1]) * (100)^(coefficients(P_log)[2])
    }
    } else {
      tot_ol_4to5  <- NA 
      #cum4 <- NA
    }
   
    #ol_above5
    if  (!any(is.na(sub$ol_above5)) & sum(sub$ol_above5) > 0) {
    cumdata      <- tail(sub$ol_above5, n=4) # cumulative values for that diameter class
    tx           <- c(10,20,30,40)           # cumulative time steps in minutes
    cum5         <- cumsum(cumdata)
    if(logtransform==F){ 
      P            <- nls(cum5 ~ a * tx^b, start=list(a=1, b=1), control = nls.control(maxiter=1000, warnOnly=T))
      tot_ol_above5 <- coef(P)[1] * (100)^(coef(P)[2]) 
    }else{
      P_log        <- lm(log(cum5) ~ log(tx))
      tot_ol_above5 <- exp(coefficients(P_log)[1]) * (100)^(coefficients(P_log)[2])
    }
    } else {
      tot_ol_above5 <- NA 
      #cum5 <- NA
    }
   
    #ml_under2 # !any(is.na(sub$ml_under2)) & 
    if  (sum(sub$ml_under2) > 0) {
    cumdata      <- tail(sub$ml_under2, n=4) # cumulative values for that diameter class
    tx           <- c(10,20,30,40)           # cumulative time steps in minutes
    cum6         <- cumsum(cumdata)
    if(logtransform==F){ 
      P            <- nls(cum6 ~ a * tx^b, start=list(a=1, b=1), control = nls.control(maxiter=1000, warnOnly=T))
      tot_ml_under2 <- coef(P)[1] * (100)^(coef(P)[2]) 
    }else{
      P_log        <- lm(log(cum6) ~ log(tx))
      tot_ml_under2 <- exp(coefficients(P_log)[1]) * (100)^(coefficients(P_log)[2])
    }
    } else {
      tot_ml_under2 <- NA 
      cum6 <- NA
    }
   
    #ml_2to3
    if  (!any(is.na(sub$ml_2to3)) & sum(sub$ml_2to3) > 0) {
    cumdata      <- tail(sub$ml_2to3, n=4)  # cumulative values for that diameter class
    tx           <- c(10,20,30,40)           # cumulative time steps in minutes
    cum7         <- cumsum(cumdata)
    if(logtransform==F){ 
      P            <- nls(cum7 ~ a * tx^b, start=list(a=1, b=1), control = nls.control(maxiter=1000, warnOnly=T))
      tot_ml_2to3 <- coef(P)[1] * (100)^(coef(P)[2]) 
    }else{
      P_log        <- lm(log(cum7) ~ log(tx))
      tot_ml_2to3 <- exp(coefficients(P_log)[1]) * (100)^(coefficients(P_log)[2])
    }
    } else {
      tot_ml_2to3 <- NA 
      #cum7 <- NA
    }
   
    #ml_3to4
    if  (!any(is.na(sub$ml_3to4)) & sum(sub$ml_3to4) > 0) {
    cumdata      <- tail(sub$ml_3to4, n=4)  # cumulative values for that diameter class
    tx           <- c(10,20,30,40)           # cumulative time steps in minutes
    cum8         <- cumsum(cumdata)
    if(logtransform==F){ 
      P            <- nls(cum8 ~ a * tx^b, start=list(a=1, b=1), control = nls.control(maxiter=1000, warnOnly=T))
      tot_ml_3to4  <- coef(P)[1] * (100)^(coef(P)[2]) 
    }else{
      P_log        <- lm(log(cum8) ~ log(tx))
      tot_ml_3to4  <- exp(coefficients(P_log)[1]) * (100)^(coefficients(P_log)[2])
    }
    } else {
      tot_ml_3to4 <- NA 
      #cum8 <- NA
    }
   
    #ml_4to5
    if  (!any(is.na(sub$ml_4to5)) & sum(sub$ml_4to5) > 0) {
    cumdata      <- tail(sub$ml_4to5, n=4)  # cumulative values for that diameter class
    tx           <- c(10,20,30,40)           # cumulative time steps in minutes
    cum8         <- cumsum(cumdata)
    if(logtransform==F){ 
      P            <- nls(cum8 ~ a * tx^b, start=list(a=1, b=1), control = nls.control(maxiter=1000, warnOnly=T))
      tot_ml_4to5  <- coef(P)[1] * (100)^(coef(P)[2]) 
    }else{
      P_log        <- lm(log(cum8) ~ log(tx))
      tot_ml_4to5  <- exp(coefficients(P_log)[1]) * (100)^(coefficients(P_log)[2])
    }
    } else {
      tot_ml_4to5 <- NA 
      #cum8 <- NA
    }
   
    #ml_above5
    if  (!any(is.na(sub$ml_above5)) & sum(sub$ml_above5) > 0) {
    cumdata      <- tail(sub$ml_above5, n=4)  # cumulative values for that diameter class
    tx           <- c(10,20,30,40)           # cumulative time steps in minutes
    cum9         <- cumsum(cumdata)
    if(logtransform==F){ 
      P            <- nls(cum9 ~ a * tx^b, start=list(a=1, b=1), control = nls.control(maxiter=1000, warnOnly=T))
      tot_ml_above5  <- coef(P)[1] * (100)^(coef(P)[2]) 
    }else{
      P_log        <- lm(log(cum9) ~ log(tx))
      tot_ml_above5  <- exp(coefficients(P_log)[1]) * (100)^(coefficients(P_log)[2])
    }
    } else {
      tot_ml_above5 <- NA 
      #cum9 <- NA
    }
   
    xx       <- rbind(xx, id)
    aa       <- rbind(aa, tot_olunder2)
    bb       <- rbind(bb, tot_ol_2to3)
    cc       <- rbind(cc, tot_ol_3to4)
    dd       <- rbind(dd, tot_ol_4to5)
    ee       <- rbind(ee, tot_ol_above5)
    ff       <- rbind(ff, tot_ml_under2)
    gg       <- rbind(gg, tot_ml_2to3)
    hh       <- rbind(hh, tot_ml_3to4)
    ii       <- rbind(ii, tot_ml_4to5)
    jj       <- rbind(jj, tot_ml_above5)
  }
  
  ################## TEMPORARY FIX ###########################################
  #data2a <-  data.frame(xx, as.numeric(as.character(aa)))
  #colnames(data2a) <- c("this_core", "rootztot")
  #############################################################################
  
  data2a <- data.frame(cbind(xx, as.numeric(as.character(aa)), as.numeric(as.character(bb)), as.numeric(as.character(cc)), as.numeric(as.character(dd)), as.numeric(as.character(ee)), as.numeric(as.character(ff)), as.numeric(as.character(gg)), as.numeric(as.character(hh)), as.numeric(as.character(ii)), as.numeric(as.character(jj))))
  colnames(data2a) <- c("this_core", "tot_olunder2", "tot_ol2to3", "tot_ol3to4", "tot_ol4to5", "tot_olabove5", "tot_mlunder2", "tot_ml2to3", "tot_ml3to4", "tot_ml4to5", "tot_mlabove5")
  
} else if (option==2) {
  # see NPProot_2015.R code for details
  }
  
  data2 <- data.frame(cbind(xx, as.numeric(as.character(aa)))) #, bb, cc, dd, ee, ff, gg, hh, ii, jj))
  colnames(data2) <- c("this_core", "tot_olunder2", "", "")
  
}

## rootztot is the sum of roots in the soil layers.
data2a$tot_olunder2 <- as.numeric(as.character(data2a$tot_olunder2))
data2a$tot_ol2to3   <- as.numeric(as.character(data2a$tot_ol2to3))
data2a$tot_ol3to4   <- as.numeric(as.character(data2a$tot_ol3to4))
data2a$tot_ol4to5   <- as.numeric(as.character(data2a$tot_ol4to5))
data2a$tot_olabove5 <- as.numeric(as.character(data2a$tot_olabove5))
data2a$tot_mlunder2 <- as.numeric(as.character(data2a$tot_mlunder2))
data2a$tot_ml2to3   <- as.numeric(as.character(data2a$tot_ml2to3))
data2a$tot_ml3to4   <- as.numeric(as.character(data2a$tot_ml3to4))
data2a$tot_ml4to5   <- as.numeric(as.character(data2a$tot_ml4to5))
data2a$tot_mlabove5 <- as.numeric(as.character(data2a$tot_mlabove5))

df <- data.frame(data2a$tot_olunder2, data2a$tot_ol2to3, data2a$tot_ol3to4, data2a$tot_ol4to5, data2a$tot_olabove5, data2a$tot_mlunder2, data2a$tot_ml2to3, data2a$tot_ml3to4, data2a$tot_ml4to5, data2a$tot_mlabove5)
zz <- rowSums(df, na.rm=T)
data2a$rootztot <- zz

data3 <- sqldf("SELECT data2a.this_core, AVG(data2a.rootztot), data.year, data.month, data.day FROM data2a JOIN data ON data2a.this_core = data.this_core GROUP BY data2a.this_core")
colnames(data3) <- c("this_core","rootztot","year","month","day")

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
data3$ciric = (3.14*tubed^2) # surface area m2
data3$volic = data3$ciric*depic
data3$rootztot[is.na(data3$rootztot)] = 0 
data3$totaic = data3$rootztot / (1-dzz)   # total roots estimated by extrapolating timesteps, plus roots growing below 30cm estimated with the correction factor dzz.
data3$ic_MgCha = (data3$totaic/data3$ciric)*10000/(2.1097*1000*1000)  # Mg roots per ha (10000m2 = 1ha, 1Mg = 1000000g divide by 2 for carbon)
                                                                   
# convert to MgC ha month #
# replace 0 by NA
data3[data3 == 0] <- NA

data4 <- sqldf("SELECT data3.year, data3.month, data3.day, AVG(data3.ic_MgCha), STDEV(data3.ic_MgCha) FROM data3 GROUP BY data3.month")
colnames(data4) <- c("year", "month", "day", "threemonthlyNPProot", "threemonthlyNPProot_sd")

data4$d     <- as.character(paste(data4$month, data4$day, data4$year, sep="/")) 
data4$date  <- as.Date(data4$d, "%m/%d/%Y")
data4 <- sqldf("SELECT data4.* FROM data4 ORDER BY data4.year, data4.month, data4.day ASC")

c_time <- as.POSIXlt(data4$date)
c_time <- rev(c_time)
tt <- difftime(c_time[1:(length(c_time)-1)] , c_time[2:length(c_time)])
data4$interval <- c(90, tt)
  
#xx <- as.numeric(difftime(data4$date[(2:n)], data4$date[1:(n-1)], units="days"))
#data4$interval <- c(90, xx)

data4$monthlyNPProot    <- (as.numeric(data4$threemonthlyNPProot)/data4$interval) * 30 # TO DO: We should change this to the number of days in that month. need a loop.
data4$monthlyNPProot_se <- ((as.numeric(data4$threemonthlyNPProot_sd)/sqrt(16))/data4$interval) * 30 

(mean(data4$monthlyNPProot, na.rm=T))*12
(mean(data4$monthlyNPProot_se, na.rm=T))*12

## Plotroutine, triggered by argument 'plotit=T'
if (plotit==T) {
  ## ggplot2 of root npp vs time
  
  top <- data4$monthlyNPProot + data4$monthlyNPProot_se
  plot1 <- ggplot(data=data4, aes(x=date, y=monthlyNPProot, na.rm=T)) +
           geom_line(linetype='solid', colour='black', size=1) +
           geom_ribbon(data=data4, aes(ymin=monthlyNPProot-monthlyNPProot_se, ymax=monthlyNPProot+monthlyNPProot_se), alpha=0.2) +
           scale_x_date(breaks = date_breaks("months"), labels = date_format("%b-%Y")) +            
           scale_colour_grey() + 
           theme(text = element_text(size=17), legend.title = element_text(colour = 'black', size = 17, hjust = 3, vjust = 7, face="plain")) +
           ylim(0, max(top, na.rm=T)) +                          
           xlab("") + ylab(expression(paste("NPP fine root (MgC ", ha^-1, mo^-1, ")", sep=""))) +
           theme_classic(base_size = 15, base_family = "") + 
           theme(legend.position="left") +
           theme(panel.border = element_rect(fill = NA, colour = "black", size = 1)) 
  
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