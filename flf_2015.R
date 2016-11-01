### Function fine litter fall: 
# This function uses data to calculate NPP from fine litterfall.

## Read-in data:
#setwd("/Users/cecile/Dropbox/GEMcarbondb/db_csv/db_csv_2015/readyforupload_db/acj_pan")
#data_flf <- read.table("/Users/cecile/Dropbox/GEMcarbondb/db_csv/db_csv_2015/readyforupload_db/acj_pan/Litterfall_ACJ_2013_2014_test.csv", sep=",", header=T)

# this is what we have in db:
# names(data_flf) <- c("plot_code", "year","month", "day","litterfall_trap_num", "litterfall_trap_size_m2","leaves_g_per_trap","twigs_g_per_trap","flowers_g_per_trap","fruits_g_per_trap",
# "bromeliads_g_per_trap", "epiphytes_g_per_trap","other_g_per_trap", "palm_leaves_g", "palm_flower_g", "palm_fruit_g", "quality_code", "comments")

# plotsize = 1 ha  ### TO DO: Different plot size is not an option yet. 

# Attention!! In some plots, data is collected twice a month (L. 92 : multiply by 2 because collected twice a month). In other plots, data are collected monthly. So do not multiply by 2.
# TO DO: We need to change this to divide by the collection time interval rather than *2 for collected twice a month!!

flf <- function(data_flf, plotname, ret="monthly.means.ts", plotit=F) {   # plotsize=1                                                                                     

  library(scales)
  library(zoo)
  require(ggplot2)

  if (class(data_flf) != "data.frame") { # if it's not a dataframe, assume it's a path+filename
    data_flf <- read.csv(data_flf)
  }
    
  # new data frame
  data_flf2 <- c()  
  
  # define each parameter
  data_flf2$plot    <- data_flf$plot_code[which(plotname==data_flf$plot_code)]
  data_flf2$year    <- data_flf$year[which(plotname==data_flf2$plot)]
  data_flf2         <- data.frame(data_flf2)
  data_flf2$month   <- data_flf$month[which(plotname==data_flf2$plot)]
  data_flf2$day     <- data_flf$day[which(plotname==data_flf2$plot)]
  data_flf2$date       <- as.Date(paste(data_flf2$year, data_flf2$month, data_flf2$day, sep="."), format="%Y.%m.%d") # 
  #data_flf2$date    <- as.Date(as.character(data_flf2$d,format="%Y-%m-%d")) 
  data_flf2$num     <- data_flf$litterfall_trap_num[which(plotname==data_flf2$plot)]
  data_flf2$leaves  <- data_flf$leaves_g_per_trap[which(plotname==data_flf2$plot)]   
  data_flf2$twigs   <- data_flf$twigs_g_per_trap[which(plotname==data_flf2$plot)]
  data_flf2$flowers <- data_flf$flowers_g_per_trap[which(plotname==data_flf2$plot)]
  data_flf2$fruits  <- data_flf$fruits_g_per_trap[which(plotname==data_flf2$plot)]
  data_flf2$seeds   <- NA #data_flf$seeds[which(plotname==data_flf2$plot)]
  data_flf2$brom    <- data_flf$bromeliads_g_per_trap[which(plotname==data_flf2$plot)]
  data_flf2$epi     <- data_flf$epiphytes_g_per_trap[which(plotname==data_flf2$plot)]
  data_flf2$other   <- data_flf$other_g_per_trap[which(plotname==data_flf2$plot)]
  
  ### Calculate total litterfall (sum of branches, leaves, flowers, fruits, seeds, Broms, Epiphs, other...):
  data_flf2$total   <- rowSums(data_flf2$leaves, data_flf2$twigs, data_flf2$flowers, data_flf2$fruits, data_flf2$seeds, data_flf2$brom, data_flf2$epi, data_flf2$other, na.rm = T)   
  
  ################################################################## !!!! SUM DOESN"T WORK HERE !!!!!!!!!!!!!!!!
  
  ### Sanity check of the inputs.
  
  data_flf2$total[which(data_flf2$total>300)] <- NA   # remove outliers with totalf > 300
  data_flf2$total[which(data_flf2$total<0)]   <- NA   # remove implausible totallf (negative litter)
  
  # Calculate leaf area ****need density from photos, we assume average SLA = 100g/m2
  # leaflaifA = leaffA/100   # convert to area     

  ### flf per trap per day
  
  data_flf2$codeb <- paste(data_flf2$plot, data_flf2$num, sep=".") 
  data_flf2$codew <- paste(data_flf2$plot, data_flf2$num, data_flf2$year, data_flf2$month, data_flf2$day, sep=".") 
  uid   <- unique(data_flf2$codeb)
  ww    <- c()
  xx    <- c()
  yy    <- c()
  zz    <- c()
  
    
  for (i in 1:length(data_flf2$num)) { 
    sub       <- subset(data_flf2, subset=(data_flf2$codeb == uid[i]))
    if(length(sub$codeb) > 1) {
      meas_int      <- difftime(sub$date[1:(length(sub$date)-1)], sub$date[2:length(sub$date)], units="days")
      meas_int_num  <- as.numeric(as.character(meas_int))
      meas_trap     <- tail(sub$leaves,-1)
      meas          <- meas_trap/(-meas_int_num)   
      id            <- tail(sub$codew,-1) 
      num           <- tail(sub$num,-1)
      ww            <- c(ww, num)
      xx            <- c(xx, id)
      yy            <- c(yy, meas_int_num)
      zz            <- c(zz, meas)
      print(i)
      print(length(ww))
      print(length(yy))
      print(length(xx))
      print(length(zz))
    } else {   
      print(paste("row number:", i))
      print(paste("trap number:", sub$num))
      print(paste("subset length:", length(sub$codeb)))
    }
  }
  data2 <- data.frame(cbind(ww, xx, yy, zz))
  colnames(data2) <- c("litterfall_trap_num", "id", "measurement_interval_days", "totalflf_g_per_trap_per_day")
  
  # get day, month, year from data_flf2
  
  data3 <- sqldf("SELECT data_flf2.*, data2.* FROM data2 JOIN data ON data2.codew = data_flf2.codew") 
  
  # flf per ha per measurement date (every two weeks)
  
  # flf per trap per month
  
  # flf per ha per month (average of all the traps)
  
  
  fir_mon = 1
  fir_mone = 12
  fir_year = min(yearfA, na.rm=T)
  fir_yeare = max(yearfA, na.rm=T)
  
  n=1
  
  # initialize variables for the loop:
  totflfAs <- matrix(data=NA, nrow=12, ncol=(fir_yeare-fir_year+1), dimnames=list(c(month.name),fir_year:fir_yeare))
  totflfAsstd <- matrix(data=NA, nrow=12, ncol=(fir_yeare-fir_year+1), dimnames=list(c(month.name),fir_year:fir_yeare))
  totflfAslen <- matrix(data=NA, nrow=12, ncol=(fir_yeare-fir_year+1), dimnames=list(c(month.name),fir_year:fir_yeare))
  # leaflaifAs <- matrix(data=NA, nrow=12, ncol=(fir_yeare-fir_year+1), dimnames=list(c(month.name),fir_year:fir_yeare))
  seedsfAs <- matrix(data=NA, nrow=12, ncol=(fir_yeare-fir_year+1), dimnames=list(c(month.name),fir_year:fir_yeare))
  seedsfAsstd <- matrix(data=NA, nrow=12, ncol=(fir_yeare-fir_year+1), dimnames=list(c(month.name),fir_year:fir_yeare))
  leafflfAs <- matrix(data=NA, nrow=12, ncol=(fir_yeare-fir_year+1), dimnames=list(c(month.name),fir_year:fir_yeare))
  leafflfAsstd <- matrix(data=NA, nrow=12, ncol=(fir_yeare-fir_year+1), dimnames=list(c(month.name),fir_year:fir_yeare))
  fruitflfAs <- matrix(data=NA, nrow=12, ncol=(fir_yeare-fir_year+1), dimnames=list(c(month.name),fir_year:fir_yeare))
  fruitflfAsstd <- matrix(data=NA, nrow=12, ncol=(fir_yeare-fir_year+1), dimnames=list(c(month.name),fir_year:fir_yeare))
  flowerflfAs <- matrix(data=NA, nrow=12, ncol=(fir_yeare-fir_year+1), dimnames=list(c(month.name),fir_year:fir_yeare))
  flowerflfAsstd <- matrix(data=NA, nrow=12, ncol=(fir_yeare-fir_year+1), dimnames=list(c(month.name),fir_year:fir_yeare))
  branchflfAs <- matrix(data=NA, nrow=12, ncol=(fir_yeare-fir_year+1), dimnames=list(c(month.name),fir_year:fir_yeare))
  branchflfAsstd <- matrix(data=NA, nrow=12, ncol=(fir_yeare-fir_year+1), dimnames=list(c(month.name),fir_year:fir_yeare))
  
  # TO DO: CHANGE THIS TO DATES RATHER THAN twice a month!!!!!
  # Get date of previous collection and divide by number of days. 
  # What happens with broken traps?
  # Add quality check here e.g. nothing >50
  # For the first collection interval, assume 14 days.
  
  # Conversions
  # Raw data is in g / litter trap = g / 0.25m2
  # Convert to ha: *(10000/0.25)
  # Convert to Mg: *1 g = 1.0 ?? 10-6 = 0.000001 Mg
  # Convert to C: *0.49

  # multiply small plot by 400 = 10,000/25*(0.5*0.5)m2 = 1600 (what is this? Ask Chris)
  
  # convert to g ha-1 month-1 
  # ATTENTION!! CHANGE THIS if data are collected once a month.
  # multiply by 2 if collected twice a month (see comments below on how to change this to daily).
  
  la = (10000/0.25) #*2
  
  ## calculate daily means:
  uid <- plot, trap, year, month, day
  meas_int <- date2/date1
  meas_trap <- measurement from the trap
  meas_day <- meas_trap/meas_int 
  aa <- uid  
  
  ## calculate monthly means in each year:
  for (j in fir_year:fir_yeare) {
    m=1
    for (i in fir_mon:12) {
      
      ind = which(monthfA==i & yearfA==j)
      
      totflfAs[m,n] = mean(totalfA[ind],na.rm=T)*(la/(2.032*1000000)) # g/ha/month convert to Mg C/ha/month multiply by 0.49 (=1/2.032)
      totflfAsstd[m,n] = (sd(totalfA[ind],na.rm=T)*(la/(2.032*1000000)))/sqrt(25)  # should be sqrt(length(totalfA)). This assumes you have 25 litter traps.
      
      # Terhi suggests calculating per litter trap and getting SE per litter trap:
      # Calculate MgC/ trap/ collection interval
      # loop per litter trap per. 
      # avg1 <- monthly average per trap over several years = average ((1st collection/ collection interval) , (2st collection/ collection interval)) * number of days in that month
      # avg2 <- monthly average per plot by averaging avg1.
      # se_avg1 <- sd(avg1)/sqrt(length(avg1)).
      # this would provide a unit per day. g / m2/ day.
      
      seedsfAs[m,n] = mean(seedsfA[ind],na.rm=T)*(la/(2.032*1000000))
      seedsfAsstd[m,n] = sd(seedsfA[ind],na.rm=T)*(la/(2.032*1000000))/sqrt(25) 
      
      leafflfAs[m,n] = mean(leaffA[ind],na.rm=T)*(la/(2.032*1000000))
      leafflfAsstd[m,n] = sd(leaffA[ind],na.rm=T)*(la/(2.032*1000000))/sqrt(25)
    
      fruitflfAs[m,n] = mean(fruitfA[ind],na.rm=T)*(la/(2.032*1000000))
      fruitflfAsstd[m,n] = (sd(fruitfA[ind],na.rm=T)*(la/(2.032*1000000)))/sqrt(25)

      flowerflfAs[m,n] = mean(flowerfA[ind],na.rm=T)*(la/(2.032*1000000))
      flowerflfAsstd[m,n] = (sd(flowerfA[ind],na.rm=T)*(la/(2.032*1000000)))/sqrt(25)
      
      branchflfAs[m,n] = mean(branchfA[ind],na.rm=T)*(la/(2.032*1000000))
      branchflfAsstd[m,n] = (sd(branchfA[ind],na.rm=T)*(la/(2.032*1000000)))/sqrt(25)
      
      # Add a column for reproductive material
      
      #pointfAa = pointfA[ind]      
      m=m+1
    }
    n=n+1
  }
    
  totflfAs[which(totflfAs==0)] <- NaN
  
  
  ## Build list with matrices that contain monthly values:

  flf.data.monthly.matrix <- list(
    (totflfAs),(totflfAsstd),
    (seedsfAs),(seedsfAsstd),
    (leafflfAs),(leafflfAsstd),
    (fruitflfAs),(fruitflfAsstd),
    (flowerflfAs),(flowerflfAsstd),
    (branchflfAs),(branchflfAsstd))
  
  names(flf.data.monthly.matrix) <- c("totflfAs","totflfAsstd",
                                "seedsfAs","seedsfAsstd",
                                "leafflfAs","leafflfAsstd",
                                "fruitflfAs","fruitflfAsstd",
                                "flowerflfAs","flowerflfAsstd",
                                "branchflfAs","branchflfAsstd")

  ###  Build data frame with time series structure

  ##Restructure the data (according to time series structure):
  Year <- NULL
  Month <- NULL
  Day <- NULL
  
  for (i in 1:dim(totflfAs)[2]) {
    Year[((i-1)*12+1):((i-1)*12+12)] <- (rep(c(min(yearfA,na.rm=T):max(yearfA,na.rm=T))[i],12))
    Month[((i-1)*12+1):((i-1)*12+12)] <- (1:12)
    Day[((i-1)*12+1):((i-1)*12+12)] <- rep(NA,12)
  }
  
 flf.data.monthly.ts <- data.frame(Year, Month, Day,
            c(totflfAs),c(totflfAsstd),
            c(seedsfAs),c(seedsfAsstd),
            c(leafflfAs),c(leafflfAsstd),
            c(fruitflfAs),c(fruitflfAsstd),
            c(flowerflfAs),c(flowerflfAsstd),
            c(branchflfAs),c(branchflfAsstd))
  
  colnames(flf.data.monthly.ts) <- c("Year","Month","Day",  
            "totflfAs","totflfAsstd",
            "seedsfAs","seedsfAsstd",
            "leafflfAs","leafflfAsstd",
            "fruitflfAs","fruitflfAsstd",
            "flowerflfAs","flowerflfAsstd",
            "branchflfAs","branchflfAsstd")



  ## Plotroutine, triggered by argument 'plotit=T'
  flf.data.monthly.ts$date <- strptime(paste(as.character(flf.data.monthly.ts$Year), as.character(flf.data.monthly.ts$Month), as.character(15), sep="-"), format="%Y-%m-%d")
  flf.data.monthly.ts$yearmonth <- as.yearmon(flf.data.monthly.ts$date)

  if (plotit==T) {
    top <- flf.data.monthly.ts$totflfAs+flf.data.monthly.ts$totflfAsstd
    flf.data.monthly.ts$date <- as.Date(flf.data.monthly.ts$date)
    plot1 <- ggplot(data=flf.data.monthly.ts, aes(x=date, y=totflfAs, na.rm=T)) +
                    geom_line(linetype='solid', colour='black', size=1) +
                    geom_ribbon(data=flf.data.monthly.ts, aes(ymin=totflfAs-totflfAsstd, ymax=totflfAs+totflfAsstd), alpha=0.2) +
                    scale_x_date(breaks = date_breaks("months"), labels = date_format("%b-%Y")) +            
                    scale_colour_grey() + 
                    theme(text = element_text(size=17), legend.title = element_text(colour = 'black', size = 17, hjust = 3, vjust = 7, face="plain")) +
                    ylim(0, max(top, na.rm=T)) +                          
                    xlab("") + ylab(expression(paste("Total fine litterfall (MgC ", ha^-1, mo^-1, ")", sep=""))) +
                    theme_classic(base_size = 15, base_family = "") + 
                    theme(legend.position="left") +
                    theme(panel.border = element_rect(fill = NA, colour = "black", size = 1)) 
                    #theme(legend.title=element_blank()) + theme(legend.key = element_blank())   
    
    top <- flf.data.monthly.ts$branchflfAs+flf.data.monthly.ts$branchflfAsstd
    plot2 <- ggplot(data=flf.data.monthly.ts, aes(x=date, y=branchflfAs, na.rm=T)) +
                    geom_line(linetype='solid', colour='black', size=1) +
                    geom_ribbon(data=flf.data.monthly.ts, aes(ymin=branchflfAs-branchflfAsstd , ymax=branchflfAs+branchflfAsstd), alpha=0.2) +
                    geom_line(data=flf.data.monthly.ts, aes(x=date, y=leafflfAs, na.rm=T), linetype='dotted', colour='black', size=1) +
                    geom_ribbon(data=flf.data.monthly.ts, aes(ymin=leafflfAs-leafflfAsstd, ymax=leafflfAs+leafflfAsstd), alpha=0.2) +
                    scale_x_date(breaks = date_breaks("months"), labels = date_format("%b-%Y")) +  
                    scale_colour_grey() + 
                    theme(text = element_text(size=17), legend.title = element_text(colour = 'black', size = 17, hjust = 3, vjust = 7, face="plain")) +
                    ylim(0, max(top, na.rm=T)) +                          
                    xlab("") + ylab(expression(paste("Fine litterfall components (MgC ", ha^-1, mo^-1, ")", sep=""))) +
                    theme_classic(base_size = 15, base_family = "") + 
                    theme(legend.position="left") +
                    theme(panel.border = element_rect(fill = NA, colour = "black", size = 1)) 
    
    top <- flf.data.monthly.ts$flowerflfAs + flf.data.monthly.ts$flowerflfAsstd
    plot3 <- ggplot(data=flf.data.monthly.ts, aes(x=date, y=flowerflfAs, na.rm=T)) +
                    geom_line(linetype='solid', colour='black', size=1) +
                    geom_ribbon(data=flf.data.monthly.ts, aes(ymin=flowerflfAs-flowerflfAsstd , ymax=flowerflfAs+flowerflfAsstd), alpha=0.2) +
                    geom_line(data=flf.data.monthly.ts, aes(x=date, y=fruitflfAs, na.rm=T), linetype='dotted', colour='black', size=1) +
                    geom_ribbon(data=flf.data.monthly.ts, aes(ymin=fruitflfAs-fruitflfAsstd, ymax=fruitflfAs+fruitflfAsstd), alpha=0.2) +
                    scale_x_date(breaks = date_breaks("months"), labels = date_format("%b-%Y")) +  
                    scale_colour_grey() + 
                    theme(text = element_text(size=17), legend.title = element_text(colour = 'black', size = 17, hjust = 3, vjust = 7, face="plain")) +
                    ylim(0, max(top, na.rm=T)) +                          
                    xlab("") + ylab(expression(paste("Fine litterfall components (MgC ", ha^-1, mo^-1, ")", sep=""))) +
                    theme_classic(base_size = 15, base_family = "") + 
                    theme(legend.position="left") +
                    theme(panel.border = element_rect(fill = NA, colour = "black", size = 1)) 
    
    fig <- grid.arrange(plot1, plot2, plot3, ncol=1, nrow=3)
  }
  
  
# Return either monthly means (ret="monthly.means") or annual means (ret="annual.means")  
  switch(ret,
         monthly.means.matrix = {return(flf.data.monthly.matrix)},
         monthly.means.ts = {return(flf.data.monthly.ts)}
         )
}

