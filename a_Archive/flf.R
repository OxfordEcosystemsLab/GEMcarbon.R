### Function fine litter fall: 
# This function uses data to calculate NPP from fine litterfall.

## Required Data:
#% year  
#% month  
#% plot	
#% Ponto	--- Point
#% Folhas	--- Leaves
#% Galhos	--- Branches
#% Flores	--- Flowers
#% Frutos	--- Fruits
#% Sementes	--- Seeds
#% Brom	--- Bromeliads
#% Epiph --- Epiphytes
#% Nao identif.

## Read-In of Chris' data:
library(R.matlab)
#setwd("C:/Users/Cecile/Dropbox/Carbon_Use_Efficieny_R/Original-matlab-code/Ken_Iq-Tang")
setwd("D:/Dokumente/My Dropbox/Carbon_Use_Efficieny_R/Original-matlab-code/Ken_Iq-Tang")
dir()
Dataall <- readMat(con="Dataall.mat")

#setwd("C:/Users/Cecile/Dropbox/Carbon_Use_Efficieny_R/R-functions(v3)")
setwd("D:/Dokumente/My Dropbox/Carbon_Use_Efficieny_R/R-functions(v3)")
# fine litterfall data:
data.flf <- data.frame(Dataall$FLFall)

names(data.flf) <- c("year","month","plot","point","leaves","branches","flowers","fruits",
                     "seeds","bromeliads","epiphytes","not_identified")
# adjust options:
plotname = 1
plotsize=1  ### VARIABLE PLOTSIZE IS NOT YET INCLUDED: DISCUSS WITH CECILE, HOW TO INCLUDE IT...



## Read-In of Sam's data:
#data.flf <- read.table("D:/Dokumente/My Dropbox/Carbon_Use_Efficieny_R/testing/flf_sam_test.csv", sep=",", header=T)
data.flf <- read.table("C:/Users/Cecile/Dropbox/Carbon_Use_Efficieny_R/testing/flf_sam_test.csv", sep=",", header=T)

colnames(data.flf) <- c("year", "month", "plot", "collector", "leaves", "branches", "flowers", "fruits", "seeds", "bromeliads",
                        "epiphytes", "not_identified", "", "palm_leaves", "palm_flower","palm_fruit", "quality", "comments", "day")
str(data.flf)
# adjust options:
plotname = 1


# flf <- function(data.flf, plotname, ret="monthly.means.ts", plotit=F) {   # plotsize=1                                                                                     
  
  plotfA = data.flf$plot #%(1=esp, 2=way, 3=sp1, 4=sp2)  
  yearfA = data.flf$year[which(plotname==plotfA)]
  monthfA = data.flf$month[which(plotname==plotfA)]
  #pointfA = data.flf$point[which(plotname==plotfA)]
  leaffA = data.flf$leaves[which(plotname==plotfA)]   #%leaf
  branchfA = data.flf$branches[which(plotname==plotfA)]
  flowerfA = data.flf$flowers[which(plotname==plotfA)]
  fruitfA = data.flf$fruits[which(plotname==plotfA)]
  seedsfA = data.flf$seeds[which(plotname==plotfA)]
  BromfA = data.flf$bromeliads[which(plotname==plotfA)]
  EpiphfA = data.flf$epiphytes[which(plotname==plotfA)]
  otherfA = data.flf$not_identified[which(plotname==plotfA)]
  
  ### Here should go some sanity checks of the inputs!!
  
  
  ### Calculates total litterfall (sum of branches, leaves, flowers, fruits, seeds, Broms, Epiphs, other...):
  totalfA <- NULL
  for (i in 1:length(yearfA)) {
    totalfA[i] = sum(leaffA[i],branchfA[i],flowerfA[i],fruitfA[i],seedsfA[i],
                    BromfA[i],EpiphfA[i],otherfA[i],
                    na.rm=T)
  }
  
  totalfA[which(totalfA>300)] <- NA   # remove outliers with totalf > 300
  totalfA[which(totalfA<0)] <- NA     # remove implausible totallf (negative litter)
  
  # Calculate leaf area ****need density from photos assume average SLA =
  # 100g/m2
  # leaflaifA = leaffA/100   # convert to area     
  

  fir_mon = 1
  fir_mone = 12
  fir_year = min(yearfA,na.rm=T)
  fir_yeare = max(yearfA,na.rm=T)
  
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
  
  
  # multiply small plot by 400 = 10,000/25*(0.5*0.5)m2 = 1600
  # convert to MgC ha month # multiply by 2 because collected twice a month
  la = (10000/0.25)*2
    
  ## calculate monthly means in each year:
  for (j in fir_year:fir_yeare) {
    m=1
    for (i in fir_mon:12) {
      ind = which(monthfA==i & yearfA==j)
      
      # multiply by 2 because collected twice a month
      totflfAs[m,n] = mean(totalfA[ind],na.rm=T)*(la/(2.032*1000000)) # Mg/ha convert to carbon divide by 2
      totflfAsstd[m,n] = (sd(totalfA[ind],na.rm=T)*(la/(2.032*1000000)))/sqrt(25)  # Mg/ha
            
      # multiply by 2 because collected twice a month
      # leaflaifAs[m,n] = mean(leaflaifA[ind],na.rm=T)*2;
      
      #Toby would prefer like this:
      #numptsthismonth=length(ind) #normally 2 but may be 1 or 3
      #leaflaifAs[m,n] = sum(leaflaifA[ind],na.rm=T) = mean(leaflaifA[ind],na.rm=T)*numptsthismonth
      
      
      seedsfAs[m,n] = mean(seedsfA[ind],na.rm=T)*(la/(2.032*1000000));
      seedsfAsstd[m,n] = sd(seedsfA[ind],na.rm=T)*(la/(2.032*1000000)); # divide by /sqrt(25)? 
      
      leafflfAs[m,n] = mean(leaffA[ind],na.rm=T)*(la/(2.032*1000000));
      leafflfAsstd[m,n] = sd(leaffA[ind],na.rm=T)*(la/(2.032*1000000))/sqrt(25);
    
      fruitflfAs[m,n] = mean(fruitfA[ind],na.rm=T)*(la/(2.032*1000000));
      fruitflfAsstd[m,n] = (sd(fruitfA[ind],na.rm=T)*(la/(2.032*1000000)))/sqrt(25);

      flowerflfAs[m,n] = mean(flowerfA[ind],na.rm=T)*(la/(2.032*1000000));
      flowerflfAsstd[m,n] = (sd(flowerfA[ind])*(la/(2.032*1000000)))/sqrt(25);
      
      branchflfAs[m,n] = mean(branchfA[ind],na.rm=T)*(la/(2.032*1000000));
      branchflfAsstd[m,n] = (sd(branchfA[ind],na.rm=T)*(la/(2.032*1000000)))/sqrt(25);
      
      #pointfAa = pointfA[ind]      
      m=m+1;
    }
    n=n+1;
  }
    
  totflfAs[which(totflfAs==0)] <- NaN
  
  
  ## Build list with matrices that contain monthly values:
{
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
}
  ###  Build data frame with time series structure
{
  ##Restructure the data (according to time series structure):
  Year <- NULL
  Month <- NULL
  Day <- NULL
  
  for (i in 1:dim(totflfAs)[2]) {
    Year[((i-1)*12+1):((i-1)*12+12)] <- (rep(c(min(yearfA,na.rm=T):max(yearfA,na.rm=T))[i],12))
    Month[((i-1)*12+1):((i-1)*12+12)] <- (1:12)
    Day[((i-1)*12+1):((i-1)*12+12)] <- rep(NA,12)
  }
  
 flf.data.monthly.ts <- data.frame(Year,Month,Day,
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
    par(mfrow=c(3,1))
    par(mar=c(4,4,0.5,0.5))
    plot(x=strptime(dates, format="%Y-%m-%d"), y=flf.data.monthly.ts$totflfAs, 
         type='l',lwd=2,
       xlab="Years", ylab="Total Fine Litterfall [MgC ha-1 mo-1]")
    legend("topleft", c("Total Litterfall"), lty=c(1), bty='n')
    
    ylim1 <- max(c(branchflfAs,leafflfAs,fruitflfAs),na.rm=T)
    plot(x=strptime(dates, format="%Y-%m-%d"), y=flf.data.monthly.ts$leafflfAs, 
         type='l',lty=1,
         xlab="Years", ylab="Litterfall Components [MgC ha-1 mo-1]", ylim=c(0,ylim1))
    lines(x=strptime(dates, format="%Y-%m-%d"), y=flf.data.monthly.ts$branchflfAs, lty=2)
    lines(x=strptime(dates, format="%Y-%m-%d"), y=flf.data.monthly.ts$fruitflfAs, lty=3)
    legend("topleft", c("Leaves","Branches","Fruits"), lty=c(1,2,3), bty='n')
    
    ylim2 <- max(c(seedsfAs,flowerflfAs),na.rm=T)
    plot(x=strptime(dates, format="%Y-%m-%d"), y=flf.data.monthly.ts$seedsfAs, 
         type='l',lty=1,
         xlab="Years", ylab="Litterfall Components [MgC ha-1 mo-1]", ylim=c(0,ylim2))
    lines(x=strptime(dates, format="%Y-%m-%d"), y=flf.data.monthly.ts$flowerflfAs, lty=2)
    legend("topleft", c("Seeds","Flowers"), lty=c(1,2), bty='n')
  }
  
  
# Return either monthly means (ret="monthly.means") or annual means (ret="annual.means")  
  switch(ret,
         monthly.means.matrix = {return(flf.data.monthly.matrix)},
         monthly.means.ts = {return(flf.data.monthly.ts)}
         )
}