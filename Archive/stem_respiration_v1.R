## this function is to estimate stem_respiration based on the RAINFOR manual, p. 51

setwd("/Users/cecile/Dropbox/Carbon_Use_Efficieny_R/Original-matlab-code/Ken_Iq-Tang/data/")
smallTree_census <- read.xlsx2("Stle10Ken_actual.xlsx", sheetName="start",
                           colClasses=rep("numeric", 6))

setwd("/Users/cecile/Dropbox/Carbon_Use_Efficieny_R/Original-matlab-code/Ken_Iq-Tang/data/largeTree_data/")
largeTree_census <- read.table("census.csv", header=T, sep=";")

## get stem respiration file:
setwd("/Users/cecile/Dropbox/Carbon_Use_Efficieny_R/Original-matlab-code/Ken_Iq-Tang/data/")
Resstemall <- read.xlsx2("Resstemall.xlsx", sheetName="Resstemall_db", colClasses=rep("numeric",10))

# get allometric equations:
source("C:/Users/Cecile/Dropbox/Carbon_Use_Efficieny_R/in-progress/allometricEquations.R")

plotname = 1

## here conmes the function command:
#stem_respiration <- function(largeTree_census, smallTree_census, Resstemall, 
#                              ret="monthly.means.ts", plotname, plotit=T) {
  

# large tree diameters and correction:
diameterlA = largeTree_census$DAP_cm_start[which(largeTree_census$plot==plotname)]   # Diameter at breast height at first census!
xdiameterl = mean(diameterlA, na.rm=T)
diameterlA[which(is.na(diameterlA))] <- xdiameterl
diameterlA[which(diameterlA==0)]=xdiameterl
# small tree diameters and correction:
diametersA = smallTree_census$DAP_cm[which(smallTree_census$plot==plotname)]
xdiameters = mean(diametersA, na.rm=T)
diametersA[which(is.na(diametersA))] <- xdiameters
diametersA[which(diametersA==0)]=xdiameters

# calculate treeSurfaceArea:
# Note: A. Shenkin is developing our own surface area equation. Check if this is the most up to date equation that we use.
SAIpsA <- Chambers2004_surfaceArea(diameter=diametersA) # Astem is in m?
SAIplA <- Chambers2004_surfaceArea(diameter=diameterlA) # Astem is in m?

#sum the area of trees
#latreesumA = (sum(SAIpA, na.rm=T)+sum(AstemsA, na.rm=T)*25)*(21.6/25) #;%scale by tall (>40cm tree height) vs 30m manaus
tree_area_sumA = sum(SAIplA) + sum(SAIpsA)*25   # no scaling; factor 25 due to RAINFOR manual, p.53

# Stem respiration

# plastic tube = 13 cm diameter, 3 cm length
# on tree closest to 25 soil respiration collars

# year  
# month	
# plot (1=BolA, 2=BolB, 3=Iq1, 4=Iq2, 5=TangA, 6=TangB)	
# plot num	
# tree num	
# CO2	
# T aire	
# VWC	
# Depth (cm)	
# DCO2

plots = Resstemall$plot # datarst[,3] # A=1 B=2
yearsA = Resstemall$year[which(plots==plotname)] # datarst[,1]
monthsA = Resstemall$month[which(plots==plotname)] # datarst[,2] 
plotnumsA = Resstemall$subplot_num[which(plots==plotname)]  # datarst[,4]
TnumsA = Resstemall$tree_num[which(plots==plotname)] # datarst[,5]
# consA = Resstemall$co2[which(plots==plotname)] # datarst[,6] 
tempsA = Resstemall$T_aire[which(plots==plotname)] # datarst[,7] 
# waters = Resstemall$volumetric_water_content[which(plots==plotname)] # datarst[,8] 
chrA = Resstemall$depth_co2[which(plots==plotname)] #datarst[,9]-2 # chamber height
fluxsA = Resstemall$dco2[which(plots==plotname)] # datarst[,10] 

# remove outliers >3*SD
outliers = mean(fluxsA, na.rm=T)+3*sd(fluxsA, na.rm=T);
fluxsA[which(fluxsA< 0)] <- NA
fluxsA[which(fluxsA>outliers)] <- NA


Vd = 1171/1000000  # chamber volume m3
A = 0.0078         # tube area m2
xchr=5             # xhr: wouldn't it be better to calculate xchr = mean(chrA, na.rm=T)
xtemps=mean(tempsA, na.rm=T);

# replace NA's with 'mean' values
chrA[which(is.na(chrA))] <- xchr
tempsA[which(is.na(tempsA))] <- xtemps

Va = A*(chrA/100)  # additional volume m3

## calculate fluxes
RucsA = (fluxsA)*(1000/1000)*(273/(tempsA+273))*(44.01/22.41)*(Vd/A)/1000*3600
# chamber volume correction
RcsA = (RucsA*A/Vd*(Va+Vd)/A)*6.312   # convert to umol m-2 s-1
RucsA = (fluxsA)*(1000/1000)*(273/(tempsA+273))*((Vd+Va)/0.02241)*(1/A)

# remove outliers
RcsA[which(RcsA>8)] <- NA

## Initialize matrices with UNIFIED size (for respiration):
fir_year = min(yearsA,na.rm=T)
fir_yeare = max(yearsA,na.rm=T)

# initialize variables for for-loop:
resAs <- matrix(data=NA,nrow=12,ncol=fir_yeare-fir_year+1, dimnames=list(c(month.name),fir_year:fir_yeare))
resAsstd <- matrix(data=NA,nrow=12,ncol=fir_yeare-fir_year+1, dimnames=list(c(month.name),fir_year:fir_yeare))
resAslen <- matrix(data=NA,nrow=12,ncol=fir_yeare-fir_year+1, dimnames=list(c(month.name),fir_year:fir_yeare))

n=1
for (j in fir_year:fir_yeare) {
  m=1
  for (i in 1:12) {
    ind = which(monthsA==i & yearsA==j)
    resAs[m,n] = tree_area_sumA*mean(RcsA[ind],na.rm=T)
    resAsstd[m,n] = tree_area_sumA*sd(RcsA[ind], na.rm=T)
    resAslen[m,n] = length(RcsA[ind])
    m=m+1
  }
  n=n+1
}


#Total respiration for the plot, 

# Total respiration from stems
# convert units umol m2s-1 to MgC ha month = 1mo=2592000sec, 10000m2=1ha,
# 1000000umol = 1 mol, 1mol = 12 g, 1000000g=1Mg

# add a temperature correction from Robertson et al Q10=1.5 and k=0.04
# soRstem=Rstemi*exp(-.04*(nanmean(tempsB)-23.4))
# Long term: do we keep the temperature correction, or not (see Lucy Rowland unpublished results from Caxiuana)?

corrstresA=exp(-0.0695*3) 

convert = (2592000*12)/(1000000*1000000)  # stem respiration in ha
stem_resAc= resAs*convert*corrstresA  # MgC ha
stem_resAcstd=(resAsstd*convert*corrstresA)/sqrt(25) # MgC ha

#NO SCALING BY GROWTH RATE YET!
#scale results by growth rate
#stem_resAc = stem_resAc*rascgrA;
#stem_resAcstd = stem_resAcstd*rascgrA;


### plotit etc:
## Build list with matrices that contain monthly values:
{
  stem_res.data.monthly.matrix <- list(
    (stem_resAc),(stem_resAcstd))
  
  names(stem_res.data.monthly.matrix) <- c("stem_res","stem_resstd")
}
###  Build data frame with time series structure
{
  ##Restructure the data (according to time series structure):
  Year <- NULL
  Month <- NULL
  Day <- NULL
  
  for (i in 1:dim(stem_resAc)[2]) {
    Year[((i-1)*12+1):((i-1)*12+12)] <- (rep(c(min(yearsA,na.rm=T):max(yearsA,na.rm=T))[i],12))
    Month[((i-1)*12+1):((i-1)*12+12)] <- (1:12)
    Day[((i-1)*12+1):((i-1)*12+12)] <- rep(NA,12)
  }
  
  stem_res.data.monthly.ts <- data.frame(Year,Month,Day,
                                    c(stem_resAc),c(stem_resAcstd))
  
  colnames(stem_res.data.monthly.ts) <- c("Year","Month","Day",  
                                     "stem_res","stem_resstd")
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
  plot(x=strptime(dates, format="%Y-%m-%d"), y=stem_res.data.monthly.ts$stem_res, 
       type='p',lwd=2,
       xlab="Years", ylab="Stem Respiration [MgC ha-1 mo-1]")
  legend("topleft", c("Stem Respiration"), lty=c(1), bty='n')
  
}


# Return either monthly means (ret="monthly.means") or annual means (ret="annual.means")  
switch(ret,
       monthly.means.matrix = {return(stem_res.data.monthly.matrix)},
       monthly.means.ts = {return(stem_res.data.monthly.ts)}
)
}

