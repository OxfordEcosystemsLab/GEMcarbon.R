## This function is to estimate stem_respiration based on the RAINFOR manual, p. 51
# Based on a matlab code by Chris Doughty, 2013.
# Translated to R by Sebastian Sippel, 2014.
# Last modified: Cécile Girardin, August 2015.
# First step: run the data from the EGM through this code: EGM_raw_to_flux_stem_2015 to get the "Rstem" data frame. Here, we use delta CO2 as "FluxsA" (not the flux). 

# get data: census, census_smalltrees, and Rstem_co2slope (this is the output from EGM_raw_to_flux_stem_2015)
setwd("/Users/cecile/Dropbox/Carbon_Use_Efficieny_R/Original-matlab-code/Ken_Iq-Tang/data/")
smallTree_census <- read.table("Stle10Ken_actual_start_2015.csv",  sep=",", header=T)
setwd("/Users/cecile/Dropbox/Carbon_Use_Efficieny_R/Original-matlab-code/Ken_Iq-Tang/data/largeTree_data/")
largeTree_census <- read.table("census.csv", header=T, sep=";")
setwd("/Users/cecile/Dropbox/Carbon_Use_Efficieny_R/Original-matlab-code/Ken_Iq-Tang/data/")
Rstem <- read.table("Resstemall_db_2015.csv",  sep=",", header=T)

# get allometric equations:
setwd("/Users/cecile/GitHub/GEMcarbon.R") 
dir()
Sys.setlocale('LC_ALL','C') 
source("allometric_equations_2014.R")

## This is the function command. You can use the code as a function when you uncomment this line (and the final bracket at the end of this code), or just run everything inside the {}, step by step.
#stem_respiration <- function(largeTree_census, smallTree_census, Rstem, ret="monthly.means.ts", plotname, avg_tree_height, plot_it=T) {

# load libraries
library(scales)
library(zoo)
require(ggplot2)


# large tree diameters and correction:
diameterlA = largeTree_census$DAP_cm_start[which(largeTree_census$plot_code==plotname)]   # Diameter at breast height at first census!
xdiameterl = mean(diameterlA, na.rm=T)
diameterlA[which(is.na(diameterlA))] <- xdiameterl
diameterlA[which(diameterlA==0)] = xdiameterl
# small tree diameters and correction:
diametersA = smallTree_census$DAP_cm[which(smallTree_census$plot_code==plotname)]
xdiameters = mean(diametersA, na.rm=T)
diametersA[which(is.na(diametersA))] <- xdiameters
diametersA[which(diametersA==0)] = xdiameters

# calculate treeSurfaceArea:
# Note: A. Shenkin is developing our own surface area equation. Check if this is the most up to date equation that we use.
SAIpsA <- Chambers2004_surfaceArea(diameter=diametersA) # Astem is in m?
SAIplA <- Chambers2004_surfaceArea(diameter=diameterlA) # Astem is in m?

#sum the area of trees
tree_area_sumA = (sum(SAIplA, na.rm=T)+sum(SAIpsA, na.rm=T))*(avg_tree_height/30) # Trees are an everage of 30m height in Manaus, where the Chambers equation was developed. So we need to correct for mean plot tree height here.

# Stem respiration

# plastic tube = 13 cm diameter, 3 cm length
# on tree closest to 25 soil respiration collars

# year  
# month	
# plot_code (1=BolA, 2=BolB, 3=Iq1, 4=Iq2, 5=TangA, 6=TangB)	
# plot_code num	
# tree num	
# CO2	
# T aire	
# VWC	
# Depth (cm)	
# DCO2

plot_codes = Rstem$plot_code 
yearsA = Rstem$year[which(plot_codes==plotname)] 
monthsA = Rstem$month[which(plot_codes==plotname)] 
plotnumsA = Rstem$sub_plot[which(plot_codes==plotname)]  
TnumsA = Rstem$tree_num[which(plot_codes==plotname)] 
# consA = Rstem$co2[which(plot_codes==plotname)] 
tempsA = Rstem$soil_temp_degC[which(plot_codes==plotname)] #Rstem$T_aire########################## ATTENTION!!!! THIS SHOULD BE AIR TEMP ###################
# waters = Rstem$volumetric_water_content[which(plot_codes==plotname)]  
chrA = Rstem$collar_depth_cm[which(plot_codes==plotname)] # OR Rstem$depth_co2# chamber height
fluxsA = Rstem$dco2[which(plot_codes==plotname)]  # FluxsA is actually delta CO2 in this code. We get delta CO2 from EGM_raw_to_flux_stem_2015

# remove outliers > 3*SD
outliers = mean(fluxsA, na.rm=T)+3*sd(fluxsA, na.rm=T)
fluxsA[which(fluxsA < 0)] <- NA
fluxsA[which(fluxsA > outliers)] <- NA


Vd = 1171/1000000    # chamber volume m3
A = 0.0078           # tube area m2
xchr = 0             # better replace by xchr = mean(chrA, na.rm=T)
xtemps = mean(tempsA, na.rm=T)

# replace NA's with 'mean' values
chrA[which(is.na(chrA))] <- xchr
tempsA[which(is.na(tempsA))] <- xtemps

Va = A*(chrA/100)  # additional volume m3

## calculate fluxes
RucsA = (fluxsA)*(1000/1000)*(273/(tempsA+273))*(44.01/22.41)*(Vd/A)/1000*3600
# chamber volume correction
RcsA  = (RucsA*A/Vd*(Va+Vd)/A)*6.312   # convert to umol m-2 s-1

#RucsA = (fluxsA)*(1000/1000)*(273/(tempsA+273))*((Vd+Va)/0.02241)*(1/A) #############   CHECK WITH CHRIS: what does this do??? Can we delete?

# remove outliers
RcsA[which(RcsA>8)] <- NA

## Initialize matrices with UNIFIED size (for respiration):
fir_year  = min(yearsA,na.rm=T)
last_year = max(yearsA,na.rm=T)

# initialize variables for for-loop:
resAs    <- matrix(data=NA,nrow=12,ncol=last_year-fir_year+1, dimnames=list(c(month.name),fir_year:last_year))
resAsstd <- matrix(data=NA,nrow=12,ncol=last_year-fir_year+1, dimnames=list(c(month.name),fir_year:last_year))
resAslen <- matrix(data=NA,nrow=12,ncol=last_year-fir_year+1, dimnames=list(c(month.name),fir_year:last_year))

n=1
for (j in fir_year:last_year) {
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


# Total respiration for the plot

# Total respiration from stems
# convert units umol m-2 s-1 to MgC ha-1 month-1 = 1 mo = 2592000 sec, 10000 m2 = 1 ha
# 1000000 umol = 1 mol, 1 mol = 12 g, 1000000 g = 1 Mg

# add a temperature correction from Robertson et al. Q10 = 1.5 and k = 0.04
# soRstem = Rstemi*exp(-.04*(nanmean(tempsB)-23.4))
# Long term: do we keep the temperature correction, or not (see Lucy Rowland unpublished results from Caxiuana)?

corrstresA = exp(-0.0695*3) 

convert = (2592000*12)/(1000000*1000000)  # stem respiration in ha
stem_resAc = resAs*convert*corrstresA  # MgC ha mo
stem_resAcstd = (resAsstd*convert*corrstresA)/sqrt(25) # MgC ha mo

#NO SCALING BY GROWTH RATE YET!
#scale results by growth rate
#stem_resAc = stem_resAc*rascgrA
#stem_resAcstd = stem_resAcstd*rascgrA

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
  
  stem_res.data.monthly.ts <- data.frame(Year,Month,Day,c(stem_resAc),c(stem_resAcstd))
  
  colnames(stem_res.data.monthly.ts) <- c("Year","Month","Day","stem_res","stem_resstd")
}


## plot_coderoutine, triggered by argument 'plot_it=T'
if (plot_it==T) {
  ## Time representation of Dates as character in the vector 'dates':
  stem_res.data.monthly.ts$date <- strptime(paste(as.character(2000 + stem_res.data.monthly.ts$Year), as.character(stem_res.data.monthly.ts$Month), as.character(15), sep="-"), format="%Y-%m-%d")
  stem_res.data.monthly.ts$yearmonth <- as.Date(as.yearmon(stem_res.data.monthly.ts$date))
  
  top <- stem_res.data.monthly.ts$stem_res + stem_res.data.monthly.ts$stem_resstd
  plot2 <- ggplot(data=stem_res.data.monthly.ts, aes(x=yearmonth, y=stem_res, na.rm=T)) +
                  geom_point(colour='black', size=2) +
                  scale_x_date(breaks = date_breaks("months"), labels = date_format("%b-%Y")) +  
                  scale_colour_grey() + 
                  theme(text = element_text(size=17), legend.title = element_text(colour = 'black', size = 17, hjust = 3, vjust = 7, face="plain")) +
                  ylim(0, max(top, na.rm=T)) +                          
                  xlab("") + ylab(expression(paste("Rstem (MgC ", ha^-1, mo^-1, ")", sep=""))) +
                  theme_classic(base_size = 15, base_family = "") + 
                  theme(legend.position="left") +
                  theme(panel.border = element_rect(fill = NA, colour = "black", size = 1)) 
  plot2
}

# Annual Rstem
Rstem_yr    <- (mean(stem_res.data.monthly.ts$stem_res, na.rm=T))*12
Rstem_yr_sd <- (mean(stem_res.data.monthly.ts$stem_resstd, na.rm=T))*12

# Return either monthly means (ret="monthly.means") or annual means (ret="annual.means")  
switch(ret, monthly.means.matrix = {return(stem_res.data.monthly.matrix)}, monthly.means.ts = {return(stem_res.data.monthly.ts)})

}

