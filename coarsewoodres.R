# Coarse litter respiration

# This program calculates monthly averages of coarse wood and repiration
# from this wood

# This function requires the following data:
# dataw:      data.frame that contains the following columns: see below
#             coarse litter (mass of coarse litter hanging around along the coarse woody debris transect)
# datar:      data.frame that contains the following columns: see below
#             coarse litter respiration (chamber measurements)
# plotname:   String or number indicating which plot should be analysed
# ret:          format of return value, indicated with strings
# plotit:       logical, whether function should be plotted

## Chris' test data:
library(R.matlab)
setwd("C:/Users/Cecile/Dropbox/Carbon_Use_Efficieny_R/Original-matlab-code/Ken_Iq-Tang")
setwd("D:/Dokumente/My Dropbox/Carbon_Use_Efficieny_R/Original-matlab-code/Ken_Iq-Tang")
dir()
Dataall <- readMat(con="Dataall.mat")

setwd("C:/Users/Cecile/Dropbox/Carbon_Use_Efficieny_R/R-functions(v3)")
setwd("D:/Dokumente/My Dropbox/Carbon_Use_Efficieny_R/R-functions(v3)")

# coarse woody biomass and respiration:
dataw = data.frame(Dataall$CWDall)     # coarse litter (mass of coarse litter hanging around along the coarse woody debris transect)
datar = data.frame(Dataall$ResCWDall)  # coarse litter respiration (chamber measurements)

# get rid of these when we have the csv files
names(dataw) <- c("year","month","plot","transect","section","decomposition_class",
                  "dry_weight_1","dry_weight_2","width_1","width_2","width_3","width_4",
                  "length_1","length_2")
names(datar) <- c("year","month","plot","decomposition_level","co2_flux","temperature_C",
                  "water_conc","chamber_height_cm","flux","dwr","diameter","length")

plotname=1
usenewfluxcor=T

# coarsewoodres <- function(dataw, datar, plotname, usenewfluxcor=F,
#                          ret="monthly.means.ts", plotit=F) {

# Establish 4 100 meter transects, 1 meter wide
# cut all dead wood pieces >2 cm diameter, record diameter and length
# within the transect, note seperately 2-5cm, 5-10, and >10cm
# 15 catagories: 3 diameter, 5 decomposition
# See ss.4.2 (p. 63) in the RAINFOR manual


  yearw = dataw$year
  monthw = dataw$month
  plotw = dataw$plot_code
  transw = dataw$transect  
  decompw  = dataw$decomposition_class
  dryweight = colMeans(t(data.frame(dataw$dry_weight_1,dataw$dry_weight_2)),na.rm=T)
  diameter1w = colMeans(t(data.frame(dataw$width_1,dataw$width_2,dataw$width_3,dataw$width_4)),na.rm=T)
  lenw = colMeans(t(data.frame(dataw$length_1,dataw$length_2)),na.rm=T)


diameterA <- NULL
lengthA <- NULL
volA <- NULL
surface_areaA <- NULL
dryweightA <- NULL
decompA <- NULL
monthwA <- NULL
yearwA <- NULL

# Coarse woody surface area per plot per unit ground area
n=1
for (i in 1:length(plotw)) {
    if (plotw[i]==plotname) {
        diameterA[n] = diameter1w[i]/1000  #%convert mm to m
        lengthA[n] = lenw[i]/100   #%convert cm to m;;
        volA[n] = 3.142 * (diameterA[n] / 2)^2 * lengthA[n]   #%m3
        surface_areaA[n] = (2 * 3.142 * ((diameterA[n] / 2)^2)) + (2 * 3.142 * (diameterA[n] / 2) * lengthA[n])
        dryweightA[n] = dryweight[i]
        decompA[n] = decompw[i]
        monthwA[n] = monthw[i]
        yearwA[n] = yearw[i]
        n=n+1
    }
}


# Coarse wood respiration: INPUT DATA TO EVALUATE MINIMUM YEAR, etc.
# Collect 5 representative wood pieces (5cm) from each decomposition
# catagory every 2 months

# input variables (already input above)
yearr = datar$year
monthr = datar$month
plotr = datar$plot
decompr = datar$decomposition_level  #decomposition level
co2r = datar$co2_flux     # co2 flux  
tempr = datar$temperature_C    # %Temp.	(C)
waterr = datar$water_conc  #%water concentration %
chr = datar$chamber_height_cm     #%chanber height (cm)	
fluxr = datar$flux  #%Flux
dwr = datar$dwr    #%Flux
diamr = datar$diameter/1000  #; %diameter mm to m
lengr = datar$length/1000  #; %length mm to m

# calculate mean surface area for each decomposition catagory for each month in each year:
fir_year = min(c(yearw,yearr),na.rm=T)
fir_yeare = max(c(yearw,yearr),na.rm=T)

# initialize variables for the loop
# 1:5 are different decomposition classes

# saa is surface area
saa1 <- matrix(data=NA, nrow=12, ncol=fir_yeare-fir_year+1, dimnames=list(c(month.name),fir_year:fir_yeare))
saa2 <- matrix(data=NA, nrow=12, ncol=fir_yeare-fir_year+1, dimnames=list(c(month.name),fir_year:fir_yeare))
saa3 <- matrix(data=NA, nrow=12, ncol=fir_yeare-fir_year+1, dimnames=list(c(month.name),fir_year:fir_yeare))
saa4 <- matrix(data=NA, nrow=12, ncol=fir_yeare-fir_year+1, dimnames=list(c(month.name),fir_year:fir_yeare))
saa5 <- matrix(data=NA, nrow=12, ncol=fir_yeare-fir_year+1, dimnames=list(c(month.name),fir_year:fir_yeare))

# va is volume
va1 <- matrix(data=NA, nrow=12, ncol=fir_yeare-fir_year+1, dimnames=list(c(month.name),fir_year:fir_yeare))
va2 <- matrix(data=NA, nrow=12, ncol=fir_yeare-fir_year+1, dimnames=list(c(month.name),fir_year:fir_yeare))
va3 <- matrix(data=NA, nrow=12, ncol=fir_yeare-fir_year+1, dimnames=list(c(month.name),fir_year:fir_yeare))
va4 <- matrix(data=NA, nrow=12, ncol=fir_yeare-fir_year+1, dimnames=list(c(month.name),fir_year:fir_yeare))
va5 <- matrix(data=NA, nrow=12, ncol=fir_yeare-fir_year+1, dimnames=list(c(month.name),fir_year:fir_yeare))

# mass
ma1 <- matrix(data=NA, nrow=12, ncol=fir_yeare-fir_year+1, dimnames=list(c(month.name),fir_year:fir_yeare))
ma2 <- matrix(data=NA, nrow=12, ncol=fir_yeare-fir_year+1, dimnames=list(c(month.name),fir_year:fir_yeare))
ma3 <- matrix(data=NA, nrow=12, ncol=fir_yeare-fir_year+1, dimnames=list(c(month.name),fir_year:fir_yeare))
ma4 <- matrix(data=NA, nrow=12, ncol=fir_yeare-fir_year+1, dimnames=list(c(month.name),fir_year:fir_yeare))
ma5 <- matrix(data=NA, nrow=12, ncol=fir_yeare-fir_year+1, dimnames=list(c(month.name),fir_year:fir_yeare))

ma1std <- matrix(data=NA, nrow=12, ncol=fir_yeare-fir_year+1, dimnames=list(c(month.name),fir_year:fir_yeare))
ma2std <- matrix(data=NA, nrow=12, ncol=fir_yeare-fir_year+1, dimnames=list(c(month.name),fir_year:fir_yeare))
ma3std <- matrix(data=NA, nrow=12, ncol=fir_yeare-fir_year+1, dimnames=list(c(month.name),fir_year:fir_yeare))
ma4std <- matrix(data=NA, nrow=12, ncol=fir_yeare-fir_year+1, dimnames=list(c(month.name),fir_year:fir_yeare))
ma5std <- matrix(data=NA, nrow=12, ncol=fir_yeare-fir_year+1, dimnames=list(c(month.name),fir_year:fir_yeare))

# this loop calculates surface area, volume and branch NPP for each month in each year
n=1
for (j in fir_year:fir_yeare) {
    m=1
    for (i in 1:12) {
        im = which(monthwA==i & yearwA==j);
        xc = surface_areaA[im];
        x1 = decompA[im];
        xx1 = which(x1==1);
        saa1[m,n] = (sum(xc[xx1],na.rm=T))/400;
        xx2 = which(x1==2);
        saa2[m,n] = (sum(xc[xx2],na.rm=T))/400;
        xx3 = which(x1==3);
        saa3[m,n] = (sum(xc[xx3],na.rm=T))/400;
        xx4 = which(x1==4);
        saa4[m,n] = (sum(xc[xx4],na.rm=T))/400;
        xx5 = which(x1==5);
        saa5[m,n] = (sum(xc[xx5],na.rm=T))/400;
        
        #%calculate volume for each month
        va1[m,n] = sum(volA[which(x1==1)],na.rm=T)/400            #;% surface area per m2
        va2[m,n] = sum(volA[which(x1==2)],na.rm=T)/400            #;% surface area per m2
        va3[m,n] = sum(volA[which(x1==3)],na.rm=T)/400            #;% surface area per m2
        va4[m,n] = sum(volA[which(x1==4)],na.rm=T)/400            #;% surface area per m2
        va5[m,n] = sum(volA[which(x1==5)],na.rm=T)/400            #;% surface area per m2
        
        #%calculate branch NPP for each month
        xac = dryweightA[im]
        xa1 = decompA[im]
        xxa1 = which(xa1==1)
        ma1[m,n] = (sum(xac[xxa1],na.rm=T))/400
        ma1std[m,n] = (sd(xac[xxa1],na.rm=T))/400
        xxa2 = which(xa1==2)
        ma2[m,n] = (sum(xac[xxa2],na.rm=T))/400
        ma2std[m,n] = (sd(xac[xxa2],na.rm=T))/400
        xxa3 = which(xa1==3)
        ma3[m,n] = (sum(xac[xxa3],na.rm=T))/400
        ma3std[m,n] = (sd(xac[xxa3],na.rm=T))/400
        xxa4 = which(xa1==4)
        ma4[m,n] = (sum(xac[xxa4],na.rm=T))/400
        ma4std[m,n] = (sd(xac[xxa4],na.rm=T))/400
        xxa5 = which(xa1==5)
        ma5[m,n] = (sum(xac[xxa5],na.rm=T))/400
        ma5std[m,n] = (sd(xac[xxa5],na.rm=T))/400
        m=m+1
    }
    n=n+1
}


## Coarse wood respiration: Data INPUT was performed above
## estimates surface area of each piece, according to RAINFOR Manual, page 61 
SArn <- NULL
for (i in 1:length(lengr)) {
SArn[i] = (2 * 3.142 * ((diamr[i] / 2)^2)) + (2 * 3.142 * (diamr[i] / 2) * lengr[i]);
}

# remove outliers >3*SD
outliers = mean(fluxr,na.rm=T)+3*sd(fluxr,na.rm=T)
fluxr[which(fluxr < 0)] <- NA
fluxr[which(fluxr > outliers)] <- NA


# The CPY-2 defaults are Volume = 2465 cm3  Area = 170 cm2 and V/A = 1450
Vd = 1171/1000000   # chamber volume m3, see RAINFOR Manual, page 73

# need to calculate surface area of wood
A1 = mean(SArn,na.rm=T)   # estimate for iquitos because no data

# Fill chamber height missing values with mean chamber height:
xchr=mean(chr,na.rm=T)
for (i in 1:length(chr)) {
    if (is.na(chr[i])) {
        chr[i]=xchr
    }
}

# Fill NA's with mean temperature:
xtempr=mean(tempr,na.rm=T)
for (i in 1:length(tempr)) {
    if (is.na(tempr[i])) {
      tempr[i]=xtempr;
    }
}

Va = A1*(chr/100)  # additional volume m3

# initialize variables in the loop:
SAcwdA <- NULL
RucA <- NULL
RcA <- NULL
monthrA <- NULL
yearrA <- NULL
decomprA <- NULL

## Calculate fluxes and flux correction for the selected plot (plotr==plotname)
n=1 
for (i in 1:length(monthr)) {
    if (plotr[i]==plotname) {
        SAcwdA[n] = 0.00011*dwr[i]+0.026
        A = 0.00011*dwr[i]+0.026
        if (usenewfluxcor==T) { # if statement to see if we are in the plot we want to be in 
        RucA[n]=(fluxr[i])*(1013/1000)*(273/(tempr[i]+273))*(44.01/22.47)*(Vd/A)/1000*3600  
        ## this is new flux correction based on RAINFOR, p. 75
        } else {
        RucA[n]=(fluxr[i])*(1013/1000)*(273/(tempr[i]+273))*(44.01/22.41)*(Vd/A)/1000*3600 }
        # chamber volume correction: See RAINFOR Manual, page 93, or Metcalfe et al (2009)
        RcA[n]= (RucA[n]*A/Vd*(Va[i]+Vd)/A)*6.312   # convert to umol m-2 s-1
        monthrA[n] = monthr[i]
        yearrA[n] = yearr[i]
        decomprA[n] = decompr[i]
        n=n+1  
    }
}

# Calculate surface areas, etc.:
# initialize variables/ mean fluxes in each month:
ra1 <- matrix(data=NA, nrow=12, ncol=fir_yeare-fir_year+1, dimnames=list(c(month.name),fir_year:fir_yeare))
ra2 <- matrix(data=NA, nrow=12, ncol=fir_yeare-fir_year+1, dimnames=list(c(month.name),fir_year:fir_yeare))
ra3 <- matrix(data=NA, nrow=12, ncol=fir_yeare-fir_year+1, dimnames=list(c(month.name),fir_year:fir_yeare))
ra4 <- matrix(data=NA, nrow=12, ncol=fir_yeare-fir_year+1, dimnames=list(c(month.name),fir_year:fir_yeare))
ra5 <- matrix(data=NA, nrow=12, ncol=fir_yeare-fir_year+1, dimnames=list(c(month.name),fir_year:fir_yeare))

ra1std <- matrix(data=NA, nrow=12, ncol=fir_yeare-fir_year+1, dimnames=list(c(month.name),fir_year:fir_yeare))
ra2std <- matrix(data=NA, nrow=12, ncol=fir_yeare-fir_year+1, dimnames=list(c(month.name),fir_year:fir_yeare))
ra3std <- matrix(data=NA, nrow=12, ncol=fir_yeare-fir_year+1, dimnames=list(c(month.name),fir_year:fir_yeare))
ra4std <- matrix(data=NA, nrow=12, ncol=fir_yeare-fir_year+1, dimnames=list(c(month.name),fir_year:fir_yeare))
ra5std <- matrix(data=NA, nrow=12, ncol=fir_yeare-fir_year+1, dimnames=list(c(month.name),fir_year:fir_yeare))

# This loop averages CWD respiration for each decomposition class
# for loops that run through each year, and each month im is the index to select surrent month & year
n=1
for (j in fir_year:fir_yeare) {
    m=1
    for (i in 1:12) {
        im = which(monthrA==i & yearrA==j)
        x1 = decomprA[im]
        RescwdAm = RcA[im]   ## RcA is corrected flux in units of umol m-2 s-1
        
        # calculate means for each decomprA in each month and year
        ra1[m,n] = mean(RescwdAm[which(x1==1)],na.rm=T)
        ra1std[m,n] = sd(RescwdAm[which(x1==1)],na.rm=T)
        ra2[m,n] = mean(RescwdAm[which(x1==2)],na.rm=T)
        ra2std[m,n] = sd(RescwdAm[which(x1==2)],na.rm=T)
        ra3[m,n] = mean(RescwdAm[which(x1==3)],na.rm=T)
        ra3std[m,n] = sd(RescwdAm[which(x1==3)],na.rm=T)
        ra4[m,n] = mean(RescwdAm[which(x1==4)],na.rm=T)
        ra4std[m,n] = sd(RescwdAm[which(x1==4)],na.rm=T)
        ra5[m,n] = mean(RescwdAm[which(x1==5)],na.rm=T)
        ra5std[m,n] = sd(RescwdAm[which(x1==5)],na.rm=T)
        
        m=m+1
    }
    n=n+1
}

## Sanity check: set all surface area that equal 0 to NA. THis could happen before all the for loops?
saa1[which(saa1==0)] <- NA
saa2[which(saa2==0)] <- NA
saa3[which(saa3==0)] <- NA
saa4[which(saa4==0)] <- NA
saa5[which(saa5==0)] <- NA

## take means over all month (e.g. January means, February means, etc.)
saa1fg = colMeans(t(saa1), na.rm=T)  ## i.e. vector of monthly flux means of decomposition class 1
saa2fg = colMeans(t(saa2), na.rm=T)  ## "" decomposition class 2
saa3fg = colMeans(t(saa3), na.rm=T)
saa4fg = colMeans(t(saa4), na.rm=T)
saa5fg = colMeans(t(saa5), na.rm=T)
                                                                                     
resCWDalla <- matrix(data=NA, nrow=12, ncol=fir_yeare-fir_year+1, dimnames=list(c(month.name),fir_year:fir_yeare))
resCWDallastd <- matrix(data=NA, nrow=12, ncol=fir_yeare-fir_year+1, dimnames=list(c(month.name),fir_year:fir_yeare))

## loop combines surface area (saa1) and flux data (ra1)
for (i in 1:12) {
    for (j in 1:(fir_yeare-fir_year+1)) {
        rrsA1 = ra1[i,j]*saa1[i,j]
        if  (!is.na(ra1[i,j]) && is.na(saa1[i,j])) {
            if (!is.na(saa1fg[i])) {
                rrsA1 = ra1[i,j]*saa1fg[i] # e.g. multiply flux in feb 2009 * surface area in feb 2009
            } else if (i==12) {
                rrsA1 = ra1[i,j]*saa1fg[i-1]
            } else {
                rrsA1 = ra1[i,j]*saa1fg[i+1]
            }
        }
        rrsA2 = ra2[i,j]*saa2[i,j]        
        if  (!is.na(ra2[i,j]) && is.na(saa2[i,j])) {
          if (!is.na(saa2fg[i])) {
            rrsA2 = ra2[i,j]*saa2fg[i]
          } else if (i==12) {
            rrsA2 = ra2[i,j]*saa2fg[i-1]
          } else {
            rrsA2 = ra2[i,j]*saa2fg[i+1]
          }
        }
        rrsA3 = ra3[i,j]*saa3[i,j]        
        if  (!is.na(ra3[i,j]) && is.na(saa3[i,j])) {
          if (!is.na(saa3fg[i])) {
            rrsA3 = ra3[i,j]*saa3fg[i]
          } else if (i==12) {
            rrsA3 = ra3[i,j]*saa3fg[i-1]
          } else {
            rrsA3 = ra3[i,j]*saa3fg[i+1]
          }
        }
        rrsA4 = ra4[i,j]*saa4[i,j]        
        if  (!is.na(ra4[i,j]) && is.na(saa4[i,j])) {
          if (!is.na(saa4fg[i])) {
              rrsA4 = ra4[i,j]*saa4fg[i];
          } else if (i==12) {
              rrsA4 = ra4[i,j]*saa4fg[i-1];
          } else {
              rrsA4 = ra4[i,j]*saa4fg[i+1]; 
          }
        }
        rrsA5 = ra5[i,j]*saa5[i,j]        
        if  (!is.na(ra5[i,j]) && is.na(saa5[i,j])) {
          if (!is.na(saa5fg[i])) {
              rrsA5 = ra5[i,j]*saa5fg[i];
          } else if (i==12) {
              rrsA5 = ra5[i,j]*saa5fg[i-1];
          } else {
              rrsA5 = ra5[i,j]*saa5fg[i+1]; 
          }
        }
        resCWDalla[i,j] = sum(c(rrsA1,rrsA2,rrsA3,rrsA4,rrsA5),na.rm=T)
        resCWDallastd[i,j] = sum(c((ra1std[i,j]*saa1[i,j]),(ra2std[i,j]*saa2[i,j]),(ra3std[i,j]*saa3[i,j]),(ra4std[i,j]*saa4[i,j]),(ra5std[i,j]*saa5[i,j])),na.rm=T)
    }
}


### Initializes variables 
# resCWDallra: mean fluxes
# NPPCWDa: NPP branch turnover
resCWDallra <- matrix(data=NA, nrow=12, ncol=fir_yeare-fir_year+1, dimnames=list(c(month.name),fir_year:fir_yeare))
resCWDallrastd <- matrix(data=NA, nrow=12, ncol=fir_yeare-fir_year+1, dimnames=list(c(month.name),fir_year:fir_yeare))
NPPCWDa <- matrix(data=NA, nrow=12, ncol=fir_yeare-fir_year+1, dimnames=list(c(month.name),fir_year:fir_yeare))
NPPCWDastd <- matrix(data=NA, nrow=12, ncol=fir_yeare-fir_year+1, dimnames=list(c(month.name),fir_year:fir_yeare))

for (i in 1:12) {
    for (j in 1:(fir_yeare-fir_year+1)) {
    resCWDallra[i,j] = mean(c(ra1[i,j],ra2[i,j],ra3[i,j],ra4[i,j],ra5[i,j]),na.rm=T)
    resCWDallrastd[i,j] = mean(c(ra1std[i,j],ra2std[i,j],ra3std[i,j],ra4std[i,j],ra5std[i,j]),na.rm=T)

    NPPCWDa[i,j] = sum(c(ma1[i,j],ma2[i,j],ma3[i,j],ma4[i,j],ma5[i,j]),na.rm=T) # g/m2month
    NPPCWDastd[i,j] = sum(c(ma1std[i,j],ma2std[i,j],ma3std[i,j],ma4std[i,j],ma5std[i,j]),na.rm=T) #%kg/m2month ??
    }
}

# convert to Mg ha-1 month-1
convert = (2592000*12*10000)/(1000000*1000000)    # coarse wood respiration in ha
resCWDallac = resCWDalla*convert;
resCWDallacstd=resCWDallastd*convert;

resCWDallrac=resCWDallra*convert;
resCWDallracstd=resCWDallrastd*convert;

convert2 = 10000/(2.1097*1000*1000)           # convert to MgC ha and 50% carbon
NPPCWDac = NPPCWDa*convert2    # MgC ha
NPPCWDacstd = NPPCWDastd*convert2   # MgC ha

resCWDalla[which(resCWDalla==0)] <- NA
resCWDallastd[which(resCWDallastd==0)] <- NA

resCWDallrac[which(resCWDallrac==0)] <- NA
resCWDallracstd[which(resCWDallracstd==0)] <- NA

NPPCWDac[which(NPPCWDac==0)] <- NA
NPPCWDacstd[which(NPPCWDacstd==0)] <- NA

resCWDallac[which(resCWDallac==0)] <- NA
resCWDallacstd[which(resCWDallacstd==0)] <- NA

## Build list with matrices that contain monthly values:
{
  coarsewoodres.monthly.matrix <- list(NPPCWDac,NPPCWDacstd,
                                       resCWDallac,resCWDallacstd)
  names(coarsewoodres.monthly.matrix) <- c("NPPCWDac","NPPCWDacstd",
                                           "resCWDallac","resCWDallacstd")
}

###  Build data frame with time series structure
{
##Restructure the data (according to time series structure):
  Year <- NULL
  Month <- NULL
  Day <- NULL
  
  for (i in 1:dim(NPPCWDac)[2]) {
    Year[((i-1)*12+1):((i-1)*12+12)] <- (rep(c(fir_year:fir_yeare)[i],12))
    Month[((i-1)*12+1):((i-1)*12+12)] <- (1:12)
    Day[((i-1)*12+1):((i-1)*12+12)] <- rep(NA,12)
  }
  
  coarsewoodres.monthly.ts <- data.frame(Year,Month,Day,
                                     c(NPPCWDac),c(NPPCWDacstd),
                                     c(resCWDallac),c(resCWDallacstd))
  
  colnames(coarsewoodres.monthly.ts) <- c("Year","Month","Day",  
                                      "NPPCWDac","NPPCWDacstd",
                                      "resCWDallac","resCWDallacstd")
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
  #par(mfrow=c(3,1))
  par(mar=c(4,4,0.5,0.5))
  plot(x=strptime(dates, format="%Y-%m-%d"), y=coarsewoodres.monthly.ts$NPPCWDac, 
       type='l',lwd=2,
       xlab="Years", ylab="Total Fine Litterfall [Units]")
  legend("topleft", c("NPP branch turnover"), lty=c(1), bty='n')
}


# Return either monthly means (ret="monthly.means") or annual means (ret="annual.means")  
switch(ret,
       monthly.means.matrix = {return(coarsewoodres.monthly.matrix)},
       monthly.means.ts = {return(coarsewoodres.monthly.ts)}
       )

}