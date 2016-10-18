### Function NPP small trees
# This function calculates the NPP of trees smaller than 10 cm.
# Based on matlab code developed by Chris Doughty, 2011.
# Last edited: Cecile Girardin, 20.09.2015 

### Required Data:
# data.frame of small tree census
# start_census: data frame for the first year of census, should include 
# running_measurements: data frame for subsequent census years, should include
# plotname: specify plotname of which plot should be calculated (eg. WAY-01)
# allometric_option:

# TESTING with ACJ data
setwd("/Users/cecile/Dropbox/GEMcarbondb/db_csv/db_csv_2015/readyforupload_db/acj_pan_2015/")
start_census <- read.table("census_smalltrees_ACJ.csv",  sep=",", header=T)
start_census$density_g_cm3 <- start_census$wood_density_g_cm3
start_census$DAP_cm <- (start_census$dbh_northsouth_cm + start_census$dbh_westeast_cm)/2
start_census$height <- start_census$tree_height_m

running_measurements <- start_census

plotname = "ACJ"
allometric_option = "Default"


setwd("C:/Users/Cecile/Dropbox/Carbon_Use_Efficieny_R/Original-matlab-code/Ken_Iq-Tang/data/")
dir()
start_census <- read.xlsx2("Stle10Ken_actual.xlsx", sheetName="start",colClasses=rep("numeric", 6))
colnames(start_census) <- c("plot_code", "tree_tag", "density_g_cm3", "DAP_cm", "DAP_altura_m", "height")

running_measurements <- read.xlsx2("Stle10Ken_actual.xlsx", sheetName="running", colClasses=rep("numeric", 10))
colnames(running_measurements) <- c("plot_code", "tree_tag", "subplot", "wood_density", "DAP_cm", "pom_height", "dbh_mm", "year", "month", "day")


# read from csv files.
start_census <- read.table("Stle10Ken_actual_start.csv", header=T, sep=";")
colnames(start_census) <- c("plot_code", "tree_tag", "density_g_cm3", "DAP_cm", "DAP_altura_m", "height")

running_measurements <- read.table("Stle10Ken_actual_running.csv", header=T, sep=";")
colnames(running_measurements) <- c("plot_code", "tree_tag", "subplot", "wood_density", "DAP_cm", "pom_height", "dbh_mm", "year", "month", "day")

## get allometric equation file:
source("/Users/cecile/Documents/GitHub/GEMcarbon.R/allometric_equations_2014.R")

## adjust options:
plotname=1
allometric_option="Default"
start_census
running_measurements

## run function test-wise:
#smallTreeNPP(start_census, running_measurements, plotname=1, allometric_option="Default")

############### COMMENT FROM CECILE: WHY DO WE NEED start_census & running_measurements? They are both in the same table in the db.

smallTreeNPP <- function(start_census, running_measurements, plotname, allometric_option="Default") {

## Set of allometric equations after Chave et al. 2005 and Chave et al. 2014 are defined in allometricEquations.R. Options defined here:
if (allometric_option == 2 | allometric_option == "dry") {
  allometrix <- 2
  print("dry equation  is used for estimating AGB, model I.3 (see Chave et al., 2005)")
} 
else if (allometric_option == 3 | allometric_option == "moist" | allometric_option == "Default" | allometric_option == 1) {
  allometrix <- 3
  print("moist equation  is used for estimating AGB, model I.6 (see Chave et al., 2005)")
} 
else if (allometric_option == 4 | allometric_option == "wet") {
  allometrix <- 4
  print("wet equation  is used for estimating AGB, model I.3 (see Chave et al., 2005)")
} 
else if (allometric_option == 5 | allometric_option == "Chave2014") {
  allometrix <- 5
  print("pantropical equation is used for estimating AGB, model (4) (see Chave et al., 2014)")
} 
else {
  print("Please specify a valid allometric_option!")
  return()
}

# define variables
par = start_census$plot_code
Tnumcen = as.numeric(start_census$tree_tag)
densitys = as.numeric(start_census$density_g_cm3)
diameters = as.numeric(start_census$DAP_cm) #in cm
heights = as.numeric(start_census$height) #in cm
dendros = as.numeric(running_measurements$DAP_cm)  

dendros[which(dendros< -1000)] <- NA #Remove implausible values

xdensitys <- mean(densitys, na.rm=T)

# Replace missing values in densitys with mean of density
for (i in 1:length(densitys)) {  
  if (is.na(densitys[i])) densitys[i] <- xdensitys  
} 


#stem surface area m2
Rbs = diameters/200 # estimated radii at base of stem convert cm to m ### meters not m2
Rts = diameters/400 # estimated radii at top of stem
xheights = mean(heights, na.rm=T) #average small tree height
heights[!is.finite(heights)] <- xheights  # heights are averaged over the entire data set, not only the plot...!

#stem surface area per plot per unit ground area

## find all trees in the data with plotname:
ind <- which(par==plotname)
TnumcenA <- Tnumcen[ind]   # trees that are in the data

# AstemsA = 3.142*(Rbs[ind]+Rts[ind])*sqrt(heights^2+((Rbs[ind]-Rts[ind])^2)) # Replace with heightsa
# Toby thinks this should be 3.1416*((Rbs*sqrt(Rbs^2+heightsa^2))-(Rts^2)) # from equation 4 area of a truncated cone.

## see Chambers et al, 2004: RESPIRATION FROM A TROPICAL FOREST ECOSYSTEM: PARTITIONING OF SOURCES AND LOW CARBON USE EFFICIENCY
#SAIpsA = 10^(-0.015-0.686*log10(diameters[ind])+(2.208*log10(diameters[ind])^2)-(0.627*log10(diameters[ind])^3))
SAIpsA <- Chambers2004_surfaceArea(diameters[ind]) # new code using allometric equation in external file
diametersA = diameters[ind]
densitysA = densitys[ind]
heightsA = heights[ind]

# calculate dendroalsA in the same format as it is in the original code:
dendroalsA <- list()
dates <- list()

## this loop for each tree has two advantages compared to the previous code:
# 1) it allows for different measurement dates for particular trees
# 2) allows for different lengths of measurement time series

# loop runs through all trees with plotname==plotname
for (ii in 1:length(TnumcenA)) {
  ## calculates temporary index which indexes all measurements of a particular tree:
  temp_ind <- which(running_measurements$plot_code == plotname & running_measurements$tree_tag == TnumcenA[ii])
  
  dendroalsA[ii] <- running_measurements$DAP_cm[temp_ind]
  dates[ii] <- strptime(paste(running_measurements$year[temp_ind], running_measurements$month[temp_ind], running_measurements$day[temp_ind], sep="-"), format="%Y-%m-%d")
}


er = 0.1 # .1cm
#(AGB, D in cm, q g/cm3, H in m)

NPPbiosA <- list()
NPPbiosAer <- list()

for (ii in 1:length(TnumcenA)) {
  diaxs    <- dendroalsA[ii] #  cm
  diaxser  <- er + as.numeric(diaxs) #convert circum to diameter ### WHY? should be /pi, not + 0.1.
  den_tree <- densitysA[ii]
  h_tree   <- heightsA[ii]
  
  ## the following line has no effect: is this intended? # CG & Toby: yes (height/noheight), but we need to add a few more options here.
  # Sebastian: Well, I understand the intention. But if there's no height, you'll run into an error message... 
  # I could include an option to calculate without height, e.g. if there's no height available?
  #nor = densitysA[tree_ind] *exp(-1.499 + 2.148*log(diaxs) + 0.207*(log(diaxs))^2 -0.0281 *log(diaxs)^3)
  
  ##new calculation using allometric equations in external file:
  if (allometrix == 2) {
    nor <- Chave2005_dry(diax = diaxs, density = den_tree, height = h_tree)
  } else if (allometrix == 3) {
    nor <- Chave2005_moist(diax = diaxs, density = den_tree, height = h_tree)
  } else if (allometrix == 4) {
    nor <- Chave2005_wet(diax = diaxs, density = den_tree, height = h_tree)
  } else if (allometrix == 5) {
    nor <- Chave2014(diax = diaxs, density = den_tree, height = h_tree)
  }
  
  ## error treatment remains to be done!
  norer = 0.0509*(diaxser)^2*den_tree*h_tree 
  
  #Toby:
  #If error on diaxs is er then error on (diaxs^2) is 2diaxs*er (rule of quadrature). Equation on
  #line 90 is basically nor=k*(diaxs^2) where k is constant (assuming no error
  #on k=0.0509*densitysA[tree_ind]*heights ) so error on nor should be norer=abs(k)*2*abs(diaxs)*er 
  
  # unit conversion must be done here; is not included in the allometric equation file
  NPPbiosA[ii] = (nor)*(1/(2.1097*1000))    # convert kgto Mg=1/1000=10 and convert to carbon = 50%
  NPPbiosAer[ii] = (norer*(1/(2.1097*1000)))# convert kgto Mg=1/1000=10 and convert to carbon = 50%                       
}

##################################################################################################################

for (ii in 1:length(TnumcenA)) {    # this is just looping through each row.
  dbh_tree <- cen$dbh[ii]
  den_tree <- cen$density[ii]
  h_tree   <- cen$height_m[ii]
  
  # this uses allometric equations from allometric Equations.R
  if (allometrix == 2) {
    bm <- Chave2005_dry(diax=dbh_tree, density=den_tree, height=h_tree)
  } else if (allometrix == 3) {
    bm <- Chave2005_moist(diax=dbh_tree, density=den_tree, height=h_tree)
  } else if (allometrix == 4) {
    bm <- Chave2005_wet(diax=dbh_tree, density=den_tree, height=h_tree)
  } else if (allometrix == 5) {
    bm <- Chave2014(diax=dbh_tree, density=den_tree, height=h_tree)
  }
  
  cen$agC[ii] <- (bm)*(1/(2.1097*1000)) # convert kg to Mg=1/1000=10 and convert to carbon = 47.8%
}
############################################################################################



## find global start and end month:
min_date <- NULL
max_date <- NULL

for (i in 1:length(TnumcenA)) {
  min_date[i] <- as.character(min(dates[[i]]))
  max_date[i] <- as.character(max(dates[[i]]))
}

fir_year <- as.numeric(format(min(strptime(min_date, format="%Y-%m-%d")), format="%Y"))
last_year <- as.numeric(format(max(strptime(max_date, format="%Y-%m-%d")), format="%Y"))

## Build NPP matrix for all trees (denNPPbiosA):
# number of columns (i.e. months:)
denNPPbiosA <- array(data=NA, dim=c(length(TnumcenA),(last_year-fir_year+1)*12))
denNPPbiosAer <- array(data=NA, dim=c(length(TnumcenA),(last_year-fir_year+1)*12))  # this is not the error. it is the same value as above.
dates_monthly <- seq.Date(from=as.Date(paste(fir_year,"-01-01", sep=""), format="%Y-%m-%d"),
                          to=as.Date(paste(last_year,"-12-01", sep=""), format="%Y-%m-%d"), by="1 months")

## Build daily NPP matrix:
dates_daily <- seq.Date(from=as.Date(paste(fir_year,"-01-01", sep=""), format="%Y-%m-%d"),
                        to=as.Date(paste(last_year,"-12-31", sep=""), format="%Y-%m-%d"), by=1)


## loop runs through each tree:
for (tree_ind in 1:length(TnumcenA)) {
  npp_daily <- rep(NA,length(dates_daily))
  npp_daily_er <- rep(NA, length(dates_daily))
  
  ## interpolation is based on the temporary index: (removes NAs to perform interepolation!)
  temp_index <-  which(!is.na(NPPbiosA[[tree_ind]]))
  
  # calculate daily NPP interpolations (i.e. remove NAs):
  temp_npp <-  NPPbiosA[[tree_ind]][temp_index]
  temp_npp_er <- NPPbiosA[[tree_ind]][temp_index]
  temp_dates <-  dates[[tree_ind]][temp_index]
  
  for (j in 1:(length(temp_npp)-1)) {
    ## daily growth rate:
    npp_daily[which(as.Date(temp_dates[j])==dates_daily):(which(as.Date(temp_dates[j+1])==dates_daily)-1)] <- (temp_npp[j+1]-temp_npp[j])/as.numeric(temp_dates[j+1]-temp_dates[j])
    npp_daily_er[which(as.Date(temp_dates[j])==dates_daily):(which(as.Date(temp_dates[j+1])==dates_daily)-1)] <- (temp_npp_er[j+1]-temp_npp_er[j])/as.numeric(temp_dates[j+1]-temp_dates[j])
  }
  
  ## loop over all months in the data:
  for (m in 1:length(dates_monthly)) {
    # index of all days in a particular months (to be used with npp_daily):
    npp_month_ind <- which(format(dates_daily, format="%Y-%m") == format(dates_monthly[m], format="%Y-%m"))
    denNPPbiosA[tree_ind, m] <- mean(npp_daily[npp_month_ind], na.rm=T)*length(npp_month_ind)
    #for error calculate NPP for tree with 1mm bigger diameter and
    #diff is the error
    denNPPbiosAer[tree_ind, m] <- mean(npp_daily_er[npp_month_ind], na.rm=T)*length(npp_month_ind)
  }
}

#convert to MgC ha month 
NPPwoodsA = colSums(denNPPbiosA, na.rm=T)
NPPwoodsAstd = colSums(denNPPbiosAer, na.rm=T)
# set NAs (i.e. columns that are entirely NA!):
for (i in 1:length(dates_monthly)) {
  if(all(is.na(denNPPbiosA[,i]))) {
    NPPwoodsA[i] <- NA}
  if(all(is.na(denNPPbiosAer[,i]))) {
    NPPwoodsAstd[i] <- NA}
}
names(NPPwoodsA) <- dates_monthly
names(NPPwoodsAstd) <- dates_monthly

return(list(NPPwoodsA, NPPwoodsAstd))
}


