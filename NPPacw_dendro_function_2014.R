# Written by: Cécile Girardin September 2014

## Calculation of large tree NPP from dendrometer data

# dendrometers measured every 3 months
# assumes all trees and lianas over 10 cm measured at 1.3 meters

# requires two .csv files: 
# census   <- read.csv() 
# dendrometer <- read.csv() 


##################### Temporary lines to test the code #################
setwd("C:/Users/Cecile/Documents/GitHub/GEMcarbon.R/example files")
dendrometer <- read.table("DendEsp_2012.csv", header=TRUE, sep=",", na.strings=c("NA", "NaN", ""), dec=".", strip.white=TRUE)

setwd("C:/Users/Cecile/Documents/GitHub/GEMcarbon.R/example files")
census <- read.table("CensusEsp_clean.csv", header=TRUE, sep=",", na.strings=c("NA", "NaN", ""), dec=".", strip.white=TRUE)
  
plotname = "ESP-01"
allometric_option = "Default"
height_correction_option = "Default"

setwd("C:/Users/Cecile/Documents/GitHub/GEMcarbon.R") 
dir()
source("allometric_equations_2014.R")
source("NPPacw_census_function_2014.R")
source("largetreebiomass_census.R")

########################################################################

#NPPacw_dendro <- function(census, dendrometer, plotname, allometric_option="Default", height_correction_option="Default") {


  # load libraries
  library(sqldf)

## get data for all trees that are in the plot selected from census & dendrometer files
cen1  <- subset(census, plot_code==plotname)
cen   <- subset(cen1, year==2006)  ### MAKE THIS INTO AN OPTION
dend1 <- subset(dendrometer, plot_code==plotname)

# re-name year, month, day in cen
cen$cenyear  <- cen$year
cen$cenmonth <- cen$month
cen$cenday   <- cen$day

## get the data you need from the census file into the dendrometer data frame: density, height, first dbh measurement, date of first dbh measurement
dend <- sqldf("SELECT dend1.*, cen.density, cen.height_m, cen.dbh, cen.cenyear, cen.cenmonth, cen.cenday FROM cen JOIN dend1 ON cen.tree_tag = dend1.tree_tag")
#write.csv(dend, file="dendtest.csv")  


  ## Allometric equation option. Set of allometric equations after Chave et al. 2005 and Chave et al. 2014 are defined in allometricEquations.R. Options defined here:
  if (allometric_option == 2 | allometric_option == "dry") {
    allometrix <- 2
    print("dry equation  is used for estimating AGB, model I.3 (see Chave et al., 2005)")
  } else if (allometric_option == 3 | allometric_option == "moist" | allometric_option == "Default" | allometric_option == 1) {
    allometrix <- 3
    print("moist equation  is used for estimating AGB, model I.6 (see Chave et al., 2005)")
  } else if (allometric_option == 4 | allometric_option == "wet") {
    allometrix <- 4
    print("wet equation  is used for estimating AGB, model I.3 (see Chave et al., 2005)")
  } else if (allometric_option == 5 | allometric_option == "Chave2014") {
    allometrix <- 5
    print("pantropical equation is used for estimating AGB, model (4) (see Chave et al., 2014)")
  } else {
    print("Please specify a valid allometric_option!")
    return()
  }
  

## Height correction options
if (height_correction_option == 1 | height_correction_option == "Default" ) {
  predheight <- 1
  print("If you have height for more than 50 trees in your plot, estimate local diameter-height relationship. If not, choose height correction option 2.")
} else if (height_correction_option == 2) {
  predheight <- 2
  print("height correction estimated as described by Feldpauch et al. (2012). Please check Feldpauch regional parameters in the code. Default is Brazilian shield.")
} else {
  print("Please specify a valid height_correction_option!")
  return()
}


## Correct for missing tree heights
# missing height function

h.est=function(dbh, h){
  l      =lm(h~dbh)
  coeffs = coefficients(l)
  pred.h = coeffs[1] + coeffs[2]*dbh
}

# Option 2: you have height for less than 50 trees in your plot. Use Fedpauch equation.

#ADD PARAMETER: Feldpauch region. 

## Feldpauch correction procedure for heights, diameters and densitys:
# Brazilian shield
Bo    = 0.6373
B1    = 0.4647  # E.C. Amazonia

So1   = 0.012  # E.C. Amazonia
Abar  = 20.4  # mean cenetered basal area m-2 ha-1

n01   = 0.0034 # E.C. Amazonia
Pvbar = 0.68 # mean centered precipitation coefficient of variation

n02   = -0.0449 # E.C. Amazonia
Sdbar = 5     # mean centered dry season length no months less than 100mm

n03   = 0.0191  # E.C. Amazonia
Tabar = 25.0  # mean centered annual averge temperature


# Define height options
if (predheight == 1) {
  w <- which(is.na(cen$height_m))
  h.pred <- h.est(cen$dbh, cen$height_m)
  cen$height_m[w] <- h.pred[w]
} else if (predheight == 2) {
  w <- which(is.na(cen$height_m))
  cen$height_m[w] <- 10^(Bo + B1*log10(cen$dbh[w]/10) + Abar*So1 + n01*Pvbar + n02*Sdbar + n03*Tabar)
} 

  
  # data cleaning
  dend$dendrometer_reading_mm[which(dend$dendrometer_reading_mm > 1000)] <- NaN
  
  # format dates
  dend$dbh_first_date    <- as.Date(paste(dend$cenyear, dend$cenmonth, dend$cenday, sep="."), format="%Y.%m.%d") 
  dend$date              <- as.Date(paste(dend$year, dend$month, dend$day, sep="."), format="%Y.%m.%d") 
  
  # add first dbh measurement (cm) to dendrometer measurement (cm) = thisdbh
  dend$dendrometer_reading_mm <- as.numeric(dend$dendrometer_reading_mm) # Ignore error message. NA introduced by coercion is ok.
  dend$thisdbh_cm             <- (dend$dbh*pi) + ((dend$dendrometer_reading_mm/10)/pi)
  # Error estimates TO DO. Error estimated as diax1er <- (diax1*pi + er)/pi in matlab code. Where diax1 <- (diameterlA[tree_ind]*pi + dendroallA[[tree_ind]]/10)/pi  

  # estimate biomass of each tree for each new thisdbh_mm
  #loop through each tree to estimate biomass (bm) and convert to above ground carbon (agC)
for (ii in 1:length(dend$tree_tag)) {  
  thistree <- which(dend$tree_tag == dend$tree_tag[ii] & dend$year == dend$year[ii] & dend$month == dend$month[ii] & dend$day == dend$day[ii])     
  dbh_tree <- dend$thisdbh_cm[thistree]
  den_tree <- dend$density[thistree]
  h_tree   <- dend$height_m[thistree]
  
  # this uses allometric equations from allometricEquations.R
  if (allometrix == 2) {
    bm <- Chave2005_dry(diax=dbh_tree, density=den_tree, height=h_tree)
  } else if (allometrix == 3) {
    bm <- Chave2005_moist(diax=dbh_tree, density=den_tree, height=h_tree)
  } else if (allometrix == 4) {
    bm <- Chave2005_wet(diax=dbh_tree, density=den_tree, height=h_tree)
  } else if (allometrix == 5) {
    bm <- Chave2014(diax=dbh_tree, density=den_tree, height=h_tree)
  }
  
  ## TO DO ## error treatment remains to be done!
  
  # Unit conversions 
  
  dend$agC[ii] <- (bm)*(1/(2.1097*1000)) # convert kg to Mg=1/1000=10 and convert to carbon = 47.8% (ADD REF!! Txx et al?)
  dend$bm_kg[ii] <- (bm)
}

  # NPPacw per tree: substact bm(t) - bm(t+1) / (t)-(t+1)
  # 1. Tree above ground Carbon stock difference

  dend$agCdiff    <- ave(dend$agC, dend$plot_code, FUN = function(x) c(NA, diff(x)))

  # 2. Date difference

first_run = T
for (ii in 1:length(dend$tree_tag)) {  
  thistree  <- which(dend$tree_tag == dend$tree_tag[ii])
  agC       <- dend$agC[thistree]
  tag       <- dend$tree_tag[thistree]
  agCdiff   <- dend$agCdiff[thistree]
  year      <- dend$year[thistree]
  month     <- dend$month[thistree]
  plot_code <- dend$plot_code[thistree]
  datediff  <- rbind(0/0, data.frame(diff(as.matrix(dend$date[thistree])))) #datediff <- data.frame(0/0, difftime(tail(dend$date[thistree], -1), head(dend$date[thistree], -1), units="days"))
  w         <- cbind (plot_code, tag, year, month, agC, agCdiff, datediff)
    if (first_run) {
      npp_tree <- w
      first_run = F
    } else {
      npp_tree <- rbind (npp_tree, w)
    }
}

colnames(npp_tree) <- c("plot_code", "tag", "year", "month", "agC", "agCdiff", "datediff")
  
  # 3. NPP: MgC per tree per day

  npp_tree$nppacw_tree_day  <- npp_tree$agCdiff/npp_tree$datediff
  
  # Dendrometer NPP: MgC per plot per year 
  www                        <- sqldf("SELECT plot_code, year, month, AVG(nppacw_tree_day) FROM npp_tree GROUP BY year, month")
  colnames (www)             <- c("plot_code", "year", "month","npp_avgtrees_day_dend")
  www$npp_avgtrees_day_dend  <- as.numeric(www$npp_avgtrees_day_dend)
  www$npp_avgtrees_yr_dend   <- www$npp_avgtrees_day_dend*365
  www$npp_avgplot_month_dend <- (www$npp_avgtrees_yr_dend/12)*878

  # scale dendrometer band data to the whole plot by applying a scaling factor 
  # scaling factor = annual NPPacw from dendrometers (~200 trees) / annual NPPacw from census (all trees)
  nppacw_dend    <- mean(www$npp_avgplot_yr_dend, na.rm=T)
  #nppacw_census  <- NPPacw_census(census, plotname="ESP-01", allometric_option="Default", height_correction_option="Default", census1_year=2010, census2_year=2011) 
  scaling_factor <-  nppacw_dend / 1.67#nppacw_census
  www$npp_month_dend <- ((www$npp_avgtrees_day_dend*29.6)*length(unique(dend$tree_tag)))/scaling_factor
  
  monthlynppacw             <- data.frame(cbind(nppacw1$plot_code, nppacw1$year, nppacw1$month, nppacw1$npp_avgplot_month))
  colnames(monthlynppacw)   <- c("plot_code", "year", "month", "npp_MgC_month") 
  
  return(monthlynppacw)
}

# SUM OF TREES EACH MONTH
xxx <- sqldf("SELECT plot_code, year, month, SUM(nppacw_tree_day) FROM npp_tree GROUP BY year, month")
colnames(xxx) <- c("plot_code", "year", "month", "npp_tree_day")
xxx$npp_plot_month <- as.numeric(xxx$npp_tree_day)*29.6 

