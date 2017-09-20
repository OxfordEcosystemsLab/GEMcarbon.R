# Written by: Cecile Girardin September 2014

## Calculation of large tree NPP from dendrometer data

# dendrometers measured every 3 months
# assumes all trees over 10 cm measured at 1.3 meters

# requires two .csv files: 
# census   <- read.csv() 
# dendrometer <- read.csv() 


NPPacw_dendro <- function(census, dendrometer, plotname, allometric_option="Default", height_correction_option="Default", ret="nppacw.perday.pertree", census_year) { #census_year 1 & year 2

  # get allometric equation function
  setwd("/Users/cecile/GitHub/GEMcarbon.R") 
  dir()
  Sys.setlocale('LC_ALL','C') 
  source("allometric_equations_2014.R")
  
  # load libraries
  library(sqldf)

## get data for all trees that are in the plot selected from census & dendrometer files
cen1  <- subset(census, plot_code==plotname)
cen   <- subset(cen1, year==census_year)  
dend1 <- subset(dendrometer, plot_code==plotname)

# re-name year, month, day in cen
cen$cenyear  <- cen$year
cen$cenmonth <- cen$month
cen$cenday   <- cen$day

## get the data you need from the census file into the dendrometer data frame: density, height, first dbh measurement, date of first dbh measurement
dend <- sqldf("SELECT dend1.*, cen.density, cen.height_m, cen.dbh, cen.cenyear, cen.cenmonth, cen.cenday FROM cen JOIN dend1 ON cen.tree_tag = dend1.tree_tag")
head(dend) # check you have data in here. If not, make sure dend1 and cen are in the right formats, e.g. using sapply(cen, class).


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
  cen$height_m[w] <- mean(h.pred, na.rm=T)
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
  dend$thisdbh_cm             <- (dend$dbh*pi) + ((dend$dendrometer_reading_mm/10)) # deleted /pi, as dendrometer reading is a measure of circumferance already  

  # estimate biomass of each tree for each new thisdbh_mm
  # loop through each tree to estimate biomass (bm) and convert to above ground carbon (agC)
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
  
  
  # Unit conversions 
  
  dend$agC[ii] <- (bm)*(1/(2.1097*1000)) # convert kg to Mg=1/1000=10 and convert to carbon = 47.8% (ADD REF!! Txx et al?)
  # dend$bm_kg[ii] <- (bm)
}

  # NPPacw per tree: substact bm(t) - bm(t+1) / (t)-(t+1)
  # 1. Tree above ground Carbon stock difference

  #dend$agCdiff    <- ave(dend$agC, dend$plot_code, FUN = function(x) c(NA, diff(x)))

  # 2. Date difference

#pb = txtProgressBar(max = length(unique(dend$tree_tag)), style = 3); i = 0

uid             <- unique(dend$tree_tag)
aa              <- c()
bb              <- c()
cc              <- c()
dd              <- c()
ee              <- c()
ff              <- c()
gg              <- c()

for (ii in 1:length(uid)){  
  
  thistree  <- which(dend$tree_tag == uid[ii]) #dend$tree_tag[ii])
  agC       <- dend$agC[thistree]
  tag       <- dend$tree_tag[thistree]
  #agCdiff   <- dend$agCdiff[thistree]
  agCdiff   <- ave(dend$agC[thistree], FUN = function(x) c(NA, diff(x)))
  year      <- dend$year[thistree]
  month     <- dend$month[thistree]
  plot_code <- dend$plot_code[thistree]
  # datediff  <- rbind(NA, data.frame(diff(as.matrix(dend$date[thistree])))) #datediff <- data.frame(0/0, difftime(tail(dend$date[thistree], -1), head(dend$date[thistree], -1), units="days"))
  # ddiff     <- datediff$diff.as.matrix.dend.date.thistree...
  ddiff   <- rbind(NA, data.frame(diff(as.matrix(dend$date[thistree]))))[,1] # ADD stings as factors
  aa            <- c(aa, plot_code)
  bb            <- c(bb, tag)
  cc            <- c(cc, year)
  dd            <- c(dd, month)
  ee            <- c(ee, agC)
  ff            <- c(ff, agCdiff)
  gg            <- c(gg, ddiff)
  
  if(ii%%100 == 0){print(ii)}
}

ff %>% summary
cc %>% summary

npp_tree        <- cbind(aa, bb, cc, dd, ee, ff, gg)
npp_tree        <- data.frame(npp_tree)
colnames(npp_tree) <- c("plot_code", "tag", "year", "month", "agC", "agCdiff", "datediff")

npp_tree$tag      = as.numeric(as.character(npp_tree$tag))
npp_tree$year     = as.numeric(as.character(npp_tree$year))
npp_tree$month    = as.numeric(as.character(npp_tree$month))
npp_tree$agC      = as.numeric(as.character(npp_tree$agC))
npp_tree$agCdiff  = as.numeric(as.character(npp_tree$agCdiff))
npp_tree$datediff = as.numeric(as.character(npp_tree$datediff))

  # 3. NPP: MgC per tree per day

  npp_tree$nppacw_tree_day  <- npp_tree$agCdiff/npp_tree$datediff
  
  npp_tree$nppacw_tree_day %>% hist(100) %>% abline(v=0, col="red", lwd=2)

  # Dendrometer NPP: MgC per plot per avgTREE per year 

  www                         <- sqldf("SELECT plot_code, year, month, AVG(nppacw_tree_day), STDEV(nppacw_tree_day) FROM npp_tree GROUP BY year, month")
  colnames (www)              <- c("plot_code", "year", "month", "npp_avgtrees_day_dend", "npp_avgtrees_day_dend_sd")
  www$npp_avgtrees_day_dend   <- as.numeric(www$npp_avgtrees_day_dend)
  www$npp_avgtrees_month_dend <- www$npp_avgtrees_day_dend*29.6 # per month
  www$npp_avgtrees_yr_dend    <- www$npp_avgtrees_month_dend*12

  www$npp_avgtrees_month_dend_sd <- www$npp_avgtrees_day_dend_sd*29.6 # per month
  www$npp_avgtrees_yr_dend_sd    <- www$npp_avgtrees_month_dend_sd*12
 #www$npp_avgtrees_day_dend_se   <- as.numeric(www$npp_avgtrees_day_dend_sd/sqrt(length(unique(npp_tree$tag))))

  # scale dendrometer band data to the whole plot by applying a scaling factor 
  # get nppacw_census value for this plot
  nppacw_cen  <- NPPacw_census(census, plotname="BOB-02", allometric_option="Default", height_correction_option="Default", census1_year=2013, census2_year=2015)
  # nppacw_cen  <- NPPacw_census(census, plotname, census1_year=2013, census2_year=2015)  are defined above.

  pertree = mean(nppacw_cen$NPPpertree_MgC_ha_yr, na.rm=T)
  npp_cen_ha_yr = pertree * length(unique(census$tree_tag)) # Refine this with Sammy

  # get mean nppacw_dend per tree value for this plot
  xxx <- sqldf("SELECT plot_code, AVG(npp_avgtrees_yr_dend) from www")
  colnames(xxx) <- c("plot_code", "nppacw_dend")
  # scaling factor (sf) = annual NPPacw from dendrometers (~200 trees) / annual NPPacw from census (all trees)
  sf  <- (xxx$nppacw_dend*length(unique(dend$tree_tag))) / npp_cen_ha_yr
  www$nppacw_month <- (www$npp_avgtrees_month_dend*length(unique(dend$tree_tag)))/sf
  www$nppacw_month_sd <- (www$npp_avgtrees_month_dend_sd*length(unique(dend$tree_tag)))/sf 
  www$nppacw_month_se <- (((www$npp_avgtrees_month_dend_sd/sqrt(length(www$npp_avgtrees_month_dend))))*length(unique(dend$tree_tag)))/sf 

  # test that result is correct: the annual value should be the same as the annual value obtained from NPPacw_census_function_2014
  yy <- data.frame((mean(www$nppacw_month, na.rm=T))*12,(mean(www$nppacw_month_se, na.rm=T))) 
  colnames(yy) <- c("nppacw_ha_yr", "nppacw_ha_yr_se")
  yy
  npp_cen_ha_yr

  monthlynppacw             <- data.frame(cbind(as.character(www$plot_code), www$year, www$month, www$nppacw_month, www$nppacw_month_sd, www$nppacw_month_se))
  colnames(monthlynppacw)   <- c("plot_code", "year", "month", "nppacw_MgC_month", "nppacw_MgC_month_sd", "nppacw_MgC_month_se") 
  monthlynppacw$nppacw_MgC_month <- as.numeric(as.character(monthlynppacw$nppacw_MgC_month))
  monthlynppacw$year             <- as.numeric(as.character(monthlynppacw$year))
  monthlynppacw$month            <- as.numeric(as.character(monthlynppacw$month))
  monthlynppacw$nppacw_MgC_month_se <- as.numeric(as.character(monthlynppacw$nppacw_MgC_month_se))

plot(monthlynppacw$month, monthlynppacw$nppacw_MgC_month)

switch(ret,
       nppacw.permonth.perha = {return(monthlynppacw)},
       nppacw.perday.pertree = {return(npp_tree)}
)

}


