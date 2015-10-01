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
small_census <- read.table("census_smalltrees_ACJ.csv",  sep=",", header=T)
plotname = "ACJ"
allometric_option = "Default"
census1_year="Default"
census2_year="Default"

#These are the column headings in the GEM database:
#plot_code; sub_plot; tree_tag; family; genus; species; year; month; day; wood_density_g_m2; tree_height_m; pom_height_m; dbh_nodirection_mm; dbh_northsouth_mm; dbh_westeast_mm; dendrometer_reading_replaced_mm; status_code_intact_moved_broken; mortality_code_alive_dead; quality_code; comments      

## get allometric equation file:
source("/Users/cecile/Documents/GitHub/GEMcarbon.R/allometric_equations_2014.R")

# CG: WHY DO WE NEED start_census & running_measurements? They are both in the same table in the db.
smallTreeNPP <- function(small_census, plotname, census1_year="Default", census2_year="Default", allometric_option="Default") {
  # TO DELETE: smallTreeNPP <- function(start_census, running_measurements, plotname, allometric_option="Default") {
  
  ## Set of allometric equations after Chave et al. 2005 and Chave et al. 2014 are defined in allometricEquations.R. Options defined here:
  
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
  }
 
  ## get data for all trees that are in the plot selected
  cen <- subset(small_census, plot_code==plotname)
    
  # define variables
  cen$DAP_cm <- (small_census$dbh_northsouth_cm + small_census$dbh_westeast_cm)/2	 
  
  cen$DAP_cm[which(cen$DAP_cm < -1000)] <- NA #Remove implausible values
  
  mean_density <- mean(cen$wood_density_g_cm3, na.rm=T)
 
  
  # dates option
  if (census1_year == "Default" ) {
    census1_year <- min(cen$year)
    print("Using first and last years of the dataset as census interval. Please precify census1_year and census2_year.")
  }
  
  if (census2_year == "Default" ) {
    census2_year <- max(cen$year)
    print("Using first and last years of the dataset as census interval.")
  }
  
  # Replace missing values in densitys with mean of density
  for (i in 1:length(cen$wood_density_g_cm3)) {  
    if (is.na(cen$wood_density_g_cm3[i])) cen$wood_density_g_cm3[i] <- mean_density  
  } 
  

  #stem surface area m2
  
  cen$radius_base = cen$DAP_cm/200 # estimated radii at base of stem convert cm to m ### meters not m2
  cen$radius_top = cen$DAP_cm/400 # estimated radii at top of stem
  mean_tree_height_m = mean(cen$tree_height_m, na.rm=T) #average small tree height
  cen$tree_height_m[!is.finite(cen$tree_height_m)] <- mean_tree_height_m  
  
  #stem surface area per plot per unit ground area
  
  ## see Chambers et al., 2004. Respiration from a tropical forest ecosystem: partitioning of sources and low CUE.
  # SAIpsA = 10^(-0.015-0.686*log10(diameters[ind])+(2.208*log10(diameters[ind])^2)-(0.627*log10(diameters[ind])^3))
  SAIpsA <- Chambers2004_surfaceArea(cen$DAP_cm) # new code using allometric equation in external file
  
  ## this loop for each tree has two advantages compared to the previous code:
  # 1) it allows for different measurement dates for particular trees
  # 2) allows for different lengths of measurement time series
  
  # get the date for each measurement
  cen$dates <- strptime(paste(cen$year, cen$month, cen$day, sep="-"), format="%Y-%m-%d")
  
  er = 0.1 # in cm # (AGB, D in cm, q g/cm3, H in m)
  
  
  # replace these variable names
  par         cen$plot_code
  Tnumcen     cen$tree_tag
  TnumcenA    Tnumcen[ind]   # trees that are in the plotname
  densitys    cen$wood_density_g_cm3
  diameters   cen$DAP_cm # at the start date. min(date)?
  heights     cen$tree_height_m
  dendros     cen$DAP_cm  
  Rbs         cen$radius_base
  Rts         cen$radius_top
  
  
  for (ii in 1:length(cen$tree_tag)) {
    diam_tree <- as.numeric(cen$DAP_cm[ii])/pi # convert circum to diameter ### this was er + as.numeric(cen$DAP_cm[ii]) WHY? should be /pi, not + 0.1. !!!! CHECK THIS !!!!
    den_tree <- cen$wood_density_g_cm3[ii]
    h_tree   <- cen$tree_height_m[ii]
    diam_tree_sd <- er + (as.numeric(cen$DAP_cm[ii])/pi) # TO DO: how to estimate error??
      
    ## using allometric equations from the external file allometric_equations_2014.R:
    if (allometrix == 2) {
      nor <- Chave2005_dry(diax = diam_tree, density = den_tree, height = h_tree)
    } else if (allometrix == 3) {
      nor <- Chave2005_moist(diax = diam_tree, density = den_tree, height = h_tree)
    } else if (allometrix == 4) {
      nor <- Chave2005_wet(diax = diam_tree, density = den_tree, height = h_tree)
    } else if (allometrix == 5) {
      nor <- Chave2014(diax = diam_tree, density = den_tree, height = h_tree)
    }
    
    ## TO DO: HOW TO ESTIMATE ERRORS??
    norer = 0.0509*(diam_tree_sd)^2*den_tree*h_tree 
    k = 0.0509*den_tree*h_tree
    norer = abs(k)*2*abs(diam_tree)*er 
    
    #Toby:
    #If error on diaxs is er then error on (diaxs^2) is 2diaxs*er (rule of quadrature). Equation on
    #line 90 is basically nor=k*(diaxs^2) where k is constant (assuming no error
    #on k=0.0509*densitysA[tree_ind]*heights ) so error on nor should be norer=abs(k)*2*abs(diaxs)*er 
    
    # unit conversion must be done here. It is not included in the allometric equation file
    cen$NPPbio[ii]    <- (nor)*(1/(2.1097*1000))    # convert kgto Mg=1/1000=10 and convert to carbon = 50%
    cen$NPPbio_er[ii] <- (norer*(1/(2.1097*1000)))  # convert kgto Mg=1/1000=10 and convert to carbon = 50%                       
  }
####################################################################################  
############################ NEW CODE ##############################################
####################################################################################

## find global start and end date:
cen$min_date <- NULL
cen$max_date <- NULL

agC_mydates  <- subset(cen, cen$year == census1_year | cen$year == census2_year, select = c(plot_code, tree_tag, year, month, day, dates, NPPbio, NPPbio_er))

for (i in 1:length(agC_mydates$tree_tag)) {
  min_date <- as.character(min(agC_mydates$dates)) 
  max_date <- as.character(max(agC_mydates$dates)) 
}

start_date <- as.Date(format(min(strptime(min_date, format="%Y-%m-%d")))) 
end_date   <- as.Date(format(max(strptime(max_date, format="%Y-%m-%d"))))
census_interval <- as.numeric(difftime(end_date, start_date, units="days"))

# (AG carbon.2 - AG carbon.1) / census_interval
agC_1         <- subset(cen, cen$year == census1_year, select = c(plot_code, tree_tag, year, month, day, NPPbio, NPPbio_er))
agC_2         <- subset(cen, cen$year == census2_year, select = c(plot_code, tree_tag, year, month, day, NPPbio, NPPbio_er))
npp           <- sqldf("SELECT agC_1.plot_code, agC_1.tree_tag, agC_1.year, agC_1.month, agC_1.NPPbio , agC_2.NPPbio, agC_1.NPPbio_er , agC_2.NPPbio_er FROM agC_1 JOIN agC_2 ON agC_1.tree_tag = agC_2.tree_tag")
colnames(npp) <- c("plot_code", "tree_tag", "year", "month", "agC.1", "agC.2", "agC.1_er", "agC.2_er")
npp_day       <- (npp$agC.2-npp$agC.1) / census_interval
NPPacw_MgC_ha_yr <- (sum(npp_day, na.rm=T))*365 
NPPacw_MgC_ha_yr_se <- sd(npp_day, na.rm=T)/length(cen$tree_tag)

# Talbot census correction function
NPPcorr = NPPacw_MgC_ha_yr + (0.0091 * NPPacw_MgC_ha_yr) * (census_interval/365)
annual <- data.frame(NPPacw_MgC_ha_yr, NPPacw_MgC_ha_yr_se)

return(annual)

}


