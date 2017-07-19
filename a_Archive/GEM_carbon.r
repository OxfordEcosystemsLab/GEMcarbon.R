
##################### CALL GEM FUNCTIONS ###################################

setwd("C:/Users/Cecile/Documents/GitHub/GEMcarbon.R") 
dir()
source("allometric_equations_2014.R")
source("NPPacw_census_function_2014.R")
source("largetreebiomass_census.R")
source("NPPacw_dendro_function_2014.R")

##################### CENSUS ##############################################

setwd("C:/Users/Cecile/Documents/GitHub/GEMcarbon.R/example files")
census <- read.table("CensusSPD02_clean.csv", header=TRUE, sep=",", na.strings=c("NA", "NaN", ""), dec=".", strip.white=TRUE)
SPD02_census <- NPPacw_census(census, plotname="SPD-02", allometric_option="Default", height_correction_option="Default", census1_year=2006, census2_year=2008)

##################### DENDROMETER BANDS ###################################

setwd("C:/Users/Cecile/Documents/GitHub/GEMcarbon.R/example files")
dendrometer <- read.table("Dend_SPD02_2013.csv", header=TRUE, sep=",", na.strings=c("NA", "NaN", ""), dec=".", strip.white=TRUE, quote = "", fill = TRUE)  
#dendrometer <- read.table("DendEsp_2012.csv", header=TRUE, sep=",", na.strings=c("NA", "NaN", ""), dec=".", strip.white=TRUE)

SPD02_dend <- NPPacw_dendro(census, dendrometer, plotname="SPD-02", allometric_option="Default", height_correction_option="Default", census_year=2006)
ESP01_dend <- NPPacw_dendro(census, dendrometer, plotname="ESP-01", allometric_option="Default", height_correction_option="Default", census_year=2006)


##################### SOIL RESPIRATION PARTITIONNING ######################

##################### ROOT INGROWTH CORES #################################

##################### FINE LITTERFALL #####################################

##################### STEM RESPIRATION ####################################

##################### LEAF RESPIRATION ####################################

##################### COARSE WOODY DEBRIS #################################

##################### LAI #################################################

##################### RHYZOTRONS ##########################################

