### This script is to scale dendro NPP with census NPP (test-wise!!)
## Sebastian Sippel, 21.03.2014



# test-functions for large tree dendrometer and census measurements are in thee Dropbox ("Carbon_Use_Efficiency/in-progress"):
setwd("D:/Dokumente/My Dropbox/Carbon_Use_Efficieny_R/in-progress/")
setwd("C:/Users/Cecile/Dropbox/Carbon_Use_Efficieny_R/in-progress/")
source("largeTreeNPP_v1_dendrometer_function.R")
source("largeTreeNPP_census_v1_function.R")

## get allometric equation file:
source("D:/Dokumente/My Dropbox/Carbon_Use_Efficieny_R/in-progress/allometricEquations.R")
source("C:/Users/Cecile/Dropbox/Carbon_Use_Efficieny_R/in-progress/allometricEquations.R")



## Estimate largeTreeNPP based on Census data:
## open Data:
setwd("C:/Users/Cecile/Dropbox/Carbon_Use_Efficieny_R/Original-matlab-code/Ken_Iq-Tang/data/largeTree_data/census_restructured")
setwd("D:/Dokumente/My Dropbox/Carbon_Use_Efficieny_R/Original-matlab-code/Ken_Iq-Tang/data/largeTree_data/census_restructured/")
start_census <- read.table("census_start.csv", header=T,sep=";")
census_running <- read.table("census_running.csv", header=T, sep=";")

## run largeTreeNPP_census_function:
NPP_census <- largeTreeNPP_census(start_cencus = start_census, running_measurements=census_running, plotname=1, allometric_option="moist")


### Estimate dendrometer NPP (takes > 5 min)
## open Data:
setwd("D:/Dokumente/My Dropbox/Carbon_Use_Efficieny_R/Original-matlab-code/Ken_Iq-Tang/data/largeTree_data/")
setwd("C:/Users/Cecile/Dropbox/Carbon_Use_Efficieny_R/Original-matlab-code/Ken_Iq-Tang/data/largeTree_data/")

census <- read.table("census.csv", header=T, sep=";")
dendrometer_db <- read.table("dendrometer_db.csv", header=T, sep=";", colClasses=c(rep("numeric",7),"character"))

NPP_dendrometer <- largeTreeNPP_v1_dendrometer(census=census, dendrometer_db=dendrometer_db, plotname=1, allometric_option="moist")

### TO DO ## add scaling and plotsize!!

plotsize <- 1 # plotsize in ha
# scale NPP_census by plotsize (add plotsize in ha):
NPP_census_ha = NPP_census / plotsize   # "plotsize" could be added as a variable to the function NPP_census()

# scale NPP_dendrometer by plotsize:
NPP_dendrometer_ha = NPP_dendrometer / plotsize


# scale NPP_dendrometer with NPP_census:

## annual sums NPP_dendrometer:
NPP_dendrometer_annual <- c()
NPP_dendrometer_annual_names <- c()
for (i in 1:(length(NPP_dendrometer)/12)) {
  NPP_dendrometer_annual[i] <- sum(NPP_dendrometer[((i-1)*12+1):((i-1)*12+12)])
  NPP_dendrometer_annual_names[i] <- names(NPP_dendrometer[(i-1)*12+1])
}
names(NPP_dendrometer_annual) <- NPP_dendrometer_annual_names

## annual sums NPP_census:
NPP_census_annual <- c()
NPP_census_annual_names <- c()
for (i in 1:(length(NPP_census)/12)) {
  NPP_census_annual[i] <- sum(NPP_census[((i-1)*12+1):((i-1)*12+12)])
  NPP_census_annual_names[i] <- names(NPP_census[(i-1)*12+1])
}
names(NPP_census_annual) <- NPP_census_annual_names

## which years correspond in NPP_census and NPP_dendrometer?
dendro_index <- NULL
census_index <- NULL
n=1
for (i in 1:length((NPP_dendrometer_annual))) {
  
  if (any(names((NPP_dendrometer_annual))[i] == names((NPP_census_annual)))) {
  dendro_index[n] <- i
  census_index[n] <- which(names((NPP_census_annual)) == names((NPP_dendrometer_annual))[i])
  n = n+1
  }
}

# determine annual scaling factor:
scaling_factor <- NPP_dendrometer_annual[dendro_index] / NPP_census_annual[census_index]

# average scaling factor:
scaling_factor_avg <- mean(scaling_factor, na.rm=T)

# scale NPP_dendrometer:
NPP_dendrometer_scaled <- NPP_dendrometer/scaling_factor_avg

### the scaling by plotsize should be included into the functions.
### we need to discuss how to deal with the scaling NPP_dendrometer/NPP_census. Do we keep this as a separate R script?

