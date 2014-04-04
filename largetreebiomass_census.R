# C. Girardin 30/03/2014
# code to estimate plot biomass with census data

# requires two .csv files: 
#start_census   <- read.table() 
#census_running <- read.table()

## column names:
#start_census
#plot
#subplot
#tag
#palm
#altura_tot_est
#DAPaltura_m
#Densidade_de_madeira_g_cm3
#Dendrometer_altura_m
#DAP_cm_start

# census_running 
#plot
#subplot
#tag  
#palm
#DAP
#year
#month
#date

# define function
treebm_census <- function(start_census, census_running, plotname, year, # add plot size (just multiply by plot size at the end?)
                          allometric_option="Default") { 

## Set of allometric equations after Chave et al 2005 are defined here:
# this set of equations could also be made externally later, e.g. if needed also for large trees, etc...
if (allometric_option == 2 | allometric_option == "dry") {
  allometrix <- 2
  print("dry equation  is used for estimating AGB, model I.3 (see Chave et al., 2005)")
  } else if (allometric_option == 3 | allometric_option == "moist" | allometric_option == "Default" | allometric_option == 1 | allometric_option == "moist_ped") {
  allometrix <- 3
  print("moist equation  is used for estimating AGB, model I.6 (see Chave et al., 2005)")
  } else if (allometric_option == 4 | allometric_option == "wet") {
  allometrix <- 4
  print("wet equation  is used for estimating AGB, model I.3 (see Chave et al., 2005)")
  } else {
  print("Please specify a valid allometric_option!")
  return()
}

## get data for all trees that are in the plot and year selected 
par       = start_census$plot
Tnumcen   = as.numeric(start_census$tag)[which(par==plotname)] 
densitys  = as.numeric(start_census$Densidade_de_madeira_g_cm3)[which(par==plotname)] 
diameters = as.numeric(start_census$DAP_cm_start)[which(par==plotname)] #in cm
heights   = as.numeric(start_census$altura_tot_est)[which(par==plotname)]
  
par_running       = census_running$plot
yr_running        = census_running$year
Tnumcen_running   = as.numeric(census_running$tag)[which(par_running==plotname & yr_running==year)] 
diameters_running = as.numeric(census_running$DAP)[which(par_running==plotname & yr_running==year)] # in mm
year_running      = census_running$year[which(par_running==plotname & yr_running==year)]
month_running     = census_running$month[which(par_running==plotname & yr_running==year)]
day_running       = census_running$day[which(par_running==plotname & yr_running==year)]

## This is the correction procedure for heights, diameters and densitys:
# copied from Chris' original code:
# if no tree height, use the Feldpautch equations to get tree height

Bo    = 0.6373
B1    = 0.4647  # E.C. Amazonia

So1   = 0.012   # E.C. Amazonia
Abar  = 20.4    # mean cenetered basal area m-2 ha-1

n01   = 0.0034  # E.C. Amazonia
Pvbar = 0.68    # mean centered precipitation coefficient of variation

n02   = -0.0449 # E.C. Amazonia
Sdbar = 5       # mean centered dry season length no months less than 100mm

n03   = 0.0191  # E.C. Amazonia
Tabar = 25.0    # mean centered annual averge temperature

# calculate average over plot:
xdensityl  = mean(densitys, na.rm=T)
xdiameterl = mean(diameters, na.rm=T)

## fill NA's with mean plot averages:
densitys[which(is.na(densitys))] <- xdensityl
diameters[which(is.na(diameters))] <- xdiameterl
heights[which(is.na(heights))] <- 10^(Bo + B1*log10(diameters[which(is.na(heights))]/10) + Abar*So1 + n01*Pvbar + n02*Sdbar + n03*Tabar)

## fill implausible values:
heights[which(heights>45)]=45 
heights[which(heights<5)]=5 

densitys[which(densitys==0)]=xdensityl
diameters[which(diameters==0)]=xdiameterl

# calculate dendroallA, in the same format as it is in the original code:
dendroallA <- list()  # we use lists so that we don't need to specify how long they are.
dates <- list()

er=0.1 # .1cm
#(AGB, D in cm, q g/cm3, H in m)

NPPbiosA <- list()
NPPbiosAer <- list()

# loop runs through all trees with plotname==plotname
for (tree_ind in 1:length(Tnumcen)) {
  ## calculates temporary index which indexes all measurements of a particular tree:
  temp_ind <- which(Tnumcen_running == Tnumcen[tree_ind])
  
  dendroallA[[tree_ind]] <- diameters_running[temp_ind]
  dates[[tree_ind]] <- strptime(paste(year_running[temp_ind], month_running[temp_ind], day_running[temp_ind], sep="-"), format="%Y-%m-%d")
  
  diaxs = dendroallA[[tree_ind]] #  cm
  diaxser = er+dendroallA[[tree_ind]] 
  
  ##new calculation using allometric equations in external file:
  if (allometrix == 2) {
    nor <- Chave2005_dry(diax=diaxs, density=densitys[tree_ind], height=heights[tree_ind])
  } else if (allometrix == 3) {
    nor <- Chave2005_moist(diax=diaxs, density=densitys[tree_ind], height=heights[tree_ind])
  } else if (allometrix == 4) {
    nor <- Chave2005_wet(diax=diaxs, density=densitys[tree_ind], height=heights[tree_ind])
  }


  ## error treatment remains to be done! See largeTreeNPP_census function for details
  norer = 0.0509*(diaxser)^2*densitys[tree_ind]*heights[tree_ind] # replace with heightsa -> DONE
  
  # unit conversion must be done here; is not included in the allometric equation file
  NPPbiosA[[tree_ind]] = (nor)*(1/(2.1097*1000))    #convert kgto Mg=1/1000=10 and convert to carbon = 50% This is still biomass at this stage, not NPP.
  NPPbiosAer[[tree_ind]] = (norer*(1/(2.1097*1000)))#convert kgto Mg=1/1000=10 and convert to carbon = 50%  
}

# extract tree biomass for each year from list
trbm <- NULL
for (tree_ind in 1:length(Tnumcen)) {
  trbm <- c(trbm, NPPbiosA[[tree_ind]][1])
}

# sum of tree biomass to get plot level biomass
bm <- sum(trbm, na.rm=T) # *plotsize

#Return biomass values  
return(bm)

}




