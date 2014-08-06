# Written: Sebastian Sippel & Cécile Girardin May 2014
# Update: Cécile Girardin July 2014

### Here comes a function that estimates annual NPP values based on census data

## This script estimates largeTreeNPP
# The monthly values you get from this code are plot-level values, as all the trees in the plot are censused. 
# Once you sum all monthly values from this, you get the annual value for the plot. 
# That is the most reliable annual value. 
# We then use this to estimate a scaling factor for the monthly dendrometer data (NPPcensus ha-1 yr-1 / NPPdend ha-1 yr-1).

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

# TO DO: add an option to plot raw data = T/F
largeTreeNPP_census <- function(start_census, census_running, plotname, # census_1_year="Default", census_2_year="Default"
                         allometric_option="Default", height_correction_option="Default") {
  
  ## Set of allometric equations after Chave et al. 2005 and Chave et al. 2014 are defined in allometricEquations.R. Options defined here:
  if (allometric_option == 2 | allometric_option == "dry") {
    allometrix <- 2
    print("dry equation  is used for estimating AGB, model I.3 (see Chave et al., 2005)")
  } else if (allometric_option == 3 | allometric_option == "moist" | allometric_option == "Default" | allometric_option == 1 | allometric_option == "moist_ped") {
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
  
  ## get data for all trees that are in the plot selected 
  par = start_census$plot
  Tnumcen = as.numeric(start_census$tag)[which(par==plotname)]
  densitys = as.numeric(start_census$Densidade_de_madeira_g_cm3)[which(par==plotname)]
  diameters = as.numeric(start_census$DAP_cm_start)[which(par==plotname)] # in cm
  heights = as.numeric(start_census$altura_tot_est)[which(par==plotname)]
  
  par_running = census_running$plot
  Tnumcen_running = as.numeric(census_running$tag)[which(par_running==plotname)]
  diameters_running = as.numeric(census_running$DAP)[which(par_running==plotname)] # in cm.
  year_running = census_running$year[which(par_running==plotname)]
  month_running = census_running$month[which(par_running==plotname)]
  day_running = census_running$day[which(par_running==plotname)]
  
  # Density: look up density at spp / genus level from global density db Zanne
  
  ## fill implausible values:
  heights[which(heights>120)] <- 120 
  heights[which(heights<2)] <- 2 
  xdensityl <- mean(densitys, na.rm=T) 
  densitys[which(densitys==0)] <- xdensityl
  densitys[which(is.na(densitys))] <- xdensityl
  
  
  ## Correct for missing tree heights
  
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
  
  # Option 1: you have height for more than 50 trees in your plot. Estimate local diameter-height relationship
  
  # Ken Feeley's code uses log & exp. just check we don't need to use log and exp for local height estimate.
  #h.est=function(dbh, h){
  #  d=log(as.numeric(as.character(dbh))) # why does he have log here?
  #  h=log(as.numeric(as.character(h)))
  #  l=lm(h~d)
  #  pred.h=l[[1]][1]+l[[1]][2]*d
  #  pred.h=exp(pred.h)                   # and exp here?
  #}
  
  h.est=function(dbh, h){
    l=lm(h~dbh)
    coeffs = coefficients(l)
    pred.h = coeffs[1] + coeffs[2]*dbh
  }
  
  # Option 2: you have height for less than 50 trees in your plot. Use Fedpauch equation.

  #ADD PARAMETER: Feldpauch region. 
  
  ## Feldpauch correction procedure for heights, diameters and densitys:
    # Brazilian shield
    Bo = 0.6373
    B1 = 0.4647  # E.C. Amazonia
    
    So1 = 0.012  # E.C. Amazonia
    Abar = 20.4  # mean cenetered basal area m-2 ha-1
    
    n01 = 0.0034 # E.C. Amazonia
    Pvbar = 0.68 # mean centered precipitation coefficient of variation
    
    n02 = -0.0449 # E.C. Amazonia
    Sdbar = 5     # mean centered dry season length no months less than 100mm
    
    n03 = 0.0191  # E.C. Amazonia
    Tabar = 25.0  # mean centered annual averge temperature
    
    
  # Define height options
  if (predheight == 1) {
    #pred.h <- h.est(diameters, heights)
    heights[which(is.na(heights))] <- h.est(diameters, heights)
  } else if (predheight == 2) {
    heights[which(is.na(heights))] <- 10^(Bo + B1*log10(diameters[which(is.na(heights))]/10) + Abar*So1 + n01*Pvbar + n02*Sdbar + n03*Tabar)
  } 
  

  # Tnumcen_running   # trees that are in the data

  # calculate censusallA, in the same format as it is in the original code:
  censusallA <- list()  # we use lists so that we don't need to specify how long they are.
  dates <- list()

  er=0.1 # .1cm sampling error is trivial compared to systematic error of allometric equation.
  # CHANGE THIS see Chave et al. 2005
  # AGB, D in cm, q g/cm3, H in m
  
  NPPbiosA <- list()
  NPPbiosAer <- list()
  
  ## this loop for each tree has two advantages compared to the previous code:
  # 1) it allows for different measurement dates for particular trees
  # 2) allows for different lengths of measurement time series
  
  # loop runs through all trees with plotname==plotname
  for (tree_ind in 1:length(Tnumcen)) {
    ## calculates temporary index which indexes all measurements of a particular tree:
    temp_ind <- which(Tnumcen_running == Tnumcen[tree_ind])
    
    censusallA[[tree_ind]] <- diameters_running[temp_ind]
    dates[[tree_ind]] <- strptime(paste(year_running[temp_ind], month_running[temp_ind], day_running[temp_ind], sep="-"), format="%Y-%m-%d")

    diaxs = censusallA[[tree_ind]] #  cm
    diaxser = er+censusallA[[tree_ind]] 
    
    ##new calculation using allometric equations in external file:
    if (allometrix == 2) {
      nor <- Chave2005_dry(diax=diaxs, density=densitys[tree_ind], height=heights[tree_ind])
    } else if (allometrix == 3) {
      nor <- Chave2005_moist(diax=diaxs, density=densitys[tree_ind], height=heights[tree_ind])
    } else if (allometrix == 4) {
      nor <- Chave2005_wet(diax=diaxs, density=densitys[tree_ind], height=heights[tree_ind])
    } else if (allometrix == 5) {
      nor <- Chave2014(diax=diaxs, density=densitys[tree_ind], height=heights[tree_ind])
    }
    
    ## TO DO ## error treatment remains to be done!
    #norer = 0.0509*(diaxser)^2*densitys[tree_ind]*heights[tree_ind] 
    
    # unit conversion is done here as it is not included in the allometric equation file
    NPPbiosA[[tree_ind]] = (nor)*(1/(2.1097*1000))      # convert kg to Mg=1/1000=10 and convert to carbon = 47.8% # This is tree biomass, not NPP.
    #NPPbiosAer[[tree_ind]] = (norer*(1/(2.1097*1000))) # convert kg to Mg=1/1000=10 and convert to carbon = 47.8%                       
  }
  
  # ADD TERHI's HEIGHT PROPAGATION CORRECTION
  
  ## find global start and end month:
  min_date <- NULL
  max_date <- NULL
  
  for (i in 1:length(Tnumcen)) {
    min_date[i] <- as.character(min(dates[[i]])) # if year = census_1_year {as.character(min(dates[[i]]))}
    max_date[i] <- as.character(max(dates[[i]])) # if year = census_2_year {as.character(max(dates[[i]]))} change to the date you want max data for year = 200X
  }
  
      
  fir_year  <- as.numeric(format(min(strptime(min_date, format="%Y-%m-%d")), format="%Y"))
  last_year <- as.numeric(format(max(strptime(max_date, format="%Y-%m-%d")), format="%Y"))
  
  ## Build NPP matrix for all trees (cenNPPbiosA):
  # number of columns (i.e. years:)
  cenNPPbiosA <- array(data=NA, dim=c(length(Tnumcen),(last_year-fir_year+1)*12))
  #cenNPPbiosAer <- array(data=NA, dim=c(length(TnumcenA),(last_year-fir_year+1)*12))  # this is not the error. it is the same value as above.
  dates_monthly <- seq.Date(from=as.Date(paste(fir_year,"-01-01", sep=""), format="%Y-%m-%d"),
                            to=as.Date(paste(last_year,"-12-01", sep=""), format="%Y-%m-%d"), by="1 months")
  
  ## Build daily NPP matrix:
  dates_daily <- seq.Date(from=as.Date(paste(fir_year,"-01-01", sep=""), format="%Y-%m-%d"),
                          to=as.Date(paste(last_year,"-12-31", sep=""), format="%Y-%m-%d"), by=1)
  
  # Talbot census correction function
  
  # AGWPcorr = AGWPobs + 0:0091AGWPobs *  t
  
  ## loop runs through each tree:
  for (tree_ind in 1:length(Tnumcen)) {
    npp_daily <- rep(NA,length(dates_daily))
    npp_daily_er <- rep(NA, length(dates_daily))
    
    ## interpolation is based on the temporary index: (removes NAs to perform interepolation)
    temp_index <-  which(!is.na(NPPbiosA[[tree_ind]])) 
    
    # calculate daily NPP interpolations (i.e. remove NAs):
    temp_npp <-  NPPbiosA[[tree_ind]][temp_index]
    temp_npp_er <- NPPbiosA[[tree_ind]][temp_index]
    temp_dates <-  dates[[tree_ind]][temp_index]
    
    for (j in 1:(length(temp_npp)-1)) {
      ## calculate daily growth rate, if more than 2 elements:
      if (length(temp_npp) > 1) {
      npp_daily[which(as.Date(temp_dates[j])==dates_daily):(which(as.Date(temp_dates[j+1])==dates_daily)-1)] <- (temp_npp[j+1]-temp_npp[j])/as.numeric(temp_dates[j+1]-temp_dates[j])
      npp_daily_er[which(as.Date(temp_dates[j])==dates_daily):(which(as.Date(temp_dates[j+1])==dates_daily)-1)] <- (temp_npp_er[j+1]-temp_npp_er[j])/as.numeric(temp_dates[j+1]-temp_dates[j])
      }
    }
    
    ## loop over all months in the data:
  for (m in 1:length(dates_monthly)) {
      # index of all days in a particular months (to be used with npp_daily):
      npp_month_ind <- which(format(dates_daily, format="%Y-%m") == format(dates_monthly[m], format="%Y-%m"))
      cenNPPbiosA[tree_ind, m] <- mean(npp_daily[npp_month_ind], na.rm=T)*length(npp_month_ind)
      #for error calculate NPP for tree with 1mm bigger diameter and diff is the error
      #cenNPPbiosAer[tree_ind, m] <- mean(npp_daily_er[npp_month_ind], na.rm=T)*length(npp_month_ind)
    }
  }
  
  # convert to MgC ha month  # THIS IS THE LOOP THAT TAKES AAAGES!
  NPPwoodsA = colSums(cenNPPbiosA, na.rm=T)
  #NPPwoodsAstd = colSums(cenNPPbiosAer, na.rm=T)
  # set NAs (i.e. columns that are entirely NA!):
  for (i in 1:length(dates_monthly)) {
    if(all(is.na(cenNPPbiosA[,i]))) {
      NPPwoodsA[i] <- NA}
    #if(all(is.na(cenNPPbiosAer[,i]))) {
    #  NPPwoodsAstd[i] <- NA}
  }
  names(NPPwoodsA) <- dates_monthly
  # names(NPPwoodsAstd) <- dates_monthly
  # NPPwoodsAer : use monte carlo approach to assing error to each parameter. 
  # See Chave et al. 2004 on error propagation and scaling. ER on individual trees, and random propagation of sum of squares for the whole plot, so you end up with less error in a plot wt lots of trees.

return(NPPwoodsA)
}

# w=which(!is.na(data$biomass.2003) & !is.na(data$biomass.2007)) # id surviving trees to estimate biomass growth
# w2=which(!is.na(data$biomass.2003) & is.na(data$biomass.2007)) # id dying trees
# w3=which(is.na(data$biomass.2003) & !is.na(data$biomass.2007)) # id recruiting trees
  
