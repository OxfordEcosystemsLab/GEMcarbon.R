### Here comes a function that estimates annual NPP values based on census data

## This script estimates largeTreeNPP


largeTreeNPP_census <- function(start_census, census_running, plotname, 
                         allometric_option="Default") {

##### different equations: moist / dry / wet (Chave et al., 2005)
  # These are the equations recommended in (Marthews et al. in review, 5 C Pools paper). 
  #     moist_ped = 1 (Default)
  #     2=dry:  0.4730*0.001*0.112*(densitysA[tree_ind]*((diaxs)^2)*heightsa)^0.916 
  #     3=moist:  0.4730*0.001*0.0509*densitysA[tree_ind]*((diaxs)^2)*heightsa
  #     4=wet:   0.4730*0.001*0.0776*(densitysA[tree_ind]*((diaxs)^2))*heightsa)^0.940
  
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
  
  ## get data for all trees that are in the plot selected 
  par = start_census$plot
  Tnumcen = as.numeric(start_census$tag)[which(par==plotname)]
  densitys = as.numeric(start_census$Densidade_de_madeira_g_cm3)[which(par==plotname)]
  diameters = as.numeric(start_census$DAP_cm_start)[which(par==plotname)] #in cm
  heights = as.numeric(start_census$altura_tot_est)[which(par==plotname)]
  
  par_running = census_running$plot
  Tnumcen_running = as.numeric(census_running$tag)[which(par_running==plotname)]
  diameters_running = as.numeric(census_running$DAP)[which(par_running==plotname)] # in mm
  year_running = census_running$year[which(par_running==plotname)]
  month_running = census_running$month[which(par_running==plotname)]
  day_running = census_running$day[which(par_running==plotname)]

  ## This is the correction procedure for heights, diameters and densitys:
  # copied from Chris' original code:
    # if no tree height, use the Feldpautch equations to get tree height
    # Kenia is in the Brazilian shield
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
    
    # calculate average over plot:
    xdensityl = mean(densitys, na.rm=T)
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
    

  # Tnumcen_running   # trees that are in the data

  # calculate dendroallA, in the same format as it is in the original code:
  dendroallA <- list()  # we use lists so that we don't need to specify how long they are.
  dates <- list()

  er=0.1 # .1cm
  #(AGB, D in cm, q g/cm3, H in m)
  
  NPPbiosA <- list()
  NPPbiosAer <- list()
  
  ## this loop for each tree has two advantages compared to the previous code:
  # 1) it allows for different measurement dates for particular trees
  # 2) allows for different lengths of measurement time series
  
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
    
    ## error treatment remains to be done!
    norer = 0.0509*(diaxser)^2*densitys[tree_ind]*heights[tree_ind] # replace with heightsa -> DONE
    
    #Toby:
    #If error on diaxs is er then error on (diaxs^2) is 2diaxs*er (rule of quadrature). Equation on
    #line 90 is basically nor=k*(diaxs^2) where k is constant (assuming no error
    #on k=0.0509*densitysA[tree_ind]*heights ) so error on nor should be
    #norer=abs(k)*2*abs(diaxs)*er 
    
    # unit conversion must be done here; is not included in the allometric equation file
    NPPbiosA[[tree_ind]] = (nor)*(1/(2.1097*1000))    #convert kgto Mg=1/1000=10 and convert to carbon = 50% This is still biomass at this stage, not NPP.
    NPPbiosAer[[tree_ind]] = (norer*(1/(2.1097*1000)))#convert kgto Mg=1/1000=10 and convert to carbon = 50%                       
  }
  
  
  ## find global start and end month:
  min_date <- NULL
  max_date <- NULL
  
  for (i in 1:length(Tnumcen)) {
    min_date[i] <- as.character(min(dates[[i]]))
    max_date[i] <- as.character(max(dates[[i]]))
  }
  
  fir_year <- as.numeric(format(min(strptime(min_date, format="%Y-%m-%d")), format="%Y"))
  last_year <- as.numeric(format(max(strptime(max_date, format="%Y-%m-%d")), format="%Y"))
  
  ## Build NPP matrix for all trees (denNPPbiosA):
  # number of columns (i.e. years:)
  denNPPbiosA <- array(data=NA, dim=c(length(Tnumcen),(last_year-fir_year+1)*12))
  #denNPPbiosAer <- array(data=NA, dim=c(length(TnumcenA),(last_year-fir_year+1)*12))  # this is not the error. it is the same value as above.
  dates_monthly <- seq.Date(from=as.Date(paste(fir_year,"-01-01", sep=""), format="%Y-%m-%d"),
                            to=as.Date(paste(last_year,"-12-01", sep=""), format="%Y-%m-%d"), by="1 months")
  
  ## Build daily NPP matrix:
  dates_daily <- seq.Date(from=as.Date(paste(fir_year,"-01-01", sep=""), format="%Y-%m-%d"),
                          to=as.Date(paste(last_year,"-12-31", sep=""), format="%Y-%m-%d"), by=1)
  
  
  ## loop runs through each tree:
  for (tree_ind in 1:length(Tnumcen)) {
    npp_daily <- rep(NA,length(dates_daily))
    npp_daily_er <- rep(NA, length(dates_daily))
    
    ## interpolation is based on the temporary index: (removes NAs to perform interepolation!)
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
      denNPPbiosA[tree_ind, m] <- mean(npp_daily[npp_month_ind], na.rm=T)*length(npp_month_ind)
      #for error calculate NPP for tree with 1mm bigger diameter and
      #diff is the error
      #denNPPbiosAer[tree_ind, m] <- mean(npp_daily_er[npp_month_ind], na.rm=T)*length(npp_month_ind)
    }
  }
  
  #convert to MgC ha month 
  NPPwoodsA = colSums(denNPPbiosA, na.rm=T)
  #NPPwoodsAstd = colSums(denNPPbiosAer, na.rm=T)
  # set NAs (i.e. columns that are entirely NA!):
  for (i in 1:length(dates_monthly)) {
    if(all(is.na(denNPPbiosA[,i]))) {
      NPPwoodsA[i] <- NA}
    #if(all(is.na(denNPPbiosAer[,i]))) {
    #  NPPwoodsAstd[i] <- NA}
  }
  names(NPPwoodsA) <- dates_monthly
  #names(NPPwoodsAstd) <- dates_monthly


###########################################################################
## Build list with matrices that contain monthly values:

#{
#  NPPwoodsA.monthly.matrix <- list(NPPwoodsA, dates_monthly) # ,NPPwoodsAstd
#  names(NPPwoodsA.monthly.matrix) <- c("NPPwoodsA") # ,"NPPwoodsAstd"
#}

##  Build data frame with time series structure
#{
  ##Restructure the data (according to time series structure):
#  Year <- NULL
#  Month <- NULL
#  Day <- NULL
  
#  for (i in 1:dim(NPPwoodsA)[2]) {   #################################### This doesn't work - we need a list of NPPwoodsA[1] = NPPwoodsA & NPPwoodsA[2] = dates
#    Year[((i-1)*12+1):((i-1)*12+12)] <- (rep(c(fir_year:last_year)[i],12))
#    Month[((i-1)*12+1):((i-1)*12+12)] <- (1:12)
#    Day[((i-1)*12+1):((i-1)*12+12)] <- rep(NA,12)
#  }
  
#  NPPwoodsA.monthly.ts <- data.frame(Year,Month,Day,
#                                     c(NPPwoodsA)) # ,c(NPPwoodsAstd)
  
#  colnames(NPPwoodsA.monthly.ts) <- c("Year","Month","Day",  
#                                      "NPPwoodsA") # ,"NPPwoodsAstd"
#}
############################################################################

return(NPPwoodsA)
}

# Get annual values: NPPwoodsA[13:24] & sum(NPPwoodsA[13:24])
  
# The monthly values you get from this code are plot-level values, as all the trees in the plot are censused. 
# Once you sum all monthly values from this, you get the annual value for the plot. 
# That is the most reliable annual value. 
# We then use this to estimate a scaling factor for the monthly dendrometer data (NPPcensus ha-1 yr-1 / NPPdend ha-1 yr-1).
