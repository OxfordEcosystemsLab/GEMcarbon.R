## Calculation of large tree NPP from dendrometer data

#all trees and lianas over 10 cm measured at 1.3 meters
#dendrometers measured every 3 months

#(1)tag number, (2) POM height if not 1.3m, (3) genus, family and species, (4) approximate height, (5)
#approximate canopy diameter, (6) subplot number, (7) estimated X and Y co-ordinates from bottom lefthand
#corner of plot, (8) bole form for live trees, (9) type of dead tree

largeTreeNPP_v1_dendrometer <- function(census, dendrometer_db, plotname, allometric_option) {

## Set of allometric equations after Chave et al 2005 is defined here:
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


# from census_start
par = census$plot
pointlA = census$subplot[which(par==plotname)]
TnumcenA = census$tag[which(par==plotname)]
PalmlA = census$palm[which(par==plotname)]
heightlA = census$Altura_tot_est[which(par==plotname)]
densitylA = census$Densidade_de_madeira_g_cm3[which(par==plotname)]
diameterlA = census$DAP_cm_start[which(par==plotname)]   # Diameter at breast height at first census!
#diameterlA_census = as.matrix(census[,10:14])  # matrix with diameter values over the years:

# from dendrometer_db:
dendro <- as.numeric(dendrometer_db$DAP_mm)

dendro[which(dendro < -1000)] <- NaN


# If no tree height, use the Feldpautch equations to get tree height

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
xdensityl = mean(densitylA, na.rm=T)
xdiameterl = mean(diameterlA, na.rm=T)

## fill NA's with mean plot averages:
densitylA[which(is.na(densitylA))] <- xdensityl
diameterlA[which(is.na(diameterlA))] <- xdiameterl
heightlA[which(is.na(heightlA))] <- 10^(Bo + B1*log10(diameterlA[which(is.na(heightlA))]/10) + Abar*So1 + n01*Pvbar + n02*Sdbar + n03*Tabar)

## fill implausible values:
heightlA[which(heightlA>45)]=45
heightlA[which(heightlA<5)]=5

densitylA[which(densitylA==0)]=xdensityl
diameterlA[which(diameterlA==0)]=xdiameterl


# assume 1zz error per dendrometer band

# calculate biomass for plot using dendrometer measurements:
er=0.1 # .1cm

## produce a list 'dendroallA' that contains all measurements for single tree:
dendroallA <- list()
dates <- list()
NPPbioA <- list()
NPPbioAer <- list()

# loop runs through all trees with plotname==plotname
for (tree_ind in 1:length(TnumcenA)) {
  ## calculates temporary index which indexes all measurements of a particular tree:
  temp_ind <- which(dendrometer_db$plot == plotname & dendrometer_db$tag == TnumcenA[tree_ind])
  
  dendroallA[[tree_ind]] <- dendro[temp_ind]
  dates[[tree_ind]] <- strptime(paste(dendrometer_db$year[temp_ind], dendrometer_db$month[temp_ind], dendrometer_db$day[temp_ind], sep="-"), format="%Y-%m-%d")
  
  ## calculate AGB for all trees:
  # (AGB, D in cm, q g/cm3, H in m)
  diax1 <- (diameterlA[tree_ind]*pi + dendroallA[[tree_ind]]/10)/pi # convert mm to cm; convert circum to diameter
  #diax1er <- (diax1*pi + er)/pi 
  
  ##new calculation using allometric equations in external file:
  if (allometrix == 2) {
    nor <- Chave2005_dry(diax=diax1, density=densitylA[tree_ind], height=heightlA[tree_ind])
    norer <- Chave2005_dry(diax=diax1er, density=densitylA[tree_ind], height=heightlA[tree_ind])
  } else if (allometrix == 3) {
    nor <- Chave2005_moist(diax=diax1, density=densitylA[tree_ind], height=heightlA[tree_ind])
    norer <- Chave2005_moist(diax=diax1er, density=densitylA[tree_ind], height=heightlA[tree_ind])
  } else if (allometrix == 4) {
    nor <- Chave2005_wet(diax=diax1, density=densitylA[tree_ind], height=heightlA[tree_ind])
    norer <- Chave2005_wet(diax=diax1er, density=densitylA[tree_ind], height=heightlA[tree_ind])
  }
  
  ## old allometric equations (i.e. moist option), outcommented on 21.03.2014; Sebastian
  #nor = Chave2005_moist(diax=diax1, density=densitylA[tree_ind], height=heightlA[tree_ind])
  #norer = Chave2005_moist(diax=diax1er, density=densitylA[tree_ind], height=heightlA[tree_ind])
  
  NPPbioA[[tree_ind]] = (nor)*(1/(2.1097*1000))     # convert kgto Mg=1/1000=10 and convert to carbon = 50%
  NPPbioAer[[tree_ind]] = (norer*(1/(2.1097*1000))) # convert kgto Mg=1/1000=10 and convert to carbon = 50%                       
}


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
denNPPbioA <- array(data=NA, dim=c(length(TnumcenA),(last_year-fir_year+1)*12))
denNPPbioAer <- array(data=NA, dim=c(length(TnumcenA),(last_year-fir_year+1)*12))  # this is not the error. it is the same value as above.
dates_monthly <- seq.Date(from=as.Date(paste(fir_year,"-01-01", sep=""), format="%Y-%m-%d"),
                          to=as.Date(paste(last_year,"-12-01", sep=""), format="%Y-%m-%d"), by="1 months")

## Build daily NPP matrix:
dates_daily <- seq.Date(from=as.Date(paste(fir_year,"-01-01", sep=""), format="%Y-%m-%d"),
                        to=as.Date(paste(last_year,"-12-31", sep=""), format="%Y-%m-%d"), by=1)

## loop runs through each tree:
for (tree_ind in 1:length(TnumcenA)) {
  npp_daily <- rep(NA,length(dates_daily)) # rep is repeat NA. We are building a vector of NAs the length of dates_daily
  npp_daily_er <- rep(NA, length(dates_daily))
  
  ## interpolation is based on the temporary index: (removes NAs to perform interepolation!)
  temp_index <-  which(!is.na(NPPbioA[[tree_ind]]))
  
  # calculate daily NPP interpolations (i.e. remove NAs):
  temp_npp <-  NPPbioA[[tree_ind]][temp_index]
  temp_npp_er <- NPPbioAer[[tree_ind]][temp_index]
  temp_dates <-  dates[[tree_ind]][temp_index]
  
  if (length(temp_npp)>1 & (!any(duplicated(temp_dates)))) {
  for (j in 1:(length(temp_npp)-1)) {
    ## daily growth rate:
    npp_daily[which(as.Date(temp_dates[j])==dates_daily):(which(as.Date(temp_dates[j+1])==dates_daily)-1)] <- (temp_npp[j+1]-temp_npp[j])/as.numeric(temp_dates[j+1]-temp_dates[j])
    npp_daily_er[which(as.Date(temp_dates[j])==dates_daily):(which(as.Date(temp_dates[j+1])==dates_daily)-1)] <- (temp_npp_er[j+1]-temp_npp_er[j])/as.numeric(temp_dates[j+1]-temp_dates[j])
  }
  
  ## loop over all months in the data:
  for (m in 1:length(dates_monthly)) {
    # index of all days in a particular months (to be used with npp_daily):
    npp_month_ind <- which(format(dates_daily, format="%Y-%m") == format(dates_monthly[m], format="%Y-%m"))
    denNPPbioA[tree_ind, m] <- mean(npp_daily[npp_month_ind], na.rm=T)*length(npp_month_ind)
    #for error calculate NPP for tree with 1mm bigger diameter and
    #diff is the error
    denNPPbioAer[tree_ind, m] <- mean(npp_daily_er[npp_month_ind], na.rm=T)*length(npp_month_ind)
  }
  }
}


#convert to MgC ha month 
NPPwoodA = colSums(denNPPbioA, na.rm=T) # sum of monthly NPP of all trees with dendrometer bands
NPPwoodAstd = colSums(denNPPbioAer, na.rm=T)
# set NAs (i.e. columns that are entirely NA!):
for (i in 1:length(dates_monthly)) {
  if(all(is.na(denNPPbioA[,i]))) {
    NPPwoodA[i] <- NA}
  if(all(is.na(denNPPbioAer[,i]))) {
    NPPwoodAstd[i] <- NA}
}
names(NPPwoodA) <- dates_monthly
names(NPPwoodAstd) <- dates_monthly

return(NPPwoodA)
}


# Once we have the monthly values from dendrometer bands, we can scale them to the plot by  
# multiplying by (NPPcensus annual value / NPPdend annual value).