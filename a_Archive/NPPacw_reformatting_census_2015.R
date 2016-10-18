# Cecile Girardin 16/7/2014
# This code compiles census data to estimate biomass and NPP of large trees for the Andes synthesis paper Malhi et al. 2014.

# Eltr census from William (Way & Trocha Union), Chris (Tam, Alp, Esp), Cecile's PhD (Tono)
#setwd("C:/Users/Cecile/Dropbox/Andes synthesis 2013/NPPacw")
setwd("/Users/cecile/Dropbox/Andes synthesis 2013/NPPacw")
eltrcensus   <- read.table("andesplots_WFR_nov2014_noroots.csv", header=TRUE, sep=",", na.strings=c("NA", "NaN", ""), dec=".", strip.white=TRUE)
#eltrcensus   <- read.table("eltr_census_WF_nolandslide_2014.csv", header=TRUE, sep=",", na.strings=c("NA", "NaN", ""), dec=".", strip.white=TRUE)
#eltrcensus   <- read.table("SPD01_nolandslide_Jan2015.csv", header=TRUE, sep=",", na.strings=c("NA", "NaN", ""), dec=".", strip.white=TRUE)
# plots lost to landslide: c(1,2,4,5,15,16,18,19,20,21,22,23,25)
 
# re-format
eltrcensus$height <- eltrcensus$mean.H
eltrcensus$density <- eltrcensus$WD.14
eltrcensus$tag <- as.character(eltrcensus$tag)
sapply(eltrcensus, class)

#eltrcensus   <- read.table("censusdataWFKF2007_Tono.csv", header=TRUE, sep=",", na.strings=c("NA", "NaN", ""), dec=".", strip.white=TRUE)
#eltrcensus$height <- eltrcensus$H
#eltrcensus$DBH.1 <- as.numeric(as.character(eltrcensus$DBH.2003)) 
#eltrcensus$DBH.2 <- as.numeric(as.character(eltrcensus$DBH.2007)) 

#eltrcensus   <- read.table("CensusEsp.csv", header=TRUE, sep=",", na.strings=c("NA", "NaN", ""), dec=".", strip.white=TRUE)
# re-format
#eltrcensus$height <- eltrcensus$mean_height
#eltrcensus$DBH.1 <- as.numeric(as.character(eltrcensus$X2006)) 
#eltrcensus$DBH.2 <- as.numeric(as.character(eltrcensus$X2010)) 
#eltrcensus$DBH.3 <- as.numeric(as.character(eltrcensus$X2011)) 

# Try ALP-01 from forest plots
# dataset used in PED papers
eltrcensus   <- read.table("CensusIq.csv", header=TRUE, sep=",", na.strings=c("NA", "NaN", ""), dec=".", strip.white=TRUE)
# re-format
eltrcensus$DBH.1 <- (as.numeric(as.character(eltrcensus$X2006)))/10 
eltrcensus$DBH.2 <- (as.numeric(as.character(eltrcensus$X2008)))/10 
eltrcensus$DBH.3 <- (as.numeric(as.character(eltrcensus$X2010)))/10 

# dataset from forestplots (downloaded Jan 2015)
eltrcensus1         <- read.table("alp01_test_jan15.csv", header=TRUE, sep=",", na.strings=c("NA", "NaN", ""), dec=".", strip.white=TRUE)
densityheight1      <- read.table("CensusIq.csv", header=TRUE, sep=",", na.strings=c("NA", "NaN", ""), dec=".", strip.white=TRUE)
densityheight       <- subset(densityheight1, plot=="ALP-11") 
eltrcensus          <- sqldf("SELECT eltrcensus1.*, densityheight.height, densityheight.density FROM eltrcensus1 JOIN densityheight ON eltrcensus1.tree_tag = densityheight.tag")
eltrcensus$height_m <- eltrcensus$height
eltrcensus$dbh      <- eltrcensus$dbh/10

# check the number of distinct tags in each df
sqldf("select count(1) from (select distinct tag from densityheight)")
sqldf("select count(1) from densityheight")

# Flag duplicate trees
sub1    <- subset(eltrcensus, year==2006)
n_occur <- data.frame(table(sub1$tree_tag))
vars    <- n_occur[n_occur$Freq > 1,]
sub1[sub1$tree_tag %in% n_occur$Var1[n_occur$Freq > 1],]

sub2 <- subset(eltrcensus, year==2008)
n_occur <- data.frame(table(sub2$tree_tag))
n_occur[n_occur$Freq > 1,]
sub2[sub2$tree_tag %in% n_occur$Var1[n_occur$Freq > 1],]

sub3 <- subset(eltrcensus, year==2010)
n_occur <- data.frame(table(sub3$tree_tag))
n_occur[n_occur$Freq > 1,]
sub3[sub3$tree_tag %in% n_occur$Var1[n_occur$Freq > 1],]

eltrcensus   <- read.table("CensusTam.csv", header=TRUE, sep=",", na.strings=c("NA", "NaN", ""), dec=".", strip.white=TRUE)
#re-format
eltrcensus$DBH.1 <- as.numeric(as.character(eltrcensus$X2007)) # supress.Warnings()
eltrcensus$DBH.2 <- as.numeric(as.character(eltrcensus$X2008)) 
eltrcensus$DBH.3 <- as.numeric(as.character(eltrcensus$X2009)) 
eltrcensus$tag   <- as.character(eltrcensus$tree)

# load packages
require(ggplot2)

#***********************************************************
#******************* Clean census data *********************
#***********************************************************
# choose a plot
data <- subset(eltrcensus, plot=="WAY-01") 

# define start and end date:
date_1 <- as.character("2003/09/24") # this is 2006-02-06 in forestplots
date_2 <- as.character("2007/07/06") 
date_3 <- as.character("2011/11/24") 
date_1 <- as.Date(format(strptime(date_1, format="%Y/%m/%d")))
date_2 <- as.Date(format(strptime(date_2, format="%Y/%m/%d")))
date_3 <- as.Date(format(strptime(date_3, format="%Y/%m/%d")))
census_interval_1 <- as.numeric(difftime(date_2, date_1, units="days"))
census_interval_yrs_1 <- census_interval_1/365
census_interval_2 <- as.numeric(difftime(date_3, date_2, units="days"))
census_interval_yrs_2 <- census_interval_2/365


# Visualise data: normal distribution of dbhgrowth per year by diameter class
data$dbh_growth_yr <- (data$DBH.2 - data$DBH.1)/census_interval_yrs_1  
gr_sd <- sd(data$dbh_growth_yr, na.rm=T)
plot1 <- ggplot(data=data, aes(x=DBH.1, y=dbh_growth_yr, na.rm=T)) +
               geom_point() +
               ylim(-10, 10) +
               ggtitle(data$plot)
plot1               # use this plot to decide your cut off point for annual growth in cm and replace below.


# should we correct for changing POM?

# do not allow recruits - QUESTION: should we have a max size for recruits, beyond which they are assumed to be missing data?
w = which(is.na(data$DBH.1) & !is.na(data$DBH.2))
data$DBH.2[w] = 0/0 
data$recruits <- "ok"
data$recruits[w] <- "recruit"

w = which(is.na(data$DBH.1) & is.na(data$DBH.2) & !is.na(data$DBH.3)) 
data$DBH.3[w] = 0/0    
data$recruits[w] <- "recruit"

# Do not allow for shrinking trees. QUESTION: Allow shrinkage of 10%?
w = which(data$DBH.1 > data$DBH.2) 
data$DBH.2[w] = data$DBH.1[w]  
data$srink <- "ok"
data$srink[w] <- "shrunk.dbh.2"

w = which(data$DBH.2 > data$DBH.3)   
data$DBH.3[w] = data$DBH.2[w]        
data$srink[w] <- "shrunk.dbh.3"

# replace missing values in the middle of the census
w = which(!is.na(data$DBH.1) & is.na(data$DBH.2) & !is.na(data$DBH.3)) 
data$DBH.2[w] = (data$DBH.1[w] + data$DBH.3[w])/2        # mean rather than meadian flag this as 8 - see RAINFOR flags.
data$missing <- "ok"
data$missing[w] <- "missing"


# Define the maximum interval you would like to allow per year (e.g. 5 cm), or use the default 3SD.
maxincrement_1 <- 5 * census_interval_yrs_1 #(3*gr_sd)
maxincrement_2 <- 5 * census_interval_yrs_2 #(3*gr_sd)

w = which((data$DBH.2 - data$DBH.1) >= maxincrement_1)      
data$overgrown <- "ok"
data$overgrown[w] <- ">2cm growth per yr dbh.2"
data$value_replaced <- NA
data$value_replaced[w] <- data$DBH.2[w]
data$DBH.2[w] = (data$DBH.1[w] + maxincrement_1)    

w = which((data$DBH.3 - data$DBH.2) >= maxincrement_2)     
data$overgrown[w] <- ">2cm growth per yr dbh.3"
data$value_replaced[w] <- data$DBH.3[w]
data$DBH.3[w] = (data$DBH.2[w] + maxincrement_2) 

# Flag duplicate trees
n_occur <- data.frame(table(data$tag))
n_occur[n_occur$Freq > 1,]
data[data$tag %in% n_occur$Var1[n_occur$Freq > 1],]

## STOP ##
# Delete duplicate trees?
# data <- data[data$tag!=570.2 & data$X2009!=13.0, ]

# Standardise NAs
data[is.na(data)] <- NA

#eltrcensus <- rbind(tru3, tru4, tru7, tru8, way1, esp1, spd1, spd2_2.5, ton1, tam5, tam6, alp11, alp30_2.5)
#write.csv(eltrcensus, file="eltrcensus_clean_18Jul14.csv")

#***********************************************************
#*******************Re-format dataframe to include dates****
#***********************************************************

eltrcensus <- data
# column names should be
# ("plot", "sp", "tag", "dbh", "height", "density", "year", "month", "day", "plot_code")

# Add dates to eltrcensus.
# See census dates at the bottom of this code.
# 1st census
# eltrcensus <- eltrcensus[1:7352,] # this is to only use the rows that do not have NA at the end
yearDAP1   <- c()
monthDAP1  <- c()
dayDAP1    <- c()
for (ii in 1:(length(eltrcensus$plot))) {  #length(eltrcensus$plot) is equivalent to nrow(eltrcensus)
  yeartmp  = NA
  monthtmp = NA
  daytmp   = NA
  # cat("At row",i,"=",eltrcensus$plot[i]) this is to print the number of row and the value in that row
  if (as.character(eltrcensus$plot[ii]) == "SPD-01") {
    yeartmp  = 2006
    monthtmp = 8
    daytmp   = 30  
  }
  if (as.character(eltrcensus$plot[ii]) == "SPD-02") {
    yeartmp  = 2006
    monthtmp = 9
    daytmp   = 20
  }
  if (as.character(eltrcensus$plot[ii]) == "TRU-03") {
    yeartmp  = 2003
    monthtmp = 10
    daytmp   = 3
  }
  if (as.character(eltrcensus$plot[ii]) == "TRU-04") {
    yeartmp  = 2003
    monthtmp = 7
    daytmp   = 14
  }
  if (as.character(eltrcensus$plot[ii]) == "TRU-07") {
    yeartmp  = 2003
    monthtmp = 9
    daytmp   = 3
  }
  if (as.character(eltrcensus$plot[ii]) == "TRU-08") {
    yeartmp  = 2003
    monthtmp = 8
    daytmp   = 9
  }
  if (as.character(eltrcensus$plot[ii]) == "WAY-01") {
    yeartmp  = 2003
    monthtmp = 9
    daytmp   = 24
  }
  if (as.character(eltrcensus$plot[ii]) == "ESP-01") {
    yeartmp  = 2006
    monthtmp = 7
    daytmp   = 4
  }
  if (as.character(eltrcensus$plot[ii]) == "TAM-05") {
    yeartmp  = 2007
    monthtmp = 1
    daytmp   = 1
  }
  if (as.character(eltrcensus$plot[ii]) == "TAM-06") {
    yeartmp  = 2007
    monthtmp = 1
    daytmp   = 1
  }
  if (as.character(eltrcensus$plot[ii]) == "ALP-11") {
    yeartmp  = 2006 # get real dates
    monthtmp = 3
    daytmp   = 1
  }
  if (as.character(eltrcensus$plot[ii]) == "ALP-30") {
    yeartmp  = 2006 # get real dates
    monthtmp = 3
    daytmp   = 1
  }
  if (as.character(eltrcensus$plot[ii]) == "TON-02") {
    yeartmp  = 2003
    monthtmp = 10
    daytmp   = 15
  }
  yearDAP1  = c(yearDAP1,yeartmp)
  monthDAP1 = c(monthDAP1,monthtmp)
  dayDAP1   = c(dayDAP1,daytmp)
  datesDAP1 = data.frame(yearDAP1, monthDAP1, dayDAP1) 
  datesDAP1 = data.frame(datesDAP1[1,], eltrcensus$plot, row.names = NULL) # Make sure this is the right number of rows.
  colnames(datesDAP1) <- c("year", "month", "day", "plot")
}

# 2nd census
yearDAP2   <- NULL   # same as yearDAP2 = c()
monthDAP2  <- NULL
dayDAP2    <- NULL
for (ii in 1:(length(eltrcensus$plot))) {  #length(eltrcensus$plot) is equivalent to nrow(eltrcensus)
  yeartmp  = NA
  monthtmp = NA
  daytmp   = NA
  # cat("At row",i,"=",eltrcensus$plot[i]) this is to print the number of row and the value in that row
  if (as.character(eltrcensus$plot[ii]) == "SPD-01") {
    yeartmp  = 2008
    monthtmp = 9
    daytmp   = 7
  }
  if (as.character(eltrcensus$plot[ii]) == "SPD-02") {
    yeartmp  = 2008
    monthtmp = 9
    daytmp   = 12
  }
  if (as.character(eltrcensus$plot[ii]) == "TRU-03") {
    yeartmp  = 2007
    monthtmp = 5
    daytmp   = 28
  }
  if (as.character(eltrcensus$plot[ii]) == "TRU-04") {
    yeartmp  = 2007
    monthtmp = 6
    daytmp   = 8
  }
  if (as.character(eltrcensus$plot[ii]) == "TRU-07") {
    yeartmp  = 2007
    monthtmp = 6
    daytmp   = 27
  }
  if (as.character(eltrcensus$plot[ii]) == "TRU-08") {
    yeartmp  = 2007
    monthtmp = 7
    daytmp   = 2
  }
  if (as.character(eltrcensus$plot[ii]) == "WAY-01") {
    yeartmp  = 2007
    monthtmp = 7
    daytmp   = 6
  }
  if (as.character(eltrcensus$plot[ii]) == "ESP-01") {
    yeartmp  = 2010
    monthtmp = 1
    daytmp   = 25
  }
  if (as.character(eltrcensus$plot[ii]) == "TAM-05") {
    yeartmp  = 2008
    monthtmp = 1
    daytmp   = 1
  }
  if (as.character(eltrcensus$plot[ii]) == "TAM-06") {
    yeartmp  = 2008
    monthtmp = 1
    daytmp   = 1
  }
  if (as.character(eltrcensus$plot[ii]) == "ALP-11") {
    yeartmp  = 2008 # get real dates
    monthtmp = 3
    daytmp   = 1
  }
  if (as.character(eltrcensus$plot[ii]) == "ALP-30") {
    yeartmp  = 2008 # get real dates
    monthtmp = 3
    daytmp   = 1
  }
  if (as.character(eltrcensus$plot[ii]) == "TON-02") {
    yeartmp  = 2007
    monthtmp = 7
    daytmp   = 10
  }
  yearDAP2  = c(yearDAP2,yeartmp)
  monthDAP2 = c(monthDAP2,monthtmp)
  dayDAP2   = c(dayDAP2,daytmp)
  datesDAP2 = data.frame(yearDAP2, monthDAP2, dayDAP2)
  datesDAP2 = data.frame(datesDAP2[1,], eltrcensus$plot, row.names = NULL)  
  colnames(datesDAP2) <- c("year", "month", "day", "plot")
}

# 3rd census
yearDAP3   <- NULL   # same as yearDAP2 = c()
monthDAP3  <- NULL
dayDAP3    <- NULL
for (ii in 1:(length(eltrcensus$plot))) {  #length(eltrcensus$plot) is equivalent to nrow(eltrcensus)
  yeartmp  = NA
  monthtmp = NA
  daytmp   = NA
  # cat("At row",i,"=",eltrcensus$plot[i]) this is to print the number of row and the value in that row
  if (as.character(eltrcensus$plot[ii]) == "SPD-01") {
    yeartmp  = 2011
    monthtmp = 10
    daytmp   = 23  
  }
  if (as.character(eltrcensus$plot[ii]) == "SPD-02") {
    yeartmp  = 2011 
    monthtmp = 10
    daytmp   = 18
  }
  if (as.character(eltrcensus$plot[ii]) == "TRU-03") {
    yeartmp  = 2011
    monthtmp = 6
    daytmp   = 27
  }
  if (as.character(eltrcensus$plot[ii]) == "TRU-04") {
    yeartmp  = 2011 
    monthtmp = 7
    daytmp   = 30
  }
  if (as.character(eltrcensus$plot[ii]) == "TRU-07") {
    yeartmp  = 2011 
    monthtmp = 9
    daytmp   = 1
  }
  if (as.character(eltrcensus$plot[ii]) == "TRU-08") {
    yeartmp  = 2011
    monthtmp = 9
    daytmp   = 10
  }
  if (as.character(eltrcensus$plot[ii]) == "WAY-01") {
    yeartmp  = 2011
    monthtmp = 11
    daytmp   = 24
  }
  if (as.character(eltrcensus$plot[ii]) == "ESP-01") {
    yeartmp  = 2011
    monthtmp = 1
    daytmp   = 1
  }
  if (as.character(eltrcensus$plot[ii]) == "TAM-05") {
    yeartmp  = 2009
    monthtmp = 1
    daytmp   = 1
  }
  if (as.character(eltrcensus$plot[ii]) == "TAM-06") {
    yeartmp  = 2009
    monthtmp = 1
    daytmp   = 1
  }
  if (as.character(eltrcensus$plot[ii]) == "ALP-11") {
    yeartmp  = 2010 # get real dates
    monthtmp = 3
    daytmp   = 1
  }
  if (as.character(eltrcensus$plot[ii]) == "ALP-30") {
    yeartmp  = 2010 # get real dates
    monthtmp = 3
    daytmp   = 1
  }
  if (as.character(eltrcensus$plot[ii]) == "TON-02") {
    yeartmp  = NA
    monthtmp = NA
    daytmp   = NA
  }
  yearDAP3  = c(yearDAP3,yeartmp)
  monthDAP3 = c(monthDAP3,monthtmp)
  dayDAP3   = c(dayDAP3,daytmp)
  datesDAP3 = data.frame(yearDAP3, monthDAP3, dayDAP3)
  datesDAP3 = data.frame(datesDAP3[1,], eltrcensus$plot, row.names = NULL)   
  colnames(datesDAP3) <- c("year", "month", "day", "plot")
}

eltrcen1 <- data.frame(plot_code=eltrcensus$plot, tree_tag=eltrcensus$tag, dbh=eltrcensus$DBH.1, height_m=eltrcensus$height, density=eltrcensus$density, datesDAP1)
eltrcen2 <- data.frame(plot_code=eltrcensus$plot, tree_tag=eltrcensus$tag, dbh=eltrcensus$DBH.2, height_m=eltrcensus$height, density=eltrcensus$density, datesDAP2)
eltrcen3 <- data.frame(plot_code=eltrcensus$plot, tree_tag=eltrcensus$tag, dbh=eltrcensus$DBH.3, height_m=eltrcensus$height, density=eltrcensus$density, datesDAP3) 
#eltrcen4 <- ...

census <- rbind(eltrcen1, eltrcen2, eltrcen3)
#write.csv(census, file="testdata_WAY01_Jan1015.csv")

# get functions
setwd("/Users/cecile/GitHub/GEMcarbon.R") 
dir()
Sys.setlocale('LC_ALL','C') 
source("NPPacw_census_function_2014.R")
source("largetreebiomass_census.R")
source("allometric_equations_2014.R")

# run function for each plot (3 4 5 6 7 1 2 = TRU-03 TRU-04 TRU-07 TRU-08 WAY-01 SPD-01 SPD-02)
# Chave 2005 & first census interval
tru3_1  <- NPPacw_census(census, plotname="TRU-03", allometric_option="Default", height_correction_option="Default", census1_year=2007, census2_year=2011)
tru4_1  <- NPPacw_census(census, plotname="TRU-04", allometric_option="Default", height_correction_option="Default", census1_year=2007, census2_year=2011)
tru7_1  <- NPPacw_census(census, plotname="TRU-07", allometric_option="Default", height_correction_option="Default", census1_year=2007, census2_year=2011)
tru8_1  <- NPPacw_census(census, plotname="TRU-08", allometric_option="Default", height_correction_option="Default", census1_year=2007, census2_year=2011)
way1_1  <- NPPacw_census(census, plotname="WAY-01", allometric_option="Default", height_correction_option="Default", census1_year=2007, census2_year=2011)
esp1_1  <- NPPacw_census(census, plotname="ESP-01", allometric_option="Default", height_correction_option="Default", census1_year=2010, census2_year=2011)
spd1_1  <- NPPacw_census(census, plotname="SPD-01", allometric_option="Default", height_correction_option="Default", census1_year=2006, census2_year=2008)
spd2_1  <- NPPacw_census(census, plotname="SPD-02", allometric_option="Default", height_correction_option="Default", census1_year=2006, census2_year=2008)
ton1_1  <- NPPacw_census(census, plotname="TON-02", allometric_option="Default", height_correction_option="Default", census1_year=2003, census2_year=2007)
tam5_1  <- NPPacw_census(census, plotname="TAM-05", allometric_option="Default", height_correction_option="Default", census1_year=2008, census2_year=2009)
tam6_1  <- NPPacw_census(census, plotname="TAM-06", allometric_option="Default", height_correction_option="Default", census1_year=2007, census2_year=2008)
alp11_1 <- NPPacw_census(census, plotname="ALP-11", allometric_option="Default", height_correction_option="Default", census1_year=2006, census2_year=2008)
alp30_1 <- NPPacw_census(census, plotname="ALP-30", allometric_option="Default", height_correction_option=2, census1_year=2008, census2_year=2010)

# Chave 2014 
tru3_14 <- NPPacw_census(census, plotname="TRU-03", allometric_option=5, height_correction_option="Default", census1_year=2003, census2_year=2007)


# annual NPPacw - estimate SE
npp_ch05_1 <- c(tru3_1, tru4_1, tru7_1, tru8_1, way1_1, spd1_1, spd2_1) # missing esp1_1, ton1_1, tam5_1, tam6_1, alp11_1, alp30_1
npp_ch05_2 <- c(tru3_2, tru4_2, tru7_2, tru8_2, way1_2, spd1_2, spd2_2)
plot     <- c("tru3", "tru4", "tru7", "tru8", "way1", "spd1", "spd2") 
plotnpp  <- data.frame(npp_ch05_1, npp_ch05_2, plot)

write.csv(plotnpp, file="AndesSyntesis_nppacw_2014.csv")

#plot annual NPPacw
yrnpp <- ggplot(data=plotnpp, aes(x=plot, y=npp_ch05, na.rm=T)) + 
  geom_point()+
  geom_point(data=plotnpp, aes(x=plot, y=npp_ch14, na.rm=T), colour='red') +
  ylim(0,3) +                          
  xlab("") + ylab(expression(paste("NPP (Mg C ha", yr^-1, ")", sep=""))) +
  theme_classic(base_size = 15, base_family = "") + theme(legend.position="top") +
  theme(legend.title=element_blank()) + theme(legend.key = element_blank())


# census intervals
#census1
#SPD-01 2006/8/30
#SPD-02 2006/9/20
#TRU-03 2003/10/3
#TRU-04 2003/7/14
#TRU-07 2003/9/3
#TRU-08 2003/8/9
#WAY-01 2003/9/24
#ESP-01 2006/7/4
#TON-01 2003/10/15 # approximate date
#TAM-05 2005/1/16
#TAM-06 2005/1/16

#census2
#SPD-01 2008/9/7
#SPD-02 2008/9/12
#TRU-03 2007/5/28
#TRU-04 2007/6/8
#TRU-07 2007/6/27
#TRU-08 2007/7/2
#WAY-01 2007/7/6
#ESP-01 2010/1/25
#TON-01 2007/7/10 # approximate date
#TAM-05 2006/1/16
#TAM-06 2006/1/16

#census3  
#SPD-01 2011/10/23
#SPD-02 2011/10/18 
#TRU-03 2011/6/27 
#TRU-04 2011/7/30 
#TRU-07 2011/9/1
#TRU-08 2011/9/10
#WAY-01 2011/11/24 
#ESP-01 2011/1/1
#TAM-05 2008/8/25
#TAM-06 2008/8/28

#census 4
#ESP-01 2011/12/1
#TAM-05 2011/8/27
#TAM-06 2011/9/2

# SPD-01 Landslide wiped out these plots: 1,2,4,5,15,16,18,19,20,21,22,23,25