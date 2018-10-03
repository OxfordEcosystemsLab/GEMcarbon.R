# Cecile Girardin June 2015
# This code re-formats census data to use the function NPPacw_census_function_2014.
# Last used for ACJ-01
# census data from William Farfan (TRU-04), Darcy Galiano (ACJ & PAN) 

require(ggplot2)
require(sqldf)
require(lubridate)

#plot_code;tree_tag;date; all dbh; NPP per tree for each time interval.

setwd("~/Github/GEMcarbon.R/a_readyforupload_db")
census <- read.table("andesplots_WFR_nov2014_noroots.csv", header=TRUE, sep=",", na.strings=c("NA", "NaN", ""), dec=".", strip.white=TRUE)
setwd("/Users/cecile/Dropbox/GEMcarbondb/db_csv/db_csv_2015/readyforupload_db/acj_pan")
census_ACJ01A <- read.table("census_ACJ_01.csv", header=TRUE, sep=",", na.strings=c("NA", "NaN", ""), dec=".", strip.white=TRUE)
wd_chave      <- read.table("wsg.csv", header=TRUE, sep=",", na.strings=c("NA", "NaN", ""), dec=".", strip.white=TRUE)

# get wood density from William Farfan's census data 2015, or from Chave's database. Or use Function wood density from Ken Feeley (2008)
census$WD14 <- census$WD.14   
census = as.data.frame(sapply(census, toupper)) # convert all characters to capitals
wd_farfan <- sqldf("SELECT family, genus, specie, AVG(WD14) FROM census GROUP BY family, genus") #, specie
colnames(wd_farfan) <- c("family", "genus", "species", "wdensity")

plotname = "SPD-01"

###############  ACJ-01

census_ACJ01A = as.data.frame(sapply(census_ACJ01A, toupper)) 

#### How do we keep all tree tags from census_ACJ01_0, and have NA as WD14 for those that don't have AND census_ACJ01_0.species = wd.specie? See Ken's function!!!
census_ACJ01 <- sqldf("SELECT census_ACJ01A.*, wd_farfan.wdensity FROM census_ACJ01A JOIN wd_farfan ON census_ACJ01A.Family = wd_farfan.family AND census_ACJ01A.genus = wd_farfan.genus") 

# re-format
str(census_ACJ01)
census_ACJ01$height  <- census_ACJ01$Height..2013.1 #should be: mean(census_ACJ01$Height..2013.1, census_ACJ01$Height..2014.2, (census_ACJ01$Height..2015.1/1000), na.rm=T)
census_ACJ01$density <- census_ACJ01$wdensity
census_ACJ01$tag     <- as.character(census_ACJ01$Tag.No)
census_ACJ01$plot    <- "ACJ-01"
census_ACJ01$DBH.1   <- (as.numeric(as.character(census_ACJ01$D..2013.1)))/10 
census_ACJ01$DBH.2   <- (as.numeric(as.character(census_ACJ01$D..2014.2)))/10 
census_ACJ01$DBH.3   <- (as.numeric(as.character(census_ACJ01$D..2015.1)))/10 

#################

#***********************************************************
#******************* Clean census data *********************
#***********************************************************

# choose a plot
data <- subset(census, plot==plotname) 

# define start and end date:
date_1 <- as.character("2006/08/30") # These dates are in William's original file andesplots_WFR_nov2014.xlx 
date_2 <- as.character("2008/09/07") 
date_3 <- as.character("NA") 
date_1 <- as.Date(format(strptime(date_1, format="%Y/%m/%d")))
date_2 <- as.Date(format(strptime(date_2, format="%Y/%m/%d")))
date_3 <- as.Date(format(strptime(date_3, format="%Y/%m/%d")))
census_interval_1 <- as.numeric(difftime(date_2, date_1, units="days"))
census_interval_yrs_1 <- census_interval_1/365
census_interval_2 <- as.numeric(difftime(date_3, date_2, units="days"))
census_interval_yrs_2 <- census_interval_2/365

# Make sure the data type are correct

data[,c("DBH.1", "DBH.2", "DBH.3", "DBH.4", "DBH.5", "WD14")] <- as.numeric(as.character(unlist(data[,c("DBH.1", "DBH.2", "DBH.3", "DBH.4", "DBH.5", "WD14")])))
str(data)

# Visualise data: normal distribution of dbhgrowth per year by diameter class
data$dbh_growth_yr <-  (data$DBH.2 - data$DBH.1) / census_interval_yrs_1  
gr_sd <- sd(data$dbh_growth_yr, na.rm=T)
plot1 <- ggplot(data=data, aes(x=DBH.1, y=dbh_growth_yr, na.rm=T)) +
               geom_point() +
               ylim(-10, 10) +
               ggtitle(data$plot)
plot1     

# use this plot to decide your cut off point for annual growth in cm and replace below.

# TO DO: should we correct for changing POM?

# do not allow recruits - TO DO: should we have a max size for recruits, beyond which they are assumed to be missing data?
w = which(is.na(data$DBH.1) & !is.na(data$DBH.2))
data$DBH.2[w] = 0/0 
data$recruits <- "ok"
data$recruits[w] <- "recruit"

w = which(is.na(data$DBH.1) & is.na(data$DBH.2) & !is.na(data$DBH.3)) 
data$DBH.3[w] = 0/0    
data$recruits[w] <- "recruit"

# Do not allow for shrinking trees. We allow shrinkage of 10%.
w = which(data$DBH.1 > data$DBH.2 + (data$DBH.1*0.1)) 
data$DBH.2[w] = data$DBH.1[w]  
data$srink <- "ok"
data$srink[w] <- "shrunk.dbh.2"

w = which(data$DBH.2 > data$DBH.3 + (data$DBH.2*0.1))   
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

#write.csv(data, file="ACJcensus_clean_date.csv")


#***********************************************************
#*******************Re-format dataframe to include dates****
#***********************************************************

eltrcensus <- data

# Add dates to eltrcensus.
# 1st census
yearDAP1   <- c()
monthDAP1  <- c()
dayDAP1    <- c()
for (ii in 1:(length(eltrcensus$plot))) {  # length(eltrcensus$plot) is equivalent to nrow(eltrcensus)
  yeartmp  = NA
  monthtmp = NA
  daytmp   = NA
  # cat("At row",i,"=",eltrcensus$plot[i]) # this is to print the number of row and the value in that row
  if (as.character(eltrcensus$plot[ii]) == plotname) {
    yeartmp  = 2006
    monthtmp = 08
    daytmp   = 30
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
  if (as.character(eltrcensus$plot[ii]) == plotname) {
    yeartmp  = 2008
    monthtmp = 09
    daytmp   = 07
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
  if (as.character(eltrcensus$plot[ii]) == plotname) {
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

eltrcensus$height <- eltrcensus$mean.H  
eltrcensus$density <- eltrcensus$WD14


eltrcen1 <- data.frame(plot_code=eltrcensus$plot, tree_tag=eltrcensus$tag, dbh=eltrcensus$DBH.1, height_m=eltrcensus$height, density=eltrcensus$density, datesDAP1)
eltrcen2 <- data.frame(plot_code=eltrcensus$plot, tree_tag=eltrcensus$tag, dbh=eltrcensus$DBH.2, height_m=eltrcensus$height, density=eltrcensus$density, datesDAP2)
eltrcen3 <- data.frame(plot_code=eltrcensus$plot, tree_tag=eltrcensus$tag, dbh=eltrcensus$DBH.3, height_m=eltrcensus$height, density=eltrcensus$density, datesDAP3) 
#eltrcen4 <- ...

census <- rbind(eltrcen1, eltrcen2) #, eltrcen3
census$height_m <- as.numeric(census$height_m)
census$plot <- NULL
str(census)
write.csv(census, file="test_SPD01_Mar1017.csv")

# get functions
setwd("~/Github/GEMcarbon.R") 
dir()
Sys.setlocale('LC_ALL','C') 
source("NPPacw_census_function_2015.r")
source("largetreebiomass_census.r")
source("allometric_equations_2014.R")

# run function for each plot (3 4 5 6 7 1 2 = TRU-03 TRU-04 TRU-07 TRU-08 WAY-01 SPD-01 SPD-02)
# Chave 2005 & first census interval
acj_01A  <- NPPacw_census(census, plotname="ACJ-01", allometric_option="Default", height_correction_option="Default", census1_year=2013, census2_year=2014)
acj_01B  <- NPPacw_census(census, plotname="ACJ-01", allometric_option="Default", height_correction_option="Default", census1_year=2014, census2_year=2015)

# Chave 2014 
tru3_14A <- NPPacw_census(census, plotname="ACJ-01", allometric_option=5, height_correction_option="Default", census1_year=2013, census2_year=2014)
tru3_14B <- NPPacw_census(census, plotname="ACJ-01", allometric_option=5, height_correction_option="Default", census1_year=2014, census2_year=2015)




