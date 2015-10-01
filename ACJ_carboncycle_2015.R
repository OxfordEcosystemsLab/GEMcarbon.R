## This code runs the functions needed to determine each component of the carbon cycle for one plot


# get functions
setwd("/Users/cecile/GitHub/GEMcarbon.R") 
dir()
Sys.setlocale('LC_ALL','C') 
source("flf_2015.R")
source("allometric_equations_2014.R")
source("NPPacw_census_function_2014.R")
source("NPProot_2015_CG.R")

#############################################################################################################################
######################### flf() : a function calculate monthly and annual NPP from fine litterfall.##########################
#############################################################################################################################

## Read-in data:
setwd("/Users/cecile/Dropbox/GEMcarbondb/db_csv/db_csv_2015/readyforupload_db/acj_pan_2015")
flf_ACJ <- read.table("/Users/cecile/Dropbox/GEMcarbondb/db_csv/db_csv_2015/readyforupload_db/acj_pan_2015/Litterfall_ACJ_2013_2014.csv", sep=",", header=T)
flf_PAN02 <- read.table("/Users/cecile/Dropbox/GEMcarbondb/db_csv/db_csv_2015/readyforupload_db/acj_pan_2015/Litterfall_PAN02_2013_2014.csv", sep=",", header=T)
flf_PAN03 <- read.table("/Users/cecile/Dropbox/GEMcarbondb/db_csv/db_csv_2015/readyforupload_db/acj_pan_2015/Litterfall_PAN03_2013_2014.csv", sep=",", header=T)
flf_TRU04 <- read.table("/Users/cecile/Dropbox/GEMcarbondb/db_csv/db_csv_2015/readyforupload_db/acj_pan_2015/Litterfall_TRU04_2013_2014.csv", sep=",", header=T)

## ACJ-01
# define parameters
plotsize = 1 
plotname = "ACJ"
data.flf <- flf_ACJ
str(flf_ACJ)
ACJ_NPPlitterfall <- flf(flf_ACJ, plotname, ret="monthly.means.ts", plotit=T) # TO DO: fix figure.
write.csv(flf.data.monthly.ts, file="ACJ01_NPPlitterfall_Sept2015.csv")

## PAN-02
# define parameters
plotsize = 1 
plotname = "PAN-02"
data.flf <- flf_PAN02
plotit=T
PAN02_NPPlitterfall <- flf(flf_PAN02, plotname, ret="monthly.means.ts", plotit=T)
write.csv(flf.data.monthly.ts, file="PAN02_NPPlitterfall_Sept2015.csv")

## PAN-03
# define parameters
plotsize = 1 
plotname = "PAN-03"
str(flf_PAN03)
PAN03_NPPlitterfall <- flf(flf_PAN03, plotname, ret="monthly.means.ts", plotit=T)
write.csv(PAN03_NPPlitterfall, file="PAN03_NPPlitterfall.csv")

NPPlitterfall <- (mean(flf.data.monthly.ts$totflfAs, na.rm=T))*12
NPPleaf
NPPflowers
NPPfruit
NPPbranches


## TRU-04 !!! GET THE DATA FROM MY PHD.
# define parameters
plotsize = 1 
plotname = "TRU-04"
str(flf_TRU04)
TRU04_NPPlitterfall <- flf(flf_TRU04, plotname, ret="monthly.means.ts", plotit=T)
write.csv(TRU04_NPPlitterfall, file="TRU04_NPPlitterfall.csv")



#############################################################################################################################
########## NPPacw_census() : a function to calculate annual NPPacw from census data for trees >10 cm.########################
#############################################################################################################################

require(ggplot2)
require(sqldf)
require(lubridate)

setwd("/Users/cecile/Dropbox/GEMcarbondb/db_csv/db_csv_2015/readyforupload_db")
census_TRU04A <- read.table("andesplots_WFR_nov2014_noroots.csv", header=TRUE, sep=",", na.strings=c("NA", "NaN", ""), dec=".", strip.white=TRUE)
setwd("/Users/cecile/Dropbox/GEMcarbondb/db_csv/db_csv_2015/readyforupload_db/acj_pan_2015")
census_ACJ01 <- read.table("census_ACJ_01.csv", header=TRUE, sep=",", na.strings=c("NA", "NaN", ""), dec=".", strip.white=TRUE)
census_PAN02 <- read.table("census_PAN_02.csv", header=TRUE, sep=",", na.strings=c("NA", "NaN", ""), dec=".", strip.white=TRUE)
census_PAN03 <- read.table("census_PAN_03.csv", header=TRUE, sep=",", na.strings=c("NA", "NaN", ""), dec=".", strip.white=TRUE)
wd_chave      <- read.table("wsg.txt", header=TRUE, sep=",", na.strings=c("NA", "NaN", ""), dec=".", strip.white=TRUE)
wsg           <- census_TRU04A

####### Function wood density from Ken Feeley (2008) 

find.wsg=function(family, genus, species, wsg){
  capply = function(str, ff) {sapply(lapply(strsplit(str, NULL), ff), paste, collapse="") }
  cap = function(char) {if (any(ind <- letters==char)) LETTERS[ind]    else char}
  capitalize = function(str) {ff <- function(x) paste(lapply(unlist(strsplit(x, NULL)),cap),collapse="")
                              capply(str,ff)}
  lower = function(char) {if (any(ind <- LETTERS==char)) letters[ind]    else char}
  lowerize = function(str) {ff <- function(x) paste(lapply(unlist(strsplit(x, NULL)),lower),collapse="")
                            capply(str,ff)}
  
  family=(as.character(family))
  genus=(as.character(genus))
  species=(as.character(species))
  
  w=which(is.na(family))
  family[w]='unknown'
  w=which(is.na(genus))
  genus[w]='unknown'
  w=which(is.na(species))
  species[w]='unknown'
  
  family=capitalize((family))
  genus=capitalize((genus))
  species=capitalize((species))
  
  family2=capitalize(as.character(wsg$family))
  genus2=capitalize(as.character(wsg$genus))
  #species2=capitalize(as.character(wsg$specie))  TO DO: what is wrong with this? Doesn't work. is it the () and numbers in the data?
  
  fam.wsg=tapply(wsg$WD.14, family2, mean, na.rm=T)
  gen.wsg=tapply(wsg$WD.14, genus2, mean, na.rm=T)
  
  family2=names(fam.wsg)
  genus2=names(gen.wsg)
  
  m=match(family, family2)
  wsg.est=fam.wsg[m]
  
  m=match(genus, genus2)
  w=which(!is.na(m))
  wsg.est[w]=gen.wsg[m][w]
  
  #m=match(species, species2) TO DO: fix species2
  #w=which(!is.na(m))
  #wsg.est[w]=wsg$WSG[m][w]
  
  names(wsg.est)=species
  wsg.est
}


# ACJ-01
# add wood density
# get wood density from William Farfan's census data 2015, or from Chave's database.

family  = census_ACJ01$Family 
genus   = census_ACJ01$genus
species = census_ACJ01$species

census_ACJ01$wdensity <- find.wsg(family, genus, species, wsg)

# convert all characters to capitals (some are caps some are not in the family column)
# census_TRU04$WD14 <- census_TRU04$WD.14
# census_ACJ01A = as.data.frame(sapply(census_ACJ01A, toupper)) 
# census_TRU04 = as.data.frame(sapply(census_TRU04, toupper)) 
# wd_farfan <- sqldf("SELECT family, genus, specie, AVG(WD14) FROM census_TRU04 GROUP BY family, genus") #, specie
# colnames(wd_farfan) <- c("family", "genus", "species", "wdensity")
#### How do we keep all tree tags from census_ACJ01_0, and have NA as WD14 for those that don't have AND census_ACJ01_0.species = wd.specie? See Ken's function!!!
#census_ACJ01 <- sqldf("SELECT census_ACJ01A.*, wd_farfan.wdensity FROM census_ACJ01A JOIN wd_farfan ON census_ACJ01A.Family = wd_farfan.family AND census_ACJ01A.genus = wd_farfan.genus") 

# re-format
str(census_ACJ01)
h <- cbind(as.numeric(census_ACJ01$Height..2013.1), as.numeric(census_ACJ01$Height..2015.1/1000)) 
census_ACJ01$height  <- rowMeans(h, na.rm = T) 
census_ACJ01$density <- census_ACJ01$wdensity
census_ACJ01$tag     <- as.character(census_ACJ01$Tag.No)
census_ACJ01$plot    <- "ACJ-01"
census_ACJ01$DBH.1   <- (as.numeric(as.character(census_ACJ01$D..2013.1)))/10 
census_ACJ01$DBH.2   <- (as.numeric(as.character(census_ACJ01$D..2014.2)))/10 
census_ACJ01$DBH.3   <- (as.numeric(as.character(census_ACJ01$D..2015.1)))/10 

sapply(census_ACJ01, class)

# check the number of distinct tags in each df
sqldf("select count(1) from (select distinct tag from census_ACJ01)")
sqldf("select count(1) from census_ACJ01")

#***********************************************************
#******************* Clean census data *********************
#***********************************************************

# choose a plot
data <- subset(census_ACJ01, plot=="ACJ-01") 

# define start and end date:

date_1 <- as.character("2013/01/22") # 22 de Enero 2013
date_2 <- as.character("2014/03/15") # 15 de Marzo 2014
date_3 <- as.character("2015/03/16") # 16 de Marzo 2015
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
dataACJ01 <- data

#write.csv(data, file="ACJcensus_clean_date.csv")

####################################################################################
# PAN-02
# add wood density - get wood density from William Farfan's census data 2015.
family  = census_PAN02$family 
genus   = census_PAN02$genus
species = census_PAN02$species

census_PAN02$wdensity <- find.wsg(family, genus, species, wsg)

# re-format
str(census_PAN02)
h <- cbind(as.numeric(census_PAN02$Height..2013.2), as.numeric(census_PAN02$Height..2015.1/1000)) 
census_PAN02$height  <- rowMeans(h, na.rm = T) 
census_PAN02$density <- census_PAN02$wdensity
census_PAN02$tag     <- as.character(census_PAN02$Tag.No)
census_PAN02$plot    <- "PAN-02"
census_PAN02$DBH.1   <- (as.numeric(as.character(census_PAN02$D..2013.2)))/10 
census_PAN02$DBH.2   <- (as.numeric(as.character(census_PAN02$D..2015.1)))/10 

sapply(census_PAN02, class)

# check the number of distinct tags in each df
sqldf("select count(1) from (select distinct tag from census_PAN02)")
sqldf("select count(1) from census_PAN02")

#***********************************************************
#******************* Clean census data *********************
#***********************************************************

# choose a plot
dataPAN02 <- subset(census_PAN02, plot=="PAN-02") 

# define start and end date:
date_1 <- as.character("2013/04/12") # the census date for small trees is: 12 de Abril 2013
date_2 <- as.character("2015/03/06") # 06 Marzo 2015 2 Marzo 2015

date_1 <- as.character("2013/03/06") 
date_2 <- as.character("2015/03/06") # PAN 02 06 Marzo 2015
date_1 <- as.Date(format(strptime(date_1, format="%Y/%m/%d")))
date_2 <- as.Date(format(strptime(date_2, format="%Y/%m/%d")))
census_interval_1 <- as.numeric(difftime(date_2, date_1, units="days"))
census_interval_yrs_1 <- census_interval_1/365

# Visualise data: normal distribution of dbhgrowth per year by diameter class
dataPAN02$dbh_growth_yr <- (dataPAN02$DBH.2 - dataPAN02$DBH.1)/census_interval_yrs_1  
gr_sd <- sd(dataPAN02$dbh_growth_yr, na.rm=T)
plot1 <- ggplot(data=dataPAN02, aes(x=DBH.1, y=dbh_growth_yr, na.rm=T)) +
  geom_point() +
  ylim(-10, 10) +
  ggtitle(dataPAN02$plot)
plot1               
# use this plot to decide your cut off point for annual growth in cm and replace below.

# TO DO: should we correct for changing POM?

# do not allow recruits - TO DO: should we have a max size for recruits, beyond which they are assumed to be missing data?
w = which(is.na(dataPAN02$DBH.1) & !is.na(dataPAN02$DBH.2))
dataPAN02$DBH.2[w] = 0/0 
dataPAN02$recruits <- "ok"
dataPAN02$recruits[w] <- "recruit"

w = which(is.na(dataPAN02$DBH.1) & is.na(dataPAN02$DBH.2) & !is.na(dataPAN02$DBH.3)) 
dataPAN02$DBH.3[w] = 0/0    
dataPAN02$recruits[w] <- "recruit"

# Do not allow for shrinking trees. We allow shrinkage of 10%.
w = which(dataPAN02$DBH.1 > dataPAN02$DBH.2 + (dataPAN02$DBH.1*0.1)) 
dataPAN02$DBH.2[w] = dataPAN02$DBH.1[w]  
dataPAN02$srink <- "ok"
dataPAN02$srink[w] <- "shrunk.dbh.2"

w = which(dataPAN02$DBH.2 > dataPAN02$DBH.3 + (dataPAN02$DBH.2*0.1))   
dataPAN02$DBH.3[w] = dataPAN02$DBH.2[w]        
dataPAN02$srink[w] <- "shrunk.dbh.3"

# replace missing values in the middle of the census
w = which(!is.na(dataPAN02$DBH.1) & is.na(dataPAN02$DBH.2) & !is.na(dataPAN02$DBH.3)) 
dataPAN02$DBH.2[w] = (dataPAN02$DBH.1[w] + dataPAN02$DBH.3[w])/2        # mean rather than meadian flag this as 8 - see RAINFOR flags.
dataPAN02$missing <- "ok"
dataPAN02$missing[w] <- "missing"

# Define the maximum interval you would like to allow per year (e.g. 5 cm), or use the default 3SD.
maxincrement_1 <- 5 * census_interval_yrs_1 #(3*gr_sd)
maxincrement_2 <- 5 * census_interval_yrs_2 #(3*gr_sd)

w = which((dataPAN02$DBH.2 - dataPAN02$DBH.1) >= maxincrement_1)      
dataPAN02$overgrown <- "ok"
dataPAN02$overgrown[w] <- ">2cm growth per yr dbh.2"
dataPAN02$value_replaced <- NA
dataPAN02$value_replaced[w] <- dataPAN02$DBH.2[w]
dataPAN02$DBH.2[w] = (dataPAN02$DBH.1[w] + maxincrement_1)    

w = which((dataPAN02$DBH.3 - dataPAN02$DBH.2) >= maxincrement_2)     
dataPAN02$overgrown[w] <- ">2cm growth per yr dbh.3"
dataPAN02$value_replaced[w] <- dataPAN02$DBH.3[w]
dataPAN02$DBH.3[w] = (dataPAN02$DBH.2[w] + maxincrement_2) 

# Flag duplicate trees
n_occur <- data.frame(table(dataPAN02$tag))
n_occur[n_occur$Freq > 1,]
dataPAN02[dataPAN02$tag %in% n_occur$Var1[n_occur$Freq > 1],]

# Delete duplicate trees?
dataPAN02 <- dataPAN02[dataPAN02$tag!="798.4A" & dataPAN02$height!=20, ]

# Standardise NAs
dataPAN02[is.na(dataPAN02)] <- NA

#write.csv(data, file="PAN02census_clean_date.csv")

############################################################################################
# PAN-03
# add wood density
# get wood density from William Farfan's census data 2015, or from Chave's database.

family  = census_PAN03$family 
genus   = census_PAN03$genus
species = census_PAN03$species

census_PAN03$wdensity <- find.wsg(family, genus, species, wsg)

# re-format
str(census_PAN03)
h <- cbind(as.numeric(census_PAN03$Height..2013.2), as.numeric(census_PAN03$Height..2014.2), as.numeric(census_PAN03$Height..2015.2/1000)) 
census_PAN03$height  <- rowMeans(h, na.rm = T) 
census_PAN03$density <- census_PAN03$wdensity
census_PAN03$tag     <- as.character(census_PAN03$Tag.No)
census_PAN03$plot    <- "PAN-03"
census_PAN03$DBH.1   <- (as.numeric(as.character(census_PAN03$D..2013.2)))/10 
census_PAN03$DBH.2   <- (as.numeric(as.character(census_PAN03$D..2014.2)))/10 
census_PAN03$DBH.3   <- (as.numeric(as.character(census_PAN03$D..2015.2)))/10

sapply(census_PAN03, class)

# check the number of distinct tags in each df
sqldf("select count(1) from (select distinct tag from census_PAN03)")
sqldf("select count(1) from census_PAN03")

#***********************************************************
#******************* Clean census data *********************
#***********************************************************

# choose a plot
dataPAN03 <- subset(census_PAN03, plot=="PAN-03") 

# define start and end date:

date_1 <- as.character("2013/03/30") # 30 Marzo 2013
date_2 <- as.character("2014/03/22") # 22 Marzo 2014
date_3 <- as.character("2015/03/05") # 05 Marzo 2015 to 04 Marzo 2015 
date_1 <- as.Date(format(strptime(date_1, format="%Y/%m/%d")))
date_2 <- as.Date(format(strptime(date_2, format="%Y/%m/%d")))
date_3 <- as.Date(format(strptime(date_3, format="%Y/%m/%d")))
census_interval_1 <- as.numeric(difftime(date_2, date_1, units="days"))
census_interval_yrs_1 <- census_interval_1/365
census_interval_2 <- as.numeric(difftime(date_3, date_2, units="days"))
census_interval_yrs_2 <- census_interval_2/365

# Visualise data: normal distribution of dbhgrowth per year by diameter class
dataPAN03$dbh_growth_yr <- (dataPAN03$DBH.2 - dataPAN03$DBH.1)/census_interval_yrs_1  
gr_sd <- sd(dataPAN03$dbh_growth_yr, na.rm=T)
plot1 <- ggplot(data=dataPAN03, aes(x=DBH.1, y=dbh_growth_yr, na.rm=T)) +
  geom_point() +
  ylim(-10, 10) +
  ggtitle(dataPAN03$plot)
plot1               
# use this plot to decide your cut off point for annual growth in cm and replace below.

# TO DO: should we correct for changing POM?

# do not allow recruits - TO DO: should we have a max size for recruits, beyond which they are assumed to be missing data?
w = which(is.na(dataPAN03$DBH.1) & !is.na(dataPAN03$DBH.2))
dataPAN03$DBH.2[w] = 0/0 
dataPAN03$recruits <- "ok"
dataPAN03$recruits[w] <- "recruit"

w = which(is.na(dataPAN03$DBH.1) & is.na(dataPAN03$DBH.2) & !is.na(dataPAN03$DBH.3)) 
dataPAN03$DBH.3[w] = 0/0    
dataPAN03$recruits[w] <- "recruit"

# Do not allow for shrinking trees. We allow shrinkage of 10%.
w = which(dataPAN03$DBH.1 > dataPAN03$DBH.2 + (dataPAN03$DBH.1*0.1)) 
dataPAN03$DBH.2[w] = dataPAN03$DBH.1[w]  
dataPAN03$srink <- "ok"
dataPAN03$srink[w] <- "shrunk.dbh.2"

w = which(dataPAN03$DBH.2 > dataPAN03$DBH.3 + (dataPAN03$DBH.2*0.1))   
dataPAN03$DBH.3[w] = dataPAN03$DBH.2[w]        
dataPAN03$srink[w] <- "shrunk.dbh.3"

# replace missing values in the middle of the census
w = which(!is.na(dataPAN03$DBH.1) & is.na(dataPAN03$DBH.2) & !is.na(dataPAN03$DBH.3)) 
dataPAN03$DBH.2[w] = (dataPAN03$DBH.1[w] + dataPAN03$DBH.3[w])/2        # mean rather than meadian flag this as 8 - see RAINFOR flags.
dataPAN03$missing <- "ok"
dataPAN03$missing[w] <- "missing"

# Define the maximum interval you would like to allow per year (e.g. 5 cm), or use the default 3SD.
maxincrement_1 <- 5 * census_interval_yrs_1 #(3*gr_sd)
maxincrement_2 <- 5 * census_interval_yrs_2 #(3*gr_sd)

w = which((dataPAN03$DBH.2 - dataPAN03$DBH.1) >= maxincrement_1)      
dataPAN03$overgrown <- "ok"
dataPAN03$overgrown[w] <- ">2cm growth per yr dbh.2"
dataPAN03$value_replaced <- NA
dataPAN03$value_replaced[w] <- dataPAN03$DBH.2[w]
dataPAN03$DBH.2[w] = (dataPAN03$DBH.1[w] + maxincrement_1)    

w = which((dataPAN03$DBH.3 - dataPAN03$DBH.2) >= maxincrement_2)     
dataPAN03$overgrown[w] <- ">2cm growth per yr dbh.3"
dataPAN03$value_replaced[w] <- dataPAN03$DBH.3[w]
dataPAN03$DBH.3[w] = (dataPAN03$DBH.2[w] + maxincrement_2) 

# Flag duplicate trees
n_occur <- data.frame(table(dataPAN03$tag))
n_occur[n_occur$Freq > 1,]
data[dataPAN03$tag %in% n_occur$Var1[n_occur$Freq > 1],]

# Delete duplicate trees?
# data <- data[data$tag!="798.4A" & data$height!=20, ]

# Standardise NAs
dataPAN03[is.na(dataPAN03)] <- NA

#write.csv(data, file="PAN03census_clean_date.csv")

################################################################################################################################################
# TRU-04
# re-format
census_TRU04 <- subset(census_TRU04A, plot=="TRU-04") 
h <- cbind(as.numeric(census_TRU04$H.1), as.numeric(census_TRU04$H.2), as.numeric(census_TRU04$H.3), as.numeric(census_TRU04$H.4), as.numeric(census_TRU04$H.5)) 
census_TRU04$height  <- rowMeans(h, na.rm = T) 
census_TRU04$density <- census_TRU04$WD.14
census_TRU04$tag     <- as.character(census_TRU04$tag)
census_TRU04$plot    <- "TRU-04"

str(census_TRU04)
sapply(census_TRU04, class)

# check the number of distinct tags in each df
sqldf("select count(1) from (select distinct tag from census_TRU04)")
sqldf("select count(1) from census_TRU04")

#***********************************************************
#******************* Clean census data *********************
#***********************************************************

# choose a plot
dataTRU04 <- subset(census_TRU04, plot=="TRU-04") 

# define start and end date:
date_1 <- as.character("2003/07/14") # CHANGE
date_2 <- as.character("2007/06/08") # CHANGE
date_3 <- as.character("2011/07/30") # CHANGE
date_1 <- as.Date(format(strptime(date_1, format="%Y/%m/%d")))
date_2 <- as.Date(format(strptime(date_2, format="%Y/%m/%d")))
date_3 <- as.Date(format(strptime(date_3, format="%Y/%m/%d")))
census_interval_1 <- as.numeric(difftime(date_2, date_1, units="days"))
census_interval_yrs_1 <- census_interval_1/365
census_interval_2 <- as.numeric(difftime(date_3, date_2, units="days"))
census_interval_yrs_2 <- census_interval_2/365

# Visualise data: normal distribution of dbhgrowth per year by diameter class
dataTRU04$dbh_growth_yr <- (dataTRU04$DBH.2 - dataTRU04$DBH.1)/census_interval_yrs_1  
gr_sd <- sd(dataTRU04$dbh_growth_yr, na.rm=T)
plot1 <- ggplot(data=dataTRU04, aes(x=DBH.1, y=dbh_growth_yr, na.rm=T)) +
  geom_point() +
  ylim(-10, 10) +
  ggtitle(dataTRU04$plot)
plot1               
# use this plot to decide your cut off point for annual growth in cm and replace below.

# TO DO: should we correct for changing POM?

# do not allow recruits - TO DO: should we have a max size for recruits, beyond which they are assumed to be missing data?
w = which(is.na(dataTRU04$DBH.1) & !is.na(dataTRU04$DBH.2))
dataTRU04$DBH.2[w] = 0/0 
dataTRU04$recruits <- "ok"
dataTRU04$recruits[w] <- "recruit"

w = which(is.na(dataTRU04$DBH.1) & is.na(dataTRU04$DBH.2) & !is.na(dataTRU04$DBH.3)) 
dataTRU04$DBH.3[w] = 0/0    
dataTRU04$recruits[w] <- "recruit"

# Do not allow for shrinking trees. We allow shrinkage of 10%.
w = which(dataTRU04$DBH.1 > dataTRU04$DBH.2 + (dataTRU04$DBH.1*0.1)) 
dataTRU04$DBH.2[w] = dataTRU04$DBH.1[w]  
dataTRU04$srink <- "ok"
dataTRU04$srink[w] <- "shrunk.dbh.2"

w = which(dataTRU04$DBH.2 > dataTRU04$DBH.3 + (dataTRU04$DBH.2*0.1))   
dataTRU04$DBH.3[w] = dataTRU04$DBH.2[w]        
dataTRU04$srink[w] <- "shrunk.dbh.3"

# replace missing values in the middle of the census
w = which(!is.na(dataTRU04$DBH.1) & is.na(dataTRU04$DBH.2) & !is.na(dataTRU04$DBH.3)) 
dataTRU04$DBH.2[w] = (dataTRU04$DBH.1[w] + dataTRU04$DBH.3[w])/2        # mean rather than meadian flag this as 8 - see RAINFOR flags.
dataTRU04$missing <- "ok"
dataTRU04$missing[w] <- "missing"

# Define the maximum interval you would like to allow per year (e.g. 5 cm), or use the default 3SD.
maxincrement_1 <- 5 * census_interval_yrs_1 #(3*gr_sd)
maxincrement_2 <- 5 * census_interval_yrs_2 #(3*gr_sd)

w = which((dataTRU04$DBH.2 - dataTRU04$DBH.1) >= maxincrement_1)      
dataTRU04$overgrown <- "ok"
dataTRU04$overgrown[w] <- ">2cm growth per yr dbh.2"
dataTRU04$value_replaced <- NA
dataTRU04$value_replaced[w] <- dataTRU04$DBH.2[w]
dataTRU04$DBH.2[w] = (dataTRU04$DBH.1[w] + maxincrement_1)    

w = which((dataTRU04$DBH.3 - dataTRU04$DBH.2) >= maxincrement_2)     
dataTRU04$overgrown[w] <- ">2cm growth per yr dbh.3"
dataTRU04$value_replaced[w] <- dataTRU04$DBH.3[w]
dataTRU04$DBH.3[w] = (dataTRU04$DBH.2[w] + maxincrement_2) 

# Flag duplicate trees
n_occur <- data.frame(table(dataTRU04$tag))
n_occur[n_occur$Freq > 1,]
dataTRU04[dataTRU04$tag %in% n_occur$Var1[n_occur$Freq > 1],]

# Delete duplicate trees?
# data <- data[data$tag!=570.2 & data$X2009!=13.0, ]

# Standardise NAs
dataTRU04[is.na(dataTRU04)] <- NA

#write.csv(data, file="TRU04census_clean_date.csv")


#***********************************************************
#*******************Re-format dataframe to include dates****
#***********************************************************
# dataACJ01 
eltrcensus <- dataACJ01

# dataPAN02 has a row of NAs, remove it like this:
dataPAN02B <- dataPAN02[-580,]
eltrcensus <- dataPAN02B

# dataPAN03 
eltrcensus <- dataPAN03

# dataTRU04
eltrcensus <- dataTRU04

# column names should be
# ("plot", "sp", "tag", "dbh", "height", "density", "year", "month", "day", "plot_code")


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
  if (as.character(eltrcensus$plot[ii]) == "ACJ-01") {
    yeartmp  = 2013
    monthtmp = 01
    daytmp   = 22  
  }
  if (as.character(eltrcensus$plot[ii]) == "PAN-02") {
    yeartmp  = 2013
    monthtmp = 04
    daytmp   = 12
  }
  if (as.character(eltrcensus$plot[ii]) == "PAN-03") {
    yeartmp  = 2013
    monthtmp = 03
    daytmp   = 30
  }
  if (as.character(eltrcensus$plot[ii]) == "TRU-04") {
    yeartmp  = 2003
    monthtmp = 07
    daytmp   = 14
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
  if (as.character(eltrcensus$plot[ii]) == "ACJ-01") {
    yeartmp  = 2014
    monthtmp = 03
    daytmp   = 15
  }
  if (as.character(eltrcensus$plot[ii]) == "PAN-02") {
    yeartmp  = 2015
    monthtmp = 03
    daytmp   = 02
  }
  if (as.character(eltrcensus$plot[ii]) == "PAN-03") {
    yeartmp  = 2014
    monthtmp = 03
    daytmp   = 02
  }
  if (as.character(eltrcensus$plot[ii]) == "TRU-04") {
    yeartmp  = 2007
    monthtmp = 06
    daytmp   = 08
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
  if (as.character(eltrcensus$plot[ii]) == "ACJ-01") {
    yeartmp  = 2015
    monthtmp = 03
    daytmp   = 16 
  }
  #if (as.character(eltrcensus$plot[ii]) == "PAN-02") {
  #  yeartmp  = 2015 
  #  monthtmp = 03
  #  daytmp   = 02
  #}
  if (as.character(eltrcensus$plot[ii]) == "PAN-03") {
    yeartmp  = 2015
    monthtmp = 03
    daytmp   = 04
  }
  if (as.character(eltrcensus$plot[ii]) == "TRU-04") {
    yeartmp  = 2011
    monthtmp = 07
    daytmp   = 30
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
#eltrcen3 <- data.frame(plot_code=eltrcensus$plot, tree_tag=eltrcensus$tag, dbh=eltrcensus$DBH.3, height_m=eltrcensus$height, density=eltrcensus$density, datesDAP3) 

census <- rbind(eltrcen1, eltrcen2)#, eltrcen3)
census$height_m <- as.numeric(census$height_m)
sapply(census, class)

# get functions
setwd("/Users/cecile/GitHub/GEMcarbon.R") 
dir()
Sys.setlocale('LC_ALL','C') 
source("NPPacw_census_function_2014.R")
source("allometric_equations_2014.R")

# Chave et al. 2005 
acj_01A  <- NPPacw_census(census, plotname="ACJ-01", allometric_option="Default", height_correction_option="Default", census1_year=2013, census2_year=2014)
acj_01B  <- NPPacw_census(census, plotname="ACJ-01", allometric_option="Default", height_correction_option="Default", census1_year=2014, census2_year=2015)

pan_02A  <- NPPacw_census(census, plotname="PAN-02", allometric_option="Default", height_correction_option="Default", census1_year=2013, census2_year=2015)

pan_03A  <- NPPacw_census(census, plotname="PAN-03", allometric_option="Default", height_correction_option="Default", census1_year=2013, census2_year=2014)
pan_03B  <- NPPacw_census(census, plotname="PAN-03", allometric_option="Default", height_correction_option="Default", census1_year=2014, census2_year=2015)

tru_04A  <- NPPacw_census(census, plotname="TRU-04", allometric_option="Default", height_correction_option="Default", census1_year=2003, census2_year=2007)
tru_04B  <- NPPacw_census(census, plotname="TRU-04", allometric_option="Default", height_correction_option="Default", census1_year=2007, census2_year=2011)

# Chave et al. 2014 
# tru3_14 <- NPPacw_census(census, plotname="TRU-03", allometric_option=5, height_correction_option="Default", census1_year=2003, census2_year=2007)


#############################################################################################################################
### NPPacw small trees
#############################################################################################################################

setwd("/Users/cecile/Dropbox/GEMcarbondb/db_csv/db_csv_2015/readyforupload_db/acj_pan_2015/")

# ACJ-01

small_census <- read.table("census_smalltrees_ACJ.csv",  sep=",", header=T)
plotname = "ACJ"
allometric_option = "Default"
small_census$DAP_cm       <- (small_census$dbh_northsouth_cm + smallTree_census$dbh_westeast_cm)/2
small_census$wood_density_g_cm3 = 0.578 # This is a rough hack!! This is the average wood density in ACJ-01, but there seems to be a problem with wood density data in census of ACJ-01. CHECK!!!!


# Add wood density to these datasets
# Run "find.wsg" function . You can find it above, line approx. 86
# TO DO - it would be better to get wood density from William Farfan's census data 2015.
wsg <- read.table("wsg.txt", header=TRUE, sep=",", na.strings=c("NA", "NaN", ""), dec=".", strip.white=TRUE)

# TRU-04
smallTree_census <- read.table(".csv",  sep=",", header=T) ##### GET THIS FROM WILLIAM - Darcy & Beisit are on the case.

# PAN-02
smallTree_census <- read.table("census_ smalltrees_PAN_02.csv",  sep=",", header=T)

# PAN-03
smallTree_census <- read.table("census_smalltrees_PAN_03.csv",  sep=",", header=T)




#############################################################################################################################
### NPPacw dendrometer bands
#############################################################################################################################

setwd("/Users/cecile/Dropbox/GEMcarbondb/db_csv/db_csv_2015/readyforupload_db/acj_pan_2015")
NPPdend_ACJ01 <- read.table("Dendrometer_ACJ_2013_2014.csv", header=TRUE, sep=",", na.strings=c("NA", "NaN", ""), dec=".", strip.white=TRUE)
NPPdend_TRU04 <- read.table(".csv", header=TRUE, sep=",", na.strings=c("NA", "NaN", ""), dec=".", strip.white=TRUE)
NPPdend_PAN02 <- read.table("Dendrometer_PAN_02_2013_2014.csv", header=TRUE, sep=",", na.strings=c("NA", "NaN", ""), dec=".", strip.white=TRUE)
NPPdend_PAN03 <- read.table("Dendrometer_PAN_03_2013_2014.csv", header=TRUE, sep=",", na.strings=c("NA", "NaN", ""), dec=".", strip.white=TRUE)

# STEP 1. for each plot: run census data cleaning function above first, to get the dataframe "census". You need to do this for each plot separately.
# STEP 2. then, select the parameters below for each plot, and start running the code "NPPacw_dendro_function_2014.R". Work on one plot at a time. 
# STEP 3. at the end of "NPPacw_dendro_function_2014.R", you use the function "NPPacw_census" to get an annual value of NPPACW from census data. L 198. Make sure you have the correct parameters in that function (plot name & census years)

#ACJ-01

dendrometer <- NPPdend_ACJ01
plotname = "ACJ-01"     # TO DO: "ACJ" should be replaced by "ACJ-01" everywhere!
allometric_option = "Default"
height_correction_option = "Default"
census_year = 2013
plotit=T

# PAN-02
dendrometer <- NPPdend_PAN02
plotname = "PAN-02"
allometric_option = "Default"
height_correction_option = "Default"
census_year = 2013
plotit=T

# PAN-03
dendrometer <- NPPdend_PAN03
plotname = "PAN-03"
allometric_option = "Default"
height_correction_option = "Default"
census_year = 2013
plotit=T

# TRU-04

dendrometer <- NPPdend_TRU04
plotname = "TRU-04"
allometric_option = "Default"
height_correction_option = "Default"
census_year = XxXXXXXXX
plotit=T


#############################################################################################################################
### NPP_CWD : 
#############################################################################################################################

#############################################################################################################################
### NPProots_IC(NPProot_2015_CG) : 
#############################################################################################################################

setwd("/Users/cecile/Dropbox/GEMcarbondb/db_csv/db_csv_2015/readyforupload_db/acj_pan_2015")
NPProot_ACJ01A <- read.table("ingrowth_core_ACJ-01_2013_2104_nostock.csv", header=TRUE, sep=",", na.strings=c("NA", "NaN", ""), dec=".", strip.white=TRUE)
NPProot_TRU04A <- read.table("ingrowth_core_TRU-04_2013-2014_nostock.csv", header=TRUE, sep=",", na.strings=c("NA", "NaN", ""), dec=".", strip.white=TRUE)
NPProot_PAN02A <- read.table("ingrowth_core_PAN-02_2013-2104_nostock.csv", header=TRUE, sep=",", na.strings=c("NA", "NaN", ""), dec=".", strip.white=TRUE)
NPProot_PAN03A <- read.table("ingrowth_core_PAN-03_2013-2014_nostock.csv", header=TRUE, sep=",", na.strings=c("NA", "NaN", ""), dec=".", strip.white=TRUE)

# Adjust options
plotname = "PAN-03" # ACJ-01, TRU-04, PAN-02, PAN-03
option = 1
logtransform = T
fine_root_cor = "Default"
tubed = 0.07  ## diameter of tube
data.ic <- NPProot_PAN03A

# as_numeric
data.ic$time_step      <- as.numeric(data.ic$time_step) 
data.ic$ol_under_2mm_g <- as.numeric(data.ic$ol_under_2mm_g) 
data.ic$ol_2to3_mm_g   <- as.numeric(data.ic$ol_2to3_mm_g)
data.ic$ol_3to4_mm_g   <- as.numeric(data.ic$ol_3to4_mm_g) 
data.ic$ml_3to4_mm_g   <- as.numeric(data.ic$ml_3to4_mm_g) 
data.ic$ol_4to5_mm_g   <- as.numeric(data.ic$ol_4to5_mm_g) 
data.ic$ml_4to5_mm_g   <- as.numeric(data.ic$ml_4to5_mm_g)
data.ic$ol_above_5mm_g <- as.numeric(data.ic$ol_above_5mm_g) 
data.ic$ml_above_5mm_g <- as.numeric(data.ic$ml_above_5mm_g) 

#NPProot_ACJ01 <- NPProot_ic(data.ic, plotname, option = 1, logtransform=T, fine_root_cor="Default", tubed=0.07, ret="monthly.means.ts", plotit=F)
#NPProot_TRU04 <- NPProot_ic(data.ic, plotname, option = 1, logtransform=T, fine_root_cor="Default", tubed=0.07, ret="monthly.means.ts", plotit=F)
#NPProot_PAN02 <- NPProot_ic(data.ic, plotname, option = 1, logtransform=T, fine_root_cor="Default", tubed=0.07, ret="monthly.means.ts", plotit=F)
#NPProot_PAN03 <- NPProot_ic(data.ic, plotname, option = 1, logtransform=T, fine_root_cor="Default", tubed=0.07, ret="monthly.means.ts", plotit=F)

#############################################################################################################################
### soilrespiration() : a function to calculate soil respiration partitionning
#############################################################################################################################

setwd("/Users/cecile/Dropbox/Dan_soil_resp_paper/Rs_flux")

data.resc1 <- read.table("Rs_control_Mar2015.csv", sep=",", header=T)
data.resp1 <- read.table("Rs_partitioning_Mar2015.csv", sep=",", header=T)
data.rest1 <- read.table("Rs_total_Mar2015.csv", sep=",", header=T)

# data.resp$collar_depth_cm has a lot of NAs, I am replacing NAs by mean(data.resp$collar_depth_cm, na.rm=T)
data.resp$collar_depth_cm[is.na(data.resp$collar_depth_cm)] <- mean(data.resp$collar_depth_cm, na.rm=T)

pressure = 1013.25
plotname = "ACJ"
partitioningoption = 1
elevation = "default"
T_ambient="Default"
plotit=T

#############################################################################################################################
### stemrespiration
#############################################################################################################################

## EGM_raw_to_flux_stem_2015
setwd("/Users/cecile/Dropbox/GEMcarbondb/db_csv/db_csv_2015/readyforupload_db/acj_pan_2015/")

# ACJ-01
Rstem_flux <- read.table("Rstem_flux_ACJ_2013_2014.csv", header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE)
Rstem_temp <- read.table("Rstem_temp_ACJ_2013.csv", header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE)

# TRU-04
Rstem_flux <- read.table("Rstem_flux_TU4_2013.csv", header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE)
Rstem_temp <- read.table("Rstem_temp_TU4_2013.csv", header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE)

# PAN-02
Rstem_flux <- read.table("Rstem_flux_PAN02_2013_2014.csv", header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE)
Rstem_temp <- read.table("Rstem_temp_PAN02_2013.csv", header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE)

# PAN-03
Rstem_flux <- read.table("Rstem_flux_PAN03_2013_2014.csv", header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE)
Rstem_temp <- read.table("Rstem_temp_PAN03_2013.csv", header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE)

## stem_respiration_2015


setwd("/Users/cecile/Dropbox/GEMcarbondb/db_csv/db_csv_2015/readyforupload_db/acj_pan_2015/")

# ACJ-01
smallTree_census <- read.table("census_smalltrees_ACJ.csv",  sep=",", header=T)
largeTree_census <- read.table("census_ACJ_01.csv", header=T, sep=",")
Rstem            <- read.table("Rstem_ACJ_co2slope_wttreenum.csv",  sep=",", header=T)
Rstem$dco2       <- Rstem$co2slope
plotname         <- "ACJ"
avg_tree_height  <- mean((largeTree_census$Height..2013.1+(largeTree_census$Height..2015.1/1000)), na.rm=T) 
plot_it=T

#re-name columns
largeTree_census$plot_code <- as.character("ACJ") #as.character(largeTree_census$plot_code)
smallTree_census$plot_code <- as.character(smallTree_census$plot_code)
Rstem$plot_code            <- as.character("ACJ")
largeTree_census$DAP_cm_start <- (largeTree_census$D..2013.1)/10 # ATTENTION!!! we are only /10 here because the data was entered in mm, not cm for this plot.
smallTree_census$DAP_cm       <- (smallTree_census$dbh_northsouth_cm + smallTree_census$dbh_westeast_cm)/2


# TRU-04
smallTree_census <- read.table(".csv",  sep=",", header=T) ##### GET THIS FROM WILLIAM - Darcy & Beisit are on the case.

setwd("/Users/cecile/Dropbox/GEMcarbondb/db_csv/db_csv_2015/readyforupload_db")
largeTree_census <- read.table("andesplots_WFR_nov2014_noroots.csv", header=TRUE, sep=",", na.strings=c("NA", "NaN", ""), dec=".", strip.white=TRUE)

setwd("/Users/cecile/Dropbox/GEMcarbondb/db_csv/db_csv_2015/readyforupload_db/acj_pan/")
Rstem            <- read.table("Rstem_TU4_co2slope_wttreenum.csv",  sep=",", header=T)
Rstem$dco2       <- Rstem$co2slope
plotname         <- "TRU-04"
avg_tree_height  <- mean((largeTree_census$Height..2013.1+(largeTree_census$Height..2015.1/1000)), na.rm=T)
plot_it=T

#re-name columns
largeTree_census$plot_code <- as.character("TRU-04") 
smallTree_census$plot_code <- as.character(smallTree_census$plot_code)
Rstem$plot_code            <- as.character("TRU-04")
largeTree_census$DAP_cm_start <- (largeTree_census$XXXXXXXXXXXXX) # ATTENTION!!! /10 here because the data was entered in mm, not cm for this plot?? check
smallTree_census$DAP_cm       <- (smallTree_census$dbh_northsouth_cm + smallTree_census$dbh_westeast_cm)/2

# PAN-02
smallTree_census <- read.table("census_ smalltrees_PAN_02.csv",  sep=",", header=T)
largeTree_census <- read.table("census_PAN_02.csv", header=T, sep=",")
Rstem            <- read.table("Rstem_PAN02_co2slope_wttreenum.csv",  sep=",", header=T)
Rstem$dco2       <- Rstem$co2slope
plotname         <- "PAN-02"
avg_tree_height  <- mean((largeTree_census$Height..2013.2+(largeTree_census$Height..2015.1/1000)), na.rm=T) 
plot_it=T

#re-name columns
largeTree_census$plot_code    <- as.character("PAN-02") 
smallTree_census$plot_code    <- as.character(smallTree_census$plot_code)
Rstem$plot_code               <- as.character("PAN-02")
largeTree_census$DAP_cm_start <- (largeTree_census$D..2013.2)/10 # ATTENTION!!! check data was entered in cm, not mm.
smallTree_census$DAP_cm       <- (smallTree_census$dbh_northsouth_cm + smallTree_census$dbh_westeast_cm)/2

# PAN-03
smallTree_census <- read.table("census_smalltrees_PAN_03.csv",  sep=",", header=T)
largeTree_census <- read.table("census_PAN_03.csv", header=T, sep=",")
Rstem            <- read.table("Rstem_PAN03_co2slope_wttreenum.csv",  sep=",", header=T) # NEED TREE NUMBERS FROM BEISIT & DARCY!!!
Rstem$dco2       <- Rstem$co2slope
plotname         <- "PAN-03"
avg_tree_height  <- mean((largeTree_census$Height..2013.2+(largeTree_census$Height..2015.2/1000)), na.rm=T)
plot_it=T

#re-name columns
largeTree_census$plot_code    <- as.character("PAN-03") 
smallTree_census$plot_code    <- as.character("PAN-03") 
Rstem$plot_code               <- as.character("PAN-03")
largeTree_census$DAP_cm_start <- (largeTree_census$D..2013.2)/10 # ATTENTION!!! check data was entered in cm, not mm.
smallTree_census$DAP_cm       <- (smallTree_census$dbh_northsouth_cm + smallTree_census$dbh_westeast_cm)/2



#############################################################################################################################
### leafrespiration
#############################################################################################################################


