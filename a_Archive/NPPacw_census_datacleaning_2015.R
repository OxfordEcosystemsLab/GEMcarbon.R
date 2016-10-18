# Cecile Girardin 23/01/2015
# This code cleans census data downloaded as advanced search / individuals/ preffered plot view from forestplot.org
# These census are used to estimate biomass and NPP of large trees (> 10 cm).

# get data files
# !!!! CHECK WT YM: check tree tags in the original file - replace all "no placa", "???", and uncertain tags by NA. OR I am just using the TreeID from now on.
setwd("/Users/cecile/Dropbox/GEMcarbondb/db_csv/db_csv_2015/readyforupload_db")
census <- read.table("andesplots_WFR_nov2014_noroots.csv", header=TRUE, sep=",", na.strings=c("NA", "NaN", ""), dec=".", strip.white=TRUE)
# Tip: if the file doesn't load, try opening it and saving it again. It may have some formatting issues, but once you re-save it, they are fixed by csv.

# load packages
require(ggplot2)
require(sqldf)
require(lubridate)

# re-format
census$tree_tag <- as.character(census$Tag.Number)
census$date     <- as.Date(date_decimal(census$Census.Mean.Date))
census$day      <- format(census$date, "%d")
census$month    <- format(census$date, "%m")
census$year     <- format(census$date, "%Y")
# Average of four DBH measurements / 10. What is the unit of dbh in ALP?
census$DBH <- (mean(census$DBH1, census$DBH2, census$DBH3, census$DBH4)) / 10 ########################################### JE SUIS LA!!!!!!  AVERAGE OVER SEVERAL MESUREMENTS RATHER THAN TAKE THE 1st.

# choose a plot
census <- subset(census, plot == "ALP-01")

# subset into different census
census1 <- subset(census, census$year == 2006)
census2 <- subset(census, census$year == 2008)
census3 <- subset(census, census$year == 2010)

data1 <- sqldf("SELECT census1.*, census2.DBH1 as DBHtwo, census2.day as day2, census2.month as month2, census2.year as year2 FROM census2 JOIN census1 ON census1.TreeID = census2.TreeID")
data  <- sqldf("SELECT data1.*, census3.DBH1 as DBHthree, census3.day as day3, census3.month as month3, census3.year as year3 FROM census3 JOIN data1 ON data1.TreeID = census3.TreeID")
data$DBHone <- data$DBH1/10
data$DBHtwo <- data$DBHtwo/10
data$DBHthree <- data$DBHthree/10

############### ??? ###########################
# Would a loop be better here?
# data <- sqldf("SELECT * FROM census ORDER BY tree_tag, year, month ASC")

# for (treenum in unique(data$tree_tag)) {    
#  thistree <- subset(data, tree_tag == treenum)
#    if (rownum > 1) {
#      this_dbh <- thistree$dbh[rownum]
#      previous_dbh <- thistree$dbh[rownum-1]
#    }
#  data$dbhincrement[rownum] <- previeous_dbh - this_dbh 
#}
#############################################

# define start and end date:
date_1 <- as.character(paste(data$day, data$month, data$year, sep= "/")) 
date_2 <- as.character(paste(data$day2, data$month2, data$year2, sep= "/")) 
date_3 <- as.character(paste(data$day3, data$month3, data$year3, sep= "/")) 
date_1 <- as.Date(format(strptime(date_1, format="%d/%m/%Y")))
date_2 <- as.Date(format(strptime(date_2, format="%d/%m/%Y")))
date_3 <- as.Date(format(strptime(date_3, format="%d/%m/%Y")))
census_interval_1     <- as.numeric(difftime(date_2, date_1, units="days"))
census_interval_yrs_1 <- census_interval_1/365
census_interval_2     <- as.numeric(difftime(date_3, date_2, units="days"))
census_interval_yrs_2 <- census_interval_2/365


# Visualise data: normal distribution of dbhgrowth per year by diameter class
data$dbh_growth_yr <- (data$DBHtwo - data$DBHone)/census_interval_yrs_1  
gr_sd <- sd(data$dbh_growth_yr, na.rm=T)
plot1 <- ggplot(data=data, aes(x=DBHone, y=dbh_growth_yr, na.rm=T)) +
               geom_point() +
               ylim(-10, 10) +
               ggtitle(data$plot)
plot1               # use this plot to decide your cut off point for annual growth in cm and replace below.


# should we correct for changing POM?

# do not allow recruits - QUESTION: should we have a max size for recruits, beyond which they are assumed to be missing data?
w = which(is.na(data$DBHone) & !is.na(data$DBHtwo))
data$DBHtwo[w] = 0/0 
data$recruits <- "ok"
data$recruits[w] <- "recruit"

w = which(is.na(data$DBHone) & is.na(data$DBHtwo) & !is.na(data$DBHthree)) 
data$DBHthree[w] = 0/0    
data$recruits[w] <- "recruit"

# Do not allow for shrinking trees. QUESTION: Allow shrinkage of 10%? - ask Gaby.
w = which(data$DBHone > data$DBHtwo) #+ (data$DBHone*0.01) 
data$DBHtwo[w] = data$DBHone[w]  
data$srink <- "ok"
data$srink[w] <- "shrunk.dbh.2"

w = which(data$DBHtwo > data$DBHthree)   
data$DBHthree[w] = data$DBHtwo[w]        
data$srink[w] <- "shrunk.dbh.3"

# replace missing values in the middle of the census
w = which(!is.na(data$DBHone) & is.na(data$DBHtwo) & !is.na(data$DBHthree)) 
data$DBHtwo[w] = (data$DBHone[w] + data$DBHthree[w])/2        # mean rather than meadian flag this as 8 - see RAINFOR flags.
data$missing <- "ok"
data$missing[w] <- "missing"

# Define the maximum interval you would like to allow per year (e.g. 5 cm), or use the default 3SD.
maxincrement_1 <- 5 * unique(census_interval_yrs_1) #(3*gr_sd)
maxincrement_2 <- 5 * unique(census_interval_yrs_2) #(3*gr_sd)

w = which((data$DBHtwo - data$DBHone) >= maxincrement_1)      
data$overgrown <- "ok"
data$overgrown[w] <- ">2cm growth per yr dbh.2"
data$value_replaced <- NA
data$value_replaced[w] <- data$DBHtwo[w]
data$DBHtwo[w] = (data$DBHone[w] + maxincrement_1)    

w = which((data$DBHthree - data$DBHtwo) >= maxincrement_2)     
data$overgrown[w] <- ">2cm growth per yr dbh.3"
data$value_replaced[w] <- data$DBHthree[w]
data$DBHthree[w] = (data$DBHtwo[w] + maxincrement_2) 

# Flag duplicate trees
n_occur <- data.frame(table(data$tag))
n_occur[n_occur$Freq > 1,]
data[data$tag %in% n_occur$Var1[n_occur$Freq > 1],]

## STOP HERE ##
# Would you like to delete duplicate trees manually?
# data <- data[data$tag!=570.2 & data$X2009!=13.0, ]

# Standardise NAs
data[is.na(data)] <- NA

# Re-format to date / DBH instead of DBHone, DBHtwo, and DBHthree.
# sqldf() does not accept ".", so we need to replace by "_"
data$Plot_code <- data$Plot.Code
data$Plot_Name <- data$Plot.Name
data$Census_Mean_Date <- data$Census.Mean.Date
data$Census_No <- data$Census.No
data$Tag_Number <- data$Tag.Number

# Attention! We are using TreeID as tree_tag. This avoids missing tags problems. Although we join the two tables census and density height on Tag_Number!
census1 <- sqldf("SELECT PlotID, Plot_Code as plot_code, Plot_Name, Country, PI, Census_Mean_Date, Census_No, TreeID as tree_tag, Tag_Number, DBHone as dbh, POM, F1, F2, F3, F4, date, day, month, year, recruits, srink, missing, overgrown, value_replaced FROM data ORDER BY date")
census2 <- sqldf("SELECT PlotID, Plot_Code as plot_code, Plot_Name, Country, PI, Census_Mean_Date, Census_No, TreeID as tree_tag, Tag_Number, DBHtwo as dbh, POM, F1, F2, F3, F4, date, day2 as day, month2 as month, year2 as year, recruits, srink, missing, overgrown, value_replaced FROM data ORDER BY date")
census3 <- sqldf("SELECT PlotID, Plot_Code as plot_code, Plot_Name, Country, PI, Census_Mean_Date, Census_No, TreeID as tree_tag, Tag_Number, DBHthree as dbh, POM, F1, F2, F3, F4, date, day3 as day, month3 as month, year3 as year, recruits, srink, missing, overgrown, value_replaced FROM data ORDER BY date")
census  <- rbind(census1, census2, census3)

# Add height_m and density from data Chis used in PED papers
setwd("/Users/cecile/Dropbox/Andes synthesis 2013/NPPacw")
densityheight1      <- read.table("CensusIq.csv", header=TRUE, sep=",", na.strings=c("NA", "NaN", ""), dec=".", strip.white=TRUE)
densityheight       <- subset(densityheight1, plot=="ALP-11") 
censusx             <- sqldf("SELECT census.*, densityheight.height, densityheight.density FROM census JOIN densityheight ON census.Tag_Number = densityheight.tag")
censusx$height_m    <- censusx$height


#write.csv(census, file="census_clean_etc.csv")

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
alp11_1 <- NPPacw_census(censusx, plotname="ALP-01", allometric_option="Default", height_correction_option="Default", census1_year=2006, census2_year=2008)
alp30_1 <- NPPacw_census(census, plotname="ALP-30", allometric_option="Default", height_correction_option=2, census1_year=2008, census2_year=2010)

# Chave 2014 
tru3_14 <- NPPacw_census(census, plotname="TRU-03", allometric_option=5, height_correction_option="Default", census1_year=2003, census2_year=2007)

