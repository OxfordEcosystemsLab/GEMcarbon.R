#######################################
########## NPPdendrometers ############
#######################################


# Priorities
# "ANK" "BOB" "CAX" "KEN" "KOG" "LPG" "NXV" "SAF" "STM" "TAM"

# done
# "BOB-02" "BOB-01" "BOB-03"  - No height data at all for BOB, assumed 30m
# "TAM-05" "TAM-06" 



# problems
# BOB-04 BOB-05 BOB-06 TAM-09 LPG-01- I get so many negative values that the NPP / ha / yr is negative.
# KEN & KOG- tree tags in dendrometer data do not coincide with tree tags in census from forest plots? 
# 

setwd("/Users/cecile/GitHub/GEMcarbon.R/") 
source("~/Github/GEMcarbon.R/allometric_equations_2014.R")
source("~/Github/GEMcarbon.R/NPPacw_census_function_2016.R")
source("~/Github/GEMcarbon.R/NPPacw_dendro_function_2016.r")

require(plyr) 
require(lubridate)

setwd("~/Github/gemcarbon_data/raw_data_ingemdb/stem_npp")
NPPdend_all <- read.table("dendro_Sep20.csv", header=TRUE, sep=",", na.strings=c("NA", "NaN", ""), dec=".", strip.white=TRUE)
census_all  <- read.table("forestplots_download_oct17.csv", header=TRUE, sep=",", na.strings=c("NA", "NaN", ""), dec=".", strip.white=TRUE) # formattedcensus_TAM05_Mar17.csv
str(census_all) 
census_all$plot_code = as.character(census_all$plot_code)
census_all$F1        = as.character(census_all$F1)

input = census_all$census_date
year  = floor(input)
doy   = round(365*(input-year))
date  = lubridate::parse_date_time(paste(year,doy),"yj") 
census_all$date = date
census_all <- mutate(census_all, date = census_all$date, day = day(date), month = month(date), year = year(date))

# ATTENTION: I am getting rid of dead trees here until I figure out what the problem is. 
# I will add a dead tree filter to the code once other problems are sorted.
census     <- census_all[ which(census_all$plot_code=='SAF-01' & census_all$F1 != 0), ]

census$density  = 0.646775 # code to estimate WD is below. I am just using this average whilst de bugging.
#census$height_m = 30
census$dbh      = (census$dbh/10)

#  estimate wsg (wood density)
#wsg    <- read.table("wsg.csv", header=TRUE, sep=",", na.strings=c("NA", "NaN", ""), dec=".", strip.white=TRUE) 
#census_BOB02$wsg     = find.wsg(census_BOB02$Family, census_BOB02$Genus, census_BOB02$Species, wsg)
#wsg_plot             = tapply(census_BOB02$wsg, census_BOB02$plot_code, mean, na.rm=T)
#w                    = which(is.na(census_BOB02$wsg))
#m                    = match(census_BOB02$plot_code[w], names(wsg_plot))
#census_BOB02$wsg[w]  = wsg_plot[m]
#w                    = which(is.na(census_BOB02$wsg))
#census_BOB02$wsg[w]  = median(unique(census_BOB02$wsg), na.rm=T)
#census_BOB02$density = census_BOB02$wsg

# re-name columns
# NPPdend_all$dendrometer_reading_mm = NPPdend_all$dbh_nodirection_mm

# input data for NPPacw_dendro function
#dendrometer              = subset(NPPdend_all, NPPdend_all$year >= 2012)  # %>% filter(year<=2012)
dendrometer              = NPPdend_all[ which(NPPdend_all$year >= 2012), ]
dendrometer$plot_code    = as.character(dendrometer$plot_code)
plotname                 = "SAF-01"     
allometric_option        = "Default"
height_correction_option = "Default"
census_year              = 2012

# dend_bob01 <- NPPacw_dendro(census, dendrometer, plotname = "TAM-09", allometric_option="Default", height_correction_option="Default", ret="nppacw.permonth.perha", census_year = 2005)

# save dendrometer data in MgC / tree / day
setwd("~/Github/gemcarbon_data/processed_ts_inELDS/")
write.csv(monthlynppacw, file="dend_tam06_2Oct17.csv")
