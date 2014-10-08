## Data cleaning for census data
## Cécile Girardin 03.07.2014.

# load packages
require(ggplot2)

# get data
setwd("C:/Users/Cecile/Dropbox/GEMcarbondb/db_csv/db_csv_eltr/forestplots_census")
data  <- read.table("eltr_census_2011.csv", header=TRUE, sep=",", na.strings=c("NA", "NaN", ""), dec=".", strip.white=TRUE)

# choose a plot
data <- subset(data, plot=="WAY-01")

# rename columnd 
data$DBH.1 <- data$DAP_cm_start
#colnames(data) <- c("plot", "subplot", "tag", "DBH.1", "DBH.2", "height", "density")

# define start and end date:
date_1 <- as.character("2003/9/24") # Change date
date_2 <- as.character("2007/7/6")
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
  geom_point()
plot1               # use this plot to decide your cut off point for annual growth in cm and replace below.


# Print the problematic rows.
aa <- subset(data, DBH.1 > DBH.2 | DBH.2 > DBH.3, select = c(plot, tag, DBH.1, DBH.2, DBH.3))
aa <- head(aa, 50)

op <- par(ask=TRUE) # http://stackoverflow.com/questions/6031093/making-a-series-of-plots-that-proceed-by-a-click
for (i in 1:length(aa$plot)){
  temp_aa <- subset(aa, plot == plot[i])
  
  for (j in 1:length(temp_aa$tag)){
      aaa <- subset(temp_aa, tag == tag[j], select = c(DBH.1, DBH.2, DBH.3))
      colnames(aaa) <- c("1", "2", "3") 
      aaa <- t(aaa)
      aaa <- data.frame(census=as.numeric(rownames(aaa)), dbh=as.numeric(aaa[,1]), tag = temp_aa$tag[j])
      #ggplot(aaa, aes(x=census, y=dbh)) + geom_point(size=3)
      plot(aaa$census, aaa$dbh, main = paste("tree tag:", head(aaa$tag, 1)), xlab="census number", ylab="dbh (cm)") 
  }
  par(op)
}

# should we correct for changing POM?

# do not allow recruits 
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
maxincrement_1 <- 2.5 * census_interval_yrs_1 #(3*gr_sd)
maxincrement_2 <- 2.5 * census_interval_yrs_2 #(3*gr_sd)

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
# Get rid of duplicate trees
# data[data$tag!=210.2 & data$height!=3.77, ]

data[is.na(data)] <- NA


#save clean data files. This file will be saved in the directory you specified above.
#write.csv(ton01, file="tono_2007_clean_16July.csv") 
#write.csv(eltrcensus, file="eltr_census_clean_16July14.csv") 
#eltrcensus   <- read.table("eltr_census_clean_16July14.csv", header=TRUE, sep=",", na.strings=c("NA", "NaN", ""), dec=".", strip.white=TRUE)


# Don't worry about this...
# Flag fast growing trees - NOT WORKING YET 
data$DBH.3 <- NA
aa <- subset(data, ((data$DBH.2 - data$DBH.1) >= maxincrement | (data$DBH.3 - data$DBH.2) >= maxincrement), select = c(plot, tag, DBH.1, DBH.2, DBH.3))

op <- par(ask=TRUE) # http://stackoverflow.com/questions/6031093/making-a-series-of-plots-that-proceed-by-a-click
for (i in 1:length(aa$plot)){
  temp_aa <- subset(aa, plot == plot[i])
  
  for (j in 1:length(temp_aa$tag)){
    aaa <- subset(temp_aa, tag == tag[j], select = c(DBH.1, DBH.2, DBH.3))
    colnames(aaa) <- c("1", "2", "3") 
    aaa <- t(aaa)
    aaa <- data.frame(census=as.numeric(rownames(aaa)), dbh=as.numeric(aaa[,1]), tag = temp_aa$tag[j])
    plot(aaa$census, aaa$dbh, main = paste("tree tag:", head(aaa$tag, 1)), xlab="census number", ylab="dbh (cm)") 
  }
  par(op)
}