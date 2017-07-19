## Data cleaning for dendrometer data
## Cecile Girardin & Daniela Requena 10.10.2016


# load packages
require(ggplot2)
library(sqldf)

# get data
setwd("/Users/cecile/Dropbox/GEMcarbondb/db_csv/db_csv_2015/")
dataA  <- read.table("LPG01.csv", header=TRUE, sep=",", na.strings=c("NA", "NaN", ""), dec=".", strip.white=TRUE)

mode(dataA)
class(dataA$plot_code)
dataA$plot_code <- as.character(dataA$plot_code)
dataA$comments <- as.character(dataA$comments)
str(dataA)

# choose a plot
dataA <- subset(dataA, plot_code == "LPG01")

# Only keep rows with measurements, i.e. remove rows where measurement is NA:
dataA <- subset(dataA, !is.na(dendrometer_reading_mm))

# Add a date column
dataA$date <- as.Date(paste(dataA$year, dataA$month, dataA$day, sep="."), format="%Y.%m.%d") 

# Order by tree_tag and date 
dataA <- dataA[order(dataA$tree_tag, dataA$date),]

# Replace NA,NaN, blank with NA only
dataA[is.na(dataA)] <- NA

# codes to identify rows: check!! Are tree_tags unique?
dataA$codeb   <- paste(dataA$sub_plot, dataA$tree_tag, sep=".")  
dataA$codew   <- paste(dataA$sub_plot, dataA$tree_tag, dataA$year, dataA$month, dataA$day, sep=".") 

# Flag duplicate codew
n_occur <- data.frame(table(dataA$codew))
n_occur[n_occur$Freq > 1,]
dataA[dataA$codew %in% n_occur$Var1[n_occur$Freq > 1],]
# Get rid of duplicate codew
# data[data$tag!=210.2 & data$height!=3.77, ]

# identify dendrometers that have been replaced.
dendrometer_reading_replaced_mm

# get unique identifyer for each measurement
uid   <- unique(dataA$codeb)
uidii <- unique(dataA$codew)
xx    <- c()  
yy    <- c()
zz    <- c()
aa    <- c()
bb    <- c()

for (i in 1:length(dataA$codew)) { 
  sub       <- subset(dataA, subset=(dataA$codeb == uid[i]))
  if(length(sub$codeb) > 1) {
    meas_int  <- difftime(sub$date[1:(length(sub$date)-1)], sub$date[2:length(sub$date)], units="days")
    meas_int_num  <- as.numeric(meas_int)
    meas_diff <- diff(sub$dendrometer_reading_mm, lag=1)
    meas      <- meas_diff/(-meas_int_num)   
    id        <- tail(sub$codew,-1)
    xx        <- c(xx, id)
    yy        <- c(yy, meas_int)
    aa        <- c(aa, meas_diff)
    zz        <- c(zz, meas)
    #print(i)
    #print(length(yy))
    #print(length(xx))
  } else {   
    #print(paste("row number:", i))
    #print(paste("tree number:", sub$tree_tag))
    #print(paste("subset length:", length(sub$codeb)))
  }
}
data2 <- data.frame(cbind(xx,yy,aa,zz))
colnames(data2) <- c("codew", "measurement_interval_days", "measurement_difference", "measurement_mmperday")

# change data2$measurement_mmperday from a factor to a number:
data2$measurement_mmperday <- as.numeric(as.character(data2$measurement_mmperday))

data3 <- sqldf("SELECT data.*, data2.* FROM data2 JOIN data ON data2.codew = data.codew") 

# Convert from mm per day to mm per month:
data3$measurement_mmpermonth <- (data3$measurement_mmperday * 30.44) # average number of days in a month

# Identify and label outlyers: the trees which present individual increases out of the established thresholds (-1 mm > x > 10 mm / mo)
w                 <- which((-1 >= data3$measurement_mmpermonth | data3$measurement_mmpermonth >= 10)) # Change threshold as needed. 5, 10, -2, -3.
data3$outlyers    <- "ok"
data3$outlyers[w] <- "no"
bb                <- subset(data3, data3$outlyers == "no")
head(bb)
length(bb$tree_tag)

setwd("/Users/cecile/Dropbox/GEMcarbondb/db_csv/db_csv_2015/")
write.csv(data3, file="LPG01_daniela_2016.csv") 

# Visualise data: normal distribution of dbhgrowth per year by diameter class

plot1 <- ggplot(data=data3, aes(x=tree_tag, y=measurement_mmpermonth, na.rm=T))
  + geom_point()
  + geom_text(aes(label=tree_tag),hjust=0, vjust=0)

plot1               # use this plot to decide your cut off point for annual growth.

# Plot growth over time for each tree in bb
bb$tree_tag
cc <- subset(bb, tree_tag == 399)
plot2 <- ggplot(data=cc, aes(x=date, y=measurement_mmpermonth, na.rm=T)) +
  geom_point()
plot2               # use this plot to visualise individual tree growth.

