# simple gap filling method to create climate input files for TFS
# nfyllas

library(lubridate)
library(ggplot2)
library(gridExtra)
library(dplyr)

# Use daily averages from Dom's work, or Nikos work.
setwd("~/Github/gemcarbon_data/raw_data_ingembd")
#data=read.csv("ANK_climate.csv",header=T)
data=read.csv("BOB_climate.csv",header=T)
#data=read.csv("KOG_climate.csv",header=T)

# create date object
data$Date=gsub("/","-",data$Date)
data$Date=dmy(data$Date)
str(data)

data$Doy  =yday(data$Date)
data$Month=month(data$Date)
data$Year=year(data$Date)

# Per Month per Year average. This is piping in dplyr.
clim.year.month=data.frame(
      data%>%
	  group_by(Year,Month)%>%
	  summarise(Rad_sd  = sd(Light,na.rm=T)        ,Rad.mday = mean(Light,na.rm=T),
	            Rain_sd = sd(Rain.gauge,na.rm=T)   ,Rain.tm= sum(Rain.gauge,na.rm=T),
				countNA = sum(is.na(Rain.gauge))   ,days=n(), 
	            Tavg_sd = sd(Temp,na.rm=T)         ,Tavg.mday= mean(Temp,na.rm=T),
				RH_sd   = sd(Rel.Humidity,na.rm=T) ,RH.mday  = mean(Rel.Humidity,na.rm=T)))

clim.year.month$Rain.tm2=clim.year.month$Rain.tm+clim.year.month$countNA/clim.year.month$days*clim.year.month$Rain.tm
clim.year.month$Rain.mday=clim.year.month$Rain.tm2/clim.year.month$days
clim.year.month


# Per Year
clim.year=data.frame(
	clim.year.month%>%
	group_by(Year)%>%
	summarise(Rad=sum(Rad.mday),Rain=sum(Rain.tm2),Tavg=mean(Tavg.mday),RH=mean(RH.mday))
)
clim.year


data=merge(data,clim.year.month,by=c("Month","Year"))
head(data)

### See which years have the most complete records
summarise(group_by(data,Year),sum(!is.na(Light)),sum(!is.na(Rain.gauge)),sum(!is.na(Temp)),sum(!is.na(Rel.Humidity)) )


### Gap Fill with current monthl mean values
data$Rad  = ifelse(!is.na(data$Light)       ,data$Light        ,data$Rad.mday)
data$Tavg = ifelse(!is.na(data$Temp)        ,data$Temp         ,data$Tavg.mday)
data$Rain = ifelse(!is.na(data$Rain.gauge)  ,data$Rain.gauge   ,data$Rain.mday)
data$RH   = ifelse(!is.na(data$Rel.Humidity),data$Rel.Humidity ,data$RH.mday)
head(data,30)

plot(Rad~Doy,data,col=factor(Year))


# If you comment out "clim.daily=filter(clim.daily,Year==2015)", you get same output for all the years.
# Select which year is best (based on the table you had above, with countNA)
clim.daily=select(data,c(Year,Doy,Month,Rad,Rad_sd,Tavg,Tavg_sd,Rain,Rain_sd,RH,RH_sd))
clim.daily=filter(clim.daily,Year==2015)
clim.daily


# Per Month for the best year only 
clim.month=data.frame(
      data%>%
	  group_by(Month)%>%
	  summarise(Rad_sd.m  = sd(Light,na.rm=T)         ,Rad.m = mean(Light,na.rm=T),
	            Rain_sd.m = sd(Rain.gauge,na.rm=T)    ,Rain.m= mean(Rain.gauge,na.rm=T),
	            Tavg_sd.m = sd(Temp,na.rm=T)          ,Tavg.m= mean(Temp,na.rm=T),
				RH_sd.m   = sd(Rel.Humidity,na.rm=T)  ,RH.m  = mean(Rel.Humidity,na.rm=T)))

clim.month



### Gap Fill with long term monthly mean values. If you are still missing values for a month, take the multi-year mean for that month. 
clim.daily=merge(clim.daily,clim.month,by=c("Month"))
head(clim.daily)


clim.daily$Rad  = ifelse(!is.na(clim.daily$Rad) ,clim.daily$Rad ,clim.daily$Rad.m)
clim.daily$Tavg = ifelse(!is.na(clim.daily$Tavg),clim.daily$Tavg,clim.daily$Tavg.m)
clim.daily$Rain = ifelse(!is.na(clim.daily$Rain),clim.daily$Rain,clim.daily$Rain.m)
clim.daily$RH   = ifelse(!is.na(clim.daily$RH)  ,clim.daily$RH  ,clim.daily$RH.m)
clim.daily$Rad_sd  = ifelse(!is.na(clim.daily$Rad_sd) ,clim.daily$Rad_sd ,clim.daily$Rad_sd.m)
clim.daily$Tavg_sd = ifelse(!is.na(clim.daily$Tavg_sd),clim.daily$Tavg_sd,clim.daily$Tavg_sd.m)
clim.daily$Rain_sd = ifelse(!is.na(clim.daily$Rain_sd),clim.daily$Rain_sd,clim.daily$Rain_sd.m)
clim.daily$RH_sd   = ifelse(!is.na(clim.daily$RH_sd)  ,clim.daily$RH_sd  ,clim.daily$RH_sd.m)
head(clim.daily)

clim.daily=select(clim.daily,c(Year,Doy,Month,Rad,Rad_sd,Tavg,Tavg_sd,Rain,Rain_sd,RH,RH_sd))

Rad.pl=ggplot(data=clim.daily)+geom_point(aes(x=Doy,y=Rad))+stat_smooth(aes(x=Doy,y=Rad))+theme_bw()
Temp.pl=ggplot(data=clim.daily)+geom_point(aes(x=Doy,y=Tavg))+stat_smooth(aes(x=Doy,y=Tavg))+theme_bw()
Rain.pl=ggplot(data=clim.daily)+geom_point(aes(x=Doy,y=Rain))+stat_smooth(aes(x=Doy,y=Rain))+theme_bw()
RH.pl=ggplot(data=clim.daily)+geom_point(aes(x=Doy,y=RH))+stat_smooth(aes(x=Doy,y=RH))+theme_bw()

grid.arrange(Rad.pl,Temp.pl,Rain.pl,RH.pl,ncol=2)



write.csv(clim.daily,"KOG_daily_climate.csv",row.names=FALSE)

