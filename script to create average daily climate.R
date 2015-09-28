# Script written by Nikos Fyllas, August 2015. contact: nfyllas@gmail.com
 
library(chron)
library(ggplot2)
library(gridExtra)
library(smatr)
rm(list=ls(all=TRUE))

#data=read.csv("ACJ-01_clean.csv",header=T)
#data=read.csv("PAN-02.csv",header=T) ### SOS ### not a full year's record
#data=read.csv("PAN-03.csv",header=T) ### SOS ### not a full year's record
#data=read.csv("SPD-alternative_clean.csv",header=T)
#data=read.csv("TON-01_clean.csv",header=T)
#data=read.csv("TRU-04_clean.csv",header=T)
#data=read.csv("TRU-2750.csv",header=T)
head(data)

data$date = chron(paste(data$Day,data$Month,data$Year,sep="-"),format="d-m-y")
data$doy  = as.numeric(format(as.Date(data$date), format = "%j"))

doy  = sort(unique(data$doy))
SW   = as.numeric(by(data$Light,data$doy,mean))
Temp = as.numeric(by(data$Temperature,data$doy,mean))

Tmax = as.numeric(by(data$Temperature,data$doy,max))
Tmin = as.numeric(by(data$Temperature,data$doy,min))

Rain = 48*as.numeric(by(data$Rainfall,data$doy,mean)) # x 48 when 30min timestep
RH   = as.numeric(by(data$RH,data$doy,mean))
Pres = as.numeric(by(data$Pressure,data$doy,mean)) 


#Simple gap filling
summary(SW)
SW[which(SW<0)]<-NA
lm.sw=lm(SW~Temp);summary(lm.sw)
plot(SW~Temp);abline(lm.sw)
SW=ifelse(!is.na(SW),SW,coef(lm.sw)[1]+coef(lm.sw)[2]*Temp[which(is.na(SW))])

summary(Temp)
lm.temp=lm(Temp~SW);summary(lm.temp)
plot(Temp~SW);abline(lm.temp)
Temp=ifelse(!is.na(Temp),Temp,coef(lm.temp)[1]+coef(lm.temp)[2]*SW[which(is.na(Temp))])

summary(Tmax)
lm.temp=lm(Tmax~Temp);summary(lm.temp)
plot(Tmax~Temp);abline(lm.temp)
Tmax=ifelse(!is.na(Tmax),Tmax,coef(lm.temp)[1]+coef(lm.temp)[2]*Temp[which(is.na(Tmax))])

summary(Tmin)
lm.temp=lm(Tmin~Temp);summary(lm.temp)
plot(Tmin~Temp);abline(lm.temp)
Tmin=ifelse(!is.na(Tmin),Tmin,coef(lm.temp)[1]+coef(lm.temp)[2]*Temp[which(is.na(Tmin))])

summary(Rain)
Rain[which(Rain<0)]<-NA
Rain[is.na(Rain)]<-0

summary(RH)
RH[which(RH<60)]<-NA
#lm.RH=lm(RH~Temp+SW);summary(lm.RH)
#plot(RH~Temp)
#RH=ifelse(!is.na(RH),RH,coef(lm.RH)[1] + coef(lm.RH)[2]*Temp[which(is.na(RH))] + coef(lm.RH)[3]*SW[which(is.na(RH))])

dew=0.38*Tmax-0.018*Tmax*Tmax+1.4*Tmin-5.0; ##Linacre 1992
RHalt = 100.0 - 5.0*(Temp - dew)
plot(RH~RHalt)
lm.RH=lm(RH~RHalt);summary(lm.RH)
RH=ifelse(!is.na(RH),RH,coef(lm.RH)[1] + coef(lm.RH)[2]*RHalt[which(is.na(RH))] )
RH=ifelse(RH>100,100,RH)
summary(RH)


#dat=data.frame(doy,SW,Temp,Rain,Pres,RH)
dat=data.frame(doy,SW,Temp,Tmax,Tmin,Rain,RH)


SW.g   = ggplot() + geom_point(aes(x=doy,y=SW))  +stat_smooth(aes(x=doy,y=SW),span=0.3)  ; SW.g
Temp.g = ggplot() + geom_point(aes(x=doy,y=Temp))+stat_smooth(aes(x=doy,y=Temp),span=0.3); Temp.g
Rain.g = ggplot() + geom_point(aes(x=doy,y=Rain))+stat_smooth(aes(x=doy,y=Rain),span=0.3); Rain.g
RH.g   = ggplot() + geom_point(aes(x=doy,y=RH))  +stat_smooth(aes(x=doy,y=RH),span=0.3)  ; RH.g
Pres.g = ggplot() + geom_point(aes(x=doy,y=Pres))+stat_smooth(aes(x=doy,y=Pres),span=0.3); Pres.g

#grid.arrange(SW.g,Temp.g,Rain.g,RH.g,Pres.g,nrow=2)
grid.arrange(SW.g,Temp.g,Rain.g,RH.g,nrow=2)

 
#write.csv(dat,"ACJ-01-daily.csv",row.names=FALSE)
#write.csv(dat,"SPD-daily.csv",row.names=FALSE) 
#write.csv(dat,"TON-01-daily.csv",row.names=FALSE)
#write.csv(dat,"TRU-04-daily.csv",row.names=FALSE)
write.csv(dat,"TRU-2750-daily.csv",row.names=FALSE)







