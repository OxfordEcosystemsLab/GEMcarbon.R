
###################################################################

#Code to read in all datafiles in a folder and plot the mean timeseries.

###################################################################

#Set input directory
dirpath = "M:\\TimeSeries\\AutSoilResp\\BOB\\"

#Set column indices
it = 1 #Time (ie. date column)
id = 2 #Data points for averaging
ie = 3 #Error on data points (if no error column set ie=0)

#Set delta for binning
delta = 30 #ie. width of bins in days

#Set output filename for pdf
pdf("M:\\TimeSeries\\AutSoilResp\\BOBMeanAnomaly.pdf")
#Set output filename for dataframe
outputcsv = "M:\\TimeSeries\\AutSoilResp\\meananom_BOB.csv"

#Set yaxis label
ylabel = "Autotrophic Soil Respiration - Monthly Means (MgC ha^-1 month^-1)"
#Set xaxis label
xlabel = 'Date'

#Set yaxis limits
ylimits_flag = 1 #Set ylimits_flag=0 if prefer code to calculate ylimits
ylimits = c(-2.0,2.0)
#Set xaxis limits
xlimits_flag = 1 #Set xlimits_flag=0 if prefer code to calculate xlimits
xlimits = c(as.Date('2014-01-01', format="%Y-%m-%d"),as.Date('2016-12-31', format="%Y-%m-%d")) 

#If errorbars are too small to plot, the code will produce the following warning, which can be ignored:
#In arrows(d$Date, d$Y_errpos, d$Date, d$Y_errneg, length = 0.05,  ... :
#  zero-length arrow is of indeterminate angle and so skipped

###################################################################

#Get list of datafiles
filelist = list.files(dirpath)
nfiles = length(filelist)
print("List of input files:")
print(filelist)
print(paste("Total no. of input files =",toString(nfiles)))

#Read in first file
fi = read.csv(paste(dirpath,filelist[1],sep=""),header=FALSE,stringsAsFactors=FALSE)
if (ie>0)
    {d = data.frame(dates=as.Date(fi[,it]),Y=as.numeric(fi[,id]),Yerr=as.numeric(fi[,ie]),fno=numeric(length(fi[,it])),bin=numeric(length(fi[,it])))
}
if (ie==0)
    {d = data.frame(dates=as.Date(fi[,it]),Y=as.numeric(fi[,id]),Yerr=as.numeric(length(fi[,it])),fno=numeric(length(fi[,it])),bin=numeric(length(fi[,it])))
}
#Fill in fno
for (j in 1:length(d$dates))
    {d$fno[j] = 1
}
#Order data chronologically
d = d[with(d,order(d$dates)),]
#Get mean dt
dt = numeric((length(d$dates)-1))
for (i in 2:length(d$dates))
    {dt[i-1] = as.numeric(d$dates[i])-as.numeric(d$dates[i-1])
}
print(dt)
print(mean(dt))
#Add on data from other files
for (i in 2:nfiles)
    {fi = read.csv(paste(dirpath,filelist[i],sep=""),header=FALSE,stringsAsFactors=FALSE)
	  if (ie>0)
        {dtemp = data.frame(dates=as.Date(fi[,it]),Y=as.numeric(fi[,id]),Yerr=as.numeric(fi[,ie]),fno=numeric(length(fi[,it])),bin=numeric(length(fi[,it])))
    }
    if (ie==0)
        {dtemp = data.frame(dates=as.Date(fi[,it]),Y=as.numeric(fi[,id]),Yerr=as.numeric(length(fi[,it])),fno=numeric(length(fi[,it])),bin=numeric(length(fi[,it])))
    }
    #Fill in fno
    for (j in 1:length(dtemp$dates))
        {dtemp$fno[j] = i
    }
    #Order data chronologically
    dtemp = dtemp[with(dtemp,order(dtemp$dates)),]
    #Get mean dt
    dt = numeric((length(dtemp$dates)-1))
    for (i in 2:length(dtemp$dates))
        {dt[i-1] = as.numeric(dtemp$dates[i])-as.numeric(dtemp$dates[i-1])
    }
    print(dt)
    print(mean(dt))
    #Append to d
	  d = rbind(d,dtemp)
}
#print(d)

#Order data chronologically
d = d[with(d,order(d$dates)),]
#print(d)

#Get binned dates
binned_d = data.frame(dates=integer(length(d$dates)),Y=numeric(length(d$dates)),Yerr=numeric(length(d$dates)),N=numeric(length(d$dates)))
starti = 1
bin_no = 1
d$bin[1] = bin_no #First date will be in bin 1
for (i in 2:(length(d$dates)))
    {#If interval between dates>delta:
	  if ((as.numeric(d$dates[i])-as.numeric(d$dates[starti]))>delta)
        {endi = i-1
        #Get mean date for previous date bin
        meand = 0.0
        meany = 0.0
        for (j in starti:endi)
            {meand = meand+as.numeric(d$dates[j])
            meany = meany+as.numeric(d$Y[j])
        }
        meand = meand/as.numeric(endi-starti+1)
        meany = meany/as.numeric(endi-starti+1)
        binned_d$dates[bin_no] = round(meand,0)
        binned_d$Y[bin_no] = as.numeric(meany)
        binned_d$Yerr[bin_no] = as.numeric(d$Yerr[starti])
        binned_d$N[bin_no] = (endi-starti+1)
        #Reset starti
        starti = i
        #Update bin no
        bin_no = bin_no+1
    }
    d$bin[i] = bin_no
}
#Do last bin
endi = length(d$dates)
meand = 0.0
meany = 0.0
for (j in starti:endi)
    {meand = meand+as.numeric(d$dates[j])
    meany = meany+as.numeric(d$Y[j])
}
meand = meand/as.numeric(endi-starti+1)
meany = meany/as.numeric(endi-starti+1)
binned_d$dates[bin_no] = meand
binned_d$Y[bin_no] = as.numeric(meany)
binned_d$Yerr[bin_no] = as.numeric(d$Yerr[starti])
binned_d$N[bin_no] = (endi-starti+1)

#Trim binned_d
binned_d = subset(binned_d[1:bin_no,])

#Go through binned_d and set Yerr to standard error on mean if N>1
for (j in 1:length(binned_d$dates))
    {if (binned_d$N[j]>1)
        {bin_no = j
        binned_d$Yerr[j] = 0.0
        for (i in 1:length(d$dates))
            {if (d$bin[i]==bin_no)
                {binned_d$Yerr[j] = binned_d$Yerr[j]+(d$Y[i]-binned_d$Y[j])**2.0
            }
        }
        binned_d$Yerr[j] = binned_d$Yerr[j]/as.numeric(binned_d$N[j]-1)
        binned_d$Yerr[j] = binned_d$Yerr[j]/sqrt(as.numeric(binned_d$N[j]))
    }
}

#Convert dates in binned_d to date type
binned_d0 = binned_d
binned_d = data.frame(dates=as.Date(binned_d0$dates,origin="1970-01-01",format="%Y-%m-%d"),Y=binned_d0$Y,Yerr=binned_d0$Yerr,N=binned_d0$N)
print(d)
print(binned_d)

#Write out
write.table(binned_d, file = outputcsv,row.names=FALSE, na="",col.names=FALSE, sep=",")

#Get ylimits if not already specified
if (ylimits_flag==0)
    {ylimits = range(c(binned_d$Y+binned_d$Yerr,binned_d$Y-binned_d$Yerr))
}
#Get xlimits if not already specified
if (xlimits_flag==0)
    {xlimits = range(binned_d$dates)
}
#Check yaxis limits are wide enough
minlimits = range(c(binned_d$Y+binned_d$Yerr,binned_d$Y-binned_d$Yerr))
if (minlimits[1]<ylimits[1])
    {print(paste("ylimits[1] must be < ",toString(minlimits[1])))
}
if (ylimits[2]<minlimits[2])
    {print(paste("ylimits[2] must be > ",toString(minlimits[2])))
}
#Check xaxis limits are wide enough
minlimits = range(binned_d$dates)
if (minlimits[1]<xlimits[1])
    {print(paste("xlimits[1] must be < ",toString(minlimits[1])))
}
if (xlimits[2]<minlimits[2])
    {print(paste("xlimits[2] must be > ",toString(minlimits[2])))
}
#Make plot
plot(binned_d$dates,binned_d$Y,pch=19,col='black',xlab="Date",ylab=ylabel,xlim=xlimits,ylim=ylimits)
lines(binned_d$dates,binned_d$Y,col='black')
points(d$dates,d$Y,col='grey')
lines(d$dates,d$Y,col='grey')
#Add error bars
arrows(binned_d$dates,binned_d$Y+binned_d$Yerr,binned_d$dates,binned_d$Y-binned_d$Yerr,length=0.05,angle=90,code=3)
graphics.off()
