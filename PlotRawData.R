###################################################################

#Code to read in raw datafile and plot raw data. 
#Output pdf contains individual timeseries for each plot (1 per page). 

###################################################################

#Set input datafile
inputfn = "M:\\TimeSeries\\InputDataFiles\\ts_TAM05_Rs_total_2017sorted.csv"
#Set column indices:
ip = 3 #Plot name
id = 11 #Data points for plotting
ie = 12 #Error on data points (if no error column set ie=0)
it = 10 #Time (ie. date column)
#Date column format flag:
#If date format is YYYY-MM-DD set it_flag=0
#If date format is DD/MM/YYYY set it_flag=1
#Use Notepad++ (or similar) to check date format in csv.
#Do not use excel to check date format, as the format excel displays the date in may be different to the format stored in the csv. 
it_flag = 1 

#Set output filename
pdf("M:\\TimeSeries\\Codes\\TAM-05.pdf")

#Set yaxis label
ylabel = "Total Soil Respiration (MgC ha^-1 month^-1)"
#Set xaxis label
xlabel = 'Date'

#Set yaxis limits
ylimits_flag = 1 #Set ylimits_flag=0 if prefer code to calculate ylimits
ylimits = c(0.0,4.5) 
#Set xaxis limits
xlimits_flag = 1 #Set xlimits_flag=0 if prefer code to calculate xlimits
xlimits = c(as.Date('2005-01-01', format="%Y-%m-%d"),as.Date('2016-12-31', format="%Y-%m-%d")) 

#If errorbars are too small to plot, the code will produce the following warning, which can be ignored:
#In arrows(d$Date, d$Y_errpos, d$Date, d$Y_errneg, length = 0.05,  ... :
#  zero-length arrow is of indeterminate angle and so skipped

###################################################################

#Load in data file
f1 = read.csv(inputfn,header=FALSE,stringsAsFactors=FALSE)
rc = dim(f1)
r1 = rc[1]
c1 = rc[2]

###################################################################

#Get list of plots and no. of non-na and non-zero datapoints for each plot
plotlist = f1[(2:r1),ip]
plotlist = unique(plotlist)
nplots = length(plotlist)
plots = matrix(0,nplots,2)
for (j in 1:nplots)
    {plots[j,1] = plotlist[j]
    for (i in 2:r1)
        {if (as.character(f1[i,ip])==plots[j,1])
            {if ((is.na(f1[i,id])==FALSE)&(is.na(f1[i,it])==FALSE))
    	          {if ((as.character(f1[i,id])!='0')&(as.character(f1[i,id])!="")&(as.character(f1[i,it])!='0')&(as.character(f1[i,it])!=""))
    	              {plots[j,2] = as.numeric(plots[j,2])+1.0
    	          }
    	      }
        }
    }
}
print("Plot  No.of non-na & non-zero datapoints")
print(plots)
print(paste("Total no. of plots =",toString(nplots)))

#Plot timeseries for each plot
for (j in 1:nplots)
	{if ((plots[j,2]>0)&(plots[j,1]!=""))
    {#Initialise empty dataframe
	  nd = as.numeric(plots[j,2])
    d = data.frame(Date=as.Date(character(nd),format="%Y-%m-%d"),Y=numeric(nd),Y_err=numeric(nd),Y_errpos=numeric(nd),Y_errneg=numeric(nd))
    #Fill in arrays with data
    counter = 0
    for (i in 2:r1)
        {if (as.character(f1[i,ip])==plots[j,1])
            {if ((is.na(f1[i,id])==FALSE)&(is.na(f1[i,it])==FALSE))
    	          {if ((as.character(f1[i,id])!='0')&(as.character(f1[i,id])!="")&(as.character(f1[i,it])!='0')&(as.character(f1[i,it])!=""))
    	              {counter = counter+1
                    strdate = as.character(f1[i,it])
                    if (it_flag==0)
                        {splitdate = strsplit(strdate,"-")[[1]]
                        d[counter,1] = as.Date(paste(splitdate[1],splitdate[2],splitdate[3],sep="-"),format="%Y-%m-%d")
                    }
                    if (it_flag==1)
                        {splitdate = strsplit(strdate,"/")[[1]]
                        d[counter,1] = as.Date(paste(splitdate[3],splitdate[2],splitdate[1],sep="-"),format="%Y-%m-%d")
                    }
                    d[counter,2] = as.numeric(f1[i,id])
                    if (ie>0)
                        {d[counter,3] = as.numeric(f1[i,ie])
                        if (is.na(d[counter,3])==TRUE)
                            {d[counter,3] = 0.0
                        }
                    }
                    else
                        {d[counter,3] = 0.0
                    }
                    d[counter,4] = d[counter,2]+d[counter,3]/2.0
                    d[counter,5] = d[counter,2]-d[counter,3]/2.0
    	          }
            }
        }
    }
    #print(d)
    #Get ylimits if not already specified
    if (ylimits_flag==0)
        {ylimits = range(c(d$Y_errneg,d$Y_errpos))
    }
    #Get xlimits if not already specified
    if (xlimits_flag==0)
        {xlimits = range(d$Date)
    }
    #Check yaxis limits are wide enough
    minlimits = range(c(d$Y_errneg,d$Y_errpos))
    if (minlimits[1]<ylimits[1])
        {print(paste("ylimits[1] must be < ",toString(minlimits[1])))
    }
    if (ylimits[2]<minlimits[2])
        {print(paste("ylimits[2] must be > ",toString(minlimits[2])))
    }
    #Check xaxis limits are wide enough
    minlimits = range(d$Date)
    if (minlimits[1]<xlimits[1])
        {print(paste("xlimits[1] must be < ",toString(minlimits[1])))
    }
    if (xlimits[2]<minlimits[2])
        {print(paste("xlimits[2] must be > ",toString(minlimits[2])))
    }
    #Make plot
    plot(d$Date,d$Y,pch=19,col='black',xlab=xlabel,ylab=ylabel,main=as.character(plots[j,1]), xlim=xlimits,ylim=ylimits)
    #Add error bars
    arrows(d$Date,d$Y_errpos,d$Date,d$Y_errneg,length=0.05,angle=90,code=3)
	}
}
graphics.off()
