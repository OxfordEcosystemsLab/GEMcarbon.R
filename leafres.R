### Input Data to the function leaf respiration:
# datalr: data.frame leaf respiration data with 9 columns (specified below)
# datalai: data.frame LAI data with x columns (specified below)
# datarest: data.frame with soil respiration data (important for soil moisture)
# plotname: String or number indicating which plot should be analysed
# shaded: logical, indicating whether an adjustment for shaded leaves should be made? (standard case according to Chris is yes)
# dryseason: vector that specifies EACH dry month 
# ret: format of return value, indicated with strings
# plotit: logical, whether function should be plotted

#### Read Chris' Data:
## open Boldata:
library(R.matlab)
setwd("C:/Users/Cecile/Dropbox/Carbon_Use_Efficieny_R/Original-matlab-code/Ken_Iq-Tang")
#setwd("D:/Dokumente/My Dropbox/Carbon_Use_Efficieny_R/Original-matlab-code/Ken_Iq-Tang")
dir()
Dataall <- readMat(con="Dataall.mat")

# leaf respiration:
datalr <- data.frame(Dataall$Resleafall)   # leaf respiration
datalai <- data.frame(Dataall$LAIall)      # LAI data
datarest <- data.frame(Dataall$Restotall)  # total respiration; # enter data for volumetric water contents

names(datalr) <- c("year","month","plot","tree_number","A_sun","A_shade","r_sun","r_shade")
names(datalai) <- c("year","month","plot","N.N","N.N","lai","lai_std")
names(datarest) <- c("year","month","plot","point","N.N","temperature_C","volumetric_water_content","chamber_height","flux")
## adjust options:
shaded=T
dryseason=c(8,9,10,11)
plotname=1


#leafres <- function(datalr, datalai, datarest, plotname, shaded = T, dryseason, 
#                    ret="monthly.means.ts", plotit=F) {

## column names of data.frame 'datalr' (leaf respiration data):
# plot
# tree_number
# A_sun
# A_shade
# r_sun
# r_shade

# divide into wet and dry season:
yearlr = datalr$year
monthlr = datalr$month
## Divide into wet and dry season:
  for (i in 1:length(monthlr)) {
   if (any(monthlr[i] == dryseason)) {  
     monthlr[i] <- 0     ## if condition is true: dry season
   } else {
     monthlr[i] <- 1     ## if condition is false: wet season
   }
  }
plotlr = datalr$plot
treenum = datalr$tree_number
Asunlr = datalr$A_sun
Ashadelr = datalr$A_shade
rsunlr = datalr$r_sun
rshadelr = datalr$r_shade

rsunlr[which(rsunlr < -1.5)] <- NA
rsunlr[which(rsunlr>0)] <- NA
rshadelr[which(rshadelr< -1.5)] <- NA
rshadelr[which(rshadelr>0)] <- NA

## The following loop divides data into sun/shade data in both wet and dry season!
## both r and A
# Initialize variables for the loop:
rsunlrAw <- NULL
rshadelrAw <- NULL
AsunlrAw <- NULL
AshadelrAw <- NULL
treenumAw <- NULL

rsunlrAd <- NULL
rshadelrAd <- NULL
AsunlrAd <- NULL
AshadelrAd <- NULL
treenumAd <- NULL

n=1
m=1
nn=1
mm=1
z=1
zz=1
n1=1
m1=1
nn1=1
mm1=1
z1=1
zz1=1

# Distinguish between wet and dry season, and allocate sun angle accordingly. This loop produces new variables with a new structure.
for (i in 1:length(plotlr)) {
    if (plotlr[i]==plotname) {
        if (monthlr[i]==1) {   # if so: wet season (==1)
            rsunlrAw[n] = rsunlr[i]
            rshadelrAw[n] = rshadelr[i]
            AsunlrAw[n] = Asunlr[i]
            AshadelrAw[n] = Ashadelr[i]
            treenumAw[n] = treenum[i]
            n=n+1;
        } else {    # else: dry season (==0)
        rsunlrAd[n1] = rsunlr[i]
        rshadelrAd[n1] = rshadelr[i]
        AsunlrAd[n1] = Asunlr[i]
        AshadelrAd[n1] = Ashadelr[i]
        treenumAd[n1] = treenum[i]
        n1=n1+1;
        }
    }
}


# calculate wet season means
rsunlrAwm = mean(rsunlrAw,na.rm=T);
rsunlrAwmstd = sd(rsunlrAw,na.rm=T);

rshadelrAwm = mean(rshadelrAw,na.rm=T);
rshadelrAwmstd = sd(rshadelrAw,na.rm=T);

AsunlrAwm = mean(AsunlrAw,na.rm=T);
AsunlrAwmstd = sd(AsunlrAw,na.rm=T);

AshadelrAwm = mean(AshadelrAw,na.rm=T);
AshadelrAwmstd = sd(AshadelrAw,na.rm=T);

# calculate dry season means
rsunlrAdm = mean(rsunlrAd,na.rm=T)
rsunlrAdmstd = sd(rsunlrAd,na.rm=T)

rshadelrAdm = mean(rshadelrAd,na.rm=T);
rshadelrAdmstd = sd(rshadelrAd,na.rm=T);

AsunlrAdm = mean(AsunlrAd,na.rm=T);
AsunlrAdmstd = sd(AsunlrAd,na.rm=T);

AshadelrAdm = mean(AshadelrAd,na.rm=T);
AshadelrAdmstd = sd(AshadelrAd,na.rm=T);


# LAI data (data.frame 'datalai') must be arranged in the following way:
# year	                                [col 1]
# month	                              [col 2]
# plot (1=esp, 2=way, 3=sp1, 4=sp2)	  [col 3]
# true LAI                             [col 6]

yearlai = datalai$year
monthlai = datalai$month
plotlai = datalai$plot
lai = datalai$lai
laistd = datalai$lai_std
ixlai = which(plotlai==plotname) 

laiA = lai[ixlai]
laistdA = laistd[ixlai]
yearlaiA = yearlai[ixlai]
monthlaiA = monthlai[ixlai]

# get total leaf area

# where K is the light extinction coefficient, L is the leaf area
# index, and Lsun is the sunlit leaf fraction. The model assumed
# a random distribution of leaves, and calculated K as:
# where Z is the solar zenith angle which we set to 30. 
K = 0.5/cos(30*pi/180)

### The loop calculates number of shaded and sun leaves!
## initialize variables for the for-loop:
LsunA <- NULL
LshadeA <- NULL
# wet season
LsunresAw <- NULL
LsunresAwstd <- NULL
LshaderesAw <- NULL
LshaderesAwstd <- NULL
LsunAAw <- NULL
LsunAAwstd <- NULL
LshadeAAw <- NULL
LshadeAAwstd <- NULL

# dry season
LsunresAd <- NULL
LsunresAdstd <- NULL
LshaderesAd <- NULL
LshaderesAdstd <- NULL
LsunAAd <- NULL
LsunAAdstd <- NULL
LshadeAAd <- NULL
LshadeAAdstd <- NULL

for (i in 1:length(laiA)) {
  if (shaded==T) {
    LsunA[i] = (1-exp(-K*laiA[i]))/K  ## estimate how many shaded leaves
  }
  if (shaded==F) {
    LsunA[i] = laiA[i]   ## in case of no shade leaves 
  }
    LshadeA[i] = laiA[i]-LsunA[i];
    # wet season
    LsunresAw[i] = LsunA[i]*rsunlrAwm;
    LsunresAwstd[i] = LsunA[i]*rsunlrAwmstd;
    LshaderesAw[i] = LshadeA[i]*rshadelrAwm;
    LshaderesAwstd[i] = LshadeA[i]*rshadelrAwmstd;
    LsunAAw[i] = LsunA[i]*AsunlrAwm;
    LsunAAwstd[i] = LsunA[i]*AsunlrAwmstd;
    LshadeAAw[i] = LshadeA[i]*AshadelrAwm;
    LshadeAAwstd[i] = LshadeA[i]*AshadelrAwmstd;
    
    # dry season
    LsunresAd[i] = LsunA[i]*rsunlrAdm;
    LsunresAdstd[i] = LsunA[i]*rsunlrAdmstd;
    LshaderesAd[i] = LshadeA[i]*rshadelrAdm;
    LshaderesAdstd[i] = LshadeA[i]*rshadelrAdmstd;
    LsunAAd[i] = LsunA[i]*AsunlrAdm;
    LsunAAdstd[i] = LsunA[i]*AsunlrAdmstd;
    LshadeAAd[i] = LshadeA[i]*AshadelrAdm;
    LshadeAAdstd[i] = LshadeA[i]*AshadelrAdmstd;
}

# wet season leaf respiration
LtotresAw = colSums(t(matrix(data=c(LsunresAw,LshaderesAw), ncol=2)),na.rm=T)
LtotresAwstd = colSums(t(matrix(data=c(LsunresAwstd,LshaderesAwstd), ncol=2)),na.rm=T)

LtotresAw[which(LtotresAw==0)] <- NA
LtotresAwstd[which(LtotresAwstd==0)] <- NA

# dry season leaf respiration
LtotresAd = colSums(t(matrix(data=c(LsunresAd,LshaderesAd), ncol=2)),na.rm=T)
LtotresAdstd = colSums(t(matrix(data=c(LsunresAdstd,LshaderesAdstd), ncol=2)),na.rm=T)

LtotresAd[which(LtotresAd==0)] <- NA
LtotresAdstd[which(LtotresAdstd==0)] <- NA

# wet leaf area??
LtotAAw = colSums(t(matrix(data=c(LsunAAw,LshadeAAw), ncol=2)),na.rm=T)
LtotAAwstd = colSums(t(matrix(data=c(LsunAAwstd,LshadeAAwstd), ncol=2)),na.rm=T)

LtotAAw[which(LtotAAw==0)] <- NA
LtotAAwstd[which(LtotAAwstd==0)] <- NA

# dry leaf area??
LtotAAd = colSums(t(matrix(data=c(LsunAAd,LshadeAAd), ncol=2)),na.rm=T)
LtotAAdstd = colSums(t(matrix(data=c(LsunAAdstd,LshadeAAdstd), ncol=2)),na.rm=T)

LtotAAd[which(LtotAAd==0)] <- NA
LtotAAdstd[which(LtotAAdstd==0)] <- NA


# Include volumetric water contents by reading total soil respiration data, which must be arranged
# in the follwing way:
# year                                                     [col 1]
# month                                                    [col 2]
# plot (1=BolA, 2=BolB, 3=Iq1, 4=Iq2, 5=TangA, 6=TangB)    [col 3]
# area	                                                    [not used]
# CO2	                                                    [not used]
# T aire	                                                  [not used]
# VWC%	                                                    [col 7]
# Depth (cm)	                                            [not used]
# DCO2                                                     [not used]

yeart = datarest$year 
montht = datarest$month  
plott = datarest$plot # 1=BolA, 2=BolB, 3=Iq1, 4=Iq2, 5=TangA, 6=TangB
watert = datarest$volumetric_water_content  

n=1;
m=1;
nn=1;
mm=1;
z=1;
zz=1;
monthtA <- NULL
yeartA <- NULL
watertA <- NULL

# get water contents with plott==plotname
for (i in 1:length(montht)) {
  if (plott[i]==plotname) {
    monthtA[n] = montht[i];
    yeartA[n] = yeart[i];
    watertA[n] = watert[i];
    n=n+1;
  }
}


## Initialize matrices with UNIFIED size (for water contents and total leaf respiration):
fir_year = min(c(yearlr,yearlaiA,yeartA),na.rm=T)
fir_yeare = max(c(yearlr,yearlaiA,yeartA),na.rm=T)

# initialize variables for for-loop:
totLresA <- matrix(data=NA, nrow=12, ncol=fir_yeare-fir_year+1, dimnames=list(c(month.name),fir_year:fir_yeare))
totLresAstd <- matrix(data=NA, nrow=12, ncol=fir_yeare-fir_year+1, dimnames=list(c(month.name),fir_year:fir_yeare))
totlaiA <- matrix(data=NA, nrow=12, ncol=fir_yeare-fir_year+1, dimnames=list(c(month.name),fir_year:fir_yeare))
totlaistdA <- matrix(data=NA, nrow=12, ncol=fir_yeare-fir_year+1, dimnames=list(c(month.name),fir_year:fir_yeare))
totLAA <- matrix(data=NA, nrow=12, ncol=fir_yeare-fir_year+1, dimnames=list(c(month.name),fir_year:fir_yeare))
totLAAstd <- matrix(data=NA, nrow=12, ncol=fir_yeare-fir_year+1, dimnames=list(c(month.name),fir_year:fir_yeare))
watertAt <- matrix(data=NA, nrow=12, ncol=fir_yeare-fir_year+1, dimnames=list(c(month.name),fir_year:fir_yeare))
watertAtstd <- matrix(data=NA, nrow=12, ncol=fir_yeare-fir_year+1, dimnames=list(c(month.name),fir_year:fir_yeare))
# temporarily outcommented
#watertAtlen <- matrix(data=NA, nrow=12, ncol=fir_yeare-fir_year+1, dimnames=list(c(month.name),fir_year:fir_yeare))


n=1
# average by month for volumetric water contents:
for (j in fir_year:fir_yeare) {
  m=1;
  for (i in 1:12) {
    ind = which(monthtA==i & yeartA==j);
    
    # changes in soil moisture
    watertAt[m,n] = mean(watertA[ind],na.rm=T);
    watertAtstd[m,n] = sd(watertA[ind],na.rm=T);
    # temporarily outcommented
    # watertAtlen[m,n] = length(watertA[ind]);
    
    m=m+1;
  }
  n=n+1;
}


n=1;
# convert to MgC ha month for wet and dry season!
for (j in (fir_year:fir_yeare)) {
    m=1;
    for (i in 1:12) {
    ind = which(monthlaiA==i & yearlaiA==j);
    
    ## dry season scale by water deficit
    ## this looks like an error: (dry season i> 5 & i < 11) -> different to above!!
    # if (i > 5 & i < 11) {   ## Seb. replaced by replaced by the follwoing: check with Chris that this is ok.
    if (any(dryseason == i)) {
    totLresA[m,n] = mean(LtotresAd[ind],na.rm=T);
    totLresAstd[m,n] = mean(LtotresAdstd[ind],na.rm=T);
    
    totlaiA[m,n] = mean(laiA[ind],na.rm=T);
    
    totlaistdA[m,n] = mean(laistdA[ind],na.rm=T);
    
    totLAA[m,n] = mean(LtotAAd[ind],na.rm=T);
    totLAAstd[m,n] = mean(LtotAAdstd[ind],na.rm=T);
    
    m=m+1;
    } else {
                # wet season 
    totLresA[m,n] = mean(LtotresAw[ind],na.rm=T);
    totLresAstd[m,n] = mean(LtotresAwstd[ind],na.rm=T);
    
    totlaiA[m,n] = mean(laiA[ind],na.rm=T);
    totlaistdA[m,n] = mean(laistdA[ind],na.rm=T);
    
    totLAA[m,n] = mean(LtotAAw[ind],na.rm=T);
    totLAAstd[m,n] = mean(LtotAAwstd[ind],na.rm=T);
    
    m=m+1;
      }
    }
    n=n+1;
}



# scale leaf respiration for the dry season using soil moisture
# measurements taken in Kenia Nov 2010 and June 2011
xi = dim(totLresA);
   ### again: different dry season values are used here:
   # for (i in 5:11 ) {
  ### should possibly be replaced by:
for (k in 1:length(dryseason)) {
# i = dryseason[k]
  for (j in 1:xi[2]) {
    totLresA[i,j] = totLresA[i,j]-((1-(watertAt[i,j]/30))*(mean(LtotresAw,na.rm=T)-mean(LtotresAd,na.rm=T)))
    totLresAstd[i,j] = totLresAstd[i,j]-((1-(watertAt[i,j]/30))*(mean(LtotresAwstd,na.rm=T)-mean(LtotresAdstd,na.rm=T)))
    }
}

# convert units umol m2s-1 to MgC ha month = 1mo=2592000sec, 10000m2=1ha,
# 1000000umol = 1 mol, 1mol = 12 g, 1000000g=1Mg

convert = (2592000*10000*12)/(1000000*1000000)
totLresAc = -totLresA*convert
totLresAcstd = (totLresAstd*convert)/sqrt(20)

totLAAc = totLAA*convert
totLAAcstd = totLAAstd*convert


## Build list with matrices that contain monthly values:
{
  totLresAc.monthly.matrix <- list(totLresAc,totLresAcstd)
  names(totLresAc.monthly.matrix) <- c("totLresAc","totLresAcstd")
}

###  Build data frame with time series structure
{
  ##Restructure the data (according to time series structure):
  Year <- NULL
  Month <- NULL
  Day <- NULL
  
  for (i in 1:dim(totLresAc)[2]) {
    Year[((i-1)*12+1):((i-1)*12+12)] <- (rep(c(fir_year:fir_yeare)[i],12))
    Month[((i-1)*12+1):((i-1)*12+12)] <- (1:12)
    Day[((i-1)*12+1):((i-1)*12+12)] <- rep(NA,12)
  }
  
  totLresAc.monthly.ts <- data.frame(Year,Month,Day,
                                    c(totLresAc),c(totLresAcstd))
  
  colnames(totLresAc.monthly.ts) <- c("Year","Month","Day",  
                                     "totLresAc","totLresAcstd")
}

## Plotroutine, triggered by argument 'plotit=T'
if (plotit==T) {
  ## Time representation of Dates as character in the vector 'dates':
  dates <- c()
  for (i in 1:length(Year)) {
    dates[i] <- as.character(strptime(paste(as.character(Year[i]),as.character(Month[i]),as.character(15),sep="-"),
                                      format="%Y-%m-%d"))
  }
  
  x11()
  #par(mfrow=c(3,1))
  par(mar=c(4,4,0.5,0.5))
  plot(x=strptime(dates, format="%Y-%m-%d"), y=totLresAc.monthly.ts$totLresAc, 
       type='l',lwd=2,
       xlab="Years", ylab="Total leaf respiration [Units]")
  legend("topleft", c("leaf respiration"), lty=c(1), bty='n')
}


# Return either monthly means (ret="monthly.means") or annual means (ret="annual.means")  
switch(ret,
       monthly.means.matrix = {return(totLresAc.monthly.matrix)},
       monthly.means.ts = {return(totLresAc.monthly.ts)}
)

}
