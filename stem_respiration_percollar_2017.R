setwd("~/Github/gemcarbon_data/raw_data_ingembd/")
stem_resp <- read.table("~/Github/gemcarbon_data/raw_data_ingembd/stem_resp_27th March.csv", sep=",", header=T)

setwd("~/Github/GEMcarbon.R")
source("~/Github/GEMcarbon.R/soilrespiration_auxfunctions.r")

setwd("~/Github/GEMcarbon.R") 
source("allometric_equations_2014.R")


# sort out plot_codes

stem_resp$plot_code <- revalue(stem_resp$plot_code, c("Tower" = "SAF-05", "E" = "SAF-03", "B South" = "SAF-01", "B North" = "SAF-02", "Seraya" = "MLA-02", "Belian" = "MLA-01", "LF" = "SAF-04", "TOWER" = "SAF-05", "DC2" = "DAN-05", "Danum Carbon 1" = "DAN-04", "Danum Carbon 2" = "DAN-05", "LOP-01" = "LPG-01","TU4" = "TRU-04"))
unique(stem_resp$plot_code)

SAF-05 SAF-03 SAF-01 SAF-02 MLA-02 MLA-01 SAF-04 E DAN-05 DAN-04 PAN-01 PAN-02 PAN-03 LPG-01 IVI-01 IVI-02 TAM-09 TAM-06 TAM-05 SPD-02 SPD-01 ACJ-01
ESP-01 TRU-04 WAY-01 KEN-02 KEN-01

# SEA plot_codes from Terhi 03/04/2017
#B North               SAF-02
#B South               SAF-01
#E                     SAF-03
#LF                    SAF-04
#Tower                 SAF-05
#Belian (in Maliau)    MLA-01
#Seraya (in Maliau)    MLA-02
#Danum Carbon 1  (DC1) DAN-04
#Danum Carbon 2  (DC2) DAN-05


 
#####################################################################################


plotname = "LPG-01"
collardiameter=12
collarheight=5
T_ambient= "Default"
  
# Mean annual soil temperature

matALP11 <- 25.2	
matALP30 <- 25.2	
matTAM   <- 24.4 # TAM-05 -06 -09	
matTON01 <- 20.7	
matSPD02 <- 18.8	
matSPD01 <- 17.4	
matTRU07 <- 17.4	
matTRU08 <- 18	
matTRU04 <- 13.5	
matTRU03 <- 11.8
matWAY01 <- 11.8
matESP01 <- 13.1

												

stemrespiration <- function(stem_resp, plotname, ret="monthly.means.ts", collardiameter=12, collarheight=5, # Add tube radius as a parameter, change A to A <- pi*(rad^2) 
                            pressure="Default", elevation="Default", T_ambient="Default",
                            plotit=T) {
# select a plot
data1   <- subset(stem_resp, plot_code==plotname)

# Collar diameter and height (cm)
data1$collar_diam   <- collardiameter
data1$collar_height <- collarheight

# air temperature (deg C)


if (T_ambient=="Default") {
  print("WARNING! Mean annual air temperature was not specified, Default = 25 deg C")
  T_ambient <- 25
}

data1$air_temp_c <- T_ambient


# Estimate missing temperature as average temperature for the plot
#w <- which(is.na(raw_totsr$air_temp_c))
# Replace missing air temp with soil temp.
#raw_totsr$air_temp_c[w] <- raw_totsr$soil_temp_c_out[w]
# If data are still missing, replace with average air temp. #ATTENTION!! THIS IS A HACK! WHAT SHOULD WE DO WHEN WE DON"T HAVE AIR TEMP?
#w <- which(is.na(raw_totsr$air_temp_c))
#raw_totsr$air_temp_c[w] <- t
#raw_totsr$air_temp_c <- as.numeric(as.character(raw_totsr$air_temp_c)) 

# Estimate missing atmospheric pressure whith temperature-dependent version of the barometric equation (see soilrespiration_auxfunctions)
t <- mean(as.numeric(data1$air_temp_c), na.rm=T) 
w <- which(is.na(data1$atmp_mb))
data1$atmp_mb[w] <- barometric_equation_T(elevation=0, temp=t)


# TIME SHOULD BE NUMERIC in dataset!!
data1$time <- as.numeric(as.character(data1$time))
length(is.na(data1$time))

## Corrections and conversions
# add a temperature correction from Sotta et al 2004 Q10=1.8 and k=0.0613
tempcorr = exp(-0.0695*(1))
# Convert units umol m2 s-1 to MgC ha month = 1mo=2592000sec, 10000m2=1ha, 1000000umol = 1 mol, 1mol = 12 g, 1000000g=1Mg
convert = (2592000*10000*12)/(1000000*1000000)

# unique identifyer for each measurement: tree_tag, replica, date 
data1$codew   <- paste(data1$tree_tag, data1$replica, data1$day, data1$month, data1$year, sep=".")


## estimate flux for each measurement

# get unique identifyer for each measurement
uid <- unique(data1$codew)
xx  <- c()
yy  <- c()

for (i in 1:length(uid)) {
  sub      <- subset(data1, subset=(data1$codew == uid[i])) 
  id       <- tail(sub$codew, n=1) 
  ten_co2  <- tail(sub$co2ref_ppm_sec, n=10)                                       # Drop first two values rather than only keep last 10 values.
  ten_time <- tail(sub$time, n=10)       
  #fit      <- lm(ten_co2~ten_time)
  #Co2slope <- fit$coefficients[2]                                                 # USE SLOPE RATHER THAN DIFFERENCE OF C10-C1.
  C10      <- tail(ten_co2, n=1)                                                   # last CO2 measurement of last 10 measurements
  C1       <- head(ten_co2, n=1)                                                   # first CO2 measurement of last 10 measurements
  t10      <- tail(ten_time, n=1)                                                  # last time step of 10 last measurements
  t1       <- head(ten_time, n=1)                                                  # first time step of 10 last measurements
  P        <- tail(sub$atmp, n=1)                                                  # ambient pressure at t10 (mb)
  Ta       <- tail(sub$air_temp_c, n=1)                                            # air temp at t10 (deg C)
  ch       <- tail(sub$collar_height, n=1)                                               # see gap filling function fill.na() in soilrespiration_auxfinctions.r
  Vd       <- 0.0012287                                                            # m3 (constant)
  A        <- 0.00950                                                              # m2 (constant)
  Ru       <- 8.31432                                                              # J mol-1 K-1 (constant)
  Va       <- A*(ch/100)                                                           # additional volume m3
  fl       <- ((C10 - C1)/(t10 - t1)) * (P/(Ta + 273.15))*(Vd/A)*((44.01*0.36)/Ru) # CO2 efflux (g CO2 m-2 h-1)
  #flux2    <- Co2slope * (P/(Ta + 273.15))*(Vd/A)*((44.01*0.36)/Ru)               # CO2 efflux (g CO2 m-2 h-1) USE SLOPE RATHER THAN DIFFERENCE OF C10-C1.
  flux     <- (fl*A/Vd*(Va+Vd)/A)*6.312                                            # Convert to umol m-2 s-1. Correct for collar height.
  xx       <- rbind(xx, id)
  yy       <- rbind(yy, flux)
  print(xx)
}
rownames(xx) <- NULL
rownames(yy) <- NULL
yy           <- as.numeric(as.character(yy)) ## WHY IS THIS AS FACTOR??

Res                 <- cbind.data.frame(xx, yy)
colnames(Res)       <- c("codew", "flux_umolm2sec")
Res$flux_MgChamonth <- Res$flux_umolm2sec*convert*tempcorr

# Check for duplicates
any(duplicated(Res$codew))
Res$codew[which(duplicated(Res$codew))]

# build the new data frame
tsstem                 <- sqldf("SELECT Res.*, data1.* FROM Res LEFT JOIN data1 ON Res.codew = data1.codew GROUP BY Res.codew")
tsstem$flux_MgChamonth <- rm.flux.outlier(tsstem$flux_MgChamonth, 4) 
tsstem$date            <- as.Date(paste(tsstem$year, tsstem$month, tsstem$day, sep="."), format="%Y.%m.%d") 
tsstem                 <- tsstem[order(tsstem$sub_plot,tsstem$date),]

# plot all stem respiration measurements
plot <- ggplot(tsstem, aes(x = date, y = flux_MgChamonth, na.rm = T)) +
        geom_point(data = tsstem, aes(x = date, y = flux_MgChamonth), size = 2, colour = tsstem$year, na.rm=T) +
        ggtitle(plotname)
plot

# save to current directory  
setwd("~/Github/gemcarbon_data/processed_ts_2017")
write.csv(tsstem, file="stem_flux_TAM09_2017.csv")

}

#########################################

## Linear fit, estimate r2 quality check. 
#umea <- unique(data1$codew)
#xx <- c()
#yy <- c()
#zz <- c()

#for (i in 1:length(umea)) {
#  sub  <- subset(data1, subset=(data1$codew == umea[i]))
#  fit  <- lm(sub$time ~ sub$co2ref_ppm_sec) 
#  r    <- summary(fit)$r.squared 
#  p    <- anova(fit)$'Pr(>F)'[1] # or p <- summary(fit)$coefficients[,4]
#  u    <- head(sub$codew, 1)
#  xx   <- rbind(xx, r) 
#  yy   <- rbind(yy, p)
#  zz   <- rbind(zz, u)  
#}
#table <- data.frame(cbind(xx, yy, zz))
#colnames(table) <- c("r2", "pvalue", "unique_code") # note: small r2 doesn't mean bad data - small flux = small r2


# Add air temperature (temp), volumetric water content (vwc), and chamber depth (depth) to the raw total data (raw_totsrA)
#wea_tot$code1     <- paste(wea_tot$collar_num, wea_tot$replica, wea_tot$month, wea_tot$year, sep="_") # wea_tot$day, wea_tot$month, wea_tot$year, sep=".") # only use code 1 to merge wea_tot and raw_totsr (subplot.day.month.year is not a unique identifier).
#raw_totsrA$code1  <- paste(raw_totsrA$collar_num, raw_totsrA$replica, raw_totsrA$month, raw_totsrA$year, sep="_") # raw_totsrA$day, raw_totsrA$month, raw_totsrA$year, sep=".")
#wea_tot_avg       <- sqldf("SELECT AVG(wea_tot.vwc_percent_in), AVG(wea_tot.vwc_percent_out), AVG(wea_tot.soil_temp_c_in), AVG(wea_tot.soil_temp_c_out), AVG(wea_tot.air_temp_c), AVG(wea_tot.ch_new), wea_tot.code1 FROM wea_tot GROUP BY code1")
#colnames(wea_tot_avg) <- c("vwc_percent_in","vwc_percent_out","soil_temp_c_in", "soil_temp_c_out", "air_temp_c", "ch_new", "code1")
#raw_totsr           <- merge(raw_totsrA, wea_tot_avg, by = 'code1', all.x = TRUE) 


## Sanity checks. Plot each flux batch to check data.

#### TO DO: this only shows the last plot. What's wrong? 
#op <- par(ask=TRUE) # http://stackoverflow.com/questions/6031093/making-a-series-of-plots-that-proceed-by-a-click
#for (i in 1:length(raw_totsr$codew)){
#  aa <- subset(raw_totsr, codew == codew[i], select = c(codew, co2ref, time))
#  colnames(aa) <- c("codew", "co2", "time")
#  plot(aa$co2, aa$time, main = paste("code:", head(aa$codew, 1)), xlab="time", ylab="CO2 (micro mol s-1? ppm?)")
#  par(op)
#}





# two options we need to test:

# calculate stem respiration per whole tree, then regress with growht rate
# OR get stem resp per unit stem area (per collar), then regress with growth rate

# respiration rates versus size? P. Meir et al. 

# INTERESTING PAPER: how does stem respiration relate to dbh and growth rates?
# look at radial growth, but also biomass growth, because wood density may affect it so that it becomes more of a constant.
# look at respiration rate as a function of basal area/dbh. 1. decreases with increases in tree size? or no size signal?
# There will be a confounding effect of sapwood/hardwood: look at parenchama cell fractions in the wood?
# 

# 1. Get: 
# stem respiration flux per unit collar: raw to EGM.
# stem area index for each tree for that date (that year) using census data & Chambers2004_surfaceArea(diameter=diametersA).
# if dying trees - do we assume they respire 1/2 of that year? (make this assumption flexible)
# if new recruits - respiring 1/2 of the year.

# 2. growth rate upscaling:
# regression: woody growth x ~ stem respiration (per collar area or per tree?) y
# estimate stem respiration for all trees in the plot by applying the regression results.
# end product plot_code, sub_plot, tree_tag, rstem_MgC_ha_yr, stem_area_cm2 -- you get total respiration per hectare by summing up the rstems. 
