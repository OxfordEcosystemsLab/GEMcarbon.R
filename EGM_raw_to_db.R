## C. Girarin, February 2014.
## Code to go from EGM-4 raw data files to the csv files we need for the db.
## The csv files for the db are the same as the ones used for the R GEM functions. The output of this code feeds into soilrespiration.R
## Need to define the disturbance and partitioning codes for each plot - search "get disturbance code" and "get partitioning code". Does this fit your data?

# simple steps:
# 1. organise respiration data into three files: total, partitionning and control (keep same numbers for ";Plot" column).
# 2. save all your files as .csv
# 3. change directory in line 21 and replace "T_S_Resp_Lp1_01.07.2013.csv" with the name of the file you want to run this code through (same for controla nd partitionning)
# 4. add file names for the weather and vwc files (use column names as described on lines 25-27). uncomment lines where wea_tot, wea_con and wea_par are used and comment lines that assing random numbers (25.8 & 50) to temp & vwc.
# 5. run code
# 6. your new .csv files will be in Dropbox/Carbon_Use_Efficieny_R/testing
# 7. Note: you need to delete the first column in all your new files (column x, just the row number)
# 8. run the code in testingCG.R, in directory Dropbox\Carbon_Use_Efficieny_R\R-functions(v2). ## This doesn't work yet as you need partitionning option 2, and that is still on Seb's to do list! Sorry :-/
# 9. your monthly averages will be in 

# load packages
  library(sqldf)

# read in data
  setwd("C:/Users/Cecile/Dropbox/Carbon_Use_Efficieny_R/testing/soilresp")
  raw_totsr         <- read.table("1T_S_Resp_Lp1_01.07.2013.csv", header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE)
  raw_parsr         <- read.table("S_P_Resp_Lp1_ExData_01.07.2013.csv", header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE)
  raw_consr         <- read.table("S_P_Resp2_Lp1_Brut_01.07.2013.csv", header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE)
  wea_tot           <- read.table("4wea_tot_27.08.2013_test.csv", header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE)  # column names: plot, day, month, year, subplot, temp, vwc  
  wea_con           <- read.table("5wea_con_27.08.2013_test.csv", header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE)  # column names: plot, day, month, year, measurement, disturb, temp, vwc. Note: "measurement" is the measurement number (e.g. 21-35) - should we call this collar number? 
  wea_par           <- read.table("65wea_par_27.08.2013_test.csv", header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE)  # column names: plot, day, month, year, measurement, temp, vwc
  
  # once you have added the temperature and vwc datasets here, you can uncomment the code on lines 35-39, 87-90, 150-153 
  # and comment out the code that assings random values (raw_parsr$temp <- 25.8 and raw_parsr$vwc <- 50).
  
  
#### Soil Respiration ####
## Total soil respiration 
# Rename columns. Note: ";Plot" in raw files is equivalent to "measurement" in the files that feed into the R GEM functions.
  colnames(raw_totsr) <- c("measurement", "recno", "day", "month", "hour", "min", "co2ref", "unused1", "unused2", "inputA", "inputB", "inputC", "inputD", "time", "inputF", "inputG", "inputH", "atmp", "probetype")

# Add air temperature (temp), volumetric water content (vwc), and chamber depth (depth) to the raw control data (raw_consr)
  wea_tot$codew     <- paste(wea_tot$subplot, wea_tot$day, wea_tot$month, wea_tot$year, sep=".") # measurement is wrong!!!
  raw_totsr$year    <- 2013  # Should we request this from raw files? or just let users change it here?
  raw_totsr$codew   <- paste(raw_totsr$measurement, raw_totsr$day, raw_totsr$month, raw_totsr$year, sep=".")
  raw_totsr         <- sqldf("SELECT r.*, w.temp, w.vwc FROM raw_totsr r JOIN wea_tot w  ON w.codew = r.codew")
  raw_totsr$temp    <- 25    
  raw_totsr$vwc     <- 50    
  raw_totsr$depth   <- 4.5   # !!!!! Attention !!!! this needs to go in sqldf L42
  
  # ATTENTION: apply missing pressures and temperatures functions from soilrespiration_aux-functions.R to EGM-raw-to-db.R
        
## estimate flux for each measurement
  
  # Add SANITY CHECKS: plot each flux batch, data sanity checks. Linear fit, estimate r2 quality check. see Chris code.
  
# get unique identifyer for each measurement
  raw_totsr$id <- as.factor(paste(raw_totsr$measurement, raw_totsr$day, raw_totsr$month, sep="."))
  uid <- unique(raw_totsr$id)
  Res <- data.frame()
  
  for (i in uid) {
    ida      <- subset(raw_totsr, subset=(id == i))
    ten_co2  <- tail(ida$co2ref, n=10) 
    ten_time <- tail(ida$time, n=10)
    C10      <- tail(ten_co2, n=1)                                                   # last CO2 measurement of last 10 measurements
    C1       <- head(ten_co2, n=1)                                                   # first CO2 measurement of last 10 measurements
    t10      <- tail(ten_time, n=1)                                                  # last time step of 10 last measurements
    t1       <- head(ten_time, n=1)                                                  # first time step of 10 last measurements
    P        <- tail(ida$atmp, n=1)                                                  # ambient pressure at t10 (mb)
    Ta       <- tail(ida$temp, n=1)                                                  # air temp at t10 (deg C)
    Vd       <- 0.0012287                                                            # m3 (constant)
    A        <- 0.00950                                                              # m2 (constant)
    Ru       <- 8.31432                                                              # J mol-1 K-1 (constant)
    flux     <- ((C10 - C1)/(t10 - t1)) * (P/(Ta + 273.15))*(Vd/A)*((44.01*0.36)/Ru) # CO2 efflux (g CO2 m-2 h-1)
    new      <- data.frame(i, flux)
    Res      <- rbind(Res, new)
  }
  colnames(Res) <- c("uid", "flux")
  #TO DO: figure out how fluxcorr fits into this.


# build the new data frame
  Res$year             <- 2013
  Res$plot             <- 1.1  
  Res$co2ref           <- 0/0
  temp                 <- sqldf("SELECT DISTINCT id, measurement, month, temp, vwc, depth FROM raw_totsr")
  Restot               <- sqldf("SELECT r.*, t.measurement, t.month, t.temp, t.vwc, t.depth FROM Res r JOIN temp t ON r.uid = t.id") 
  Restotall            <- data.frame(Restot$year, Restot$month, Restot$plot, Restot$measurement, Restot$co2ref, Restot$temp, Restot$vwc, Restot$depth, Restot$flux)
  colnames(Restotall)  <- c("year", "month", "plot", "measurement", "co2ref", "temperature", "vwc", "depth", "flux") 

# save to current directory  
  setwd("C:/Users/Cecile/Dropbox/Carbon_Use_Efficieny_R/testing")
  write.csv(Restotall, file="Restotallsam.csv")

  # Colums in Data Frame ""data.rest"" must be arranged in the following way: 
  # [1] Year; [2] Month; [3] Plot; [4] Point; [5] UNUSED; [6] Temperature; [7] UNUSED; [8] chamber depth; [9] Flux; 
  # Seb called it "point", I renamed it "measurement"
    
## Soil respiration control
  colnames(raw_consr) <- c("measurement", "recno", "day", "month", "hour", "min", "co2ref", "unused1", "unused2", "inputA", "inputB", "inputC", "inputD", "time", "inputF", "inputG", "inputH", "atmp", "probetype")
  
# Add air temperature (temp), volumetric water content (vwc), and chamber depth (depth) to the raw control data (raw_consr)
  #wea_con$codew     <- paste(wea_con$measurement, wea_con$day, wea_con$month, wea_con$year, sep=".")
  raw_consr$year    <- 2013  # we should request year in the raw files.
  raw_consr$codew   <- paste(raw_consr$measurement, raw_consr$day, raw_consr$month, raw_consr$year, sep=".")
  #raw_consr         <- sqldf("SELECT r.*, w.temp, w.vwc FROM raw_consr r JOIN wea_con w  ON w.codew = r.codew")
  
  raw_consr$temp    <- 25.8 # to delete when the code above is working   
  raw_consr$vwc     <- 50   # to delete when the code above is working
  raw_consr$depth   <- 4.5  # do we need this?
  
## estimate flux for each measurement  
# get unique identifyer for each measurement
  raw_consr$id <- as.factor(paste(raw_consr$measurement, raw_consr$day, raw_consr$month, sep="."))
  uid          <- unique(raw_consr$id)
  
  Res <- data.frame()
  for (i in uid) {
    ida      <- subset(raw_consr, subset=(id == i))
    ten_co2  <- tail(ida$co2ref, n=10)
    ten_time <- tail(ida$time, n=10)
    C10      <- tail(ten_co2, n=1)                                                   # last CO2 measurement of last 10 measurements
    C1       <- head(ten_co2, n=1)                                                   # first CO2 measurement of last 10 measurements
    t10      <- tail(ten_time, n=1)                                                  # last time step of 10 last measurements
    t1       <- head(ten_time, n=1)                                                  # first time step of 10 last measurements
    P        <- tail(ida$atmp, n=1)                                                  # ambient pressure at t10 (mb)
    Ta       <- tail(ida$temp, n=1)                                                  # air temp at t10 (deg C)
    Vd       <- 0.0012287                                                            # m3 (constant)
    A        <- 0.00950                                                              # m2 (constant)
    Ru       <- 8.31432                                                              # J mol-1 K-1 (constant)
    flux     <- ((C10 - C1)/(t10 - t1)) * (P/(Ta + 273.15))*(Vd/A)*((44.01*0.36)/Ru) # CO2 efflux (g CO2 m-2 h-1)
    new      <- data.frame(i, flux)
    Res      <- rbind(Res, new)
  }
  colnames(Res) <- c("uid", "flux")
  #
  
# build the new data frame
  Res$year             <- 2013
  Res$plot             <- 1.1 
  Res$co2ref           <- 0/0
# get disturbance code  
  po                   <- c(21:30)                 
  di                   <- c(2,2,2,2,2,1,1,1,1,1)   # 1 = no disturbance, 2 = disturbed
  dist                 <- data.frame(po, di)
  raw_consr            <- sqldf("SELECT t.*, d.di FROM raw_consr t JOIN dist d  ON d.po = t.measurement") 
  temp                 <- sqldf("SELECT DISTINCT id, measurement, month, temp, vwc, depth, di FROM raw_consr")
  Rescon               <- sqldf("SELECT r.*, t.measurement, t.month, t.temp, t.vwc, t.depth, t.di FROM Res r JOIN temp t ON r.uid = t.id") 
  colnames(Rescon)     <- c("id", "flux", "year", "plot", "co2ref", "measurement", "month", "temperature", "vwc", "depth", "dist") 
  Resconall            <- data.frame(Rescon$year, Rescon$month, Rescon$plot, Rescon$measurement, Rescon$dist, Rescon$co2ref, Rescon$temperature, Rescon$vwc, Rescon$depth, Rescon$flux)
  colnames(Resconall)  <- c("year", "month", "plot", "measurement", "disturbance", "co2ref", "temperature", "vwc", "depth", "flux") 
  
  # save to current directory  
  write.csv(Resconall, file="Resconallsam.csv")  
  
  # Colums in Data Frame ""data.resc"" must be arranged in the following way: 
  # [1] Year; [2] Month; [3] Plot; [4-6] UNUSED; [7] Temperature; [8] UNUSED; [9] chamber depth; [10] Flux;
  # I replaced "point" by "measurement"
  

## Soil respiration partitionning 
  # Comment from the team: there are different ways of doing this. This is a simple approach, we should explore other approaches.
  
  colnames(raw_parsr) <- c("measurement", "recno", "day", "month", "hour", "min", "co2ref", "unused1", "unused2", "inputA", "inputB", "inputC", "inputD", "time", "inputF", "inputG", "inputH", "atmp", "probetype")

# Add air temperature (temp), volumetric water content (vwc), and chamber depth (depth) to the raw partitioning data (raw_parsr)
  #wea_par$codew     <- paste(wea_par$measurement, wea_par$day, wea_par$month, wea_par$year, sep=".")
  raw_totsr$year    <- 2013  # we should request year in the raw files.
  raw_parsr$codew   <- paste(raw_parsr$measurement, raw_parsr$day, raw_parsr$month, raw_parsr$year, sep=".")
  #raw_parsr         <- sqldf("SELECT r.*, w.temp, w.vwc FROM raw_parsr r JOIN wea_par w  ON w.codew = r.codew")
  
  raw_parsr$temp    <- 25.8  # to delete when the code above is working    
  raw_parsr$vwc     <- 50    # to delete when the code above is working
  raw_parsr$depth   <- 4.5   # do we need this?
  
# estimate flux for each measurement  
# get unique identifyer for each measurement
  raw_parsr$id <- as.factor(paste(raw_parsr$measurement, raw_parsr$day, raw_parsr$month, sep="."))
  uid          <- unique(raw_parsr$id)
  
  Res <- data.frame()
  for (i in uid) {
    ida      <- subset(raw_parsr, subset=(id == i))
    ten_co2  <- tail(ida$co2ref, n=10)
    ten_time <- tail(ida$time, n=10)
    C10      <- tail(ten_co2, n=1)                                                    # last CO2 measurement of last 10 measurements
    C1       <- head(ten_co2, n=1)                                                    # first CO2 measurement of last 10 measurements
    t10      <- tail(ten_time, n=1)                                                   # last time step of 10 last measurements
    t1       <- head(ten_time, n=1)                                                   # first time step of 10 last measurements
    P        <- tail(ida$atmp, n=1)                                                   # ambient pressure at t10 (mb)
    Ta       <- tail(ida$temp, n=1)                                                     # air temp at t10 (deg C)
    Vd       <- 0.0012287                                                             # m3 (constant)
    A        <- 0.00950                                                               # m2 (constant)
    Ru       <- 8.31432                                                               # J mol-1 K-1 (constant)
    flux     <- ((C10 - C1)/(t10 - t1)) * (P/(Ta + 273.15))*(Vd/A)*((44.01*0.36)/Ru)  # CO2 efflux (g CO2 m-2 h-1)
    new      <- data.frame(i, flux)
    Res      <- rbind(Res, new)
  }
  colnames(Res) <- c("uid", "flux")
  
# build the new data frame
  Res$year             <- 2013
  Res$plot             <- 1.1
  Res$co2ref           <- 0/0
# get partitioning code
  pt                   <- c(1:20)                            
  ar                   <- c(1,2,3,4,1,2,3,4,1,2,3,4,1,2,3,4,1,2,3,4)
  area1                <- data.frame(pt, ar)
  pt                   <- c(31:46)                            
  ar                   <- c(1,2,3,4,1,2,3,4,1,2,3,4,1,2,3,4)
  area2                <- data.frame(pt, ar)
  area                 <- rbind(area1, area2)
  raw_parsr            <- sqldf("SELECT t.*, a.ar, a.pt FROM raw_parsr t JOIN area a  ON a.pt = t.measurement")
  
# combine raw_parsr and Res
  short                <- sqldf("SELECT DISTINCT id, measurement, month, temp, vwc, depth, ar, pt FROM raw_parsr")
  Respar               <- sqldf("SELECT r.*, t.pt, t.ar, t.month, t.temp, t.vwc, t.depth FROM Res r JOIN short t ON r.uid = t.id") 
  Resparall            <- data.frame(Respar$year, Respar$month, Respar$plot, Respar$ar,  Respar$pt, Respar$temp, Respar$vwc, Respar$depth, Respar$flux)
  colnames(Resparall)  <- c("year", "month", "plot", "area", "measurement", "temperature", "vwc", "depth", "flux") 
  
# save to current directory  
  write.csv(Resparall, file="Resparallsam.csv") 
  
  # Colums in Data Frame ""data.resp"" must be arranged in the following way:
  # [1] Year; [2] Month; [3] Plot; [4] num; [5] UNUSED; [6] Temperature; [7] UNUSED; [8] chamber depth; [9] Flux;

  ## style guide on how to lay out R code: http://google-styleguide.googlecode.com/svn/trunk/Rguide.xml#indentation 