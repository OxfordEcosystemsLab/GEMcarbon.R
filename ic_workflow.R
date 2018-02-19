# call function
setwd("~/Github/GEMcarbon.R")
source("~/Github/GEMcarbon.R/ingrowth_cores_20180213.R")
source("~/Github/GEMcarbon.R/functions.r")

setwd("~/Github/gemcarbon_data/raw_data_ingemdb_forELDS")
# rename plots
# datafile$plot_code <- revalue(rawic1$plot_code, c("TRU-4" = "TRU-04", "DC1" = "DAN-04", "DC2" = "DAN-05", "BZ11" = "BLZ-11", "BZ12" = "BLZ-12", "BZ22" = "BLZ-22", "BZ21" = "BLZ-21", "OP" = "OP"))

# read in data
datafile1 <- read.csv("ic_20180129.csv", sep=",", header=T, stringsAsFactors=FALSE) 
datafile <- subset(datafile1, plot_code %in% c("ALP-11", "ALP-12"))

                                               "BOB-04", "BOB-02", "BOB-03", "BOB-01", "ALP-11", "ALP-12", "BLZ-11", "BLZ-12", "BLZ-21", "BLZ-22", "KEN-01", "KEN-02", "TAN-01", "TAN-02", "ANK-01", "ANK-02", "ANK-03", "BOB-05", "BOB-06",
                                               "KOG-02", "KOG-03", "KOG-04", "KOG-05", "KOG-06", "MNG-03", "MNG-04", "LPG-01", "LPG-02", "IVI-01", "IVI-02", "DAN-04", "DAN-05", "MLA-01", "MLA-02", "OP", "SAF-01", "SAF-02", "SAF-03",
                                               "SAF-04", "SAF-05", "NXV-02", "NXV-01", "STQ-08", "STQ-11", "STB-08", "STB-12", "STJ-01", "STO-03", "STJ-05", "STO-06", "STN-06", "STN-09", "STD-05", "STD-11", "STN-02", "STO-07", "STJ-04",
                                               "STL-10", "STL-09", "STD-10", "STN-03", "STN-04", "ACJ-01", "ESP-01", "SPD-01", "SPD-02", "TAM-05", "TAM-06", "TAM-09", "TRU-04", "PAN-03", "PAN-02"))

# "WAY-01"

# Define IC diameter
datafile = datafile %>% mutate(tubed = if plot = c(,,,,,) then 11.5, else 14)



#"STQ-08" "STQ-11" "STB-08" "STB-12" "STJ-01" "STO-03" "STJ-05" 
#"STO-06" "STN-06" "STJ-04" "STL-10" "STN-09" "STD-05" "STD-11" 
#"STN-02" "STO-07" "STL-09" "STD-10" "STN-03" "STN-04"

# These are missing from the data that goes into the model:
# STB-12, STJ-01, STN-02, STN-09, STQ-08  

write.csv(data4, file="ic_STN04.csv")

datafile = set_df_coltypes(datafile, ic_column_types)

xyz <- NPProot_ic(subset(datafile, plot_code %in% c("STB-08"), ret_type = "list")) #, "BOB-01", "BOB-02", "SAF-05", "SAF-01", "SAF-02", "TAM-06"
xy <- xyz[["three_monthly"]]
xy <- data.frame(xy)
head(xy)
