# call function
setwd("~/Github/GEMcarbon.R")
source("~/Github/GEMcarbon.R/ingrowth_cores_Oct17.R")

# read in data
rawic1 <- read.csv("xxxx Santarem IC data xxx.csv", sep=",", header=T, stringsAsFactors=FALSE) 


# rename plots
# rawic1$plot_code <- revalue(rawic1$plot_code, c("TRU-4" = "TRU-04", "DC1" = "DAN-04", "DC2" = "DAN-05", "BZ11" = "BLZ-11", "BZ12" = "BLZ-12", "BZ22" = "BLZ-22", "BZ21" = "BLZ-21", "OP" = "OP"))

# clean NA
rawic1[rawic1 == 'NA'] <- NA

datafile = rawic
plotname = "STN-04"
logmodel = T
fine_root_cor = "Default" 
tubed = 0.07 
remove_stock_meas = T 
ret = "monthly.means.ts"
ret_type = "list" 

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
