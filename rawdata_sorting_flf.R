# October 2016
# Cécile Girardin: I use the following script to re-format raw xl sheets that come from the field. 

# load packages
library(sqldf)

## Fine litterfall.

### Read test data:
setwd("~/Desktop/data_sorting/flf")
list.files()

way01  <- read.table("~/Desktop/data_sorting/ic/WAY_IC_2013.csv", sep=",", header=T)

# GET MET DATA!

#way01_wea <- read.table("~/Desktop/data_sorting/ic/.csv", sep=",", header=T)
esp01  <- read.table("~/Desktop/data_sorting/ic/ESP_IC_2013.csv", sep=",", header=T)
#esp01_wea <- read.table("~/Desktop/data_sorting/ic/.csv", sep=",", header=T)
spd02  <- read.table("~/Desktop/data_sorting/ic/SP1500_IC_2013.csv", sep=",", header=T)
#spd02_wea <- read.table("~/Desktop/data_sorting/ic/.csv", sep=",", header=T)
spd01  <- read.table("~/Desktop/data_sorting/ic/SP1750_IC_2013.csv", sep=",", header=T)
#spd01_wea <- read.table("~/Desktop/data_sorting/ic/.csv", sep=",", header=T)
tam05a <- read.table("~/Desktop/data_sorting/ic/TAM05_IC_2012.csv", sep=",", header=T)
#tam05a_wea <- read.table("~/Desktop/data_sorting/ic/.csv", sep=",", header=T)
tam05b <- read.table("~/Desktop/data_sorting/ic/TAM05_IC_2013.csv", sep=",", header=T)
#tam05b_wea <- read.table("~/Desktop/data_sorting/ic/.csv", sep=",", header=T)
tam05c <- read.table("~/Desktop/data_sorting/ic/TAM05_IC_2014.csv", sep=",", header=T)
#tam05c_wea <- read.table("~/Desktop/data_sorting/ic/.csv", sep=",", header=T)
tam06a <- read.table("~/Desktop/data_sorting/ic/TAM06_IC_2012.csv", sep=",", header=T)
#tam06a_wea <- read.table("~/Desktop/data_sorting/ic/.csv", sep=",", header=T)
tam06b <- read.table("~/Desktop/data_sorting/ic/TAM06_IC_2013.csv", sep=",", header=T)
#tam06b_wea <- read.table("~/Desktop/data_sorting/ic/.csv", sep=",", header=T)
tam06c <- read.table("~/Desktop/data_sorting/ic/TAM06_IC_2014.csv", sep=",", header=T)
#tam06c_wea <- read.table("~/Desktop/data_sorting/ic/.csv", sep=",", header=T)
tam09a <- read.table("~/Desktop/data_sorting/ic/TAM09_IC_2012.csv", sep=",", header=T)
#tam09a_wea <- read.table("~/Desktop/data_sorting/ic/.csv", sep=",", header=T)
tam09b <- read.table("~/Desktop/data_sorting/ic/TAM09_IC_2013.csv", sep=",", header=T)
#tam09b_wea <- read.table("~/Desktop/data_sorting/ic/.csv", sep=",", header=T)
stock_2013A <- read.table("~/Desktop/data_sorting/ic/stock_2013.csv", sep=",", header=T)
#tam09_2014 <- read.table("~/Desktop/data_sorting/ic/xxxxxxxxxxxxx.csv", sep=",", header=T)

head(way01)
way01$plot_code                   <- "WAY-01"
way01$year                        <- way01$YY                      
way01$month                       <- way01$MM                        
way01$day                         <- way01$DD  
way01$ingrowth_core_num           <- way01$IC.Num         
way01$is_stock                    <- NA   
way01$ingrowth_core_litterfall_g  <- way01$LitWeigth 
way01$soil_humidity_pcnt          <- "GET THIS"       
way01$soil_temperature_c          <- "GET THIS"   
way01$ol_layer_depth_cm	          <- "GET THIS"   
way01$ml_layer_depth_cm	          <- "GET THIS"   
way01$time_step                   <- NA     
way01$time_step_minutes           <- way01$TIME.MIN  
way01$ol_under_2mm_g              <- way01$OL..2    
way01$ol_above_2mm_g              <- way01$OL.2 
way01$ml_under_2mm_g              <- way01$ML..2
way01$ml_above_2mm_g              <- way01$ML.2               
way01$ol_2to3_mm_g	              <- NA      
way01$ml_2to3_mm_g	              <- NA       
way01$ol_3to4_mm_g	              <- NA       
way01$ml_3to4_mm_g	              <- NA      
way01$ol_4to5_mm_g	              <- NA
way01$ml_4to5_mm_g	              <- NA      
way01$ol_above_5mm_g	            <- NA         
way01$ml_above_5mm_g	            <- NA        
way01$quality_code                <- "good"          
way01$comments                    <- NA

# remove these columns
way01$Plot      <- NULL
way01$TypeMeas  <- NULL
way01$IC.Num    <- NULL
way01$TIME.MIN  <- NULL
way01$DD        <- NULL
way01$MM        <- NULL
way01$YY        <- NULL
way01$Id        <- NULL                     
way01$OL..2     <- NULL
way01$ML..2     <- NULL
way01$OL.2      <- NULL
way01$ML.2      <- NULL
way01$LitWeigth <- NULL
way01$X         <- NULL

head(way01)


eltr_ic <- rbind(way01, esp01, spd02, spd01, tam05a, tam05b, tam05c, tam06a, tam06b, tam06c, tam09a, tam09b, stock_2013)
write.csv(eltr_ic, file="eltr_ic_2012to2014.csv") 
