# October 2016
# Cécile Girardin: I use the following script to re-format raw xl sheets that come from the field

# load packages
library(sqldf)

## Ingrowth Cores:

### Read test data:
setwd("~/Desktop/data_sorting/ic")
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

head(esp01) 

esp01$plot_code                   <- "ESP-01"
esp01$year                        <- esp01$YY                       
esp01$month                       <- esp01$MM                        
esp01$day                         <- esp01$DD  
esp01$ingrowth_core_num           <- esp01$IC.Num         
esp01$is_stock                    <- NA   
esp01$ingrowth_core_litterfall_g  <- esp01$LitWeigth 
esp01$soil_humidity_pcnt          <- "GET THIS"       
esp01$soil_temperature_c          <- "GET THIS"   
esp01$ol_layer_depth_cm           <- "GET THIS"   
esp01$ml_layer_depth_cm	          <- "GET THIS"   
esp01$time_step                   <- NA     
esp01$time_step_minutes           <- esp01$TIME.MIN  
esp01$ol_under_2mm_g              <- esp01$OL..2    
esp01$ol_above_2mm_g              <- esp01$OL.2 
esp01$ml_under_2mm_g              <- esp01$ML..2
esp01$ml_above_2mm_g              <- esp01$ML.2               
esp01$ol_2to3_mm_g	              <- NA      
esp01$ml_2to3_mm_g	              <- NA       
esp01$ol_3to4_mm_g	              <- NA       
esp01$ml_3to4_mm_g	              <- NA      
esp01$ol_4to5_mm_g	              <- NA
esp01$ml_4to5_mm_g	              <- NA      
esp01$ol_above_5mm_g	            <- NA         
esp01$ml_above_5mm_g	            <- NA        
esp01$quality_code                <- "good"          
esp01$comments                    <- NA

# remove these columns
esp01$Plot      <- NULL
esp01$TypeMeas  <- NULL
esp01$IC.Num    <- NULL
esp01$TIME.MIN  <- NULL
esp01$DD        <- NULL
esp01$MM        <- NULL
esp01$YY        <- NULL
esp01$Id        <- NULL                     
esp01$OL..2     <- NULL
esp01$ML..2     <- NULL
esp01$OL.2      <- NULL
esp01$ML.2      <- NULL
esp01$LitWeigth <- NULL
esp01$X         <- NULL

head(esp01)

head(spd02)

spd02$plot_code                   <- "SPD-02"
spd02$year                        <- spd02$YY                       
spd02$month                       <- spd02$MM                        
spd02$day                         <- spd02$DD  
spd02$ingrowth_core_num           <- spd02$IC.Num         
spd02$is_stock                    <- NA   
spd02$ingrowth_core_litterfall_g  <- spd02$LitWeigth 
spd02$soil_humidity_pcnt          <- "GET THIS"       
spd02$soil_temperature_c          <- "GET THIS"   
spd02$ol_layer_depth_cm           <- "GET THIS"   
spd02$ml_layer_depth_cm            <- "GET THIS"   
spd02$time_step                   <- NA     
spd02$time_step_minutes           <- spd02$TIME.MIN  
spd02$ol_under_2mm_g              <- spd02$R.W...2OL    
spd02$ol_above_2mm_g              <- spd02$R.W..2OL 
spd02$ml_under_2mm_g              <- spd02$R.W...2ML
spd02$ml_above_2mm_g              <- spd02$R.W..2ML               
spd02$ol_2to3_mm_g	              <- NA      
spd02$ml_2to3_mm_g	              <- NA       
spd02$ol_3to4_mm_g	              <- NA       
spd02$ml_3to4_mm_g	              <- NA      
spd02$ol_4to5_mm_g	              <- NA
spd02$ml_4to5_mm_g	              <- NA      
spd02$ol_above_5mm_g	            <- NA         
spd02$ml_above_5mm_g	            <- NA        
spd02$quality_code                <- "good"          
spd02$comments                    <- NA

# remove these columns
spd02$Plot          <- NULL
spd02$TypeMeas      <- NULL
spd02$IC.Num        <- NULL
spd02$TIME.MIN      <- NULL
spd02$DD            <- NULL
spd02$MM            <- NULL
spd02$YY            <- NULL
spd02$Id            <- NULL                     
spd02$R.W...2OL     <- NULL
spd02$R.W...2ML     <- NULL
spd02$R.W..2OL      <- NULL
spd02$R.W..2ML      <- NULL
spd02$LitWeigth     <- NULL
spd02$X             <- NULL
spd02$X.1           <- NULL
spd02$X.2           <- NULL

head(spd02)


head(spd01)

spd01$plot_code                   <- "SPD-01"
spd01$year                        <- spd01$YY                       
spd01$month                       <- spd01$MM                        
spd01$day                         <- spd01$DD  
spd01$ingrowth_core_num           <- spd01$IC.Num         
spd01$is_stock                    <- NA 
spd01$ingrowth_core_litterfall_g  <- spd01$LitWeigth 
spd01$soil_humidity_pcnt          <- "GET THIS"       
spd01$soil_temperature_c          <- "GET THIS"   
spd01$ol_layer_depth_cm           <- "GET THIS"   
spd01$ml_layer_depth_cm           <- "GET THIS"   
spd01$time_step                   <- NA     
spd01$time_step_minutes           <- spd01$TIME.MIN  
spd01$ol_under_2mm_g              <- spd01$R.W...2OL   
spd01$ol_above_2mm_g              <- spd01$R.W..2OL 
spd01$ml_under_2mm_g              <- spd01$R.W...2ML
spd01$ml_above_2mm_g              <- spd01$R.W..2ML               
spd01$ol_2to3_mm_g                <- NA      
spd01$ml_2to3_mm_g	              <- NA       
spd01$ol_3to4_mm_g	              <- NA       
spd01$ml_3to4_mm_g	              <- NA      
spd01$ol_4to5_mm_g	              <- NA
spd01$ml_4to5_mm_g	              <- NA      
spd01$ol_above_5mm_g	            <- NA         
spd01$ml_above_5mm_g	            <- NA        
spd01$quality_code                <- "good"          
spd01$comments                    <- NA

# remove these columns
spd01$Plot      <- NULL
spd01$TypeMeas  <- NULL
spd01$IC.Num    <- NULL
spd01$TIME.MIN  <- NULL
spd01$DD        <- NULL
spd01$MM        <- NULL
spd01$YY        <- NULL
spd01$Id        <- NULL                     
spd01$R.W...2OL <- NULL
spd01$R.W...2ML <- NULL
spd01$R.W..2OL  <- NULL
spd01$R.W..2ML  <- NULL
spd01$LitWeigth <- NULL
spd01$X         <- NULL
spd01$X.1       <- NULL
spd01$X.2       <- NULL

head(spd01)

head(tam05a)
tam05a$plot_code                   <- "TAM-05"
tam05a$year                        <- tam05a$YY                       
tam05a$month                       <- tam05a$MM                        
tam05a$day                         <- tam05a$DD  
tam05a$ingrowth_core_num           <- tam05a$NumbIC         
tam05a$is_stock                    <- NA 
tam05a$ingrowth_core_litterfall_g  <- NA 
tam05a$soil_humidity_pcnt          <- "GET THIS"       
tam05a$soil_temperature_c          <- "GET THIS"   
tam05a$ol_layer_depth_cm           <- "GET THIS"   
tam05a$ml_layer_depth_cm           <- "GET THIS"   
tam05a$time_step                   <- NA     
tam05a$time_step_minutes           <- tam05a$TIME.MIN  
tam05a$ol_under_2mm_g              <- NA   
tam05a$ol_above_2mm_g              <- NA
tam05a$ml_under_2mm_g              <- NA
tam05a$ml_above_2mm_g              <- NA               
tam05a$ol_2to3_mm_g                <- NA      
tam05a$ml_2to3_mm_g                <- NA       
tam05a$ol_3to4_mm_g	              <- NA       
tam05a$ml_3to4_mm_g	              <- NA      
tam05a$ol_4to5_mm_g	              <- NA
tam05a$ml_4to5_mm_g	              <- NA      
tam05a$ol_above_5mm_g	            <- tam05a$P.IC          
tam05a$ml_above_5mm_g	            <- NA        
tam05a$quality_code                <- "not_sure"          
tam05a$comments                    <- "Roots not separated by diameter class: all roots were weighed under one heading - peso ic."

# remove these columns
tam05a$Plot      <- NULL
tam05a$TypeMeans <- NULL
tam05a$NumbIC    <- NULL
tam05a$TIME.MIN  <- NULL
tam05a$DD        <- NULL
tam05a$MM        <- NULL
tam05a$YY        <- NULL
tam05a$ID        <- NULL                     
tam05a$P.IC      <- NULL

head(tam05a)

head(tam05b)

tam05b$plot_code                   <- "TAM-05"
tam05b$year                        <- tam05b$YY                       
tam05b$month                       <- tam05b$MM                        
tam05b$day                         <- tam05b$DD  
tam05b$ingrowth_core_num           <- tam05b$IC.Num         
tam05b$is_stock                    <- NA   
tam05b$ingrowth_core_litterfall_g  <- tam05b$LitWeigth 
tam05b$soil_humidity_pcnt          <- "GET THIS"       
tam05b$soil_temperature_c          <- "GET THIS"   
tam05b$ol_layer_depth_cm           <- "GET THIS"   
tam05b$ml_layer_depth_cm           <- "GET THIS"   
tam05b$time_step                   <- NA
tam05b$time_step_minutes           <- tam05b$TIME.MIN  
tam05b$ol_under_2mm_g              <- tam05b$R.W...2    
tam05b$ol_above_2mm_g              <- NA 
tam05b$ml_under_2mm_g              <- NA
tam05b$ml_above_2mm_g              <- NA               
tam05b$ol_2to3_mm_g                <- tam05b$R.W.2.3      
tam05b$ml_2to3_mm_g	              <- NA       
tam05b$ol_3to4_mm_g	              <- tam05b$R.W.3.4       
tam05b$ml_3to4_mm_g	              <- NA      
tam05b$ol_4to5_mm_g	              <- tam05b$R.W.4.5
tam05b$ml_4to5_mm_g	              <- NA      
tam05b$ol_above_5mm_g	            <- tam05b$R.W..5        
tam05b$ml_above_5mm_g	            <- NA        
tam05b$quality_code                <- "good"          
tam05b$comments                    <- tam05b$notes

# remove these columns
tam05b$Plot      <- NULL
tam05b$TypeMeas  <- NULL
tam05b$IC.Num    <- NULL
tam05b$TIME.MIN  <- NULL
tam05b$DD        <- NULL
tam05b$MM        <- NULL
tam05b$YY        <- NULL
tam05b$Id        <- NULL                     
tam05b$R.W...2   <- NULL
tam05b$R.W.2.3   <- NULL
tam05b$R.W.3.4   <- NULL
tam05b$R.W.4.5   <- NULL
tam05b$R.W..5    <- NULL
tam05b$LitWeigth <- NULL
tam05b$X         <- NULL
tam05b$X.1       <- NULL 
tam05b$notes     <- NULL

head(tam05b)
                    
head(tam05c)

tam05c$plot_code                   <- "TAM-05"
tam05c$year                        <- tam05c$YY                       
tam05c$month                       <- tam05c$MM                        
tam05c$day                         <- tam05c$DD  
tam05c$ingrowth_core_num           <- tam05c$IC.Num         
tam05c$is_stock                    <- NA   
tam05c$ingrowth_core_litterfall_g  <- tam05c$LitWeigth 
tam05c$soil_humidity_pcnt          <- "GET THIS"       
tam05c$soil_temperature_c          <- "GET THIS"   
tam05c$ol_layer_depth_cm           <- "GET THIS"   
tam05c$ml_layer_depth_cm           <- "GET THIS"   
tam05c$time_step                   <- NA     
tam05c$time_step_minutes           <- tam05c$TIME.MIN  
tam05c$ol_under_2mm_g              <- NA    
tam05c$ol_above_2mm_g              <- NA 
tam05c$ml_under_2mm_g              <- tam05c$R.W...2ML
tam05c$ml_above_2mm_g              <- tam05c$R.W..2ML               
tam05c$ol_2to3_mm_g                <- NA      
tam05c$ml_2to3_mm_g                <- NA       
tam05c$ol_3to4_mm_g	               <- NA       
tam05c$ml_3to4_mm_g	               <- NA      
tam05c$ol_4to5_mm_g	               <- NA
tam05c$ml_4to5_mm_g	               <- NA      
tam05c$ol_above_5mm_g	             <- NA       
tam05c$ml_above_5mm_g	             <- NA        
tam05c$quality_code                <- "not_sure"          
tam05c$comments                    <- NA

# remove these columns
tam05c$Plot      <- NULL
tam05c$TypeMeas  <- NULL
tam05c$IC.Num    <- NULL
tam05c$TIME.MIN  <- NULL
tam05c$DD        <- NULL
tam05c$MM        <- NULL
tam05c$YY        <- NULL
tam05c$Id        <- NULL                     
tam05c$R.W...2ML <- NULL
tam05c$R.W..2ML  <- NULL
tam05c$LitWeigth <- NULL

head(tam05c)

head(tam06a)

tam06a$plot_code                   <- "TAM-06"
tam06a$year                        <- tam06a$YY                       
tam06a$month                       <- tam06a$MM                        
tam06a$day                         <- tam06a$DD  
tam06a$ingrowth_core_num           <- tam06a$NumbIC         
tam06a$is_stock                    <- NA 
tam06a$ingrowth_core_litterfall_g  <- NA 
tam06a$soil_humidity_pcnt          <- "GET THIS"       
tam06a$soil_temperature_c          <- "GET THIS"   
tam06a$ol_layer_depth_cm           <- "GET THIS"   
tam06a$ml_layer_depth_cm           <- "GET THIS"   
tam06a$time_step                   <- NA     
tam06a$time_step_minutes           <- tam06a$TIME.MIN  
tam06a$ol_under_2mm_g              <- NA   
tam06a$ol_above_2mm_g              <- NA
tam06a$ml_under_2mm_g              <- NA
tam06a$ml_above_2mm_g              <- NA               
tam06a$ol_2to3_mm_g                <- NA      
tam06a$ml_2to3_mm_g                <- NA       
tam06a$ol_3to4_mm_g                <- NA       
tam06a$ml_3to4_mm_g	               <- NA      
tam06a$ol_4to5_mm_g	               <- NA
tam06a$ml_4to5_mm_g	               <- NA      
tam06a$ol_above_5mm_g	             <- tam06a$P.IC          
tam06a$ml_above_5mm_g	             <- NA        
tam06a$quality_code                <- "not_sure"          
tam06a$comments                    <- "Roots not separated by diameter class: all roots were weighed under one heading - peso ic."

# remove these columns
tam06a$Plot      <- NULL
tam06a$TypeMeans <- NULL
tam06a$NumbIC    <- NULL
tam06a$TIME.MIN  <- NULL
tam06a$DD        <- NULL
tam06a$MM        <- NULL
tam06a$YY        <- NULL
tam06a$ID        <- NULL                     
tam06a$P.IC      <- NULL

head(tam06a)

head(tam06b)

tam06b$plot_code                   <- "TAM-06"
tam06b$year                        <- tam06b$YY                       
tam06b$month                       <- tam06b$MM                        
tam06b$day                         <- tam06b$DD  
tam06b$ingrowth_core_num           <- tam06b$IC.Num         
tam06b$is_stock                    <- NA   
tam06b$ingrowth_core_litterfall_g  <- tam06b$LitWeigth 
tam06b$soil_humidity_pcnt          <- "GET THIS"       
tam06b$soil_temperature_c          <- "GET THIS"   
tam06b$ol_layer_depth_cm           <- "GET THIS"   
tam06b$ml_layer_depth_cm           <- "GET THIS"   
tam06b$time_step                   <- NA     
tam06b$time_step_minutes           <- tam06b$TIME.MIN  
tam06b$ol_under_2mm_g              <- tam06b$R.W...2    
tam06b$ol_above_2mm_g              <- NA 
tam06b$ml_under_2mm_g              <- NA
tam06b$ml_above_2mm_g              <- NA               
tam06b$ol_2to3_mm_g                <- tam06b$R.W.2.3      
tam06b$ml_2to3_mm_g                <- NA       
tam06b$ol_3to4_mm_g	               <- tam06b$R.W.3.4       
tam06b$ml_3to4_mm_g	               <- NA      
tam06b$ol_4to5_mm_g	               <- tam06b$R.W.4.5
tam06b$ml_4to5_mm_g	               <- NA      
tam06b$ol_above_5mm_g	             <- tam06b$R.W..5        
tam06b$ml_above_5mm_g	             <- NA        
tam06b$quality_code                <- "good"          
tam06b$comments                    <- tam06b$notes

# remove these columns
tam06b$Plot      <- NULL
tam06b$TypeMeas  <- NULL
tam06b$IC.Num    <- NULL
tam06b$TIME.MIN  <- NULL
tam06b$DD        <- NULL
tam06b$MM        <- NULL
tam06b$YY        <- NULL
tam06b$Id        <- NULL                     
tam06b$R.W...2   <- NULL
tam06b$R.W.2.3   <- NULL
tam06b$R.W.3.4   <- NULL
tam06b$R.W.4.5   <- NULL
tam06b$R.W..5    <- NULL
tam06b$LitWeigth <- NULL
tam06b$X         <- NULL
tam06b$X.1       <- NULL
tam06b$notes     <- NULL

head(tam06b)

head(tam06c)

tam06c$plot_code                   <- "TAM-06"
tam06c$year                        <- tam06c$YY                       
tam06c$month                       <- tam06c$MM                        
tam06c$day                         <- tam06c$DD  
tam06c$ingrowth_core_num           <- tam06c$IC.Num         
tam06c$is_stock                    <- NA   
tam06c$ingrowth_core_litterfall_g  <- tam06c$LitWeigth 
tam06c$soil_humidity_pcnt          <- "GET THIS"       
tam06c$soil_temperature_c          <- "GET THIS"   
tam06c$ol_layer_depth_cm           <- "GET THIS"   
tam06c$ml_layer_depth_cm           <- "GET THIS"   
tam06c$time_step                   <- NA     
tam06c$time_step_minutes           <- tam06c$TIME.MIN  
tam06c$ol_under_2mm_g              <- NA    
tam06c$ol_above_2mm_g              <- NA 
tam06c$ml_under_2mm_g              <- tam06c$R.W...2ML
tam06c$ml_above_2mm_g              <- tam06c$R.W..2ML               
tam06c$ol_2to3_mm_g                <- NA      
tam06c$ml_2to3_mm_g                <- NA       
tam06c$ol_3to4_mm_g                <- NA       
tam06c$ml_3to4_mm_g	               <- NA      
tam06c$ol_4to5_mm_g	               <- NA
tam06c$ml_4to5_mm_g	               <- NA      
tam06c$ol_above_5mm_g	             <- NA       
tam06c$ml_above_5mm_g	             <- NA        
tam06c$quality_code                <- "not_sure"          
tam06c$comments                    <- NA

# remove these columns
tam06c$Plot      <- NULL
tam06c$TypeMeas  <- NULL
tam06c$IC.Num    <- NULL
tam06c$TIME.MIN  <- NULL
tam06c$DD        <- NULL
tam06c$MM        <- NULL
tam06c$YY        <- NULL
tam06c$Id        <- NULL                     
tam06c$R.W...2ML <- NULL
tam06c$R.W..2ML  <- NULL
tam06c$LitWeigth <- NULL

head(tam06c)

head(tam09a)

tam09a$plot_code                   <- "TAM-09"
tam09a$year                        <- tam09a$YY                       
tam09a$month                       <- tam09a$MM                        
tam09a$day                         <- tam09a$DD  
tam09a$ingrowth_core_num           <- tam09a$NumbIC         
tam09a$is_stock                    <- NA 
tam09a$ingrowth_core_litterfall_g  <- NA 
tam09a$soil_humidity_pcnt          <- "GET THIS"       
tam09a$soil_temperature_c          <- "GET THIS"   
tam09a$ol_layer_depth_cm           <- "GET THIS"   
tam09a$ml_layer_depth_cm           <- "GET THIS"   
tam09a$time_step                   <- NA     
tam09a$time_step_minutes           <- tam09a$TIME.MIN  
tam09a$ol_under_2mm_g              <- NA   
tam09a$ol_above_2mm_g              <- NA
tam09a$ml_under_2mm_g              <- NA
tam09a$ml_above_2mm_g              <- NA               
tam09a$ol_2to3_mm_g                <- NA      
tam09a$ml_2to3_mm_g                <- NA       
tam09a$ol_3to4_mm_g                <- NA       
tam09a$ml_3to4_mm_g                <- NA      
tam09a$ol_4to5_mm_g	               <- NA
tam09a$ml_4to5_mm_g	               <- NA      
tam09a$ol_above_5mm_g	             <- tam09a$P.IC          
tam09a$ml_above_5mm_g	             <- NA        
tam09a$quality_code                <- "good"          
tam09a$comments                    <- "Roots not separated by diameter class: all roots were weighed under one heading - peso ic."

# remove these columns
tam09a$Plot      <- NULL
tam09a$TypeMeans <- NULL
tam09a$NumbIC    <- NULL
tam09a$TIME.MIN  <- NULL
tam09a$DD        <- NULL
tam09a$MM        <- NULL
tam09a$YY        <- NULL
tam09a$ID        <- NULL                     
tam09a$P.IC      <- NULL

head(tam09a)

head(tam09b)

tam09b$plot_code                   <- "TAM-09"
tam09b$year                        <- tam09b$YY                       
tam09b$month                       <- tam09b$MM                        
tam09b$day                         <- tam09b$DD  
tam09b$ingrowth_core_num           <- tam09b$IC.Num         
tam09b$is_stock                    <- NA   
tam09b$ingrowth_core_litterfall_g  <- tam09b$LitWeigth 
tam09b$soil_humidity_pcnt          <- "GET THIS"       
tam09b$soil_temperature_c          <- "GET THIS"   
tam09b$ol_layer_depth_cm           <- "GET THIS"   
tam09b$ml_layer_depth_cm           <- "GET THIS"   
tam09b$time_step                   <- NA     
tam09b$time_step_minutes           <- tam09b$TIME.MIN  
tam09b$ol_under_2mm_g              <- tam09b$R.W...2    
tam09b$ol_above_2mm_g              <- NA 
tam09b$ml_under_2mm_g              <- NA
tam09b$ml_above_2mm_g              <- NA               
tam09b$ol_2to3_mm_g                <- tam09b$R.W.2.3      
tam09b$ml_2to3_mm_g                <- NA       
tam09b$ol_3to4_mm_g                <- tam09b$R.W.3.4       
tam09b$ml_3to4_mm_g	               <- NA      
tam09b$ol_4to5_mm_g	               <- tam09b$R.W.4.5
tam09b$ml_4to5_mm_g	               <- NA      
tam09b$ol_above_5mm_g	             <- tam09b$R.W..5        
tam09b$ml_above_5mm_g	             <- NA        
tam09b$quality_code                <- "good"          
tam09b$comments                    <- tam09b$notes

# remove these columns
tam09b$Plot      <- NULL
tam09b$TypeMeas  <- NULL
tam09b$IC.Num    <- NULL
tam09b$TIME.MIN  <- NULL
tam09b$DD        <- NULL
tam09b$MM        <- NULL
tam09b$YY        <- NULL
tam09b$Id        <- NULL                     
tam09b$R.W...2   <- NULL
tam09b$R.W.2.3   <- NULL
tam09b$R.W.3.4   <- NULL
tam09b$R.W.4.5   <- NULL
tam09b$R.W..5    <- NULL
tam09b$LitWeigth <- NULL
tam09b$X         <- NULL
tam09b$X.1       <- NULL
tam09b$notes     <- NULL

head(tam09b) 

head(stock_2013A)

# Re-name plot_codes
stock_2013A$Plot                        <- gsub("ACJ","ACJ-01", stock_2013A$Plot, ignore.case=T)
stock_2013A$Plot                        <- gsub("WAY","WAY-01", stock_2013A$Plot, ignore.case=T)
stock_2013A$Plot                        <- gsub("ESP","ESP-01", stock_2013A$Plot, ignore.case=T)
stock_2013A$Plot                        <- gsub("SP1500","SPD-02", stock_2013A$Plot, ignore.case=T)
stock_2013A$Plot                        <- gsub("SP1750","SPD-01", stock_2013A$Plot, ignore.case=T)

# create new data.frame stock_2013
stock_2013 <- c()

stock_2013$plot_code                   <- stock_2013A$Plot
stock_2013$year                        <- stock_2013A$year                      
stock_2013$month                       <- stock_2013A$month                        
stock_2013$day                         <- stock_2013A$day

stock_2013    <- data.frame(stock_2013)

stock_2013$ingrowth_core_num           <- stock_2013A$IC         
stock_2013$is_stock                    <- "y"   
stock_2013$ingrowth_core_litterfall_g  <- NA 
stock_2013$soil_humidity_pcnt          <- NA       
stock_2013$soil_temperature_c          <- NA   
stock_2013$ol_layer_depth_cm           <- stock_2013A$OL.cm.   
stock_2013$ml_layer_depth_cm           <- stock_2013A$ML.cm.   
stock_2013$time_step                   <- NA     
stock_2013$time_step_minutes           <- NA  
stock_2013$ol_under_2mm_g              <- stock_2013A$OL.g...2mm    
stock_2013$ol_above_2mm_g              <- stock_2013A$OL.g...2mm.1 
stock_2013$ml_under_2mm_g              <- stock_2013A$ML.g...2mm
stock_2013$ml_above_2mm_g              <- stock_2013A$ML.g...2mm.1               
stock_2013$ol_2to3_mm_g                <- NA      
stock_2013$ml_2to3_mm_g                <- NA       
stock_2013$ol_3to4_mm_g                <- NA       
stock_2013$ml_3to4_mm_g                <- NA      
stock_2013$ol_4to5_mm_g	               <- NA
stock_2013$ml_4to5_mm_g	               <- NA      
stock_2013$ol_above_5mm_g	             <- NA        
stock_2013$ml_above_5mm_g	             <- NA        
stock_2013$quality_code                <- "good"          
stock_2013$comments                    <- NA


eltr_ic <- rbind(way01, esp01, spd02, spd01, tam05a, tam05b, tam05c, tam06a, tam06b, tam06c, tam09a, tam09b, stock_2013)
write.csv(eltr_ic, file="eltr_ic_2012to2014.csv") 
