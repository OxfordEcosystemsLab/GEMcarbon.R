# October 2016
# Cécile Girardin: I use the following script to re-format raw xl sheets that come from the field. 

# load packages
library(sqldf)

## Fine litterfall.

### Read test data:
setwd("~/Desktop/data_sorting/flf")
list.files()

way01A  <- read.table("~/Desktop/data_sorting/flf/Litterfall_WAY01_2006_2009.csv", skip=1, sep=",", header=T)
way01B  <- read.table("~/Desktop/data_sorting/flf/Litterfall_WAY01_2009_2011.csv", sep=",", header=T)
way01C  <- read.table("~/Desktop/data_sorting/flf/Litterfall_WAY01_2013.csv", sep=",", header=T)
     
#esp01A  <- read.table("~/Desktop/data_sorting/flf/Litterfall_ESP_2009_2010.csv", sep=",", header=T)
esp01B  <- read.table("~/Desktop/data_sorting/flf/Litterfall_ESP01_2009_2011.csv", sep=",", header=T)
esp01C  <- read.table("~/Desktop/data_sorting/flf/Litterfall_ESP_2013_2014.csv", sep=",", header=T)

acj01A  <- read.table("~/Desktop/data_sorting/flf/Litterfall_ACJ_2013_2014.csv", sep=",", header=T)

pan02A  <- read.table("~/Desktop/data_sorting/flf/Litterfall_PAN02_2013_2014.csv", sep=",", header=T)
pan03A  <- read.table("~/Desktop/data_sorting/flf/Litterfall_PAN03_2013_2014.csv", sep=",", header=T)
tru04A  <- read.table("~/Desktop/data_sorting/flf/Litterfall_TU4_2103_2014.csv", sep=",", header=T)

#spd02A  <- read.table("~/Desktop/data_sorting/flf/Litterfall_SP_1500_2009_2010.csv", sep=",", header=T)
spd02A  <- read.table("~/Desktop/data_sorting/flf/Litterfall_SP1500_2006_2011.csv", sep=",", header=T)
spd02B  <- read.table("~/Desktop/data_sorting/flf/Litterfall_SP1500_2013.csv", sep=",", header=T)

#spd01A  <- read.table("~/Desktop/data_sorting/flf/Litterfall_SP_1750_2009.csv", sep=",", header=T)
spd01B  <- read.table("~/Desktop/data_sorting/flf/Litterfall_SP1750_2009_2011.csv", sep=",", header=T)
spd01C  <- read.table("~/Desktop/data_sorting/flf/Litterfall_SP1750_2013.csv", sep=",", header=T)

tam05A  <- read.table("~/Desktop/data_sorting/flf/Litterfall_TAM05_2013_2014.csv", sep=",", header=T)
tam06A  <- read.table("~/Desktop/data_sorting/flf/Litterfall_TAM06_2013_2014.csv", sep=",", header=T)
tam09A  <- read.table("~/Desktop/data_sorting/flf/Litterfall_TAM09_2013.csv", sep=",", header=T)
tam05bis  <- read.table("~/Desktop/data_sorting/flf/TAM-05_2013_2014.csv", sep=",", header=T)
tam06bis  <- read.table("~/Desktop/data_sorting/flf/TAM-06_2013_2014.csv", sep=",", header=T)


head(way01A)

# Change months to numbers
way01A$Month  <- gsub("January","1", way01A$Month, ignore.case=T)
way01A$Month  <- gsub("February","2", way01A$Month, ignore.case=T)
way01A$Month  <- gsub("March","3", way01A$Month, ignore.case=T)
way01A$Month  <- gsub("April","4", way01A$Month, ignore.case=T)
way01A$Month  <- gsub("May","5", way01A$Month, ignore.case=T)
way01A$Month  <- gsub("June","6", way01A$Month, ignore.case=T)
way01A$Month  <- gsub("July","7", way01A$Month, ignore.case=T)
way01A$Month  <- gsub("August","8", way01A$Month, ignore.case=T)
way01A$Month  <- gsub("September","9", way01A$Month, ignore.case=T)
way01A$Month  <- gsub("October","10", way01A$Month, ignore.case=T)
way01A$Month  <- gsub("November","11", way01A$Month, ignore.case=T)
way01A$Month  <- gsub("December","12", way01A$Month, ignore.case=T)

way01A$Month  <- as.numeric(as.character(way01A$Month))

way01a <- c()  
way01a$plot_code <- "WAY-01"
  
way01a$year                         <- way01A$Year 

way01a <- data.frame(way01a)

way01a$month                        <- way01A$Month                   
way01a$day                          <- way01A$Day                   
way01a$litterfall_trap_num          <- way01A$Colector
way01a$litterfall_trap_size_m2      <- 0.25
way01a$leaves_g_per_trap            <- way01A$Hojas
way01a$twigs_g_per_trap             <- way01A$Tallos 	
way01a$flowers_g_per_trap           <- way01A$Flores  	
way01a$fruits_g_per_trap            <- way01A$Frutos
way01a$seeds_g_per_trap      	      <- NA  
way01a$bromeliads_g_per_trap        <- way01A$Brom
way01a$epiphytes_g_per_trap         <- way01A$Epif
way01a$other_g_per_trap             <- way01A$Otros
way01a$palm_leaves_g_per_trap       <- NA       
way01a$palm_flower_g_per_trap       <- NA
way01a$palm_fruit_g_per_trap        <- NA        
way01a$total_litterfall_g_per_trap	<- rowSums(way01A[, c("Hojas", "Tallos", "Flores", "Frutos", "Brom", "Epif", "Otros")])
way01a$quality_code                 <- "good"
way01a$comments                     <- NA

head(way01a)


head(way01B)

way01b <- c()  
way01b$test                         <- way01B$collector
way01b$plot_code                    <-  "WAY-01"
way01b$year                         <- way01B$year + 2000 

way01b <- data.frame(way01b)

way01b$month                        <- way01B$month                   
way01b$day                          <- way01B$Day                   
way01b$litterfall_trap_num          <- way01B$collector
way01b$litterfall_trap_size_m2      <- 0.25
way01b$leaves_g_per_trap            <- way01B$Leaves
way01b$twigs_g_per_trap             <- way01B$Twigs   
way01b$flowers_g_per_trap           <- way01B$Flowers  	
way01b$fruits_g_per_trap            <- way01B$Fruits 
way01b$seeds_g_per_trap      	      <- NA  
way01b$bromeliads_g_per_trap        <- way01B$Brom
way01b$epiphytes_g_per_trap         <- way01B$Epiph
way01b$other_g_per_trap             <- way01B$Other
way01b$palm_leaves_g_per_trap       <- NA       
way01b$palm_flower_g_per_trap       <- NA
way01b$palm_fruit_g_per_trap        <- NA        
way01b$total_litterfall_g_per_trap	<- rowSums(way01B[, c("Leaves", "Twigs", "Flowers", "Fruits", "Brom", "Epiph", "Other")])
way01b$quality_code                 <- "good"
way01b$comments                     <- NA
way01b$test                         <- NULL

head(way01b)
length(way01b)


head(way01C)

way01c <- c()  
way01c$test                         <- way01C$collector
way01c$plot_code                    <-  "WAY-01"
way01c$year                         <- way01C$Year

way01c <- data.frame(way01c)

way01c$month                        <- way01C$Month                   
way01c$day                          <- way01C$Day                   
way01c$litterfall_trap_num          <- way01C$Sub.plot
way01c$litterfall_trap_size_m2      <- 0.25
way01c$leaves_g_per_trap            <- way01C$Litter..Weigth
way01c$twigs_g_per_trap             <- NA   
way01c$flowers_g_per_trap           <- NA    
way01c$fruits_g_per_trap            <- NA
way01c$seeds_g_per_trap      	      <- NA  
way01c$bromeliads_g_per_trap        <- NA
way01c$epiphytes_g_per_trap         <- NA
way01c$other_g_per_trap             <- NA
way01c$palm_leaves_g_per_trap       <- NA       
way01c$palm_flower_g_per_trap       <- NA
way01c$palm_fruit_g_per_trap        <- NA        
way01c$total_litterfall_g_per_trap	<- way01C$Total.weigth
way01c$quality_code                 <- "good"
way01c$comments                     <- way01C$Notes
way01c$test                         <- NULL

head(way01c)


head(esp01B)

esp01B$Epiph <- as.numeric(esp01B$Epiph)

esp01b <- c()  
esp01b$test                         <- esp01B$Collector
esp01b$plot_code                    <-  "ESP-01"
esp01b$year                         <- esp01B$year + 2000

esp01b <- data.frame(esp01b)

esp01b$month                        <- esp01B$month                   
esp01b$day                          <- esp01B$Day                   
esp01b$litterfall_trap_num          <- esp01B$Collector
esp01b$litterfall_trap_size_m2      <- 0.25
esp01b$leaves_g_per_trap            <- esp01B$Leaves
esp01b$twigs_g_per_trap             <- esp01B$Twigs  
esp01b$flowers_g_per_trap           <- esp01B$Flowers   
esp01b$fruits_g_per_trap            <- esp01B$Fruits
esp01b$seeds_g_per_trap             <- NA  
esp01b$bromeliads_g_per_trap        <- esp01B$Brom
esp01b$epiphytes_g_per_trap         <- esp01B$Epiph
esp01b$other_g_per_trap             <- esp01B$Other
esp01b$palm_leaves_g_per_trap       <- NA       
esp01b$palm_flower_g_per_trap       <- NA
esp01b$palm_fruit_g_per_trap        <- NA        
esp01b$total_litterfall_g_per_trap	<- rowSums(esp01B[, c("Leaves", "Twigs", "Flowers", "Fruits", "Brom", "Epiph", "Other")])
esp01b$quality_code                 <- "good"
esp01b$comments                     <- NA
esp01b$test                         <- NULL

head(esp01b)


head(esp01C)

esp01c <- c()  
esp01c$test                         <- esp01C$Sub.plot
esp01c$plot_code                    <-  "ESP-01"
esp01c$year                         <- esp01C$Year 

esp01c <- data.frame(esp01c)

esp01c$month                        <- esp01C$Month                   
esp01c$day                          <- esp01C$Day                   
esp01c$litterfall_trap_num          <- esp01C$Sub.plot
esp01c$litterfall_trap_size_m2      <- 0.25
esp01c$leaves_g_per_trap            <- esp01C$Litter..Weigth
esp01c$twigs_g_per_trap             <- NA  
esp01c$flowers_g_per_trap           <- NA   
esp01c$fruits_g_per_trap            <- NA
esp01c$seeds_g_per_trap             <- NA  
esp01c$bromeliads_g_per_trap        <- NA
esp01c$epiphytes_g_per_trap         <- NA
esp01c$other_g_per_trap             <- NA
esp01c$palm_leaves_g_per_trap       <- NA       
esp01c$palm_flower_g_per_trap       <- NA
esp01c$palm_fruit_g_per_trap        <- NA        
esp01c$total_litterfall_g_per_trap  <- esp01C$Total.weigth
esp01c$quality_code                 <- "good"
esp01c$comments                     <- esp01C$Notes
esp01c$test                         <- NULL

head(esp01c)


############################################### TO DO ######################################################################
acj01A 
pan02A 
pan03A 
tru04A 
############################################### TO DO ######################################################################

head(spd02A)  

spd02a <- c()  
spd02a$test                         <- spd02A$Collector
spd02a$plot_code                    <- "SPD-02"
spd02a$year                         <- spd02A$year + 2000

spd02a <- data.frame(spd02a)

spd02a$month                        <- spd02A$month                   
spd02a$day                          <- spd02A$Day                   
spd02a$litterfall_trap_num          <- spd02A$Collector
spd02a$litterfall_trap_size_m2      <- 0.25
spd02a$leaves_g_per_trap            <- spd02A$Leaves
spd02a$twigs_g_per_trap             <- spd02A$Twigs  
spd02a$flowers_g_per_trap           <- spd02A$Flowers   
spd02a$fruits_g_per_trap            <- spd02A$Fruits
spd02a$seeds_g_per_trap             <- NA  
spd02a$bromeliads_g_per_trap        <- spd02A$Brom
spd02a$epiphytes_g_per_trap         <- spd02A$Epiph
spd02a$other_g_per_trap             <- spd02A$Other
spd02a$palm_leaves_g_per_trap       <- NA       
spd02a$palm_flower_g_per_trap       <- NA
spd02a$palm_fruit_g_per_trap        <- NA        
spd02a$total_litterfall_g_per_trap  <- rowSums(spd02A[, c("Leaves", "Twigs", "Flowers", "Fruits", "Brom", "Epiph", "Other")])
spd02a$quality_code                 <- "good"
spd02a$comments                     <- NA
spd02a$test                         <- NULL

head(spd02a)


head(spd02B)

spd02b <- c()  
spd02b$test                         <- spd02B$Sub.plot
spd02b$plot_code                    <-  "SPD-02"
spd02b$year                         <- spd02B$Year

spd02b <- data.frame(spd02b)

spd02b$month                        <- spd02B$Month                   
spd02b$day                          <- spd02B$Day                   
spd02b$litterfall_trap_num          <- spd02B$Sub.plot
spd02b$litterfall_trap_size_m2      <- 0.25
spd02b$leaves_g_per_trap            <- spd02B$Litter..Weigth
spd02b$twigs_g_per_trap             <- NA  
spd02b$flowers_g_per_trap           <- NA   
spd02b$fruits_g_per_trap            <- NA
spd02b$seeds_g_per_trap             <- NA  
spd02b$bromeliads_g_per_trap        <- NA
spd02b$epiphytes_g_per_trap         <- NA
spd02b$other_g_per_trap             <- NA
spd02b$palm_leaves_g_per_trap       <- NA       
spd02b$palm_flower_g_per_trap       <- NA
spd02b$palm_fruit_g_per_trap        <- NA        
spd02b$total_litterfall_g_per_trap  <- spd02B$Total.weigth
spd02b$quality_code                 <- "good"
spd02b$comments                     <- spd02B$Notes
spd02b$test                         <- NULL

head(spd02b)


head(spd01B)  

spd01b <- c()  
spd01b$test                         <- spd01B$Collector
spd01b$plot_code                    <- "SPD-01"
spd01b$year                         <- spd01B$year + 2000

spd01b <- data.frame(spd01b)

spd01b$month                        <- spd01B$month                   
spd01b$day                          <- spd01B$Day                   
spd01b$litterfall_trap_num          <- spd01B$Collector
spd01b$litterfall_trap_size_m2      <- 0.25
spd01b$leaves_g_per_trap            <- spd01B$Leaves
spd01b$twigs_g_per_trap             <- spd01B$Twigs  
spd01b$flowers_g_per_trap           <- spd01B$Flowers   
spd01b$fruits_g_per_trap            <- spd01B$Fruits
spd01b$seeds_g_per_trap             <- NA  
spd01b$bromeliads_g_per_trap        <- spd01B$Brom
spd01b$epiphytes_g_per_trap         <- spd01B$Epiph
spd01b$other_g_per_trap             <- spd01B$Other
spd01b$palm_leaves_g_per_trap       <- NA       
spd01b$palm_flower_g_per_trap       <- NA
spd01b$palm_fruit_g_per_trap        <- NA        
spd01b$total_litterfall_g_per_trap  <- rowSums(spd01B[, c("Leaves", "Twigs", "Flowers", "Fruits", "Brom", "Epiph", "Other")])
spd01b$quality_code                 <- "good"
spd01b$comments                     <- NA
spd01b$test                         <- NULL

head(spd01b)


head(spd01C) 

spd01c <- c()  
spd01c$test                         <- spd01C$Sub.plot
spd01c$plot_code                    <- "SPD-01"
spd01c$year                         <- spd01C$Year + 2000

spd01c <- data.frame(spd01c)

spd01c$month                        <- spd01C$Month                   
spd01c$day                          <- spd01C$Day                   
spd01c$litterfall_trap_num          <- spd01C$Sub.plot
spd01c$litterfall_trap_size_m2      <- 0.25
spd01c$leaves_g_per_trap            <- spd01C$Litter..Weigth
spd01c$twigs_g_per_trap             <- NA  
spd01c$flowers_g_per_trap           <- NA   
spd01c$fruits_g_per_trap            <- NA
spd01c$seeds_g_per_trap             <- NA  
spd01c$bromeliads_g_per_trap        <- NA
spd01c$epiphytes_g_per_trap         <- NA
spd01c$other_g_per_trap             <- NA
spd01c$palm_leaves_g_per_trap       <- NA       
spd01c$palm_flower_g_per_trap       <- NA
spd01c$palm_fruit_g_per_trap        <- NA        
spd01c$total_litterfall_g_per_trap  <- spd01C$Total.weigth
spd01c$quality_code                 <- "good"
spd01c$comments                     <- spd01C$Notes
spd01c$test                         <- NULL

head(spd01c)


head(tam05A)

tam05a <- c()  
tam05a$test                         <- tam05A$No.Colector
tam05a$plot_code                    <- "TAM-05"
tam05a$year                         <- tam05A$Year 

tam05a <- data.frame(tam05a)

tam05a$month                        <- tam05A$Month                   
tam05a$day                          <- tam05A$Day                   
tam05a$litterfall_trap_num          <- tam05A$No.Colector
tam05a$litterfall_trap_size_m2      <- 0.25
tam05a$leaves_g_per_trap            <- tam05A$Litter.weight
tam05a$twigs_g_per_trap             <- NA  
tam05a$flowers_g_per_trap           <- NA   
tam05a$fruits_g_per_trap            <- NA
tam05a$seeds_g_per_trap             <- NA  
tam05a$bromeliads_g_per_trap        <- NA
tam05a$epiphytes_g_per_trap         <- NA
tam05a$other_g_per_trap             <- NA
tam05a$palm_leaves_g_per_trap       <- NA       
tam05a$palm_flower_g_per_trap       <- NA
tam05a$palm_fruit_g_per_trap        <- NA        
tam05a$total_litterfall_g_per_trap  <- tam05A$Total.Weight
tam05a$quality_code                 <- "good"
tam05a$comments                     <- tam05A$Notes
tam05a$test                         <- NULL

head(tam05a)


head(tam06A)

tam06a <- c()  
tam06a$test                         <- tam06A$No.Colector
tam06a$plot_code                    <- "TAM-06"
tam06a$year                         <- tam06A$Y 

tam06a <- data.frame(tam06a)

tam06a$month                        <- tam06A$Mes                   
tam06a$day                          <- tam06A$Dia                   
tam06a$litterfall_trap_num          <- tam06A$No.Colector
tam06a$litterfall_trap_size_m2      <- 0.25
tam06a$leaves_g_per_trap            <- tam06A$Litter.weight
tam06a$twigs_g_per_trap             <- NA  
tam06a$flowers_g_per_trap           <- NA   
tam06a$fruits_g_per_trap            <- NA
tam06a$seeds_g_per_trap             <- NA  
tam06a$bromeliads_g_per_trap        <- NA
tam06a$epiphytes_g_per_trap         <- NA
tam06a$other_g_per_trap             <- NA
tam06a$palm_leaves_g_per_trap       <- NA       
tam06a$palm_flower_g_per_trap       <- NA
tam06a$palm_fruit_g_per_trap        <- NA        
tam06a$total_litterfall_g_per_trap  <- tam06A$Total.Weight
tam06a$quality_code                 <- "good"
tam06a$comments                     <- tam06A$Notes
tam06a$test                         <- NULL

head(tam06a)


head(tam09A)

tam09a <- c()  
tam09a$test                         <- tam09A$No.Colector
tam09a$plot_code                    <- "TAM-09"
tam09a$year                         <- tam09A$Y

tam09a <- data.frame(tam09a)

tam09a$month                        <- tam09A$Mes                   
tam09a$day                          <- tam09A$Dia                   
tam09a$litterfall_trap_num          <- tam09A$No.Colector
tam09a$litterfall_trap_size_m2      <- 0.25
tam09a$leaves_g_per_trap            <- tam09A$Litter.weight
tam09a$twigs_g_per_trap             <- NA  
tam09a$flowers_g_per_trap           <- NA   
tam09a$fruits_g_per_trap            <- NA
tam09a$seeds_g_per_trap             <- NA  
tam09a$bromeliads_g_per_trap        <- NA
tam09a$epiphytes_g_per_trap         <- NA
tam09a$other_g_per_trap             <- NA
tam09a$palm_leaves_g_per_trap       <- NA       
tam09a$palm_flower_g_per_trap       <- NA
tam09a$palm_fruit_g_per_trap        <- NA        
tam09a$total_litterfall_g_per_trap  <- tam09A$Total.Weight
tam09a$quality_code                 <- "good"
tam09a$comments                     <- NA
tam09a$test                         <- NULL

head(tam09a)


head(tam05bis)

head(tam06bis)



eltr_flf <- rbind(way01a, way01b, way01c, esp01b, esp01c, spd02a, spd02b, spd01b, spd01c, tam05a, tam06a, tam09a)

eltr_flf$leaves_g_per_trap <- as.numeric(as.character(eltr_flf$leaves_g_per_trap)) 
eltr_flf$day <- as.numeric(as.character(eltr_flf$day))
eltr_flf$month <- as.numeric(as.character(eltr_flf$month))

write.csv(eltr_flf, file="eltr_flf_2009to2014.csv") 



