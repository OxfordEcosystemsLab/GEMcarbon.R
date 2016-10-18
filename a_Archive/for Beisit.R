> setwd("/Users/cecile/Dropbox/GEMcarbondb/db_csv/db_csv_2015/readyforupload_db/acj_pan_2015")
> census_ACJ01 <- read.table("ACJcensus_clean_date.csv", header=TRUE, sep=",", na.strings=c("NA", "NaN", ""), dec=".", strip.white=TRUE)
> head(census_ACJ01)
X.1 Tag.No Old.No T1 T2  X  Y       Family      genus   species Old.Tree.Notes D..2013.1 POM..2013.1 F1..2013.1 F2..2013.1 Height..2013.1 F5..2013.1
1   1      1     NA  1 NA NA NA Symplocaceae  Symplocos quitensis           <NA>       129        1300          a          1            8.0          4
2   2      2     NA  1 NA NA NA  Cunoniaceae Weinmannia cochensis           <NA>       234        1300          a          1            8.4          4
3   3      3     NA  1  2 NA NA  Cunoniaceae Weinmannia cochensis           <NA>       119        1300          a          1            7.0          4
4   4      4     NA  1 NA NA NA  Cunoniaceae Weinmannia cochensis           <NA>       134        1300          a          1            8.5          4
5   5      5     NA  1 NA NA NA     Rosaceae  Polylepis     pauta           <NA>       312        1300          a          1            9.0          4
6   6      6     NA  1 NA NA NA  Cunoniaceae Weinmannia cochensis           <NA>       128        1300          a          1            7.0          4
F3..2013.1 F4..2013.1 LI..2013.1 CI..2013.1 Census.Notes..2013.1 D..2014.2 POM..2014.2 F1 F2 Height..2014.2 F5 F3 F4 LI..2014.2 CI..2014.2 Tree.ID
1          0          0          0          1                   NA       130        1300  a  1             NA NA  0  0         NA         NA 1105383
2          0          0          0          1                   NA       234        1300 ah  1             NA NA  0  0         NA         NA 1105384
3          0          0          0          1                   NA       112        1300 ah  1             NA NA  0  0         NA         NA 1105385
4          0          0          0          1                   NA       131        1300  c  1             NA NA  0  0         NA         NA 1105386
5          0          0          0          1                   NA       317        1300  c  1             NA NA  0  0         NA         NA 1105387
6          0          0          0          1                   NA       131        1300  c  1             NA NA  0  0         NA         NA 1105388
D..2015.1 POM..2015.1 F1.1 F2.1 Height..2015.1 F5.1 F3.1 F4.1 LI..2015.1 CI..2015.1 Census.Notes Tree.ID.1 New.Tag.No  wdensity height   density tag
1       132        1300    m    1           7000   NA    0    0          2         3b         <NA>   1105383         NA 0.5745640   7.50 0.5745640   1
2       268        1300   ah    1           7000   NA    0    0          0         3b         <NA>   1105384         NA 0.6392357   7.70 0.6392357   2
3       115        1300   ah    1           7000   NA    0    0          0         3a         <NA>   1105385         NA 0.6392357   7.00 0.6392357   3
4       130        1300    c    1           8000   NA    0    0          1         3a         <NA>   1105386         NA 0.6392357   8.25 0.6392357   4
5       327        1300    c    1          10000   NA    0    0          0         3b         <NA>   1105387         NA 0.6697630   9.50 0.6697630   5
6       131        1300    c    1           8000   NA    0    0          0         2a         <NA>   1105388         NA 0.6392357   7.50 0.6392357   6
plot DBH.1 DBH.2 DBH.3 dbh_growth_yr recruits srink missing overgrown value_replaced
1 ACJ-01  12.9  13.0  13.2    0.08752998       ok    ok      ok        ok             NA
2 ACJ-01  23.4  23.4  26.8    0.00000000       ok    ok      ok        ok             NA
3 ACJ-01  11.9  11.2  11.5   -0.61270983       ok    ok      ok        ok             NA
4 ACJ-01  13.4  13.1  13.0   -0.26258993       ok    ok      ok        ok             NA
5 ACJ-01  31.2  31.7  32.7    0.43764988       ok    ok      ok        ok             NA
6 ACJ-01  12.8  13.1  13.1    0.26258993       ok    ok      ok        ok             NA
> data <- subset(census_ACJ01, plot=="ACJ-01")
> date_1 <- as.character("2013/01/22") # 22 de Enero 2013
> date_2 <- as.character("2014/03/15") # 15 de Marzo 2014
> date_3 <- as.character("2015/03/16") # 16 de Marzo 2015
> date_1 <- as.Date(format(strptime(date_1, format="%Y/%m/%d")))
> date_2 <- as.Date(format(strptime(date_2, format="%Y/%m/%d")))
> date_3 <- as.Date(format(strptime(date_3, format="%Y/%m/%d")))
> census_interval_1 <- as.numeric(difftime(date_2, date_1, units="days"))
> census_interval_yrs_1 <- census_interval_1/365
> census_interval_2 <- as.numeric(difftime(date_3, date_2, units="days"))
> census_interval_yrs_2 <- census_interval_2/365
> data$dbh_growth_yr <- (data$DBH.2 - data$DBH.1)/census_interval_yrs_1  
> gr_sd <- sd(data$dbh_growth_yr, na.rm=T)
> plot1 <- ggplot(data=data, aes(x=DBH.1, y=dbh_growth_yr, na.rm=T)) +
  +   geom_point() +
  +   ylim(-10, 10) +
  +   ggtitle(data$plot)
> plot1
Warning message:
  Removed 7 rows containing missing values (geom_point). 
> w = which(is.na(data$DBH.1) & !is.na(data$DBH.2))
> data$DBH.2[w] = 0/0 
> data$recruits <- "ok"
> data$recruits[w] <- "recruit"
> w = which(is.na(data$DBH.1) & is.na(data$DBH.2) & !is.na(data$DBH.3)) 
> data$DBH.3[w] = 0/0    
> data$recruits[w] <- "recruit"
> w = which(data$DBH.1 > data$DBH.2 + (data$DBH.1*0.1)) 
> data$DBH.2[w] = data$DBH.1[w]  
> data$srink <- "ok"
> data$srink[w] <- "shrunk.dbh.2"
> w = which(data$DBH.2 > data$DBH.3 + (data$DBH.2*0.1))   
> data$DBH.3[w] = data$DBH.2[w]        
> data$srink[w] <- "shrunk.dbh.3"
> w = which(!is.na(data$DBH.1) & is.na(data$DBH.2) & !is.na(data$DBH.3)) 
> data$DBH.2[w] = (data$DBH.1[w] + data$DBH.3[w])/2        # mean rather than meadian flag this as 8 - see RAINFOR flags.
> data$missing <- "ok"
> data$missing[w] <- "missing"
> maxincrement_1 <- 5 * census_interval_yrs_1 #(3*gr_sd)
> maxincrement_2 <- 5 * census_interval_yrs_2 #(3*gr_sd)
> w = which((data$DBH.2 - data$DBH.1) >= maxincrement_1)      
> data$overgrown <- "ok"
> data$overgrown[w] <- ">2cm growth per yr dbh.2"
> data$value_replaced <- NA
> data$value_replaced[w] <- data$DBH.2[w]
> data$DBH.2[w] = (data$DBH.1[w] + maxincrement_1)
> w = which((data$DBH.3 - data$DBH.2) >= maxincrement_2)     
> data$overgrown[w] <- ">2cm growth per yr dbh.3"
> data$value_replaced[w] <- data$DBH.3[w]
> data$DBH.3[w] = (data$DBH.2[w] + maxincrement_2)
> n_occur <- data.frame(table(data$tag))
> n_occur[n_occur$Freq > 1,]
[1] Var1 Freq
<0 rows> (or 0-length row.names)
> data[data$tag %in% n_occur$Var1[n_occur$Freq > 1],]
[1] X.1                  Tag.No               Old.No               T1                   T2                   X                   
[7] Y                    Family               genus                species              Old.Tree.Notes       D..2013.1           
[13] POM..2013.1          F1..2013.1           F2..2013.1           Height..2013.1       F5..2013.1           F3..2013.1          
[19] F4..2013.1           LI..2013.1           CI..2013.1           Census.Notes..2013.1 D..2014.2            POM..2014.2         
[25] F1                   F2                   Height..2014.2       F5                   F3                   F4                  
[31] LI..2014.2           CI..2014.2           Tree.ID              D..2015.1            POM..2015.1          F1.1                
[37] F2.1                 Height..2015.1       F5.1                 F3.1                 F4.1                 LI..2015.1          
[43] CI..2015.1           Census.Notes         Tree.ID.1            New.Tag.No           wdensity             height              
[49] density              tag                  plot                 DBH.1                DBH.2                DBH.3               
[55] dbh_growth_yr        recruits             srink                missing              overgrown            value_replaced      
<0 rows> (or 0-length row.names)
> data[is.na(data)] <- NA
> dataACJ01 <- data
> head(dataACJ01)
X.1 Tag.No Old.No T1 T2  X  Y       Family      genus   species Old.Tree.Notes D..2013.1 POM..2013.1 F1..2013.1 F2..2013.1
1   1      1     NA  1 NA NA NA Symplocaceae  Symplocos quitensis           <NA>       129        1300          a          1
2   2      2     NA  1 NA NA NA  Cunoniaceae Weinmannia cochensis           <NA>       234        1300          a          1
3   3      3     NA  1  2 NA NA  Cunoniaceae Weinmannia cochensis           <NA>       119        1300          a          1
4   4      4     NA  1 NA NA NA  Cunoniaceae Weinmannia cochensis           <NA>       134        1300          a          1
5   5      5     NA  1 NA NA NA     Rosaceae  Polylepis     pauta           <NA>       312        1300          a          1
6   6      6     NA  1 NA NA NA  Cunoniaceae Weinmannia cochensis           <NA>       128        1300          a          1
Height..2013.1 F5..2013.1 F3..2013.1 F4..2013.1 LI..2013.1 CI..2013.1 Census.Notes..2013.1 D..2014.2 POM..2014.2 F1 F2
1            8.0          4          0          0          0          1                   NA       130        1300  a  1
2            8.4          4          0          0          0          1                   NA       234        1300 ah  1
3            7.0          4          0          0          0          1                   NA       112        1300 ah  1
4            8.5          4          0          0          0          1                   NA       131        1300  c  1
5            9.0          4          0          0          0          1                   NA       317        1300  c  1
6            7.0          4          0          0          0          1                   NA       131        1300  c  1
Height..2014.2 F5 F3 F4 LI..2014.2 CI..2014.2 Tree.ID D..2015.1 POM..2015.1 F1.1 F2.1 Height..2015.1 F5.1 F3.1 F4.1 LI..2015.1
1             NA NA  0  0         NA         NA 1105383       132        1300    m    1           7000   NA    0    0          2
2             NA NA  0  0         NA         NA 1105384       268        1300   ah    1           7000   NA    0    0          0
3             NA NA  0  0         NA         NA 1105385       115        1300   ah    1           7000   NA    0    0          0
4             NA NA  0  0         NA         NA 1105386       130        1300    c    1           8000   NA    0    0          1
5             NA NA  0  0         NA         NA 1105387       327        1300    c    1          10000   NA    0    0          0
6             NA NA  0  0         NA         NA 1105388       131        1300    c    1           8000   NA    0    0          0
CI..2015.1 Census.Notes Tree.ID.1 New.Tag.No  wdensity height   density tag   plot DBH.1 DBH.2 DBH.3 dbh_growth_yr recruits
1         3b         <NA>   1105383         NA 0.5745640   7.50 0.5745640   1 ACJ-01  12.9  13.0  13.2    0.08752998       ok
2         3b         <NA>   1105384         NA 0.6392357   7.70 0.6392357   2 ACJ-01  23.4  23.4  26.8    0.00000000       ok
3         3a         <NA>   1105385         NA 0.6392357   7.00 0.6392357   3 ACJ-01  11.9  11.2  11.5   -0.61270983       ok
4         3a         <NA>   1105386         NA 0.6392357   8.25 0.6392357   4 ACJ-01  13.4  13.1  13.0   -0.26258993       ok
5         3b         <NA>   1105387         NA 0.6697630   9.50 0.6697630   5 ACJ-01  31.2  31.7  32.7    0.43764988       ok
6         2a         <NA>   1105388         NA 0.6392357   7.50 0.6392357   6 ACJ-01  12.8  13.1  13.1    0.26258993       ok
srink missing overgrown value_replaced
1    ok      ok        ok             NA
2    ok      ok        ok             NA
3    ok      ok        ok             NA
4    ok      ok        ok             NA
5    ok      ok        ok             NA
6    ok      ok        ok             NA
> eltrcensus <- dataACJ01
> yearDAP1   <- c()
> monthDAP1  <- c()
> dayDAP1    <- c()
> for (ii in 1:(length(eltrcensus$plot))) {  # length(eltrcensus$plot) is equivalent to nrow(eltrcensus)
  +   yeartmp  = NA
  +   monthtmp = NA
  +   daytmp   = NA
  +   # cat("At row",i,"=",eltrcensus$plot[i]) # this is to print the number of row and the value in that row
    +   if (as.character(eltrcensus$plot[ii]) == "ACJ-01") {
      +     yeartmp  = 2013
      +     monthtmp = 01
      +     daytmp   = 22  
      +   }
  +   if (as.character(eltrcensus$plot[ii]) == "PAN-02") {
    +     yeartmp  = 2013
    +     monthtmp = 04
    +     daytmp   = 12
    +   }
  +   if (as.character(eltrcensus$plot[ii]) == "PAN-03") {
    +     yeartmp  = 2013
    +     monthtmp = 03
    +     daytmp   = 30
    +   }
  +   if (as.character(eltrcensus$plot[ii]) == "TRU-04") {
    +     yeartmp  = 2003
    +     monthtmp = 07
    +     daytmp   = 14
    +   }
  +   yearDAP1  = c(yearDAP1,yeartmp)
  +   monthDAP1 = c(monthDAP1,monthtmp)
  +   dayDAP1   = c(dayDAP1,daytmp)
  +   datesDAP1 = data.frame(yearDAP1, monthDAP1, dayDAP1) 
  +   datesDAP1 = data.frame(datesDAP1[1,], eltrcensus$plot, row.names = NULL) # Make sure this is the right number of rows.
  +   colnames(datesDAP1) <- c("year", "month", "day", "plot")
  + }
> 
  > # 2nd census
  > yearDAP2   <- NULL   # same as yearDAP2 = c()
> monthDAP2  <- NULL
> dayDAP2    <- NULL
> for (ii in 1:(length(eltrcensus$plot))) {  #length(eltrcensus$plot) is equivalent to nrow(eltrcensus)
  +   yeartmp  = NA
  +   monthtmp = NA
  +   daytmp   = NA
  +   # cat("At row",i,"=",eltrcensus$plot[i]) this is to print the number of row and the value in that row
    +   if (as.character(eltrcensus$plot[ii]) == "ACJ-01") {
      +     yeartmp  = 2014
      +     monthtmp = 03
      +     daytmp   = 15
      +   }
  +   if (as.character(eltrcensus$plot[ii]) == "PAN-02") {
    +     yeartmp  = 2015
    +     monthtmp = 03
    +     daytmp   = 02
    +   }
  +   if (as.character(eltrcensus$plot[ii]) == "PAN-03") {
    +     yeartmp  = 2014
    +     monthtmp = 03
    +     daytmp   = 02
    +   }
  +   if (as.character(eltrcensus$plot[ii]) == "TRU-04") {
    +     yeartmp  = 2007
    +     monthtmp = 06
    +     daytmp   = 08
    +   }
  +   yearDAP2  = c(yearDAP2,yeartmp)
  +   monthDAP2 = c(monthDAP2,monthtmp)
  +   dayDAP2   = c(dayDAP2,daytmp)
  +   datesDAP2 = data.frame(yearDAP2, monthDAP2, dayDAP2)
  +   datesDAP2 = data.frame(datesDAP2[1,], eltrcensus$plot, row.names = NULL)  
  +   colnames(datesDAP2) <- c("year", "month", "day", "plot")
  + }
> 
  > # 3rd census
  > yearDAP3   <- NULL   # same as yearDAP2 = c()
> monthDAP3  <- NULL
> dayDAP3    <- NULL
> for (ii in 1:(length(eltrcensus$plot))) {  #length(eltrcensus$plot) is equivalent to nrow(eltrcensus)
  +   yeartmp  = NA
  +   monthtmp = NA
  +   daytmp   = NA
  +   # cat("At row",i,"=",eltrcensus$plot[i]) this is to print the number of row and the value in that row
    +   if (as.character(eltrcensus$plot[ii]) == "ACJ-01") {
      +     yeartmp  = 2015
      +     monthtmp = 03
      +     daytmp   = 16 
      +   }
  +   #if (as.character(eltrcensus$plot[ii]) == "PAN-02") {
    +   #  yeartmp  = 2015 
    +   #  monthtmp = 03
    +   #  daytmp   = 02
    +   #}
    +   if (as.character(eltrcensus$plot[ii]) == "PAN-03") {
      +     yeartmp  = 2015
      +     monthtmp = 03
      +     daytmp   = 04
      +   }
  +   if (as.character(eltrcensus$plot[ii]) == "TRU-04") {
    +     yeartmp  = 2011
    +     monthtmp = 07
    +     daytmp   = 30
    +   }
  +   yearDAP3  = c(yearDAP3,yeartmp)
  +   monthDAP3 = c(monthDAP3,monthtmp)
  +   dayDAP3   = c(dayDAP3,daytmp)
  +   datesDAP3 = data.frame(yearDAP3, monthDAP3, dayDAP3)
  +   datesDAP3 = data.frame(datesDAP3[1,], eltrcensus$plot, row.names = NULL)   
  +   colnames(datesDAP3) <- c("year", "month", "day", "plot")
  + }
> eltrcen1 <- data.frame(plot_code=eltrcensus$plot, tree_tag=eltrcensus$tag, dbh=eltrcensus$DBH.1, height_m=eltrcensus$height, density=eltrcensus$density, datesDAP1)
> head(eltrcen1)
plot_code tree_tag  dbh height_m   density year month day   plot
1    ACJ-01        1 12.9     7.50 0.5745640 2013     1  22 ACJ-01
2    ACJ-01        2 23.4     7.70 0.6392357 2013     1  22 ACJ-01
3    ACJ-01        3 11.9     7.00 0.6392357 2013     1  22 ACJ-01
4    ACJ-01        4 13.4     8.25 0.6392357 2013     1  22 ACJ-01
5    ACJ-01        5 31.2     9.50 0.6697630 2013     1  22 ACJ-01
6    ACJ-01        6 12.8     7.50 0.6392357 2013     1  22 ACJ-01
> eltrcen2 <- data.frame(plot_code=eltrcensus$plot, tree_tag=eltrcensus$tag, dbh=eltrcensus$DBH.2, height_m=eltrcensus$height, density=eltrcensus$density, datesDAP2)
> head(eltrcen2)
plot_code tree_tag  dbh height_m   density year month day   plot
1    ACJ-01        1 13.0     7.50 0.5745640 2014     3  15 ACJ-01
2    ACJ-01        2 23.4     7.70 0.6392357 2014     3  15 ACJ-01
3    ACJ-01        3 11.2     7.00 0.6392357 2014     3  15 ACJ-01
4    ACJ-01        4 13.1     8.25 0.6392357 2014     3  15 ACJ-01
5    ACJ-01        5 31.7     9.50 0.6697630 2014     3  15 ACJ-01
6    ACJ-01        6 13.1     7.50 0.6392357 2014     3  15 ACJ-01
> eltrcen3 <- data.frame(plot_code=eltrcensus$plot, tree_tag=eltrcensus$tag, dbh=eltrcensus$DBH.3, height_m=eltrcensus$height, density=eltrcensus$density, datesDAP3)
> head(eltrcen3)
plot_code tree_tag  dbh height_m   density year month day   plot
1    ACJ-01        1 13.2     7.50 0.5745640 2015     3  16 ACJ-01
2    ACJ-01        2 26.8     7.70 0.6392357 2015     3  16 ACJ-01
3    ACJ-01        3 11.5     7.00 0.6392357 2015     3  16 ACJ-01
4    ACJ-01        4 13.0     8.25 0.6392357 2015     3  16 ACJ-01
5    ACJ-01        5 32.7     9.50 0.6697630 2015     3  16 ACJ-01
6    ACJ-01        6 13.1     7.50 0.6392357 2015     3  16 ACJ-01
> census <- rbind(eltrcen1, eltrcen2)#, eltrcen3)
> census$height_m <- as.numeric(census$height_m)
> sapply(census, class)
plot_code  tree_tag       dbh  height_m   density      year     month       day      plot 
"factor" "numeric" "numeric" "numeric" "numeric" "numeric" "numeric" "numeric"  "factor" 
> head(census)
plot_code tree_tag  dbh height_m   density year month day   plot
1    ACJ-01        1 12.9     7.50 0.5745640 2013     1  22 ACJ-01
2    ACJ-01        2 23.4     7.70 0.6392357 2013     1  22 ACJ-01
3    ACJ-01        3 11.9     7.00 0.6392357 2013     1  22 ACJ-01
4    ACJ-01        4 13.4     8.25 0.6392357 2013     1  22 ACJ-01
5    ACJ-01        5 31.2     9.50 0.6697630 2013     1  22 ACJ-01
6    ACJ-01        6 12.8     7.50 0.6392357 2013     1  22 ACJ-01
> tail(census)
plot_code tree_tag  dbh height_m   density year month day   plot
1721    ACJ-01      851 11.1      5.0 0.7332520 2014     3  15 ACJ-01
1722    ACJ-01      852 16.4     10.0 0.5400146 2014     3  15 ACJ-01
1723    ACJ-01      853 16.4      2.5 0.5745640 2014     3  15 ACJ-01
1724    ACJ-01      854 11.9      2.5 0.5745640 2014     3  15 ACJ-01
1725    ACJ-01      855 25.6     13.0 0.5400146 2014     3  15 ACJ-01
1726    ACJ-01      856 44.4     13.5 0.5400146 2014     3  15 ACJ-01
> sapply(census, class)
plot_code  tree_tag       dbh  height_m   density      year     month       day      plot 
"factor" "numeric" "numeric" "numeric" "numeric" "numeric" "numeric" "numeric"  "factor" 
> setwd("/Users/cecile/GitHub/GEMcarbon.R") 
> dir()
[1] "ACJ_carboncycle_2015.R"                   "Archive"                                 
[3] "EGM_raw_to_flux_option1.R"                "EGM_raw_to_flux_stem_2015.R"             
[5] "GEM_carbon.R"                             "NPPacw_census_function_2015.R"           
[7] "NPPacw_dendro_function_2015.R"            "NPPacw_step1_reformatting_census_2015.R" 
[9] "NPProot_2015.R"                           "SAFE_IngrowthCores_Rcode_toCecile_2.txt" 
[11] "allometric_equations_2014.R"              "coarsewoodNPP.R"                         
[13] "coarsewoodres.R"                          "datacleaning_census_2014.R"              
[15] "example files"                            "flf_2015.R"                              
[17] "leafres.R"                                "script to create average daily climate.R"
[19] "smallTreeNPP_2015.R"                      "smallTreeNPP_v3.R"                       
[21] "soil_respiration_2015.R"                  "soilrespiration_auxfunctions.R"          
[23] "stem_respiration_2015.R"                 
> Sys.setlocale('LC_ALL','C') 
[1] "C/C/C/C/C/en_GB.UTF-8"
> source("NPPacw_census_function_2014.R")
Error in file(filename, "r", encoding = encoding) : 
  cannot open the connection
In addition: Warning message:
  In file(filename, "r", encoding = encoding) :
  cannot open file 'NPPacw_census_function_2014.R': No such file or directory
> source("allometric_equations_2014.R")
> setwd("/Users/cecile/GitHub/GEMcarbon.R") 
> dir()
[1] "ACJ_carboncycle_2015.R"                   "Archive"                                 
[3] "EGM_raw_to_flux_option1.R"                "EGM_raw_to_flux_stem_2015.R"             
[5] "GEM_carbon.R"                             "NPPacw_census_function_2015.R"           
[7] "NPPacw_dendro_function_2015.R"            "NPPacw_step1_reformatting_census_2015.R" 
[9] "NPProot_2015.R"                           "SAFE_IngrowthCores_Rcode_toCecile_2.txt" 
[11] "allometric_equations_2014.R"              "coarsewoodNPP.R"                         
[13] "coarsewoodres.R"                          "datacleaning_census_2014.R"              
[15] "example files"                            "flf_2015.R"                              
[17] "leafres.R"                                "script to create average daily climate.R"
[19] "smallTreeNPP_2015.R"                      "smallTreeNPP_v3.R"                       
[21] "soil_respiration_2015.R"                  "soilrespiration_auxfunctions.R"          
[23] "stem_respiration_2015.R"                 
> Sys.setlocale('LC_ALL','C') 
[1] "C/C/C/C/C/en_GB.UTF-8"
> source("NPPacw_census_function_2015.R")
> source("allometric_equations_2014.R")
> acj_01A  <- NPPacw_census(census, plotname="ACJ-01", allometric_option="Default", height_correction_option="Default", census1_year=2013, census2_year=2014)
[1] "moist equation  is used for estimating AGB, model I.6 (see Chave et al., 2005)"
[1] "If you have height for more than 50 trees in your plot, estimate local diameter-height relationship. If not, choose height correction option 2."
> acj_01A
NPPacw_MgC_ha_yr NPPacw_MgC_ha_yr_se
1         2.482465        1.252718e-08
> acj_01B  <- NPPacw_census(census, plotname="ACJ-01", allometric_option="Default", height_correction_option="Default", census1_year=2014, census2_year=2015)
[1] "moist equation  is used for estimating AGB, model I.6 (see Chave et al., 2005)"
[1] "If you have height for more than 50 trees in your plot, estimate local diameter-height relationship. If not, choose height correction option 2."
> acj_01B
NPPacw_MgC_ha_yr NPPacw_MgC_ha_yr_se
1                0                  NA
> setwd("/Users/cecile/Dropbox/GEMcarbondb/db_csv/db_csv_2015/readyforupload_db/acj_pan_2015")
> NPPdend_ACJ01 <- read.table("Dendrometer_ACJ_2013_2014.csv", header=TRUE, sep=",", na.strings=c("NA", "NaN", ""), dec=".", strip.white=TRUE)
> dendrometer <- NPPdend_ACJ01
> plotname = "ACJ-01"     # TO DO: "ACJ" should be replaced by "ACJ-01" everywhere!
> allometric_option = "Default"
> height_correction_option = "Default"
> census_year = 2013
> plotit=T
> setwd("/Users/cecile/GitHub/GEMcarbon.R") 
> dir()
[1] "ACJ_carboncycle_2015.R"                   "Archive"                                 
[3] "EGM_raw_to_flux_option1.R"                "EGM_raw_to_flux_stem_2015.R"             
[5] "GEM_carbon.R"                             "NPPacw_census_function_2015.R"           
[7] "NPPacw_dendro_function_2015.R"            "NPPacw_step1_reformatting_census_2015.R" 
[9] "NPProot_2015.R"                           "SAFE_IngrowthCores_Rcode_toCecile_2.txt" 
[11] "allometric_equations_2014.R"              "coarsewoodNPP.R"                         
[13] "coarsewoodres.R"                          "datacleaning_census_2014.R"              
[15] "example files"                            "flf_2015.R"                              
[17] "leafres.R"                                "script to create average daily climate.R"
[19] "smallTreeNPP_2015.R"                      "smallTreeNPP_v3.R"                       
[21] "soil_respiration_2015.R"                  "soilrespiration_auxfunctions.R"          
[23] "stem_respiration_2015.R"                 
> Sys.setlocale('LC_ALL','C') 
[1] "C/C/C/C/C/en_GB.UTF-8"
> source("allometric_equations_2014.R")
> # load libraries
  >   library(sqldf)
> head(census)
plot_code tree_tag  dbh height_m   density year month day   plot
1    ACJ-01        1 12.9     7.50 0.5745640 2013     1  22 ACJ-01
2    ACJ-01        2 23.4     7.70 0.6392357 2013     1  22 ACJ-01
3    ACJ-01        3 11.9     7.00 0.6392357 2013     1  22 ACJ-01
4    ACJ-01        4 13.4     8.25 0.6392357 2013     1  22 ACJ-01
5    ACJ-01        5 31.2     9.50 0.6697630 2013     1  22 ACJ-01
6    ACJ-01        6 12.8     7.50 0.6392357 2013     1  22 ACJ-01
> cen1  <- subset(census, plot_code==plotname)
> cen   <- subset(cen1, year==census_year)  
> dend1 <- subset(dendrometer, plot_code==plotname)
> cen$cenyear  <- cen$year
> cen$cenmonth <- cen$month
> cen$cenday   <- cen$day
> dend <- sqldf("SELECT dend1.*, cen.density, cen.height_m, cen.dbh, cen.cenyear, cen.cenmonth, cen.cenday FROM cen JOIN dend1 ON cen.tree_tag = dend1.tree_tag")
> head(dend) # check you have data in here. If not, make sure dend1 and cen are in the right formats, e.g. using sapply(cen, class).
plot_code sub_plot tree_tag year month day dend_pom_height_m dendrometer_reading_mm comments status_code_aintact_bmoved_cbroken
1    ACJ-01        1        6 2013     6  21               1.4                   -1.8     <NA>                                 NA
2    ACJ-01        1        6 2013     9  24               1.4                  -1.54     <NA>                                 NA
3    ACJ-01        1        6 2013    12  10               1.4                   -2.1     <NA>                                 NA
4    ACJ-01        1        6 2014     3   6               1.4                   -1.6     <NA>                                 NA
5    ACJ-01        1       10 2013     6  21               1.4                   -0.7     <NA>                                 NA
6    ACJ-01        1       10 2013     9  24               1.4                      0     <NA>                                 NA
mortality_aalive_ddead   density height_m  dbh cenyear cenmonth cenday
1                     NA 0.6392357      7.5 12.8    2013        1     22
2                     NA 0.6392357      7.5 12.8    2013        1     22
3                     NA 0.6392357      7.5 12.8    2013        1     22
4                     NA 0.6392357      7.5 12.8    2013        1     22
5                     NA 0.5745640      9.5 17.0    2013        1     22
6                     NA 0.5745640      9.5 17.0    2013        1     22
> if (allometric_option == 2 | allometric_option == "dry") {
  +     allometrix <- 2
  +     print("dry equation  is used for estimating AGB, model I.3 (see Chave et al., 2005)")
  +   } else if (allometric_option == 3 | allometric_option == "moist" | allometric_option == "Default" | allometric_option == 1) {
    +     allometrix <- 3
    +     print("moist equation  is used for estimating AGB, model I.6 (see Chave et al., 2005)")
    +   } else if (allometric_option == 4 | allometric_option == "wet") {
      +     allometrix <- 4
      +     print("wet equation  is used for estimating AGB, model I.3 (see Chave et al., 2005)")
      +   } else if (allometric_option == 5 | allometric_option == "Chave2014") {
        +     allometrix <- 5
        +     print("pantropical equation is used for estimating AGB, model (4) (see Chave et al., 2014)")
        +   } else {
          +     print("Please specify a valid allometric_option!")
          +     return()
          +   }
[1] "moist equation  is used for estimating AGB, model I.6 (see Chave et al., 2005)"
> if (height_correction_option == 1 | height_correction_option == "Default" ) {
  +   predheight <- 1
  +   print("If you have height for more than 50 trees in your plot, estimate local diameter-height relationship. If not, choose height correction option 2.")
  + } else if (height_correction_option == 2) {
    +   predheight <- 2
    +   print("height correction estimated as described by Feldpauch et al. (2012). Please check Feldpauch regional parameters in the code. Default is Brazilian shield.")
    + } else {
      +   print("Please specify a valid height_correction_option!")
      +   return()
      + }
[1] "If you have height for more than 50 trees in your plot, estimate local diameter-height relationship. If not, choose height correction option 2."
> h.est=function(dbh, h){
  +   l      =lm(h~dbh)
  +   coeffs = coefficients(l)
  +   pred.h = coeffs[1] + coeffs[2]*dbh
  + }
> Bo    = 0.6373
> B1    = 0.4647  # E.C. Amazonia
> 
  > So1   = 0.012  # E.C. Amazonia
> Abar  = 20.4  # mean cenetered basal area m-2 ha-1
> 
  > n01   = 0.0034 # E.C. Amazonia
> Pvbar = 0.68 # mean centered precipitation coefficient of variation
> 
  > n02   = -0.0449 # E.C. Amazonia
> Sdbar = 5     # mean centered dry season length no months less than 100mm
> 
  > n03   = 0.0191  # E.C. Amazonia
> Tabar = 25.0  # mean centered annual averge temperature
> 
  > 
  > # Define height options
  > if (predheight == 1) {
    +   w <- which(is.na(cen$height_m))
    +   h.pred <- h.est(cen$dbh, cen$height_m)
    +   cen$height_m[w] <- h.pred[w]
    + } else if (predheight == 2) {
      +   w <- which(is.na(cen$height_m))
      +   cen$height_m[w] <- 10^(Bo + B1*log10(cen$dbh[w]/10) + Abar*So1 + n01*Pvbar + n02*Sdbar + n03*Tabar)
      + }
> # data cleaning
  >   dend$dendrometer_reading_mm[which(dend$dendrometer_reading_mm > 1000)] <- NaN
>   
  >   # format dates
  >   dend$dbh_first_date    <- as.Date(paste(dend$cenyear, dend$cenmonth, dend$cenday, sep="."), format="%Y.%m.%d") 
>   dend$date              <- as.Date(paste(dend$year, dend$month, dend$day, sep="."), format="%Y.%m.%d")
> dend$dendrometer_reading_mm <- as.numeric(dend$dendrometer_reading_mm) # Ignore error message. NA introduced by coercion is ok.
Warning message:
  NAs introduced by coercion 
>   dend$thisdbh_cm             <- (dend$dbh*pi) + ((dend$dendrometer_reading_mm/10)/pi)
> # estimate biomass of each tree for each new thisdbh_mm
  >   #loop through each tree to estimate biomass (bm) and convert to above ground carbon (agC)
  > for (ii in 1:length(dend$tree_tag)) {  
    +   thistree <- which(dend$tree_tag == dend$tree_tag[ii] & dend$year == dend$year[ii] & dend$month == dend$month[ii] & dend$day == dend$day[ii])     
    +   dbh_tree <- dend$thisdbh_cm[thistree]
    +   den_tree <- dend$density[thistree]
    +   h_tree   <- dend$height_m[thistree]
    +   
      +   # this uses allometric equations from allometricEquations.R
      +   if (allometrix == 2) {
        +     bm <- Chave2005_dry(diax=dbh_tree, density=den_tree, height=h_tree)
        +   } else if (allometrix == 3) {
          +     bm <- Chave2005_moist(diax=dbh_tree, density=den_tree, height=h_tree)
          +   } else if (allometrix == 4) {
            +     bm <- Chave2005_wet(diax=dbh_tree, density=den_tree, height=h_tree)
            +   } else if (allometrix == 5) {
              +     bm <- Chave2014(diax=dbh_tree, density=den_tree, height=h_tree)
              +   }
    +   
      +   ## TO DO ## error treatment remains to be done!
      +   
      +   # Unit conversions 
      +   
      +   dend$agC[ii] <- (bm)*(1/(2.1097*1000)) # convert kg to Mg=1/1000=10 and convert to carbon = 47.8% (ADD REF!! Txx et al?)
    +   dend$bm_kg[ii] <- (bm)
    + }
> dend$agCdiff    <- ave(dend$agC, dend$plot_code, FUN = function(x) c(NA, diff(x)))
> first_run = T
> for (ii in 1:length(dend$tree_tag)) {  
  +   thistree  <- which(dend$tree_tag == dend$tree_tag[ii])
  +   agC       <- dend$agC[thistree]
  +   tag       <- dend$tree_tag[thistree]
  +   agCdiff   <- dend$agCdiff[thistree]
  +   year      <- dend$year[thistree]
  +   month     <- dend$month[thistree]
  +   plot_code <- dend$plot_code[thistree]
  +   datediff  <- rbind(0/0, data.frame(diff(as.matrix(dend$date[thistree])))) #datediff <- data.frame(0/0, difftime(tail(dend$date[thistree], -1), head(dend$date[thistree], -1), units="days"))
  +   w         <- cbind (plot_code, tag, year, month, agC, agCdiff, datediff)
  +     if (first_run) {
    +       npp_tree <- w
    +       first_run = F
    +     } else {
      +       npp_tree <- rbind (npp_tree, w)
      +     }
  + }
> 
  > colnames(npp_tree) <- c("plot_code", "tag", "year", "month", "agC", "agCdiff", "datediff")
> npp_tree$nppacw_tree_day  <- npp_tree$agCdiff/npp_tree$datediff
> www                         <- sqldf("SELECT plot_code, year, month, AVG(nppacw_tree_day), STDEV(nppacw_tree_day) FROM npp_tree GROUP BY year, month")
>   colnames (www)              <- c("plot_code", "year", "month", "npp_avgtrees_day_dend", "npp_avgtrees_day_dend_sd")
>   www$npp_avgtrees_day_dend   <- as.numeric(www$npp_avgtrees_day_dend)
>   www$npp_avgtrees_month_dend <- www$npp_avgtrees_day_dend*29.6 
>   www$npp_avgtrees_yr_dend    <- www$npp_avgtrees_month_dend*12
> www$npp_avgtrees_month_dend_sd <- www$npp_avgtrees_day_dend_sd*29.6 
>   www$npp_avgtrees_yr_dend_sd    <- www$npp_avgtrees_month_dend_sd*12
> nppacw_cen  <- NPPacw_census(census, plotname="PAN-03", allometric_option="Default", height_correction_option="Default", census1_year=2013, census2_year=2014)
[1] "moist equation  is used for estimating AGB, model I.6 (see Chave et al., 2005)"
[1] "If you have height for more than 50 trees in your plot, estimate local diameter-height relationship. If not, choose height correction option 2."
Error in lm.fit(x, y, offset = offset, singular.ok = singular.ok, ...) : 
  0 (non-NA) cases
Called from: lm.fit(x, y, offset = offset, singular.ok = singular.ok, ...)
Browse[1]> 
  >   xxx <- sqldf("SELECT plot_code, AVG(npp_avgtrees_yr_dend) from www")
>   colnames(xxx) <- c("plot_code", "nppacw_dend")
>   sf  <- (xxx$nppacw_dend*length(unique(dend$tree_tag))) / nppacw_cen[,1]
Error: object 'nppacw_cen' not found
>   www$nppacw_month <- (www$npp_avgtrees_month_dend*length(unique(dend$tree_tag)))/sf
Error: object 'sf' not found
>   www$nppacw_month_sd <- (www$npp_avgtrees_month_dend_sd*length(unique(dend$tree_tag)))/sf # TO DO: so we want SE or SD???
Error: object 'sf' not found
> head(census)
plot_code tree_tag  dbh height_m   density year month day   plot
1    ACJ-01        1 12.9     7.50 0.5745640 2013     1  22 ACJ-01
2    ACJ-01        2 23.4     7.70 0.6392357 2013     1  22 ACJ-01
3    ACJ-01        3 11.9     7.00 0.6392357 2013     1  22 ACJ-01
4    ACJ-01        4 13.4     8.25 0.6392357 2013     1  22 ACJ-01
5    ACJ-01        5 31.2     9.50 0.6697630 2013     1  22 ACJ-01
6    ACJ-01        6 12.8     7.50 0.6392357 2013     1  22 ACJ-01
> nppacw_cen  <- NPPacw_census(census, plotname="ACJ-01", allometric_option="Default", height_correction_option="Default", census1_year=2013, census2_year=2014)
[1] "moist equation  is used for estimating AGB, model I.6 (see Chave et al., 2005)"
[1] "If you have height for more than 50 trees in your plot, estimate local diameter-height relationship. If not, choose height correction option 2."
> xxx <- sqldf("SELECT plot_code, AVG(npp_avgtrees_yr_dend) from www")
>   colnames(xxx) <- c("plot_code", "nppacw_dend")
>   sf  <- (xxx$nppacw_dend*length(unique(dend$tree_tag))) / nppacw_cen[,1]
>   www$nppacw_month <- (www$npp_avgtrees_month_dend*length(unique(dend$tree_tag)))/sf
>   www$nppacw_month_sd <- (www$npp_avgtrees_month_dend_sd*length(unique(dend$tree_tag)))/sf # TO DO: so we want SE or SD???
> yy <- data.frame((mean(www$nppacw_month, na.rm=T))*12,(mean(www$nppacw_month_sd, na.rm=T))*12) 
>   colnames(yy) <- c("nppacw_month", "nppacw_month_sd")
>   yy
nppacw_month nppacw_month_sd
1     2.482465        9.023387
>   nppacw_cen
NPPacw_MgC_ha_yr NPPacw_MgC_ha_yr_se
1         2.482465        1.252718e-08
> monthlynppacw             <- data.frame(cbind(as.character(www$plot_code), www$year, www$month, www$nppacw_month, www$nppacw_month_sd))
>   colnames(monthlynppacw)   <- c("plot_code", "year", "month", "nppacw_MgC_month", "nppacw_MgC_month_sd")
> plot(www$month, www$nppacw_month)
> plotit=T
> plot <- ggplot(data=www, aes(x=month, y=nppacw_month, na.rm=T)) +
  +                geom_point(colour='black', size=2) +
  +                #geom_ribbon(data=www, aes(ymin=nppacw_month-nppacw_month_se , ymax=nppacw_month+nppacw_month_se), alpha=0.2) +
  +                geom_errorbar(aes(ymin=nppacw_month-nppacw_month_sd, ymax=nppacw_month+nppacw_month_sd), width=.1) +             
  +                #scale_x_date(breaks = date_breaks("months"), labels = date_format("%b-%Y")) +  
  +                scale_colour_grey() + 
  +                theme(text = element_text(size=17), legend.title = element_text(colour = 'black', size = 17, hjust = 3, vjust = 7, face="plain")) +
  +                xlab("month") + ylab(expression(paste("NPP ACW (MgC ", ha^-1, mo^-1, ")", sep=""))) +
  +                theme_classic(base_size = 15, base_family = "") + 
  +                theme(legend.position="left") +
  +                theme(panel.border = element_rect(fill = NA, colour = "black", size = 1))
> 
  > plot
Warning message:
  Removed 1 rows containing missing values (geom_point). 
> plot(www$month, www$nppacw_month)
> 