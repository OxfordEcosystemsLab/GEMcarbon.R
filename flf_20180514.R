### Function fine litter fall: 
# This function uses data to calculate NPP from fine litterfall.

## Read-in data:

# this is what we have in db:
# names(data_flf) <- c("plot_code", "year","month", "day","litterfall_trap_num", "litterfall_trap_size_m2","leaves_g_per_trap","twigs_g_per_trap","flowers_g_per_trap","fruits_g_per_trap",
# "bromeliads_g_per_trap", "epiphytes_g_per_trap","other_g_per_trap", "palm_leaves_g", "palm_flower_g", "palm_fruit_g", "quality_code", "comments")

# plotsize = 1 ha  ### TO DO: Different plot size is not an option yet. 

#library(zoo)
require(ggplot2)
library(dplyr)


flf <- function(data_flf, ..., ret_type = c("concat", "list")) {
  
  # the following lines are to ensure each column is in the correct format
  script.dir <- function() {
    getSrcDirectory(script.dir);
  }
  source(paste(script.dir(), "functions.r", sep = "/"))
  
  
  flf_column_types = c(
    "plot_code" = "character",
    "year" = "integer",
    "month" = "integer",
    "day" = "integer",
    "litterfall_trap_num" = "Factor",
    "litterfall_trap_size_m2" = "numeric",
    "leaves_g_per_trap" = "numeric",
    "twigs_g_per_trap" = "numeric",
    "flowers_g_per_trap" = "numeric",
    "fruits_g_per_trap" = "numeric",
    "seeds_g_per_trap" = "numeric",
    "bromeliads_g_per_trap" = "numeric",
    "epiphytes_g_per_trap" = "numeric",
    "other_g_per_trap" = "numeric",
    "palm_leaves_g_per_trap" = "numeric",
    "palm_flower_g_per_trap" = "numeric",
    "palm_fruit_g_per_trap" = "numeric",
    "total_litter_g_per_trap" = "numeric",
    "quality_code" = "factor",
    "comments" = "character"
  )
  
  
  
  # set column datatypes as defined above # TO DO; this doesn't work. Error: attempt to apply non-function.
  # data_flf = set_df_coltypes(data_flf, flf_column_types)
  
  if (class(data_flf) != "data.frame") { # if it's not a dataframe, assume it's a path+filename
    data_flf <- read.csv(data_flf)
  }
  ret_type = match.arg(ret_type)
  
  pb = txtProgressBar(max = length(unique(data_flf$plot_code)), style = 3); i = 0
  output = list()
  first_run = T
  for (thisplot in unique(data_flf$plot_code)) {
    output[[thisplot]] = flf_oneplot(data_flf, thisplot, ...)
    if (first_run) {
      first_run = F
      output_concat = output[[thisplot]]
    } else {
      output_concat = rbind(output_concat, output[[thisplot]])
    }
    i = i + 1
    setTxtProgressBar(pb, i)
  }
  close(pb)
  
  if (ret_type == "list") { # return plot results in different list elements
    return(output)
  } else { # return results concatenated across plots
    return(output_concat)
  }
  
}

# START HERE

#flf_oneplot <- function(data_flf, plotname, ret="monthly.means.ts", verbose = T) {   
  # add plotsize=1   
  # ret = monthly.means.subplot or monthly.means.ts for plot averages.
  # verbose = print out unique id's for debugging or otherwise
  
  if (class(data_flf) != "data.frame") { # if it's not a dataframe, assume it's a path+filename
    data_flf <- read.csv(data_flf)
  }
  
  # some data was getting imported in the wrong format
  data_flf$leaves_g_per_trap = as.numeric(as.character(data_flf$leaves_g_per_trap))
  data_flf$fruits_g_per_trap = as.numeric(as.character(data_flf$fruits_g_per_trap))
  data_flf$twigs_g_per_trap = as.numeric(as.character(data_flf$twigs_g_per_trap))
  data_flf$seeds_g_per_trap = as.numeric(as.character(data_flf$seeds_g_per_trap))
  data_flf$total_litter_g_per_trap = as.numeric(as.character(data_flf$total_litter_g_per_trap))
  data_flf$day = as.integer(as.character(data_flf$day))
  
  # new data frame
  #data_flf2 <- c()  
  
  data_flf2 = data_flf %>% dplyr::rename(plot = plot_code,
                                          num = litterfall_trap_num,
                                          leaves = leaves_g_per_trap,
                                          twigs = twigs_g_per_trap,
                                          flowers = flowers_g_per_trap,
                                          fruits = fruits_g_per_trap,
                                          brom = bromeliads_g_per_trap,
                                          epi = epiphytes_g_per_trap,
                                          other = other_g_per_trap) %>% 
                                   dplyr::mutate(seeds = NA,
                                          date = as.Date(paste(data_flf$year, data_flf$month, data_flf$day, sep="-"), format="%Y-%m-%d"),
                                          date = as.POSIXct(date),
                                          total = select(., leaves:palm_fruit_g_per_trap) %>% apply(1, sum, na.rm=TRUE)) 
   
  
  # In some cases, only total litterfall is recorded
  total_only = data_flf2$total == 0 & ! is.na(data_flf2$total_litter_g_per_trap)
  data_flf2[total_only,]$total = data_flf2[total_only,]$total_litter_g_per_trap
  
  
  ### Sanity check of the inputs.
  
  data_flf2$total[which(data_flf2$total>1500)] <- NA   # remove outliers with totalf > 1000
  data_flf2$total[which(data_flf2$total<0)]   <- NA   # remove implausible totallf (negative litter)
  
  # Calculate leaf area ****need density from photos, we assume average SLA = 100g/m2
  # leaflaifA = leaffA/100   # convert to area     
  
  ### flf per trap per day
  
  # TO DO: For the first collection interval, assume 14 days. At the moment, the code ignores the first collection.
  
  data_flf2$codeb <- paste(data_flf2$plot, data_flf2$num, sep=".") 
  data_flf2$codew <- paste(data_flf2$plot, data_flf2$num, data_flf2$year, data_flf2$month, data_flf2$day, sep=".") 
  uid             <- unique(data_flf2$codeb)
  xx              <- c()
  yy              <- c()
  aa              <- c()
  bb              <- c()
  cc              <- c()
  dd              <- c()
  ee              <- c()
  ff              <- c()
  gg              <- c()
  hh              <- c()
  
  pb = txtProgressBar(max = length(uid), style = 3)
  
  for (i in 1:length(data_flf2$num)) { 
    sub       <- subset(data_flf2, subset=(data_flf2$codeb == uid[i]))
    sub       <- arrange(sub, date)                      # Order subset by date.
    
    if(length(sub$codeb) > 1) {
      meas_int      <- get_time_diffs(sub$date)          # difference between collection dates, in days. See get_time_diffs in functions.r
      aleaves       <- tail(sub$leaves,-1)
      atwigs        <- tail(sub$twigs,-1)
      aflowers      <- tail(sub$flowers,-1)
      afruits       <- tail(sub$fruits,-1)
      abrom         <- tail(sub$brom,-1)
      aepi          <- tail(sub$epi,-1)
      aother        <- tail(sub$other,-1)
      atotal        <- tail(sub$total,-1)
      
      bleaves       <- aleaves/(meas_int) 
      btwigs        <- atwigs/(meas_int)
      bflowers      <- aflowers/(meas_int)
      bfruits       <- afruits/(meas_int)
      bbrom         <- abrom/(meas_int)
      bepi          <- aepi/(meas_int)
      bother        <- aother/(meas_int)
      btotal        <- atotal/(meas_int)
      
      id            <- tail(sub$codew,-1) 
      
      xx            <- c(xx, id)
      yy            <- c(yy, meas_int)
      aa            <- c(aa, bleaves)
      bb            <- c(bb, btwigs)
      cc            <- c(cc, bflowers)
      dd            <- c(dd, bfruits)
      ee            <- c(ee, bbrom)
      ff            <- c(ff, bepi)
      gg            <- c(gg, bother)
      hh            <- c(hh, btotal)
      
      print(xx)
      
    } else {  
      # print(paste("row number:", i))
      # print(paste("trap number:", sub$num))
      # print(paste("subset length:", length(sub$codeb)))
      if(exists("error_df")) {
        error_df <- rbind(error_df, data.frame(row = i, trap = sub$num[i], sub_len = length(sub$codeb)))
      } else {
        error_df <- data.frame(row = i, trap = sub$num[i], sub_len = length(sub$codeb))
      }
    }
    setTxtProgressBar(pb, i)
  }
  close(pb)
  
  error_df_g <<- error_df # assigning to global variable outside the function.
  print(paste(nrow(error_df), "errors in the data.  See error_df_g."))
  data2 <- data.frame(xx, yy, aa, bb, cc, dd, ee, ff, gg, hh)
  colnames(data2) <- c("id", "meas_int_days", "bleavesflf_g_trap_day", "btwigs", "bflowers", "bfruits", "bbrom", "bepi", "bother", "btotal")
  

  # Separate info in id
  # paste(data_flf2$plot, data_flf2$num, data_flf2$year, data_flf2$month, data_flf2$day, sep=".") 
  data2$id                = as.character(data2$id)
  temp                    = strsplit(data2$id, "[.]")                     # split this_core into the information we need (plot_code, year, month, day, ingrowth_core_num).
  data2$plot_code         = unlist(lapply(temp, `[[`, 1))
  data2$num               = unlist(lapply(temp, `[[`, 2))
  data2$year              = unlist(lapply(temp, `[[`, 3))
  data2$month             = unlist(lapply(temp, `[[`, 4)) 
  data2$day               = unlist(lapply(temp, `[[`, 5))
  data2$collectiondate    = as.Date(paste(data2$year, data2$month, data2$day, sep="-"), format="%Y-%m-%d") 
  
  
  ### Conversions: flf per ha per month (for each trap)
  
  # Raw data is in g / litter trap = g / 0.25m2
  # Convert to ha: *(10000/0.25)
  # Convert to Mg: *1 g = 0.000001 Mg
  # Convert to C: *0.49
  data3 = data2 %>% mutate(leavesflf_MgC_ha_month = (((bleavesflf_g_trap_day*(10000/0.25))*0.000001)*0.49)*30, 
                           twigsflf   = (((btwigs*(10000/0.25))*0.000001)*0.49)*30,
                           flowersflf = (((bflowers*(10000/0.25))*0.000001)*0.49)*30,
                           fruitsflf  = (((bfruits*(10000/0.25))*0.000001)*0.49)*30,
                           bromflf    = (((bbrom*(10000/0.25))*0.000001)*0.49)*30,
                           epiflf     = (((bepi*(10000/0.25))*0.000001)*0.49)*30,
                           otherflf   = (((bother*(10000/0.25))*0.000001)*0.49)*30,
                           totalflf   = (((btotal*(10000/0.25))*0.000001)*0.49)*30,
                           plot_code = plot_code,
                           num = num,
                           year = year, 
                           month = month,
                           collectiondate = collectiondate)
  
  data3 = na_if(data3, Inf)
  
  # flf per ha per month (for each trap) 
  data4 = data3 %>% group_by(plot_code, num, year, month) %>%
    dplyr::summarize(leavesflf_MgC_ha_month_trap = mean(leavesflf_MgC_ha_month, na.rm = T),
                     twigsflf_MgC_ha_month_trap = mean(twigsflf, na.rm = T),
                     flowersflf_MgC_ha_month_trap = mean(flowersflf, na.rm = T),
                     fruitsflf_MgC_ha_month_trap = mean(fruitsflf, na.rm = T),
                     bromflf_MgC_ha_month_trap = mean(bromflf, na.rm = T),
                     epiflf_MgC_ha_month_trap = mean(epiflf, na.rm = T),
                     otherflf_MgC_ha_month_trap = mean(otherflf, na.rm = T),
                     totalflf_MgC_ha_month_trap = mean(totalflf, na.rm = T),
                     interval = - mean(meas_int_days, na.rm = T),
                     sd_leavesflf = sd(leavesflf_MgC_ha_month, na.rm = T),
                     sd_twigsflf = sd(twigsflf, na.rm = T),
                     sd_flowersflf = sd(flowersflf, na.rm = T),
                     sd_fruitsflf = sd(fruitsflf, na.rm = T),
                     sd_bromflf = sd(bromflf, na.rm = T),
                     sd_epiflf = sd(epiflf, na.rm = T),
                     sd_otherflf = sd(otherflf, na.rm = T),
                     sd_totalflf = sd(totalflf, na.rm = T)) %>%
    dplyr::rename(litterfall_trap_num = num)
  
  # calculate standard error sd/sqrt(length(unique(data3$year)))
  
  data4$se_leavesflf  <- data4$sd_leavesflf/sqrt(length(unique(data3$year)))
  data4$se_twigsflf   <- data4$sd_twigsflf/sqrt(length(unique(data3$year))) 
  data4$se_flowersflf <- data4$sd_flowersflf/sqrt(length(unique(data3$year))) 
  data4$se_fruitsflf  <- data4$sd_fruitsflf/sqrt(length(unique(data3$year))) 
  data4$se_bromflf    <- data4$sd_bromflf/sqrt(length(unique(data3$year))) 
  data4$se_epiflf     <- data4$sd_epiflf/sqrt(length(unique(data3$year))) 
  data4$se_otherflf   <- data4$sd_otherflf/sqrt(length(unique(data3$year))) 
  data4$se_totalflf   <- data4$sd_totalflf/sqrt(length(unique(data3$year)))
  
  # flf per ha per month (average of all the traps)
  # calculate standard error sd/sqrt(length(unique(data3$year)))

  data5 = data3 %>% group_by(plot_code, year, month) %>% 
    dplyr::summarize(leavesflf_MgC_ha_month = mean(leavesflf_MgC_ha_month, na.rm = T),
                     twigsflf_MgC_ha_month = mean(twigsflf, na.rm = T),
                     flowersflf_MgC_ha_month = mean(flowersflf, na.rm = T),
                     fruitsflf_MgC_ha_month = mean(fruitsflf, na.rm = T),
                     bromflf_MgC_ha_month = mean(bromflf, na.rm = T),
                     epiflf_MgC_ha_month = mean(epiflf, na.rm = T),
                     otherflf_MgC_ha_month = mean(otherflf, na.rm = T),
                     totalflf_MgC_ha_month = mean(totalflf, na.rm = T),
                     sd_leavesflf = sd(leavesflf_MgC_ha_month, na.rm = T),
                     sd_twigsflf = sd(twigsflf, na.rm = T),
                     sd_flowersflf = sd(flowersflf, na.rm = T),
                     sd_fruitsflf = sd(fruitsflf, na.rm = T),
                     sd_bromflf = sd(bromflf, na.rm = T),
                     sd_epiflf = sd(epiflf, na.rm = T),
                     sd_otherflf = sd(otherflf, na.rm = T),
                     sd_totalflf = sd(totalflf, na.rm = T)) 
  
    data5 = data.frame(data5)
  
  
  data5$se_leavesflf  <- data5$sd_leavesflf/sqrt(length(unique(data3$num)))
  data5$se_twigsflf   <- data5$sd_twigsflf/sqrt(length(unique(data3$num))) 
  data5$se_flowersflf <- data5$sd_flowersflf/sqrt(length(unique(data3$num))) 
  data5$se_fruitsflf  <- data5$sd_fruitsflf/sqrt(length(unique(data3$num))) 
  data5$se_bromflf    <- data5$sd_bromflf/sqrt(length(unique(data3$num))) 
  data5$se_epiflf     <- data5$sd_epiflf/sqrt(length(unique(data3$num))) 
  data5$se_otherflf   <- data5$sd_otherflf/sqrt(length(unique(data3$num))) 
  data5$se_totalflf   <- data5$sd_totalflf/sqrt(length(unique(data3$num)))
  
  
  # NPP litterfall in g m-2 mo-1
  
  data5$totalflf_g_m2_mo <- data5$totalflf_MgC_ha_month * 0.49 * 10000 * 0.000001
  
  # month as numeric
  #data5$month = as.numeric(as.character(data5$month))
  
  "ACJ-01", "ESP-01", "WAY-01", "TRU-04", "SPD-01", "SPD-02", "PAN-02", "PAN-03" 
  "ALP-11", "ALP-12", "JEN-11", "JEN-12", "KEN-01", "KEN-02", "TAM-05", "TAM-06", "TAM-09"
  "AGU-01", "REQ-14", "NXV-01", "NXV-02", "BLZ-11", "BLZ-12", "BLZ-21", "BLZ-22", 
  
  "STB-08", "STB-12", "STD-05", "STD-10", "STD-11", "STJ-01", "STJ-04", "STJ-05", "STL-09", "STL-10", "STN-02", "STN-03", "STN-04", "STN-06", "STN-09", "STO-03", "STO-06", "STO-07", "STQ-08", "STQ-11", 
  
  "ANK-01", "ANK-02", "ANK-03", "BOB-01", "BOB-02", "BOB-03", "BOB-04", "BOB-05", "BOB-06", "KOG-02", "KOG-03", "KOG-04", "KOG-05", "KOG-06" 
  
  "LPG-01", "LPG-02", "MNG-03", "MNG-04", "IVI-01", "IVI-02",  
  
  "MLA-01", "MLA-02", "DAN-04", "DAN-05", "SAF-01", "SAF-02", "SAF-03", "SAF-04", "SAF-05" 
  
  
  # plot it
  plotit = subset(data5, plot_code %in% c("LPG-01", "LPG-02", "MNG-03", "MNG-04", "IVI-01", "IVI-02"))
  plotit %>% group_by(plot_code) %>% arrange(plot_code, month, year) %>% 
    ggplot(data=., aes(month, leavesflf_MgC_ha_month, colour=year)) + geom_point() +
    #ylim(0,1.5) +
    facet_wrap(~plot_code)
  
  #check outlyers
  plotit = subset(data3, plot_code %in% c("IVI-02"))
  plotit %>% group_by(plot_code) %>% arrange(plot_code, month, year) %>% 
    ggplot(data=., aes(month, leavesflf_MgC_ha_month, colour=year)) + geom_point() 
  #+
    #ylim(0,1.5) +
    #facet_wrap(~plot_code)
  
  
  subset(data5, leavesflf_MgC_ha_month >= 2)
  
  
  # Return either monthly means (ret="monthly.means") or annual means (ret="annual.means")  
  #switch(ret,
  #       monthly.means.subplot = {return(data4)},
  #       monthly.means.ts = {return(data5)}
 # )
#}
