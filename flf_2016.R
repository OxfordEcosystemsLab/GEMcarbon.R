### Function fine litter fall: 
# This function uses data to calculate NPP from fine litterfall.

## Read-in data:
#setwd("/Users/cecile/Dropbox/GEMcarbondb/db_csv/db_csv_2015/readyforupload_db/acj_pan")
#data_flf <- read.table("/Users/cecile/Dropbox/GEMcarbondb/db_csv/db_csv_2015/readyforupload_db/acj_pan/Litterfall_ACJ_2013_2014_test.csv", sep=",", header=T)

# this is what we have in db:
# names(data_flf) <- c("plot_code", "year","month", "day","litterfall_trap_num", "litterfall_trap_size_m2","leaves_g_per_trap","twigs_g_per_trap","flowers_g_per_trap","fruits_g_per_trap",
# "bromeliads_g_per_trap", "epiphytes_g_per_trap","other_g_per_trap", "palm_leaves_g", "palm_flower_g", "palm_fruit_g", "quality_code", "comments")

# plotsize = 1 ha  ### TO DO: Different plot size is not an option yet. 

# Attention!! In some plots, data is collected twice a month (L. 92 : multiply by 2 because collected twice a month). In other plots, data are collected monthly. So do not multiply by 2.
# TO DO: We need to change this to divide by the collection time interval rather than *2 for collected twice a month!!

library(zoo)
library(sqldf)
require(ggplot2)
library(dplyr)

flf <- function(data_flf, ..., ret_type = c("concat", "list")) {
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


flf_oneplot <- function(data_flf, plotname, ret="monthly.means.ts", plotit=F) {   # add plotsize=1   
  # ret = monthly.means.subplot or monthly.means.ts for plot averages.
  if (class(data_flf) != "data.frame") { # if it's not a dataframe, assume it's a path+filename
    data_flf <- read.csv(data_flf)
  }
    
  # some data was getting imported in the wrong format
  data_flf$fruits_g_per_trap = as.numeric(data_flf$fruits_g_per_trap)
  data_flf$seeds_g_per_trap = as.numeric(as.character(data_flf$seeds_g_per_trap))
  data_flf$day = as.integer(as.character(data_flf$day))
    
  # new data frame
  data_flf2 <- c()  
  
  # define each parameter
  if (missing(plotname)) {  # calculate for first-mentioned plot if plot not specified.  rethink whether we should really have this here...
    plotname = data_flf$plot_code[1]
  }

  data_flf2 = subset(data_flf, plot_code == plotname)
  
  data_flf2 = data_flf2 %>% dplyr::rename(plot = plot_code,
                                   num = litterfall_trap_num,
                                   leaves = leaves_g_per_trap,
                                   twigs = twigs_g_per_trap,
                                   flowers = flowers_g_per_trap,
                                   fruits = fruits_g_per_trap,
                                   brom = bromeliads_g_per_trap,
                                   epi = epiphytes_g_per_trap,
                                   other = other_g_per_trap) %>% 
                            dplyr::mutate(seeds = NA,
                                   date = as.Date(paste(data_flf2$year, data_flf2$month, data_flf2$day, sep="."), format="%Y.%m.%d"))
                            # add a column for seeds and replace with : data_flf$seeds[which(plotname==data_flf2$plot)]
  
  
  # data_flf2$plot    <- data_flf$plot_code[which(plotname==data_flf$plot_code)]
  # data_flf2$year    <- data_flf$year[which(plotname==data_flf2$plot)]
  # data_flf2         <- data.frame(data_flf2)
  # data_flf2$month   <- data_flf$month[which(plotname==data_flf2$plot)]
  # data_flf2$day     <- data_flf$day[which(plotname==data_flf2$plot)]
  # data_flf2$date       <- as.Date(paste(data_flf2$year, data_flf2$month, data_flf2$day, sep="."), format="%Y.%m.%d")  
  # data_flf2$num     <- data_flf$litterfall_trap_num[which(plotname==data_flf2$plot)]
  # data_flf2$leaves  <- data_flf$leaves_g_per_trap[which(plotname==data_flf2$plot)]   
  # data_flf2$twigs   <- data_flf$twigs_g_per_trap[which(plotname==data_flf2$plot)]
  # data_flf2$flowers <- data_flf$flowers_g_per_trap[which(plotname==data_flf2$plot)]
  # data_flf2$fruits  <- data_flf$fruits_g_per_trap[which(plotname==data_flf2$plot)]
  # data_flf2$seeds   <- NA # add a column for seeds and replace with : data_flf$seeds[which(plotname==data_flf2$plot)]
  # data_flf2$brom    <- data_flf$bromeliads_g_per_trap[which(plotname==data_flf2$plot)]
  # data_flf2$epi     <- data_flf$epiphytes_g_per_trap[which(plotname==data_flf2$plot)]
  # data_flf2$other   <- data_flf$other_g_per_trap[which(plotname==data_flf2$plot)]
  
  # Calculate total litterfall (sum of branches, leaves, flowers, fruits, seeds, Broms, Epiphs, other...):
  x <- cbind(data_flf2$leaves, data_flf2$twigs, data_flf2$flowers, data_flf2$fruits, data_flf2$seeds, data_flf2$brom, data_flf2$epi, data_flf2$other)   
  data_flf2$total <- rowSums(x, na.rm = T)
  
  # In some cases, only total litterfall is recorded
  total_only = data_flf2$total == 0 & ! is.na(data_flf2$total_litter_g_per_trap)
  data_flf2[total_only,]$total = data_flf2[total_only,]$total_litter_g_per_trap
  
  ### Sanity check of the inputs.
  
  data_flf2$total[which(data_flf2$total>300)] <- NA   # remove outliers with totalf > 300
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
  
    
  for (i in 1:length(data_flf2$num)) { 
    sub       <- subset(data_flf2, subset=(data_flf2$codeb == uid[i]))
    if(length(sub$codeb) > 1) {
      meas_int      <- difftime(sub$date[1:(length(sub$date)-1)], sub$date[2:length(sub$date)], units="days")
      meas_int_num  <- as.numeric(as.character(meas_int))
  
      aleaves       <- tail(sub$leaves,-1)
      atwigs        <- tail(sub$twigs,-1)
      aflowers      <- tail(sub$flowers,-1)
      afruits       <- tail(sub$fruits,-1)
      abrom         <- tail(sub$brom,-1)
      aepi          <- tail(sub$epi,-1)
      aother        <- tail(sub$other,-1)
      atotal        <- tail(sub$total,-1)
      
      bleaves       <- aleaves/(-meas_int_num) 
      btwigs        <- atwigs/(-meas_int_num)
      bflowers      <- aflowers/(-meas_int_num)
      bfruits       <- afruits/(-meas_int_num)
      bbrom         <- abrom/(-meas_int_num)
      bepi          <- aepi/(-meas_int_num)
      bother        <- aother/(-meas_int_num)
      btotal        <- atotal/(-meas_int_num)
      
      id            <- tail(sub$codew,-1) 
      
      xx            <- c(xx, id)
      yy            <- c(yy, meas_int_num)
      aa            <- c(aa, bleaves)
      bb            <- c(bb, btwigs)
      cc            <- c(cc, bflowers)
      dd            <- c(dd, bfruits)
      ee            <- c(ee, bbrom)
      ff            <- c(ff, bepi)
      gg            <- c(gg, bother)
      hh            <- c(hh, btotal)

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
  }
  error_df_g <<- error_df
  print(paste(nrow(error_df), "errors in the data.  See error_df_g."))
  data2 <- data.frame(cbind(xx, yy, aa, bb, cc, dd, ee, ff, gg, hh))
  colnames(data2) <- c("id", "meas_int_days", "bleavesflf_g_trap_day", "btwigs", "bflowers", "bfruits", "bbrom", "bepi", "bother", "btotal")
  
  # get day, month, year from data_flf2
  
  data3 <- sqldf("SELECT data_flf2.*, data2.* FROM data2 JOIN data_flf2 ON data2.id = data_flf2.codew") 
  
  # OR you can set the data type in your sqldf("...", method = c("character", "numeric", "numeric", "numeric"))
  
  data3$leavesflf_g_trap_day <- as.numeric(as.character(data3$bleavesflf_g_trap_day))
  data3$meas_int_days        <- as.numeric(as.character(data3$meas_int_days))
  data3$twigs                <- as.numeric(as.character(data3$btwigs))
  data3$flowers              <- as.numeric(as.character(data3$bflowers))
  data3$fruits               <- as.numeric(as.character(data3$bfruits))
  data3$brom                 <- as.numeric(as.character(data3$bbrom))
  data3$epi                  <- as.numeric(as.character(data3$bepi))
  data3$other                <- as.numeric(as.character(data3$bother))
  data3$total                <- as.numeric(as.character(data3$btotal))
  
  ### Conversions: flf per ha per day (for each trap)
  
  # Raw data is in g / litter trap = g / 0.25m2
  # Convert to ha: *(10000/0.25)
  # Convert to Mg: *1 g = 0.000001 Mg
  # Convert to C: *0.49
  
  data3$leavesflf_MgC_ha_month  <- (((data3$leavesflf_g_trap_day*(10000/0.25))*0.000001)*0.49)*30 # TO DO: multiply by number of days in that month, not 30.
  data3$twigsflf   <- (((data3$twigs*(10000/0.25))*0.000001)*0.49)*30
  data3$flowersflf <- (((data3$flowers*(10000/0.25))*0.000001)*0.49)*30
  data3$fruitsflf  <- (((data3$fruits*(10000/0.25))*0.000001)*0.49)*30
  data3$bromflf    <- (((data3$brom*(10000/0.25))*0.000001)*0.49)*30
  data3$epiflf     <- (((data3$epi*(10000/0.25))*0.000001)*0.49)*30
  data3$otherflf   <- (((data3$other*(10000/0.25))*0.000001)*0.49)*30
  data3$totalflf   <- (((data3$total*(10000/0.25))*0.000001)*0.49)*30
  
  # flf per ha per month (for each trap)
  data4 = data3 %>% group_by(plot, num, year, month) %>% 
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
  
  data5            <- sqldf("SELECT plot, year, month, 
                            AVG(leavesflf_MgC_ha_month), AVG(twigsflf), AVG(flowersflf), AVG(fruitsflf), AVG(bromflf), AVG(epiflf), AVG(otherflf), AVG(totalflf),  
                            STDEV(leavesflf_MgC_ha_month), STDEV(twigsflf), STDEV(flowersflf), STDEV(fruitsflf), STDEV(bromflf), STDEV(epiflf), STDEV(otherflf), STDEV(totalflf)
                            FROM data3 GROUP BY plot, year, month")
  
  colnames(data5)  <- c("plot", "year", "month","leavesflf_MgC_ha_month", "twigsflf_MgC_ha_month", "flowersflf_MgC_ha_month", "fruitsflf_MgC_ha_month", "bromflf_MgC_ha_month", "epiflf_MgC_ha_month", "otherflf_MgC_ha_month", "totalflf_MgC_ha_month", "sd_leavesflf", "sd_twigsflf", "sd_flowersflf", "sd_fruitsflf", "sd_bromflf", "sd_epiflf", "sd_otherflf", "sd_totalflf") # Note: SD is 0 if we only have a year's worth of data!
  
  
  # calculate standard error sd/sqrt(length(unique(data3$year)))
  
  data5$se_leavesflf  <- data5$sd_leavesflf/sqrt(length(unique(data3$num)))
  data5$se_twigsflf   <- data5$sd_twigsflf/sqrt(length(unique(data3$num))) 
  data5$se_flowersflf <- data5$sd_flowersflf/sqrt(length(unique(data3$num))) 
  data5$se_fruitsflf  <- data5$sd_fruitsflf/sqrt(length(unique(data3$num))) 
  data5$se_bromflf    <- data5$sd_bromflf/sqrt(length(unique(data3$num))) 
  data5$se_epiflf     <- data5$sd_epiflf/sqrt(length(unique(data3$num))) 
  data5$se_otherflf   <- data5$sd_otherflf/sqrt(length(unique(data3$num))) 
  data5$se_totalflf   <- data5$sd_totalflf/sqrt(length(unique(data3$num)))
  
  
  # TO DO: PLOT ROUTINE NEEDS WORK. DOESN'T work at the moment.
  
  ## Plotroutine, triggered by argument 'plotit=T' 
  data5$date      <- strptime(paste(as.character(data5$year), as.character(data5$month), as.character(15), sep="-"), format="%Y-%m-%d")
  data5$date <- as.POSIXct(data5$date)
  data5$yearmonth <- as.yearmon(data5$date)
  
  if (plotit==T) {
    top <- data5$totalflf_MgC_ha_month + data5$sd_totalflf
    data5$date <- as.Date(data5$date)
    plot1 <- ggplot(data = data5, aes(x = date, y = totalflf_MgC_ha_month, na.rm = T)) +
                    geom_line(linetype = 'solid', colour = 'black', size = 1) +
                    geom_ribbon(data = data5, aes(ymin = totalflf_MgC_ha_month - sd_totalflf, ymax = totalflf_MgC_ha_month + sd_totalflf), alpha = 0.2) +
                    scale_x_date(breaks = date_breaks("months"), labels = date_format("%b-%Y")) +            
                    scale_colour_grey() + 
                    theme(text = element_text(size=17), legend.title = element_text(colour = 'black', size = 17, hjust = 3, vjust = 7, face="plain")) +
                    ylim(0, max(top, na.rm = T)) +                          
                    xlab("") + ylab(expression(paste("Total fine litterfall (MgC ", ha^-1, mo^-1, ")", sep=""))) +
                    theme_classic(base_size = 15, base_family = "") + 
                    theme(legend.position="left") +
                    theme(panel.border = element_rect(fill = NA, colour = "black", size = 1)) 
                    #theme(legend.title=element_blank()) + theme(legend.key = element_blank())   
    
    top <- data5$branchflfAs+data5$branchflfAsstd
    plot2 <- ggplot(data = data5, aes(x=date, y=branchflfAs, na.rm=T)) +
                    geom_line(linetype='solid', colour='black', size=1) +
                    geom_ribbon(data = data5, aes(ymin=branchflfAs-branchflfAsstd , ymax=branchflfAs+branchflfAsstd), alpha=0.2) +
                    geom_line(data = data5, aes(x=date, y=leafflfAs, na.rm=T), linetype='dotted', colour='black', size=1) +
                    geom_ribbon(data = data5, aes(ymin=leafflfAs-leafflfAsstd, ymax=leafflfAs+leafflfAsstd), alpha=0.2) +
                    scale_x_date(breaks = date_breaks("months"), labels = date_format("%b-%Y")) +  
                    scale_colour_grey() + 
                    theme(text = element_text(size=17), legend.title = element_text(colour = 'black', size = 17, hjust = 3, vjust = 7, face="plain")) +
                    ylim(0, max(top, na.rm=T)) +                          
                    xlab("") + ylab(expression(paste("Fine litterfall components (MgC ", ha^-1, mo^-1, ")", sep=""))) +
                    theme_classic(base_size = 15, base_family = "") + 
                    theme(legend.position="left") +
                    theme(panel.border = element_rect(fill = NA, colour = "black", size = 1)) 
    
    top <- data5$flowerflfAs + data5$flowerflfAsstd
    plot3 <- ggplot(data = data5, aes(x=date, y=flowerflfAs, na.rm=T)) +
                    geom_line(linetype='solid', colour='black', size=1) +
                    geom_ribbon(data = data5, aes(ymin=flowerflfAs-flowerflfAsstd , ymax=flowerflfAs+flowerflfAsstd), alpha=0.2) +
                    geom_line(data = data5, aes(x=date, y=fruitflfAs, na.rm=T), linetype='dotted', colour='black', size=1) +
                    geom_ribbon(data = data5, aes(ymin=fruitflfAs-fruitflfAsstd, ymax=fruitflfAs+fruitflfAsstd), alpha=0.2) +
                    scale_x_date(breaks = date_breaks("months"), labels = date_format("%b-%Y")) +  
                    scale_colour_grey() + 
                    theme(text = element_text(size=17), legend.title = element_text(colour = 'black', size = 17, hjust = 3, vjust = 7, face="plain")) +
                    ylim(0, max(top, na.rm=T)) +                          
                    xlab("") + ylab(expression(paste("Fine litterfall components (MgC ", ha^-1, mo^-1, ")", sep=""))) +
                    theme_classic(base_size = 15, base_family = "") + 
                    theme(legend.position="left") +
                    theme(panel.border = element_rect(fill = NA, colour = "black", size = 1)) 
    
    fig <- grid.arrange(plot1, plot2, plot3, ncol=1, nrow=3)
  }
  
  
# Return either monthly means (ret="monthly.means") or annual means (ret="annual.means")  
  switch(ret,
         monthly.means.subplot = {return(data4)},
         monthly.means.ts = {return(data5)}
         )
}

