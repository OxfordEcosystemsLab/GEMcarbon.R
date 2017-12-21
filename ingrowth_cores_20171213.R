## This function calculates fine root productivity
# Based on Matlab code by C. Doughty (2013)
# Updated and adapted to GEM plots by C. Girardin (2015)

## option = 1: this means the time steps 10,20,30,40 minutes are chosen
## option = 2: this means the time steps 5, 10, 15 minutes are chosen
# ATTENTION!! Make sure that 1st timestep is always followed by timesteps 2,3,4 in that order.
# Note: options have been obviated in leiu of pasing in timesteps


NPProot_ic_oneplot <- function(datafile, plotname, logmodel = T, fine_root_cor = "Default", tubed = 0.07, remove_stock_meas = F, ret = "monthly.means.ts", tx) {  
  
  # If stock measurements aren't removed, they're used as the beginning of the first interval (otherwise assumed to be 90 days)
  
  library(sqldf)
  library(ggplot2)
  library(scales)
  library(nlme)
  library(dplyr)
  
  coef_func = ifelse(logmodel, coef, coefficients) # nls & lm have different methods for extracting coefs.  use this when testing exponent > 1
  
  if (class(datafile) != "data.frame") { # if it's not a dataframe, assume it's a path + filename
    data.ic <- read.csv(datafile, na.strings = c("NA", "NaN"))
  } else {
    data.ic = datafile # data.frame passed in directly
  }
  
  data <- subset(data.ic, plot_code == plotname)
  
  # clean NAs
  rawic1[rawic1 == 'NA'] <- NA
  
  # Remove stock measurement
  
  if (remove_stock_meas) {
    stock_meas = rep(F, nrow(data))
    stock_meas[data$is_stock %in% "y"] = T
    data <- data[!stock_meas,]
  } 
  
  if(nrow(data) == 0) { return(NA) }
  
  
  # re-name columns rather than building new ones!
  data$ol_under2 = data$ol_under_2mm_g   
  data$ol_2to3   = data$ol_2to3_mm_g 
  data$ol_3to4   = data$ol_3to4_mm_g 
  data$ol_4to5   = data$ol_4to5_mm_g   
  data$ol_above5 = data$ol_above_5mm_g 
  data$ml_under2 = data$ml_under_2mm_g                                
  data$ml_2to3   = data$ml_2to3_mm_g                           
  data$ml_3to4   = data$ml_3to4_mm_g 	           
  data$ml_4to5   = data$ml_4to5_mm_g 	                    
  data$ml_above5 = data$ml_above_5mm_g 
  data$total     = data$total_g 
  
  # Replace NAs by 0
  
  data$ol_under2[is.na(data$ol_under2)] = 0
  data$ol_2to3[is.na(data$ol_2to3)]     = 0
  data$ol_3to4[is.na(data$ol_3to4)]     = 0
  data$ol_4to5[is.na(data$ol_4to5)]     = 0
  data$ol_above5[is.na(data$ol_above5)] = 0
  data$ml_under2[is.na(data$ml_under2)] = 0                        
  data$ml_2to3[is.na(data$ml_2to3)]     = 0                       
  data$ml_3to4[is.na(data$ml_3to4)]     = 0           
  data$ml_4to5[is.na(data$ml_4to5)]     = 0                  
  data$ml_above5[is.na(data$ml_above5)] = 0
  data$total[is.na(data$total)]         = 0
  
  # Cumulative time step
  data$time_step_cum <- data$time_step*data$time_step_minutes 
  #if (!is.na(data$time_step) & data$time_step_minutes != 5 & data$time_step_minutes != 15), 
  #then (data$time_step_cum <- data$time_step*10) 
  #if {is.na(data$time_step) & !is.na(time_step_minutes)}
  #then (data$time_step_cum <- data$time_step_minutes)
  
  
  # Replace NAs in days by 1
  data$day[is.na(data$day)] = 1  # TO DO. Replace by the previous collection day?
  
  data$this_core = (paste(as.character(data$year), as.character(data$month), as.character(data$day), as.character(data$ingrowth_core_num), as.character(data$is_stock), sep="-"))
  
  data = transform(data, persist_id = paste(plotname, ingrowth_core_num, sep="_"))
  if (! remove_stock_meas) {
    # need to keep track of which measurements are stocks if we don't remove them initially
    stock_meas = data[data$is_stock %in% "y", c("persist_id", "plot_code", "ingrowth_core_num", "year", "month", "day")]
    stock_meas = stock_meas[!duplicated(stock_meas),]
  }
  
  
  uid <- unique(data$this_core)
  xx <- c()
  aa <- c()
  bb <- c() 
  cc <- c() 
  dd <- c()
  ee <- c()
  ff <- c()
  gg <- c()
  hh <- c()
  ii <- c()
  jj <- c()
  kk <- c()
  
  for (i in 1:length(uid)) {
    sub          <- subset(data, subset=(data$this_core == uid[i]))
    id           <- tail(sub$this_core, n=1) 
    
    # THIS IS WHERE WE SHOULD REPLACE MISSING DAYS: sub$day[is.na(sub$day)] <- sub$day - 1 
    
    tot_olunder2  = calc_roots(sub, "ol_under2", plotname) #ol_under2
    tot_ol2to3    = calc_roots(sub, "ol_2to3", plotname) #ol_2to3
    tot_ol3to4    = calc_roots(sub, "ol_3to4", plotname) #ol_3to4
    tot_ol4to5    = calc_roots(sub, "ol_4to5", plotname) #ol_4to5
    tot_olabove5  = calc_roots(sub, "ol_above5", plotname) #ol_above5
    tot_mlunder2  = calc_roots(sub, "ml_under2", plotname) #ml_under2
    tot_ml2to3    = calc_roots(sub, "ml_2to3", plotname) #ml_2to3
    tot_ml3to4    = calc_roots(sub, "ml_3to4", plotname) #ml_3to4
    tot_ml4to5    = calc_roots(sub, "ml_4to5", plotname) #ml_4to5
    tot_mlabove5  = calc_roots(sub, "ml_above5", plotname) #ml_above5
    tot_total     = calc_roots(sub, "total", plotname)
    
    xx       <- rbind(xx, id) # use this
    # yy       <- rbind(yy, persist_id) # use this
    aa       <- rbind(aa, tot_olunder2) 
    bb       <- rbind(bb, tot_ol2to3)
    cc       <- rbind(cc, tot_ol3to4)
    dd       <- rbind(dd, tot_ol4to5)
    ee       <- rbind(ee, tot_olabove5)
    ff       <- rbind(ff, tot_mlunder2) 
    gg       <- rbind(gg, tot_ml2to3)
    hh       <- rbind(hh, tot_ml3to4)
    ii       <- rbind(ii, tot_ml4to5)
    jj       <- rbind(jj, tot_mlabove5)
    kk       <- rbind(kk, tot_total)
  }
  
  data2a <- data.frame(cbind(as.character(xx), as.numeric(as.character(aa)), as.numeric(as.character(bb)), as.numeric(as.character(cc)), 
                             as.numeric(as.character(dd)), as.numeric(as.character(ee)), as.numeric(as.character(ff)), 
                             as.numeric(as.character(gg)), as.numeric(as.character(hh)), as.numeric(as.character(ii)), 
                             as.numeric(as.character(jj)), as.numeric(as.character(kk))))
  colnames(data2a) <- c("this_core", "tot_olunder2", "tot_ol2to3", "tot_ol3to4", "tot_ol4to5", "tot_olabove5", 
                        "tot_mlunder2", "tot_ml2to3", "tot_ml3to4", "tot_ml4to5", "tot_mlabove5", "tot_total")
  
  # data2 <- data.frame(cbind(xx, as.numeric(as.character(aa)))) 
  # colnames(data2) <- c("this_core", "tot_olunder2", "", "")
  
  ## rootztot is the sum of roots in the soil layers.
  
  data2a$tot_olunder2 <- as.numeric(as.character(data2a$tot_olunder2))
  data2a$tot_ol2to3   <- as.numeric(as.character(data2a$tot_ol2to3))
  data2a$tot_ol3to4   <- as.numeric(as.character(data2a$tot_ol3to4))
  data2a$tot_ol4to5   <- as.numeric(as.character(data2a$tot_ol4to5))
  data2a$tot_olabove5 <- as.numeric(as.character(data2a$tot_olabove5))
  data2a$tot_mlunder2 <- as.numeric(as.character(data2a$tot_mlunder2))
  data2a$tot_ml2to3   <- as.numeric(as.character(data2a$tot_ml2to3))
  data2a$tot_ml3to4   <- as.numeric(as.character(data2a$tot_ml3to4))
  data2a$tot_ml4to5   <- as.numeric(as.character(data2a$tot_ml4to5))
  data2a$tot_mlabove5 <- as.numeric(as.character(data2a$tot_mlabove5))
  data2a$tot_total    <- as.numeric(as.character(data2a$tot_total))
  
  df <- data.frame(data2a$tot_olunder2, data2a$tot_ol2to3, data2a$tot_ol3to4, data2a$tot_ol4to5, data2a$tot_olabove5, data2a$tot_mlunder2, data2a$tot_ml2to3, data2a$tot_ml3to4, data2a$tot_ml4to5, data2a$tot_mlabove5, data2a$tot_total)
  zz <- rowSums(df, na.rm=T)
  data2a$rootztot <- zz
  
  # !! Replace sqldf with dplyr 
  # Create new dataframe with date & total root in g day-1
  data3 <- sqldf("SELECT data2a.this_core, data.persist_id, AVG(data2a.rootztot), data.year, data.month, data.day FROM data2a JOIN data ON data2a.this_core = data.this_core GROUP BY data2a.this_core")
  colnames(data3) <- c("this_core","persist_id", "rootztot","year","month","day")
  
  # Find where does this correction come from??  
  # dzz is the correction for fine root productivity below 30cm. dzz can be specified in fine_root_cor.
  if (fine_root_cor=="Default") {
    tubeh = 0 # why is this 0? ask Chris.
    # depth profile of roots
    depic = (30-tubeh/10)/100
    dzz = 0.5*(exp(-7.344*depic)+exp(-1.303*depic)) 
  } else {
    dzz <- fine_root_cor 
  }
  
  # Add root estimates below 30cm
  # In David Galbraith study, 37% of the fine roots (<2mm) were below 30cm, this is close to 39% found by this equation.
  # Please note: there is a discrepancy between here and the RAINFOR manual (2.3, pp. 47), because the assumption there is 45% in the top 30 cm of the soil.
  
  # sum total carbon from roots (diameter ~ 14cm, depth ~ 30cm)
  data3$ciric    = (3.14*tubed^2) # surface area m2
  data3$volic    = data3$ciric*depic
  data3$rootztot[is.na(data3$rootztot)] = 0 
  data3$totaic   = data3$rootztot / (1-dzz)   # total roots estimated by extrapolating timesteps, plus roots growing below 30cm estimated with the correction factor dzz.
  data3$ic_MgCha = (data3$totaic/data3$ciric)*10000/(2.1097*1000*1000)  # Mg roots per ha (10000m2 = 1ha, 1Mg = 1000000g divide by 2 for carbon)
  data3$d        = as.character(paste(data3$month, data3$day, data3$year, sep="/")) 
  data3$date     = as.Date(data3$d, "%m/%d/%Y") # POSIXct
  
  # convert to MgC / ha / month per plot ####
  data3[data3 == 0] <- NA
  
  #data4 <- sqldf("SELECT data3.year, data3.month, data3.day, AVG(data3.ic_MgCha), STDEV(data3.ic_MgCha) FROM data3 GROUP BY data3.month")
  #colnames(data4) <- c("year", "month", "day", "threemonthlyNPProot", "threemonthlyNPProot_sd")
  #data4$year = sub("^(\\d\\d)$", "20\\1", data4$year) # make 2-digit years into 4-digit years.  Assume 20xx.
  
  data4 = data3 %>% group_by(year, month) %>% 
    dplyr::summarize(day = mean(day, na.rm = T),
                     threemonthlyNPProot = mean(ic_MgCha, na.rm = T), 
                     threemonthlyNPProot_sd = sd(ic_MgCha, na.rm = T), 
                     date = max(date))
  data4       = data.frame(data4)
  data4$d     = as.character(paste(data4$month, data4$day, data4$year, sep="/")) 
  data4$date  = as.Date(data4$d, "%m/%d/%Y")
  data4       = sqldf("SELECT data4.* FROM data4 ORDER BY data4.year, data4.month, data4.day ASC")
  
  # split out into per-tube summaries here #### LUBRIDATE!!
  data4_pertube       = sqldf("SELECT data3.persist_id, data3.year, data3.month, data3.day, SUM(data3.ic_MgCha) FROM data3 GROUP BY data3.year, data3.month, data3.persist_id")
  colnames(data4_pertube) = c("persist_id", "year", "month", "day", "threemonthlyNPProot")
  data4_pertube$year  = sub("^(\\d\\d)$", "20\\1", data4_pertube$year) # make 2-digit years into 4-digit years.  Assume 20xx.
  data4_pertube$d     = as.character(paste(data4_pertube$month, data4_pertube$day, data4_pertube$year, sep="/")) 
  data4_pertube$date  = as.Date(data4_pertube$d, "%m/%d/%Y")
  data4_pertube       = sqldf("SELECT data4_pertube.* FROM data4_pertube ORDER BY data4_pertube.persist_id, data4_pertube.year, data4_pertube.month, data4_pertube.day ASC")
  
  # 3 monthly data divided by collection interval. Get collection interval: 
  c_time = as.POSIXlt(data4$date)
  c_time = rev(c_time)
  tt = difftime(c_time[1:(length(c_time)-1)] , c_time[2:length(c_time)]) # this gets the collection interval 
  data4$interval = c(90, tt)  # I add 90 days as first collection interval. You can change this.
  data4$monthlyNPProot    = (as.numeric(data4$threemonthlyNPProot)/data4$interval) * 30 # TO DO: We should change this to the number of days in that month. need a loop.
  data4$monthlyNPProot_se = ((as.numeric(data4$threemonthlyNPProot_sd)/sqrt(16))/data4$interval) * 30 
  data4$monthlyNPProot_sd = (data4$threemonthlyNPProot_sd/data4$interval) * 30 
  # (mean(data4$monthlyNPProot, na.rm=T))*12
  # (mean(data4$monthlyNPProot_se, na.rm=T))*12
  
  # 3 monthly data divided by collection interval per tube
  data4_pertube = data4_pertube %>% 
    group_by(persist_id) %>% arrange(persist_id, date) %>% 
    mutate(interval = ifelse(is.na(lag(date)), 90, as.numeric(difftime(date, lag(date)))),  # I add 90 days as first collection interval. You can change this.
           monthlyNPProot = threemonthlyNPProot/interval * 30) # TODO: change to reflect days per month
  
  data4$plot_code = plotname
  data4_pertube$plot_code = plotname
  data3$plot_code = plotname
  
  data4_pertube$tubenum = sub("^.*_(.*)$", "\\1", data4_pertube$persist_id)
  data3$tubenum = sub("^.*_(.*)$", "\\1", data3$persist_id)
  
  # record previous meas in each row for data3 so we know the period for which the measurement is referring
  data3 = data3 %>% group_by(persist_id) %>%
    mutate(date = as.Date(sprintf("%d-%2d-%2d", year, month, day))) %>%
    arrange(date) %>%
    mutate(prev_meas = lag(date))
  if (! remove_stock_meas) {
    # remove the first measurement from all tubes if the stock measurement wasn't removed initially
    stock_meas_times = unique(with(stock_meas, paste(year, month, day)))
    data3 = subset(data3, ! paste(year, month, day) %in% stock_meas_times)
    data4 = subset(data4, ! paste(year, month, day) %in% stock_meas_times)
    data4_pertube = subset(data4_pertube, ! paste(year, month, day) %in% stock_meas_times)
    # match measurements
    # remove measurements
  }
  
  return(list("three_monthly" = data4, "three_monthly_pertube" = data4_pertube, "all_times_and_tubes" = data3))
}



