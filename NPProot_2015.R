
## This function calculates fine root productivity
# Based on Matlab code by C. Doughty (2013)
# Updated and adapted to GEM plots by C. Girardin (2015)

## option = 1: this means the time steps 10,20,30,40 minutes are chosen
## option = 2: this means the time steps 5, 10, 15 minutes are chosen
# ATTENTION!! Make sure that 1st timestep is always followed by timesteps 2,3,4 in that order.
# Note: options have been obviated in leiu of pasing in timesteps

# STOCKS: delete stocks data, we are only dealing with NPP here.

# colnames in database:
# plot_code                  
# year                        
# month                         
# day                         
# ingrowth_core_num           
# is_stock_yn                 
# ingrowth_core_litterfall_g  
# soil_humidity_pcnt          
# soil_temperature_c          		
# ol_layer_depth_cm	    
# ml_layer_depth_cm	    
# time_step                   
# time_step_minutes           
# ol_under_2mm_g                         
# ml_under_2mm_g                           
# ol_2to3_mm_g	            
# ml_2to3_mm_g	            
# ol_3to4_mm_g	            
# ml_3to4_mm_g	           
# ol_4to5_mm_g	
# ml_4to5_mm_g	           
# ol_above_5mm_g	            
# ml_above_5mm_g	           
# quality_code                
# comments   

# get script location in order to find functions.r
# from http://stackoverflow.com/questions/1815606/rscript-determine-path-of-the-executing-script
  # script.dir <- try(dirname(sys.frame(1)$ofile), silent = T)
  # if (class(script.dir) == "try-error") {
  #   script.dir = "."
  # }
  
  # initial.options <- commandArgs(trailingOnly = FALSE)
  # file.arg.name <- "--file="
  # script.name <- sub(file.arg.name, "", initial.options[grep(file.arg.name, initial.options)])
  # script.basename <- dirname(script.name)
  
  script.dir <- function() {
    getSrcDirectory(script.dir);
  }
  
source(paste(script.dir(), "functions.r", sep = "/"))

ic_column_types = c(
  "plot_code" = "character",
  "year" = "integer",
  "month" = "integer",
  "day" = "integer",
  "ingrowth_core_num" = "integer",
  "is_stock" = "logical",
  "ingrowth_core_litterfall_g" = "numeric",
  "soil_humidity_pcnt" = "numeric",
  "soil_temperature_c" = "numeric",
  "ol_layer_depth_cm" = "numeric",
  "ml_layer_depth_cm" = "numeric",
  "time_step" = "integer",
  "time_step_minutes" = "integer",
  "ol_under_2mm_g" = "numeric",
  "ol_above_2mm_g" = "numeric",
  "ml_under_2mm_g" = "numeric",
  "ml_above_2mm_g" = "numeric",
  "ol_2to3_mm_g" = "numeric",
  "ml_2to3_mm_g" = "numeric",
  "ol_3to4_mm_g" = "numeric",
  "ml_3to4_mm_g" = "numeric",
  "ol_4to5_mm_g" = "numeric",
  "ml_4to5_mm_g" = "numeric",
  "ol_above_5mm_g" = "numeric",
  "ml_above_5mm_g" = "numeric",
  "quality_code" = "factor",
  "comments" = "character"
)

plotit = F

# Set exponent of curve for when models will not fit the data
mean_exponents = c("Default" = 0.22,
                   "ACJ-01" = 0.22)

solve_for_a <- function(cum, tx, b) {
  # Fit the curve through the final
  #   point (x,y = tx, cum) with a site-mean exponent (b, perhaps per size-class), solving for linear slope (a).  
  #   For example, cum1 ~ a * tx^b.  Thus, a = cum1/tx^b.
  a = cum[length(cum)]/tx[length(tx)]^b
}

extrapolate_failed_model <- function(cum, tx, b, mins = 100) {
  a = solve_for_a(cum, tx, b)
  cum_tot = a * mins^b
  return(cum_tot)
}

calc_roots <- function(core_data, root_type, plotname, tx = c(10,20,30,40), mins = 100, logmodel = T) {
  # subset core data before passing in.  E.g. sub <- subset(data, subset=(data$this_core == uid[i]))
  this_exponent = ifelse(plotname %in% names(mean_exponents), mean_exponents[plotname], mean_exponents["Default"])
  coef_func = ifelse(logmodel, coef, coefficients) # nls & lm have different methods for extracting coefs.  use this when testing exponent > 1
  if  (!any(is.na(core_data[,root_type])) & sum(core_data[,root_type]) > 0) {
    cumdata      <- tail(core_data[,root_type], n=length(tx)) # cumulative values for that diameter class
    #tx           <- c(10,20,30,40)           # cumulative time steps in minutes WE SHOULD USE data.ic$time_step_cum
    cum          <- cumsum(cumdata)
    if(cum[1] == 0) {                       # make sure we find some roots in first sample.  Otherwise, log models blow up.
      tot_roots = extrapolate_failed_model(cum, tx, this_exponent, mins = mins)
    } else {
      if(logmodel) {  
        P_log <- lm(log(cum) ~ log(tx))
        tot_roots <- exp(coefficients(P_log)[1]) * (100)^(coefficients(P_log)[2])  # We use the same method as Khoon & Terhi (120 mins and log-curve as default).
      } else {
        P <- nls(cum ~ a * tx^b, start=list(a=1, b=1), control = nls.control(maxiter=1000, warnOnly=T))
        tot_roots <- coef(P)[1] * (100)^(coef(P)[2])  # Chris used 100 mins and power law, but here we use 120 min (Khoon & Terhi).
      }
      if(coef_func(P_log)[2] > 1) {         # make sure accumulation of roots is declining with time
        tot_roots = extrapolate_failed_model(cum, tx, this_exponent, mins = 100)
      }
    }
  } else {
    tot_roots <- NA 
  }
  return(tot_roots)
}


NPProot_ic <- function(datafile, ..., ret_type = c("concat", "list")) {
                       
                       #logmodel = T, fine_root_cor = "Default", tubed = 0.07, ret = "monthly.means.ts", ret_type = c("list", "concat")) {
  ret_type = match.arg(ret_type)
  if (class(datafile) != "data.frame") { # if it's not a dataframe, assume it's a path+filename
      data.ic <- read.csv(datafile, na.strings = c("NA", "NaN"))
  } else {
      data.ic = datafile # data.frame passed in directly
  }

  # set column datatypes as defined above
    data.ic = set_df_coltypes(data.ic, ic_column_types)
  
  output = list()
  first_run = T
  pb = txtProgressBar(max = length(unique(data.ic$plot_code)), style = 3); i = 0
  for (thisplot in unique(data.ic$plot_code)) {
      
    this_output = NPProot_ic_oneplot(datafile, thisplot, ...)
    if (class(this_output) == "logical" & is.na(this_output)) {
        next() # likely no rows after removing stock measurements
    }
    output[[thisplot]] = this_output
    
    if (first_run) {
      first_run = F
      three_monthly = output[[thisplot]][["three_monthly"]]
      three_monthly_pertube = output[[thisplot]][["three_monthly_pertube"]]
      all_times_and_tubes = output[[thisplot]][["all_times_and_tubes"]]
    } else {
      three_monthly = rbind(three_monthly, output[[thisplot]][["three_monthly"]])
      three_monthly_pertube = rbind(three_monthly_pertube, output[[thisplot]][["three_monthly_pertube"]])
      all_times_and_tubes = rbind(all_times_and_tubes, output[[thisplot]][["all_times_and_tubes"]])
    }
    i = i + 1
    setTxtProgressBar(pb, i)
  }
  close(pb)

  if (ret_type == "list") { # return plot results in different list elements
    return(output)
  } else { # return results concatenated across plots
    return(list("three_monthly" = three_monthly, "three_monthly_pertube" = three_monthly_pertube, "all_times_and_tubes" = all_times_and_tubes))
  }
}
  
NPProot_ic_oneplot <- function(datafile, plotname, logmodel = T, fine_root_cor = "Default", tubed = 0.07, remove_stock_meas = F, ret = "monthly.means.ts") {
  # If stock measurements aren't removed, they're used as the beginning of the first interval (otherwise assumed to be 90 days)

  library(sqldf)
  library(ggplot2)
  library(scales)
  library(nlme)
  library(dplyr)
    
  coef_func = ifelse(logmodel, coef, coefficients) # nls & lm have different methods for extracting coefs.  use this when testing exponent > 1

  if (class(datafile) != "data.frame") { # if it's not a dataframe, assume it's a path+filename
      data.ic <- read.csv(datafile, na.strings = c("NA", "NaN"))
  } else {
      data.ic = datafile # data.frame passed in directly
  }
  
  data <- subset(data.ic, plot_code == plotname)
  
  if (remove_stock_meas) {
      stock_meas = rep(F, nrow(data))
      stock_meas[data$is_stock %in% "y"] = T
      data <- data[!stock_meas,]
  } 
  
  if(nrow(data) == 0) { return(NA) }
  
  # re-name columns rather than building new ones?
  data$ol_under2 <- data$ol_under_2mm_g   
  data$ol_2to3   <- data$ol_2to3_mm_g 
  data$ol_3to4   <- data$ol_3to4_mm_g 
  data$ol_4to5   <- data$ol_4to5_mm_g   
  data$ol_above5 <- data$ol_above_5mm_g 
  data$ml_under2 <- data$ml_under_2mm_g                                
  data$ml_2to3   <- data$ml_2to3_mm_g 	                        
  data$ml_3to4   <- data$ml_3to4_mm_g 	           
  data$ml_4to5   <- data$ml_4to5_mm_g 	                    
  data$ml_above5 <- data$ml_above_5mm_g 
  data$time_step_cum <- data$time_step*data$time_step_minutes # get cumulative time step
  
  ## TO DO: We should add sanity checks for the input data here.
  
  # Replace NAs by 0
  
  data$ol_under2[is.na(data$ol_under2)] <- 0
  data$ol_2to3[is.na(data$ol_2to3)]     <- 0
  data$ol_3to4[is.na(data$ol_3to4)]     <- 0
  data$ol_4to5[is.na(data$ol_4to5)]     <- 0
  data$ol_above5[is.na(data$ol_above5)] <- 0
  data$ml_under2[is.na(data$ml_under2)] <- 0                        
  data$ml_2to3[is.na(data$ml_2to3)]     <- 0                       
  data$ml_3to4[is.na(data$ml_3to4)]     <- 0           
  data$ml_4to5[is.na(data$ml_4to5)]     <- 0                  
  data$ml_above5[is.na(data$ml_above5)] <- 0
  
  data$this_core <- (paste(as.character(data$year),as.character(data$month),as.character(data$day),as.character(data$ingrowth_core_num), sep="-"))

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
  
  for (i in 1:length(uid)) {
    sub          <- subset(data, subset=(data$this_core == uid[i]))
    id           <- tail(sub$this_core, n=1) 
    
    #ol_under2
    tot_olunder2 = calc_roots(sub, "ol_under2", plotname)
    #ol_2to3
    tot_ol2to3 = calc_roots(sub, "ol_2to3", plotname)
    #ol_3to4
    tot_ol3to4 = calc_roots(sub, "ol_3to4", plotname)
    #ol_4to5
    tot_ol4to5 = calc_roots(sub, "ol_4to5", plotname)
    #ol_above5
    tot_olabove5 = calc_roots(sub, "ol_above5", plotname)
    #ml_under2 
    tot_mlunder2 = calc_roots(sub, "ml_under2", plotname)
    #ml_2to3
    tot_ml2to3 = calc_roots(sub, "ml_2to3", plotname)
    #ml_3to4
    tot_ml3to4 = calc_roots(sub, "ml_3to4", plotname)
    #ml_4to5
    tot_ml4to5 = calc_roots(sub, "ml_4to5", plotname)
    #ml_above5
    tot_mlabove5 = calc_roots(sub, "ml_above5", plotname)
   
    xx       <- rbind(xx, id) # use this
    # yy       <- rbind(yy, persist_id) # use this
    aa       <- rbind(aa, tot_olunder2) # use this
    bb       <- rbind(bb, tot_ol2to3)
    cc       <- rbind(cc, tot_ol3to4)
    dd       <- rbind(dd, tot_ol4to5)
    ee       <- rbind(ee, tot_olabove5)
    ff       <- rbind(ff, tot_mlunder2) # use this
    gg       <- rbind(gg, tot_ml2to3)
    hh       <- rbind(hh, tot_ml3to4)
    ii       <- rbind(ii, tot_ml4to5)
    jj       <- rbind(jj, tot_mlabove5)
  }
  
  data2a <- data.frame(cbind(xx, as.numeric(as.character(aa)), as.numeric(as.character(bb)), as.numeric(as.character(cc)), as.numeric(as.character(dd)), as.numeric(as.character(ee)), as.numeric(as.character(ff)), as.numeric(as.character(gg)), as.numeric(as.character(hh)), as.numeric(as.character(ii)), as.numeric(as.character(jj))))
  colnames(data2a) <- c("this_core", "tot_olunder2", "tot_ol2to3", "tot_ol3to4", "tot_ol4to5", "tot_olabove5", "tot_mlunder2", "tot_ml2to3", "tot_ml3to4", "tot_ml4to5", "tot_mlabove5")
    
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
  
  df <- data.frame(data2a$tot_olunder2, data2a$tot_ol2to3, data2a$tot_ol3to4, data2a$tot_ol4to5, data2a$tot_olabove5, data2a$tot_mlunder2, data2a$tot_ml2to3, data2a$tot_ml3to4, data2a$tot_ml4to5, data2a$tot_mlabove5)
  zz <- rowSums(df, na.rm=T)
  data2a$rootztot <- zz
  
  # Create new dataframe with date & total root in g day-1
  data3 <- sqldf("SELECT data2a.this_core, data.persist_id, AVG(data2a.rootztot), data.year, data.month, data.day FROM data2a JOIN data ON data2a.this_core = data.this_core GROUP BY data2a.this_core")
  colnames(data3) <- c("this_core","persist_id", "rootztot","year","month","day")
  
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
    # In David's study, 37% of the fine roots (<2mm) were below 30cm, this is close to 39% found by this equation.
    # Please note: there is a discrepancy between here and the RAINFOR manual (2.3, pp. 47), because the assumption there is 45% in the top 30 cm of the soil.
    
    # sum total carbon from roots (diameter ~ 14cm, depth ~ 30cm)
      data3$ciric = (3.14*tubed^2) # surface area m2
      data3$volic = data3$ciric*depic
      data3$rootztot[is.na(data3$rootztot)] = 0 
      data3$totaic = data3$rootztot / (1-dzz)   # total roots estimated by extrapolating timesteps, plus roots growing below 30cm estimated with the correction factor dzz.
      data3$ic_MgCha = (data3$totaic/data3$ciric)*10000/(2.1097*1000*1000)  # Mg roots per ha (10000m2 = 1ha, 1Mg = 1000000g divide by 2 for carbon)
                                                                   
  # convert to MgC / ha / month per plot ####
    data3[data3 == 0] <- NA
    data4 <- sqldf("SELECT data3.year, data3.month, data3.day, AVG(data3.ic_MgCha), STDEV(data3.ic_MgCha) FROM data3 GROUP BY data3.month")
    colnames(data4) <- c("year", "month", "day", "threemonthlyNPProot", "threemonthlyNPProot_sd")
    data4$year = sub("^(\\d\\d)$", "20\\1", data4$year) # make 2-digit years into 4-digit years.  Assume 20xx.
    data4$d     <- as.character(paste(data4$month, data4$day, data4$year, sep="/")) 
    data4$date  <- as.Date(data4$d, "%m/%d/%Y")
    data4 <- sqldf("SELECT data4.* FROM data4 ORDER BY data4.year, data4.month, data4.day ASC")

  # split out into per-tube summaries here ####
    data4_pertube <- sqldf("SELECT data3.persist_id, data3.year, data3.month, data3.day, SUM(data3.ic_MgCha) FROM data3 GROUP BY data3.year, data3.month, data3.persist_id")
    colnames(data4_pertube) <- c("persist_id", "year", "month", "day", "threemonthlyNPProot")
    data4_pertube$year = sub("^(\\d\\d)$", "20\\1", data4_pertube$year) # make 2-digit years into 4-digit years.  Assume 20xx.
    data4_pertube$d     <- as.character(paste(data4_pertube$month, data4_pertube$day, data4_pertube$year, sep="/")) 
    data4_pertube$date  <- as.Date(data4_pertube$d, "%m/%d/%Y")
    data4_pertube <- sqldf("SELECT data4_pertube.* FROM data4_pertube ORDER BY data4_pertube.persist_id, data4_pertube.year, data4_pertube.month, data4_pertube.day ASC")
  
  # 3 monthly data divided by collection interval. Get collection interval: 
    c_time <- as.POSIXlt(data4$date)
    c_time <- rev(c_time)
    tt <- difftime(c_time[1:(length(c_time)-1)] , c_time[2:length(c_time)]) # this gets the collection interval 
    data4$interval <- c(90, tt)  # I add 90 days as first collection interval. You can change this.
    data4$monthlyNPProot    <- (as.numeric(data4$threemonthlyNPProot)/data4$interval) * 30 # TO DO: We should change this to the number of days in that month. need a loop.
    data4$monthlyNPProot_se <- ((as.numeric(data4$threemonthlyNPProot_sd)/sqrt(16))/data4$interval) * 30 
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

## Plotroutine, triggered by argument 'plotit=T'
if (plotit==T) {
  ## ggplot2 of root npp vs time
  
  top <- data4$monthlyNPProot + data4$monthlyNPProot_se
  plot1 <- ggplot(data=data4, aes(x=date, y=monthlyNPProot, na.rm=T)) +
           geom_line(linetype='solid', colour='black', size=1) +
           geom_ribbon(data=data4, aes(ymin=monthlyNPProot-monthlyNPProot_se, ymax=monthlyNPProot+monthlyNPProot_se), alpha=0.2) +
           scale_x_date(breaks = date_breaks("months"), labels = date_format("%b-%Y")) +            
           scale_colour_grey() + 
           theme(text = element_text(size=17), legend.title = element_text(colour = 'black', size = 17, hjust = 3, vjust = 7, face="plain")) +
           ylim(0, max(top, na.rm=T)) +                          
           xlab("") + ylab(expression(paste("NPP fine root (MgC ", ha^-1, mo^-1, ")", sep=""))) +
           theme_classic(base_size = 15, base_family = "") + 
           theme(legend.position="left") +
           theme(panel.border = element_rect(fill = NA, colour = "black", size = 1)) 
  
}

# # Return either monthly means (ret="monthly.means")   
# switch(ret,
#        monthly.means.matrix = {return(npproot.data.monthly.matrix)},
#        monthly.means.ts = {return(npproot.data.monthly.ts)}
# )
# 
# x <- mean(npproot.data.monthly.ts$toticalA, na.rm=T)
# NPProot_MgCHaYr <- x*12
# 
# y <- mean(npproot.data.monthly.ts$toticalAstd, na.rm=T)
# NPProot_MgCHaYr_sd <- y*12

# Testing ####
    # ### Read test data:
    # #setwd("C:/Users/Cecile/Dropbox/NPPflux/db_csv")
    # #data.ic <- read.table("C:/Users/Cecile/Dropbox/Carbon_Use_Efficieny_R/testing/ICEltr_samtest.csv", sep=",", header=T)
    # 
    # datadir = "a_readyforupload_db/acj_pan_2015/"
    # testfile = "ingrowth_core_ACJ-01_2013_2104_nostock.csv"
    # 
    # 
    # 
    # ## adjust options:
    # # plotname = "ACJ-01"
    # # option = 1
    # # logmodel = T
    # # fine_root_cor <- "Default" 
    # # tubed = 0.07  ## diameter of tube
    # # plotit=T
    # # ret="monthly.means"