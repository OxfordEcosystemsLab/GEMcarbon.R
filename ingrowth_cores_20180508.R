## This function calculates fine root productivity
# Based on Matlab code by C. Doughty (2013)
# Re-written and adapted to GEM plots by C. Girardin (2015)
# Edited by A. Shenkin (2017)

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

# ASK ALLIE - this doesn't work
#script.dir <- function() {
#  getSrcDirectory(script.dir);
#}
#source(paste(script.dir(), "functions.r", sep = "/"))



# Functions used in this code:

ic_column_types = c(
  "plot_code" = "character",
  "year" = "integer",
  "month" = "integer",
  "day" = "integer",
  "ingrowth_core_num" = "integer",
  "is_stock" = "character", # this use to be "logical", but it created problems. 
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
  "total_g" = "numeric", # this column needs to be added to db
  "quality_code" = "factor",
  "comments" = "character"
)


# Set exponent of curve for when models will not fit the data
mean_exponents = c("Default" = 0.22) # Allie: how do we calculate mean exponent for the plot?

solve_for_a <- function(cum, tx, b) {
  # Fit the curve through the final point (x,y = tx, cum) with a site-mean exponent (b, perhaps per size-class), solving for linear slope (a).  
  # For example, cum1 ~ a * tx^b.  Thus, a = cum1/tx^b.
  a = cum[length(cum)]/tx[length(tx)]^b
}

extrapolate_failed_model <- function(cum, tx, b, mins = 120) {
  a = solve_for_a(cum, tx, b)
  cum_tot = a * mins^b
  return(cum_tot)
}

# Comment from Terhi. How to estimate max mass when the model does not fit.
# The way I deal with the in-growth core data, when the saturating model does not converge*:
#I take the maximum of the cumulative root mass for the core (or the sum of all the four time points, which is of course the same thing). 
#Then I apply a correction factor to that value. I calculate the max mass for all data, and for those data points where the model converges I calculate the modelled:max ratio. 
#The modelled value is of course higher than the max, because it has been solved to 120 mins (or whatever the chosen time now is).  
#I use the average modelled:max ratio as a correction factor for those data that I could not fit the model. x_cummulative * (plotagv(modelled:max)).
#I have used the same correction for all my plots, but in the pan-tropical context it might be better to make the correction site or plot specific.

#*Sometimes the model does converge, but the model is rubbish. This is quite tricky, because it is harder to spot. 
#I have used an arbitrary threshold of 2. If the modelled:max is >2, I assume the model is rubbish. 
#I don’t think that there are >50% root mass remaining in the soil after we have finished sorting. I just assume that the model fit is bad in that case. 
#But that 2 is just really an arbitrary number that I have chosen.



calc_roots <- function(core_data, root_type, mins = 120, logmodel = T) {          # I removed plotname from the parameters
  
  # subset core data before passing in.  E.g. sub <- subset(data, subset=(data$this_core == uid[i]))
  this_exponent = 0.22                                                             # Use this if we need to have default per plot: this_exponent = ifelse(plotname %in% names(mean_exponents), mean_exponents[plotname], mean_exponents["Default"]) 
                                                                                   
  coef_func = ifelse(logmodel, coef, coefficients)                                 # nls & lm have different methods for extracting coefs.  use this when testing exponent > 1
  
  tx = sub$time_step_cum                                                          
  
  if  (!any(is.na(core_data[,root_type])) & sum(core_data[,root_type]) > 0) {
    cumdata      <- tail(core_data[,root_type], n=length(tx))                      # cumulative values for that diameter class
    cum          <- cumsum(cumdata)
    if(cum[1] == 0) {                                                              # make sure we find some roots in first sample.  Otherwise, log models blow up.
      tot_roots = extrapolate_failed_model(cum, tx, this_exponent, mins = mins)
    } else {
      if(logmodel) {
        P_log <- lm(log(cum) ~ log(tx))
        tot_roots <- exp(coefficients(P_log)[1]) * (120)^(coefficients(P_log)[2])  # We use the same method as Khoon & Terhi (120 mins and log-curve as default). This is in Dan's paper.
      } else {
        P <- nls(cum ~ a * tx^b, start=list(a=1, b=1), control = nls.control(maxiter=1000, warnOnly=T))
        tot_roots <- coef(P)[1] * (100)^(coef(P)[2])                               # Chris used 100 mins and power law.
      }
      if(coef_func(P_log)[2] > 1) {                                                # make sure accumulation of roots is declining with time
        tot_roots = extrapolate_failed_model(cum, tx, this_exponent, mins = 120)
      }
    }
  } else {
    tot_roots <- NA 
  }
  return(tot_roots)
}


# NPProot_ic function parameters:
logmodel = T
fine_root_cor = "Default" 
tubed = 0.07 # This is the radius of the collar in meters, so diameter is 14 cm. 
remove_stock_meas = T 
ret = "monthly.means.ts"
ret_type = "list" 

# Do not run this as a function. Just run this code step by step.
# NPProot_ic <- function(datafile, logmodel = T, fine_root_cor = "Default", tubed = 0.07, remove_stock_meas = F, ret = "monthly.means.ts", tx) {  
  
  # If stock measurements aren't removed, they're used as the beginning of the first interval (otherwise assumed to be 90 days)
  
  library(scales)
  library(nlme)
  library(tidyverse)
  
  coef_func = ifelse(logmodel, coef, coefficients) # nls & lm have different methods for extracting coefs.  use this when testing exponent > 1
  
  if (class(datafile) != "data.frame") { # if it's not a dataframe, assume it's a path + filename
    icdata <- read.csv(datafile, na.strings = c("NA", "NaN"))
  } else {
    icdata = datafile # data.frame passed in directly
  }
  
  # set column datatypes as defined above
  icdata = set_df_coltypes(icdata, ic_column_types)
  
  #icdata <- subset(icdata, plot_code == plotname)
  
  # clean NAs
  icdata[icdata == 'NA'] <- NA
  
  # Remove stock measurement
  
  if (remove_stock_meas) {
    stock_meas = rep(F, nrow(icdata))
    stock_meas[icdata$is_stock %in% "Y"] = T  
    icdata <- icdata[!stock_meas,]
  } 
  
  if(nrow(icdata) == 0) { return(NA) }
  
  
  # re-name columns
  icdata = icdata %>% rename(ol_under2 = ol_under_2mm_g,  
                    ol_above2 = ol_above_2mm_g,  
                    ol_2to3   = ol_2to3_mm_g, 
                    ol_3to4   = ol_3to4_mm_g, 
                    ol_4to5   = ol_4to5_mm_g,   
                    ol_above5 = ol_above_5mm_g, 
                    ml_under2 = ml_under_2mm_g, 
                    ml_above2 = ml_above_2mm_g, 
                    ml_2to3   = ml_2to3_mm_g,                           
                    ml_3to4   = ml_3to4_mm_g, 	           
                    ml_4to5   = ml_4to5_mm_g, 	                    
                    ml_above5 = ml_above_5mm_g, 
                    total     = total_g)
  
  # Time_40min test: compare measurements from the roots at 40 min to the estimates from the calc_roots model fit.
    icdata = icdata %>% select(ol_under2:ml_above5) %>%
    mutate(test_40min = rowSums(., na.rm = TRUE)) %>% bind_cols(icdata, .)
  
  
  # Replace NAs by 0 # should we be careful not to replace measurements that have all NA by 0 (some plots have dates & no data)
  
  icdata$ol_under2[is.na(icdata$ol_under2)] = 0
  icdata$ol_above2[is.na(icdata$ol_above2)] = 0
  icdata$ol_2to3[is.na(icdata$ol_2to3)]     = 0
  icdata$ol_3to4[is.na(icdata$ol_3to4)]     = 0
  icdata$ol_4to5[is.na(icdata$ol_4to5)]     = 0
  icdata$ol_above5[is.na(icdata$ol_above5)] = 0
  icdata$ml_under2[is.na(icdata$ml_under2)] = 0  
  icdata$ml_above2[is.na(icdata$ml_above2)] = 0
  icdata$ml_2to3[is.na(icdata$ml_2to3)]     = 0                       
  icdata$ml_3to4[is.na(icdata$ml_3to4)]     = 0           
  icdata$ml_4to5[is.na(icdata$ml_4to5)]     = 0                  
  icdata$ml_above5[is.na(icdata$ml_above5)] = 0
  icdata$total[is.na(icdata$total)]         = 0
  
  # Cumulative time step
  icdata$time_step_cum <- icdata$time_step*icdata$time_step_minutes 
  
  # Replace NAs in days by 1
  icdata$day[is.na(icdata$day)] = 1   # TO DO: replace missing days by the previous collection day sub$day[is.na(sub$day)] <- sub$day - 1 
  
  icdata$this_core = (paste(as.character(icdata$plot_code), as.character(icdata$year), as.character(icdata$month), as.character(icdata$day), as.character(icdata$ingrowth_core_num), sep="_"))
  
  if (!remove_stock_meas) {
    # need to keep track of which measurements are stocks if we don't remove them initially
    stock_meas = icdata[icdata$is_stock %in% "y", c("this_core", "plot_code", "ingrowth_core_num", "year", "month", "day")]
    stock_meas = stock_meas[!duplicated(stock_meas),]
  }
  
  
  uid <- unique(icdata$this_core)
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
  ll <- c()
  mm <- c()
  zz <- c()
  
  pb = txtProgressBar(max = length(uid), style = 3)
  
  for (i in 1:length(uid)) {
    sub = subset(icdata, subset=(icdata$this_core == uid[i]))
    sub %>% arrange(time_step)
    id  = tail(sub$this_core, n=1) 
    
    if (nrow(sub) < 2) {
      warning(paste("You don't have enough timesteps in ", uid, "\n"))
      next()
    }
    
   
    tot_olunder2  = calc_roots(sub, "ol_under2") 
    tot_olabove2  = calc_roots(sub, "ol_above2")
    tot_ol2to3    = calc_roots(sub, "ol_2to3") 
    tot_ol3to4    = calc_roots(sub, "ol_3to4") 
    tot_ol4to5    = calc_roots(sub, "ol_4to5") 
    tot_olabove5  = calc_roots(sub, "ol_above5") 
    tot_mlunder2  = calc_roots(sub, "ml_under2")
    tot_mlabove2  = calc_roots(sub, "ml_above2")
    tot_ml2to3    = calc_roots(sub, "ml_2to3") 
    tot_ml3to4    = calc_roots(sub, "ml_3to4") 
    tot_ml4to5    = calc_roots(sub, "ml_4to5")   
    tot_mlabove5  = calc_roots(sub, "ml_above5") 
    tot_total     = calc_roots(sub, "total")
    test40        = sum(sub$test_40min, na.rm=T) # compare the last value of the measurement (cumulative value after 40 min) to the estimated measurement.
    
    xx = rbind(xx, id)          
    aa = rbind(aa, tot_olunder2) 
    ll = rbind(ll, tot_olabove2)
    bb = rbind(bb, tot_ol2to3)
    cc = rbind(cc, tot_ol3to4)
    dd = rbind(dd, tot_ol4to5)
    ee = rbind(ee, tot_olabove5)
    ff = rbind(ff, tot_mlunder2) 
    mm = rbind(mm, tot_mlabove2) 
    gg = rbind(gg, tot_ml2to3)
    hh = rbind(hh, tot_ml3to4)
    ii = rbind(ii, tot_ml4to5)
    jj = rbind(jj, tot_mlabove5)
    kk = rbind(kk, tot_total)
    zz = rbind(zz, test40)
    setTxtProgressBar(pb, i)
  }
  close(pb)
  
  data2a <- data.frame(cbind(xx, aa, ll, bb, cc, dd, ee, ff, mm, gg, hh, ii, jj, kk, zz))

  
  colnames(data2a) = c("this_core", "tot_olunder2", "tot_olabove2", "tot_ol2to3", "tot_ol3to4", "tot_ol4to5", "tot_olabove5", 
                        "tot_mlunder2", "tot_mlabove2", "tot_ml2to3", "tot_ml3to4", "tot_ml4to5", "tot_mlabove5", "tot_total", "test_40min")
  str(data2a)
  
  # test: are all the plot codes in the new dataframe? 
  temp  = strsplit(as.character(xx), "[_]")                    
  xxx   = unlist(lapply(temp, `[[`, 1))
  unique(xxx)
  temp  = strsplit(as.character(data2a$this_core), "[_]")                   
  data2a$plot_code   = unlist(lapply(temp, `[[`, 1))
  unique(data2a$plot_code)
  
  ## rootztot is the sum of roots in the soil layers.
  data2 = data2a %>% mutate(this_core = as.character(this_core),
                            tot_olunder2 = as.numeric(as.character(tot_olunder2)),
                            tot_olabove2 = as.numeric(as.character(tot_olabove2)),
                            tot_ol2to3   = as.numeric(as.character(tot_ol2to3)),
                            tot_ol3to4   = as.numeric(as.character(tot_ol3to4)),
                            tot_ol4to5   = as.numeric(as.character(tot_ol4to5)),
                            tot_olabove5 = as.numeric(as.character(tot_olabove5)),
                            tot_mlunder2 = as.numeric(as.character(tot_mlunder2)),
                            tot_mlabove2 = as.numeric(as.character(tot_mlabove2)),
                            tot_ml2to3   = as.numeric(as.character(tot_ml2to3)),
                            tot_ml3to4   = as.numeric(as.character(tot_ml3to4)),
                            tot_ml4to5   = as.numeric(as.character(tot_ml4to5)),
                            tot_mlabove5 = as.numeric(as.character(tot_mlabove5)),
                            tot_total    = as.numeric(as.character(tot_total)),
                            test_40min   = as.numeric(as.character(test_40min))
                            )
                     
  data2 = data2 %>% select(tot_olunder2:tot_mlabove5) %>%
                    mutate(rootztot = rowSums(., na.rm = T)) %>% 
                    bind_cols(data2, .) 
  
  data2 = data2 %>% mutate(compare_test_40min = (rootztot - test_40min)) 
                    
  #df <- data.frame(data2$tot_olunder2, data2$tot_olabove2, data2$tot_ol2to3, data2$tot_ol3to4, data2$tot_ol4to5, data2$tot_olabove5, data2$tot_mlunder2, data2$tot_mlabove2, data2$tot_ml2to3, data2$tot_ml3to4, data2$tot_ml4to5, data2$tot_mlabove5, data2$tot_total)
  #zz <- rowSums(df, na.rm=T)
  #data2$rootztot <- zz
  
  # Separate info in this_core
  temp                    = strsplit(data2$this_core, "[_]")                     # split this_core into the information we need (plot_code, year, month, day, ingrowth_core_num).
  data2$plot_code         = unlist(lapply(temp, `[[`, 1))
  data2$year              = unlist(lapply(temp, `[[`, 2))
  data2$month             = unlist(lapply(temp, `[[`, 3)) 
  data2$day               = unlist(lapply(temp, `[[`, 4))
  data2$ingrowth_core_num = unlist(lapply(temp, `[[`, 5))
  data2$collectiondate    = as.Date(paste(data2$year, data2$month, data2$day, sep="-"), format="%Y-%m-%d") 

data2 = data2 %>% mutate(year = as.numeric(as.character(year)),
                         month = as.numeric(as.character(month)),
                         day   = as.numeric(as.character(day)),
                         ingrowth_core_num = as.numeric(as.character(ingrowth_core_num)))

  # dzz is the correction for fine root productivity below 30cm. dzz can be specified in fine_root_cor.
  if (fine_root_cor=="Default") {
    tubeh = 0                     # the gap between IC and soil level? Ask Chris.
    # depth profile of roots
    depic = (30-tubeh/10)/100
    dzz = 0.5*(exp(-7.344*depic)+exp(-1.303*depic)) 
  } else {
    dzz <- fine_root_cor 
  }
  
  # Add root estimates below 30cm - Check this with Yadvinder.
  # In David Galbraith study, 37% of the fine roots (<2mm) were below 30cm, this is close to 39% found by Jackson et al. (date?) that we use here.
  # Please note: there is a discrepancy between here and the RAINFOR manual (2.3, pp. 47), because the assumption there is 45% in the top 30 cm of the soil.
  
  # sum total carbon from roots (diameter ~ 14cm, depth ~ 30cm)
  data2$ciric    = (3.14*tubed^2) # surface area m2, tubed is defined in the function parameters at the start of the code.
  data2$volic    = data2$ciric*depic
  data2$rootztot[is.na(data2$rootztot)] = 0 
  data2$totaic   = data2$rootztot / (1-dzz)   # total roots estimated by extrapolating timesteps, plus roots growing below 30cm estimated with the correction factor dzz.
  data2$ic_MgCha = (data2$totaic/data2$ciric)*10000/(2.1097*1000*1000)  # Mg roots per ha (10000m2 = 1ha, 1Mg = 1000000g divide by 2 for carbon)
  data2$ic_id = paste(data2$plot_code, data2$ingrowth_core_num, sep="_")
  
  
  # Use this if any ingrowth core has only one measurement. 
  # If this is the case, it will result in an error when you try to get_time_diffs below.
  #singlemeas = data2 %>% group_by(ic_id) %>%
  #                       summarize(nobs=n()) %>%
  #                       mutate(nobs = as.numeric(nobs)) %>% 
  #                       subset(nobs %in% 1)
  #                       ggplot(data=., aes(ic_id, nobs)) + geom_point() 
  #singlemeas
  
  #data2 = subset(data2, ic_id != singlemeas$ic_id)
  
data3 = data2 %>% group_by(ic_id) %>%
                  arrange(ic_id, collectiondate) %>%                        # Order data by ingrowth core and date.
                  # filter(., ic_id > 1) %>%                 
                  mutate(collectiondate = as.POSIXct(collectiondate),       # Convert dates to POSIXct
                  interval  = c(90, get_time_diffs(collectiondate)),        # Get collection interval using get_time_diffs (sourced from functions.r)
                  dailyNPProot = ic_MgCha/interval,
                  monthlyNPProot = dailyNPProot * 30.42) %>%
                  data.frame(.) 


# TO DO:
# record previous meas in each row for data3 so we know the period for which the measurement is referring
# mutate(date = as.Date(sprintf("%d-%2d-%2d", year, month, day))) %>%
# arrange(date) %>%
# mutate(prev_meas = lag(date))

# Check & remove outlyers > 2.5 in data3
outmeas = data3 %>% subset(monthlyNPProot >= 1)
outmeas

data3 = data3[!data3$monthlyNPProot >= 2.5,]

# convert to MgC / ha / month per plot 
  
  
  data4 = data3 %>% group_by(plot_code, year, month) %>% 
    dplyr::summarize(monthlyNPProot_1 = mean(monthlyNPProot, na.rm = T), 
                     n_perplot = n(),
                     monthlyNPProot_sd = sd(monthlyNPProot, na.rm = T), 
                     collectiondate = min(collectiondate)) %>%
                     data.frame(.) %>%
                     arrange(year, month) %>% 
                     ungroup() %>% 
                     rename(monthlyNPProot=monthlyNPProot_1)
  
  
# plot it
  data4 %>% group_by(plot_code) %>%
    ggplot(data=., aes(collectiondate, monthlyNPProot)) + geom_point() +
    #ylim(0,2) +
    facet_wrap(~plot_code)
  

  # remove the first measurement from all tubes if the stock measurement wasn't removed initially
  if (! remove_stock_meas) {
    stock_meas_times = unique(with(stock_meas, paste(year, month, day)))
    data3 = subset(data3, ! paste(year, month, day) %in% stock_meas_times)
    data4 = subset(data4, ! paste(year, month, day) %in% stock_meas_times)
  }
  
  return(list("per_ic_MgC_ha_mo" = data3, "plotaverage_MgC_ha_mo" = data4))
#}


