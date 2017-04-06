library(mice)
library(zoo)
library(dplyr)

gapfill_subplots <- function(GEMdf, all_dates = F, from, to, date_col = "date", meas_col = "MgC_ha_yr_corr", meas_interval_col = "interval", 
                             trim_dates = T, out = c("matrix", "dataframe_monthly", "dataframe_annual")) {
  # expects monthly data, and that plot & subplot columns exist
  # set all_dates = T to use all data.  Otherwise, set from and to dates (they must be Date objects).
  # specify the date column with the date_col argument
  # trim_dates: trim dates per plot to existing data?
  # meas_interval_col in days
  
  if (!missing(from) & !missing(to)) {
    stopifnot(class(from) %in% c("Date", "POSIXct", "POSIXlt") & class(to) == class(from))
    dateclass = class(from)
  } else if (missing(from) + missing(to) == 1) {
    stop("Must supply both 'from' and 'to' arguments")
  }
  
  if (! class(GEMdf[,date_col][[1]]) %in% c("Date", "POSIXct", "POSIXlt")) {
    stop("Date column must be of class Date, POSIXct, or POSIXlt")
  }
  
  if (FALSE) { # don't deal with intervals prior to first measurement for now... let GEM ts code handle that...
    # incorporate measurment intervals into available data date ranges.  This is necessary when measurements are the result of measuring
    #   change of time, such as with litterfall or root npp.  This is not appropriate for instantaneous sample measurments, such as respiration.
    # if the previous measurment interval was more than 3.5 months ago, then don't use it.
    long_intervals = abs(GEMdf[,meas_interval_col][[1]]) > 105
    long_intervals[is.na(long_intervals)] = T
    GEMdf[long_intervals,meas_interval_col] = 0 # should we set this to 90?  what is the interval in cases where we have large gaps in measurements?
    GEMdf = GEMdf %>% group_by(plot, subplot) %>% mutate_(interval_start = interp(~ this_date_col - abs(this_meas_interval_col), this_date_col = as.name(date_col), this_meas_interval_col = as.name(meas_interval_col)))
  }
  
  # Cut data based on to and from dates before interpolating data (plot level - don't cut based on subplot dates).
  #   This determines the outer bounds of available data for each plot.
  dateslice = GEMdf %>% group_by(plot) %>% arrange_(date_col) %>%
  { if (all_dates) . else 
    filter_(., interp(~ this_date_col >= from & this_date_col <= to, this_date_col = as.name(date_col))) }
  
  # interpolate missing inner values.  This also ensures a row exists for each day within the date range (defined above) of that plot.
  #  this may be doing the wrong thing, if missing internal values result from a long interval.  Look at your data before doing this!
  interp_df = dateslice %>% group_by(plot, subplot) %>%
    do({
      temp = na.approx( merge(zoo(.[,meas_col][[1]], .[,date_col][[1]]), # GEM ts
                              zoo( ,seq(min(.[,date_col][[1]]), max(.[,date_col][[1]]), by = "day") ), all = T), na.rm = F) # empty & complete ts.  add na.rm = F to not drop leading NA's
      return(data.frame(date = index(temp), meas = coredata(temp)))
    })
  
  # trim entire dataset to minimum common date span across plots
  if (trim_dates) {
    max_start = max(aggregate(date ~ plot, data = interp_df, min)$date)
    warning(paste("start date set to", max_start))
    min_end = min(aggregate(date ~ plot, data = interp_df, max)$date)
    warning(paste("end date set to", min_end))
  } else {
    max_start = from
    min_end = to
  }
  
  trimmed_df = interp_df %>% ungroup() %>% group_by(plot) %>%
    filter(date >= max_start & date <= min_end) %>% # Trim data for each experiment to cover the same date range per plot.  Will result in leading/trailing NAs for some subplots.
    setNames(gsub("-","",names(.)))
  
  # impute leading and trailing NA's in subplots
  # note that we're imputing on a daily basis here.  probably better to impute on the raw data, and then fill in with zoo above.  but need to
  #   figure out how to just impute outer values first, and let zoo impute inner values.  Or, use mice for all imputation...
  trimmed_df = trimmed_df %>% group_by(plot) %>% spread(subplot, meas)
  
  trimmed_mice = trimmed_df %>% group_by(plot) %>%
    do({
      imp_data = data.frame(.) # work with data frames instead of dplyr::tbl's
      # Don't impute values for subplots that are all NA
      imp0 = mice(imp_data, maxit = 0)
      predmat = imp0$predictorMatrix
      allNAs = sapply(imp_data, function(x)all(is.na(x)))
      predmat[allNAs,] = 0
      # keep plot and date intact in mice
      predmat[1:2,] = 0 # mask out date and plot columns
      predmat[,1:2] = 0 # mask out date and plot columns
      # if there are no missing values, don't impute
      anyNAs = sapply(imp_data[,!allNAs], function(x)any(is.na(x)))
      if (!any(anyNAs)) {
        return(imp_data)
      }
      # otherwise, impute
      temp_mice = try(mice(imp_data, predictorMatrix = predmat))
      if (class(temp_mice) != "mids") browser()
      return(data.frame(complete(temp_mice,1)))   # return the first complete case.  more are produced that we could work with...
    })
  trimmed_mice = trimmed_mice %>% gather(subplot, meas, -c(plot, date)) 
  trimmed_mice$subplot = as.integer(sub("X","",trimmed_mice$subplot)) # dplyr prepends "X" to columns with numbers as names
  # names(outdf_mice)[names(outdf_mice) == "meas"] = meas_col
  # names(outdf_mice)[names(outdf_mice) == "date"] = date_col
  
  # diagnostic data plot
  # ggplot(trimmed_mice, aes(x=date, y=meas)) + geom_point(aes(group = subplot)) + facet_wrap(~plot) + 
  #  scale_x_date(breaks = pretty_breaks()) +
  #  theme(axis.text.x = element_text(angle = 90, hjust = 0, vjust = 0.5))
  
  if (out == "matrix") {
    outdf = trimmed_mice %>% group_by(plot, subplot) %>% arrange(date) %>% #na.omit() %>%
      summarize(temp_ann = sum(meas) / ( as.numeric(difftime(last(date), first(date), units = "days")) / 365 )) %>% 
      spread(plot, temp_ann) %>%
      setNames(gsub("-","",names(.)))
  } else if (out == "dataframe_timeseries") {
    outdf = trimmed_mice %>% group_by(plot, subplot) %>%
      setNames(gsub("-","",names(.)))
    names(outdf)[names(outdf) == "meas"] = meas_col
    names(outdf)[names(outdf) == "date"] = date_col
  } else if (out == "dataframe_annual") {
    outdf = trimmed_mice %>% group_by(plot, subplot) %>% na.omit() %>%
      summarize(temp_ann = sum(meas) / ( as.numeric(difftime(last(date), first(date))) / 365 )) %>% 
      setNames(gsub("-","",names(.)))
    names(outdf)[names(outdf) == "temp_ann"] = paste0(meas_col, "_ann")
    names(outdf)[names(outdf) == "date"] = date_col
  }
  
  return(outdf)
}
