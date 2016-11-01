# Main routine to create per-subplot productivity datasets

library(dplyr)

datadir = "a_readyforupload_db/acj_pan_2015/"

# Root ingrowth cores ####
  source("NPProot_2015.R")
  testfile = "ingrowth_core_ACJ-01_2013_2104_nostock.csv"
  roots_acj = NPProot_ic(paste0(datadir,testfile), "ACJ-01")
  
# Fine litterfall ####
  # lots happens inside the function that would be a pain to split across individual traps.  So, subset the data outside, and run the function on the subsetted data.
  source("flf_2015.R")
  #testfile = "Litterfall_ACJ_2013_2014.csv"
  testfile = "eltr_flf_2009to2014.csv"
  flf_data = read.csv(paste0(datadir,testfile))
  
  #flf_processed = flf_data %>% group_by(plot_code, litterfall_trap_num) %>% do(flf(data.frame(.)))
  flf_processed = flf_data %>% group_by(plot_code) %>% do(flf(data.frame(.)))
  
  
  for (plot_code in flf_data$plot_code) {
    for ()
  }, flf_data$litterfall_trap_num)) {
    flf_acj = flf(flf_data, "ACJ")
  }
  
  #data.flf <- read.table("/Users/cecile/Dropbox/GEMcarbondb/db_csv/db_csv_2015/readyforupload_db/acj_pan/Litterfall_ACJ_2013_2014_test.csv", sep=",", header=T)
  