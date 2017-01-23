# Main routine to create per-subplot productivity datasets

library(dplyr)

datadir = "a_readyforupload_db/acj_pan_2015/"

# Root ingrowth cores ####
  source("NPProot_2015.R")
  testfile = "ingrowth_core_ACJ-01_2013_2104_nostock.csv"
  roots_acj = NPProot_ic(paste0(datadir,testfile), "ACJ-01")
  
# Fine litterfall ####
  # lots happens inside the function that would be a pain to split across individual traps.  So, subset the data outside, and run the function on the subsetted data.
  source("a_Archive/flf_2015.R")
  #testfile = "Litterfall_ACJ_2013_2014.csv"
  testfile = "eltr_flf_2006to2014.csv"
  flf_data = read.csv(paste0(datadir,testfile))
  
  #flf_processed = flf_data %>% group_by(plot_code, litterfall_trap_num) %>% do(flf(data.frame(.)))
  flf_processed = flf_data %>% group_by(plot_code) %>% do(flf(data.frame(.)))
  
  # new code
  source("flf_2016.R")
  temp = flf(paste0(datadir, testfile))
  flf_processed = flf_data %>% group_by(plot_code) %>% do(flf(data.frame(.)))
  
  temp = list()
  for (plot in unique(flf_data$plot_code)) {
    print(paste("trying plot", plot))
    temp[[plot]] = flf(subset(flf_data, plot_code == plot))
    print("success!")
    print(str(temp[[plot]]))
  }
  
  #data.flf <- read.table("/Users/cecile/Dropbox/GEMcarbondb/db_csv/db_csv_2015/readyforupload_db/acj_pan/Litterfall_ACJ_2013_2014_test.csv", sep=",", header=T)
  