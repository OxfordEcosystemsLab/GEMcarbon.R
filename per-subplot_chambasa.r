# Main routine to create per-subplot productivity datasets

# Root ingrowth cores
  source("NPProot_2015.R")
  datadir = "a_readyforupload_db/acj_pan_2015/"
  testfile = "ingrowth_core_ACJ-01_2013_2104_nostock.csv"
  roots_acj = NPProot_ic(paste0(datadir,testfile), "ACJ-01")
  