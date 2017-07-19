setwd("/Users/cecile/Dropbox/GEMcarbondb/db_csv/db_csv_2015/readyforupload_db/acj_pan_2015/")

Rstem1   <- read.table("Rstem_PAN03_co2slope.csv",  sep=",", header=T)
tree_num <- read.table("Rstem_tree_num.csv",  sep=",", header=T)

Rstem    <- sqldf("SELECT Rstem1.*, tree_num.PAN03 FROM  Rstem1 JOIN  tree_num  ON tree_num.sub_plot = Rstem1.sub_plot")
Rstem$tree_num    <- Rstem$PAN03
  
write.csv(Rstem, file="Rstem_PAN03_co2slope_wttreenum.csv")
