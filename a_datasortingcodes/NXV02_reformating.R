library(dplyr)
library(grDevices)
library(tidyverse)
require(tidyverse)


setwd("~/Github/gemcarbon_data/raw_data_ingemdb_forELDS")

nxv02  <- read.table("NXV-02_2013-2017_Imma.csv", sep=",", header=T)

nxv02$d013 = as.numeric(as.character(nxv02$DAP.2013))*10
nxv02$d015 = as.numeric(as.character(nxv02$DAP.2015))*10 
nxv02$d017 = as.numeric(as.character(nxv02$DAP.2017))*10
nxv02$h13  = as.numeric(as.character(nxv02$H.2013)) 
nxv02$h15  = as.numeric(as.character(nxv02$Height.2015))
nxv02$h17  = as.numeric(as.character(nxv02$H.2017))

nxv13 = nxv02 %>% select(Sub_Plot_T1 = T1.Plot, Tag.no, Tag.TRO, T2.Plot.TRO, 
                         x, y,
                         Family, Species, Original.identification, 
                         height = h13,
                         DAS = DAS.2013, DAP = d013, POM = POM.2013,
                         F1 = Flag.1.2013, F2 = Flag.2.2013,
                         CI, CF, LP, LI,  
                         notes = Notes.2013) %>%
                   mutate(date = 2013.0014) %>% data.frame(.)

nxv15 = nxv02 %>% select(Sub_Plot_T1 = T1.Plot, Tag.no, Tag.TRO, T2.Plot.TRO, 
                         x, y, 
                         Family, Species, Original.identification,
                         DAS = DAS...2015, DAP = d015, POM = POM.2015,    
                         F1 = `F.1.2015`, F2 = `F.2.2015`, height = h15, 
                         CI = CI.1, CF = CF.1, LP = LP.1, LI = LI.1, 
                         notes = Census.Notes.2015 ) %>%
                   mutate(date = 2015.0014) %>% data.frame(.)


nxv17 = nxv02 %>% select(Sub_Plot_T1 = T1.Plot, Tag.no, Tag.TRO, T2.Plot.TRO, 
                         x, y,
                         Family, Species, Original.identification,     
                         DAS = DAS.2017, DAP = d017, POM = POM.dap.2017, height = h17,      
                         F1 = F1.2017, F2 = F2.2017,      
                         CI = CI.2, CF = CF.2, LP = LP.2, LI = LI.2,    
                         notes = Notes.2017) %>%
                   mutate(date = 2017.0014) %>% data.frame(.)


nxv02_new = rbind(nxv13, nxv15, nxv17)
write.csv(nxv02_new, file="nxv02_reformat_cg20180320.csv")

# test

#census_all  <- read_csv("~/Github/gemcarbon_data/raw_data_ingemdb_forELDS/forestplots20180320.csv", na=c("NA", "NaN", "")) # Census_Santarem_2014_2016.csv
census_all = nxv02_new
names(census_all) <- tolower(names(census_all))
names(census_all) <- gsub(pattern=" ",replacement="_",names(census_all))
census_all <- census_all %>% rename(tree_tag = tag.no) %>% mutate(tree_tag = as.character(tree_tag))

census_all %>% #filter(plot_code=="NXV-02") %>%
  filter(near(date, 2013.001, tol=0.01)) %>%
  filter(dap>=100) %>%
  pull(dap)


census_all %>%
  filter()

 