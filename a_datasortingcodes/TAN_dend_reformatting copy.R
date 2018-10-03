library(dplyr)
library(grDevices)
library(tidyverse)
require(tidyverse)


setwd("~/Github/gemcarbon_data/raw_data_ingemdb_forELDS")

tan  <- read.table("DendTang.csv", sep=",", header=T) #read.csv(..., stringsAsFactors = F) 
tan$dap08_mm <- tan$dap08*10
# control plot is 5  & in census plot 1 = plot A (the control, unburned plot)
# plot C = plot 6 = 
# 1,2,3 = A,B,C - so B should have the most burnt damage.
# Send mail to Paolo to confirm if we can upload 1 ha plots to Forestplots (not publically available) plots A & C. Species code - 

tan1 = tan %>% select(plot = p5r6el5, tree_tag = placa,  
                         dap08_mm, 
                         height = p.height,
                         dap = X12.17.08) %>% # dap in mm
                   mutate(day = 17, month = 12, year = 2008) %>% 
                          data.frame(.)

tan2 = tan %>% select(plot = p5r6el5, tree_tag = placa,  
                          dap08_mm, 
                          height = p.height,
                          dap = X03.06.09) %>% # dap in mm
  mutate(day = 6, month = 3, year = 2009) %>% data.frame(.)

tan3 = tan %>% select(plot = p5r6el5, tree_tag = placa,  
                          dap08_mm, 
                          height = p.height,
                          dap = X06.03.09 ) %>% # dap in mm
  mutate(day = 3, month = 6, year = 2009) %>% data.frame(.)


tan4 = tan %>% select(plot = p5r6el5, tree_tag = placa,  
                          dap08_mm, 
                          height = p.height,
                          dap = X09.16.09) %>% # dap in mm
  mutate(day = 16, month = 9, year = 2009) %>% data.frame(.)


tan5 = tan %>% select(plot = p5r6el5, tree_tag = placa,  
                          dap08_mm, 
                          height = p.height,
                          dap = X12.10.09) %>% # dap in mm
  mutate(day = 10, month = 12, year = 2009) %>% data.frame(.)


tan6 = tan %>% select(plot = p5r6el5, tree_tag = placa,  
                          dap08_mm, 
                          height = p.height,
                          dap = X03.17.10) %>% # dap in mm
  mutate(day = 17, month = 3, year = 2010) %>% data.frame(.)


tan7 = tan %>% select(plot = p5r6el5, tree_tag = placa,  
                          dap08_mm, 
                          height = p.height,
                          dap = X06.02.10) %>% # dap in mm
  mutate(day = 2, month = 6, year = 2010) %>% data.frame(.)


tan8 = tan %>% select(plot = p5r6el5, tree_tag = placa,  
                          dap08_mm, 
                          height = p.height,
                          dap = X07.09.10 ) %>% # dap in mm
  mutate(day = 9, month = 7, year = 2010) %>% data.frame(.)


tan9 = tan %>% select(plot = p5r6el5, tree_tag = placa,  
                          dap08_mm, 
                          height = p.height,
                          dap = X12.23.10 ) %>% # dap in mm
  mutate(day = 23, month = 12, year = 2010) %>% data.frame(.)


tan10 = tan %>% select(plot = p5r6el5, tree_tag = placa,  
                          dap08_mm, 
                          height = p.height,
                          dap = X03.03.11 ) %>% # dap in mm
  mutate(day = 03, month = 03, year = 2011) %>% data.frame(.)

tan_new = rbind(tan1, tan2, tan3, tan4, tan5, tan6, tan7, tan8, tan9, tan10)
tan_new = tan_new %>% mutate(date = as.Date(paste(year, month, day, sep="."), format="%Y.%m.%d"))

write.csv(tan_new, file="tan_reformat_cg20180326.csv")


