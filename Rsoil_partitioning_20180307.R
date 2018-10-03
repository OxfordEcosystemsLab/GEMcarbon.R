
# call packages
library(tidyverse)
library(lubridate)

# load dataset
setwd("~/Github/gemcarbon_data/raw_data_ingemdb_forELDS/soil_respiration")
rtot1  = read.table("rsoil_tot_persubplot_20180213.csv", sep=",", header=T)
rpart = read.table("rsoil_part_MgChamo_20180306.csv", sep=",", header=T)


rtot = rtot1 %>% select(plot_code, sub_plot, collar_number, replica, Rflux_MgC_ha_mo, collection) %>%
                 mutate(id = paste(plot_code, collection, sep = "_")) %>% data.frame(.)
         
# Estimate get soil respiration paritioning fractions for each experimental set. 
# Fraction of autotrophic respiration: Faut 
#rrA1 = ((con_no_litA-So_no_litA))/(con_no_litA)) # (Rtot - Rhet) / Rtot

# Fraction of soil organic matter: Fsom 
# Fraction of litterfall respiration: Flit (control collars: avg(Rlit-Rnolitter, Rdouble-Rlitter)) 
# Fraction of mycorrhizae: Fmyc (Rmyco - Rsom)

rpart_con_nor_lit = rpart %>% filter(treatment_code_partitioning == "con_nor_lit") %>%
                              mutate(id = paste(plot_code, collection_date, sep = "_"),
                                     Rcon_nor_lit = avg,
                                     sdcon_nor_lit = sd) %>% data.frame(.) %>%
                                     select(id, Rcon_nor_lit, sdcon_nor_lit)

rpart_con_doub_lit = rpart %>% filter(treatment_code_partitioning == "con_doub_lit") %>%
                               mutate(id = paste(plot_code, collection_date, sep = "_"),
                                      Rcon_doub_lit = avg,
                                      sdcon_doub_lit = sd) %>% data.frame(.) %>%
                                      select(id, Rcon_doub_lit, sdcon_doub_lit)
  
rpart_con_no_lit = rpart %>% filter(treatment_code_partitioning == "con_no_lit") %>%
                             mutate(id = paste(plot_code, collection_date, sep = "_"),
                                    Rcon_no_lit = avg,
                                    sdcon_no_lit = sd) %>% data.frame(.) %>%
                                    select(id, Rcon_no_lit, sdcon_no_lit)

rpart_M = rpart %>% filter(treatment_code_partitioning == "M") %>%
                    mutate(id = paste(plot_code, collection_date, sep = "_"),
                           RM = avg,
                           sdM = sd) %>% data.frame(.) %>%
                           select(id, RM, sdM)

rpart_ml_nor_lit = rpart %>% filter(treatment_code_partitioning == "ml_nor_lit") %>%
                             mutate(id = paste(plot_code, collection_date, sep = "_"),
                                    Rml_nor_lit = avg,
                                    sdml_nor_lit = sd) %>% data.frame(.) %>%
                                    select(id, Rml_nor_lit, sdml_nor_lit)

rpart_my_nor_lit = rpart %>% filter(treatment_code_partitioning == "my_nor_lit") %>%
                             mutate(id = paste(plot_code, collection_date, sep = "_"),
                                    Rmy_nor_lit = avg,
                                    sdmy_nor_lit = sd) %>% data.frame(.) %>%
                                    select(id, Rmy_nor_lit, sdmy_nor_lit)

rpart_so_nor_lit = rpart %>% filter(treatment_code_partitioning == "so_nor_lit") %>%
                             mutate(id = paste(plot_code, collection_date, sep = "_"),
                             Rso_nor_lit = avg,
                             sdso_nor_lit = sd) %>% data.frame(.) %>%
                             select(id, Rso_nor_lit, sdso_nor_lit)




 
my_doub_lit 
my_no_lit 
so_doub_lit 
so_no_lit 

  
# con_nor_lit  = control & normal litterfall, con_no_lit   = control & no litterfall, 
# con_doub_lit = control & double litterfall, my_nor_lit  = mycorrhizae & normal litterfall, 
# my_no_lit = mycorrhizae & no litterfall, my_doub_lit = mycorrhizae & double litterfall,
# so_nor_lit = soil & normal litterfall, so_no_lit = soil & no litterfall, 
# so_doub_lit = soil & double litterfall. + ADD ml = mineral layer.

Fpart1 = left_join(rpart_con_nor_lit, rpart_con_doub_lit, by='id') %>%
                left_join(., rpart_con_no_lit , by='id') %>%
                left_join(., rpart_ml_nor_lit, by='id') %>%
                left_join(., rpart_my_nor_lit, by='id') %>%
                left_join(., rpart_so_nor_lit, by='id') %>% 
                   mutate(Faut = (Rcon_nor_lit - Rso_nor_lit) / Rcon_nor_lit, # (Rtot - Rhet) / Rtotn  = Raut / Rtot 
                          Fhet = Rso_nor_lit / Rcon_nor_lit,  # Rhet / Rtot,
                          # or Fhet = mean((so_nor_lit / con_nor_lit), (so_no_lit / con_no_lit), (so_doub_lit / con_doub_lit)) 
                          aa = (Rcon_nor_lit - Rcon_no_lit),
                          bb = (Rcon_doub_lit - Rcon_nor_lit),
                          Flit = ((aa+bb)/2), #(control collars: avg(Rlit-Rnolitter, Rdouble-Rlitter)) ,
                          Fmyc = (Rmy_nor_lit - Rso_nor_lit) / Rcon_nor_lit # Fmyc = (Rmyco - Rhet) / Rtot
                            ) %>% data.frame(.)

# IF Faut < 0, 
  
# Estimate the mean and SE across the plot for each fraction. 
Fpart = Fpart1 %>% group_by(id) %>%
                   summarise(avgFaut = mean(Faut),
                             avgFhet = mean(Fhet)) %>%
                   data.frame(.)

rtot2 = left_join(rtot, Fpart, by='id') %>% data.frame(.)
  
# Estimate Respiration of each component across the plot by multiplying total soil respiration by the fraction of that component 

  #TEST
  rtot3 = rtot2  %>% filter(plot_code == "ESP-01")

uid = unique(rtot1$codew) 
aa = c()
bb = c()
cc = c()
dd = c()
ee = c()
ff = c()
gg = c()

for (i in 1:length(uid)) {
  sub  = subset(rtot3, subset=(rtot3$codew == uid[i]))      
  id   = tail(sub$codew, n=1)
  Rtot = sub$Rflux_MgC_ha_mo  
  faut = sub$Faut
  fhet = sub$Fhet
  flit = sub$Flit
  fmyc = sub$Fmyc

  if (sum(is.na(Rtot)) < 7 & length(Rtot) >= 1){                   # if at least 7 of the 10 values are different from NA, we apply a linear a regression to get CO2 flux
    
    Raut = Rtot * faut #rtot$Rflux_MgC_ha_mo
    Rhet = Rtot * fhet
    Rlit = Rtot * flit 
    Rmyc = Rtot * fmyc
    
  }else{ Raut = NA
         Rhet = NA 
         Rlit = NA 
         Rmyc = NA 
        }
  
  aa        = rbind(aa, id)
  bb        = rbind(bb, Raut)
  cc        = rbind(cc, Rhet)
  dd        = rbind(dd, Rlit)
  ee        = rbind(ee, Rmyc)
  print(bb)
}
  

#