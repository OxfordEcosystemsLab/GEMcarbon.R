
setwd("~/Github/gemcarbon_data/raw_data_ingemdb_forELDS")
hank1 = read.table("Ankasa_dendrometers_Jan_plot1.csv", sep=",", header=T)
hank2 = read.table("Ankasa_dendrometers_Jan_plot2.csv", sep=",", header=T)
hank3 = read.table("Ankasa_dendrometers_Jan_plot3.csv", sep=",", header=T)
dend = read.table("dendro_20180207.csv", sep=",", header=T)

dend1 = dend %>% mutate(id = paste(plot_code, sub_plot, tree_tag, sep = '_')) # %>% select(id, plot_code, sub_plot, tree_tag, year, month, day, dendrometer_reading_mm)

hank1a = hank1 %>% mutate(plot_code = "ANK-01",
                          id = paste(plot_code, subplot, tag, sep = '_')) %>%
                          select(id, DBH_cm_2011 = DBH_cm_July11, height_m_July12)

hank2a = hank2 %>% mutate(plot_code = "ANK-02",
                          id = paste(plot_code, subplot, tag, sep = '_')) %>%
                          select(id, DBH_cm_2011 = DBH..cm..Oct.11, height_m_July12 = height..m.July.12)

hank3a = hank3 %>% mutate(plot_code = "ANK-03",
                          id = paste(plot_code, subplot, tag, sep = '_')) %>%
                          select(id, DBH_cm_2011 = DBH_cm_Oct11, height_m_July12)

ank = bind_rows(hank1a, hank2a, hank3a)

dend2 = dend1 %>% left_join(ank, by = "id")

census_all %>% filter(d0>=100) %>%
  group_by(plot_code, census_date) %>% summarize(nobs=n()) %>%
  ggplot(data=., aes(census_date,nobs))+geom_path()+geom_point()+
  facet_wrap(~plot_code, scales = "free")

setwd("~/Github/gemcarbon_data/raw_data_ingemdb_forELDS")
write.csv(dend2, file="dendro_20180208.csv")