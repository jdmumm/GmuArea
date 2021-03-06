# load ----
library(tidyverse)
read.csv('./data/GMU.csv') -> GMU
read.csv('./data/UCU_marine.csv') -> MAR
read.csv('./data/AdminLand_GMU.csv') -> BLM
read.csv('./data/CA_GMU.csv') -> CA

# Select, Filter, Sum ----
GMU %>% group_by (GMU = GMUvalue) %>% 
  summarize (Total = sum(SqMi)) -> gmu # 2 recs for gmu 26 and 14 
   
MAR %>% group_by (GMU = GMUvalue) %>% summarise(
    Marine = sum(sqmiwater)) -> mar
BLM %>% filter (AGENCY_NAM %in% c("Private", "Alaska Native Allotment",
                                  "Alaska Native Lands Patented or Interim Conveyed",
                                  "National Park Service")) %>%
  group_by (GMU = GMUvalue, AGENCY_NAM) %>% summarise(
  Blm = sum(sqMi_BLM, na.rm = T)) %>%
  spread (AGENCY_NAM,Blm) -> blm
CA %>% select(AREANAME , GMU = GMUlabel, sqMi_CA) %>% filter (!is.na(AREANAME)) %>% 
  group_by(GMU) %>% summarize ('State Closed Areas' = sum(sqMi_CA, na.rm=T)) -> ca

# JOin ----
gmu %>% left_join (mar) %>% left_join (ca) %>% left_join (blm) %>% replace(is.na(.), 0) %>%
        rowwise () %>% mutate (
                Land = Total - Marine,
                LandNotOpen = sum(`State Closed Areas` + `Alaska Native Allotment` + 
                                  `Alaska Native Lands Patented or Interim Conveyed`+
                                  `National Park Service` +  Private, na.rm = T),
                LandOpen = Land - LandNotOpen,
                LandOpenPercent = 100 * LandOpen/Land) %>% 
        mutate_at (-1,round, 2) %>% 
        relocate (Land, .after = Marine) -> out
  # sqmi to acres 
    out %>% mutate(across(2:11, ~.x * 640)) -> acres

# write ----
out %>% write.csv('./out/openAreaByGMU_sqmi.csv', row.names = F)
acres %>% write.csv('./out/openAreaByGMU_acres.csv', row.names = F)
    
# compare to results via erase ----
read.csv ('./data/gmuOpen_viaEras.csv') -> ers
read.csv('./out/openAreaByGMU_sqmi.csv') -> out

ers %>% mutate (GMU = GMUvalue) %>%
  group_by(GMU) %>% summarize (open_ers = sum(sqmi_open))   -> ers        

out %>% left_join (ers) %>%
  select(GMU, LandOpen, open_ers) %>% mutate (dif = open_ers - LandOpen, perdif = round(100* dif/open_ers, 2))

