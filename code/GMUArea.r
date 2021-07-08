library(tidyverse)

# load ----
read.csv('./data/GMU.csv') -> GMU
read.csv('./data/UCU_marine.csv') -> MAR
read.csv('./data/AdminLand_GMU.csv') -> BLM
read.csv('./data/CA_GMU.csv') -> CA

# Select, Filter, Sum ----
GMU %>% group_by (GMU = GMUvalue) %>% 
  summarize (Total = sum(SqMi)) -> gmu # 2 records for gmu 26 and 14 
   
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
                LandNotOpen = sum(`State Closed Areas` + `Alaska Native Allotment` + `Alaska Native Lands Patented or Interim Conveyed`+
                              `National Park Service` +  Private, na.rm = T),
                LandOpen = Land - LandNotOpen,
                LandOpenPercent = 100 * LandOpen/Land) %>% 
        mutate_at (-1,round, 2) %>% 
        relocate (Land, .after = Marine) -> out
# write ----
out %>% write.csv('./out/openAreaByGMU_sqmi.csv')

                