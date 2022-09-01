rm(list = ls())

# DESCRIPTION ####
# generate list of blocks that were annexed ####

# get environment ready 
setwd("~/Google Drive/My Drive/Stanford/QE2")

library("stringr")
library("dplyr")
library("stargazer")
library("tidyverse")
library("tidycensus")
library("lme4")
library("readr")
library("data.table")
#library("readstata13")
library("magrittr")

# 00-07 ####
# clean: 
# 1. for contiguous blocks, keep only eligible blocks 
# 2. for annexed blocks, keep only eligible blocks 
# we just need the blockids of blocks that are annexed
state_codes <- c("AL_01", "AR_05", "AZ_04", "CA_06", "CO_08", 
                 "DE_10", "FL_12", "GA_13", "IA_19", "ID_16", "IL_17", "IN_18",
                 "KS_20", "KY_21", "LA_22", 
                 "MI_26", "MN_27", "MS_28", "MO_29", "MT_30", 
                 "NC_37", "ND_38", "NE_31", "NM_35", "NV_32", 
                 "OH_39", "OK_40", "OR_41", 
                 "SC_45", "SD_46", "TN_47", "TX_48", "UT_49", "VA_51",
                 "WA_53", "WV_54", "WI_55", "WY_56"
)

# places that annexed in 2000-2007 
pl0007 <- read_csv("analyticalfiles/bas_years.csv") %>%
  filter(period == "2000 to 2007")

# want to know which places are CDPs--they do not annex
cdps07 <- read_csv("pl2007_cleaned.csv") %>% 
  select(Geo_NAME, plid) %>%
  mutate(cdp = ifelse(grepl("CDP|cdp", Geo_NAME), 1, 0)) %>%
  filter(cdp==1)
pl0007 %<>% # filter out places in 2007 that are CDPs
  filter(!(plid %in% cdps07$plid) & !is.na(plid))
rm(cdps07)

# get block-level 
aa <- read_csv("2000buffers.csv") %>%
  filter(!duplicated(blkid)) %>%
  rename(plid = bufferplace)

aa %<>%
  mutate(annexed = ifelse(plid %in% pl0007$plid, 1, 0))
table(aa$annexed)

blocks2000 <- read_csv("blocks2000_var.csv")

blocks2000 %<>%
  mutate(STATEA = str_pad(STATEA, 2, side = "left", pad = "0"),
         PLACEA = str_pad(PLACEA, 5, side = "left", pad = "0"),
         plid = ifelse(PLACEA == "99999", NA, paste0(STATEA, PLACEA))) 

aa %<>%
  filter(blkid %in% blocks2000$blkid) %>%
  left_join(blocks2000 %>% select(blkid:nbminvap00b), by = "blkid") %>%
  filter(pop00b > 1)

rm(blocks2000)

#clean up and get ready for Census data 
rac2000 <- read_csv("LODES data/rac_2000.csv")
names(rac2000)

wac2000 <- read_csv("LODES data/wac_2000.csv")
names(wac2000)

# check they seem to be in comparable formats
head(aa$blkid)
head(rac2000$h_geocode)
head(wac2000$w_geocode)

aa %<>%
  left_join(rac2000 %>% select(h_geocode:nhincjobs00), by = c("blkid" = "h_geocode")) %>%
  mutate_at(c("njobs00", "nhincjobs00"), ~ifelse(is.na(.), 0, .)) %>%
  left_join(wac2000 %>% select(w_geocode:ret), by = c("blkid" = "w_geocode")) %>%
  mutate_at(c("jobs", "man", "ret"), ~ifelse(is.na(.), 0, .))
rm(rac2000, wac2000)

table(aa$annexed)

# we can't have places that annexed all their blocks, 
# nor places that only had 1 contiguous block
aa %<>%
  group_by(plid) %>%
  mutate(n = n()) %>%
  ungroup() %>%
  filter(n >= 2) %>%
  select(-n)

table(aa$annexed)

# merge in vra 
vrastates <- c("01", "02", "04", "13", "22", "28", "45", "48", "51")
vra.df <- read_csv("vra_counties.csv")
vra.df %<>% 
  mutate(countyfips = str_pad(countyfips, 5, side = "left", pad = "0"),
         sectionv = 1)

aa %<>%
  mutate(countyfips = paste0(STATEFP00, COUNTYFP00), 
         vra = case_when(
    STATEFP00 %in% vrastates ~ 1,
    countyfips %in% vra.df$countyfips ~ 1,
    TRUE ~ 0
  ))

table(aa$vra)
table(aa$annexed)

# prep for rbind 
aa$period <- "0007"
write_csv(aa, "analyticalfiles/annexedblocks0007dem_bas.csv") 
rm(list = ls())

# 0713 ####
pl0713 <- read_csv("analyticalfiles/bas_years.csv") %>%
  filter(period == "2007 to 2013")

cw <- read_csv("cw/2000-to-2010_unique.csv")

# want to know which places are CDPs--they do not annex
cdps13 <- read_csv("acs13.csv") %>% 
  select(Geo_NAME, plid) %>%
  mutate(cdp = ifelse(grepl("CDP|cdp", Geo_NAME), 1, 0)) %>%
  filter(cdp==1)
pl0713 %<>% # filter out places in 2007 that are CDPs
  filter(!(plid %in% cdps13$plid) & !is.na(plid))
rm(cdps13)

# start with all blocks in 2007 and identify those that were annexed 
aa <- read_csv("blocks2007_buffers.csv") %>%
  filter(!duplicated(blkid)) %>%
  rename(plid = bufferplace)

aa %<>%
  mutate(annexed = ifelse(plid %in% pl0713$plid, 1, 0))
table(aa$annexed)

blocks2007 <- read_csv("blocks2007_int.csv") 

aa %<>%
  left_join(cw, by = c("blkid" = "GEOID00")) %>%
  filter(!is.na(WEIGHT) & !is.na(PAREA)) %>%
  select(blkid, plid, annexed, GEOID10) %>%
  left_join(blocks2007 %>% select(blkid, pop:nbminvap), by = c("GEOID10" = "blkid")) %>%
  filter(pop > 1)
rm(blocks2007)

#clean up and get ready for Census data 
rac2007 <- read_csv("LODES data/rac_2007.csv")
names(rac2007)

rac2007 %<>%
  left_join(cw, by = c("h_geocode" = "GEOID00")) %>%
  filter(!is.na(WEIGHT) & !is.na(PAREA)) %>%
  mutate(h_geocode = ifelse(h_geocode == GEOID10, h_geocode, GEOID10)) %>%
  mutate_at(vars(njobs07:nhincjobs07), ~(.*WEIGHT))

wac2007 <- read_csv("LODES data/wac_2007.csv")
names(wac2007)

wac2007 %<>%
  left_join(cw, by = c("w_geocode" = "GEOID00")) %>%
  filter(!is.na(WEIGHT) & !is.na(PAREA)) %>%
  mutate(w_geocode = ifelse(w_geocode == GEOID10, w_geocode, GEOID10)) %>%
  mutate_at(vars(jobs:ret), ~(.*WEIGHT))

# check they seem to be in comparable formats
head(aa$blkid)
head(rac2007$h_geocode)
head(wac2007$w_geocode)

aa %<>%
  left_join(rac2007 %>% select(h_geocode:nhincjobs07), by = c("GEOID10" = "h_geocode")) %>%
  mutate_at(c("njobs07", "nhincjobs07"), ~ifelse(is.na(.), 0, .)) %>%
  left_join(wac2007 %>% select(w_geocode:ret), by = c("GEOID10" = "w_geocode")) %>%
  mutate_at(c("jobs", "man", "ret"), ~ifelse(is.na(.), 0, .))
rm(rac2007, wac2007)

# we can't have places that annexed all their blocks, 
# nor places that only had 1 contiguous block
aa %<>%
  group_by(plid) %>%
  mutate(n = n()) %>%
  ungroup() %>%
  filter(n >= 2) %>%
  select(-n)

# merge in vra 
vrastates <- c("01", "02", "04", "13", "22", "28", "45", "48", "51")
vra.df <- read_csv("vra_counties.csv")
vra.df %<>% 
  mutate(countyfips = str_pad(countyfips, 5, side = "left", pad = "0"),
         sectionv = 1)

aa %<>%
  mutate(countyfips = substr(blkid, 1, 5),
         STATEFP = substr(blkid, 1, 2),
         vra = case_when(
           STATEFP %in% vrastates ~ 1,
           countyfips %in% vra.df$countyfips ~ 1,
           TRUE ~ 0
         ))

table(aa$annexed)

# prep for rbind 
aa$period <- "0713"
write_csv(aa, "analyticalfiles/annexedblocks0713dem_bas.csv") 
rm(list = ls())

# 14-20 ####
pl1420 <- read_csv("analyticalfiles/bas_years.csv") %>%
  filter(period == "2014 to 2020")

# want to know which places are CDPs--they do not annex
cdps20 <- read_csv("places2020_cleaned.csv") %>% 
  select(Geo_NAME, plid) %>%
  mutate(cdp = ifelse(grepl("CDP|cdp", Geo_NAME), 1, 0)) %>%
  filter(cdp==1)

# filter out places in 2020 that are CDPs
pl1420 %<>% 
  filter(!(plid %in% cdps20$plid) & !is.na(plid))
rm(cdps20)

# start with all blocks in 2014 and identify those that were annexed 
aa <- read_csv("blocks2014_buffers.csv") %>%
  rename(plid = bufferplace) %>%
  filter(!duplicated(blkid))

aa %<>%
  mutate(annexed = ifelse(plid %in% pl1420$plid, 1, 0))

table(aa$annexed)

blocks2014 <- read_csv("blocks2014_int.csv")

aa %<>%
  left_join(blocks2014 %>% select(blkid, pop:nbminvap), by = "blkid") %>%
  filter(pop > 1)
rm(blocks2014)

#clean up and get ready for Census data 
rac2014 <- read_csv("LODES data/rac_2014.csv")
names(rac2014)
wac2014 <- read_csv("LODES data/wac_2014.csv")
names(wac2014)

# check they seem to be in comparable formats
head(aa$blkid)
head(rac2014$h_geocode)
head(wac2014$w_geocode)

aa %<>%
  left_join(rac2014 %>% select(-Year), by = c("blkid" = "h_geocode")) %>%
  mutate_at(c("njobs14", "nhincjobs14"), ~ifelse(is.na(.), 0, .)) %>%
  left_join(wac2014 %>% select(-Year), by = c("blkid" = "w_geocode")) %>%
  mutate_at(c("jobs", "man", "ret"), ~ifelse(is.na(.), 0, .))
rm(rac2014, wac2014)

# we can't have places that only had 1 contiguous block
aa %<>%
  group_by(plid) %>%
  mutate(n = n()) %>%
  ungroup() %>%
  filter(n >= 2) %>%
  select(-n)

# merge in vra 
vrastates <- c("01", "02", "04", "13", "22", "28", "45", "48", "51")
vra.df <- read_csv("vra_counties.csv")
vra.df %<>% 
  mutate(countyfips = str_pad(countyfips, 5, side = "left", pad = "0"),
         sectionv = 1)

aa %<>%
  mutate(countyfips = paste0(STATEFP10, COUNTYFP10), 
         vra = case_when(
           STATEFP10 %in% vrastates ~ 1,
           countyfips %in% vra.df$countyfips ~ 1,
           TRUE ~ 0
         ))
table(aa$annexed)

# prep for rbind 
aa$period <- "1420"
write_csv(aa, "analyticalfiles/annexedblocks1420dem_bas.csv") 
rm(list = ls())
