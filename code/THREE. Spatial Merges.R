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
blocks2000 <- read_csv("blocks2000_var.csv")

blocks2000 %<>%
  mutate(STATEA = str_pad(STATEA, 2, side = "left", pad = "0"),
         PLACEA = str_pad(PLACEA, 5, side = "left", pad = "0"),
         plid = ifelse(PLACEA == "99999", NA, paste0(STATEA, PLACEA))) %>%
  select(plid, blkid)

cw <- read_csv("cw/2000-to-2010_unique.csv")

# this is code used to generate this cw_use file ####
# cw <- read_csv("cw/2000-to-2010/nhgis_blk2000_blk2010_ge.csv")
# cw %<>%
#   group_by(GEOID00) %>%
#   slice(which.max(PAREA)) %>%
#   ungroup() %>%
#   group_by(GEOID10) %>%
#   slice(which.max(PAREA)) %>%
#   ungroup()
# length(unique(cw$GEOID00))
# length(unique(cw$GEOID10))
# write_csv(cw, "cw/2000-to-2010_unique.csv")

blocks2000 %<>% 
  left_join(cw, by = c("blkid" = "GEOID00")) %>%
  filter(!is.na(WEIGHT) & !is.na(PAREA)) %>%
  mutate(blkid = GEOID10) %>%
  select(-GEOID10) %>%
  filter(!is.na(blkid))

summary(blocks2000$WEIGHT)
summary(blocks2000$PAREA)

# just all the blkids from shapefiles
blocks2007 <- read_csv("blocks2007_blkids.csv") 
length(unique(blocks2007$blkid))
blocks2007 %<>%
  filter(!duplicated(blkid))

# blk-to-place crosswalk 
blocks2007_pl <- read_csv("blocks2007_plids.csv") 
length(unique(blocks2007_pl$blkid))
blocks2007_pl %<>%
  filter(!duplicated(blkid))

blocks2007 %<>%
  left_join(blocks2007_pl %>% 
              select(blkid, plid), by = "blkid")

# want to know which places are CDPs--they do not annex
cdps07 <- read_csv("pl2007_cleaned.csv") %>% 
  select(Geo_NAME, plid) %>%
  mutate(cdp = ifelse(grepl("CDP|cdp", Geo_NAME), 1, 0)) %>%
  filter(cdp==1)
blocks2007 %<>% # filter out places in 2007 that are CDPs
  filter(!(plid %in% cdps07$plid) & !is.na(plid))
rm(cdps07, blocks2007_pl)

blocks2007 %<>%
  left_join(cw, by = c("blkid" = "GEOID00")) %>%
  mutate(blkid = GEOID10) %>%
  select(-GEOID10) %>%
  filter(!is.na(blkid))

# keep only plid = na or cdp places in 2000 (i.e., the annexable blocks)
cdps00 <- read_csv("pl2000_cleaned.csv") %>% # want to know which places are CDPs
  select(Geo_NAME, plid) %>%
  mutate(cdp = ifelse(grepl("CDP|cdp", Geo_NAME), 1, 0)) %>%
  filter(cdp==1)
blocks2000 %<>% # annexable if unincorporated
  filter(is.na(plid) | (plid %in% cdps00$plid))
rm(cdps00)

# find only blocks common to each other 
blkids <- Reduce(intersect, list(unique(blocks2000$blkid), unique(blocks2007$blkid))) 
blocks2000 %<>%
  filter(blkid %in% blkids) %>%
  filter(!duplicated(blkids))
blocks2007 %<>%
  filter(blkid %in% blkids) %>%
  filter(!duplicated(blkid))
rm(blkids)

# annexed if in place in 2007 but not in 2000 
blocks2007 %<>%
  select(blkid, plid) %>%
  rename(plid_annexed = plid) %>%
  filter(blkid %in% blocks2000$blkid) %>%
  left_join(blocks2000, by = "blkid") 

write_csv(blocks2007, "aa_baseline_full_0007.csv")

rm(list = ls())

# start with all contiguous blocks in 2000 and identify those that were annexed 
aa <- read_csv("2000buffers.csv") %>%
  filter(!duplicated(blkid))

cw <- read_csv("cw/2000-to-2010_unique.csv")

aa %<>% 
  left_join(cw, by = c("blkid" = "GEOID00")) %>%
  mutate(blkid = GEOID10) %>%
  select(-GEOID10) %>%
  filter(!is.na(blkid)) %>%
  filter(!is.na(WEIGHT) & !is.na(PAREA)) %>%
  select(STATEFP00:bufferplace) %>%
  rename(plid = bufferplace)

annexed <- read_csv("aa_baseline_full_0007.csv")

aa %<>%
  left_join(annexed %>% select(blkid, plid_annexed), by = "blkid") %>%
  mutate(annexed = ifelse(is.na(plid_annexed), 0, 1),
         plid_use = ifelse(annexed == 1, plid_annexed, plid)) %>%
  select(-plid_annexed, -plid) %>%
  rename(plid = plid_use)

table(aa$annexed)
rm(annexed)

blocks2000 <- read_csv("blocks2000_var_2010b.csv")

blocks2000 %<>%
  mutate(STATEA = str_pad(STATEA, 2, side = "left", pad = "0"),
         PLACEA = str_pad(PLACEA, 5, side = "left", pad = "0"),
         plid = ifelse(PLACEA == "99999", NA, paste0(STATEA, PLACEA))) 

# code for 2010b ####
# blocks2000 %<>% 
#   left_join(cw, by = c("blkid" = "GEOID00")) %>%
#   filter(!is.na(WEIGHT) & !is.na(PAREA)) %>%
#   mutate(blkid = ifelse(blkid == GEOID10, blkid, GEOID10)) %>%
#   mutate_at(vars(pop00b:nbminvap00b), ~(.*WEIGHT)) %>%
#   select(-c(GEOID10, WEIGHT, PAREA)) 

aa %<>%
  left_join(blocks2000 %>% select(blkid:nbminvap00b), by = "blkid") %>%
  filter(pop00b > 1)

rm(blocks2000)

#clean up and get ready for Census data 
rac2000 <- read_csv("LODES data/rac_2000.csv")
names(rac2000)

rac2000 %<>%
  left_join(cw, by = c("h_geocode" = "GEOID00")) %>%
  filter(!is.na(WEIGHT) & !is.na(PAREA)) %>%
  mutate(h_geocode = ifelse(h_geocode == GEOID10, h_geocode, GEOID10)) %>%
  mutate_at(vars(njobs00:nhincjobs00), ~(.*WEIGHT))

wac2000 <- read_csv("LODES data/wac_2000.csv")
names(wac2000)

wac2000 %<>%
  left_join(cw, by = c("w_geocode" = "GEOID00")) %>%
  filter(!is.na(WEIGHT) & !is.na(PAREA)) %>%
  mutate(w_geocode = ifelse(w_geocode == GEOID10, w_geocode, GEOID10)) %>%
  mutate_at(vars(jobs:ret), ~(.*WEIGHT))

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

aa %<>% 
  group_by(plid) %>%
  mutate(n = sum(annexed==1),
         annexing_place = ifelse(n==0, 0, 1)) %>%
  ungroup() %>%
  dplyr::select(-n) 
table(aa$annexing_place)

# we can't have places that annexed all their blocks, 
# nor places that only had 1 contiguous block
aa %<>%
  group_by(plid) %>%
  mutate(n_annexed = sum(annexed==1),
         n = n()) %>%
  ungroup() %>%
  filter(n_annexed < n &
           n >= 2) %>%
  select(-n_annexed, -n)

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
write_csv(aa, "analyticalfiles/annexedblocks0007dem.csv") 
rm(list = ls())

# 0713 ####
blocks2007 <- read_csv("blocks2007_int.csv")
length(unique(blocks2007$blkid))

blocks2007_plids <- read_csv("blocks2007_plids.csv")
length(unique(blocks2007_plids$blkid))

blocks2007_plids %<>%
  filter(!duplicated(blkid)) %>%
  left_join(cw, by = c("blkid" = "GEOID00")) %>%
  filter(!is.na(WEIGHT) & !is.na(PAREA)) %>%
  mutate(blkid = GEOID10) %>%
  select(blkid, plid)

blocks2007 %<>%
  left_join(blocks2007_plids %>% select(blkid, plid), by = "blkid") %>%
  filter(!duplicated(blkid))

rm(blocks2007_plids)

# want to know which places are CDPs
cdps07 <- read_csv("pl2007_cleaned.csv") %>% 
  select(Geo_NAME, plid) %>%
  mutate(cdp = ifelse(grepl("CDP|cdp", Geo_NAME), 1, 0)) %>%
  filter(cdp==1)

# filter out places in 2007 that are CDPs
blocks2007 %<>% 
  filter((plid %in% cdps07$plid) | is.na(plid)) %>%
  select(blkid, plid)

# get 2013 
# blk-to-place crosswalk 
blocks2013_pl <- read_csv("blocks2013_plids.csv") 

# want to know which places are CDPs--they do not annex
cdps13 <- read_csv("acs13.csv") %>% 
  select(Geo_NAME, plid) %>%
  mutate(cdp = ifelse(grepl("CDP|cdp", Geo_NAME), 1, 0)) %>%
  filter(cdp==1)
blocks2013_pl %<>% # filter out places in 2007 that are CDPs
  filter(!(plid %in% cdps13$plid) & !is.na(plid))
rm(cdps13)

length(unique(blocks2013_pl$blkid))
blocks2013_pl %<>%
  filter(!duplicated(blkid))

# find only blocks common to each other because the block had to exist in 2007 and in 2013 to be annexable and annexed 
blkids <- Reduce(intersect, list(unique(blocks2007$blkid), unique(blocks2013_pl$blkid)))  
blocks2007 %<>%
  filter(blkid %in% blkids) %>%
  filter(!duplicated(blkid))
blocks2013_pl %<>%
  filter(blkid %in% blkids) %>%
  filter(!duplicated(blkid))
rm(blkids)

# annexed if in place in 2007 but not in 2000 
blocks2013_pl %<>%
  select(blkid, plid) %>%
  rename(plid_annexed = plid) %>%
  filter(blkid %in% blocks2007$blkid) %>%
  left_join(blocks2007, by = "blkid") 

write_csv(blocks2013_pl, "aa_baseline_full_0713.csv")

rm(list = ls())

# start with all blocks in 2007 and identify those that were annexed 
aa <- read_csv("blocks2007_buffers.csv")
cw <- read_csv("cw/2000-to-2010_unique.csv")

aa %<>% 
  left_join(cw, by = c("blkid" = "GEOID00")) %>%
  filter(!is.na(WEIGHT) & !is.na(PAREA)) %>%
  mutate(blkid = GEOID10) %>%
  select(blkid, bufferplace) %>%
  rename(plid = bufferplace) 

annexed <- read_csv("aa_baseline_full_0713.csv")

aa %<>%
  left_join(annexed %>% select(blkid, plid_annexed), by = "blkid") %>%
  mutate(annexed = ifelse(is.na(plid_annexed), 0, 1),
         plid_use = ifelse(annexed == 1, plid_annexed, plid)) %>%
  select(-plid_annexed, -plid) %>%
  rename(plid = plid_use)

table(aa$annexed)
rm(annexed)

blocks2007 <- read_csv("blocks2007_int.csv")

aa %<>%
  left_join(blocks2007 %>% select(blkid, pop:nbminvap), by = "blkid") %>%
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
  left_join(rac2007 %>% select(h_geocode:nhincjobs07), by = c("blkid" = "h_geocode")) %>%
  mutate_at(c("njobs07", "nhincjobs07"), ~ifelse(is.na(.), 0, .)) %>%
  left_join(wac2007 %>% select(w_geocode:ret), by = c("blkid" = "w_geocode")) %>%
  mutate_at(c("jobs", "man", "ret"), ~ifelse(is.na(.), 0, .))
rm(rac2007, wac2007)

aa %<>% 
  group_by(plid) %>%
  mutate(n = sum(annexed==1),
         annexing_place = ifelse(n==0, 0, 1)) %>%
  ungroup() %>%
  dplyr::select(-n) 
table(aa$annexing_place)

# we can't have places that annexed all their blocks, 
# nor places that only had 1 contiguous block
aa %<>%
  group_by(plid) %>%
  mutate(n_annexed = sum(annexed==1),
         n = n()) %>%
  ungroup() %>%
  filter(n_annexed < n &
           n >= 2) %>%
  select(-n_annexed, -n)

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
write_csv(aa, "analyticalfiles/annexedblocks0713dem.csv") 
rm(list = ls())

# 14-20 ####
blocks2014 <- read_csv("blocks2014_int.csv")
blocks2014_plids <- read_csv("blocks2014_plids.csv")

length(unique(blocks2014$blkid))
length(unique(blocks2014_plids$blkid))

blocks2014 %<>%
  left_join(blocks2014_plids, by = "blkid")
rm(blocks2014_plids)

blocks2014 %<>%
  select(plid, blkid)

# want to know which places are CDPs
cdps14 <- read_csv("places2014_cleaned.csv") %>% 
  select(Geo_NAME, plid) %>%
  mutate(cdp = ifelse(grepl("CDP|cdp", Geo_NAME), 1, 0)) %>%
  filter(cdp==1)

# filter out places in 2014 that are CDPs/na
blocks2014 %<>% 
  filter((plid %in% cdps14$plid) | is.na(plid))
rm(cdps14)

cw <- read_csv("cw/2020-to-2010_unique.csv")

# code to produce cw_use ####
# cw <- read_csv("cw/2020-to-2010/nhgis_blk2020_blk2010_ge.csv") %>%
#   filter(PAREA > 0) %>%
#   group_by(GEOID20) %>%
#   slice(which.max(PAREA)) %>%
#   ungroup() %>%
#   group_by(GEOID10) %>%
#   slice(which.max(PAREA)) %>%
# ungroup()
# 
# length(unique(cw$GEOID20))
# length(unique(cw$GEOID10)) 
# 
# write_csv(cw, "cw/2020-to-2010_unique.csv")

# get 2020
# blk-to-place crosswalk 
blocks2020_pl <- read_csv("blocks2020_var.csv") 
blocks2020_pl %<>% 
  mutate(STATEA = str_pad(STATEA, 2, side = "left", pad = "0"),
         PLACEA = str_pad(PLACEA, 5, side = "left", pad = "0"),
         plid = ifelse(PLACEA=="99999", NA, paste0(STATEA, PLACEA)),
         blkid = paste0(str_pad(as.character(STATEA), 2, side = "left", pad = "0"), 
                        str_pad(as.character(COUNTYA), 3, side = "left", pad = "0"),
                        str_pad(as.character(TRACTA), 6, side = "left", pad = "0"), 
                        str_pad(as.character(BLOCKA), 4, side = "left", pad = "0"))) 

# want to know which places are CDPs--they do not annex
cdps20 <- read_csv("places2020_cleaned.csv") %>% 
  select(Geo_NAME, plid) %>%
  mutate(cdp = ifelse(grepl("CDP|cdp", Geo_NAME), 1, 0)) %>%
  filter(cdp==1)

# filter out places in 2020 that are CDPs
blocks2020_pl %<>% 
  filter(!(plid %in% cdps20$plid) & !is.na(plid))
rm(cdps20)

blocks2020_pl %<>% 
  left_join(cw, by = c("blkid" = "GEOID20")) %>%
  filter(!is.na(WEIGHT) & !is.na(PAREA)) %>%
  mutate(blkid = GEOID10) %>%
  select(blkid, plid)

length(unique(blocks2020_pl$blkid))
length(unique(blocks2014$blkid))

# find only blocks common to each other 
blkids <- Reduce(intersect, list(unique(blocks2014$blkid), unique(blocks2020_pl$blkid))) 

blocks2014 %<>%
  filter(blkid %in% blkids) 
blocks2020_pl %<>%
  filter(blkid %in% blkids) 
rm(blkids, cw)

# annexed if in place in 2020 but not in 2014
blocks2020_pl %<>%
  select(blkid, plid) %>%
  rename(plid_annexed = plid) %>%
  filter(blkid %in% blocks2014$blkid) %>%
  left_join(blocks2014, by = "blkid") 

write_csv(blocks2020_pl, "aa_baseline_full_1420.csv")

rm(list = ls())

# start with all blocks in 2014 and identify those that were annexed 
aa <- read_csv("blocks2014_buffers.csv")

aa %<>%
  rename(plid = bufferplace) %>%
  filter(!duplicated(blkid))

annexed <- read_csv("aa_baseline_full_1420.csv")

aa %<>%
  left_join(annexed %>% select(blkid, plid_annexed), by = "blkid") %>%
  mutate(annexed = ifelse(is.na(plid_annexed), 0, 1),
         plid_use = ifelse(annexed == 1, plid_annexed, plid)) %>%
  select(-plid_annexed, -plid) %>%
  rename(plid = plid_use) %>%
  filter(!is.na(plid))

table(aa$annexed)
rm(annexed)

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

aa %<>% 
  group_by(plid) %>%
  mutate(n = sum(annexed==1),
         annexing_place = ifelse(n==0, 0, 1)) %>%
  ungroup() %>%
  dplyr::select(-n) 
table(aa$annexing_place)

# we can't have places that annexed all their blocks, 
# nor places that only had 1 contiguous block
aa %<>%
  group_by(plid) %>%
  mutate(n_annexed = sum(annexed==1),
         n = n()) %>%
  ungroup() %>%
  filter(n_annexed < n &
           n >= 2) %>%
  select(-n_annexed, -n)

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
write_csv(aa, "analyticalfiles/annexedblocks1420dem.csv") 
rm(list = ls())
