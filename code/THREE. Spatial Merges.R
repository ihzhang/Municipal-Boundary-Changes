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
library("sf")

# 00-07 ####
# clean: 
# 1. for contiguous blocks, keep only eligible blocks 
# 2. for annexed blocks, keep only eligible blocks 
# we just need the blockids of blocks that are annexed
state_codes <- c("AL_01", "AS_02", "AR_05", "AZ_04", "CA_06", "CO_08", 
                 "DE_10", "FL_12", "GA_13", "HI_15", "IA_19", "ID_16", "IL_17", "IN_18",
                 "KS_20", "KY_21", "LA_22", "MD_24",
                 "MI_26", "MN_27", "MS_28", "MO_29", "MT_30", 
                 "NC_37", "ND_38", "NE_31", "NM_35", "NV_32", 
                 "OH_39", "OK_40", "OR_41", 
                 "SC_45", "SD_46", "TN_47", "TX_48", "UT_49", "VA_51",
                 "WA_53", "WV_54", "WI_55", "WY_56"
)

# archived: using 90pct only
# blocks_list <- list()
# for(i in 1:length(state_codes)) {
#   blocks_list[[i]] <- read_csv(paste0("spatial_files_oct22/2000/", state_codes[[i]], "_block_plids_2000blk-2000pl.csv"))
# }
# blocks_list <- rbindlist(blocks_list)
# write_csv(blocks_list, "2000blk-2000plid.csv")

blocks_list <- list()
for(i in 1:length(state_codes)) {
  blocks_list[[i]] <- read_csv(paste0("spatial_files_oct22/2000/", state_codes[[i]], "_block_plids_2000blk-2000pl_90pct.csv"))
}
blocks_list <- rbindlist(blocks_list)
write_csv(blocks_list, "2000blk-2000plid_90pct.csv")
rm(blocks_list)

blocks_list <- list()
for(i in 1:length(state_codes)) {
  blocks_list[[i]] <- read_csv(paste0("spatial_files_oct22/2000/", state_codes[[i]], "_block_plids_2000blk-2007pl_90pct.csv"))
}
blocks_list <- rbindlist(blocks_list)
write_csv(blocks_list, "2000blk-2007plid_90pct.csv")
rm(blocks_list)

blocks2000 <- read_csv("blocks2000_var.csv")
blocks2000 %<>%
  mutate(plid = ifelse(PLACEA=="99999", NA, 
                       paste0(str_pad(STATEA, 2, "left", "0"), 
                              str_pad(PLACEA, 5, "left", "0")))) %>%
  select(plid, blkid)
blocks2000 %<>%
  filter(!duplicated(blkid)) 

#cw <- read_csv("cw/2000-to-2010_unique.csv")

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

# blocks2000 %<>% 
#   left_join(cw, by = c("blkid" = "GEOID00")) %>%
#   filter(!is.na(WEIGHT) & !is.na(PAREA)) %>%
#   mutate(blkid = GEOID10) %>%
#   select(-GEOID10) %>%
#   filter(!is.na(blkid))

# summary(blocks2000$WEIGHT)
# summary(blocks2000$PAREA)

# just all the blkids from shapefiles
blocks2007 <- read_csv("2000blk-2007plid_90pct.csv") 
length(unique(blocks2007$blkid))
blocks2007 %<>%
  filter(!duplicated(blkid))

# blk-to-place crosswalk 
# blocks2007_pl <- read_csv("blocks2007_plids.csv") 
# length(unique(blocks2007_pl$blkid))
# blocks2007_pl %<>%
#   filter(!duplicated(blkid))
# 
# blocks2007 %<>%
#   left_join(blocks2007_pl %>% 
#               select(blkid, plid), by = "blkid")

# want to know which places are CDPs--they do not annex
cdps07 <- read_csv("pl2007_cleaned.csv") %>% 
  select(Geo_NAME, plid) %>%
  mutate(cdp = ifelse(grepl("CDP|cdp", Geo_NAME), 1, 0)) %>%
  filter(cdp==1)
blocks2007 %<>% # filter out places in 2007 that are CDPs
  filter(!(plid %in% cdps07$plid) & !is.na(plid))
rm(cdps07)

# blocks2007 %<>%
#   left_join(cw, by = c("blkid" = "GEOID00")) %>%
#   mutate(blkid = GEOID10) %>%
#   select(-GEOID10) %>%
#   filter(!is.na(blkid))

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
  #left_join(cw, by = c("blkid" = "GEOID00")) %>%
  #mutate(blkid = GEOID10) %>%
  #select(-GEOID10) %>%
  filter(!is.na(blkid)) %>%
  #filter(!is.na(WEIGHT) & !is.na(PAREA)) %>%
  select(blkid:bufferplace) %>%
  rename(plid = bufferplace)

annexed <- read_csv("aa_baseline_full_0007.csv") %>%
  filter(!duplicated(blkid)) 
table(annexed$blkid %in% aa$blkid) #28925 false 
755991+28925
#784916
aa %<>%
  full_join(annexed %>% select(blkid, plid_annexed), by = "blkid") %>%
  mutate(annexed = ifelse(is.na(plid_annexed), 0, 1),
         plid_use = ifelse(annexed == 1, plid_annexed, plid)) %>%
  select(-plid_annexed, -plid) %>%
  rename(plid = plid_use)

table(aa$annexed)
rm(annexed)

blocks2000 <- read_csv("blocks2000_var.csv")

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
# 
# aa %<>%
#   left_join(cw, by = c("blkid" = "GEOID00")) %>%
#   filter(!is.na(WEIGHT) & !is.na(PAREA)) %>%
#   mutate(blkid = GEOID10) %>%
#   filter(blkid %in% blocks2000$blkid) %>%
#   left_join(blocks2000 %>% select(blkid:nbminvap00b), by = "blkid") 
# 
# rm(blocks2000)

# 2000 b ----
aa %<>%
  filter(blkid %in% blocks2000$blkid) %>%
  left_join(blocks2000 %>% select(blkid:nbminvap00b), by = "blkid")

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

# rac2000 %<>%
#   left_join(cw, by = c("h_geocode" = "GEOID00")) %>%
#   filter(!is.na(WEIGHT) & !is.na(PAREA)) %>%
#   mutate(h_geocode = ifelse(h_geocode == GEOID10, h_geocode, GEOID10)) %>%
#   mutate_at(vars(njobs00:nhincjobs00), ~(.*WEIGHT))
# 
# wac2000 %<>%
#   left_join(cw, by = c("w_geocode" = "GEOID00")) %>%
#   filter(!is.na(WEIGHT) & !is.na(PAREA)) %>%
#   mutate(w_geocode = ifelse(w_geocode == GEOID10, w_geocode, GEOID10)) %>%
#   mutate_at(vars(jobs:ret), ~(.*WEIGHT))

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

# we can't have places that had no populated contiguous block
table(is.na(aa$pop00b))

aa %<>%
  group_by(plid) %>%
  mutate(npop = sum(pop00b > 0)) %>%
  filter(npop > 0) %>%
  select(-npop) %>%
  ungroup()

# merge in vra 
vrastates <- c("01", "02", "04", "13", "22", "28", "45", "48", "51")
vra.df <- read_csv("vra_counties.csv")
vra.df %<>% 
  mutate(countyfips = str_pad(countyfips, 5, side = "left", pad = "0"),
         sectionv = 1)

aa %<>%
  mutate(countyfips = substr(blkid, 1, 5),
         state = substr(blkid, 1, 2),
         vra = case_when(
    state %in% vrastates ~ 1,
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
state_codes <- c("AL_01", "AS_02", "AR_05", "AZ_04", "CA_06", "CO_08", 
                 "DE_10", "FL_12", "GA_13", "HI_15", "IA_19", "ID_16", "IL_17", "IN_18",
                 "KS_20", "KY_21", "LA_22", "MD_24",
                 "MI_26", "MN_27", "MS_28", "MO_29", "MT_30", 
                 "NC_37", "ND_38", "NE_31", "NM_35", "NV_32", 
                 "OH_39", "OK_40", "OR_41", 
                 "SC_45", "SD_46", "TN_47", "TX_48", "UT_49", "VA_51",
                 "WA_53", "WV_54", "WI_55", "WY_56"
)

# 2007 place boundary is named differently
blocks_list <- list()
for(i in 1:length(state_codes)) {
  blocks_list[[i]] <- read_csv(paste0("spatial_files_oct22/2007/", state_codes[[i]], "_block_plids_2007blk-2007pl_90pct.csv"))
}
blocks_list <- rbindlist(blocks_list, fill = T)
write_csv(blocks_list, "2007blk-2007plid_90pct.csv")

blocks_list <- list()
for(i in 1:length(state_codes)) {
  blocks_list[[i]] <- read_csv(paste0("spatial_files_oct22/2007/", state_codes[[i]], "_block_plids_2007blk-2013pl_90pct.csv"))
}
blocks_list <- rbindlist(blocks_list, fill = T)
write_csv(blocks_list, "2007blk-2013plid_90pct.csv")

rm(blocks_list)

# get just all 2007 block IDs
blocks_list <- list()
for(i in 1:length(state_codes)) {
  blocks_list[[i]] <- st_read(paste0("SHP_blk_0010/2007/states/", state_codes[[i]], "_allblocks.shp")) %>%
    as.data.frame() %>%
    mutate(blkid = paste0(str_pad(as.character(STATEFP00), 2, side = "left", pad = "0"), str_pad(as.character(COUNTYFP00), 3, side = "left", pad = "0"),
                          str_pad(as.character(TRACTCE00), 6, side = "left", pad = "0"), str_pad(as.character(BLOCKCE00), 4, side = "left", pad = "0"))) %>%
    select(blkid)
}

blocks_list <- rbindlist(blocks_list, fill = T)
write_csv(blocks_list, "all2007blocks.csv")
rm(blocks_list)

# annex 
blocks2007 <- read_csv("2007blk-2007plid_90pct.csv") %>%
  filter(!duplicated(blkid))
length(unique(blocks2007$blkid))

blocks2007_na <- read_csv("all2007blocks.csv")
blocks2007_na %<>%
  filter(!duplicated(blkid)) 
length(unique(blocks2007_na$blkid))
table(blocks2007$blkid %in% blocks2007_na$blkid) 

table(is.na(blocks2007$plid))
blocks2007_na %<>%
  filter(!blkid %in% blocks2007$blkid) %>%
  mutate(plid = NA) %>%
  select(blkid, plid)

blocks2007 <- base::rbind(blocks2007 %>% select(blkid, plid), blocks2007_na)
rm(blocks2007_na)

# want to know which places are CDPs
cdps07 <- read_csv("pl2007_cleaned.csv") %>% 
  select(Geo_NAME, plid) %>%
  mutate(cdp = ifelse(grepl("CDP|cdp", Geo_NAME), 1, 0)) %>%
  filter(cdp==1)

# only retain actually annexable blocks
blocks2007 %<>% 
  filter((plid %in% cdps07$plid) | is.na(plid)) %>%
  select(blkid, plid)

# get 2013 
# blk-to-place crosswalk 
blocks2013 <- read_csv("2007blk-2013plid_90pct.csv") 

# want to know which places are CDPs--they do not annex
cdps13 <- read_csv("acs13.csv") %>% 
  select(Geo_NAME, plid) %>%
  mutate(cdp = ifelse(grepl("CDP|cdp", Geo_NAME), 1, 0)) %>%
  filter(cdp==1)
blocks2013 %<>% # filter out places in 2007 that are CDPs
  filter(!(plid %in% cdps13$plid) & !is.na(plid))
rm(cdps13)

length(unique(blocks2013$blkid))

# find only blocks common to each other because the block had to exist in 2007 and in 2013 to be annexable and annexed 
blkids <- Reduce(intersect, list(unique(blocks2007$blkid), unique(blocks2013$blkid)))  
blocks2007 %<>%
  filter(blkid %in% blkids) %>%
  filter(!duplicated(blkid))
blocks2013 %<>%
  filter(blkid %in% blkids) %>%
  filter(!duplicated(blkid))
rm(blkids)

# annexed if in place in 2007 but not in 2000 
blocks2013 %<>%
  select(blkid, plid) %>%
  rename(plid_annexed = plid) %>%
  filter(blkid %in% blocks2007$blkid) %>%
  left_join(blocks2007, by = "blkid") 

write_csv(blocks2013, "aa_baseline_full_0713.csv")

rm(list = ls())

# start with all blocks in 2007 and identify those that were annexed 
aa <- read_csv("2007buffers.csv") %>%
  filter(!duplicated(blkid))
cw <- read_csv("cw/2000-to-2010_unique.csv")

aa %<>% 
  select(blkid, bufferplace) %>%
  rename(plid = bufferplace) 

annexed <- read_csv("aa_baseline_full_0713.csv") #33954
table(annexed$blkid %in% aa$blkid)
16643 + 594978

aa %<>%
  full_join(annexed %>% select(blkid, plid_annexed), by = "blkid") %>%
  mutate(annexed = ifelse(is.na(plid_annexed), 0, 1),
         plid_use = ifelse(annexed == 1, plid_annexed, plid)) %>%
  select(-plid_annexed, -plid) %>%
  rename(plid = plid_use)

table(aa$annexed)
rm(annexed)

blocks2007 <- read_csv("blocks2007_int.csv") 

aa %<>%
  left_join(cw, by = c("blkid" = "GEOID00")) %>%
  filter(!is.na(WEIGHT) & !is.na(PAREA)) %>%
  select(-blkid) %>%
  rename(blkid = GEOID10) %>%
  select(plid, annexed, blkid)

table(aa$blkid %in% blocks2007$blkid)

aa %<>%
  filter(blkid %in% blocks2007$blkid) %>%
  left_join(blocks2007 %>% select(blkid, pop:nbminvap), by = c("blkid")) 
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

# we can't have places that had no populated annexable blocks 
aa %<>%
  group_by(plid) %>%
  mutate(npop = sum(pop > 0)) %>%
  ungroup() %>%
  filter(npop > 0) %>%
  select(-npop)

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
state_codes <- c("AL_01", "AS_02", "AR_05", "AZ_04", "CA_06", "CO_08", 
                 "DE_10", "FL_12", "GA_13", "HI_15", "IA_19", "ID_16", "IL_17", "IN_18",
                 "KS_20", "KY_21", "LA_22", "MD_24",
                 "MI_26", "MN_27", "MS_28", "MO_29", "MT_30", 
                 "NC_37", "ND_38", "NE_31", "NM_35", "NV_32", 
                 "OH_39", "OK_40", "OR_41", 
                 "SC_45", "SD_46", "TN_47", "TX_48", "UT_49", "VA_51",
                 "WA_53", "WV_54", "WI_55", "WY_56"
)

blocks_list <- list()
for(i in 1:length(state_codes)) {
  blocks_list[[i]] <- read_csv(paste0("spatial_files_oct22/2014/", state_codes[[i]], "_block_plids_2014blk-2014pl_90pct.csv"))
}
blocks_list <- rbindlist(blocks_list, fill = T)
write_csv(blocks_list, "2014blk-2014plid_90pct.csv")

blocks_list <- list()
for(i in 1:length(state_codes)) {
  blocks_list[[i]] <- read_csv(paste0("spatial_files_oct22/2014/", state_codes[[i]], "_block_plids_2014blk-2020pl_90pct.csv"))
}
blocks_list <- rbindlist(blocks_list, fill = T)
write_csv(blocks_list, "2014blk-2020plid_90pct.csv")

rm(blocks_list)

# annex 
# get 2014 IDs 
blocks_list <- list()
for(i in 1:length(state_codes)) {
  blocks_list[[i]] <- st_read(paste0("SHP_blk_0010/2014/", state_codes[[i]], "/tl_2014_", substr(state_codes[[i]], 4, 5), "_tabblock10.shp")) %>%
    as.data.frame() %>%
    mutate(blkid = paste0(str_pad(as.character(STATEFP10), 2, side = "left", pad = "0"), str_pad(as.character(COUNTYFP10), 3, side = "left", pad = "0"),
                          str_pad(as.character(TRACTCE10), 6, side = "left", pad = "0"), str_pad(as.character(BLOCKCE10), 4, side = "left", pad = "0"))) %>%
    select(blkid)
}

blocks_list <- rbindlist(blocks_list, fill = T)
write_csv(blocks_list, "all2014blocks.csv")
rm(blocks_list)

# annex 
blocks2014 <- read_csv("2014blk-2014plid_90pct.csv") %>%
  filter(!duplicated(blkid)) %>%
  select(plid, blkid)

blocks2014_na <- read_csv("all2014blocks.csv") %>%
  filter(!duplicated(blkid)) %>%
  filter(!blkid %in% blocks2014$blkid) %>%
  mutate(plid = NA) %>%
  select(plid, blkid)

# universe of annexable
blocks2014 <- base::rbind(blocks2014 %>% select(blkid, plid), blocks2014_na)
rm(blocks2014_na)

# want to know which places are CDPs
cdps14 <- read_csv("places2014_cleaned.csv") %>% 
  select(Geo_NAME, plid) %>%
  mutate(cdp = ifelse(grepl("CDP|cdp", Geo_NAME), 1, 0)) %>%
  filter(cdp==1)

# filter out places in 2014 that are CDPs/na
blocks2014 %<>% 
  filter((plid %in% cdps14$plid) | is.na(plid))
rm(cdps14)

#cw <- read_csv("cw/2020-to-2010_unique.csv")

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
# blocks2020_pl <- read_csv("blocks2020_var.csv") 
# blocks2020_pl %<>% 
#   mutate(STATEA = str_pad(STATEA, 2, side = "left", pad = "0"),
#          PLACEA = str_pad(PLACEA, 5, side = "left", pad = "0"),
#          plid = ifelse(PLACEA=="99999", NA, paste0(STATEA, PLACEA)),
#          blkid = paste0(str_pad(as.character(STATEA), 2, side = "left", pad = "0"), 
#                         str_pad(as.character(COUNTYA), 3, side = "left", pad = "0"),
#                         str_pad(as.character(TRACTA), 6, side = "left", pad = "0"), 
#                         str_pad(as.character(BLOCKA), 4, side = "left", pad = "0"))) 

blocks2020 <- read_csv("2014blk-2020plid_90pct.csv") %>%
  filter(!duplicated(blkid))

# want to know which places are CDPs--they do not annex
cdps20 <- read_csv("places2020_cleaned.csv") %>% 
  select(Geo_NAME, plid) %>%
  mutate(cdp = ifelse(grepl("CDP|cdp", Geo_NAME), 1, 0)) %>%
  filter(cdp==1)

# filter out places in 2020 that are CDPs
blocks2020 %<>% 
  filter(!(plid %in% cdps20$plid) & !is.na(plid))
rm(cdps20)

blocks2020 %<>% 
  select(blkid, plid)

length(unique(blocks2020$blkid))
length(unique(blocks2014$blkid))

# find only blocks common to each other 
blkids <- Reduce(intersect, list(unique(blocks2014$blkid), unique(blocks2020$blkid))) 

blocks2014 %<>%
  filter(blkid %in% blkids) 
blocks2020 %<>%
  filter(blkid %in% blkids) 
rm(blkids, cw)

# annexed if in place in 2020 but not in 2014
blocks2020 %<>%
  select(blkid, plid) %>%
  rename(plid_annexed = plid) %>%
  filter(blkid %in% blocks2014$blkid) %>%
  left_join(blocks2014, by = "blkid") 

write_csv(blocks2020, "aa_baseline_full_1420.csv")

rm(list = ls())

# start with all blocks in 2014 and identify those that were annexed 
aa <- read_csv("blocks2014_buffers.csv")

aa %<>%
  rename(plid = bufferplace) %>%
  filter(!duplicated(blkid))

annexed <- read_csv("aa_baseline_full_1420.csv") #53869
table(annexed$blkid %in% aa$blkid)
1963777 + 58381

aa %<>%
  full_join(annexed %>% select(blkid, plid_annexed), by = "blkid") %>%
  mutate(annexed = ifelse(is.na(plid_annexed), 0, 1),
         plid_use = ifelse(annexed == 1, plid_annexed, plid)) %>%
  select(-plid_annexed, -plid) %>%
  rename(plid = plid_use) %>%
  filter(!is.na(plid))

table(aa$annexed)
rm(annexed)

blocks2014 <- read_csv("blocks2014_int.csv")
table(aa$blkid %in% blocks2014$blkid)

aa %<>%
  filter(blkid %in% blocks2014$blkid) %>%
  left_join(blocks2014 %>% select(blkid, pop:nbminvap), by = "blkid")
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

# we can't have places that only had 1 contiguous block
aa %<>%
  group_by(plid) %>%
  mutate(npop = sum(pop > 0)) %>%
  ungroup() %>%
  filter(npop > 0) %>%
  select(-npop)

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

# 2007-2008 ----
state_codes <- c("AL_01", "AS_02", "AR_05", "AZ_04", "CA_06", "CO_08", 
                 "DE_10", "FL_12", "GA_13", "HI_15", "IA_19", "ID_16", "IL_17", "IN_18",
                 "KS_20", "KY_21", "LA_22", "MD_24",
                 "MI_26", "MN_27", "MS_28", "MO_29", "MT_30", 
                 "NC_37", "ND_38", "NE_31", "NM_35", "NV_32", 
                 "OH_39", "OK_40", "OR_41", 
                 "SC_45", "SD_46", "TN_47", "TX_48", "UT_49", "VA_51",
                 "WA_53", "WV_54", "WI_55", "WY_56"
)

# the 2007blk-2007pl_90pct.csv file exists already 
blocks_list <- list()
for(i in 1:length(state_codes)) {
  blocks_list[[i]] <- read_csv(paste0("spatial_files/2007/", state_codes[[i]], "_block_plids_2007blk-2008pl_90pct.csv"))
}
blocks_list <- rbindlist(blocks_list, fill = T)
write_csv(blocks_list, "2007blk-2008plid_90pct.csv")

rm(blocks_list)

# annex 
# get 2008 IDs 
blocks_list <- list()
for(i in 1:length(state_codes)) {
  blocks_list[[i]] <- st_read(paste0("SHP_blk_0010/2008/tl_2008_", substr(state_codes[[i]], 4, 5), "_tabblock.shp")) %>%
    as.data.frame() %>%
    mutate(blkid = paste0(str_pad(as.character(STATEFP00), 2, side = "left", pad = "0"), str_pad(as.character(COUNTYFP00), 3, side = "left", pad = "0"),
                          str_pad(as.character(TRACTCE00), 6, side = "left", pad = "0"), str_pad(as.character(BLOCKCE00), 4, side = "left", pad = "0"))) %>%
    select(blkid)
}

blocks_list <- rbindlist(blocks_list, fill = T)
write_csv(blocks_list, "all2008blocks.csv")
rm(blocks_list)

# annex 
blocks2007 <- read_csv("2007blk-2007plid_90pct.csv") %>%
  filter(!duplicated(blkid)) %>%
  select(plid, blkid)

blocks2007_na <- read_csv("all2007blocks.csv") %>%
  filter(!duplicated(blkid)) %>%
  filter(!blkid %in% blocks2007$blkid) %>%
  mutate(plid = NA) %>%
  select(plid, blkid)

# universe of annexable
blocks2007 <- base::rbind(blocks2007 %>% select(blkid, plid), blocks2007_na)
rm(blocks2007_na)

# want to know which places are CDPs
cdps07 <- read_csv("pl2007_cleaned.csv") %>% 
  select(Geo_NAME, plid) %>%
  mutate(cdp = ifelse(grepl("CDP|cdp", Geo_NAME), 1, 0)) %>%
  filter(cdp==1)

# filter out places in 2007 that are CDPs/na
blocks2007 %<>% 
  filter((plid %in% cdps07$plid) | is.na(plid))
rm(cdps07)

blocks2008 <- read_csv("2007blk-2008plid_90pct.csv") %>%
  filter(!duplicated(blkid))

# want to know which places are CDPs--they do not annex
cdps08 <- read_csv("pl2008_cleaned.csv") %>% 
  select(Geo_NAME, plid) %>%
  mutate(cdp = ifelse(grepl("CDP|cdp", Geo_NAME), 1, 0)) %>%
  filter(cdp==1)

# filter out places in 2008 that are CDPs
blocks2008 %<>% 
  filter(!(plid %in% cdps08$plid) & !is.na(plid))
rm(cdps08)

blocks2008 %<>% 
  select(blkid, plid)

length(unique(blocks2008$blkid))
length(unique(blocks2007$blkid))

# find only blocks common to each other 
blkids <- Reduce(intersect, list(unique(blocks2008$blkid), unique(blocks2007$blkid))) 

blocks2007 %<>%
  filter(blkid %in% blkids) 
blocks2008 %<>%
  filter(blkid %in% blkids) 
rm(blkids, cw)

# annexed if in place in 2020 but not in 2014
blocks2008 %<>%
  select(blkid, plid) %>%
  rename(plid_annexed = plid) %>%
  filter(blkid %in% blocks2007$blkid) %>%
  left_join(blocks2007, by = "blkid") 

write_csv(blocks2008, "aa_baseline_full_0708.csv")

rm(list = ls())

# start with all blocks in 2007 and identify those that were annexed 
aa <- read_csv("blocks2007_buffers.csv")

aa %<>%
  rename(plid = bufferplace) %>%
  filter(!duplicated(blkid))

annexed <- read_csv("aa_baseline_full_0708.csv") #53869
table(annexed$blkid %in% aa$blkid)
30011 + 5441

aa %<>%
  full_join(annexed %>% select(blkid, plid_annexed), by = "blkid") %>%
  mutate(annexed = ifelse(is.na(plid_annexed), 0, 1),
         plid_use = ifelse(annexed == 1, plid_annexed, plid)) %>%
  select(-plid_annexed, -plid) %>%
  rename(plid = plid_use) %>%
  filter(!is.na(plid))

table(aa$annexed)
rm(annexed)

blocks2007 <- read_csv("blocks2007_int.csv")
table(aa$blkid %in% blocks2007$blkid)

aa %<>%
  filter(blkid %in% blocks2007$blkid) %>%
  left_join(blocks2007 %>% select(blkid, pop), by = "blkid")
rm(blocks2007)

aa %<>% 
  group_by(plid) %>%
  mutate(n = sum(annexed==1),
         annexing_place = ifelse(n==0, 0, 1)) %>%
  ungroup() %>%
  dplyr::select(-n) 
table(aa$annexing_place)

# we can't have places that only had 1 contiguous block
aa %<>%
  group_by(plid) %>%
  mutate(npop = sum(pop > 0)) %>%
  ungroup() %>%
  filter(npop > 0) %>%
  select(-npop)

# merge in vra 
vrastates <- c("01", "02", "04", "13", "22", "28", "45", "48", "51")
vra.df <- read_csv("vra_counties.csv")
vra.df %<>% 
  mutate(countyfips = str_pad(countyfips, 5, side = "left", pad = "0"),
         sectionv = 1)

aa %<>%
  mutate(countyfips = paste0(STATEFP, COUNTYFP), 
         vra = case_when(
           STATEFP %in% vrastates ~ 1,
           countyfips %in% vra.df$countyfips ~ 1,
           TRUE ~ 0
         ))
table(aa$annexed)

# prep for rbind 
aa$period <- "0708"
write_csv(aa, "analyticalfiles/annexedblocks0708dem.csv") 
rm(list = ls())

# 2008-2009 ----
state_codes <- c("AL_01", "AS_02", "AR_05", "AZ_04", "CA_06", "CO_08", 
                 "DE_10", "FL_12", "GA_13", "HI_15", "IA_19", "ID_16", "IL_17", "IN_18",
                 "KS_20", "KY_21", "LA_22", "MD_24",
                 "MI_26", "MN_27", "MS_28", "MO_29", "MT_30", 
                 "NC_37", "ND_38", "NE_31", "NM_35", "NV_32", 
                 "OH_39", "OK_40", "OR_41", 
                 "SC_45", "SD_46", "TN_47", "TX_48", "UT_49", "VA_51",
                 "WA_53", "WV_54", "WI_55", "WY_56"
)

# 2008-2008
blocks_list <- list()
for(i in 1:length(state_codes)) {
  blocks_list[[i]] <- read_csv(paste0("spatial_files/2008/", state_codes[[i]], "_block_plids_2008blk-2008pl_90pct.csv"))
}
blocks_list <- rbindlist(blocks_list, fill = T)
write_csv(blocks_list, "2008blk-2008plid_90pct.csv")

rm(blocks_list)

blocks_list <- list()
for(i in 1:length(state_codes)) {
  blocks_list[[i]] <- read_csv(paste0("spatial_files/2008/", state_codes[[i]], "_block_plids_2008blk-2009pl_90pct.csv"))
}
blocks_list <- rbindlist(blocks_list, fill = T)
write_csv(blocks_list, "2008blk-2009plid_90pct.csv")

rm(blocks_list)

# annex 
# get 2009 IDs 
blocks_list <- list()
for(i in 1:length(state_codes)) {
  blocks_list[[i]] <- st_read(paste0("SHP_blk_0010/2009/tl_2009_", substr(state_codes[[i]], 4, 5), "_tabblock.shp")) %>%
    as.data.frame() %>%
    mutate(blkid = paste0(str_pad(as.character(STATEFP00), 2, side = "left", pad = "0"), str_pad(as.character(COUNTYFP00), 3, side = "left", pad = "0"),
                          str_pad(as.character(TRACTCE00), 6, side = "left", pad = "0"), str_pad(as.character(BLOCKCE00), 4, side = "left", pad = "0"))) %>%
    select(blkid)
}

blocks_list <- rbindlist(blocks_list, fill = T)
write_csv(blocks_list, "all2009blocks.csv")
rm(blocks_list)

# annex 
blocks2008 <- read_csv("2008blk-2008plid_90pct.csv") %>%
  filter(!duplicated(blkid)) %>%
  select(plid, blkid)

blocks2008_na <- read_csv("all2008blocks.csv") %>%
  filter(!duplicated(blkid)) %>%
  filter(!blkid %in% blocks2008$blkid) %>%
  mutate(plid = NA) %>%
  select(plid, blkid)

# universe of annexable
blocks2008 <- base::rbind(blocks2008 %>% select(blkid, plid), blocks2008_na)
rm(blocks2008_na)

# want to know which places are CDPs
cdps08 <- read_csv("plids/pl2008.csv") %>% 
  select(Geo_NAME, plid) %>%
  mutate(cdp = ifelse(grepl("CDP|cdp", Geo_NAME), 1, 0)) %>%
  filter(cdp==1)

# filter out places in 2008 that are CDPs/na
blocks2008 %<>% 
  filter((plid %in% cdps08$plid) | is.na(plid))
rm(cdps08)

blocks2009 <- read_csv("2008blk-2009plid_90pct.csv") %>%
  filter(!duplicated(blkid))

# want to know which places are CDPs--they do not annex
cdps09 <- read_csv("plids/pl2009.csv") %>% 
  select(Geo_NAME, plid) %>%
  mutate(cdp = ifelse(grepl("CDP|cdp", Geo_NAME), 1, 0)) %>%
  filter(cdp==1)

# filter out places in 2009 that are CDPs
blocks2009 %<>% 
  filter(!(plid %in% cdps09$plid) & !is.na(plid))
rm(cdps09)

blocks2009 %<>% 
  select(blkid, plid)

length(unique(blocks2008$blkid))
length(unique(blocks2009$blkid))

# find only blocks common to each other 
blkids <- Reduce(intersect, list(unique(blocks2008$blkid), unique(blocks2009$blkid))) 

blocks2008 %<>%
  filter(blkid %in% blkids) 
blocks2009 %<>%
  filter(blkid %in% blkids) 
rm(blkids)

# annexed if in place in 2020 but not in 2014
blocks2009 %<>%
  select(blkid, plid) %>%
  rename(plid_annexed = plid) %>%
  filter(blkid %in% blocks2008$blkid) %>%
  left_join(blocks2008, by = "blkid") 

write_csv(blocks2009, "aa_baseline_full_0809.csv")

rm(list = ls())

# start with all blocks in 2007 and identify those that were annexed 
aa <- read_csv("2008buffers.csv")

aa %<>%
  rename(plid = bufferplace) %>%
  filter(!duplicated(blkid))

annexed <- read_csv("aa_baseline_full_0809.csv") #53869
table(annexed$blkid %in% aa$blkid)

aa %<>%
  full_join(annexed %>% select(blkid, plid_annexed), by = "blkid") %>%
  mutate(annexed = ifelse(is.na(plid_annexed), 0, 1),
         plid_use = ifelse(annexed == 1, plid_annexed, plid)) %>%
  select(-plid_annexed, -plid) %>%
  rename(plid = plid_use) %>%
  filter(!is.na(plid))

table(aa$annexed)
rm(annexed)

blocks2008 <- read_csv("blocks2008_int.csv")
table(aa$blkid %in% blocks2008$blkid)

aa %<>%
  filter(blkid %in% blocks2008$blkid) %>%
  left_join(blocks2008 %>% select(blkid, pop), by = "blkid")
rm(blocks2008)

aa %<>% 
  group_by(plid) %>%
  mutate(n = sum(annexed==1),
         annexing_place = ifelse(n==0, 0, 1)) %>%
  ungroup() %>%
  dplyr::select(-n) 
table(aa$annexing_place)

# we can't have places that only had 1 contiguous block
aa %<>%
  group_by(plid) %>%
  mutate(npop = sum(pop > 0)) %>%
  ungroup() %>%
  filter(npop > 0) %>%
  select(-npop)

# merge in vra 
vrastates <- c("01", "02", "04", "13", "22", "28", "45", "48", "51")
vra.df <- read_csv("vra_counties.csv")
vra.df %<>% 
  mutate(countyfips = str_pad(countyfips, 5, side = "left", pad = "0"),
         sectionv = 1)

aa %<>%
  mutate(countyfips = paste0(STATEFP, COUNTYFP), 
         vra = case_when(
           STATEFP %in% vrastates ~ 1,
           countyfips %in% vra.df$countyfips ~ 1,
           TRUE ~ 0
         ))
table(aa$annexed)

# prep for rbind 
aa$period <- "0809"
write_csv(aa, "analyticalfiles/annexedblocks0809dem.csv") 
rm(list = ls())

# 2009-2010 ----
state_codes <- c("AL_01", "AS_02", "AR_05", "AZ_04", "CA_06", "CO_08", 
                 "DE_10", "FL_12", "GA_13", "HI_15", "IA_19", "ID_16", "IL_17", "IN_18",
                 "KS_20", "KY_21", "LA_22", "MD_24",
                 "MI_26", "MN_27", "MS_28", "MO_29", "MT_30", 
                 "NC_37", "ND_38", "NE_31", "NM_35", "NV_32", 
                 "OH_39", "OK_40", "OR_41", 
                 "SC_45", "SD_46", "TN_47", "TX_48", "UT_49", "VA_51",
                 "WA_53", "WV_54", "WI_55", "WY_56"
)

# 2009-2009
blocks_list <- list()
for(i in 1:length(state_codes)) {
  blocks_list[[i]] <- read_csv(paste0("spatial_files/2009/", state_codes[[i]], "_block_plids_2009blk-2009pl_90pct.csv"))
}
blocks_list <- rbindlist(blocks_list, fill = T)
write_csv(blocks_list, "2009blk-2009plid_90pct.csv")

rm(blocks_list)

# 2009-2010
blocks_list <- list()
for(i in 1:length(state_codes)) {
  blocks_list[[i]] <- read_csv(paste0("spatial_files/2009/", state_codes[[i]], "_block_plids_2009blk-2010pl_90pct.csv"))
}
blocks_list <- rbindlist(blocks_list, fill = T)
write_csv(blocks_list, "2009blk-2010plid_90pct.csv")

rm(blocks_list)

# annex 
# 2010 IDs is just from the blkid file 
# annex 
blocks2009 <- read_csv("2009blk-2009plid_90pct.csv") %>%
  filter(!duplicated(blkid)) %>%
  select(plid, blkid)

blocks2009_na <- read_csv("all2009blocks.csv") %>%
  filter(!duplicated(blkid)) %>%
  filter(!blkid %in% blocks2009$blkid) %>%
  mutate(plid = NA) %>%
  select(plid, blkid)

# universe of annexable
blocks2009 <- base::rbind(blocks2009 %>% select(blkid, plid), blocks2009_na)
rm(blocks2009_na)

# want to know which places are CDPs
cdps09 <- read_csv("plids/pl2009.csv") %>% 
  select(Geo_NAME, plid) %>%
  mutate(cdp = ifelse(grepl("CDP|cdp", Geo_NAME), 1, 0)) %>%
  filter(cdp==1)

# filter out places in 2009 that are CDPs/na
blocks2009 %<>% 
  filter((plid %in% cdps09$plid) | is.na(plid))
rm(cdps09)

blocks2010 <- read_csv("2009blk-2010plid_90pct.csv") %>%
  filter(!duplicated(blkid))

# want to know which places are CDPs--they do not annex
cdps10 <- read_csv("plids/pl2010.csv") %>% 
  select(Geo_NAME, plid) %>%
  mutate(cdp = ifelse(grepl("CDP|cdp", Geo_NAME), 1, 0)) %>%
  filter(cdp==1)

# filter out places in 2008 that are CDPs
blocks2010 %<>% 
  filter(!(plid %in% cdps10$plid) & !is.na(plid))
rm(cdps10)

blocks2010 %<>% 
  select(blkid, plid)

length(unique(blocks2009$blkid))
length(unique(blocks2010$blkid))

# find only blocks common to each other 
blkids <- Reduce(intersect, list(unique(blocks2009$blkid), unique(blocks2010$blkid))) 

blocks2009 %<>%
  filter(blkid %in% blkids) 
blocks2010 %<>%
  filter(blkid %in% blkids) 
rm(blkids)

# annexed if in place in 2010 but not in 2009
blocks2010 %<>%
  select(blkid, plid) %>%
  rename(plid_annexed = plid) %>%
  filter(blkid %in% blocks2009$blkid) %>%
  left_join(blocks2009, by = "blkid") 

write_csv(blocks2010, "aa_baseline_full_0910.csv")

rm(list = ls())

# start with all blocks in 2009 and identify those that were annexed 
aa <- read_csv("2009buffers.csv")

aa %<>%
  rename(plid = bufferplace) %>%
  filter(!duplicated(blkid))

annexed <- read_csv("aa_baseline_full_0910.csv") #53869
table(annexed$blkid %in% aa$blkid)

aa %<>%
  full_join(annexed %>% select(blkid, plid_annexed), by = "blkid") %>%
  mutate(annexed = ifelse(is.na(plid_annexed), 0, 1),
         plid_use = ifelse(annexed == 1, plid_annexed, plid)) %>%
  select(-plid_annexed, -plid) %>%
  rename(plid = plid_use) %>%
  filter(!is.na(plid))

table(aa$annexed)
rm(annexed)

blocks2009 <- read_csv("blocks2009_int.csv")
table(aa$blkid %in% blocks2009$blkid)

aa %<>%
  filter(blkid %in% blocks2009$blkid) %>%
  left_join(blocks2009 %>% select(blkid, pop), by = "blkid")
rm(blocks2009)

aa %<>% 
  group_by(plid) %>%
  mutate(n = sum(annexed==1),
         annexing_place = ifelse(n==0, 0, 1)) %>%
  ungroup() %>%
  dplyr::select(-n) 
table(aa$annexing_place)

# we can't have places that only had 1 contiguous block
aa %<>%
  group_by(plid) %>%
  mutate(npop = sum(pop > 0)) %>%
  ungroup() %>%
  filter(npop > 0) %>%
  select(-npop)

# merge in vra 
vrastates <- c("01", "02", "04", "13", "22", "28", "45", "48", "51")
vra.df <- read_csv("vra_counties.csv")
vra.df %<>% 
  mutate(countyfips = str_pad(countyfips, 5, side = "left", pad = "0"),
         sectionv = 1)

aa %<>%
  mutate(countyfips = paste0(STATEFP, COUNTYFP), 
         vra = case_when(
           STATEFP %in% vrastates ~ 1,
           countyfips %in% vra.df$countyfips ~ 1,
           TRUE ~ 0
         ))
table(aa$annexed)

# prep for rbind 
aa$period <- "0910"
write_csv(aa, "analyticalfiles/annexedblocks0910dem.csv") 
rm(list = ls())

# 2010-2011 ----
state_codes <- c("AL_01", "AS_02", "AR_05", "AZ_04", "CA_06", "CO_08", 
                 "DE_10", "FL_12", "GA_13", "HI_15", "IA_19", "ID_16", "IL_17", "IN_18",
                 "KS_20", "KY_21", "LA_22", "MD_24",
                 "MI_26", "MN_27", "MS_28", "MO_29", "MT_30", 
                 "NC_37", "ND_38", "NE_31", "NM_35", "NV_32", 
                 "OH_39", "OK_40", "OR_41", 
                 "SC_45", "SD_46", "TN_47", "TX_48", "UT_49", "VA_51",
                 "WA_53", "WV_54", "WI_55", "WY_56"
)

# 2010-2010
blocks_list <- read_csv("blocks2010_var.csv") %>%
  mutate(blkid = paste0(str_pad(STATEA, 2, "left", "0"), str_pad(COUNTYA, 3, "left", "0"), str_pad(TRACTA, 6, "left", "0"), str_pad(BLOCKA, 4, "left", "0")),
         plid = ifelse((PLACEA == "999" | PLACEA == "99999" | is.na(PLACEA)), NA, paste0(str_pad(STATEA, 2, "left", "0"), str_pad(PLACEA, 5, "left", "0")))) %>%
  select(blkid, plid)
write_csv(blocks_list, "2010blk-2010plid_90pct.csv")

rm(blocks_list)

# 2010-2011
blocks_list <- list()
for(i in 1:length(state_codes)) {
  blocks_list[[i]] <- read_csv(paste0("spatial_files/2010/", state_codes[[i]], "_block_plids_2010blk-2011pl_90pct.csv"))
}
blocks_list <- rbindlist(blocks_list, fill = T)
write_csv(blocks_list, "2010blk-2011plid_90pct.csv")

rm(blocks_list)

# annex -- for 2010, we already know all the blocks to plids because it's decennial 
blocks2010 <- read_csv("2010blk-2010plid_90pct.csv") %>%
  filter(!duplicated(blkid)) %>%
  select(plid, blkid)

# want to know which places are CDPs
cdps10 <- read_csv("plids/pl2010.csv") %>% 
  select(Geo_NAME, plid) %>%
  mutate(cdp = ifelse(grepl("CDP|cdp", Geo_NAME), 1, 0)) %>%
  filter(cdp==1)

# only keep places in 2010 that are CDPs/na as annexable 
blocks2010 %<>% 
  filter((plid %in% cdps10$plid) | is.na(plid))
rm(cdps10)

blocks2011 <- read_csv("2010blk-2011plid_90pct.csv") %>%
  filter(!duplicated(blkid))

# want to know which places are CDPs--they do not annex
cdps11 <- read_csv("plids/pl2011.csv") %>% 
  select(Geo_NAME, plid) %>%
  mutate(cdp = ifelse(grepl("CDP|cdp", Geo_NAME), 1, 0)) %>%
  filter(cdp==1)

# filter out places in 2011 that are CDPs
blocks2011 %<>% 
  filter(!(plid %in% cdps11$plid) & !is.na(plid))
rm(cdps11)

blocks2011 %<>% 
  select(blkid, plid)

length(unique(blocks2010$blkid))
length(unique(blocks2011$blkid))

# find only blocks common to each other 
blkids <- Reduce(intersect, list(unique(blocks2010$blkid), unique(blocks2011$blkid))) 

blocks2010 %<>%
  filter(blkid %in% blkids) 
blocks2011 %<>%
  filter(blkid %in% blkids) 
rm(blkids)

# annexed if in place in 2011 but not in 2010
blocks2011 %<>%
  select(blkid, plid) %>%
  rename(plid_annexed = plid) %>%
  filter(blkid %in% blocks2010$blkid) %>%
  left_join(blocks2010, by = "blkid") 

write_csv(blocks2011, "aa_baseline_full_1011.csv")

rm(list = ls())

# start with all blocks in 2009 and identify those that were annexed 
aa <- read_csv("2010buffers.csv")

aa %<>%
  rename(plid = bufferplace) %>%
  filter(!duplicated(blkid))

annexed <- read_csv("aa_baseline_full_0910.csv") #53869
table(annexed$blkid %in% aa$blkid)

aa %<>%
  full_join(annexed %>% select(blkid, plid_annexed), by = "blkid") %>%
  mutate(annexed = ifelse(is.na(plid_annexed), 0, 1),
         plid_use = ifelse(annexed == 1, plid_annexed, plid)) %>%
  select(-plid_annexed, -plid) %>%
  rename(plid = plid_use) %>%
  filter(!is.na(plid))

table(aa$annexed)
rm(annexed)

blocks2009 <- read_csv("blocks2009_int.csv")
table(aa$blkid %in% blocks2009$blkid)

aa %<>%
  filter(blkid %in% blocks2009$blkid) %>%
  left_join(blocks2009 %>% select(blkid, pop), by = "blkid")
rm(blocks2009)

aa %<>% 
  group_by(plid) %>%
  mutate(n = sum(annexed==1),
         annexing_place = ifelse(n==0, 0, 1)) %>%
  ungroup() %>%
  dplyr::select(-n) 
table(aa$annexing_place)

# we can't have places that only had 1 contiguous block
aa %<>%
  group_by(plid) %>%
  mutate(npop = sum(pop > 0)) %>%
  ungroup() %>%
  filter(npop > 0) %>%
  select(-npop)

# merge in vra 
vrastates <- c("01", "02", "04", "13", "22", "28", "45", "48", "51")
vra.df <- read_csv("vra_counties.csv")
vra.df %<>% 
  mutate(countyfips = str_pad(countyfips, 5, side = "left", pad = "0"),
         sectionv = 1)

aa %<>%
  mutate(countyfips = paste0(STATEFP, COUNTYFP), 
         vra = case_when(
           STATEFP %in% vrastates ~ 1,
           countyfips %in% vra.df$countyfips ~ 1,
           TRUE ~ 0
         ))
table(aa$annexed)

# prep for rbind 
aa$period <- "0910"
write_csv(aa, "analyticalfiles/annexedblocks0910dem.csv") 
rm(list = ls())

# 2011-2012 ----
state_codes <- c("AL_01", "AS_02", "AR_05", "AZ_04", "CA_06", "CO_08", 
                 "DE_10", "FL_12", "GA_13", "HI_15", "IA_19", "ID_16", "IL_17", "IN_18",
                 "KS_20", "KY_21", "LA_22", "MD_24",
                 "MI_26", "MN_27", "MS_28", "MO_29", "MT_30", 
                 "NC_37", "ND_38", "NE_31", "NM_35", "NV_32", 
                 "OH_39", "OK_40", "OR_41", 
                 "SC_45", "SD_46", "TN_47", "TX_48", "UT_49", "VA_51",
                 "WA_53", "WV_54", "WI_55", "WY_56"
)

# 2011-2011
blocks_list <- list()
for(i in 1:length(state_codes)) {
  blocks_list[[i]] <- read_csv(paste0("spatial_files/2011/", state_codes[[i]], "_block_plids_2011blk-2011pl_90pct.csv"))
}
blocks_list <- rbindlist(blocks_list, fill = T)
write_csv(blocks_list, "2011blk-2011plid_90pct.csv")

rm(blocks_list)

# 2011-2012
blocks_list <- list()
for(i in 1:length(state_codes)) {
  blocks_list[[i]] <- read_csv(paste0("spatial_files/2011/", state_codes[[i]], "_block_plids_2011blk-2012pl_90pct.csv"))
}
blocks_list <- rbindlist(blocks_list, fill = T)
write_csv(blocks_list, "2011blk-2012plid_90pct.csv")

rm(blocks_list)

# get block ids 
# 2011
blocks_list <- list()
for(i in 1:length(state_codes)) {
  blocks_list[[i]] <- st_read(paste0("SHP_blk_0010/2011/tl_2011_", substr(state_codes[[i]], 4, 5), "_tabblock.shp")) %>%
    as.data.frame() %>%
    mutate(blkid = paste0(str_pad(as.character(STATEFP10), 2, side = "left", pad = "0"), str_pad(as.character(COUNTYFP10), 3, side = "left", pad = "0"),
                          str_pad(as.character(TRACTCE10), 6, side = "left", pad = "0"), str_pad(as.character(BLOCKCE10), 4, side = "left", pad = "0"))) %>%
    select(blkid)
}

blocks_list <- rbindlist(blocks_list, fill = T)
write_csv(blocks_list, "all2011blocks.csv")
rm(blocks_list)

# 2012
blocks_list <- list()
for(i in 1:length(state_codes)) {
  blocks_list[[i]] <- st_read(paste0("SHP_blk_0010/2012/tl_2012_", substr(state_codes[[i]], 4, 5), "_tabblock.shp")) %>%
    as.data.frame() %>%
    mutate(blkid = paste0(str_pad(as.character(STATEFP10), 2, side = "left", pad = "0"), str_pad(as.character(COUNTYFP10), 3, side = "left", pad = "0"),
                          str_pad(as.character(TRACTCE10), 6, side = "left", pad = "0"), str_pad(as.character(BLOCKCE10), 4, side = "left", pad = "0"))) %>%
    select(blkid)
}

blocks_list <- rbindlist(blocks_list, fill = T)
write_csv(blocks_list, "all2012blocks.csv")
rm(blocks_list)

# annex 
# 2010 IDs is just from the blkid file 
# annex 
blocks2009 <- read_csv("2009blk-2009plid_90pct.csv") %>%
  filter(!duplicated(blkid)) %>%
  select(plid, blkid)

blocks2009_na <- read_csv("all2009blocks.csv") %>%
  filter(!duplicated(blkid)) %>%
  filter(!blkid %in% blocks2009$blkid) %>%
  mutate(plid = NA) %>%
  select(plid, blkid)

# universe of annexable
blocks2009 <- base::rbind(blocks2009 %>% select(blkid, plid), blocks2009_na)
rm(blocks2009_na)

# want to know which places are CDPs
cdps09 <- read_csv("plids/pl2009.csv") %>% 
  select(Geo_NAME, plid) %>%
  mutate(cdp = ifelse(grepl("CDP|cdp", Geo_NAME), 1, 0)) %>%
  filter(cdp==1)

# filter out places in 2009 that are CDPs/na
blocks2009 %<>% 
  filter((plid %in% cdps09$plid) | is.na(plid))
rm(cdps09)

blocks2010 <- read_csv("2009blk-2010plid_90pct.csv") %>%
  filter(!duplicated(blkid))

# want to know which places are CDPs--they do not annex
cdps10 <- read_csv("plids/pl2010.csv") %>% 
  select(Geo_NAME, plid) %>%
  mutate(cdp = ifelse(grepl("CDP|cdp", Geo_NAME), 1, 0)) %>%
  filter(cdp==1)

# filter out places in 2008 that are CDPs
blocks2010 %<>% 
  filter(!(plid %in% cdps10$plid) & !is.na(plid))
rm(cdps10)

blocks2010 %<>% 
  select(blkid, plid)

length(unique(blocks2009$blkid))
length(unique(blocks2010$blkid))

# find only blocks common to each other 
blkids <- Reduce(intersect, list(unique(blocks2009$blkid), unique(blocks2010$blkid))) 

blocks2009 %<>%
  filter(blkid %in% blkids) 
blocks2010 %<>%
  filter(blkid %in% blkids) 
rm(blkids)

# annexed if in place in 2010 but not in 2009
blocks2010 %<>%
  select(blkid, plid) %>%
  rename(plid_annexed = plid) %>%
  filter(blkid %in% blocks2009$blkid) %>%
  left_join(blocks2009, by = "blkid") 

write_csv(blocks2010, "aa_baseline_full_0910.csv")

rm(list = ls())

# start with all blocks in 2009 and identify those that were annexed 
aa <- read_csv("2009buffers.csv")

aa %<>%
  rename(plid = bufferplace) %>%
  filter(!duplicated(blkid))

annexed <- read_csv("aa_baseline_full_0910.csv") #53869
table(annexed$blkid %in% aa$blkid)

aa %<>%
  full_join(annexed %>% select(blkid, plid_annexed), by = "blkid") %>%
  mutate(annexed = ifelse(is.na(plid_annexed), 0, 1),
         plid_use = ifelse(annexed == 1, plid_annexed, plid)) %>%
  select(-plid_annexed, -plid) %>%
  rename(plid = plid_use) %>%
  filter(!is.na(plid))

table(aa$annexed)
rm(annexed)

blocks2009 <- read_csv("blocks2009_int.csv")
table(aa$blkid %in% blocks2009$blkid)

aa %<>%
  filter(blkid %in% blocks2009$blkid) %>%
  left_join(blocks2009 %>% select(blkid, pop), by = "blkid")
rm(blocks2009)

aa %<>% 
  group_by(plid) %>%
  mutate(n = sum(annexed==1),
         annexing_place = ifelse(n==0, 0, 1)) %>%
  ungroup() %>%
  dplyr::select(-n) 
table(aa$annexing_place)

# we can't have places that only had 1 contiguous block
aa %<>%
  group_by(plid) %>%
  mutate(npop = sum(pop > 0)) %>%
  ungroup() %>%
  filter(npop > 0) %>%
  select(-npop)

# merge in vra 
vrastates <- c("01", "02", "04", "13", "22", "28", "45", "48", "51")
vra.df <- read_csv("vra_counties.csv")
vra.df %<>% 
  mutate(countyfips = str_pad(countyfips, 5, side = "left", pad = "0"),
         sectionv = 1)

aa %<>%
  mutate(countyfips = paste0(STATEFP, COUNTYFP), 
         vra = case_when(
           STATEFP %in% vrastates ~ 1,
           countyfips %in% vra.df$countyfips ~ 1,
           TRUE ~ 0
         ))
table(aa$annexed)

# prep for rbind 
aa$period <- "0910"
write_csv(aa, "analyticalfiles/annexedblocks0910dem.csv") 
rm(list = ls())