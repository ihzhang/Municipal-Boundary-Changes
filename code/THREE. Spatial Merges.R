rm(list = ls())

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

# 07-10 ####
# clean: 
# 1. for contiguous blocks, keep only eligible blocks 
# 2. for annexeed blocks, keep only eligible blocks 
# we just need the blockids of blocks that are annexed
blocks2007 <- read_csv("blocks2007_blkids.csv")
blocks2007_pl <- read_csv("blocks2007_plids.csv")
blocks2007 %<>%
  left_join(blocks2007_pl)
cdps07 <- read_csv("pl2007_cleaned.csv") %>%
  select(Geo_NAME, plid) %>%
  mutate(cdp = ifelse(grepl("CDP|cdp", Geo_NAME), 1, 0)) %>%
  filter(cdp==1)

blocks2010 <- fread("ipumsblocks_allstates/2010blocks/nhgis0036_ds172_2010_block.csv", 
                    select = c("PLACEA", "STATEA", "COUNTYA", "TRACTA", "BLOCKA"))
blocks2010 <- blocks2010 %>%
  mutate(PLACEA = as.character(PLACEA),
         STATEA = as.character(STATEA), 
         STATEA = str_pad(STATEA, 2, side = "left", pad = "0"),
         PLACEA = str_pad(PLACEA, 5, side = "left", pad = "0"),
         plid = paste0(STATEA, PLACEA),
         blkid = paste0(str_pad(STATEA, 2, side = "left", pad = "0"), str_pad(COUNTYA, 3, side = "left", pad = "0"),
                        str_pad(TRACTA, 6, side = "left", pad = "0"), str_pad(BLOCKA, 4, side = "left", pad = "0"))
  ) %>%
  select(plid, blkid, PLACEA)
cdps10 <- read_csv("pl2010_cleaned.csv") %>%
  select(Geo_NAME, plid) %>%
  mutate(cdp = ifelse(grepl("CDP|cdp", Geo_NAME), 1, 0)) %>%
  filter(cdp==1)
blocks2010 %<>%
  filter(!(plid %in% cdps10$plid))
rm(cdps10)

blkids <- Reduce(intersect, list(unique(blocks2010$blkid), unique(blocks2007$blkid)))
blocks2010 %<>%
  filter(blkid %in% blkids) %>%
  filter(!duplicated(blkids))
blocks2007 %<>%
  filter(blkid %in% blkids) %>%
  filter(!duplicated(blkid))
rm(blkids)

# 1. first, for each place, we get a list of their blocks in 2010 and 2020 
# 2. then, we only retain the blocks that weren't part of that place in 2010 
# @RA: Would love your thoughts on how to make this process faster
blocks2007_na <- blocks2007 %>%
  filter(is.na(plid) | plid %in% cdps10$plid)
rm(cdps07)

blocks <- split(blocks2010, f = blocks2010$plid)
rm(blocks2010)
for(i in 1:length(blocks)) {
  block07 <- blocks2007 %>% filter(plid %in% blocks[[i]]$plid)
  blocks[[i]] %<>%
    filter((!(blkid %in% block07$blkid) & (blkid %in% blocks2007_na$blkid))) 
  print(i)
}

annexedblocks <- rbindlist(blocks)
write_csv(annexedblocks, "aa_baseline_full_0710.csv")

rm(list = ls())


#clean up and get ready for Census data 
aa <- read_csv("annexedblocks0710_base_unincorp.csv")

#clean up and get ready for Census data 
# 2017 block data 
blocks2007 <- fread("blocks2007_int.csv")
length(unique(blocks2007$blkid))

blocks2007 %<>%
  mutate(blkid = as.character(blkid),
         blkid = str_pad(blkid, 15, "left", "0"),
         STATEA = substr(blkid, 1, 2),
         countyfips = substr(blkid, 1, 5))

rac2007 <- read_csv("rac_2007.csv")
names(rac2007)
b2007names <- names(blocks2007)
blocks2007 %<>%
  mutate_at(all_of(b2007names)[1:26], ~ifelse(is.na(.), 0, .))

rac2007 %<>%
  mutate_at(c("njobs07", "nhincjobs07"), ~ifelse(is.na(.), 0, .))

# check they seem to be in comparable formats
head(aa$blkid)
head(blocks2007$blkid)
head(rac2007$h_geocode)

blocks2007 %<>%
  left_join(rac2007 %>% select(-Year), by = c("blkid" = "h_geocode"))
rm(rac2007)

aa %<>%
  left_join(blocks2007, by = "blkid")
rm(blocks2007)

# drop missing values
# aa %<>% 
#   mutate(keep = ifelse((annexed == 0 | (annexed == 1 & (!is.na(pop) & pop > 0 & !is.na(vap) & vap > 0))), 1, 0))
# table(aa$keep)
# aa %<>%
#   filter(keep==1)
# table(aa$annexed)

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

# merge in place data for 2017, as well as 2014-2017 trends 
# one version with cdps, one version without? 
pl0710 <- read_csv("pl0710_var.csv")
pl0710 %<>%
  mutate(cdp = ifelse(grepl("CDP|cdp", Geo_NAME), 1, 0)) %>%
  filter(cdp!=1) %>%
  select(-cdp)
table(aa$plid %in% pl0710$plid) 

aa %<>%
  filter(plid %in% pl0710$plid) %>%
  left_join(pl0710, by = "plid")

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
  mutate(vra = case_when(
    STATEA %in% vrastates ~ 1,
    countyfips %in% vra.df$countyfips ~ 1,
    TRUE ~ 0
  ))

# prep for rbind 
aa$period <- "0710"
write_csv(aa, "analyticalfiles/annexedblocks0710dem.csv") # 562627
rm(list = ls())

# 2010-2013 ####
blocks2010 <- fread("ipumsblocks_allstates/2010blocks/nhgis0036_ds172_2010_block.csv", 
                    select = c("PLACEA", "STATEA", "COUNTYA", "TRACTA", "BLOCKA"))
state_list <- list.files("SHP_blk_0010/2013/", all.files = FALSE, full.names = FALSE)
blocks2013_plids <- list()
for (i in 1:length(state_list)) {
  blocks2013_plids[[i]] <- read_csv(file = paste0("SHP_blk_0010/2013/", state_list[[i]], "/", substr(state_list[[i]], 1, 2), "_block_plids.csv")) %>%
    mutate(State = substr(state_list[[i]], 4, 5))
} 

#names(blocks2013) <- state_list
blocks2013_plids <- rbindlist(blocks2013_plids, use.names = TRUE)
rm(state_list)
write_csv(blocks2013_plids, file = "blocks2013_plids.csv")

state_list <- list.files("SHP_blk_0010/2013/", all.files = FALSE, full.names = FALSE)
blocks2013 <- list()
for (i in 1:length(state_list)) {
  blocks2013[[i]] <- st_read(paste0("SHP_blk_0010/2013/", state_list[[i]], "/tl_2013_", substr(state_list[[i]], 4, 5), "_tabblock.shp")) %>%
    as.data.frame() %>%
    select(STATEFP10, COUNTYFP10, TRACTCE10, BLOCKCE10) %>%
    mutate(blkid = paste0(str_pad(as.character(STATEFP10), 2, side = "left", pad = "0"), str_pad(as.character(COUNTYFP10), 3, side = "left", pad = "0"),
                          str_pad(as.character(TRACTCE10), 6, side = "left", pad = "0"), str_pad(as.character(BLOCKCE10), 4, side = "left", pad = "0"))) %>%
    select(blkid)
} 

#names(blocks2013) <- state_list
blocks2013 <- rbindlist(blocks2013, use.names = TRUE)
rm(state_list)
write_csv(blocks2013, file = "blocks2013.csv")

blocks2013 %<>%
  left_join(blocks2013_plids) %>%
  select(blkid, plid)
rm(blocks2013_plids)
write_csv(blocks2013, file = "blocks2013.csv")

# 2010 block data 
# we need to generate unique place IDs (e.g., place 6238 exists in both state 1 and 2, so we need to differentiate those places)
blocks2013 <- read_csv("blocks2013.csv") %>%
  select(blkid, plid)
cdps13 <- read_csv("acs13.csv") %>%
  select(Geo_NAME, plid) %>%
  mutate(cdp = ifelse(grepl("CDP|cdp", Geo_NAME), 1, 0)) %>%
  filter(cdp==1)
blocks2013 %<>%
  filter(!(plid %in% cdps13$plid))
rm(cdps13)

blocks2010 <- blocks2010 %>%
  mutate(PLACEA = as.character(PLACEA),
         STATEA = as.character(STATEA), 
         STATEA = str_pad(STATEA, 2, side = "left", pad = "0"),
         PLACEA = str_pad(PLACEA, 5, side = "left", pad = "0"),
         plid = paste0(STATEA, PLACEA),
         blkid = paste0(str_pad(STATEA, 2, side = "left", pad = "0"), str_pad(COUNTYA, 3, side = "left", pad = "0"),
                        str_pad(TRACTA, 6, side = "left", pad = "0"), str_pad(BLOCKA, 4, side = "left", pad = "0"))
  ) %>%
  select(plid, blkid, PLACEA)
cdps10 <- read_csv("pl2010_cleaned.csv") %>%
  select(Geo_NAME, plid) %>%
  mutate(cdp = ifelse(grepl("CDP|cdp", Geo_NAME), 1, 0)) %>%
  filter(cdp==1)
blkids <- Reduce(intersect, list(unique(blocks2010$blkid), unique(blocks2013$blkid)))
blocks2010 %<>%
  filter(blkid %in% blkids) %>%
  filter(!duplicated(blkids))
blocks2013 %<>%
  filter(blkid %in% blkids) %>%
  filter(!duplicated(blkid))
rm(blkids)

# 1. first, for each place, we get a list of their blocks in 2010 and 2020 
# 2. then, we only retain the blocks that weren't part of that place in 2010 
# @RA: Would love your thoughts on how to make this process faster
# ^ nvm, tyler solved it 
blocks2010_na <- blocks2010 %>%
  filter(PLACEA=="99999" | is.na(plid) | plid %in% cdps10$plid)
rm(cdps10)

blocks <- split(blocks2013, f = blocks2013$plid)
for(i in 12621:length(blocks)) {
  block10 <- blocks2010 %>% filter(plid %in% blocks[[i]]$plid)
  blocks[[i]] %<>%
    filter((!(blkid %in% block10$blkid) & (blkid %in% blocks2010_na$blkid))) 
  print(i)
}

annexedblocks <- rbindlist(blocks, use.names = TRUE)
write_csv(annexedblocks, "aa_baseline_full_1013.csv")

rm(blocks, block10, block07, blocks2007, blocks2010, plids, i, blocks2007_pl, blocks2007_na)
length(unique(annexedblocks$plid)) # how many places does this cover?

# clean 
# 1. for contiguous blocks, keep only eligible blocks 
# 2. for annexed blocks, keep only eligible blocks 
# we just need the blockids of blocks that are annexed
annexedblocks <- read_csv("aa_baseline_full_1013.csv")
#annexedblocks <- read_csv("annexed1013.csv")
annexedblocks %<>%
  #mutate(blkid = paste0(str_pad(STATEA, 2, side = "left", pad = "0"), str_pad(COUNTYA, 3, side = "left", pad = "0"),
  #                      str_pad(TRACTA, 6, side = "left", pad = "0"), str_pad(BLOCKA, 4, side = "left", pad = "0"))) %>%
  dplyr::select("blkid", "plid")
names(annexedblocks) <- c("blkid", "plid_annexed")
annexedblocks$annexed <- 1

# contiguous blocks 
state_list <- list.files("SHP_blk_0010/2010/", all.files = FALSE, full.names = FALSE)
contig_list <- list()
for (i in 1:length(state_list)) {
  contig_list[[i]] <- read_csv(file = paste0("SHP_blk_0010/2010/", state_list[[i]], "/", substr(state_list[[i]], 1, 2), "_contig.csv")) %>%
    mutate(State = substr(state_list[[i]], 4, 5))
} 

names(contig_list) <- state_list
contigall2010 <- rbindlist(contig_list, use.names = TRUE)
rm(contig_list, state_list)
table(contigall2010$State)
write_csv(contigall2010, file = "allcontigblocks2010.csv")

# identify contiguous blocks and actually annexed blocks in the all-block file ####
contigall2010 <- read_csv("allcontigblocks2010.csv")
contigall2010 %<>%
  dplyr::select(blkid, contigplace)

# annexing analytical file "aa"
aa <- annexedblocks %>% 
  full_join(contigall2010, by = "blkid")

aa %<>%
  mutate(plid = ifelse(is.na(plid_annexed), contigplace, plid_annexed),
         annexed = ifelse(is.na(annexed), 0, 1),
         contig = ifelse(is.na(contigplace), 0, 1)) %>%
  dplyr::select(blkid, plid, annexed, contig)

table(aa$annexed)
length(unique(aa$plid))

write_csv(aa, "annexedblocks1013_base_unincorp.csv")
rm(list = ls())
aa <- read_csv("annexedblocks1013_base_unincorp.csv")

#clean up and get ready for Census data 
blocks2010 <- read_csv("blocks2010_var.csv")
blocks2010 %<>%
  mutate(STATEA = str_pad(STATEA, 2, side = "left", pad = "0"),
         blkid = paste0(str_pad(STATEA, 2, side = "left", pad = "0"), str_pad(COUNTYA, 3, side = "left", pad = "0"),
                        str_pad(TRACTA, 6, side = "left", pad = "0"), str_pad(BLOCKA, 4, side = "left", pad = "0")),
         county = substr(blkid, 1, 5)
  ) %>%
  select(-TRACTA, -BLOCKA, -PLACEA, -COUNTYA)

rac2010 <- read_csv("rac_2010.csv")
names(rac2010)
b2010names <- names(blocks2010)
blocks2010 %<>%
  mutate_at(all_of(b2010names)[2:30], ~ifelse(is.na(.), 0, .))

rac2010 %<>%
  mutate_at(c("njobs10", "nhincjobs10"), ~ifelse(is.na(.), 0, .))

# check they are in comparable formats
head(aa$blkid)
head(blocks2010$blkid)
head(rac2010$h_geocode)

blocks2010 %<>%
  left_join(rac2010 %>% select(-Year), by = c("blkid" = "h_geocode"))
rm(rac2010)

aa %<>%
    left_join(blocks2010, by = "blkid")
rm(blocks2010, b2010names)

names(aa) <- gsub("10b", "", names(aa))
table(aa$annexed)

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

table(aa$annexed)

# merge in place data for 2010, as well as 2000-2010 trends 
pl1013 <- read_csv("pl1013_var.csv")
table(aa$plid %in% pl1013$plid) 

aa %<>%
  filter(plid %in% pl1013$plid) %>%
    left_join(pl1013, by = "plid")

names(aa)
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
  mutate(vra = case_when(
    STATEA %in% vrastates ~ 1,
    county %in% vra.df$countyfips ~ 1,
    TRUE ~ 0
  ))

# aa %<>% 
#   mutate(keep = ifelse((annexed == 0 | (annexed == 1 & (!is.na(pop) & pop > 0 & !is.na(vap) & vap > 0))), 1, 0))
# table(aa$keep)
# aa %<>%
#   filter(keep==1)
names(aa)

aa %<>% 
  mutate_at(all_of(vars(6:34, 36:37)), ~ifelse(is.na(.), 0, .))

# prep for rbind 
aa$period <- "1013"
write_csv(aa, "analyticalfiles/annexedblocks1013dem.csv") # 280122
rm (list = ls())

# repeat for 2014 to 2017 #### 
state_list <- list.files("SHP_blk_0010/2014/", all.files = FALSE, full.names = FALSE)
blocks2013 <- list()
for (i in 1:length(state_list)) {
  blocks2013[[i]] <- read_csv(file = paste0("SHP_blk_0010/2014/", state_list[[i]], "/", substr(state_list[[i]], 1, 2), "_block_plids.csv")) %>%
    mutate(State = substr(state_list[[i]], 4, 5))
} 

#names(blocks2013) <- state_list
blocks2014 <- rbindlist(blocks2014, use.names = TRUE)
rm(state_list)
write_csv(blocks2014, file = "blocks2014_plids.csv")
blocks2014 <- read_csv("blocks2014_plids.csv")

state_list <- list.files("SHP_blk_0010/2014/", all.files = FALSE, full.names = FALSE)
blocks2014 <- list()
for (i in 1:length(state_list)) {
  blocks2014[[i]] <- st_read(paste0("SHP_blk_0010/2014/", state_list[[i]], "/tl_2014_", substr(state_list[[i]], 4, 5), "_tabblock10.shp")) %>%
    as.data.frame() %>%
    select(STATEFP10, COUNTYFP10, TRACTCE10, BLOCKCE10) %>%
    mutate(blkid = paste0(str_pad(as.character(STATEFP10), 2, side = "left", pad = "0"), str_pad(as.character(COUNTYFP10), 3, side = "left", pad = "0"),
                          str_pad(as.character(TRACTCE10), 6, side = "left", pad = "0"), str_pad(as.character(BLOCKCE10), 4, side = "left", pad = "0"))) %>%
    select(blkid)
} 

blocks2014 <- rbindlist(blocks2014, use.names = TRUE)
rm(state_list)
write_csv(blocks2014, file = "blocks2014.csv")

#annex 1417 ####
blocks2014 <- read_csv("blocks2014.csv") %>%
  select(blkid, plid)

cdps14 <- read_csv("places2014_cleaned.csv") %>%
  select(Geo_NAME, plid) %>%
  mutate(cdp = ifelse(grepl("CDP|cdp", Geo_NAME), 1, 0)) %>%
  filter(cdp==1)
blocks2014 %<>%
  mutate(cdp = ifelse(plid %in% cdps14$plid, 1, 0))
table(blocks2014$cdp)
rm(cdps14)

blocks2017 <- read_csv("blocks2017_blkids.csv")
blocks2017plids <- read_csv("places2017_plids.csv") %>%
  select(blkid, plid)
blocks2017 %<>%
  left_join(blocks2017plids, by = "blkid")
rm(blocks2017plids)
cdps <- read_csv("places2017_cleaned.csv") %>%
  select(Geo_NAME, plid) %>%
  mutate(cdp = ifelse(grepl("CDP|cdp", Geo_NAME), 1, 0)) %>%
  filter(cdp==1)

blkids <- Reduce(intersect, list(unique(blocks2014$blkid), unique(blocks2017$blkid)))

blocks2014 %<>%
  filter(blkid %in% blkids & !duplicated(blkid))
blocks2017 %<>%
  filter(blkid %in% blkids & !duplicated(blkid) & !is.na(plid) & !(plid %in% cdps$plid))
rm(blkids, cdps)

blocks2014_na <- blocks2014 %>%
  filter(is.na(plid) | cdp==1)
blocks2014 %<>%
  filter(plid %in% blocks2017$plid)
blocks2017 %<>%
  filter(plid %in% blocks2014$plid)
length(unique(blocks2014$plid))
length(unique(blocks2017$plid))

blocks2014_na %<>%
  mutate(plid = ifelse(is.na(plid), "99999NA", plid))

blocks2017 %<>%
  left_join(blocks2014_na %>% rename(plid14 = plid), by = "blkid")

blocks2017 %<>%
  mutate(annexed = case_when(
    (plid14!= "99999NA" & (plid14 != plid)) ~ 1, 
    plid14=="99999NA" ~ 1, 
    TRUE ~ 0
  ))

annexedblocks <- blocks2017 %>%
  filter(annexed == 1)

write_csv(annexedblocks, "analyticalfiles/aa_baseline_full_1417.csv")

rm(list = ls())

annexedblocks <- read_csv("aa_baseline_full_1417.csv")
annexedblocks %<>%
  #mutate(blkid = paste0(str_pad(STATEA, 2, side = "left", pad = "0"), str_pad(COUNTYA, 3, side = "left", pad = "0"),
  #                      str_pad(TRACTA, 6, side = "left", pad = "0"), str_pad(BLOCKA, 4, side = "left", pad = "0"))) %>%
  dplyr::select("blkid", "plid")
annexedblocks$annexed <- 1

# contiguous blocks 
state_list <- list.files("SHP_blk_0010/2014/", all.files = FALSE, full.names = FALSE)
contig_list <- list()
for (i in 1:length(state_list)) {
  contig_list[[i]] <- read_csv(file = paste0("SHP_blk_0010/2014/", state_list[[i]], "/", substr(state_list[[i]], 1, 2), "_buffers.csv")) %>%
    mutate(State = substr(state_list[[i]], 4, 5))
} 

names(contig_list) <- state_list
contigall2014 <- rbindlist(contig_list, use.names = TRUE)
rm(contig_list, state_list)
table(contigall2014$State)
write_csv(contigall2014, file = "allcontigblocks2014.csv")

# identify contiguous blocks and actually annexed blocks in the all-block file ####
contigall2014 <- read_csv("allcontigblocks2014.csv")
contigall2014 %<>%
  dplyr::select(blkid, bufferplace)

# annexing analytical file "aa"
aa <- annexedblocks %>% 
  full_join(contigall2014, by = "blkid")

aa %<>%
  mutate(annexed = ifelse(is.na(annexed), 0, 1), 
         plid = ifelse(is.na(plid), bufferplace, plid),
         contig = ifelse(is.na(bufferplace), 0, 1)) %>%
  dplyr::select(blkid, plid, annexed, contig) 

table(aa$annexed)
length(unique(aa$plid))

write_csv(aa, "annexedblocks1417_base_unincorp.csv")

rm(annexedblocks, contigall2014)
aa <- read_csv("annexedblocks1417_base_unincorp.csv")

#clean up and get ready for Census data 
# 2014 block data ####
blocks2014 <- fread("blocks2014_int.csv")
blocks2014 %<>%
  mutate(STATEA = substr(blkid, 1, 2),
         countyfips = substr(blkid, 1, 5)
  ) %>%
  select(-Year)

rac2014 <- read_csv("rac_2014.csv")
names(rac2014)

b2014names <- names(blocks2014)
b2014names
blocks2014 %<>%
  mutate_at(all_of(b2014names)[1:25], ~ifelse(is.na(.), 0, .))

rac2014 %<>%
  mutate_at(c("njobs14", "nhincjobs14"), ~ifelse(is.na(.), 0, .))

head(aa$blkid)
head(blocks2014$blkid)
head(rac2014$h_geocode)

blocks2014 %<>%
  left_join(rac2014 %>% select(-Year), by = c("blkid" = "h_geocode"))
rm(rac2014)

# check they seem to be in comparable formats
aa %<>%
  left_join(blocks2014, by = "blkid")
rm(blocks2014)

# drop missing values
# aa %<>% 
#   mutate(keep = ifelse((annexed == 0 | (annexed == 1 & (!is.na(pop) & pop > 0 & !is.na(vap) & vap > 0))), 1, 0))
# table(aa$keep)
# aa %<>%
#   filter(keep==1)
# 
table(aa$annexed)

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

table(aa$annexed)

# merge in place data for 2014, as well as 2010-2014 trends 
pl1417 <- read_csv("places1417_var.csv")
table(aa$plid %in% pl1417$plid) 
aa %<>%
  filter(plid %in% pl1417$plid) %>%
  left_join(pl1417, by = "plid")

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
  mutate(vra = case_when(
    STATEA %in% vrastates ~ 1,
    countyfips %in% vra.df$countyfips ~ 1,
    TRUE ~ 0
  ))

names(aa)
aa %<>% 
  mutate_at(all_of(vars(5:29, 32:33)), ~ifelse(is.na(.), 0, .))
summary(aa)

# prep for rbind 
aa$period <- "1417"
write_csv(aa, "analyticalfiles/annexedblocks1417dem.csv") # 156527

rm(list = ls())

# repeat for 2017-2020 ####
# clean: 
# 1. for contiguous blocks, keep only eligible blocks 
# 2. for annexeed blocks, keep only eligible blocks 
# we just need the blockids of blocks that are annexed
annexedblocks <- read_csv("aa_baseline_full_1720.csv")

annexedblocks %<>%
  dplyr::select("blkid", "plid")
names(annexedblocks) <- c("blkid", "plid_annexed")
annexedblocks$annexed <- 1

# contiguous blocks 
contigall2017 <- read_csv("allcontigblocks2017.csv")

# identify contiguous blocks and actually annexed blocks in the all-block file ####
contigall2017 %<>%
  dplyr::select(blkid, bufferplace)

# annexing analytical file "aa"
table(annexedblocks$blkid %in% contigall2017$blkid)
table(annexedblocks$plid_annexed %in% contigall2017$bufferplace)

aa <- annexedblocks %>% 
  full_join(contigall2017, by = "blkid")

aa %<>%
  mutate(annexed = ifelse(is.na(annexed), 0, 1), 
         plid = ifelse(is.na(plid_annexed), bufferplace, plid_annexed),
         contig = ifelse(is.na(bufferplace), 0, 1)) %>%
  dplyr::select(blkid, plid, annexed, contig) 

table(aa$annexed)
length(unique(aa$plid))

write_csv(aa, "annexedblocks1720_base_unincorp.csv")
rm(list = ls())
#clean up and get ready for Census data 
aa <- read_csv("annexedblocks1720_base_unincorp.csv")

#clean up and get ready for Census data 
# 2017 block data 
blocks2017 <- fread("blocks2017_int.csv")
blocks2017 %<>%
  mutate(STATEA = substr(blkid, 1, 2),
         countyfips = substr(blkid, 1, 5)) 

rac2017 <- read_csv("rac_2017.csv")
names(rac2017)

b2017names <- names(blocks2017)
b2017names
blocks2017 %<>%
  mutate_at(all_of(b2017names)[1:25], ~ifelse(is.na(.), 0, .))

rac2017 %<>%
  mutate_at(c("njobs17", "nhincjobs17"), ~ifelse(is.na(.), 0, .))

head(aa$blkid)
head(blocks2017$blkid)
head(rac2017$h_geocode)

blocks2017 %<>%
  left_join(rac2017 %>% select(-Year), by = c("blkid" = "h_geocode"))
rm(rac2017)

# check they seem to be in comparable formats
aa %<>%
  left_join(blocks2017, by = "blkid")
rm(blocks2017)

# drop missing values
# aa %<>% 
#   mutate(keep = ifelse((annexed == 0 | (annexed == 1 & (!is.na(pop) & pop > 0 & !is.na(vap) & vap > 0))), 1, 0))
# table(aa$keep)
# aa %<>%
#   filter(keep==1)
# table(aa$annexed)
# rm(annexedblocks, contigall2017)

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

# merge in place data for 2017, as well as 2014-2017 trends 
pl1720 <- read_csv("places1720_var.csv")
table(aa$plid %in% pl1720$plid) 

aa %<>%
  filter(plid %in% pl1720$plid) %>%
  left_join(pl1720, by = "plid")

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
  mutate(vra = case_when(
    STATEA %in% vrastates ~ 1,
    countyfips %in% vra.df$countyfips ~ 1,
    TRUE ~ 0
  ))

names(aa)
aa %<>% 
  mutate_at(all_of(vars(5:29, 33:34)), ~ifelse(is.na(.), 0, .))
summary(aa)

# prep for rbind 
aa$period <- "1720"
write_csv(aa, "analyticalfiles/annexedblocks1720dem.csv") # 280122
rm(list = ls())
