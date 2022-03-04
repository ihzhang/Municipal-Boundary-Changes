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

# create annexed blocks file for 2000-2010 ####
# 2000-2010, which is needed to test PTA 
# create a file of annexed blocks between 2000 and 2010, defined as: 
# a) blocks in 2010 places that were not in those places in 2000, on 2010 boundaries
# b) blocks that were previously not part of any place and are now in 2010 places 
# this is done place by place; first isolate the place in 2000 and 2010, then compare the blocks in each list
blocks2000 <- read_csv("blocks2000_var.csv") 
blocks2010 <- fread("ipumsblocks_allstates/2010blocks/nhgis0036_ds172_2010_block.csv",
                       select = c("PLACEA", "STATEA", "GISJOIN", "COUNTYA", "TRACTA", "BLOCKA"))

# 2000 block data 
# we need to generate unique place IDs (e.g., place 6238 exists in both state 1 and 2, so we need to differentiate those places)
blocks2000 <- blocks2000 %>%
  mutate(PLACEA = as.character(PLACEA),
         STATEA = as.character(STATEA), 
         STATEA = str_pad(STATEA, 2, side = "left", pad = "0"),
         PLACEA = str_pad(PLACEA, 5, side = "left", pad = "0"),
         plid = paste0(STATEA, PLACEA) # this 7-digit number is the unique place ID for each Census place 
) %>%
  select(plid, blkid, PLACEA)

# 2010 block data 
# we repeat this process for 2010 data
blocks2010 <- blocks2010 %>%
  mutate(PLACEA = as.character(PLACEA),
         STATEA = as.character(STATEA), 
         STATEA = str_pad(STATEA, 2, side = "left", pad = "0"),
         PLACEA = str_pad(PLACEA, 5, side = "left", pad = "0"),
         plid = paste0(STATEA, PLACEA),
         blkid = paste0(str_pad(STATEA, 2, side = "left", pad = "0"), str_pad(COUNTYA, 3, side = "left", pad = "0"),
                        str_pad(TRACTA, 6, side = "left", pad = "0"), str_pad(BLOCKA, 4, side = "left", pad = "0"))
  ) %>% # we don't want to bother with null places in 2010; also, 0199999 is not a unique place ID, for e.g.
  filter(PLACEA!="99999" & !is.na(PLACEA)) %>% 
  select(plid, blkid)

blkids <- Reduce(intersect, list(unique(blocks2000$blkid), unique(blocks2010$blkid)))
blocks2010 %<>%
  filter(blkid %in% blkids)
blocks2000 %<>%
  filter(blkid %in% blkids)

# 1. first, for each place, we get a list of their blocks in 2000 and 2010 
# 2. then, we only retain the blocks that weren't part of that place in 2000 
# @RA: Would love your thoughts on how to make this process faster
annexedblocks <- data.frame()
plids <- unique(blocks2010$plid)
for (i in 1:length(plids)) {
  block00 <- blocks2000 %>% filter(plid==plids[i])
  block10 <- blocks2010 %>% filter(plid==plids[i])
  block <- block10 %>% 
    filter(!blkid %in% block00$blkid) %>%
    left_join(blocks2000 %>% select(-plid), by = "blkid") %>%
    filter (PLACEA=="99999" | is.na(PLACEA)) %>%
    select(-PLACEA)
  annexedblocks <- base::rbind(annexedblocks, block)
}

write_csv(annexedblocks, "aa_baseline_full_0010.csv")

rm(block, block00, block10, blocks2010, plids, i)
length(unique(annexedblocks$plid)) # how many places does this cover?

# clean: 
# 1. for contiguous blocks, keep only eligible blocks 
# 2. for annexeed blocks, keep only eligible blocks 
# we just need the blockids of blocks that are annexed
annexedblocks <- read_csv("aa_baseline_full_0010.csv")
# need cross-walk for 2000 place ids 

plids2010cw <- read_csv("seplaces_allstates/2010places.csv") %>%
  select(Geo_NAME, Geo_FIPS) %>%
  rename(plid = Geo_FIPS)

annexedblocks %<>%
  left_join(plids2010cw)

plids2000 <- read_csv("seplaces_allstates/2000places.csv") %>%
  select(Geo_QName, Geo_FIPS) %>%
  rename(plid2000 = Geo_FIPS,
         Geo_NAME = Geo_QName)

annexedblocks %<>%
  left_join(plids2000, by = "Geo_NAME")

table(annexedblocks$plid == annexedblocks$plid2000)

annexedblocks %<>%
    #mutate(blkid = paste0(str_pad(STATEA, 2, side = "left", pad = "0"), str_pad(COUNTYA, 3, side = "left", pad = "0"),
    #                      str_pad(TRACTA, 6, side = "left", pad = "0"), str_pad(BLOCKA, 4, side = "left", pad = "0"))) %>%
    dplyr::select("blkid", "plid", "plid2000")
names(annexedblocks) <- c("blkid", "plid_annexed", "plid2000")
annexedblocks$annexed <- 1
# if annexed, annex = 1
rm(plids2010cw)

pl0010_var <- read_csv("pl0010_var.csv")

pl0010_var %<>%
    mutate(annexing_places = ifelse(plid %in% annexedblocks$plid_annexed, 1, 0))
table(pl0010_var$annexing_places)

write_csv(pl0010_var, "pl0010_var.csv")

# contiguous blocks 
state_list <- list.files("SHP_blk_0010/2000/", all.files = FALSE, full.names = FALSE)
contig_list <- list()
for (i in 1:length(state_list)) {
    contig_list[[i]] <- read_csv(file = paste0("SHP_blk_0010/2000/", state_list[[i]], "/", substr(state_list[[i]], 1, 2), "_contig.csv")) %>%
        mutate(State = substr(state_list[[i]], 4, 5))
} 

names(contig_list) <- state_list
contigall2000 <- rbindlist(contig_list, use.names = TRUE)
rm(contig_list, state_list)
table(contigall2000$State)
write_csv(contigall2000, file = "allcontigblocks2000.csv")
contigall2000 <- read_csv("allcontigblocks2000.csv")

# identify contiguous blocks and actually annexed blocks in the all-block file ####
contigall2000 %<>%
    dplyr::select(blkid, contigplace)

# annexing analytical file "aa"
table(annexedblocks$blkid %in% contigall2000$blkid)
table(annexedblocks$plid_annexed %in% contigall2000$contigplace)

aa <- annexedblocks %>% 
    full_join(contigall2000, by = "blkid")

aa %<>%
    mutate(annexed = ifelse(is.na(annexed), 0, 1), 
           plid = ifelse(is.na(plid_annexed), contigplace, plid_annexed),
           contig = ifelse(is.na(contigplace), 0, 1)) %>%
    dplyr::select(blkid, plid, annexed, contig) 

table(aa$annexed)
length(unique(aa$plid))

write_csv(aa, "annexedblocks0010_base_unincorp.csv")

#clean up and get ready for Census data 
aa <- read_csv("annexedblocks0010_base_unincorp.csv")

#clean up and get ready for Census data 
# 2000 block data 
blocks2000 <- read_csv("blocks2000_var.csv")
blocks2000 %<>%
  mutate(STATEA = str_pad(STATEA, 2, side = "left", pad = "0"),
         blkid = paste0(str_pad(STATEA, 2, side = "left", pad = "0"), str_pad(COUNTYA, 3, side = "left", pad = "0"),
                        str_pad(TRACTA, 6, side = "left", pad = "0"), str_pad(BLOCKA, 4, side = "left", pad = "0")),
         county = substr(blkid, 1, 5)
  ) %>%
  select(-TRACTA, -BLOCKA, -PLACEA, -COUNTYA)

head(aa$blkid)
head(blocks2000$blkid)

# check they seem to be in comparable formats
aa %<>%
  left_join(blocks2000, by = "blkid")
rm(blocks2000)

names(aa) <- gsub("00b", "", names(aa))

# drop missing values
aa %<>% 
  mutate(keep = ifelse((annexed == 0 | (annexed == 1 & (!is.na(pop) & pop > 0 & !is.na(vap) & vap > 0))), 1, 0))
table(aa$keep)
aa %<>%
  filter(keep==1)
table(aa$annexed)

aa %<>% 
  #select(-plid2000) %>%
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

# merge in place data for 2010, as well as 2000-2010 trends 
pl9000 <- read_csv("pl9000_var.csv")
table(aa$plid %in% pl9000$plid) 

aa %<>%
  filter(plid %in% pl9000$plid) %>%
  left_join(pl9000, by = "plid")

# places that would have gone from majority-white to majority-minority  
aa %<>%
  select(-c(contains("90p")))
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

# prep for rbind 
names(aa) <- gsub("00p", "_p", names(aa))
aa$period <- "0010"
write_csv(aa, "analyticalfiles/annexedblocks0010dem_pl00_newsample_unincorp.csv") # 280122
rm(list = ls())

# repeat for 2010-2013 ####
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

blkids <- Reduce(intersect, list(unique(blocks2010$blkid), unique(blocks2013$blkid)))
blocks2010 %<>%
  filter(blkid %in% blkids)
blocks2013 %<>%
  filter(blkid %in% blkids)
rm(blkids)
# 1. first, for each place, we get a list of their blocks in 2010 and 2020 
# 2. then, we only retain the blocks that weren't part of that place in 2010 
# @RA: Would love your thoughts on how to make this process faster
annexedblocks <- list()
blocks <- split(blocks2013, f = blocks2013$plid)
foreach (i = 1:length(blocks)) %do% {
  block10 <- blocks2010 %>% filter(plid %in% blocks[[i]]$plid)
  blocks[[i]] %<>%
    filter(!blkid %in% block10$blkid) %>%
    left_join(blocks2010 %>% select(-plid), by = "blkid") %>%
    filter (PLACEA=="99999" | is.na(PLACEA)) 
}

for (i in 1:length(plids)) {
  block10 <- blocks2010 %>% filter(plid==plids[i])
  block13 <- blocks2013 %>% filter(plid==plids[i])
  block <- block13 %>% 
    filter(!blkid %in% block10$blkid) %>%
    left_join(blocks2010 %>% select(-plid), by = "blkid") %>%
    filter (PLACEA=="99999" | is.na(PLACEA)) 
  if (nrow(block) < 1) {
    next
  } else {
  annexedblocks[[i]] <- block
  }
}

write_csv(annexedblocks, "aa_baseline_full_1013.csv")

rm(block, block10, block13, blocks2013, blocks2010, plids, i)
length(unique(annexedblocks$GISJOIN)) # check that every entry is unique
length(unique(annexedblocks$plid)) # how many places does this cover?

# clean 
# 1. for contiguous blocks, keep only eligible blocks 
# 2. for annexed blocks, keep only eligible blocks 
# we just need the blockids of blocks that are annexed
annexedblocks <- read_csv("aa_baseline_full_1013.csv")
annexedblocks <- read_csv("annexed1013.csv")
annexedblocks %<>%
  #mutate(blkid = paste0(str_pad(STATEA, 2, side = "left", pad = "0"), str_pad(COUNTYA, 3, side = "left", pad = "0"),
  #                      str_pad(TRACTA, 6, side = "left", pad = "0"), str_pad(BLOCKA, 4, side = "left", pad = "0"))) %>%
  dplyr::select("blkid", "plid")
annexedblocks %<>% select(blkid, plid)
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
# 2013 block data 
blocks2010 <- read_csv("blocks2010_var.csv")
blocks2010 %<>%
  mutate(STATEA = str_pad(STATEA, 2, side = "left", pad = "0"),
         blkid = paste0(str_pad(STATEA, 2, side = "left", pad = "0"), str_pad(COUNTYA, 3, side = "left", pad = "0"),
                        str_pad(TRACTA, 6, side = "left", pad = "0"), str_pad(BLOCKA, 4, side = "left", pad = "0")),
         county = substr(blkid, 1, 5)
  ) %>%
  select(-TRACTA, -BLOCKA, -PLACEA, -COUNTYA)

head(aa$blkid)
head(blocks2010$blkid)

# check they seem to be in comparable formats
aa %<>%
    left_join(blocks2010, by = "blkid")
rm(blocks2010)

names(aa) <- gsub("10b", "", names(aa))

# drop missing values
# aa %<>% 
#     filter(!is.na(pop) & pop > 0 & 
#              !is.na(pctnhblack) & 
#              !is.na(pctnhwhite) & 
#              !is.na(pcth) & 
#              !is.na(pctmin) & 
#              !is.na(hispvap) & 
#              !is.na(nhwvap) & 
#              !is.na(nhbvap) & 
#              !is.na(vacancy))

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
pl0010 <- read_csv("pl0010_var.csv")
table(aa$plid %in% pl0010$plid) 

aa %<>%
  filter(plid %in% pl0010$plid) %>%
    left_join(pl0010, by = "plid")

aa %<>%
  select(-c(contains("00p")))
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

aa %<>% 
  mutate(keep = ifelse((annexed == 0 | (annexed == 1 & (!is.na(pop) & pop > 0 & !is.na(vap) & vap > 0))), 1, 0))
table(aa$keep)
aa %<>%
  filter(keep==1)

# prep for rbind 
names(aa) <- gsub("10p", "_p", names(aa))
aa$period <- "1013"
write_csv(aa, "analyticalfiles/annexedblocks1013dem_pl00_newsample_unincorp.csv") # 280122
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
blocks2014 %<>%
  left_join(blocks2014_plids) %>%
  #filter(is.na(plid)) %>%
  select(blkid, plid)
rm(blocks2014_plids)
write_csv(blocks2014, "blocks2014.csv")

# 2010 block data 
# we need to generate unique place IDs (e.g., place 6238 exists in both state 1 and 2, so we need to differentiate those places)
blocks2020 <- fread("ipumsblocks_allstates/2010blocks/nhgis0036_ds172_2010_block.csv", 
                    select = c("PLACEA", "STATEA", "COUNTYA", "TRACTA", "BLOCKA"))
blocks2020 <- blocks2020 %>%
  mutate(PLACEA = as.character(PLACEA),
         STATEA = as.character(STATEA), 
         STATEA = str_pad(STATEA, 2, side = "left", pad = "0"),
         PLACEA = str_pad(PLACEA, 5, side = "left", pad = "0"),
         plid = paste0(STATEA, PLACEA),
         blkid = paste0(str_pad(STATEA, 2, side = "left", pad = "0"), str_pad(COUNTYA, 3, side = "left", pad = "0"),
                        str_pad(TRACTA, 6, side = "left", pad = "0"), str_pad(BLOCKA, 4, side = "left", pad = "0"))
  ) %>%
  select(plid, blkid)

blkids <- Reduce(intersect, list(unique(blocks2014$blkid), unique(blocks2020$blkid)))
blocks2014 %<>%
  filter(blkid %in% blkids)
blocks2020 %<>%
  filter(blkid %in% blkids)

# 1. first, for each place, we get a list of their blocks in 2010 and 2020 
# 2. then, we only retain the blocks that weren't part of that place in 2010 
# @RA: Would love your thoughts on how to make this process faster
annexedblocks <- data.frame()
plids <- unique(blocks2014$plid)
for (i in 1:length(plids)) {
  block14 <- blocks2014 %>% filter(plid==plids[i])
  block20 <- blocks2020 %>% filter(plid==plids[i])
  block <- block20 %>% 
         filter(!blkid %in% block14$blkid) %>%
         left_join(blocks2014 %>% rename(plid14 = plid), by = "blkid") %>%
         filter(is.na(plid14)) %>%
        select(-plid14)

annexedblocks <- base::rbind(annexedblocks, block)
}
write_csv(annexedblocks, "aa_baseline_full_1420.csv")

rm(block, block10, block13, blocks2013, blocks2010, plids, i)
length(unique(annexedblocks$GISJOIN)) # check that every entry is unique
length(unique(annexedblocks$plid)) # how many places does this cover?

annexedblocks <- read_csv("aa_baseline_full_1420.csv")
annexedblocks %<>%
  mutate(blkid = paste0(str_pad(STATEA, 2, side = "left", pad = "0"), str_pad(COUNTYA, 3, side = "left", pad = "0"),
                        str_pad(TRACTA, 6, side = "left", pad = "0"), str_pad(BLOCKA, 4, side = "left", pad = "0"))) %>%
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
aa <- contigall2014 %>% 
  left_join(annexedblocks, by = "blkid")

aa %<>%
  mutate(plid = ifelse(is.na(plid), bufferplace, plid),
         annexed = ifelse(is.na(annexed), 0, annexed)) %>%
  dplyr::select(blkid, plid, annexed)

table(aa$annexed)
length(unique(aa$plid))

write_csv(aa, "annexedblocks1420_base_unincorp.csv")

rm(annexedblocks, contigall2014)
aa <- read_csv("annexedblocks1420_base_unincorp.csv")

#clean up and get ready for Census data 
# 2014 block data 
blocks2014 <- read_csv("blocks2014_int.csv")
blocks2014 %<>%
  mutate(STATEA = substr(blkid, 1, 2),
         county = substr(blkid, 1, 5)
  ) %>%
  select(-Year)
names(blocks2014)

head(aa$blkid)
head(blocks2014$blkid)

# check they seem to be in comparable formats

aa %<>%
  left_join(blocks2014, by = "blkid")
rm(blocks2014)

# drop missing values
aa %<>% 
  mutate(keep = ifelse((annexed == 0 | (annexed == 1 & (!is.na(pop) & pop > 0 & !is.na(vap) & vap > 0))), 1, 0))
table(aa$keep)
aa %<>%
  filter(keep==1)

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
107354/nrow(aa)

# merge in place data for 2014, as well as 2010-2014 trends 
pl1014 <- read_csv("pl1014_var.csv")
table(aa$plid %in% pl1014$plid) 
aa %<>%
  filter(plid %in% pl1014$plid)

aa %<>%
  left_join(pl1014, by = "plid")

aa %<>%
  select(-c(contains("10p")))

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

# prep for rbind 
names(aa) <- gsub("14p", "_p", names(aa))
aa$period <- "1420"
write_csv(aa, "analyticalfiles/annexedblocks1420dem_pl00_newsample_unincorp.csv") # 156527

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
blocks2017 <- read_csv("blocks2017_int.csv")
blocks2017 %<>%
  mutate(STATEA = substr(blkid, 1, 2),
         county = substr(blkid, 1, 5)) 

head(aa$blkid)
head(blocks2017$blkid)

# check they seem to be in comparable formats
aa %<>%
  left_join(blocks2017, by = "blkid")
rm(blocks2017)

# drop missing values
aa %<>% 
  mutate(keep = ifelse((annexed == 0 | (annexed == 1 & (!is.na(pop) & pop > 0 & !is.na(vap) & vap > 0))), 1, 0))
table(aa$keep)
aa %<>%
  filter(keep==1)
table(aa$annexed)

aa %<>% 
  #select(-plid2000) %>%
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

# merge in place data for 2010, as well as 2000-2010 trends 
pl9000 <- read_csv("pl9000_var.csv")
table(aa$plid %in% pl9000$plid) 

aa %<>%
  filter(plid %in% pl9000$plid) %>%
  left_join(pl9000, by = "plid")

# places that would have gone from majority-white to majority-minority  
aa %<>%
  select(-c(contains("90p")))
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

# prep for rbind 
names(aa) <- gsub("00p", "_p", names(aa))
aa$period <- "0010"
write_csv(aa, "analyticalfiles/annexedblocks0010dem_pl00_newsample_unincorp.csv") # 280122
rm(list = ls())

# merge ####
aa_1013 <- read_csv("analyticalfiles/annexedblocks1013dem_pl00_newsample_unincorp.csv")
aa_1420 <- read_csv("analyticalfiles/annexedblocks1420dem_pl00_newsample_unincorp.csv")

# we don't actually have block-level vap, so keep only block race 
cpi <- c(1.09, 1)
names(aa_1013)
names(aa_1420)

aa_1013 %<>%
  select(period, 
         blkid,
         plid,
         annexed,
         annexing_place,
         STATEA,
         vra,
         pctnhblack,
         pctnhwhite,
         pcth,
         pctmin,
         vacancy,
         hispvap,
         nhwvap,
         nhbvap,
         minorityvap,
         pop_p,
         popdensity_p,
         pctnhblack_p:pctmin_p,
         pctrecimm_p,
         hinc_p,
         whitepov_p,
         blackpov_p,
         hpov_p,
         minpov_p,
         nhwhitevap_p,
         nhblackvap_p,
         hispvap_p,
         minvap_p,
         threat_white,
         threat_white_vap,
         densifying,
         economic_need,
         recimmgrowth
         )

aa_1420 %<>%
  select(period, 
         blkid,
         plid,
         annexed,
         annexing_place,
         STATEA,
         vra,
         pctnhblack,
         pctnhwhite,
         pcth,
         pctmin,
         vacancy,
         hispvap,
         nhwvap,
         nhbvap,
         minorityvap,
         pop_p,
         popdensity_p,
         pctnhblack_p:pctmin_p,
         pctrecimm_p,
         hinc_p,
         whitepov_p,
         blackpov_p,
         hpov_p,
         minpov_p,
         nhwhitevap_p,
         nhblackvap_p,
         hispvap_p,
         minvap_p,
         threat_white,
         threat_white_vap,
         densifying,
         economic_need,
         recimmgrowth) 

names(aa_1013)
names(aa_1420)

aa_1020 <- base::rbind(aa_1013, aa_1420)
write_csv(aa_1020, "analyticalfiles/aa_1020.csv")

rm(list = ls())

# # old? 2000-2020 ####
# # save this file: all places in 2000 that annexed, the annexed blocks and their corresponding places annexed to between 2000 and 2010
# write_csv(annexedblocks, file = "annexedblocks0020.csv")
# 
# # if annexed, annex = 1
# pl9000_var <- read_csv("pl9000_var.csv")
# 
# pl9000_var <- pl9000_var %>%
#   mutate(annexing_places = ifelse(plid %in% annexedblocks$plid_annexed, 1, 0))
# table(pl9000_var$annexing_places)
# 
# write_csv(pl9000_var, "pl9000_var.csv")
# 
# # contiguous blocks 
# state_list <- list.files("SHP_blk_0010/2000/", all.files = FALSE, full.names = FALSE)
# contig_list <- list()
# for (i in 1:length(state_list)) {
# contig_list[[i]] <- read_csv(file = paste0("SHP_blk_0010/2000/", state_list[[i]], "/", substr(state_list[[i]], 1, 2), "_contig.csv")) %>%
#   mutate(State = substr(state_list[[i]], 4, 5))
# } 
# 
# names(contig_list) <- state_list
# contigall2000 <- rbindlist(contig_list, use.names = TRUE)
# rm(contig_list, state_list)
# table(contigall2000$State)
# write_csv(contigall2000, file = "allcontigblocks.csv")
# 
# # identify contiguous blocks and actually annexed blocks in the all-block file ####
# contigall2000 <- contigall2000 %>%
#   dplyr::select(blkid, contigplace)
# 
# # annexing analytical file "aa"
# aa <- contigall2000 %>% 
#   left_join(annexedblocks, by = "blkid")
# 
# aa <- aa %>%
#   mutate(plid = ifelse(is.na(plid_annexed), contigplace, plid_annexed),
#          annexed = ifelse(is.na(annexed), 0, annexed)) %>%
#   dplyr::select(blkid, plid, annexed)
# 
# table(aa$annexed)
# length(unique(aa$plid))
# 
# # we don't want places that didn't annex at all 
# no_annex <- aa %>% 
#   group_by(plid) %>%
#   summarize(n = sum(annexed==1)) %>%
#   filter(n==0) %>%
#   dplyr::select(plid) #13561. this would leave us with 
# length(unique(aa$plid)) - nrow(no_annex) #11968 places 
# 
# aa <- aa %>%
#   filter(!plid %in% no_annex$plid)
# length(unique(aa$plid))
# 
# write_csv(aa, "annexedblocks0020_base_unincorp.csv")
# 
# #clean up and get ready for Census data 
# rm(list = ls())
# aa <- read_csv("annexedblocks0020_base_unincorp.csv")
# # aa <- aa %>% distinct(blkid, annexed, .keep_all = TRUE)
# 
# # 2000 block data 
# blocks2000 <- read_csv("blocks2000_var.csv")
# 
# blocks2000 <- blocks2000 %>%
#   mutate(blkid = paste0(str_pad(STATEA, 2, side = "left", pad = "0"), str_pad(COUNTYA, 3, side = "left", pad = "0"),
#                         str_pad(TRACTA, 6, side = "left", pad = "0"), str_pad(BLOCKA, 4, side = "left", pad = "0")))
# head(aa$blkid)
# head(blocks2000$blkid)
# # check they seem to be in comparable formats
# 
# aa <- aa %>%
#   left_join(blocks2000, by = "blkid")
# 
# # drop if pop = 0 or hu = 0 
# aa <- aa %>% 
#   filter(pop00b > 0)
# 
# # do filtering of non-annexing places again 
# no_annex <- aa %>% 
#   group_by(plid) %>%
#   summarize(n = sum(annexed==1)) %>%
#   filter(n==0) %>%
#   dplyr::select(plid) #948. this would leave us with 
# length(unique(aa$plid)) - nrow(no_annex) #10728 places 
# 
# aa <- aa %>%
#   filter(!plid %in% no_annex$plid)
# length(unique(aa$plid))
# rm(no_annex)
# 
# # merge in place data for 2000, as well as 1990-2000 trends 
# pl9000 <- read_csv("pl9000_var.csv")
# 
# table(aa$plid %in% pl9000$plid) #21237 not
# aa <- aa %>% 
#   filter(plid %in% pl9000$plid)
#   
# aa <- aa %>%
#   left_join(pl9000, by = "plid")
# 
# # filter out places with no pop, no black/white/hisp/min population 
# 
# aa <- aa %>%
#   filter(pop00p > 0 & nhblack00p > 0 & nhwhite00p > 0 & h00p > 0 & min00p > 0) %>%
#   dplyr::select(-annexing_places)
# 
# aa <- aa %>%
#   filter(!is.na(pop00b) & !is.na(pctnhwhite00b) & !is.na(dependencyratio00b) & !is.na(pctowneroccupied00b) & 
#            is.finite(pop00b) & is.finite(pctnhwhite00b) & is.finite(dependencyratio00b) & is.finite(pctowneroccupied00b) & 
#            !is.na(pcth00b) & !is.na(pctmin00b) & !is.na(pctnhwhite00p) & !is.na(pctmin00p) & !is.na(pcth00p) & !is.na(popgrowth) & 
#            !is.na(hpov00p) & !is.na(blackpov00p) & !is.na(minpov00p) & !is.na(nhwhitepov00p) &
#            !is.na(recimmgrowth) & !is.na(blackpov00p) & !is.na(hinc00p) & 
#            !is.na(hispvap00p) & !is.na(nhwhitevap00p) & !is.na(minvap00p) & 
#            !is.na(hispvap00b) & !is.na(nhwvap00b) & !is.na(minorityvap00b)) 
# 
# # we also don't need any of the 1990 variables 
# aa <- aa %>% 
#   select(-c(contains("90p")))
# 
# # last check of non-annexing places 
# no_annex <- aa %>% 
#   group_by(plid) %>%
#   summarize(n = sum(annexed==1)) %>%
#   filter(n==0) %>%
#   dplyr::select(plid) #83. this would leave us with 
# length(unique(aa$plid)) - nrow(no_annex) #4770 places 
# 
# aa <- aa %>%
#   filter(!plid %in% no_annex$plid)
# length(unique(aa$plid))
# rm(no_annex)
# 
# write_csv(aa, "annexedblocks0020dem_pl00_newsample_unincorp.csv") # 207043