rm(list = ls())

# get environment ready 
setwd("~/Google Drive/Stanford/QE2")

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

# create annexed blocks file ####
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
)

# 2010 block data 
# we repeat this process for 2010 data
blocks2010 <- blocks2010 %>%
  mutate(PLACEA = as.character(PLACEA),
         STATEA = as.character(STATEA), 
         STATEA = str_pad(STATEA, 2, side = "left", pad = "0"),
         PLACEA = str_pad(PLACEA, 5, side = "left", pad = "0"),
         plid = paste0(STATEA, PLACEA)
  )

blocks2010 <- blocks2010 %>% # we don't want to bother with null places in 2010; also, 0199999 is not a unique place ID, for e.g.
  filter(PLACEA!="99999" & !is.na(PLACEA))

# 1. first, for each place, we get a list of their blocks in 2000 and 2010 
# 2. then, we only retain the blocks that weren't part of that place in 2000 
# @RA: Would love your thoughts on how to make this process faster
annexedblocks <- data.frame()
plids <- unique(blocks2010$plid)
for (i in 1:length(plids)) {
  block00 <- blocks2000 %>% filter(plid==plids[i])
  block10 <- blocks2010 %>% filter(plid==plids[i])
  block <- block10 %>% filter(!GISJOIN %in% block00$GISJOIN) # which blocks part of 2010 places were not part of those places in 2000?
  annexedblocks <- base::rbind(annexedblocks, block)
}

write_csv(annexedblocks, "aa_baseline_full_0010.csv")

rm(block, block00, block10, blocks2010, plids, i)
length(unique(annexedblocks$GISJOIN)) # check that every entry is unique
length(unique(annexedblocks$plid)) # how many places does this cover?

# clean: 
# 1. for contiguous blocks, keep only eligible blocks 
# 2. for annexeed blocks, keep only eligible blocks 
# we just need the blockids of blocks that are annexed
annexedblocks <- read_csv("aa_baseline_full_0010.csv")
annexedblocks %<>%
    mutate(blkid = paste0(str_pad(STATEA, 2, side = "left", pad = "0"), str_pad(COUNTYA, 3, side = "left", pad = "0"),
                          str_pad(TRACTA, 6, side = "left", pad = "0"), str_pad(BLOCKA, 4, side = "left", pad = "0"))) %>%
    dplyr::select("blkid", "plid")
names(annexedblocks) <- c("blkid", "plid_annexed")
annexedblocks$annexed <- 1

# if annexed, annex = 1
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

# identify contiguous blocks and actually annexed blocks in the all-block file ####
contigall2000 %<>%
    dplyr::select(blkid, contigplace)

# annexing analytical file "aa"
aa <- contigall2000 %>% 
    left_join(annexedblocks, by = "blkid")

aa %<>%
    mutate(plid = ifelse(is.na(plid_annexed), contigplace, plid_annexed),
           annexed = ifelse(is.na(annexed), 0, annexed)) %>%
    dplyr::select(blkid, plid, annexed)

table(aa$annexed)
length(unique(aa$plid))

# we don't want places that didn't annex at all 
no_annex <- aa %>% 
    group_by(plid) %>%
    summarize(n = sum(annexed==1)) %>%
    filter(n==0) %>%
    dplyr::select(plid) #13912. this would leave us with 
length(unique(aa$plid)) - nrow(no_annex) #13912 places 

aa %<>%
    filter(!plid %in% no_annex$plid)
length(unique(aa$plid))

write_csv(aa, "annexedblocks0010_base_unincorp.csv")

#clean up and get ready for Census data 
# 2000 block data 
blocks2000 <- read_csv("blocks2000_var.csv")

blocks2000 %<>%
    mutate(blkid = paste0(str_pad(STATEA, 2, side = "left", pad = "0"), str_pad(COUNTYA, 3, side = "left", pad = "0"),
                          str_pad(TRACTA, 6, side = "left", pad = "0"), str_pad(BLOCKA, 4, side = "left", pad = "0")))
head(aa$blkid)
head(blocks2000$blkid)

# check they seem to be in comparable formats
aa %<>%
    left_join(blocks2000, by = "blkid")

# drop if pop = 0 or hu = 0 
aa %<>% 
    filter(pop00b > 0)

# do filtering of non-annexing places again 
no_annex <- aa %>% 
    group_by(plid) %>%
    summarize(n = sum(annexed==1)) %>%
    filter(n==0) %>%
    dplyr::select(plid) #965. this would leave us with 
length(unique(aa$plid)) - nrow(no_annex) #12635 places 

aa %<>%
    filter(!plid %in% no_annex$plid)
length(unique(aa$plid))
rm(no_annex)

write_csv(aa, "annexedblocks0010dem_pl00_newsample_unincorp.csv") # 251103

# repeat for 2010-2013 ####
blocks2010 <- fread("ipumsblocks_allstates/2010blocks/nhgis0036_ds172_2010_block.csv", 
                    select = c("PLACEA", "STATEA", "GISJOIN", "COUNTYA", "TRACTA", "BLOCKA"))

state_list <- list.files("SHP_blk_0010/2013/", all.files = FALSE, full.names = FALSE)
blocks2013 <- list()
for (i in 1:length(state_list)) {
    blocks2013[[i]] <- read_csv(file = paste0("SHP_blk_0010/2013/", state_list[[i]], "/", substr(state_list[[i]], 1, 2), "_blocks_plids.csv")) %>%
        mutate(State = substr(state_list[[i]], 4, 5))
} 

names(blocks2013) <- state_list
blocks2013 <- rbindlist(blocks2013, use.names = TRUE)
rm(state_list)
write_csv(blocks2013, file = "blocks2013_plids.csv")


# 2010 block data 
# we need to generate unique place IDs (e.g., place 6238 exists in both state 1 and 2, so we need to differentiate those places)
blocks2010 <- blocks2010 %>%
  mutate(PLACEA = as.character(PLACEA),
         STATEA = as.character(STATEA), 
         STATEA = str_pad(STATEA, 2, side = "left", pad = "0"),
         PLACEA = str_pad(PLACEA, 5, side = "left", pad = "0"),
         plid = paste0(STATEA, PLACEA) # this 7-digit number is the unique place ID for each Census place 
  )

# 2020 block data 
# we repeat this process for 2020 data
blocks2013 %<>%
  mutate(PLACEA = as.character(PLACEA),
         STATEA = as.character(STATEA), 
         STATEA = str_pad(STATEA, 2, side = "left", pad = "0"),
         PLACEA = str_pad(PLACEA, 5, side = "left", pad = "0"),
         plid = paste0(STATEA, PLACEA)
  )

blocks2013 %<>% # we don't want to bother with null places in 2020; also, 0199999 is not a unique place ID, for e.g.
  filter(PLACEA!="99999" & !is.na(PLACEA))

# 1. first, for each place, we get a list of their blocks in 2010 and 2020 
# 2. then, we only retain the blocks that weren't part of that place in 2010 
# @RA: Would love your thoughts on how to make this process faster
annexedblocks <- data.frame()
plids <- unique(blocks2013$plid)
for (i in 1:length(plids)) {
  block10 <- blocks2010 %>% filter(plid==plids[i])
  block13 <- blocks2013 %>% filter(plid==plids[i])
  block <- block13 %>% filter(!GISJOIN %in% block10$GISJOIN) # which blocks part of 2010 places were not part of those places in 2000?
  annexedblocks <- base::rbind(annexedblocks, block)
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
annexedblocks %<>%
    mutate(blkid = paste0(str_pad(STATEA, 2, side = "left", pad = "0"), str_pad(COUNTYA, 3, side = "left", pad = "0"),
                          str_pad(TRACTA, 6, side = "left", pad = "0"), str_pad(BLOCKA, 4, side = "left", pad = "0"))) %>%
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
contigall2010 %<>%
    dplyr::select(blkid, contigplace)

# annexing analytical file "aa"
aa <- contigall2010 %>% 
    left_join(annexedblocks, by = "blkid")

aa %<>%
    mutate(plid = ifelse(is.na(plid_annexed), contigplace, plid_annexed),
           annexed = ifelse(is.na(annexed), 0, annexed)) %>%
    dplyr::select(blkid, plid, annexed)

table(aa$annexed)
length(unique(aa$plid))

# we don't want places that didn't annex at all 
no_annex <- aa %>% 
    group_by(plid) %>%
    summarize(n = sum(annexed==1)) %>%
    filter(n==0) %>%
    dplyr::select(plid) #15242. this would leave us with 
length(unique(aa$plid)) - nrow(no_annex) #10190 places 

aa %<>%
    filter(!plid %in% no_annex$plid)
length(unique(aa$plid))

write_csv(aa, "annexedblocks1013_base_unincorp.csv")

#clean up and get ready for Census data 
# 2013 block data 
blocks2013 <- read_csv("blocks2013_var.csv")

blocks2013 %<>%
    mutate(blkid = paste0(str_pad(STATEA, 2, side = "left", pad = "0"), str_pad(COUNTYA, 3, side = "left", pad = "0"),
                          str_pad(TRACTA, 6, side = "left", pad = "0"), str_pad(BLOCKA, 4, side = "left", pad = "0")))
head(aa$blkid)
head(blocks2000$blkid)
# check they seem to be in comparable formats

aa %<>%
    left_join(blocks2010, by = "blkid")

# drop if pop = 0 or hu = 0 
aa %<>% 
    filter(pop00b > 0)

# do filtering of non-annexing places again 
no_annex <- aa %>% 
    group_by(plid) %>%
    summarize(n = sum(annexed==1)) %>%
    filter(n==0) %>%
    dplyr::select(plid) #965. this would leave us with 
length(unique(aa$plid)) - nrow(no_annex) #12635 places 

aa %<>%
    filter(!plid %in% no_annex$plid)
length(unique(aa$plid))
rm(no_annex)

# merge in place data for 2000, as well as 1990-2000 trends 
pl0010 <- read_csv("pl1020_var.csv")

table(aa$plid %in% pl1020$plid) #8183 not
aa %<>% 
    filter(plid %in% pl1020$plid)

aa %<>%
    left_join(pl1020, by = "plid")

# filter out places with no pop, no black/white/hisp/min population 

aa %<>%
    filter(!is.na(pop10b) & !is.na(pctnhwhite10b) & !is.na(dependencyratio10b) & !is.na(pctowneroccupied10b) & 
               is.finite(pop10b) & is.finite(dependencyratio10b) & is.finite(pctowneroccupied10b) & 
               !is.na(pcth10b) & !is.na(pctmin10b)) 

# last check of non-annexing places 
no_annex <- aa %>% 
    group_by(plid) %>%
    summarize(n = sum(annexed==1)) %>%
    filter(n==0) %>%
    dplyr::select(plid) #39. this would leave us with 
length(unique(aa$plid)) - nrow(no_annex) #6289 places 

aa %<>%
    filter(!plid %in% no_annex$plid)
length(unique(aa$plid))
rm(no_annex)

write_csv(aa, "annexedblocks0010dem_pl00_newsample_unincorp.csv") # 251103

# repeat for 2014 to 2020 #### 
state_list <- list.files("SHP_blk_0010/2014/", all.files = FALSE, full.names = FALSE)
blocks2014 <- list()
for (i in 1:length(state_list)) {
    blocks2014[[i]] <- read_csv(file = paste0("SHP_blk_0010/2014/", state_list[[i]], "/", substr(state_list[[i]], 1, 2), "_blocks_plids.csv")) %>%
        mutate(State = substr(state_list[[i]], 4, 5))
} 

names(blocks2014) <- state_list
blocks2014 <- rbindlist(blocks2014, use.names = TRUE)
rm(state_list)
write_csv(blocks2014, file = "blocks2014_plids.csv")

blocks2020 <- fread("ipumsblocks_allstates/2020blocks/nhgis0031_ds248_2020_block.csv",
                    select = c("PLACEA", "STATEA", "GISJOIN", "COUNTYA", "TRACTA", "BLOCKA"))

# 2000 block data 
# we need to generate unique place IDs (e.g., place 6238 exists in both state 1 and 2, so we need to differentiate those places)
blocks2014 %<>%
  mutate(PLACEA = as.character(PLACEA),
         STATEA = as.character(STATEA), 
         STATEA = str_pad(STATEA, 2, side = "left", pad = "0"),
         PLACEA = str_pad(PLACEA, 5, side = "left", pad = "0"),
         plid = paste0(STATEA, PLACEA) # this 7-digit number is the unique place ID for each Census place 
  )

# 2020 block data 
# we repeat this process for 2020 data
blocks2020 <- blocks2020 %>%
  mutate(PLACEA = as.character(PLACEA),
         STATEA = as.character(STATEA), 
         STATEA = str_pad(STATEA, 2, side = "left", pad = "0"),
         PLACEA = str_pad(PLACEA, 5, side = "left", pad = "0"),
         plid = paste0(STATEA, PLACEA)
  )

blocks2020 <- blocks2020 %>% # we don't want to bother with null places in 2020; also, 0199999 is not a unique place ID, for e.g.
  filter(PLACEA!="99999" & !is.na(PLACEA))

# 1. first, for each place, we get a list of their blocks in 2010 and 2020 
# 2. then, we only retain the blocks that weren't part of that place in 2010 
# @RA: Would love your thoughts on how to make this process faster
annexedblocks <- data.frame()
plids <- unique(blocks2020$plid)
for (i in 1:length(plids)) {
  block14 <- blocks2014 %>% filter(plid==plids[i])
  block20 <- blocks2020 %>% filter(plid==plids[i])
  block <- block20 %>% filter(!GISJOIN %in% block14$GISJOIN) # which blocks part of 2010 places were not part of those places in 2000?
  annexedblocks <- base::rbind(annexedblocks, block)
}

write_csv(annexedblocks, "aa_baseline_full_1420.csv")

# clean 1420 for first-stage analysis ####
annexedblocks <- read_csv("aa_baseline_full_1420.csv")
blocks2000 <- blocks2000 %>%
  mutate(blkid = paste0(str_pad(STATEA, 2, side = "left", pad = "0"), str_pad(COUNTYA, 3, side = "left", pad = "0"),
                        str_pad(TRACTA, 6, side = "left", pad = "0"), str_pad(BLOCKA, 4, side = "left", pad = "0")))
annexedblocks <- annexedblocks %>%
  mutate(blkid = paste0(str_pad(STATEA, 2, side = "left", pad = "0"), str_pad(COUNTYA, 3, side = "left", pad = "0"),
                        str_pad(TRACTA, 6, side = "left", pad = "0"), str_pad(BLOCKA, 4, side = "left", pad = "0")))

annexedblocks <- annexedblocks[annexedblocks$blkid %in% blocks2000$blkid,] # filter out newly created blockss

# we just need the blockids of blocks that are annexed
annexedblocks <- annexedblocks %>%
  dplyr::select("blkid", "plid")
names(annexedblocks) <- c("blkid", "plid_annexed")
annexedblocks$annexed <- 1

# save this file: all places in 2000 that annexed, the annexed blocks and their corresponding places annexed to between 2000 and 2010
write_csv(annexedblocks, file = "annexedblocks0020.csv")

# if annexed, annex = 1
pl9000_var <- read_csv("pl9000_var.csv")

pl9000_var <- pl9000_var %>%
  mutate(annexing_places = ifelse(plid %in% annexedblocks$plid_annexed, 1, 0))
table(pl9000_var$annexing_places)

write_csv(pl9000_var, "pl9000_var.csv")

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
write_csv(contigall2000, file = "allcontigblocks.csv")

# identify contiguous blocks and actually annexed blocks in the all-block file ####
contigall2000 <- contigall2000 %>%
  dplyr::select(blkid, contigplace)

# annexing analytical file "aa"
aa <- contigall2000 %>% 
  left_join(annexedblocks, by = "blkid")

aa <- aa %>%
  mutate(plid = ifelse(is.na(plid_annexed), contigplace, plid_annexed),
         annexed = ifelse(is.na(annexed), 0, annexed)) %>%
  dplyr::select(blkid, plid, annexed)

table(aa$annexed)
length(unique(aa$plid))

# we don't want places that didn't annex at all 
no_annex <- aa %>% 
  group_by(plid) %>%
  summarize(n = sum(annexed==1)) %>%
  filter(n==0) %>%
  dplyr::select(plid) #13561. this would leave us with 
length(unique(aa$plid)) - nrow(no_annex) #11968 places 

aa <- aa %>%
  filter(!plid %in% no_annex$plid)
length(unique(aa$plid))

write_csv(aa, "annexedblocks0020_base_unincorp.csv")

#clean up and get ready for Census data 
rm(list = ls())
aa <- read_csv("annexedblocks0020_base_unincorp.csv")
# aa <- aa %>% distinct(blkid, annexed, .keep_all = TRUE)

# 2000 block data 
blocks2000 <- read_csv("blocks2000_var.csv")

blocks2000 <- blocks2000 %>%
  mutate(blkid = paste0(str_pad(STATEA, 2, side = "left", pad = "0"), str_pad(COUNTYA, 3, side = "left", pad = "0"),
                        str_pad(TRACTA, 6, side = "left", pad = "0"), str_pad(BLOCKA, 4, side = "left", pad = "0")))
head(aa$blkid)
head(blocks2000$blkid)
# check they seem to be in comparable formats

aa <- aa %>%
  left_join(blocks2000, by = "blkid")

# drop if pop = 0 or hu = 0 
aa <- aa %>% 
  filter(pop00b > 0)

# do filtering of non-annexing places again 
no_annex <- aa %>% 
  group_by(plid) %>%
  summarize(n = sum(annexed==1)) %>%
  filter(n==0) %>%
  dplyr::select(plid) #948. this would leave us with 
length(unique(aa$plid)) - nrow(no_annex) #10728 places 

aa <- aa %>%
  filter(!plid %in% no_annex$plid)
length(unique(aa$plid))
rm(no_annex)

# merge in place data for 2000, as well as 1990-2000 trends 
pl9000 <- read_csv("pl9000_var.csv")

table(aa$plid %in% pl9000$plid) #21237 not
aa <- aa %>% 
  filter(plid %in% pl9000$plid)
  
aa <- aa %>%
  left_join(pl9000, by = "plid")

# filter out places with no pop, no black/white/hisp/min population 

aa <- aa %>%
  filter(pop00p > 0 & nhblack00p > 0 & nhwhite00p > 0 & h00p > 0 & min00p > 0) %>%
  dplyr::select(-annexing_places)

aa <- aa %>%
  filter(!is.na(pop00b) & !is.na(pctnhwhite00b) & !is.na(dependencyratio00b) & !is.na(pctowneroccupied00b) & 
           is.finite(pop00b) & is.finite(pctnhwhite00b) & is.finite(dependencyratio00b) & is.finite(pctowneroccupied00b) & 
           !is.na(pcth00b) & !is.na(pctmin00b) & !is.na(pctnhwhite00p) & !is.na(pctmin00p) & !is.na(pcth00p) & !is.na(popgrowth) & 
           !is.na(hpov00p) & !is.na(blackpov00p) & !is.na(minpov00p) & !is.na(nhwhitepov00p) &
           !is.na(recimmgrowth) & !is.na(blackpov00p) & !is.na(hinc00p) & 
           !is.na(hispvap00p) & !is.na(nhwhitevap00p) & !is.na(minvap00p) & 
           !is.na(hispvap00b) & !is.na(nhwvap00b) & !is.na(minorityvap00b)) 

# we also don't need any of the 1990 variables 
aa <- aa %>% 
  select(-c(contains("90p")))

# last check of non-annexing places 
no_annex <- aa %>% 
  group_by(plid) %>%
  summarize(n = sum(annexed==1)) %>%
  filter(n==0) %>%
  dplyr::select(plid) #83. this would leave us with 
length(unique(aa$plid)) - nrow(no_annex) #4770 places 

aa <- aa %>%
  filter(!plid %in% no_annex$plid)
length(unique(aa$plid))
rm(no_annex)

write_csv(aa, "annexedblocks0020dem_pl00_newsample_unincorp.csv") # 207043