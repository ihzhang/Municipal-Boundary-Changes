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

# create annexed blocks file ####
# create a file of annexed blocks between 2000 and 2010, defined as: 
# a) blocks in 2010 places that were not in those places in 2000, on 2010 boundaries
# b) blocks that were previously not part of any place and are now in 2010 places 
# this is done place by place; first isolate the place in 2000 and 2010, then compare the blocks in each list
blocks2000 <- read_csv("blocks2000_var.csv")
blocks2010 <- fread("ipumsblocks_allstates/2010blocks/nhgis0033_ds172_2010_block.csv",
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

# get list of all annexed blocks to a place ####
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

write_csv(annexedblocks, "aa_baseline_full.csv")

rm(block, block00, block10, blocks2010, plids, i)
length(unique(annexedblocks$GISJOIN))
length(unique(annexedblocks$plid))

# clean up analytical blocks 
table(is.na(annexedblocks$PLACEA))
annexedblocks <- annexedblocks %>%
  mutate(blkid = paste0(STATEA, sprintf("%03.0f", COUNTYA), sprintf("%06.0f", TRACTA), sprintf("%04.0f", BLOCKA)))

blocks2000 <- blocks2000 %>%
  mutate(blkid = paste0(str_pad(STATEA, 2, side = "left", pad = "0"), str_pad(COUNTYA, 3, side = "left", pad = "0"),
                        str_pad(TRACTA, 6, side = "left", pad = "0"), str_pad(BLOCKA, 4, side = "left", pad = "0")))
head(annexedblocks$blkid)
head(blocks2000$blkid)

annexedblocks <- annexedblocks[annexedblocks$blkid %in% blocks2000$blkid,] # filter out newly created blockss

# we just need the blockids of blocks that are annexed
annexedblocks <- annexedblocks %>%
  dplyr::select("STATEA", "COUNTYA", "PLACEA", "TRACTA", "BLOCKA", "blkid", "plid")
names(annexedblocks) <- c("STATEA_annexed", "COUNTYA_annexed", "PLACEA_annexed","TRACT_annexed", "BLOCK_annexed", "blkid", "plid_annexed")
annexedblocks$annexed <- 1

# save this file: all places in 2000 that annexed, the annexed blocks and their corresponding places annexed to between 2000 and 2010
write_csv(annexedblocks, file = "annexedblocks0010.csv")
annexedblocks <- read_csv("annexedblocks0010.csv")

# if annexed, annex = 1
pl9000_var <- read_csv("pl9000_var.csv")

pl9000_var <- pl9000_var %>%
  mutate(plid = paste0(str_pad(Geo_STATE, 2, side = "left", pad = "0"), str_pad(Geo_PLACE, 5, side = "left", pad = "0")),
         annexing_places = ifelse(plid %in% annexedblocks$plid, 1, 0))
table(pl9000_var$annexing_places)

write_csv(pl9000_var, "pl9000_var.csv")

# contiguous blocks ####
state_list <- list.files("SHP_blk_0010/2000/", all.files = FALSE, full.names = FALSE)
contig_list <- list()
for (i in 1:length(state_list)) {
contig_list[[i]] <- read_csv(file = paste0("SHP_blk_0010/2000/", state_list[[i]], "/", substr(state_list[[i]], 1, 2), "_contig.csv")) %>%
  mutate(State = substr(state_list[[i]], 4, 5))
} 

names(contig_list) <- state_list
contigall2000 <- rbindlist(contig_list, use.names = TRUE)
rm(contig_list, state_list)

contigall2000$GISJOIN <- as.character(contigall2000$GISJOIN)
write_csv(contigall2000, file = "allcontigblocks.csv")

# identify contiguous blocks and actually annexed blocks in the all-block file ####
contigall2000 <- contigall2000 %>%
  mutate(blkid = paste0(sprintf("%05.0f", FIPSSTCO), sprintf("%06.0f", TRACT2000), sprintf("%04.0f", BLOCK2000)),
         plid = str_pad(contigplace, 7, side = "left", pad = "0")) %>%
  dplyr::select(blkid, plid)

# annexing analytical file "aa"
aa <- full_join(contigall2000, annexedblocks, by = "blkid")

aa <- aa %>%
  mutate(plid = ifelse(is.na(PLACEA_annexed), plid, plid_annexed),
         annexed = ifelse(is.na(annexed), 0, annexed)) %>%
  dplyr::select(blkid, plid, annexed)

# we don't want places that didn't annex at all 
no_annex <- aa %>% 
  group_by(plid) %>%
  summarize(n = sum(annexed==1)) %>%
  filter(n==0) %>%
  dplyr::select(plid)

aa <- aa %>%
  filter(!plid %in% no_annex$plid)

write_csv(aa, "annexedblocks0010_base_unincorp.csv")

#clean up and get ready for Census data ####
rm(list = ls())
aa <- read_csv("annexedblocks0010_base_unincorp.csv")
aa <- aa %>% distinct(blkid, annexed, .keep_all = TRUE)

# 2000 block data 
blocks2000 <- read_csv("blocks2000_var.csv")

blocks2000 <- blocks2000 %>%
  mutate(blkid = paste0(str_pad(STATEA, 2, side = "left", pad = "0"), str_pad(COUNTYA, 3, side = "left", pad = "0"),
                        str_pad(TRACTA, 6, side = "left", pad = "0"), str_pad(BLOCKA, 4, side = "left", pad = "0")))
head(aa$blkid)
head(blocks2000$blkid)

aa <- aa %>%
  left_join(blocks2000 %>% dplyr::select(7:ncol(blocks2000)), by = "blkid")

# drop if pop = 0 or hu = 0 
aa <- aa %>% 
  filter(pop00b > 0 & hu > 0)

# merge in block-level vap data 
vapblock <- read_csv("vap2000block.csv")
aa <- aa %>% 
  left_join(vapblock, by = "blkid")

# merge in place data for 2000, as well as 1990-2000 trends 
pl9000 <- read_csv("pl9000_var.csv")
pl9000 <- pl9000 %>%
  dplyr::select(-plid2) %>%
  filter(Geo_PLACE != "99999") %>%
  mutate(plid = paste0(str_pad(Geo_STATE, 2, side = "left", pad = "0"), str_pad(Geo_PLACE, 5, side = "left", pad = "0"))) %>%
    dplyr::select(-c(Geo_QName, Geo_STATE, Geo_PLACE))

table(aa$plid %in% pl9000$plid)
  
aa <- aa %>%
  filter(plid %in% pl9000$plid) %>%
  left_join(pl9000, by = "plid")

# filter out places with no pop, no hu, no black/white/hisp/min population 

aa <- aa %>%
  filter(pop00p > 0 & nhblack00p > 0 & nhwhite00p > 0 & h00p > 0 & min00p > 0) %>%
  dplyr::select(-annexing)

aa <- aa %>%
  filter(!is.na(pop00b) & !is.na(pctnhwhite00b) & !is.na(dependencyratio00b) & !is.na(pctowneroccupied) & 
           is.finite(pop00b) & is.finite(pctnhwhite00b) & is.finite(dependencyratio00b) & is.finite(pctowneroccupied) & 
           !is.na(pcth00b) & !is.na(pctmin00b) & !is.na(pctnhwhite00p) & !is.na(pctmin00p) & !is.na(pcth00p) & !is.na(popgrowth) & 
           !is.na(hpov00p) & !is.na(blackpov00p) & !is.na(minpov00p) & !is.na(nhwhitepov00p) &
           !is.na(recimmgrowth) & !is.na(blackpov00p) & !is.na(hinc00p) & 
           !is.na(hispvap00p) & !is.na(nhwvap00p) & !is.na(minorityvap00p) & 
           !is.na(hispvap00b) & !is.na(nhwvap00b) & !is.na(minorityvap00b)) 

write_csv(aa, "annexedblocks0010dem_pl00_newsample_unincorp.csv") # 84,697








