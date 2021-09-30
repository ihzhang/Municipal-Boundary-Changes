rm(list = ls())

# get environment ready 
setwd("~/Google Drive/Stanford/QE2")

library("stringr")
library("dplyr")
library("stargazer")
library("tidyverse")
library("tidycensus")
library("lme4")

#IF DATA IS TO BE REPLICATED, START HERE ####
#BAS from Census, 2000-2010 
bas <- 
  read.delim(file = "BAS0010.txt", header = T, na = "")

#save only States that are identified
bas <- bas[!is.na(bas$State), ]

#create unique ids for each annexing place
bas$plid <- paste0(sprintf("%02.0f", bas$State), sprintf("%05.0f", bas$Place))
bas %>%
  filter(Action == "Annexation") %>%
  group_by(plid) 

length(unique(bas$plid))
7943/25376

# states for analysis ####
bas_states <- bas[bas$State==1 | bas$State==5 | bas$State==13 | bas$State==22 | bas$State==28 | bas$State==37 | bas$State==45 |
                    bas$State==51 | bas$State==24 | bas$State==38 | bas$State==46 | bas$State==47 | bas$State==21 | bas$State==10,]
bas_states_places <- as.data.frame(unique(bas_states$plid))
names(bas_states_places) <- "plid"
bas_states_places$plid <- as.character(bas_states_places$plid)
rm(bas_states)

astate <- bas %>% 
  filter(Action == "Annexation") %>%
  group_by(State) %>%
  tally()
View(astate)
# some states have way more annexations than others 
hist(astate$n)

# create annexed blocks file ####
# create a file of annexed blocks between 2000 and 2010, defined as: 
# a) blocks in 2010 places that were not in those places in 2000 
# b) blocks that were previously not part of any place and are now in 2010 places 
# this is done place by place; first isolate the place in 2000 and 2010, then compare the blocks in each list
blocks2000 <- read_csv("blocks2000_var.csv")
blocks2010 <- read_csv("2010blocks_converted.csv")

blocks2000$PLACEA <- as.character(blocks2000$PLACEA)
blocks2000$STATEA <- as.character(blocks2000$STATEA)
blocks2000$STATEA <- str_pad(blocks2000$STATEA, 2, side = "left", pad = "0")
blocks2000$PLACEA <- str_pad(blocks2000$PLACEA, 5, side = "left", pad = "0")
blocks2000$plid <- paste0(blocks2000$STATEA, blocks2000$PLACEA)

blocks2010$PLACEA <- as.character(blocks2010$PLACEA)
blocks2010$STATEA <- as.character(blocks2010$STATEA)
blocks2010$STATEA <- str_pad(blocks2010$STATEA, 2, side = "left", pad = "0")
blocks2010$PLACEA <- str_pad(blocks2010$PLACEA, 5, side = "left", pad = "0")
blocks2010$plid <- paste0(blocks2010$STATEA, blocks2010$PLACEA)

blocks2010 <- blocks2010 %>%
  filter(PLACEA!="99999" & !is.na(PLACEA))

annexedblocks <- data.frame()
plids <- unique(blocks2010$plid)
for (i in 1:length(plids)) {
  block00 <- blocks2000[blocks2000$plid==plids[i], ]
  block10 <- blocks2010[blocks2010$plid==plids[i],]
  block <- block10[!block10$GISJOIN %in% block00$GISJOIN,]
  annexedblocks <- rbind(annexedblocks, block)
}

write_csv(annexedblocks, "aa_baseline.csv")
annexedblocks <- read_csv("aa_baseline.csv")

table(is.na(annexedblocks$PLACEA))
annexedblocks <- annexedblocks[annexedblocks$totalr > 0, ]

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
aa <- read_csv("annexedblocks0010.csv")

# contiguous blocks ####
al_contig <- read.csv(file = "Shapefiles_contig/AL_01/AL_contig.csv", sep = ",", header = T, na = "")
al_contig$State <- "01"
ar_contig <- read.csv(file = "Shapefiles_contig/AR_05/AR_contig.csv", sep = ",", header = T, na = "")
ar_contig$State <- "05"
de_contig <- read.csv(file = "Shapefiles_contig/DE_10/DE_contig.csv", sep = ",", header = T, na = "")
de_contig$State <- "10"
ga_contig <- read.csv(file = "Shapefiles_contig/GA_13/GA_contig.csv", sep = ",", header = T, na = "")
ga_contig$State <- "13"
ky_contig <- read.csv(file = "Shapefiles_contig/KY_21/KY_contig.csv", sep = ",", header = T, na = "")
ky_contig$State <- "21"
la_contig <- read.csv(file = "Shapefiles_contig/LA_22/LA_contig.csv", sep = ",", header = T, na = "")
la_contig$State <- "22"
md_contig <- read.csv(file = "Shapefiles_contig/MD_24/MD_contig.csv", sep = ",", header = T, na = "")
md_contig$State <- "24"
ms_contig <- read.csv(file = "Shapefiles_contig/MS_28/MS_contig.csv", sep = ",", header = T, na = "")
ms_contig$State <- "28"
nc_contig <- read.csv(file = "Shapefiles_contig/NC_37/NC_contig.csv", sep = ",", header = T, na = "")
nc_contig$State <- "37"
nd_contig <- read.csv(file = "Shapefiles_contig/ND_38/ND_contig.csv", sep = ",", header = T, na = "")
nd_contig$State <- "38"
sc_contig <- read.csv(file = "Shapefiles_contig/SC_45/SC_contig.csv", sep = ",", header = T, na = "")
sc_contig$State <- "45"
sd_contig <- read.csv(file = "Shapefiles_contig/SD_46/SD_contig.csv", sep = ",", header = T, na = "")
sd_contig$State <- "46"
tn_contig <- read.csv(file = "Shapefiles_contig/TN_47/TN_contig.csv", sep = ",", header = T, na = "")
tn_contig$State <- "47"
va_contig <- read.csv(file = "Shapefiles_contig/VA_51/VA_contig.csv", sep = ",", header = T, na = "")
va_contig$State <- "51"

contigall2000 <- base::rbind(al_contig, ar_contig, de_contig, ga_contig, ky_contig, la_contig, md_contig,
                       ms_contig, nc_contig, nd_contig, nd_contig, sc_contig, sd_contig, tn_contig, va_contig)
rm(al_contig, ar_contig, de_contig, ga_contig, ky_contig, la_contig, md_contig,
   ms_contig, nc_contig, nd_contig, nd_contig, sc_contig, sd_contig, tn_contig, va_contig)

contigall2000$GISJOIN <- as.character(contigall2000$GISJOIN)
write_csv(contigall2000, file = "allcontigblocks.csv")

# identify contiguous blocks and actually annexed blocks in the all-block file ####
contigall2000 <- read_csv("allcontigblocks.csv")
contigall2000 <- contigall2000 %>%
  filter(contigplace!="0" | contigplace!="99999" | !is.na(contigplace)) %>%
  mutate(blkid = paste0(sprintf("%05.0f", FIPSSTCO), sprintf("%06.0f", TRACT2000), sprintf("%04.0f", BLOCK2000)),
         plid = paste0(State, sprintf("%05.0f", contigplace))) %>%
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

write_csv(aa, "annexedblocks0010_base.csv")

#clean up and get ready for Census data ####
rm(list = ls())
aa <- read_csv("annexedblocks0010_base.csv")
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

write_csv(aa, "annexedblocks0010dem_pl00_newsample.csv") # 443,642








