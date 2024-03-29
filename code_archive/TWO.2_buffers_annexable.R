# Create universe of annexable blocks in all states ####
# 400-m buffers (Durst 2018) for every place ##############
# return .csvs for merging with Census data ###############
# Created by: Iris Zhang, Sept 27, 2021 ###################

# updates log ----
# 10/1: note that various files have references to "contiguity" that are 
# outdated from prior analytical attempts relying on contiguity to define 
# annexable. Future versions of code will correct the naming convention. 
# 5/16/2023 

library("rgeos")
library("sf")
library("data.table")
library("tidyverse")
library("magrittr")
library("foreach")
library("doParallel")

setwd("~/Google Drive/My Drive/Stanford/QE2")

# 1. 2000 blocks in buffers of 2000 places (for 2000-2007)
# 2. 2007 blocks in buffers of 2007 places (for 2007-2013)
# 3. 2014 blocks in buffers of 2014 places (for 2014-2020)
# 4. 2008 blocks in buffers of 2008 places
# 5. 2009 blocks in buffers of 2009 places
# 6. 2010 blocks in buffers of 2010 places 
# 7. 2011 blocks in buffers of 2011 places
# 8. 2012 blocks in buffers of 2012 places 
# 9. 2015 blocks in buffers of 2015 places
# 10. 2016 blocks in buffers of 2016 places 
# 11. 2017 blocks in buffers of 2017 places 
# 12. 2018 blocks in buffers of 2018 places 
# 13. 2019 blocks in buffers of 2019 places 
# 14. 2020 blocks in buffers of 2020 places 

# 1. 
# find all blocks within 400-m buffer of every place in 2007 ####
# this forms universe of "annexable" blocks 
get_buffers <- function(state_code, year) {
  blocks <- st_read(paste0("SHP_blk_0010/", year, "/", state_code, "/tl_2010_", substr(state_code, 4, 5), "_tabblock", substr(year, 3, 4), ".shp"))
  blocks <- st_transform(blocks, 3488)
  blocks %<>%
    mutate(blkid = paste0(str_pad(as.character(STATEFP10), 2, side = "left", pad = "0"), str_pad(as.character(COUNTYFP10), 3, side = "left", pad = "0"),
                          str_pad(as.character(TRACTCE10), 6, side = "left", pad = "0"), str_pad(as.character(BLOCKCE10), 4, side = "left", pad = "0")))
  
  # should only retain those not already part of a place
  cdps07 <- read_csv("pl2007_cleaned.csv") %>%
    select(Geo_NAME, plid) %>%
    mutate(cdp = ifelse(grepl("CDP|cdp", Geo_NAME), 1, 0)) %>%
    filter(cdp==1)
  
  blocks %<>%
    left_join(blocks2010 %>% dplyr::select(blkid, PLACEA, plid), 
              by = "blkid") %>% 
    filter(is.na(PLACEA) | PLACEA=="99999" | plid %in% cdps07$plid) 
  rm(blocks2010)
  
  places <- st_read(paste0("SHP_pl/", year, "/", state_code, "/tl_2010_", substr(state_code, 4, 5), "_place", substr(year, 3, 4), ".shp"))
  places <- st_transform(places, 3488)

  places %<>% 
    mutate(PLACE = as.character(.[[2]])) %>%
    filter(!is.na(PLACE) & PLACE != "99999" & PLACE != "999") %>%
    mutate(plid = paste0(str_pad(as.character(.[[1]]), 2, side = "left", pad = "0"), str_pad(as.character(.[[2]]), 5, side = "left", pad = "0"))) %>%
    filter(!(plid %in% cdps07$plid))
  rm(cdps07)
  datalist <- list()
  places_df <- split(places, f = places$plid)
  cl <- makeCluster(3)
  registerDoParallel(cl)
  getDoParWorkers()

  datalist <- foreach (i = 1:length(places_df), 
           .packages = c("sf", "dplyr", "data.table", "readr", "magrittr")) %dopar% {
    p1buffer <- sf::st_buffer(places_df[[i]], 400)
    p1buffer_intersects <- sf::st_intersects(p1buffer, blocks)
    if(nrow(as.data.frame(blocks[p1buffer_intersects[[1]],])) < 1) return(NULL)
    test <- as.data.frame(blocks[p1buffer_intersects[[1]],])
    test$bufferplace <- places_df[[i]]$plid[[1]]
    test %<>% 
      select(c(1:4), blkid, bufferplace)
    return(test)
           }
  
  buffers <- data.table::rbindlist(datalist) 
  readr::write_csv(buffers, file = paste0("SHP_blk_0010/", year, "/", state_code, "/", substr(state_code, 1, 2), "_buffers.csv"))
  stopCluster(cl)
  rm(list=ls(name=foreach:::.foreachGlobals), pos=foreach:::.foreachGlobals)
  
  }

state_codes <- c("AL_01", "AS_02", "AR_05", "AZ_04", "CA_06", "CO_08", "CT_09", 
                 "DE_10", "FL_12", "GA_13", "HI_15", "IA_19", "ID_16", "IL_17", "IN_18",
                 "KS_20", "KY_21", "LA_22", 
                 "MA_25", "MD_24", "ME_23", "MI_26", "MN_27", "MS_28", "MO_29", "MT_30", 
                 "NC_37", "ND_38", "NE_31", "NH_33", "NJ_34", "NM_35", "NV_32", "NY_36",
                 "OH_39", "OK_40", "OR_41", "PA_42", "RI_44",
                 "SC_45", "SD_46", "TN_47", "TX_48", "UT_49", "VT_50", "VA_51",
                 "WA_53", "WV_54", "WI_55", "WY_56"
                 )

for (state_code in state_codes) {
  start_time <- Sys.time()
  print(start_time)
  get_buffers(state_code, 2000)
  end_time <- Sys.time()
  print(state_code)
  print(end_time - start_time)
  print(end_time)
  Sys.sleep(5)
}

# get buffers for 2014 #### 
# first have to filter out by plid; we want is.na or 99999 or cdp only 
# note sometimes tigris will fail, likely because of a firewall block on repeated calls
# so the function still includes an optional line to read the file in from a local path 
# switch the lines on and off using # 

get_buffers_14 <- function(state_code) {
  blocks <- st_read(paste0("SHP_blk_0010/2014/", state_code, "/tl_2014_", substr(state_code, 4, 5), "_tabblock10.shp"))
  #blocks <- tigris::blocks(state = substr(state_code, 4, 5), year = 2014)
  blocks <- st_transform(blocks, 3488)
  blocks %<>%
    mutate(blkid = paste0(str_pad(as.character(STATEFP10), 2, side = "left", pad = "0"), 
                          str_pad(as.character(COUNTYFP10), 3, side = "left", pad = "0"),
                          str_pad(as.character(TRACTCE10), 6, side = "left", pad = "0"), 
                          str_pad(as.character(BLOCKCE10), 4, side = "left", pad = "0")))
  
  # should only retain those not already part of a place
  plid_list <- read_csv(file = paste0("SHP_blk_0010/2014/", state_code, "/", substr(state_code, 1, 2), "_block_plids.csv"))
  plid_list %<>% 
    select(blkid, plid) %>%
    mutate(blkid = as.character(blkid))
  cdps <- read_csv("places2014_cleaned.csv") %>%
    select(Geo_NAME, plid) %>%
    mutate(cdp = ifelse(grepl("CDP|cdp", Geo_NAME), 1, 0)) %>%
    filter(cdp==1)
  blocks %<>%
    left_join(plid_list, by = "blkid") %>% 
    filter((is.na(plid) | plid %in% cdps$plid))
  rm(plid_list)
  
  # place shapefile
  #places <- tigris::places(state = substr(state_code, 4, 5), year = 2014)
  places <- st_read(paste0("SHP_pl/2014/", state_code, "/tl_2014_", substr(state_code, 4, 5), "_place.shp"))
  places <- st_transform(places, 3488)
  places %<>% 
    mutate(plid = paste0(
      str_pad(as.character(STATEFP), 2, side = "left", pad = "0"), 
      str_pad(as.character(PLACEFP), 5, side = "left", pad = "0"))) 
  
  datalist <- list()
  places_df <- split(places, f = places$plid)
  # I have 8 cores; using anything more than 5 will crash computer if using multiple apps
  
  cl <- makeCluster(3) 
  registerDoParallel(cl)
  getDoParWorkers()
  
  datalist <- foreach (i = 1:length(places_df), 
                       .packages = c("sf", "dplyr", "data.table", "readr", "magrittr")) %dopar% {
                         p1buffer <- sf::st_buffer(st_as_sf(places_df[[i]]), 400)
                         p1buffer_intersects <- sf::st_intersects(p1buffer, blocks)
                         if(nrow(as.data.frame(blocks[p1buffer_intersects[[1]],])) < 1) return(NULL)
                         test <- as.data.frame(blocks[p1buffer_intersects[[1]],])
                         test$bufferplace <- places_df[[i]]$plid[[1]]
                         test %<>% 
                           select(c(1:4), blkid, bufferplace)
                         return(test)
                       }
  
  buffers <- data.table::rbindlist(datalist) 
  readr::write_csv(buffers, file = paste0("SHP_blk_0010/2014/", state_code, "/", substr(state_code, 1, 2), "_buffers.csv"))
  stopCluster(cl)
  rm(list=ls(name=foreach:::.foreachGlobals), pos=foreach:::.foreachGlobals)
}

state_codes <- c("AL_01", "AS_02", "AR_05", "AZ_04", "CA_06", "CO_08", "CT_09", 
                 "DE_10", "FL_12", "GA_13", "HI_15", "IA_19", "ID_16", "IL_17", "IN_18",
                 "KS_20", "KY_21", "LA_22", 
                 "MA_25", "MD_24", "ME_23", "MI_26", "MN_27", "MS_28", "MO_29", "MT_30", 
                 "NC_37", "ND_38", "NE_31", "NH_33", "NJ_34", "NM_35", "NV_32", "NY_36",
                 "OH_39", "OK_40", "OR_41", "PA_42", "RI_44",
                 "SC_45", "SD_46", "TN_47", "TX_48", "UT_49", "VT_50", "VA_51",
                 "WA_53", "WV_54", "WI_55", "WY_56"
)

for (state_code in state_codes) {
  start_time <- Sys.time()
  print(start_time)
  get_buffers_14(state_code)
  end_time <- Sys.time()
  print(state_code)
  print(end_time - start_time)
  print(end_time)
  Sys.sleep(60)
}

# make buffer file for 2000, 2007, and 2014 ####
state_codes <- c("AL_01", "AS_02", "AR_05", "AZ_04", "CA_06", "CO_08", 
                 "DE_10", "FL_12", "GA_13", "IA_19", "ID_16", "IL_17", "IN_18",
                 "KS_20", "KY_21", "LA_22", "MD_24",
                 "MI_26", "MN_27", "MS_28", "MO_29", "MT_30", 
                 "NC_37", "ND_38", "NE_31", "NM_35", "NV_32", 
                 "OH_39", "OK_40", "OR_41", 
                 "SC_45", "SD_46", "TN_47", "TX_48", "UT_49", "VA_51",
                 "WA_53", "WV_54", "WI_55", "WY_56"
)

blocks2000 <- read_csv("blocks2000_var.csv")
blocks2000 %<>%
  mutate(plid = ifelse(PLACEA=="99999", NA, 
                       paste0(str_pad(STATEA, 2, "left", "0"), 
                              str_pad(PLACEA, 5, "left", "0")))) %>%
  select(plid, blkid)
blocks2000 %<>%
  filter(!duplicated(blkid)) 

blocks2000 %<>%
  filter(!is.na(plid))

blocks_list <- list()
for(i in 1:length(state_codes)) {
  blocks <- list.files(paste0("SHP_blk_0010/2000/", state_codes[[i]], "/"), pattern = "buffers.csv")
  block_file <- read_csv(file = paste0("SHP_blk_0010/2000/", state_codes[[i]], "/", blocks))
  block_file %<>%
    filter(!is.na(bufferplace))
  plid_list <- unique(block_file$bufferplace)
  for (j in 1:length(plid_list)) {
    blocklist <- blocks2000 %>%
      filter(plid == plid_list[[j]]) 
    block_file %<>%
      filter(!blkid %in% blocklist$blkid)
  }
  blocks_list[[i]] <- block_file
}
blocks_list <- rbindlist(blocks_list)
write_csv(blocks_list, "2000buffers.csv")
rm(list = ls())

# 2007 ----
blocks2007 <- read_csv("2007blk-2007plid_90pct.csv")
blocks2007 %<>%
 select(plid, blkid)

blocks2007 %<>%
  filter(!duplicated(blkid)) 

blocks2007 %<>%
  filter(!is.na(plid))

blocks_list <- list()
for(i in 28:length(state_codes)) {
  blocks <- list.files(paste0("spatial_files/2007/"), pattern = paste0(substr(state_codes[[i]], 1, 2), "_buffers.csv"), full.names = T)
  block_file <- read_csv(blocks)
  block_file %<>%
    filter(!is.na(bufferplace))
  plid_list <- unique(block_file$bufferplace)
  for (j in 1:length(plid_list)) {
    blocklist <- blocks2007 %>%
      filter(plid == plid_list[[j]]) 
    block_file %<>%
      filter(!blkid %in% blocklist$blkid)
  }
  blocks_list[[i]] <- block_file
}
blocks_list <- rbindlist(blocks_list, fill = T)
write_csv(blocks_list, "2007buffers.csv")

rm(blocks2007, blocks_list)

# 2008 ----
state_codes <- c("AL_01", "AS_02", "AR_05", "AZ_04", "CA_06", "CO_08", 
                 "DE_10", "FL_12", "GA_13", "IA_19", "ID_16", "IL_17", "IN_18",
                 "KS_20", "KY_21", "LA_22", "MD_24",
                 "MI_26", "MN_27", "MS_28", "MO_29", "MT_30", 
                 "NC_37", "ND_38", "NE_31", "NM_35", "NV_32", 
                 "OH_39", "OK_40", "OR_41", 
                 "SC_45", "SD_46", "TN_47", "TX_48", "UT_49", "VA_51",
                 "WA_53", "WV_54", "WI_55", "WY_56"
)

blocks2008 <- read_csv("2008blk-2008plid_90pct.csv")
blocks2008 %<>%
  select(plid, blkid)

blocks2008 %<>%
  filter(!duplicated(blkid)) 

blocks2008 %<>%
  filter(!is.na(plid))

blocks_list <- list()
for(i in 1:length(state_codes)) {
  blocks <- list.files(paste0("spatial_files/2008/"), pattern = paste0(substr(state_codes[[i]], 1, 2), "_buffers.csv"), full.names = T)
  block_file <- read_csv(blocks)
  block_file %<>%
    filter(!is.na(bufferplace))
  plid_list <- unique(block_file$bufferplace)
  for (j in 1:length(plid_list)) {
    blocklist <- blocks2008 %>%
      filter(plid == plid_list[[j]]) 
    block_file %<>%
      filter(!blkid %in% blocklist$blkid)
  }
  blocks_list[[i]] <- block_file
}
blocks_list <- rbindlist(blocks_list, fill = T)
write_csv(blocks_list, "2008buffers.csv")

rm(blocks2008, blocks_list)

# 2009 ----
state_codes <- c("AL_01", "AS_02", "AR_05", "AZ_04", "CA_06", "CO_08", 
                 "DE_10", "FL_12", "GA_13", "HI_15", "IA_19", "ID_16", "IL_17", "IN_18",
                 "KS_20", "KY_21", "LA_22", "MD_24",
                 "MI_26", "MN_27", "MS_28", "MO_29", "MT_30", 
                 "NC_37", "ND_38", "NE_31", "NM_35", "NV_32", 
                 "OH_39", "OK_40", "OR_41", 
                 "SC_45", "SD_46", "TN_47", "TX_48", "UT_49", "VA_51",
                 "WA_53", "WV_54", "WI_55", "WY_56"
)

blocks2009 <- read_csv("2009blk-2009plid_90pct.csv")
blocks2009 %<>%
  select(plid, blkid)

blocks2009 %<>%
  filter(!duplicated(blkid)) 

blocks2009 %<>%
  filter(!is.na(plid))

blocks_list <- list()
for(i in 1:length(state_codes)) {
  blocks <- list.files(paste0("spatial_files/2010/"), pattern = paste0(substr(state_codes[[i]], 1, 2), "_buffers.csv"), full.names = T)
  block_file <- read_csv(blocks)
  block_file %<>%
    filter(!is.na(bufferplace))
  plid_list <- unique(block_file$bufferplace)
  for (j in 1:length(plid_list)) {
    blocklist <- blocks2009 %>%
      filter(plid == plid_list[[j]]) 
    block_file %<>%
      filter(!blkid %in% blocklist$blkid)
  }
  blocks_list[[i]] <- block_file
}
blocks_list <- rbindlist(blocks_list, fill = T)
write_csv(blocks_list, "2009buffers.csv")

rm(blocks2009, blocks_list)

# 2010 ----
state_codes <- c("AL_01", "AS_02", "AR_05", "AZ_04", "CA_06", "CO_08", 
                 "DE_10", "FL_12", "GA_13", "HI_15", "IA_19", "ID_16", "IL_17", "IN_18",
                 "KS_20", "KY_21", "LA_22", "MD_24",
                 "MI_26", "MN_27", "MS_28", "MO_29", "MT_30", 
                 "NC_37", "ND_38", "NE_31", "NM_35", "NV_32", 
                 "OH_39", "OK_40", "OR_41", 
                 "SC_45", "SD_46", "TN_47", "TX_48", "UT_49", "VA_51",
                 "WA_53", "WV_54", "WI_55", "WY_56"
)

blocks2010 <- read_csv("2010blk-2010plid_90pct.csv")
blocks2010 %<>%
  select(plid, blkid) %>%
  filter(!duplicated(blkid) & !is.na(plid)) 

blocks_list <- list()
for(i in 1:length(state_codes)) {
  blocks <- list.files(paste0("SHP_pl_0010/2010/", states_code[[i]], "/"), pattern = paste0(substr(state_codes[[i]], 1, 2), "_buffers.csv"), full.names = T)
  block_file <- read_csv(blocks)
  block_file %<>%
    filter(!is.na(bufferplace))
  plid_list <- unique(block_file$bufferplace)
  for (j in 1:length(plid_list)) {
    blocklist <- blocks2010 %>%
      filter(plid == plid_list[[j]]) 
    block_file %<>%
      filter(!blkid %in% blocklist$blkid)
  }
  blocks_list[[i]] <- block_file
}
blocks_list <- rbindlist(blocks_list, fill = T)
write_csv(blocks_list, "2010buffers.csv")

rm(blocks2010, blocks_list)

# 2011 ----
state_codes <- c("AL_01", "AS_02", "AR_05", "AZ_04", "CA_06", "CO_08", 
                 "DE_10", "FL_12", "GA_13", "HI_15", "IA_19", "ID_16", "IL_17", "IN_18",
                 "KS_20", "KY_21", "LA_22", "MD_24",
                 "MI_26", "MN_27", "MS_28", "MO_29", "MT_30", 
                 "NC_37", "ND_38", "NE_31", "NM_35", "NV_32", 
                 "OH_39", "OK_40", "OR_41", 
                 "SC_45", "SD_46", "TN_47", "TX_48", "UT_49", "VA_51",
                 "WA_53", "WV_54", "WI_55", "WY_56"
)

blocks2011 <- read_csv("2011blk-2011plid_90pct.csv")
blocks2011 %<>%
  select(plid, blkid) %>%
  filter(!duplicated(blkid) & !is.na(plid)) 

blocks_list <- list()
for(i in 1:length(state_codes)) {
  blocks <- list.files(paste0("spatial_files/2011/"), pattern = paste0(substr(state_codes[[i]], 1, 2), "_buffers.csv"), full.names = T)
  block_file <- read_csv(blocks)
  block_file %<>%
    filter(!is.na(bufferplace))
  plid_list <- unique(block_file$bufferplace)
  for (j in 1:length(plid_list)) {
    blocklist <- blocks2011 %>%
      filter(plid == plid_list[[j]]) 
    block_file %<>%
      filter(!blkid %in% blocklist$blkid)
  }
  blocks_list[[i]] <- block_file
}
blocks_list <- rbindlist(blocks_list, fill = T)
write_csv(blocks_list, "2011buffers.csv")

rm(blocks2011, blocks_list)

# 2012 ----
state_codes <- c("AL_01", "AS_02", "AR_05", "AZ_04", "CA_06", "CO_08", 
                 "DE_10", "FL_12", "GA_13", "HI_15", "IA_19", "ID_16", "IL_17", "IN_18",
                 "KS_20", "KY_21", "LA_22", "MD_24",
                 "MI_26", "MN_27", "MS_28", "MO_29", "MT_30", 
                 "NC_37", "ND_38", "NE_31", "NM_35", "NV_32", 
                 "OH_39", "OK_40", "OR_41", 
                 "SC_45", "SD_46", "TN_47", "TX_48", "UT_49", "VA_51",
                 "WA_53", "WV_54", "WI_55", "WY_56"
)

blocks2012 <- read_csv("2012blk-2012plid_90pct.csv")
blocks2012 %<>%
  select(plid, blkid) %>%
  filter(!duplicated(blkid) & !is.na(plid)) 

blocks_list <- list()
for(i in 1:length(state_codes)) {
  blocks <- list.files(paste0("spatial_files/2012/"), pattern = paste0(substr(state_codes[[i]], 1, 2), "_buffers.csv"), full.names = T)
  block_file <- read_csv(blocks)
  block_file %<>%
    filter(!is.na(bufferplace))
  plid_list <- unique(block_file$bufferplace)
  for (j in 1:length(plid_list)) {
    blocklist <- blocks2012 %>%
      filter(plid == plid_list[[j]]) 
    block_file %<>%
      filter(!blkid %in% blocklist$blkid)
  }
  blocks_list[[i]] <- block_file
}
blocks_list <- rbindlist(blocks_list, fill = T)
write_csv(blocks_list, "2012buffers.csv")

rm(blocks2012, blocks_list)

# 2014 ----
blocks2014 <- read_csv("2014blk-2014plid_90pct.csv")
blocks2014 %<>%
  select(plid, blkid)

blocks2014 %<>%
  filter(!duplicated(blkid)) 

blocks2014 %<>%
  filter(!is.na(plid))

blocks_list <- list()
for(i in 1:length(state_codes)) {
  blocks <- list.files(paste0("SHP_blk_0010/2014/", state_codes[[i]], "/"), pattern = "_buffers.csv")
  block_file <- read_csv(file = paste0("SHP_blk_0010/2014/", state_codes[[i]], "/", blocks))
  block_file %<>%
    filter(!is.na(bufferplace))
  plid_list <- unique(block_file$bufferplace)
  for (j in 1:length(plid_list)) {
    blocklist <- blocks2014 %>%
      filter(plid == plid_list[[j]]) 
    block_file %<>%
      filter(!blkid %in% blocklist$blkid)
  }
  blocks_list[[i]] <- block_file
}
blocks_list <- rbindlist(blocks_list, fill = T)
write_csv(blocks_list, "2014buffers.csv")

# 2015 ----
state_codes <- c("AL_01", "AS_02", "AR_05", "AZ_04", "CA_06", "CO_08", 
                 "DE_10", "FL_12", "GA_13", "HI_15", "IA_19", "ID_16", "IL_17", "IN_18",
                 "KS_20", "KY_21", "LA_22", "MD_24",
                 "MI_26", "MN_27", "MS_28", "MO_29", "MT_30", 
                 "NC_37", "ND_38", "NE_31", "NM_35", "NV_32", 
                 "OH_39", "OK_40", "OR_41", 
                 "SC_45", "SD_46", "TN_47", "TX_48", "UT_49", "VA_51",
                 "WA_53", "WV_54", "WI_55", "WY_56"
)

blocks2015 <- read_csv("2015blk-2015plid_90pct.csv")
blocks2015 %<>%
  select(plid, blkid) %>%
  filter(!duplicated(blkid) & !is.na(plid)) 

blocks_list <- list()
for(i in 1:length(state_codes)) {
  blocks <- list.files(paste0("spatial_files/2015/"), pattern = paste0(substr(state_codes[[i]], 1, 2), "_buffers.csv"), full.names = T)
  block_file <- read_csv(blocks)
  block_file %<>%
    filter(!is.na(bufferplace))
  plid_list <- unique(block_file$bufferplace)
  for (j in 1:length(plid_list)) {
    blocklist <- blocks2015 %>%
      filter(plid == plid_list[[j]]) 
    block_file %<>%
      filter(!blkid %in% blocklist$blkid)
  }
  blocks_list[[i]] <- block_file
}
blocks_list <- rbindlist(blocks_list, fill = T)
write_csv(blocks_list, "2015buffers.csv")

rm(blocks2015, blocks_list)

# 2016 ----
state_codes <- c("AL_01", "AS_02", "AR_05", "AZ_04", "CA_06", "CO_08", 
                 "DE_10", "FL_12", "GA_13", "HI_15", "IA_19", "ID_16", "IL_17", "IN_18",
                 "KS_20", "KY_21", "LA_22", "MD_24",
                 "MI_26", "MN_27", "MS_28", "MO_29", "MT_30", 
                 "NC_37", "ND_38", "NE_31", "NM_35", "NV_32", 
                 "OH_39", "OK_40", "OR_41", 
                 "SC_45", "SD_46", "TN_47", "TX_48", "UT_49", "VA_51",
                 "WA_53", "WV_54", "WI_55", "WY_56"
)

blocks2016 <- read_csv("2016blk-2016plid_90pct.csv")
blocks2016 %<>%
  select(plid, blkid) %>%
  filter(!duplicated(blkid) & !is.na(plid)) 

blocks_list <- list()
for(i in 1:length(state_codes)) {
  blocks <- list.files(paste0("spatial_files/2016/"), pattern = paste0(substr(state_codes[[i]], 1, 2), "_buffers.csv"), full.names = T)
  block_file <- read_csv(blocks)
  block_file %<>%
    filter(!is.na(bufferplace))
  plid_list <- unique(block_file$bufferplace)
  for (j in 1:length(plid_list)) {
    blocklist <- blocks2016 %>%
      filter(plid == plid_list[[j]]) 
    block_file %<>%
      filter(!blkid %in% blocklist$blkid)
  }
  blocks_list[[i]] <- block_file
}
blocks_list <- rbindlist(blocks_list, fill = T)
write_csv(blocks_list, "2016buffers.csv")

rm(blocks2016, blocks_list)

# 2017 ----
state_codes <- c("AL_01", "AS_02", "AR_05", "AZ_04", "CA_06", "CO_08", 
                 "DE_10", "FL_12", "GA_13", "HI_15", "IA_19", "ID_16", "IL_17", "IN_18",
                 "KS_20", "KY_21", "LA_22", "MD_24",
                 "MI_26", "MN_27", "MS_28", "MO_29", "MT_30", 
                 "NC_37", "ND_38", "NE_31", "NM_35", "NV_32", 
                 "OH_39", "OK_40", "OR_41", 
                 "SC_45", "SD_46", "TN_47", "TX_48", "UT_49", "VA_51",
                 "WA_53", "WV_54", "WI_55", "WY_56"
)

blocks2017 <- read_csv("2017blk-2017plid_90pct.csv")
blocks2017 %<>%
  select(plid, blkid) %>%
  filter(!duplicated(blkid) & !is.na(plid)) 

blocks_list <- list()
for(i in 1:length(state_codes)) {
  blocks <- list.files(paste0("spatial_files/2017/"), pattern = paste0(substr(state_codes[[i]], 1, 2), "_buffers.csv"), full.names = T)
  block_file <- read_csv(blocks)
  block_file %<>%
    filter(!is.na(bufferplace))
  plid_list <- unique(block_file$bufferplace)
  for (j in 1:length(plid_list)) {
    blocklist <- blocks2017 %>%
      filter(plid == plid_list[[j]]) 
    block_file %<>%
      filter(!blkid %in% blocklist$blkid)
  }
  blocks_list[[i]] <- block_file
}
blocks_list <- rbindlist(blocks_list, fill = T)
write_csv(blocks_list, "2017buffers.csv")

rm(blocks2017, blocks_list)

# 2018 ----
state_codes <- c("AL_01", "AS_02", "AR_05", "AZ_04", "CA_06", "CO_08", 
                 "DE_10", "FL_12", "GA_13", "HI_15", "IA_19", "ID_16", "IL_17", "IN_18",
                 "KS_20", "KY_21", "LA_22", "MD_24",
                 "MI_26", "MN_27", "MS_28", "MO_29", "MT_30", 
                 "NC_37", "ND_38", "NE_31", "NM_35", "NV_32", 
                 "OH_39", "OK_40", "OR_41", 
                 "SC_45", "SD_46", "TN_47", "TX_48", "UT_49", "VA_51",
                 "WA_53", "WV_54", "WI_55", "WY_56"
)

blocks2018 <- read_csv("2018blk-2018plid_90pct.csv")
blocks2018 %<>%
  select(plid, blkid) %>%
  filter(!duplicated(blkid) & !is.na(plid)) 

blocks_list <- list()
for(i in 1:length(state_codes)) {
  blocks <- list.files(paste0("spatial_files/2018/"), pattern = paste0(substr(state_codes[[i]], 1, 2), "_buffers.csv"), full.names = T)
  block_file <- read_csv(blocks)
  block_file %<>%
    filter(!is.na(bufferplace))
  plid_list <- unique(block_file$bufferplace)
  for (j in 1:length(plid_list)) {
    blocklist <- blocks2018 %>%
      filter(plid == plid_list[[j]]) 
    block_file %<>%
      filter(!blkid %in% blocklist$blkid)
  }
  blocks_list[[i]] <- block_file
}
blocks_list <- rbindlist(blocks_list, fill = T)
write_csv(blocks_list, "2018buffers.csv")

rm(blocks2018, blocks_list)

# 2019 ----
state_codes <- c("AL_01", "AS_02", "AR_05", "AZ_04", "CA_06", "CO_08", 
                 "DE_10", "FL_12", "GA_13", "HI_15", "IA_19", "ID_16", "IL_17", "IN_18",
                 "KS_20", "KY_21", "LA_22", "MD_24",
                 "MI_26", "MN_27", "MS_28", "MO_29", "MT_30", 
                 "NC_37", "ND_38", "NE_31", "NM_35", "NV_32", 
                 "OH_39", "OK_40", "OR_41", 
                 "SC_45", "SD_46", "TN_47", "TX_48", "UT_49", "VA_51",
                 "WA_53", "WV_54", "WI_55", "WY_56"
)

blocks2019 <- read_csv("2019blk-2019plid_90pct.csv")
blocks2019 %<>%
  select(plid, blkid) %>%
  filter(!duplicated(blkid) & !is.na(plid)) 

blocks_list <- list()
for(i in 1:length(state_codes)) {
  blocks <- list.files(paste0("spatial_files/2019/"), pattern = paste0(substr(state_codes[[i]], 1, 2), "_buffers.csv"), full.names = T)
  block_file <- read_csv(blocks)
  block_file %<>%
    filter(!is.na(bufferplace))
  plid_list <- unique(block_file$bufferplace)
  for (j in 1:length(plid_list)) {
    blocklist <- blocks2019 %>%
      filter(plid == plid_list[[j]]) 
    block_file %<>%
      filter(!blkid %in% blocklist$blkid)
  }
  blocks_list[[i]] <- block_file
}
blocks_list <- rbindlist(blocks_list, fill = T)
write_csv(blocks_list, "2019buffers.csv")

rm(blocks2019, blocks_list)

# NEW ANNEX SCHEME ####
# 2000 blocks (2010b) with 2000 place (2010b) shapefiles; 2000 blocks (2010b) with 2007 (2000b) place shapefiles
# 2007 blocks (2000b) with 2007 (2000b) place shapefiles (done); 2007 (2000b) blocks with 2013 (2010b) place shapefiles
# 2014 blocks (2010b) with 2014 place (2010b) shapefiles; 2014 blocks (2010b) with 2020 (2020b) place shapefiles 

state_codes <- c("AL_01", "AR_05", "AZ_04", "CA_06", "CO_08", 
                 "DE_10", "FL_12", "GA_13", "IA_19", "ID_16", "IL_17", "IN_18",
                 "KS_20", "KY_21", "LA_22", 
                 "MI_26", "MN_27", "MS_28", "MO_29", "MT_30", 
                 "NC_37", "ND_38", "NE_31", "NM_35", "NV_32", 
                 "OH_39", "OK_40", "OR_41", 
                 "SC_45", "SD_46", "TN_47", "TX_48", "UT_49", "VA_51",
                 "WA_53", "WV_54", "WI_55", "WY_56"
)

# 2000 blocks on 2000 places ####
get_block_ids_2000 <- function (state_code, place_year) {
  blocks <- st_read(paste0("SHP_blk_0010/2000/", state_code, "/tl_2010_", substr(state_code, 4, 5), "_tabblock00.shp")) %>%
    st_transform(., 3488)
  blocks %<>%
    mutate(blkid = paste0(str_pad(as.character(STATEFP00), 2, side = "left", pad = "0"), str_pad(as.character(COUNTYFP00), 3, side = "left", pad = "0"),
                          str_pad(as.character(TRACTCE00), 6, side = "left", pad = "0"), str_pad(as.character(BLOCKCE00), 4, side = "left", pad = "0")))
  
  place_file <- ifelse(place_year == 2000, paste0("SHP_pl/2000/", state_code, "/tl_2010_", substr(state_code, 4, 5), "_place00.shp"),
                       paste0("SHP_pl/2007/fe_2007_", substr(state_code, 4, 5), "_place.shp")
  )
  places <- st_read(place_file) %>%
    st_transform(., 3488)
  
places %<>%
    mutate(plid = paste0(
    str_pad(as.character(.[[1]]), 2, side = "left", pad = "0"), 
    str_pad(as.character(.[[2]]), 5, side = "left", pad = "0")))
  
  datalist <- list()
  places_df <- split(places, f = unique(places$plid))
  cl <- makeCluster(3) 
  registerDoParallel(cl)
  getDoParWorkers()
  
  datalist <- foreach (i = 1:length(places_df), 
                       .packages = c("sf", "dplyr", "data.table", "readr", "magrittr")) %dopar% {
    p1 <- st_as_sf(places_df[[i]])
    p1blocks <- c(sf::st_contains(p1, blocks), sf::st_overlaps(p1, blocks))
    if(nrow(as.data.frame(blocks[unique(c(p1blocks[[1]], p1blocks[[2]])),])) < 1) return(NULL)
      test <- as.data.frame(blocks[unique(c(p1blocks[[1]], p1blocks[[2]])),])
      test$plid <- p1$plid
      test %<>% 
        select(c(1:6), blkid, plid)
      return(test)
    }
  contig <- data.table::rbindlist(datalist) 
  readr::write_csv(contig, file = paste0("spatial_files/2000/", state_code, "_block_plids_2000blk-", place_year, "pl.csv"))
  
  stopCluster(cl)
  rm(list=ls(name=foreach:::.foreachGlobals), pos=foreach:::.foreachGlobals)
}

for (state_code in state_codes) {
  start_time <- Sys.time()
  print(start_time)
  get_block_ids_2000(state_code, 2000)
  print(state_code)
  end_time <- Sys.time()
  print(end_time - start_time)
  start_time <- Sys.time()
  print(start_time)
  get_block_ids_2000(state_code, 2007)
  end_time <- Sys.time()
  print(state_code)
  print(end_time - start_time)

  print(end_time)
  Sys.sleep(60)
}
