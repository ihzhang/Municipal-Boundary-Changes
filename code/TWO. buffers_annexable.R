# Create universe of annexable blocks in the 14 states ####
# 400-m buffers (Durst 2018) for every place ##############
# return .csvs for merging with Census data ###############
# Created by: Iris Zhang, Sept 27, 2021 ###################

# updates log ----
# 10/1: note that various files have references to "contiguity" that are 
# outdated from prior analytical attempts relying on contiguity to define 
# annexable. Future versions of code will correct the naming convention. 

library("rgeos")
library("sf")
library("data.table")
library("tidyverse")
library("magrittr")
library("crsuggest")
library("tigris")

setwd("~/Google Drive/My Drive/Stanford/QE2")

# 1. 2000 blocks in buffers of 2000 places 
# 2. 2010 blocks in buffers of 2010 places 
# 3. 2014 blocks in buffers of 2014 places

# 1. 
# find all blocks within 400-m buffer of every place in 2000 
# this forms universe of "annexable" blocks 
blocks_list <- list()
blocks_list[[1]] <- fread("ipumsblocks_allstates/2000blocks/nhgis0032_ds147_2000_block.csv", select = 
                      c("GISJOIN", "STATEA", "COUNTYA", "TRACTA", "BLOCKA", "PLACEA"))
blocks_list[[1]] <- blocks_list[[1]][-1, ]

blocks_list[[2]] <- fread("ipumsblocks_allstates/2010blocks/nhgis0036_ds172_2010_block.csv", select = 
                            c("GISJOIN", "STATEA", "COUNTYA", "TRACTA", "BLOCKA", "PLACEA"))
years <- c("2000", "2010")

for(i in 1:length(years)) {
blocks_list[[i]] <- blocks_list[[i]] %>%
  mutate(blkid = paste0(str_pad(STATEA, 2, side = "left", pad = "0"), str_pad(COUNTYA, 3, side = "left", pad = "0"),
                        str_pad(TRACTA, 6, side = "left", pad = "0"), str_pad(BLOCKA, 4, side = "left", pad = "0")))
}

get_buffers <- function(state_code, year) {
  blocks <- st_read(paste0("SHP_blk_0010/", year, "/", state_code, "/tl_2010_", substr(state_code, 4, 5), "_tabblock", substr(year, 3, 4), ".shp"))
  blocks <- st_transform(blocks, 3488)
  if (year == 2010) {
    blocks <- blocks %>%
      mutate(blkid = paste0(str_pad(as.character(STATEFP10), 2, side = "left", pad = "0"), str_pad(as.character(COUNTYFP10), 3, side = "left", pad = "0"),
                            str_pad(as.character(TRACTCE10), 6, side = "left", pad = "0"), str_pad(as.character(BLOCKCE10), 4, side = "left", pad = "0")))
  } else {
    blocks <- blocks %>%
      mutate(blkid = as.character(BLKIDFP00))
  }
  
  # should only retain those not already part of a place
  blocks <- blocks %>%
    left_join(blocks_list[[which(years==year)]] %>% dplyr::select(blkid, PLACEA, GISJOIN), 
              by = "blkid") %>% 
    filter(is.na(PLACEA) | PLACEA=="99999") 
  
  places <- st_read(paste0("SHP_pl/", year, "/", state_code, "/tl_2010_", substr(state_code, 4, 5), "_place", substr(year, 3, 4), ".shp"))
  places <- st_transform(places, 3488)
  places <- places %>% 
    mutate(PLACE = as.character(.[[2]])) %>%
    filter(!is.na(PLACE) & PLACE != "99999" & PLACE != "999") %>%
    mutate(plid = paste0(str_pad(as.character(.[[1]]), 2, side = "left", pad = "0"), str_pad(as.character(.[[2]]), 5, side = "left", pad = "0")))
  
  datalist <- list()
  places_df <- split(places, f = places$plid)
  cl <- makeCluster(detectCores())
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

# 2. 2010-2020 ####
for (state_code in state_codes) {
  start_time <- Sys.time()
  print(start_time)
  get_buffers(state_code, 2010)
  end_time <- Sys.time()
  print(state_code)
  print(end_time - start_time)
  print(end_time)
  Sys.sleep(5)
}

# get place-to-block crosswalk for 2013 ####
# 2013 blocks and their 2013 place id 
# 2014, their 2014 place id and their contiguous blocks 
get_block_ids <- function (state_code, year) {
    blocks <- st_read(paste0("SHP_blk_0010/", year, "/", state_code, "/tl_", year, "_", substr(state_code, 4, 5), "_tabblock.shp"))
    blocks <- st_transform(blocks, 3488)
    blocks %<>%
        mutate(blkid = paste0(str_pad(as.character(STATEFP10), 2, side = "left", pad = "0"), str_pad(as.character(COUNTYFP10), 3, side = "left", pad = "0"),
                              str_pad(as.character(TRACTCE10), 6, side = "left", pad = "0"), str_pad(as.character(BLOCKCE10), 4, side = "left", pad = "0")))
    
    places <- st_read(paste0("SHP_pl/", year, "/", state_code, "/tl_", year, "_", substr(state_code, 4, 5), "_place.shp"))
    places <- st_transform(places, 3488)
    
    places %<>% 
        mutate(plid = paste0(
            str_pad(as.character(STATEFP), 2, side = "left", pad = "0"), 
            str_pad(as.character(PLACEFP), 5, side = "left", pad = "0")))
    
    datalist <- list()
    for (i in 1:length(unique(places$plid))) {
        p1 <- places[i,]
        p1blocks <- st_contains(p1, blocks)
        if(nrow(as.data.frame(blocks[p1blocks[[1]],])) < 1) {
            next
        } else {
            test <- as.data.frame(blocks[p1blocks[[1]],])
            test$plid <- p1$plid
            datalist[[i]] <- test %>% 
                select(c(1:6), blkid, plid, GEOID)
        }
    }
    non.null.list <- lapply(datalist, Filter, f = Negate(is.null))
    rm(datalist)
    contig <- plyr::rbind.fill(lapply(non.null.list, as.data.frame))
    write_csv(contig, file = paste0("SHP_blk_0010/", year, "/", state_code, "/", substr(state_code, 1, 2), "_block_plids.csv"))
}

# blocks <- st_read(paste0("SHP_blk_0010/", "2014", "/", "AL_01", "/tl_2014_", substr("AL_01", 4, 5), "_tabblock10.shp"))
# blocks <- st_transform(blocks, 3488)
# places <- st_read(paste0("SHP_pl/", "2014", "/", "AL_01", "/tl_2014_", substr("AL_01", 4, 5), "_place.shp"))
# places <- st_transform(places, 3488)

ggplot() + 
    geom_sf(data = places, fill = "black") +
    geom_sf(data = blocks %>% 
                filter(blkid %in% contig$blkid), fill="#CCFFCC") 

years <- c(2013, 2014)
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
        get_block_ids(state_code, 2014)
    print(state_code)
}

get_block_ids(AL_01, 2014)

# get contiguity for 2014 #### 
# first have to filter out by plid; we want is.na or 99999 only 
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
    blocks %<>%
        left_join(plid_list, by = "blkid") %>% 
        filter(is.na(plid))
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
    foreach (i = 1:length(places_df)) %do% {
      p1buffer <- st_buffer(places_df[[i]], 400)
      p1buffer_intersects <- st_intersects(p1buffer, blocks)
      if(nrow(as.data.frame(blocks[p1buffer_intersects[[1]],])) < 1) return(NULL)
      test <- as.data.frame(blocks[p1buffer_intersects[[1]],])
      test$bufferplace <- places_df[[i]]$plid[[1]]
      datalist[[i]] <- test %>% 
        select(c(1:4), blkid, bufferplace)
    }
    
    non.null.list <- lapply(datalist, Filter, f = Negate(is.null))
    rm(datalist)
    buffers <- plyr::rbind.fill(lapply(non.null.list, as.data.frame))
    write_csv(buffers, file = paste0("SHP_blk_0010/2014", state_code, "/", substr(state_code, 1, 2), "_buffers.csv"))
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
