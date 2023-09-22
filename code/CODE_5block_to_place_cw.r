library("dplyr")
#install_version("dplyr", repos = "http://cran.us.r-project.org")
library("readr") # for write_csv and read_csv functions
#install_version("readr", repos = "http://cran.us.r-project.org")
library("data.table") # package for handling large datasets 
#install_version("data.table", repos = "http://cran.us.r-project.org")
library("magrittr") # for %<>% operator
library("zoo") # for na.approx function
#install_version("zoo", repos = "http://cran.us.r-project.org")
library("stringr")
library("sf")
#install.packages("devtools", repos = "http://cran.us.r-project.org", lib = "~/R_libs")
#require(devtools)
#install_version("sf", repos = "http://cran.us.r-project.org", version = "0.9-8", lib = "~/R_libs")
#install_version("foreach", repos = "http://cran.us.r-project.org")
library(foreach)
#install_version("doParallel", repos = "http://cran.us.r-project.org", lib = "~/R_libs")
library(doParallel)
install_version("rgdal", repos = "http://cran.us.r-project.org", lib = "~/R_libs")
install_version("tigris", repos = "http://cran.us.r-project.org", lib = "~/R_libs")
install_version("rvest", repos = "http://cran.us.r-project.org", lib = "~/R_libs")
install_version("tidyr", repos = "http://cran.us.r-project.org", lib = "~/R_libs")
library("rvest")

# NEW ANNEX SCHEME ####
# 2000 blocks (2000b) with 2000 place (2000b) shapefiles; 2000 blocks (2000b) with 2007 (2000b) place shapefiles
# 2007 blocks (2000b) with 2007 (2000b) place shapefiles; 2007 (2000b) blocks with 2013 (2010b) place shapefiles
# 2014 blocks (2010b) with 2014 place (2010b) shapefiles; 2014 blocks (2010b) with 2020 (2020b) place shapefiles 

# 2000 blocks on 2000 and 2007 places ----
state_codes <- c("AL_01", "AS_02", "AR_05", "AZ_04", "CA_06", "CO_08", 
                 "DE_10", "FL_12", "GA_13", "HI_15", "IA_19", "ID_16", "IL_17", "IN_18",
                 "KS_20", "KY_21", "LA_22", "MD_24",
                 "MI_26", "MN_27", "MS_28", "MO_29", "MT_30", 
                 "NC_37", "ND_38", "NE_31", "NM_35", "NV_32", 
                 "OH_39", "OK_40", "OR_41", 
                 "SC_45", "SD_46", "TN_47", "TX_48", "UT_49", "VA_51",
                 "WA_53", "WV_54", "WI_55", "WY_56"
)

get_block_ids_2000 <- function (state_code, place_year) {
  blocks <- st_read(paste0("shapefiles/2000/blocks/", state_code, "/tl_2010_", substr(state_code, 4, 5), "_tabblock00.shp")) %>%
    st_transform(., 3488) %>%
    mutate(area_blk = st_area(.))
  blocks %<>%
    mutate(blkid = paste0(str_pad(as.character(STATEFP00), 2, side = "left", pad = "0"), str_pad(as.character(COUNTYFP00), 3, side = "left", pad = "0"),
                          str_pad(as.character(TRACTCE00), 6, side = "left", pad = "0"), str_pad(as.character(BLOCKCE00), 4, side = "left", pad = "0"))) %>%
    filter(!duplicated(blkid))
  
  place_file <- ifelse(place_year == 2000, paste0("shapefiles/2000/places/", state_code, "/tl_2010_", substr(state_code, 4, 5), "_place00.shp"),
                       paste0("shapefiles/2007/places/fe_2007_", substr(state_code, 4, 5), "_place.shp")
  )
  places <- st_read(place_file) 
  
  if (state_code == "HI_15" & place_year == 2007) {
    places <- st_set_crs(places, 4135) %>% st_transform(., 3488)} else {
      places <- st_transform(places, 3488)
    }
  
  places %<>%
    mutate(plid = paste0(
      str_pad(as.character(.[[1]]), 2, side = "left", pad = "0"), 
      str_pad(as.character(.[[2]]), 5, side = "left", pad = "0")))
  
  datalist <- list()
  places_df <- split(places, f = unique(places$plid))
  cl <- makeCluster(16) 
  registerDoParallel(cl)
  getDoParWorkers()
  
  datalist <- foreach (i = 1:length(places_df), 
                       .packages = c("sf", "dplyr", "data.table", "readr", "magrittr")) %dopar% {
                         p1 <- st_as_sf(places_df[[i]])
                         p1blocks <- c(sf::st_contains(p1, blocks), sf::st_overlaps(p1, blocks))
                         if(nrow(as.data.frame(blocks[unique(c(p1blocks[[1]], p1blocks[[2]])),])) < 1) return(NULL)
                         test <- as.data.frame(blocks[unique(c(p1blocks[[1]], p1blocks[[2]])),])
                         blkids <- blocks %>% filter(blkid %in% test$blkid)
                         area_int <- st_intersection(blkids, p1) %>%
                           mutate(area_int = st_area(.),
                                  pct_int = (area_int/area_blk)*100, 
                                  pct_int = as.numeric(pct_int)) %>%
                           filter(pct_int >= 90)
                         test$plid <- p1$plid
                         test %<>% 
                           filter(blkid %in% area_int$blkid) %>%
                           select(c(1:6), blkid, plid)
                         return(test)
                       }
  contig <- data.table::rbindlist(datalist) 
  readr::write_csv(contig, file = paste0("spatial_files/2000/", state_code, "_block_plids_2000blk-", place_year, "pl_90pct.csv"))
  
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

# 2007 blocks on 2007 and 2013 places ----
state_codes <- c("AL_01", "AS_02", "AR_05", "AZ_04", "CA_06", "CO_08", 
                 "DE_10", "FL_12", "GA_13", "HI_15", "IA_19", "ID_16", "IL_17", "IN_18",
                 "KS_20", "KY_21", "LA_22", "MD_24",
                 "MI_26", "MN_27", "MS_28", "MO_29", "MT_30", 
                 "NC_37", "ND_38", "NE_31", "NM_35", "NV_32", 
                 "OH_39", "OK_40", "OR_41", 
                 "SC_45", "SD_46", "TN_47", "TX_48", "UT_49", "VA_51",
                 "WA_53", "WV_54", "WI_55", "WY_56"
)

get_block_ids_2007 <- function (state_code, place_year) {
  blocks <- st_read(paste0("shapefiles/2007/blocks/states/", state_code, "_allblocks.shp")) %>%
    st_transform(., 3488) %>%
    mutate(area_blk = st_area(.))
  
  blocks %<>%
    mutate(blkid = paste0(str_pad(as.character(STATEFP00), 2, side = "left", pad = "0"), str_pad(as.character(COUNTYFP00), 3, side = "left", pad = "0"),
                          str_pad(as.character(TRACTCE00), 6, side = "left", pad = "0"), str_pad(as.character(BLOCKCE00), 4, side = "left", pad = "0"))) %>%
    filter(!duplicated(blkid))
  
  place_file <- ifelse(place_year == 2013, paste0("shapefiles/2013/places/", state_code, "/tl_2013_", substr(state_code, 4, 5), "_place.shp"),
                       paste0("shapefiles/2007/places/fe_2007_", substr(state_code, 4, 5), "_place.shp")
  )
  if (state_code == "HI_15" & place_year == 2007) {
    places <- st_read(place_file) %>%
      st_set_crs(., 4135) %>%
      st_transform(., 3488)
  } else {
    places <- st_read(place_file) %>%
      st_transform(., 3488)
  }
  
  places %<>%
    mutate(plid = paste0(
      str_pad(as.character(.[[1]]), 2, side = "left", pad = "0"), 
      str_pad(as.character(.[[2]]), 5, side = "left", pad = "0")))
  
  datalist <- list()
  places_df <- split(places, f = unique(places$plid))
  cl <- makeCluster(16) 
  registerDoParallel(cl)
  getDoParWorkers()
  datalist <- foreach (i = 1:length(places_df), 
                       .packages = c("sf", "dplyr", "data.table", "readr", "magrittr")) %dopar% {
                         p1 <- st_as_sf(places_df[[i]])
                         p1blocks <- c(sf::st_contains(p1, blocks), sf::st_overlaps(p1, blocks))
                         if(nrow(as.data.frame(blocks[unique(c(p1blocks[[1]], p1blocks[[2]])),])) < 1) return(NULL)
                         test <- as.data.frame(blocks[unique(c(p1blocks[[1]], p1blocks[[2]])),])
                         blkids <- blocks %>% filter(blkid %in% test$blkid)
                         area_int <- st_intersection(blkids, p1) %>%
                           mutate(area_int = st_area(.),
                                  pct_int = (area_int/area_blk)*100, 
                                  pct_int = as.numeric(pct_int)) %>%
                           filter(pct_int >= 90)
                         test$plid <- p1$plid
                         test %<>% 
                           filter(blkid %in% area_int$blkid) %>%
                           select(c(1:6), blkid, plid)
                         return(test)
                       }
  contig <- data.table::rbindlist(datalist) 
  readr::write_csv(contig, file = paste0("spatial_files/2007/", state_code, "_block_plids_2007blk-", place_year, "pl_90pct.csv"))
  
  stopCluster(cl)
}

for (state_code in state_codes) {
  start_time <- Sys.time()
  print(start_time)
  get_block_ids_2007(state_code, 2007)
  get_block_ids_2007(state_code, 2013)
  end_time <- Sys.time()
  print(state_code)
  print(end_time - start_time)
  print(end_time)
  Sys.sleep(60)
}

# 2008 blocks on 2008 and 2009 places ----
state_codes <- c("AL_01", "AS_02", "AR_05", "AZ_04", "CA_06", "CO_08", 
                 "DE_10", "FL_12", "GA_13", "HI_15", "IA_19", "ID_16", "IL_17", "IN_18",
                 "KS_20", "KY_21", "LA_22", "MD_24",
                 "MI_26", "MN_27", "MS_28", "MO_29", "MT_30", 
                 "NC_37", "ND_38", "NE_31", "NM_35", "NV_32", 
                 "OH_39", "OK_40", "OR_41", 
                 "SC_45", "SD_46", "TN_47", "TX_48", "UT_49", "VA_51",
                 "WA_53", "WV_54", "WI_55", "WY_56"
)

get_block_ids <- function (state_code, baseline_year) {
  end_year <- baseline_year + 1
  blocks <- st_read(paste0("shapefiles/blocks/", baseline_year, "/blocks/", state_code, "_allblocks.shp")) %>%
    st_transform(., 3488) %>%
    mutate(area_blk = st_area(.))
  
  blocks %<>%
    mutate(blkid = paste0(str_pad(as.character(STATEFP00), 2, side = "left", pad = "0"), str_pad(as.character(COUNTYFP00), 3, side = "left", pad = "0"),
                          str_pad(as.character(TRACTCE00), 6, side = "left", pad = "0"), str_pad(as.character(BLOCKCE00), 4, side = "left", pad = "0"))) %>%
    filter(!duplicated(blkid))
  
  # place_file_baseline <- paste0("shapefiles/places/", baseline_year, "/tl_", baseline_year, "_", substr(state_code, 4, 5), "_place.shp")
  # if (state_code == "HI_15" & end_year == 2007) {
  #   places <- st_read(place_file_baseline) %>%
  #     st_set_crs(., 4135) %>%
  #     st_transform(., 3488)
  # } else {
  #   places <- st_read(place_file_baseline) %>%
  #     st_transform(., 3488)
  # }
  # 
  # places %<>%
  #   mutate(plid = paste0(
  #     str_pad(as.character(.[[1]]), 2, side = "left", pad = "0"),
  #     str_pad(as.character(.[[2]]), 5, side = "left", pad = "0")))
  # 
  # datalist <- list()
  # places_df <- split(places, f = unique(places$plid))
  # cl <- makeCluster(16)
  # registerDoParallel(cl)
  # getDoParWorkers()
  # datalist <- foreach (i = 1:length(places_df),
  #                      .packages = c("sf", "dplyr", "data.table", "readr", "magrittr"),
  #                      .errorhandling = 'remove') %dopar% {
  #                        p1 <- st_as_sf(places_df[[i]])
  #                        p1blocks <- c(sf::st_contains(p1, blocks), sf::st_overlaps(p1, blocks))
  #                        if(nrow(as.data.frame(blocks[unique(c(p1blocks[[1]], p1blocks[[2]])),])) < 1) return(NULL)
  #                        test <- as.data.frame(blocks[unique(c(p1blocks[[1]], p1blocks[[2]])),])
  #                        blkids <- blocks %>% filter(blkid %in% test$blkid)
  #                        area_int <- st_intersection(blkids, p1) %>%
  #                          mutate(area_int = st_area(.),
  #                                 pct_int = (area_int/area_blk)*100,
  #                                 pct_int = as.numeric(pct_int)) %>%
  #                          filter(pct_int >= 90)
  #                        test$plid <- p1$plid
  #                        test %<>%
  #                          filter(blkid %in% area_int$blkid) %>%
  #                          select(c(1:6), blkid, plid)
  #                        return(test)
  #                      }
  # contig <- data.table::rbindlist(datalist)
  # readr::write_csv(contig, file = paste0("spatial_files/", baseline_year, "/", state_code, "_block_plids_", baseline_year, "blk-", baseline_year, "pl_90pct.csv"))
  # 
  # stopCluster(cl)
  # rm(list=ls(name=foreach:::.foreachGlobals), pos=foreach:::.foreachGlobals)
  # 
  place_file_end <- paste0("shapefiles/places/", end_year, "/tl_", end_year, "_", substr(state_code, 4, 5), "_place.shp")
  if (state_code == "HI_15" & end_year == 2007) {
    places <- st_read(place_file_end) %>%
      st_set_crs(., 4135) %>%
      st_transform(., 3488)
  } else {
    places <- st_read(place_file_end) %>%
      st_transform(., 3488)
  }
  
  places %<>%
    mutate(plid = paste0(
      str_pad(as.character(.[[1]]), 2, side = "left", pad = "0"), 
      str_pad(as.character(.[[2]]), 5, side = "left", pad = "0")))
  
  datalist <- list()
  places_df <- split(places, f = unique(places$plid))
  cl <- makeCluster(16) 
  registerDoParallel(cl)
  getDoParWorkers()
  datalist <- foreach (i = 1:length(places_df), 
                       .packages = c("sf", "dplyr", "data.table", "readr", "magrittr"),
                       .errorhandling = 'remove') %dopar% {
                         p1 <- st_as_sf(places_df[[i]])
                         p1blocks <- c(sf::st_contains(p1, blocks), sf::st_overlaps(p1, blocks))
                         if(nrow(as.data.frame(blocks[unique(c(p1blocks[[1]], p1blocks[[2]])),])) < 1) return(NULL)
                         test <- as.data.frame(blocks[unique(c(p1blocks[[1]], p1blocks[[2]])),])
                         blkids <- blocks %>% filter(blkid %in% test$blkid)
                         area_int <- st_intersection(blkids, p1) %>%
                           mutate(area_int = st_area(.),
                                  pct_int = (area_int/area_blk)*100, 
                                  pct_int = as.numeric(pct_int)) %>%
                           filter(pct_int >= 90)
                         test$plid <- p1$plid
                         test %<>% 
                           filter(blkid %in% area_int$blkid) %>%
                           select(c(1:6), blkid, plid)
                         return(test)
                       }
  contig <- data.table::rbindlist(datalist) 
  readr::write_csv(contig, file = paste0("spatial_files/", baseline_year, "/", state_code, "_block_plids_", baseline_year, "blk-", end_year, "pl_90pct.csv"))
  
  stopCluster(cl)
  rm(list=ls(name=foreach:::.foreachGlobals), pos=foreach:::.foreachGlobals)
}

year <- 2007
for (state_code in state_codes) {
  start_time <- Sys.time()
  print(start_time)
  get_block_ids(state_code = state_code, baseline_year = year)
  print(state_code)
  end_time <- Sys.time()
  print(end_time - start_time)
  print(end_time)
  Sys.sleep(60)
}

# 2014 blocks on 2014 and 2020 places ####
state_codes <- c("AL_01", "AR_05", "AS_02", "AZ_04", "CA_06", "CO_08", 
                 "DE_10", "FL_12", "GA_13", "HI_15", "IA_19", "ID_16", "IL_17", "IN_18",
                 "KS_20", "KY_21", "LA_22", "MD_24",
                 "MI_26", "MN_27", "MS_28", "MO_29", "MT_30", 
                 "NC_37", "ND_38", "NE_31", "NM_35", "NV_32", 
                 "OH_39", "OK_40", "OR_41", 
                 "SC_45", "SD_46", "TN_47", "TX_48", "UT_49", "VA_51",
                 "WA_53", "WV_54", "WI_55", "WY_56"
)

get_block_ids_2014 <- function (state_code, place_year) {
  blocks <- st_read(paste0("shapefiles/2014/blocks/", state_code, "/tl_2014_", substr(state_code, 4, 5), "_tabblock10.shp")) %>%
    st_transform(., 3488) %>%
    mutate(area_blk = st_area(.))
  
  blocks %<>%
    mutate(blkid = paste0(str_pad(as.character(STATEFP10), 2, side = "left", pad = "0"), str_pad(as.character(COUNTYFP10), 3, side = "left", pad = "0"),
                          str_pad(as.character(TRACTCE10), 6, side = "left", pad = "0"), str_pad(as.character(BLOCKCE10), 4, side = "left", pad = "0"))) %>%
    filter(!duplicated(blkid))
  
  place_file <- ifelse(place_year == 2014, paste0("shapefiles/2014/places/", state_code, "/tl_2014_", substr(state_code, 4, 5), "_place.shp"),
                       paste0("shapefiles/2020/places/tl_2020_", substr(state_code, 4, 5), "_place.shp")
  )
  places <- st_read(place_file) %>%
    st_transform(., 3488)
  
  places %<>%
    mutate(plid = paste0(
      str_pad(as.character(.[[1]]), 2, side = "left", pad = "0"), 
      str_pad(as.character(.[[2]]), 5, side = "left", pad = "0")))
  
  datalist <- list()
  places_df <- split(places, f = unique(places$plid))
  cl <- makeCluster(20) 
  registerDoParallel(cl)
  getDoParWorkers()
  datalist <- foreach (i = 1:length(places_df),
                       .packages = c("sf", "dplyr", "data.table", "readr", "magrittr")) %dopar% {
                         p1 <- st_as_sf(places_df[[i]])
                         p1blocks <- c(sf::st_contains(p1, blocks), sf::st_overlaps(p1, blocks))
                         if(nrow(as.data.frame(blocks[unique(c(p1blocks[[1]], p1blocks[[2]])),])) < 1) return(NULL)
                         test <- as.data.frame(blocks[unique(c(p1blocks[[1]], p1blocks[[2]])),])
                         blkids <- blocks %>% filter(blkid %in% test$blkid)
                         area_int <- st_intersection(blkids, p1) %>%
                           mutate(area_int = st_area(.),
                                  pct_int = (area_int/area_blk)*100, 
                                  pct_int = as.numeric(pct_int)) %>%
                           filter(pct_int >= 90)
                         test$plid <- p1$plid
                         test %<>% 
                           filter(blkid %in% area_int$blkid) %>%
                           select(c(1:6), blkid, plid)
                         return(test)
                       }
  contig <- data.table::rbindlist(datalist) 
  readr::write_csv(contig, file = paste0("spatial_files/2014/", state_code, "_block_plids_2014blk-", place_year, "pl_90pct.csv"))
  stopCluster(cl)
}

for (state_code in state_codes) {
  start_time <- Sys.time()
  print(start_time)
  get_block_ids_2014(state_code, 2014)
  end_time <- Sys.time()
  print(state_code)
  print(end_time - start_time)
  print(end_time)
  start_time <- Sys.time()
  print(start_time)
  get_block_ids_2014(state_code, 2020)
  end_time <- Sys.time()
  print(state_code)
  print(end_time - start_time)
  print(end_time)
  Sys.sleep(60)
}


# buffers ----
get_buffers <- function(state_code, year) {
  cdps <- read_csv(paste0("plids/pl", year, ".csv")) %>%
    mutate(cdp = ifelse(grepl("CDP|cdp", Geo_NAME), 1, 0)) %>%
    filter(cdp==1)
  
  if (year < 2010) {
  blocks <- st_read(paste0("shapefiles/blocks/", year, "/blocks/tl_", year, "_", substr(state_code, 4, 5), "_tabblock.shp")) %>%
    st_transform(., 3488)
  blocks %<>%
    mutate(blkid = paste0(str_pad(as.character(STATEFP00), 2, side = "left", pad = "0"),
                          str_pad(as.character(COUNTYFP00), 3, side = "left", pad = "0"),
                          str_pad(as.character(TRACTCE00), 6, side = "left", pad = "0"),
                          str_pad(as.character(BLOCKCE00), 4, side = "left", pad = "0"))) %>%
    filter(!duplicated(blkid))} else {
      blocks <- st_read(paste0("shapefiles/blocks/", year, "/tl_", year, "_", substr(state_code, 4, 5), "_tabblock10.shp")) %>%
        st_transform(., 3488)
      blocks %<>%
        mutate(blkid = paste0(str_pad(as.character(STATEFP10), 2, side = "left", pad = "0"),
                              str_pad(as.character(COUNTYFP10), 3, side = "left", pad = "0"),
                              str_pad(as.character(TRACTCE10), 6, side = "left", pad = "0"),
                              str_pad(as.character(BLOCKCE10), 4, side = "left", pad = "0"))) %>%
        filter(!duplicated(blkid))
    }
  
  
  # should only retain those not already part of a place
  plid_list <- read_csv(file = paste0("spatial_files/", year, "/", state_code, "_block_plids_", year, "blk-", year, "pl_90pct.csv"))
  plid_list %<>%
    select(blkid, plid) %>%
    mutate(blkid = as.character(blkid))
  
  blocks %<>%
    left_join(plid_list, by = "blkid") %>%
    filter(is.na(plid) | plid %in% cdps$plid)
  
  # place shapefile
  if (year == 2007) {
    places <- st_read(paste0("shapefiles/2007/places/fe_2007_", substr(state_code, 4, 5), "_place.shp"))
    if (state_code != "HI_15") {places <- st_transform(places, 3488)} else {
      places <- st_set_crs(places, 4135) %>% st_transform(., 3488)
    }
    places %<>%
      mutate(plid = paste0(
        str_pad(as.character(STATEFP), 2, side = "left", pad = "0"),
        str_pad(as.character(PLACEFP), 5, side = "left", pad = "0")))} else {
          places <- st_read(paste0("shapefiles/places/", year, "/tl_", year, "_", substr(state_code, 4, 5), "_place.shp"))
          if (state_code != "HI_15") {places <- st_transform(places, 3488)} else {
            places <- st_set_crs(places, 4135) %>% st_transform(., 3488)
          }
          places %<>%
            mutate(plid = paste0(
              str_pad(as.character(STATEFP), 2, side = "left", pad = "0"),
              str_pad(as.character(PLACEFP), 5, side = "left", pad = "0")))
        }
  
  datalist <- list()
  places_df <- split(places, f = places$plid)
  cl <- makeCluster(16)
  registerDoParallel(cl)
  getDoParWorkers()
  
  datalist <- foreach (i = 1:length(places_df),
                       .packages = c("sf", "dplyr", "data.table", "readr", "magrittr"),
                       .errorhandling = "remove") %dopar% {
                         place_blkid <- plid_list %>% filter(plid == places_df[[i]]$plid) %>%
                           filter(!duplicated(blkid))
                         p1buffer <- sf::st_buffer(places_df[[i]], 400)
                         p1buffer_intersects <- sf::st_intersects(p1buffer, blocks)
                         if(nrow(as.data.frame(blocks[p1buffer_intersects[[1]],])) < 1) return(NULL)
                         test <- as.data.frame(blocks[p1buffer_intersects[[1]],])
                         test$bufferplace <- places_df[[i]]$plid[[1]]
                         test %<>%
                           filter(!blkid %in% place_blkid$blkid) %>%
                           select(c(1:4), blkid, bufferplace)
                         return(test)
                       }
  
  buffers <- data.table::rbindlist(datalist)
  readr::write_csv(buffers, file = paste0("spatial_files/", year, "/",  substr(state_code, 1, 2), "_buffers.csv"))
  stopCluster(cl)
  rm(list=ls(name=foreach:::.foreachGlobals), pos=foreach:::.foreachGlobals)
}

state_codes <- c("AL_01", "AS_02", "AR_05", "AZ_04", "CA_06", "CO_08", 
                 "DE_10", "FL_12", "GA_13", "HI_15", "IA_19", "ID_16", "IL_17", "IN_18",
                 "KS_20", "KY_21", "LA_22", "MD_24",
                 "MI_26", "MN_27", "MS_28", "MO_29", "MT_30", 
                 "NC_37", "ND_38", "NE_31", "NM_35", "NV_32", 
                 "OH_39", "OK_40", "OR_41", 
                 "SC_45", "SD_46", "TN_47", "TX_48", "UT_49", "VA_51",
                 "WA_53", "WV_54", "WI_55", "WY_56"
)

for (state_code in state_codes) {
  start_time <- Sys.time()
  print(start_time)
  get_buffers(state_code, 2010)
  end_time <- Sys.time()
  print(state_code)
  print(end_time - start_time)
  print(end_time)
}

# 2000 ----
cdps <- read_csv("pl2000_cleaned.csv") %>%
  select(Geo_NAME, plid) %>%
  mutate(cdp = ifelse(grepl("CDP|cdp", Geo_NAME), 1, 0)) %>%
  filter(cdp==1)
get_buffers_00 <- function(state_code) {
  blocks <- st_read(paste0("shapefiles/2000/blocks/", state_code, "/tl_2010_", substr(state_code, 4, 5), "_tabblock00.shp")) %>%
    st_transform(., 3488)
  blocks %<>%
    mutate(blkid = paste0(str_pad(as.character(STATEFP00), 2, side = "left", pad = "0"), 
                          str_pad(as.character(COUNTYFP00), 3, side = "left", pad = "0"),
                          str_pad(as.character(TRACTCE00), 6, side = "left", pad = "0"), 
                          str_pad(as.character(BLOCKCE00), 4, side = "left", pad = "0"))) %>%
    filter(!duplicated(blkid))
  
  # should only retain those not already part of a place
  plid_list <- read_csv(file = paste0("spatial_files/2000/", state_code, "_block_plids_2000blk-2000pl_90pct.csv"))
  plid_list %<>% 
    select(blkid, plid) %>%
    mutate(blkid = as.character(blkid))
  
  blocks %<>%
    left_join(plid_list, by = "blkid") %>% 
    filter(is.na(plid) | plid %in% cdps$plid)
  
  # place shapefile
  places <- st_read(paste0("shapefiles/2000/places/", state_code, "/tl_2010_", substr(state_code, 4, 5), "_place00.shp"))
  if (state_code != "HI_15") {places <- st_transform(places, 3488)} else {
    places <- st_set_crs(places, 4135) %>% st_transform(., 3488)
  }
  places %<>% 
    mutate(plid = paste0(
      str_pad(as.character(STATEFP), 2, side = "left", pad = "0"), 
      str_pad(as.character(PLACEFP), 5, side = "left", pad = "0"))) 
  
  datalist <- list()
  places_df <- split(places, f = places$plid)
  
  datalist <- foreach (i = 1:length(places_df), 
                       .packages = c("sf", "dplyr", "data.table", "readr", "magrittr")) %dopar% {
                         place_blkid <- plid_list %>% filter(plid == places_df[[i]]$plid) %>%
                           filter(!duplicated(blkid))
                         p1buffer <- sf::st_buffer(places_df[[i]], 400)
                         p1buffer_intersects <- sf::st_intersects(p1buffer, blocks)
                         if(nrow(as.data.frame(blocks[p1buffer_intersects[[1]],])) < 1) return(NULL)
                         test <- as.data.frame(blocks[p1buffer_intersects[[1]],])
                         test$bufferplace <- places_df[[i]]$plid[[1]]
                         test %<>% 
                           filter(!blkid %in% place_blkid$blkid) %>%
                           select(c(1:4), blkid, bufferplace)
                         return(test)
                       }
  
  buffers <- data.table::rbindlist(datalist) 
  readr::write_csv(buffers, file = paste0("shapefiles/2000/blocks/", state_code, "/", substr(state_code, 1, 2), "_buffers.csv"))
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
  get_buffers_00(state_code)
  end_time <- Sys.time()
  print(state_code)
  print(end_time - start_time)
  print(end_time)
}

# 2014 ----
cdps <- read_csv("places2014_cleaned.csv") %>%
  select(Geo_NAME, plid) %>%
  mutate(cdp = ifelse(grepl("CDP|cdp", Geo_NAME), 1, 0)) %>%
  filter(cdp==1)

get_buffers_14 <- function(state_code) {
  blocks <- st_read(paste0("shapefiles/2014/blocks/", state_code, "/tl_2014_", substr(state_code, 4, 5), "_tabblock10.shp"))
  blocks <- st_transform(blocks, 3488)
  blocks %<>%
    mutate(blkid = paste0(str_pad(as.character(STATEFP10), 2, side = "left", pad = "0"), 
                          str_pad(as.character(COUNTYFP10), 3, side = "left", pad = "0"),
                          str_pad(as.character(TRACTCE10), 6, side = "left", pad = "0"), 
                          str_pad(as.character(BLOCKCE10), 4, side = "left", pad = "0"))) %>%
    filter(!duplicated(blkid))
  
  # should only retain those not already part of a place
  plid_list <- read_csv(file = paste0("spatial_files/2014/", state_code, "_block_plids_2014blk-2014pl_90pct.csv"))
  plid_list %<>% 
    select(blkid, plid) %>%
    mutate(blkid = as.character(blkid))
  blocks %<>%
    left_join(plid_list, by = "blkid") %>% 
    filter((is.na(plid) | plid %in% cdps$plid))
  rm(plid_list)
  
  # place shapefile
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
                         place_blkid <- plid_list %>% filter(plid == places_df[[i]]$plid) %>%
                           filter(!duplicated(blkid))
                         p1buffer <- sf::st_buffer(places_df[[i]], 400)
                         p1buffer_intersects <- sf::st_intersects(p1buffer, blocks)
                         if(nrow(as.data.frame(blocks[p1buffer_intersects[[1]],])) < 1) return(NULL)
                         test <- as.data.frame(blocks[p1buffer_intersects[[1]],])
                         test$bufferplace <- places_df[[i]]$plid[[1]]
                         test %<>% 
                           filter(!blkid %in% place_blkid$blkid) %>%
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

# OLD: check intersection area ----
# check whether the identified contained blocks are right
state_codes <- c("AL_01", "AR_05", "AZ_04", "CA_06", "CO_08", 
                 "DE_10", "FL_12", "GA_13", "IA_19", "ID_16", "IL_17", "IN_18",
                 "KS_20", "KY_21", "LA_22", 
                 "MI_26", "MN_27", "MS_28", "MO_29", "MT_30", 
                 "NC_37", "ND_38", "NE_31", "NM_35", "NV_32", 
                 "OH_39", "OK_40", "OR_41", 
                 "SC_45", "SD_46", "TN_47", "TX_48", "UT_49", "VA_51",
                 "WA_53", "WV_54", "WI_55", "WY_56"
)

blocks14_20 <- read_csv("2014blk-2020plid.csv")

checkint_1420 <- function(state_code) {
  blocks14 <- st_read(paste0("shapefiles/2014/blocks/", state_code, "/tl_2014_", substr(state_code, 4, 5), "_tabblock10.shp")) %>%
    st_transform(., 3488) %>%
    mutate(area_blk = st_area(.)) %>%
    left_join(blocks14_20 %>% select(blkid, plid), by = c("GEOID10" = "blkid"))
  
  places20 <- st_read(paste0("shapefiles/2020/places/tl_2020_", substr(state_code, 4, 5), "_place.shp")) %>%
    st_transform(., 3488) %>%
    mutate(plid = paste0(str_pad(.[[1]], 2, "left", "0"),
                         str_pad(.[[2]], 5, "left", "0")))
  
  places_df <- split(places20, f = places20$plid)
  datalist <- list()
  cl <- makeCluster(20) 
  registerDoParallel(cl)
  getDoParWorkers()
  
  datalist <- foreach (i = 1:length(places_df),
                       .packages = c("sf", "dplyr", "data.table", "readr", "magrittr")) %dopar% {
                         b14 <- blocks14 %>% 
                           filter(plid %in% places_df[[i]]) 
                         
                         pl <- st_as_sf(places_df[[i]])
                         
                         ann_int <- st_intersection(b14, pl) %>%
                           mutate(area_int = st_area(.),
                                  pct_int = (area_int/area_blk)*100, 
                                  pct_int = as.numeric(pct_int)) %>%
                           filter(pct_int >= 90)
                         
                         if(nrow(as.data.frame(ann_int)) < 1) return(NULL)
                         test <- as.data.frame(ann_int) %>% 
                           select(GEOID10, plid, pct_int)
                         return(test)
                       }
  contig <- data.table::rbindlist(datalist) 
  readr::write_csv(contig, file = paste0("spatial_files/2014/", state_code, "_block_plids_2014blk-2020pl_90pct.csv"))
  
  stopCluster(cl)
  rm(list=ls(name=foreach:::.foreachGlobals), pos=foreach:::.foreachGlobals)
}

for (state_code in state_codes) {
  start_time <- Sys.time()
  print(start_time)
  checkint_1420(state_code)
  print(state_code)
  end_time <- Sys.time()
  print(end_time - start_time)
  print(end_time)
  Sys.sleep(60)
}

blocks07_13 <- read_csv("2007blk-2013plid.csv")

checkint_0713 <- function(state_code) {
  blocks07 <- st_read(paste0("shapefiles/2007/blocks/states/", state_code, "_allblocks.shp")) %>%
    st_transform(., 3488) %>%
    mutate(area_blk = st_area(.)) %>%
    left_join(blocks07_13 %>% select(blkid, plid), by = c("BLKIDFP" = "blkid"))
  
  places13 <- st_read(paste0("shapefiles/2013/places/", state_code, "/tl_2013_", substr(state_code, 4, 5), "_place.shp")) %>%
    st_transform(., 3488) %>%
    mutate(plid = paste0(str_pad(.[[1]], 2, "left", "0"),
                         str_pad(.[[2]], 5, "left", "0")))
  
  places_df <- split(places13, f = places13$plid)
  datalist <- list()
  cl <- makeCluster(20) 
  registerDoParallel(cl)
  getDoParWorkers()
  datalist <- foreach (i = 1:length(places_df),
                       .packages = c("sf", "dplyr", "data.table", "readr", "magrittr")) %dopar% {
                         b07 <- blocks07 %>% 
                           filter(plid %in% places_df[[i]]) 
                         
                         pl <- st_as_sf(places_df[[i]])
                         
                         ann_int <- st_intersection(b07, pl) %>%
                           mutate(area_int = st_area(.),
                                  pct_int = (area_int/area_blk)*100, 
                                  pct_int = as.numeric(pct_int)) %>%
                           filter(pct_int >= 90)
                         
                         if(nrow(as.data.frame(ann_int)) < 1) return(NULL)
                         test <- as.data.frame(ann_int) %>% 
                           select(BLKIDFP, plid, pct_int)
                         return(test)
                       }
  contig <- data.table::rbindlist(datalist) 
  readr::write_csv(contig, file = paste0("spatial_files/2007/", state_code, "_block_plids_2007blk-2013plid_90pct.csv"))
  
  stopCluster(cl)
  rm(list=ls(name=foreach:::.foreachGlobals), pos=foreach:::.foreachGlobals)
}

state_codes <- c("AS_02", "HI_15", "MD_24")

for (state_code in state_codes) {
  start_time <- Sys.time()
  print(start_time)
  checkint_0713(state_code)
  print(state_code)
  end_time <- Sys.time()
  print(end_time - start_time)
  print(end_time)
  Sys.sleep(60)
}

# 2000 
blocks00_07 <- read_csv("2000blk-2007plid.csv")

checkint_0007 <- function(state_code) {
  blocks00 <- st_read(paste0("shapefiles/2000/blocks/", state_code, "/tl_2010_", substr(state_code, 4, 5), "_tabblock00.shp")) %>%
    st_transform(., 3488) %>%
    mutate(area_blk = st_area(.)) %>%
    left_join(blocks00_07 %>% select(blkid, plid), by = c("BLKIDFP00" = "blkid"))
  
  places07 <- st_read(paste0("shapefiles/2007/places/fe_2007_", substr(state_code, 4, 5), "_place.shp")) 
  
  if (state_code == "HI_15") {
    places07 %<>%
      st_set_crs(., 4135) %>%
      st_transform(., 3488)
  } else {
    places07 %<>%
      st_transform(., 3488)
  }
  places07 %<>%
    mutate(plid = paste0(str_pad(.[[1]], 2, "left", "0"),
                         str_pad(.[[2]], 5, "left", "0")))
  
  places_df <- split(places07, f = places07$plid)
  datalist <- list()
  cl <- makeCluster(20) 
  registerDoParallel(cl)
  getDoParWorkers()
  datalist <- foreach (i = 1:length(places_df),
                       .packages = c("sf", "dplyr", "data.table", "readr", "magrittr")) %dopar% {
                         b00 <- blocks00 %>% 
                           filter(plid %in% places_df[[i]]) 
                         
                         pl <- st_as_sf(places_df[[i]])
                         
                         ann_int <- st_intersection(b00, pl) %>%
                           mutate(area_int = st_area(.),
                                  pct_int = (area_int/area_blk)*100, 
                                  pct_int = as.numeric(pct_int)) %>%
                           filter(pct_int >= 90)
                         
                         if(nrow(as.data.frame(ann_int)) < 1) return(NULL)
                         test <- as.data.frame(ann_int) %>% 
                           select(BLKIDFP00, plid, pct_int)
                         return(test)
                       }
  contig <- data.table::rbindlist(datalist) 
  readr::write_csv(contig, file = paste0("spatial_files/2000/", state_code, "_block_plids_2000blk-2007plid_90pct.csv"))
  
  stopCluster(cl)
  rm(list=ls(name=foreach:::.foreachGlobals), pos=foreach:::.foreachGlobals)
}

for (state_code in state_codes) {
  start_time <- Sys.time()
  print(start_time)
  checkint_0007(state_code)
  print(state_code)
  end_time <- Sys.time()
  print(end_time - start_time)
  print(end_time)
  Sys.sleep(60)
}

# 2007-2013 ####
# get place ids for 2007 blocks first 
# and then get buffers 
# the folder format is very unwieldy 
# it's all in one folder, and each block file corresponds to a particular county, although that doesn't really matter for us
state_codes <- c("AL_01", "AS_02", "AR_05", "AZ_04", "CA_06", "CO_08", "CT_09", 
                 "DE_10", "FL_12", "GA_13", "IA_19", "ID_16", "IL_17", "IN_18",
                 "KS_20", "KY_21", "LA_22", 
                 "MA_25", "MD_24", "ME_23", "MI_26", "MN_27", "MS_28", "MO_29", "MT_30", 
                 "NC_37", "ND_38", "NE_31", "NH_33", "NJ_34", "NM_35", "NV_32", "NY_36",
                 "OH_39", "OK_40", "OR_41", "PA_42", "RI_44",
                 "SC_45", "SD_46", "TN_47", "TX_48", "UT_49", "VT_50", "VA_51",
                 "WA_53", "WV_54", "WI_55", "WY_56"
)
state_codes <- "HI_15"
for (state in state_codes) {
  county_list <- intersect(list.files("shapefiles/2007/blocks/", pattern = paste0("fe_2007_", substr(state, 4, 5))), list.files("shapefiles/2007/blocks/", pattern = ".shp$"))
  blocks_list <- list()
  for (i in 1:length(county_list)) {
    county_list[[i]] <- paste0("shapefiles/2007/blocks/", county_list[[i]])
  }
  county_shape <- list()
  for (i in 1:length(county_list)) {
    county_shape[[i]] <- st_read(county_list[[i]]) 
  }
  res <- dplyr::bind_rows(county_shape)
  
  write_sf(res, paste0("shapefiles/2007/blocks/states/", state, "_allblocks.shp"))
}


# get ids first 
get_block_ids_07 <- function (state_code) {
  blocks <- st_read(paste0("shapefiles/2007/blocks/states/", state_code, "_allblocks.shp")) %>%
    st_transform(., 3488) %>%
    mutate(area_blk = st_area(.)) 
  blocks %<>%
    mutate(blkid = paste0(str_pad(as.character(STATEFP00), 2, side = "left", pad = "0"), str_pad(as.character(COUNTYFP00), 3, side = "left", pad = "0"),
                          str_pad(as.character(TRACTCE00), 6, side = "left", pad = "0"), str_pad(as.character(BLOCKCE00), 4, side = "left", pad = "0"))) %>%
    filter(!duplicated(blkid))
  
  places <- st_read(paste0("shapefiles/2007/places/fe_2007_", substr(state_code, 4, 5), "_place.shp")) 
  
  if (state_code == "HI_15" & place_year == 2007) {
    places %<>% st_set_crs(., 4135) %>% st_transform(., 3488)
  } else {
    places %<>%
      st_transform(., 3488)}
  
  places %<>% 
    mutate(plid = paste0(
      str_pad(as.character(STATEFP), 2, side = "left", pad = "0"), 
      str_pad(as.character(PLACEFP), 5, side = "left", pad = "0")))
  
  datalist <- list()
  for (i in 1:length(unique(places$plid))) {
    p1 <- places[i,]
    p1blocks <- c(st_contains(p1, blocks), st_overlaps(p1, blocks))
    if(nrow(as.data.frame(blocks[unique(c(p1blocks[[1]], p1blocks[[2]])),])) < 1) {
      next
    } else {
      test <- as.data.frame(blocks[unique(c(p1blocks[[1]], p1blocks[[2]])),]) %>%
        filter(!duplicated(blkid))
      blkids <- blocks %>% filter(blkid %in% test$blkid) %>%
        filter(!duplicated(blkid))
      area_int <- st_intersection(blkids, p1) %>%
        mutate(area_int = st_area(.),
               pct_int = (area_int/area_blk)*100, 
               pct_int = as.numeric(pct_int)) %>%
        filter(pct_int >= 90)
      test$plid <- p1$plid
      datalist[[i]] <- test %>% 
        filter(blkid %in% area_int$blkid) %>% 
        select(c(1:6), blkid, plid)
    }
  }
  non.null.list <- lapply(datalist, Filter, f = Negate(is.null))
  rm(datalist)
  contig <- data.table::rbindlist(non.null.list) 
  write_csv(contig, file = paste0("shapefiles/2007/blocks/states/", substr(state_code, 1, 2), "_block_plids.csv"))
}

for (state_code in state_codes) {
  start_time <- Sys.time()
  print(start_time)
  get_block_ids_07(state_code, 2007)
  get_block_ids_07(state_code, 2013)
  end_time <- Sys.time()
  print(state_code)
  print(end_time - start_time)
  print(end_time)
  Sys.sleep(60)
}

# dunno what this is ----

state_list <- list.files("shapefiles/2007/blocks/states/", pattern = "_block_plids.csv")
state_list <- paste0("shapefiles/2007/blocks/states/", substr(state_list, 1, 2), "_block_plids.csv")

blocks2007 <- list()
for (i in 1:length(state_list)) {
  blocks2007[[i]] <- read_csv(file = state_list[[i]]) 
} 

blocks2007 <- data.table::rbindlist(blocks2007, use.names = TRUE)
rm(state_list)
write_csv(blocks2007, file = "blocks2007_plids.csv")

# just all blocks 
state_list <- list.files("shapefiles/2007/blocks/states/", pattern = "_allblocks.shp")

blocks2007 <- list()
for (i in 1:length(state_list)) {
  blocks2007[[i]] <- st_read(paste0("shapefiles/2007/blocks/states/", state_list[[i]])) %>%
    as.data.frame() %>%
    select(STATEFP00, COUNTYFP00, TRACTCE00, BLOCKCE00) %>%
    mutate(blkid = paste0(str_pad(as.character(STATEFP00), 2, side = "left", pad = "0"), 
                          str_pad(as.character(COUNTYFP00), 3, side = "left", pad = "0"),
                          str_pad(as.character(TRACTCE00), 6, side = "left", pad = "0"), 
                          str_pad(as.character(BLOCKCE00), 4, side = "left", pad = "0"))) %>%
    select(blkid) 
} 

blocks2007 <- data.table::rbindlist(blocks2007, fill = T)
write_csv(blocks2007, file = "blocks2007_blkids.csv")
