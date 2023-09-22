# process ####
# 1. get list of annexed blocks from the annexedt1t2dem.csv files 
# 2. isolate the blocks that those are from the t2 block shapefile 
# 3. for each place at t2, check intersection with blocks from #2 
# 4. st_area on the intersection, and compare as a ratio with the area of the block

# annexed blocks for part 1
aa007 <- read_csv("analyticalfiles/annexedblocks0007dem.csv") 
aa0713 <- read_csv("analyticalfiles/annexedblocks0713dem.csv") 
aa1420 <- read_csv("analyticalfiles/annexedblocks1420dem.csv") 

# block shapefiles for part 2
# SHP_blk_0010/year/state_code_tabblock10.shp 

# place shapefiles for part 3
# SHP_pl/year/state_code_place.shp

# test run for 2007 to 2013 ####
# try GA_13
# get part 1
aa13 <- read_csv("analyticalfiles/annexedblocks0713dem.csv") 
aa13 %<>% 
  filter(annexed==1 & STATEFP==13)

# get part 2 
# try GA_13 
blocks13 <- st_read("SHP_blk_0010/2013/GA_13/tl_2013_13_tabblock.shp") %>%
  st_transform(., 3488)

blocks13 %<>%
  filter(GEOID %in% aa13$blkid) %>%
  rename(blkid = GEOID)

# get plids 
plids <- read_csv("SHP_blk_0010/2013/GA_13/GA_block_plids.csv") %>%
  select(blkid, plid) %>%
  mutate(blkid = as.character(blkid),
         plid = as.character(plid))

blocks13 %<>%
  left_join(plids, by = "blkid")

rm(plids)

# get part 3 
places13 <- st_read("SHP_pl/2013/GA_13/tl_2013_13_place.shp") %>%
  st_transform(., 3488)

places13 %<>%
  filter(GEOID %in% aa13$plid) %>%
  rename(plid = GEOID)

# pick the most numerous one 
aa0007 %>%
  group_by(plid) %>%
  summarize(n = n()) %>%
  arrange(desc(n)) %>% 
  select(plid, n)

# 1366668 but try the problematic one first 

# move into #4
# filter everything to the place 

places13 %<>%
  filter(plid == "1365856")

blocks13 %<>%
  filter(plid == "1365856") %>%
  mutate(area_blk = st_area(.))

# 4.1 get intersection between "annexed" blocks and the place 
ann_int <- st_intersection(blocks13, places13) %>%
  mutate(area_int = st_area(.))

ggplot() +  
  geom_sf(data = places13, size = 0.4, fill = "white") +
  geom_sf(data = ann_int, size = 0.4, fill= "grey", color = "red") 

# what about places07??? 
places07 <- st_read("SHP_pl/2007/fe_2007_13_place.shp") %>%
  st_transform(., 3488)

places07 %<>%
  filter(PLCIDFP %in% blocks13$plid) %>%
  rename(plid = PLCIDFP)

ggplot() +  
  geom_sf(data = places07, size = 0.4, fill = "white") + 
  geom_sf(data = blocks13, size = 0.25, color = "red") 

test <- as.data.frame(blocks07[ann_int[[1]],])
test$plid <- p1$plid
  datalist[[i]] <- test %>% 
    select(c(1:6), blkid, plid, GEOID)
  
# try comparing 2010 and 2020 places ####
places14 <- st_read("SHP_pl/2014/GA_13/tl_2014_13_place.shp") %>%
    st_transform(., 3488) %>%
    filter(NAME == "Atlanta") %>%
    mutate(area_14 = st_area(.))
  
places20 <- st_read("SHP_pl/2020/tl_2020_13_place.shp") %>%
    st_transform(., 3488) %>%
    filter(NAME == "Atlanta") %>%
  mutate(area_20 = st_area(.))

diff <- st_difference(places20, places14) %>%
  mutate(area = st_area(.))

# compare intersection with 2020 blocks 
blocks2020 <- st_read("SHP_blk_0010/2020/tl_2020_13_tabblock20.shp") %>%
  st_transform(., 3488)
p1blocks <- st_contains(diff, blocks2020)
test <- blocks2020[p1blocks[[1]],] 
ggplot() +
  geom_sf(data = places20, size = 0.1, fill = "grey") +
  geom_sf(data = test, size = 0.15, fill = "transparent", color = "black")

state_codes <- c("AL_01", "AS_02", "AR_05", "AZ_04", "CA_06", "CO_08", "CT_09", 
                 "DE_10", "FL_12", "GA_13", "HI_15", "IA_19", "ID_16", "IL_17", "IN_18",
                 "KS_20", "KY_21", "LA_22", 
                 "MA_25", "MD_24", "ME_23", "MI_26", "MN_27", "MS_28", "MO_29", "MT_30", 
                 "NC_37", "ND_38", "NE_31", "NH_33", "NJ_34", "NM_35", "NV_32", "NY_36",
                 "OH_39", "OK_40", "OR_41", "PA_42", "RI_44",
                 "SC_45", "SD_46", "TN_47", "TX_48", "UT_49", "VT_50", "VA_51",
                 "WA_53", "WV_54", "WI_55", "WY_56"
)

# check difference between st_contains and st_contains_properly for which blocks are where ####
blocks07 <- st_read("SHP_blk_0010/2007/states/GA_13_allblocks.shp") %>%
  st_transform(., 3488)

blocks07 %<>%
  left_join(cw, by = c("BLKIDFP" = "GEOID00")) %>%
  rename(blkid = GEOID10)  %>%
  filter(!is.na(blkid))
  
blocks13 <- st_read("SHP_blk_0010/2013/GA_13/tl_2013_13_tabblock.shp") %>%
  st_transform(., 3488) %>%
  filter(GEOID %in% blocks07$blkid)

# get part 3 
places13 <- st_read("SHP_pl/2013/GA_13/tl_2013_13_place.shp") %>%
  st_transform(., 3488)

places07 <- st_read("SHP_pl/2007/fe_2007_13_place.shp") %>%
  st_transform(., 3488)

places13 %<>%
  filter(GEOID == "1365856")

places07 %<>%
  filter(PLCIDFP == "1365856")

# 4.1 get intersection between "annexed" blocks and the place 
p1blocks_07 <- st_contains(places07, blocks07)
p1blocks_int_07 <- st_intersects(places07, blocks07)

p1blocks_13 <- st_contains(places13, blocks13)
p1blocks_int_13 <- st_intersects(places13, blocks13)

b07 <- blocks07[p1blocks_07[[1]],]
b_int_07 <- blocks07[p1blocks_int_07[[1]],]

b13 <- blocks13[p1blocks_13[[1]],]
b_int_13 <- blocks13[p1blocks_int_13[[1]],]

# test after spatial analysis process
blocks2007_plids <- read_csv("blocks2007_plids.csv") %>%
  filter(plid == "1365856")

b07 <- blocks07 %>%
  filter(blkid %in% blocks2007_plids$blkid)

blocks2007_buffers <- read_csv("blocks2007_buffers.csv") %>%
  filter(bufferplace == "1365856")

b07 <- blocks07 %>%
  filter(blkid %in% blocks2007_buffers$blkid)

blocks2013_plids <- read_csv("blocks2013_plids.csv") %>%
  filter(plid == "1365856")
b13 <- blocks13 %>%
  filter(GEOID %in% blocks2013_plids$blkid)

ggplot() + 
  geom_sf(data = places07, fill = "grey") + 
  geom_sf(data = b07, color = "red", fill = "transparent")

ggplot() + 
  #geom_sf(data = places07, fill = "grey") + 
  geom_sf(data = b_int_07, color = "red", fill = "transparent")

# 2014
blocks14 <- st_read("SHP_blk_0010/2014/GA_13/tl_2014_13_tabblock10.shp") %>%
  st_transform(., 3488)

places14 <- st_read("SHP_pl/2014/GA_13/tl_2014_13_place.shp") %>%
  st_transform(., 3488)

places14 %<>%
  filter(GEOID == "1365856")

# 4.1 get intersection between "annexed" blocks and the place 
p1blocks_14 <- st_contains(places14, blocks14)
p1blocks_int_14 <- st_intersects(places14, blocks14)

b14 <- blocks14[p1blocks_14[[1]],]
b_int_14 <- blocks14[p1blocks_int_14[[1]],]

# test after spatial analysis process
blocks2014_plids <- read_csv("blocks2014_plids.csv") %>%
  filter(plid == "1365856")

b14 <- blocks14 %>%
  filter(GEOID10 %in% blocks2014_plids$blkid)

ggplot() + 
  #geom_sf(data = places14, fill = "grey") + 
  geom_sf(data = b14, color = "red", fill = "transparent")

ggplot() + 
  #geom_sf(data = places14, fill = "grey") + 
  geom_sf(data = b_int_14, color = "red", fill = "transparent")

# compare annexed blocks to block shapefiles 
aa1420 <- read_csv("analyticalfiles/annexedblocks1420dem.csv") %>%
  filter(plid == "1365856" & annexed==1)

b14 <- blocks14 %>%
  filter(GEOID10 %in% aa1420$blkid)

ggplot() + 
  geom_sf(data = places14, fill = "grey") + 
  geom_sf(data = b14, color = "red", fill = "transparent")

# 1342604 ####
blocks07 <- st_read("SHP_blk_0010/2007/states/GA_13_allblocks.shp") %>%
  st_transform(., 3488)

blocks07 %<>%
  left_join(cw, by = c("BLKIDFP" = "GEOID00")) %>%
  rename(blkid = GEOID10)  %>%
  filter(!is.na(blkid))

blocks13 <- st_read("SHP_blk_0010/2013/GA_13/tl_2013_13_tabblock.shp") %>%
  st_transform(., 3488) %>%
  filter(GEOID %in% blocks07$blkid)

# get part 3 
places13 <- st_read("SHP_pl/2013/GA_13/tl_2013_13_place.shp") %>%
  st_transform(., 3488)

places07 <- st_read("SHP_pl/2007/fe_2007_13_place.shp") %>%
  st_transform(., 3488)

places13 %<>%
  filter(GEOID == "1342604")

places07 %<>%
  filter(PLCIDFP == "1342604")

# 4.1 get intersection between "annexed" blocks and the place 
p1blocks_07 <- st_contains(places07, blocks07)
p1blocks_int_07 <- st_intersects(places07, blocks07)

p1blocks_13 <- st_contains(places13, blocks13)
p1blocks_int_13 <- st_intersects(places13, blocks13)

b07 <- blocks07[p1blocks_07[[1]],]
b_int_07 <- blocks07[p1blocks_int_07[[1]],]

b13 <- blocks13[p1blocks_13[[1]],]
b_int_13 <- blocks13[p1blocks_int_13[[1]],]

ggplot() + 
  geom_sf(data = places07, fill = "grey") + 
  geom_sf(data = b07, color = "red", fill = "transparent")

ggplot() + 
  geom_sf(data = places07, fill = "grey") + 
  geom_sf(data = b_int_07, color = "red", fill = "transparent")

ggplot() + 
  geom_sf(data = places13, fill = "grey") + 
  geom_sf(data = b13, color = "red", fill = "transparent")

ggplot() + 
  geom_sf(data = places13, fill = "grey") + 
  geom_sf(data = b_int_13, color = "red", fill = "transparent")

# test after spatial analysis process
blocks2007_plids <- read_csv("blocks2007_plids.csv") %>%
  filter(plid == "1342604")

b07 <- blocks07 %>%
  filter(BLKIDFP %in% blocks2007_plids$blkid)

ggplot() + 
  geom_sf(data = places07, fill = "grey") + 
  geom_sf(data = b07, color = "red", fill = "transparent")

blocks2007_buffers <- read_csv("blocks2007_buffers.csv") %>%
  filter(bufferplace == "1342604")

b07 <- blocks07 %>%
  filter(BLKIDFP %in% blocks2007_buffers$blkid)

ggplot() + 
  geom_sf(data = places07, fill = "grey") + 
  geom_sf(data = b07, color = "red", fill = "transparent")

blocks2013_plids <- read_csv("blocks2013_plids.csv") %>%
  filter(plid == "1342604")
b13 <- blocks13 %>%
  filter(GEOID %in% blocks2013_plids$blkid)

ggplot() + 
  geom_sf(data = places13, fill = "grey") + 
  geom_sf(data = b13, color = "red", fill = "transparent")

ggplot() + 
  #geom_sf(data = places07, fill = "grey") + 
  geom_sf(data = b_int_07, color = "red", fill = "transparent")

# 2014
blocks14 <- st_read("SHP_blk_0010/2014/GA_13/tl_2014_13_tabblock10.shp") %>%
  st_transform(., 3488) %>%
  mutate(area_blk = st_area(.))

places20 <- st_read("SHP_pl/2020/tl_2020_13_place.shp") %>%
  st_transform(., 3488)

places20 %<>%
  filter(GEOID == "1342604")

# 4.1 get intersection between "annexed" blocks and the place 
p1blocks_20 <- c(st_contains(places20, blocks14), st_overlaps(places20, blocks14))
b14 <- blocks14[unique(c(p1blocks_20[[1]], p1blocks_20[[2]])),]
ann_int <- st_intersection(b14, places20) %>%
  mutate(area_int = st_area(.),
         pct_int = (area_int/area_blk)*100, 
         pct_int = as.numeric(pct_int)) %>%
  filter(pct_int >= 90)

jones_ann <- aa1420 %>%
  filter(plid == "1342604" & annexed==1)
jones_ann_shp <- blocks14 %>%
  filter(GEOID10 %in% jones_ann$blkid)

p1blocks_14 <- st_contains(places14, blocks14)
b14 <- blocks14[p1blocks_14[[1]],]

a <- ggplot() + 
  geom_sf(data = ann_int, color = "red") +
  geom_sf(data = places20, fill = "transparent") +
  geom_sf(data = jones_ann_shp, color = "black") #+
  #geom_sf(data = b14, color = "red", fill = "transparent") + 
  

b <- ggplot() + 
  geom_sf(data = places14, fill = "grey") + 
  geom_sf(data = b14, color = "red", fill = "transparent")

ggsave(filename = "analyticalfiles/right_contains-overlap_Jonesboro-city.pdf",
       plot = a,
       dpi = 300)

ggsave(filename = "analyticalfiles/wrong_contains_Jonesboro-city.pdf",
       plot = b,
       dpi = 300)

ggplot() + 
  geom_sf(data = places14, fill = "grey") + 
  geom_sf(data = b_int_14, color = "red", fill = "transparent")

# test after spatial analysis process
blocks2014_plids <- read_csv("blocks2014_plids.csv") %>%
  filter(plid == "1342604")

b14 <- blocks14 %>%
  filter(GEOID10 %in% blocks2014_plids$blkid)

ggplot() + 
  #geom_sf(data = places14, fill = "grey") + 
  geom_sf(data = b14, color = "red", fill = "transparent")

ggplot() + 
  #geom_sf(data = places14, fill = "grey") + 
  geom_sf(data = b_int_14, color = "red", fill = "transparent")

# compare annexed blocks to block shapefiles ####
aa1420 <- read_csv("analyticalfiles/annexedblocks1420dem.csv") %>%
  filter(plid == "1342604" & annexed==1)

b14 <- blocks14 %>%
  filter(GEOID10 %in% aa1420$blkid)

annexed_14 <- st_contains(places14, b14)

b_ann_14 <- blocks14[annexed_14[[1]],]

ggplot() + 
  geom_sf(data = places14, fill = "grey") + 
  geom_sf(data = b14, color = "red", fill = "transparent")

# write a function that only returns 'annexed blocks' that are actually contained in the place
check_annexed <- function(state_code, blocks, t1) {
  state_fips <- substr(state_code, 4, 5)
  aa_filename <- ifelse(t1 == 2020, paste0("analyticalfiles/annexedblocks1420dem.csv"),
                        ifelse(t1 == 2013, paste0("analyticalfiles/annexedblocks0713dem.csv"), paste0("analyticalfiles/annexedblocks0007dem.csv")))
  aa1420 <- read_csv(aa_filename) %>%
    mutate(state = substr(blkid, 1, 2)) %>%
    filter(annexed==1 & state==state_fips)
  
  datalist <- list()
  places_df <- split(aa1420, f = aa1420$plid)
  
  blocks_filepath <- 
    ifelse(t1 == 2020, paste0("SHP_blk_0010/2014/", state_code, "/tl_2014_", substr(state_code, 4, 5), "_tabblock10.shp"), 
           ifelse(t1 == 2013, paste0("SHP_blk_0010/2007/blocks/states/", state_code, "_allblocks.shp"), paste0("SHP_blk_0010/2000/", state_code, "/tl_2010_", substr(state_code, 4, 5), "_tabblock00.shp")))
  
  blocks <- st_read(blocks_filepath) %>%
    st_transform(., 3488)
  
  blocks$blkid <- 
    ifelse(t1 %in% c(2020, 2007), 
           paste0(str_pad(as.character(STATEFP10), 2, side = "left", pad = "0"), 
                  str_pad(as.character(COUNTYFP10), 3, side = "left", pad = "0"),
                  str_pad(as.character(TRACTCE10), 6, side = "left", pad = "0"), 
                  str_pad(as.character(BLOCKCE10), 4, side = "left", pad = "0")), 
           paste0(str_pad(as.character(STATEFP00), 2, side = "left", pad = "0"), 
                  str_pad(as.character(COUNTYFP00), 3, side = "left", pad = "0"),
                  str_pad(as.character(TRACTCE00), 6, side = "left", pad = "0"), 
                  str_pad(as.character(BLOCKCE00), 4, side = "left", pad = "0")))
  
  places_filepath <- 
    ifelse(t1 == 2020, paste0("SHP_pl/2020/tl_2020_", substr(state_code, 4, 5), "_place.shp"), 
           ifelse(t1 == 2013, paste0("SHP_pl/2013/", state_code, "/tl_2013_", substr(state_code, 4, 5), "_place.shp"), paste0("SHP_pl/2007/fe_2007_", substr(state_code, 4, 5), "_place.shp")))
  
  places <- st_read(places_filepath) %>% 
    st_transform(., 3488)
  
  places$plid <- paste0(
    str_pad(as.character(STATEFP), 2, side = "left", pad = "0"), 
    str_pad(as.character(PLACEFP), 5, side = "left", pad = "0"))
  
  for (i in length(places_df)) {
    place_t1 <- places %>%
      filter(plid %in% places_df[[i]]$plid)
    
    block_t0 <- blocks %>%
      filter(blkid %in% places_df[[i]]$blkid)
    
  actually_wihin <- st_contains(place_t1, block_t0)
  if(nrow(as.data.frame(blocks[p1buffer_intersects[[1]],])) < 1) return(NULL)
  test <- as.data.frame(blocks[p1buffer_intersects[[1]],])
  test$bufferplace <- places_df[[i]]$plid[[1]]
  test %<>% 
    select(c(1:4), blkid, bufferplace)
  return(test)
  }
  
  buffers <- data.table::rbindlist(datalist) 
  readr::write_csv(buffers, file = paste0("shapefiles/2007/blocks/states/",  substr(state_code, 1, 2), "_buffers.csv"))
  
}




b14 <- blocks14 %>%
  filter(GEOID10 %in% aa1420$blkid)

annexed_14 <- st_contains(places14, b14)

b14 %<>%
  anti_join(annexed_14, )


ggplot() + 
  geom_sf(data = place, fill = "grey") + 
  geom_sf(data = block_buff, color = "red", fill = "transparent")

# why is 1979948 wrong...
blocks14 <- st_read("SHP_blk_0010/2014/GA_13/tl_2014_13_tabblock10.shp") %>%
  st_transform(., 3488)

places20 <- st_read("SHP_pl/2020/tl_2020_13_place.shp") %>%
  st_transform(., 3488)

places20 %<>%
  filter(GEOID == "1379948")

# 4.1 get intersection between "annexed" blocks and the place 
p1blocks_14 <- c(st_contains(places20, blocks14), st_overlaps(places20, blocks14))
b14 <- blocks14[unique(c(p1blocks_14[[1]], p1blocks_14[[2]])),]

a <- ggplot() + 
  geom_sf(data = places20, fill = "grey") + 
  geom_sf(data = b14, color = "red", fill = "transparent") + 
  ggtitle("2014 blocks on 2020 places, \ncontains and overlaps")
a

p1blocks_14 <- st_contains(places20, blocks14)
b14 <- blocks14[p1blocks_14[[1]],]

b <- ggplot() + 
  geom_sf(data = places20, fill = "grey") + 
  geom_sf(data = ann_int, color = "red", fill = "transparent") + 
  ggtitle("2014 blocks on 2020 places, \n90% contain thresh.")
b

# and 2014? 
places14 <- st_read("SHP_pl/2014/GA_13/tl_2014_13_place.shp") %>%
  st_transform(., 3488)

places14 %<>%
  filter(GEOID == "1379948")

# 4.1 get intersection between "annexed" blocks and the place 
p1blocks_14 <- c(st_contains(places14, blocks14), st_overlaps(places14, blocks14))
b14 <- blocks14[unique(c(p1blocks_14[[1]], p1blocks_14[[2]])),]

c <- ggplot() + 
  geom_sf(data = places14, fill = "grey") + 
  geom_sf(data = b14, color = "red", fill = "transparent") + 
  ggtitle("2014 blocks on 2014 places, \ncontains and overlaps")
c

p1blocks_14 <- st_contains(places14, blocks14)
b14 <- blocks14[p1blocks_14[[1]],]

d <- ggplot() + 
  geom_sf(data = places14, fill = "grey") + 
  geom_sf(data = b14, color = "red", fill = "transparent") + 
  ggtitle("2014 blocks on 2014 places, \ncontains only")
d

grid <- ggarrange(a, b, c, d, 
               nrow = 2, ncol = 2) 
grid

ggsave("analyticalfiles/differing_boundaries_2014-2020.pdf",
       grid, 
       dpi = 300)

ggsave("analyticalfiles/differing_boundaries_fixed_2014-2020.pdf",
       grid, 
       dpi = 300)

# intersection of 2014 blocks on 2020 boundaries 
blocks14 <- st_read("SHP_blk_0010/2014/GA_13/tl_2014_13_tabblock10.shp") %>%
  st_transform(., 3488) %>%
  mutate(area_blk = st_area(.))

p1blocks_14 <- c(st_contains(places20, blocks14), st_overlaps(places20, blocks14))
b14 <- blocks14[unique(c(p1blocks_14[[1]], p1blocks_14[[2]])),]

# 4.1 get intersection between "annexed" blocks and the place 
ann_int <- st_intersection(b14, places20) %>%
  mutate(area_int = st_area(.),
         pct_int = (area_int/area_blk)*100, 
         pct_int = as.numeric(pct_int)) %>%
  filter(pct_int >= 90)

ggplot() +  
  geom_sf(data = places20, size = 0.4, fill = "white") +
  geom_sf(data = ann_int, size = 0.4, fill= "grey", color = "red")

places20 <- st_read("SHP_pl/2020/tl_2020_13_place.shp") %>%
  st_transform(., 3488)

places20 %<>%
  filter(GEOID == "1379948")

# 4.1 get intersection between "annexed" blocks and the place 

a <- ggplot() + 
  geom_sf(data = places20, fill = "grey") + 
  geom_sf(data = b14, color = "red", fill = "transparent") + 
  ggtitle("2014 blocks on 2020 places, \ncontains and overlaps")
a

p1blocks_14 <- st_contains(places20, blocks14)
b14 <- blocks14[p1blocks_14[[1]],]

b <- ggplot() + 
  geom_sf(data = places20, fill = "grey") + 
  geom_sf(data = b14, color = "red", fill = "transparent") + 
  ggtitle("2014 blocks on 2020 places, \ncontains only")
b

# what about on 2020 boundaries 
blocks20 <- st_read("SHP_blk_0010/2020/tl_2020_13_tabblock20.shp") %>%
  st_transform(., 3488)

places20 <- st_read("SHP_pl/2020/tl_2020_13_place.shp") %>%
  st_transform(., 3488)

places20 %<>%
  filter(GEOID == "1379948")

# 4.1 get intersection between "annexed" blocks and the place 
p1blocks_20 <- c(st_contains(places20, blocks20), st_overlaps(places20, blocks20))
b14 <- blocks20[unique(c(p1blocks_20[[1]], p1blocks_20[[2]])),]

a <- ggplot() + 
  geom_sf(data = places20, fill = "grey") + 
  geom_sf(data = b20, color = "red", fill = "transparent") + 
  ggtitle("2020 blocks on 2020 places, \ncontains and overlaps")
a

p1blocks_14 <- st_contains(places20, blocks14)
b14 <- blocks14[p1blocks_14[[1]],]

b <- ggplot() + 
  geom_sf(data = places20, fill = "grey") + 
  geom_sf(data = b14, color = "red", fill = "transparent") + 
  ggtitle("2014 blocks on 2020 places, \ncontains only")
b

# 2014 and 2010 comparison 
# why is 1979948 wrong...
blocks10 <- st_read("SHP_blk_0010/2010/GA_13/tl_2010_13_tabblock10.shp") %>%
  st_transform(., 3488)

places10 <- st_read("SHP_pl/2010/GA_13/tl_2010_13_place10.shp") %>%
  st_transform(., 3488)

places10 %<>%
  mutate(plid = paste0(str_pad(STATEFP10, 2, "left", "0"), 
                       str_pad(PLACEFP10, 5, "left", "0"))) %>%
  filter(plid == "1379948")

# 4.1 get intersection between "annexed" blocks and the place 
p1blocks_10 <- c(st_contains(places14, blocks10), st_overlaps(places14, blocks10))
b10 <- blocks10[unique(c(p1blocks_10[[1]], p1blocks_10[[2]])),]

a <- ggplot() + 
  geom_sf(data = places14, fill = "grey") + 
  geom_sf(data = b10, color = "red", fill = "transparent") + 
  ggtitle("2010 blocks on 2014 places, \ncontains and overlaps")
a

p1blocks_10 <- st_contains(places14, blocks10)
b10 <- blocks10[p1blocks_10[[1]],]

b <- ggplot() + 
  geom_sf(data = places14, fill = "grey") + 
  geom_sf(data = b10, color = "red", fill = "transparent") + 
  ggtitle("2010 blocks on 2014 places, \ncontains only")
b

c <- ggarrange(a, b)
c

# check whether the identified contained blocks are right
blocks14_20 <- read_csv("2014blk-2020plid.csv")
blocks14 <- st_read(paste0("SHP_blk_0010/2014/", state_code, "/tl_2014_", substr(state_code, 4, 5), "_tabblock10.shp")) %>%
  st_transform(., 3488) %>%
  mutate(area_blk = st_area(.)) %>%
  left_join(blocks14_20 %>% select(blkid, plid), by = c("GEOID10" = "blkid"))

places20 <- st_read(paste0("SHP_pl/2020/tl_2020_", substr(state_code, 4, 5), "_place.shp")) %>%
  st_transform(., 3488) %>%
  mutate(plid = paste0(str_pad(.[[1]], 2, "left", "0"),
                       str_pad(.[[2]], 5, "left", "0")))

places_df <- split(places20, f = places20$plid)
datalist <- list()
datalist <- foreach (i = 1:length(places_df)) %dopar% {
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

# 4.1 get intersection between "annexed" blocks and the place 

