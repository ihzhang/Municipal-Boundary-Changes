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

setwd("~/Google Drive/Stanford/QE2")

# first need to collapse block file to show places 
# find all blocks within 400-m buffer of every place in 2000 
# this forms universe of "annexable" blocks 
# I will use alabama as test, then use a loop for the remaining states 
al_contig <- st_read("SHP_blk_0010/2000/TX_48/tl_2010_48_tabblock00.shp")
al_contig <- st_transform(al_contig, 3488) # Albers for flattening to get meters

al_contig <- al_contig %>%
  mutate(blkid = as.character(BLKIDFP00))
  
# should only retain those not already part of a place
blocks2000 <- fread("ipumsblocks_allstates/2000blocks/nhgis0032_csv/nhgis0032_ds147_2000_block.csv", select = 
                      c("GISJOIN", "STATEA", "COUNTYA", "TRACTA", "BLOCKA", "PLACEA"))
blocks2000 <- blocks2000[-1, ]
blocks2000 <- blocks2000 %>%
  mutate(blkid = paste0(str_pad(STATEA, 2, side = "left", pad = "0"), str_pad(COUNTYA, 3, side = "left", pad = "0"),
                        str_pad(TRACTA, 6, side = "left", pad = "0"), str_pad(BLOCKA, 4, side = "left", pad = "0")))

al_contig <- al_contig %>%
  left_join(blocks2000 %>% dplyr::select(blkid, PLACEA), 
            by = "blkid") %>% 
  filter(is.na(PLACEA) | PLACEA=="99999") 

al_places <- st_read("SHP_pl/TX_48/tl_2010_48_place00.shp")
al_places <- st_transform(al_places, 3488)

ggplot() + 
  geom_sf(data = al_contig, fill = "grey", color = "grey") +
  geom_sf(data = al_places, fill = "black", color = "black") 

al_places <- al_places %>% 
  mutate(PLACEFP00 = as.character(PLACEFP00),
         STATEFP00 = as.character(STATEFP00),
         PLCIDFP00 = as.character(PLCIDFP00)) %>%
  filter(!is.na(PLACEFP00) & PLACEFP00 != "99999") 

p1 <- al_places[100,]
p1buffer <- st_buffer(p1, 400)
p1buffer_intersects <- st_intersects(p1buffer, al_contig)
test <- al_contig[p1buffer_intersects[[1]],]

for (i in 1:length(unique(al_places$PLCIDFP00))) { # run a loop for every place in AL 
  p1 <- al_places[i,]
  p1buffer <- st_buffer(p1, 400)
  p1buffer_intersects <- st_intersects(p1buffer, al_contig)
  
  test <- as.data.frame(al_contig[p1buffer_intersects[[1]],])
  test$contigplace <- unique(al_places$PLCIDFP00)[i]
  if (i == 1) {
    al <- test
  } else {
    al <- base::rbind(al, test)
  }
}

get_buffers <- function(state_code) {
  blocks <- st_read(paste0("SHP_blk_0010/2000/", state_code, "/tl_2010_", substr(state_code, 4, 5), "_tabblock00.shp"))
  blocks <- st_transform(blocks, 3488)
  blocks <- blocks %>%
    mutate(blkid = as.character(BLKIDFP00))
  
  # should only retain those not already part of a place
  blocks <- blocks %>%
    left_join(blocks2000 %>% dplyr::select(blkid, PLACEA, GISJOIN), 
              by = "blkid") %>% 
    filter(is.na(PLACEA) | PLACEA=="99999") 
  
  places <- st_read(paste0("SHP_pl/", state_code, "/tl_2010_", substr(state_code, 4, 5), "_place00.shp"))
  places <- st_transform(places, 3488)
  places <- places %>% 
    mutate(PLACEFP00 = as.character(PLACEFP00)) %>%
    filter(!is.na(PLACEFP00) & PLACEFP00 != "99999")
  
  for (i in 1:length(unique(places$PLCIDFP00))) {
    p1 <- places[i,]
    p1buffer <- st_buffer(p1, 400)
    p1buffer_intersects <- st_intersects(p1buffer, blocks)
    if(nrow(as.data.frame(blocks[p1buffer_intersects[[1]],])) < 1) {
      next
    } else {
      test <- as.data.frame(blocks[p1buffer_intersects[[1]],])
      test$contigplace <- unique(places$PLCIDFP00)[i]
      if (i == 1) {
        al <- test
      } else {
        al <- base::rbind(al, test)
      }
    }
  }
  
  al <- al %>% dplyr::select(
    STATEFP00, COUNTYFP00, TRACTCE00, BLOCKCE00, blkid, GISJOIN, contigplace
  )
  
  write_csv(al, file = paste0("SHP_blk_0010/2000/", state_code, "/", substr(state_code, 1, 2), "_contig.csv"))
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

state_codes <- c("UT_49", "VT_50", "VA_51",
                 "WA_53", "WV_54", "WI_55", "WY_56"
)

# missing PA, TX, DC
for (state_code in state_codes) {
  get_buffers(state_code)
  print(state_code)
}
