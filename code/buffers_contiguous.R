# Create universe of annexable blocks in the 14 states ####
# 400-m buffers (Durst 2018) for every place ##############
# return .csvs for merging with Census data ###############
# Created by: Iris Zhang, Sept 27, 2021 ###################

# updates log ----

library("rgeos")
library("sf")

setwd("~/Google Drive/Stanford/QE2")

# first need to collapse block file to show places 
# find all blocks within 400-m buffer of every place in 2000 
# this forms universe of "annexable" blocks 
# I will use alabama as test, then use a loop for the remaining states 
al_contig <- st_read("Shapefiles_archived/Shapefiles_to_merge/AL_01/AL_block_2000.shp")
al_contig <- st_transform(al_contig, 3488) # Albers for flattening to get meters

al_places <- st_read("pl/AL_01/tl_2010_01_place00.shp")
al_places <- st_transform(al_places, 3488)
al_places <- al_places %>% 
  mutate(PLACEFP00 = as.character(PLACEFP00),
         STATEFP00 = as.character(STATEFP00)) %>%
  filter(!is.na(PLACEFP00) & PLACEFP00 != "99999") 

p1 <- al_places[1,]
p1buffer <- st_buffer(p1, 400)
p1buffer_intersects <- st_intersects(p1buffer, al_contig)
test <- al_contig[p1buffer_intersects[[1]],]

for (i in 1:length(unique(al_places$PLACEFP00))) { # run a loop for every place in AL 
  p1 <- al_places[i,]
  p1buffer <- st_buffer(p1, 400)
  p1buffer_intersects <- st_intersects(p1buffer, al_contig)
  test <- as.data.frame(al_contig[p1buffer_intersects[[1]],])
  test$contigplace <- unique(al_places$PLACEFP00)[i]
  if (i == 1) {
    al <- test
  } else {
    al <- base::rbind(al, test)
  }
}

al <- al %>% dplyr::select(
  FIPSSTCO, TRACT2000, BLOCK2000, GISJOIN, contigplace
)

write_csv(al, "Shapefiles_contig/AL_01/AL_contig.csv")

get_buffers <- function(state_code) {
  blocks <- st_read(paste0("Shapefiles_archived/Shapefiles_to_merge/", state_code, "/", substr(state_code, 1, 2), "_block_2000.shp"))
  blocks <- st_transform(blocks, 3488)
  
  places <- st_read(paste0("pl/", state_code, "/tl_2010_", substr(state_code, 4, 5), "_place00.shp"))
  places <- st_transform(places, 3488)
  places <- places %>% 
    mutate(PLACEFP00 = as.character(PLACEFP00)) %>%
    filter(!is.na(PLACEFP00) & PLACEFP00 != "99999")
  
  for (i in 1:length(unique(places$PLACEFP00))) {
    p1 <- places[i,]
    p1buffer <- st_buffer(p1, 400)
    p1buffer_intersects <- st_intersects(p1buffer, blocks)
    test <- as.data.frame(blocks[p1buffer_intersects[[1]],])
    test$contigplace <- unique(places$PLACEFP00)[i]
    if (i == 1) {
      al <- test
    } else {
      al <- base::rbind(al, test)
    }
  }
  
  al <- al %>% dplyr::select(
    FIPSSTCO, TRACT2000, BLOCK2000, GISJOIN, contigplace
  )
  
  write_csv(al, file = paste0("Shapefiles_contig/", state_code, "/", substr(state_code, 1, 2), "_contig.csv"))
}

state_codes <- c("AR_05", "DE_10", "GA_13", "KY_21", "LA_22", "MD_24", "MS_28", "NC_37", "ND_38",
                 "SC_45", "SD_46", "TN_47", "VA_51")

for (state_code in state_codes) {
  get_buffers(state_code)
}
