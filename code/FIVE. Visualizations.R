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
library("sf")

# compare place boundaries for 2000 and 2010 for an annexing place 
# compared to block boundaries for annexable in 2000 and actually annexed in 2010 

pl_annex_var_0010 <- read_csv("analyticalfiles/pl_annex_var_0010.csv")
indices <- grep("^vap_total$", names(pl_annex_var_0010))
indices <- names(pl_annex_var_0010)[indices]
rm(pl_annex_var_0010)

NE <- c("09", "23", "25", "33", "34", "36", "42", "24", "44", "50", "15")
pl0010 <- read_csv("analyticalfiles/pl_annex_var_0010.csv") %>%
  mutate(STATE = substr(plid, 1, 2)) %>%
  filter(!(STATE %in% NE)) %>%
  filter(annexing==1) %>%
  filter_at(all_of(indices), ~(.>0)) %>%
  filter(STATE=="06") %>%
  filter(pct_annexed == 0.5)

plids_viz <- pl0010$plid

# 2000 places 
ca_pl00 <- st_read("SHP_pl/2000/CA_06/tl_2010_06_place00.shp") %>%
  filter(PLCIDFP00 %in% plids_viz) %>%
  st_transform(., 3488)

# ca_pl10 <- st_read("SHP_pl/2010/CA_06/tl_2010_06_place10.shp") %>%
#  filter(GEOID10 %in% plids_viz)

# get contig blocks and actually annexed blocks for each of those places
ca_blks00 <- read_csv("SHP_blk_0010/2000/CA_06/CA_buffers.csv") %>%
  filter(bufferplace %in% plids_viz) %>%
  select(blkid, bufferplace) %>%
  rename(plid = bufferplace)

# # grab placeids 
# ca_blkplids <- read_csv("blocks2000_var.csv") %>%
#   mutate(plid = paste0(str_pad(STATEA, 2, "left", "0"), str_pad(PLACEA, 5, "left", "0"))) %>%
#   filter(plid %in% plids_viz)

ca_blk00 <- st_read("SHP_blk_0010/2000/CA_06/tl_2010_06_tabblock00.shp") %>%
  filter(BLKIDFP00 %in% ca_blks00$blkid) %>%
  st_transform(., 3488) %>%
  left_join(ca_blks00, by = c("BLKIDFP00" = "blkid"))

# get which blocks were annexed 
aa0010 <- read_csv("analyticalfiles/annexedblocks0010dem_pl00_newsample_unincorp.csv") %>%
  filter(annexed == 1 & plid %in% plids_viz & blkid %in% ca_blks00$blkid)
ca_blk00 %<>%
  mutate(annexed = ifelse(BLKIDFP00 %in% aa0010$blkid, 1, 0))
table(ca_blk00$annexed)

g <- ggplot() + 
  geom_sf(data = ca_blk00 %>% filter(plid == plids_viz[[1]]), aes(fill = as.factor(annexed))) + 
  geom_sf(data = ca_pl00 %>% filter(PLCIDFP00 == plids_viz[[1]]), fill = "black") + 
  labs(fill='Annexed',
       title = "Annexations for Orland City, CA, 2000-2010") +
  scale_fill_grey(start = 0.7, end = 0.4) 
g
ggsave(filename = "analyticalfiles/Orland_annex.png",
       plot = g,
       dpi = 300)

g <- ggplot() + 
  geom_sf(data = ca_blk00 %>% filter(plid == plids_viz[[5]]), aes(fill = as.factor(annexed))) + 
  geom_sf(data = ca_pl00 %>% filter(PLCIDFP00 == plids_viz[[5]]), fill = "black") + 
  labs(fill='Annexed',
       title = "Annexations for Live Oak City, CA, 2000-2010") +
  scale_fill_grey(start = 0.7, end = 0.4) 
g
ggsave(filename = "analyticalfiles/LiveOak_annex.png",
       plot = g,
       dpi = 300)

#biggest city in TX 
pl0010 <- read_csv("analyticalfiles/pl_annex_var_0010.csv") %>%
  mutate(STATE = substr(plid, 1, 2)) %>%
  filter(!(STATE %in% NE)) %>%
  filter(annexing==1) %>%
  filter_at(all_of(indices), ~(.>0)) %>%
  filter(STATE=="48") %>%
  filter(plid=="4835000")

plids_viz <- pl0010$plid

# 2000 places 
tx_pl00 <- st_read("SHP_pl/2000/TX_48/tl_2010_48_place00.shp") %>%
  filter(PLCIDFP00 %in% plids_viz) %>%
  st_transform(., 3488)

# ca_pl10 <- st_read("SHP_pl/2010/CA_06/tl_2010_06_place10.shp") %>%
#  filter(GEOID10 %in% plids_viz)

# get contig blocks and actually annexed blocks for each of those places
tx_blks00 <- read_csv("SHP_blk_0010/2000/TX_48/TX_buffers.csv") %>%
  filter(bufferplace %in% plids_viz) %>%
  select(blkid, bufferplace) %>%
  rename(plid = bufferplace) %>%
  mutate(blkid = as.character(blkid))

# # grab placeids 
# ca_blkplids <- read_csv("blocks2000_var.csv") %>%
#   mutate(plid = paste0(str_pad(STATEA, 2, "left", "0"), str_pad(PLACEA, 5, "left", "0"))) %>%
#   filter(plid %in% plids_viz)

tx_blk00 <- st_read("SHP_blk_0010/2000/TX_48/tl_2010_48_tabblock00.shp") %>%
  filter(BLKIDFP00 %in% tx_blks00$blkid) %>%
  st_transform(., 3488) %>%
  left_join(tx_blks00, by = c("BLKIDFP00" = "blkid"))

# get which blocks were annexed 
aa0010 <- read_csv("analyticalfiles/annexedblocks0010dem_pl00_newsample_unincorp.csv") %>%
  filter(annexed == 1 & plid %in% plids_viz & blkid %in% tx_blks00$blkid)
tx_blk00 %<>%
  mutate(annexed = ifelse(BLKIDFP00 %in% aa0010$blkid, 1, 0))
table(tx_blk00$annexed)

g <- ggplot() + 
  geom_sf(data = tx_blk00, aes(fill = as.factor(annexed))) + 
  scale_fill_grey(start = 0.7, end = 0.4) +
  labs(fill = "Annexed") +
  geom_sf(data = tx_pl00, fill = "black", color = "black") +
  scale_fill_manual(color = "black", name = "Houston") +
  labs(
    title = "Annexations for Houston, TX, 2000-2010") 
g
ggsave(filename = "analyticalfiles/Houston_annex.png",
       plot = g,
       dpi = 300)

# biggest city in CA ####
pl0010 <- read_csv("analyticalfiles/pl_annex_var_0010.csv") %>%
  mutate(STATE = substr(plid, 1, 2)) %>%
  filter(!(STATE %in% NE)) %>%
  filter(annexing==1) %>%
  filter_at(all_of(indices), ~(.>0)) %>%
  filter(STATE=="06") %>%
  filter(plid=="0603526")

plids_viz <- pl0010$plid

# 2014 places 
ca_pl14 <- st_read("SHP_pl/2014/CA_06/tl_2014_06_place.shp") %>%
  filter(PLACEFP %in% plids_viz) %>%
  st_transform(., 3488)

# ca_pl10 <- st_read("SHP_pl/2010/CA_06/tl_2010_06_place10.shp") %>%
#  filter(GEOID10 %in% plids_viz)

# get contig blocks and actually annexed blocks for each of those places
ca_blks14 <- read_csv("SHP_blk_0010/2014/CA_06/CA_buffers.csv") %>%
  filter(bufferplace %in% aa1420$plid) %>%
  select(blkid, bufferplace) %>%
  rename(plid = bufferplace)

# # grab placeids 
# ca_blkplids <- read_csv("blocks2000_var.csv") %>%
#   mutate(plid = paste0(str_pad(STATEA, 2, "left", "0"), str_pad(PLACEA, 5, "left", "0"))) %>%
#   filter(plid %in% plids_viz)

ca_blk14 <- st_read("SHP_blk_0010/2014/CA_06/tl_2014_06_tabblock10.shp") %>%
  filter(GEOID10 %in% ca_blks14$blkid) %>%
  st_transform(., 3488) %>%
  left_join(ca_blks14, by = c("GEOID10" = "blkid"))

# get which blocks were annexed 
aa1420 <- read_csv("analyticalfiles/annexedblocks1420dem.csv") %>%
  filter(blkid %in% ca_blk14$GEOID10)
ca_blk14 %<>%
  left_join(aa1420, by = c("GEOID10" = "blkid"))
table(ca_blk14$annexed)

g <- ggplot() + 
  geom_sf(data = ca_blk14, size = 0.1, aes(fill = as.factor(annexed))) + 
  geom_sf(data = ca_pl14, size = 0.1, fill = "black") + 
  labs(fill='Annexed',
       title = "Annexations for Bakersfield, CA, 2014-2020") +
  scale_fill_grey(start = 0.7, end = 0.4) 
g
ggsave(filename = "analyticalfiles/Bakersfield_annex.png",
       plot = g,
       dpi = 300)

# biggest underbound_black city
panel0020_did %>%
  filter(period == 1 & underbound_black==1) %>%
  arrange(desc(pop_p0))

pl1420 <- read_csv("analyticalfiles/annexedblocks1420dem.csv") %>%
  filter(plid=="0603526")

plids_viz <- substr(unique(pl1420$plid), 3, 7)

# 2014 places 
ca_pl14 <- st_read("SHP_pl/2014/CA_06/tl_2014_06_place.shp") %>%
  filter(PLACEFP %in% plids_viz) %>%
  st_transform(., 3488)

# get contig blocks and actually annexed blocks for each of those places
ca_blks14 <- read_csv("SHP_blk_0010/2014/CA_06/CA_buffers.csv") %>%
  filter(bufferplace %in% pl1420$plid) %>%
  select(blkid, bufferplace) %>%
  rename(plid = bufferplace)

# # grab placeids 
# ca_blkplids <- read_csv("blocks2000_var.csv") %>%
#   mutate(plid = paste0(str_pad(STATEA, 2, "left", "0"), str_pad(PLACEA, 5, "left", "0"))) %>%
#   filter(plid %in% plids_viz)
ca_blk14 <- st_read("SHP_blk_0010/2014/CA_06/tl_2014_06_tabblock10.shp") %>%
  filter(GEOID10 %in% ca_blks14$blkid) %>%
  st_transform(., 3488) %>%
  left_join(ca_blks14, by = c("GEOID10" = "blkid"))

# get which blocks were annexed 
aa1420 <- read_csv("analyticalfiles/annexedblocks1420dem.csv") %>%
  filter(blkid %in% ca_blk14$GEOID10)
ca_blk14 %<>%
  left_join(aa1420, by = c("GEOID10" = "blkid"))
table(ca_blk14$annexed)

annexed <- ca_blk14 %>% filter(annexed == 1)

nhb <- ggplot() +  
  geom_sf(data = ca_blk14, size = 0.0001, aes(fill = pctnhblack)) + 
  geom_sf(data = annexed, color = "red", size = 0.25, aes(fill = pctnhblack)) + 
  scale_fill_continuous() + 
  geom_sf(data = ca_pl14, size = 0.1, fill = "grey") + 
  labs(color='Annexed',
       fill = "% Non-Hispanic Black",
       title = "Annexations for Bakersfield, CA, 2014-2020") 
nhb

h <- ggplot() +  
  geom_sf(data = ca_blk14, size = 0.0001, aes(fill = pcth)) + 
  geom_sf(data = annexed, color = "red", size = 0.25, aes(fill = pcth)) + 
  scale_fill_continuous() + 
  geom_sf(data = ca_pl14, size = 0.1, fill = "grey") + 
  labs(color='Annexed',
       fill = "% Hispanic") 
h

nhw <- ggplot() +  
  geom_sf(data = ca_blk14, size = 0.0001, aes(fill = pctnhwhite)) + 
  geom_sf(data = annexed, color = "red", size = 0.25, aes(fill = pctnhwhite)) + 
  scale_fill_continuous() + 
  geom_sf(data = ca_pl14, size = 0.1, fill = "grey") + 
  labs(color='Annexed',
       fill = "% Non-Hispanic White", 
       caption = "Annexed Blocks in Red") 
nhw

bakersfield <- grid.arrange(nhb,
             h,
             nhw,                             
             ncol = 2, nrow = 2)
bakersfield
ggsave(filename = "analyticalfiles/Bakersfield_annex_race.png",
       plot = bakersfield,
       dpi = 300)

# underbound city with largest differential in annexable and city 
# 1309040
pl1420 <- read_csv("analyticalfiles/annexedblocks1420dem.csv") %>%
  filter(plid=="2855760")

plids_viz <- substr(unique(pl1420$plid), 3, 7)

# 2014 places 
pl14 <- st_read("SHP_pl/2014/MS_28/tl_2014_28_place.shp") %>%
  filter(PLACEFP %in% plids_viz) %>%
  st_transform(., 3488)

# get contig blocks and actually annexed blocks for each of those places
blks14 <- read_csv("SHP_blk_0010/2014/MS_28/MS_buffers.csv") %>%
  filter(bufferplace %in% pl1420$plid) %>%
  select(blkid, bufferplace) %>%
  rename(plid = bufferplace) %>%
  mutate(blkid = as.character(blkid)) %>%
  filter(!duplicated(blkid))

# # grab placeids 
# ca_blkplids <- read_csv("blocks2000_var.csv") %>%
#   mutate(plid = paste0(str_pad(STATEA, 2, "left", "0"), str_pad(PLACEA, 5, "left", "0"))) %>%
#   filter(plid %in% plids_viz)
blk14 <- st_read("SHP_blk_0010/2014/MS_28/tl_2014_28_tabblock10.shp") %>%
  filter(GEOID10 %in% blks14$blkid) %>%
  filter(!duplicated(GEOID10)) %>%
  st_transform(., 3488) %>%
  left_join(blks14, by = c("GEOID10" = "blkid"))

# get which blocks were annexed 
aa1420 <- read_csv("analyticalfiles/annexedblocks1420dem.csv") %>%
  filter(blkid %in% blk14$GEOID10)
blk14 %<>%
  left_join(aa1420, by = c("GEOID10" = "blkid")) %>%
  filter(pop > 0) 
table(blk14$annexed)

annexed <- blk14 %>% filter(annexed == 1)

nhb <- ggplot() +  
  geom_sf(data = pl14, size = 0.1, fill = "grey") + 
  geom_sf(data = blk14, size = 0.5, aes(fill = pctnhblack)) + 
  geom_sf(data = annexed, color = "red", size = 0.25, aes(fill = pctnhblack)) + 
  labs(color='Annexed',
       fill = "% Non-Hispanic Black",
       title = "Annexations for Pearl City, MS 2014-2020 (pop = 25,092, 26% Black)",
       caption = "Described as a \"sundown city\"") 
nhb

ggsave(filename = "analyticalfiles/Pearl MS_annex_race.png",
       plot = nhb,
       dpi = 300)

# tx 4806128
pl1420 <- read_csv("analyticalfiles/annexedblocks1420dem.csv") %>%
  filter(plid=="4806128")

plids_viz <- substr(unique(pl1420$plid), 3, 7)

# 2014 places 
pl14 <- st_read("SHP_pl/2014/TX_48/tl_2014_48_place.shp") %>%
  filter(PLACEFP %in% plids_viz) %>%
  st_transform(., 3488)

# get contig blocks and actually annexed blocks for each of those places
blks14 <- read_csv("SHP_blk_0010/2014/TX_48/TX_buffers.csv") %>%
  filter(bufferplace %in% pl1420$plid) %>%
  select(blkid, bufferplace) %>%
  rename(plid = bufferplace) %>%
  mutate(blkid = as.character(blkid)) %>%
  filter(!duplicated(blkid))

# # grab placeids 
# ca_blkplids <- read_csv("blocks2000_var.csv") %>%
#   mutate(plid = paste0(str_pad(STATEA, 2, "left", "0"), str_pad(PLACEA, 5, "left", "0"))) %>%
#   filter(plid %in% plids_viz)
blk14 <- st_read("SHP_blk_0010/2014/TX_48/tl_2014_48_tabblock10.shp") %>%
  filter(GEOID10 %in% blks14$blkid) %>%
  filter(!duplicated(GEOID10)) %>%
  st_transform(., 3488) %>%
  left_join(blks14, by = c("GEOID10" = "blkid"))

# get which blocks were annexed 
aa1420 <- read_csv("analyticalfiles/annexedblocks1420dem.csv") %>%
  filter(blkid %in% blk14$GEOID10)
blk14 %<>%
  left_join(aa1420, by = c("GEOID10" = "blkid")) %>%
  filter(pop > 0) 
table(blk14$annexed)

annexed <- blk14 %>% filter(annexed == 1)

nhb <- ggplot() +  
  geom_sf(data = pl14, size = 0.1, fill = "grey") + 
  geom_sf(data = blk14, size = 0.5, aes(fill = pctnhblack)) + 
  geom_sf(data = annexed, color = "red", size = 0.25, aes(fill = pctnhblack)) + 
  labs(color='Annexed',
       fill = "% Non-Hispanic Black",
       title = "Annexations for Baytown City, TX 2014-2020 (pop = 76,089, maj-Hispanic, 32% white)",
       caption = "Dilution of Black and Hispanic") 
nhb

h <- ggplot() +  
  geom_sf(data = pl14, size = 0.1, fill = "grey") + 
  geom_sf(data = blk14, size = 0.5, aes(fill = pcth)) + 
  geom_sf(data = annexed, color = "red", size = 0.25, aes(fill = pcth)) + 
  labs(color='Annexed',
       fill = "% Hispanic",
       title = "Annexations for Baytown City, TX 2014-2020 (pop = 76,089, maj-Hispanic, 32% white)",
       caption = "Dilution of Black and Hispanic") 
h

ggsave(filename = "analyticalfiles/Baytown TX_annex_race.png",
       plot = nhb,
       dpi = 300)
rm(list = ls())

