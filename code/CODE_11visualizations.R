# get environment ready 
setwd("~/Google Drive/My Drive/Stanford/QE2")

library("stringr")
library("dplyr")
library("ggplot2")
library("ggpubr")
library("stargazer")
library("tidyverse")
library("lme4")
library("readr")
library("data.table")
library("magrittr")
library("sf")

# place 1304000 ####
map_theme <- theme(
  plot.title = element_text(
    hjust = .5,
    vjust = 0.2,
    size = 8
  ),
  plot.margin = grid::unit(c(0, 0, 0, 0), "mm"),
  axis.title.x = element_blank(),
  axis.title.y = element_blank(),
  axis.text.x = element_blank(),
  axis.text.y = element_blank(),
  axis.ticks.x = element_blank(),
  axis.ticks.y = element_blank()
)

pl0910 <- read_csv("analyticalfiles/annexedblocks0910dem.csv") %>%
  filter(plid=="1304000")

plids_viz <- unique(pl0910$plid)

# t1 place 
pl09 <- st_read("SHP_pl/2009/tl_2009_13_place.shp") %>%
  filter(PLCIDFP %in% pl0910$plid) %>%
  st_transform(., 3488)

buff09 <- read_csv("2009buffers.csv") %>%
  filter(!duplicated(blkid))

buff09 %<>%
  filter(bufferplace %in% pl0910$plid & !is.na(blkid)) 

blk09 <- st_read("SHP_blk_0010/2009/tl_2009_13_tabblock.shp") %>% 
  #left_join(cw, by = c("BLKIDFP00" = "GEOID00")) %>%
  #mutate(blkid = GEOID10) %>%
  #filter(!is.na(blkid)) %>%
  rename(blkid = BLKIDFP) %>%
  filter(blkid %in% buff09$blkid) %>%
  st_transform(., 3488) %>%
  left_join(pl0910, by = c("blkid")) %>%
  mutate_at(vars(nhblack, nbmin), ~ifelse(pop == 0, 0, ((./pop)*100)))

# get which blocks were annexed 
annexed <- blk09 %>% filter(annexed == 1)

# Create common legend breaks for 
breaks <- c(0, 20, 40, 60, 80, 100)
limits = c(0, 100)

nhb_09 <- ggplot() +  
  geom_sf(data = pl09, size = 0.1, color = "black", fill = alpha("ghostwhite", 0.7)) + 
  geom_sf(data = blk09, size = 0.1, aes(fill = nhblack)) + 
  scale_fill_gradient(low="grey87", high="grey51", breaks = breaks, limits = limits) +
  geom_sf(data = annexed, size = 0.5, aes(fill = nhblack, color = "Annexed Block"), show.legend = "line") + 
  scale_color_manual(values = "red", guide = guide_legend(override.aes = list(fill = "red", color = "red"))) +  
  labs(fill = "% Non-Hispanic Black", 
       caption = "2009-2010") +
  theme(legend.position="none",
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank()) + 
  map_theme
nhb_09

# 1213
pl1213 <- read_csv("analyticalfiles/annexedblocks1213dem.csv") %>%
  filter(plid=="1304000")

plids_viz <- unique(pl1213$plid)

# t1 place 
pl12 <- st_read("SHP_pl/2012/tl_2012_13_place.shp") %>%
  filter(GEOID %in% pl1213$plid) %>%
  st_transform(., 3488)

buff12 <- read_csv("2012buffers.csv") %>%
  filter(!duplicated(blkid))

buff12 %<>%
  filter(bufferplace %in% pl1213$plid & !is.na(blkid)) 

blk12 <- st_read("SHP_blk_0010/2012/tl_2012_13_tabblock.shp") %>% 
  #left_join(cw, by = c("BLKIDFP00" = "GEOID00")) %>%
  #mutate(blkid = GEOID10) %>%
  #filter(!is.na(blkid)) %>%
  rename(blkid = GEOID) %>%
  filter(blkid %in% buff12$blkid) %>%
  st_transform(., 3488) %>%
  left_join(pl1213, by = c("blkid")) %>%
  mutate_at(vars(nhblack, nbmin), ~ifelse(pop == 0, 0, ((./pop)*100)))

# get which blocks were annexed 
annexed <- blk12 %>% filter(annexed == 1)

# Create common legend breaks for 
breaks <- c(0, 20, 40, 60, 80, 100)
limits = c(0, 100)

nhb_12 <- ggplot() +  
  geom_sf(data = pl12, size = 0.1, color = "black", fill = alpha("ghostwhite", 0.7)) + 
  geom_sf(data = blk12, size = 0.1, aes(fill = nhblack)) + 
  scale_fill_gradient(low="grey87", high="grey51", breaks = breaks, limits = limits) +
  #geom_sf(data = annexed, size = 0.5, aes(fill = nhblack, color = "Annexed Block"), show.legend = "line") + 
  scale_color_manual(values = "black", guide = guide_legend(override.aes = list(fill = "black", color = "black"))) +  
  labs(fill = "% Non-Hispanic Black", 
       caption = "2012-2013\n (no annexations)") +
  theme(legend.position="none",
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank()) + 
  map_theme
nhb_12

# 2015-2016
pl1516 <- read_csv("analyticalfiles/annexedblocks1516dem.csv") %>%
  filter(plid==plids_viz)

pl15 <- st_read("SHP_pl/2015/tl_2015_13_place.shp") %>%
  filter(GEOID %in% pl1516$plid) %>%
  st_transform(., 3488)

buff15 <- read_csv("2015buffers.csv") %>%
  filter(!duplicated(blkid))

buff15 %<>%
  filter(bufferplace %in% pl1516$plid) 

blk15 <- st_read("SHP_blk_0010/2015/tl_2015_13_tabblock10.shp") %>% 
  filter(GEOID10 %in% buff15$blkid) %>%
  st_transform(., 3488) %>%
  left_join(pl1516, by = c("GEOID10" = "blkid")) %>%
  mutate_at(vars(nhblack:nbmin), ~ifelse(pop == 0 | is.na(pop), 0, ((./pop)*100)))

annexed <- blk15 %>% 
  filter(annexed == 1)

nhb_15 <- ggplot() +  
  geom_sf(data = pl15, size = 0.1, color = "black", fill = alpha("ghostwhite", 0.7)) + 
  geom_sf(data = blk15, size = 0.1, aes(fill = nhblack)) + 
  scale_fill_gradient(low="grey87", high="grey51", breaks = breaks, limits = limits) +
  geom_sf(data = annexed, size = 0.75, aes(fill = nhblack, color = "Annexed Block"), show.legend = "line") + 
  scale_color_manual(values = "red", guide = guide_legend(override.aes = list(fill = "red", color = "red"))) +
  labs(fill = "% Non-Hispanic Black", 
       caption = "2015-2016") +
  theme(legend.position="none",
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank()) +
  map_theme
nhb_15

# 2017-2018
pl1718 <- read_csv("analyticalfiles/annexedblocks1718dem.csv") %>%
  filter(plid==plids_viz)

# 2014 places 
pl17 <- st_read("SHP_pl/2017/tl_2017_13_place/tl_2017_13_place.shp") %>%
  filter(GEOID %in% plids_viz) %>%
  st_transform(., 3488)

blk17 <- st_read("SHP_blk_0010/2017/tl_2017_13_tabblock10/tl_2017_13_tabblock10.shp") %>%
  filter(GEOID10 %in% pl1718$blkid) %>%
  filter(!duplicated(GEOID10)) %>%
  st_transform(., 3488) %>%
  left_join(pl1718, by = c("GEOID10" = "blkid")) %>%
  mutate_at(vars(nhblack:nbmin), ~ifelse(pop == 0, 0, ((./pop)*100)))

# which were annexed 
annexed <- blk17 %>%
  filter(annexed == 1)

nhb_17 <- ggplot() +  
  geom_sf(data = blk17, size = 0.1, aes(fill = nhblack)) + 
  geom_sf(data = pl17, size = 0.1, color = "black", fill = alpha("ghostwhite", 0.7)) + 
  scale_fill_gradient(low="grey87", high="grey51", breaks =  breaks, limits = limits) +
  geom_sf(data = annexed, size = 0.75, aes(fill = nhblack, color = "Annexed Block"), show.legend = "line") + 
  scale_color_manual(values = "red", guide = guide_legend(override.aes = list(fill = "red", color = "red"))) +
  labs(color='Annexed',
       fill = "% Non-Hispanic Black",
       caption = "2017-2018") +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank()) +
  map_theme
nhb_17

leg <- as_ggplot(get_legend(nhb_17))
nhb_17 <- nhb_17 + 
  theme(legend.position = "none")

nhb_attempt <- ggarrange(
  plotlist = list(nhb_09, nhb_12, leg, nhb_15, nhb_17), ncol = 3, nrow = 2, widths = c(2, 2, 2))
#  common.legend = F)

nhb_attempt

ggsave(paste0(savedir, "Atlanta_annex_annual.pdf"),
       plot = nhb_attempt)

# 1772000 ----
map_theme <- theme(
  plot.title = element_text(
    hjust = .5,
    vjust = 0.2,
    size = 8
  ),
  plot.margin = grid::unit(c(0, 0, 0, 0), "mm"),
  axis.title.x = element_blank(),
  axis.title.y = element_blank(),
  axis.text.x = element_blank(),
  axis.text.y = element_blank(),
  axis.ticks.x = element_blank(),
  axis.ticks.y = element_blank()
)

pl0910 <- read_csv("analyticalfiles/annexedblocks0910dem.csv") %>%
  filter(plid=="1772000")

plids_viz <- unique(pl0910$plid)

# t1 place 
pl09 <- st_read("SHP_pl/2009/tl_2009_17_place.shp") %>%
  filter(PLCIDFP %in% pl0910$plid) %>%
  st_transform(., 3488)

buff09 <- read_csv("2009buffers.csv") %>%
  filter(!duplicated(blkid))

buff09 %<>%
  filter(bufferplace %in% pl0910$plid & !is.na(blkid)) 

blk09 <- st_read("SHP_blk_0010/2009/tl_2009_17_tabblock.shp") %>% 
  #left_join(cw, by = c("BLKIDFP00" = "GEOID00")) %>%
  #mutate(blkid = GEOID10) %>%
  #filter(!is.na(blkid)) %>%
  rename(blkid = BLKIDFP) %>%
  filter(blkid %in% buff09$blkid) %>%
  st_transform(., 3488) %>%
  left_join(pl0910, by = c("blkid")) %>%
  mutate_at(vars(nhblack, nbmin), ~ifelse(pop == 0, 0, ((./pop)*100)))

# get which blocks were annexed 
annexed <- blk09 %>% filter(annexed == 1)

# Create common legend breaks for 
breaks <- c(0, 20, 40, 60, 80, 100)
limits = c(0, 100)

nhb_09 <- ggplot() +  
  geom_sf(data = pl09, size = 0.1, color = "black", fill = alpha("ghostwhite", 0.7)) + 
  geom_sf(data = blk09, size = 0.1, aes(fill = nhblack)) + 
  scale_fill_gradient(low="grey87", high="grey51", breaks = breaks, limits = limits) +
  geom_sf(data = annexed, size = 0.5, aes(fill = nhblack, color = "Annexed Block"), show.legend = "line") + 
  scale_color_manual(values = "red", guide = guide_legend(override.aes = list(fill = "red", color = "red"))) +  
  labs(fill = "% Non-Hispanic Black", 
       caption = "2009-2010") +
  theme(legend.position="none",
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank()) + 
  map_theme
nhb_09

# 1213
pl1213 <- read_csv("analyticalfiles/annexedblocks1213dem.csv") %>%
  filter(plid==plids_viz)

# t1 place 
pl12 <- st_read("SHP_pl/2012/tl_2012_17_place.shp") %>%
  filter(GEOID %in% pl1213$plid) %>%
  st_transform(., 3488)

buff12 <- read_csv("2012buffers.csv") %>%
  filter(!duplicated(blkid))

buff12 %<>%
  filter(bufferplace %in% pl1213$plid & !is.na(blkid)) 

blk12 <- st_read("SHP_blk_0010/2012/tl_2012_17_tabblock.shp") %>% 
  #left_join(cw, by = c("BLKIDFP00" = "GEOID00")) %>%
  #mutate(blkid = GEOID10) %>%
  #filter(!is.na(blkid)) %>%
  rename(blkid = GEOID) %>%
  filter(blkid %in% buff12$blkid) %>%
  st_transform(., 3488) %>%
  left_join(pl1213, by = c("blkid")) %>%
  mutate_at(vars(nhblack, nbmin), ~ifelse(pop == 0, 0, ((./pop)*100)))

# get which blocks were annexed 
annexed <- blk12 %>% filter(annexed == 1)

# Create common legend breaks for 
breaks <- c(0, 20, 40, 60, 80, 100)
limits = c(0, 100)

nhb_12 <- ggplot() +  
  geom_sf(data = pl12, size = 0.1, color = "black", fill = alpha("ghostwhite", 0.7)) + 
  geom_sf(data = blk12, size = 0.1, aes(fill = nhblack)) + 
  scale_fill_gradient(low="grey87", high="grey51", breaks = breaks, limits = limits) +
  geom_sf(data = annexed, size = 0.5, aes(fill = nhblack, color = "Annexed Block"), show.legend = "line") + 
  scale_color_manual(values = "red", guide = guide_legend(override.aes = list(fill = "red", color = "red"))) +  
  labs(fill = "% Non-Hispanic Black", 
       caption = "2012-2013") +
  theme(legend.position="none",
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank()) + 
  map_theme
nhb_12

# 2015-2016
pl1516 <- read_csv("analyticalfiles/annexedblocks1516dem.csv") %>%
  filter(plid==plids_viz)

pl15 <- st_read("SHP_pl/2015/tl_2015_17_place.shp") %>%
  filter(GEOID %in% pl1516$plid) %>%
  st_transform(., 3488)

buff15 <- read_csv("2015buffers.csv") %>%
  filter(!duplicated(blkid))

buff15 %<>%
  filter(bufferplace %in% pl1516$plid) 

blk15 <- st_read("SHP_blk_0010/2015/tl_2015_17_tabblock10.shp") %>% 
  filter(GEOID10 %in% buff15$blkid) %>%
  st_transform(., 3488) %>%
  left_join(pl1516, by = c("GEOID10" = "blkid")) %>%
  mutate_at(vars(nhblack:nbmin), ~ifelse(pop == 0 | is.na(pop), 0, ((./pop)*100)))

annexed <- blk15 %>% 
  filter(annexed == 1)

nhb_15 <- ggplot() +  
  geom_sf(data = pl15, size = 0.1, color = "black", fill = alpha("ghostwhite", 0.7)) + 
  geom_sf(data = blk15, size = 0.1, aes(fill = nhblack)) + 
  scale_fill_gradient(low="grey87", high="grey51", breaks = breaks, limits = limits) +
  #geom_sf(data = annexed, size = 0.75, aes(fill = nhblack, color = "Annexed Block"), show.legend = "line") + 
  scale_color_manual(values = "red", guide = guide_legend(override.aes = list(fill = "red", color = "red"))) +
  labs(fill = "% Non-Hispanic Black", 
       caption = "2015-2016\n (no annexations)") +
  theme(legend.position="none",
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank()) +
  map_theme
nhb_15

# 2017-2018
pl1718 <- read_csv("analyticalfiles/annexedblocks1718dem.csv") %>%
  filter(plid==plids_viz)

# 2014 places 
pl17 <- st_read("SHP_pl/2017/tl_2017_17_place/tl_2017_17_place.shp") %>%
  filter(GEOID %in% plids_viz) %>%
  st_transform(., 3488)

blk17 <- st_read("SHP_blk_0010/2017/tl_2017_17_tabblock10/tl_2017_17_tabblock10.shp") %>%
  filter(GEOID10 %in% pl1718$blkid) %>%
  filter(!duplicated(GEOID10)) %>%
  st_transform(., 3488) %>%
  left_join(pl1718, by = c("GEOID10" = "blkid")) %>%
  mutate_at(vars(nhblack:nbmin), ~ifelse(pop == 0, 0, ((./pop)*100)))

# which were annexed 
annexed <- blk17 %>%
  filter(annexed == 1)

nhb_17 <- ggplot() +  
  geom_sf(data = blk17, size = 0.1, aes(fill = nhblack)) + 
  geom_sf(data = pl17, size = 0.1, color = "black", fill = alpha("ghostwhite", 0.7)) + 
  scale_fill_gradient(low="grey87", high="grey51", breaks =  breaks, limits = limits) +
  geom_sf(data = annexed, size = 0.75, aes(fill = nhblack, color = "Annexed Block"), show.legend = "line") + 
  scale_color_manual(values = "red", guide = guide_legend(override.aes = list(fill = "red", color = "red"))) +
  labs(color='Annexed',
       fill = "% Non-Hispanic Black",
       caption = "2017-2018") +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank()) +
  map_theme
nhb_17

leg <- as_ggplot(get_legend(nhb_17))
nhb_17 <- nhb_17 + 
  theme(legend.position = "none")

nhb_attempt <- ggarrange(
  plotlist = list(nhb_09, nhb_12, leg, nhb_15, nhb_17), ncol = 3, nrow = 2, widths = c(2, 2, 2))
#  common.legend = F)

nhb_attempt

ggsave(paste0(savedir, "Springfield_annex_annual.pdf"),
       plot = nhb_attempt)



# 1379948 ---- 
map_theme <- theme(
  plot.title = element_text(
    hjust = .5,
    vjust = 0.2,
    size = 8
  ),
  plot.margin = grid::unit(c(0, 0, 0, 0), "mm"),
  axis.title.x = element_blank(),
  axis.title.y = element_blank(),
  axis.text.x = element_blank(),
  axis.text.y = element_blank(),
  axis.ticks.x = element_blank(),
  axis.ticks.y = element_blank()
)

pl0910 <- read_csv("analyticalfiles/annexedblocks0910dem.csv") %>%
  filter(plid=="1379948")

plids_viz <- unique(pl0910$plid)

# t1 place 
pl09 <- st_read("SHP_pl/2009/tl_2009_13_place.shp") %>%
  filter(PLCIDFP %in% pl0910$plid) %>%
  st_transform(., 3488)

buff09 <- read_csv("2009buffers.csv") %>%
  filter(!duplicated(blkid))

buff09 %<>%
  filter(bufferplace %in% pl0910$plid & !is.na(blkid)) 

blk09 <- st_read("SHP_blk_0010/2009/tl_2009_13_tabblock.shp") %>% 
  #left_join(cw, by = c("BLKIDFP00" = "GEOID00")) %>%
  #mutate(blkid = GEOID10) %>%
  #filter(!is.na(blkid)) %>%
  rename(blkid = BLKIDFP) %>%
  filter(blkid %in% buff09$blkid) %>%
  st_transform(., 3488) %>%
  left_join(pl0910, by = c("blkid")) %>%
  mutate_at(vars(nhblack, nbmin), ~ifelse(pop == 0, 0, ((./pop)*100)))

# get which blocks were annexed 
annexed <- blk09 %>% filter(annexed == 1)

# Create common legend breaks for 
breaks <- c(0, 20, 40, 60, 80, 100)
limits = c(0, 100)

nhb_09 <- ggplot() +  
  geom_sf(data = pl09, size = 0.1, color = "black", fill = alpha("ghostwhite", 0.7)) + 
  geom_sf(data = blk09, size = 0.1, aes(fill = nhblack)) + 
  scale_fill_gradient(low="grey87", high="grey51", breaks = breaks, limits = limits) +
  #geom_sf(data = annexed, size = 0.5, aes(fill = nhblack, color = "Annexed Block"), show.legend = "line") + 
  scale_color_manual(values = "red", guide = guide_legend(override.aes = list(fill = "red", color = "red"))) +  
  labs(fill = "% Non-Hispanic Black", 
       caption = "2009-2010") +
  theme(legend.position="none",
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank()) + 
  map_theme
nhb_09

# 1213
pl1213 <- read_csv("analyticalfiles/annexedblocks1213dem.csv") %>%
  filter(plid==plids_viz)

# t1 place 
pl12 <- st_read("SHP_pl/2012/tl_2012_13_place.shp") %>%
  filter(GEOID %in% pl1213$plid) %>%
  st_transform(., 3488)

buff12 <- read_csv("2012buffers.csv") %>%
  filter(!duplicated(blkid))

buff12 %<>%
  filter(bufferplace %in% pl1213$plid & !is.na(blkid)) 

blk12 <- st_read("SHP_blk_0010/2012/tl_2012_13_tabblock.shp") %>% 
  #left_join(cw, by = c("BLKIDFP00" = "GEOID00")) %>%
  #mutate(blkid = GEOID10) %>%
  #filter(!is.na(blkid)) %>%
  rename(blkid = GEOID) %>%
  filter(blkid %in% buff12$blkid) %>%
  st_transform(., 3488) %>%
  left_join(pl1213, by = c("blkid")) %>%
  mutate_at(vars(nhblack, nbmin), ~ifelse(pop == 0, 0, ((./pop)*100)))

# get which blocks were annexed 
annexed <- blk12 %>% filter(annexed == 1)

# Create common legend breaks for 
breaks <- c(0, 20, 40, 60, 80, 100)
limits = c(0, 100)

nhb_12 <- ggplot() +  
  geom_sf(data = pl12, size = 0.1, color = "black", fill = alpha("ghostwhite", 0.7)) + 
  geom_sf(data = blk12, size = 0.1, aes(fill = nhblack)) + 
  scale_fill_gradient(low="grey87", high="grey51", breaks = breaks, limits = limits) +
  #geom_sf(data = annexed, size = 0.5, aes(fill = nhblack, color = "Annexed Block"), show.legend = "line") + 
  scale_color_manual(values = "black", guide = guide_legend(override.aes = list(fill = "black", color = "black"))) +  
  labs(fill = "% Non-Hispanic Black", 
       caption = "2012-2013") +
  theme(legend.position="none",
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank()) + 
  map_theme
nhb_12

# 2015-2016
pl1516 <- read_csv("analyticalfiles/annexedblocks1516dem.csv") %>%
  filter(plid==plids_viz)

pl15 <- st_read("SHP_pl/2015/tl_2015_13_place.shp") %>%
  filter(GEOID %in% pl1516$plid) %>%
  st_transform(., 3488)

buff15 <- read_csv("2015buffers.csv") %>%
  filter(!duplicated(blkid))

buff15 %<>%
  filter(bufferplace %in% pl1516$plid) 

blk15 <- st_read("SHP_blk_0010/2015/tl_2015_13_tabblock10.shp") %>% 
  filter(GEOID10 %in% buff15$blkid) %>%
  st_transform(., 3488) %>%
  left_join(pl1516, by = c("GEOID10" = "blkid")) %>%
  mutate_at(vars(nhblack:nbmin), ~ifelse(pop == 0 | is.na(pop), 0, ((./pop)*100)))

annexed <- blk15 %>% 
  filter(annexed == 1)

nhb_15 <- ggplot() +  
  geom_sf(data = pl15, size = 0.1, color = "black", fill = alpha("ghostwhite", 0.7)) + 
  geom_sf(data = blk15, size = 0.1, aes(fill = nhblack)) + 
  scale_fill_gradient(low="grey87", high="grey51", breaks = breaks, limits = limits) +
  #geom_sf(data = annexed, size = 0.75, aes(fill = nhblack, color = "Annexed Block"), show.legend = "line") + 
  scale_color_manual(values = "red", guide = guide_legend(override.aes = list(fill = "red", color = "red"))) +
  labs(fill = "% Non-Hispanic Black", 
       caption = "2015-2016") +
  theme(legend.position="none",
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank()) +
  map_theme
nhb_15

# 2017-2018
pl1718 <- read_csv("analyticalfiles/annexedblocks1718dem.csv") %>%
  filter(plid==plids_viz)

# 2014 places 
pl17 <- st_read("SHP_pl/2017/tl_2017_13_place/tl_2017_13_place.shp") %>%
  filter(GEOID %in% plids_viz) %>%
  st_transform(., 3488)

blk17 <- st_read("SHP_blk_0010/2017/tl_2017_13_tabblock10/tl_2017_13_tabblock10.shp") %>%
  filter(GEOID10 %in% pl1718$blkid) %>%
  filter(!duplicated(GEOID10)) %>%
  st_transform(., 3488) %>%
  left_join(pl1718, by = c("GEOID10" = "blkid")) %>%
  mutate_at(vars(nhblack:nbmin), ~ifelse(pop == 0, 0, ((./pop)*100)))

# which were annexed 
annexed <- blk17 %>%
  filter(annexed == 1)

nhb_17 <- ggplot() +  
  geom_sf(data = blk17, size = 0.1, aes(fill = nhblack)) + 
  geom_sf(data = pl17, size = 0.1, color = "black", fill = alpha("ghostwhite", 0.7)) + 
  scale_fill_gradient(low="grey87", high="grey51", breaks =  breaks, limits = limits) +
  #geom_sf(data = annexed, size = 0.75, aes(fill = nhblack, color = "Annexed Block"), show.legend = "line") + 
  scale_color_manual(values = "red", guide = guide_legend(override.aes = list(fill = "red", color = "red"))) +
  labs(color='Annexed',
       fill = "% Non-Hispanic Black",
       caption = "2017-2018") +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank()) +
  map_theme
nhb_17

leg <- as_ggplot(get_legend(nhb_17))
nhb_17 <- nhb_17 + 
  theme(legend.position = "none")

nhb_attempt <- ggarrange(
  plotlist = list(nhb_09, nhb_12, leg, nhb_15, nhb_17), ncol = 3, nrow = 2, widths = c(2, 2, 2))
#  common.legend = F)

nhb_attempt

ggsave(paste0(savedir, "Waleska_annex_annual.pdf"),
       plot = nhb_attempt)

