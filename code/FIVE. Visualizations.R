# get environment ready 
setwd("~/Google Drive/My Drive/QE2")

library("stringr")
library("dplyr")
library("ggplot2")
library("ggpubr")
library("stargazer")
library("tidyverse")
library("tidycensus")
library("lme4")
library("readr")
library("data.table")
library("magrittr")
library("sf")

# goal: map places that annexed and their racial compositions
# there are three layers of information: 
# 1. the place's shapefile, for example, Palo Alto is the place 
# 2. the blocks that are within a 400-meter radius to the place 
# 3. among 2., which are the ones that actually got annexed?

# here's an example with the city that has place ID 2855760, which is Pearl City, Mississippi
# here's how you get information for # 2 and # 3--
# for 2014-2020, the file name is annexedblocks1420dem; for 2000-2007, it is annexedblocks0007dem, and similarly for 
# 2007-2013 it is annexedblocks0713dem. 
# we filter only to populated blocks, defined population > 1 since population is "bottom-coded" at 1
pl1420 <- read_csv("analyticalfiles/annexedblocks1420dem.csv") %>%
  filter(plid=="2855760" & pop > 1) 

plids_viz <- substr(unique(pl1420$plid), 3, 7) # here's how you get the 5-digit place ID, which is used in the place-level shapefiles instead of the 7-digit one (2-digit state ID + 5-digit place)

# 2014 places 
# each state has its own sub-folder for each year, so make you know the syntax for the file path 
# 28 is the two-digit state code for MS 
pl14 <- st_read("SHP_pl/2014/MS_28/tl_2014_28_place.shp") %>%
  filter(PLACEFP %in% plids_viz) %>%
  st_transform(., 3488)

# we need the *bloc*-level shapefile too; again, note the syntax for the file path 
# only need the geometries for the blocks in our sample (pl1420)
# why do we need to join with pl1420? Because we want to join the block shapes with corresponding block-level demographic data, which are stored in the variables of pl1420 (e.g., pop, nhblack, etc.)
blk14 <- st_read("SHP_blk_0010/2014/MS_28/tl_2014_28_tabblock10.shp") %>%
  filter(GEOID10 %in% pl1420$blkid) %>%
  filter(!duplicated(GEOID10)) %>%
  st_transform(., 3488) %>%
  left_join(pl1420, by = c("GEOID10" = "blkid"))

# you have to create % variables yourself, sorry. They are not included in the base file because for statistical analysis, I use % in a different way
blk14 %<>%
  mutate_at(vars(nhblack:nbmin), ~((./pop)*100))

# #3. get which blocks were annexed 
annexed <- blk14 %>% filter(annexed == 1)

# plot this; updated with labeling
nhb <- ggplot() +  
  geom_sf(data = pl14, size = 0.4, fill = "transparent") + 
  geom_sf(data = blk14, size = 0.25, aes(fill = nhblack)) + 
  scale_fill_gradient(low="grey77", high="grey31") +
  geom_sf(data = annexed, size = 0.5, aes(fill = nhblack, color = "Annexed Block"), show.legend = "line") +
  labs(color = "", 
       fill = "% Non-Hispanic Black",
       title = "Annexations for Pearl City, MS 2014-2020") + 
  scale_color_manual(values = "black",
                     guide = guide_legend(override.aes = list(fill = "transparent", color = "black"))) 

# SOLVED: as you can see, the color scale is wrong (should go from light to dark) 
# SOLVED:I also want to be able to add a legend showing what the red is (i.e., annexed) but it's not working
# and, I want everything in greyscale
nhb

# not run
# ggsave(filename = "analyticalfiles/Pearl MS_annex_race.png",
#        plot = nhb,
#        dpi = 300)

# next, I want to try showing this over time, 2000-2007, 2007-2013, and 2014-2020
# tx 4806128 ("Baytown") -- you can also find the name of the city in the place shapefile

# JC: trying plid 1365856, GA_13

# Common map_theme
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

# 0007
pl0007 <- read_csv("analyticalfiles/annexedblocks0007dem.csv") %>%
  filter(plid=="1365856")

plids_viz <- substr(unique(pl0007$plid), 3, 7)

# t1 place 
pl00 <- st_read("SHP_pl/2000/GA_13/tl_2010_13_place00.shp") %>%
  filter(PLCIDFP00 %in% pl0007$plid) %>%
  st_transform(., 3488)

buff00 <- read_csv("2000buffers.csv") %>%
  filter(!duplicated(blkid))

cw <- read_csv("cw/2000-to-2010_unique.csv")

buff00 %<>%
  left_join(cw, by = c("blkid" = "GEOID00")) %>%
  mutate(blkid = GEOID10)

buff00 %<>%
  filter(bufferplace %in% pl0007$plid & !is.na(blkid)) 

blk00 <- st_read("SHP_blk_0010/2000/GA_13/tl_2010_13_tabblock00.shp") %>% 
  left_join(cw, by = c("BLKIDFP00" = "GEOID00")) %>%
  mutate(blkid = GEOID10) %>%
  filter(!is.na(blkid)) %>%
  filter(blkid %in% buff00$blkid) %>%
  st_transform(., 3488) %>%
  left_join(pl0007, by = c("blkid")) %>%
  filter(pop00b > 1) %>%
  mutate_at(vars(nhblack00b:nbmin00b), ~((./pop00b)*100))

# get which blocks were annexed 
annexed <- blk00 %>% filter(annexed == 1)

# Create common legend breaks for 
breaks <- c(0, 20, 40, 60, 80, 100)
limits = c(0, 100)

nhb_00 <- ggplot() +  
  geom_sf(data = pl00, size = 0.4, fill = "white") + 
  geom_sf(data = blk00, size = 0.25, aes(fill = nhblack00b)) + 
  scale_fill_gradient(low="grey77", high="grey31", breaks = breaks, limits = limits) +
#  geom_sf(data = annexed, size = 0.5, aes(fill = nhblack00b, color = "Annexed Block"), show.legend = "line") + 
#  scale_color_manual(values = "black", guide = guide_legend(override.aes = list(fill = "transparent", color = "black"))) +  
  labs(fill = "% Non-Hispanic Black", 
       caption = "2000-2007*") +
  theme(legend.position="none",
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank()) + 
  map_theme
nhb_00

# 2007-2013
pl0713 <- read_csv("analyticalfiles/annexedblocks0713dem.csv") %>%
  filter(plid=="1365856")

plids_viz <- substr(unique(pl0713$plid), 3, 7)

# 2007 places 
pl07 <- st_read("SHP_pl/2007/fe_2007_13_place.shp") %>%
filter(PLCIDFP %in% pl0007$plid) %>%
  st_transform(., 3488)

buff07 <- read_csv("blocks2007_buffers.csv") %>%
  filter(!duplicated(blkid))

cw <- read_csv("cw/2000-to-2010_unique.csv")

buff07 %<>%
  left_join(cw, by = c("blkid" = "GEOID00")) %>%
  mutate(blkid = GEOID10) 

buff07 %<>%
  filter(bufferplace %in% pl0713$plid & !is.na(blkid)) 

blk07 <- st_read("SHP_blk_0010/2007/states/GA_13_allblocks.shp") %>% 
  left_join(cw, by = c("BLKIDFP" = "GEOID00")) %>%
  mutate(blkid = GEOID10) %>%
  filter(!is.na(blkid)) %>%
  filter(blkid %in% buff07$blkid) %>%
  st_transform(., 3488) %>%
  left_join(pl0713, by = c("blkid")) %>%
  filter(pop > 1) %>%
  mutate_at(vars(nhblack:nbmin), ~((./pop)*100))

annexed <- blk07 %>% 
  filter(annexed == 1)

nhb_07 <- ggplot() +  
  geom_sf(data = pl07, size = 0.4, fill = "white") + 
  geom_sf(data = blk07, size = 0.25, aes(fill = nhblack)) + 
  scale_fill_gradient(low="grey77", high="grey31", breaks = breaks, limits = limits) +
#  geom_sf(data = annexed, size = 0.55, aes(fill = nhblack, color = "Annexed Block"), show.legend = "line") + 
#  scale_color_manual(values = "black", guide = guide_legend(override.aes = list(fill = "transparent", color = "black"))) +
  labs(fill = "% Non-Hispanic Black", 
       caption = "2007-2013*") +
  theme(legend.position="none",
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank()) +
  map_theme
nhb_07

# 2014-2020
pl1420 <- read_csv("analyticalfiles/annexedblocks1420dem.csv") %>%
  filter(plid=="1365856")

plids_viz <- substr(unique(pl1420$plid), 3, 7)

# 2014 places 
pl14 <- st_read("SHP_pl/2014/GA_13/tl_2014_13_place.shp") %>%
  filter(PLACEFP %in% plids_viz) %>%
  st_transform(., 3488)

blk14 <- st_read("SHP_blk_0010/2014/GA_13/tl_2014_13_tabblock10.shp") %>%
  filter(GEOID10 %in% pl1420$blkid) %>%
  filter(!duplicated(GEOID10)) %>%
  st_transform(., 3488) %>%
  left_join(pl1420, by = c("GEOID10" = "blkid")) %>%
  filter(pop > 1) %>%
  mutate_at(vars(nhblack:nbmin), ~((./pop)*100))

# which were annexed in 2020 
annexed <- blk14 %>%
  filter(annexed == 1)

nhb_14 <- ggplot() +  
  geom_sf(data = pl14, size = 0.4, fill = "white") + 
  geom_sf(data = blk14, size = 0.25, aes(fill = nhblack)) + 
  scale_fill_gradient(low="grey77", high="grey31", breaks =  breaks, limits = limits) +
#  geom_sf(data = annexed, size = 0.5, aes(fill = nhblack, color = "Annexed Block"), show.legend = "line") + 
#  scale_color_manual(values = "black", guide = guide_legend(override.aes = list(fill = "transparent", color = "black"))) +
  labs(color='Annexed',
       fill = "% Non-Hispanic Black",
       caption = "2014-2020*") +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank()) +
  map_theme
nhb_14

leg <- as_ggplot(get_legend(nhb_14))
nhb_14 <- nhb_14 + 
  theme(legend.position = "none")

# As you can see, a lot about this graph would be nice to to change 
# could the 3 plots be more centered?
# SOLVED: can we add a title?
# SOLVED: can I make sure the legend breaks are the same across the maps? 
# SOLVED: can we remove the lat-longs? 
nhb_attempt <- ggarrange(
  plotlist = list(nhb_00, nhb_07, nhb_14, leg), ncol = 4, nrow = 1, widths = c(2, 1.4, 2))
 #  common.legend = F)

nhb_attempt <- annotate_figure(nhb_attempt,
                top = text_grob("Annexations for Roberta City, GA (none)"),
                bottom = text_grob("*showing populated blocks only \n 2010 boundaries"))
# it will take a while to load
nhb_attempt

ggsave("analyticalfiles/Roberta_annex.pdf",
       plot = nhb_attempt)

# place 1304000 ####
# Common map_theme
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

# 0007
pl0007 <- read_csv("analyticalfiles/annexedblocks0007dem.csv") %>%
  filter(plid=="1304000")

plids_viz <- unique(pl0007$plid)

# t1 place 
pl00 <- st_read("SHP_pl/2000/GA_13/tl_2010_13_place00.shp") %>%
  filter(PLCIDFP00 %in% pl0007$plid) %>%
  st_transform(., 3488)

buff00 <- read_csv("2000buffers.csv") %>%
  filter(!duplicated(blkid))

#cw <- read_csv("cw/2000-to-2010_unique.csv")

# buff00 %<>%
#   left_join(cw, by = c("blkid" = "GEOID00")) %>%
#   mutate(blkid = GEOID10)

buff00 %<>%
  filter(bufferplace %in% pl0007$plid & !is.na(blkid)) 

blk00 <- st_read("SHP_blk_0010/2000/GA_13/tl_2010_13_tabblock00.shp") %>% 
  #left_join(cw, by = c("BLKIDFP00" = "GEOID00")) %>%
  #mutate(blkid = GEOID10) %>%
  #filter(!is.na(blkid)) %>%
  rename(blkid = BLKIDFP00) %>%
  filter(blkid %in% buff00$blkid) %>%
  st_transform(., 3488) %>%
  left_join(pl0007, by = c("blkid")) %>%
  mutate_at(vars(nhblack00b:nbmin00b), ~ifelse(pop00b == 0, 0, ((./pop00b)*100)))

# get which blocks were annexed 
annexed <- blk00 %>% filter(annexed == 1)

# Create common legend breaks for 
breaks <- c(0, 20, 40, 60, 80, 100)
limits = c(0, 100)

nhb_00 <- ggplot() +  
  geom_sf(data = pl00, size = 0.25, color = "black", fill = "white") + 
  geom_sf(data = blk00, size = 0.1, aes(fill = nhblack00b)) + 
  scale_fill_gradient(low="grey77", high="grey31", breaks = breaks, limits = limits) +
  #geom_sf(data = annexed, size = 0.5, aes(fill = nhblack00b, color = "Annexed Block"), show.legend = "line") + 
  scale_color_manual(values = "red", guide = guide_legend(override.aes = list(fill = "transparent", color = "red"))) +  
  labs(fill = "% Non-Hispanic Black", 
       caption = "2000-2007") +
  theme(legend.position="none",
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank()) + 
  map_theme
nhb_00

# 2007-2013
pl0713 <- read_csv("analyticalfiles/annexedblocks0713dem.csv") %>%
  filter(plid==plids_viz)

# 2007 places 
pl07 <- st_read("SHP_pl/2007/fe_2007_13_place.shp") %>%
  filter(PLCIDFP %in% pl0713$plid) %>%
  st_transform(., 3488)

buff07 <- read_csv("blocks2007_buffers.csv") %>%
  filter(!duplicated(blkid))

cw <- read_csv("cw/2000-to-2010_unique.csv")

buff07 %<>%
  left_join(cw, by = c("blkid" = "GEOID00")) %>%
  filter(bufferplace %in% pl0713$plid & !is.na(GEOID10)) 

blk07 <- st_read("SHP_blk_0010/2007/states/GA_13_allblocks.shp") %>% 
  left_join(cw, by = c("BLKIDFP" = "GEOID00")) %>%
  filter(!is.na(GEOID10) & GEOID10 %in% buff07$GEOID10) %>%
  st_transform(., 3488) %>%
  left_join(pl0713, by = c("GEOID10")) %>%
  mutate_at(vars(nhblack:nbmin), ~ifelse(pop == 0, 0, ((./pop)*100)))

annexed <- blk07 %>% 
  filter(annexed == 1)

nhb_07 <- ggplot() +  
  geom_sf(data = pl07, size = 0.25, color = "black", fill = "white") + 
  geom_sf(data = blk07, size = 0.1, aes(fill = nhblack)) + 
  scale_fill_gradient(low="grey77", high="grey31", breaks = breaks, limits = limits) +
  geom_sf(data = annexed, size = 0.55, aes(fill = nhblack, color = "Annexed Block"), show.legend = "line") + 
  scale_color_manual(values = "red", guide = guide_legend(override.aes = list(fill = "transparent", color = "red"))) +
  labs(fill = "% Non-Hispanic Black", 
       caption = "2007-2013") +
  theme(legend.position="none",
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank()) +
  map_theme
nhb_07

# 2014-2020
pl1420 <- read_csv("analyticalfiles/annexedblocks1420dem.csv") %>%
  filter(plid==plids_viz)

# 2014 places 
pl14 <- st_read("SHP_pl/2014/GA_13/tl_2014_13_place.shp") %>%
  filter(GEOID %in% plids_viz) %>%
  st_transform(., 3488)

blk14 <- st_read("SHP_blk_0010/2014/GA_13/tl_2014_13_tabblock10.shp") %>%
  filter(GEOID10 %in% pl1420$blkid) %>%
  filter(!duplicated(GEOID10)) %>%
  st_transform(., 3488) %>%
  left_join(pl1420, by = c("GEOID10" = "blkid")) %>%
  mutate_at(vars(nhblack:nbmin), ~ifelse(pop == 0, 0, ((./pop)*100)))

# which were annexed in 2020 
annexed <- blk14 %>%
  filter(annexed == 1)

nhb_14 <- ggplot() +  
  geom_sf(data = blk14, size = 0.1, aes(fill = nhblack)) + 
  geom_sf(data = pl14, size = 0.25, color = "black", fill = "white") + 
  scale_fill_gradient(low="grey77", high="grey31", breaks =  breaks, limits = limits) +
  geom_sf(data = annexed, size = 0.5, aes(fill = nhblack, color = "Annexed Block"), show.legend = "line") + 
  scale_color_manual(values = "red", guide = guide_legend(override.aes = list(fill = "transparent", color = "red"))) +
  labs(color='Annexed',
       fill = "% Non-Hispanic Black",
       caption = "2014-2020") +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank()) +
  map_theme
nhb_14

leg <- as_ggplot(get_legend(nhb_14))
nhb_14 <- nhb_14 + 
  theme(legend.position = "none")

pl20 <- st_read("SHP_pl/2020/tl_2020_13_place.shp") %>%
  filter(GEOID %in% plids_viz) %>%
  st_transform(., 3488)

nhb_20 <- ggplot() + 
  geom_sf(data = pl20, size = 0.25, color = "black", fill = "white") +
  labs(caption = "2020") +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank()) +
  map_theme
nhb_20

nhb_attempt <- ggarrange(
  plotlist = list(nhb_00, nhb_07, leg, nhb_14, nhb_20), ncol = 3, nrow = 2, widths = c(2, 1.4, 2))
#  common.legend = F)

nhb_attempt

ggsave("analyticalfiles/final/Atlanta_annex_0020.pdf",
       plot = nhb_attempt)

# 0477000 ####
# Common map_theme
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

# 0007
pl0007 <- read_csv("analyticalfiles/annexedblocks0007dem.csv") %>%
  filter(plid=="0477000")

plids_viz <- unique(pl0007$plid)

# t1 place 
pl00 <- st_read("SHP_pl/2000/AZ_04/tl_2010_04_place00.shp") %>%
  filter(PLCIDFP00 %in% pl0007$plid) %>%
  st_transform(., 3488)

buff00 <- read_csv("2000buffers.csv") %>%
  filter(!duplicated(blkid))

buff00 %<>%
  filter(bufferplace %in% pl0007$plid & !is.na(blkid)) 

blk00 <- st_read("SHP_blk_0010/2000/AZ_04/tl_2010_04_tabblock00.shp") %>% 
  #left_join(cw, by = c("BLKIDFP00" = "GEOID00")) %>%
  #mutate(blkid = GEOID10) %>%
  #filter(!is.na(blkid)) %>%
  rename(blkid = BLKIDFP00) %>%
  filter(blkid %in% buff00$blkid) %>%
  st_transform(., 3488) %>%
  left_join(pl0007, by = c("blkid")) %>%
  mutate_at(vars(nhblack00b:nbmin00b), ~ifelse(pop00b == 0, 0, ((./pop00b)*100)))

# get which blocks were annexed 
annexed <- blk00 %>% filter(annexed == 1)

# Create common legend breaks for 
breaks <- c(0, 20, 40, 60, 80, 100)
limits = c(0, 100)

nhb_00 <- ggplot() +  
  geom_sf(data = pl00, size = 0.25, color = "black", fill = "transparent") + 
  geom_sf(data = blk00, size = 0.1, aes(fill = nbmin00b)) + 
  scale_fill_gradient(low="grey77", high="grey31", breaks = breaks, limits = limits) +
  geom_sf(data = annexed, size = 0.5, aes(fill = nbmin00b, color = "Annexed Block"), show.legend = "line") + 
  scale_color_manual(values = "red", guide = guide_legend(override.aes = list(fill = "transparent", color = "red"))) +  
  labs(fill = "% Non-Black minority", 
       caption = "2000-2007") +
  theme(legend.position="none",
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank()) + 
  map_theme
nhb_00

# 2007-2013
pl0713 <- read_csv("analyticalfiles/annexedblocks0713dem.csv") %>%
  filter(plid==plids_viz)

# 2007 places 
pl07 <- st_read("SHP_pl/2007/fe_2007_04_place.shp") %>%
  filter(PLCIDFP %in% pl0713$plid) %>%
  st_transform(., 3488)

buff07 <- read_csv("blocks2007_buffers.csv") %>%
  filter(!duplicated(blkid))

cw <- read_csv("cw/2000-to-2010_unique.csv")

buff07 %<>%
  left_join(cw, by = c("blkid" = "GEOID00")) %>%
  filter(!is.na(GEOID10))

buff07 %<>%
  filter(bufferplace %in% pl0713$plid) 

blk07 <- st_read("SHP_blk_0010/2007/states/AZ_04_allblocks.shp") %>% 
  left_join(cw, by = c("BLKIDFP" = "GEOID00")) %>%
  filter(!is.na(GEOID10)) %>%
  filter(GEOID10 %in% buff07$GEOID10) %>%
  st_transform(., 3488) %>%
  left_join(pl0713, by = c("GEOID10")) %>%
  mutate_at(vars(nhblack:nbmin), ~ifelse(pop == 0, 0, ((./pop)*100)))

annexed <- blk07 %>% 
  filter(annexed == 1)

nhb_07 <- ggplot() +  
  geom_sf(data = pl07, size = 0.25, color = "black", fill = "transparent") + 
  geom_sf(data = blk07, size = 0.1, aes(fill = nbmin)) + 
  scale_fill_gradient(low="grey77", high="grey31", breaks = breaks, limits = limits) +
  geom_sf(data = annexed, size = 0.55, aes(fill = nbmin, color = "Annexed Block"), show.legend = "line") + 
  scale_color_manual(values = "red", guide = guide_legend(override.aes = list(fill = "transparent", color = "red"))) +
  labs(fill = "% Non-Black Minority", 
       caption = "2007-2013") +
  theme(legend.position="none",
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank()) +
  map_theme
nhb_07

# 2014-2020
pl1420 <- read_csv("analyticalfiles/annexedblocks1420dem.csv") %>%
  filter(plid==plids_viz)

# 2014 places 
pl14 <- st_read("SHP_pl/2014/AZ_04/tl_2014_04_place.shp") %>%
  filter(GEOID %in% plids_viz) %>%
  st_transform(., 3488)

blk14 <- st_read("SHP_blk_0010/2014/AZ_04/tl_2014_04_tabblock10.shp") %>%
  filter(GEOID10 %in% pl1420$blkid) %>%
  filter(!duplicated(GEOID10)) %>%
  st_transform(., 3488) %>%
  left_join(pl1420, by = c("GEOID10" = "blkid")) %>%
  mutate_at(vars(nhblack:nbmin), ~ifelse(pop == 0, 0, ((./pop)*100)))

# which were annexed in 2020 
annexed <- blk14 %>%
  filter(annexed == 1)

nhb_14 <- ggplot() +  
  geom_sf(data = blk14, size = 0.1, aes(fill = nbmin)) + 
  geom_sf(data = pl14, size = 0.25, color = "black", fill = "transparent") + 
  scale_fill_gradient(low="grey77", high="grey31", breaks =  breaks, limits = limits) +
  geom_sf(data = annexed, size = 0.5, aes(fill = nbmin, color = "Annexed Block"), show.legend = "line") + 
  scale_color_manual(values = "red", guide = guide_legend(override.aes = list(fill = "transparent", color = "red"))) +
  labs(color='Annexed',
       fill = "% Non-Black Minority",
       caption = "2014-2020") +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank()) +
  map_theme
nhb_14

leg <- as_ggplot(get_legend(nhb_14))
nhb_14 <- nhb_14 + 
  theme(legend.position = "none")

pl20 <- st_read("SHP_pl/2020/tl_2020_04_place.shp") %>%
  filter(GEOID %in% plids_viz) %>%
  st_transform(., 3488)

nhb_20 <- ggplot() + 
  geom_sf(data = pl20, size = 0.25, color = "black", fill = "transparent") +
  labs(caption = "2020") +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank()) +
  map_theme
nhb_20

nhb_attempt <- ggarrange(
  plotlist = list(nhb_00, nhb_07, leg, nhb_14, nhb_20), ncol = 3, nrow = 2, widths = c(2, 1.4, 2))
#  common.legend = F)

nhb_attempt

ggsave("analyticalfiles/final/Tucson_annex_0020_nbmin.pdf",
       plot = nhb_attempt)

# 4805000 ----
# Common map_theme
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

# 0007
pl0007 <- read_csv("analyticalfiles/annexedblocks0007dem.csv") %>%
  filter(plid=="4805000")

plids_viz <- unique(pl0007$plid)

# t1 place 
pl00 <- st_read("SHP_pl/2000/TX_48/tl_2010_48_place00.shp") %>%
  filter(PLCIDFP00 %in% pl0007$plid) %>%
  st_transform(., 3488)

buff00 <- read_csv("2000buffers.csv") %>%
  filter(!duplicated(blkid))

buff00 %<>%
  filter(bufferplace %in% pl0007$plid & !is.na(blkid)) 

blk00 <- st_read("SHP_blk_0010/2000/TX_48/tl_2010_48_tabblock00.shp") %>% 
  #left_join(cw, by = c("BLKIDFP00" = "GEOID00")) %>%
  #mutate(blkid = GEOID10) %>%
  #filter(!is.na(blkid)) %>%
  rename(blkid = BLKIDFP00) %>%
  filter(blkid %in% buff00$blkid) %>%
  st_transform(., 3488) %>%
  left_join(pl0007, by = c("blkid")) %>%
  mutate_at(vars(nhblack00b:nbmin00b), ~ifelse(pop00b == 0, 0, ((./pop00b)*100)))

# get which blocks were annexed 
annexed <- blk00 %>% filter(annexed == 1)

# Create common legend breaks for 
breaks <- c(0, 20, 40, 60, 80, 100)
limits = c(0, 100)

nhb_00 <- ggplot() +  
  geom_sf(data = pl00, size = 0.25, color = "black", fill = "transparent") + 
  geom_sf(data = blk00, size = 0.1, aes(fill = nbmin00b)) + 
  scale_fill_gradient(low="grey77", high="grey31", breaks = breaks, limits = limits) +
  geom_sf(data = annexed, size = 0.5, aes(fill = nbmin00b, color = "Annexed Block"), show.legend = "line") + 
  scale_color_manual(values = "red", guide = guide_legend(override.aes = list(fill = "transparent", color = "red"))) +  
  labs(fill = "% Non-Black minority", 
       caption = "2000-2007") +
  theme(legend.position="none",
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank()) + 
  map_theme
nhb_00

# 2007-2013
pl0713 <- read_csv("analyticalfiles/annexedblocks0713dem.csv") %>%
  filter(plid==plids_viz)

# 2007 places 
pl07 <- st_read("SHP_pl/2007/fe_2007_48_place.shp") %>%
  filter(PLCIDFP %in% pl0713$plid) %>%
  st_transform(., 3488)

buff07 <- read_csv("blocks2007_buffers.csv") %>%
  filter(!duplicated(blkid))

cw <- read_csv("cw/2000-to-2010_unique.csv")

buff07 %<>%
  left_join(cw, by = c("blkid" = "GEOID00")) %>%
  filter(!is.na(GEOID10))

buff07 %<>%
  filter(bufferplace %in% pl0713$plid) 

blk07 <- st_read("SHP_blk_0010/2007/states/TX_48_allblocks.shp") %>% 
  left_join(cw, by = c("BLKIDFP" = "GEOID00")) %>%
  filter(!is.na(GEOID10)) %>%
  filter(GEOID10 %in% buff07$GEOID10) %>%
  st_transform(., 3488) %>%
  left_join(pl0713, by = c("GEOID10")) %>%
  mutate_at(vars(nhblack:nbmin), ~ifelse(pop == 0, 0, ((./pop)*100)))

annexed <- blk07 %>% 
  filter(annexed == 1)

nhb_07 <- ggplot() +  
  geom_sf(data = pl07, size = 0.25, color = "black", fill = "transparent") + 
  geom_sf(data = blk07, size = 0.1, aes(fill = nbmin)) + 
  scale_fill_gradient(low="grey77", high="grey31", breaks = breaks, limits = limits) +
  geom_sf(data = annexed, size = 0.55, aes(fill = nbmin, color = "Annexed Block"), show.legend = "line") + 
  scale_color_manual(values = "red", guide = guide_legend(override.aes = list(fill = "transparent", color = "red"))) +
  labs(fill = "% Non-Black Minority", 
       caption = "2007-2013") +
  theme(legend.position="none",
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank()) +
  map_theme
nhb_07

# 2014-2020
pl1420 <- read_csv("analyticalfiles/annexedblocks1420dem.csv") %>%
  filter(plid==plids_viz)

# 2014 places 
pl14 <- st_read("SHP_pl/2014/TX_48/tl_2014_48_place.shp") %>%
  filter(GEOID %in% plids_viz) %>%
  st_transform(., 3488)

blk14 <- st_read("SHP_blk_0010/2014/TX_48/tl_2014_48_tabblock10.shp") %>%
  filter(GEOID10 %in% pl1420$blkid) %>%
  filter(!duplicated(GEOID10)) %>%
  st_transform(., 3488) %>%
  left_join(pl1420, by = c("GEOID10" = "blkid")) %>%
  mutate_at(vars(nhblack:nbmin), ~ifelse(pop == 0, 0, ((./pop)*100)))

# which were annexed in 2020 
annexed <- blk14 %>%
  filter(annexed == 1)

nhb_14 <- ggplot() +  
  geom_sf(data = blk14, size = 0.1, aes(fill = nbmin)) + 
  geom_sf(data = pl14, size = 0.25, color = "black", fill = "transparent") + 
  scale_fill_gradient(low="grey77", high="grey31", breaks =  breaks, limits = limits) +
  geom_sf(data = annexed, size = 0.5, aes(fill = nbmin, color = "Annexed Block"), show.legend = "line") + 
  scale_color_manual(values = "red", guide = guide_legend(override.aes = list(fill = "transparent", color = "red"))) +
  labs(color='Annexed',
       fill = "% Non-Black Minority",
       caption = "2014-2020") +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank()) +
  map_theme
nhb_14

leg <- as_ggplot(get_legend(nhb_14))
nhb_14 <- nhb_14 + 
  theme(legend.position = "none")

pl20 <- st_read("SHP_pl/2020/tl_2020_48_place.shp") %>%
  filter(GEOID %in% plids_viz) %>%
  st_transform(., 3488)

nhb_20 <- ggplot() + 
  geom_sf(data = pl20, size = 0.25, color = "black", fill = "transparent") +
  labs(caption = "2020") +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank()) +
  map_theme
nhb_20

nhb_attempt <- ggarrange(
  plotlist = list(nhb_00, nhb_07, leg, nhb_14, nhb_20), ncol = 3, nrow = 2, widths = c(2, 1.4, 2))
#  common.legend = F)

nhb_attempt

ggsave("analyticalfiles/final/Austin_annex_0020_nbmin.pdf",
       plot = nhb_attempt)

# 4835000 ----
# Common map_theme
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

# 0007
pl0007 <- read_csv("analyticalfiles/annexedblocks0007dem.csv") %>%
  filter(plid=="4835000")

plids_viz <- unique(pl0007$plid)

# t1 place 
pl00 <- st_read("SHP_pl/2000/TX_48/tl_2010_48_place00.shp") %>%
  filter(PLCIDFP00 %in% pl0007$plid) %>%
  st_transform(., 3488)

buff00 <- read_csv("2000buffers.csv") %>%
  filter(!duplicated(blkid))

buff00 %<>%
  filter(bufferplace %in% pl0007$plid & !is.na(blkid)) 

blk00 <- st_read("SHP_blk_0010/2000/TX_48/tl_2010_48_tabblock00.shp") %>% 
  #left_join(cw, by = c("BLKIDFP00" = "GEOID00")) %>%
  #mutate(blkid = GEOID10) %>%
  #filter(!is.na(blkid)) %>%
  rename(blkid = BLKIDFP00) %>%
  filter(blkid %in% buff00$blkid) %>%
  st_transform(., 3488) %>%
  left_join(pl0007, by = c("blkid")) %>%
  mutate_at(vars(nhblack00b:nbmin00b), ~ifelse(pop00b == 0, 0, ((./pop00b)*100)))

# get which blocks were annexed 
annexed <- blk00 %>% filter(annexed == 1)

# Create common legend breaks for 
breaks <- c(0, 20, 40, 60, 80, 100)
limits = c(0, 100)

nhb_00 <- ggplot() +  
  geom_sf(data = pl00, size = 0.25, color = "black", fill = "white") + 
  geom_sf(data = blk00, size = 0.1, aes(fill = nbmin00b)) + 
  scale_fill_gradient(low="grey77", high="grey31", breaks = breaks, limits = limits) +
  geom_sf(data = annexed, size = 0.5, aes(fill = nbmin00b, color = "Annexed Block"), show.legend = "line") + 
  scale_color_manual(values = "red", guide = guide_legend(override.aes = list(fill = "transparent", color = "red"))) +  
  labs(fill = "% Non-Black minority", 
       caption = "2000-2007") +
  theme(legend.position="none",
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank()) + 
  map_theme
nhb_00

# 2007-2013
pl0713 <- read_csv("analyticalfiles/annexedblocks0713dem.csv") %>%
  filter(plid==plids_viz)

# 2007 places 
pl07 <- st_read("SHP_pl/2007/fe_2007_48_place.shp") %>%
  filter(PLCIDFP %in% pl0713$plid) %>%
  st_transform(., 3488)

buff07 <- read_csv("blocks2007_buffers.csv") %>%
  filter(!duplicated(blkid))

cw <- read_csv("cw/2000-to-2010_unique.csv")

buff07 %<>%
  left_join(cw, by = c("blkid" = "GEOID00")) %>%
  filter(!is.na(GEOID10))

buff07 %<>%
  filter(bufferplace %in% pl0713$plid) 

blk07 <- st_read("SHP_blk_0010/2007/states/TX_48_allblocks.shp") %>% 
  left_join(cw, by = c("BLKIDFP" = "GEOID00")) %>%
  filter(!is.na(GEOID10)) %>%
  filter(GEOID10 %in% buff07$GEOID10) %>%
  st_transform(., 3488) %>%
  left_join(pl0713, by = c("GEOID10")) %>%
  mutate_at(vars(nhblack:nbmin), ~ifelse(pop == 0, 0, ((./pop)*100)))

annexed <- blk07 %>% 
  filter(annexed == 1)

nhb_07 <- ggplot() +  
  geom_sf(data = pl07, size = 0.25, color = "black", fill = "white") + 
  geom_sf(data = blk07, size = 0.1, aes(fill = nbmin)) + 
  scale_fill_gradient(low="grey77", high="grey31", breaks = breaks, limits = limits) +
  geom_sf(data = annexed, size = 0.55, aes(fill = nbmin, color = "Annexed Block"), show.legend = "line") + 
  scale_color_manual(values = "red", guide = guide_legend(override.aes = list(fill = "transparent", color = "red"))) +
  labs(fill = "% Non-Black Minority", 
       caption = "2007-2013") +
  theme(legend.position="none",
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank()) +
  map_theme
nhb_07

# 2014-2020
pl1420 <- read_csv("analyticalfiles/annexedblocks1420dem.csv") %>%
  filter(plid==plids_viz)

# 2014 places 
pl14 <- st_read("SHP_pl/2014/TX_48/tl_2014_48_place.shp") %>%
  filter(GEOID %in% plids_viz) %>%
  st_transform(., 3488)

blk14 <- st_read("SHP_blk_0010/2014/TX_48/tl_2014_48_tabblock10.shp") %>%
  filter(GEOID10 %in% pl1420$blkid) %>%
  filter(!duplicated(GEOID10)) %>%
  st_transform(., 3488) %>%
  left_join(pl1420, by = c("GEOID10" = "blkid")) %>%
  mutate_at(vars(nhblack:nbmin), ~ifelse(pop == 0, 0, ((./pop)*100)))

# which were annexed in 2020 
annexed <- blk14 %>%
  filter(annexed == 1)

nhb_14 <- ggplot() +  
  geom_sf(data = blk14, size = 0.1, aes(fill = nbmin)) + 
  geom_sf(data = pl14, size = 0.25, color = "black", fill = "white") + 
  scale_fill_gradient(low="grey77", high="grey31", breaks =  breaks, limits = limits) +
  geom_sf(data = annexed, size = 0.5, aes(fill = nbmin, color = "Annexed Block"), show.legend = "line") + 
  scale_color_manual(values = "red", guide = guide_legend(override.aes = list(fill = "transparent", color = "red"))) +
  labs(color='Annexed',
       fill = "% Non-Black Minority",
       caption = "2014-2020") +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank()) +
  map_theme
nhb_14

leg <- as_ggplot(get_legend(nhb_14))
nhb_14 <- nhb_14 + 
  theme(legend.position = "none")

pl20 <- st_read("SHP_pl/2020/tl_2020_48_place.shp") %>%
  filter(GEOID %in% plids_viz) %>%
  st_transform(., 3488)

nhb_20 <- ggplot() + 
  geom_sf(data = pl20, size = 0.25, color = "black", fill = "white") +
  labs(caption = "2020") +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank()) +
  map_theme
nhb_20

nhb_attempt <- ggarrange(
  plotlist = list(nhb_14, nhb_20, leg), ncol = 3, widths = c(2, 1.4, 2))
#  common.legend = F)

nhb_attempt

ggsave("analyticalfiles/final/Houston_annex_0020_nbmin.png",
       plot = nhb_14,
       dpi = 300)

# 1342604 ----
# Common map_theme
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

# 0007
pl0007 <- read_csv("analyticalfiles/annexedblocks0007dem.csv") %>%
  filter(plid=="1342604")

plids_viz <- unique(pl0007$plid)

# t1 place 
pl00 <- st_read("SHP_pl/2000/GA_13/tl_2010_13_place00.shp") %>%
  filter(PLCIDFP00 %in% pl0007$plid) %>%
  st_transform(., 3488)

buff00 <- read_csv("2000buffers.csv") %>%
  filter(!duplicated(blkid))

#cw <- read_csv("cw/2000-to-2010_unique.csv")

# buff00 %<>%
#   left_join(cw, by = c("blkid" = "GEOID00")) %>%
#   mutate(blkid = GEOID10)

buff00 %<>%
  filter(bufferplace %in% pl0007$plid & !is.na(blkid)) 

blk00 <- st_read("SHP_blk_0010/2000/GA_13/tl_2010_13_tabblock00.shp") %>% 
  #left_join(cw, by = c("BLKIDFP00" = "GEOID00")) %>%
  #mutate(blkid = GEOID10) %>%
  #filter(!is.na(blkid)) %>%
  rename(blkid = BLKIDFP00) %>%
  filter(blkid %in% buff00$blkid) %>%
  st_transform(., 3488) %>%
  left_join(pl0007, by = c("blkid")) %>%
  mutate_at(vars(nhblack00b:nbmin00b), ~ifelse(pop00b == 0, 0, ((./pop00b)*100)))

# get which blocks were annexed 
annexed <- blk00 %>% filter(annexed == 1)

# Create common legend breaks for 
breaks <- c(0, 20, 40, 60, 80, 100)
limits = c(0, 100)

nhb_00 <- ggplot() +  
  geom_sf(data = pl00, size = 0.25, color = "black", fill = "white") + 
  geom_sf(data = blk00, size = 0.1, aes(fill = nhblack00b)) + 
  scale_fill_gradient(low="grey77", high="grey31", breaks = breaks, limits = limits) +
 # geom_sf(data = annexed, size = 0.5, aes(fill = nhblack00b, color = "Annexed Block"), show.legend = "line") + 
  scale_color_manual(values = "red", guide = guide_legend(override.aes = list(fill = "transparent", color = "red"))) +  
  labs(fill = "% Non-Hispanic Black", 
       caption = "2000-2007") +
  theme(legend.position="none",
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank()) + 
  map_theme
nhb_00

# 2007-2013
pl0713 <- read_csv("analyticalfiles/annexedblocks0713dem.csv") %>%
  filter(plid==plids_viz)

# 2007 places 
pl07 <- st_read("SHP_pl/2007/fe_2007_13_place.shp") %>%
  filter(PLCIDFP %in% pl0713$plid) %>%
  st_transform(., 3488)

buff07 <- read_csv("blocks2007_buffers.csv") %>%
  filter(!duplicated(blkid))

cw <- read_csv("cw/2000-to-2010_unique.csv")

buff07 %<>%
  left_join(cw, by = c("blkid" = "GEOID00")) %>%
  filter(bufferplace %in% pl0713$plid & !is.na(GEOID10)) 

blk07 <- st_read("SHP_blk_0010/2007/states/GA_13_allblocks.shp") %>% 
  left_join(cw, by = c("BLKIDFP" = "GEOID00")) %>%
  filter(!is.na(GEOID10) & GEOID10 %in% buff07$GEOID10) %>%
  st_transform(., 3488) %>%
  left_join(pl0713, by = c("GEOID10")) %>%
  mutate_at(vars(nhblack:nbmin), ~ifelse(pop == 0, 0, ((./pop)*100)))

annexed <- blk07 %>% 
  filter(annexed == 1)

nhb_07 <- ggplot() +  
  geom_sf(data = pl07, size = 0.25, color = "black", fill = "white") + 
  geom_sf(data = blk07, size = 0.1, aes(fill = nhblack)) + 
  scale_fill_gradient(low="grey77", high="grey31", breaks = breaks, limits = limits) +
  geom_sf(data = annexed, size = 0.55, aes(fill = nhblack, color = "Annexed Block"), show.legend = "line") + 
  scale_color_manual(values = "red", guide = guide_legend(override.aes = list(fill = "transparent", color = "red"))) +
  labs(fill = "% Non-Hispanic Black", 
       caption = "2007-2013") +
  theme(legend.position="none",
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank()) +
  map_theme
nhb_07

# 2014-2020
pl1420 <- read_csv("analyticalfiles/annexedblocks1420dem.csv") %>%
  filter(plid==plids_viz)

# 2014 places 
pl14 <- st_read("SHP_pl/2014/GA_13/tl_2014_13_place.shp") %>%
  filter(GEOID %in% plids_viz) %>%
  st_transform(., 3488)

blk14 <- st_read("SHP_blk_0010/2014/GA_13/tl_2014_13_tabblock10.shp") %>%
  filter(GEOID10 %in% pl1420$blkid) %>%
  filter(!duplicated(GEOID10)) %>%
  st_transform(., 3488) %>%
  left_join(pl1420, by = c("GEOID10" = "blkid")) %>%
  mutate_at(vars(nhblack:nbmin), ~ifelse(pop == 0, 0, ((./pop)*100)))

# which were annexed in 2020 
annexed <- blk14 %>%
  filter(annexed == 1)

nhb_14 <- ggplot() +  
  geom_sf(data = blk14, size = 0.1, aes(fill = nhblack)) + 
  geom_sf(data = pl14, size = 0.25, color = "black", fill = "white") + 
  scale_fill_gradient(low="grey77", high="grey31", breaks =  breaks, limits = limits) +
  geom_sf(data = annexed, size = 0.5, aes(fill = nhblack, color = "Annexed Block"), show.legend = "line") + 
  scale_color_manual(values = "red", guide = guide_legend(override.aes = list(fill = "transparent", color = "red"))) +
  labs(color='Annexed',
       fill = "% Non-Hispanic Black",
       caption = "2014-2020") +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank()) +
  map_theme
nhb_14

leg <- as_ggplot(get_legend(nhb_14))
nhb_14 <- nhb_14 + 
  theme(legend.position = "none")

pl20 <- st_read("SHP_pl/2020/tl_2020_13_place.shp") %>%
  filter(GEOID %in% plids_viz) %>%
  st_transform(., 3488)

nhb_20 <- ggplot() + 
  geom_sf(data = pl20, size = 0.25, color = "black", fill = "white") +
  labs(caption = "2020") +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank()) +
  map_theme
nhb_20

nhb_attempt <- ggarrange(
  plotlist = list(nhb_00, nhb_07, leg, nhb_14, nhb_20), ncol = 3, nrow = 2, widths = c(2, 1.4, 2))
#  common.legend = F)

nhb_attempt

ggsave("analyticalfiles/final/Jonesboro_annex_0020.pdf",
       plot = nhb_attempt)

# 1379948 ---- 
# Common map_theme
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

# 0007
pl0007 <- read_csv("analyticalfiles/annexedblocks0007dem.csv") %>%
  filter(plid=="1379948")

plids_viz <- unique(pl0007$plid)

# t1 place 
pl00 <- st_read("SHP_pl/2000/GA_13/tl_2010_13_place00.shp") %>%
  filter(PLCIDFP00 %in% pl0007$plid) %>%
  st_transform(., 3488)

buff00 <- read_csv("2000buffers.csv") %>%
  filter(!duplicated(blkid))

#cw <- read_csv("cw/2000-to-2010_unique.csv")

# buff00 %<>%
#   left_join(cw, by = c("blkid" = "GEOID00")) %>%
#   mutate(blkid = GEOID10)

buff00 %<>%
  filter(bufferplace %in% pl0007$plid & !is.na(blkid)) 

blk00 <- st_read("SHP_blk_0010/2000/GA_13/tl_2010_13_tabblock00.shp") %>% 
  #left_join(cw, by = c("BLKIDFP00" = "GEOID00")) %>%
  #mutate(blkid = GEOID10) %>%
  #filter(!is.na(blkid)) %>%
  rename(blkid = BLKIDFP00) %>%
  filter(blkid %in% buff00$blkid) %>%
  st_transform(., 3488) %>%
  left_join(pl0007, by = c("blkid")) %>%
  mutate_at(vars(nhblack00b:nbmin00b), ~ifelse(pop00b == 0, 0, ((./pop00b)*100)))

# get which blocks were annexed 
annexed <- blk00 %>% filter(annexed == 1)

# Create common legend breaks for 
breaks <- c(0, 20, 40, 60, 80, 100)
limits = c(0, 100)

nhb_00 <- ggplot() +  
  geom_sf(data = pl00, size = 0.25, color = "black", fill = "white") + 
  geom_sf(data = blk00, size = 0.1, aes(fill = nhblack00b)) + 
  scale_fill_gradient(low="grey77", high="grey31", breaks = breaks, limits = limits) +
  # geom_sf(data = annexed, size = 0.5, aes(fill = nhblack00b, color = "Annexed Block"), show.legend = "line") + 
  scale_color_manual(values = "red", guide = guide_legend(override.aes = list(fill = "transparent", color = "red"))) +  
  labs(fill = "% Non-Hispanic Black", 
       caption = "2000-2007") +
  theme(legend.position="none",
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank()) + 
  map_theme
nhb_00

# 2007-2013
pl0713 <- read_csv("analyticalfiles/annexedblocks0713dem.csv") %>%
  filter(plid==plids_viz)

# 2007 places 
pl07 <- st_read("SHP_pl/2007/fe_2007_13_place.shp") %>%
  filter(PLCIDFP %in% pl0713$plid) %>%
  st_transform(., 3488)

buff07 <- read_csv("blocks2007_buffers.csv") %>%
  filter(!duplicated(blkid))

cw <- read_csv("cw/2000-to-2010_unique.csv")

buff07 %<>%
  left_join(cw, by = c("blkid" = "GEOID00")) %>%
  filter(bufferplace %in% pl0713$plid & !is.na(GEOID10)) 

blk07 <- st_read("SHP_blk_0010/2007/states/GA_13_allblocks.shp") %>% 
  left_join(cw, by = c("BLKIDFP" = "GEOID00")) %>%
  filter(!is.na(GEOID10) & GEOID10 %in% buff07$GEOID10) %>%
  st_transform(., 3488) %>%
  left_join(pl0713, by = c("GEOID10")) %>%
  mutate_at(vars(nhblack:nbmin), ~ifelse(pop == 0, 0, ((./pop)*100)))

annexed <- blk07 %>% 
  filter(annexed == 1)

nhb_07 <- ggplot() +  
  geom_sf(data = pl07, size = 0.25, color = "black", fill = "white") + 
  geom_sf(data = blk07, size = 0.1, aes(fill = nhblack)) + 
  scale_fill_gradient(low="grey77", high="grey31", breaks = breaks, limits = limits) +
 # geom_sf(data = annexed, size = 0.55, aes(fill = nhblack, color = "Annexed Block"), show.legend = "line") + 
  scale_color_manual(values = "red", guide = guide_legend(override.aes = list(fill = "transparent", color = "red"))) +
  labs(fill = "% Non-Hispanic Black", 
       caption = "2007-2013") +
  theme(legend.position="none",
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank()) +
  map_theme
nhb_07

# 2014-2020
pl1420 <- read_csv("analyticalfiles/annexedblocks1420dem.csv") %>%
  filter(plid==plids_viz)

# 2014 places 
pl14 <- st_read("SHP_pl/2014/GA_13/tl_2014_13_place.shp") %>%
  filter(GEOID %in% plids_viz) %>%
  st_transform(., 3488)

blk14 <- st_read("SHP_blk_0010/2014/GA_13/tl_2014_13_tabblock10.shp") %>%
  filter(GEOID10 %in% pl1420$blkid) %>%
  filter(!duplicated(GEOID10)) %>%
  st_transform(., 3488) %>%
  left_join(pl1420, by = c("GEOID10" = "blkid")) %>%
  mutate_at(vars(nhblack:nbmin), ~ifelse(pop == 0, 0, ((./pop)*100)))

# which were annexed in 2020 
annexed <- blk14 %>%
  filter(annexed == 1)

nhb_14 <- ggplot() +  
  geom_sf(data = blk14, size = 0.1, aes(fill = nhblack)) + 
  geom_sf(data = pl14, size = 0.25, color = "black", fill = "white") + 
  scale_fill_gradient(low="grey77", high="grey31", breaks =  breaks, limits = limits) +
#  geom_sf(data = annexed, size = 0.5, aes(fill = nhblack, color = "Annexed Block"), show.legend = "line") + 
  scale_color_manual(values = "red", guide = guide_legend(override.aes = list(fill = "transparent", color = "red"))) +
  labs(color='Annexed',
       fill = "% Non-Hispanic Black",
       caption = "2014-2020") +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank()) +
  map_theme
nhb_14

leg <- as_ggplot(get_legend(nhb_14))
nhb_14 <- nhb_14 + 
  theme(legend.position = "none")

pl20 <- st_read("SHP_pl/2020/tl_2020_13_place.shp") %>%
  filter(GEOID %in% plids_viz) %>%
  st_transform(., 3488)

nhb_20 <- ggplot() + 
  geom_sf(data = pl20, size = 0.25, color = "black", fill = "white") +
  labs(caption = "2020") +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank()) +
  map_theme
nhb_20

nhb_attempt <- ggarrange(
  plotlist = list(nhb_00, nhb_07, leg, nhb_14, nhb_20), ncol = 3, nrow = 2, widths = c(2, 1.4, 2))
#  common.legend = F)

nhb_attempt

ggsave("analyticalfiles/final/Waleska_annex_0020.pdf",
       plot = nhb_attempt)

# 3767420 ----
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

# 0007
pl0007 <- read_csv("analyticalfiles/annexedblocks0007dem.csv") %>%
  filter(plid=="3767420")

plids_viz <- unique(pl0007$plid)

# t1 place 
pl00 <- st_read("SHP_pl/2000/NC_37/tl_2010_37_place00.shp") %>%
  filter(PLCIDFP00 %in% pl0007$plid) %>%
  st_transform(., 3488)

buff00 <- read_csv("2000buffers.csv") %>%
  filter(!duplicated(blkid))

#cw <- read_csv("cw/2000-to-2010_unique.csv")

# buff00 %<>%
#   left_join(cw, by = c("blkid" = "GEOID00")) %>%
#   mutate(blkid = GEOID10)

buff00 %<>%
  filter(bufferplace %in% pl0007$plid & !is.na(blkid)) 

blk00 <- st_read("SHP_blk_0010/2000/NC_37/tl_2010_37_tabblock00.shp") %>% 
  #left_join(cw, by = c("BLKIDFP00" = "GEOID00")) %>%
  #mutate(blkid = GEOID10) %>%
  #filter(!is.na(blkid)) %>%
  rename(blkid = BLKIDFP00) %>%
  filter(blkid %in% buff00$blkid) %>%
  st_transform(., 3488) %>%
  left_join(pl0007, by = c("blkid")) %>%
  mutate_at(vars(nhblack00b:nbmin00b), ~ifelse(pop00b == 0, 0, ((./pop00b)*100)))

# get which blocks were annexed 
annexed <- blk00 %>% filter(annexed == 1)

# Create common legend breaks for 
breaks <- c(0, 20, 40, 60, 80, 100)
limits = c(0, 100)

nhb_00 <- ggplot() +  
  geom_sf(data = pl00, size = 0.25, color = "black", fill = "white") + 
  geom_sf(data = blk00, size = 0.1, aes(fill = nhblack00b)) + 
  scale_fill_gradient(low="grey77", high="grey31", breaks = breaks, limits = limits) +
  geom_sf(data = annexed, size = 0.5, aes(fill = nhblack00b, color = "Annexed Block"), show.legend = "line") + 
  scale_color_manual(values = "red", guide = guide_legend(override.aes = list(fill = "transparent", color = "red"))) +  
  labs(fill = "% Non-Hispanic Black", 
       caption = "2000-2007") +
  theme(legend.position="none",
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank()) + 
  map_theme
nhb_00

# 2007-2013
pl0713 <- read_csv("analyticalfiles/annexedblocks0713dem.csv") %>%
  filter(plid==plids_viz)

# 2007 places 
pl07 <- st_read("SHP_pl/2007/fe_2007_37_place.shp") %>%
  filter(PLCIDFP %in% pl0713$plid) %>%
  st_transform(., 3488)

buff07 <- read_csv("blocks2007_buffers.csv") %>%
  filter(!duplicated(blkid))

cw <- read_csv("cw/2000-to-2010_unique.csv")

buff07 %<>%
  left_join(cw, by = c("blkid" = "GEOID00")) %>%
  filter(bufferplace %in% pl0713$plid & !is.na(GEOID10)) 

blk07 <- st_read("SHP_blk_0010/2007/states/NC_37_allblocks.shp") %>% 
  left_join(cw, by = c("BLKIDFP" = "GEOID00")) %>%
  filter(!is.na(GEOID10) & GEOID10 %in% buff07$GEOID10) %>%
  st_transform(., 3488) %>%
  left_join(pl0713, by = c("GEOID10")) %>%
  mutate_at(vars(nhblack:nbmin), ~ifelse(pop == 0, 0, ((./pop)*100)))

annexed <- blk07 %>% 
  filter(annexed == 1)

nhb_07 <- ggplot() +  
  geom_sf(data = pl07, size = 0.25, color = "black", fill = "transparent") + 
  geom_sf(data = blk07, size = 0.1, aes(fill = nhblack)) + 
  scale_fill_gradient(low="grey77", high="grey31", breaks = breaks, limits = limits) +
  geom_sf(data = annexed, size = 0.55, aes(fill = nhblack, color = "Annexed Block"), show.legend = "line") + 
  scale_color_manual(values = "red", guide = guide_legend(override.aes = list(fill = "transparent", color = "red"))) +
  labs(fill = "% Non-Hispanic Black", 
       caption = "2007-2013") +
  theme(legend.position="none",
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank()) +
  map_theme
nhb_07

# 2014-2020
pl1420 <- read_csv("analyticalfiles/annexedblocks1420dem.csv") %>%
  filter(plid==plids_viz)

# 2014 places 
pl14 <- st_read("SHP_pl/2014/NC_37/tl_2014_37_place.shp") %>%
  filter(GEOID %in% plids_viz) %>%
  st_transform(., 3488)

blk14 <- st_read("SHP_blk_0010/2014/NC_37/tl_2014_37_tabblock10.shp") %>%
  filter(GEOID10 %in% pl1420$blkid) %>%
  filter(!duplicated(GEOID10)) %>%
  st_transform(., 3488) %>%
  left_join(pl1420, by = c("GEOID10" = "blkid")) %>%
  mutate_at(vars(nhblack:nbmin), ~ifelse(pop == 0, 0, ((./pop)*100)))

# which were annexed in 2020 
annexed <- blk14 %>%
  filter(annexed == 1)

nhb_14 <- ggplot() +  
  geom_sf(data = blk14, size = 0.1, aes(fill = nhblack)) + 
  geom_sf(data = pl14, size = 0.25, color = "black", fill = "white") + 
  scale_fill_gradient(low="grey77", high="grey31", breaks =  breaks, limits = limits) +
  #  geom_sf(data = annexed, size = 0.5, aes(fill = nhblack, color = "Annexed Block"), show.legend = "line") + 
  scale_color_manual(values = "red", guide = guide_legend(override.aes = list(fill = "transparent", color = "red"))) +
  labs(color='Annexed',
       fill = "% Non-Hispanic Black",
       caption = "2014-2020") +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank()) +
  map_theme
nhb_14

leg <- as_ggplot(get_legend(nhb_14))
nhb_14 <- nhb_14 + 
  theme(legend.position = "none")

pl20 <- st_read("SHP_pl/2020/tl_2020_37_place.shp") %>%
  filter(GEOID %in% plids_viz) %>%
  st_transform(., 3488)

nhb_20 <- ggplot() + 
  geom_sf(data = pl20, size = 0.25, color = "black", fill = "white") +
  labs(caption = "2020") +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank()) +
  map_theme
nhb_20

nhb_attempt <- ggarrange(
  plotlist = list(nhb_00, nhb_07, leg, nhb_14, nhb_20), ncol = 3, nrow = 2, widths = c(2, 1.4, 2))
#  common.legend = F)

nhb_attempt

ggsave("analyticalfiles/final/Waleska_annex_0020.pdf",
       plot = nhb_attempt)

# 3728000 ----
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

# 0007
pl0007 <- read_csv("analyticalfiles/annexedblocks0007dem.csv") %>%
  filter(plid=="3728000")

plids_viz <- unique(pl0007$plid)

# t1 place 
pl00 <- st_read("SHP_pl/2000/NC_37/tl_2010_37_place00.shp") %>%
  filter(PLCIDFP00 %in% pl0007$plid) %>%
  st_transform(., 3488)

buff00 <- read_csv("2000buffers.csv") %>%
  filter(!duplicated(blkid))

#cw <- read_csv("cw/2000-to-2010_unique.csv")

# buff00 %<>%
#   left_join(cw, by = c("blkid" = "GEOID00")) %>%
#   mutate(blkid = GEOID10)

buff00 %<>%
  filter(bufferplace %in% pl0007$plid & !is.na(blkid)) 

blk00 <- st_read("SHP_blk_0010/2000/NC_37/tl_2010_37_tabblock00.shp") %>% 
  #left_join(cw, by = c("BLKIDFP00" = "GEOID00")) %>%
  #mutate(blkid = GEOID10) %>%
  #filter(!is.na(blkid)) %>%
  rename(blkid = BLKIDFP00) %>%
  filter(blkid %in% buff00$blkid) %>%
  st_transform(., 3488) %>%
  left_join(pl0007, by = c("blkid")) %>%
  mutate_at(vars(nhblack00b:nbmin00b), ~ifelse(pop00b == 0, 0, ((./pop00b)*100)))

# get which blocks were annexed 
annexed <- blk00 %>% filter(annexed == 1)

# Create common legend breaks for 
breaks <- c(0, 20, 40, 60, 80, 100)
limits = c(0, 100)

nhb_00 <- ggplot() +  
  geom_sf(data = pl00, size = 0.25, color = "black", fill = "white") + 
  geom_sf(data = blk00, size = 0.1, aes(fill = nhblack00b)) + 
  scale_fill_gradient(low="grey77", high="grey31", breaks = breaks, limits = limits) +
  geom_sf(data = annexed, size = 0.5, aes(fill = nhblack00b, color = "Annexed Block"), show.legend = "line") + 
  scale_color_manual(values = "red", guide = guide_legend(override.aes = list(fill = "transparent", color = "red"))) +  
  labs(fill = "% Non-Hispanic Black", 
       caption = "2000-2007") +
  theme(legend.position="none",
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank()) + 
  map_theme
nhb_00

# 2007-2013
pl0713 <- read_csv("analyticalfiles/annexedblocks0713dem.csv") %>%
  filter(plid==plids_viz)

# 2007 places 
pl07 <- st_read("SHP_pl/2007/fe_2007_37_place.shp") %>%
  filter(PLCIDFP %in% pl0713$plid) %>%
  st_transform(., 3488)

buff07 <- read_csv("blocks2007_buffers.csv") %>%
  filter(!duplicated(blkid))

cw <- read_csv("cw/2000-to-2010_unique.csv")

buff07 %<>%
  left_join(cw, by = c("blkid" = "GEOID00")) %>%
  filter(bufferplace %in% pl0713$plid & !is.na(GEOID10)) 

blk07 <- st_read("SHP_blk_0010/2007/states/NC_37_allblocks.shp") %>% 
  left_join(cw, by = c("BLKIDFP" = "GEOID00")) %>%
  filter(!is.na(GEOID10) & GEOID10 %in% buff07$GEOID10) %>%
  st_transform(., 3488) %>%
  left_join(pl0713, by = c("GEOID10")) %>%
  mutate_at(vars(nhblack:nbmin), ~ifelse(pop == 0, 0, ((./pop)*100)))

annexed <- blk07 %>% 
  filter(annexed == 1)

nhb_07 <- ggplot() +  
  geom_sf(data = pl07, size = 0.25, color = "black", fill = "white") + 
  geom_sf(data = blk07, size = 0.1, aes(fill = nhblack)) + 
  scale_fill_gradient(low="grey77", high="grey31", breaks = breaks, limits = limits) +
  geom_sf(data = annexed, size = 0.55, aes(fill = nhblack, color = "Annexed Block"), show.legend = "line") + 
  scale_color_manual(values = "red", guide = guide_legend(override.aes = list(fill = "transparent", color = "red"))) +
  labs(fill = "% Non-Hispanic Black", 
       caption = "2007-2013") +
  theme(legend.position="none",
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank()) +
  map_theme
nhb_07

# 2014-2020
pl1420 <- read_csv("analyticalfiles/annexedblocks1420dem.csv") %>%
  filter(plid==plids_viz)

# 2014 places 
pl14 <- st_read("SHP_pl/2014/NC_37/tl_2014_37_place.shp") %>%
  filter(GEOID %in% plids_viz) %>%
  st_transform(., 3488)

blk14 <- st_read("SHP_blk_0010/2014/NC_37/tl_2014_37_tabblock10.shp") %>%
  filter(GEOID10 %in% pl1420$blkid) %>%
  filter(!duplicated(GEOID10)) %>%
  st_transform(., 3488) %>%
  left_join(pl1420, by = c("GEOID10" = "blkid")) %>%
  mutate_at(vars(nhblack:nbmin), ~ifelse(pop == 0, 0, ((./pop)*100)))

# which were annexed in 2020 
annexed <- blk14 %>%
  filter(annexed == 1)

nhb_14 <- ggplot() +  
  geom_sf(data = blk14, size = 0.1, aes(fill = nhblack)) + 
  geom_sf(data = pl14, size = 0.25, color = "black", fill = "white") + 
  scale_fill_gradient(low="grey77", high="grey31", breaks =  breaks, limits = limits) +
   geom_sf(data = annexed, size = 0.5, aes(fill = nhblack, color = "Annexed Block"), show.legend = "line") + 
  scale_color_manual(values = "red", guide = guide_legend(override.aes = list(fill = "transparent", color = "red"))) +
  labs(color='Annexed',
       fill = "% Non-Hispanic Black",
       caption = "2014-2020") +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank()) +
  map_theme
nhb_14

leg <- as_ggplot(get_legend(nhb_14))
nhb_14 <- nhb_14 + 
  theme(legend.position = "none")

pl20 <- st_read("SHP_pl/2020/tl_2020_37_place.shp") %>%
  filter(GEOID %in% plids_viz) %>%
  st_transform(., 3488)

nhb_20 <- ggplot() + 
  geom_sf(data = pl20, size = 0.25, color = "black", fill = "white") +
  labs(caption = "2020") +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank()) +
  map_theme
nhb_20

nhb_attempt <- ggarrange(
  plotlist = list(nhb_00, nhb_07, leg, nhb_14, nhb_20), ncol = 3, nrow = 2, widths = c(2, 1.4, 2))
#  common.legend = F)

nhb_attempt

ggsave("analyticalfiles/final/Greensboro_annex_0020.pdf",
       plot = nhb_attempt)

# 4516000 ---- 
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

# 0007
pl0007 <- read_csv("analyticalfiles/annexedblocks0007dem.csv") %>%
  filter(plid=="4516000")

plids_viz <- unique(pl0007$plid)

# t1 place 
pl00 <- st_read("SHP_pl/2000/SC_45/tl_2010_45_place00.shp") %>%
  filter(PLCIDFP00 %in% pl0007$plid) %>%
  st_transform(., 3488)

buff00 <- read_csv("2000buffers.csv") %>%
  filter(!duplicated(blkid))

#cw <- read_csv("cw/2000-to-2010_unique.csv")

# buff00 %<>%
#   left_join(cw, by = c("blkid" = "GEOID00")) %>%
#   mutate(blkid = GEOID10)

buff00 %<>%
  filter(bufferplace %in% pl0007$plid & !is.na(blkid)) 

blk00 <- st_read("SHP_blk_0010/2000/SC_45/tl_2010_45_tabblock00.shp") %>% 
  #left_join(cw, by = c("BLKIDFP00" = "GEOID00")) %>%
  #mutate(blkid = GEOID10) %>%
  #filter(!is.na(blkid)) %>%
  rename(blkid = BLKIDFP00) %>%
  filter(blkid %in% buff00$blkid) %>%
  st_transform(., 3488) %>%
  left_join(pl0007, by = c("blkid")) %>%
  mutate_at(vars(nhblack00b:nbmin00b), ~ifelse(pop00b == 0, 0, ((./pop00b)*100)))

# get which blocks were annexed 
annexed <- blk00 %>% filter(annexed == 1)

# Create common legend breaks for 
breaks <- c(0, 20, 40, 60, 80, 100)
limits = c(0, 100)

nhb_00 <- ggplot() +  
  geom_sf(data = pl00, size = 0.25, color = "black", fill = "white") + 
  geom_sf(data = blk00, size = 0.1, aes(fill = nhblack00b)) + 
  scale_fill_gradient(low="grey77", high="grey31", breaks = breaks, limits = limits) +
  geom_sf(data = annexed, size = 0.5, aes(fill = nhblack00b, color = "Annexed Block"), show.legend = "line") + 
  scale_color_manual(values = "red", guide = guide_legend(override.aes = list(fill = "transparent", color = "red"))) +  
  labs(fill = "% Non-Hispanic Black", 
       caption = "2000-2007 \n 45.6% Black") +
  theme(legend.position="none",
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank()) + 
  map_theme
nhb_00

# 2007-2013
pl0713 <- read_csv("analyticalfiles/annexedblocks0713dem.csv") %>%
  filter(plid==plids_viz)

# 2007 places 
pl07 <- st_read("SHP_pl/2007/fe_2007_45_place.shp") %>%
  filter(PLCIDFP %in% pl0713$plid) %>%
  st_transform(., 3488)

buff07 <- read_csv("blocks2007_buffers.csv") %>%
  filter(!duplicated(blkid))

cw <- read_csv("cw/2000-to-2010_unique.csv")

buff07 %<>%
  left_join(cw, by = c("blkid" = "GEOID00")) %>%
  filter(bufferplace %in% pl0713$plid & !is.na(GEOID10)) 

blk07 <- st_read("SHP_blk_0010/2007/states/SC_45_allblocks.shp") %>% 
  left_join(cw, by = c("BLKIDFP" = "GEOID00")) %>%
  filter(!is.na(GEOID10) & GEOID10 %in% buff07$GEOID10) %>%
  st_transform(., 3488) %>%
  left_join(pl0713, by = c("GEOID10")) %>%
  mutate_at(vars(nhblack:nbmin), ~ifelse(pop == 0, 0, ((./pop)*100)))

annexed <- blk07 %>% 
  filter(annexed == 1)

nhb_07 <- ggplot() +  
  geom_sf(data = pl07, size = 0.25, color = "black", fill = "white") + 
  geom_sf(data = blk07, size = 0.1, aes(fill = nhblack)) + 
  scale_fill_gradient(low="grey77", high="grey31", breaks = breaks, limits = limits) +
  geom_sf(data = annexed, size = 0.55, aes(fill = nhblack, color = "Annexed Block"), show.legend = "line") + 
  scale_color_manual(values = "red", guide = guide_legend(override.aes = list(fill = "transparent", color = "red"))) +
  labs(fill = "% Non-Hispanic Black", 
       caption = "2007-2013 \n 41.5% Black") +
  theme(legend.position="none",
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank()) +
  map_theme
nhb_07

# 2014-2020
pl1420 <- read_csv("analyticalfiles/annexedblocks1420dem.csv") %>%
  filter(plid==plids_viz)

# 2014 places 
pl14 <- st_read("SHP_pl/2014/SC_45/tl_2014_45_place.shp") %>%
  filter(GEOID %in% plids_viz) %>%
  st_transform(., 3488)

blk14 <- st_read("SHP_blk_0010/2014/SC_45/tl_2014_45_tabblock10.shp") %>%
  filter(GEOID10 %in% pl1420$blkid) %>%
  filter(!duplicated(GEOID10)) %>%
  st_transform(., 3488) %>%
  left_join(pl1420, by = c("GEOID10" = "blkid")) %>%
  mutate_at(vars(nhblack:nbmin), ~ifelse(pop == 0, 0, ((./pop)*100)))

# which were annexed in 2020 
annexed <- blk14 %>%
  filter(annexed == 1)

nhb_14 <- ggplot() +  
  geom_sf(data = blk14, size = 0.1, aes(fill = nhblack)) + 
  geom_sf(data = pl14, size = 0.25, color = "black", fill = "white") + 
  scale_fill_gradient(low="grey77", high="grey31", breaks =  breaks, limits = limits) +
  geom_sf(data = annexed, size = 0.5, aes(fill = nhblack, color = "Annexed Block"), show.legend = "line") + 
  scale_color_manual(values = "red", guide = guide_legend(override.aes = list(fill = "transparent", color = "red"))) +
  labs(color='Annexed',
       fill = "% Non-Hispanic Black",
       caption = "2014-2020 \n 40.7% Black") +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank()) +
  map_theme
nhb_14

leg <- as_ggplot(get_legend(nhb_14))
nhb_14 <- nhb_14 + 
  theme(legend.position = "none")

pl20 <- st_read("SHP_pl/2020/tl_2020_45_place.shp") %>%
  filter(GEOID %in% plids_viz) %>%
  st_transform(., 3488)

nhb_20 <- ggplot() + 
  geom_sf(data = pl20, size = 0.25, color = "black", fill = "white") +
  labs(caption = "2020 \n 38.1% Black") +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank()) +
  map_theme
nhb_20

nhb_attempt <- ggarrange(
  plotlist = list(nhb_00, nhb_07, leg, nhb_14, nhb_20), ncol = 3, nrow = 2, widths = c(2, 1.4, 2))
#  common.legend = F)

nhb_attempt

ggsave("analyticalfiles/final/Columbia_annex_0020.pdf",
       plot = nhb_attempt)

# 4542190 ----
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

# 0007
pl0007 <- read_csv("analyticalfiles/annexedblocks0007dem.csv") %>%
  filter(plid=="4542190")

plids_viz <- unique(pl0007$plid)

# t1 place 
pl00 <- st_read("SHP_pl/2000/SC_45/tl_2010_45_place00.shp") %>%
  filter(PLCIDFP00 %in% pl0007$plid) %>%
  st_transform(., 3488)

buff00 <- read_csv("2000buffers.csv") %>%
  filter(!duplicated(blkid))

#cw <- read_csv("cw/2000-to-2010_unique.csv")

# buff00 %<>%
#   left_join(cw, by = c("blkid" = "GEOID00")) %>%
#   mutate(blkid = GEOID10)

buff00 %<>%
  filter(bufferplace %in% pl0007$plid & !is.na(blkid)) 

blk00 <- st_read("SHP_blk_0010/2000/SC_45/tl_2010_45_tabblock00.shp") %>% 
  #left_join(cw, by = c("BLKIDFP00" = "GEOID00")) %>%
  #mutate(blkid = GEOID10) %>%
  #filter(!is.na(blkid)) %>%
  rename(blkid = BLKIDFP00) %>%
  filter(blkid %in% buff00$blkid) %>%
  st_transform(., 3488) %>%
  left_join(pl0007, by = c("blkid")) %>%
  mutate_at(vars(nhblack00b:nbmin00b), ~ifelse(pop00b == 0, 0, ((./pop00b)*100)))

# get which blocks were annexed 
annexed <- blk00 %>% filter(annexed == 1)

# Create common legend breaks for 
breaks <- c(0, 20, 40, 60, 80, 100)
limits = c(0, 100)

nhb_00 <- ggplot() +  
  geom_sf(data = pl00, size = 0.25, color = "black", fill = "white") + 
  geom_sf(data = blk00, size = 0.1, aes(fill = nhblack00b)) + 
  scale_fill_gradient(low="grey77", high="grey31", breaks = breaks, limits = limits) +
  geom_sf(data = annexed, size = 0.5, aes(fill = nhblack00b, color = "Annexed Block"), show.legend = "line") + 
  scale_color_manual(values = "red", guide = guide_legend(override.aes = list(fill = "transparent", color = "red"))) +  
  labs(fill = "% Non-Hispanic Black", 
       caption = "2000-2007") +
  theme(legend.position="none",
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank()) + 
  map_theme
nhb_00

# 2007-2013
pl0713 <- read_csv("analyticalfiles/annexedblocks0713dem.csv") %>%
  filter(plid==plids_viz)

# 2007 places 
pl07 <- st_read("SHP_pl/2007/fe_2007_45_place.shp") %>%
  filter(PLCIDFP %in% pl0713$plid) %>%
  st_transform(., 3488)

buff07 <- read_csv("blocks2007_buffers.csv") %>%
  filter(!duplicated(blkid))

cw <- read_csv("cw/2000-to-2010_unique.csv")

buff07 %<>%
  left_join(cw, by = c("blkid" = "GEOID00")) %>%
  filter(bufferplace %in% pl0713$plid & !is.na(GEOID10)) 

blk07 <- st_read("SHP_blk_0010/2007/states/SC_45_allblocks.shp") %>% 
  left_join(cw, by = c("BLKIDFP" = "GEOID00")) %>%
  filter(!is.na(GEOID10) & GEOID10 %in% buff07$GEOID10) %>%
  st_transform(., 3488) %>%
  left_join(pl0713, by = c("GEOID10")) %>%
  mutate_at(vars(nhblack:nbmin), ~ifelse(pop == 0, 0, ((./pop)*100)))

annexed <- blk07 %>% 
  filter(annexed == 1)

nhb_07 <- ggplot() +  
  geom_sf(data = pl07, size = 0.25, color = "black", fill = "white") + 
  geom_sf(data = blk07, size = 0.1, aes(fill = nhblack)) + 
  scale_fill_gradient(low="grey77", high="grey31", breaks = breaks, limits = limits) +
 # geom_sf(data = annexed, size = 0.55, aes(fill = nhblack, color = "Annexed Block"), show.legend = "line") + 
  scale_color_manual(values = "red", guide = guide_legend(override.aes = list(fill = "transparent", color = "red"))) +
  labs(fill = "% Non-Hispanic Black", 
       caption = "2007-2013") +
  theme(legend.position="none",
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank()) +
  map_theme
nhb_07

# 2014-2020
pl1420 <- read_csv("analyticalfiles/annexedblocks1420dem.csv") %>%
  filter(plid==plids_viz)

# 2014 places 
pl14 <- st_read("SHP_pl/2014/SC_45/tl_2014_45_place.shp") %>%
  filter(GEOID %in% plids_viz) %>%
  st_transform(., 3488)

blk14 <- st_read("SHP_blk_0010/2014/SC_45/tl_2014_45_tabblock10.shp") %>%
  filter(GEOID10 %in% pl1420$blkid) %>%
  filter(!duplicated(GEOID10)) %>%
  st_transform(., 3488) %>%
  left_join(pl1420, by = c("GEOID10" = "blkid")) %>%
  mutate_at(vars(nhblack:nbmin), ~ifelse(pop == 0, 0, ((./pop)*100)))

# which were annexed in 2020 
annexed <- blk14 %>%
  filter(annexed == 1)

nhb_14 <- ggplot() +  
  geom_sf(data = blk14, size = 0.1, aes(fill = nhblack)) + 
  geom_sf(data = pl14, size = 0.25, color = "black", fill = "white") + 
  scale_fill_gradient(low="grey77", high="grey31", breaks =  breaks, limits = limits) +
 # geom_sf(data = annexed, size = 0.5, aes(fill = nhblack, color = "Annexed Block"), show.legend = "line") + 
  scale_color_manual(values = "red", guide = guide_legend(override.aes = list(fill = "transparent", color = "red"))) +
  labs(color='Annexed',
       fill = "% Non-Hispanic Black",
       caption = "2014-2020") +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank()) +
  map_theme
nhb_14

leg <- as_ggplot(get_legend(nhb_14))
nhb_14 <- nhb_14 + 
  theme(legend.position = "none")

pl20 <- st_read("SHP_pl/2020/tl_2020_45_place.shp") %>%
  filter(GEOID %in% plids_viz) %>%
  st_transform(., 3488)

nhb_20 <- ggplot() + 
  geom_sf(data = pl20, size = 0.25, color = "black", fill = "white") +
  labs(caption = "2020") +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank()) +
  map_theme
nhb_20

nhb_attempt <- ggarrange(
  plotlist = list(nhb_00, nhb_07, leg, nhb_14, nhb_20), ncol = 3, nrow = 2, widths = c(2, 1.4, 2))
#  common.legend = F)

nhb_attempt

ggsave("analyticalfiles/final/Lockhart_SC_annex_0020.pdf",
       plot = nhb_attempt)

# 0686972 ----
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

# 0007
pl0007 <- read_csv("analyticalfiles/annexedblocks0007dem.csv") %>%
  filter(plid=="0686972")

plids_viz <- unique(pl0007$plid)

# t1 place 
pl00 <- st_read("SHP_pl/2000/CA_06/tl_2010_06_place00.shp") %>%
  filter(PLCIDFP00 %in% pl0007$plid) %>%
  st_transform(., 3488)

buff00 <- read_csv("2000buffers.csv") %>%
  filter(!duplicated(blkid))

#cw <- read_csv("cw/2000-to-2010_unique.csv")

# buff00 %<>%
#   left_join(cw, by = c("blkid" = "GEOID00")) %>%
#   mutate(blkid = GEOID10)

buff00 %<>%
  filter(bufferplace %in% pl0007$plid & !is.na(blkid)) 

blk00 <- st_read("SHP_blk_0010/2000/CA_06/tl_2010_06_tabblock00.shp") %>% 
  #left_join(cw, by = c("BLKIDFP00" = "GEOID00")) %>%
  #mutate(blkid = GEOID10) %>%
  #filter(!is.na(blkid)) %>%
  rename(blkid = BLKIDFP00) %>%
  filter(blkid %in% buff00$blkid) %>%
  st_transform(., 3488) %>%
  left_join(pl0007, by = c("blkid")) %>%
  mutate_at(vars(nhblack00b:nbmin00b), ~ifelse(pop00b == 0, 0, ((./pop00b)*100)))

# get which blocks were annexed 
annexed <- blk00 %>% filter(annexed == 1)

# Create common legend breaks for 
breaks <- c(0, 20, 40, 60, 80, 100)
limits = c(0, 100)

nhb_00 <- ggplot() +  
  geom_sf(data = pl00, size = 0.25, color = "black", fill = "white") + 
  geom_sf(data = blk00, size = 0.1, aes(fill = nhblack00b)) + 
  scale_fill_gradient(low="grey77", high="grey31", breaks = breaks, limits = limits) +
  geom_sf(data = annexed, size = 0.5, aes(fill = nhblack00b, color = "Annexed Block"), show.legend = "line") + 
  scale_color_manual(values = "red", guide = guide_legend(override.aes = list(fill = "transparent", color = "red"))) +  
  labs(fill = "% Non-Hispanic Black", 
       caption = "2000-2007 \n 45.6% Black") +
  theme(legend.position="none",
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank()) + 
  map_theme
nhb_00

# 2007-2013
pl0713 <- read_csv("analyticalfiles/annexedblocks0713dem.csv") %>%
  filter(plid==plids_viz)

# 2007 places 
pl07 <- st_read("SHP_pl/2007/fe_2007_06_place.shp") %>%
  filter(PLCIDFP %in% pl0713$plid) %>%
  st_transform(., 3488)

buff07 <- read_csv("blocks2007_buffers.csv") %>%
  filter(!duplicated(blkid))

cw <- read_csv("cw/2000-to-2010_unique.csv")

buff07 %<>%
  left_join(cw, by = c("blkid" = "GEOID00")) %>%
  filter(bufferplace %in% pl0713$plid & !is.na(GEOID10)) 

blk07 <- st_read("SHP_blk_0010/2007/states/CA_06_allblocks.shp") %>% 
  left_join(cw, by = c("BLKIDFP" = "GEOID00")) %>%
  filter(!is.na(GEOID10) & GEOID10 %in% buff07$GEOID10) %>%
  st_transform(., 3488) %>%
  left_join(pl0713, by = c("GEOID10")) %>%
  mutate_at(vars(nhblack:nbmin), ~ifelse(pop == 0, 0, ((./pop)*100)))

annexed <- blk07 %>% 
  filter(annexed == 1)

nhb_07 <- ggplot() +  
  geom_sf(data = pl07, size = 0.25, color = "black", fill = "white") + 
  geom_sf(data = blk07, size = 0.1, aes(fill = nhblack)) + 
  scale_fill_gradient(low="grey77", high="grey31", breaks = breaks, limits = limits) +
  geom_sf(data = annexed, size = 0.55, aes(fill = nhblack, color = "Annexed Block"), show.legend = "line") + 
  scale_color_manual(values = "red", guide = guide_legend(override.aes = list(fill = "transparent", color = "red"))) +
  labs(fill = "% Non-Hispanic Black", 
       caption = "2007-2013 \n 41.5% Black") +
  theme(legend.position="none",
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank()) +
  map_theme
nhb_07

# 2014-2020
pl1420 <- read_csv("analyticalfiles/annexedblocks1420dem.csv") %>%
  filter(plid==plids_viz)

# 2014 places 
pl14 <- st_read("SHP_pl/2014/CA_06/tl_2014_06_place.shp") %>%
  filter(GEOID %in% plids_viz) %>%
  st_transform(., 3488)

blk14 <- st_read("SHP_blk_0010/2014/CA_06/tl_2014_06_tabblock10.shp") %>%
  filter(GEOID10 %in% pl1420$blkid) %>%
  filter(!duplicated(GEOID10)) %>%
  st_transform(., 3488) %>%
  left_join(pl1420, by = c("GEOID10" = "blkid")) %>%
  mutate_at(vars(nhblack:nbmin), ~ifelse(pop == 0, 0, ((./pop)*100)))

# which were annexed in 2020 
annexed <- blk14 %>%
  filter(annexed == 1)

nhb_14 <- ggplot() +  
  geom_sf(data = blk14, size = 0.1, aes(fill = nhblack)) + 
  geom_sf(data = pl14, size = 0.25, color = "black", fill = "white") + 
  scale_fill_gradient(low="grey77", high="grey31", breaks =  breaks, limits = limits) +
  geom_sf(data = annexed, size = 0.5, aes(fill = nhblack, color = "Annexed Block"), show.legend = "line") + 
  scale_color_manual(values = "red", guide = guide_legend(override.aes = list(fill = "transparent", color = "red"))) +
  labs(color='Annexed',
       fill = "% Non-Hispanic Black",
       caption = "2014-2020 \n 40.7% Black") +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank()) +
  map_theme
nhb_14

leg <- as_ggplot(get_legend(nhb_14))
nhb_14 <- nhb_14 + 
  theme(legend.position = "none")

pl20 <- st_read("SHP_pl/2020/tl_2020_06_place.shp") %>%
  filter(GEOID %in% plids_viz) %>%
  st_transform(., 3488)

nhb_20 <- ggplot() + 
  geom_sf(data = pl20, size = 0.25, color = "black", fill = "white") +
  labs(caption = "2020 \n 38.1% Black") +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank()) +
  map_theme
nhb_20

nhb_attempt <- ggarrange(
  plotlist = list(nhb_00, nhb_07, leg, nhb_14, nhb_20), ncol = 3, nrow = 2, widths = c(2, 1.4, 2))
#  common.legend = F)

nhb_attempt

ggsave("analyticalfiles/final/Columbia_annex_0020.pdf",
       plot = nhb_attempt)

# 2205000 ----
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

# 0007
pl0007 <- read_csv("analyticalfiles/annexedblocks0007dem.csv") %>%
  filter(plid=="2205000")

plids_viz <- unique(pl0007$plid)

# t1 place 
pl00 <- st_read("SHP_pl/2000/LA_22/tl_2010_22_place00.shp") %>%
  filter(PLCIDFP00 %in% pl0007$plid) %>%
  st_transform(., 3488)

buff00 <- read_csv("2000buffers.csv") %>%
  filter(!duplicated(blkid))

#cw <- read_csv("cw/2000-to-2010_unique.csv")

# buff00 %<>%
#   left_join(cw, by = c("blkid" = "GEOID00")) %>%
#   mutate(blkid = GEOID10)

buff00 %<>%
  filter(bufferplace %in% pl0007$plid & !is.na(blkid)) 

blk00 <- st_read("SHP_blk_0010/2000/LA_22/tl_2010_22_tabblock00.shp") %>% 
  #left_join(cw, by = c("BLKIDFP00" = "GEOID00")) %>%
  #mutate(blkid = GEOID10) %>%
  #filter(!is.na(blkid)) %>%
  rename(blkid = BLKIDFP00) %>%
  filter(blkid %in% buff00$blkid) %>%
  st_transform(., 3488) %>%
  left_join(pl0007, by = c("blkid")) %>%
  mutate_at(vars(nhblack00b:nbmin00b), ~ifelse(pop00b == 0, 0, ((./pop00b)*100)))

# get which blocks were annexed 
annexed <- blk00 %>% filter(annexed == 1)

# Create common legend breaks for 
breaks <- c(0, 20, 40, 60, 80, 100)
limits = c(0, 100)

panel0020_did %>% filter(time == "2000 to 2007" & plid == "2205000") %>%
  select(pctnhblack_p0)

nhb_00 <- ggplot() +  
  geom_sf(data = pl00, size = 0.25, color = "black", fill = "white") + 
  geom_sf(data = blk00, size = 0.1, aes(fill = nhblack00b)) + 
  scale_fill_gradient(low="grey77", high="grey31", breaks = breaks, limits = limits) +
  # geom_sf(data = annexed, size = 0.5, aes(fill = nhblack00b, color = "Annexed Block"), show.legend = "line") + 
  scale_color_manual(values = "red", guide = guide_legend(override.aes = list(fill = "transparent", color = "red"))) +  
  labs(fill = "% Non-Hispanic Black", 
       caption = "2000-2007 \n 49.5% Black") +
  theme(legend.position="none",
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank()) + 
  map_theme
nhb_00

# 2007-2013
pl0713 <- read_csv("analyticalfiles/annexedblocks0713dem.csv") %>%
  filter(plid==plids_viz)

# 2007 places 
pl07 <- st_read("SHP_pl/2007/fe_2007_22_place.shp") %>%
  filter(PLCIDFP %in% pl0713$plid) %>%
  st_transform(., 3488)

buff07 <- read_csv("blocks2007_buffers.csv") %>%
  filter(!duplicated(blkid))

cw <- read_csv("cw/2000-to-2010_unique.csv")

buff07 %<>%
  left_join(cw, by = c("blkid" = "GEOID00")) %>%
  filter(bufferplace %in% pl0713$plid & !is.na(GEOID10)) 

blk07 <- st_read("SHP_blk_0010/2007/states/LA_22_allblocks.shp") %>% 
  left_join(cw, by = c("BLKIDFP" = "GEOID00")) %>%
  filter(!is.na(GEOID10) & GEOID10 %in% buff07$GEOID10) %>%
  st_transform(., 3488) %>%
  left_join(pl0713, by = c("GEOID10")) %>%
  mutate_at(vars(nhblack:nbmin), ~ifelse(pop == 0, 0, ((./pop)*100)))

annexed <- blk07 %>% 
  filter(annexed == 1)

panel0020_did %>% filter(time == "2007 to 2013" & plid == "2205000") %>%
  select(pctnhblack_p0)
nhb_07 <- ggplot() +  
  geom_sf(data = pl07, size = 0.25, color = "black", fill = "white") + 
  geom_sf(data = blk07, size = 0.1, aes(fill = nhblack)) + 
  scale_fill_gradient(low="grey77", high="grey31", breaks = breaks, limits = limits) +
  geom_sf(data = annexed, size = 0.55, aes(fill = nhblack, color = "Annexed Block"), show.legend = "line") + 
  scale_color_manual(values = "red", guide = guide_legend(override.aes = list(fill = "transparent", color = "red"))) +
  labs(fill = "% Non-Hispanic Black", 
       caption = "2007-2013 \n 53.5% Black") +
  theme(legend.position="none",
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank()) +
  map_theme
nhb_07

# 2014-2020
pl1420 <- read_csv("analyticalfiles/annexedblocks1420dem.csv") %>%
  filter(plid==plids_viz)

# 2014 places 
pl14 <- st_read("SHP_pl/2014/LA_22/tl_2014_22_place.shp") %>%
  filter(GEOID %in% plids_viz) %>%
  st_transform(., 3488)

blk14 <- st_read("SHP_blk_0010/2014/LA_22/tl_2014_22_tabblock10.shp") %>%
  filter(GEOID10 %in% pl1420$blkid) %>%
  filter(!duplicated(GEOID10)) %>%
  st_transform(., 3488) %>%
  left_join(pl1420, by = c("GEOID10" = "blkid")) %>%
  mutate_at(vars(nhblack:nbmin), ~ifelse(pop == 0, 0, ((./pop)*100)))

# which were annexed in 2020 
annexed <- blk14 %>%
  filter(annexed == 1)

panel0020_did %>% filter(time == "2014 to 2020" & plid == "2205000") %>%
  select(pctnhblack_p0)
nhb_14 <- ggplot() +  
  geom_sf(data = blk14, size = 0.1, aes(fill = nhblack)) + 
  geom_sf(data = pl14, size = 0.25, color = "black", fill = "white") + 
  scale_fill_gradient(low="grey77", high="grey31", breaks =  breaks, limits = limits) +
  geom_sf(data = annexed, size = 0.5, aes(fill = nhblack, color = "Annexed Block"), show.legend = "line") + 
  scale_color_manual(values = "red", guide = guide_legend(override.aes = list(fill = "transparent", color = "red"))) +
  labs(color='Annexed',
       fill = "% Non-Hispanic Black",
       caption = "2014-2020 \n 54.5% Black") +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank()) +
  map_theme
nhb_14

leg <- as_ggplot(get_legend(nhb_14))
nhb_14 <- nhb_14 + 
  theme(legend.position = "none")

pl20 <- st_read("SHP_pl/2020/tl_2020_22_place.shp") %>%
  filter(GEOID %in% plids_viz) %>%
  st_transform(., 3488)

panel0020_did %>% filter(time == "2000 to 2007" & plid == "2205000") %>%
  select(pctnhblack_p1)

nhb_20 <- ggplot() + 
  geom_sf(data = pl20, size = 0.25, color = "black", fill = "white") +
  labs(caption = "2020 \n 53.3% Black") +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank()) +
  map_theme
nhb_20

nhb_attempt <- ggarrange(
  plotlist = list(nhb_00, nhb_07, leg, nhb_14, nhb_20), ncol = 3, nrow = 2, widths = c(2, 1.4, 2))
#  common.legend = F)

nhb_attempt

ggsave("analyticalfiles/final/Baton_Rouge_0020.png",
       plot = nhb_attempt, 
       dpi = 300)
