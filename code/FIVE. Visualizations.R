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
  geom_sf(data = pl14, size = 0.1, fill = "grey") + 
  geom_sf(data = blk14, size = 0.5, aes(fill = nhblack)) + 
  scale_fill_gradient(low="grey77", high="grey31") +
  geom_sf(data = annexed, size = 0.25, aes(fill = nhblack, color = "Annexed Block"), show.legend = "line") + 
  scale_color_manual(values = "yellow",
                     guide = guide_legend(override.aes = list(fill = "white", color = "yellow"))) +
  labs(color = "", 
       fill = "% Non-Hispanic Black",
       title = "Annexations for Pearl City, MS 2014-2020")

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

# 2000 places 
pl00 <- st_read("SHP_pl/2000/GA_13/tl_2010_13_place00.shp") %>%
  filter(PLACEFP00 %in% plids_viz) %>%
  st_transform(., 3488)

blk00 <- st_read("SHP_blk_0010/2000/GA_13/tl_2010_13_tabblock00.shp") %>% 
  filter(BLKIDFP00 %in% pl0007$blkid) %>%
  filter(!duplicated(BLKIDFP00)) %>%
  st_transform(., 3488) %>%
  left_join(pl0007, by = c("BLKIDFP00" = "blkid")) %>%
  filter(pop00b > 1) %>%
  mutate_at(vars(nhblack00b:nbmin00b), ~((./pop00b)*100))

# get which blocks were annexed 
annexed <- blk00 %>% filter(annexed == 1)

# Create common legend breaks for 
breaks <- c(0, 20, 40, 60, 80, 100)
limits = c(0, 100)

nhb_00 <- ggplot() +  
  geom_sf(data = pl00, size = 0.1, fill = "grey") + 
  geom_sf(data = blk00, size = 0.5, aes(fill = nhblack00b)) + 
  scale_fill_gradient(low="grey77", high="grey31", breaks = breaks, limits = limits) +
  geom_sf(data = annexed, size = 0.25, aes(fill = nhblack, color = "Annexed Block"), show.legend = "line") + 
  scale_color_manual(values = "yellow",
                     guide = guide_legend(override.aes = list(fill = "white", color = "yellow"))) +  labs(fill = "% Non-Hispanic Black", 
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

# 2014 places 
pl07 <- st_read("SHP_pl/2007/fe_2007_13_place.shp") %>%
  filter(PLACEFP %in% plids_viz) %>%
  st_transform(., 3488)

blk07 <- st_read("SHP_blk_0010/2007/states/GA_13_allblocks.shp") %>% 
  filter(BLKIDFP %in% pl0713$blkid) %>%
  filter(!duplicated(BLKIDFP)) %>%
  st_transform(., 3488) %>%
  left_join(pl0713, by = c("BLKIDFP" = "blkid")) %>%
  filter(pop > 1) %>%
  mutate_at(vars(nhblack:nbmin), ~((./pop)*100))

annexed <- blk07 %>% filter(annexed == 1)

nhb_07 <- ggplot() +  
  geom_sf(data = pl07, size = 0.1, fill = "grey") + 
  geom_sf(data = blk07, size = 0.5, aes(fill = nhblack)) + 
  scale_fill_gradient(low="grey77", high="grey31", breaks = breaks, limits = limits) +
  geom_sf(data = annexed, size = 0.25, aes(fill = nhblack, color = "Annexed Block"), show.legend = "line") + 
  scale_color_manual(values = "yellow",
                     guide = guide_legend(override.aes = list(fill = "white", color = "yellow"))) +
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

# which were annexed
annexed <- blk14 %>% filter(annexed == 1)

nhb_14 <- ggplot() +  
  geom_sf(data = pl14, size = 0.1, fill = "grey") + 
  geom_sf(data = blk14, size = 0.5, aes(fill = nhblack)) + 
  scale_fill_gradient(low="grey77", high="grey31", breaks =  breaks, limits = limits) +
  geom_sf(data = annexed, size = 0.25, aes(fill = nhblack, color = "Annexed Block"), show.legend = "line") + 
  scale_color_manual(values = "yellow",
                     guide = guide_legend(override.aes = list(fill = "white", color = "yellow"))) +
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
                top = text_grob("Annexations for Baytown City, TX"),
                bottom = text_grob("\n*showing populated blocks only"))
# it will take a while to load
nhb_attempt

# I like to save in pdf because you don't lose pixels when zooming
setwd("~/Desktop")
# ggsave(filename = "GA_13_annexed_race.pdf",
#        plot = nhb,
#        dpi = 300)
# ggsave(filename = "analyticalfiles/Baytown TX_annexed_race.pdf",
#        plot = nhb,
#        dpi = 300)

# another interesting place to look is 0115640, which is Clio City, Alabama
#0115640


nhb <- ggplot() +  
  geom_sf(data = pl14, size = 0.1, fill = "grey") + 
  geom_sf(data = blk14, size = 0.5, aes(fill = nhblack)) + 
  geom_sf(data = annexed, size = 0.25, aes(fill = nhblack, color = "Annexed Block"), show.legend = "line") + 
scale_color_manual(values = "yellow",
                         guide = guide_legend(override.aes = list(fill = "white", color = "yellow"))) +
  labs(color = "", 
       fill = "% Non-Hispanic Black",
       title = "Annexations for Pearl City, MS 2014-2020") 
