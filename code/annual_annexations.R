# -------------------------------------------------------------------------
# Created by: Iris Zhang                     
# Date created: 07/10/2023             
# Last revised:            
# Project: MBC         
# Subproject: Analysis
# Re: Do annual annexations analysis for R&R       
# -------------------------------------------------------------------------

rm(list = ls())
# Script Description ------------------------------------------------------
# Inputs:
# 

# Outputs:
# 

# Updates log: 

# Setup -------------------------------------------------------------------
rm(list = ls())
# Packages: 

# Directories: 
setwd("~/Google Drive/My Drive/Stanford/QE2")
curdir <- paste0(dirname(rstudioapi::getSourceEditorContext()$path))
savedir <- paste0("../results/")
# homedir <- The smallest directory that contains all input/output folders.
# workdir <- The smallest directory that contains all necessary inputs.
# savedir <- The smallest directory that contains all output folders.
# setwd(paste0(homedir, workdir))

# Import data: 

# Parameters:

# Main Script -------------------------------------------------------------

# get environment ready 
library("stringr")
library("tidyverse")
library("dplyr")
library("stargazer")
library("fixest")
library("readr")
library("data.table")
library("magrittr")
library("openxlsx")
library("broom")
library("sjPlot")

# 2007-2008 ----
aa0708 <- read_csv("analyticalfiles/annexedblocks0708dem.csv") 
names(aa0708)

# transform this into place-level summaries
# characteristic of all annexable blocks
# diff between annexed and not annexed 
place_all <- aa0708 %>% 
  group_by(plid) %>%
  dplyr::summarize(pop_total = sum(pop, na.rm = T),
                   pct_annexed = mean(annexed, na.rm = T)
  ) %>%
  ungroup() 

place_by_annex <- aa0708 %>%
  filter(annexed==1) %>%
  group_by(plid, annexed) %>%
  dplyr::summarize(pop_total = sum(pop, na.rm = T)) %>%
  ungroup() %>%
  pivot_wider(
    id_cols = plid,
    names_from = annexed,
    values_from = pop_total
  ) %>%
  rename(pop_total_1 = `1`) %>%
  filter(pop_total_1 > 1)

sapply(place_all, function(x) sum(is.na(x)))  

pl_annex_var_0708 <- left_join(
  place_all, place_by_annex, 
  by = "plid"
) %>%
  mutate(annexing = ifelse(!is.na(pop_total_1), 1, 0))
table(pl_annex_var_0708$annexing)
sapply(place_all, function(x) sum(is.na(x)))  

# add vra indicator 
places_vra <- aa0708 %>%
  group_by(plid) %>%
  dplyr::summarize(vra = mean(vra, na.rm = T)) %>%
  ungroup() %>%
  mutate(vra = ifelse(vra > 0, 1, 0))

pl_annex_var_0708 %<>%
  left_join(places_vra, by = "plid") 

cdps07 <- read_csv("plids/pl2007.csv") %>% # want to know which places are CDPs--they do not annex
  select(Geo_NAME, plid) %>%
  mutate(cdp = ifelse(grepl("CDP|cdp", Geo_NAME), 1, 0)) %>%
  filter(cdp==1)

pl_annex_var_0708 %<>%
  filter(!(plid %in% cdps07$plid)) %>%
  mutate(post = 0,
         time = "2007 to 2008") 

table(pl_annex_var_0708$annexing)

pl_annex_var_0708 %<>%
  filter(pop_total > 0) %>%
  mutate(annexing = ifelse(annexing == 1 & (pop_total_1 < 1 | is.na(pop_total_1)), 0, annexing))
table(pl_annex_var_0708$annexing)

sapply(pl_annex_var_0708, function(x) sum(is.na(x)))  
pl_annex_var_0708 <- unique(pl_annex_var_0708$plid[pl_annex_var_0708$vra==1])
rm(aa0708, cdps07, place_all, place_by_annex)

# 2008-2009 ----
aa0809 <- read_csv("analyticalfiles/annexedblocks0809dem.csv") 
names(aa0809)

# transform this into place-level summaries
# characteristic of all annexable blocks
place_all <- aa0809 %>% 
  group_by(plid) %>%
  dplyr::summarize(pop_total = sum(pop, na.rm = T),
                   pct_annexed = mean(annexed, na.rm = T)
  ) %>%
  ungroup() 

place_by_annex <- aa0809 %>%
  filter(annexed==1) %>%
  group_by(plid, annexed) %>%
  dplyr::summarize(pop_total = sum(pop, na.rm = T)) %>%
  ungroup() %>%
  pivot_wider(
    id_cols = plid,
    names_from = annexed,
    values_from = pop_total
  ) %>%
  rename(pop_total_1 = `1`) %>%
  filter(pop_total_1 > 1)

sapply(place_all, function(x) sum(is.na(x)))  

pl_annex_var_0809 <- left_join(
  place_all, place_by_annex, 
  by = "plid"
) %>%
  mutate(annexing = ifelse(!is.na(pop_total_1), 1, 0))
table(pl_annex_var_0809$annexing)
sapply(pl_annex_var_0809, function(x) sum(is.na(x)))  

# add vra indicator 
places_vra <- aa0809 %>%
  group_by(plid) %>%
  dplyr::summarize(vra = mean(vra, na.rm = T)) %>%
  ungroup() %>%
  mutate(vra = ifelse(vra > 0, 1, 0))

pl_annex_var_0809 %<>%
  left_join(places_vra, by = "plid") 

cdps08 <- read_csv("plids/pl2008.csv") %>% # want to know which places are CDPs--they do not annex
  select(Geo_NAME, plid) %>%
  mutate(cdp = ifelse(grepl("CDP|cdp", Geo_NAME), 1, 0)) %>%
  filter(cdp==1)

pl_annex_var_0809 %<>%
  filter(!(plid %in% cdps08$plid)) %>%
  mutate(post = 0,
         time = "2008 to 2009") 

table(pl_annex_var_0809$annexing)

pl_annex_var_0708 %<>%
  filter(pop_total > 0) %>%
  mutate(annexing = ifelse(annexing == 1 & (pop_total_1 < 1 | is.na(pop_total_1)), 0, annexing))
table(pl_annex_var_0708$annexing)

sapply(pl_annex_var_0708, function(x) sum(is.na(x)))  
pl_annex_var_0708 <- unique(pl_annex_var_0708$plid[pl_annex_var_0708$vra==1])
rm(aa0708, cdps07, place_all, place_by_annex)

# 2009-2010 ----
aa0910 <- read_csv("analyticalfiles/annexedblocks0910dem.csv") 
names(aa0910)

pl0910 <- aa0910 %>% 
  group_by(plid) %>%
  mutate(pct_annexed = mean(annexed, na.rm = T),
         annexing = ifelse(pct_annexed > 0 , 1, 0),
         vra = mean(vra, na.rm = T),
         vra = ifelse(vra > 0, 1, 0)) %>%
  ungroup() %>%
  group_by(plid, annexing) %>%
  dplyr::summarize(pop_total = sum(pop, na.rm = T),
                   pct_annexed = mean(annexed, na.rm = T), 
                   vra = mean(vra)) %>%
  ungroup() %>%
  mutate(period = "2009-2010",
         state = substr(plid, 1, 2),
         annexing_place = annexing) %>%
  pivot_wider(names_from = annexing,
              values_from = c(pop_total, pct_annexed)) %>%
  mutate(annexing_place = ifelse((annexing_place == 1 & pop_total_1 < 1), 0, annexing_place)) 

table(pl0910$annexing_place, pl0910$vra)
places_vra_0910 <- unique(pl0910$plid[pl0910$vra==1])
rm(aa0910)

# 2010-2011 ----
aa1011 <- read_csv("analyticalfiles/annexedblocks1011dem.csv") 
names(aa1011)

pl1011 <- aa1011 %>% 
  group_by(plid) %>%
  mutate(pct_annexed = mean(annexed, na.rm = T),
         annexing = ifelse(pct_annexed > 0 , 1, 0),
         vra = mean(vra, na.rm = T),
         vra = ifelse(vra > 0, 1, 0)) %>%
  ungroup() %>%
  group_by(plid, annexing) %>%
  dplyr::summarize(pop_total = sum(pop, na.rm = T),
                   pct_annexed = mean(annexed, na.rm = T), 
                   vra = mean(vra)) %>%
  ungroup() %>%
  mutate(period = "2010-2011",
         state = substr(plid, 1, 2),
         annexing_place = annexing) %>%
  pivot_wider(names_from = annexing,
              values_from = c(pop_total, pct_annexed)) %>%
  mutate(annexing_place = ifelse((annexing_place == 1 & pop_total_1 < 1), 0, annexing_place)) 

table(pl1011$annexing_place, pl1011$vra)
places_vra_1011 <- unique(pl1011$plid[pl1011$vra==1])
rm(aa1011)

# 2011-2012 ----
aa1112 <- read_csv("analyticalfiles/annexedblocks1112dem.csv") 
names(aa1112)

pl1112 <- aa1112 %>% 
  group_by(plid) %>%
  mutate(pct_annexed = mean(annexed, na.rm = T),
         annexing = ifelse(pct_annexed > 0 , 1, 0),
         vra = mean(vra, na.rm = T),
         vra = ifelse(vra > 0, 1, 0)) %>%
  ungroup() %>%
  group_by(plid, annexing) %>%
  dplyr::summarize(pop_total = sum(pop, na.rm = T),
                   pct_annexed = mean(annexed, na.rm = T), 
                   vra = mean(vra)) %>%
  ungroup() %>%
  mutate(period = "2011-2012",
         state = substr(plid, 1, 2),
         annexing_place = annexing) %>%
  pivot_wider(names_from = annexing,
              values_from = c(pop_total, pct_annexed)) %>%
  mutate(annexing_place = ifelse((annexing_place == 1 & pop_total_1 < 1), 0, annexing_place)) 

table(pl1112$annexing_place, pl1112$vra)
places_vra_1112 <- unique(pl1112$plid[pl1112$vra==1])
rm(aa1112)

# 2012-2013 
aa1213 <- read_csv("analyticalfiles/annexedblocks1213dem.csv") 
names(aa1213)

pl1213 <- aa1213 %>% 
  group_by(plid) %>%
  mutate(pct_annexed = mean(annexed, na.rm = T),
         annexing = ifelse(pct_annexed > 0 , 1, 0),
         vra = mean(vra, na.rm = T),
         vra = ifelse(vra > 0, 1, 0)) %>%
  ungroup() %>%
  group_by(plid, annexing) %>%
  dplyr::summarize(pop_total = sum(pop, na.rm = T),
                   pct_annexed = mean(annexed, na.rm = T), 
                   vra = mean(vra)) %>%
  ungroup() %>%
  mutate(period = "2012-2013",
         state = substr(plid, 1, 2),
         annexing_place = annexing) %>%
  pivot_wider(names_from = annexing,
              values_from = c(pop_total, pct_annexed)) %>%
  mutate(annexing_place = ifelse((annexing_place == 1 & pop_total_1 < 1), 0, annexing_place)) 

table(pl1213$annexing_place, pl1213$vra)
places_vra_1213 <- unique(pl1213$plid[pl1213$vra==1])
rm(aa1213)

# 2014-2015 ----
aa1415 <- read_csv("analyticalfiles/annexedblocks1415dem.csv") 
names(aa1415)

pl1415 <- aa1415 %>% 
  group_by(plid) %>%
  mutate(pct_annexed = mean(annexed, na.rm = T),
         annexing = ifelse(pct_annexed > 0 , 1, 0),
         vra = mean(vra, na.rm = T),
         vra = ifelse(vra > 0, 1, 0)) %>%
  ungroup() %>%
  group_by(plid, annexing) %>%
  dplyr::summarize(pop_total = sum(pop, na.rm = T),
                   pct_annexed = mean(annexed, na.rm = T), 
                   vra = mean(vra)) %>%
  ungroup() %>%
  mutate(period = "2014-2015",
         state = substr(plid, 1, 2),
         annexing_place = annexing) %>%
  pivot_wider(names_from = annexing,
              values_from = c(pop_total, pct_annexed)) %>%
  mutate(annexing_place = ifelse((annexing_place == 1 & pop_total_1 < 1), 0, annexing_place)) 

table(pl1415$annexing_place, pl1415$vra)
places_vra_1415 <- unique(pl1415$plid[pl1415$vra==1])
rm(aa1415)

# 2015-2016 ----
aa1516 <- read_csv("analyticalfiles/annexedblocks1516dem.csv") 
names(aa1516)

pl1516 <- aa1516 %>% 
  group_by(plid) %>%
  mutate(pct_annexed = mean(annexed, na.rm = T),
         annexing = ifelse(pct_annexed > 0 , 1, 0),
         vra = mean(vra, na.rm = T),
         vra = ifelse(vra > 0, 1, 0)) %>%
  ungroup() %>%
  group_by(plid, annexing) %>%
  dplyr::summarize(pop_total = sum(pop, na.rm = T),
                   pct_annexed = mean(annexed, na.rm = T), 
                   vra = mean(vra)) %>%
  ungroup() %>%
  mutate(period = "2015-2016",
         state = substr(plid, 1, 2),
         annexing_place = annexing) %>%
  pivot_wider(names_from = annexing,
              values_from = c(pop_total, pct_annexed)) %>%
  mutate(annexing_place = ifelse((annexing_place == 1 & pop_total_1 < 1), 0, annexing_place)) 

table(pl1516$annexing_place, pl1516$vra)
places_vra_1516 <- unique(pl1516$plid[pl1516$vra==1])
rm(aa1516)

# 2016-2017 ----
aa1617 <- read_csv("analyticalfiles/annexedblocks1617dem.csv") 
names(aa1617)

pl1617 <- aa1617 %>% 
  group_by(plid) %>%
  mutate(pct_annexed = mean(annexed, na.rm = T),
         annexing = ifelse(pct_annexed > 0 , 1, 0),
         vra = mean(vra, na.rm = T),
         vra = ifelse(vra > 0, 1, 0)) %>%
  ungroup() %>%
  group_by(plid, annexing) %>%
  dplyr::summarize(pop_total = sum(pop, na.rm = T),
                   pct_annexed = mean(annexed, na.rm = T), 
                   vra = mean(vra)) %>%
  ungroup() %>%
  mutate(period = "2016-2017",
         state = substr(plid, 1, 2),
         annexing_place = annexing) %>%
  pivot_wider(names_from = annexing,
              values_from = c(pop_total, pct_annexed)) %>%
  mutate(annexing_place = ifelse((annexing_place == 1 & pop_total_1 < 1), 0, annexing_place)) 

table(pl1617$annexing_place, pl1617$vra)
places_vra_1617 <- unique(pl1617$plid[pl1617$vra==1])
rm(aa1617)

# 2017-2018 ----
aa1718 <- read_csv("analyticalfiles/annexedblocks1718dem.csv") 
names(aa1718)

pl1718 <- aa1718 %>% 
  group_by(plid) %>%
  mutate(pct_annexed = mean(annexed, na.rm = T),
         annexing = ifelse(pct_annexed > 0 , 1, 0),
         vra = mean(vra, na.rm = T),
         vra = ifelse(vra > 0, 1, 0)) %>%
  ungroup() %>%
  group_by(plid, annexing) %>%
  dplyr::summarize(pop_total = sum(pop, na.rm = T),
                   pct_annexed = mean(annexed, na.rm = T), 
                   vra = mean(vra)) %>%
  ungroup() %>%
  mutate(period = "2017-2018",
         state = substr(plid, 1, 2),
         annexing_place = annexing) %>%
  pivot_wider(names_from = annexing,
              values_from = c(pop_total, pct_annexed)) %>%
  mutate(annexing_place = ifelse((annexing_place == 1 & pop_total_1 < 1), 0, annexing_place)) 

table(pl1718$annexing_place, pl1718$vra)
places_vra_1718 <- unique(pl1718$plid[pl1718$vra==1])
rm(aa1718)

# 2018-2019 ----
aa1819 <- read_csv("analyticalfiles/annexedblocks1819dem.csv") 
names(aa1819)

pl1819 <- aa1819 %>% 
  group_by(plid) %>%
  mutate(pct_annexed = mean(annexed, na.rm = T),
         annexing = ifelse(pct_annexed > 0 , 1, 0),
         vra = mean(vra, na.rm = T),
         vra = ifelse(vra > 0, 1, 0)) %>%
  ungroup() %>%
  group_by(plid, annexing) %>%
  dplyr::summarize(pop_total = sum(pop, na.rm = T),
                   pct_annexed = mean(annexed, na.rm = T), 
                   vra = mean(vra)) %>%
  ungroup() %>%
  mutate(period = "2018-2019",
         state = substr(plid, 1, 2),
         annexing_place = annexing) %>%
  pivot_wider(names_from = annexing,
              values_from = c(pop_total, pct_annexed)) %>%
  mutate(annexing_place = ifelse((annexing_place == 1 & pop_total_1 < 1), 0, annexing_place)) 

table(pl1819$annexing_place, pl1819$vra)
places_vra_1819 <- unique(pl1819$plid[pl1819$vra==1])
rm(aa1819)

# 2019-2020 ----
aa1920 <- read_csv("analyticalfiles/annexedblocks1920dem.csv") 
names(aa1920)

pl1920 <- aa1920 %>% 
  group_by(plid) %>%
  mutate(pct_annexed = mean(annexed, na.rm = T),
         annexing = ifelse(pct_annexed > 0 , 1, 0),
         vra = mean(vra, na.rm = T),
         vra = ifelse(vra > 0, 1, 0)) %>%
  ungroup() %>%
  group_by(plid, annexing) %>%
  dplyr::summarize(pop_total = sum(pop, na.rm = T),
                   pct_annexed = mean(annexed, na.rm = T), 
                   vra = mean(vra)) %>%
  ungroup() %>%
  mutate(period = "2019-2020",
         state = substr(plid, 1, 2),
         annexing_place = annexing) %>%
  pivot_wider(names_from = annexing,
              values_from = c(pop_total, pct_annexed)) %>%
  mutate(annexing_place = ifelse((annexing_place == 1 & pop_total_1 < 1), 0, annexing_place)) 

table(pl1920$annexing_place, pl1920$vra)
places_vra_1920 <- unique(pl1920$plid[pl1920$vra==1])
rm(aa1920)

# unique IDS ----
# unique plids common across time 
plids <- Reduce(intersect, list(unique(pl0708$plid), unique(pl0809$plid), unique(pl0910$plid), unique(pl1011$plid), unique(pl1112$plid), unique(pl1213$plid), unique(pl1415$plid), unique(pl1516$plid), unique(pl1617$plid), unique(pl1718$plid), unique(pl1819$plid), unique(pl1920$plid)))

# unique vra plids
vraplids <- unique(c(places_vra_0708, places_vra_0809, places_vra_0910, places_vra_1011, places_vra_1112, places_vra_1213, places_vra_1415, places_vra_1516, places_vra_1617, places_vra_1718, places_vra_1819, places_vra_1920))

# make panel data ----
panel_annual <- rbind(pl0708, pl0809, pl0910, pl1011, pl1112, pl1213, pl1415, pl1516, pl1617, pl1718, pl1819, pl1920)

table(panel_annual$vra, exclude = NULL)
length(unique(panel_annual$plid))

panel_annual %<>%
  mutate(vra = ifelse(plid %in% vraplids, 1, 0))

table(panel_annual$vra, exclude = NULL)

save.image(paste0(curdir, "/", savedir, "annual_annexations.RData"))

# make cross-tab 
pl1314 <- panel_annual %>%
  filter(period == "2012-2013") %>%
  mutate(period = "2013-2014", 
         annexing_place = 0) 

panel_annual <- rbind(panel_annual, pl1314)

panel_annual_xt <- panel_annual %>%
  group_by(period, vra) %>%
  summarize(annex = mean(annexing_place, na.rm = T)*100) %>%
  mutate(vra = as.character(vra),
         annex = ifelse(period == "2013-2014", NA, annex))

annual_xt <- ggplot(panel_annual_xt, aes(x = period, y = annex, group = vra)) + 
  geom_line(aes(linetype = vra)) + geom_point() + 
  theme(axis.text.x = element_text(angle = 50, hjust = 1, size = 10),
        axis.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12)) + 
  labs(linetype = "Covered by VRA") +
  xlab("Annual Period") +
  ylab("% of Places that Annexed")

annual_xt

ggsave(filename = paste0(curdir, "/", savedir, "annual_xt_viz.pdf"),
       plot = annual_xt,
       dpi = 300)

# version with "stable places" 
panel_annual_stable <- panel_annual %>%
  filter(plid %in% plids) 

panel_annual_xt <- panel_annual_stable %>%
  group_by(period, vra) %>%
  summarize(annex = mean(annexing_place, na.rm = T)*100) %>%
  mutate(vra = as.character(vra)) %>%
  mutate(annex = ifelse(period == "2013-2014", NA, annex))

annual_xt <- ggplot(panel_annual_xt, aes(x = period, y = annex, group = vra)) + 
  geom_line(aes(linetype = vra)) + geom_point() + 
  theme(axis.text.x = element_text(angle = 50, hjust = 1, size = 10),
        axis.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12)) + 
  labs(linetype = "Covered by VRA") +
       xlab("Annual Period") +
       ylab("% of Places that Annexed")

annual_xt

ggsave(filename = paste0(curdir, "/", savedir, "annual_xt_stable_viz.pdf"),
       plot = annual_xt,
       dpi = 300)

# can you check it so that when it aggregates it still tracks with the prior panel form? ----
agg <- panel_annual_stable %>%
  filter(!period %in% "2013-2014") %>% 
  mutate(post = ifelse(period %in% c("2007-2008", "2008-2009", "2009-2010", "2010-2011", "2011-2012", "2012-2013"), "2007-2013", "2014-2020")) %>%
  group_by(plid, post) %>%
  summarize(annex = mean(annexing_place),
            vra = mean(vra)) %>%
  ungroup() %>%
  mutate(annex = ifelse(annex > 0, 1, 0))

agg
table(agg$vra)
table(agg$annex)

agg %<>%
  group_by(vra, post) %>%
  summarize(annex = mean(annex)) %>%
  mutate(vra = as.character(vra))

agg_xt <- ggplot(agg, aes(x = post, y = annex, group = vra)) + 
  geom_line(aes(linetype = vra)) + geom_point() + 
  theme(axis.text.x = element_text(angle = 50, hjust = 1, size = 10),
        axis.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12)) + 
  labs(linetype = "Covered by VRA") +
  xlab("Annual Period") +
  ylab("% of Places that Annexed")

agg_xt

# this looks so weird 
