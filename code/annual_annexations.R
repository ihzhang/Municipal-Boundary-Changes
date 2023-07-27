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
savedir <- paste0(curdir, "/../results/")
# homedir <- The smallest directory that contains all input/output folders.
# workdir <- The smallest directory that contains all necessary inputs.
# savedir <- The smallest directory that contains all output folders.
# setwd(paste0(homedir, workdir))

# Import data: 
load(paste0(savedir, "annual_annexations.RData"))
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

# plids in the DBR panel ----
panel0020_did <- read_csv("analyticalfiles/panel_prestandard.csv") %>%
  filter(time %in% c("2007 to 2013", "2014 to 2020")) 

plids <- unique(panel0020_did$plid)
rm(panel0020_did)

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

# cdps08 <- read_csv("plids/pl2008.csv") %>% # want to know which places are CDPs--they do not annex
#   select(Geo_NAME, plid) %>%
#   mutate(cdp = ifelse(grepl("CDP|cdp", Geo_NAME), 1, 0)) %>%
#   filter(cdp==1)

pl_annex_var_0708 %<>%
  mutate(post = 0,
         time = "2007 to 2008") 

table(pl_annex_var_0708$annexing)

pl_annex_var_0708 %<>%
  #filter(pop_total > 0) %>%
  mutate(annexing = ifelse(annexing == 1 & (pop_total_1 < 1 | is.na(pop_total_1) | pop_total < 1), 0, annexing))
table(pl_annex_var_0708$annexing)

sapply(pl_annex_var_0708, function(x) sum(is.na(x)))  

table(pl_annex_var_0708$plid %in% plids)
p0708 <- unique(pl_annex_var_0708$plid)
pl_annex_vra_0708 <- unique(pl_annex_var_0708$plid[pl_annex_var_0708$vra==1])

rm(aa0708, place_all, place_by_annex, places_vra)

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

# cdps09 <- read_csv("plids/pl2009.csv") %>% # want to know which places are CDPs--they do not annex
#   select(Geo_NAME, plid) %>%
#   mutate(cdp = ifelse(grepl("CDP|cdp", Geo_NAME), 1, 0)) %>%
#   filter(cdp==1)

pl_annex_var_0809 %<>%
  #filter(!(plid %in% cdps09$plid)) %>%
  mutate(post = 0,
         time = "2008 to 2009") 

table(pl_annex_var_0809$annexing)

pl_annex_var_0809 %<>%
  #filter(pop_total > 0) %>%
  mutate(annexing = ifelse(annexing == 1 & (pop_total_1 < 1 | is.na(pop_total_1) | pop_total < 1), 0, annexing))
table(pl_annex_var_0809$annexing)
sapply(pl_annex_var_0809, function(x) sum(is.na(x)))  

table(pl_annex_var_0809$plid %in% plids)
table(pl_annex_var_0809$plid %in% p0708)

p0809 <- unique(pl_annex_var_0809$plid)
pl_annex_vra_0809 <- unique(pl_annex_var_0809$plid[pl_annex_var_0809$vra==1])
rm(aa0809, cdps09, place_all, place_by_annex, places_vra)

# 2009-2010 ----
aa0910 <- read_csv("analyticalfiles/annexedblocks0910dem.csv") 
names(aa0910)

# transform this into place-level summaries
# characteristic of all annexable blocks
place_all <- aa0910 %>% 
  group_by(plid) %>%
  dplyr::summarize(pop_total = sum(pop, na.rm = T),
                   pct_annexed = mean(annexed, na.rm = T)
  ) %>%
  ungroup() 

place_by_annex <- aa0910 %>%
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

pl_annex_var_0910 <- left_join(
  place_all, place_by_annex, 
  by = "plid"
) %>%
  mutate(annexing = ifelse(!is.na(pop_total_1), 1, 0))
table(pl_annex_var_0910$annexing)
sapply(pl_annex_var_0910, function(x) sum(is.na(x)))  

# add vra indicator 
places_vra <- aa0910 %>%
  group_by(plid) %>%
  dplyr::summarize(vra = mean(vra, na.rm = T)) %>%
  ungroup() %>%
  mutate(vra = ifelse(vra > 0, 1, 0))

pl_annex_var_0910 %<>%
  left_join(places_vra, by = "plid") 

# cdps10 <- read_csv("plids/pl2010.csv") %>% # want to know which places are CDPs--they do not annex
#   select(Geo_NAME, plid) %>%
#   mutate(cdp = ifelse(grepl("CDP|cdp", Geo_NAME), 1, 0)) %>%
#   filter(cdp==1)

pl_annex_var_0910 %<>%
  #filter(!(plid %in% cdps10$plid)) %>%
  mutate(post = 0,
         time = "2009 to 2010") 

table(pl_annex_var_0910$annexing)

pl_annex_var_0910 %<>%
  #filter(pop_total > 0) %>%
  mutate(annexing = ifelse(annexing == 1 & (pop_total_1 < 1 | is.na(pop_total_1) | pop_total < 1), 0, annexing))
table(pl_annex_var_0910$annexing)
sapply(pl_annex_var_0910, function(x) sum(is.na(x)))  

table(pl_annex_var_0910$plid %in% plids)
table(pl_annex_var_0910$plid %in% p0708)
table(pl_annex_var_0910$plid %in% p0809)

p0910 <- unique(pl_annex_var_0910$plid)
pl_annex_vra_0910 <- unique(pl_annex_var_0910$plid[pl_annex_var_0910$vra==1])
rm(aa0910, cdps10, place_all, place_by_annex, places_vra)

# 2010-2011 ----
aa1011 <- read_csv("analyticalfiles/annexedblocks1011dem.csv") 
names(aa1011)

# transform this into place-level summaries
# characteristic of all annexable blocks
place_all <- aa1011 %>% 
  group_by(plid) %>%
  dplyr::summarize(pop_total = sum(pop, na.rm = T),
                   pct_annexed = mean(annexed, na.rm = T)
  ) %>%
  ungroup() 

place_by_annex <- aa1011 %>%
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

pl_annex_var_1011 <- left_join(
  place_all, place_by_annex, 
  by = "plid"
) %>%
  mutate(annexing = ifelse(!is.na(pop_total_1), 1, 0))
table(pl_annex_var_1011$annexing)
sapply(pl_annex_var_1011, function(x) sum(is.na(x)))  

# add vra indicator 
places_vra <- aa1011 %>%
  group_by(plid) %>%
  dplyr::summarize(vra = mean(vra, na.rm = T)) %>%
  ungroup() %>%
  mutate(vra = ifelse(vra > 0, 1, 0))

pl_annex_var_1011 %<>%
  left_join(places_vra, by = "plid") 

# cdps11 <- read_csv("plids/pl2011.csv") %>% # want to know which places are CDPs--they do not annex
#   select(Geo_NAME, plid) %>%
#   mutate(cdp = ifelse(grepl("CDP|cdp", Geo_NAME), 1, 0)) %>%
#   filter(cdp==1)

pl_annex_var_1011 %<>%
  #filter(!(plid %in% cdps11$plid)) %>%
  mutate(post = 0,
         time = "2010 to 2011") 

table(pl_annex_var_1011$annexing)

pl_annex_var_1011 %<>%
  #filter(pop_total > 0) %>%
  mutate(annexing = ifelse(annexing == 1 & (pop_total_1 < 1 | is.na(pop_total_1) | pop_total < 1), 0, annexing))
table(pl_annex_var_1011$annexing)
sapply(pl_annex_var_1011, function(x) sum(is.na(x)))  

table(pl_annex_var_1011$plid %in% plids)
table(pl_annex_var_1011$plid %in% p0708)
table(pl_annex_var_1011$plid %in% p0809)
table(pl_annex_var_1011$plid %in% p0910)

p1011 <- unique(pl_annex_var_1011$plid)
pl_annex_vra_1011 <- unique(pl_annex_var_1011$plid[pl_annex_var_1011$vra==1])
rm(aa1011, cdps11, place_all, place_by_annex, places_vra)

# 2011-2012 ----
aa1112 <- read_csv("analyticalfiles/annexedblocks1112dem.csv") 
names(aa1112)

# transform this into place-level summaries
# characteristic of all annexable blocks
place_all <- aa1112 %>% 
  group_by(plid) %>%
  dplyr::summarize(pop_total = sum(pop, na.rm = T),
                   pct_annexed = mean(annexed, na.rm = T)
  ) %>%
  ungroup() 

place_by_annex <- aa1112 %>%
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

pl_annex_var_1112 <- left_join(
  place_all, place_by_annex, 
  by = "plid"
) %>%
  mutate(annexing = ifelse(!is.na(pop_total_1), 1, 0))
table(pl_annex_var_1112$annexing)
sapply(pl_annex_var_1112, function(x) sum(is.na(x)))  

# add vra indicator 
places_vra <- aa1112 %>%
  group_by(plid) %>%
  dplyr::summarize(vra = mean(vra, na.rm = T)) %>%
  ungroup() %>%
  mutate(vra = ifelse(vra > 0, 1, 0))

pl_annex_var_1112 %<>%
  left_join(places_vra, by = "plid") 

# cdps12 <- read_csv("plids/pl2011.csv") %>% # want to know which places are CDPs--they do not annex
#   select(Geo_NAME, plid) %>%
#   mutate(cdp = ifelse(grepl("CDP|cdp", Geo_NAME), 1, 0)) %>%
#   filter(cdp==1)

pl_annex_var_1112 %<>%
  #filter(!(plid %in% cdps12$plid)) %>%
  mutate(post = 0,
         time = "2011 to 2012") 

table(pl_annex_var_1112$annexing)

pl_annex_var_1112 %<>%
  #filter(pop_total > 0) %>%
  mutate(annexing = ifelse(annexing == 1 & (pop_total_1 < 1 | is.na(pop_total_1) | pop_total < 1), 0, annexing))
table(pl_annex_var_1112$annexing)
sapply(pl_annex_var_1112, function(x) sum(is.na(x)))  

table(pl_annex_var_1112$plid %in% plids)
table(pl_annex_var_1112$plid %in% p0708)
table(pl_annex_var_1112$plid %in% p0809)
table(pl_annex_var_1112$plid %in% p0910)
table(pl_annex_var_1112$plid %in% p1011)

p1112 <- unique(pl_annex_var_1112$plid)
pl_annex_vra_1112 <- unique(pl_annex_var_1112$plid[pl_annex_var_1112$vra==1])
rm(aa1112, cdps12, place_all, place_by_annex, places_vra)

# 2012-2013 ----
aa1213 <- read_csv("analyticalfiles/annexedblocks1213dem.csv") 
names(aa1213)

# transform this into place-level summaries
# characteristic of all annexable blocks
place_all <- aa1213 %>% 
  group_by(plid) %>%
  dplyr::summarize(pop_total = sum(pop, na.rm = T),
                   pct_annexed = mean(annexed, na.rm = T)
  ) %>%
  ungroup() 

place_by_annex <- aa1213 %>%
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

pl_annex_var_1213 <- left_join(
  place_all, place_by_annex, 
  by = "plid"
) %>%
  mutate(annexing = ifelse(!is.na(pop_total_1), 1, 0))
table(pl_annex_var_1213$annexing)
sapply(pl_annex_var_1213, function(x) sum(is.na(x)))  

# add vra indicator 
places_vra <- aa1213 %>%
  group_by(plid) %>%
  dplyr::summarize(vra = mean(vra, na.rm = T)) %>%
  ungroup() %>%
  mutate(vra = ifelse(vra > 0, 1, 0))

pl_annex_var_1213 %<>%
  left_join(places_vra, by = "plid") 

# cdps13 <- read_csv("plids/pl2013.csv") %>% # want to know which places are CDPs--they do not annex
#   select(Geo_NAME, plid) %>%
#   mutate(cdp = ifelse(grepl("CDP|cdp", Geo_NAME), 1, 0)) %>%
#   filter(cdp==1)

pl_annex_var_1213 %<>%
  #filter(!(plid %in% cdps13$plid)) %>%
  mutate(post = 0,
         time = "2012 to 2013") 

table(pl_annex_var_1213$annexing)

pl_annex_var_1213 %<>%
  #filter(pop_total > 0) %>%
  mutate(annexing = ifelse(annexing == 1 & (pop_total_1 < 1 | is.na(pop_total_1) | pop_total < 1), 0, annexing))
table(pl_annex_var_1213$annexing)
sapply(pl_annex_var_1213, function(x) sum(is.na(x)))  

table(pl_annex_var_1213$plid %in% plids)
table(pl_annex_var_1213$plid %in% p0708)
table(pl_annex_var_1213$plid %in% p0809)
table(pl_annex_var_1213$plid %in% p0910)
table(pl_annex_var_1213$plid %in% p1011)
table(pl_annex_var_1213$plid %in% p1112)

p1213 <- unique(pl_annex_var_1213$plid)
pl_annex_vra_1213 <- unique(pl_annex_var_1213$plid[pl_annex_var_1213$vra==1])
rm(aa1213, cdps13, place_all, place_by_annex, places_vra)

# 2014-2015 ----
aa1415 <- read_csv("analyticalfiles/annexedblocks1415dem.csv") 
names(aa1415)

# transform this into place-level summaries
# characteristic of all annexable blocks
place_all <- aa1415 %>% 
  group_by(plid) %>%
  dplyr::summarize(pop_total = sum(pop, na.rm = T),
                   pct_annexed = mean(annexed, na.rm = T)
  ) %>%
  ungroup() 

place_by_annex <- aa1415 %>%
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

pl_annex_var_1415 <- left_join(
  place_all, place_by_annex, 
  by = "plid"
) %>%
  mutate(annexing = ifelse(!is.na(pop_total_1), 1, 0))
table(pl_annex_var_1415$annexing)
sapply(pl_annex_var_1415, function(x) sum(is.na(x)))  

# add vra indicator 
places_vra <- aa1415 %>%
  group_by(plid) %>%
  dplyr::summarize(vra = mean(vra, na.rm = T)) %>%
  ungroup() %>%
  mutate(vra = ifelse(vra > 0, 1, 0))

pl_annex_var_1415 %<>%
  left_join(places_vra, by = "plid") 

# cdps15 <- read_csv("plids/pl2015.csv") %>% # want to know which places are CDPs--they do not annex
#   select(Geo_NAME, plid) %>%
#   mutate(cdp = ifelse(grepl("CDP|cdp", Geo_NAME), 1, 0)) %>%
#   filter(cdp==1)

pl_annex_var_1415 %<>%
  #filter(!(plid %in% cdps15$plid)) %>%
  mutate(post = 0,
         time = "2014 to 2015") 

table(pl_annex_var_1415$annexing)

pl_annex_var_1415 %<>%
  #filter(pop_total > 0) %>%
  mutate(annexing = ifelse(annexing == 1 & (pop_total_1 < 1 | is.na(pop_total_1) | pop_total < 1), 0, annexing))
table(pl_annex_var_1415$annexing)
sapply(pl_annex_var_1415, function(x) sum(is.na(x)))  

table(pl_annex_var_1415$plid %in% plids)
table(pl_annex_var_1415$plid %in% p0708)
table(pl_annex_var_1415$plid %in% p0809)
table(pl_annex_var_1415$plid %in% p0910)
table(pl_annex_var_1415$plid %in% p1011)
table(pl_annex_var_1415$plid %in% p1112)
table(pl_annex_var_1415$plid %in% p1213)

p1415 <- unique(pl_annex_var_1415$plid)
pl_annex_vra_1415 <- unique(pl_annex_var_1415$plid[pl_annex_var_1415$vra==1])
rm(aa1415, cdps15, place_all, place_by_annex, places_vra)

# 2015-2016 ----
aa1516 <- read_csv("analyticalfiles/annexedblocks1516dem.csv") 
names(aa1516)

# transform this into place-level summaries
# characteristic of all annexable blocks
place_all <- aa1516 %>% 
  group_by(plid) %>%
  dplyr::summarize(pop_total = sum(pop, na.rm = T),
                   pct_annexed = mean(annexed, na.rm = T)
  ) %>%
  ungroup() 

place_by_annex <- aa1516 %>%
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

pl_annex_var_1516 <- left_join(
  place_all, place_by_annex, 
  by = "plid"
) %>%
  mutate(annexing = ifelse(!is.na(pop_total_1), 1, 0))
table(pl_annex_var_1516$annexing)
sapply(pl_annex_var_1516, function(x) sum(is.na(x)))  

# add vra indicator 
places_vra <- aa1516 %>%
  group_by(plid) %>%
  dplyr::summarize(vra = mean(vra, na.rm = T)) %>%
  ungroup() %>%
  mutate(vra = ifelse(vra > 0, 1, 0))

pl_annex_var_1516 %<>%
  left_join(places_vra, by = "plid") 

# cdps16 <- read_csv("plids/pl2016.csv") %>% # want to know which places are CDPs--they do not annex
#   select(Geo_NAME, plid) %>%
#   mutate(cdp = ifelse(grepl("CDP|cdp", Geo_NAME), 1, 0)) %>%
#   filter(cdp==1)

pl_annex_var_1516 %<>%
  #filter(!(plid %in% cdps16$plid)) %>%
  mutate(post = 0,
         time = "2015 to 2016") 

table(pl_annex_var_1516$annexing)

pl_annex_var_1516 %<>%
  #filter(pop_total > 0) %>%
  mutate(annexing = ifelse(annexing == 1 & (pop_total_1 < 1 | is.na(pop_total_1) | pop_total < 1), 0, annexing))
table(pl_annex_var_1516$annexing)
sapply(pl_annex_var_1516, function(x) sum(is.na(x)))  

table(pl_annex_var_1516$plid %in% plids)
table(pl_annex_var_1516$plid %in% p0708)
table(pl_annex_var_1516$plid %in% p0809)
table(pl_annex_var_1516$plid %in% p0910)
table(pl_annex_var_1516$plid %in% p1011)
table(pl_annex_var_1516$plid %in% p1112)
table(pl_annex_var_1516$plid %in% p1213)
table(pl_annex_var_1516$plid %in% p1415)

p1516 <- unique(pl_annex_var_1516$plid)
pl_annex_vra_1516 <- unique(pl_annex_var_1516$plid[pl_annex_var_1516$vra==1])
rm(aa1516, cdps15, place_all, place_by_annex, places_vra)

# 2016-2017 ----
aa1617 <- read_csv("analyticalfiles/annexedblocks1617dem.csv") 
names(aa1617)

# transform this into place-level summaries
# characteristic of all annexable blocks
place_all <- aa1617 %>% 
  group_by(plid) %>%
  dplyr::summarize(pop_total = sum(pop, na.rm = T),
                   pct_annexed = mean(annexed, na.rm = T)
  ) %>%
  ungroup() 

place_by_annex <- aa1617 %>%
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

pl_annex_var_1617 <- left_join(
  place_all, place_by_annex, 
  by = "plid"
) %>%
  mutate(annexing = ifelse(!is.na(pop_total_1), 1, 0))
table(pl_annex_var_1617$annexing)
sapply(pl_annex_var_1617, function(x) sum(is.na(x)))  

# add vra indicator 
places_vra <- aa1617 %>%
  group_by(plid) %>%
  dplyr::summarize(vra = mean(vra, na.rm = T)) %>%
  ungroup() %>%
  mutate(vra = ifelse(vra > 0, 1, 0))

pl_annex_var_1617 %<>%
  left_join(places_vra, by = "plid") 

# cdps17 <- read_csv("plids/pl2016.csv") %>% # want to know which places are CDPs--they do not annex
#   select(Geo_NAME, plid) %>%
#   mutate(cdp = ifelse(grepl("CDP|cdp", Geo_NAME), 1, 0)) %>%
#   filter(cdp==1)

pl_annex_var_1617 %<>%
  #filter(!(plid %in% cdps17$plid)) %>%
  mutate(post = 0,
         time = "2016 to 2017") 

table(pl_annex_var_1617$annexing)

pl_annex_var_1617 %<>%
  #filter(pop_total > 0) %>%
  mutate(annexing = ifelse(annexing == 1 & (pop_total_1 < 1 | is.na(pop_total_1) | pop_total < 1), 0, annexing))
table(pl_annex_var_1617$annexing)
sapply(pl_annex_var_1617, function(x) sum(is.na(x)))  

table(pl_annex_var_1617$plid %in% plids)
table(pl_annex_var_1617$plid %in% p0708)
table(pl_annex_var_1617$plid %in% p0809)
table(pl_annex_var_1617$plid %in% p0910)
table(pl_annex_var_1617$plid %in% p1011)
table(pl_annex_var_1617$plid %in% p1112)
table(pl_annex_var_1617$plid %in% p1213)
table(pl_annex_var_1617$plid %in% p1415)
table(pl_annex_var_1617$plid %in% p1516)

p1617 <- unique(pl_annex_var_1617$plid)
pl_annex_vra_1617 <- unique(pl_annex_var_1617$plid[pl_annex_var_1617$vra==1])
rm(aa1617, cdps15, place_all, place_by_annex, places_vra)

# 2017-2018 ----
aa1718 <- read_csv("analyticalfiles/annexedblocks1718dem.csv") 
names(aa1718)

# transform this into place-level summaries
# characteristic of all annexable blocks
place_all <- aa1718 %>% 
  group_by(plid) %>%
  dplyr::summarize(pop_total = sum(pop, na.rm = T),
                   pct_annexed = mean(annexed, na.rm = T)
  ) %>%
  ungroup() 

place_by_annex <- aa1718 %>%
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

pl_annex_var_1718 <- left_join(
  place_all, place_by_annex, 
  by = "plid"
) %>%
  mutate(annexing = ifelse(!is.na(pop_total_1), 1, 0))
table(pl_annex_var_1718$annexing)
sapply(pl_annex_var_1718, function(x) sum(is.na(x)))  

# add vra indicator 
places_vra <- aa1718 %>%
  group_by(plid) %>%
  dplyr::summarize(vra = mean(vra, na.rm = T)) %>%
  ungroup() %>%
  mutate(vra = ifelse(vra > 0, 1, 0))

pl_annex_var_1718 %<>%
  left_join(places_vra, by = "plid") 

# cdps18 <- read_csv("plids/pl2016.csv") %>% # want to know which places are CDPs--they do not annex
#   select(Geo_NAME, plid) %>%
#   mutate(cdp = ifelse(grepl("CDP|cdp", Geo_NAME), 1, 0)) %>%
#   filter(cdp==1)

pl_annex_var_1718 %<>%
  #filter(!(plid %in% cdps18$plid)) %>%
  mutate(post = 0,
         time = "2017 to 2018") 

table(pl_annex_var_1718$annexing)

pl_annex_var_1718 %<>%
  #filter(pop_total > 0) %>%
  mutate(annexing = ifelse(annexing == 1 & (pop_total_1 < 1 | is.na(pop_total_1) | pop_total < 1), 0, annexing))
table(pl_annex_var_1718$annexing)
sapply(pl_annex_var_1718, function(x) sum(is.na(x)))  

table(pl_annex_var_1718$plid %in% plids)
table(pl_annex_var_1718$plid %in% p0708)
table(pl_annex_var_1718$plid %in% p0809)
table(pl_annex_var_1718$plid %in% p0910)
table(pl_annex_var_1718$plid %in% p1011)
table(pl_annex_var_1718$plid %in% p1112)
table(pl_annex_var_1718$plid %in% p1213)
table(pl_annex_var_1718$plid %in% p1415)
table(pl_annex_var_1718$plid %in% p1516)
table(pl_annex_var_1718$plid %in% p1617)

p1718 <- unique(pl_annex_var_1718$plid)
pl_annex_vra_1718 <- unique(pl_annex_var_1718$plid[pl_annex_var_1718$vra==1])
rm(aa1718, cdps18, place_all, place_by_annex, places_vra)

# 2018-2019 ----
aa1819 <- read_csv("analyticalfiles/annexedblocks1819dem.csv") 
names(aa1819)

# transform this into place-level summaries
# characteristic of all annexable blocks
place_all <- aa1819 %>% 
  group_by(plid) %>%
  dplyr::summarize(pop_total = sum(pop, na.rm = T),
                   pct_annexed = mean(annexed, na.rm = T)
  ) %>%
  ungroup() 

place_by_annex <- aa1819 %>%
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

pl_annex_var_1819 <- left_join(
  place_all, place_by_annex, 
  by = "plid"
) %>%
  mutate(annexing = ifelse(!is.na(pop_total_1), 1, 0))
table(pl_annex_var_1819$annexing)
sapply(pl_annex_var_1819, function(x) sum(is.na(x)))  

# add vra indicator 
places_vra <- aa1819 %>%
  group_by(plid) %>%
  dplyr::summarize(vra = mean(vra, na.rm = T)) %>%
  ungroup() %>%
  mutate(vra = ifelse(vra > 0, 1, 0))

pl_annex_var_1819 %<>%
  left_join(places_vra, by = "plid") 

# cdps19 <- read_csv("plids/pl2016.csv") %>% # want to know which places are CDPs--they do not annex
#   select(Geo_NAME, plid) %>%
#   mutate(cdp = ifelse(grepl("CDP|cdp", Geo_NAME), 1, 0)) %>%
#   filter(cdp==1)

pl_annex_var_1819 %<>%
  #filter(!(plid %in% cdps19$plid)) %>%
  mutate(post = 0,
         time = "2018 to 2019") 

table(pl_annex_var_1819$annexing)

pl_annex_var_1819 %<>%
  #filter(pop_total > 0) %>%
  mutate(annexing = ifelse(annexing == 1 & (pop_total_1 < 1 | is.na(pop_total_1) | pop_total < 1), 0, annexing))
table(pl_annex_var_1819$annexing)
sapply(pl_annex_var_1819, function(x) sum(is.na(x)))  

table(pl_annex_var_1819$plid %in% plids)
table(pl_annex_var_1819$plid %in% p0708)
table(pl_annex_var_1819$plid %in% p0809)
table(pl_annex_var_1819$plid %in% p0910)
table(pl_annex_var_1819$plid %in% p1011)
table(pl_annex_var_1819$plid %in% p1112)
table(pl_annex_var_1819$plid %in% p1213)
table(pl_annex_var_1819$plid %in% p1415)
table(pl_annex_var_1819$plid %in% p1516)
table(pl_annex_var_1819$plid %in% p1617)
table(pl_annex_var_1819$plid %in% p1718)

p1819 <- unique(pl_annex_var_1819$plid)
pl_annex_vra_1819 <- unique(pl_annex_var_1819$plid[pl_annex_var_1819$vra==1])
rm(aa1819, cdps19, place_all, place_by_annex, places_vra)

# 2019-2020 ----
aa1920 <- read_csv("analyticalfiles/annexedblocks1920dem.csv") 
names(aa1920)

# transform this into place-level summaries
# characteristic of all annexable blocks
place_all <- aa1920 %>% 
  group_by(plid) %>%
  dplyr::summarize(pop_total = sum(pop, na.rm = T),
                   pct_annexed = mean(annexed, na.rm = T)
  ) %>%
  ungroup() 

place_by_annex <- aa1920 %>%
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

pl_annex_var_1920 <- left_join(
  place_all, place_by_annex, 
  by = "plid"
) %>%
  mutate(annexing = ifelse(!is.na(pop_total_1), 1, 0))
table(pl_annex_var_1920$annexing)
sapply(pl_annex_var_1920, function(x) sum(is.na(x)))  

# add vra indicator 
places_vra <- aa1920 %>%
  group_by(plid) %>%
  dplyr::summarize(vra = mean(vra, na.rm = T)) %>%
  ungroup() %>%
  mutate(vra = ifelse(vra > 0, 1, 0))

pl_annex_var_1920 %<>%
  left_join(places_vra, by = "plid") 

# cdps20 <- read_csv("plids/pl2016.csv") %>% # want to know which places are CDPs--they do not annex
#   select(Geo_NAME, plid) %>%
#   mutate(cdp = ifelse(grepl("CDP|cdp", Geo_NAME), 1, 0)) %>%
#   filter(cdp==1)

pl_annex_var_1920 %<>%
  #filter(!(plid %in% cdps20$plid)) %>%
  mutate(post = 0,
         time = "2019 to 2020") 

table(pl_annex_var_1920$annexing)

pl_annex_var_1920 %<>%
  #filter(pop_total > 0) %>%
  mutate(annexing = ifelse(annexing == 1 & (pop_total_1 < 1 | is.na(pop_total_1) | pop_total < 1), 0, annexing))
table(pl_annex_var_1920$annexing)
sapply(pl_annex_var_1920, function(x) sum(is.na(x)))  

table(pl_annex_var_1920$plid %in% plids)
table(pl_annex_var_1920$plid %in% p0708)
table(pl_annex_var_1920$plid %in% p0809)
table(pl_annex_var_1920$plid %in% p0910)
table(pl_annex_var_1920$plid %in% p1011)
table(pl_annex_var_1920$plid %in% p1112)
table(pl_annex_var_1920$plid %in% p1213)
table(pl_annex_var_1920$plid %in% p1415)
table(pl_annex_var_1920$plid %in% p1516)
table(pl_annex_var_1920$plid %in% p1617)
table(pl_annex_var_1920$plid %in% p1718)
table(pl_annex_var_1920$plid %in% p1819)

p1920 <- unique(pl_annex_var_1920$plid)
pl_annex_vra_1920 <- unique(pl_annex_var_1920$plid[pl_annex_var_1920$vra==1])
rm(aa1920, cdps20, place_all, place_by_annex, places_vra)

# unique IDS ----
# unique plids common across time 
plids <- Reduce(intersect, list(unique(p0809), unique(p0910), unique(p1011), unique(p1112), unique(p1213), unique(p1415), unique(p1516), unique(p1617), unique(p1718), unique(p1819), unique(p1920)))

# unique vra plids
vraplids <- unique(c(pl_annex_vra_0708, pl_annex_vra_0809, pl_annex_vra_0910, pl_annex_vra_1011, pl_annex_vra_1112, pl_annex_vra_1213, pl_annex_vra_1415, pl_annex_vra_1516, pl_annex_vra_1617, pl_annex_vra_1718, pl_annex_vra_1819, pl_annex_vra_1920))

# make panel data ----
panel_annual <- rbind(pl_annex_var_0809, pl_annex_var_0910, pl_annex_var_1011, pl_annex_var_1112, pl_annex_var_1213, pl_annex_var_1415, pl_annex_var_1516, pl_annex_var_1617, pl_annex_var_1718, pl_annex_var_1819, pl_annex_var_1920)

table(panel_annual$vra, exclude = NULL)
length(unique(panel_annual$plid))

panel_annual %<>%
  mutate(vra = ifelse(plid %in% vraplids, 1, 0)) %>%
  filter(plid %in% plids)

table(panel_annual$vra, exclude = NULL)

save.image(paste0(curdir, "/", savedir, "annual_annexations.RData"))

# make cross-tab 
pl1314 <- panel_annual %>%
  filter(time == "2012 to 2013") %>%
  mutate(time = "2013 to 2014", 
         annexing = 0) 

panel_annual <- bind_rows(panel_annual, pl1314)

panel_annual_xt <- panel_annual %>%
  group_by(time, vra) %>%
  dplyr::summarize(annex = mean(annexing, na.rm = T)*100) %>%
  mutate(vra = as.character(vra),
         annex = ifelse(time == "2013 to 2014", NA, annex))

annual_xt <- ggplot(panel_annual_xt, aes(x = time, y = annex, group = vra)) + 
  geom_line(aes(linetype = vra)) + geom_point() + 
  theme(axis.text.x = element_text(angle = 50, hjust = 1, size = 10),
        axis.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12)) + 
  labs(linetype = "Covered by VRA") +
  xlab("Annual Period") +
  ylab("% of Places that Annexed")

annual_xt

ggsave(filename = paste0(savedir, "annual_xt_viz.pdf"),
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
  filter(!time %in% "2013 to 2014") %>% 
  mutate(post = ifelse(time %in% c("2007 to 2008", "2008 to 2009", "2009 to 2010", "2010 to 2011", "2011 to 2012", "2012 to 2013"), "2007 to 2013", "2014 to 2020")) %>%
  group_by(plid, post) %>%
  summarize(annex = mean(annexing),
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

ggsave(filename = paste0(curdir, "/", savedir, "annual_xt_agg_viz.pdf"),
       plot = annual_xt,
       dpi = 300)

# this looks good/consistent with the paper 

# event study -- need to do this with annual periods ####
modsa <- feols(annexing ~ i(time, ref = "2014 to 2015"),
               cluster = "plid",
               data = panel_annual)

iplot(modsa, drop = "(Intercept)", xlab = "Time")

save.image(paste0(curdir, "/", savedir, "annual_annexations.RData"))
