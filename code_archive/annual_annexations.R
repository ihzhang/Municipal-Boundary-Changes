# -------------------------------------------------------------------------
# Created by: Iris Zhang                     
# Date created: 07/10/2023             
# Last revised:            
# Project: MBC         
# Subproject: Analysis
# Re: Do annual annexations analysis for R&R       
# -------------------------------------------------------------------------

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
# load(paste0(savedir, "annual_annexations.RData"))
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
  mutate(incopp = man + ret) %>%
  group_by(plid) %>%
  dplyr::summarize(pop_total = sum(pop, na.rm = T),
                   area_total = sum(area, na.rm = T),
                   nhblack_total = sum(nhblack, na.rm = T),
                   nhwhite_total = sum(nhwhite, na.rm = T),
                   nbmin_total = sum(nbmin, na.rm = T),
                   pct_annexed = mean(annexed, na.rm = T),
                   njobs_total = sum(njobs07, na.rm = T),
                   nhincjobs_total = sum(nhincjobs07, na.rm = T),
                   nwork_total = sum(jobs, na.rm = T),
                   incopp_total = sum(incopp, na.rm = T),
                   hu_total = sum(hu, na.rm = T),
                   owneroccupied_total = sum(owneroccupied, na.rm = T)
  ) %>%
  ungroup() %>%
  mutate(pctnhblack_total = ifelse(pop_total == 0, 0, (nhblack_total/pop_total)*100),
         pctnhwhite_total = ifelse(pop_total == 0, 0, (nhwhite_total/pop_total)*100),
         pctnbmin_total = ifelse(pop_total == 0, 0, (nbmin_total/pop_total)*100),
         pctownerocc_total = ifelse(hu_total == 0, 0, (owneroccupied_total/hu_total)*100),
         pctincopp_total = ifelse(nwork_total == 0, 0, (incopp_total/nwork_total)*100),
         pcthincjobs_total = ifelse(njobs_total == 0, 0, (nhincjobs_total/njobs_total)*100))

place_by_annex <- aa0708 %>%
  mutate(incopp = man + ret) %>%
  filter(annexed==1) %>%
  group_by(plid, annexed) %>%
  dplyr::summarize(pop_total = sum(pop, na.rm = T),
                   area_total = sum(area, na.rm = T),
                   nhblack_total = sum(nhblack, na.rm = T),
                   nhwhite_total = sum(nhwhite, na.rm = T),
                   nbmin_total = sum(nbmin, na.rm = T),
                   njobs_total = sum(njobs07, na.rm = T),
                   nhincjobs_total = sum(nhincjobs07, na.rm = T),
                   nwork_total = sum(jobs, na.rm = T),
                   incopp_total = sum(incopp, na.rm = T),
                   hu_total = sum(hu, na.rm = T),
                   owneroccupied_total = sum(owneroccupied, na.rm = T)) %>%
  ungroup() %>%
  pivot_wider(
    id_cols = plid,
    names_from = annexed,
    values_from = c(pop_total:owneroccupied_total)
  ) %>%
  mutate(pctnhblack_total_1 = ifelse(pop_total_1 == 0, 0, (nhblack_total_1/pop_total_1)*100),
         pctnhblack_total_1 = ifelse(pop_total_1 == 0, 0, (nhwhite_total_1/pop_total_1)*100),
         pctnbmin_total_1 = ifelse(pop_total_1 == 0, 0, (nbmin_total_1/pop_total_1)*100),
         pctincopp_total_1 = ifelse(nwork_total_1 == 0, 0, (incopp_total_1/nwork_total_1)*100),
         pcthincjobs_total_1 = ifelse(njobs_total_1 == 0, 0, (nhincjobs_total_1/njobs_total_1)*100),
         pctownerocc_total_1 = (owneroccupied_total_1/hu_total_1)*100) 

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

# place-level variables
pl07 <- read_csv("pl0017_interpolated.csv") %>% filter(Year == "2007")
table(pl_annex_var_0708$plid %in% pl07$plid) 

cdps08 <- read_csv("plids/pl2008.csv") %>% # want to know which places are CDPs--they do not annex
  select(Geo_NAME, plid) %>%
  mutate(cdp = ifelse(grepl("CDP|cdp", Geo_NAME), 1, 0)) %>%
  filter(cdp==1)

pl_annex_var_0708 %<>%
  filter(plid %in% pl07$plid & !(plid %in% cdps08$plid)) %>%
  left_join(pl07, by = "plid") %>%
  mutate(post = 0,
         time = "2007 to 2008",
         pctowneroccupied = (owneroccupied/hu)*100) 

table(pl_annex_var_0708$annexing)

pl_annex_var_0708 %<>%
  #filter(pop_total > 0) %>%
  mutate(annexing = ifelse(annexing == 1 & (pop_total_1 < 1 | is.na(pop_total_1) | pop_total < 1), 0, annexing))
table(pl_annex_var_0708$annexing)

sapply(pl_annex_var_0708, function(x) sum(is.na(x)))  

pl_annex_var_0708 %<>%
  mutate(
    underbound_black = ifelse(
      (annexing == 1 & pctnhblack_total_1 < pctnhblack), 1, 0), 
    underbound_nbmin = ifelse(
      (annexing == 1 & pctnbmin_total_1 < pctnbmin), 1, 0)
  )

pl_annex_var_0708 %<>%
  filter(pop > 0) 

pl_annex_var_0708 %<>%
  mutate(more_white = ifelse(pctnhwhite_total > pctnhwhite, 1, 0)) %>%
  filter_at(vars(pop, popdensity, pctnhblack_total, pctnbmin_total, more_white, pctowneroccupied, mhmval, hinc, ppov, pctblackpov, pctnbminpov, pctownerocc_total, pcthincjobs_total, pctincopp_total), ~!is.na(.))

p0708 <- unique(pl_annex_var_0708$plid)
pl_annex_vra_0708 <- unique(pl_annex_var_0708$plid[pl_annex_var_0708$vra==1])

rm(aa0708, place_all, place_by_annex, places_vra, cdps08, pl07)

# 2008-2009 ----
aa0809 <- read_csv("analyticalfiles/annexedblocks0809dem.csv") 
names(aa0809)

# transform this into place-level summaries
# characteristic of all annexable blocks
# diff between annexed and not annexed 
place_all <- aa0809 %>% 
  mutate(incopp = man + ret) %>%
  group_by(plid) %>%
  dplyr::summarize(pop_total = sum(pop, na.rm = T),
                   area_total = sum(area, na.rm = T),
                   nhblack_total = sum(nhblack, na.rm = T),
                   nhwhite_total = sum(nhwhite, na.rm = T),
                   nbmin_total = sum(nbmin, na.rm = T),
                   pct_annexed = mean(annexed, na.rm = T),
                   njobs_total = sum(njobs08, na.rm = T),
                   nhincjobs_total = sum(nhincjobs08, na.rm = T),
                   nwork_total = sum(jobs, na.rm = T),
                   incopp_total = sum(incopp, na.rm = T),
                   hu_total = sum(hu, na.rm = T),
                   owneroccupied_total = sum(owneroccupied, na.rm = T)
  ) %>%
  ungroup() %>%
  mutate(pctnhblack_total = ifelse(pop_total == 0, 0, (nhblack_total/pop_total)*100),
         pctnhwhite_total = ifelse(pop_total == 0, 0, (nhwhite_total/pop_total)*100),
         pctnbmin_total = ifelse(pop_total == 0, 0, (nbmin_total/pop_total)*100),
         pctownerocc_total = ifelse(hu_total == 0, 0, (owneroccupied_total/hu_total)*100),
         pctincopp_total = ifelse(nwork_total == 0, 0, (incopp_total/nwork_total)*100),
         pcthincjobs_total = ifelse(njobs_total == 0, 0, (nhincjobs_total/njobs_total)*100))

place_by_annex <- aa0809 %>%
  mutate(incopp = man + ret) %>%
  filter(annexed==1) %>%
  group_by(plid, annexed) %>%
  dplyr::summarize(pop_total = sum(pop, na.rm = T),
                   area_total = sum(area, na.rm = T),
                   nhblack_total = sum(nhblack, na.rm = T),
                   nhwhite_total = sum(nhwhite, na.rm = T),
                   nbmin_total = sum(nbmin, na.rm = T),
                   njobs_total = sum(njobs08, na.rm = T),
                   nhincjobs_total = sum(nhincjobs08, na.rm = T),
                   nwork_total = sum(jobs, na.rm = T),
                   incopp_total = sum(incopp, na.rm = T),
                   hu_total = sum(hu, na.rm = T),
                   owneroccupied_total = sum(owneroccupied, na.rm = T)) %>%
  ungroup() %>%
  pivot_wider(
    id_cols = plid,
    names_from = annexed,
    values_from = c(pop_total:owneroccupied_total)
  ) %>%
  mutate(pctnhblack_total_1 = ifelse(pop_total_1 == 0, 0, (nhblack_total_1/pop_total_1)*100),
         pctnhblack_total_1 = ifelse(pop_total_1 == 0, 0, (nhwhite_total_1/pop_total_1)*100),
         pctnbmin_total_1 = ifelse(pop_total_1 == 0, 0, (nbmin_total_1/pop_total_1)*100),
         pctincopp_total_1 = ifelse(nwork_total_1 == 0, 0, (incopp_total_1/nwork_total_1)*100),
         pcthincjobs_total_1 = ifelse(njobs_total_1 == 0, 0, (nhincjobs_total_1/njobs_total_1)*100),
         pctownerocc_total_1 = (owneroccupied_total_1/hu_total_1)*100) 

sapply(place_all, function(x) sum(is.na(x)))  

pl_annex_var_0809 <- left_join(
  place_all, place_by_annex, 
  by = "plid"
) %>%
  mutate(annexing = ifelse(!is.na(pop_total_1), 1, 0))
table(pl_annex_var_0809$annexing)
sapply(place_all, function(x) sum(is.na(x)))  

# add vra indicator 
places_vra <- aa0809 %>%
  group_by(plid) %>%
  dplyr::summarize(vra = mean(vra, na.rm = T)) %>%
  ungroup() %>%
  mutate(vra = ifelse(vra > 0, 1, 0))

pl_annex_var_0809 %<>%
  left_join(places_vra, by = "plid") 

# place-level variables
pl08 <- read_csv("pl0017_interpolated.csv") %>% filter(Year == "2008")
table(pl_annex_var_0809$plid %in% pl08$plid) 

cdps09 <- read_csv("plids/pl2009.csv") %>% # want to know which places are CDPs--they do not annex
  select(Geo_NAME, plid) %>%
  mutate(cdp = ifelse(grepl("CDP|cdp", Geo_NAME), 1, 0)) %>%
  filter(cdp==1)

pl_annex_var_0809 %<>%
  filter(plid %in% pl08$plid & !(plid %in% cdps09$plid)) %>%
  left_join(pl08, by = "plid") %>%
  mutate(post = 0,
         time = "2008 to 2009",
         pctowneroccupied = (owneroccupied/hu)*100) 

table(pl_annex_var_0809$annexing)

pl_annex_var_0809 %<>%
  #filter(pop_total > 0) %>%
  mutate(annexing = ifelse(annexing == 1 & (pop_total_1 < 1 | is.na(pop_total_1) | pop_total < 1), 0, annexing))
table(pl_annex_var_0809$annexing)

sapply(pl_annex_var_0809, function(x) sum(is.na(x)))  

pl_annex_var_0809 %<>%
  mutate(
    underbound_black = ifelse(
      (annexing == 1 & pctnhblack_total_1 < pctnhblack), 1, 0), 
    underbound_nbmin = ifelse(
      (annexing == 1 & pctnbmin_total_1 < pctnbmin), 1, 0)
  )

pl_annex_var_0809 %<>%
  filter(pop > 0) 

pl_annex_var_0809 %<>%
  mutate(more_white = ifelse(pctnhwhite_total > pctnhwhite, 1, 0)) %>%
  filter_at(vars(pop, popdensity, pctnhblack_total, pctnbmin_total, more_white, pctowneroccupied, mhmval, hinc, ppov, pctblackpov, pctnbminpov, pctownerocc_total, pcthincjobs_total, pctincopp_total), ~!is.na(.))

p0809 <- unique(pl_annex_var_0809$plid)
pl_annex_vra_0809 <- unique(pl_annex_var_0809$plid[pl_annex_var_0809$vra==1])

rm(aa0809, place_all, place_by_annex, places_vra, cdps09, pl08)

# 2009-2010 ----
aa0910 <- read_csv("analyticalfiles/annexedblocks0910dem.csv") 
names(aa0910)

# transform this into place-level summaries
# characteristic of all annexable blocks
# diff between annexed and not annexed 
place_all <- aa0910 %>% 
  mutate(incopp = man + ret) %>%
  group_by(plid) %>%
  dplyr::summarize(pop_total = sum(pop, na.rm = T),
                   area_total = sum(area, na.rm = T),
                   nhblack_total = sum(nhblack, na.rm = T),
                   nhwhite_total = sum(nhwhite, na.rm = T),
                   nbmin_total = sum(nbmin, na.rm = T),
                   pct_annexed = mean(annexed, na.rm = T),
                   njobs_total = sum(njobs09, na.rm = T),
                   nhincjobs_total = sum(nhincjobs09, na.rm = T),
                   nwork_total = sum(jobs, na.rm = T),
                   incopp_total = sum(incopp, na.rm = T),
                   hu_total = sum(hu, na.rm = T),
                   owneroccupied_total = sum(owneroccupied, na.rm = T)
  ) %>%
  ungroup() %>%
  mutate(pctnhblack_total = ifelse(pop_total == 0, 0, (nhblack_total/pop_total)*100),
         pctnhwhite_total = ifelse(pop_total == 0, 0, (nhwhite_total/pop_total)*100),
         pctnbmin_total = ifelse(pop_total == 0, 0, (nbmin_total/pop_total)*100),
         pctownerocc_total = ifelse(hu_total == 0, 0, (owneroccupied_total/hu_total)*100),
         pctincopp_total = ifelse(nwork_total == 0, 0, (incopp_total/nwork_total)*100),
         pcthincjobs_total = ifelse(njobs_total == 0, 0, (nhincjobs_total/njobs_total)*100))

place_by_annex <- aa0910 %>%
  mutate(incopp = man + ret) %>%
  filter(annexed==1) %>%
  group_by(plid, annexed) %>%
  dplyr::summarize(pop_total = sum(pop, na.rm = T),
                   area_total = sum(area, na.rm = T),
                   nhblack_total = sum(nhblack, na.rm = T),
                   nhwhite_total = sum(nhwhite, na.rm = T),
                   nbmin_total = sum(nbmin, na.rm = T),
                   njobs_total = sum(njobs09, na.rm = T),
                   nhincjobs_total = sum(nhincjobs09, na.rm = T),
                   nwork_total = sum(jobs, na.rm = T),
                   incopp_total = sum(incopp, na.rm = T),
                   hu_total = sum(hu, na.rm = T),
                   owneroccupied_total = sum(owneroccupied, na.rm = T)) %>%
  ungroup() %>%
  pivot_wider(
    id_cols = plid,
    names_from = annexed,
    values_from = c(pop_total:owneroccupied_total)
  ) %>%
  mutate(pctnhblack_total_1 = ifelse(pop_total_1 == 0, 0, (nhblack_total_1/pop_total_1)*100),
         pctnhblack_total_1 = ifelse(pop_total_1 == 0, 0, (nhwhite_total_1/pop_total_1)*100),
         pctnbmin_total_1 = ifelse(pop_total_1 == 0, 0, (nbmin_total_1/pop_total_1)*100),
         pctincopp_total_1 = ifelse(nwork_total_1 == 0, 0, (incopp_total_1/nwork_total_1)*100),
         pcthincjobs_total_1 = ifelse(njobs_total_1 == 0, 0, (nhincjobs_total_1/njobs_total_1)*100),
         pctownerocc_total_1 = (owneroccupied_total_1/hu_total_1)*100) 

sapply(place_all, function(x) sum(is.na(x)))  

pl_annex_var_0910 <- left_join(
  place_all, place_by_annex, 
  by = "plid"
) %>%
  mutate(annexing = ifelse(!is.na(pop_total_1), 1, 0))
table(pl_annex_var_0910$annexing)
sapply(place_all, function(x) sum(is.na(x)))  

# add vra indicator 
places_vra <- aa0910 %>%
  group_by(plid) %>%
  dplyr::summarize(vra = mean(vra, na.rm = T)) %>%
  ungroup() %>%
  mutate(vra = ifelse(vra > 0, 1, 0))

pl_annex_var_0910 %<>%
  left_join(places_vra, by = "plid") 

# place-level variables
pl09 <- read_csv("pl0017_interpolated.csv") %>% filter(Year == "2009")
table(pl_annex_var_0910$plid %in% pl09$plid) 

cdps10 <- read_csv("plids/pl2010.csv") %>% # want to know which places are CDPs--they do not annex
  select(Geo_NAME, plid) %>%
  mutate(cdp = ifelse(grepl("CDP|cdp", Geo_NAME), 1, 0)) %>%
  filter(cdp==1)

pl_annex_var_0910 %<>%
  filter(plid %in% pl09$plid & !(plid %in% cdps10$plid)) %>%
  left_join(pl09, by = "plid") %>%
  mutate(post = 0,
         time = "2009 to 2010",
         pctowneroccupied = (owneroccupied/hu)*100) 

table(pl_annex_var_0910$annexing)

pl_annex_var_0910 %<>%
  #filter(pop_total > 0) %>%
  mutate(annexing = ifelse(annexing == 1 & (pop_total_1 < 1 | is.na(pop_total_1) | pop_total < 1), 0, annexing))
table(pl_annex_var_0910$annexing)

sapply(pl_annex_var_0910, function(x) sum(is.na(x)))  

pl_annex_var_0910 %<>%
  mutate(
    underbound_black = ifelse(
      (annexing == 1 & pctnhblack_total_1 < pctnhblack), 1, 0), 
    underbound_nbmin = ifelse(
      (annexing == 1 & pctnbmin_total_1 < pctnbmin), 1, 0)
  )

pl_annex_var_0910 %<>%
  filter(pop > 0) 

pl_annex_var_0910 %<>%
  mutate(more_white = ifelse(pctnhwhite_total > pctnhwhite, 1, 0)) %>%
  filter_at(vars(pop, popdensity, pctnhblack_total, pctnbmin_total, more_white, pctowneroccupied, mhmval, hinc, ppov, pctblackpov, pctnbminpov, pctownerocc_total, pcthincjobs_total, pctincopp_total), ~!is.na(.))

p0910 <- unique(pl_annex_var_0910$plid)
pl_annex_vra_0910 <- unique(pl_annex_var_0910$plid[pl_annex_var_0910$vra==1])

rm(aa0910, place_all, place_by_annex, places_vra, cdps10, pl09)

# 2010-2011 ----
aa1011 <- read_csv("analyticalfiles/annexedblocks1011dem.csv") 
names(aa1011)

# transform this into place-level summaries
# characteristic of all annexable blocks
# diff between annexed and not annexed 
place_all <- aa1011 %>% 
  mutate(incopp = man + ret) %>%
  group_by(plid) %>%
  dplyr::summarize(pop_total = sum(pop, na.rm = T),
                   area_total = sum(area, na.rm = T),
                   nhblack_total = sum(nhblack, na.rm = T),
                   nhwhite_total = sum(nhwhite, na.rm = T),
                   nbmin_total = sum(nbmin, na.rm = T),
                   pct_annexed = mean(annexed, na.rm = T),
                   njobs_total = sum(njobs10, na.rm = T),
                   nhincjobs_total = sum(nhincjobs10, na.rm = T),
                   nwork_total = sum(jobs, na.rm = T),
                   incopp_total = sum(incopp, na.rm = T),
                   hu_total = sum(hu, na.rm = T),
                   owneroccupied_total = sum(owneroccupied, na.rm = T)
  ) %>%
  ungroup() %>%
  mutate(pctnhblack_total = ifelse(pop_total == 0, 0, (nhblack_total/pop_total)*100),
         pctnhwhite_total = ifelse(pop_total == 0, 0, (nhwhite_total/pop_total)*100),
         pctnbmin_total = ifelse(pop_total == 0, 0, (nbmin_total/pop_total)*100),
         pctownerocc_total = ifelse(hu_total == 0, 0, (owneroccupied_total/hu_total)*100),
         pctincopp_total = ifelse(nwork_total == 0, 0, (incopp_total/nwork_total)*100),
         pcthincjobs_total = ifelse(njobs_total == 0, 0, (nhincjobs_total/njobs_total)*100))

place_by_annex <- aa1011 %>%
  mutate(incopp = man + ret) %>%
  filter(annexed==1) %>%
  group_by(plid, annexed) %>%
  dplyr::summarize(pop_total = sum(pop, na.rm = T),
                   area_total = sum(area, na.rm = T),
                   nhblack_total = sum(nhblack, na.rm = T),
                   nhwhite_total = sum(nhwhite, na.rm = T),
                   nbmin_total = sum(nbmin, na.rm = T),
                   njobs_total = sum(njobs10, na.rm = T),
                   nhincjobs_total = sum(nhincjobs10, na.rm = T),
                   nwork_total = sum(jobs, na.rm = T),
                   incopp_total = sum(incopp, na.rm = T),
                   hu_total = sum(hu, na.rm = T),
                   owneroccupied_total = sum(owneroccupied, na.rm = T)) %>%
  ungroup() %>%
  pivot_wider(
    id_cols = plid,
    names_from = annexed,
    values_from = c(pop_total:owneroccupied_total)
  ) %>%
  mutate(pctnhblack_total_1 = ifelse(pop_total_1 == 0, 0, (nhblack_total_1/pop_total_1)*100),
         pctnhblack_total_1 = ifelse(pop_total_1 == 0, 0, (nhwhite_total_1/pop_total_1)*100),
         pctnbmin_total_1 = ifelse(pop_total_1 == 0, 0, (nbmin_total_1/pop_total_1)*100),
         pctincopp_total_1 = ifelse(nwork_total_1 == 0, 0, (incopp_total_1/nwork_total_1)*100),
         pcthincjobs_total_1 = ifelse(njobs_total_1 == 0, 0, (nhincjobs_total_1/njobs_total_1)*100),
         pctownerocc_total_1 = (owneroccupied_total_1/hu_total_1)*100) 

sapply(place_all, function(x) sum(is.na(x)))  

pl_annex_var_1011 <- left_join(
  place_all, place_by_annex, 
  by = "plid"
) %>%
  mutate(annexing = ifelse(!is.na(pop_total_1), 1, 0))
table(pl_annex_var_1011$annexing)
sapply(place_all, function(x) sum(is.na(x)))  

# add vra indicator 
places_vra <- aa1011 %>%
  group_by(plid) %>%
  dplyr::summarize(vra = mean(vra, na.rm = T)) %>%
  ungroup() %>%
  mutate(vra = ifelse(vra > 0, 1, 0))

pl_annex_var_1011 %<>%
  left_join(places_vra, by = "plid") 

# place-level variables
pl10 <- read_csv("pl0017_interpolated.csv") %>% filter(Year == "2010")
table(pl_annex_var_1011$plid %in% pl10$plid) 

cdps11 <- read_csv("plids/pl2011.csv") %>% # want to know which places are CDPs--they do not annex
  select(Geo_NAME, plid) %>%
  mutate(cdp = ifelse(grepl("CDP|cdp", Geo_NAME), 1, 0)) %>%
  filter(cdp==1)

pl_annex_var_1011 %<>%
  filter(plid %in% pl10$plid & !(plid %in% cdps11$plid)) %>%
  left_join(pl10, by = "plid") %>%
  mutate(post = 0,
         time = "2010 to 2011",
         pctowneroccupied = (owneroccupied/hu)*100) 

table(pl_annex_var_1011$annexing)

pl_annex_var_1011 %<>%
  #filter(pop_total > 0) %>%
  mutate(annexing = ifelse(annexing == 1 & (pop_total_1 < 1 | is.na(pop_total_1) | pop_total < 1), 0, annexing))
table(pl_annex_var_1011$annexing)

sapply(pl_annex_var_1011, function(x) sum(is.na(x)))  

pl_annex_var_1011 %<>%
  mutate(
    underbound_black = ifelse(
      (annexing == 1 & pctnhblack_total_1 < pctnhblack), 1, 0), 
    underbound_nbmin = ifelse(
      (annexing == 1 & pctnbmin_total_1 < pctnbmin), 1, 0)
  )

pl_annex_var_1011 %<>%
  filter(pop > 0) 

pl_annex_var_1011 %<>%
  mutate(more_white = ifelse(pctnhwhite_total > pctnhwhite, 1, 0)) %>%
  filter_at(vars(pop, popdensity, pctnhblack_total, pctnbmin_total, more_white, pctowneroccupied, mhmval, hinc, ppov, pctblackpov, pctnbminpov, pctownerocc_total, pcthincjobs_total, pctincopp_total), ~!is.na(.))

p1011 <- unique(pl_annex_var_1011$plid)
pl_annex_vra_1011 <- unique(pl_annex_var_1011$plid[pl_annex_var_1011$vra==1])

rm(aa1011, place_all, place_by_annex, places_vra, cdps11, pl10)

# 2011-2012 ----
aa1112 <- read_csv("analyticalfiles/annexedblocks1112dem.csv") 
names(aa1112)

# transform this into place-level summaries
# characteristic of all annexable blocks
# diff between annexed and not annexed 
place_all <- aa1112 %>% 
  mutate(incopp = man + ret) %>%
  group_by(plid) %>%
  dplyr::summarize(pop_total = sum(pop, na.rm = T),
                   area_total = sum(area, na.rm = T),
                   nhblack_total = sum(nhblack, na.rm = T),
                   nhwhite_total = sum(nhwhite, na.rm = T),
                   nbmin_total = sum(nbmin, na.rm = T),
                   pct_annexed = mean(annexed, na.rm = T),
                   njobs_total = sum(njobs11, na.rm = T),
                   nhincjobs_total = sum(nhincjobs11, na.rm = T),
                   nwork_total = sum(jobs, na.rm = T),
                   incopp_total = sum(incopp, na.rm = T),
                   hu_total = sum(hu, na.rm = T),
                   owneroccupied_total = sum(owneroccupied, na.rm = T)
  ) %>%
  ungroup() %>%
  mutate(pctnhblack_total = ifelse(pop_total == 0, 0, (nhblack_total/pop_total)*100),
         pctnhwhite_total = ifelse(pop_total == 0, 0, (nhwhite_total/pop_total)*100),
         pctnbmin_total = ifelse(pop_total == 0, 0, (nbmin_total/pop_total)*100),
         pctownerocc_total = ifelse(hu_total == 0, 0, (owneroccupied_total/hu_total)*100),
         pctincopp_total = ifelse(nwork_total == 0, 0, (incopp_total/nwork_total)*100),
         pcthincjobs_total = ifelse(njobs_total == 0, 0, (nhincjobs_total/njobs_total)*100))

place_by_annex <- aa1112 %>%
  mutate(incopp = man + ret) %>%
  filter(annexed==1) %>%
  group_by(plid, annexed) %>%
  dplyr::summarize(pop_total = sum(pop, na.rm = T),
                   area_total = sum(area, na.rm = T),
                   nhblack_total = sum(nhblack, na.rm = T),
                   nhwhite_total = sum(nhwhite, na.rm = T),
                   nbmin_total = sum(nbmin, na.rm = T),
                   njobs_total = sum(njobs11, na.rm = T),
                   nhincjobs_total = sum(nhincjobs11, na.rm = T),
                   nwork_total = sum(jobs, na.rm = T),
                   incopp_total = sum(incopp, na.rm = T),
                   hu_total = sum(hu, na.rm = T),
                   owneroccupied_total = sum(owneroccupied, na.rm = T)) %>%
  ungroup() %>%
  pivot_wider(
    id_cols = plid,
    names_from = annexed,
    values_from = c(pop_total:owneroccupied_total)
  ) %>%
  mutate(pctnhblack_total_1 = ifelse(pop_total_1 == 0, 0, (nhblack_total_1/pop_total_1)*100),
         pctnhblack_total_1 = ifelse(pop_total_1 == 0, 0, (nhwhite_total_1/pop_total_1)*100),
         pctnbmin_total_1 = ifelse(pop_total_1 == 0, 0, (nbmin_total_1/pop_total_1)*100),
         pctincopp_total_1 = ifelse(nwork_total_1 == 0, 0, (incopp_total_1/nwork_total_1)*100),
         pcthincjobs_total_1 = ifelse(njobs_total_1 == 0, 0, (nhincjobs_total_1/njobs_total_1)*100),
         pctownerocc_total_1 = (owneroccupied_total_1/hu_total_1)*100) 

sapply(place_all, function(x) sum(is.na(x)))  

pl_annex_var_1112 <- left_join(
  place_all, place_by_annex, 
  by = "plid"
) %>%
  mutate(annexing = ifelse(!is.na(pop_total_1), 1, 0))
table(pl_annex_var_1112$annexing)
sapply(place_all, function(x) sum(is.na(x)))  

# add vra indicator 
places_vra <- aa1112 %>%
  group_by(plid) %>%
  dplyr::summarize(vra = mean(vra, na.rm = T)) %>%
  ungroup() %>%
  mutate(vra = ifelse(vra > 0, 1, 0))

pl_annex_var_1112 %<>%
  left_join(places_vra, by = "plid") 

# place-level variables
pl11 <- read_csv("pl0017_interpolated.csv") %>% filter(Year == "2011")
table(pl_annex_var_1112$plid %in% pl11$plid) 

cdps12 <- read_csv("plids/pl2012.csv") %>% # want to know which places are CDPs--they do not annex
  select(Geo_NAME, plid) %>%
  mutate(cdp = ifelse(grepl("CDP|cdp", Geo_NAME), 1, 0)) %>%
  filter(cdp==1)

pl_annex_var_1112 %<>%
  filter(plid %in% pl11$plid & !(plid %in% cdps12$plid)) %>%
  left_join(pl11, by = "plid") %>%
  mutate(post = 0,
         time = "2011 to 2012",
         pctowneroccupied = (owneroccupied/hu)*100) 

table(pl_annex_var_1112$annexing)

pl_annex_var_1112 %<>%
  #filter(pop_total > 0) %>%
  mutate(annexing = ifelse(annexing == 1 & (pop_total_1 < 1 | is.na(pop_total_1) | pop_total < 1), 0, annexing))
table(pl_annex_var_1112$annexing)

sapply(pl_annex_var_1112, function(x) sum(is.na(x)))  

pl_annex_var_1112 %<>%
  mutate(
    underbound_black = ifelse(
      (annexing == 1 & pctnhblack_total_1 < pctnhblack), 1, 0), 
    underbound_nbmin = ifelse(
      (annexing == 1 & pctnbmin_total_1 < pctnbmin), 1, 0)
  )

pl_annex_var_1112 %<>%
  filter(pop > 0) 

pl_annex_var_1112 %<>%
  mutate(more_white = ifelse(pctnhwhite_total > pctnhwhite, 1, 0)) %>%
  filter_at(vars(pop, popdensity, pctnhblack_total, pctnbmin_total, more_white, pctowneroccupied, mhmval, hinc, ppov, pctblackpov, pctnbminpov, pctownerocc_total, pcthincjobs_total, pctincopp_total), ~!is.na(.))

p1112 <- unique(pl_annex_var_1112$plid)
pl_annex_vra_1112 <- unique(pl_annex_var_1112$plid[pl_annex_var_1112$vra==1])

rm(aa1112, place_all, place_by_annex, places_vra, cdps12, pl11)

# 2012-2013 ----
aa1213 <- read_csv("analyticalfiles/annexedblocks1213dem.csv") 
names(aa1213)

# transform this into place-level summaries
# characteristic of all annexable blocks
# diff between annexed and not annexed 
place_all <- aa1213 %>% 
  mutate(incopp = man + ret) %>%
  group_by(plid) %>%
  dplyr::summarize(pop_total = sum(pop, na.rm = T),
                   area_total = sum(area, na.rm = T),
                   nhblack_total = sum(nhblack, na.rm = T),
                   nhwhite_total = sum(nhwhite, na.rm = T),
                   nbmin_total = sum(nbmin, na.rm = T),
                   pct_annexed = mean(annexed, na.rm = T),
                   njobs_total = sum(njobs12, na.rm = T),
                   nhincjobs_total = sum(nhincjobs12, na.rm = T),
                   nwork_total = sum(jobs, na.rm = T),
                   incopp_total = sum(incopp, na.rm = T),
                   hu_total = sum(hu, na.rm = T),
                   owneroccupied_total = sum(owneroccupied, na.rm = T)
  ) %>%
  ungroup() %>%
  mutate(pctnhblack_total = ifelse(pop_total == 0, 0, (nhblack_total/pop_total)*100),
         pctnhwhite_total = ifelse(pop_total == 0, 0, (nhwhite_total/pop_total)*100),
         pctnbmin_total = ifelse(pop_total == 0, 0, (nbmin_total/pop_total)*100),
         pctownerocc_total = ifelse(hu_total == 0, 0, (owneroccupied_total/hu_total)*100),
         pctincopp_total = ifelse(nwork_total == 0, 0, (incopp_total/nwork_total)*100),
         pcthincjobs_total = ifelse(njobs_total == 0, 0, (nhincjobs_total/njobs_total)*100))

place_by_annex <- aa1213 %>%
  mutate(incopp = man + ret) %>%
  filter(annexed==1) %>%
  group_by(plid, annexed) %>%
  dplyr::summarize(pop_total = sum(pop, na.rm = T),
                   area_total = sum(area, na.rm = T),
                   nhblack_total = sum(nhblack, na.rm = T),
                   nhwhite_total = sum(nhwhite, na.rm = T),
                   nbmin_total = sum(nbmin, na.rm = T),
                   njobs_total = sum(njobs12, na.rm = T),
                   nhincjobs_total = sum(nhincjobs12, na.rm = T),
                   nwork_total = sum(jobs, na.rm = T),
                   incopp_total = sum(incopp, na.rm = T),
                   hu_total = sum(hu, na.rm = T),
                   owneroccupied_total = sum(owneroccupied, na.rm = T)) %>%
  ungroup() %>%
  pivot_wider(
    id_cols = plid,
    names_from = annexed,
    values_from = c(pop_total:owneroccupied_total)
  ) %>%
  mutate(pctnhblack_total_1 = ifelse(pop_total_1 == 0, 0, (nhblack_total_1/pop_total_1)*100),
         pctnhblack_total_1 = ifelse(pop_total_1 == 0, 0, (nhwhite_total_1/pop_total_1)*100),
         pctnbmin_total_1 = ifelse(pop_total_1 == 0, 0, (nbmin_total_1/pop_total_1)*100),
         pctincopp_total_1 = ifelse(nwork_total_1 == 0, 0, (incopp_total_1/nwork_total_1)*100),
         pcthincjobs_total_1 = ifelse(njobs_total_1 == 0, 0, (nhincjobs_total_1/njobs_total_1)*100),
         pctownerocc_total_1 = (owneroccupied_total_1/hu_total_1)*100) 

sapply(place_all, function(x) sum(is.na(x)))  

pl_annex_var_1213 <- left_join(
  place_all, place_by_annex, 
  by = "plid"
) %>%
  mutate(annexing = ifelse(!is.na(pop_total_1), 1, 0))
table(pl_annex_var_1213$annexing)
sapply(place_all, function(x) sum(is.na(x)))  

# add vra indicator 
places_vra <- aa1213 %>%
  group_by(plid) %>%
  dplyr::summarize(vra = mean(vra, na.rm = T)) %>%
  ungroup() %>%
  mutate(vra = ifelse(vra > 0, 1, 0))

pl_annex_var_1213 %<>%
  left_join(places_vra, by = "plid") 

# place-level variables
pl12 <- read_csv("pl0017_interpolated.csv") %>% filter(Year == "2012")
table(pl_annex_var_1213$plid %in% pl12$plid) 

cdps13 <- read_csv("plids/pl2013.csv") %>% # want to know which places are CDPs--they do not annex
  select(Geo_NAME, plid) %>%
  mutate(cdp = ifelse(grepl("CDP|cdp", Geo_NAME), 1, 0)) %>%
  filter(cdp==1)

pl_annex_var_1213 %<>%
  filter(plid %in% pl12$plid & !(plid %in% cdps13$plid)) %>%
  left_join(pl12, by = "plid") %>%
  mutate(post = 0,
         time = "2012 to 2013",
         pctowneroccupied = (owneroccupied/hu)*100) 

table(pl_annex_var_1213$annexing)

pl_annex_var_1213 %<>%
  #filter(pop_total > 0) %>%
  mutate(annexing = ifelse(annexing == 1 & (pop_total_1 < 1 | is.na(pop_total_1) | pop_total < 1), 0, annexing))
table(pl_annex_var_1213$annexing)

sapply(pl_annex_var_1213, function(x) sum(is.na(x)))  

pl_annex_var_1213 %<>%
  mutate(
    underbound_black = ifelse(
      (annexing == 1 & pctnhblack_total_1 < pctnhblack), 1, 0), 
    underbound_nbmin = ifelse(
      (annexing == 1 & pctnbmin_total_1 < pctnbmin), 1, 0)
  )

pl_annex_var_1213 %<>%
  filter(pop > 0) 

pl_annex_var_1213 %<>%
  mutate(more_white = ifelse(pctnhwhite_total > pctnhwhite, 1, 0)) %>%
  filter_at(vars(pop, popdensity, pctnhblack_total, pctnbmin_total, more_white, pctowneroccupied, mhmval, hinc, ppov, pctblackpov, pctnbminpov, pctownerocc_total, pcthincjobs_total, pctincopp_total), ~!is.na(.))

p1213 <- unique(pl_annex_var_1213$plid)
pl_annex_vra_1213 <- unique(pl_annex_var_1213$plid[pl_annex_var_1213$vra==1])

rm(aa1213, place_all, place_by_annex, places_vra, cdps13, pl12)

# 2014-2015 ----
aa1415 <- read_csv("analyticalfiles/annexedblocks1415dem.csv") 
names(aa1415)

# transform this into place-level summaries
# characteristic of all annexable blocks
# diff between annexed and not annexed 
place_all <- aa1415 %>% 
  mutate(incopp = man + ret) %>%
  group_by(plid) %>%
  dplyr::summarize(pop_total = sum(pop, na.rm = T),
                   area_total = sum(area, na.rm = T),
                   nhblack_total = sum(nhblack, na.rm = T),
                   nhwhite_total = sum(nhwhite, na.rm = T),
                   nbmin_total = sum(nbmin, na.rm = T),
                   pct_annexed = mean(annexed, na.rm = T),
                   njobs_total = sum(njobs14, na.rm = T),
                   nhincjobs_total = sum(nhincjobs14, na.rm = T),
                   nwork_total = sum(jobs, na.rm = T),
                   incopp_total = sum(incopp, na.rm = T),
                   hu_total = sum(hu, na.rm = T),
                   owneroccupied_total = sum(owneroccupied, na.rm = T)
  ) %>%
  ungroup() %>%
  mutate(pctnhblack_total = ifelse(pop_total == 0, 0, (nhblack_total/pop_total)*100),
         pctnhwhite_total = ifelse(pop_total == 0, 0, (nhwhite_total/pop_total)*100),
         pctnbmin_total = ifelse(pop_total == 0, 0, (nbmin_total/pop_total)*100),
         pctownerocc_total = ifelse(hu_total == 0, 0, (owneroccupied_total/hu_total)*100),
         pctincopp_total = ifelse(nwork_total == 0, 0, (incopp_total/nwork_total)*100),
         pcthincjobs_total = ifelse(njobs_total == 0, 0, (nhincjobs_total/njobs_total)*100))

place_by_annex <- aa1415 %>%
  mutate(incopp = man + ret) %>%
  filter(annexed==1) %>%
  group_by(plid, annexed) %>%
  dplyr::summarize(pop_total = sum(pop, na.rm = T),
                   area_total = sum(area, na.rm = T),
                   nhblack_total = sum(nhblack, na.rm = T),
                   nhwhite_total = sum(nhwhite, na.rm = T),
                   nbmin_total = sum(nbmin, na.rm = T),
                   njobs_total = sum(njobs14, na.rm = T),
                   nhincjobs_total = sum(nhincjobs14, na.rm = T),
                   nwork_total = sum(jobs, na.rm = T),
                   incopp_total = sum(incopp, na.rm = T),
                   hu_total = sum(hu, na.rm = T),
                   owneroccupied_total = sum(owneroccupied, na.rm = T)) %>%
  ungroup() %>%
  pivot_wider(
    id_cols = plid,
    names_from = annexed,
    values_from = c(pop_total:owneroccupied_total)
  ) %>%
  mutate(pctnhblack_total_1 = ifelse(pop_total_1 == 0, 0, (nhblack_total_1/pop_total_1)*100),
         pctnhblack_total_1 = ifelse(pop_total_1 == 0, 0, (nhwhite_total_1/pop_total_1)*100),
         pctnbmin_total_1 = ifelse(pop_total_1 == 0, 0, (nbmin_total_1/pop_total_1)*100),
         pctincopp_total_1 = ifelse(nwork_total_1 == 0, 0, (incopp_total_1/nwork_total_1)*100),
         pcthincjobs_total_1 = ifelse(njobs_total_1 == 0, 0, (nhincjobs_total_1/njobs_total_1)*100),
         pctownerocc_total_1 = (owneroccupied_total_1/hu_total_1)*100) 

sapply(place_all, function(x) sum(is.na(x)))  

pl_annex_var_1415 <- left_join(
  place_all, place_by_annex, 
  by = "plid"
) %>%
  mutate(annexing = ifelse(!is.na(pop_total_1), 1, 0))
table(pl_annex_var_1415$annexing)
sapply(place_all, function(x) sum(is.na(x)))  

# add vra indicator 
places_vra <- aa1415 %>%
  group_by(plid) %>%
  dplyr::summarize(vra = mean(vra, na.rm = T)) %>%
  ungroup() %>%
  mutate(vra = ifelse(vra > 0, 1, 0))

pl_annex_var_1415 %<>%
  left_join(places_vra, by = "plid") 

# place-level variables
pl14 <- read_csv("pl0017_interpolated.csv") %>% filter(Year == "2014")
table(pl_annex_var_1415$plid %in% pl14$plid) 

cdps15 <- read_csv("plids/pl2015.csv") %>% # want to know which places are CDPs--they do not annex
  select(Geo_NAME, plid) %>%
  mutate(cdp = ifelse(grepl("CDP|cdp", Geo_NAME), 1, 0)) %>%
  filter(cdp==1)

pl_annex_var_1415 %<>%
  filter(plid %in% pl14$plid & !(plid %in% cdps15$plid)) %>%
  left_join(pl14, by = "plid") %>%
  mutate(post = 1,
         time = "2014 to 2015",
         pctowneroccupied = (owneroccupied/hu)*100) 

table(pl_annex_var_1415$annexing)

pl_annex_var_1415 %<>%
  #filter(pop_total > 0) %>%
  mutate(annexing = ifelse(annexing == 1 & (pop_total_1 < 1 | is.na(pop_total_1) | pop_total < 1), 0, annexing))
table(pl_annex_var_1415$annexing)

sapply(pl_annex_var_1415, function(x) sum(is.na(x)))  

pl_annex_var_1415 %<>%
  mutate(
    underbound_black = ifelse(
      (annexing == 1 & pctnhblack_total_1 < pctnhblack), 1, 0), 
    underbound_nbmin = ifelse(
      (annexing == 1 & pctnbmin_total_1 < pctnbmin), 1, 0)
  )

pl_annex_var_1415 %<>%
  filter(pop > 0) 

pl_annex_var_1415 %<>%
  mutate(more_white = ifelse(pctnhwhite_total > pctnhwhite, 1, 0)) %>%
  filter_at(vars(pop, popdensity, pctnhblack_total, pctnbmin_total, more_white, pctowneroccupied, mhmval, hinc, ppov, pctblackpov, pctnbminpov, pctownerocc_total, pcthincjobs_total, pctincopp_total), ~!is.na(.))

p1415 <- unique(pl_annex_var_1415$plid)
pl_annex_vra_1415 <- unique(pl_annex_var_1415$plid[pl_annex_var_1415$vra==1])

rm(aa1415, place_all, place_by_annex, places_vra, cdps15, pl14)

# 2015-2016 ----
aa1516 <- read_csv("analyticalfiles/annexedblocks1516dem.csv") 
names(aa1516)

# transform this into place-level summaries
# characteristic of all annexable blocks
# diff between annexed and not annexed 
place_all <- aa1516 %>% 
  mutate(incopp = man + ret) %>%
  group_by(plid) %>%
  dplyr::summarize(pop_total = sum(pop, na.rm = T),
                   area_total = sum(area, na.rm = T),
                   nhblack_total = sum(nhblack, na.rm = T),
                   nhwhite_total = sum(nhwhite, na.rm = T),
                   nbmin_total = sum(nbmin, na.rm = T),
                   pct_annexed = mean(annexed, na.rm = T),
                   njobs_total = sum(njobs15, na.rm = T),
                   nhincjobs_total = sum(nhincjobs15, na.rm = T),
                   nwork_total = sum(jobs, na.rm = T),
                   incopp_total = sum(incopp, na.rm = T),
                   hu_total = sum(hu, na.rm = T),
                   owneroccupied_total = sum(owneroccupied, na.rm = T)
  ) %>%
  ungroup() %>%
  mutate(pctnhblack_total = ifelse(pop_total == 0, 0, (nhblack_total/pop_total)*100),
         pctnhwhite_total = ifelse(pop_total == 0, 0, (nhwhite_total/pop_total)*100),
         pctnbmin_total = ifelse(pop_total == 0, 0, (nbmin_total/pop_total)*100),
         pctownerocc_total = ifelse(hu_total == 0, 0, (owneroccupied_total/hu_total)*100),
         pctincopp_total = ifelse(nwork_total == 0, 0, (incopp_total/nwork_total)*100),
         pcthincjobs_total = ifelse(njobs_total == 0, 0, (nhincjobs_total/njobs_total)*100))

place_by_annex <- aa1516 %>%
  mutate(incopp = man + ret) %>%
  filter(annexed==1) %>%
  group_by(plid, annexed) %>%
  dplyr::summarize(pop_total = sum(pop, na.rm = T),
                   area_total = sum(area, na.rm = T),
                   nhblack_total = sum(nhblack, na.rm = T),
                   nhwhite_total = sum(nhwhite, na.rm = T),
                   nbmin_total = sum(nbmin, na.rm = T),
                   njobs_total = sum(njobs15, na.rm = T),
                   nhincjobs_total = sum(nhincjobs15, na.rm = T),
                   nwork_total = sum(jobs, na.rm = T),
                   incopp_total = sum(incopp, na.rm = T),
                   hu_total = sum(hu, na.rm = T),
                   owneroccupied_total = sum(owneroccupied, na.rm = T)) %>%
  ungroup() %>%
  pivot_wider(
    id_cols = plid,
    names_from = annexed,
    values_from = c(pop_total:owneroccupied_total)
  ) %>%
  mutate(pctnhblack_total_1 = ifelse(pop_total_1 == 0, 0, (nhblack_total_1/pop_total_1)*100),
         pctnhblack_total_1 = ifelse(pop_total_1 == 0, 0, (nhwhite_total_1/pop_total_1)*100),
         pctnbmin_total_1 = ifelse(pop_total_1 == 0, 0, (nbmin_total_1/pop_total_1)*100),
         pctincopp_total_1 = ifelse(nwork_total_1 == 0, 0, (incopp_total_1/nwork_total_1)*100),
         pcthincjobs_total_1 = ifelse(njobs_total_1 == 0, 0, (nhincjobs_total_1/njobs_total_1)*100),
         pctownerocc_total_1 = (owneroccupied_total_1/hu_total_1)*100) 

sapply(place_all, function(x) sum(is.na(x)))  

pl_annex_var_1516 <- left_join(
  place_all, place_by_annex, 
  by = "plid"
) %>%
  mutate(annexing = ifelse(!is.na(pop_total_1), 1, 0))
table(pl_annex_var_1516$annexing)
sapply(place_all, function(x) sum(is.na(x)))  

# add vra indicator 
places_vra <- aa1516 %>%
  group_by(plid) %>%
  dplyr::summarize(vra = mean(vra, na.rm = T)) %>%
  ungroup() %>%
  mutate(vra = ifelse(vra > 0, 1, 0))

pl_annex_var_1516 %<>%
  left_join(places_vra, by = "plid") 

# place-level variables
pl15 <- read_csv("pl0017_interpolated.csv") %>% filter(Year == "2015")
table(pl_annex_var_1516$plid %in% pl15$plid) 

cdps16 <- read_csv("plids/pl2016.csv") %>% # want to know which places are CDPs--they do not annex
  select(Geo_NAME, plid) %>%
  mutate(cdp = ifelse(grepl("CDP|cdp", Geo_NAME), 1, 0)) %>%
  filter(cdp==1)

pl_annex_var_1516 %<>%
  filter(plid %in% pl15$plid & !(plid %in% cdps16$plid)) %>%
  left_join(pl15, by = "plid") %>%
  mutate(post = 1,
         time = "2015 to 2016",
         pctowneroccupied = (owneroccupied/hu)*100) 

table(pl_annex_var_1516$annexing)

pl_annex_var_1516 %<>%
  #filter(pop_total > 0) %>%
  mutate(annexing = ifelse(annexing == 1 & (pop_total_1 < 1 | is.na(pop_total_1) | pop_total < 1), 0, annexing))
table(pl_annex_var_1516$annexing)

sapply(pl_annex_var_1516, function(x) sum(is.na(x)))  

pl_annex_var_1516 %<>%
  mutate(
    underbound_black = ifelse(
      (annexing == 1 & pctnhblack_total_1 < pctnhblack), 1, 0), 
    underbound_nbmin = ifelse(
      (annexing == 1 & pctnbmin_total_1 < pctnbmin), 1, 0)
  )

pl_annex_var_1516 %<>%
  filter(pop > 0) 

pl_annex_var_1516 %<>%
  mutate(more_white = ifelse(pctnhwhite_total > pctnhwhite, 1, 0)) %>%
  filter_at(vars(pop, popdensity, pctnhblack_total, pctnbmin_total, more_white, pctowneroccupied, mhmval, hinc, ppov, pctblackpov, pctnbminpov, pctownerocc_total, pcthincjobs_total, pctincopp_total), ~!is.na(.))

p1516 <- unique(pl_annex_var_1516$plid)
pl_annex_vra_1516 <- unique(pl_annex_var_1516$plid[pl_annex_var_1516$vra==1])

rm(aa1516, place_all, place_by_annex, places_vra, cdps16, pl15)

# 2016-2017 ----
aa1617 <- read_csv("analyticalfiles/annexedblocks1617dem.csv") 
names(aa1617)

# transform this into place-level summaries
# characteristic of all annexable blocks
# diff between annexed and not annexed 
place_all <- aa1617 %>% 
  mutate(incopp = man + ret) %>%
  group_by(plid) %>%
  dplyr::summarize(pop_total = sum(pop, na.rm = T),
                   area_total = sum(area, na.rm = T),
                   nhblack_total = sum(nhblack, na.rm = T),
                   nhwhite_total = sum(nhwhite, na.rm = T),
                   nbmin_total = sum(nbmin, na.rm = T),
                   pct_annexed = mean(annexed, na.rm = T),
                   njobs_total = sum(njobs16, na.rm = T),
                   nhincjobs_total = sum(nhincjobs16, na.rm = T),
                   nwork_total = sum(jobs, na.rm = T),
                   incopp_total = sum(incopp, na.rm = T),
                   hu_total = sum(hu, na.rm = T),
                   owneroccupied_total = sum(owneroccupied, na.rm = T)
  ) %>%
  ungroup() %>%
  mutate(pctnhblack_total = ifelse(pop_total == 0, 0, (nhblack_total/pop_total)*100),
         pctnhwhite_total = ifelse(pop_total == 0, 0, (nhwhite_total/pop_total)*100),
         pctnbmin_total = ifelse(pop_total == 0, 0, (nbmin_total/pop_total)*100),
         pctownerocc_total = ifelse(hu_total == 0, 0, (owneroccupied_total/hu_total)*100),
         pctincopp_total = ifelse(nwork_total == 0, 0, (incopp_total/nwork_total)*100),
         pcthincjobs_total = ifelse(njobs_total == 0, 0, (nhincjobs_total/njobs_total)*100))

place_by_annex <- aa1617 %>%
  mutate(incopp = man + ret) %>%
  filter(annexed==1) %>%
  group_by(plid, annexed) %>%
  dplyr::summarize(pop_total = sum(pop, na.rm = T),
                   area_total = sum(area, na.rm = T),
                   nhblack_total = sum(nhblack, na.rm = T),
                   nhwhite_total = sum(nhwhite, na.rm = T),
                   nbmin_total = sum(nbmin, na.rm = T),
                   njobs_total = sum(njobs16, na.rm = T),
                   nhincjobs_total = sum(nhincjobs16, na.rm = T),
                   nwork_total = sum(jobs, na.rm = T),
                   incopp_total = sum(incopp, na.rm = T),
                   hu_total = sum(hu, na.rm = T),
                   owneroccupied_total = sum(owneroccupied, na.rm = T)) %>%
  ungroup() %>%
  pivot_wider(
    id_cols = plid,
    names_from = annexed,
    values_from = c(pop_total:owneroccupied_total)
  ) %>%
  mutate(pctnhblack_total_1 = ifelse(pop_total_1 == 0, 0, (nhblack_total_1/pop_total_1)*100),
         pctnhblack_total_1 = ifelse(pop_total_1 == 0, 0, (nhwhite_total_1/pop_total_1)*100),
         pctnbmin_total_1 = ifelse(pop_total_1 == 0, 0, (nbmin_total_1/pop_total_1)*100),
         pctincopp_total_1 = ifelse(nwork_total_1 == 0, 0, (incopp_total_1/nwork_total_1)*100),
         pcthincjobs_total_1 = ifelse(njobs_total_1 == 0, 0, (nhincjobs_total_1/njobs_total_1)*100),
         pctownerocc_total_1 = (owneroccupied_total_1/hu_total_1)*100) 

sapply(place_all, function(x) sum(is.na(x)))  

pl_annex_var_1617 <- left_join(
  place_all, place_by_annex, 
  by = "plid"
) %>%
  mutate(annexing = ifelse(!is.na(pop_total_1), 1, 0))
table(pl_annex_var_1617$annexing)
sapply(place_all, function(x) sum(is.na(x)))  

# add vra indicator 
places_vra <- aa1617 %>%
  group_by(plid) %>%
  dplyr::summarize(vra = mean(vra, na.rm = T)) %>%
  ungroup() %>%
  mutate(vra = ifelse(vra > 0, 1, 0))

pl_annex_var_1617 %<>%
  left_join(places_vra, by = "plid") 

# place-level variables
pl16 <- read_csv("pl0017_interpolated.csv") %>% filter(Year == "2016")
table(pl_annex_var_1617$plid %in% pl16$plid) 

cdps17 <- read_csv("plids/pl2017.csv") %>% # want to know which places are CDPs--they do not annex
  select(Geo_NAME, plid) %>%
  mutate(cdp = ifelse(grepl("CDP|cdp", Geo_NAME), 1, 0)) %>%
  filter(cdp==1)

pl_annex_var_1617 %<>%
  filter(plid %in% pl16$plid & !(plid %in% cdps17$plid)) %>%
  left_join(pl16, by = "plid") %>%
  mutate(post = 1,
         time = "2016 to 2017",
         pctowneroccupied = (owneroccupied/hu)*100) 

table(pl_annex_var_1617$annexing)

pl_annex_var_1617 %<>%
  #filter(pop_total > 0) %>%
  mutate(annexing = ifelse(annexing == 1 & (pop_total_1 < 1 | is.na(pop_total_1) | pop_total < 1), 0, annexing))
table(pl_annex_var_1617$annexing)

sapply(pl_annex_var_1617, function(x) sum(is.na(x)))  

pl_annex_var_1617 %<>%
  mutate(
    underbound_black = ifelse(
      (annexing == 1 & pctnhblack_total_1 < pctnhblack), 1, 0), 
    underbound_nbmin = ifelse(
      (annexing == 1 & pctnbmin_total_1 < pctnbmin), 1, 0)
  )

pl_annex_var_1617 %<>%
  filter(pop > 0) 

pl_annex_var_1617 %<>%
  mutate(more_white = ifelse(pctnhwhite_total > pctnhwhite, 1, 0)) %>%
  filter_at(vars(pop, popdensity, pctnhblack_total, pctnbmin_total, more_white, pctowneroccupied, mhmval, hinc, ppov, pctblackpov, pctnbminpov, pctownerocc_total, pcthincjobs_total, pctincopp_total), ~!is.na(.))

p1617 <- unique(pl_annex_var_1617$plid)
pl_annex_vra_1617 <- unique(pl_annex_var_1617$plid[pl_annex_var_1617$vra==1])

rm(aa1617, place_all, place_by_annex, places_vra, cdps17, pl16)

# 2017-2018 ----
aa1718 <- read_csv("analyticalfiles/annexedblocks1718dem.csv") 
names(aa1718)

# transform this into place-level summaries
# characteristic of all annexable blocks
# diff between annexed and not annexed 
place_all <- aa1718 %>% 
  mutate(incopp = man + ret) %>%
  group_by(plid) %>%
  dplyr::summarize(pop_total = sum(pop, na.rm = T),
                   area_total = sum(area, na.rm = T),
                   nhblack_total = sum(nhblack, na.rm = T),
                   nhwhite_total = sum(nhwhite, na.rm = T),
                   nbmin_total = sum(nbmin, na.rm = T),
                   pct_annexed = mean(annexed, na.rm = T),
                   njobs_total = sum(njobs17, na.rm = T),
                   nhincjobs_total = sum(nhincjobs17, na.rm = T),
                   nwork_total = sum(jobs, na.rm = T),
                   incopp_total = sum(incopp, na.rm = T),
                   hu_total = sum(hu, na.rm = T),
                   owneroccupied_total = sum(owneroccupied, na.rm = T)
  ) %>%
  ungroup() %>%
  mutate(pctnhblack_total = ifelse(pop_total == 0, 0, (nhblack_total/pop_total)*100),
         pctnhwhite_total = ifelse(pop_total == 0, 0, (nhwhite_total/pop_total)*100),
         pctnbmin_total = ifelse(pop_total == 0, 0, (nbmin_total/pop_total)*100),
         pctownerocc_total = ifelse(hu_total == 0, 0, (owneroccupied_total/hu_total)*100),
         pctincopp_total = ifelse(nwork_total == 0, 0, (incopp_total/nwork_total)*100),
         pcthincjobs_total = ifelse(njobs_total == 0, 0, (nhincjobs_total/njobs_total)*100))

place_by_annex <- aa1718 %>%
  mutate(incopp = man + ret) %>%
  filter(annexed==1) %>%
  group_by(plid, annexed) %>%
  dplyr::summarize(pop_total = sum(pop, na.rm = T),
                   area_total = sum(area, na.rm = T),
                   nhblack_total = sum(nhblack, na.rm = T),
                   nhwhite_total = sum(nhwhite, na.rm = T),
                   nbmin_total = sum(nbmin, na.rm = T),
                   njobs_total = sum(njobs17, na.rm = T),
                   nhincjobs_total = sum(nhincjobs17, na.rm = T),
                   nwork_total = sum(jobs, na.rm = T),
                   incopp_total = sum(incopp, na.rm = T),
                   hu_total = sum(hu, na.rm = T),
                   owneroccupied_total = sum(owneroccupied, na.rm = T)) %>%
  ungroup() %>%
  pivot_wider(
    id_cols = plid,
    names_from = annexed,
    values_from = c(pop_total:owneroccupied_total)
  ) %>%
  mutate(pctnhblack_total_1 = ifelse(pop_total_1 == 0, 0, (nhblack_total_1/pop_total_1)*100),
         pctnhblack_total_1 = ifelse(pop_total_1 == 0, 0, (nhwhite_total_1/pop_total_1)*100),
         pctnbmin_total_1 = ifelse(pop_total_1 == 0, 0, (nbmin_total_1/pop_total_1)*100),
         pctincopp_total_1 = ifelse(nwork_total_1 == 0, 0, (incopp_total_1/nwork_total_1)*100),
         pcthincjobs_total_1 = ifelse(njobs_total_1 == 0, 0, (nhincjobs_total_1/njobs_total_1)*100),
         pctownerocc_total_1 = (owneroccupied_total_1/hu_total_1)*100) 

sapply(place_all, function(x) sum(is.na(x)))  

pl_annex_var_1718 <- left_join(
  place_all, place_by_annex, 
  by = "plid"
) %>%
  mutate(annexing = ifelse(!is.na(pop_total_1), 1, 0))
table(pl_annex_var_1718$annexing)
sapply(place_all, function(x) sum(is.na(x)))  

# add vra indicator 
places_vra <- aa1718 %>%
  group_by(plid) %>%
  dplyr::summarize(vra = mean(vra, na.rm = T)) %>%
  ungroup() %>%
  mutate(vra = ifelse(vra > 0, 1, 0))

pl_annex_var_1718 %<>%
  left_join(places_vra, by = "plid") 

# place-level variables
pl17 <- read_csv("pl0017_interpolated.csv") %>% filter(Year == "2017")
table(pl_annex_var_1718$plid %in% pl17$plid) 

cdps18 <- read_csv("plids/pl2018.csv") %>% # want to know which places are CDPs--they do not annex
  select(Geo_NAME, plid) %>%
  mutate(cdp = ifelse(grepl("CDP|cdp", Geo_NAME), 1, 0)) %>%
  filter(cdp==1)

pl_annex_var_1718 %<>%
  filter(plid %in% pl17$plid & !(plid %in% cdps18$plid)) %>%
  left_join(pl17, by = "plid") %>%
  mutate(post = 1,
         time = "2017 to 2018",
         pctowneroccupied = (owneroccupied/hu)*100) 

table(pl_annex_var_1718$annexing)

pl_annex_var_1718 %<>%
  #filter(pop_total > 0) %>%
  mutate(annexing = ifelse(annexing == 1 & (pop_total_1 < 1 | is.na(pop_total_1) | pop_total < 1), 0, annexing))
table(pl_annex_var_1718$annexing)

sapply(pl_annex_var_1718, function(x) sum(is.na(x)))  

pl_annex_var_1718 %<>%
  mutate(
    underbound_black = ifelse(
      (annexing == 1 & pctnhblack_total_1 < pctnhblack), 1, 0), 
    underbound_nbmin = ifelse(
      (annexing == 1 & pctnbmin_total_1 < pctnbmin), 1, 0)
  )

pl_annex_var_1718 %<>%
  filter(pop > 0) 

pl_annex_var_1718 %<>%
  mutate(more_white = ifelse(pctnhwhite_total > pctnhwhite, 1, 0)) %>%
  filter_at(vars(pop, popdensity, pctnhblack_total, pctnbmin_total, more_white, pctowneroccupied, mhmval, hinc, ppov, pctblackpov, pctnbminpov, pctownerocc_total, pcthincjobs_total, pctincopp_total), ~!is.na(.))

p1718 <- unique(pl_annex_var_1718$plid)
pl_annex_vra_1718 <- unique(pl_annex_var_1718$plid[pl_annex_var_1718$vra==1])

rm(aa1718, place_all, place_by_annex, places_vra, cdps18, pl17)

# 2018-2019 ----
aa1819 <- read_csv("analyticalfiles/annexedblocks1819dem.csv") 
names(aa1819)

# transform this into place-level summaries
# characteristic of all annexable blocks
# diff between annexed and not annexed 
place_all <- aa1819 %>% 
  mutate(incopp = man + ret) %>%
  group_by(plid) %>%
  dplyr::summarize(pop_total = sum(pop, na.rm = T),
                   area_total = sum(area, na.rm = T),
                   nhblack_total = sum(nhblack, na.rm = T),
                   nhwhite_total = sum(nhwhite, na.rm = T),
                   nbmin_total = sum(nbmin, na.rm = T),
                   pct_annexed = mean(annexed, na.rm = T),
                   njobs_total = sum(njobs18, na.rm = T),
                   nhincjobs_total = sum(nhincjobs18, na.rm = T),
                   nwork_total = sum(jobs, na.rm = T),
                   incopp_total = sum(incopp, na.rm = T),
                   hu_total = sum(hu, na.rm = T),
                   owneroccupied_total = sum(owneroccupied, na.rm = T)
  ) %>%
  ungroup() %>%
  mutate(pctnhblack_total = ifelse(pop_total == 0, 0, (nhblack_total/pop_total)*100),
         pctnhwhite_total = ifelse(pop_total == 0, 0, (nhwhite_total/pop_total)*100),
         pctnbmin_total = ifelse(pop_total == 0, 0, (nbmin_total/pop_total)*100),
         pctownerocc_total = ifelse(hu_total == 0, 0, (owneroccupied_total/hu_total)*100),
         pctincopp_total = ifelse(nwork_total == 0, 0, (incopp_total/nwork_total)*100),
         pcthincjobs_total = ifelse(njobs_total == 0, 0, (nhincjobs_total/njobs_total)*100))

place_by_annex <- aa1819 %>%
  mutate(incopp = man + ret) %>%
  filter(annexed==1) %>%
  group_by(plid, annexed) %>%
  dplyr::summarize(pop_total = sum(pop, na.rm = T),
                   area_total = sum(area, na.rm = T),
                   nhblack_total = sum(nhblack, na.rm = T),
                   nhwhite_total = sum(nhwhite, na.rm = T),
                   nbmin_total = sum(nbmin, na.rm = T),
                   njobs_total = sum(njobs18, na.rm = T),
                   nhincjobs_total = sum(nhincjobs18, na.rm = T),
                   nwork_total = sum(jobs, na.rm = T),
                   incopp_total = sum(incopp, na.rm = T),
                   hu_total = sum(hu, na.rm = T),
                   owneroccupied_total = sum(owneroccupied, na.rm = T)) %>%
  ungroup() %>%
  pivot_wider(
    id_cols = plid,
    names_from = annexed,
    values_from = c(pop_total:owneroccupied_total)
  ) %>%
  mutate(pctnhblack_total_1 = ifelse(pop_total_1 == 0, 0, (nhblack_total_1/pop_total_1)*100),
         pctnhblack_total_1 = ifelse(pop_total_1 == 0, 0, (nhwhite_total_1/pop_total_1)*100),
         pctnbmin_total_1 = ifelse(pop_total_1 == 0, 0, (nbmin_total_1/pop_total_1)*100),
         pctincopp_total_1 = ifelse(nwork_total_1 == 0, 0, (incopp_total_1/nwork_total_1)*100),
         pcthincjobs_total_1 = ifelse(njobs_total_1 == 0, 0, (nhincjobs_total_1/njobs_total_1)*100),
         pctownerocc_total_1 = (owneroccupied_total_1/hu_total_1)*100) 

sapply(place_all, function(x) sum(is.na(x)))  

pl_annex_var_1819 <- left_join(
  place_all, place_by_annex, 
  by = "plid"
) %>%
  mutate(annexing = ifelse(!is.na(pop_total_1), 1, 0))
table(pl_annex_var_1819$annexing)
sapply(place_all, function(x) sum(is.na(x)))  

# add vra indicator 
places_vra <- aa1819 %>%
  group_by(plid) %>%
  dplyr::summarize(vra = mean(vra, na.rm = T)) %>%
  ungroup() %>%
  mutate(vra = ifelse(vra > 0, 1, 0))

pl_annex_var_1819 %<>%
  left_join(places_vra, by = "plid") 

# place-level variables
pl18 <- read_csv("pl0017_interpolated.csv") %>% filter(Year == "2018")
table(pl_annex_var_1819$plid %in% pl18$plid) 

cdps19 <- read_csv("plids/pl2019.csv") %>% # want to know which places are CDPs--they do not annex
  select(Geo_NAME, plid) %>%
  mutate(cdp = ifelse(grepl("CDP|cdp", Geo_NAME), 1, 0)) %>%
  filter(cdp==1)

pl_annex_var_1819 %<>%
  filter(plid %in% pl18$plid & !(plid %in% cdps19$plid)) %>%
  left_join(pl18, by = "plid") %>%
  mutate(post = 1,
         time = "2018 to 2019",
         pctowneroccupied = (owneroccupied/hu)*100) 

table(pl_annex_var_1819$annexing)

pl_annex_var_1819 %<>%
  #filter(pop_total > 0) %>%
  mutate(annexing = ifelse(annexing == 1 & (pop_total_1 < 1 | is.na(pop_total_1) | pop_total < 1), 0, annexing))
table(pl_annex_var_1819$annexing)

sapply(pl_annex_var_1819, function(x) sum(is.na(x)))  

pl_annex_var_1819 %<>%
  mutate(
    underbound_black = ifelse(
      (annexing == 1 & pctnhblack_total_1 < pctnhblack), 1, 0), 
    underbound_nbmin = ifelse(
      (annexing == 1 & pctnbmin_total_1 < pctnbmin), 1, 0)
  )

pl_annex_var_1819 %<>%
  filter(pop > 0) 

pl_annex_var_1819 %<>%
  mutate(more_white = ifelse(pctnhwhite_total > pctnhwhite, 1, 0)) %>%
  filter_at(vars(pop, popdensity, pctnhblack_total, pctnbmin_total, more_white, pctowneroccupied, mhmval, hinc, ppov, pctblackpov, pctnbminpov, pctownerocc_total, pcthincjobs_total, pctincopp_total), ~!is.na(.))

p1819 <- unique(pl_annex_var_1819$plid)
pl_annex_vra_1819 <- unique(pl_annex_var_1819$plid[pl_annex_var_1819$vra==1])

rm(aa1819, place_all, place_by_annex, places_vra, cdps19, pl18)

# 2019-2020 ----
aa1920 <- read_csv("analyticalfiles/annexedblocks1920dem.csv") 
names(aa1920)

# transform this into place-level summaries
# characteristic of all annexable blocks
# diff between annexed and not annexed 
place_all <- aa1920 %>% 
  mutate(incopp = man + ret) %>%
  group_by(plid) %>%
  dplyr::summarize(pop_total = sum(pop, na.rm = T),
                   area_total = sum(area, na.rm = T),
                   nhblack_total = sum(nhblack, na.rm = T),
                   nhwhite_total = sum(nhwhite, na.rm = T),
                   nbmin_total = sum(nbmin, na.rm = T),
                   pct_annexed = mean(annexed, na.rm = T),
                   njobs_total = sum(njobs19, na.rm = T),
                   nhincjobs_total = sum(nhincjobs19, na.rm = T),
                   nwork_total = sum(jobs, na.rm = T),
                   incopp_total = sum(incopp, na.rm = T),
                   hu_total = sum(hu, na.rm = T),
                   owneroccupied_total = sum(owneroccupied, na.rm = T)
  ) %>%
  ungroup() %>%
  mutate(pctnhblack_total = ifelse(pop_total == 0, 0, (nhblack_total/pop_total)*100),
         pctnhwhite_total = ifelse(pop_total == 0, 0, (nhwhite_total/pop_total)*100),
         pctnbmin_total = ifelse(pop_total == 0, 0, (nbmin_total/pop_total)*100),
         pctownerocc_total = ifelse(hu_total == 0, 0, (owneroccupied_total/hu_total)*100),
         pctincopp_total = ifelse(nwork_total == 0, 0, (incopp_total/nwork_total)*100),
         pcthincjobs_total = ifelse(njobs_total == 0, 0, (nhincjobs_total/njobs_total)*100))

place_by_annex <- aa1920 %>%
  mutate(incopp = man + ret) %>%
  filter(annexed==1) %>%
  group_by(plid, annexed) %>%
  dplyr::summarize(pop_total = sum(pop, na.rm = T),
                   area_total = sum(area, na.rm = T),
                   nhblack_total = sum(nhblack, na.rm = T),
                   nhwhite_total = sum(nhwhite, na.rm = T),
                   nbmin_total = sum(nbmin, na.rm = T),
                   njobs_total = sum(njobs19, na.rm = T),
                   nhincjobs_total = sum(nhincjobs19, na.rm = T),
                   nwork_total = sum(jobs, na.rm = T),
                   incopp_total = sum(incopp, na.rm = T),
                   hu_total = sum(hu, na.rm = T),
                   owneroccupied_total = sum(owneroccupied, na.rm = T)) %>%
  ungroup() %>%
  pivot_wider(
    id_cols = plid,
    names_from = annexed,
    values_from = c(pop_total:owneroccupied_total)
  ) %>%
  mutate(pctnhblack_total_1 = ifelse(pop_total_1 == 0, 0, (nhblack_total_1/pop_total_1)*100),
         pctnhblack_total_1 = ifelse(pop_total_1 == 0, 0, (nhwhite_total_1/pop_total_1)*100),
         pctnbmin_total_1 = ifelse(pop_total_1 == 0, 0, (nbmin_total_1/pop_total_1)*100),
         pctincopp_total_1 = ifelse(nwork_total_1 == 0, 0, (incopp_total_1/nwork_total_1)*100),
         pcthincjobs_total_1 = ifelse(njobs_total_1 == 0, 0, (nhincjobs_total_1/njobs_total_1)*100),
         pctownerocc_total_1 = (owneroccupied_total_1/hu_total_1)*100) 

sapply(place_all, function(x) sum(is.na(x)))  

pl_annex_var_1920 <- left_join(
  place_all, place_by_annex, 
  by = "plid"
) %>%
  mutate(annexing = ifelse(!is.na(pop_total_1), 1, 0))
table(pl_annex_var_1920$annexing)
sapply(place_all, function(x) sum(is.na(x)))  

# add vra indicator 
places_vra <- aa1920 %>%
  group_by(plid) %>%
  dplyr::summarize(vra = mean(vra, na.rm = T)) %>%
  ungroup() %>%
  mutate(vra = ifelse(vra > 0, 1, 0))

pl_annex_var_1920 %<>%
  left_join(places_vra, by = "plid") 

# place-level variables
pl19 <- read_csv("pl0017_interpolated.csv") %>% filter(Year == "2019")
table(pl_annex_var_1920$plid %in% pl19$plid) 

cdps20 <- read_csv("plids/pl2020.csv") %>% # want to know which places are CDPs--they do not annex
  select(Geo_NAME, plid) %>%
  mutate(cdp = ifelse(grepl("CDP|cdp", Geo_NAME), 1, 0)) %>%
  filter(cdp==1)

pl_annex_var_1920 %<>%
  filter(plid %in% pl19$plid & !(plid %in% cdps20$plid)) %>%
  left_join(pl19, by = "plid") %>%
  mutate(post = 1,
         time = "2019 to 2020",
         pctowneroccupied = (owneroccupied/hu)*100) 

table(pl_annex_var_1920$annexing)

pl_annex_var_1920 %<>%
  #filter(pop_total > 0) %>%
  mutate(annexing = ifelse(annexing == 1 & (pop_total_1 < 1 | is.na(pop_total_1) | pop_total < 1), 0, annexing))
table(pl_annex_var_1920$annexing)

sapply(pl_annex_var_1920, function(x) sum(is.na(x)))  

pl_annex_var_1920 %<>%
  mutate(
    underbound_black = ifelse(
      (annexing == 1 & pctnhblack_total_1 < pctnhblack), 1, 0), 
    underbound_nbmin = ifelse(
      (annexing == 1 & pctnbmin_total_1 < pctnbmin), 1, 0)
  )

pl_annex_var_1920 %<>%
  filter(pop > 0) 

pl_annex_var_1920 %<>%
  mutate(more_white = ifelse(pctnhwhite_total > pctnhwhite, 1, 0)) %>%
  filter_at(vars(pop, popdensity, pctnhblack_total, pctnbmin_total, more_white, pctowneroccupied, mhmval, hinc, ppov, pctblackpov, pctnbminpov, pctownerocc_total, pcthincjobs_total, pctincopp_total), ~!is.na(.))

p1920 <- unique(pl_annex_var_1920$plid)
pl_annex_vra_1920 <- unique(pl_annex_var_1920$plid[pl_annex_var_1920$vra==1])

rm(aa1920, place_all, place_by_annex, places_vra, cdps20, pl19)

# unique IDS ----
# unique plids common across time 
plids <- Reduce(intersect, list(unique(p0708), unique(p0809), unique(p0910), unique(p1011), unique(p1112), unique(p1213), unique(p1415), unique(p1516), unique(p1617), unique(p1718), unique(p1819), unique(p1920)))

# unique vra plids
vraplids <- unique(c(pl_annex_vra_0708, pl_annex_vra_0809, pl_annex_vra_0910, pl_annex_vra_1011, pl_annex_vra_1112, pl_annex_vra_1213, pl_annex_vra_1415, pl_annex_vra_1516, pl_annex_vra_1617, pl_annex_vra_1718, pl_annex_vra_1819, pl_annex_vra_1920))

# make panel data ----
names <- Reduce(intersect, list(names(pl_annex_var_0708), names(pl_annex_var_0809), names(pl_annex_var_0910), names(pl_annex_var_1011), names(pl_annex_var_1112), names(pl_annex_var_1213), names(pl_annex_var_1415), names(pl_annex_var_1516), names(pl_annex_var_1617), names(pl_annex_var_1718), names(pl_annex_var_1819), names(pl_annex_var_1920)))

pl_annex_var_0708 %<>%
  select(names) %>%
  filter(plid %in% plids)
pl_annex_var_0809 %<>%
  select(names) %>%
  filter(plid %in% plids)
pl_annex_var_0910 %<>%
  select(names) %>%
  filter(plid %in% plids)
pl_annex_var_1011 %<>%
  select(names) %>%
  filter(plid %in% plids)
pl_annex_var_1112 %<>%
  select(names) %>%
  filter(plid %in% plids)
pl_annex_var_1213 %<>%
  select(names) %>%
  filter(plid %in% plids)
pl_annex_var_1415 %<>%
  select(names) %>%
  filter(plid %in% plids)
pl_annex_var_1516 %<>%
  select(names) %>%
  filter(plid %in% plids)
pl_annex_var_1617 %<>%
  select(names) %>%
  filter(plid %in% plids)
pl_annex_var_1718 %<>%
  select(names) %>%
  filter(plid %in% plids)
pl_annex_var_1819 %<>%
  select(names) %>%
  filter(plid %in% plids)
pl_annex_var_1920 %<>%
  select(names) %>%
  filter(plid %in% plids)

panel_annual <- rbind(pl_annex_var_0708, pl_annex_var_0809, pl_annex_var_0910, pl_annex_var_1011, pl_annex_var_1112, pl_annex_var_1213, pl_annex_var_1415, pl_annex_var_1516, pl_annex_var_1617, pl_annex_var_1718, pl_annex_var_1819, pl_annex_var_1920)

table(panel_annual$vra, exclude = NULL)
length(unique(panel_annual$plid))

panel_annual %<>%
  mutate(vra = ifelse(plid %in% vraplids, 1, 0)) 

table(panel_annual$vra, exclude = NULL)

save.image("annual_annexations.RData")

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

# can you check it so that when it aggregates it still tracks with the prior panel form? ----
agg <- panel_annual %>%
  filter(!time %in% "2013 to 2014") %>% 
  mutate(post = ifelse(time %in% c("2007 to 2008", "2008 to 2009", "2009 to 2010", "2010 to 2011", "2011 to 2012", "2012 to 2013"), "2007 to 2013", "2014 to 2020")) %>%
  group_by(plid, post) %>%
  dplyr::summarize(annex = mean(annexing),
            vra = mean(vra)) %>%
  ungroup() %>%
  mutate(annex = ifelse(annex > 0, 1, 0))

agg
table(agg$vra)
table(agg$annex)

agg %<>%
  group_by(vra, post) %>%
  dplyr::summarize(annex = round(mean(annex)*100, 0)) %>%
  mutate(vra = as.character(vra))

agg_xt <- ggplot(agg, aes(x = post, y = annex, group = vra)) + 
  geom_line(aes(linetype = vra)) + geom_point() + 
  ylim(c(8, 24)) +
  theme(axis.text.x = element_text(angle = 50, hjust = 1, size = 10),
        axis.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12)) + 
  labs(linetype = "Covered by VRA") +
  xlab("Annual Period") +
  ylab("% of Places that Annexed")

agg_xt

ggsave(filename = paste0(savedir, "annual_xt_agg_viz.pdf"),
       plot = annual_xt,
       dpi = 300)

# this looks good/consistent with the paper 

# event study -- need to do this with annual periods ####
modsa <- feols(annexing ~ i(time, ref = "2014 to 2015"),
               cluster = "plid",
               data = panel_annual)

iplot(modsa, drop = "(Intercept)", xlab = "Time")

