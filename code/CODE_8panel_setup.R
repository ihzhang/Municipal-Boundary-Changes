# get environment ready 
setwd("~/Google Drive/My Drive/Stanford/QE2")

library("dplyr")
library("stargazer")
library("fixest")
library("readr")
library("stringr")
library("data.table")
library("magrittr")
library("openxlsx")
library("broom")
library("sjPlot")
library("ipw")
library("ggplot2")
library("tidyr")

rm(list = ls())
curdir <- paste0(dirname(rstudioapi::getSourceEditorContext()$path))
savedir <- paste0(curdir, "/../results/")

# make DiD panel ####
# 1. get plid for annexations to 2007-2013 and 2014-2020
# and their avg % annexed, avg % between annexed and non-annexed 
# 2. for the did panel, make outcome of annexed-annexable at 2007 and 2014 

# outcome var = 
# 1/0 to make a racially selective annexation 
# racially selective = not annexing the block because of racial considerations 
# this is nebulous to define, so create a few thresholds 
# post-annexation results in any pct VAP decrease 
# 0.5% 
# 1% 
# 3% according to VRAA

# 2007-2013 ####
aa0713 <- read_csv("analyticalfiles/annexedblocks0713dem.csv") 
names(aa0713)

vap_block <- read_csv("data_repo/dem_data_clean/blocks/blocks2007_int.csv") %>%
  select(blkid, contains("vap")) 

# transform this into place-level summaries
# characteristic of all annexable blocks
# diff between annexed and not annexed 
place_all <- aa0713 %>% 
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

place_by_annex <- aa0713 %>%
  #left_join(vap_block, by = "blkid") %>%
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
                   vap_total = sum(vap, na.rm = T),
                   nhbvap_total = sum(nhbvap, na.rm = T),
                   hispvap_total = sum(hispvap, na.rm = T),
                   nativevap_total = sum(nativevap, na.rm = T),
                   asianvap_total = sum(asianvap, na.rm = T),
                   othervap_total = sum(othervap, na.rm = T),
                   owneroccupied_total = sum(owneroccupied, na.rm = T)) %>%
  ungroup() %>%
  pivot_wider(
    id_cols = plid,
    names_from = annexed,
    values_from = c(pop_total:owneroccupied_total)
  ) %>%
  mutate(pctnhblack_total_1 = ifelse(pop_total_1 == 0, 0, (nhblack_total_1/pop_total_1)*100),
         pctnhblack_total_1 = ifelse(pop_total_1 == 0, 0, (nhwhite_total_1/pop_total_1)*100),
         pctnhbvap_total_1 = ifelse(vap_total_1 == 0, 0, (nhbvap_total_1/vap_total_1)*100),
         pcthispvap_total_1 = ifelse(vap_total_1 == 0, 0, (hispvap_total_1/vap_total_1)*100),
         pctnativevap_total_1 = ifelse(vap_total_1 == 0, 0, (nativevap_total_1/vap_total_1)*100),
         pctasianvap_total_1 = ifelse(vap_total_1 == 0, 0, (asianvap_total_1/vap_total_1)*100),
         pctothervap_total_1 = ifelse(vap_total_1 == 0, 0, (othervap_total_1/vap_total_1)*100),
         pctnbmin_total_1 = ifelse(pop_total_1 == 0, 0, (nbmin_total_1/pop_total_1)*100),
         pctincopp_total_1 = ifelse(nwork_total_1 == 0, 0, (incopp_total_1/nwork_total_1)*100),
         pcthincjobs_total_1 = ifelse(njobs_total_1 == 0, 0, (nhincjobs_total_1/njobs_total_1)*100),
         pctownerocc_total_1 = (owneroccupied_total_1/hu_total_1)*100) 

sapply(place_all, function(x) sum(is.na(x)))  

pl_annex_var_0713 <- left_join(
  place_all, place_by_annex, 
  by = "plid"
) %>%
  mutate(annexing = ifelse(!is.na(pop_total_1), 1, 0))
table(pl_annex_var_0713$annexing)
sapply(place_all, function(x) sum(is.na(x)))  

# add vra indicator 
places_vra <- aa0713 %>%
  group_by(plid) %>%
  dplyr::summarize(vra = mean(vra, na.rm = T)) %>%
  ungroup() %>%
  mutate(vra = ifelse(vra > 0, 1, 0))

pl_annex_var_0713 %<>%
  left_join(places_vra, by = "plid") 

# place-level variables
pl07 <- read_csv("data_repo/dem_data_clean/places/annual/pl0017_interpolated.csv") %>% filter(Year == "2007")
table(pl_annex_var_0713$plid %in% pl07$plid) 

cdps13 <- read_csv("plids/pl2013.csv") %>% # want to know which places are CDPs--they do not annex
  select(Geo_NAME, plid) %>%
  mutate(cdp = ifelse(grepl("CDP|cdp", Geo_NAME), 1, 0)) %>%
  filter(cdp==1)

pl_annex_var_0713 %<>%
  filter(plid %in% pl07$plid & !(plid %in% cdps13$plid)) %>%
  left_join(pl07, by = "plid") %>%
  mutate(post = 0,
         time = "2007 to 2013",
         pctowneroccupied = (owneroccupied/hu)*100) 

table(pl_annex_var_0713$annexing)

pl_annex_var_0713 %<>%
  #filter(pop_total > 0) %>%
  mutate(annexing = ifelse(annexing == 1 & (pop_total_1 < 1 | is.na(pop_total_1) | pop_total < 1), 0, annexing))
table(pl_annex_var_0713$annexing)

sapply(pl_annex_var_0713, function(x) sum(is.na(x)))  

# get VAP data 
vap <- read_csv("data_repo/dem_data_clean/places/pl0713_var.csv") %>%
  select(plid, contains("vap07p")) %>%
  select(plid, starts_with("pct"))

names(vap) <- gsub("07p", "", names(vap))

pl_annex_var_0713 %<>%
  left_join(vap, by = "plid") %>%
  mutate(
    vraa = ifelse((pctnhblackvap >= 20 & pctasianvap >= 20 | pctnhblackvap >= 20 & pctnativevap >= 20 | pctnhblackvap >= 20 & pcthispvap >= 20 | pctnhblackvap >= 20 & pctnativevap >= 20 | pctnhblackvap >= 20 & pctothervap >= 20 | pctasianvap >= 20 & pctnativevap >= 20 | pctasianvap >= 20 & pcthispvap >= 20 | pctasianvap >= 20 & pctothervap >= 20 | pctnativevap >= 20 & pcthispvap >= 20 | pctnativevap >= 20 & pctothervap >= 20), 1, 0),
    underbound_black_vraa = ifelse(vraa == 1 & annexing == 1 & ((pctnhbvap_total_1 - pctnhblackvap) < -0.03), 1, 0),
    underbound_black = ifelse(
      (annexing == 1 & pctnhblack_total_1 < pctnhblack), 1, 0), 
    underbound_nbmin = ifelse(
      (annexing == 1 & pctnbmin_total_1 < pctnbmin), 1, 0)
  )

pl_annex_var_0713 %<>%
  filter(pop > 0) 

pl_annex_var_0713 %<>%
  mutate(more_white = ifelse(pctnhwhite_total > pctnhwhite, 1, 0)) %>%
  filter_at(vars(pop, popdensity, pctnhblack_total, pctnbmin_total, more_white, pctowneroccupied, mhmval, hinc, ppov, pctblackpov, pctnbminpov, pctownerocc_total, pcthincjobs_total, pctincopp_total), ~!is.na(.))

p0713 <- unique(pl_annex_var_0713$plid)
pl_annex_vra_0713 <- unique(pl_annex_var_0713$plid[pl_annex_var_0713$vra==1])

rm(aa0713, place_all, place_by_annex, places_vra, cdps13, pl07)

#repeat for 1420 ####
aa1420 <- read_csv("analyticalfiles/annexedblocks1420dem.csv") 
names(aa1420)

# transform this into place-level summaries
# characteristic of all annexable blocks
# diff between annexed and not annexed 
place_all <- aa1420 %>% 
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

place_by_annex <- aa1420 %>%
  mutate(incopp = man + ret) %>%
  filter(annexed==1) %>%
  group_by(plid, annexed) %>%
  dplyr::summarize(pop_total = sum(pop, na.rm = T),
                   area_total = sum(area, na.rm = T),
                   nhblack_total = sum(nhblack, na.rm = T),
                   nhwhite_total = sum(nhwhite, na.rm = T),
                   nbmin_total = sum(nbmin, na.rm = T),
                   vap_total = sum(vap, na.rm = T),
                   nhbvap_total = sum(nhbvap, na.rm = T),
                   hispvap_total = sum(hispvap, na.rm = T),
                   nativevap_total = sum(nativevap, na.rm = T),
                   asianvap_total = sum(asianvap, na.rm = T),
                   othervap_total = sum(othervap, na.rm = T),
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
         pctnhbvap_total_1 = ifelse(vap_total_1 == 0, 0, (nhbvap_total_1/vap_total_1)*100),
         pcthispvap_total_1 = ifelse(vap_total_1 == 0, 0, (hispvap_total_1/vap_total_1)*100),
         pctnativevap_total_1 = ifelse(vap_total_1 == 0, 0, (nativevap_total_1/vap_total_1)*100),
         pctasianvap_total_1 = ifelse(vap_total_1 == 0, 0, (asianvap_total_1/vap_total_1)*100),
         pctothervap_total_1 = ifelse(vap_total_1 == 0, 0, (othervap_total_1/vap_total_1)*100),
         pctincopp_total_1 = ifelse(nwork_total_1 == 0, 0, (incopp_total_1/nwork_total_1)*100),
         pcthincjobs_total_1 = ifelse(njobs_total_1 == 0, 0, (nhincjobs_total_1/njobs_total_1)*100),
         pctownerocc_total_1 = (owneroccupied_total_1/hu_total_1)*100) 

sapply(place_all, function(x) sum(is.na(x)))  

pl_annex_var_1420 <- left_join(
  place_all, place_by_annex, 
  by = "plid"
) %>%
  mutate(annexing = ifelse(!is.na(pop_total_1), 1, 0))
table(pl_annex_var_1420$annexing)
sapply(place_all, function(x) sum(is.na(x)))  

# add vra indicator 
places_vra <- aa1420 %>%
  group_by(plid) %>%
  dplyr::summarize(vra = mean(vra, na.rm = T)) %>%
  ungroup() %>%
  mutate(vra = ifelse(vra > 0, 1, 0))

pl_annex_var_1420 %<>%
  left_join(places_vra, by = "plid") 

# place-level variables
pl14 <- read_csv("data_repo/dem_data_clean/places/annual/pl0017_interpolated.csv") %>% filter(Year == "2014")
table(pl_annex_var_1420$plid %in% pl14$plid) 

cdps20 <- read_csv("plids/pl2020.csv") %>% # want to know which places are CDPs--they do not annex
  select(Geo_NAME, plid) %>%
  mutate(cdp = ifelse(grepl("CDP|cdp", Geo_NAME), 1, 0)) %>%
  filter(cdp==1)

pl_annex_var_1420 %<>%
  filter(plid %in% pl14$plid & !(plid %in% cdps20$plid)) %>%
  left_join(pl14, by = "plid") %>%
  mutate(post = 0,
         time = "2014 to 2020",
         pctowneroccupied = (owneroccupied/hu)*100) 

table(pl_annex_var_1420$annexing)

pl_annex_var_1420 %<>%
  #filter(pop_total > 0) %>%
  mutate(annexing = ifelse(annexing == 1 & (pop_total_1 < 1 | is.na(pop_total_1) | pop_total < 1), 0, annexing))
table(pl_annex_var_1420$annexing)

sapply(pl_annex_var_1420, function(x) sum(is.na(x)))  

vap <- read_csv("data_repo/dem_data_clean/places/pl0714_var.csv") %>%
  select(plid, contains("vap14p")) %>%
  select(plid, starts_with("pct"))

names(vap) <- gsub("14p", "", names(vap))

pl_annex_var_1420 %<>%
  left_join(vap, by = "plid") %>%
  mutate(
    vraa = ifelse((pctnhblackvap >= 20 & pctasianvap >= 20 | pctnhblackvap >= 20 & pctnativevap >= 20 | pctnhblackvap >= 20 & pcthispvap >= 20 | pctnhblackvap >= 20 & pctnativevap >= 20 | pctnhblackvap >= 20 & pctothervap >= 20 | pctasianvap >= 20 & pctnativevap >= 20 | pctasianvap >= 20 & pcthispvap >= 20 | pctasianvap >= 20 & pctothervap >= 20 | pctnativevap >= 20 & pcthispvap >= 20 | pctnativevap >= 20 & pctothervap >= 20), 1, 0),
    underbound_black_vraa = ifelse(vraa == 1 & annexing == 1 & ((pctnhbvap_total_1 - pctnhblackvap) < -0.03), 1, 0),
    underbound_black = ifelse(
      (annexing == 1 & pctnhblack_total_1 < pctnhblack), 1, 0), 
    underbound_nbmin = ifelse(
      (annexing == 1 & pctnbmin_total_1 < pctnbmin), 1, 0)
  )

pl_annex_var_1420 %<>%
  filter(pop > 0) 

pl_annex_var_1420 %<>%
  mutate(more_white = ifelse(pctnhwhite_total > pctnhwhite, 1, 0)) %>%
  filter_at(vars(pop, popdensity, pctnhblack_total, pctnbmin_total, more_white, pctowneroccupied, mhmval, hinc, ppov, pctblackpov, pctnbminpov, pctownerocc_total, pcthincjobs_total, pctincopp_total), ~!is.na(.))

p1420 <- unique(pl_annex_var_1420$plid)
pl_annex_vra_1420 <- unique(pl_annex_var_1420$plid[pl_annex_var_1420$vra==1])

rm(aa1420, place_all, place_by_annex, places_vra, cdps20, pl14)

# repeat for 0007 ####
aa0007 <- read_csv("analyticalfiles/annexedblocks0007dem.csv") 
names(aa0007)

names(aa0007) <- gsub("00b", "", names(aa0007))

# transform this into place-level summaries
# characteristic of all annexable blocks
# diff between annexed and not annexed 
place_all <- aa0007 %>% 
  mutate(incopp = man + ret) %>%
  group_by(plid) %>%
  dplyr::summarize(pop_total = sum(pop, na.rm = T),
                   area_total = sum(area, na.rm = T),
                   nhblack_total = sum(nhblack, na.rm = T),
                   nhwhite_total = sum(nhwhite, na.rm = T),
                   nbmin_total = sum(nbmin, na.rm = T),
                   pct_annexed = mean(annexed, na.rm = T),
                   njobs_total = sum(njobs00, na.rm = T),
                   nhincjobs_total = sum(nhincjobs00, na.rm = T),
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

place_by_annex <- aa0007 %>%
  mutate(incopp = man + ret) %>%
  filter(annexed==1) %>%
  group_by(plid, annexed) %>%
  dplyr::summarize(pop_total = sum(pop, na.rm = T),
                   area_total = sum(area, na.rm = T),
                   nhblack_total = sum(nhblack, na.rm = T),
                   nhwhite_total = sum(nhwhite, na.rm = T),
                   nbmin_total = sum(nbmin, na.rm = T),
                   vap_total = sum(vap, na.rm = T),
                   nhbvap_total = sum(nhbvap, na.rm = T),
                   hispvap_total = sum(hispvap, na.rm = T),
                   nativevap_total = sum(nativevap, na.rm = T),
                   asianvap_total = sum(asianvap, na.rm = T),
                   othervap_total = sum(othervap, na.rm = T),
                   njobs_total = sum(njobs00, na.rm = T),
                   nhincjobs_total = sum(nhincjobs00, na.rm = T),
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
         pctnhbvap_total_1 = ifelse(vap_total_1 == 0, 0, (nhbvap_total_1/vap_total_1)*100),
         pcthispvap_total_1 = ifelse(vap_total_1 == 0, 0, (hispvap_total_1/vap_total_1)*100),
         pctnativevap_total_1 = ifelse(vap_total_1 == 0, 0, (nativevap_total_1/vap_total_1)*100),
         pctasianvap_total_1 = ifelse(vap_total_1 == 0, 0, (asianvap_total_1/vap_total_1)*100),
         pctothervap_total_1 = ifelse(vap_total_1 == 0, 0, (othervap_total_1/vap_total_1)*100),
         pctincopp_total_1 = ifelse(nwork_total_1 == 0, 0, (incopp_total_1/nwork_total_1)*100),
         pcthincjobs_total_1 = ifelse(njobs_total_1 == 0, 0, (nhincjobs_total_1/njobs_total_1)*100),
         pctownerocc_total_1 = (owneroccupied_total_1/hu_total_1)*100) 

sapply(place_all, function(x) sum(is.na(x)))  

pl_annex_var_0007 <- left_join(
  place_all, place_by_annex, 
  by = "plid"
) %>%
  mutate(annexing = ifelse(!is.na(pop_total_1), 1, 0))
table(pl_annex_var_0007$annexing)
sapply(place_all, function(x) sum(is.na(x)))  

# add vra indicator 
places_vra <- aa0007 %>%
  group_by(plid) %>%
  dplyr::summarize(vra = mean(vra, na.rm = T)) %>%
  ungroup() %>%
  mutate(vra = ifelse(vra > 0, 1, 0))

pl_annex_var_0007 %<>%
  left_join(places_vra, by = "plid") 

# place-level variables
pl00 <- read_csv("data_repo/dem_data_clean/places/annual/pl0017_interpolated.csv") %>% filter(Year == "2000")
table(pl_annex_var_0007$plid %in% pl00$plid) 

cdps07 <- read_csv("plids/pl2007.csv") %>% # want to know which places are CDPs--they do not annex
  select(Geo_NAME, plid) %>%
  mutate(cdp = ifelse(grepl("CDP|cdp", Geo_NAME), 1, 0)) %>%
  filter(cdp==1)

pl_annex_var_0007 %<>%
  filter(plid %in% pl00$plid & !(plid %in% cdps07$plid)) %>%
  left_join(pl00, by = "plid") %>%
  mutate(post = 0,
         time = "2000 to 2007",
         pctowneroccupied = (owneroccupied/hu)*100) 

table(pl_annex_var_0007$annexing)

pl_annex_var_0007 %<>%
  #filter(pop_total > 0) %>%
  mutate(annexing = ifelse(annexing == 1 & (pop_total_1 < 1 | is.na(pop_total_1) | pop_total < 1), 0, annexing))
table(pl_annex_var_0007$annexing)

sapply(pl_annex_var_0007, function(x) sum(is.na(x)))  

vap <- read_csv("data_repo/dem_data_clean/places/pl0007_var.csv") %>%
  select(plid, contains("vap00p")) %>%
  select(plid, starts_with("pct"))

names(vap) <- gsub("00p", "", names(vap))

pl_annex_var_0007 %<>%
  left_join(vap, by = "plid") %>%
  mutate(
    vraa = ifelse((pctnhblackvap >= 20 & pctasianvap >= 20 | pctnhblackvap >= 20 & pctnativevap >= 20 | pctnhblackvap >= 20 & pcthispvap >= 20 | pctnhblackvap >= 20 & pctnativevap >= 20 | pctnhblackvap >= 20 & pctothervap >= 20 | pctasianvap >= 20 & pctnativevap >= 20 | pctasianvap >= 20 & pcthispvap >= 20 | pctasianvap >= 20 & pctothervap >= 20 | pctnativevap >= 20 & pcthispvap >= 20 | pctnativevap >= 20 & pctothervap >= 20), 1, 0),
    underbound_black_vraa = ifelse(vraa == 1 & annexing == 1 & ((pctnhbvap_total_1 - pctnhblackvap) < -0.03), 1, 0),
    underbound_black = ifelse(
      (annexing == 1 & pctnhblack_total_1 < pctnhblack), 1, 0), 
    underbound_nbmin = ifelse(
      (annexing == 1 & pctnbmin_total_1 < pctnbmin), 1, 0)
  )

pl_annex_var_0007 %<>%
  filter(pop > 0) 

pl_annex_var_0007 %<>%
  mutate(more_white = ifelse(pctnhwhite_total > pctnhwhite, 1, 0)) %>%
  filter_at(vars(pop, popdensity, pctnhblack_total, pctnbmin_total, more_white, pctowneroccupied, mhmval, hinc, ppov, pctblackpov, pctnbminpov, pctownerocc_total, pcthincjobs_total, pctincopp_total), ~!is.na(.))

p0007 <- unique(pl_annex_var_0007$plid)
pl_annex_vra_0007 <- unique(pl_annex_var_0007$plid[pl_annex_var_0007$vra==1])

rm(aa0007, place_all, place_by_annex, places_vra, cdps07, pl00)

# unique IDS ----
# unique plids common across time 
plids <- Reduce(intersect, list(unique(p0007), unique(p0713), unique(p1420)))

# unique vra plids
vraplids <- unique(c(pl_annex_vra_0007, pl_annex_vra_0713, pl_annex_vra_1420))

# make panel data ----
names <- Reduce(intersect, list(names(pl_annex_var_0007), names(pl_annex_var_0713), names(pl_annex_var_1420)))

pl_annex_var_0007 %<>%
  select(names) %>%
  filter(plid %in% plids)
pl_annex_var_0713 %<>%
  select(names) %>%
  filter(plid %in% plids)
pl_annex_var_1420 %<>%
  select(names) %>%
  filter(plid %in% plids)

panel_annual <- rbind(pl_annex_var_0007, pl_annex_var_0713, pl_annex_var_1420)

table(panel_annual$vra, exclude = NULL)
table(panel_annual$vraa, exclude = NULL)
table(panel_annual$underbound_black_vraa, exclude = NULL)

length(unique(panel_annual$plid))

panel_annual %<>%
  mutate(vra = ifelse(plid %in% vraplids, 1, 0)) 

table(panel_annual$vra, exclude = NULL)

save.image(paste0(savedir, "/THREE_July.RData"))

