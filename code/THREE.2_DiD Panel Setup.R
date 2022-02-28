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

# make DiD panel ####
# 1. get plid for annexations to 2000-2010 and 2010-2013 
# and their avg % annexed, avg % between annexed and non-annexed (2013 block)
### filter out 2010-2013 annexations 
# 2. for the did panel, make outcome of annexed-annexable at 2013 and 2020 

# outcome var = 
# 1/0 to make a racially selective annexation 
# racially selective = not annexing the block because of racial considerations 
# this is nebulous to define, so create a few thresholds 
# if annexed %black is negative at all compared to annexable (or the place, if did not annex)
# if annexed % block is 75% less black comparedd to annexable 
# first need to clean 2010-2020 annexation data compared to BAS 
# next, if a place annexed from 2000-2013, they are given a 0 for time, and 1 otherwise 
# calculate outcomes 
# need county code for each place 

# 2010-2013 ####
aa1013 <- read_csv("analyticalfiles/annexedblocks1013dem_pl00_newsample_unincorp.csv")

# transform this into place-level summaries
# characteristic of all annexable blocks
# diff between annexed and not annexed 
place_all <- aa1013 %>% 
  group_by(plid) %>%
  summarize(pop_total = sum(pop),
         nhblack_total = sum(nhblack),
         nhblack_mean = mean((nhblack_total/pop_total)*100, na.rm = T),
         nhwhite_total = sum(nhwhite),
         nhwhite_mean = mean((nhwhite_total/pop_total)*100, na.rm = T),
         h_total = sum(h),
         h_mean = mean((h_total/pop_total)*100, na.rm = T),
         vap_total = sum(vap, na.rm = T),
         nhblackvap_total = sum(nhbvap),
         nhblackvap_mean = mean((nhblackvap_total/vap_total)*100),
         nhwhitevap_total = sum(nhwvap),
         nhwhitevap_mean = mean((nhwhitevap_total/vap_total)*100),
         hvap_total = sum(hispvap),
         hvap_mean = mean((hvap_total/vap_total)*100),
         nativevap_total = sum(nativevap, na.rm = T),
         asianvap_total = sum(asianvap, na.rm = T),
         othervap_total = sum(othervap, na.rm = T),
         pct_annexed = mean(annexed, na.rm = T)) %>%
  ungroup()
  
place_by_annex <- aa1013 %>%
  group_by(plid, annexed) %>%
  summarize(pop_total = sum(pop),
            nhblack_total = sum(nhblack),
            nhblack_mean = mean((nhblack_total/pop_total)*100, na.rm = T),
            nhwhite_total = sum(nhwhite),
            nhwhite_mean = mean((nhwhite_total/pop_total)*100, na.rm = T),
            h_total = sum(h),
            h_mean = mean((h_total/pop_total)*100, na.rm = T),
            vap_total = sum(vap, na.rm = T),
            nhblackvap_total = sum(nhbvap),
            nhblackvap_mean = mean((nhblackvap_total/vap_total)*100),
            nhwhitevap_total = sum(nhwvap),
            nhwhitevap_mean = mean((nhwhitevap_total/vap_total)*100),
            hvap_total = sum(hispvap),
            hvap_mean = mean((hvap_total/vap_total)*100),
            nativevap_total = sum(nativevap, na.rm = T),
            asianvap_total = sum(asianvap, na.rm = T),
            othervap_total = sum(othervap, na.rm = T)) %>%
  ungroup() %>%
  pivot_wider(
    id_cols = plid,
    names_from = annexed,
    values_from = c(pop_total:othervap_total)
  )
  
pl_annex_var_1013 <- left_join(
  place_all, place_by_annex, 
  by = "plid"
)

# add vra indicator 
places_vra <- aa1013 %>%
  group_by(plid) %>%
  summarize(vra = mean(vra, na.rm = T),
            annexing = mean(annexed, na.rm = T)) %>%
  ungroup() %>%
  mutate(vra = ifelse(vra > 0, 1, 0),
         annexing = ifelse(annexing > 0, 1, 0))

pl_annex_var_1013 %<>%
  left_join(places_vra, by = "plid")

# add place-level data for race and vap 
# vra violation = if pctwhite after annex > pctwhite before annex compared to
# what would have been, i.e. pctwhite given growth rate by 2013
# or if pctblack after annex < pctblack before annex compared to what would 
# have been, i.e. pctwhite given growth rate by 2013
# or if pcth after annex < pcth before annex compared to what would have been,
# i.e. pcth given growth rate by 2013 
pl0010 <- read_csv("pl0010_var.csv")

pl0010 %<>%
  filter(
    is.finite(popgrowth) & 
      is.finite(nhwhitegrowth) & 
      is.finite(nhwhitevapgrowth) &
      is.finite(nhblackgrowth) & 
      is.finite(nhblackvapgrowth) &
      is.finite(hgrowth) & 
      is.finite(hispvapgrowth) &
      is.finite(mingrowth)
  ) %>%
  mutate(vapgrowth = ((vap10p-vap00p)/vap10p)*100,
         proj_growth_vap = (((vapgrowth/10)*3)/100)+1,
    proj_growth_white = (((nhwhitegrowth/10)*3)/100)+1,
         proj_growth_black = (((nhblackgrowth/10)*3)/100)+1,
         proj_growth_h = (((hgrowth/10)*3)/100)+1,
         proj_growth_min = (((mingrowth/10)*3)/100)+1,
         proj_growth_whitevap = (((nhwhitevapgrowth/10)*3)/100)+1, 
         proj_growth_blackvap = (((nhblackvapgrowth/10)*3)/100)+1,
         proj_growth_hvap = (((hispvapgrowth/10)*3)/100)+1,
         proj_pop = pop10p*((((popgrowth/10)*3)/100)+1),
         proj_vap = vap10p*proj_growth_vap,
         proj_nhwhite = nhwhite10p*proj_growth_white,
         proj_nhblack = nhblack10p*proj_growth_black,
         proj_h = h10p*proj_growth_h,
         proj_nhwhitevap = nhwhitevap10p*proj_growth_whitevap,
         proj_nhblackvap = nhblackvap10p*proj_growth_blackvap,
         proj_hvap = hispvap10p*proj_growth_hvap,
         densifying = ifelse(is.na(densification), NA,
                             ifelse(densification > 0, 1, 0)),
         economic_need = ifelse(is.na(hinc10p), NA,
                                ifelse(((hinc10p-hinc00p*1.25)/(hinc00p*1.25)) < 0, 1, 0))
  ) %>%
  select(plid, c(contains("proj")), c(contains("growth")), -c(contains("_growth")), vraa, c(contains("vap")))

table(pl_annex_var_1013$plid %in% pl0010$plid) #4695 false

pl_annex_var_1013 %<>%
  filter(plid %in% pl0010$plid) %>%
  left_join(pl0010, by = "plid") %>%
  mutate(post = 0)

table(pl_annex_var_1013$annexing)

# make underbound variable
# # of black_annexed(2013) + blackvap(2010)/(vap2000 + annexed_vap2013) - blackvap2010/
pl_annex_var_1013 %<>%
  mutate(
    underbound_blackvap_vraa = ifelse(
      (annexing == 1 & vraa == 1 & ((((nhblackvap_total_1 + nhblackvap10p)/(vap_total_1 + vap10p)) - (nhblackvap10p/vap10p))) < -0.03), 1, 0), 
    underbound_hispvap_vraa = ifelse(
      (annexing == 1 & vraa == 1 & ((((hvap_total_1 + hispvap10p)/(vap_total_1 + vap10p)) - (hispvap10p/vap10p))) < -0.03), 1, 0),
    underbound_asianvap_vraa = ifelse(
      (annexing == 1 & vraa == 1 & ((((asianvap_total_1 + asianvap10p)/(vap10p + vap_total_1)) - (asianvap10p/vap10p))) < -0.03), 1, 0), 
    underbound_nativevap_vraa = ifelse(
      (annexing == 1 & vraa == 1 & ((((nativevap_total_1 + nativevap10p)/(vap10p + vap_total_1)) - (nativevap10p/vap10p))) < -0.03), 1, 0),
    underbound_othervap_vraa = ifelse(
      (annexing == 1 & vraa == 1 & ((((othervap_total_1 + othervap10p)/(vap10p + vap_total_1)) - (othervap10p/vap10p))) < -0.03), 1, 0)
    
  )

pl_annex_var_1013 %<>%
  mutate(
    underbound_blackvap = ifelse(
      (annexing == 1 & ((((nhblackvap_total_1 + nhblackvap10p)/(vap_total_1 + vap10p)) < (nhblackvap10p/vap10p)))), 1, 0), 
    underbound_hispvap = ifelse(
      (annexing == 1 & ((((hvap_total_1 + hispvap10p)/(vap_total_1 + vap10p)) < (hispvap10p/vap10p)))), 1, 0),
    underbound_asianvap = ifelse(
      (annexing == 1 & ((((asianvap_total_1 + asianvap10p)/(vap10p + vap_total_1)) < (asianvap10p/vap10p)))), 1, 0), 
    underbound_nativevap = ifelse(
      (annexing == 1 & ((((nativevap_total_1 + nativevap10p)/(vap10p + vap_total_1)) < (nativevap10p/vap10p)))), 1, 0),
    underbound_othervap = ifelse(
      (annexing == 1 & ((((othervap_total_1 + othervap10p)/(vap10p + vap_total_1)) < (othervap10p/vap10p)))), 1, 0)
  )

table(pl_annex_var_1013$underbound_blackvap)
table(pl_annex_var_1013$underbound_hispvap)
table(pl_annex_var_1013$underbound_asianvap)
table(pl_annex_var_1013$underbound_nativevap)
table(pl_annex_var_1013$underbound_othervap)
table(pl_annex_var_1013$underbound_blackvap_vraa)
table(pl_annex_var_1013$underbound_hispvap_vraa)
table(pl_annex_var_1013$underbound_asianvap_vraa)
table(pl_annex_var_1013$underbound_nativevap_vraa)
table(pl_annex_var_1013$underbound_othervap_vraa)

pl_annex_var_1013 %<>%
  mutate(drop = ifelse(
  vap_total == 0, 1, 0
  ))
table(pl_annex_var_1013$drop)

pl_annex_var_1013 %<>%
  filter(drop==0) %>%
  select(-drop)

write_csv(pl_annex_var_1013, "analyticalfiles/pl_annex_var_1013.csv")
rm(list = ls())

#repeat for 1420 ####
aa1420 <- read_csv("analyticalfiles/annexedblocks1420dem_pl00_newsample_unincorp.csv")

# transform this into place-level summaries
# characteristic of all annexable blocks
# diff between annexed and not annexed 
place_all <- aa1420 %>% 
  group_by(plid) %>%
  summarize(pop_total = sum(pop),
            nhblack_total = sum((pctnhblack*pop)),
            nhblack_mean = mean((nhblack_total/pop_total)*100, na.rm = T),
            nhwhite_total = sum((pctnhwhite*pop)),
            nhwhite_mean = mean((nhwhite_total/pop_total)*100, na.rm = T),
            h_total = sum((pcth*pop)),
            h_mean = mean((h_total/pop_total)*100, na.rm = T),
            vap_total = sum(vap, na.rm = T),
            hu_total = sum(hu, na.rm = T),
            nhblackvap_total = sum(nhbvap, na.rm = T),
            nhblackvap_mean = mean((nhblackvap_total/vap_total)*100),
            nhwhitevap_total = sum(nhwvap, na.rm = T),
            nhwhitevap_mean = mean((nhwhitevap_total/vap_total)*100),
            hvap_total = sum(hispvap, na.rm = T),
            hvap_mean = mean((hvap_total/vap_total)*100),
            nativevap_total = sum(nativevap, na.rm = T),
            asianvap_total = sum(asianvap, na.rm = T),
            othervap_total = sum(othervap, na.rm = T),
            pct_annexed = mean(annexed, na.rm = T)) %>%
  ungroup()

place_by_annex <- aa1420 %>%
  group_by(plid, annexed) %>%
  summarize(pop_total = sum(pop),
            nhblack_total = sum(nhblack),
            nhblack_mean = mean((nhblack_total/pop_total)*100, na.rm = T),
            nhwhite_total = sum(nhwhite),
            nhwhite_mean = mean((nhwhite_total/pop_total)*100, na.rm = T),
            h_total = sum(h),
            h_mean = mean((h_total/pop_total)*100, na.rm = T),
            vap_total = sum(vap, na.rm = T),
            hu_total = sum(hu, na.rm = T),
            nhblackvap_total = sum(nhbvap, na.rm = T),
            nhblackvap_mean = mean((nhblackvap_total/vap_total)*100),
            nhwhitevap_total = sum(nhwvap, na.rm = T),
            nhwhitevap_mean = mean((nhwhitevap_total/vap_total)*100),
            hvap_total = sum(hispvap, na.rm = T),
            hvap_mean = mean((hvap_total/vap_total)*100),
            nativevap_total = sum(nativevap, na.rm = T),
            asianvap_total = sum(asianvap, na.rm = T),
            othervap_total = sum(othervap, na.rm = T)) %>%
  ungroup() %>%
  pivot_wider(
    id_cols = plid,
    names_from = annexed,
    values_from = c(pop_total:othervap_total)
  )

pl_annex_var_1420 <- left_join(
  place_all, place_by_annex, 
  by = "plid"
)

# add vra indicator 
places_vra <- aa1420 %>%
  group_by(plid) %>%
  summarize(vra = mean(vra, na.rm = T),
            annexing = mean(annexed, na.rm = T)) %>%
  ungroup() %>%
  mutate(vra = ifelse(vra > 0, 1, 0),
         annexing = ifelse(annexing > 0, 1, 0))

pl_annex_var_1420 %<>%
  left_join(places_vra, by = "plid")

# add place-level data for race and vap 
# vra violation = if pctwhite after annex > pctwhite before annex compared to
# what would have been, i.e. pctwhite given growth rate by 2013
# or if pctblack after annex < pctblack before annex compared to what would 
# have been, i.e. pctwhite given growth rate by 2013
# or if pcth after annex < pcth before annex compared to what would have been,
# i.e. pcth given growth rate by 2013 
pl1014 <- read_csv("pl1014_var.csv")

pl1014 %<>%
  filter(
    is.finite(popgrowth) & 
      is.finite(nhwhitegrowth) & 
      is.finite(nhwhitevapgrowth) &
      is.finite(nhblackgrowth) & 
      is.finite(nhblackvapgrowth) &
      is.finite(hgrowth) & 
      is.finite(hispvapgrowth) &
      is.finite(mingrowth)
  ) %>%
  mutate(proj_growth_white = (((nhwhitegrowth/4)*6)/100)+1,
         proj_growth_black = (((nhblackgrowth/4)*6)/100)+1,
         proj_growth_h = (((hgrowth/4)*6)/100)+1,
         proj_growth_whitevap = (((nhwhitevapgrowth/4)*6)/100)+1, 
         proj_growth_blackvap = (((nhblackvapgrowth/4)*6)/100)+1,
         proj_growth_hvap = (((hispvapgrowth/4)*6)/100)+1,
         proj_pop = pop14p*((((popgrowth/4)*6)/100)+1),
         vapgrowth = ((vap14p-vap10p)/vap10p)*100,
         proj_growth_vap = (((vapgrowth/4)*6)/100)+1,
         proj_vap = vap14p*proj_growth_vap,
         proj_nhwhite = nhwhite14p*proj_growth_white,
         proj_nhblack = nhblack14p*proj_growth_black,
         proj_h = h14p*proj_growth_h,
         proj_nhwhitevap = nhwhitevap14p*proj_growth_whitevap,
         proj_nhblackvap = nhblackvap14p*proj_growth_blackvap,
         proj_hvap = hispvap14p*proj_growth_hvap,
         densifying = ifelse(is.na(densification), NA,
                             ifelse(densification > 0, 1, 0)),
         economic_need = ifelse(is.na(hinc14p), NA,
                                ifelse(((hinc14p-hinc10p*1.25)/(hinc10p*1.25)) < 0, 1, 0))
  ) %>%
  select(plid, c(contains("proj")), densifying, economic_need, c(contains("growth")), -c(contains("_growth")), vraa, c(contains("vap")))

table(pl_annex_var_1420$plid %in% pl1014$plid) 

pl_annex_var_1420 %<>%
  filter(plid %in% pl1014$plid) %>%
  left_join(pl1014, by = "plid") %>%
  mutate(post = 1)

table(pl_annex_var_1420$annexing)
# make underbound variable
pl_annex_var_1420 %<>%
  mutate(
    underbound_blackvap_vraa = ifelse(
      (annexing == 1 & vraa == 1 & ((((nhblackvap_total_1 + nhblackvap14p)/(vap14p + vap_total_1)) - (nhblackvap14p/vap14p))) < -0.03), 1, 0), 
    underbound_hispvap_vraa = ifelse(
      (annexing == 1 & vraa == 1 & ((((hvap_total_1 + hispvap14p)/(vap14p + vap_total_1)) - (hispvap14p/vap14p))) < -0.03), 1, 0),
    underbound_asianvap_vraa = ifelse(
      (annexing == 1 & vraa == 1 & ((((asianvap_total_1 + asianvap14p)/(vap14p + vap_total_1)) - (asianvap14p/vap14p))) < -0.03), 1, 0), 
    underbound_nativevap_vraa = ifelse(
      (annexing == 1 & vraa == 1 & ((((nativevap_total_1 + nativevap14p)/(vap14p + vap_total_1)) - (nativevap14p/vap14p))) < -0.03), 1, 0),
    underbound_othervap_vraa = ifelse(
      (annexing == 1 & vraa == 1 & ((((othervap_total_1 + othervap14p)/(vap14p + vap_total_1)) - (othervap14p/vap14p))) < -0.03), 1, 0)
  )

pl_annex_var_1420 %<>%
  mutate(
    underbound_blackvap = ifelse(
      (annexing == 1 & ((((nhblackvap_total_1 + nhblackvap14p)/(vap14p + vap_total_1)) < (nhblackvap14p/vap14p)))), 1, 0), 
    underbound_hispvap = ifelse(
      (annexing == 1 & ((((hvap_total_1 + hispvap14p)/(vap14p + vap_total_1)) < (hispvap14p/vap14p)))), 1, 0),
    underbound_asianvap = ifelse(
      (annexing == 1 & ((((asianvap_total_1 + asianvap14p)/(vap14p + vap_total_1)) < (asianvap14p/vap14p)))), 1, 0), 
    underbound_nativevap = ifelse(
      (annexing == 1 & ((((nativevap_total_1 + nativevap14p)/(vap14p + vap_total_1)) < (nativevap14p/vap14p)))), 1, 0),
    underbound_othervap = ifelse(
      (annexing == 1 & ((((othervap_total_1 + othervap14p)/(vap14p + vap_total_1)) < (othervap14p/vap14p)))), 1, 0)
  )

table(pl_annex_var_1420$underbound_blackvap)
table(pl_annex_var_1420$underbound_hispvap)
table(pl_annex_var_1420$underbound_nativevap)
table(pl_annex_var_1420$underbound_asianvap)
table(pl_annex_var_1420$underbound_othervap)
table(pl_annex_var_1420$underbound_blackvap_vraa)
table(pl_annex_var_1420$underbound_hispvap_vraa)
table(pl_annex_var_1420$underbound_nativevap_vraa)
table(pl_annex_var_1420$underbound_asianvap_vraa)
table(pl_annex_var_1420$underbound_othervap_vraa)

pl_annex_var_1420 %<>%
  mutate(drop = ifelse(
    vap_total == 0, 1, 0
  ))
table(pl_annex_var_1420$drop)

pl_annex_var_1420 %<>%
  filter(drop==0) %>%
  select(-drop)
write_csv(pl_annex_var_1420, "analyticalfiles/pl_annex_var_1420.csv")

rm(list = ls())

# 0010 ####
aa0010 <- read_csv("analyticalfiles/annexedblocks0010dem_pl00_newsample_unincorp.csv")

# transform this into place-level summaries
# characteristic of all annexable blocks
# diff between annexed and not annexed 
place_all <- aa0010 %>% 
  group_by(plid) %>%
  summarize(pop_total = sum(pop),
            nhblack_total = sum((pctnhblack*pop)),
            nhblack_mean = mean((nhblack_total/pop_total)*100, na.rm = T),
            nhwhite_total = sum((pctnhwhite*pop)),
            nhwhite_mean = mean((nhwhite_total/pop_total)*100, na.rm = T),
            h_total = sum((pcth*pop)),
            h_mean = mean((h_total/pop_total)*100, na.rm = T),
            vap_total = sum(vap, na.rm = T),
            nhblackvap_total = sum(nhbvap),
            nhblackvap_mean = mean((nhblackvap_total/vap_total)*100),
            nhwhitevap_total = sum(nhwvap),
            nhwhitevap_mean = mean((nhwhitevap_total/vap_total)*100),
            hvap_total = sum(hispvap),
            hvap_mean = mean((hvap_total/vap_total)*100),
            nativevap_total = sum(nativevap, na.rm = T),
            asianvap_total = sum(asianvap, na.rm = T),
            othervap_total = sum(othervap, na.rm = T),
            pct_annexed = mean(annexed, na.rm = T)) %>%
  ungroup()

place_by_annex <- aa0010 %>%
  group_by(plid, annexed) %>%
  summarize(pop_total = sum(pop),
            nhblack_total = sum(nhblack),
            nhblack_mean = mean((nhblack_total/pop_total)*100, na.rm = T),
            nhwhite_total = sum(nhwhite),
            nhwhite_mean = mean((nhwhite_total/pop_total)*100, na.rm = T),
            h_total = sum(h),
            h_mean = mean((h_total/pop_total)*100, na.rm = T),
            vap_total = sum(vap, na.rm = T),
            nhblackvap_total = sum(nhbvap),
            nhblackvap_mean = mean((nhblackvap_total/vap_total)*100),
            nhwhitevap_total = sum(nhwvap),
            nhwhitevap_mean = mean((nhwhitevap_total/vap_total)*100),
            hvap_total = sum(hispvap),
            hvap_mean = mean((hvap_total/vap_total)*100),
            nativevap_total = sum(nativevap, na.rm = T),
            asianvap_total = sum(asianvap, na.rm = T),
            othervap_total = sum(othervap, na.rm = T)) %>%
  ungroup() %>%
  pivot_wider(
    id_cols = plid,
    names_from = annexed,
    values_from = c(pop_total:othervap_total)
  )

pl_annex_var_0010 <- left_join(
  place_all, place_by_annex, 
  by = "plid"
)

# add vra indicator 
places_vra <- aa0010 %>%
  group_by(plid) %>%
  summarize(vra = mean(vra, na.rm = T),
            annexing = mean(annexed, na.rm = T)) %>%
  ungroup() %>%
  mutate(vra = ifelse(vra > 0, 1, 0),
         annexing = ifelse(annexing > 0, 1, 0))

table(places_vra$annexing)
pl_annex_var_0010 %<>%
  left_join(places_vra, by = "plid")

# add place-level data for race and vap 
# vra violation = if pctwhite after annex > pctwhite before annex compared to
# what would have been, i.e. pctwhite given growth rate by 2013
# or if pctblack after annex < pctblack before annex compared to what would 
# have been, i.e. pctwhite given growth rate by 2013
# or if pcth after annex < pcth before annex compared to what would have been,
# i.e. pcth given growth rate by 2013 
pl9000 <- read_csv("pl9000_var.csv")

pl9000 %<>%
  filter(is.finite(popgrowth) & 
      is.finite(nhwhitegrowth) & 
      is.finite(nhwhitevapgrowth) &
      is.finite(nhblackgrowth) & 
      is.finite(nhblackvapgrowth) &
      is.finite(hgrowth) & 
      is.finite(hispvapgrowth) &
      is.finite(mingrowth)
  ) %>%
  mutate(proj_growth_white = (nhwhitegrowth/100)+1,
         proj_growth_black = (nhblackgrowth/100)+1,
         proj_growth_h = (hgrowth/100)+1,
         proj_growth_min = (mingrowth/100)+1,
         proj_growth_whitevap = (nhwhitevapgrowth/100)+1, 
         proj_growth_blackvap = (nhblackvapgrowth/100)+1,
         proj_growth_hvap = (hispvapgrowth/100)+1,
         vapgrowth = ((vap00p-vap90p)/vap90p)*100,
         proj_growth_vap = (vapgrowth/100) + 1,
         proj_pop = pop00p*((popgrowth/100)+1),
         proj_vap = vap00p * proj_growth_vap,
         proj_nhwhite = nhwhite00p*proj_growth_white,
         proj_nhblack = nhblack00p*proj_growth_black,
         proj_h = h00p*proj_growth_h,
         proj_min = min00p*proj_growth_min,
         proj_nhwhitevap = nhwhitevap00p*proj_growth_whitevap,
         proj_nhblackvap = nhblackvap00p*proj_growth_blackvap,
         proj_hvap = hispvap00p*proj_growth_hvap,
         densifying = ifelse(is.na(densification), NA,
                             ifelse(densification > 0, 1, 0)),
         economic_need = ifelse(is.na(hinc00p), NA,
                                ifelse(((hinc00p-hinc90p*1.25)/(hinc90p*1.25)) < 0, 1, 0))
  ) %>%
  select(plid, c(contains("proj")), densifying, economic_need, c(contains("growth")), -c(contains("_growth")), vraa, c(contains("vap")))

table(pl_annex_var_0010$plid %in% pl9000$plid) 

pl_annex_var_0010 %<>%
  filter(plid %in% pl9000$plid) %>%
  left_join(pl9000, by = "plid") %>%
  mutate(post = -1)

table(pl_annex_var_0010$annexing)

# make underbound variable
pl_annex_var_0010 %<>%
  mutate(
    underbound_blackvap_vraa = ifelse(
      (annexing == 1 & vraa == 1 & ((((nhblackvap_total_1 + nhblackvap00p)/(vap00p + vap_total_1)) - (nhblackvap00p/vap00p))) < -0.03), 1, 0), 
    underbound_hispvap_vraa = ifelse(
      (annexing == 1 & vraa == 1 & ((((hvap_total_1 + hispvap00p)/(vap00p + vap_total_1)) - (hispvap00p/vap00p))) < -0.03), 1, 0),
    underbound_asianvap_vraa = ifelse(
      (annexing == 1 & vraa == 1 & ((((asianvap_total_1 + asianvap00p)/(vap00p + vap_total_1)) - (asianvap00p/vap00p))) < -0.03), 1, 0), 
    underbound_nativevap_vraa = ifelse(
      (annexing == 1 & vraa == 1 & ((((nativevap_total_1 + nativevap00p)/(vap00p + vap_total_1)) - (nativevap00p/vap00p))) < -0.03), 1, 0),
    underbound_othervap_vraa = ifelse(
      (annexing == 1 & vraa == 1 & ((((othervap_total_1 + othervap00p)/(vap00p + vap_total_1)) - (othervap00p/vap00p))) < -0.03), 1, 0)
  )

pl_annex_var_0010 %<>%
  mutate(
    underbound_blackvap = ifelse(
      (annexing == 1 & ((((nhblackvap_total_1 + nhblackvap00p)/(vap00p + vap_total_1)) < (nhblackvap00p/vap00p)))), 1, 0), 
    underbound_hispvap = ifelse(
      (annexing == 1 & ((((hvap_total_1 + hispvap00p)/(vap00p + vap_total_1)) < (hispvap00p/vap00p)))), 1, 0),
    underbound_asianvap = ifelse(
      (annexing == 1 & ((((asianvap_total_1 + asianvap00p)/(vap00p + vap_total_1)) < (asianvap00p/vap00p)))), 1, 0), 
    underbound_nativevap = ifelse(
      (annexing == 1 & ((((nativevap_total_1 + nativevap00p)/(vap00p + vap_total_1)) < (nativevap00p/vap00p)))), 1, 0),
    underbound_othervap = ifelse(
      (annexing == 1 & ((((othervap_total_1 + othervap00p)/(vap00p + vap_total_1)) < (othervap00p/vap00p)))), 1, 0)
  )

table(pl_annex_var_0010$underbound_blackvap)
table(pl_annex_var_0010$underbound_hispvap)
table(pl_annex_var_0010$underbound_nativevap)
table(pl_annex_var_0010$underbound_asianvap)
table(pl_annex_var_0010$underbound_othervap)
table(pl_annex_var_0010$underbound_blackvap_vraa)
table(pl_annex_var_0010$underbound_hispvap_vraa)
table(pl_annex_var_0010$underbound_nativevap_vraa)
table(pl_annex_var_0010$underbound_asianvap_vraa)
table(pl_annex_var_0010$underbound_othervap_vraa)

pl_annex_var_0010 %<>%
  mutate(drop = ifelse(
    vap_total == 0, 1, 0
  ))
table(pl_annex_var_0010$drop)

pl_annex_var_0010 %<>%
  filter(drop==0) %>%
  select(-drop)

write_csv(pl_annex_var_0010, "analyticalfiles/pl_annex_var_0010.csv")
rm(list = ls())

# make panel data!!!!! ####
# take out ne and hawaii
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
  filter(nhwhitevapgrowth >= 0)
pl1013 <- read_csv("analyticalfiles/pl_annex_var_1013.csv") %>%
  mutate(STATE = substr(plid, 1, 2)) %>%
  filter(!(STATE %in% NE)) %>%
  filter(annexing==1) %>%
  filter_at(all_of(indices), ~(.>0)) %>%
  filter(nhwhitevapgrowth >= 0)
pl1420 <- read_csv("analyticalfiles/pl_annex_var_1420.csv") %>%
  mutate(STATE = substr(plid, 1, 2)) %>%
  filter(!(STATE %in% NE)) %>%
  filter(annexing==1) %>%
  filter_at(all_of(indices), ~(.>0)) %>%
  filter(nhwhitevapgrowth >= 0)

plids <- Reduce(intersect, list(unique(pl0010$plid), unique(pl1013$plid), unique(pl1420$plid)))
names_list <- Reduce(intersect, list(names(pl0010), names(pl1013), names(pl1420)))

#2-period only 
plids <- Reduce(intersect, list(unique(pl1013$plid), unique(pl1420$plid)))
names_list <- Reduce(intersect, list(names(pl1013), names(pl1420)))

pl0010 %<>% 
  filter(plid %in% plids) %>%
  select(all_of(names_list)) %>%
  filter(!duplicated(plid))
pl1013 %<>%
  filter(plid %in% plids) %>%
  select(all_of(names_list))
pl1420 %<>%
  filter(plid %in% plids) %>%
  select(all_of(names_list))

panel0020_did <- base::rbind(
    pl0010, pl1013, pl1420
)

panel0020_did <- base::rbind(
  pl1013, pl1420
)

panel0020_did %<>%
  mutate(
    nhblack_proj_dem = (proj_nhblack/proj_pop),
    nhblack_annex = ((nhblack_total_1 + proj_nhblack)/(proj_pop + pop_total_1)),
    nhblack_diff = nhblack_annex - nhblack_proj_dem,
    nhblack_diff_ann = nhblack_mean_1 - nhblack_mean_0,
    hisp_annex = ((h_total_1 + proj_h)/(proj_pop + pop_total_1)),
    hisp_proj_dem = (proj_h/proj_pop),
    hisp_diff = hisp_annex - hisp_proj_dem,
    white_annex = ((nhwhite_total_1 + proj_nhwhite)/(proj_pop + pop_total_1)) ,
    white_proj_dem = (proj_nhwhite/proj_pop),
    white_diff = white_annex - white_proj_dem,
    nhblackvap_proj_dem = (proj_nhblackvap/proj_vap),
    nhblackvap_annex = ((nhblackvap_total_1 + proj_nhblackvap)/(proj_vap + vap_total_1)),
    nhblackvap_diff = nhblackvap_annex - nhblackvap_proj_dem,
    hispvap_annex = ((hvap_total_1 + proj_hvap)/(proj_vap + pop_total_1)),
    hispvap_proj_dem = (proj_hvap/proj_vap),
    hispvap_diff = hispvap_annex - hispvap_proj_dem,
    whitevap_annex = ((nhwhitevap_total_1 + proj_nhwhitevap)/(proj_vap + pop_total_1)),
    whitevap_proj_dem = (proj_nhwhitevap/proj_vap),
    whitevap_diff = whitevap_annex - whitevap_proj_dem
  )

z <- function(var_name) {
  (var_name - mean(var_name, na.rm = T))/sd(var_name, na.rm = T)
}

panel0020_did %<>%
  mutate(STATEA = as.character(substr(plid, 1, 2)),
         recimmgrowth_b = ifelse(recimmgrowth > 0, 1, 0),
         nhblackgrowth_b = ifelse(nhblackgrowth > 0, 1, 0),
         pct_annexed_z = z(pct_annexed),
         incomegrowth_z = z(incomegrowth),
         recimmgrowth_z = z(recimmgrowth),
         nhblackgrowth_z = z(nhblackgrowth),
         nhblackvapgrowth_z = z(nhblackvapgrowth),
         nhwhitegrowth_z = z(nhwhitegrowth),
         nhwhitevapgrowth_z = z(nhwhitevapgrowth),
         hgrowth_z = z(hgrowth),
         hispvapgrowth_z = z(hispvapgrowth),
         popgrowth_z = z(popgrowth),
         nhblack_diff_z = z(nhblack_diff))


panel0020_did %<>% 
  mutate(period = as.factor(post), 
         post = ifelse(post < 0, 0, 1), 
         vra = as.factor(vra),
         post = as.factor(post))

# test reg
testdid <- fixest::feols(underbound_blackvap ~ as.factor(vra)*as.factor(post) + recimmgrowth_b + nhblackvapgrowth_z + popgrowth_z + incomegrowth_z + pct_annexed_z + nhwhitevapgrowth_z | plid, data = panel0020_did)
summary(testdid)
testdid <- fixest::feols(underbound_blackvap ~ as.factor(vra)*as.factor(post) | plid + STATE, data = panel0020_did)
summary(testdid)

testdid2 <- fixest::feols(underbound_hisp ~ vra*post | plid, data = panel0020_did)
summary(testdid2)
testdid2 <- fixest::feols(underbound_hispvap ~ as.factor(vra)*as.factor(post) + recimmgrowth_b + hispvapgrowth_z + popgrowth_z + incomegrowth_z + pct_annexed_z + nhwhitevapgrowth_z  | plid, data = panel0020_did)
summary(testdid2)
testdid2 <- fixest::feols(underbound_hispvap ~ as.factor(vra)*as.factor(post) | plid + STATE, data = panel0020_did)
summary(testdid2)

testdid3 <- fixest::feols(underbound_hispvap ~ as.factor(vra)*as.factor(post) + recimmgrowth_b + hispvapgrowth_z + popgrowth_z + incomegrowth_z + pct_annexed_z + nhwhitevapgrowth_z  | plid, data = panel0020_did)
summary(testdid3)
testdid3 <- fixest::feols(underbound_asianvap ~ as.factor(vra)*as.factor(post) | plid + STATE, data = panel0020_did)
summary(testdid3)

testdid4 <- fixest::feols(underbound_nativevap ~ as.factor(vra)*as.factor(post) | plid + STATE, data = panel0020_did)
summary(testdid3)

# make did panel for vis ####
NE <- c("09", "23", "25", "33", "34", "36", "42", "24", "44", "50", "15")
pl0010 <- read_csv("analyticalfiles/pl_annex_var_0010.csv") %>%
  mutate(STATE = substr(plid, 1, 2)) %>%
  filter(!(STATE %in% NE)) #%>%
  filter(annexing==1)
pl1013 <- read_csv("analyticalfiles/pl_annex_var_1013.csv") %>%
  mutate(STATE = substr(plid, 1, 2)) %>%
  filter(!(STATE %in% NE)) #%>%
  filter(annexing==1)
pl1420 <- read_csv("analyticalfiles/pl_annex_var_1420.csv") %>%
  mutate(STATE = substr(plid, 1, 2)) %>%
  filter(!(STATE %in% NE)) #%>%
  filter(annexing==1)

plids <- Reduce(intersect, list(unique(pl0010$plid), unique(pl1013$plid), unique(pl1420$plid)))
names_list <- Reduce(intersect, list(names(pl0010), names(pl1013), names(pl1420)))

pl0010 %<>% 
  filter(plid %in% plids) %>%
  select(names_list)
pl0010 %<>%
  filter(!duplicated(plid)) %>%
  select(names_list)
pl1013 %<>%
  filter(plid %in% plids) %>%
  select(names_list)
pl1420 %<>%
  filter(plid %in% plids) %>%
  select(names_list)

panel0020_did <- base::rbind(
  pl0010, pl1013, pl1420
)

panel0020_did %<>%
  mutate(
    nhblack_proj_dem = (proj_nhblack/proj_pop),
    nhblack_annex = ((nhblack_total_1 + proj_nhblack)/(proj_pop + pop_total_1)),
    nhblack_diff = nhblack_annex - nhblack_proj_dem,
    nhblack_diff_ann = nhblack_mean_1 - nhblack_mean_0,
    hisp_annex = ((h_total_1 + proj_h)/(proj_pop + pop_total_1)),
    hisp_proj_dem = (proj_h/proj_pop),
    hisp_diff = hisp_annex - hisp_proj_dem,
    white_annex = ((nhwhite_total_1 + proj_nhwhite)/(proj_pop + pop_total_1)) ,
    white_proj_dem = (proj_nhwhite/proj_pop),
    white_diff = white_annex - white_proj_dem,
    nhblackvap_proj_dem = (proj_nhblackvap/proj_vap),
    nhblackvap_annex = ((nhblackvap_total_1 + proj_nhblackvap)/(proj_vap + vap_total_1)),
    nhblackvap_diff = nhblackvap_annex - nhblackvap_proj_dem,
    hispvap_annex = ((hvap_total_1 + proj_hvap)/(proj_vap + pop_total_1)),
    hispvap_proj_dem = (proj_hvap/proj_vap),
    hispvap_diff = hispvap_annex - hispvap_proj_dem,
    whitevap_annex = ((nhwhitevap_total_1 + proj_nhwhitevap)/(proj_vap + pop_total_1)),
    whitevap_proj_dem = (proj_nhwhitevap/proj_vap),
    whitevap_diff = whitevap_annex - whitevap_proj_dem
  )

indices <- c(grep("vap_diff", names(panel0020_did)))
indices <- names(panel0020_did)[indices]

did_vis <- panel0020_did %>%
  #filter(annexing==1) %>%
  group_by(vra, post) %>%
  summarise_at(all_of(indices), 
               ~mean(., na.rm = T)) %>%
  ungroup() %>%
  pivot_longer(cols = contains("_diff"),
               names_to = "Race", 
               values_to = "mean") %>%
  mutate(Race = case_when(
    grepl("nhblack", Race) ~ "Non-Hispanic Black",
    grepl("hisp", Race) ~ "Hispanic",
    grepl("white", Race) ~ "Non-Hispanic White"
  ),
  vra = as.character(vra),
  # Time = case_when(
  #   grepl("-1", post) ~ "2000-2010",
  #   grepl("0", post) ~ "2010-2013",
  #   grepl("1", post) ~ "2014-2020"
  # ),
  # Time = factor(Time, levels = c("2000-2010", "2010-2013", "2014-2020"))
  Race = factor(Race, levels = c("Non-Hispanic Black", "Hispanic", "Non-Hispanic White")))

did_vis_total <- ggplot(did_vis,
         aes(y = mean, x = post, color = vra)) + 
  geom_line() + scale_x_continuous(breaks = c(-1, 0, 1)) + 
  scale_y_continuous(labels = scales::percent) +
  geom_vline(xintercept = 0, linetype="dotted", 
             color = "red", size=0.5) +
  facet_grid(~Race, scales = "free") + 
  labs(color = "Covered Under \n Section V",
       x = "Period Relative to Shelby v. Holder", 
       y = "Mean Difference", 
       title = "Differences in Post-Annexation Demographic Composition and 
Projected Compositions Without Annexation, 
Pre- and Post-Shelby Trends by Race")
did_vis_total
ggsave(filename = "analyticalfiles/did_vis_vap.png",
       plot = did_vis_total,
       dpi = 300)

indices <- c(grep("underbound", names(panel0020_did)))
indices <- names(panel0020_did)[indices]
indices <- indices[grepl("vap$", indices)]

did_vis <- panel0020_did %>%
  #filter(annexing==1) %>%
  group_by(vra, post) %>%
  summarise_at(all_of(indices), 
               ~mean(., na.rm = T)) %>%
  ungroup() %>%
  pivot_longer(cols = contains("underbound"),
               names_to = "Race", 
               values_to = "mean") %>%
  mutate(Race = case_when(
    grepl("black", Race) ~ "Non-Hispanic Black",
    grepl("hisp", Race) ~ "Hispanic",
    grepl("native", Race) ~ "Native",
    grepl("asian", Race) ~ "Asian",
    grepl("other", Race) ~ "Other",
    grepl("white", Race) ~ "Non-Hispanid White"
  ),
  vra = as.character(vra)
  # Time = case_when(
  #   grepl("-1", post) ~ "2000-2010",
  #   grepl("0", post) ~ "2010-2013",
  #   grepl("1", post) ~ "2014-2020"
  # ),
  # Time = factor(Time, levels = c("2000-2010", "2010-2013", "2014-2020"))
  )

did_vis_total <- ggplot(did_vis,
                        aes(y = mean, x = as.numeric(as.character(post)), color = vra)) + 
  geom_point() + geom_line() + 
  scale_x_continuous(breaks = c(-1, 0, 1)) + 
  scale_y_continuous(labels = scales::percent) +
  geom_vline(xintercept = 0, linetype="dotted", 
             color = "red", size=0.5) +
  facet_grid(~Race, scales = "free") + 
  labs(color = "Covered Under \n Section V",
       x = "Period Relative to Shelby v. Holder", 
       y = "Mean, Ammong Annexing Places", 
       title = "% of Annexations Potentially Subject to Pre-clearance Scrunity,
Pre- and Post-Shelby Trends by Race, VAP")
did_vis_total
ggsave(filename = "analyticalfiles/did_vis_annexed_mean.png",
       plot = did_vis_total,
       dpi = 300)

# vap
vapindices <- c(grep("*vap_diff", names(panel0020_did)))
vapindices <- names(panel0020_did)[vapindices]

did_vis_vap <- panel0020_did %>%
  group_by(vra, post) %>%
  summarise_at(all_of(vapindices), 
               ~mean(., na.rm = T)) %>%
  ungroup() %>%
  pivot_longer(cols = contains("_diff"),
               names_to = "Race", 
               values_to = "mean") %>%
  mutate(Race = case_when(
    grepl("nhblack", Race) ~ "Non-Hispanic Black",
    grepl("hisp", Race) ~ "Hispanic",
    grepl("min", Race) ~ "All Non-White",
    grepl("white", Race) ~ "Non-Hispanic White"
  ),
  vra = as.character(vra),
  # Time = case_when(
  #   grepl("-1", post) ~ "2000-2010",
  #   grepl("0", post) ~ "2010-2013",
  #   grepl("1", post) ~ "2014-2020"
  # ),
  # Time = factor(Time, levels = c("2000-2010", "2010-2013", "2014-2020"))
  Race = factor(Race, levels = c("Non-Hispanic Black", "Hispanic", "All Non-White", "Non-Hispanic White")))

