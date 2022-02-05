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

# load in places that definitely annexed prior to 2013, using 0010 data 
annexed0010 <- read_csv("annexedblocks0010dem_pl00_newsample_unincorp.csv")

# get 2010-2013 
# find annexations 2010-2013 
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
         min_total = sum(min),
         min_mean = mean((min_total/pop_total)*100, na.rm = T),
         vap_total = sum(sum(nhwvap), sum(minorityvap)),
         nhblackvap_total = sum(nhbvap),
         nhblackvap_mean = mean((nhblackvap_total/vap_total)*100),
         nhwhitevap_total = sum(nhwvap),
         nhwhitevap_mean = mean((nhwhitevap_total/vap_total)*100),
         hvap_total = sum(hispvap),
         hvap_mean = mean((hvap_total/vap_total)*100),
         minorityvap_total = sum(minorityvap),
         minorityvap_mean = mean((minorityvap_total/vap_total)*100),
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
            min_total = sum(min),
            min_mean = mean((min_total/pop_total)*100, na.rm = T),
            vap_total = sum(sum(nhwvap), sum(minorityvap)),
            nhblackvap_total = sum(nhbvap),
            nhblackvap_mean = mean((nhblackvap_total/vap_total)*100),
            nhwhitevap_total = sum(nhwvap),
            nhwhitevap_mean = mean((nhwhitevap_total/vap_total)*100),
            hvap_total = sum(hispvap),
            hvap_mean = mean((hvap_total/vap_total)*100),
            minorityvap_total = sum(minorityvap),
            minorityvap_mean = mean((minorityvap_total/vap_total)*100)) %>%
  ungroup() %>%
  pivot_wider(
    id_cols = plid,
    names_from = annexed,
    values_from = c(pop_total:minorityvap_mean)
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
      is.finite(mingrowth) & 
      is.finite(minvapgrowth) 
  ) %>%
  mutate(proj_growth_white = (((nhwhitegrowth/10)*3)/100)+1,
         proj_growth_black = (((nhblackgrowth/10)*3)/100)+1,
         proj_growth_h = (((hgrowth/10)*3)/100)+1,
         proj_growth_min = (((mingrowth/10)*3)/100)+1,
         proj_growth_whitevap = (((nhwhitevapgrowth/10)*3)/100)+1, 
         proj_growth_blackvap = (((nhblackvapgrowth/10)*3)/100)+1,
         proj_growth_hvap = (((hispvapgrowth/10)*3)/100)+1,
         proj_growth_minvap = (((minvapgrowth/10)*3)/100)+1,
         proj_pop = pop10p*((((popgrowth/10)*3)/100)+1),
         proj_vap = nhwhitevap10p*proj_growth_whitevap + 
           minvap10p*proj_growth_minvap,
         proj_nhwhite = nhwhite10p*proj_growth_white,
         proj_nhblack = nhblack10p*proj_growth_black,
         proj_h = h10p*proj_growth_h,
         proj_min = min10p*proj_growth_min,
         proj_nhwhitevap = nhwhitevap10p*proj_growth_whitevap,
         proj_nhblackvap = nhblackvap10p*proj_growth_blackvap,
         proj_hvap = hispvap10p*proj_growth_hvap,
         proj_minvap = minvap10p*proj_growth_minvap,
         densifying = ifelse(is.na(densification), NA,
                             ifelse(densification > 0, 1, 0)),
         economic_need = ifelse(is.na(hinc10p), NA,
                                ifelse(((hinc10p-hinc00p*1.25)/(hinc00p*1.25)) < 0, 1, 0))
  ) %>%
  select(plid, c(contains("proj")), densifying, economic_need, c(contains("growth")), -c(contains("_growth")))

table(pl_annex_var_1013$plid %in% pl0010$plid) #4695 false

pl_annex_var_1013 %<>%
  filter(plid %in% pl0010$plid) %>%
  left_join(pl0010, by = "plid") %>%
  mutate(post = 0)

table(pl_annex_var_1013$annexing)

# make underbound variable
pl_annex_var_1013 %<>%
  mutate(
    underbound_black = ifelse(
      (annexing == 1 & (((nhblack_total_1 + proj_nhblack)/(proj_pop + pop_total_1)) < (proj_nhblack/proj_pop))), 1, 
      ifelse(annexing == 0 & (((nhblack_total_0 + proj_nhblack)/proj_pop + pop_total_0) < (proj_nhblack/proj_pop)), 1, 0)
    ),
    underbound_hisp = ifelse(
      (annexing == 1 & (((h_total_1 + proj_h)/(proj_pop + pop_total_1)) < (proj_h/proj_pop))), 1, 
      ifelse(annexing == 0 & (((h_total_0 + proj_h)/proj_pop + pop_total_0) < (proj_h/proj_pop)), 1, 0)
    ),
    underbound_minority = ifelse(
      (annexing == 1 & (((min_total_1 + proj_min)/(proj_pop + pop_total_1)) < (proj_min/proj_pop))), 1, 
      ifelse(annexing == 0 & (((min_total_0 + proj_min)/proj_pop + pop_total_0) < (proj_min/proj_pop)), 1, 0)
    ),
    overbound_white = ifelse(
      (annexing == 1 & (((nhwhite_total_1 + proj_nhwhite)/(proj_pop + pop_total_1)) > (proj_nhwhite/proj_pop))), 1, 
      ifelse(annexing == 0 & (((nhwhite_total_0 + proj_nhwhite)/proj_pop + pop_total_0) > (proj_nhwhite/proj_pop)), 1, 0)
    ),
    underbound_blackvap = ifelse(
      (annexing == 1 & (((nhblackvap_total_1 + proj_nhblackvap)/(proj_vap + vap_total_1)) < (proj_nhblackvap/proj_vap))), 1, 
      ifelse(annexing == 0 & (((nhblackvap_total_0 + proj_nhblackvap)/proj_vap + vap_total_0) < (proj_nhblackvap/proj_vap)), 1, 0)
    ),
    underbound_hispvap = ifelse(
      (annexing == 1 & (((hvap_total_1 + proj_hvap)/(proj_vap + pop_total_1)) < (proj_hvap/proj_vap))), 1, 
      ifelse(annexing == 0 & (((hvap_total_0 + proj_hvap)/proj_vap + pop_total_0) < (proj_hvap/proj_vap)), 1, 0)
    ),
    underbound_minorityvap = ifelse(
      (annexing == 1 & (((minorityvap_total_1 + proj_minvap)/(proj_vap + pop_total_1)) < (proj_minvap/proj_vap))), 1, 
      ifelse(annexing == 0 & (((minorityvap_total_0 + proj_minvap)/proj_vap + pop_total_0) < (proj_minvap/proj_vap)), 1, 0)
    ),
    overbound_whitevap = ifelse(
      (annexing == 1 & (((nhwhitevap_total_1 + proj_nhwhitevap)/(proj_vap + pop_total_1)) > (proj_nhwhitevap/proj_vap))), 1, 
      ifelse(annexing == 0 & (((nhwhitevap_total_0 + proj_nhwhitevap)/proj_vap + pop_total_0) > (proj_nhwhitevap/proj_vap)), 1, 0)
    )
  )

table(pl_annex_var_1013$underbound_black)
table(pl_annex_var_1013$underbound_hisp)
table(pl_annex_var_1013$underbound_minority)
table(pl_annex_var_1013$overbound_white)
table(pl_annex_var_1013$underbound_blackvap)
table(pl_annex_var_1013$underbound_hispvap)
table(pl_annex_var_1013$underbound_minorityvap)
table(pl_annex_var_1013$overbound_whitevap)

write_csv(pl_annex_var_1013, "analyticalfiles/pl_annex_var_1013.csv")

table(pl_annex_var_1013$vra, pl_annex_var_1013$underbound_black)
table(pl_annex_var_1013$vra, pl_annex_var_1013$underbound_hisp)
table(pl_annex_var_1013$vra, pl_annex_var_1013$underbound_minority)
table(pl_annex_var_1013$vra, pl_annex_var_1013$overbound_white)
table(pl_annex_var_1013$vra, pl_annex_var_1013$underbound_blackvap)
table(pl_annex_var_1013$vra, pl_annex_var_1013$underbound_hispvap)
table(pl_annex_var_1013$vra, pl_annex_var_1013$underbound_minorityvap)
table(pl_annex_var_1013$vra, pl_annex_var_1013$overbound_whitevap)

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
            min_total = sum((pctmin*pop)),
            min_mean = mean((min_total/pop_total)*100, na.rm = T),
            vap_total = sum(sum(nhwvap), sum(minorityvap)),
            nhblackvap_total = sum(nhbvap),
            nhblackvap_mean = mean((nhblackvap_total/vap_total)*100),
            nhwhitevap_total = sum(nhwvap),
            nhwhitevap_mean = mean((nhwhitevap_total/vap_total)*100),
            hvap_total = sum(hispvap),
            hvap_mean = mean((hvap_total/vap_total)*100),
            minorityvap_total = sum(minorityvap),
            minorityvap_mean = mean((minorityvap_total/vap_total)*100),
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
            min_total = sum(min),
            min_mean = mean((min_total/pop_total)*100, na.rm = T),
            vap_total = sum(sum(nhwvap), sum(minorityvap)),
            nhblackvap_total = sum(nhbvap),
            nhblackvap_mean = mean((nhblackvap_total/vap_total)*100),
            nhwhitevap_total = sum(nhwvap),
            nhwhitevap_mean = mean((nhwhitevap_total/vap_total)*100),
            hvap_total = sum(hispvap),
            hvap_mean = mean((hvap_total/vap_total)*100),
            minorityvap_total = sum(minorityvap),
            minorityvap_mean = mean((minorityvap_total/vap_total)*100)) %>%
  ungroup() %>%
  pivot_wider(
    id_cols = plid,
    names_from = annexed,
    values_from = c(pop_total:minorityvap_mean)
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
      is.finite(mingrowth) & 
      is.finite(minvapgrowth) 
  ) %>%
  mutate(proj_growth_white = (((nhwhitegrowth/10)*3)/100)+1,
         proj_growth_black = (((nhblackgrowth/10)*3)/100)+1,
         proj_growth_h = (((hgrowth/10)*3)/100)+1,
         proj_growth_min = (((mingrowth/10)*3)/100)+1,
         proj_growth_whitevap = (((nhwhitevapgrowth/10)*3)/100)+1, 
         proj_growth_blackvap = (((nhblackvapgrowth/10)*3)/100)+1,
         proj_growth_hvap = (((hispvapgrowth/10)*3)/100)+1,
         proj_growth_minvap = (((minvapgrowth/10)*3)/100)+1,
         proj_pop = pop10p*((((popgrowth/10)*3)/100)+1),
         proj_vap = nhwhitevap10p*proj_growth_whitevap + 
           minvap10p*proj_growth_minvap,
         proj_nhwhite = nhwhite10p*proj_growth_white,
         proj_nhblack = nhblack10p*proj_growth_black,
         proj_h = h10p*proj_growth_h,
         proj_min = min10p*proj_growth_min,
         proj_nhwhitevap = nhwhitevap10p*proj_growth_whitevap,
         proj_nhblackvap = nhblackvap10p*proj_growth_blackvap,
         proj_hvap = hispvap10p*proj_growth_hvap,
         proj_minvap = minvap10p*proj_growth_minvap,
         densifying = ifelse(is.na(densification), NA,
                             ifelse(densification > 0, 1, 0)),
         economic_need = ifelse(is.na(hinc10p), NA,
                                ifelse(((hinc10p-hinc00p*1.25)/(hinc00p*1.25)) < 0, 1, 0))
  ) %>%
  select(plid, c(contains("proj")), densifying, economic_need, c(contains("growth")), -c(contains("_growth")))

table(pl_annex_var_1420$plid %in% pl0010$plid) #4695 false

pl_annex_var_1420 %<>%
  filter(plid %in% pl0010$plid) %>%
  left_join(pl0010, by = "plid") %>%
  mutate(post = 0)

table(pl_annex_var_1420$annexing)

# make underbound variable
pl_annex_var_1420 %<>%
  mutate(
    underbound_black = ifelse(
      (annexing == 1 & (((nhblack_total_1 + proj_nhblack)/(proj_pop + pop_total_1)) < (proj_nhblack/proj_pop))), 1, 
      ifelse(annexing == 0 & (((nhblack_total_0 + proj_nhblack)/proj_pop + pop_total_0) < (proj_nhblack/proj_pop)), 1, 0)
    ),
    underbound_hisp = ifelse(
      (annexing == 1 & (((h_total_1 + proj_h)/(proj_pop + pop_total_1)) < (proj_h/proj_pop))), 1, 
      ifelse(annexing == 0 & (((h_total_0 + proj_h)/proj_pop + pop_total_0) < (proj_h/proj_pop)), 1, 0)
    ),
    underbound_minority = ifelse(
      (annexing == 1 & (((min_total_1 + proj_min)/(proj_pop + pop_total_1)) < (proj_min/proj_pop))), 1, 
      ifelse(annexing == 0 & (((min_total_0 + proj_min)/proj_pop + pop_total_0) < (proj_min/proj_pop)), 1, 0)
    ),
    overbound_white = ifelse(
      (annexing == 1 & (((nhwhite_total_1 + proj_nhwhite)/(proj_pop + pop_total_1)) > (proj_nhwhite/proj_pop))), 1, 
      ifelse(annexing == 0 & (((nhwhite_total_0 + proj_nhwhite)/proj_pop + pop_total_0) > (proj_nhwhite/proj_pop)), 1, 0)
    ),
    underbound_blackvap = ifelse(
      (annexing == 1 & (((nhblackvap_total_1 + proj_nhblackvap)/(proj_vap + vap_total_1)) < (proj_nhblackvap/proj_vap))), 1, 
      ifelse(annexing == 0 & (((nhblackvap_total_0 + proj_nhblackvap)/proj_vap + vap_total_0) < (proj_nhblackvap/proj_vap)), 1, 0)
    ),
    underbound_hispvap = ifelse(
      (annexing == 1 & (((hvap_total_1 + proj_hvap)/(proj_vap + pop_total_1)) < (proj_hvap/proj_vap))), 1, 
      ifelse(annexing == 0 & (((hvap_total_0 + proj_hvap)/proj_vap + pop_total_0) < (proj_hvap/proj_vap)), 1, 0)
    ),
    underbound_minorityvap = ifelse(
      (annexing == 1 & (((minorityvap_total_1 + proj_minvap)/(proj_vap + pop_total_1)) < (proj_minvap/proj_vap))), 1, 
      ifelse(annexing == 0 & (((minorityvap_total_0 + proj_minvap)/proj_vap + pop_total_0) < (proj_minvap/proj_vap)), 1, 0)
    ),
    overbound_whitevap = ifelse(
      (annexing == 1 & (((nhwhitevap_total_1 + proj_nhwhitevap)/(proj_vap + pop_total_1)) > (proj_nhwhitevap/proj_vap))), 1, 
      ifelse(annexing == 0 & (((nhwhitevap_total_0 + proj_nhwhitevap)/proj_vap + pop_total_0) > (proj_nhwhitevap/proj_vap)), 1, 0)
    )
  )

table(pl_annex_var_1420$underbound_black)
table(pl_annex_var_1420$underbound_hisp)
table(pl_annex_var_1420$underbound_minority)
table(pl_annex_var_1420$overbound_white)
table(pl_annex_var_1420$underbound_blackvap)
table(pl_annex_var_1420$underbound_hispvap)
table(pl_annex_var_1420$underbound_minorityvap)
table(pl_annex_var_1420$overbound_whitevap)

write_csv(pl_annex_var_1420, "analyticalfiles/pl_annex_var_1420.csv")

table(pl_annex_var_1420$vra, pl_annex_var_1420$underbound_black)
table(pl_annex_var_1420$vra, pl_annex_var_1420$underbound_hisp)
table(pl_annex_var_1420$vra, pl_annex_var_1420$underbound_minority)
table(pl_annex_var_1420$vra, pl_annex_var_1420$overbound_white)
table(pl_annex_var_1420$vra, pl_annex_var_1420$underbound_blackvap)
table(pl_annex_var_1420$vra, pl_annex_var_1420$underbound_hispvap)
table(pl_annex_var_1420$vra, pl_annex_var_1420$underbound_minorityvap)
table(pl_annex_var_1420$vra, pl_annex_var_1420$overbound_whitevap)

rm(list = ls())

# make panel data!!!!! ####
places_to_merge2013 <- read_csv("places_to_merge2013.csv")
places_to_merge2013 %<>%
    mutate(pctnhblack_diff = ifelse(is.na(pctnhblack_diff), pctnhblack00b_diff, pctnhblack_diff),
           pctnhwhite_diff = ifelse(is.na(pctnhwhite_diff), pctnhwhite00b_diff, pctnhwhite_diff),
           pcth_diff = ifelse(is.na(pcth_diff), pcth00b_diff, pcth_diff),
           pctmin_diff = ifelse(is.na(pctmin_diff), pctmin00b_diff, pctmin_diff),
    ) %>%
    select(-c(contains("00b")))
   
places_to_merge2013 %<>%
    filter(plid %in% places_to_merge2020$plid)

places_to_merge2020 %<>%
    filter(plid %in% places_to_merge2013$plid)

panel1320_did <- base::rbind(
    places_to_merge2013,
    places_to_merge2020
)

write_csv(panel1320_did, "panel1320_did.csv")

# test reg
panel1320_did <- read_csv("panel1320_did.csv")
panel1320_did %<>%
    mutate(post = ifelse(Year > 2013, 1, 0),
           treatment = ifelse(vra==1, 1, 0)
           )

panel1320_did %<>% 
    filter(!is.na(pctnhblack_diff)) %>%
    group_by(plid) %>%
    mutate(appear = n()) %>%
    filter(appear == 2) %>%
    ungroup()

panel1320_did %<>%
    mutate(underbound_nhb_abs = ifelse(pctnhblack_diff < 0, 1, 0),
           underbound_nhb_med = ifelse(pctnhblack_diff < 6.06, 1, 0),
           underbound_nhw_med = ifelse(pctnhwhite_diff > 5.03, 1, 0))

testdid <- fixest::feglm(underbound_nhb_abs ~ treatment + post + treatment*post | as.factor(as.character(plid)), family = "binomial", data = panel1320_did)
testdid2 <- fixest::feglm(underbound_nhb_med ~ treatment + post + treatment*post | as.factor(as.character(plid)), family = "binomial", data = panel1320_did)
testdid2 <- fixest::feglm(underbound_nhw_med ~ treatment + post + treatment*post | as.factor(as.character(plid)), family = "binomial", data = panel1320_did)

summary(testdid)
summary(testdid2)

predictdf <- with(panel1320_did, data.frame(
                  treatment = c(0, 0, 1, 1),
                      post = c(0, 1, 0, 1),
                  plid = rep("0100124", 4)))

predictdf <- cbind(predictdf, predict(testdid, newdata = predictdf)) 
names(predictdf)[4] <- "pred_val"
predictdf %<>% mutate(predicted_val = exp(pred_val - 1))

summary(testdid)

panel1320_did %<>%
    mutate(plid = as.numeric(as.character(plid)))

testdid2 <- att_gt(
    yname = "pctnhblack_diff",
    gname = "treatment",
    tname = "post",
    idname = "plid",
    xformla = ~1,
    data = panel1320_did,
    est_method = "reg"
)

