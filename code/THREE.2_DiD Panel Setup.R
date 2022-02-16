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
                                ifelse(((hinc10p-hinc00p*1.25)/(hinc00p*1.25)) < 0, 1, 0)),
         threat_white = case_when(
           (pctnhwhite10p >= 60 & 
              ((pctnhwhite10p*proj_growth_white)/proj_pop) >= 0.6) ~ "none",
           (pctnhwhite10p >= 60 & 
              ((pctnhwhite10p*proj_growth_white)/proj_pop) < 0.6) ~ "loss_solid",
           (pctnhwhite10p >= 50 & pctnhwhite10p < 60 &
              ((pctnhwhite10p*proj_growth_white)/proj_pop) < 0.5) ~ "loss_competitive",
           pctnhwhite10p < 50 ~ "maj-min",
           TRUE ~ NA_character_
         ),
         threat_white_vap = case_when(
           (nhwhitevap10p >= 60 & 
              ((nhwhitevap10p*proj_growth_whitevap)/proj_vap) >= 0.6) ~ "none",
           (nhwhitevap10p >= 60 & 
              ((nhwhitevap10p*proj_growth_whitevap)/proj_vap) < 0.6) ~ "loss_solid",
           (nhwhitevap10p >= 50 & nhwhitevap10p < 60 &
              ((nhwhitevap10p*proj_growth_whitevap)/proj_vap) < 0.5) ~ "loss_competitive",
           nhwhitevap10p < 50 ~ "maj-min",
           TRUE ~ NA_character_
         )
  ) %>%
  select(plid, c(contains("proj")), densifying, economic_need, c(contains("growth")), c(contains("threat")), -c(contains("_growth")))

table(pl_annex_var_1013$plid %in% pl0010$plid) #4695 false

pl_annex_var_1013 %<>%
  filter(plid %in% pl0010$plid) %>%
  left_join(pl0010, by = "plid") %>%
  mutate(post = 0)

table(pl_annex_var_1013$annexing)
table(pl_annex_var_1013$threat_white)

# make underbound variable
pl_annex_var_1013 %<>%
  mutate(
    underbound_black = ifelse(
      (annexing == 1 & (((nhblack_total_1 + proj_nhblack)/(proj_pop + pop_total_1)) < (proj_nhblack/proj_pop))), 1, 
      ifelse(annexing == 0 & (economic_need == 1 | densifying == 1) & (((nhblack_total_0 + proj_nhblack)/proj_pop + pop_total_0) < (proj_nhblack/proj_pop)), 1, 0)
    ),
    underbound_hisp = ifelse(
      (annexing == 1 & (((h_total_1 + proj_h)/(proj_pop + pop_total_1)) < (proj_h/proj_pop))), 1, 
      ifelse(annexing == 0 & (economic_need == 1 | densifying == 1) & (((h_total_0 + proj_h)/proj_pop + pop_total_0) < (proj_h/proj_pop)), 1, 0)
    ),
    underbound_minority = ifelse(
      (annexing == 1 & (((min_total_1 + proj_min)/(proj_pop + pop_total_1)) < (proj_min/proj_pop))), 1, 
      ifelse(annexing == 0 & (economic_need == 1 | densifying == 1) & (((min_total_0 + proj_min)/proj_pop + pop_total_0) < (proj_min/proj_pop)), 1, 0)
    ),
    overbound_white = ifelse(
      (annexing == 1 & (((nhwhite_total_1 + proj_nhwhite)/(proj_pop + pop_total_1)) > (proj_nhwhite/proj_pop))), 1, 
      ifelse(annexing == 0 & (economic_need == 1 | densifying == 1) & (((nhwhite_total_0 + proj_nhwhite)/proj_pop + pop_total_0) > (proj_nhwhite/proj_pop)), 1, 0)
    ),
    underbound_blackvap = ifelse(
      (annexing == 1 & (((nhblackvap_total_1 + proj_nhblackvap)/(proj_vap + vap_total_1)) < (proj_nhblackvap/proj_vap))), 1, 
      ifelse(annexing == 0 & (economic_need == 1 | densifying == 1) & (((nhblackvap_total_0 + proj_nhblackvap)/proj_vap + vap_total_0) < (proj_nhblackvap/proj_vap)), 1, 0)
    ),
    underbound_hispvap = ifelse(
      (annexing == 1 & (((hvap_total_1 + proj_hvap)/(proj_vap + pop_total_1)) < (proj_hvap/proj_vap))), 1, 
      ifelse(annexing == 0 & (economic_need == 1 | densifying == 1) & (((hvap_total_0 + proj_hvap)/proj_vap + pop_total_0) < (proj_hvap/proj_vap)), 1, 0)
    ),
    underbound_minorityvap = ifelse(
      (annexing == 1 & (((minorityvap_total_1 + proj_minvap)/(proj_vap + pop_total_1)) < (proj_minvap/proj_vap))), 1, 
      ifelse(annexing == 0 & (economic_need == 1 | densifying == 1) & (((minorityvap_total_0 + proj_minvap)/proj_vap + pop_total_0) < (proj_minvap/proj_vap)), 1, 0)
    ),
    overbound_whitevap = ifelse(
      (annexing == 1 & (((nhwhitevap_total_1 + proj_nhwhitevap)/(proj_vap + pop_total_1)) > (proj_nhwhitevap/proj_vap))), 1, 
      ifelse(annexing == 0 & (economic_need == 1 | densifying == 1) & (((nhwhitevap_total_0 + proj_nhwhitevap)/proj_vap + pop_total_0) > (proj_nhwhitevap/proj_vap)), 1, 0)
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
      is.finite(mingrowth) & 
      is.finite(minvapgrowth) 
  ) %>%
  mutate(proj_growth_white = (((nhwhitegrowth/4)*6)/100)+1,
         proj_growth_black = (((nhblackgrowth/4)*6)/100)+1,
         proj_growth_h = (((hgrowth/4)*6)/100)+1,
         proj_growth_min = (((mingrowth/4)*6)/100)+1,
         proj_growth_whitevap = (((nhwhitevapgrowth/4)*6)/100)+1, 
         proj_growth_blackvap = (((nhblackvapgrowth/4)*6)/100)+1,
         proj_growth_hvap = (((hispvapgrowth/4)*6)/100)+1,
         proj_growth_minvap = (((minvapgrowth/4)*6)/100)+1,
         proj_pop = pop14p*((((popgrowth/4)*6)/100)+1),
         proj_vap = nhwhitevap14p*proj_growth_whitevap + 
           minvap14p*proj_growth_minvap,
         proj_nhwhite = nhwhite14p*proj_growth_white,
         proj_nhblack = nhblack14p*proj_growth_black,
         proj_h = h14p*proj_growth_h,
         proj_min = min14p*proj_growth_min,
         proj_nhwhitevap = nhwhitevap14p*proj_growth_whitevap,
         proj_nhblackvap = nhblackvap14p*proj_growth_blackvap,
         proj_hvap = hispvap14p*proj_growth_hvap,
         proj_minvap = minvap14p*proj_growth_minvap,
         densifying = ifelse(is.na(densification), NA,
                             ifelse(densification > 0, 1, 0)),
         economic_need = ifelse(is.na(hinc14p), NA,
                                ifelse(((hinc14p-hinc10p*1.25)/(hinc10p*1.25)) < 0, 1, 0)),
         threat_white = case_when(
           (pctnhwhite14p >= 60 & 
              ((pctnhwhite14p*proj_growth_white)/proj_pop) >= 0.6) ~ "none",
           (pctnhwhite14p >= 60 & 
              ((pctnhwhite14p*proj_growth_white)/proj_pop) < 0.6) ~ "loss_solid",
           (pctnhwhite14p >= 50 & pctnhwhite14p < 60 &
              ((pctnhwhite14p*proj_growth_white)/proj_pop) < 0.5) ~ "loss_competitive",
           pctnhwhite14p < 50 ~ "maj-min",
           TRUE ~ NA_character_
         ),
         threat_white_vap = case_when(
           (nhwhitevap14p >= 60 & 
              ((nhwhitevap14p*proj_growth_whitevap)/proj_vap) >= 0.6) ~ "none",
           (nhwhitevap14p >= 60 & 
              ((nhwhitevap14p*proj_growth_whitevap)/proj_vap) < 0.6) ~ "loss_solid",
           (nhwhitevap14p >= 50 & nhwhitevap14p < 60 &
              ((nhwhitevap14p*proj_growth_whitevap)/proj_vap) < 0.5) ~ "loss_competitive",
           nhwhitevap14p < 50 ~ "maj-min",
           TRUE ~ NA_character_
         )
  ) %>%
  select(plid, c(contains("proj")), densifying, economic_need, c(contains("growth")), c(contains("threat")), -c(contains("_growth")))

table(pl_annex_var_1420$plid %in% pl1014$plid) #5 false 

pl_annex_var_1420 %<>%
  filter(plid %in% pl1014$plid) %>%
  left_join(pl1014, by = "plid") %>%
  mutate(post = 1)

table(pl_annex_var_1420$annexing)
table(pl_annex_var_1420$threat_white)
# make underbound variable
pl_annex_var_1420 %<>%
  mutate(
    underbound_black = ifelse(
      (annexing == 1 & (((nhblack_total_1 + proj_nhblack)/(proj_pop + pop_total_1)) < (proj_nhblack/proj_pop))), 1, 
      ifelse(annexing == 0 & (economic_need == 1 | densifying == 1) & (((nhblack_total_0 + proj_nhblack)/proj_pop + pop_total_0) < (proj_nhblack/proj_pop)), 1, 0)
    ),
    underbound_hisp = ifelse(
      (annexing == 1 & (((h_total_1 + proj_h)/(proj_pop + pop_total_1)) < (proj_h/proj_pop))), 1, 
      ifelse(annexing == 0 & (economic_need == 1 | densifying == 1) & (((h_total_0 + proj_h)/proj_pop + pop_total_0) < (proj_h/proj_pop)), 1, 0)
    ),
    underbound_minority = ifelse(
      (annexing == 1 & (((min_total_1 + proj_min)/(proj_pop + pop_total_1)) < (proj_min/proj_pop))), 1, 
      ifelse(annexing == 0 & (economic_need == 1 | densifying == 1) & (((min_total_0 + proj_min)/proj_pop + pop_total_0) < (proj_min/proj_pop)), 1, 0)
    ),
    overbound_white = ifelse(
      (annexing == 1 & (((nhwhite_total_1 + proj_nhwhite)/(proj_pop + pop_total_1)) > (proj_nhwhite/proj_pop))), 1, 
      ifelse(annexing == 0 & (economic_need == 1 | densifying == 1) & (((nhwhite_total_0 + proj_nhwhite)/proj_pop + pop_total_0) > (proj_nhwhite/proj_pop)), 1, 0)
    ),
    underbound_blackvap = ifelse(
      (annexing == 1 & (((nhblackvap_total_1 + proj_nhblackvap)/(proj_vap + vap_total_1)) < (proj_nhblackvap/proj_vap))), 1, 
      ifelse(annexing == 0 & (economic_need == 1 | densifying == 1) & (((nhblackvap_total_0 + proj_nhblackvap)/proj_vap + vap_total_0) < (proj_nhblackvap/proj_vap)), 1, 0)
    ),
    underbound_hispvap = ifelse(
      (annexing == 1 & (((hvap_total_1 + proj_hvap)/(proj_vap + pop_total_1)) < (proj_hvap/proj_vap))), 1, 
      ifelse(annexing == 0 & (economic_need == 1 | densifying == 1) & (((hvap_total_0 + proj_hvap)/proj_vap + pop_total_0) < (proj_hvap/proj_vap)), 1, 0)
    ),
    underbound_minorityvap = ifelse(
      (annexing == 1 & (((minorityvap_total_1 + proj_minvap)/(proj_vap + pop_total_1)) < (proj_minvap/proj_vap))), 1, 
      ifelse(annexing == 0 & (economic_need == 1 | densifying == 1) & (((minorityvap_total_0 + proj_minvap)/proj_vap + pop_total_0) < (proj_minvap/proj_vap)), 1, 0)
    ),
    overbound_whitevap = ifelse(
      (annexing == 1 & (((nhwhitevap_total_1 + proj_nhwhitevap)/(proj_vap + pop_total_1)) > (proj_nhwhitevap/proj_vap))), 1, 
      ifelse(annexing == 0 & (economic_need == 1 | densifying == 1) & (((nhwhitevap_total_0 + proj_nhwhitevap)/proj_vap + pop_total_0) > (proj_nhwhitevap/proj_vap)), 1, 0)
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

place_by_annex <- aa0010 %>%
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
      is.finite(mingrowth) & 
      is.finite(minvapgrowth) 
  ) %>%
  mutate(proj_growth_white = (nhwhitegrowth/100)+1,
         proj_growth_black = (nhblackgrowth/100)+1,
         proj_growth_h = (hgrowth/100)+1,
         proj_growth_min = (mingrowth/100)+1,
         proj_growth_whitevap = (nhwhitevapgrowth/100)+1, 
         proj_growth_blackvap = (nhblackvapgrowth/100)+1,
         proj_growth_hvap = (hispvapgrowth/100)+1,
         proj_growth_minvap = (minvapgrowth/100)+1,
         proj_pop = pop00p*((popgrowth/100)+1),
         proj_vap = nhwhitevap00p*proj_growth_whitevap + 
           minvap00p*proj_growth_minvap,
         proj_nhwhite = nhwhite00p*proj_growth_white,
         proj_nhblack = nhblack00p*proj_growth_black,
         proj_h = h00p*proj_growth_h,
         proj_min = min00p*proj_growth_min,
         proj_nhwhitevap = nhwhitevap00p*proj_growth_whitevap,
         proj_nhblackvap = nhblackvap00p*proj_growth_blackvap,
         proj_hvap = hispvap00p*proj_growth_hvap,
         proj_minvap = minvap00p*proj_growth_minvap,
         densifying = ifelse(is.na(densification), NA,
                             ifelse(densification > 0, 1, 0)),
         economic_need = ifelse(is.na(hinc00p), NA,
                                ifelse(((hinc00p-hinc90p*1.25)/(hinc90p*1.25)) < 0, 1, 0)),
         threat_white = case_when(
           (pctnhwhite00p >= 60 & 
              ((pctnhwhite00p*proj_growth_white)/proj_pop) >= 0.6) ~ "none",
           (pctnhwhite00p >= 60 & 
              ((pctnhwhite00p*proj_growth_white)/proj_pop) < 0.6) ~ "loss_solid",
           (pctnhwhite00p >= 50 & pctnhwhite00p < 60 &
              ((pctnhwhite00p*proj_growth_white)/proj_pop) < 0.5) ~ "loss_competitive",
           pctnhwhite00p < 50 ~ "maj-min",
           TRUE ~ NA_character_
         ),
         threat_white_vap = case_when(
           (nhwhitevap00p >= 60 & 
              ((nhwhitevap00p*proj_growth_whitevap)/proj_vap) >= 0.6) ~ "none",
           (nhwhitevap00p >= 60 & 
              ((nhwhitevap00p*proj_growth_whitevap)/proj_vap) < 0.6) ~ "loss_solid",
           (nhwhitevap00p >= 50 & nhwhitevap00p < 60 &
              ((nhwhitevap00p*proj_growth_whitevap)/proj_vap) < 0.5) ~ "loss_competitive",
           nhwhitevap00p < 50 ~ "maj-min",
           TRUE ~ NA_character_
         )
  ) %>%
  select(plid, c(contains("proj")), densifying, economic_need, c(contains("growth")), c(contains("threat")), -c(contains("_growth")))

table(pl_annex_var_0010$plid %in% pl9000$plid) 

pl_annex_var_0010 %<>%
  filter(plid %in% pl9000$plid) %>%
  left_join(pl9000, by = "plid") %>%
  mutate(post = -1)

table(pl_annex_var_0010$annexing)
table(pl_annex_var_0010$threat_white)

# make underbound variable
pl_annex_var_0010 %<>%
  mutate(
    underbound_black = ifelse(
      (annexing == 1 & (((nhblack_total_1 + proj_nhblack)/(proj_pop + pop_total_1)) < (proj_nhblack/proj_pop))), 1, 
      ifelse(annexing == 0 & (economic_need == 1 | densifying == 1) & (((nhblack_total_0 + proj_nhblack)/proj_pop + pop_total_0) < (proj_nhblack/proj_pop)), 1, 0)
    ),
    underbound_hisp = ifelse(
      (annexing == 1 & (((h_total_1 + proj_h)/(proj_pop + pop_total_1)) < (proj_h/proj_pop))), 1, 
      ifelse(annexing == 0 & (economic_need == 1 | densifying == 1) & (((h_total_0 + proj_h)/proj_pop + pop_total_0) < (proj_h/proj_pop)), 1, 0)
    ),
    underbound_minority = ifelse(
      (annexing == 1 & (((min_total_1 + proj_min)/(proj_pop + pop_total_1)) < (proj_min/proj_pop))), 1, 
      ifelse(annexing == 0 & (economic_need == 1 | densifying == 1) & (((min_total_0 + proj_min)/proj_pop + pop_total_0) < (proj_min/proj_pop)), 1, 0)
    ),
    overbound_white = ifelse(
      (annexing == 1 & (((nhwhite_total_1 + proj_nhwhite)/(proj_pop + pop_total_1)) > (proj_nhwhite/proj_pop))), 1, 
      ifelse(annexing == 0 & (economic_need == 1 | densifying == 1) & (((nhwhite_total_0 + proj_nhwhite)/proj_pop + pop_total_0) > (proj_nhwhite/proj_pop)), 1, 0)
    ),
    underbound_blackvap = ifelse(
      (annexing == 1 & (((nhblackvap_total_1 + proj_nhblackvap)/(proj_vap + vap_total_1)) < (proj_nhblackvap/proj_vap))), 1, 
      ifelse(annexing == 0 & (economic_need == 1 | densifying == 1) & (((nhblackvap_total_0 + proj_nhblackvap)/proj_vap + vap_total_0) < (proj_nhblackvap/proj_vap)), 1, 0)
    ),
    underbound_hispvap = ifelse(
      (annexing == 1 & (((hvap_total_1 + proj_hvap)/(proj_vap + pop_total_1)) < (proj_hvap/proj_vap))), 1, 
      ifelse(annexing == 0 & (economic_need == 1 | densifying == 1) & (((hvap_total_0 + proj_hvap)/proj_vap + pop_total_0) < (proj_hvap/proj_vap)), 1, 0)
    ),
    underbound_minorityvap = ifelse(
      (annexing == 1 & (((minorityvap_total_1 + proj_minvap)/(proj_vap + pop_total_1)) < (proj_minvap/proj_vap))), 1, 
      ifelse(annexing == 0 & (economic_need == 1 | densifying == 1) & (((minorityvap_total_0 + proj_minvap)/proj_vap + pop_total_0) < (proj_minvap/proj_vap)), 1, 0)
    ),
    overbound_whitevap = ifelse(
      (annexing == 1 & (((nhwhitevap_total_1 + proj_nhwhitevap)/(proj_vap + pop_total_1)) > (proj_nhwhitevap/proj_vap))), 1, 
      ifelse(annexing == 0 & (economic_need == 1 | densifying == 1) & (((nhwhitevap_total_0 + proj_nhwhitevap)/proj_vap + pop_total_0) > (proj_nhwhitevap/proj_vap)), 1, 0)
    )
  )

table(pl_annex_var_0010$underbound_black)
table(pl_annex_var_0010$underbound_hisp)
table(pl_annex_var_0010$underbound_minority)
table(pl_annex_var_0010$overbound_white)
table(pl_annex_var_0010$underbound_blackvap)
table(pl_annex_var_0010$underbound_hispvap)
table(pl_annex_var_0010$underbound_minorityvap)
table(pl_annex_var_0010$overbound_whitevap)

write_csv(pl_annex_var_0010, "analyticalfiles/pl_annex_var_0010.csv")

rm(list = ls())

# make panel data!!!!! ####
# take out ne and hawaii
NE <- c("09", "23", "25", "33", "34", "36", "42", "24", "44", "50", "15")
pl0010 <- read_csv("analyticalfiles/pl_annex_var_0010.csv") %>%
  mutate(STATE = substr(plid, 1, 2)) %>%
  filter(!(STATE %in% NE)) %>%
  filter(annexing==1)
pl1013 <- read_csv("analyticalfiles/pl_annex_var_1013.csv") %>%
  mutate(STATE = substr(plid, 1, 2)) %>%
  filter(!(STATE %in% NE)) %>%
  filter(annexing==1)
pl1420 <- read_csv("analyticalfiles/pl_annex_var_1420.csv") %>%
  mutate(STATE = substr(plid, 1, 2)) %>%
  filter(!(STATE %in% NE)) %>%
  filter(annexing==1)

plids <- Reduce(intersect, list(unique(pl0010$plid), unique(pl1013$plid), unique(pl1420$plid)))

pl0010 %<>% 
  filter(plid %in% plids)
pl0010 %<>%
  filter(!duplicated(plid))
pl1013 %<>%
  filter(plid %in% plids)
pl1420 %<>%
  filter(plid %in% plids)

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
    min_annex = ((min_total_1 + proj_min)/(proj_pop + pop_total_1)),
    min_proj_dem = (proj_min/proj_pop),
    min_diff = min_annex - min_proj_dem,
    white_annex = ((nhwhite_total_1 + proj_nhwhite)/(proj_pop + pop_total_1)) ,
    white_proj_dem = (proj_nhwhite/proj_pop),
    white_diff = white_annex - white_proj_dem,
    nhblackvap_proj_dem = (proj_nhblackvap/proj_vap),
    nhblackvap_annex = ((nhblackvap_total_1 + proj_nhblackvap)/(proj_vap + vap_total_1)),
    nhblackvap_diff = nhblackvap_annex - nhblackvap_proj_dem,
    hispvap_annex = ((hvap_total_1 + proj_hvap)/(proj_vap + pop_total_1)),
    hispvap_proj_dem = (proj_hvap/proj_vap),
    hispvap_diff = hispvap_annex - hispvap_proj_dem,
    minvap_annex = ((minorityvap_total_1 + proj_minvap)/(proj_vap + pop_total_1)),
    minvap_proj_dem = (proj_minvap/proj_vap),
    minvap_diff = minvap_annex - minvap_proj_dem,
    whitevap_annex = ((nhwhitevap_total_1 + proj_nhwhitevap)/(proj_vap + pop_total_1)),
    whitevap_proj_dem = (proj_nhwhitevap/proj_vap),
    whitevap_diff = whitevap_annex - whitevap_proj_dem
  )


write_csv(panel0020_did, "analyticalfiles/panel0020_did.csv")

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
         minrowth_z = z(mingrowth),
         minvapgrowth_z = z(minvapgrowth),
         hgrowth_z = z(hgrowth),
         hispvapgrowth_z = z(hispvapgrowth),
         popgrowth_z = z(popgrowth))


panel0020_did %<>% 
  filter(post >= 0) %>%
  mutate(vra = as.factor(vra),
         post = as.factor(post), 
         recimmgrowth_b = as.factor(recimmgrowth_b),
         nhblackgrowth_b = as.factor(nhblackgrowth_b))

# test reg
testdid <- fixest::feols(underbound_black ~ as.factor(vra) + as.factor(post) + as.factor(vra)*as.factor(post) | plid, data = panel0020_did)
testdid <- fixest::feols(underbound_black ~ vra*post*recimmgrowth_b | plid, data = panel0020_did)
testdid <- fixest::feols(underbound_black ~ vra*post*nhblackgrowth_b | plid, data = panel0020_did)
testdid <- fixest::feols(nhblack_mean_1 ~ vra*post*recimmgrowth_b | plid, data = panel0020_did)
testdid <- fixest::feols(nhblack_mean_1 ~ vra*post*nhblackgrowth_b | plid, data = panel0020_did)

testdid2 <- fixest::feols(underbound_hisp ~ vra + post + vra*post | plid, data = panel0020_did)
testdid2 <- fixest::feols(underbound_hisp ~ vra*post*recimmgrowth_b | plid, data = panel0020_did)
testdid2 <- fixest::feols(h_mean_1 ~ vra + post + vra*post | plid, data = panel0020_did)
testdid2 <- fixest::feols(h_mean_1 ~ vra*post*recimmgrowth_b | plid, data = panel0020_did)

testdid3 <- fixest::feols(underbound_minority ~ vra*post | plid, data = panel0020_did)
testdid3 <- fixest::feols(underbound_minority ~ vra*post*recimmgrowth_b | plid, data = panel0020_did)
testdid3 <- fixest::feols(min_mean_1 ~ vra + post + vra*post | plid, data = panel0020_did)
testdid3 <- fixest::feols(min_diff ~ vra + post + vra*post | plid, data = panel0020_did)

testdid3 <- fixest::feols(underbound_minority ~ vra + post + vra*post | plid + STATEA, data = panel0020_did)
testdid4 <- fixest::feols(overbound_white ~ vra + post + vra*post | plid, data = panel0020_did)

summary(testdid)
summary(testdid2)
summary(testdid3)
summary(testdid4)

vapdid <- fixest::feols(nhblackvap_diff ~ vra + post + vra*post + 
                           pct_annexed_z + popgrowth_z + recimmgrowth_z +
                           incomegrowth_z + nhblackvapgrowth_z | plid, data = panel0020_did)

summary(vapdid)
summary(vapdid2)
summary(vapdid3)
summary(vapdid4)

indices <- c(grep("_diff", names(panel0020_did)))
indices <- names(panel0020_did)[indices]
indices <- indices[!grepl("vap_diff", indices)]
indices <- indices[!grepl("diff_ann", indices)]

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
ggsave(filename = "analyticalfiles/did_vis_total.png",
       plot = did_vis_total,
       dpi = 300)

# add in immigration 
did_vis <- panel0020_did %>%
  filter(!is.na(recimmgrowth_b)) %>%
  group_by(vra, post, recimmgrowth_b) %>%
  summarise_at(all_of(indices), 
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

did_vis_total <- ggplot(did_vis,
                        aes(y = mean, x = post, color = vra)) + 
  geom_line() + scale_x_continuous(breaks = c(-1, 0, 1)) + 
  scale_y_continuous(labels = scales::percent) +
  geom_vline(xintercept = 0, linetype="dotted", 
             color = "red", size=0.5) +
  facet_grid(recimmgrowth_b~Race, scales = "free") + 
  labs(color = "Covered Under \n Section V",
       x = "Period Relative to Shelby v. Holder", 
       y = "Mean Difference", 
       title = "Differences in Post-Annexation Demographic Composition and 
Projected Compositions Without Annexation, 
Pre- and Post-Shelby Trends by Race")
did_vis_total
ggsave(filename = "analyticalfiles/did_vis_total.png",
       plot = did_vis_total,
       dpi = 300)

vapindices <- c(grep("*vap_diff", names(panel0020_did)))
vapindices <- names(panel0020_did)[vapindices]

did_vis_vap <- panel0020_did %>%
  #filter(annexing==1) %>%
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

did_vis_vap <- ggplot(did_vis_vap,
                        aes(y = mean, x = post, color = vra)) + 
  geom_line() + scale_x_continuous(breaks = c(-1, 0, 1)) + 
  geom_vline(xintercept = 0, linetype="dotted", 
             color = "red", size=0.5) +
  facet_grid(~Race, scales = "free") + 
  labs(color = "Covered Under \n Section V",
       x = "Period Relative to Shelby v. Holder", 
       y = "Mean Difference", 
       title = "Differences in Post-Annexation Demographic Composition and 
Projected Compositions Without Annexation, VAP,
Pre- and Post-Shelby Trends by Race")
did_vis_vap
ggsave(filename = "analyticalfiles/did_vis_total.png",
       plot = did_vis_total,
       dpi = 300)

# keep NE, take out HI ####
NE <- c("15")
pl0010 <- read_csv("analyticalfiles/pl_annex_var_0010.csv") %>%
  mutate(STATE = substr(plid, 1, 2)) %>%
  filter(!(STATE %in% NE)) %>%
  filter(annexing==1)
pl1013 <- read_csv("analyticalfiles/pl_annex_var_1013.csv") %>%
  mutate(STATE = substr(plid, 1, 2)) %>%
  filter(!(STATE %in% NE)) %>%
  filter(annexing==1)
pl1420 <- read_csv("analyticalfiles/pl_annex_var_1420.csv") %>%
  mutate(STATE = substr(plid, 1, 2)) %>%
  filter(!(STATE %in% NE)) %>%
  filter(annexing==1)

plids <- Reduce(intersect, list(unique(pl0010$plid), unique(pl1013$plid), unique(pl1420$plid)))

pl0010 %<>% 
  filter(plid %in% plids)
pl0010 %<>%
  filter(!duplicated(plid))
pl1013 %<>%
  filter(plid %in% plids)
pl1420 %<>%
  filter(plid %in% plids)

panel0020_did <- base::rbind(
  pl0010 %>% mutate(post = -1), pl1013, pl1420
)

panel0020_did %<>%
  mutate(
    nhblack_proj_dem = (proj_nhblack/proj_pop),
    nhblack_annex = ((nhblack_total_1 + proj_nhblack)/(proj_pop + pop_total_1)),
    nhblack_diff = nhblack_annex - nhblack_proj_dem,
    hisp_annex = ((h_total_1 + proj_h)/(proj_pop + pop_total_1)),
    hisp_proj_dem = (proj_h/proj_pop),
    hisp_diff = hisp_annex - hisp_proj_dem,
    min_annex = ((min_total_1 + proj_min)/(proj_pop + pop_total_1)),
    min_proj_dem = (proj_min/proj_pop),
    min_diff = min_annex - min_proj_dem,
    white_annex = ((nhwhite_total_1 + proj_nhwhite)/(proj_pop + pop_total_1)) ,
    white_proj_dem = (proj_nhwhite/proj_pop),
    white_diff = white_annex - white_proj_dem,
    nhblackvap_proj_dem = (proj_nhblackvap/proj_vap),
    nhblackvap_annex = ((nhblackvap_total_1 + proj_nhblackvap)/(proj_vap + vap_total_1)),
    nhblackvap_diff = nhblackvap_annex - nhblackvap_proj_dem,
    hispvap_annex = ((hvap_total_1 + proj_hvap)/(proj_vap + pop_total_1)),
    hispvap_proj_dem = (proj_hvap/proj_vap),
    hispvap_diff = hispvap_annex - hispvap_proj_dem,
    minvap_annex = ((minorityvap_total_1 + proj_minvap)/(proj_vap + pop_total_1)),
    minvap_proj_dem = (proj_minvap/proj_vap),
    minvap_diff = minvap_annex - minvap_proj_dem,
    whitevap_annex = ((nhwhitevap_total_1 + proj_nhwhitevap)/(proj_vap + pop_total_1)),
    whitevap_proj_dem = (proj_nhwhitevap/proj_vap),
    whitevap_diff = whitevap_annex - whitevap_proj_dem
  )

indices <- c(grep("_diff", names(panel0020_did)))
indices <- names(panel0020_did)[indices]
indices <- indices[!grepl("vap_diff", indices)]

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
Pre- and Post-Shelby Trends by Race, Including NE States")
did_vis_total
ggsave(filename = "analyticalfiles/did_vis_total_NE.png",
       plot = did_vis_total,
       dpi = 300)

vapindices <- c(grep("*vap_diff", names(panel0020_did)))
vapindices <- names(panel0020_did)[vapindices]

did_vis_vap <- panel0020_did %>%
  #filter(annexing==1) %>%
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

did_vis_vap <- ggplot(did_vis_vap,
                      aes(y = mean, x = post, color = vra)) + 
  geom_line() + scale_x_continuous(breaks = c(-1, 0, 1)) + 
  geom_vline(xintercept = 0, linetype="dotted", 
             color = "red", size=0.5) +
  facet_grid(~Race, scales = "free") + 
  labs(color = "Covered Under \n Section V",
       x = "Period Relative to Shelby v. Holder", 
       y = "Mean Difference", 
       title = "Differences in Post-Annexation Demographic Composition and 
Projected Compositions Without Annexation, VAP,
Pre- and Post-Shelby Trends by Race")
did_vis_vap
ggsave(filename = "analyticalfiles/did_vis_total.png",
       plot = did_vis_total,
       dpi = 300)

NE <- c("09", "23", "25", "33", "34", "36", "42", "24", "44", "50", "15")
table(panel0020_did$STATE %in% NE)
# 1869 annexing cities 