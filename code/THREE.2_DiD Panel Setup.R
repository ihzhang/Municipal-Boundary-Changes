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
  mutate(proj_growth_white = (((nhwhitegrowth/10)*3)/100)+1,
         proj_growth_black = (((nhblackgrowth/10)*3)/100)+1,
         proj_growth_h = (((hgrowth/10)*3)/100)+1,
         proj_growth_min = (((mingrowth/10)*3)/100)+1,
         proj_growth_whitevap = (((nhwhitevapgrowth/10)*3)/100)+1, 
         proj_growth_blackvap = (((nhblackvapgrowth/10)*3)/100)+1,
         proj_growth_hvap = (((hispvapgrowth/10)*3)/100)+1,
         proj_growth_minvap = (((minvapgrowth/10)*3)/100)+1,
         proj_pop = pop14p*((((popgrowth/10)*3)/100)+1),
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
                                ifelse(((hinc14p-hinc10p*1.25)/(hinc10p*1.25)) < 0, 1, 0))
  ) %>%
  select(plid, c(contains("proj")), densifying, economic_need, c(contains("growth")), -c(contains("_growth")))

table(pl_annex_var_1420$plid %in% pl1014$plid) #7828 false (damn)

pl_annex_var_1420 %<>%
  filter(plid %in% pl1014$plid) %>%
  left_join(pl1014, by = "plid") %>%
  mutate(post = 1)

table(pl_annex_var_1420$annexing)

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

# make panel data!!!!! ####
pl1013 <- read_csv("analyticalfiles/pl_annex_var_1013.csv")
pl1420 <- read_csv("analyticalfiles/pl_annex_var_1420.csv")

pl1013 %<>%
  filter(plid %in% pl1420$plid)

pl1420 %<>%
  filter(plid %in% pl1013$plid)

panel1020_did <- base::rbind(
    pl1013, pl1420
)

write_csv(panel1020_did, "analyticalfiles/panel1020_did.csv")

z <- function(var_name) {
  (var_name - mean(var_name, na.rm = T))/sd(var_name, na.rm = T)
}

panel1020_did %<>%
  mutate(STATEA = as.character(substr(plid, 1, 2)),
         recimmgrowth_b = ifelse(recimmgrowth > 0, 1, 0),
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

# test reg
testdid <- fixest::feols(underbound_black ~ vra + post + vra*post + 
                           pct_annexed_z + popgrowth_z + recimmgrowth_z +
                           incomegrowth_z + nhblackgrowth_z | plid + STATEA, data = panel1020_did)
testdid2 <- fixest::feols(underbound_hisp ~ vra + post + vra*post | plid + STATEA, data = panel1020_did)
testdid3 <- fixest::feols(underbound_minority ~ vra + post + vra*post | plid + STATEA, data = panel1020_did)
testdid4 <- fixest::feols(overbound_white ~ vra + post + vra*post | plid, data = panel1020_did)

summary(testdid)
summary(testdid2)
summary(testdid3)
summary(testdid4)

vapdid <- fixest::feols(underbound_blackvap ~ vra + post + vra*post + 
                          log(pop_total + 1) + pct_annexed + log(popgrowth + 1) + log(recimmgrowth + 1) +
                          log(incomegrowth + 1) + log(blackpovgrowth + 1) + log(nhblackgrowth + 1) | plid, data = panel1020_did)
vapdid2 <- fixest::feols(underbound_hispvap ~ vra + post + vra*post | plid, data = panel1020_did)
vapdid3 <- fixest::feols(underbound_minorityvap ~ vra + post + vra*post | plid, data = panel1020_did)
vapdid4 <- fixest::feols(overbound_whitevap ~ vra + post + vra*post | plid, data = panel1020_did)

summary(vapdid)
summary(vapdid2)
summary(vapdid3)
summary(vapdid4)

indices <- c(grep("underbound", names(panel1020_did)), grep("overbound", names(panel1020_did)))
indices <- names(panel1020_did)[indices]

did_vis <- panel1020_did %>%
  group_by(vra, post) %>%
  summarise_at(all_of(indices), 
               ~mean(., na.rm = T))
  

ggplot(did_vis %>%
           select(vra, post, contains("black")) %>%
           pivot_longer(cols = starts_with("underbound"),
                        names_to = "underbound", 
                        values_to = "mean") %>%
           mutate(underbound = gsub("underbound_", "", underbound),
                  vra = as.character(vra)), 
         aes(y = mean, x = post, color = vra)) + 
  geom_line() + 
  facet_wrap(~underbound)
  
  
  
  
  

