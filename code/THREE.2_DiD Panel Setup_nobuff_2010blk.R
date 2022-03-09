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
library("magrittr")
library("openxlsx")
library("broom")

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
  filter(annexed==1) %>%
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

# pl0010 %<>%
#   filter(
#     is.finite(popgrowth) & 
#       is.finite(nhwhitegrowth) & 
#       is.finite(nhwhitevapgrowth) &
#       is.finite(nhblackgrowth) & 
#       is.finite(nhblackvapgrowth) &
#       is.finite(hgrowth) & 
#       is.finite(hispvapgrowth) &
#       is.finite(mingrowth)
#   ) %>%
#   mutate(vapgrowth = ((vap10p-vap00p)/vap10p)*100,
#          proj_growth_vap = (((vapgrowth/10)*3)/100)+1,
#     proj_growth_white = (((nhwhitegrowth/10)*3)/100)+1,
#          proj_growth_black = (((nhblackgrowth/10)*3)/100)+1,
#          proj_growth_h = (((hgrowth/10)*3)/100)+1,
#          proj_growth_min = (((mingrowth/10)*3)/100)+1,
#          proj_growth_whitevap = (((nhwhitevapgrowth/10)*3)/100)+1, 
#          proj_growth_blackvap = (((nhblackvapgrowth/10)*3)/100)+1,
#          proj_growth_hvap = (((hispvapgrowth/10)*3)/100)+1,
#          proj_pop = pop10p*((((popgrowth/10)*3)/100)+1),
#          proj_vap = vap10p*proj_growth_vap,
#          proj_nhwhite = nhwhite10p*proj_growth_white,
#          proj_nhblack = nhblack10p*proj_growth_black,
#          proj_h = h10p*proj_growth_h,
#          proj_nhwhitevap = nhwhitevap10p*proj_growth_whitevap,
#          proj_nhblackvap = nhblackvap10p*proj_growth_blackvap,
#          proj_hvap = hispvap10p*proj_growth_hvap,
#          densifying = ifelse(is.na(densification), NA,
#                              ifelse(densification > 0, 1, 0)),
#          economic_need = ifelse(is.na(hinc10p), NA,
#                                 ifelse(((hinc10p-hinc00p*1.25)/(hinc00p*1.25)) < 0, 1, 0))
#   ) %>%
#   select(plid, c(contains("proj")), c(contains("growth")), -c(contains("_growth")), vraa, c(contains("vap")))

table(pl_annex_var_1013$plid %in% pl0010$plid) #4695 false

pl_annex_var_1013 %<>%
  filter(plid %in% pl0010$plid) %>%
  left_join(pl0010, by = "plid") %>%
  mutate(post = 0) %>%
  group_by(substr(plid, 1, 2)) %>%
  mutate(pop_tercile = ntile(pop10p, 3),
         maj_white = ifelse(pctnhwhitevap10p >= 0.5, 1, 0)) %>%
  ungroup()

table(pl_annex_var_1013$annexing)
table(pl_annex_var_1013$maj_white)

# make underbound variable
# # of black_annexed(2013) + blackvap(2010)/(vap2000 + annexed_vap2013) - blackvap2010/
pl_annex_var_1013 %<>%
  mutate(
    increase_white = ifelse((annexing == 1 & ((((nhwhitevap_total_1 + nhwhitevap10p)/(vap10p + vap_total_1)) - (nhwhitevap10p/vap10p))) > 0), 1, 0),
    underbound_blackvap_vraa = ifelse(
      (annexing == 1 & vraa == 1 & ((((nhblackvap_total_1 + nhblackvap10p)/(vap_total_1 + vap10p)) - (nhblackvap10p/vap10p))) < -0.03), 1, 0), 
    underbound_hispvap_vraa = ifelse(
      (annexing == 1 & vraa == 1 & ((((hvap_total_1 + hispvap10p)/(vap_total_1 + vap10p)) - (hispvap10p/vap10p))) < -0.03), 1, 0),
    underbound_asianvap_vraa = ifelse(
      (annexing == 1 & vraa == 1 & ((((asianvap_total_1 + asianvap10p)/(vap10p + vap_total_1)) - (asianvap10p/vap10p))) < -0.03), 1, 0), 
    underbound_nativevap_vraa = ifelse(
      (annexing == 1 & vraa == 1 & ((((nativevap_total_1 + nativevap10p)/(vap10p + vap_total_1)) - (nativevap10p/vap10p))) < -0.03), 1, 0),
    underbound_othervap_vraa = ifelse(
      (annexing == 1 & vraa == 1 & ((((othervap_total_1 + othervap10p)/(vap10p + vap_total_1)) - (othervap10p/vap10p))) < -0.03), 1, 0),
    underbound_nhwhitevap_vraa = ifelse(
      (annexing == 1 & vraa == 1 & ((((nhwhitevap_total_1 + nhwhitevap10p)/(vap10p + vap_total_1)) - (nhwhitevap10p/vap10p))) < -0.03), 1, 0),
    underbound_blackvap_3pct = ifelse(
      (annexing == 1 & increase_white == 1 & ((((nhblackvap_total_1 + nhblackvap10p)/(vap_total_1 + vap10p)) - (nhblackvap10p/vap10p))) < -0.03), 1, 0), 
    underbound_hispvap_3pct = ifelse(
      (annexing == 1 & increase_white == 1 & ((((hvap_total_1 + hispvap10p)/(vap_total_1 + vap10p)) - (hispvap10p/vap10p))) < -0.03), 1, 0),
    underbound_asianvap_3pct = ifelse(
      (annexing == 1 & increase_white == 1 & ((((asianvap_total_1 + asianvap10p)/(vap10p + vap_total_1)) - (asianvap10p/vap10p))) < -0.03), 1, 0), 
    underbound_nativevap_3pct = ifelse(
      (annexing == 1 & increase_white == 1 & ((((nativevap_total_1 + nativevap10p)/(vap10p + vap_total_1)) - (nativevap10p/vap10p))) < -0.03), 1, 0),
    underbound_othervap_3pct = ifelse(
      (annexing == 1 & increase_white == 1 & ((((othervap_total_1 + othervap10p)/(vap10p + vap_total_1)) - (othervap10p/vap10p))) < -0.03), 1, 0),
    underbound_nhwhitevap_3pct = ifelse(
      (annexing == 1 & ((((nhwhitevap_total_1 + nhwhitevap10p)/(vap10p + vap_total_1)) - (nhwhitevap10p/vap10p))) < -0.03), 1, 0),
    underbound_blackvap_hpct = ifelse(
      (annexing == 1 & increase_white == 1 & ((((nhblackvap_total_1 + nhblackvap10p)/(vap_total_1 + vap10p)) - (nhblackvap10p/vap10p))) < -0.005), 1, 0), 
    underbound_hispvap_hpct = ifelse(
      (annexing == 1 & increase_white == 1 & ((((hvap_total_1 + hispvap10p)/(vap_total_1 + vap10p)) - (hispvap10p/vap10p))) < -0.005), 1, 0),
    underbound_asianvap_hpct = ifelse(
      (annexing == 1 & increase_white == 1 & ((((asianvap_total_1 + asianvap10p)/(vap10p + vap_total_1)) - (asianvap10p/vap10p))) < -0.005), 1, 0), 
    underbound_nativevap_hpct = ifelse(
      (annexing == 1 & increase_white == 1 & ((((nativevap_total_1 + nativevap10p)/(vap10p + vap_total_1)) - (nativevap10p/vap10p))) < -0.005), 1, 0),
    underbound_othervap_hpct = ifelse(
      (annexing == 1 & increase_white == 1 & ((((othervap_total_1 + othervap10p)/(vap10p + vap_total_1)) - (othervap10p/vap10p))) < -0.005), 1, 0),
    underbound_nhwhitevap_hpct = ifelse(
      (annexing == 1 & ((((nhwhitevap_total_1 + nhwhitevap10p)/(vap10p + vap_total_1)) - (nhwhitevap10p/vap10p))) < -0.005), 1, 0),
    underbound_blackvap_1pct = ifelse(
      (annexing == 1 & increase_white == 1 & ((((nhblackvap_total_1 + nhblackvap10p)/(vap_total_1 + vap10p)) - (nhblackvap10p/vap10p))) < -0.01), 1, 0), 
    underbound_hispvap_1pct = ifelse(
      (annexing == 1 & increase_white == 1 & ((((hvap_total_1 + hispvap10p)/(vap_total_1 + vap10p)) - (hispvap10p/vap10p))) < -0.01), 1, 0),
    underbound_asianvap_1pct = ifelse(
      (annexing == 1 & increase_white == 1 & ((((asianvap_total_1 + asianvap10p)/(vap10p + vap_total_1)) - (asianvap10p/vap10p))) < -0.01), 1, 0), 
    underbound_nativevap_1pct = ifelse(
      (annexing == 1 & increase_white == 1 & ((((nativevap_total_1 + nativevap10p)/(vap10p + vap_total_1)) - (nativevap10p/vap10p))) < -0.01), 1, 0),
    underbound_othervap_1pct = ifelse(
      (annexing == 1 & increase_white == 1 & ((((othervap_total_1 + othervap10p)/(vap10p + vap_total_1)) - (othervap10p/vap10p))) < -0.01), 1, 0),
    underbound_nhwhitevap_1pct = ifelse(
      (annexing == 1 & ((((nhwhitevap_total_1 + nhwhitevap10p)/(vap10p + vap_total_1)) - (nhwhitevap10p/vap10p))) < -0.01), 1, 0)
  )

pl_annex_var_1013 %<>%
  mutate(
    underbound_blackvap = ifelse(
      (annexing == 1 & increase_white == 1 & ((((nhblackvap_total_1 + nhblackvap10p)/(vap_total_1 + vap10p)) < (nhblackvap10p/vap10p)))), 1, 0), 
    underbound_hispvap = ifelse(
      (annexing == 1 & increase_white == 1 & ((((hvap_total_1 + hispvap10p)/(vap_total_1 + vap10p)) < (hispvap10p/vap10p)))), 1, 0),
    underbound_asianvap = ifelse(
      (annexing == 1 & increase_white == 1 & ((((asianvap_total_1 + asianvap10p)/(vap10p + vap_total_1)) < (asianvap10p/vap10p)))), 1, 0), 
    underbound_nativevap = ifelse(
      (annexing == 1 & increase_white == 1 & ((((nativevap_total_1 + nativevap10p)/(vap10p + vap_total_1)) < (nativevap10p/vap10p)))), 1, 0),
    underbound_othervap = ifelse(
      (annexing == 1 & increase_white == 1 & ((((othervap_total_1 + othervap10p)/(vap10p + vap_total_1)) < (othervap10p/vap10p)))), 1, 0),
    underbound_nhwhitevap = ifelse(
      (annexing == 1 & ((((nhwhitevap_total_1 + nhwhitevap10p)/(vap10p + vap_total_1)) < (nhwhitevap10p/vap10p)))), 1, 0)
  )

pl_annex_var_1013 %<>%
  mutate(drop = ifelse(
    vap_total == 0, 1, 0
  ))
table(pl_annex_var_1013$drop)
pl_annex_var_1013 %<>%
  filter(drop==0) %>%
  select(-drop)

table(pl_annex_var_1013$underbound_blackvap)
table(pl_annex_var_1013$underbound_hispvap)
table(pl_annex_var_1013$underbound_asianvap)
table(pl_annex_var_1013$underbound_nativevap)
table(pl_annex_var_1013$underbound_othervap)
table(pl_annex_var_1013$underbound_nhwhitevap)
table(pl_annex_var_1013$underbound_blackvap_vraa)
table(pl_annex_var_1013$underbound_hispvap_vraa)
table(pl_annex_var_1013$underbound_asianvap_vraa)
table(pl_annex_var_1013$underbound_nativevap_vraa)
table(pl_annex_var_1013$underbound_othervap_vraa)
table(pl_annex_var_1013$underbound_nhwhitevap_vraa)
table(pl_annex_var_1013$underbound_blackvap_3pct)
table(pl_annex_var_1013$underbound_hispvap_3pct)
table(pl_annex_var_1013$underbound_asianvap_3pct)
table(pl_annex_var_1013$underbound_nativevap_3pct)
table(pl_annex_var_1013$underbound_othervap_3pct)
table(pl_annex_var_1013$underbound_nhwhitevap_3pct)
table(pl_annex_var_1013$underbound_blackvap_hpct)
table(pl_annex_var_1013$underbound_hispvap_hpct)
table(pl_annex_var_1013$underbound_asianvap_hpct)
table(pl_annex_var_1013$underbound_nativevap_hpct)
table(pl_annex_var_1013$underbound_othervap_hpct)
table(pl_annex_var_1013$underbound_nhwhitevap_hpct)
table(pl_annex_var_1013$underbound_blackvap_1pct)
table(pl_annex_var_1013$underbound_hispvap_1pct)
table(pl_annex_var_1013$underbound_asianvap_1pct)
table(pl_annex_var_1013$underbound_nativevap_1pct)
table(pl_annex_var_1013$underbound_othervap_1pct)
table(pl_annex_var_1013$underbound_nhwhitevap_1pct)

write_csv(pl_annex_var_1013, "analyticalfiles/pl_annex_var_1013.csv")
pl_annex_var_1013 <- read_csv("analyticalfiles/pl_annex_var_1013.csv")

desc1013 <- pl_annex_var_1013 %>%
  group_by(pop_tercile) %>%
  summarize_at(vars(c(contains("underbound"))), ~mean(., na.rm = T)) %>%
  mutate(Year = "2010 to 2013") %>%
  pivot_longer(cols = !c(Year, pop_tercile),
               names_to = "underbound") %>%
  mutate(vra_basis = case_when(
    grepl("vraa", underbound) ~ "JLVRAA",
    grepl("_hpct", underbound) ~ "VRA, 0.5%",
    grepl("_1pct", underbound) ~ "VRA, 1%",
    grepl("_3pct", underbound) ~ "VRA, 3%",
    TRUE ~ "VRA, 0%"
  ),
         race = case_when(
           grepl("black", underbound) ~ "Non-Hispanic Black",
           grepl("hisp", underbound) ~ "Hispanic",
           grepl("native", underbound) ~ "Non-Hispanic Native",
           grepl("white", underbound) ~ "Non-Hispanic White",
           grepl("asian", underbound) ~ "Non-Hispanic Asian",
           grepl("other", underbound) ~ "Non-Hispanic Other"
         ))

rm(aa1013, pl_annex_var_1013, pl0010)

#repeat for 1417 ####
aa1417 <- read_csv("analyticalfiles/annexedblocks1417dem_pl00_newsample_unincorp.csv")

# transform this into place-level summaries
# characteristic of all annexable blocks
# diff between annexed and not annexed 
place_all <- aa1417 %>% 
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

place_by_annex <- aa1417 %>%
  filter(annexed==1) %>%
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

pl_annex_var_1417 <- left_join(
  place_all, place_by_annex, 
  by = "plid"
)

# add vra indicator 
places_vra <- aa1417 %>%
  group_by(plid) %>%
  summarize(vra = mean(vra, na.rm = T),
            annexing = mean(annexed, na.rm = T)) %>%
  ungroup() %>%
  mutate(vra = ifelse(vra > 0, 1, 0),
         annexing = ifelse(annexing > 0, 1, 0))

pl_annex_var_1417 %<>%
  left_join(places_vra, by = "plid")

# add place-level data for race and vap 
# vra violation = if pctwhite after annex > pctwhite before annex compared to
# what would have been, i.e. pctwhite given growth rate by 2013
# or if pctblack after annex < pctblack before annex compared to what would 
# have been, i.e. pctwhite given growth rate by 2013
# or if pcth after annex < pcth before annex compared to what would have been,
# i.e. pcth given growth rate by 2013 
pl1014 <- read_csv("pl1014_var.csv")

# pl1014 %<>%
#   filter(
#     is.finite(popgrowth) & 
#       is.finite(nhwhitegrowth) & 
#       is.finite(nhwhitevapgrowth) &
#       is.finite(nhblackgrowth) & 
#       is.finite(nhblackvapgrowth) &
#       is.finite(hgrowth) & 
#       is.finite(hispvapgrowth) &
#       is.finite(mingrowth)
#   ) %>%
#   mutate(proj_growth_white = (((nhwhitegrowth/4)*6)/100)+1,
#          proj_growth_black = (((nhblackgrowth/4)*6)/100)+1,
#          proj_growth_h = (((hgrowth/4)*6)/100)+1,
#          proj_growth_whitevap = (((nhwhitevapgrowth/4)*6)/100)+1, 
#          proj_growth_blackvap = (((nhblackvapgrowth/4)*6)/100)+1,
#          proj_growth_hvap = (((hispvapgrowth/4)*6)/100)+1,
#          proj_pop = pop14p*((((popgrowth/4)*6)/100)+1),
#          vapgrowth = ((vap14p-vap10p)/vap10p)*100,
#          proj_growth_vap = (((vapgrowth/4)*6)/100)+1,
#          proj_vap = vap14p*proj_growth_vap,
#          proj_nhwhite = nhwhite14p*proj_growth_white,
#          proj_nhblack = nhblack14p*proj_growth_black,
#          proj_h = h14p*proj_growth_h,
#          proj_nhwhitevap = nhwhitevap14p*proj_growth_whitevap,
#          proj_nhblackvap = nhblackvap14p*proj_growth_blackvap,
#          proj_hvap = hispvap14p*proj_growth_hvap,
#          densifying = ifelse(is.na(densification), NA,
#                              ifelse(densification > 0, 1, 0)),
#          economic_need = ifelse(is.na(hinc14p), NA,
#                                 ifelse(((hinc14p-hinc10p*1.25)/(hinc10p*1.25)) < 0, 1, 0))
#   ) %>%
#   select(plid, c(contains("proj")), densifying, economic_need, c(contains("growth")), -c(contains("_growth")), vraa, c(contains("vap")))

table(pl_annex_var_1417$plid %in% pl1014$plid) 

pl_annex_var_1417 %<>%
  filter(plid %in% pl1014$plid) %>%
  left_join(pl1014, by = "plid") %>%
  mutate(post = 1) %>%
  group_by(substr(plid, 1, 2)) %>%
  mutate(pop_tercile = ntile(pop14p, 3),
         maj_white = ifelse(pctnhwhitevap14p >= 0.5, 1, 0)) %>%
  ungroup()

table(pl_annex_var_1417$annexing)
table(pl_annex_var_1417$maj_white)

# make underbound variable
pl_annex_var_1417 %<>%
  mutate(
    increase_white = ifelse((annexing == 1 & ((((nhwhitevap_total_1 + nhwhitevap14p)/(vap14p + vap_total_1)) - (nhwhitevap14p/vap14p))) > 0), 1, 0),
    underbound_blackvap_vraa = ifelse(
      (annexing == 1 & vraa == 1 & ((((nhblackvap_total_1 + nhblackvap14p)/(vap_total_1 + vap14p)) - (nhblackvap14p/vap14p))) < -0.03), 1, 0), 
    underbound_hispvap_vraa = ifelse(
      (annexing == 1 & vraa == 1 & ((((hvap_total_1 + hispvap14p)/(vap_total_1 + vap14p)) - (hispvap14p/vap14p))) < -0.03), 1, 0),
    underbound_asianvap_vraa = ifelse(
      (annexing == 1 & vraa == 1 & ((((asianvap_total_1 + asianvap14p)/(vap14p + vap_total_1)) - (asianvap14p/vap14p))) < -0.03), 1, 0), 
    underbound_nativevap_vraa = ifelse(
      (annexing == 1 & vraa == 1 & ((((nativevap_total_1 + nativevap14p)/(vap14p + vap_total_1)) - (nativevap14p/vap14p))) < -0.03), 1, 0),
    underbound_othervap_vraa = ifelse(
      (annexing == 1 & vraa == 1 & ((((othervap_total_1 + othervap14p)/(vap14p + vap_total_1)) - (othervap14p/vap14p))) < -0.03), 1, 0),
    underbound_nhwhitevap_vraa = ifelse(
      (annexing == 1 & vraa == 1 & ((((nhwhitevap_total_1 + nhwhitevap14p)/(vap14p + vap_total_1)) - (nhwhitevap14p/vap14p))) < -0.03), 1, 0),
    underbound_blackvap_3pct = ifelse(
      (annexing == 1 & increase_white == 1 & ((((nhblackvap_total_1 + nhblackvap14p)/(vap_total_1 + vap14p)) - (nhblackvap14p/vap14p))) < -0.03), 1, 0), 
    underbound_hispvap_3pct = ifelse(
      (annexing == 1 & increase_white == 1 & ((((hvap_total_1 + hispvap14p)/(vap_total_1 + vap14p)) - (hispvap14p/vap14p))) < -0.03), 1, 0),
    underbound_asianvap_3pct = ifelse(
      (annexing == 1 & increase_white == 1 & ((((asianvap_total_1 + asianvap14p)/(vap14p + vap_total_1)) - (asianvap14p/vap14p))) < -0.03), 1, 0), 
    underbound_nativevap_3pct = ifelse(
      (annexing == 1 & increase_white == 1 & ((((nativevap_total_1 + nativevap14p)/(vap14p + vap_total_1)) - (nativevap14p/vap14p))) < -0.03), 1, 0),
    underbound_othervap_3pct = ifelse(
      (annexing == 1 & increase_white == 1 & ((((othervap_total_1 + othervap14p)/(vap14p + vap_total_1)) - (othervap14p/vap14p))) < -0.03), 1, 0),
    underbound_nhwhitevap_3pct = ifelse(
      (annexing == 1 & ((((nhwhitevap_total_1 + nhwhitevap14p)/(vap14p + vap_total_1)) - (nhwhitevap14p/vap14p))) < -0.03), 1, 0),
    underbound_blackvap_hpct = ifelse(
      (annexing == 1 & increase_white == 1 & ((((nhblackvap_total_1 + nhblackvap14p)/(vap_total_1 + vap14p)) - (nhblackvap14p/vap14p))) < -0.005), 1, 0), 
    underbound_hispvap_hpct = ifelse(
      (annexing == 1 & increase_white == 1 & ((((hvap_total_1 + hispvap14p)/(vap_total_1 + vap14p)) - (hispvap14p/vap14p))) < -0.005), 1, 0),
    underbound_asianvap_hpct = ifelse(
      (annexing == 1 & increase_white == 1 & ((((asianvap_total_1 + asianvap14p)/(vap14p + vap_total_1)) - (asianvap14p/vap14p))) < -0.005), 1, 0), 
    underbound_nativevap_hpct = ifelse(
      (annexing == 1 & increase_white == 1 & ((((nativevap_total_1 + nativevap14p)/(vap14p + vap_total_1)) - (nativevap14p/vap14p))) < -0.005), 1, 0),
    underbound_othervap_hpct = ifelse(
      (annexing == 1 & increase_white == 1 & ((((othervap_total_1 + othervap14p)/(vap14p + vap_total_1)) - (othervap14p/vap14p))) < -0.005), 1, 0),
    underbound_nhwhitevap_hpct = ifelse(
      (annexing == 1 & ((((nhwhitevap_total_1 + nhwhitevap14p)/(vap14p + vap_total_1)) - (nhwhitevap14p/vap14p))) < -0.005), 1, 0),
    underbound_blackvap_1pct = ifelse(
      (annexing == 1 & increase_white == 1 & ((((nhblackvap_total_1 + nhblackvap14p)/(vap_total_1 + vap14p)) - (nhblackvap14p/vap14p))) < -0.01), 1, 0), 
    underbound_hispvap_1pct = ifelse(
      (annexing == 1 & increase_white == 1 & ((((hvap_total_1 + hispvap14p)/(vap_total_1 + vap14p)) - (hispvap14p/vap14p))) < -0.01), 1, 0),
    underbound_asianvap_1pct = ifelse(
      (annexing == 1 & increase_white == 1 & ((((asianvap_total_1 + asianvap14p)/(vap14p + vap_total_1)) - (asianvap14p/vap14p))) < -0.01), 1, 0), 
    underbound_nativevap_1pct = ifelse(
      (annexing == 1 & increase_white == 1 & ((((nativevap_total_1 + nativevap14p)/(vap14p + vap_total_1)) - (nativevap14p/vap14p))) < -0.01), 1, 0),
    underbound_othervap_1pct = ifelse(
      (annexing == 1 & increase_white == 1 & ((((othervap_total_1 + othervap14p)/(vap14p + vap_total_1)) - (othervap14p/vap14p))) < -0.01), 1, 0),
    underbound_nhwhitevap_1pct = ifelse(
      (annexing == 1 & ((((nhwhitevap_total_1 + nhwhitevap14p)/(vap14p + vap_total_1)) - (nhwhitevap14p/vap14p))) < -0.01), 1, 0)
  )

pl_annex_var_1417 %<>%
  mutate(
    underbound_blackvap = ifelse(
      (annexing == 1 & increase_white == 1 & ((((nhblackvap_total_1 + nhblackvap14p)/(vap_total_1 + vap14p)) < (nhblackvap14p/vap14p)))), 1, 0), 
    underbound_hispvap = ifelse(
      (annexing == 1 & increase_white == 1 & ((((hvap_total_1 + hispvap14p)/(vap_total_1 + vap14p)) < (hispvap14p/vap14p)))), 1, 0),
    underbound_asianvap = ifelse(
      (annexing == 1 & increase_white == 1 & ((((asianvap_total_1 + asianvap14p)/(vap14p + vap_total_1)) < (asianvap14p/vap14p)))), 1, 0), 
    underbound_nativevap = ifelse(
      (annexing == 1 & increase_white == 1 & ((((nativevap_total_1 + nativevap14p)/(vap14p + vap_total_1)) < (nativevap14p/vap14p)))), 1, 0),
    underbound_othervap = ifelse(
      (annexing == 1 & increase_white == 1 & ((((othervap_total_1 + othervap14p)/(vap14p + vap_total_1)) < (othervap14p/vap14p)))), 1, 0),
    underbound_nhwhitevap = ifelse(
      (annexing == 1 & ((((nhwhitevap_total_1 + nhwhitevap14p)/(vap14p + vap_total_1)) < (nhwhitevap14p/vap14p)))), 1, 0)
  )


pl_annex_var_1417 %<>%
  mutate(drop = ifelse(
    vap_total == 0, 1, 0
  ))
table(pl_annex_var_1417$drop)

pl_annex_var_1417 %<>%
  filter(drop==0) %>%
  select(-drop)

table(pl_annex_var_1417$underbound_blackvap)
table(pl_annex_var_1417$underbound_hispvap)
table(pl_annex_var_1417$underbound_nativevap)
table(pl_annex_var_1417$underbound_asianvap)
table(pl_annex_var_1417$underbound_othervap)
table(pl_annex_var_1417$underbound_nhwhitevap)
table(pl_annex_var_1417$underbound_blackvap_vraa)
table(pl_annex_var_1417$underbound_hispvap_vraa)
table(pl_annex_var_1417$underbound_nativevap_vraa)
table(pl_annex_var_1417$underbound_asianvap_vraa)
table(pl_annex_var_1417$underbound_othervap_vraa)
table(pl_annex_var_1417$underbound_nhwhitevap_vraa)
table(pl_annex_var_1417$underbound_blackvap_hpct)
table(pl_annex_var_1417$underbound_hispvap_hpct)
table(pl_annex_var_1417$underbound_nativevap_hpct)
table(pl_annex_var_1417$underbound_asianvap_hpct)
table(pl_annex_var_1417$underbound_othervap_hpct)
table(pl_annex_var_1417$underbound_nhwhitevap_hpct)
table(pl_annex_var_1417$underbound_blackvap_1pct)
table(pl_annex_var_1417$underbound_hispvap_1pct)
table(pl_annex_var_1417$underbound_nativevap_1pct)
table(pl_annex_var_1417$underbound_asianvap_1pct)
table(pl_annex_var_1417$underbound_othervap_1pct)
table(pl_annex_var_1417$underbound_nhwhitevap_1pct)
table(pl_annex_var_1417$underbound_blackvap_3pct)
table(pl_annex_var_1417$underbound_hispvap_3pct)
table(pl_annex_var_1417$underbound_nativevap_3pct)
table(pl_annex_var_1417$underbound_asianvap_3pct)
table(pl_annex_var_1417$underbound_othervap_3pct)
table(pl_annex_var_1417$underbound_nhwhitevap_3pct)

write_csv(pl_annex_var_1417, "analyticalfiles/pl_annex_var_1417.csv")
pl_annex_var_1417 <- read_csv("analyticalfiles/pl_annex_var_1417.csv")

desc1417 <- pl_annex_var_1417 %>%
  group_by(pop_tercile) %>%
  summarize_at(vars(c(contains("underbound"))), ~mean(., na.rm = T)) %>%
  mutate(Year = "2014 to 2017") %>%
  pivot_longer(cols = !c(Year, pop_tercile),
               names_to = "underbound") %>%
  mutate(vra_basis = case_when(
    grepl("vraa", underbound) ~ "JLVRAA",
    grepl("_hpct", underbound) ~ "VRA, 0.5%",
    grepl("_1pct", underbound) ~ "VRA, 1%",
    grepl("_3pct", underbound) ~ "VRA, 3%",
    TRUE ~ "VRA, 0%"),
         race = case_when(
           grepl("black", underbound) ~ "Non-Hispanic Black",
           grepl("hisp", underbound) ~ "Hispanic",
           grepl("native", underbound) ~ "Non-Hispanic Native",
           grepl("white", underbound) ~ "Non-Hispanic White",
           grepl("asian", underbound) ~ "Non-Hispanic Asian",
           grepl("other", underbound) ~ "Non-Hispanic Other"
         ))

#rm(list = ls())

# repeat for 1720 ####
aa1720 <- read_csv("analyticalfiles/annexedblocks1720dem_pl00_newsample_unincorp.csv")

# characteristic of all annexable blocks
# diff between annexed and not annexed 
place_all <- aa1720 %>% 
  group_by(plid) %>%
  summarize(pop_total = sum(pop, na.rm = T),
            nhblack_total = sum((pctnhblack*pop), na.rm = T),
            nhblack_mean = mean((nhblack_total/pop_total)*100, na.rm = T),
            nhwhite_total = sum((pctnhwhite*pop), na.rm = T),
            nhwhite_mean = mean((nhwhite_total/pop_total)*100, na.rm = T),
            h_total = sum((pcth*pop), na.rm = T),
            h_mean = mean((h_total/pop_total)*100, na.rm = T),
            vap_total = sum(vap, na.rm = T),
            hu_total = sum(hu, na.rm = T),
            nhblackvap_total = sum(nhbvap, na.rm = T),
            nhblackvap_mean = mean(((nhblackvap_total/vap_total)*100), na.rm = T),
            nhwhitevap_total = sum(nhwvap, na.rm = T),
            nhwhitevap_mean = mean(((nhwhitevap_total/vap_total)*100), na.rm = T),
            hvap_total = sum(hispvap, na.rm = T),
            hvap_mean = mean(((hvap_total/vap_total)*100), na.rm = T),
            nativevap_total = sum(nativevap, na.rm = T),
            asianvap_total = sum(asianvap, na.rm = T),
            othervap_total = sum(othervap, na.rm = T),
            pct_annexed = mean(annexed, na.rm = T)) %>%
  ungroup()

place_by_annex <- aa1720 %>%
  filter(annexed==1) %>%
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

pl_annex_var_1720 <- left_join(
  place_all, place_by_annex, 
  by = "plid"
)

# add vra indicator 
places_vra <- aa1720 %>%
  group_by(plid) %>%
  summarize(vra = mean(vra, na.rm = T),
            annexing = mean(annexed, na.rm = T)) %>%
  ungroup() %>%
  mutate(vra = ifelse(vra > 0, 1, 0),
         annexing = ifelse(annexing > 0, 1, 0))

pl_annex_var_1720 %<>%
  left_join(places_vra, by = "plid")

# add place-level data for race and vap 
# vra violation = if pctwhite after annex > pctwhite before annex compared to
# what would have been, i.e. pctwhite given growth rate by 2013
# or if pctblack after annex < pctblack before annex compared to what would 
# have been, i.e. pctwhite given growth rate by 2013
# or if pcth after annex < pcth before annex compared to what would have been,
# i.e. pcth given growth rate by 2013 
pl1417 <- read_csv("places1417_var.csv")

table(pl_annex_var_1720$plid %in% pl1417$plid) 

pl_annex_var_1720 %<>%
  filter(plid %in% pl1417$plid) %>%
  left_join(pl1417, by = "plid") %>%
  mutate(post = 1) %>%
  group_by(substr(plid, 1, 2)) %>%
  mutate(pop_tercile = ntile(pop17p, 3),
         maj_white = ifelse(pctnhwhitevap17p >= 0.5, 1, 0)) %>%
  ungroup()

table(pl_annex_var_1720$annexing)
# make underbound variable
pl_annex_var_1720 %<>%
  mutate(
    increase_white = ifelse((annexing == 1 & ((((nhwhitevap_total_1 + nhwhitevap17p)/(vap17p + vap_total_1)) - (nhwhitevap17p/vap17p))) > 0), 1, 0),
    underbound_blackvap_vraa = ifelse(
      (annexing == 1 & vraa == 1 & ((((nhblackvap_total_1 + nhblackvap17p)/(vap_total_1 + vap17p)) - (nhblackvap17p/vap17p))) < -0.03), 1, 0), 
    underbound_hispvap_vraa = ifelse(
      (annexing == 1 & vraa == 1 & ((((hvap_total_1 + hispvap17p)/(vap_total_1 + vap17p)) - (hispvap17p/vap17p))) < -0.03), 1, 0),
    underbound_asianvap_vraa = ifelse(
      (annexing == 1 & vraa == 1 & ((((asianvap_total_1 + asianvap17p)/(vap17p + vap_total_1)) - (asianvap17p/vap17p))) < -0.03), 1, 0), 
    underbound_nativevap_vraa = ifelse(
      (annexing == 1 & vraa == 1 & ((((nativevap_total_1 + nativevap17p)/(vap17p + vap_total_1)) - (nativevap17p/vap17p))) < -0.03), 1, 0),
    underbound_othervap_vraa = ifelse(
      (annexing == 1 & vraa == 1 & ((((othervap_total_1 + othervap17p)/(vap17p + vap_total_1)) - (othervap17p/vap17p))) < -0.03), 1, 0),
    underbound_nhwhitevap_vraa = ifelse(
      (annexing == 1 & vraa == 1 & ((((nhwhitevap_total_1 + nhwhitevap17p)/(vap17p + vap_total_1)) - (nhwhitevap17p/vap17p))) < -0.03), 1, 0),
    underbound_blackvap_3pct = ifelse(
      (annexing == 1 & increase_white == 1 & ((((nhblackvap_total_1 + nhblackvap17p)/(vap_total_1 + vap17p)) - (nhblackvap17p/vap17p))) < -0.03), 1, 0), 
    underbound_hispvap_3pct = ifelse(
      (annexing == 1 & increase_white == 1 & ((((hvap_total_1 + hispvap17p)/(vap_total_1 + vap17p)) - (hispvap17p/vap17p))) < -0.03), 1, 0),
    underbound_asianvap_3pct = ifelse(
      (annexing == 1 & increase_white == 1 & ((((asianvap_total_1 + asianvap17p)/(vap17p + vap_total_1)) - (asianvap17p/vap17p))) < -0.03), 1, 0), 
    underbound_nativevap_3pct = ifelse(
      (annexing == 1 & increase_white == 1 & ((((nativevap_total_1 + nativevap17p)/(vap17p + vap_total_1)) - (nativevap17p/vap17p))) < -0.03), 1, 0),
    underbound_othervap_3pct = ifelse(
      (annexing == 1 & increase_white == 1 & ((((othervap_total_1 + othervap17p)/(vap17p + vap_total_1)) - (othervap17p/vap17p))) < -0.03), 1, 0),
    underbound_nhwhitevap_3pct = ifelse(
      (annexing == 1 & ((((nhwhitevap_total_1 + nhwhitevap17p)/(vap17p + vap_total_1)) - (nhwhitevap17p/vap17p))) < -0.03), 1, 0),
    underbound_blackvap_hpct = ifelse(
      (annexing == 1 & increase_white == 1 & ((((nhblackvap_total_1 + nhblackvap17p)/(vap_total_1 + vap17p)) - (nhblackvap17p/vap17p))) < -0.005), 1, 0), 
    underbound_hispvap_hpct = ifelse(
      (annexing == 1 & increase_white == 1 & ((((hvap_total_1 + hispvap17p)/(vap_total_1 + vap17p)) - (hispvap17p/vap17p))) < -0.005), 1, 0),
    underbound_asianvap_hpct = ifelse(
      (annexing == 1 & increase_white == 1 & ((((asianvap_total_1 + asianvap17p)/(vap17p + vap_total_1)) - (asianvap17p/vap17p))) < -0.005), 1, 0), 
    underbound_nativevap_hpct = ifelse(
      (annexing == 1 & increase_white == 1 & ((((nativevap_total_1 + nativevap17p)/(vap17p + vap_total_1)) - (nativevap17p/vap17p))) < -0.005), 1, 0),
    underbound_othervap_hpct = ifelse(
      (annexing == 1 & increase_white == 1 & ((((othervap_total_1 + othervap17p)/(vap17p + vap_total_1)) - (othervap17p/vap17p))) < -0.005), 1, 0),
    underbound_nhwhitevap_hpct = ifelse(
      (annexing == 1 & ((((nhwhitevap_total_1 + nhwhitevap17p)/(vap17p + vap_total_1)) - (nhwhitevap17p/vap17p))) < -0.005), 1, 0),
    underbound_blackvap_1pct = ifelse(
      (annexing == 1 & increase_white == 1 & ((((nhblackvap_total_1 + nhblackvap17p)/(vap_total_1 + vap17p)) - (nhblackvap17p/vap17p))) < -0.01), 1, 0), 
    underbound_hispvap_1pct = ifelse(
      (annexing == 1 & increase_white == 1 & ((((hvap_total_1 + hispvap17p)/(vap_total_1 + vap17p)) - (hispvap17p/vap17p))) < -0.01), 1, 0),
    underbound_asianvap_1pct = ifelse(
      (annexing == 1 & increase_white == 1 & ((((asianvap_total_1 + asianvap17p)/(vap17p + vap_total_1)) - (asianvap17p/vap17p))) < -0.01), 1, 0), 
    underbound_nativevap_1pct = ifelse(
      (annexing == 1 & increase_white == 1 & ((((nativevap_total_1 + nativevap17p)/(vap17p + vap_total_1)) - (nativevap17p/vap17p))) < -0.01), 1, 0),
    underbound_othervap_1pct = ifelse(
      (annexing == 1 & increase_white == 1 & ((((othervap_total_1 + othervap17p)/(vap17p + vap_total_1)) - (othervap17p/vap17p))) < -0.01), 1, 0),
    underbound_nhwhitevap_1pct = ifelse(
      (annexing == 1 & ((((nhwhitevap_total_1 + nhwhitevap17p)/(vap17p + vap_total_1)) - (nhwhitevap17p/vap17p))) < -0.01), 1, 0)
  )

pl_annex_var_1720 %<>%
  mutate(
    underbound_blackvap = ifelse(
      (annexing == 1 & increase_white == 1 & ((((nhblackvap_total_1 + nhblackvap17p)/(vap_total_1 + vap17p)) < (nhblackvap17p/vap17p)))), 1, 0), 
    underbound_hispvap = ifelse(
      (annexing == 1 & increase_white == 1 & ((((hvap_total_1 + hispvap17p)/(vap_total_1 + vap17p)) < (hispvap17p/vap17p)))), 1, 0),
    underbound_asianvap = ifelse(
      (annexing == 1 & increase_white == 1 & ((((asianvap_total_1 + asianvap17p)/(vap17p + vap_total_1)) < (asianvap17p/vap17p)))), 1, 0), 
    underbound_nativevap = ifelse(
      (annexing == 1 & increase_white == 1 & ((((nativevap_total_1 + nativevap17p)/(vap17p + vap_total_1)) < (nativevap17p/vap17p)))), 1, 0),
    underbound_othervap = ifelse(
      (annexing == 1 & increase_white == 1 & ((((othervap_total_1 + othervap17p)/(vap17p + vap_total_1)) < (othervap17p/vap17p)))), 1, 0),
    underbound_nhwhitevap = ifelse(
      (annexing == 1 & ((((nhwhitevap_total_1 + nhwhitevap17p)/(vap17p + vap_total_1)) < (nhwhitevap17p/vap17p)))), 1, 0)
  )

pl_annex_var_1720 %<>%
  mutate(drop = ifelse(
    vap_total == 0, 1, 0
  ))
table(pl_annex_var_1720$drop)

pl_annex_var_1720 %<>%
  filter(drop==0) %>%
  select(-drop)

table(pl_annex_var_1720$underbound_blackvap)
table(pl_annex_var_1720$underbound_hispvap)
table(pl_annex_var_1720$underbound_nativevap)
table(pl_annex_var_1720$underbound_asianvap)
table(pl_annex_var_1720$underbound_othervap)
table(pl_annex_var_1720$underbound_nhwhitevap)
table(pl_annex_var_1720$underbound_blackvap_3pct)
table(pl_annex_var_1720$underbound_hispvap_3pct)
table(pl_annex_var_1720$underbound_nativevap_3pct)
table(pl_annex_var_1720$underbound_asianvap_3pct)
table(pl_annex_var_1720$underbound_othervap_3pct)
table(pl_annex_var_1720$underbound_nhwhitevap_3pct)
table(pl_annex_var_1720$underbound_blackvap_hpct)
table(pl_annex_var_1720$underbound_hispvap_hpct)
table(pl_annex_var_1720$underbound_nativevap_hpct)
table(pl_annex_var_1720$underbound_asianvap_hpct)
table(pl_annex_var_1720$underbound_othervap_hpct)
table(pl_annex_var_1720$underbound_nhwhitevap_hpct)
table(pl_annex_var_1720$underbound_blackvap_1pct)
table(pl_annex_var_1720$underbound_hispvap_1pct)
table(pl_annex_var_1720$underbound_nativevap_1pct)
table(pl_annex_var_1720$underbound_asianvap_1pct)
table(pl_annex_var_1720$underbound_othervap_1pct)
table(pl_annex_var_1720$underbound_nhwhitevap_1pct)

write_csv(pl_annex_var_1720, "analyticalfiles/pl_annex_var_1720.csv")
pl_annex_var_1720 <- read_csv("analyticalfiles/pl_annex_var_1720.csv")

desc1720 <- pl_annex_var_1720 %>%
  group_by(pop_tercile) %>%
  summarize_at(vars(c(contains("underbound"))), ~mean(., na.rm = T)) %>%
  mutate(Year = "2017 to 2020") %>%
  pivot_longer(cols = !c(Year, pop_tercile),
               names_to = "underbound") %>%
  mutate(vra_basis = case_when(
    grepl("vraa", underbound) ~ "JLVRAA",
    grepl("_hpct", underbound) ~ "VRA, 0.5%",
    grepl("_1pct", underbound) ~ "VRA, 1%",
    grepl("_3pct", underbound) ~ "VRA, 3%",
    TRUE ~ "VRA, 0%"),
         race = case_when(
           grepl("black", underbound) ~ "Non-Hispanic Black",
           grepl("hisp", underbound) ~ "Hispanic",
           grepl("native", underbound) ~ "Non-Hispanic Native",
           grepl("white", underbound) ~ "Non-Hispanic White",
           grepl("asian", underbound) ~ "Non-Hispanic Asian",
           grepl("other", underbound) ~ "Non-Hispanic Other"
         ))

#rm(list = ls())

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
  filter(annexed==1) %>%
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

# pl9000 %<>%
#   filter(is.finite(popgrowth) & 
#       is.finite(nhwhitegrowth) & 
#       is.finite(nhwhitevapgrowth) &
#       is.finite(nhblackgrowth) & 
#       is.finite(nhblackvapgrowth) &
#       is.finite(hgrowth) & 
#       is.finite(hispvapgrowth) &
#       is.finite(mingrowth)
#   ) %>%
#   mutate(proj_growth_white = (nhwhitegrowth/100)+1,
#          proj_growth_black = (nhblackgrowth/100)+1,
#          proj_growth_h = (hgrowth/100)+1,
#          proj_growth_min = (mingrowth/100)+1,
#          proj_growth_whitevap = (nhwhitevapgrowth/100)+1, 
#          proj_growth_blackvap = (nhblackvapgrowth/100)+1,
#          proj_growth_hvap = (hispvapgrowth/100)+1,
#          vapgrowth = ((vap00p-vap90p)/vap90p)*100,
#          proj_growth_vap = (vapgrowth/100) + 1,
#          proj_pop = pop00p*((popgrowth/100)+1),
#          proj_vap = vap00p * proj_growth_vap,
#          proj_nhwhite = nhwhite00p*proj_growth_white,
#          proj_nhblack = nhblack00p*proj_growth_black,
#          proj_h = h00p*proj_growth_h,
#          proj_min = min00p*proj_growth_min,
#          proj_nhwhitevap = nhwhitevap00p*proj_growth_whitevap,
#          proj_nhblackvap = nhblackvap00p*proj_growth_blackvap,
#          proj_hvap = hispvap00p*proj_growth_hvap,
#          densifying = ifelse(is.na(densification), NA,
#                              ifelse(densification > 0, 1, 0)),
#          economic_need = ifelse(is.na(hinc00p), NA,
#                                 ifelse(((hinc00p-hinc90p*1.25)/(hinc90p*1.25)) < 0, 1, 0))
#   ) %>%
#   select(plid, c(contains("proj")), densifying, economic_need, c(contains("growth")), -c(contains("_growth")), vraa, c(contains("vap")))

table(pl_annex_var_0010$plid %in% pl9000$plid) 

pl_annex_var_0010 %<>%
  filter(plid %in% pl9000$plid) %>%
  left_join(pl9000, by = "plid") %>%
  mutate(post = -1) %>%
  group_by(substr(plid, 1, 2)) %>%
  mutate(pop_tercile = ntile(pop00p, 3),
         maj_white = ifelse(pctnhwhitevap00p >= 0.5, 1, 0)) %>%
  ungroup()

table(pl_annex_var_0010$annexing)
table(pl_annex_var_0010$maj_white)

# make underbound variable
pl_annex_var_0010 %<>%
  mutate(
    increase_white = ifelse((annexing == 1 & ((((nhwhitevap_total_1 + nhwhitevap00p)/(vap00p + vap_total_1)) - (nhwhitevap00p/vap00p))) > 0), 1, 0),
    underbound_blackvap_vraa = ifelse(
      (annexing == 1 & vraa == 1 & ((((nhblackvap_total_1 + nhblackvap00p)/(vap_total_1 + vap00p)) - (nhblackvap00p/vap00p))) < -0.03), 1, 0), 
    underbound_hispvap_vraa = ifelse(
      (annexing == 1 & vraa == 1 & ((((hvap_total_1 + hispvap00p)/(vap_total_1 + vap00p)) - (hispvap00p/vap00p))) < -0.03), 1, 0),
    underbound_asianvap_vraa = ifelse(
      (annexing == 1 & vraa == 1 & ((((asianvap_total_1 + asianvap00p)/(vap00p + vap_total_1)) - (asianvap00p/vap00p))) < -0.03), 1, 0), 
    underbound_nativevap_vraa = ifelse(
      (annexing == 1 & vraa == 1 & ((((nativevap_total_1 + nativevap00p)/(vap00p + vap_total_1)) - (nativevap00p/vap00p))) < -0.03), 1, 0),
    underbound_othervap_vraa = ifelse(
      (annexing == 1 & vraa == 1 & ((((othervap_total_1 + othervap00p)/(vap00p + vap_total_1)) - (othervap00p/vap00p))) < -0.03), 1, 0),
    underbound_nhwhitevap_vraa = ifelse(
      (annexing == 1 & vraa == 1 & ((((nhwhitevap_total_1 + nhwhitevap00p)/(vap00p + vap_total_1)) - (nhwhitevap00p/vap00p))) < -0.03), 1, 0),
    underbound_blackvap_3pct = ifelse(
      (annexing == 1 & increase_white == 1 & ((((nhblackvap_total_1 + nhblackvap00p)/(vap_total_1 + vap00p)) - (nhblackvap00p/vap00p))) < -0.03), 1, 0), 
    underbound_hispvap_3pct = ifelse(
      (annexing == 1 & increase_white == 1 & ((((hvap_total_1 + hispvap00p)/(vap_total_1 + vap00p)) - (hispvap00p/vap00p))) < -0.03), 1, 0),
    underbound_asianvap_3pct = ifelse(
      (annexing == 1 & increase_white == 1 & ((((asianvap_total_1 + asianvap00p)/(vap00p + vap_total_1)) - (asianvap00p/vap00p))) < -0.03), 1, 0), 
    underbound_nativevap_3pct = ifelse(
      (annexing == 1 & increase_white == 1 & ((((nativevap_total_1 + nativevap00p)/(vap00p + vap_total_1)) - (nativevap00p/vap00p))) < -0.03), 1, 0),
    underbound_othervap_3pct = ifelse(
      (annexing == 1 & increase_white == 1 & ((((othervap_total_1 + othervap00p)/(vap00p + vap_total_1)) - (othervap00p/vap00p))) < -0.03), 1, 0),
    underbound_nhwhitevap_3pct = ifelse(
      (annexing == 1 & ((((nhwhitevap_total_1 + nhwhitevap00p)/(vap00p + vap_total_1)) - (nhwhitevap00p/vap00p))) < -0.03), 1, 0),
    underbound_blackvap_hpct = ifelse(
      (annexing == 1 & increase_white == 1 & ((((nhblackvap_total_1 + nhblackvap00p)/(vap_total_1 + vap00p)) - (nhblackvap00p/vap00p))) < -0.005), 1, 0), 
    underbound_hispvap_hpct = ifelse(
      (annexing == 1 & increase_white == 1 & ((((hvap_total_1 + hispvap00p)/(vap_total_1 + vap00p)) - (hispvap00p/vap00p))) < -0.005), 1, 0),
    underbound_asianvap_hpct = ifelse(
      (annexing == 1 & increase_white == 1 & ((((asianvap_total_1 + asianvap00p)/(vap00p + vap_total_1)) - (asianvap00p/vap00p))) < -0.005), 1, 0), 
    underbound_nativevap_hpct = ifelse(
      (annexing == 1 & increase_white == 1 & ((((nativevap_total_1 + nativevap00p)/(vap00p + vap_total_1)) - (nativevap00p/vap00p))) < -0.005), 1, 0),
    underbound_othervap_hpct = ifelse(
      (annexing == 1 & increase_white == 1 & ((((othervap_total_1 + othervap00p)/(vap00p + vap_total_1)) - (othervap00p/vap00p))) < -0.005), 1, 0),
    underbound_nhwhitevap_hpct = ifelse(
      (annexing == 1 & ((((nhwhitevap_total_1 + nhwhitevap00p)/(vap00p + vap_total_1)) - (nhwhitevap00p/vap00p))) < -0.005), 1, 0),
    underbound_blackvap_1pct = ifelse(
      (annexing == 1 & increase_white == 1 & ((((nhblackvap_total_1 + nhblackvap00p)/(vap_total_1 + vap00p)) - (nhblackvap00p/vap00p))) < -0.01), 1, 0), 
    underbound_hispvap_1pct = ifelse(
      (annexing == 1 & increase_white == 1 & ((((hvap_total_1 + hispvap00p)/(vap_total_1 + vap00p)) - (hispvap00p/vap00p))) < -0.01), 1, 0),
    underbound_asianvap_1pct = ifelse(
      (annexing == 1 & increase_white == 1 & ((((asianvap_total_1 + asianvap00p)/(vap00p + vap_total_1)) - (asianvap00p/vap00p))) < -0.01), 1, 0), 
    underbound_nativevap_1pct = ifelse(
      (annexing == 1 & increase_white == 1 & ((((nativevap_total_1 + nativevap00p)/(vap00p + vap_total_1)) - (nativevap00p/vap00p))) < -0.01), 1, 0),
    underbound_othervap_1pct = ifelse(
      (annexing == 1 & increase_white == 1 & ((((othervap_total_1 + othervap00p)/(vap00p + vap_total_1)) - (othervap00p/vap00p))) < -0.01), 1, 0),
    underbound_nhwhitevap_1pct = ifelse(
      (annexing == 1 & ((((nhwhitevap_total_1 + nhwhitevap00p)/(vap00p + vap_total_1)) - (nhwhitevap00p/vap00p))) < -0.01), 1, 0)
  )

pl_annex_var_0010 %<>%
  mutate(
    underbound_blackvap = ifelse(
      (annexing == 1 & increase_white == 1 & ((((nhblackvap_total_1 + nhblackvap00p)/(vap_total_1 + vap00p)) < (nhblackvap00p/vap00p)))), 1, 0), 
    underbound_hispvap = ifelse(
      (annexing == 1 & increase_white == 1 & ((((hvap_total_1 + hispvap00p)/(vap_total_1 + vap00p)) < (hispvap00p/vap00p)))), 1, 0),
    underbound_asianvap = ifelse(
      (annexing == 1 & increase_white == 1 & ((((asianvap_total_1 + asianvap00p)/(vap00p + vap_total_1)) < (asianvap00p/vap00p)))), 1, 0), 
    underbound_nativevap = ifelse(
      (annexing == 1 & increase_white == 1 & ((((nativevap_total_1 + nativevap00p)/(vap00p + vap_total_1)) < (nativevap00p/vap00p)))), 1, 0),
    underbound_othervap = ifelse(
      (annexing == 1 & increase_white == 1 & ((((othervap_total_1 + othervap00p)/(vap00p + vap_total_1)) < (othervap00p/vap00p)))), 1, 0),
    underbound_nhwhitevap = ifelse(
      (annexing == 1 & ((((nhwhitevap_total_1 + nhwhitevap00p)/(vap00p + vap_total_1)) < (nhwhitevap00p/vap00p)))), 1, 0)
  )

pl_annex_var_0010 %<>%
  mutate(drop = ifelse(
    vap_total == 0, 1, 0
  ))
table(pl_annex_var_0010$drop)

pl_annex_var_0010 %<>%
  filter(drop==0) %>%
  select(-drop)

table(pl_annex_var_0010$underbound_blackvap)
table(pl_annex_var_0010$underbound_hispvap)
table(pl_annex_var_0010$underbound_nativevap)
table(pl_annex_var_0010$underbound_asianvap)
table(pl_annex_var_0010$underbound_othervap)
table(pl_annex_var_0010$underbound_nhwhitevap)
table(pl_annex_var_0010$underbound_blackvap_vraa)
table(pl_annex_var_0010$underbound_hispvap_vraa)
table(pl_annex_var_0010$underbound_nativevap_vraa)
table(pl_annex_var_0010$underbound_asianvap_vraa)
table(pl_annex_var_0010$underbound_othervap_vraa)
table(pl_annex_var_0010$underbound_nhwhitevap_vraa)
table(pl_annex_var_0010$underbound_blackvap_hpct)
table(pl_annex_var_0010$underbound_hispvap_hpct)
table(pl_annex_var_0010$underbound_nativevap_hpct)
table(pl_annex_var_0010$underbound_asianvap_hpct)
table(pl_annex_var_0010$underbound_othervap_hpct)
table(pl_annex_var_0010$underbound_nhwhitevap_hpct)
table(pl_annex_var_0010$underbound_blackvap_1pct)
table(pl_annex_var_0010$underbound_hispvap_1pct)
table(pl_annex_var_0010$underbound_nativevap_1pct)
table(pl_annex_var_0010$underbound_asianvap_1pct)
table(pl_annex_var_0010$underbound_othervap_1pct)
table(pl_annex_var_0010$underbound_nhwhitevap_1pct)
table(pl_annex_var_0010$underbound_blackvap_3pct)
table(pl_annex_var_0010$underbound_hispvap_3pct)
table(pl_annex_var_0010$underbound_nativevap_3pct)
table(pl_annex_var_0010$underbound_asianvap_3pct)
table(pl_annex_var_0010$underbound_othervap_3pct)
table(pl_annex_var_0010$underbound_nhwhitevap_3pct)

write_csv(pl_annex_var_0010, "analyticalfiles/pl_annex_var_0010.csv")
pl_annex_var_0010 <- read_csv("analyticalfiles/pl_annex_var_0010.csv")

desc0010 <- pl_annex_var_0010 %>%
  group_by(pop_tercile) %>%
  summarize_at(vars(c(contains("underbound"))), ~mean(., na.rm = T)) %>%
  mutate(Year = "2000 to 2010") %>%
  pivot_longer(cols = !c(Year, pop_tercile),
               names_to = "underbound") %>%
  mutate(vra_basis = case_when(
    grepl("vraa", underbound) ~ "JLVRAA",
    grepl("_hpct", underbound) ~ "VRA, 0.5%",
    grepl("_1pct", underbound) ~ "VRA, 1%",
    grepl("_3pct", underbound) ~ "VRA, 3%",
    TRUE ~ "VRA, 0%"),
         race = case_when(
           grepl("black", underbound) ~ "Non-Hispanic Black",
           grepl("hisp", underbound) ~ "Hispanic",
           grepl("native", underbound) ~ "Non-Hispanic Native",
           grepl("white", underbound) ~ "Non-Hispanic White",
           grepl("asian", underbound) ~ "Non-Hispanic Asian",
           grepl("other", underbound) ~ "Non-Hispanic Other"
         ))

desc <- base::rbind(desc0010, desc1013, desc1417, desc1720)
desc %<>%
  mutate(Year = factor(Year, levels = c("2000 to 2010", "2010 to 2013", 
                                        "2014 to 2017", "2017 to 2020")),
         vra_basis = factor(vra_basis, levels = c("VRA, 0%", "VRA, 0.5%", 
                                                  "VRA, 1%", "VRA, 3%", "JLVRAA"
         )),
         Proportion = value*100,
         race = factor(race, levels = c("Non-Hispanic Black",
                                        "Hispanic",
                                        "Non-Hispanic Asian",
                                        "Non-Hispanic Native",
                                        "Non-Hispanic Other",
                                        "Non-Hispanic White")))

plot <- ggplot(desc, aes(y = Proportion, x = Year, group = vra_basis)) + 
  geom_line(aes(color = vra_basis)) + geom_point(aes(color = vra_basis)) + 
  facet_grid(pop_tercile~race, labeller = label_wrap_gen(width=12)) + 
  theme(axis.text.x = element_text(angle = 50, hjust = 1, size = 8),
        strip.text.x = element_text(size = 6)) + 
  ggtitle("Proportion of (Annexing) Places Conducting Questionable Annexations 
by VRA or JLVRAA Benchmark for Preclearance VRA Coverage, 2000-2020") + 
  labs(color = "VRA Coverage")
plot
ggsave(filename = "analyticalfiles/VRA_VRAA_pop_nowhite_noannex_fixedblocks.png",
       plot = plot,
       dpi = 300)
write_csv(desc, "analyticalfiles/desc_vra_vraa_pop_nowhite_fixedblocks.csv")
rm(list = ls())

#make one without pop_tercile ####
pl_annex_var_1013 <- read_csv("analyticalfiles/pl_annex_var_1013.csv")

desc1013 <- pl_annex_var_1013 %>%
  summarize_at(vars(c(contains("underbound"))), ~mean(., na.rm = T)) %>%
  mutate(Year = "2010 to 2013") %>%
  pivot_longer(cols = !c(Year),
               names_to = "underbound") %>%
  mutate(vra_basis = case_when(
    grepl("vraa", underbound) ~ "JLVRAA",
    grepl("_hpct", underbound) ~ "VRA, 0.5%",
    grepl("_1pct", underbound) ~ "VRA, 1%",
    grepl("_3pct", underbound) ~ "VRA, 3%",
    TRUE ~ "VRA, 0%"
  ),
  race = case_when(
    grepl("black", underbound) ~ "Non-Hispanic Black",
    grepl("hisp", underbound) ~ "Hispanic",
    grepl("native", underbound) ~ "Non-Hispanic Native",
    grepl("white", underbound) ~ "Non-Hispanic White",
    grepl("asian", underbound) ~ "Non-Hispanic Asian",
    grepl("other", underbound) ~ "Non-Hispanic Other"
  ))
rm(pl_annex_var_1013)

pl_annex_var_1417 <- read_csv("analyticalfiles/pl_annex_var_1417.csv")
desc1417 <- pl_annex_var_1417 %>%
  summarize_at(vars(c(contains("underbound"))), ~mean(., na.rm = T)) %>%
  mutate(Year = "2014 to 2017") %>%
  pivot_longer(cols = !c(Year),
               names_to = "underbound") %>%
  mutate(vra_basis = case_when(
    grepl("vraa", underbound) ~ "JLVRAA",
    grepl("_hpct", underbound) ~ "VRA, 0.5%",
    grepl("_1pct", underbound) ~ "VRA, 1%",
    grepl("_3pct", underbound) ~ "VRA, 3%",
    TRUE ~ "VRA, 0%"),
    race = case_when(
      grepl("black", underbound) ~ "Non-Hispanic Black",
      grepl("hisp", underbound) ~ "Hispanic",
      grepl("native", underbound) ~ "Non-Hispanic Native",
      grepl("white", underbound) ~ "Non-Hispanic White",
      grepl("asian", underbound) ~ "Non-Hispanic Asian",
      grepl("other", underbound) ~ "Non-Hispanic Other"
    ))
rm(pl_annex_var_1417)

pl_annex_var_1720 <- read_csv("analyticalfiles/pl_annex_var_1720.csv")
desc1720 <- pl_annex_var_1720 %>%
  summarize_at(vars(c(contains("underbound"))), ~mean(., na.rm = T)) %>%
  mutate(Year = "2017 to 2020") %>%
  pivot_longer(cols = !c(Year),
               names_to = "underbound") %>%
  mutate(vra_basis = case_when(
    grepl("vraa", underbound) ~ "JLVRAA",
    grepl("_hpct", underbound) ~ "VRA, 0.5%",
    grepl("_1pct", underbound) ~ "VRA, 1%",
    grepl("_3pct", underbound) ~ "VRA, 3%",
    TRUE ~ "VRA, 0%"),
    race = case_when(
      grepl("black", underbound) ~ "Non-Hispanic Black",
      grepl("hisp", underbound) ~ "Hispanic",
      grepl("native", underbound) ~ "Non-Hispanic Native",
      grepl("white", underbound) ~ "Non-Hispanic White",
      grepl("asian", underbound) ~ "Non-Hispanic Asian",
      grepl("other", underbound) ~ "Non-Hispanic Other"
    ))
rm(pl_annex_var_1720)

pl_annex_var_0010 <- read_csv("analyticalfiles/pl_annex_var_0010.csv")
desc0010 <- pl_annex_var_0010 %>%
  summarize_at(vars(c(contains("underbound"))), ~mean(., na.rm = T)) %>%
  mutate(Year = "2000 to 2010") %>%
  pivot_longer(cols = !c(Year),
               names_to = "underbound") %>%
  mutate(vra_basis = case_when(
    grepl("vraa", underbound) ~ "JLVRAA",
    grepl("_hpct", underbound) ~ "VRA, 0.5%",
    grepl("_1pct", underbound) ~ "VRA, 1%",
    grepl("_3pct", underbound) ~ "VRA, 3%",
    TRUE ~ "VRA, 0%"),
    race = case_when(
      grepl("black", underbound) ~ "Non-Hispanic Black",
      grepl("hisp", underbound) ~ "Hispanic",
      grepl("native", underbound) ~ "Non-Hispanic Native",
      grepl("white", underbound) ~ "Non-Hispanic White",
      grepl("asian", underbound) ~ "Non-Hispanic Asian",
      grepl("other", underbound) ~ "Non-Hispanic Other"
    ))

desc <- base::rbind(desc0010, desc1013, desc1417, desc1720)
desc %<>%
  mutate(Year = factor(Year, levels = c("2000 to 2010", "2010 to 2013", 
                                        "2014 to 2017", "2017 to 2020")),
         vra_basis = factor(vra_basis, levels = c("VRA, 0%", "VRA, 0.5%", 
                                                  "VRA, 1%", "VRA, 3%", "JLVRAA"
         )),
         Proportion = value*100,
         race = factor(race, levels = c("Non-Hispanic Black",
                                        "Hispanic",
                                        "Non-Hispanic Asian",
                                        "Non-Hispanic Native",
                                        "Non-Hispanic Other",
                                        "Non-Hispanic White")))

plot <- ggplot(desc, aes(y = Proportion, x = Year, group = vra_basis)) + 
  geom_line(aes(color = vra_basis)) + geom_point(aes(color = vra_basis)) + 
  facet_grid(~race, labeller = label_wrap_gen(width=12)) + 
  theme(axis.text.x = element_text(angle = 50, hjust = 1, size = 8),
        strip.text.x = element_text(size = 6)) + 
  ggtitle("Proportion of (Annexing) Places Conducting Questionable Annexations 
by VRA or JLVRAA Benchmark for Preclearance VRA Coverage, 2000-2020") + 
  labs(color = "VRA Coverage")
plot
ggsave(filename = "analyticalfiles/VRA_VRAA_pop_nowhite_noannex_fixedblocks_nopop.png",
       plot = plot,
       dpi = 300)
write_csv(desc, "analyticalfiles/desc_vra_vraa_pop_nowhite_fixedblocks_nopop.csv")
rm(list = ls())
# make panel data!!!!! ####
# take out ne and hawaii
NE <- c("09", "23", "25", "33", "34", "36", "42", "24", "44", "50", "15")
pl0010 <- read_csv("analyticalfiles/pl_annex_var_0010.csv") %>%
  mutate(STATE = substr(plid, 1, 2)) %>%
  filter(!(STATE %in% NE)) %>%
  #filter(annexing==1) %>%
  filter(vap_total > 0)
pl1013 <- read_csv("analyticalfiles/pl_annex_var_1013.csv") %>%
  mutate(STATE = substr(plid, 1, 2)) %>%
  filter(!(STATE %in% NE)) %>%
  #filter(annexing==1) %>%
  filter(vap_total > 0)
pl1417 <- read_csv("analyticalfiles/pl_annex_var_1417.csv") %>%
  mutate(STATE = substr(plid, 1, 2)) %>%
  filter(!(STATE %in% NE)) %>%
  #filter(annexing==1) %>%
  filter(vap_total > 0)
pl1720 <- read_csv("analyticalfiles/pl_annex_var_1720.csv") %>%
  mutate(STATE = substr(plid, 1, 2)) %>%
  filter(!(STATE %in% NE)) %>%
  #filter(annexing==1) %>%
  filter(vap_total > 0)

plids <- Reduce(intersect, list(unique(pl0010$plid), unique(pl1013$plid), unique(pl1417$plid), unique(pl1720$plid)))
names_list <- Reduce(intersect, list(names(pl0010), names(pl1013), names(pl1417), names(pl1720)))

pl0010 %<>% 
  filter(plid %in% plids) %>%
  select(all_of(names_list)) %>%
  filter(!duplicated(plid))
pl1013 %<>%
  filter(plid %in% plids) %>%
  select(all_of(names_list))
pl1417 %<>%
  filter(plid %in% plids) %>%
  select(all_of(names_list))
pl1720 %<>%
  filter(plid %in% plids) %>%
  select(all_of(names_list)) %>%
  mutate(post = 2)

panel0020_did <- base::rbind(
    pl0010, pl1013, pl1417, pl1720
)

rm(pl0010, pl1013, pl1417, pl1720)

panel0020_did %<>% 
  mutate(period = post, 
         post = ifelse(post > 0, 1, 0), 
         vra = as.factor(vra),
         post = as.factor(post))
panel2p <- panel0020_did %>%
  filter(period %in% c(0, 1, 2)) 

# models ####
outcomes <- c("", "_hpct", "_1pct", "_3pct")
races <- c("black", "hisp", "native", "asian", "nhwhite", "other")

# black 
# all versus 2p
all <- fixest::feols(underbound_blackvap ~ as.factor(vra)*as.factor(post) + as.factor(maj_white) + as.factor(pop_tercile) + as.factor(annexing) + pct_annexed | plid + STATE, data = panel0020_did)
summary(all)
hpct <- fixest::feols(underbound_blackvap_hpct ~ as.factor(vra)*as.factor(post) + as.factor(maj_white) + as.factor(pop_tercile) + as.factor(annexing) + pct_annexed | plid + STATE, data = panel0020_did)
summary(hpct)
pct1 <- fixest::feols(underbound_blackvap_1pct ~ as.factor(vra)*as.factor(post) + as.factor(maj_white) + as.factor(pop_tercile) + as.factor(annexing) + pct_annexed | plid + STATE, data = panel0020_did)
summary(pct1)
pct3 <- fixest::feols(underbound_blackvap_3pct ~ as.factor(vra)*as.factor(post) + as.factor(maj_white) + as.factor(pop_tercile) + as.factor(annexing) + pct_annexed | plid + STATE, data = panel0020_did)
summary(pct3)
black_all <- list(tidy(all), tidy(hpct), tidy(pct1), tidy(pct3))
openxlsx::write.xlsx(black_all, "analyticalfiles/results/black_all_nowhite.xlsx")
all <- fixest::feols(underbound_blackvap ~ as.factor(vra)*as.factor(post) + as.factor(maj_white) + as.factor(pop_tercile) + as.factor(annexing) + pct_annexed | plid + STATE, data = panel2p)
summary(all)
hpct <- fixest::feols(underbound_blackvap_hpct ~ as.factor(vra)*as.factor(post) + as.factor(maj_white) +  as.factor(pop_tercile) + as.factor(annexing) + pct_annexed | plid + STATE, data = panel2p)
summary(hpct)
pct1 <- fixest::feols(underbound_blackvap_1pct ~ as.factor(vra)*as.factor(post) + as.factor(maj_white) + as.factor(pop_tercile) + as.factor(annexing) + pct_annexed | plid + STATE, data = panel2p)
summary(pct1)
pct3 <- fixest::feols(underbound_blackvap_3pct ~ as.factor(vra)*as.factor(post) + as.factor(maj_white) + as.factor(pop_tercile) + as.factor(annexing) + pct_annexed | plid + STATE, data = panel2p)
summary(pct3)
black_all <- list(tidy(all), tidy(hpct), tidy(pct1), tidy(pct3))
openxlsx::write.xlsx(black_all, "analyticalfiles/results/black_2p_nowhite.xlsx")

#hisp 
all <- fixest::feols(underbound_hispvap ~ as.factor(vra)*as.factor(post) + as.factor(maj_white) + as.factor(pop_tercile)+ as.factor(annexing) + pct_annexed | plid + STATE, data = panel0020_did)
summary(all)
hpct <- fixest::feols(underbound_hispvap_hpct ~ as.factor(vra)*as.factor(post) + as.factor(maj_white) + as.factor(pop_tercile)+ as.factor(annexing) + pct_annexed | plid + STATE, data = panel0020_did)
summary(hpct)
pct1 <- fixest::feols(underbound_hispvap_1pct ~ as.factor(vra)*as.factor(post) + as.factor(maj_white) + as.factor(pop_tercile)+ as.factor(annexing) + pct_annexed | plid + STATE, data = panel0020_did)
summary(pct1)
pct3 <- fixest::feols(underbound_hispvap_3pct ~ as.factor(vra)*as.factor(post) + as.factor(maj_white) + as.factor(pop_tercile)+ as.factor(annexing) + pct_annexed | plid + STATE, data = panel0020_did)
summary(pct3)
hisp_all <- list(tidy(all), tidy(hpct), tidy(pct1), tidy(pct3))
openxlsx::write.xlsx(hisp_all, "analyticalfiles/results/hisp_all_nowhite.xlsx")
all <- fixest::feols(underbound_hispvap ~ as.factor(vra)*as.factor(post) + as.factor(maj_white) + as.factor(pop_tercile)+ as.factor(annexing) + pct_annexed | plid + STATE, data = panel2p)
summary(all)
hpct <- fixest::feols(underbound_hispvap_hpct ~ as.factor(vra)*as.factor(post) + as.factor(maj_white) + as.factor(pop_tercile)+ as.factor(annexing) + pct_annexed | plid + STATE, data = panel2p)
summary(hpct)
pct1 <- fixest::feols(underbound_hispvap_1pct ~ as.factor(vra)*as.factor(post) + as.factor(maj_white) + as.factor(pop_tercile)+ as.factor(annexing) + pct_annexed | plid + STATE, data = panel2p)
summary(pct1)
pct3 <- fixest::feols(underbound_hispvap_3pct ~ as.factor(vra)*as.factor(post) + as.factor(maj_white) + as.factor(pop_tercile)+ as.factor(annexing) + pct_annexed | plid + STATE, data = panel2p)
summary(pct3)
hisp_all <- list(tidy(all), tidy(hpct), tidy(pct1), tidy(pct3))
openxlsx::write.xlsx(hisp_all, "analyticalfiles/results/hisp_2p_nowhite.xlsx")

# asian
all <- fixest::feols(underbound_asianvap ~ as.factor(vra)*as.factor(post) + as.factor(maj_white) +as.factor(pop_tercile)+ as.factor(annexing) + pct_annexed | plid + STATE, data = panel0020_did)
summary(all)
hpct <- fixest::feols(underbound_asianvap_hpct ~ as.factor(vra)*as.factor(post) + as.factor(maj_white) + as.factor(pop_tercile)+ as.factor(annexing) + pct_annexed | plid + STATE, data = panel0020_did)
summary(hpct)
pct1 <- fixest::feols(underbound_asianvap_1pct ~ as.factor(vra)*as.factor(post) + as.factor(maj_white) + as.factor(pop_tercile)+ as.factor(annexing) + pct_annexed | plid + STATE, data = panel0020_did)
summary(pct1)
pct3 <- fixest::feols(underbound_asianvap_3pct ~ as.factor(vra)*as.factor(post) + as.factor(maj_white) + as.factor(pop_tercile)+ as.factor(annexing) + pct_annexed | plid + STATE, data = panel0020_did)
summary(pct3)
asian_all <- list(tidy(all), tidy(hpct), tidy(pct1), tidy(pct3))
openxlsx::write.xlsx(asian_all, "analyticalfiles/results/asian_all_nowhite.xlsx")
all <- fixest::feols(underbound_asianvap ~ as.factor(vra)*as.factor(post) + as.factor(maj_white) + as.factor(pop_tercile)+ as.factor(annexing) + pct_annexed | plid + STATE, data = panel2p)
summary(all)
hpct <- fixest::feols(underbound_asianvap_hpct ~ as.factor(vra)*as.factor(post) + as.factor(maj_white) + as.factor(pop_tercile)+ as.factor(annexing) + pct_annexed | plid + STATE, data = panel2p)
summary(hpct)
pct1 <- fixest::feols(underbound_asianvap_1pct ~ as.factor(vra)*as.factor(post) + as.factor(maj_white) + as.factor(pop_tercile)+ as.factor(annexing) + pct_annexed | plid + STATE, data = panel2p)
summary(pct1)
pct3 <- fixest::feols(underbound_asianvap_3pct ~ as.factor(vra)*as.factor(post) + as.factor(maj_white) + as.factor(pop_tercile)+ as.factor(annexing) + pct_annexed | plid + STATE, data = panel2p)
summary(pct3)
asian_all <- list(tidy(all), tidy(hpct), tidy(pct1), tidy(pct3))
openxlsx::write.xlsx(asian_all, "analyticalfiles/results/asian_2p_nowhite.xlsx")

#other
all <- fixest::feols(underbound_othervap ~ as.factor(vra)*as.factor(post) + as.factor(maj_white) + as.factor(pop_tercile)+ as.factor(annexing) + pct_annexed | plid + STATE, data = panel0020_did)
summary(all)
hpct <- fixest::feols(underbound_othervap_hpct ~ as.factor(vra)*as.factor(post) + as.factor(maj_white) + as.factor(pop_tercile)+ as.factor(annexing) + pct_annexed | plid + STATE, data = panel0020_did)
summary(hpct)
pct1 <- fixest::feols(underbound_othervap_1pct ~ as.factor(vra)*as.factor(post) + as.factor(maj_white) + as.factor(pop_tercile)+ as.factor(annexing) + pct_annexed | plid + STATE, data = panel0020_did)
summary(pct1)
pct3 <- fixest::feols(underbound_othervap_3pct ~ as.factor(vra)*as.factor(post) + as.factor(maj_white) + as.factor(pop_tercile)+ as.factor(annexing) + pct_annexed | plid + STATE, data = panel0020_did)
summary(pct3)
other_all <- list(tidy(all), tidy(hpct), tidy(pct1), tidy(pct3))
openxlsx::write.xlsx(other_all, "analyticalfiles/results/other_all_nowhite.xlsx")
all <- fixest::feols(underbound_othervap ~ as.factor(vra)*as.factor(post) + as.factor(maj_white) + as.factor(pop_tercile)+ as.factor(annexing) + pct_annexed | plid + STATE, data = panel2p)
summary(all)
hpct <- fixest::feols(underbound_othervap_hpct ~ as.factor(vra)*as.factor(post) + as.factor(maj_white) + as.factor(pop_tercile)+ as.factor(annexing) + pct_annexed | plid + STATE, data = panel2p)
summary(hpct)
pct1 <- fixest::feols(underbound_othervap_1pct ~ as.factor(vra)*as.factor(post) + as.factor(maj_white) + as.factor(pop_tercile)+ as.factor(annexing) + pct_annexed | plid + STATE, data = panel2p)
summary(pct1)
pct3 <- fixest::feols(underbound_othervap_3pct ~ as.factor(vra)*as.factor(post) + as.factor(maj_white) + as.factor(pop_tercile)+ as.factor(annexing) + pct_annexed | plid + STATE, data = panel2p)
summary(pct3)
other_all <- list(tidy(all), tidy(hpct), tidy(pct1), tidy(pct3))
openxlsx::write.xlsx(other_all, "analyticalfiles/results/other_2p_nowhite.xlsx")

#nhw
all <- fixest::feols(underbound_nhwhitevap ~ as.factor(vra)*as.factor(post)+ as.factor(maj_white) + as.factor(pop_tercile)+ as.factor(annexing) + pct_annexed | plid + STATE, data = panel0020_did)
summary(all)
hpct <- fixest::feols(underbound_nhwhitevap_hpct ~ as.factor(vra)*as.factor(post)+ as.factor(maj_white) + as.factor(pop_tercile)+ as.factor(annexing) + pct_annexed | plid + STATE, data = panel0020_did)
summary(hpct)
pct1 <- fixest::feols(underbound_nhwhitevap_1pct ~ as.factor(vra)*as.factor(post)+ as.factor(maj_white) + as.factor(pop_tercile)+ as.factor(annexing) + pct_annexed | plid + STATE, data = panel0020_did)
summary(pct1)
pct3 <- fixest::feols(underbound_nhwhitevap_3pct ~ as.factor(vra)*as.factor(post)+ as.factor(maj_white) + as.factor(pop_tercile)+ as.factor(annexing) + pct_annexed | plid + STATE, data = panel0020_did)
summary(pct3)
nhwhite_all <- list(tidy(all), tidy(hpct), tidy(pct1), tidy(pct3))
openxlsx::write.xlsx(nhwhite_all, "analyticalfiles/results/nhwhite_all.xlsx")
all <- fixest::feols(underbound_nhwhitevap ~ as.factor(vra)*as.factor(post)+ as.factor(maj_white) + as.factor(pop_tercile)+ as.factor(annexing) + pct_annexed | plid + STATE, data = panel2p)
summary(all)
hpct <- fixest::feols(underbound_nhwhitevap_hpct ~ as.factor(vra)*as.factor(post)+ as.factor(maj_white) + as.factor(pop_tercile)+ as.factor(annexing) + pct_annexed | plid + STATE, data = panel2p)
summary(hpct)
pct1 <- fixest::feols(underbound_nhwhitevap_1pct ~ as.factor(vra)*as.factor(post)+ as.factor(maj_white) + as.factor(pop_tercile)+ as.factor(annexing) + pct_annexed | plid + STATE, data = panel2p)
summary(pct1)
pct3 <- fixest::feols(underbound_nhwhitevap_3pct ~ as.factor(vra)*as.factor(post)+ as.factor(maj_white) + as.factor(pop_tercile)+ as.factor(annexing) + pct_annexed | plid + STATE, data = panel2p)
summary(pct3)
nhwhite_all <- list(tidy(all), tidy(hpct), tidy(pct1), tidy(pct3))
openxlsx::write.xlsx(nhwhite_all, "analyticalfiles/results/nhwhite_2p.xlsx")

# native 
all <- fixest::feols(underbound_nativevap ~ as.factor(vra)*as.factor(post) + as.factor(maj_white) + as.factor(pop_tercile)+ as.factor(annexing) + pct_annexed | plid + STATE, data = panel0020_did)
summary(all)
hpct <- fixest::feols(underbound_nativevap_hpct ~ as.factor(vra)*as.factor(post) + as.factor(maj_white) + as.factor(pop_tercile)+ as.factor(annexing) + pct_annexed | plid + STATE, data = panel0020_did)
summary(hpct)
pct1 <- fixest::feols(underbound_nativevap_1pct ~ as.factor(vra)*as.factor(post) + as.factor(maj_white) + as.factor(pop_tercile)+ as.factor(annexing) + pct_annexed | plid + STATE, data = panel0020_did)
summary(pct1)
pct3 <- fixest::feols(underbound_nativevap_3pct ~ as.factor(vra)*as.factor(post) + as.factor(maj_white) + as.factor(pop_tercile)+ as.factor(annexing) + pct_annexed | plid + STATE, data = panel0020_did)
summary(pct3)
native_all <- list(tidy(all), tidy(hpct), tidy(pct1), tidy(pct3))
openxlsx::write.xlsx(native_all, "analyticalfiles/results/native_all_nowhite.xlsx")
all <- fixest::feols(underbound_nativevap ~ as.factor(vra)*as.factor(post) + as.factor(maj_white) + as.factor(pop_tercile)+ as.factor(annexing) + pct_annexed | plid + STATE, data = panel2p)
summary(all)
hpct <- fixest::feols(underbound_nativevap_hpct ~ as.factor(vra)*as.factor(post) + as.factor(maj_white) + as.factor(pop_tercile)+ as.factor(annexing) + pct_annexed | plid + STATE, data = panel2p)
summary(hpct)
pct1 <- fixest::feols(underbound_nativevap_1pct ~ as.factor(vra)*as.factor(post) + as.factor(maj_white) + as.factor(pop_tercile)+ as.factor(annexing) + pct_annexed | plid + STATE, data = panel2p)
summary(pct1)
pct3 <- fixest::feols(underbound_nativevap_3pct ~ as.factor(vra)*as.factor(post) + as.factor(maj_white) + as.factor(pop_tercile)+ as.factor(annexing) + pct_annexed | plid + STATE, data = panel2p)
summary(pct3)
native_all <- list(tidy(all), tidy(hpct), tidy(pct1), tidy(pct3))
openxlsx::write.xlsx(native_all, "analyticalfiles/results/native_2p_nowhite.xlsx")

# viz ####
indices <- c(grep("underbound", names(panel0020_did)))
indices <- names(panel0020_did)[indices]
indices <- indices[!grepl("vraa", indices)]

did_vis <- panel0020_did %>%
  filter(annexing==1) %>%
  group_by(vra, period) %>%
  summarise_at(all_of(indices), 
               ~mean(., na.rm = T)) %>%
  ungroup() %>%
  pivot_longer(cols = contains("underbound"),
               names_to = "Race", 
               values_to = "mean") %>%
  mutate(vra_basis = case_when(
    grepl("vraa", Race) ~ "JLVRAA",
    grepl("_hpct", Race) ~ "VRA, 0.5%",
    grepl("_1pct", Race) ~ "VRA, 1%",
    grepl("_3pct", Race) ~ "VRA, 3%",
    TRUE ~ "VRA, 0%"),
    Race = case_when(
    grepl("black", Race) ~ "Non-Hispanic Black",
    grepl("hisp", Race) ~ "Hispanic",
    grepl("native", Race) ~ "Native",
    grepl("asian", Race) ~ "Asian",
    grepl("other", Race) ~ "Other",
    grepl("white", Race) ~ "Non-Hispanic White"
  ),
  vra = as.character(vra),
  vra_basis = factor(vra_basis, levels = c(
    "VRA, 0%",
    "VRA, 0.5%",
    "VRA, 1%",
    "VRA, 3%"
  )),
  Race = factor(Race, levels = c(
    "Non-Hispanic Black",
    "Hispanic",
    "Asian",
    "Native",
    "Other",
    "Non-Hispanic White"
  ))
  )

did_vis_total <- ggplot(did_vis,
                        aes(y = mean, x = as.numeric(as.character(period)), color = vra)) + 
  geom_point() + geom_line() + 
  scale_x_continuous(breaks = c(-1, 0, 1, 2)) + 
  scale_y_continuous(labels = scales::percent) +
  geom_vline(xintercept = 0, linetype="dotted", 
             color = "red", size=0.5) +
  facet_grid(vra_basis~Race, labeller = label_wrap_gen(width=12)) + 
  labs(color = "Covered Under \n Section V",
       x = "Period Relative to Shelby v. Holder", 
       y = "Mean", 
       title = "% of Annexations Potentially Subject to Pre-clearance Scrunity,
Pre- and Post-Shelby Trends by Race, VAP, and Threshold") + 
  theme(strip.text.x = element_text(size = 7))
did_vis_total
ggsave(filename = "analyticalfiles/did_vis_annexed_mean_vra_allannex_fixedblocks.png",
       plot = did_vis_total,
       dpi = 300)

did_vis_total <- ggplot(did_vis %>% filter(Race != "Non-Hispanic White"),
                        aes(y = mean, x = as.numeric(as.character(period)), color = vra)) + 
  geom_point() + geom_line() + 
  scale_x_continuous(breaks = c(-1, 0, 1, 2)) + 
  scale_y_continuous(labels = scales::percent) +
  geom_vline(xintercept = 0, linetype="dotted", 
             color = "red", size=0.5) +
  facet_grid(vra_basis~Race, labeller = label_wrap_gen(width=12)) + 
  labs(color = "Covered Under \n Section V",
       x = "Period Relative to Shelby v. Holder", 
       y = "Mean, Among Annexing Places", 
       title = "% of Annexations Potentially Subject to Pre-clearance Scrunity,
Pre- and Post-Shelby Trends by Race, VAP, and Threshold") + 
  theme(strip.text.x = element_text(size = 7))
did_vis_total
ggsave(filename = "analyticalfiles/did_vis_annexed_mean_vra_no_fixedblocks_white.png",
       plot = did_vis_total,
       dpi = 300)

# zero in by terciles
did_vis <- panel0020_did %>%
  filter(pop_tercile == 1 & annexing == 1) %>%
  group_by(vra, period) %>%
  summarise_at(all_of(indices), 
               ~mean(., na.rm = T)) %>%
  ungroup() %>%
  pivot_longer(cols = contains("underbound"),
               names_to = "Race", 
               values_to = "mean") %>%
  mutate(vra_basis = case_when(
    grepl("vraa", Race) ~ "JLVRAA",
    grepl("_hpct", Race) ~ "VRA, 0.5%",
    grepl("_1pct", Race) ~ "VRA, 1%",
    grepl("_3pct", Race) ~ "VRA, 3%",
    TRUE ~ "VRA, 0%"),
    Race = case_when(
      grepl("black", Race) ~ "Non-Hispanic Black",
      grepl("hisp", Race) ~ "Hispanic",
      grepl("native", Race) ~ "Native",
      grepl("asian", Race) ~ "Asian",
      grepl("other", Race) ~ "Other",
      grepl("white", Race) ~ "Non-Hispanic White"
    ),
    vra = as.character(vra),
    vra_basis = factor(vra_basis, levels = c(
      "VRA, 0%",
      "VRA, 0.5%",
      "VRA, 1%",
      "VRA, 3%"
    )),
    Race = factor(Race, levels = c(
      "Non-Hispanic Black",
      "Hispanic",
      "Asian",
      "Native",
      "Other",
      "Non-Hispanic White"
    ))
)

did_vis_total <- ggplot(did_vis,
                        aes(y = mean, x = as.numeric(as.character(period)), color = vra)) + 
  geom_point() + geom_line() + 
  scale_x_continuous(breaks = c(-1, 0, 1, 2)) + 
  scale_y_continuous(labels = scales::percent) +
  geom_vline(xintercept = 0, linetype="dotted", 
             color = "red", size=0.5) +
  facet_grid(vra_basis~Race, labeller = label_wrap_gen(width=12)) + 
  labs(color = "Covered Under \n Section V",
       x = "Period Relative to Shelby v. Holder", 
       y = "Mean, Among Annexing Places", 
       title = "% of Annexations Potentially Subject to Pre-clearance Scrunity,
Pre- and Post-Shelby Trends by Race, VAP, and Threshold") + 
  theme(strip.text.x = element_text(size = 7))
did_vis_total
ggsave(filename = "analyticalfiles/did_vis_annexed_mean_vra_t1.png",
       plot = did_vis_total,
       dpi = 300)

#t2
did_vis <- panel0020_did %>%
  filter(pop_tercile == 2 & annexing == 1) %>%
  group_by(vra, period) %>%
  summarise_at(all_of(indices), 
               ~mean(., na.rm = T)) %>%
  ungroup() %>%
  pivot_longer(cols = contains("underbound"),
               names_to = "Race", 
               values_to = "mean") %>%
  mutate(vra_basis = case_when(
    grepl("vraa", Race) ~ "JLVRAA",
    grepl("_hpct", Race) ~ "VRA, 0.5%",
    grepl("_1pct", Race) ~ "VRA, 1%",
    grepl("_3pct", Race) ~ "VRA, 3%",
    TRUE ~ "VRA, 0%"),
    Race = case_when(
      grepl("black", Race) ~ "Non-Hispanic Black",
      grepl("hisp", Race) ~ "Hispanic",
      grepl("native", Race) ~ "Native",
      grepl("asian", Race) ~ "Asian",
      grepl("other", Race) ~ "Other",
      grepl("white", Race) ~ "Non-Hispanic White"
    ),
    vra = as.character(vra),
    vra_basis = factor(vra_basis, levels = c(
      "VRA, 0%",
      "VRA, 0.5%",
      "VRA, 1%",
      "VRA, 3%"
    )),
    Race = factor(Race, levels = c(
      "Non-Hispanic Black",
      "Hispanic",
      "Asian",
      "Native",
      "Other",
      "Non-Hispanic White"
    ))
  )

did_vis_total <- ggplot(did_vis,
                        aes(y = mean, x = as.numeric(as.character(period)), color = vra)) + 
  geom_point() + geom_line() + 
  scale_x_continuous(breaks = c(-1, 0, 1, 2)) + 
  scale_y_continuous(labels = scales::percent) +
  geom_vline(xintercept = 0, linetype="dotted", 
             color = "red", size=0.5) +
  facet_grid(vra_basis~Race, labeller = label_wrap_gen(width=12)) + 
  labs(color = "Covered Under \n Section V",
       x = "Period Relative to Shelby v. Holder", 
       y = "Mean, Among Annexing Places", 
       title = "% of Annexations Potentially Subject to Pre-clearance Scrunity,
Pre- and Post-Shelby Trends by Race, VAP, and Threshold") + 
  theme(strip.text.x = element_text(size = 7))
did_vis_total
ggsave(filename = "analyticalfiles/did_vis_annexed_mean_vra_t2.png",
       plot = did_vis_total,
       dpi = 300)

#t3
did_vis <- panel0020_did %>%
  filter(pop_tercile == 3 & annexing == 1) %>%
  group_by(vra, period) %>%
  summarise_at(all_of(indices), 
               ~mean(., na.rm = T)) %>%
  ungroup() %>%
  pivot_longer(cols = contains("underbound"),
               names_to = "Race", 
               values_to = "mean") %>%
  mutate(vra_basis = case_when(
    grepl("vraa", Race) ~ "JLVRAA",
    grepl("_hpct", Race) ~ "VRA, 0.5%",
    grepl("_1pct", Race) ~ "VRA, 1%",
    grepl("_3pct", Race) ~ "VRA, 3%",
    TRUE ~ "VRA, 0%"),
    Race = case_when(
      grepl("black", Race) ~ "Non-Hispanic Black",
      grepl("hisp", Race) ~ "Hispanic",
      grepl("native", Race) ~ "Native",
      grepl("asian", Race) ~ "Asian",
      grepl("other", Race) ~ "Other",
      grepl("white", Race) ~ "Non-Hispanic White"
    ),
    vra = as.character(vra),
    vra_basis = factor(vra_basis, levels = c(
      "VRA, 0%",
      "VRA, 0.5%",
      "VRA, 1%",
      "VRA, 3%"
    )),
    Race = factor(Race, levels = c(
      "Non-Hispanic Black",
      "Hispanic",
      "Asian",
      "Native",
      "Other",
      "Non-Hispanic White"
    ))
  )

did_vis_total <- ggplot(did_vis,
                        aes(y = mean, x = as.numeric(as.character(period)), color = vra)) + 
  geom_point() + geom_line() + 
  scale_x_continuous(breaks = c(-1, 0, 1, 2)) + 
  scale_y_continuous(labels = scales::percent) +
  geom_vline(xintercept = 0, linetype="dotted", 
             color = "red", size=0.5) +
  facet_grid(vra_basis~Race, labeller = label_wrap_gen(width=12)) + 
  labs(color = "Covered Under \n Section V",
       x = "Period Relative to Shelby v. Holder", 
       y = "Mean, Among Annexing Places", 
       title = "% of Annexations Potentially Subject to Pre-clearance Scrunity,
Pre- and Post-Shelby Trends by Race, VAP, and Threshold") + 
  theme(strip.text.x = element_text(size = 7))
did_vis_total
ggsave(filename = "analyticalfiles/did_vis_annexed_mean_vra_t3.png",
       plot = did_vis_total,
       dpi = 300)
