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
library("sjPlot")

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

rm(list = ls ())

# 2007-2013 ####
aa0713 <- read_csv("analyticalfiles/annexedblocks0713dem.csv")
names(aa0713)

# transform this into place-level summaries
# characteristic of all annexable blocks
# diff between annexed and not annexed 
place_all <- aa0713 %>% 
  mutate(incopp = man + ret) %>%
  group_by(plid) %>%
  summarize(pop_total = sum(pop, na.rm = T),
            nhblack_total = sum(nhblack, na.rm = T),
            nhwhite_total = sum(nhwhite, na.rm = T),
            h_total = sum(h, na.rm = T),
         vap_total = sum(vap, na.rm = T),
         nhblackvap_total = sum(nhbvap),
         nhwhitevap_total = sum(nhwvap),
         hvap_total = sum(hispvap),
         nativevap_total = sum(nativevap, na.rm = T),
         asianvap_total = sum(asianvap, na.rm = T),
         othervap_total = sum(othervap, na.rm = T),
         pct_annexed = mean(annexed, na.rm = T),
         njobs_total = sum(njobs07, na.rm = T),
         nhincjobs_total = sum(nhincjobs07, na.rm = T),
         nwork_total = sum(jobs, na.rm = T),
         incopp_total = sum(incopp, na.rm = T),
         hu_total = sum(hu, na.rm = T),
         owneroccupied_total = sum(owneroccupied, na.rm = T),
         vacancy_total = sum(vacancy)
         ) %>%
  ungroup() %>%
  mutate(pctnhblack_total = (nhblack_total/pop_total)*100,
         pcth_total = (h_total/pop_total)*100,
         pctnhwhite_total = (nhwhite_total/pop_total)*100,
         pctownerocc_total = (owneroccupied_total/hu_total)*100,
         pctincocc_total = (incopp_total/nwork_total)*100,
         pcthincjobs_total = (nhincjobs_total/njobs_total)*100,
         pctnhblackvap_total = (nhblackvap_total/vap_total)*100,
         pcthvap_total = (hvap_total/vap_total)*100,
         pctnhwhitevap_total = (nhwhitevap_total/vap_total)*100)
  
place_by_annex <- aa0713 %>%
  mutate(incopp = man + ret,
         pctincopp = (incopp/jobs)*100,
         pcthincjobs = (nhincjobs07/njobs07)*100,
         pctownerocc = (owneroccupied/hu)*100
         ) %>%
  filter(annexed==1) %>%
  group_by(plid, annexed) %>%
  summarize(pop_total = sum(pop, na.rm = T),
            nhblack_total = sum(nhblack, na.rm = T),
            nhwhite_total = sum(nhwhite, na.rm = T),
            h_total = sum(h, na.rm = T),
            njobs_total = sum(njobs07, na.rm = T),
            nhincjobs_total = sum(nhincjobs07, na.rm = T),
            nwork_total = sum(jobs, na.rm = T),
            incopp_total = sum(incopp, na.rm = T),
            pctincopp_total = mean(pctincopp, na.rm = T),
            pcthincjobs_total = mean(pcthincjobs, na.rm = T),
            pctownerocc_total = mean(pctownerocc, na.rm = T),
            hu_total = sum(hu, na.rm = T),
            owneroccupied_total = sum(owneroccupied, na.rm = T),
            vacancy_total = sum(vacancy, na.rm = T),
            vap_total = sum(vap, na.rm = T),
            nhblackvap_total = sum(nhbvap, na.rm = T),
            nhwhitevap_total = sum(nhwvap, na.rm = T),
            hvap_total = sum(hispvap, na.rm = T),
            nativevap_total = sum(nativevap, na.rm = T),
            asianvap_total = sum(asianvap, na.rm = T),
            othervap_total = sum(othervap, na.rm = T)) %>%
  ungroup() %>%
  pivot_wider(
    id_cols = plid,
    names_from = annexed,
    values_from = c(pop_total:othervap_total)
  ) %>%
  mutate(pctnhblack_total_1 = (nhblack_total_1/pop_total_1)*100,
        pctnhwhite_total_1 = (nhwhite_total_1/pop_total_1)*100,
        pcth_total_1 = (h_total_1/pop_total_1)*100,
        pctnhblackvap_total_1 = (nhblackvap_total_1/vap_total_1)*100,
        pctnhwhitevap_total_1 = (nhwhitevap_total_1/vap_total_1)*100,
        pcthvap_total_1 = (hvap_total_1/vap_total_1)*100)
  
pl_annex_var_0713 <- left_join(
  place_all, place_by_annex, 
  by = "plid"
)

# add vra indicator 
places_vra <- aa0713 %>%
  group_by(plid) %>%
  summarize(vra = mean(vra, na.rm = T),
            annexing = mean(annexed, na.rm = T)) %>%
  ungroup() %>%
  mutate(vra = ifelse(vra > 0, 1, 0),
         annexing = ifelse(annexing > 0, 1, 0))

pl_annex_var_0713 %<>%
  left_join(places_vra, by = "plid")

# add place-level data for race and vap 
# vra violation = if pctwhite after annex > pctwhite before annex compared to
# what would have been, i.e. pctwhite given growth rate by 2013
# or if pctblack after annex < pctblack before annex compared to what would 
# have been, i.e. pctwhite given growth rate by 2013
# or if pcth after annex < pcth before annex compared to what would have been,
# i.e. pcth given growth rate by 2013 
pl0007 <- read_csv("pl0007_var.csv")

table(pl_annex_var_0713$plid %in% pl0007$plid) #213 false

cdps07 <- read_csv("pl2007_cleaned.csv") %>% # want to know which places are CDPs--they do not annex
  select(Geo_NAME, plid) %>%
  mutate(cdp = ifelse(grepl("CDP|cdp", Geo_NAME), 1, 0)) %>%
  filter(cdp==1)

pl_annex_var_0713 %<>%
  filter(plid %in% pl0007$plid & !(plid %in% cdps07$plid)) %>%
  left_join(pl0007, by = "plid") %>%
  mutate(post = 0,
         state = substr(plid, 1, 2),
         pctowneroccupied07p = (owneroccupied07p/hu07p)*100) %>%
  group_by(state) %>%
  mutate(pop_tercile = ntile(pop07p, 3),
         maj_white = ifelse(pctnhwhitevap07p >= 50, 1, 0)) %>%
  ungroup()

table(pl_annex_var_0713$annexing)
table(pl_annex_var_0713$maj_white)

# make underbound variable
# # of black_annexed(2013) + blackvap(2010)/(vap2000 + annexed_vap2013) - blackvap2010/
pl_annex_var_0713 %<>%
  mutate(
    increase_whitevap = ifelse((annexing == 1 & ((((nhwhitevap_total_1 + nhwhitevap07p)/(vap07p + vap_total_1)) - (nhwhitevap07p/vap07p))) > 0), 1, 0),
    underbound_blackvap_vraa = ifelse(
      (annexing == 1 & nhblackvap_total >= 1 & increase_whitevap == 1 & vraa == 1 & ((((nhblackvap_total_1 + nhblackvap07p)/(vap_total_1 + vap07p)) - (nhblackvap07p/vap07p))) < -0.03), 1, 0), 
    underbound_hispvap_vraa = ifelse(
      (annexing == 1 & hvap_total >= 1 & increase_whitevap == 1 & vraa == 1 & ((((hvap_total_1 + hispvap07p)/(vap_total_1 + vap07p)) - (hispvap07p/vap07p))) < -0.03), 1, 0),
    underbound_asianvap_vraa = ifelse(
      (annexing == 1 & increase_whitevap == 1 & vraa == 1 & ((((asianvap_total_1 + asianvap07p)/(vap07p + vap_total_1)) - (asianvap07p/vap07p))) < -0.03), 1, 0), 
    underbound_nativevap_vraa = ifelse(
      (annexing == 1 & increase_whitevap == 1 & vraa == 1 & ((((nativevap_total_1 + nativevap07p)/(vap07p + vap_total_1)) - (nativevap07p/vap07p))) < -0.03), 1, 0),
    underbound_othervap_vraa = ifelse(
      (annexing == 1 & increase_whitevap == 1 & vraa == 1 & ((((othervap_total_1 + othervap07p)/(vap07p + vap_total_1)) - (othervap07p/vap07p))) < -0.03), 1, 0),
    underbound_nhwhitevap_vraa = ifelse(
      (annexing == 1 & nhwhitevap_total >= 1 & vraa == 1 & ((((nhwhitevap_total_1 + nhwhitevap07p)/(vap07p + vap_total_1)) - (nhwhitevap07p/vap07p))) < -0.03), 1, 0),
    underbound_blackvap_3pct = ifelse(
      (annexing == 1 & nhblackvap_total >= 1 & increase_whitevap == 1 & ((((nhblackvap_total_1 + nhblackvap07p)/(vap_total_1 + vap07p)) - (nhblackvap07p/vap07p))) < -0.03), 1, 0), 
    underbound_hispvap_3pct = ifelse(
      (annexing == 1 & hvap_total >= 1 & increase_whitevap == 1 & ((((hvap_total_1 + hispvap07p)/(vap_total_1 + vap07p)) - (hispvap07p/vap07p))) < -0.03), 1, 0),
    underbound_asianvap_3pct = ifelse(
      (annexing == 1 & increase_whitevap == 1 & ((((asianvap_total_1 + asianvap07p)/(vap07p + vap_total_1)) - (asianvap07p/vap07p))) < -0.03), 1, 0), 
    underbound_nativevap_3pct = ifelse(
      (annexing == 1 & increase_whitevap == 1 & ((((nativevap_total_1 + nativevap07p)/(vap07p + vap_total_1)) - (nativevap07p/vap07p))) < -0.03), 1, 0),
    underbound_othervap_3pct = ifelse(
      (annexing == 1 & increase_whitevap == 1 & ((((othervap_total_1 + othervap07p)/(vap07p + vap_total_1)) - (othervap07p/vap07p))) < -0.03), 1, 0),
    underbound_nhwhitevap_3pct = ifelse(
      (annexing == 1 & nhwhitevap_total >= 1 & ((((nhwhitevap_total_1 + nhwhitevap07p)/(vap07p + vap_total_1)) - (nhwhitevap07p/vap07p))) < -0.03), 1, 0),
    underbound_blackvap_hpct = ifelse(
      (annexing == 1 & nhblackvap_total >= 1 & increase_whitevap == 1 & ((((nhblackvap_total_1 + nhblackvap07p)/(vap_total_1 + vap07p)) - (nhblackvap07p/vap07p))) < -0.005), 1, 0), 
    underbound_hispvap_hpct = ifelse(
      (annexing == 1 & hvap_total >= 1 & increase_whitevap == 1 & ((((hvap_total_1 + hispvap07p)/(vap_total_1 + vap07p)) - (hispvap07p/vap07p))) < -0.005), 1, 0),
    underbound_asianvap_hpct = ifelse(
      (annexing == 1 & increase_whitevap == 1 & ((((asianvap_total_1 + asianvap07p)/(vap07p + vap_total_1)) - (asianvap07p/vap07p))) < -0.005), 1, 0), 
    underbound_nativevap_hpct = ifelse(
      (annexing == 1 & increase_whitevap == 1 & ((((nativevap_total_1 + nativevap07p)/(vap07p + vap_total_1)) - (nativevap07p/vap07p))) < -0.005), 1, 0),
    underbound_othervap_hpct = ifelse(
      (annexing == 1 & increase_whitevap == 1 & ((((othervap_total_1 + othervap07p)/(vap07p + vap_total_1)) - (othervap07p/vap07p))) < -0.005), 1, 0),
    underbound_nhwhitevap_hpct = ifelse(
      (annexing == 1 & nhwhitevap_total >= 1 & ((((nhwhitevap_total_1 + nhwhitevap07p)/(vap07p + vap_total_1)) - (nhwhitevap07p/vap07p))) < -0.005), 1, 0),
    underbound_blackvap_1pct = ifelse(
      (annexing == 1 & nhblackvap_total >= 1 & increase_whitevap == 1 & ((((nhblackvap_total_1 + nhblackvap07p)/(vap_total_1 + vap07p)) - (nhblackvap07p/vap07p))) < -0.01), 1, 0), 
    underbound_hispvap_1pct = ifelse(
      (annexing == 1 & hvap_total >= 1 & increase_whitevap == 1 & ((((hvap_total_1 + hispvap07p)/(vap_total_1 + vap07p)) - (hispvap07p/vap07p))) < -0.01), 1, 0),
    underbound_asianvap_1pct = ifelse(
      (annexing == 1 & increase_whitevap == 1 & ((((asianvap_total_1 + asianvap07p)/(vap07p + vap_total_1)) - (asianvap07p/vap07p))) < -0.01), 1, 0), 
    underbound_nativevap_1pct = ifelse(
      (annexing == 1 & increase_whitevap == 1 & ((((nativevap_total_1 + nativevap07p)/(vap07p + vap_total_1)) - (nativevap07p/vap07p))) < -0.01), 1, 0),
    underbound_othervap_1pct = ifelse(
      (annexing == 1 & increase_whitevap == 1 & ((((othervap_total_1 + othervap07p)/(vap07p + vap_total_1)) - (othervap07p/vap07p))) < -0.01), 1, 0),
    underbound_nhwhitevap_1pct = ifelse(
      (annexing == 1 & nhwhitevap_total >= 1 & ((((nhwhitevap_total_1 + nhwhitevap07p)/(vap07p + vap_total_1)) - (nhwhitevap07p/vap07p))) < -0.01), 1, 0),
    underbound_blackvap_10pct = ifelse(
      (annexing == 1 & nhblackvap_total >= 1 & increase_whitevap == 1 & ((((nhblackvap_total_1 + nhblackvap07p)/(vap_total_1 + vap07p)) - (nhblackvap07p/vap07p))) < -0.1), 1, 0), 
    underbound_hispvap_10pct = ifelse(
      (annexing == 1 & hvap_total >= 1 & increase_whitevap == 1 & ((((hvap_total_1 + hispvap07p)/(vap_total_1 + vap07p)) - (hispvap07p/vap07p))) < -0.1), 1, 0),
    underbound_asianvap_10pct = ifelse(
      (annexing == 1 & increase_whitevap == 1 & ((((asianvap_total_1 + asianvap07p)/(vap07p + vap_total_1)) - (asianvap07p/vap07p))) < -0.1), 1, 0), 
    underbound_nativevap_10pct = ifelse(
      (annexing == 1 & increase_whitevap == 1 & ((((nativevap_total_1 + nativevap07p)/(vap07p + vap_total_1)) - (nativevap07p/vap07p))) < -0.1), 1, 0),
    underbound_othervap_10pct = ifelse(
      (annexing == 1 & increase_whitevap == 1 & ((((othervap_total_1 + othervap07p)/(vap07p + vap_total_1)) - (othervap07p/vap07p))) < -0.1), 1, 0),
    underbound_nhwhitevap_10pct = ifelse(
      (annexing == 1 & nhwhitevap_total >= 1 & ((((nhwhitevap_total_1 + nhwhitevap07p)/(vap07p + vap_total_1)) - (nhwhitevap07p/vap07p))) < -0.1), 1, 0)
  )

pl_annex_var_0713 %<>%
  mutate(
    underbound_blackvap = ifelse(
      (annexing == 1 & nhblackvap_total >= 1 & increase_whitevap == 1 & ((((nhblackvap_total_1 + nhblackvap07p)/(vap_total_1 + vap07p)) < (nhblackvap07p/vap07p)))), 1, 0), 
    underbound_hispvap = ifelse(
      (annexing == 1 & hvap_total >= 1 & increase_whitevap == 1 & ((((hvap_total_1 + hispvap07p)/(vap_total_1 + vap07p)) < (hispvap07p/vap07p)))), 1, 0),
    underbound_asianvap = ifelse(
      (annexing == 1 & increase_whitevap == 1 & ((((asianvap_total_1 + asianvap07p)/(vap07p + vap_total_1)) < (asianvap07p/vap07p)))), 1, 0), 
    underbound_nativevap = ifelse(
      (annexing == 1 & increase_whitevap == 1 & ((((nativevap_total_1 + nativevap07p)/(vap07p + vap_total_1)) < (nativevap07p/vap07p)))), 1, 0),
    underbound_othervap = ifelse(
      (annexing == 1 & increase_whitevap == 1 & ((((othervap_total_1 + othervap07p)/(vap07p + vap_total_1)) < (othervap07p/vap07p)))), 1, 0),
    underbound_nhwhitevap = ifelse(
      (annexing == 1 & nhwhitevap_total >= 1 & ((((nhwhitevap_total_1 + nhwhitevap07p)/(vap07p + vap_total_1)) < (nhwhitevap07p/vap07p)))), 1, 0)
  )

# not about VAP 
pl_annex_var_0713 %<>%
  mutate(
    increase_white = ifelse((annexing == 1 & ((((nhwhite_total_1 + nhwhite07p)/(pop07p + pop_total_1)) - (nhwhite07p/pop07p))) > 0), 1, 0),
    underbound_black_vraa = ifelse(
      (annexing == 1 & nhblack_total >= 1 & increase_white == 1 & vraa == 1 & ((((nhblack_total_1 + nhblack07p)/(pop_total_1 + pop07p)) - (nhblack07p/pop07p))) < -0.03), 1, 0), 
    underbound_hisp_vraa = ifelse(
      (annexing == 1 & h_total >= 1 & increase_white == 1 & vraa == 1 & ((((h_total_1 + h07p)/(pop_total_1 + pop07p)) - (h07p/pop07p))) < -0.03), 1, 0),
    #underbound_asian_vraa = ifelse(
    #  (annexing == 1 & increase_white == 1 & vraa == 1 & ((((asian_total_1 + asian07p)/(pop07p + pop_total_1)) - (asian07p/pop07p))) < -0.03), 1, 0), 
    #underbound_native_vraa = ifelse(
    #  (annexing == 1 & increase_white == 1 & vraa == 1 & ((((native_total_1 + native07p)/(pop07p + pop_total_1)) - (native07p/pop07p))) < -0.03), 1, 0),
    #underbound_other_vraa = ifelse(
    #  (annexing == 1 & increase_white == 1 & vraa == 1 & ((((other_total_1 + other07p)/(pop07p + pop_total_1)) - (other07p/pop07p))) < -0.03), 1, 0),
    underbound_nhwhite_vraa = ifelse(
      (annexing == 1 & nhwhite_total >= 1 & vraa == 1 & ((((nhwhite_total_1 + nhwhite07p)/(pop07p + pop_total_1)) - (nhwhite07p/pop07p))) < -0.03), 1, 0),
    underbound_black_3pct = ifelse(
      (annexing == 1 & nhblack_total >= 1 & increase_white == 1 & ((((nhblack_total_1 + nhblack07p)/(pop_total_1 + pop07p)) - (nhblack07p/pop07p))) < -0.03), 1, 0), 
    underbound_hisp_3pct = ifelse(
      (annexing == 1 & h_total >= 1 & increase_white == 1 & ((((h_total_1 + h07p)/(pop_total_1 + pop07p)) - (h07p/pop07p))) < -0.03), 1, 0),
    # underbound_asian_3pct = ifelse(
    #   (annexing == 1 & increase_white == 1 & ((((asian_total_1 + asian07p)/(pop07p + pop_total_1)) - (asian07p/pop07p))) < -0.03), 1, 0), 
    # underbound_native_3pct = ifelse(
    #   (annexing == 1 & increase_white == 1 & ((((native_total_1 + native07p)/(pop07p + pop_total_1)) - (native07p/pop07p))) < -0.03), 1, 0),
    # underbound_other_3pct = ifelse(
    #   (annexing == 1 & increase_white == 1 & ((((other_total_1 + other07p)/(pop07p + pop_total_1)) - (other07p/pop07p))) < -0.03), 1, 0),
    underbound_nhwhite_3pct = ifelse(
      (annexing == 1 & nhwhite_total >= 1 & ((((nhwhite_total_1 + nhwhite07p)/(pop07p + pop_total_1)) - (nhwhite07p/pop07p))) < -0.03), 1, 0),
    underbound_black_hpct = ifelse(
      (annexing == 1 & nhblack_total >= 1 & increase_white == 1 & ((((nhblack_total_1 + nhblack07p)/(pop_total_1 + pop07p)) - (nhblack07p/pop07p))) < -0.005), 1, 0), 
    underbound_hisp_hpct = ifelse(
      (annexing == 1 & h_total >= 1 & increase_white == 1 & ((((h_total_1 + h07p)/(pop_total_1 + pop07p)) - (h07p/pop07p))) < -0.005), 1, 0),
    # underbound_asian_hpct = ifelse(
    #   (annexing == 1 & increase_white == 1 & ((((asian_total_1 + asian07p)/(pop07p + pop_total_1)) - (asian07p/pop07p))) < -0.005), 1, 0), 
    # underbound_native_hpct = ifelse(
    #   (annexing == 1 & increase_white == 1 & ((((native_total_1 + native07p)/(pop07p + pop_total_1)) - (native07p/pop07p))) < -0.005), 1, 0),
    # underbound_other_hpct = ifelse(
    #   (annexing == 1 & increase_white == 1 & ((((other_total_1 + other07p)/(pop07p + pop_total_1)) - (other07p/pop07p))) < -0.005), 1, 0),
    underbound_nhwhite_hpct = ifelse(
      (annexing == 1 & nhwhite_total >= 1 & ((((nhwhite_total_1 + nhwhite07p)/(pop07p + pop_total_1)) - (nhwhite07p/pop07p))) < -0.005), 1, 0),
    underbound_black_1pct = ifelse(
      (annexing == 1 & nhblack_total >= 1 & increase_white == 1 & ((((nhblack_total_1 + nhblack07p)/(pop_total_1 + pop07p)) - (nhblack07p/pop07p))) < -0.01), 1, 0), 
    underbound_hisp_1pct = ifelse(
      (annexing == 1 & h_total >= 1 & increase_white == 1 & ((((h_total_1 + h07p)/(pop_total_1 + pop07p)) - (h07p/pop07p))) < -0.01), 1, 0),
    # underbound_asian_1pct = ifelse(
    #   (annexing == 1 & increase_white == 1 & ((((asian_total_1 + asian07p)/(pop07p + pop_total_1)) - (asian07p/pop07p))) < -0.01), 1, 0), 
    # underbound_native_1pct = ifelse(
    #   (annexing == 1 & increase_white == 1 & ((((native_total_1 + native07p)/(pop07p + pop_total_1)) - (native07p/pop07p))) < -0.01), 1, 0),
    # underbound_other_1pct = ifelse(
    #   (annexing == 1 & increase_white == 1 & ((((other_total_1 + other07p)/(pop07p + pop_total_1)) - (other07p/pop07p))) < -0.01), 1, 0),
    underbound_nhwhite_1pct = ifelse(
      (annexing == 1 & nhwhite_total >= 1 & ((((nhwhite_total_1 + nhwhite07p)/(pop07p + pop_total_1)) - (nhwhite07p/pop07p))) < -0.01), 1, 0),
    underbound_black_10pct = ifelse(
      (annexing == 1 & nhblack_total >= 1 & increase_white == 1 & ((((nhblack_total_1 + nhblack07p)/(pop_total_1 + pop07p)) - (nhblack07p/pop07p))) < -0.1), 1, 0), 
    underbound_hisp_10pct = ifelse(
      (annexing == 1 & h_total >= 1 & increase_white == 1 & ((((h_total_1 + h07p)/(pop_total_1 + pop07p)) - (h07p/pop07p))) < -0.1), 1, 0),
    # underbound_asian_10pct = ifelse(
    #   (annexing == 1 & increase_white == 1 & ((((asian_total_1 + asian07p)/(pop07p + pop_total_1)) - (asian07p/pop07p))) < -0.1), 1, 0), 
    # underbound_native_10pct = ifelse(
    #   (annexing == 1 & increase_white == 1 & ((((native_total_1 + native07p)/(pop07p + pop_total_1)) - (native07p/pop07p))) < -0.1), 1, 0),
    # underbound_other_10pct = ifelse(
    #   (annexing == 1 & increase_white == 1 & ((((other_total_1 + other07p)/(pop07p + pop_total_1)) - (other07p/pop07p))) < -0.1), 1, 0),
    underbound_nhwhite_10pct = ifelse(
      (annexing == 1 & nhwhite_total >= 1 & ((((nhwhite_total_1 + nhwhite07p)/(pop07p + pop_total_1)) - (nhwhite07p/pop07p))) < -0.1), 1, 0)
  )

pl_annex_var_0713 %<>%
  mutate(
    underbound_black = ifelse(
      (annexing == 1 & nhblack_total >= 1 & increase_white == 1 & ((((nhblack_total_1 + nhblack07p)/(pop_total_1 + pop07p)) < (nhblack07p/pop07p)))), 1, 0), 
    underbound_hisp = ifelse(
      (annexing == 1 & h_total >= 1 & increase_white == 1 & ((((h_total_1 + h07p)/(pop_total_1 + pop07p)) < (h07p/pop07p)))), 1, 0),
    # underbound_asian = ifelse(
    #   (annexing == 1 & increase_white == 1 & ((((asian_total_1 + asian07p)/(pop07p + pop_total_1)) < (asian07p/pop07p)))), 1, 0), 
    # underbound_native = ifelse(
    #   (annexing == 1 & increase_white == 1 & ((((native_total_1 + native07p)/(pop07p + pop_total_1)) < (native07p/pop07p)))), 1, 0),
    # underbound_other = ifelse(
    #   (annexing == 1 & increase_white == 1 & ((((other_total_1 + other07p)/(pop07p + pop_total_1)) < (other07p/pop07p)))), 1, 0),
    underbound_nhwhite = ifelse(
      (annexing == 1 & nhwhite_total >= 1 & ((((nhwhite_total_1 + nhwhite07p)/(pop07p + pop_total_1)) < (nhwhite07p/pop07p)))), 1, 0)
  )

# compare SES
pl_annex_var_0713 %<>% 
  mutate(
    property_inc = ifelse(
      (annexing == 1 & owneroccupied_total >= 1 & (((owneroccupied_total_1 + owneroccupied07p)/(hu07p + hu_total_1)) > (hu07p + hu_total_1))), 1, 0),
    hincjobs = ifelse(
      (annexing == 1 & nhincjobs_total >= 1 & (((nhincjobs_total_1 + overlodesthresh07p)/(emp07p + njobs_total_1)) > (emp07p + njobs_total_1))), 1, 0)
  )

pl_annex_var_0713 %<>%
  filter(pop_total > 0) 

table(pl_annex_var_0713$underbound_blackvap)
table(pl_annex_var_0713$underbound_hispvap)
table(pl_annex_var_0713$underbound_asianvap)
table(pl_annex_var_0713$underbound_nativevap)
table(pl_annex_var_0713$underbound_othervap)
table(pl_annex_var_0713$underbound_nhwhitevap)
table(pl_annex_var_0713$underbound_blackvap_vraa)
table(pl_annex_var_0713$underbound_hispvap_vraa)
table(pl_annex_var_0713$underbound_asianvap_vraa)
table(pl_annex_var_0713$underbound_nativevap_vraa)
table(pl_annex_var_0713$underbound_othervap_vraa)
table(pl_annex_var_0713$underbound_nhwhitevap_vraa)
table(pl_annex_var_0713$underbound_blackvap_3pct)
table(pl_annex_var_0713$underbound_hispvap_3pct)
table(pl_annex_var_0713$underbound_asianvap_3pct)
table(pl_annex_var_0713$underbound_nativevap_3pct)
table(pl_annex_var_0713$underbound_othervap_3pct)
table(pl_annex_var_0713$underbound_nhwhitevap_3pct)
table(pl_annex_var_0713$underbound_blackvap_hpct)
table(pl_annex_var_0713$underbound_hispvap_hpct)
table(pl_annex_var_0713$underbound_asianvap_hpct)
table(pl_annex_var_0713$underbound_nativevap_hpct)
table(pl_annex_var_0713$underbound_othervap_hpct)
table(pl_annex_var_0713$underbound_nhwhitevap_hpct)
table(pl_annex_var_0713$underbound_blackvap_1pct)
table(pl_annex_var_0713$underbound_hispvap_1pct)
table(pl_annex_var_0713$underbound_asianvap_1pct)
table(pl_annex_var_0713$underbound_nativevap_1pct)
table(pl_annex_var_0713$underbound_othervap_1pct)
table(pl_annex_var_0713$underbound_nhwhitevap_1pct)
table(pl_annex_var_0713$underbound_blackvap_10pct)
table(pl_annex_var_0713$underbound_hispvap_10pct)
table(pl_annex_var_0713$underbound_asianvap_10pct)
table(pl_annex_var_0713$underbound_nativevap_10pct)
table(pl_annex_var_0713$underbound_othervap_10pct)
table(pl_annex_var_0713$underbound_nhwhitevap_10pct)
table(pl_annex_var_0713$underbound_black)
table(pl_annex_var_0713$underbound_hisp)
table(pl_annex_var_0713$underbound_nhwhite)
table(pl_annex_var_0713$underbound_black_vraa)
table(pl_annex_var_0713$underbound_hisp_vraa)
table(pl_annex_var_0713$underbound_nhwhite_vraa)
table(pl_annex_var_0713$underbound_black_3pct)
table(pl_annex_var_0713$underbound_hisp_3pct)
table(pl_annex_var_0713$underbound_nhwhite_3pct)
table(pl_annex_var_0713$underbound_black_hpct)
table(pl_annex_var_0713$underbound_hisp_hpct)
table(pl_annex_var_0713$underbound_nhwhite_hpct)
table(pl_annex_var_0713$underbound_black_1pct)
table(pl_annex_var_0713$underbound_hisp_1pct)
table(pl_annex_var_0713$underbound_nhwhite_1pct)
table(pl_annex_var_0713$underbound_black_10pct)
table(pl_annex_var_0713$underbound_hisp_10pct)
table(pl_annex_var_0713$underbound_nhwhite_10pct)
table(pl_annex_var_0713$property_inc)
table(pl_annex_var_0713$hincjobs)

names(pl_annex_var_0713)
pl_annex_var_0713 %<>%
  select(-ends_with("00p"))
names(pl_annex_var_0713)

names(pl_annex_var_0713) <- gsub("07p", "_p0", names(pl_annex_var_0713))
names(pl_annex_var_0713)

# merge in 2013 data 
places2013 <- read_csv("acs13.csv")

pl_annex_var_0713 %<>%
  left_join(places2013 %>% select(-Geo_NAME), by = "plid")

names(pl_annex_var_0713)
names(pl_annex_var_0713) <- gsub("13p", "_p1", names(pl_annex_var_0713))

write_csv(pl_annex_var_0713, "analyticalfiles/pl_annex_var_0713.csv")
pl_annex_var_0713 <- read_csv("analyticalfiles/pl_annex_var_0713.csv")

nhb <- pl_annex_var_0713 %>%
  filter(nhblack_total >= 1) %>%
  summarize_at(vars(c(contains("underbound") & contains("black") & !contains("vap"))), ~mean(., na.rm = T)) %>%
  mutate(Year = "2007 to 2013") %>%
  pivot_longer(cols = !c(Year),
               names_to = "underbound") %>%
  mutate(vra_basis = case_when(
    grepl("vraa", underbound) ~ "JLVRAA",
    grepl("_hpct", underbound) ~ "VRA, 0.5%",
    grepl("_1pct", underbound) ~ "VRA, 1%",
    grepl("_3pct", underbound) ~ "VRA, 3%",
    grepl("_10pct", underbound) ~ "VRA, 10%",
    TRUE ~ "VRA, 0%"
  ),
  race = "Non-Hispanic Black")

h <- pl_annex_var_0713 %>%
  filter(h_total >= 1) %>%
  summarize_at(vars(c(contains("underbound") & contains("hisp") & !contains("vap"))), ~mean(., na.rm = T)) %>%
  mutate(Year = "2007 to 2013") %>%
  pivot_longer(cols = !c(Year),
               names_to = "underbound") %>%
  mutate(vra_basis = case_when(
    grepl("vraa", underbound) ~ "JLVRAA",
    grepl("_hpct", underbound) ~ "VRA, 0.5%",
    grepl("_1pct", underbound) ~ "VRA, 1%",
    grepl("_3pct", underbound) ~ "VRA, 3%",
    grepl("_10pct", underbound) ~ "VRA, 10%",
    TRUE ~ "VRA, 0%"
  ),
  race = "Hispanic")

nhw <- pl_annex_var_0713 %>%
  filter(nhwhite_total >= 1) %>%
  summarize_at(vars(c(contains("underbound") & contains("white") & !contains("vap"))), ~mean(., na.rm = T)) %>%
  mutate(Year = "2007 to 2013") %>%
  pivot_longer(cols = !c(Year),
               names_to = "underbound") %>%
  mutate(vra_basis = case_when(
    grepl("vraa", underbound) ~ "JLVRAA",
    grepl("_hpct", underbound) ~ "VRA, 0.5%",
    grepl("_1pct", underbound) ~ "VRA, 1%",
    grepl("_3pct", underbound) ~ "VRA, 3%",
    grepl("_10pct", underbound) ~ "VRA, 10%",
    TRUE ~ "VRA, 0%"
  ),
  race = "Non-Hispanic White")

desc0713 <- base::rbind(nhb, h, nhw)

nhbvap <- pl_annex_var_0713 %>%
  filter(nhblackvap_total >= 1) %>%
  summarize_at(vars(c(contains("underbound") & contains("black") & contains("vap"))), ~mean(., na.rm = T)) %>%
  mutate(Year = "2007 to 2013") %>%
  pivot_longer(cols = !c(Year),
               names_to = "underbound") %>%
  mutate(vra_basis = case_when(
    grepl("vraa", underbound) ~ "JLVRAA",
    grepl("_hpct", underbound) ~ "VRA, 0.5%",
    grepl("_1pct", underbound) ~ "VRA, 1%",
    grepl("_3pct", underbound) ~ "VRA, 3%",
    grepl("_10pct", underbound) ~ "VRA, 10%",
    TRUE ~ "VRA, 0%"
  ),
  race = "Non-Hispanic Black")

hvap <- pl_annex_var_0713 %>%
  filter(hvap_total >= 1) %>%
  summarize_at(vars(c(contains("underbound") & contains("hisp") & contains("vap"))), ~mean(., na.rm = T)) %>%
  mutate(Year = "2007 to 2013") %>%
  pivot_longer(cols = !c(Year),
               names_to = "underbound") %>%
  mutate(vra_basis = case_when(
    grepl("vraa", underbound) ~ "JLVRAA",
    grepl("_hpct", underbound) ~ "VRA, 0.5%",
    grepl("_1pct", underbound) ~ "VRA, 1%",
    grepl("_3pct", underbound) ~ "VRA, 3%",
    grepl("_10pct", underbound) ~ "VRA, 10%",
    TRUE ~ "VRA, 0%"
  ),
  race = "Hispanic")

nhwvap <- pl_annex_var_0713 %>%
  filter(nhwhitevap_total >= 1) %>%
  summarize_at(vars(c(contains("underbound") & contains("white") & contains("vap"))), ~mean(., na.rm = T)) %>%
  mutate(Year = "2007 to 2013") %>%
  pivot_longer(cols = !c(Year),
               names_to = "underbound") %>%
  mutate(vra_basis = case_when(
    grepl("vraa", underbound) ~ "JLVRAA",
    grepl("_hpct", underbound) ~ "VRA, 0.5%",
    grepl("_1pct", underbound) ~ "VRA, 1%",
    grepl("_3pct", underbound) ~ "VRA, 3%",
    grepl("_10pct", underbound) ~ "VRA, 10%",
    TRUE ~ "VRA, 0%"
  ),
  race = "Non-Hispanic White")

desc0713vap <- base::rbind(nhbvap, hvap, nhwvap)
rm(aa0713, pl_annex_var_0713, pl0007, places2013, cdps07)

#repeat for 1420 ####
aa1420 <- read_csv("analyticalfiles/annexedblocks1420dem.csv")
names(aa1420)
summary(aa1420$pop)
summary(aa1420$vap)
aa1420 %<>%
  filter(!is.na(pop))

# transform this into place-level summaries
# characteristic of all annexable blocks
# diff between annexed and not annexed 
place_all <- aa1420 %>% 
  mutate(incopp = man + ret) %>%
  group_by(plid) %>%
  summarize(pop_total = sum(pop, na.rm = T),
            nhblack_total = sum(nhblack, na.rm = T),
            nhwhite_total = sum(nhwhite, na.rm = T),
            h_total = sum(h, na.rm = T),
            vap_total = sum(vap, na.rm = T),
            nhblackvap_total = sum(nhbvap, na.rm = T),
            nhwhitevap_total = sum(nhwvap, na.rm = T),
            hvap_total = sum(hispvap, na.rm = T),
            nativevap_total = sum(nativevap, na.rm = T),
            asianvap_total = sum(asianvap, na.rm = T),
            othervap_total = sum(othervap, na.rm = T),
            pct_annexed = mean(annexed, na.rm = T),
            njobs_total = sum(njobs14, na.rm = T),
            nhincjobs_total = sum(nhincjobs14, na.rm = T),
            nwork_total = sum(jobs, na.rm = T),
            incopp_total = sum(incopp, na.rm = T),
            hu_total = sum(hu, na.rm = T),
            owneroccupied_total = sum(owneroccupied, na.rm = T),
            vacancy_total = sum(vacancy, na.rm = T)
  ) %>%
  ungroup() %>%
  mutate(pctnhblack_total = (nhblack_total/pop_total)*100,
         pcth_total = (h_total/pop_total)*100,
         pctnhwhite_total = (nhwhite_total/pop_total)*100,
         pctownerocc_total = (owneroccupied_total/hu_total)*100,
         pctincocc_total = (incopp_total/nwork_total)*100,
         pcthincjobs_total = (nhincjobs_total/njobs_total)*100,
         pctnhblackvap_total = (nhblackvap_total/vap_total)*100,
         pcthvap_total = (hvap_total/vap_total)*100,
         pctnhwhitevap_total = (nhwhitevap_total/vap_total)*100)

place_by_annex <- aa1420 %>%
  mutate(incopp = man + ret,
         pctincopp = (incopp/jobs)*100,
         pcthincjobs = (nhincjobs14/njobs14)*100,
         pctownerocc = (owneroccupied/hu)*100) %>%
  filter(annexed==1) %>%
  group_by(plid, annexed) %>%
  summarize(pop_total = sum(pop, na.rm = T),
            nhblack_total = sum(nhblack, na.rm = T),
            nhwhite_total = sum(nhwhite, na.rm = T),
            h_total = sum(h, na.rm = T),
            pctnhblack_total = mean(pctnhblack, na.rm = T),
            pctnhwhite_total = mean(pctnhwhite, na.rm = T),
            pcth_total = mean(pcth, na.rm = T),
            njobs_total = sum(njobs14, na.rm = T),
            nhincjobs_total = sum(nhincjobs14, na.rm = T),
            nwork_total = sum(jobs, na.rm = T),
            incopp_total = sum(incopp, na.rm = T),
            hu_total = sum(hu, na.rm = T),
            owneroccupied_total = sum(owneroccupied, na.rm = T),
            pctincopp_total = mean(pctincopp, na.rm = T),
            pcthincjobs_total = mean(pcthincjobs, na.rm = T),
            pctownerocc_total = mean(pctownerocc, na.rm = T),
            vacancy_total = sum(vacancy, na.rm = T),
            vap_total = sum(vap, na.rm = T),
            nhblackvap_total = sum(nhbvap, na.rm = T),
            nhwhitevap_total = sum(nhwvap, na.rm = T),
            hvap_total = sum(hispvap, na.rm = T),
            nativevap_total = sum(nativevap, na.rm = T),
            asianvap_total = sum(asianvap, na.rm = T),
            othervap_total = sum(othervap, na.rm = T)) %>%
  ungroup() %>%
  pivot_wider(
    id_cols = plid,
    names_from = annexed,
    values_from = c(pop_total:othervap_total)
  ) %>%
  mutate(pctnhblack_total_1 = (nhblack_total_1/pop_total_1)*100,
         pctnhwhite_total_1 = (nhwhite_total_1/pop_total_1)*100,
         pcth_total_1 = (h_total_1/pop_total_1)*100,
         pctnhblackvap_total_1 = (nhblackvap_total_1/vap_total_1)*100,
         pctnhwhitevap_total_1 = (nhwhitevap_total_1/vap_total_1)*100,
         pcthvap_total_1 = (hvap_total_1/vap_total_1)*100)

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
pl0714 <- read_csv("pl0714_var.csv")

table(pl_annex_var_1420$plid %in% pl0714$plid) #213 false

cdps14 <- read_csv("places2014_cleaned.csv") %>% # want to know which places are CDPs--they do not annex
  select(Geo_NAME, plid) %>%
  mutate(cdp = ifelse(grepl("CDP|cdp", Geo_NAME), 1, 0)) %>%
  filter(cdp==1)

pl_annex_var_1420 %<>%
  filter(plid %in% pl0714$plid & !(plid %in% cdps14$plid)) %>%
  left_join(pl0714, by = "plid") %>%
  mutate(post = 1,
         pctowneroccupied14p = (owneroccupied14p/hu14p)*100) %>%
  group_by(substr(plid, 1, 2)) %>%
  mutate(pop_tercile = ntile(pop14p, 3),
         maj_white = ifelse(pctnhwhitevap14p >= 50, 1, 0)) %>%
  ungroup()

table(pl_annex_var_1420$annexing)
table(pl_annex_var_1420$maj_white)

# make underbound variable
# # of black_annexed(2013) + blackvap(2010)/(vap2000 + annexed_vap2013) - blackvap2010/
pl_annex_var_1420 %<>%
  mutate(
    increase_whitevap = ifelse((annexing == 1 & ((((nhwhitevap_total_1 + nhwhitevap14p)/(vap14p + vap_total_1)) - (nhwhitevap14p/vap14p))) > 0), 1, 0),
    underbound_blackvap_vraa = ifelse(
      (annexing == 1 & nhblackvap_total >= 1 & increase_whitevap == 1 & vraa == 1 & ((((nhblackvap_total_1 + nhblackvap14p)/(vap_total_1 + vap14p)) - (nhblackvap14p/vap14p))) < -0.03), 1, 0), 
    underbound_hispvap_vraa = ifelse(
      (annexing == 1 & hvap_total >= 1 & increase_whitevap == 1 & vraa == 1 & ((((hvap_total_1 + hispvap14p)/(vap_total_1 + vap14p)) - (hispvap14p/vap14p))) < -0.03), 1, 0),
    underbound_asianvap_vraa = ifelse(
      (annexing == 1 & increase_whitevap == 1 & vraa == 1 & ((((asianvap_total_1 + asianvap14p)/(vap14p + vap_total_1)) - (asianvap14p/vap14p))) < -0.03), 1, 0), 
    underbound_nativevap_vraa = ifelse(
      (annexing == 1 & increase_whitevap == 1 & vraa == 1 & ((((nativevap_total_1 + nativevap14p)/(vap14p + vap_total_1)) - (nativevap14p/vap14p))) < -0.03), 1, 0),
    underbound_othervap_vraa = ifelse(
      (annexing == 1 & increase_whitevap == 1 & vraa == 1 & ((((othervap_total_1 + othervap14p)/(vap14p + vap_total_1)) - (othervap14p/vap14p))) < -0.03), 1, 0),
    underbound_nhwhitevap_vraa = ifelse(
      (annexing == 1 & nhwhitevap_total >= 1 & vraa == 1 & ((((nhwhitevap_total_1 + nhwhitevap14p)/(vap14p + vap_total_1)) - (nhwhitevap14p/vap14p))) < -0.03), 1, 0),
    underbound_blackvap_3pct = ifelse(
      (annexing == 1 & nhblackvap_total >= 1 & increase_whitevap == 1 & ((((nhblackvap_total_1 + nhblackvap14p)/(vap_total_1 + vap14p)) - (nhblackvap14p/vap14p))) < -0.03), 1, 0), 
    underbound_hispvap_3pct = ifelse(
      (annexing == 1 & hvap_total >= 1 & increase_whitevap == 1 & ((((hvap_total_1 + hispvap14p)/(vap_total_1 + vap14p)) - (hispvap14p/vap14p))) < -0.03), 1, 0),
    underbound_asianvap_3pct = ifelse(
      (annexing == 1 & increase_whitevap == 1 & ((((asianvap_total_1 + asianvap14p)/(vap14p + vap_total_1)) - (asianvap14p/vap14p))) < -0.03), 1, 0), 
    underbound_nativevap_3pct = ifelse(
      (annexing == 1 & increase_whitevap == 1 & ((((nativevap_total_1 + nativevap14p)/(vap14p + vap_total_1)) - (nativevap14p/vap14p))) < -0.03), 1, 0),
    underbound_othervap_3pct = ifelse(
      (annexing == 1 & increase_whitevap == 1 & ((((othervap_total_1 + othervap14p)/(vap14p + vap_total_1)) - (othervap14p/vap14p))) < -0.03), 1, 0),
    underbound_nhwhitevap_3pct = ifelse(
      (annexing == 1 & nhwhitevap_total >= 1 & ((((nhwhitevap_total_1 + nhwhitevap14p)/(vap14p + vap_total_1)) - (nhwhitevap14p/vap14p))) < -0.03), 1, 0),
    underbound_blackvap_hpct = ifelse(
      (annexing == 1 & nhblackvap_total >= 1 & increase_whitevap == 1 & ((((nhblackvap_total_1 + nhblackvap14p)/(vap_total_1 + vap14p)) - (nhblackvap14p/vap14p))) < -0.005), 1, 0), 
    underbound_hispvap_hpct = ifelse(
      (annexing == 1 & hvap_total >= 1 & increase_whitevap == 1 & ((((hvap_total_1 + hispvap14p)/(vap_total_1 + vap14p)) - (hispvap14p/vap14p))) < -0.005), 1, 0),
    underbound_asianvap_hpct = ifelse(
      (annexing == 1 & increase_whitevap == 1 & ((((asianvap_total_1 + asianvap14p)/(vap14p + vap_total_1)) - (asianvap14p/vap14p))) < -0.005), 1, 0), 
    underbound_nativevap_hpct = ifelse(
      (annexing == 1 & increase_whitevap == 1 & ((((nativevap_total_1 + nativevap14p)/(vap14p + vap_total_1)) - (nativevap14p/vap14p))) < -0.005), 1, 0),
    underbound_othervap_hpct = ifelse(
      (annexing == 1 & increase_whitevap == 1 & ((((othervap_total_1 + othervap14p)/(vap14p + vap_total_1)) - (othervap14p/vap14p))) < -0.005), 1, 0),
    underbound_nhwhitevap_hpct = ifelse(
      (annexing == 1 & nhwhitevap_total >= 1 & ((((nhwhitevap_total_1 + nhwhitevap14p)/(vap14p + vap_total_1)) - (nhwhitevap14p/vap14p))) < -0.005), 1, 0),
    underbound_blackvap_1pct = ifelse(
      (annexing == 1 & nhblackvap_total >= 1 & increase_whitevap == 1 & ((((nhblackvap_total_1 + nhblackvap14p)/(vap_total_1 + vap14p)) - (nhblackvap14p/vap14p))) < -0.01), 1, 0), 
    underbound_hispvap_1pct = ifelse(
      (annexing == 1 & hvap_total >= 1 & increase_whitevap == 1 & ((((hvap_total_1 + hispvap14p)/(vap_total_1 + vap14p)) - (hispvap14p/vap14p))) < -0.01), 1, 0),
    underbound_asianvap_1pct = ifelse(
      (annexing == 1 & increase_whitevap == 1 & ((((asianvap_total_1 + asianvap14p)/(vap14p + vap_total_1)) - (asianvap14p/vap14p))) < -0.01), 1, 0), 
    underbound_nativevap_1pct = ifelse(
      (annexing == 1 & increase_whitevap == 1 & ((((nativevap_total_1 + nativevap14p)/(vap14p + vap_total_1)) - (nativevap14p/vap14p))) < -0.01), 1, 0),
    underbound_othervap_1pct = ifelse(
      (annexing == 1 & increase_whitevap == 1 & ((((othervap_total_1 + othervap14p)/(vap14p + vap_total_1)) - (othervap14p/vap14p))) < -0.01), 1, 0),
    underbound_nhwhitevap_1pct = ifelse(
      (annexing == 1 & nhwhitevap_total >= 1 & ((((nhwhitevap_total_1 + nhwhitevap14p)/(vap14p + vap_total_1)) - (nhwhitevap14p/vap14p))) < -0.01), 1, 0),
    underbound_blackvap_10pct = ifelse(
      (annexing == 1 & nhblackvap_total >= 1 & increase_whitevap == 1 & ((((nhblackvap_total_1 + nhblackvap14p)/(vap_total_1 + vap14p)) - (nhblackvap14p/vap14p))) < -0.1), 1, 0), 
    underbound_hispvap_10pct = ifelse(
      (annexing == 1 & hvap_total >= 1 & increase_whitevap == 1 & ((((hvap_total_1 + hispvap14p)/(vap_total_1 + vap14p)) - (hispvap14p/vap14p))) < -0.1), 1, 0),
    underbound_asianvap_10pct = ifelse(
      (annexing == 1 & increase_whitevap == 1 & ((((asianvap_total_1 + asianvap14p)/(vap14p + vap_total_1)) - (asianvap14p/vap14p))) < -0.1), 1, 0), 
    underbound_nativevap_10pct = ifelse(
      (annexing == 1 & increase_whitevap == 1 & ((((nativevap_total_1 + nativevap14p)/(vap14p + vap_total_1)) - (nativevap14p/vap14p))) < -0.1), 1, 0),
    underbound_othervap_10pct = ifelse(
      (annexing == 1 & increase_whitevap == 1 & ((((othervap_total_1 + othervap14p)/(vap14p + vap_total_1)) - (othervap14p/vap14p))) < -0.1), 1, 0),
    underbound_nhwhitevap_10pct = ifelse(
      (annexing == 1 & nhwhitevap_total >= 1 & ((((nhwhitevap_total_1 + nhwhitevap14p)/(vap14p + vap_total_1)) - (nhwhitevap14p/vap14p))) < -0.1), 1, 0)
  )

pl_annex_var_1420 %<>%
  mutate(
    underbound_blackvap = ifelse(
      (annexing == 1 & nhblackvap_total >= 1 & increase_whitevap == 1 & ((((nhblackvap_total_1 + nhblackvap14p)/(vap_total_1 + vap14p)) < (nhblackvap14p/vap14p)))), 1, 0), 
    underbound_hispvap = ifelse(
      (annexing == 1 & hvap_total >= 1 & increase_whitevap == 1 & ((((hvap_total_1 + hispvap14p)/(vap_total_1 + vap14p)) < (hispvap14p/vap14p)))), 1, 0),
    underbound_asianvap = ifelse(
      (annexing == 1 & increase_whitevap == 1 & ((((asianvap_total_1 + asianvap14p)/(vap14p + vap_total_1)) < (asianvap14p/vap14p)))), 1, 0), 
    underbound_nativevap = ifelse(
      (annexing == 1 & increase_whitevap == 1 & ((((nativevap_total_1 + nativevap14p)/(vap14p + vap_total_1)) < (nativevap14p/vap14p)))), 1, 0),
    underbound_othervap = ifelse(
      (annexing == 1 & increase_whitevap == 1 & ((((othervap_total_1 + othervap14p)/(vap14p + vap_total_1)) < (othervap14p/vap14p)))), 1, 0),
    underbound_nhwhitevap = ifelse(
      (annexing == 1 & nhwhitevap_total >= 1 & ((((nhwhitevap_total_1 + nhwhitevap14p)/(vap14p + vap_total_1)) < (nhwhitevap14p/vap14p)))), 1, 0)
  )

# not vap 
pl_annex_var_1420 %<>%
  mutate(
    increase_white = ifelse((annexing == 1 & ((((nhwhite_total_1 + nhwhite14p)/(pop14p + pop_total_1)) - (nhwhite14p/pop14p))) > 0), 1, 0),
    underbound_black_vraa = ifelse(
      (annexing == 1 & nhblack_total >= 1 & increase_white == 1 & vraa == 1 & ((((nhblack_total_1 + nhblack14p)/(pop_total_1 + pop14p)) - (nhblack14p/pop14p))) < -0.03), 1, 0), 
    underbound_hisp_vraa = ifelse(
      (annexing == 1 & h_total >= 1 & increase_white == 1 & vraa == 1 & ((((h_total_1 + h14p)/(pop_total_1 + pop14p)) - (h14p/pop14p))) < -0.03), 1, 0),
    #underbound_asian_vraa = ifelse(
    #  (annexing == 1 & increase_white == 1 & vraa == 1 & ((((asian_total_1 + asian14p)/(pop14p + pop_total_1)) - (asian14p/pop14p))) < -0.03), 1, 0), 
    #underbound_native_vraa = ifelse(
    #  (annexing == 1 & increase_white == 1 & vraa == 1 & ((((native_total_1 + native14p)/(pop14p + pop_total_1)) - (native14p/pop14p))) < -0.03), 1, 0),
    #underbound_other_vraa = ifelse(
    #  (annexing == 1 & increase_white == 1 & vraa == 1 & ((((other_total_1 + other14p)/(pop14p + pop_total_1)) - (other14p/pop14p))) < -0.03), 1, 0),
    underbound_nhwhite_vraa = ifelse(
      (annexing == 1 & nhwhite_total >= 1 & vraa == 1 & ((((nhwhite_total_1 + nhwhite14p)/(pop14p + pop_total_1)) - (nhwhite14p/pop14p))) < -0.03), 1, 0),
    underbound_black_3pct = ifelse(
      (annexing == 1 & nhblack_total >= 1 & increase_white == 1 & ((((nhblack_total_1 + nhblack14p)/(pop_total_1 + pop14p)) - (nhblack14p/pop14p))) < -0.03), 1, 0), 
    underbound_hisp_3pct = ifelse(
      (annexing == 1 & h_total >= 1 & increase_white == 1 & ((((h_total_1 + h14p)/(pop_total_1 + pop14p)) - (h14p/pop14p))) < -0.03), 1, 0),
    # underbound_asian_3pct = ifelse(
    #   (annexing == 1 & increase_white == 1 & ((((asian_total_1 + asian14p)/(pop14p + pop_total_1)) - (asian14p/pop14p))) < -0.03), 1, 0), 
    # underbound_native_3pct = ifelse(
    #   (annexing == 1 & increase_white == 1 & ((((native_total_1 + native14p)/(pop14p + pop_total_1)) - (native14p/pop14p))) < -0.03), 1, 0),
    # underbound_other_3pct = ifelse(
    #   (annexing == 1 & increase_white == 1 & ((((other_total_1 + other14p)/(pop14p + pop_total_1)) - (other14p/pop14p))) < -0.03), 1, 0),
    underbound_nhwhite_3pct = ifelse(
      (annexing == 1 & nhwhite_total >= 1 & ((((nhwhite_total_1 + nhwhite14p)/(pop14p + pop_total_1)) - (nhwhite14p/pop14p))) < -0.03), 1, 0),
    underbound_black_hpct = ifelse(
      (annexing == 1 & nhblack_total >= 1 & increase_white == 1 & ((((nhblack_total_1 + nhblack14p)/(pop_total_1 + pop14p)) - (nhblack14p/pop14p))) < -0.005), 1, 0), 
    underbound_hisp_hpct = ifelse(
      (annexing == 1 & h_total >= 1 & increase_white == 1 & ((((h_total_1 + h14p)/(pop_total_1 + pop14p)) - (h14p/pop14p))) < -0.005), 1, 0),
    # underbound_asian_hpct = ifelse(
    #   (annexing == 1 & increase_white == 1 & ((((asian_total_1 + asian14p)/(pop14p + pop_total_1)) - (asian14p/pop14p))) < -0.005), 1, 0), 
    # underbound_native_hpct = ifelse(
    #   (annexing == 1 & increase_white == 1 & ((((native_total_1 + native14p)/(pop14p + pop_total_1)) - (native14p/pop14p))) < -0.005), 1, 0),
    # underbound_other_hpct = ifelse(
    #   (annexing == 1 & increase_white == 1 & ((((other_total_1 + other14p)/(pop14p + pop_total_1)) - (other14p/pop14p))) < -0.005), 1, 0),
    underbound_nhwhite_hpct = ifelse(
      (annexing == 1 & nhwhite_total >= 1 & ((((nhwhite_total_1 + nhwhite14p)/(pop14p + pop_total_1)) - (nhwhite14p/pop14p))) < -0.005), 1, 0),
    underbound_black_1pct = ifelse(
      (annexing == 1 & nhblack_total >= 1 & increase_white == 1 & ((((nhblack_total_1 + nhblack14p)/(pop_total_1 + pop14p)) - (nhblack14p/pop14p))) < -0.01), 1, 0), 
    underbound_hisp_1pct = ifelse(
      (annexing == 1 & h_total >= 1 & increase_white == 1 & ((((h_total_1 + h14p)/(pop_total_1 + pop14p)) - (h14p/pop14p))) < -0.01), 1, 0),
    # underbound_asian_1pct = ifelse(
    #   (annexing == 1 & increase_white == 1 & ((((asian_total_1 + asian14p)/(pop14p + pop_total_1)) - (asian14p/pop14p))) < -0.01), 1, 0), 
    # underbound_native_1pct = ifelse(
    #   (annexing == 1 & increase_white == 1 & ((((native_total_1 + native14p)/(pop14p + pop_total_1)) - (native14p/pop14p))) < -0.01), 1, 0),
    # underbound_other_1pct = ifelse(
    #   (annexing == 1 & increase_white == 1 & ((((other_total_1 + other14p)/(pop14p + pop_total_1)) - (other14p/pop14p))) < -0.01), 1, 0),
    underbound_nhwhite_1pct = ifelse(
      (annexing == 1 & nhwhite_total >= 1 & ((((nhwhite_total_1 + nhwhite14p)/(pop14p + pop_total_1)) - (nhwhite14p/pop14p))) < -0.01), 1, 0),
    underbound_black_10pct = ifelse(
      (annexing == 1 & nhblack_total >= 1 & increase_white == 1 & ((((nhblack_total_1 + nhblack14p)/(pop_total_1 + pop14p)) - (nhblack14p/pop14p))) < -0.1), 1, 0), 
    underbound_hisp_10pct = ifelse(
      (annexing == 1 & h_total >= 1 & increase_white == 1 & ((((h_total_1 + h14p)/(pop_total_1 + pop14p)) - (h14p/pop14p))) < -0.1), 1, 0),
    # underbound_asian_10pct = ifelse(
    #   (annexing == 1 & increase_white == 1 & ((((asian_total_1 + asian14p)/(pop14p + pop_total_1)) - (asian14p/pop14p))) < -0.1), 1, 0), 
    # underbound_native_10pct = ifelse(
    #   (annexing == 1 & increase_white == 1 & ((((native_total_1 + native14p)/(pop14p + pop_total_1)) - (native14p/pop14p))) < -0.1), 1, 0),
    # underbound_other_10pct = ifelse(
    #   (annexing == 1 & increase_white == 1 & ((((other_total_1 + other14p)/(pop14p + pop_total_1)) - (other14p/pop14p))) < -0.1), 1, 0),
    underbound_nhwhite_10pct = ifelse(
      (annexing == 1 & nhwhite_total >= 1 & ((((nhwhite_total_1 + nhwhite14p)/(pop14p + pop_total_1)) - (nhwhite14p/pop14p))) < -0.1), 1, 0)
  )

pl_annex_var_1420 %<>%
  mutate(
    underbound_black = ifelse(
      (annexing == 1 & nhblack_total >= 1 & increase_white == 1 & ((((nhblack_total_1 + nhblack14p)/(pop_total_1 + pop14p)) < (nhblack14p/pop14p)))), 1, 0), 
    underbound_hisp = ifelse(
      (annexing == 1 & h_total >= 1 & increase_white == 1 & ((((h_total_1 + h14p)/(pop_total_1 + pop14p)) < (h14p/pop14p)))), 1, 0),
    # underbound_asian = ifelse(
    #   (annexing == 1 & increase_white == 1 & ((((asian_total_1 + asian14p)/(pop14p + pop_total_1)) < (asian14p/pop14p)))), 1, 0), 
    # underbound_native = ifelse(
    #   (annexing == 1 & increase_white == 1 & ((((native_total_1 + native14p)/(pop14p + pop_total_1)) < (native14p/pop14p)))), 1, 0),
    # underbound_other = ifelse(
    #   (annexing == 1 & increase_white == 1 & ((((other_total_1 + other14p)/(pop14p + pop_total_1)) < (other14p/pop14p)))), 1, 0),
    underbound_nhwhite = ifelse(
      (annexing == 1 & nhwhite_total >= 1 & ((((nhwhite_total_1 + nhwhite14p)/(pop14p + pop_total_1)) < (nhwhite14p/pop14p)))), 1, 0)
  )

# compare SES
pl_annex_var_1420 %<>% 
  mutate(
    property_inc = ifelse(
      (annexing == 1 & owneroccupied_total >= 1 & (((owneroccupied_total_1 + owneroccupied14p)/(hu14p + hu_total_1)) > (hu14p + hu_total_1))), 1, 0),
    hincjobs = ifelse(
      (annexing == 1 & nhincjobs_total >= 1 & (((nhincjobs_total_1 + overlodesthresh14p)/(emp14p + njobs_total_1)) > (emp14p + njobs_total_1))), 1, 0)
  )

names(pl_annex_var_1420)
pl_annex_var_1420 %<>%
  select(-ends_with("07p"))
names(pl_annex_var_1420)

names(pl_annex_var_1420) <- gsub("14p", "_p0", names(pl_annex_var_1420))
names(pl_annex_var_1420)

pl_annex_var_1420 %<>%
  filter(pop_total > 0) 

table(pl_annex_var_1420$underbound_blackvap[pl_annex_var_1420$nhblackvap_total >= 1])
table(pl_annex_var_1420$underbound_hispvap)
table(pl_annex_var_1420$underbound_asianvap)
table(pl_annex_var_1420$underbound_nativevap)
table(pl_annex_var_1420$underbound_othervap)
table(pl_annex_var_1420$underbound_nhwhitevap[pl_annex_var_1420$nhwhitevap_total >= 1])
table(pl_annex_var_1420$underbound_blackvap_vraa)
table(pl_annex_var_1420$underbound_hispvap_vraa)
table(pl_annex_var_1420$underbound_asianvap_vraa)
table(pl_annex_var_1420$underbound_nativevap_vraa)
table(pl_annex_var_1420$underbound_othervap_vraa)
table(pl_annex_var_1420$underbound_nhwhitevap_vraa)
table(pl_annex_var_1420$underbound_blackvap_3pct)
table(pl_annex_var_1420$underbound_hispvap_3pct)
table(pl_annex_var_1420$underbound_asianvap_3pct)
table(pl_annex_var_1420$underbound_nativevap_3pct)
table(pl_annex_var_1420$underbound_othervap_3pct)
table(pl_annex_var_1420$underbound_nhwhitevap_3pct)
table(pl_annex_var_1420$underbound_blackvap_hpct)
table(pl_annex_var_1420$underbound_hispvap_hpct)
table(pl_annex_var_1420$underbound_asianvap_hpct)
table(pl_annex_var_1420$underbound_nativevap_hpct)
table(pl_annex_var_1420$underbound_othervap_hpct)
table(pl_annex_var_1420$underbound_nhwhitevap_hpct)
table(pl_annex_var_1420$underbound_blackvap_1pct)
table(pl_annex_var_1420$underbound_hispvap_1pct)
table(pl_annex_var_1420$underbound_asianvap_1pct)
table(pl_annex_var_1420$underbound_nativevap_1pct)
table(pl_annex_var_1420$underbound_othervap_1pct)
table(pl_annex_var_1420$underbound_nhwhitevap_1pct)
table(pl_annex_var_1420$underbound_blackvap_10pct)
table(pl_annex_var_1420$underbound_hispvap_10pct)
table(pl_annex_var_1420$underbound_asianvap_10pct)
table(pl_annex_var_1420$underbound_nativevap_10pct)
table(pl_annex_var_1420$underbound_othervap_10pct)
table(pl_annex_var_1420$underbound_nhwhitevap_10pct)
table(pl_annex_var_1420$property_inc)
table(pl_annex_var_1420$hincjobs)
table(pl_annex_var_1420$underbound_black)
table(pl_annex_var_1420$underbound_hisp)
table(pl_annex_var_1420$underbound_nhwhite)
table(pl_annex_var_1420$underbound_black_vraa)
table(pl_annex_var_1420$underbound_hisp_vraa)
table(pl_annex_var_1420$underbound_nhwhite_vraa)
table(pl_annex_var_1420$underbound_black_3pct)
table(pl_annex_var_1420$underbound_hisp_3pct)
table(pl_annex_var_1420$underbound_nhwhite_3pct)
table(pl_annex_var_1420$underbound_black_hpct)
table(pl_annex_var_1420$underbound_hisp_hpct)
table(pl_annex_var_1420$underbound_nhwhite_hpct)
table(pl_annex_var_1420$underbound_black_1pct)
table(pl_annex_var_1420$underbound_hisp_1pct)
table(pl_annex_var_1420$underbound_nhwhite_1pct)
table(pl_annex_var_1420$underbound_black_10pct)
table(pl_annex_var_1420$underbound_hisp_10pct)
table(pl_annex_var_1420$underbound_nhwhite_10pct)

# merge in 2020 data 
places2020 <- read_csv("places2020_cleaned.csv")

pl_annex_var_1420 %<>%
  left_join(places2020 %>% select(-Geo_NAME), by = "plid")

names(pl_annex_var_1420)
names(pl_annex_var_1420) <- gsub("20p", "_p1", names(pl_annex_var_1420))

write_csv(pl_annex_var_1420, "analyticalfiles/pl_annex_var_1420.csv")
pl_annex_var_1420 <- read_csv("analyticalfiles/pl_annex_var_1420.csv")

nhb <- pl_annex_var_1420 %>%
  filter(nhblack_total >= 1) %>%
  summarize_at(vars(c(contains("underbound") & contains("black") & !contains("vap"))), ~mean(., na.rm = T)) %>%
  mutate(Year = "2014 to 2020") %>%
  pivot_longer(cols = !c(Year),
               names_to = "underbound") %>%
  mutate(vra_basis = case_when(
    grepl("vraa", underbound) ~ "JLVRAA",
    grepl("_hpct", underbound) ~ "VRA, 0.5%",
    grepl("_1pct", underbound) ~ "VRA, 1%",
    grepl("_3pct", underbound) ~ "VRA, 3%",
    grepl("_10pct", underbound) ~ "VRA, 10%",
    TRUE ~ "VRA, 0%"
  ),
  race = "Non-Hispanic Black")

h <- pl_annex_var_1420 %>%
  filter(h_total >= 1) %>%
  summarize_at(vars(c(contains("underbound") & contains("hisp") & !contains("vap"))), ~mean(., na.rm = T)) %>%
  mutate(Year = "2014 to 2020") %>%
  pivot_longer(cols = !c(Year),
               names_to = "underbound") %>%
  mutate(vra_basis = case_when(
    grepl("vraa", underbound) ~ "JLVRAA",
    grepl("_hpct", underbound) ~ "VRA, 0.5%",
    grepl("_1pct", underbound) ~ "VRA, 1%",
    grepl("_3pct", underbound) ~ "VRA, 3%",
    grepl("_10pct", underbound) ~ "VRA, 10%",
    TRUE ~ "VRA, 0%"
  ),
  race = "Hispanic")

nhw <- pl_annex_var_1420 %>%
  filter(nhwhite_total >= 1) %>%
  summarize_at(vars(c(contains("underbound") & contains("white") & !contains("vap"))), ~mean(., na.rm = T)) %>%
  mutate(Year = "2014 to 2020") %>%
  pivot_longer(cols = !c(Year),
               names_to = "underbound") %>%
  mutate(vra_basis = case_when(
    grepl("vraa", underbound) ~ "JLVRAA",
    grepl("_hpct", underbound) ~ "VRA, 0.5%",
    grepl("_1pct", underbound) ~ "VRA, 1%",
    grepl("_3pct", underbound) ~ "VRA, 3%",
    grepl("_10pct", underbound) ~ "VRA, 10%",
    TRUE ~ "VRA, 0%"
  ),
  race = "Non-Hispanic White")

desc1420 <- base::rbind(nhb, h, nhw)

nhbvap <- pl_annex_var_1420 %>%
  filter(nhblackvap_total >= 1) %>%
  summarize_at(vars(c(contains("underbound") & contains("black") & contains("vap"))), ~mean(., na.rm = T)) %>%
  mutate(Year = "2014 to 2020") %>%
  pivot_longer(cols = !c(Year),
               names_to = "underbound") %>%
  mutate(vra_basis = case_when(
    grepl("vraa", underbound) ~ "JLVRAA",
    grepl("_hpct", underbound) ~ "VRA, 0.5%",
    grepl("_1pct", underbound) ~ "VRA, 1%",
    grepl("_3pct", underbound) ~ "VRA, 3%",
    grepl("_10pct", underbound) ~ "VRA, 10%",
    TRUE ~ "VRA, 0%"
  ),
  race = "Non-Hispanic Black")

hvap <- pl_annex_var_1420 %>%
  filter(hvap_total >= 1) %>%
  summarize_at(vars(c(contains("underbound") & contains("hisp") & contains("vap"))), ~mean(., na.rm = T)) %>%
  mutate(Year = "2014 to 2020") %>%
  pivot_longer(cols = !c(Year),
               names_to = "underbound") %>%
  mutate(vra_basis = case_when(
    grepl("vraa", underbound) ~ "JLVRAA",
    grepl("_hpct", underbound) ~ "VRA, 0.5%",
    grepl("_1pct", underbound) ~ "VRA, 1%",
    grepl("_3pct", underbound) ~ "VRA, 3%",
    grepl("_10pct", underbound) ~ "VRA, 10%",
    TRUE ~ "VRA, 0%"
  ),
  race = "Hispanic")

nhwvap <- pl_annex_var_1420 %>%
  filter(nhwhitevap_total >= 1) %>%
  summarize_at(vars(c(contains("underbound") & contains("white") & contains("vap"))), ~mean(., na.rm = T)) %>%
  mutate(Year = "2014 to 2020") %>%
  pivot_longer(cols = !c(Year),
               names_to = "underbound") %>%
  mutate(vra_basis = case_when(
    grepl("vraa", underbound) ~ "JLVRAA",
    grepl("_hpct", underbound) ~ "VRA, 0.5%",
    grepl("_1pct", underbound) ~ "VRA, 1%",
    grepl("_3pct", underbound) ~ "VRA, 3%",
    grepl("_10pct", underbound) ~ "VRA, 10%",
    TRUE ~ "VRA, 0%"
  ),
  race = "Non-Hispanic White")

desc1420vap <- base::rbind(nhbvap, hvap, nhwvap)
rm(aa1420, pl_annex_var_1420, pl0714, places2020, cdps14)

# repeat for 0007 ####
aa0007 <- read_csv("analyticalfiles/annexedblocks0007dem.csv")
names(aa0007)
names(aa0007) <- gsub("00b", "", names(aa0007))

summary(aa0007$pop)
aa0007 %<>%
  filter(!is.na(pop))

# transform this into place-level summaries
# characteristic of all annexable blocks
# diff between annexed and not annexed 
place_all <- aa0007 %>% 
  mutate(incopp = man + ret) %>%
  group_by(plid) %>%
  summarize(pop_total = sum(pop, na.rm = T),
            nhblack_total = sum(nhblack, na.rm = T),
            nhwhite_total = sum(nhwhite, na.rm = T),
            h_total = sum(h, na.rm = T),
            vap_total = sum(vap, na.rm = T),
            nhblackvap_total = sum(nhbvap),
            nhwhitevap_total = sum(nhwvap),
            hvap_total = sum(hispvap),
            nativevap_total = sum(nativevap, na.rm = T),
            asianvap_total = sum(asianvap, na.rm = T),
            othervap_total = sum(othervap, na.rm = T),
            pct_annexed = mean(annexed, na.rm = T),
            njobs_total = sum(njobs00, na.rm = T),
            nhincjobs_total = sum(nhincjobs00, na.rm = T),
            nwork_total = sum(jobs, na.rm = T),
            incopp_total = sum(incopp, na.rm = T),
            hu_total = sum(hu, na.rm = T),
            owneroccupied_total = sum(owneroccupied, na.rm = T),
            vacancy_total = sum(vacancy, na.rm = T)
  ) %>%
  ungroup() %>%
  mutate(pctnhblack_total = (nhblack_total/pop_total)*100,
         pcth_total = (h_total/pop_total)*100,
         pctnhwhite_total = (nhwhite_total/pop_total)*100,
         pctownerocc_total = (owneroccupied_total/hu_total)*100,
         pctincocc_total = (incopp_total/nwork_total)*100,
         pcthincjobs_total = (nhincjobs_total/njobs_total)*100,
         pctnhblackvap_total = (nhblackvap_total/vap_total)*100,
         pcthvap_total = (hvap_total/vap_total)*100,
         pctnhwhitevap_total = (nhwhitevap_total/vap_total)*100)

place_by_annex <- aa0007 %>%
  mutate(incopp = man + ret,
         pctincopp = (incopp/jobs)*100,
         pcthincjobs = (nhincjobs00/njobs00)*100,
         pctownerocc = (owneroccupied/hu)*100) %>%
  filter(annexed==1) %>%
  group_by(plid, annexed) %>%
  summarize(pop_total = sum(pop, na.rm = T),
            nhblack_total = sum(nhblack, na.rm = T),
            nhwhite_total = sum(nhwhite, na.rm = T),
            pctnhblack_total = mean(pctnhblack, na.rm = T),
            pctnhwhite_total = mean(pctnhwhite, na.rm = T),
            pcth_total = mean(pcth, na.rm = T),
            h_total = sum(h, na.rm = T),
            njobs_total = sum(njobs00, na.rm = T),
            nhincjobs_total = sum(nhincjobs00, na.rm = T),
            nwork_total = sum(jobs, na.rm = T),
            incopp_total = sum(incopp, na.rm = T),
            hu_total = sum(hu, na.rm = T),
            owneroccupied_total = sum(owneroccupied, na.rm = T),
            pctincopp_total = mean(pctincopp, na.rm = T),
            pcthincjobs_total = mean(pcthincjobs, na.rm = T),
            pctownerocc_total = mean(pctownerocc, na.rm = T),
            vacancy_total = sum(vacancy, na.rm = T),
            vap_total = sum(vap, na.rm = T),
            nhblackvap_total = sum(nhbvap, na.rm = T),
            nhwhitevap_total = sum(nhwvap, na.rm = T),
            hvap_total = sum(hispvap, na.rm = T),
            nativevap_total = sum(nativevap, na.rm = T),
            asianvap_total = sum(asianvap, na.rm = T),
            othervap_total = sum(othervap, na.rm = T)) %>%
  ungroup() %>%
  pivot_wider(
    id_cols = plid,
    names_from = annexed,
    values_from = c(pop_total:othervap_total)
  ) %>%
  mutate(pctnhblack_total_1 = (nhblack_total_1/pop_total_1)*100,
         pctnhwhite_total_1 = (nhwhite_total_1/pop_total_1)*100,
         pcth_total_1 = (h_total_1/pop_total_1)*100,
         pctnhblackvap_total_1 = (nhblackvap_total_1/vap_total_1)*100,
         pctnhwhitevap_total_1 = (nhwhitevap_total_1/vap_total_1)*100,
         pcthvap_total_1 = (hvap_total_1/vap_total_1)*100)

pl_annex_var_0007 <- left_join(
  place_all, place_by_annex, 
  by = "plid"
)

# add vra indicator 
places_vra <- aa0007 %>%
  group_by(plid) %>%
  summarize(vra = mean(vra, na.rm = T),
            annexing = mean(annexed, na.rm = T)) %>%
  ungroup() %>%
  mutate(vra = ifelse(vra > 0, 1, 0),
         annexing = ifelse(annexing > 0, 1, 0))

pl_annex_var_0007 %<>%
  left_join(places_vra, by = "plid")

# add place-level data for race and vap 
# vra violation = if pctwhite after annex > pctwhite before annex compared to
# what would have been, i.e. pctwhite given growth rate by 2013
# or if pctblack after annex < pctblack before annex compared to what would 
# have been, i.e. pctwhite given growth rate by 2013
# or if pcth after annex < pcth before annex compared to what would have been,
# i.e. pcth given growth rate by 2013 
pl9000 <- read_csv("pl9000_var.csv")

table(pl_annex_var_0007$plid %in% pl9000$plid) #213 false

cdps00 <- read_csv("pl2000_cleaned.csv") %>% # want to know which places are CDPs--they do not annex
  select(Geo_NAME, plid) %>%
  mutate(cdp = ifelse(grepl("CDP|cdp", Geo_NAME), 1, 0)) %>%
  filter(cdp==1)

pl_annex_var_0007 %<>%
  filter(plid %in% pl9000$plid & !(plid %in% cdps00$plid)) %>%
  left_join(pl9000, by = "plid") %>%
  mutate(post = -1,
         pctowneroccupied00p = (owneroccupied00p/hu00p)*100) %>%
  group_by(substr(plid, 1, 2)) %>%
  mutate(pop_tercile = ntile(pop00p, 3),
         maj_white = ifelse(pctnhwhitevap00p >= 50, 1, 0)) %>%
  ungroup()

table(pl_annex_var_0007$annexing)
table(pl_annex_var_0007$maj_white)

# make underbound variable
# # of black_annexed(2013) + blackvap(2010)/(vap2000 + annexed_vap2013) - blackvap2010/
pl_annex_var_0007 %<>%
  mutate(
    increase_whitevap = ifelse((annexing == 1 & ((((nhwhitevap_total_1 + nhwhitevap00p)/(vap00p + vap_total_1)) - (nhwhitevap00p/vap00p))) > 0), 1, 0),
    underbound_blackvap_vraa = ifelse(
      (annexing == 1 & nhblackvap_total >= 1 & increase_whitevap == 1 & vraa == 1 & ((((nhblackvap_total_1 + nhblackvap00p)/(vap_total_1 + vap00p)) - (nhblackvap00p/vap00p))) < -0.03), 1, 0), 
    underbound_hispvap_vraa = ifelse(
      (annexing == 1 & hvap_total >= 1 & increase_whitevap == 1 & vraa == 1 & ((((hvap_total_1 + hispvap00p)/(vap_total_1 + vap00p)) - (hispvap00p/vap00p))) < -0.03), 1, 0),
    underbound_asianvap_vraa = ifelse(
      (annexing == 1 & increase_whitevap == 1 & vraa == 1 & ((((asianvap_total_1 + asianvap00p)/(vap00p + vap_total_1)) - (asianvap00p/vap00p))) < -0.03), 1, 0), 
    underbound_nativevap_vraa = ifelse(
      (annexing == 1 & increase_whitevap == 1 & vraa == 1 & ((((nativevap_total_1 + nativevap00p)/(vap00p + vap_total_1)) - (nativevap00p/vap00p))) < -0.03), 1, 0),
    underbound_othervap_vraa = ifelse(
      (annexing == 1 & increase_whitevap == 1 & vraa == 1 & ((((othervap_total_1 + othervap00p)/(vap00p + vap_total_1)) - (othervap00p/vap00p))) < -0.03), 1, 0),
    underbound_nhwhitevap_vraa = ifelse(
      (annexing == 1 & nhwhitevap_total >= 1 & vraa == 1 & ((((nhwhitevap_total_1 + nhwhitevap00p)/(vap00p + vap_total_1)) - (nhwhitevap00p/vap00p))) < -0.03), 1, 0),
    underbound_blackvap_3pct = ifelse(
      (annexing == 1 & nhblackvap_total >= 1 & increase_whitevap == 1 & ((((nhblackvap_total_1 + nhblackvap00p)/(vap_total_1 + vap00p)) - (nhblackvap00p/vap00p))) < -0.03), 1, 0), 
    underbound_hispvap_3pct = ifelse(
      (annexing == 1 & hvap_total >= 1 & increase_whitevap == 1 & ((((hvap_total_1 + hispvap00p)/(vap_total_1 + vap00p)) - (hispvap00p/vap00p))) < -0.03), 1, 0),
    underbound_asianvap_3pct = ifelse(
      (annexing == 1 & increase_whitevap == 1 & ((((asianvap_total_1 + asianvap00p)/(vap00p + vap_total_1)) - (asianvap00p/vap00p))) < -0.03), 1, 0), 
    underbound_nativevap_3pct = ifelse(
      (annexing == 1 & increase_whitevap == 1 & ((((nativevap_total_1 + nativevap00p)/(vap00p + vap_total_1)) - (nativevap00p/vap00p))) < -0.03), 1, 0),
    underbound_othervap_3pct = ifelse(
      (annexing == 1 & increase_whitevap == 1 & ((((othervap_total_1 + othervap00p)/(vap00p + vap_total_1)) - (othervap00p/vap00p))) < -0.03), 1, 0),
    underbound_nhwhitevap_3pct = ifelse(
      (annexing == 1 & nhwhitevap_total >= 1 & ((((nhwhitevap_total_1 + nhwhitevap00p)/(vap00p + vap_total_1)) - (nhwhitevap00p/vap00p))) < -0.03), 1, 0),
    underbound_blackvap_hpct = ifelse(
      (annexing == 1 & nhblackvap_total >= 1 & increase_whitevap == 1 & ((((nhblackvap_total_1 + nhblackvap00p)/(vap_total_1 + vap00p)) - (nhblackvap00p/vap00p))) < -0.005), 1, 0), 
    underbound_hispvap_hpct = ifelse(
      (annexing == 1 & hvap_total >= 1 & increase_whitevap == 1 & ((((hvap_total_1 + hispvap00p)/(vap_total_1 + vap00p)) - (hispvap00p/vap00p))) < -0.005), 1, 0),
    underbound_asianvap_hpct = ifelse(
      (annexing == 1 & increase_whitevap == 1 & ((((asianvap_total_1 + asianvap00p)/(vap00p + vap_total_1)) - (asianvap00p/vap00p))) < -0.005), 1, 0), 
    underbound_nativevap_hpct = ifelse(
      (annexing == 1 & increase_whitevap == 1 & ((((nativevap_total_1 + nativevap00p)/(vap00p + vap_total_1)) - (nativevap00p/vap00p))) < -0.005), 1, 0),
    underbound_othervap_hpct = ifelse(
      (annexing == 1 & increase_whitevap == 1 & ((((othervap_total_1 + othervap00p)/(vap00p + vap_total_1)) - (othervap00p/vap00p))) < -0.005), 1, 0),
    underbound_nhwhitevap_hpct = ifelse(
      (annexing == 1 & nhwhitevap_total >= 1 & ((((nhwhitevap_total_1 + nhwhitevap00p)/(vap00p + vap_total_1)) - (nhwhitevap00p/vap00p))) < -0.005), 1, 0),
    underbound_blackvap_1pct = ifelse(
      (annexing == 1 & nhblackvap_total >= 1 & increase_whitevap == 1 & ((((nhblackvap_total_1 + nhblackvap00p)/(vap_total_1 + vap00p)) - (nhblackvap00p/vap00p))) < -0.01), 1, 0), 
    underbound_hispvap_1pct = ifelse(
      (annexing == 1 & hvap_total >= 1 & increase_whitevap == 1 & ((((hvap_total_1 + hispvap00p)/(vap_total_1 + vap00p)) - (hispvap00p/vap00p))) < -0.01), 1, 0),
    underbound_asianvap_1pct = ifelse(
      (annexing == 1 & increase_whitevap == 1 & ((((asianvap_total_1 + asianvap00p)/(vap00p + vap_total_1)) - (asianvap00p/vap00p))) < -0.01), 1, 0), 
    underbound_nativevap_1pct = ifelse(
      (annexing == 1 & increase_whitevap == 1 & ((((nativevap_total_1 + nativevap00p)/(vap00p + vap_total_1)) - (nativevap00p/vap00p))) < -0.01), 1, 0),
    underbound_othervap_1pct = ifelse(
      (annexing == 1 & increase_whitevap == 1 & ((((othervap_total_1 + othervap00p)/(vap00p + vap_total_1)) - (othervap00p/vap00p))) < -0.01), 1, 0),
    underbound_nhwhitevap_1pct = ifelse(
      (annexing == 1 & nhwhitevap_total >= 1 & ((((nhwhitevap_total_1 + nhwhitevap00p)/(vap00p + vap_total_1)) - (nhwhitevap00p/vap00p))) < -0.01), 1, 0),
    underbound_blackvap_10pct = ifelse(
      (annexing == 1 & nhblackvap_total >= 1 & increase_whitevap == 1 & ((((nhblackvap_total_1 + nhblackvap00p)/(vap_total_1 + vap00p)) - (nhblackvap00p/vap00p))) < -0.1), 1, 0), 
    underbound_hispvap_10pct = ifelse(
      (annexing == 1 & hvap_total >= 1 & increase_whitevap == 1 & ((((hvap_total_1 + hispvap00p)/(vap_total_1 + vap00p)) - (hispvap00p/vap00p))) < -0.1), 1, 0),
    underbound_asianvap_10pct = ifelse(
      (annexing == 1 & increase_whitevap == 1 & ((((asianvap_total_1 + asianvap00p)/(vap00p + vap_total_1)) - (asianvap00p/vap00p))) < -0.1), 1, 0), 
    underbound_nativevap_10pct = ifelse(
      (annexing == 1 & increase_whitevap == 1 & ((((nativevap_total_1 + nativevap00p)/(vap00p + vap_total_1)) - (nativevap00p/vap00p))) < -0.1), 1, 0),
    underbound_othervap_10pct = ifelse(
      (annexing == 1 & increase_whitevap == 1 & ((((othervap_total_1 + othervap00p)/(vap00p + vap_total_1)) - (othervap00p/vap00p))) < -0.1), 1, 0),
    underbound_nhwhitevap_10pct = ifelse(
      (annexing == 1 & nhwhitevap_total >= 1 & ((((nhwhitevap_total_1 + nhwhitevap00p)/(vap00p + vap_total_1)) - (nhwhitevap00p/vap00p))) < -0.1), 1, 0)
  )

pl_annex_var_0007 %<>%
  mutate(
    underbound_blackvap = ifelse(
      (annexing == 1 & nhblackvap_total >= 1 & increase_whitevap == 1 & ((((nhblackvap_total_1 + nhblackvap00p)/(vap_total_1 + vap00p)) < (nhblackvap00p/vap00p)))), 1, 0), 
    underbound_hispvap = ifelse(
      (annexing == 1 & hvap_total >= 1 & increase_whitevap == 1 & ((((hvap_total_1 + hispvap00p)/(vap_total_1 + vap00p)) < (hispvap00p/vap00p)))), 1, 0),
    underbound_asianvap = ifelse(
      (annexing == 1 & increase_whitevap == 1 & ((((asianvap_total_1 + asianvap00p)/(vap00p + vap_total_1)) < (asianvap00p/vap00p)))), 1, 0), 
    underbound_nativevap = ifelse(
      (annexing == 1 & increase_whitevap == 1 & ((((nativevap_total_1 + nativevap00p)/(vap00p + vap_total_1)) < (nativevap00p/vap00p)))), 1, 0),
    underbound_othervap = ifelse(
      (annexing == 1 & increase_whitevap == 1 & ((((othervap_total_1 + othervap00p)/(vap00p + vap_total_1)) < (othervap00p/vap00p)))), 1, 0),
    underbound_nhwhitevap = ifelse(
      (annexing == 1 & nhwhitevap_total >= 1 & ((((nhwhitevap_total_1 + nhwhitevap00p)/(vap00p + vap_total_1)) < (nhwhitevap00p/vap00p)))), 1, 0)
  )

# not vap 
pl_annex_var_0007 %<>%
  mutate(
    increase_white = ifelse((annexing == 1 & ((((nhwhite_total_1 + nhwhite00p)/(pop00p + pop_total_1)) - (nhwhite00p/pop00p))) > 0), 1, 0),
    underbound_black_vraa = ifelse(
      (annexing == 1 & nhblack_total >= 1 & increase_white == 1 & vraa == 1 & ((((nhblack_total_1 + nhblack00p)/(pop_total_1 + pop00p)) - (nhblack00p/pop00p))) < -0.03), 1, 0), 
    underbound_hisp_vraa = ifelse(
      (annexing == 1 & h_total >= 1 & hvap_total >= 1 & increase_white == 1 & vraa == 1 & ((((h_total_1 + h00p)/(pop_total_1 + pop00p)) - (h00p/pop00p))) < -0.03), 1, 0),
    #underbound_asian_vraa = ifelse(
    #  (annexing == 1 & increase_white == 1 & vraa == 1 & ((((asian_total_1 + asian00p)/(pop00p + pop_total_1)) - (asian00p/pop00p))) < -0.03), 1, 0), 
    #underbound_native_vraa = ifelse(
    #  (annexing == 1 & increase_white == 1 & vraa == 1 & ((((native_total_1 + native00p)/(pop00p + pop_total_1)) - (native00p/pop00p))) < -0.03), 1, 0),
    #underbound_other_vraa = ifelse(
    #  (annexing == 1 & increase_white == 1 & vraa == 1 & ((((other_total_1 + other00p)/(pop00p + pop_total_1)) - (other00p/pop00p))) < -0.03), 1, 0),
    underbound_nhwhite_vraa = ifelse(
      (annexing == 1 & nhwhite_total >= 1 & vraa == 1 & ((((nhwhite_total_1 + nhwhite00p)/(pop00p + pop_total_1)) - (nhwhite00p/pop00p))) < -0.03), 1, 0),
    underbound_black_3pct = ifelse(
      (annexing == 1 & nhblack_total >= 1 & increase_white == 1 & ((((nhblack_total_1 + nhblack00p)/(pop_total_1 + pop00p)) - (nhblack00p/pop00p))) < -0.03), 1, 0), 
    underbound_hisp_3pct = ifelse(
      (annexing == 1 & h_total >= 1 & increase_white == 1 & ((((h_total_1 + h00p)/(pop_total_1 + pop00p)) - (h00p/pop00p))) < -0.03), 1, 0),
    # underbound_asian_3pct = ifelse(
    #   (annexing == 1 & increase_white == 1 & ((((asian_total_1 + asian00p)/(pop00p + pop_total_1)) - (asian00p/pop00p))) < -0.03), 1, 0), 
    # underbound_native_3pct = ifelse(
    #   (annexing == 1 & increase_white == 1 & ((((native_total_1 + native00p)/(pop00p + pop_total_1)) - (native00p/pop00p))) < -0.03), 1, 0),
    # underbound_other_3pct = ifelse(
    #   (annexing == 1 & increase_white == 1 & ((((other_total_1 + other00p)/(pop00p + pop_total_1)) - (other00p/pop00p))) < -0.03), 1, 0),
    underbound_nhwhite_3pct = ifelse(
      (annexing == 1 & nhwhite_total >= 1 & ((((nhwhite_total_1 + nhwhite00p)/(pop00p + pop_total_1)) - (nhwhite00p/pop00p))) < -0.03), 1, 0),
    underbound_black_hpct = ifelse(
      (annexing == 1 & nhblack_total >= 1 & increase_white == 1 & ((((nhblack_total_1 + nhblack00p)/(pop_total_1 + pop00p)) - (nhblack00p/pop00p))) < -0.005), 1, 0), 
    underbound_hisp_hpct = ifelse(
      (annexing == 1 & h_total >= 1 & increase_white == 1 & ((((h_total_1 + h00p)/(pop_total_1 + pop00p)) - (h00p/pop00p))) < -0.005), 1, 0),
    # underbound_asian_hpct = ifelse(
    #   (annexing == 1 & increase_white == 1 & ((((asian_total_1 + asian00p)/(pop00p + pop_total_1)) - (asian00p/pop00p))) < -0.005), 1, 0), 
    # underbound_native_hpct = ifelse(
    #   (annexing == 1 & increase_white == 1 & ((((native_total_1 + native00p)/(pop00p + pop_total_1)) - (native00p/pop00p))) < -0.005), 1, 0),
    # underbound_other_hpct = ifelse(
    #   (annexing == 1 & increase_white == 1 & ((((other_total_1 + other00p)/(pop00p + pop_total_1)) - (other00p/pop00p))) < -0.005), 1, 0),
    underbound_nhwhite_hpct = ifelse(
      (annexing == 1 & nhwhite_total >= 1 & ((((nhwhite_total_1 + nhwhite00p)/(pop00p + pop_total_1)) - (nhwhite00p/pop00p))) < -0.005), 1, 0),
    underbound_black_1pct = ifelse(
      (annexing == 1 & nhblack_total >= 1 & increase_white == 1 & ((((nhblack_total_1 + nhblack00p)/(pop_total_1 + pop00p)) - (nhblack00p/pop00p))) < -0.01), 1, 0), 
    underbound_hisp_1pct = ifelse(
      (annexing == 1 & h_total >= 1 & increase_white == 1 & ((((h_total_1 + h00p)/(pop_total_1 + pop00p)) - (h00p/pop00p))) < -0.01), 1, 0),
    # underbound_asian_1pct = ifelse(
    #   (annexing == 1 & increase_white == 1 & ((((asian_total_1 + asian00p)/(pop00p + pop_total_1)) - (asian00p/pop00p))) < -0.01), 1, 0), 
    # underbound_native_1pct = ifelse(
    #   (annexing == 1 & increase_white == 1 & ((((native_total_1 + native00p)/(pop00p + pop_total_1)) - (native00p/pop00p))) < -0.01), 1, 0),
    # underbound_other_1pct = ifelse(
    #   (annexing == 1 & increase_white == 1 & ((((other_total_1 + other00p)/(pop00p + pop_total_1)) - (other00p/pop00p))) < -0.01), 1, 0),
    underbound_nhwhite_1pct = ifelse(
      (annexing == 1 & nhwhite_total >= 1 & ((((nhwhite_total_1 + nhwhite00p)/(pop00p + pop_total_1)) - (nhwhite00p/pop00p))) < -0.01), 1, 0),
    underbound_black_10pct = ifelse(
      (annexing == 1 & nhblack_total >= 1 & increase_white == 1 & ((((nhblack_total_1 + nhblack00p)/(pop_total_1 + pop00p)) - (nhblack00p/pop00p))) < -0.1), 1, 0), 
    underbound_hisp_10pct = ifelse(
      (annexing == 1 & h_total >= 1 & increase_white == 1 & ((((h_total_1 + h00p)/(pop_total_1 + pop00p)) - (h00p/pop00p))) < -0.1), 1, 0),
    # underbound_asian_10pct = ifelse(
    #   (annexing == 1 & increase_white == 1 & ((((asian_total_1 + asian00p)/(pop00p + pop_total_1)) - (asian00p/pop00p))) < -0.1), 1, 0), 
    # underbound_native_10pct = ifelse(
    #   (annexing == 1 & increase_white == 1 & ((((native_total_1 + native00p)/(pop00p + pop_total_1)) - (native00p/pop00p))) < -0.1), 1, 0),
    # underbound_other_10pct = ifelse(
    #   (annexing == 1 & increase_white == 1 & ((((other_total_1 + other00p)/(pop00p + pop_total_1)) - (other00p/pop00p))) < -0.1), 1, 0),
    underbound_nhwhite_10pct = ifelse(
      (annexing == 1 & nhwhite_total >= 1 & ((((nhwhite_total_1 + nhwhite00p)/(pop00p + pop_total_1)) - (nhwhite00p/pop00p))) < -0.1), 1, 0)
  )

pl_annex_var_0007 %<>%
  mutate(
    underbound_black = ifelse(
      (annexing == 1 & nhblack_total >= 1 & increase_white == 1 & ((((nhblack_total_1 + nhblack00p)/(pop_total_1 + pop00p)) < (nhblack00p/pop00p)))), 1, 0), 
    underbound_hisp = ifelse(
      (annexing == 1 & h_total >= 1 & increase_white == 1 & ((((h_total_1 + h00p)/(pop_total_1 + pop00p)) < (h00p/pop00p)))), 1, 0),
    # underbound_asian = ifelse(
    #   (annexing == 1 & increase_white == 1 & ((((asian_total_1 + asian00p)/(pop00p + pop_total_1)) < (asian00p/pop00p)))), 1, 0), 
    # underbound_native = ifelse(
    #   (annexing == 1 & increase_white == 1 & ((((native_total_1 + native00p)/(pop00p + pop_total_1)) < (native00p/pop00p)))), 1, 0),
    # underbound_other = ifelse(
    #   (annexing == 1 & increase_white == 1 & ((((other_total_1 + other00p)/(pop00p + pop_total_1)) < (other00p/pop00p)))), 1, 0),
    underbound_nhwhite = ifelse(
      (annexing == 1 & nhwhite_total >= 1 & ((((nhwhite_total_1 + nhwhite00p)/(pop00p + pop_total_1)) < (nhwhite00p/pop00p)))), 1, 0)
  )

# compare SES
pl_annex_var_0007 %<>% 
  mutate(
    property_inc = ifelse(
      (annexing == 1 & owneroccupied_total >= 1 & (((owneroccupied_total_1 + owneroccupied00p)/(hu00p + hu_total_1)) > (hu00p + hu_total_1))), 1, 0),
    hincjobs = ifelse(
      (annexing == 1 & nhincjobs_total >= 1 & (((nhincjobs_total_1 + overlodesthresh00p)/(emp00p + njobs_total_1)) > (emp00p + njobs_total_1))), 1, 0)
  )

pl_annex_var_0007 %<>%
  filter(pop_total > 0) 

table(pl_annex_var_0007$underbound_blackvap)
table(pl_annex_var_0007$underbound_hispvap)
table(pl_annex_var_0007$underbound_asianvap)
table(pl_annex_var_0007$underbound_nativevap)
table(pl_annex_var_0007$underbound_othervap)
table(pl_annex_var_0007$underbound_nhwhitevap)
table(pl_annex_var_0007$underbound_blackvap_vraa)
table(pl_annex_var_0007$underbound_hispvap_vraa)
table(pl_annex_var_0007$underbound_asianvap_vraa)
table(pl_annex_var_0007$underbound_nativevap_vraa)
table(pl_annex_var_0007$underbound_othervap_vraa)
table(pl_annex_var_0007$underbound_nhwhitevap_vraa)
table(pl_annex_var_0007$underbound_blackvap_3pct)
table(pl_annex_var_0007$underbound_hispvap_3pct)
table(pl_annex_var_0007$underbound_asianvap_3pct)
table(pl_annex_var_0007$underbound_nativevap_3pct)
table(pl_annex_var_0007$underbound_othervap_3pct)
table(pl_annex_var_0007$underbound_nhwhitevap_3pct)
table(pl_annex_var_0007$underbound_blackvap_hpct)
table(pl_annex_var_0007$underbound_hispvap_hpct)
table(pl_annex_var_0007$underbound_asianvap_hpct)
table(pl_annex_var_0007$underbound_nativevap_hpct)
table(pl_annex_var_0007$underbound_othervap_hpct)
table(pl_annex_var_0007$underbound_nhwhitevap_hpct)
table(pl_annex_var_0007$underbound_blackvap_1pct)
table(pl_annex_var_0007$underbound_hispvap_1pct)
table(pl_annex_var_0007$underbound_asianvap_1pct)
table(pl_annex_var_0007$underbound_nativevap_1pct)
table(pl_annex_var_0007$underbound_othervap_1pct)
table(pl_annex_var_0007$underbound_nhwhitevap_1pct)
table(pl_annex_var_0007$underbound_blackvap_10pct)
table(pl_annex_var_0007$underbound_hispvap_10pct)
table(pl_annex_var_0007$underbound_asianvap_10pct)
table(pl_annex_var_0007$underbound_nativevap_10pct)
table(pl_annex_var_0007$underbound_othervap_10pct)
table(pl_annex_var_0007$underbound_nhwhitevap_10pct)
table(pl_annex_var_0007$property_inc)
table(pl_annex_var_0007$hincjobs)
table(pl_annex_var_0007$underbound_black)
table(pl_annex_var_0007$underbound_hisp)
table(pl_annex_var_0007$underbound_nhwhite)
table(pl_annex_var_0007$underbound_black_vraa)
table(pl_annex_var_0007$underbound_hisp_vraa)
table(pl_annex_var_0007$underbound_nhwhite_vraa)
table(pl_annex_var_0007$underbound_black_3pct)
table(pl_annex_var_0007$underbound_hisp_3pct)
table(pl_annex_var_0007$underbound_nhwhite_3pct)
table(pl_annex_var_0007$underbound_black_hpct)
table(pl_annex_var_0007$underbound_hisp_hpct)
table(pl_annex_var_0007$underbound_nhwhite_hpct)
table(pl_annex_var_0007$underbound_black_1pct)
table(pl_annex_var_0007$underbound_hisp_1pct)
table(pl_annex_var_0007$underbound_nhwhite_1pct)
table(pl_annex_var_0007$underbound_black_10pct)
table(pl_annex_var_0007$underbound_hisp_10pct)
table(pl_annex_var_0007$underbound_nhwhite_10pct)

names(pl_annex_var_0007)
pl_annex_var_0007 %<>%
  select(-ends_with("90p"))
names(pl_annex_var_0007)

names(pl_annex_var_0007) <- gsub("00p", "_p0", names(pl_annex_var_0007))
names(pl_annex_var_0007)

# merge in 2020 data 
places2007 <- read_csv("pl2007_cleaned.csv")

pl_annex_var_0007 %<>%
  left_join(places2007 %>% select(-Geo_NAME), by = "plid")

names(pl_annex_var_0007)
names(pl_annex_var_0007) <- gsub("07p", "_p1", names(pl_annex_var_0007))

write_csv(pl_annex_var_0007, "analyticalfiles/pl_annex_var_0007.csv")
pl_annex_var_0007 <- read_csv("analyticalfiles/pl_annex_var_0007.csv")

nhb <- pl_annex_var_0007 %>%
  filter(nhblack_total >= 1) %>%
  summarize_at(vars(c(contains("underbound") & contains("black") & !contains("vap"))), ~mean(., na.rm = T)) %>%
  mutate(Year = "2000 to 2007") %>%
  pivot_longer(cols = !c(Year),
               names_to = "underbound") %>%
  mutate(vra_basis = case_when(
    grepl("vraa", underbound) ~ "JLVRAA",
    grepl("_hpct", underbound) ~ "VRA, 0.5%",
    grepl("_1pct", underbound) ~ "VRA, 1%",
    grepl("_3pct", underbound) ~ "VRA, 3%",
    grepl("_10pct", underbound) ~ "VRA, 10%",
    TRUE ~ "VRA, 0%"
  ),
  race = "Non-Hispanic Black")

h <- pl_annex_var_0007 %>%
  filter(h_total >= 1) %>%
  summarize_at(vars(c(contains("underbound") & contains("hisp") & !contains("vap"))), ~mean(., na.rm = T)) %>%
  mutate(Year = "2000 to 2007") %>%
  pivot_longer(cols = !c(Year),
               names_to = "underbound") %>%
  mutate(vra_basis = case_when(
    grepl("vraa", underbound) ~ "JLVRAA",
    grepl("_hpct", underbound) ~ "VRA, 0.5%",
    grepl("_1pct", underbound) ~ "VRA, 1%",
    grepl("_3pct", underbound) ~ "VRA, 3%",
    grepl("_10pct", underbound) ~ "VRA, 10%",
    TRUE ~ "VRA, 0%"
  ),
  race = "Hispanic")

nhw <- pl_annex_var_0007 %>%
  filter(nhwhite_total >= 1) %>%
  summarize_at(vars(c(contains("underbound") & contains("white") & !contains("vap"))), ~mean(., na.rm = T)) %>%
  mutate(Year = "2000 to 2007") %>%
  pivot_longer(cols = !c(Year),
               names_to = "underbound") %>%
  mutate(vra_basis = case_when(
    grepl("vraa", underbound) ~ "JLVRAA",
    grepl("_hpct", underbound) ~ "VRA, 0.5%",
    grepl("_1pct", underbound) ~ "VRA, 1%",
    grepl("_3pct", underbound) ~ "VRA, 3%",
    grepl("_10pct", underbound) ~ "VRA, 10%",
    TRUE ~ "VRA, 0%"
  ),
  race = "Non-Hispanic White")

desc0007 <- base::rbind(nhb, h, nhw)

nhbvap <- pl_annex_var_0007 %>%
  filter(nhblackvap_total >= 1) %>%
  summarize_at(vars(c(contains("underbound") & contains("black") & contains("vap"))), ~mean(., na.rm = T)) %>%
  mutate(Year = "2000 to 2007") %>%
  pivot_longer(cols = !c(Year),
               names_to = "underbound") %>%
  mutate(vra_basis = case_when(
    grepl("vraa", underbound) ~ "JLVRAA",
    grepl("_hpct", underbound) ~ "VRA, 0.5%",
    grepl("_1pct", underbound) ~ "VRA, 1%",
    grepl("_3pct", underbound) ~ "VRA, 3%",
    grepl("_10pct", underbound) ~ "VRA, 10%",
    TRUE ~ "VRA, 0%"
  ),
  race = "Non-Hispanic Black")

hvap <- pl_annex_var_0007 %>%
  filter(hvap_total >= 1) %>%
  summarize_at(vars(c(contains("underbound") & contains("hisp") & contains("vap"))), ~mean(., na.rm = T)) %>%
  mutate(Year = "2000 to 2007") %>%
  pivot_longer(cols = !c(Year),
               names_to = "underbound") %>%
  mutate(vra_basis = case_when(
    grepl("vraa", underbound) ~ "JLVRAA",
    grepl("_hpct", underbound) ~ "VRA, 0.5%",
    grepl("_1pct", underbound) ~ "VRA, 1%",
    grepl("_3pct", underbound) ~ "VRA, 3%",
    grepl("_10pct", underbound) ~ "VRA, 10%",
    TRUE ~ "VRA, 0%"
  ),
  race = "Hispanic")

nhwvap <- pl_annex_var_0007 %>%
  filter(nhwhitevap_total >= 1) %>%
  summarize_at(vars(c(contains("underbound") & contains("white") & contains("vap"))), ~mean(., na.rm = T)) %>%
  mutate(Year = "2000 to 2007") %>%
  pivot_longer(cols = !c(Year),
               names_to = "underbound") %>%
  mutate(vra_basis = case_when(
    grepl("vraa", underbound) ~ "JLVRAA",
    grepl("_hpct", underbound) ~ "VRA, 0.5%",
    grepl("_1pct", underbound) ~ "VRA, 1%",
    grepl("_3pct", underbound) ~ "VRA, 3%",
    grepl("_10pct", underbound) ~ "VRA, 10%",
    TRUE ~ "VRA, 0%"
  ),
  race = "Non-Hispanic White")

desc0007vap <- base::rbind(nhbvap, hvap, nhwvap)
rm(aa0007, pl_annex_var_0007, pl9000, places2007, cdps00)

desc <- base::rbind(desc0007, desc0713, desc1420)
descvap <- base::rbind(desc0007vap, desc0713vap, desc1420vap)

desc %<>%
  mutate(Year = factor(Year, levels = c("2000 to 2007", "2007 to 2013", 
                                        "2014 to 2020")),
         vra_basis = factor(vra_basis, levels = c("VRA, 0%", "VRA, 0.5%", 
                                                  "VRA, 1%", "VRA, 3%", "VRA, 10%", "JLVRAA"
         )),
         Proportion = value*100,
         race = factor(race, levels = c("Non-Hispanic Black",
                                        "Hispanic",
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
ggsave(filename = "analyticalfiles/VRA_VRAA_pop_nowhite_noannex_newdenom.png",
       plot = plot,
       dpi = 300)

descvap %<>%
  mutate(Year = factor(Year, levels = c("2000 to 2007", "2007 to 2013", 
                                        "2014 to 2020")),
         vra_basis = factor(vra_basis, levels = c("VRA, 0%", "VRA, 0.5%", 
                                                  "VRA, 1%", "VRA, 3%", "VRA, 10%", "JLVRAA"
         )),
         Proportion = value*100,
         race = factor(race, levels = c("Non-Hispanic Black",
                                        "Hispanic",
                                        "Non-Hispanic White")))

plot <- ggplot(descvap, aes(y = Proportion, x = Year, group = vra_basis)) + 
  geom_line(aes(color = vra_basis)) + geom_point(aes(color = vra_basis)) + 
  facet_grid(~race, labeller = label_wrap_gen(width=12)) + 
  theme(axis.text.x = element_text(angle = 50, hjust = 1, size = 8),
        strip.text.x = element_text(size = 6)) + 
  ggtitle("Proportion of (Annexing) Places Conducting Questionable Annexations 
by VRA or JLVRAA Benchmark for Preclearance VRA Coverage, 2000-2020") + 
  labs(color = "VRA Coverage")
plot
ggsave(filename = "analyticalfiles/VRA_VRAA_vap_nowhite_noannex_newdenom.png",
       plot = plot,
       dpi = 300)
rm(list = ls())

# make panel data!!!!! ####
# take out ne and hawaii
NE <- c("09", "23", "25", "33", "34", "36", "42", "24", "44", "50", "15")
pl0007 <- read_csv("analyticalfiles/pl_annex_var_0007.csv") %>%
  mutate(STATE = substr(plid, 1, 2)) %>%
  filter(!(STATE %in% NE)) %>%
  #filter(annexing==1) %>%
  filter(vap_total > 0)
pl0713 <- read_csv("analyticalfiles/pl_annex_var_0713.csv") %>%
  mutate(STATE = substr(plid, 1, 2)) %>%
  filter(!(STATE %in% NE)) %>%
  #filter(annexing==1) %>%
  filter(vap_total > 0)
pl1420 <- read_csv("analyticalfiles/pl_annex_var_1420.csv") %>%
  mutate(STATE = substr(plid, 1, 2)) %>%
  filter(!(STATE %in% NE)) %>%
  #filter(annexing==1) %>%
  filter(vap_total > 0)

plids <- Reduce(intersect, list(unique(pl0007$plid), unique(pl0713$plid), unique(pl1420$plid)))
names_list <- Reduce(intersect, list(names(pl0007), names(pl0713), names(pl1420)))

pl0007 %<>% 
  filter(plid %in% plids) %>%
  select(all_of(names_list)) %>%
  filter(!duplicated(plid))
pl0713 %<>%
  filter(plid %in% plids) %>%
  select(all_of(names_list))
pl1420 %<>%
  filter(plid %in% plids) %>%
  select(all_of(names_list)) 

panel0020_did <- base::rbind(
    pl0007, pl0713, pl1420
)

rm(pl0007, pl0713, pl1420)

panel0020_did %<>% 
  mutate(period = ifelse(post > 0, 1, 0), 
         vra = as.factor(vra),
         period = as.factor(period))

panel0020_did %<>%
  mutate_at(c("pctincocc_total", "pctownerocc_total", "pcthincjobs_total"), ~ifelse(!is.finite(.) | is.na(.), 0, .)) %>%
  mutate_at(c("pctowneroccupied_p0", "pctemp_p0"), ~ifelse(!is.finite(.) | is.na(.), 0, .))

panel0020_did %<>%
  mutate_at(c("pop_total", "pop_p0", "pctnhwhite_p0", "pctincocc_total", "pctownerocc_total", "pcthincjobs_total", "pctnhblack_total",
              "pctnhblack_p0", "pctowneroccupied_p0", "mhmval_p0", "incomepp_p0", "popdensity_p0",
              "pctnhwhitevap_p0", "pctnhblackvap_total", "pctnhblackvap_p0", "pcth_total", "pcthvap_total",
              "pcth_p0", "pcthispvap_p0", "pctemp_p0", "hinc_p0"), 
            ~((.-mean(.))/sd(.)))
summary(panel0020_did)

# models ####
outcomes <- c("", "_hpct", "_1pct", "_3pct", "_10pct")
races <- c("black", "hisp", "native", "asian", "nhwhite", "other")

#annex or not ####
set_label(panel0020_did$annexing) <- "Conducted Annexation"
set_label(panel0020_did$vra) <- "Previously Covered by Section V"
set_label(panel0020_did$period) <- "After Shelby"
set_label(panel0020_did$pop_p0) <- "Population Size"
set_label(panel0020_did$popdensity_p0) <- "Population Density"
set_label(panel0020_did$pctnhwhite_p0) <- "% Non-Hispanic White"
set_label(panel0020_did$pctemp_p0) <- "% Employed"
set_label(panel0020_did$hinc_p0) <- "Median Household Income"
set_label(panel0020_did$pctowneroccupied_p0) <- "% Owner-Occupied Units"
set_label(panel0020_did$mhmval_p0) <- "Median Home Value"
set_label(panel0020_did$incomepp_p0) <- "Per Capita Income"

nhw <- fixest::feols(annexing ~ vra*period + pop_p0 + popdensity_p0 + pctnhwhite_p0 +
                       pctemp_p0 + hinc_p0 + pctowneroccupied_p0 + mhmval_p0 + incomepp_p0 | plid, data = panel0020_did)
sjPlot::plot_model(nhw, terms = c("vra1", "period1", "vra1:period1"),
                   axis.labels = c("DiD Estimator", "Previously Covered by Section V", "After Shelby"),
                   dot.size = 1,
                   show.values = TRUE)

annex <- tidy(nhw)
openxlsx::write.xlsx(annex, "analyticalfiles/results/annex.xlsx")

# what kind of annex or not ####
# black 
# not vap 
all <- fixest::feols(underbound_black ~ as.factor(vra)*as.factor(period) + pop_p0 + pctnhwhite_p0 + pctnhwhite_total + pctincocc_total + pctownerocc_total +
                       pcthincjobs_total + pctnhblack_total + pctnhblack_p0 + pcth_total + pcth_p0 + pctowneroccupied_p0 + pctemp_p0 + mhmval_p0 + incomepp_p0 + hinc_p0 | plid, data = panel0020_did %>% filter(nhblack_total >= 1))
summary(all)

hpct <- fixest::feols(underbound_black_hpct ~ as.factor(vra)*as.factor(period) + pop_p0 + pctnhwhite_p0 + pctnhwhite_total + pctincocc_total + pctownerocc_total +
                        pcthincjobs_total + pctnhblack_total + pctnhblack_p0 + pcth_total + pcth_p0 + pctowneroccupied_p0 + pctemp_p0 + mhmval_p0 + incomepp_p0 + hinc_p0 | plid, data = panel0020_did %>% filter(nhblack_total >= 1))
summary(hpct)

pct3 <- fixest::feols(underbound_black_3pct ~ as.factor(vra)*as.factor(period) + pop_p0 + pctnhwhite_p0 + pctnhwhite_total + pctincocc_total + pctownerocc_total +
                        pcthincjobs_total + pctnhblack_total + pctnhblack_p0 + pcth_total + pcth_p0 + pctowneroccupied_p0 + pctemp_p0 + mhmval_p0 + incomepp_p0 + hinc_p0 | plid, data = panel0020_did %>% filter(nhblack_total >= 1))
summary(pct3)
pct10 <- fixest::feols(underbound_black_10pct ~ as.factor(vra)*as.factor(period) + pop_p0 + pctnhwhite_p0 + pctnhwhite_total + pctincocc_total + pctownerocc_total +
                         pcthincjobs_total + pctnhblack_total + pctnhblack_p0 + pcth_total + pcth_p0 + pctowneroccupied_p0 + pctemp_p0 +mhmval_p0 + incomepp_p0 + hinc_p0 | plid, data = panel0020_did %>% filter(nhblack_total >= 1))
summary(pct10)
black_all <- list(tidy(all), tidy(hpct), tidy(pct3), tidy(pct10))
openxlsx::write.xlsx(black_all, "analyticalfiles/results/black_all_nowhite.xlsx")

#hisp 
all <- fixest::feols(underbound_hisp ~ as.factor(vra)*as.factor(period) + pop_p0 + pctnhwhite_p0 + pctnhwhite_total + pctincocc_total + pctownerocc_total +
                       pcthincjobs_total + pctnhblack_total + pctnhblack_p0 + pcth_total + pcth_p0 + pctowneroccupied_p0 + pctemp_p0 +mhmval_p0 + incomepp_p0 + hinc_p0 | plid, data = panel0020_did %>% filter(h_total >= 1))
summary(all)
hpct <- fixest::feols(underbound_hisp_hpct ~ as.factor(vra)*as.factor(period) + pop_p0 + pctnhwhite_p0 + pctnhwhite_total + pctincocc_total + pctownerocc_total +
                        pcthincjobs_total + pctnhblack_total + pctnhblack_p0 + pcth_total + pcth_p0 + pctowneroccupied_p0 + pctemp_p0 +mhmval_p0 + incomepp_p0 + hinc_p0 | plid, data = panel0020_did %>% filter(h_total >= 1))
summary(hpct)
pct3 <- fixest::feols(underbound_hisp_3pct ~ as.factor(vra)*as.factor(period) + pop_p0 + pctnhwhite_p0 + pctnhwhite_total + pctincocc_total + pctownerocc_total +
                        pcthincjobs_total + pctnhblack_total + pctnhblack_p0 + pcth_total + pcth_p0 + pctowneroccupied_p0 + pctemp_p0 +mhmval_p0 + incomepp_p0 + hinc_p0 | plid, data = panel0020_did %>% filter(h_total >= 1))
summary(pct3)
pct10 <- fixest::feols(underbound_hisp_10pct ~ as.factor(vra)*as.factor(period) + pop_p0 + pctnhwhite_p0 + pctnhwhite_total + pctincocc_total + pctownerocc_total +
                         pcthincjobs_total + pctnhblack_total + pctnhblack_p0 + pcth_total + pcth_p0 + pctowneroccupied_p0 + pctemp_p0 +mhmval_p0 + incomepp_p0 + hinc_p0 | plid, data = panel0020_did %>% filter(h_total >= 1))
summary(pct10)

hisp_all <- list(tidy(all), tidy(hpct), tidy(pct3), tidy(pct10))
openxlsx::write.xlsx(hisp_all, "analyticalfiles/results/hisp_all_nowhite.xlsx")

#nhw
all <- fixest::feols(underbound_nhwhite ~ as.factor(vra)*as.factor(period) + pop_p0 + pctnhwhite_p0 + pctnhwhite_total + pctincocc_total + pctownerocc_total +
                       pcthincjobs_total + pctnhblack_total + pctnhblack_p0 + pcth_total + pcth_p0 + pctowneroccupied_p0 + pctemp_p0 + mhmval_p0 + incomepp_p0 + hinc_p0 | plid, data = panel0020_did %>% filter(nhwhite_total >= 1))
summary(all)
hpct <- fixest::feols(underbound_nhwhite_hpct ~ as.factor(vra)*as.factor(period) + pop_p0 + pctnhwhite_p0 + pctnhwhite_total + pctincocc_total + pctownerocc_total +
                        pcthincjobs_total + pctnhblack_total + pctnhblack_p0 + pcth_total + pcth_p0 + pctowneroccupied_p0 + pctemp_p0 + mhmval_p0 + incomepp_p0 + hinc_p0 | plid, data = panel0020_did %>% filter(nhwhite_total >= 1))
summary(hpct)
pct3 <- fixest::feols(underbound_nhwhite_3pct ~ as.factor(vra)*as.factor(period) + pop_p0 + pctnhwhite_p0 + pctnhwhite_total + pctincocc_total + pctownerocc_total +
                        pcthincjobs_total + pctnhblack_total + pctnhblack_p0 + pcth_total + pcth_p0 + pctowneroccupied_p0 + pctemp_p0 + mhmval_p0 + incomepp_p0 + hinc_p0 | plid, data = panel0020_did %>% filter(nhwhite_total >= 1))
summary(pct3)

nhwhite_all <- list(tidy(all), tidy(hpct), tidy(pct3))
openxlsx::write.xlsx(nhwhite_all, "analyticalfiles/results/nhwhite_all.xlsx")

# vap 
all <- fixest::feols(underbound_blackvap ~ as.factor(vra)*as.factor(period) + pop_p0 + pctnhwhitevap_p0 + pctnhwhitevap_total + pctincocc_total + pctownerocc_total +
                       pcthincjobs_total + pctnhblackvap_total + pctnhblackvap_p0 + pcthvap_total + pcthispvap_p0 + pctowneroccupied_p0 + pctemp_p0 + mhmval_p0 + incomepp_p0 + hinc_p0 | plid, data = panel0020_did %>% filter(nhblackvap_total >= 1))
summary(all)
hpct <- fixest::feols(underbound_blackvap_hpct ~ as.factor(vra)*as.factor(period) + pop_p0 + pctnhwhitevap_p0 + pctnhwhitevap_total + pctincocc_total + pctownerocc_total +
                        pcthincjobs_total + pctnhblackvap_total + pctnhblackvap_p0 + pcthvap_total + pcthispvap_p0 + pctowneroccupied_p0 + pctemp_p0 + mhmval_p0 + incomepp_p0 + hinc_p0 | plid, data = panel0020_did %>% filter(nhblackvap_total >= 1))
summary(hpct)
pct3 <- fixest::feols(underbound_blackvap_3pct ~ as.factor(vra)*as.factor(period) + pop_p0 + pctnhwhitevap_p0 + pctnhwhitevap_total + pctincocc_total + pctownerocc_total +
                        pcthincjobs_total + pctnhblackvap_total + pctnhblackvap_p0 + pcthvap_total + pcthispvap_p0 + pctowneroccupied_p0 + pctemp_p0 + mhmval_p0 + incomepp_p0 + hinc_p0 | plid, data = panel0020_did %>% filter(nhblackvap_total >= 1))
summary(pct3)
pct10 <- fixest::feols(underbound_blackvap_10pct ~ as.factor(vra)*as.factor(period) + pop_p0 + pctnhwhitevap_p0 + pctnhwhitevap_total + pctincocc_total + pctownerocc_total +
                         pcthincjobs_total + pctnhblackvap_total + pctnhblackvap_p0 + pcthvap_total + pcthispvap_p0 + pctowneroccupied_p0 + pctemp_p0 + mhmval_p0 + incomepp_p0 + hinc_p0 | plid, data = panel0020_did %>% filter(nhblackvap_total >= 1))
summary(pct10)
black_all <- list(tidy(all), tidy(hpct), tidy(pct3), tidy(pct10))
openxlsx::write.xlsx(black_all, "analyticalfiles/results/blackvap_all_nowhite.xlsx")

#hisp 
all <- fixest::feols(underbound_hispvap ~ as.factor(vra)*as.factor(period) + pop_p0 + pctnhwhitevap_p0 + pctnhwhitevap_total + pctincocc_total + pctownerocc_total +
                       pcthincjobs_total + pctnhblackvap_total + pctnhblackvap_p0 + pcthvap_total + pcthispvap_p0 + pctowneroccupied_p0 + pctemp_p0 + mhmval_p0 + incomepp_p0 + hinc_p0 | plid, data = panel0020_did %>% filter(hvap_total >= 1))
summary(all)
hpct <- fixest::feols(underbound_hispvap_hpct ~ as.factor(vra)*as.factor(period) + pop_p0 + pctnhwhitevap_p0 + pctnhwhitevap_total + pctincocc_total + pctownerocc_total +
                        pcthincjobs_total + pctnhblackvap_total + pctnhblackvap_p0 + pcthvap_total + pcthispvap_p0 + pctowneroccupied_p0 + pctemp_p0 + mhmval_p0 + incomepp_p0 + hinc_p0 | plid, data = panel0020_did %>% filter(hvap_total >= 1))
summary(hpct)
pct3 <- fixest::feols(underbound_hispvap_3pct ~ as.factor(vra)*as.factor(period) + pop_p0 + pctnhwhitevap_p0 + pctnhwhitevap_total + pctincocc_total + pctownerocc_total +
                        pcthincjobs_total + pctnhblackvap_total + pctnhblackvap_p0 + pcthvap_total + pcthispvap_p0 + pctowneroccupied_p0 + pctemp_p0 + mhmval_p0 + incomepp_p0 + hinc_p0 | plid, data = panel0020_did %>% filter(hvap_total >= 1))
summary(pct3)
pct10 <- fixest::feols(underbound_hispvap_10pct ~ as.factor(vra)*as.factor(period) + pop_p0 + pctnhwhitevap_p0 + pctnhwhitevap_total + pctincocc_total + pctownerocc_total +
                         pcthincjobs_total + pctnhblackvap_total + pctnhblackvap_p0 + pcthvap_total + pcthispvap_p0 + pctowneroccupied_p0 + pctemp_p0 + mhmval_p0 + incomepp_p0 + hinc_p0 | plid, data = panel0020_did %>% filter(hvap_total >= 1))
summary(pct10)

hisp_all <- list(tidy(all), tidy(hpct), tidy(pct3), tidy(pct10))
openxlsx::write.xlsx(hisp_all, "analyticalfiles/results/hispvap_all_nowhite.xlsx")

#nhw
all <- fixest::feols(underbound_nhwhitevap ~ as.factor(vra)*as.factor(period) + pop_p0 + pctnhwhitevap_p0 + pctnhwhitevap_total + pctincocc_total + pctownerocc_total +
                       pcthincjobs_total + pctnhblackvap_total + pctnhblackvap_p0 + pcthvap_total + pcthispvap_p0 + pctowneroccupied_p0 + pctemp_p0 + mhmval_p0 + incomepp_p0 + hinc_p0 | plid, data = panel0020_did %>% filter(nhwhitevap_total >= 1))
summary(all)
hpct <- fixest::feols(underbound_nhwhitevap_hpct ~ as.factor(vra)*as.factor(period) + pop_p0 + pctnhwhitevap_p0 + pctnhwhitevap_total + pctincocc_total + pctownerocc_total +
                        pcthincjobs_total + pctnhblackvap_total + pctnhblackvap_p0 + pcthvap_total + pcthispvap_p0 + pctowneroccupied_p0 + pctemp_p0 + mhmval_p0 + incomepp_p0 + hinc_p0 | plid, data = panel0020_did %>% filter(nhwhitevap_total >= 1))
summary(hpct)
pct3 <- fixest::feols(underbound_nhwhitevap_3pct ~ as.factor(vra)*as.factor(period) + pop_p0 + pctnhwhitevap_p0 + pctnhwhitevap_total + pctincocc_total + pctownerocc_total +
                        pcthincjobs_total + pctnhblackvap_total + pctnhblackvap_p0 + pcthvap_total + pcthispvap_p0 + pctowneroccupied_p0 + pctemp_p0 + mhmval_p0 + incomepp_p0 + hinc_p0 | plid, data = panel0020_did %>% filter(nhwhitevap_total >= 1))
summary(pct3)

nhwhite_all <- list(tidy(all), tidy(hpct), tidy(pct3))
openxlsx::write.xlsx(nhwhite_all, "analyticalfiles/results/nhwhitevap_all.xlsx")

# p1 race ####
nhb <- fixest::feols(pctnhblack_p1 ~ as.factor(vra)*as.factor(period) + pctnhblack_p0 + pctnhblack_total | plid, data = panel0020_did %>% filter(annexing==1 & nhblack_total >= 1))
summary(nhb)
hisp <- fixest::feols(pcth_p1 ~ as.factor(vra)*as.factor(period) + pcth_p0 + pcth_total | plid, data = panel0020_did %>% filter(annexing==1 & h_total >= 1))
summary(hisp)
nhw <- fixest::feols(pctnhwhite_p1 ~ as.factor(vra)*as.factor(period) + pctnhwhite_p0 + pctnhwhite_total | plid, data = panel0020_did %>% filter(annexing==1 & nhwhite_total >= 1))
summary(nhw)
race_all <- list(tidy(nhb), tidy(hisp), tidy(nhw))
openxlsx::write.xlsx(race_all, "analyticalfiles/results/race_all.xlsx")

# vap 
nhb <- fixest::feols(pctnhblackvap_p1 ~ as.factor(vra)*as.factor(period) + pctnhblackvap_p0 + pctnhblackvap_total | plid, data = panel0020_did %>% filter(annexing==1 & nhblackvap_total >= 1))
summary(nhb)
hisp <- fixest::feols(pcthispvap_p1 ~ as.factor(vra)*as.factor(period) + pcthispvap_p0 + pcthvap_total | plid, data = panel0020_did %>% filter(annexing==1 & hvap_total >= 1))
summary(hisp)
nhw <- fixest::feols(pctnhwhitevap_p1 ~ as.factor(vra)*as.factor(period) + pctnhwhitevap_p0 + pctnhwhitevap_total | plid, data = panel0020_did %>% filter(annexing==1 & nhwhitevap_total >= 1))
summary(nhw)
race_all <- list(tidy(nhb), tidy(hisp), tidy(nhw))
openxlsx::write.xlsx(race_all, "analyticalfiles/results/racevap_all.xlsx")

# viz ####
indices <- c(grep("underbound", names(panel0020_did)))
indices <- names(panel0020_did)[indices]
indices <- indices[!grepl("vraa", indices)]
indices <- indices[grepl("blackvap", indices)]

nhb <- panel0020_did %>%
  filter(nhblackvap_total >= 1) %>%
  group_by(vra, post) %>%
  summarise_at(all_of(indices), 
               ~mean(., na.rm = T)) %>%
  ungroup() %>%
  pivot_longer(cols = contains("underbound"),
               names_to = "Race", 
               values_to = "mean") %>%
  mutate(vra_basis = case_when(
    grepl("vraa", Race) ~ "JLVRAA",
    grepl("_hpct", Race) ~ "0.5%",
    grepl("_1pct", Race) ~ "1%",
    grepl("_3pct", Race) ~ "3%",
    grepl("_10pct", Race) ~ "10%",
    TRUE ~ "0%"),
    Race = "Non-Hispanic Black",
  vra = as.character(vra),
  vra_basis = factor(vra_basis, levels = c(
    "0%",
    "0.5%",
    "1%",
    "3%",
    "10%"
  )))

indices <- c(grep("underbound", names(panel0020_did)))
indices <- names(panel0020_did)[indices]
indices <- indices[!grepl("vraa", indices)]
indices <- indices[grepl("hispvap", indices)]

h <- panel0020_did %>%
  filter(hvap_total >= 1) %>%
  group_by(vra, post) %>%
  summarise_at(all_of(indices), 
               ~mean(., na.rm = T)) %>%
  ungroup() %>%
  pivot_longer(cols = contains("underbound"),
               names_to = "Race", 
               values_to = "mean") %>%
  mutate(vra_basis = case_when(
    grepl("vraa", Race) ~ "JLVRAA",
    grepl("_hpct", Race) ~ "0.5%",
    grepl("_1pct", Race) ~ "1%",
    grepl("_3pct", Race) ~ "3%",
    grepl("_10pct", Race) ~ "10%",
    TRUE ~ "0%"),
    Race = "Hispanic",
    vra = as.character(vra),
    vra_basis = factor(vra_basis, levels = c(
      "0%",
      "0.5%",
      "1%",
      "3%",
      "10%"
    )))

indices <- c(grep("underbound", names(panel0020_did)))
indices <- names(panel0020_did)[indices]
indices <- indices[!grepl("vraa", indices)]
indices <- indices[grepl("whitevap", indices)]

nhw <- panel0020_did %>%
  filter(nhwhitevap_total >= 1) %>%
  group_by(vra, post) %>%
  summarise_at(all_of(indices), 
               ~mean(., na.rm = T)) %>%
  ungroup() %>%
  pivot_longer(cols = contains("underbound"),
               names_to = "Race", 
               values_to = "mean") %>%
  mutate(vra_basis = case_when(
    grepl("vraa", Race) ~ "JLVRAA",
    grepl("_hpct", Race) ~ "0.5%",
    grepl("_1pct", Race) ~ "1%",
    grepl("_3pct", Race) ~ "3%",
    grepl("_10pct", Race) ~ "10%",
    TRUE ~ "0%"),
    Race = "Non-Hispanic White",
    vra = as.character(vra),
    vra_basis = factor(vra_basis, levels = c(
      "0%",
      "0.5%",
      "1%",
      "3%",
      "10%"
    )))

did_vis <- rbind(nhb, h, nhw)
did_vis_total <- ggplot(did_vis,
                        aes(y = mean, x = as.numeric(as.character(post)), color = vra)) + 
  geom_point() + geom_line() + 
  scale_x_continuous(breaks = c(-1, 0, 1, 2)) + 
  scale_y_continuous(labels = scales::percent) +
  geom_vline(xintercept = 0, linetype="dotted", 
             color = "red", size=0.5) +
  facet_grid(vra_basis~Race, labeller = label_wrap_gen(width=12)) + 
  labs(color = "Covered Under \n Section V",
       x = "Period Relative to Shelby v. Holder", 
       y = "Proportion", 
       title = "% of Places Conducting Annexations Resulting in Population Diluation,
Pre- and Post-Shelby Trends by Race, VRA, and Dilution Threshold, VAP") + 
  theme(strip.text.x = element_text(size = 7))
did_vis_total
ggsave(filename = "analyticalfiles/did_vis_annexed_mean_vra_allannex_vap.png",
       plot = did_vis_total,
       dpi = 300)

# no vap 
indices <- c(grep("underbound", names(panel0020_did)))
indices <- names(panel0020_did)[indices]
indices <- indices[!grepl("vraa", indices)]
indices <- indices[!grepl("vap", indices)]
indices <- indices[grepl("black", indices)]

nhb <- panel0020_did %>%
  filter(nhblack_total >= 1) %>%
  group_by(vra, post) %>%
  summarise_at(all_of(indices), 
               ~mean(., na.rm = T)) %>%
  ungroup() %>%
  pivot_longer(cols = contains("underbound"),
               names_to = "Race", 
               values_to = "mean") %>%
  mutate(vra_basis = case_when(
    grepl("vraa", Race) ~ "JLVRAA",
    grepl("_hpct", Race) ~ "0.5%",
    grepl("_1pct", Race) ~ "1%",
    grepl("_3pct", Race) ~ "3%",
    grepl("_10pct", Race) ~ "10%",
    TRUE ~ "0%"),
    Race = "Non-Hispanic Black",
    vra = as.character(vra),
    vra_basis = factor(vra_basis, levels = c(
      "0%",
      "0.5%",
      "1%",
      "3%",
      "10%"
    )))

indices <- c(grep("underbound", names(panel0020_did)))
indices <- names(panel0020_did)[indices]
indices <- indices[!grepl("vraa", indices)]
indices <- indices[!grepl("vap", indices)]
indices <- indices[grepl("hisp", indices)]

h <- panel0020_did %>%
  filter(h_total >= 1) %>%
  group_by(vra, post) %>%
  summarise_at(all_of(indices), 
               ~mean(., na.rm = T)) %>%
  ungroup() %>%
  pivot_longer(cols = contains("underbound"),
               names_to = "Race", 
               values_to = "mean") %>%
  mutate(vra_basis = case_when(
    grepl("vraa", Race) ~ "JLVRAA",
    grepl("_hpct", Race) ~ "0.5%",
    grepl("_1pct", Race) ~ "1%",
    grepl("_3pct", Race) ~ "3%",
    grepl("_10pct", Race) ~ "10%",
    TRUE ~ "0%"),
    Race = "Hispanic",
    vra = as.character(vra),
    vra_basis = factor(vra_basis, levels = c(
      "0%",
      "0.5%",
      "1%",
      "3%",
      "10%"
    )))

indices <- c(grep("underbound", names(panel0020_did)))
indices <- names(panel0020_did)[indices]
indices <- indices[!grepl("vraa", indices)]
indices <- indices[!grepl("vap", indices)]
indices <- indices[grepl("white", indices)]

nhw <- panel0020_did %>%
  filter(nhwhite_total >= 1) %>%
  group_by(vra, post) %>%
  summarise_at(all_of(indices), 
               ~mean(., na.rm = T)) %>%
  ungroup() %>%
  pivot_longer(cols = contains("underbound"),
               names_to = "Race", 
               values_to = "mean") %>%
  mutate(vra_basis = case_when(
    grepl("vraa", Race) ~ "JLVRAA",
    grepl("_hpct", Race) ~ "0.5%",
    grepl("_1pct", Race) ~ "1%",
    grepl("_3pct", Race) ~ "3%",
    grepl("_10pct", Race) ~ "10%",
    TRUE ~ "0%"),
    Race = "Non-Hispanic White",
    vra = as.character(vra),
    vra_basis = factor(vra_basis, levels = c(
      "0%",
      "0.5%",
      "1%",
      "3%",
      "10%"
    )))

did_vis <- rbind(nhb, h, nhw)
did_vis_total <- ggplot(did_vis,
                        aes(y = mean, x = as.numeric(as.character(post)), color = vra)) + 
  geom_point() + geom_line() + 
  scale_x_continuous(breaks = c(-1, 0, 1, 2)) + 
  scale_y_continuous(labels = scales::percent) +
  geom_vline(xintercept = 0, linetype="dotted", 
             color = "red", size=0.5) +
  facet_grid(vra_basis~Race, labeller = label_wrap_gen(width=12)) + 
  labs(color = "Covered Under \n Section V",
       x = "Period Relative to Shelby v. Holder", 
       y = "Proportion", 
       title = "% of Places Conducting Annexations Resulting in Population Diluation,
Pre- and Post-Shelby Trends by Race, VRA, and Dilution Threshold") + 
  theme(strip.text.x = element_text(size = 7))
did_vis_total
ggsave(filename = "analyticalfiles/did_vis_annexed_mean_vra_allannex_novap.png",
       plot = did_vis_total,
       dpi = 300)

# annex or not 
did_vis <- panel0020_did %>%
  group_by(vra, post) %>%
  summarise(mean_annexing = mean(annexing, na.rm = T)) %>%
  ungroup() %>%
  mutate(vra = as.character(vra)) 

did_vis_total <- ggplot(did_vis,
                        aes(y = mean_annexing, x = as.numeric(as.character(post)), color = vra)) + 
  geom_point() + geom_line() + 
  scale_x_continuous(breaks = c(-1, 0, 1, 2)) + 
  scale_y_continuous(labels = scales::percent) +
  geom_vline(xintercept = 0, linetype="dotted", 
             color = "red", size=0.5) +
  #facet_grid(vra_basis~Race, labeller = label_wrap_gen(width=12)) + 
  labs(color = "Covered Under \n Section V",
       x = "Period Relative to Shelby v. Holder", 
       y = "Proportion", 
       title = "% of Places Conducting Any Annexations,
Pre- and Post-Shelby Trends by VRA Coverage") + 
  theme(strip.text.x = element_text(size = 7))
did_vis_total
ggsave(filename = "analyticalfiles/did_vis_annexed_mean_vra.png",
       plot = did_vis_total,
       dpi = 300)

# % black 
indices <- c(grep("_p1", names(panel0020_did)))
indices <- names(panel0020_did)[indices]
indices <- indices[grepl("pct", indices)]
indices <- indices[grepl("black_", indices)]

nhb <- panel0020_did %>%
  filter(annexing==1 & nhblack_total >= 1) %>%
  group_by(vra, post) %>%
  summarise_at(all_of(indices), 
               ~mean(., na.rm = T)) %>%
  ungroup() %>%
  pivot_longer(cols = contains("p1"),
               names_to = "Race", 
               values_to = "mean") %>%
  mutate(
    Race ="Non-Hispanic Black",
    vra = as.character(vra))

indices <- c(grep("_p1", names(panel0020_did)))
indices <- names(panel0020_did)[indices]
indices <- indices[grepl("pct", indices)]
indices <- indices[grepl("h_", indices)]
h <- panel0020_did %>%
  filter(annexing==1 & h_total >= 1) %>%
  group_by(vra, post) %>%
  summarise_at(all_of(indices), 
               ~mean(., na.rm = T)) %>%
  ungroup() %>%
  pivot_longer(cols = contains("_p1"),
               names_to = "Race", 
               values_to = "mean") %>%
  mutate(
    Race ="Hispanic",
    vra = as.character(vra))

indices <- c(grep("_p1", names(panel0020_did)))
indices <- names(panel0020_did)[indices]
indices <- indices[grepl("pct", indices)]
indices <- indices[grepl("white_", indices)]
nhw <- panel0020_did %>%
  filter(annexing==1 & nhwhite_total >= 1) %>%
  group_by(vra, post) %>%
  summarise_at(all_of(indices), 
               ~mean(., na.rm = T)) %>%
  ungroup() %>%
  pivot_longer(cols = contains("_p1"),
               names_to = "Race", 
               values_to = "mean") %>%
  mutate(
    Race ="Non-Hispanic White",
    vra = as.character(vra))

did_vis <- rbind(nhb, h, nhw)

did_vis_total <- ggplot(did_vis,
                        aes(y = mean, x = as.numeric(as.character(post)), color = vra)) + 
  geom_point() + geom_line() + 
  scale_x_continuous(breaks = c(-1, 0, 1, 2)) + 
  #scale_y_continuous(labels = scales::percent) +
  geom_vline(xintercept = 0, linetype="dotted", 
             color = "red", size=0.5) +
  facet_grid(~Race, labeller = label_wrap_gen(width=12)) + 
  labs(color = "Covered Under \n Section V",
       x = "Period Relative to Shelby v. Holder", 
       y = "Mean", 
       title = "VAP Composition of Annexing Places at End of Period
Pre- and Post-Shelby Trends by Race and VRA Coverage") + 
  theme(strip.text.x = element_text(size = 7))
did_vis_total
ggsave(filename = "analyticalfiles/did_vis_racep1.png",
       plot = did_vis_total,
       dpi = 300)

# descriptives? ####
white_mean <- panel0020_did %>%
  filter(annexing==1) %>%
  group_by(underbound_nhwhite) %>%
  summarize_at(vars(c(pop_total:othervap_total_1, pop_p0:othervapgrowth, maj_white, pctowneroccupied_p0, property_inc:pctothervap_p1)), ~mean(., na.rm = T)) %>%
  t() %>%
  as.data.frame()
write.csv(white_mean, "analyticalfiles/white_mean.csv", row.names = T)

black_mean <- panel0020_did %>%
  filter(annexing==1) %>%
  group_by(underbound_black) %>% 
  summarize_at(vars(c(pop_total:othervap_total_1,  pop_p0:othervapgrowth, maj_white, pctowneroccupied_p0, property_inc:pctothervap_p1)), ~mean(., na.rm = T)) %>%
  t() %>%
  as.data.frame()
write.csv(black_mean, "analyticalfiles/black_mean.csv", row.names = T)

hisp_mean <- panel0020_did %>%
  filter(annexing==1) %>%
  group_by(underbound_hisp) %>% 
  summarize_at(vars(c(pop_total:othervap_total_1,  pop_p0:othervapgrowth, maj_white, pctowneroccupied_p0, property_inc:pctothervap_p1)), ~mean(., na.rm = T)) %>%
  t() %>%
  as.data.frame()
write.csv(hisp_mean, "analyticalfiles/hisp_mean.csv", row.names = T)

# which places are most segregated?
panel0020_did %<>%
  mutate(blackdiff = pctnhblack_total - pctnhblack_p0)

panel0020_did %>%
  filter(underbound_black == 1 & period == 1) %>%
  arrange(desc(blackdiff), desc(pop_p0)) %>%
  View()
