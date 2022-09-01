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
  dplyr::summarize(pop_total = sum(pop, na.rm = T),
            nhblack_total = sum(nhblack, na.rm = T),
            nhwhite_total = sum(nhwhite, na.rm = T),
            h_total = sum(h, na.rm = T),
            asian_total = sum(asian, na.rm = T), 
            native_total = sum(native, na.rm = T), 
            other_total = sum(other, na.rm = T),
            nbmin_total = sum(nbmin, na.rm = T),
         vap_total = sum(vap, na.rm = T),
         nhblackvap_total = sum(nhbvap),
         nhwhitevap_total = sum(nhwvap),
         hvap_total = sum(hispvap),
         nativevap_total = sum(nativevap, na.rm = T),
         asianvap_total = sum(asianvap, na.rm = T),
         othervap_total = sum(othervap, na.rm = T),
         nbminvap_total = sum(nbminvap, na.rm = T),
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
         pctasian_total = (asian_total/pop_total)*100,
         pctnative_total = (native_total/pop_total)*100,
         pctother_total = (other_total/pop_total)*100,
         pctnbmin_total = (nbmin_total/pop_total)*100,
         pctownerocc_total = (owneroccupied_total/hu_total)*100,
         pctincopp_total = ifelse(nwork_total == 0, 0, (incopp_total/nwork_total)*100),
         pcthincjobs_total = ifelse(njobs_total == 0, 0, (nhincjobs_total/njobs_total)*100),
         pctnhblackvap_total = (nhblackvap_total/vap_total)*100,
         pcthvap_total = (hvap_total/vap_total)*100,
         pctnhwhitevap_total = (nhwhitevap_total/vap_total)*100,
         pctasianvap_total = (asianvap_total/vap_total)*100,
         pctnativevap_total = (nativevap_total/vap_total)*100,
         pctothervap_total = (othervap_total/vap_total)*100,
         pctnbminvap_total = (nbminvap_total/vap_total)*100)
  
place_by_annex <- aa0713 %>%
  mutate(incopp = man + ret) %>%
  filter(annexed==1) %>%
  group_by(plid, annexed) %>%
  dplyr::summarize(pop_total = sum(pop, na.rm = T),
            nhblack_total = sum(nhblack, na.rm = T),
            nhwhite_total = sum(nhwhite, na.rm = T),
            h_total = sum(h, na.rm = T),
            asian_total = sum(asian, na.rm = T),
            native_total = sum(native, na.rm = T),
            other_total = sum(other, na.rm = T),
            nbmin_total = sum(nbmin, na.rm = T),
            njobs_total = sum(njobs07, na.rm = T),
            nhincjobs_total = sum(nhincjobs07, na.rm = T),
            nwork_total = sum(jobs, na.rm = T),
            incopp_total = sum(incopp, na.rm = T),
            hu_total = sum(hu, na.rm = T),
            owneroccupied_total = sum(owneroccupied, na.rm = T),
            vacancy_total = sum(vacancy, na.rm = T),
            vap_total = sum(vap, na.rm = T),
            nhblackvap_total = sum(nhbvap, na.rm = T),
            nhwhitevap_total = sum(nhwvap, na.rm = T),
            hvap_total = sum(hispvap, na.rm = T),
            nativevap_total = sum(nativevap, na.rm = T),
            asianvap_total = sum(asianvap, na.rm = T),
            othervap_total = sum(othervap, na.rm = T),
            nbminvap_total = sum(nbminvap, na.rm = T)) %>%
  ungroup() %>%
  pivot_wider(
    id_cols = plid,
    names_from = annexed,
    values_from = c(pop_total:nbminvap_total)
  ) %>%
  mutate(pctnhblack_total_1 = (nhblack_total_1/pop_total_1)*100,
        pctnhwhite_total_1 = (nhwhite_total_1/pop_total_1)*100,
        pcth_total_1 = (h_total_1/pop_total_1)*100,
        pctnhblackvap_total_1 = (nhblackvap_total_1/vap_total_1)*100,
        pctnhwhitevap_total_1 = (nhwhitevap_total_1/vap_total_1)*100,
        pcthvap_total_1 = (hvap_total_1/vap_total_1)*100,
        pctasian_total_1 = (asian_total_1/pop_total_1)*100,
        pctnative_total_1 = (native_total_1/pop_total_1)*100,
        pctother_total_1 = (other_total_1/pop_total_1)*100,
        pctnbmin_total_1 = (nbmin_total_1/pop_total_1)*100,
        pctasianvap_total_1 = (asianvap_total_1/vap_total_1)*100,
        pctnativevap_total_1 = (nativevap_total_1/vap_total_1)*100,
        pctothervap_total_1 = (othervap_total_1/vap_total_1)*100,
        pctnbminvap_total_1 = (nbminvap_total_1/vap_total_1)*100,
        pctincopp_total_1 = ifelse(nwork_total_1 == 0, 0, (incopp_total_1/nwork_total_1)*100),
        pcthincjobs_total_1 = ifelse(njobs_total_1 == 0, 0, (nhincjobs_total_1/njobs_total_1)*100),
        pctownerocc_total_1 = (owneroccupied_total_1/hu_total_1)*100) %>%
  filter(pop_total_1 > 1, 
         hu_total_1 > 1)

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

places_vra_0713 <- unique(places_vra$plid[places_vra$vra==1])

# add place-level data for race and vap 
# vra violation = if pctwhite after annex > pctwhite before annex compared to
# what would have been, i.e. pctwhite given growth rate by 2013
# or if pctblack after annex < pctblack before annex compared to what would 
# have been, i.e. pctwhite given growth rate by 2013
# or if pcth after annex < pcth before annex compared to what would have been,
# i.e. pcth given growth rate by 2013 
pl0007 <- read_csv("pl0007_var.csv")
summary(pl0007$popgrowth)
table(pl_annex_var_0713$plid %in% pl0007$plid) #239 false

cdps07 <- read_csv("pl2007_cleaned.csv") %>% # want to know which places are CDPs--they do not annex
  select(Geo_NAME, plid) %>%
  mutate(cdp = ifelse(grepl("CDP|cdp", Geo_NAME), 1, 0)) %>%
  filter(cdp==1)

pl_annex_var_0713 %<>%
  filter(plid %in% pl0007$plid & !(plid %in% cdps07$plid)) %>%
  left_join(pl0007, by = "plid") %>%
  mutate(post = 0,
         time = "2007 to 2013",
         pctowneroccupied07p = (owneroccupied07p/hu07p)*100) 

table(pl_annex_var_0713$annexing)

# make underbound variable
# # of black_annexed(2013) + blackvap(2010)/(vap2000 + annexed_vap2013) - blackvap2010/
pl_annex_var_0713 %<>%
  mutate(
    increase_whitevap = ifelse((annexing == 1 & ((((nhwhitevap_total_1 + nhwhitevap07p)/(vap07p + vap_total_1)) - (nhwhitevap07p/vap07p))) > 0), 1, 0),
    underbound_blackvap_vraa = ifelse(
      (annexing == 1 & nhblackvap_total > 1 & increase_whitevap == 1 & vraa == 1 & ((((nhblackvap_total_1 + nhblackvap07p)/(vap_total_1 + vap07p)) - (nhblackvap07p/vap07p))) < -0.03), 1, 0), 
    underbound_hispvap_vraa = ifelse(
      (annexing == 1 & hvap_total > 1 & increase_whitevap == 1 & vraa == 1 & ((((hvap_total_1 + hispvap07p)/(vap_total_1 + vap07p)) - (hispvap07p/vap07p))) < -0.03), 1, 0),
    underbound_asianvap_vraa = ifelse(
      (annexing == 1 & increase_whitevap == 1 & asianvap_total > 1 & vraa == 1 & ((((asianvap_total_1 + asianvap07p)/(vap07p + vap_total_1)) - (asianvap07p/vap07p))) < -0.03), 1, 0), 
    underbound_nativevap_vraa = ifelse(
      (annexing == 1 & increase_whitevap == 1 & nativevap_total > 1 & vraa == 1 & ((((nativevap_total_1 + nativevap07p)/(vap07p + vap_total_1)) - (nativevap07p/vap07p))) < -0.03), 1, 0),
    underbound_othervap_vraa = ifelse(
      (annexing == 1 & increase_whitevap == 1 & othervap_total > 1 & vraa == 1 & ((((othervap_total_1 + othervap07p)/(vap07p + vap_total_1)) - (othervap07p/vap07p))) < -0.03), 1, 0),
    underbound_nbminvap_vraa = ifelse(
      (annexing == 1 & increase_whitevap == 1 & nbminvap_total > 1 & vraa == 1 & ((((nbminvap_total_1 + nbminvap07p)/(vap07p + vap_total_1)) - (nbminvap07p/vap07p))) < -0.03), 1, 0),
    underbound_nhwhitevap_vraa = ifelse(
      (annexing == 1 & nhwhitevap_total > 1 & vraa == 1 & ((((nhwhitevap_total_1 + nhwhitevap07p)/(vap07p + vap_total_1)) - (nhwhitevap07p/vap07p))) < -0.03), 1, 0),
    underbound_blackvap_3pct = ifelse(
      (annexing == 1 & nhblackvap_total > 1 & increase_whitevap == 1 & ((((nhblackvap_total_1 + nhblackvap07p)/(vap_total_1 + vap07p)) - (nhblackvap07p/vap07p))) < -0.03), 1, 0), 
    underbound_hispvap_3pct = ifelse(
      (annexing == 1 & hvap_total > 1 & increase_whitevap == 1 & ((((hvap_total_1 + hispvap07p)/(vap_total_1 + vap07p)) - (hispvap07p/vap07p))) < -0.03), 1, 0),
    underbound_asianvap_3pct = ifelse(
      (annexing == 1 & increase_whitevap == 1 & asianvap_total > 1 & ((((asianvap_total_1 + asianvap07p)/(vap07p + vap_total_1)) - (asianvap07p/vap07p))) < -0.03), 1, 0), 
    underbound_nativevap_3pct = ifelse(
      (annexing == 1 & increase_whitevap == 1 & nativevap_total > 1 & ((((nativevap_total_1 + nativevap07p)/(vap07p + vap_total_1)) - (nativevap07p/vap07p))) < -0.03), 1, 0),
    underbound_othervap_3pct = ifelse(
      (annexing == 1 & increase_whitevap == 1 & othervap_total > 1 & ((((othervap_total_1 + othervap07p)/(vap07p + vap_total_1)) - (othervap07p/vap07p))) < -0.03), 1, 0),
    underbound_nbminvap_3pct = ifelse(
      (annexing == 1 & increase_whitevap == 1 & nbminvap_total > 1 & ((((nbminvap_total_1 + nbminvap07p)/(vap07p + vap_total_1)) - (nbminvap07p/vap07p))) < -0.03), 1, 0),
    underbound_nhwhitevap_3pct = ifelse(
      (annexing == 1 & nhwhitevap_total > 1 & ((((nhwhitevap_total_1 + nhwhitevap07p)/(vap07p + vap_total_1)) - (nhwhitevap07p/vap07p))) < -0.03), 1, 0),
    underbound_blackvap_hpct = ifelse(
      (annexing == 1 & nhblackvap_total > 1 & increase_whitevap == 1 & ((((nhblackvap_total_1 + nhblackvap07p)/(vap_total_1 + vap07p)) - (nhblackvap07p/vap07p))) < -0.005), 1, 0), 
    underbound_hispvap_hpct = ifelse(
      (annexing == 1 & hvap_total > 1 & increase_whitevap == 1 & ((((hvap_total_1 + hispvap07p)/(vap_total_1 + vap07p)) - (hispvap07p/vap07p))) < -0.005), 1, 0),
    underbound_asianvap_hpct = ifelse(
      (annexing == 1 & increase_whitevap == 1 & asianvap_total > 1 & ((((asianvap_total_1 + asianvap07p)/(vap07p + vap_total_1)) - (asianvap07p/vap07p))) < -0.005), 1, 0), 
    underbound_nativevap_hpct = ifelse(
      (annexing == 1 & increase_whitevap == 1 & nativevap_total > 1 & ((((nativevap_total_1 + nativevap07p)/(vap07p + vap_total_1)) - (nativevap07p/vap07p))) < -0.005), 1, 0),
    underbound_othervap_hpct = ifelse(
      (annexing == 1 & increase_whitevap == 1 & othervap_total > 1 & ((((othervap_total_1 + othervap07p)/(vap07p + vap_total_1)) - (othervap07p/vap07p))) < -0.005), 1, 0),
    underbound_nbminvap_hpct = ifelse(
      (annexing == 1 & increase_whitevap == 1 & nbminvap_total > 1 & ((((nbminvap_total_1 + nbminvap07p)/(vap07p + vap_total_1)) - (nbminvap07p/vap07p))) < -0.005), 1, 0),
    underbound_nhwhitevap_hpct = ifelse(
      (annexing == 1 & nhwhitevap_total > 1 & ((((nhwhitevap_total_1 + nhwhitevap07p)/(vap07p + vap_total_1)) - (nhwhitevap07p/vap07p))) < -0.005), 1, 0),
    underbound_blackvap_10pct = ifelse(
      (annexing == 1 & nhblackvap_total > 1 & increase_whitevap == 1 & ((((nhblackvap_total_1 + nhblackvap07p)/(vap_total_1 + vap07p)) - (nhblackvap07p/vap07p))) < -0.1), 1, 0), 
    underbound_hispvap_10pct = ifelse(
      (annexing == 1 & hvap_total >1 & increase_whitevap == 1 & ((((hvap_total_1 + hispvap07p)/(vap_total_1 + vap07p)) - (hispvap07p/vap07p))) < -0.1), 1, 0),
    underbound_asianvap_10pct = ifelse(
      (annexing == 1 & increase_whitevap == 1 & asianvap_total > 1 & ((((asianvap_total_1 + asianvap07p)/(vap07p + vap_total_1)) - (asianvap07p/vap07p))) < -0.1), 1, 0), 
    underbound_nativevap_10pct = ifelse(
      (annexing == 1 & increase_whitevap == 1 & nativevap_total > 1 & ((((nativevap_total_1 + nativevap07p)/(vap07p + vap_total_1)) - (nativevap07p/vap07p))) < -0.1), 1, 0),
    underbound_othervap_10pct = ifelse(
      (annexing == 1 & increase_whitevap == 1 & othervap_total > 1 & ((((othervap_total_1 + othervap07p)/(vap07p + vap_total_1)) - (othervap07p/vap07p))) < -0.1), 1, 0),
    underbound_nbminvap_10pct = ifelse(
      (annexing == 1 & increase_whitevap == 1 & nbminvap_total > 1 & ((((nbminvap_total_1 + nbminvap07p)/(vap07p + vap_total_1)) - (nbminvap07p/vap07p))) < -0.1), 1, 0),
    underbound_nhwhitevap_10pct = ifelse(
      (annexing == 1 & nhwhitevap_total > 1 & ((((nhwhitevap_total_1 + nhwhitevap07p)/(vap07p + vap_total_1)) - (nhwhitevap07p/vap07p))) < -0.1), 1, 0),
    underbound_blackvap = ifelse(
      (annexing == 1 & nhblackvap_total > 1 & increase_whitevap == 1 & ((((nhblackvap_total_1 + nhblackvap07p)/(vap_total_1 + vap07p)) < (nhblackvap07p/vap07p)))), 1, 0), 
    underbound_hispvap = ifelse(
      (annexing == 1 & hvap_total > 1 & increase_whitevap == 1 & ((((hvap_total_1 + hispvap07p)/(vap_total_1 + vap07p)) < (hispvap07p/vap07p)))), 1, 0),
    underbound_asianvap = ifelse(
      (annexing == 1 & increase_whitevap == 1 & asianvap_total > 1 & ((((asianvap_total_1 + asianvap07p)/(vap07p + vap_total_1)) < (asianvap07p/vap07p)))), 1, 0), 
    underbound_nativevap = ifelse(
      (annexing == 1 & increase_whitevap == 1 & nativevap_total > 1 & ((((nativevap_total_1 + nativevap07p)/(vap07p + vap_total_1)) < (nativevap07p/vap07p)))), 1, 0),
    underbound_othervap = ifelse(
      (annexing == 1 & increase_whitevap == 1 & othervap_total > 1 & ((((othervap_total_1 + othervap07p)/(vap07p + vap_total_1)) < (othervap07p/vap07p)))), 1, 0),
    underbound_nbminvap = ifelse(
      (annexing == 1 & increase_whitevap == 1 & nbminvap_total > 1 & ((((nbminvap_total_1 + nbminvap07p)/(vap07p + vap_total_1)) < (nbminvap07p/vap07p)))), 1, 0),
    underbound_nhwhitevap = ifelse(
      (annexing == 1 & nhwhitevap_total > 1 & ((((nhwhitevap_total_1 + nhwhitevap07p)/(vap07p + vap_total_1)) < (nhwhitevap07p/vap07p)))), 1, 0)
  )

# not about VAP 
pl_annex_var_0713 %<>%
  mutate(
    increase_white = ifelse((annexing == 1 & ((((nhwhite_total_1 + nhwhite07p)/(pop07p + pop_total_1)) - (nhwhite07p/pop07p))) > 0), 1, 0),
    underbound_black_vraa = ifelse(
      (annexing == 1 & nhblack_total > 1 & increase_white == 1 & vraa == 1 & ((((nhblack_total_1 + nhblack07p)/(pop_total_1 + pop07p)) - (nhblack07p/pop07p))) < -0.03), 1, 0), 
    underbound_hisp_vraa = ifelse(
      (annexing == 1 & h_total > 1 & increase_white == 1 & vraa == 1 & ((((h_total_1 + h07p)/(pop_total_1 + pop07p)) - (h07p/pop07p))) < -0.03), 1, 0),
    underbound_asian_vraa = ifelse(
     (annexing == 1 & increase_white == 1 & vraa == 1 & asian_total > 1 & ((((asian_total_1 + asian07p)/(pop07p + pop_total_1)) - (asian07p/pop07p))) < -0.03), 1, 0),
    underbound_native_vraa = ifelse(
     (annexing == 1 & increase_white == 1 & vraa == 1 & native_total > 1 & ((((native_total_1 + native07p)/(pop07p + pop_total_1)) - (native07p/pop07p))) < -0.03), 1, 0),
    underbound_other_vraa = ifelse(
     (annexing == 1 & increase_white == 1 & vraa == 1 & other_total > 1 & ((((other_total_1 + other07p)/(pop07p + pop_total_1)) - (other07p/pop07p))) < -0.03), 1, 0),
    underbound_nbmin_vraa = ifelse(
      (annexing == 1 & increase_white == 1 & vraa == 1 & nbmin_total > 1 & ((((nbmin_total_1 + nbmin07p)/(pop07p + pop_total_1)) - (nbmin07p/pop07p))) < -0.03), 1, 0),
    underbound_nhwhite_vraa = ifelse(
      (annexing == 1 & nhwhite_total > 1 & vraa == 1 & ((((nhwhite_total_1 + nhwhite07p)/(pop07p + pop_total_1)) - (nhwhite07p/pop07p))) < -0.03), 1, 0),
    underbound_black_3pct = ifelse(
      (annexing == 1 & nhblack_total > 1 & increase_white == 1 & ((((nhblack_total_1 + nhblack07p)/(pop_total_1 + pop07p)) - (nhblack07p/pop07p))) < -0.03), 1, 0), 
    underbound_hisp_3pct = ifelse(
      (annexing == 1 & h_total > 1 & increase_white == 1 & ((((h_total_1 + h07p)/(pop_total_1 + pop07p)) - (h07p/pop07p))) < -0.03), 1, 0),
    underbound_asian_3pct = ifelse(
      (annexing == 1 & increase_white == 1 & asian_total > 1 & ((((asian_total_1 + asian07p)/(pop07p + pop_total_1)) - (asian07p/pop07p))) < -0.03), 1, 0),
    underbound_native_3pct = ifelse(
      (annexing == 1 & increase_white == 1 & native_total > 1 & ((((native_total_1 + native07p)/(pop07p + pop_total_1)) - (native07p/pop07p))) < -0.03), 1, 0),
    underbound_other_3pct = ifelse(
      (annexing == 1 & increase_white == 1 & other_total > 1 & ((((other_total_1 + other07p)/(pop07p + pop_total_1)) - (other07p/pop07p))) < -0.03), 1, 0),
    underbound_nbmin_3pct = ifelse(
      (annexing == 1 & increase_white == 1 & nbmin_total > 1 & ((((nbmin_total_1 + nbmin07p)/(pop07p + pop_total_1)) - (nbmin07p/pop07p))) < -0.03), 1, 0),
    underbound_nhwhite_3pct = ifelse(
      (annexing == 1 & nhwhite_total > 1 & ((((nhwhite_total_1 + nhwhite07p)/(pop07p + pop_total_1)) - (nhwhite07p/pop07p))) < -0.03), 1, 0),
    underbound_black_hpct = ifelse(
      (annexing == 1 & nhblack_total > 1 & increase_white == 1 & ((((nhblack_total_1 + nhblack07p)/(pop_total_1 + pop07p)) - (nhblack07p/pop07p))) < -0.005), 1, 0), 
    underbound_hisp_hpct = ifelse(
      (annexing == 1 & h_total > 1 & increase_white == 1 & ((((h_total_1 + h07p)/(pop_total_1 + pop07p)) - (h07p/pop07p))) < -0.005), 1, 0),
    underbound_asian_hpct = ifelse(
      (annexing == 1 & increase_white == 1 & asian_total > 1 & ((((asian_total_1 + asian07p)/(pop07p + pop_total_1)) - (asian07p/pop07p))) < -0.005), 1, 0),
    underbound_native_hpct = ifelse(
      (annexing == 1 & increase_white == 1 & native_total > 1 & ((((native_total_1 + native07p)/(pop07p + pop_total_1)) - (native07p/pop07p))) < -0.005), 1, 0),
    underbound_other_hpct = ifelse(
      (annexing == 1 & increase_white == 1 & other_total > 1 & ((((other_total_1 + other07p)/(pop07p + pop_total_1)) - (other07p/pop07p))) < -0.005), 1, 0),
    underbound_nbmin_hpct = ifelse(
      (annexing == 1 & increase_white == 1 & nbmin_total > 1 & ((((nbmin_total_1 + nbmin07p)/(pop07p + pop_total_1)) - (nbmin07p/pop07p))) < -0.005), 1, 0),
    underbound_nhwhite_hpct = ifelse(
      (annexing == 1 & nhwhite_total > 1 & ((((nhwhite_total_1 + nhwhite07p)/(pop07p + pop_total_1)) - (nhwhite07p/pop07p))) < -0.005), 1, 0),
    underbound_black_10pct = ifelse(
      (annexing == 1 & nhblack_total > 1 & increase_white == 1 & ((((nhblack_total_1 + nhblack07p)/(pop_total_1 + pop07p)) - (nhblack07p/pop07p))) < -0.1), 1, 0), 
    underbound_hisp_10pct = ifelse(
      (annexing == 1 & h_total > 1 & increase_white == 1 & ((((h_total_1 + h07p)/(pop_total_1 + pop07p)) - (h07p/pop07p))) < -0.1), 1, 0),
    underbound_asian_10pct = ifelse(
      (annexing == 1 & increase_white == 1 & asian_total > 1 & ((((asian_total_1 + asian07p)/(pop07p + pop_total_1)) - (asian07p/pop07p))) < -0.1), 1, 0),
    underbound_native_10pct = ifelse(
      (annexing == 1 & increase_white == 1 & native_total > 1 & ((((native_total_1 + native07p)/(pop07p + pop_total_1)) - (native07p/pop07p))) < -0.1), 1, 0),
    underbound_other_10pct = ifelse(
      (annexing == 1 & increase_white == 1 & other_total > 1 & ((((other_total_1 + other07p)/(pop07p + pop_total_1)) - (other07p/pop07p))) < -0.1), 1, 0),
    underbound_nbmin_10pct = ifelse(
      (annexing == 1 & increase_white == 1 & nbmin_total > 1 & ((((nbmin_total_1 + nbmin07p)/(pop07p + pop_total_1)) - (nbmin07p/pop07p))) < -0.1), 1, 0),
    underbound_nhwhite_10pct = ifelse(
      (annexing == 1 & nhwhite_total > 1 & ((((nhwhite_total_1 + nhwhite07p)/(pop07p + pop_total_1)) - (nhwhite07p/pop07p))) < -0.1), 1, 0),
    underbound_black = ifelse(
      (annexing == 1 & nhblack_total > 1 & increase_white == 1 & ((((nhblack_total_1 + nhblack07p)/(pop_total_1 + pop07p)) < (nhblack07p/pop07p)))), 1, 0), 
    underbound_hisp = ifelse(
      (annexing == 1 & h_total > 1 & increase_white == 1 & ((((h_total_1 + h07p)/(pop_total_1 + pop07p)) < (h07p/pop07p)))), 1, 0),
    underbound_asian = ifelse(
      (annexing == 1 & increase_white == 1 & asian_total > 1 & ((((asian_total_1 + asian07p)/(pop07p + pop_total_1)) < (asian07p/pop07p)))), 1, 0),
    underbound_native = ifelse(
      (annexing == 1 & increase_white == 1 & native_total > 1 & ((((native_total_1 + native07p)/(pop07p + pop_total_1)) < (native07p/pop07p)))), 1, 0),
    underbound_other = ifelse(
      (annexing == 1 & increase_white == 1 & other_total > 1 & ((((other_total_1 + other07p)/(pop07p + pop_total_1)) < (other07p/pop07p)))), 1, 0),
    underbound_nbmin = ifelse(
      (annexing == 1 & increase_white == 1 & nbmin_total > 1 & ((((nbmin_total_1 + nbmin07p)/(pop07p + pop_total_1)) < (nbmin07p/pop07p)))), 1, 0),
    underbound_nhwhite = ifelse(
      (annexing == 1 & nhwhite_total > 1 & ((((nhwhite_total_1 + nhwhite07p)/(pop07p + pop_total_1)) < (nhwhite07p/pop07p)))), 1, 0)
  )

# compare SES
pl_annex_var_0713 %<>% 
  mutate(
    property_inc = ifelse(
      (annexing == 1 & owneroccupied_total > 1 & (((owneroccupied_total_1 + owneroccupied07p)/(hu07p + hu_total_1)) > (hu07p + hu_total_1))), 1, 0),
    hincjobs = ifelse(
      (annexing == 1 & nhincjobs_total > 1 & (((nhincjobs_total_1 + overlodesthresh07p)/(emp07p + njobs_total_1)) > (emp07p + njobs_total_1))), 1, 0)
  )

pl_annex_var_0713 %<>%
  filter(pop_total > 1 & hu_total > 1 & pop07p > 0) 

table(pl_annex_var_0713$underbound_blackvap)
table(pl_annex_var_0713$underbound_hispvap)
table(pl_annex_var_0713$underbound_asianvap)
table(pl_annex_var_0713$underbound_nativevap)
table(pl_annex_var_0713$underbound_othervap)
table(pl_annex_var_0713$underbound_nbminvap)
table(pl_annex_var_0713$underbound_nhwhitevap)
table(pl_annex_var_0713$underbound_blackvap_vraa)
table(pl_annex_var_0713$underbound_hispvap_vraa)
table(pl_annex_var_0713$underbound_asianvap_vraa)
table(pl_annex_var_0713$underbound_nativevap_vraa)
table(pl_annex_var_0713$underbound_othervap_vraa)
table(pl_annex_var_0713$underbound_nbminvap_vraa)
table(pl_annex_var_0713$underbound_nhwhitevap_vraa)
table(pl_annex_var_0713$underbound_blackvap_3pct)
table(pl_annex_var_0713$underbound_hispvap_3pct)
table(pl_annex_var_0713$underbound_asianvap_3pct)
table(pl_annex_var_0713$underbound_nativevap_3pct)
table(pl_annex_var_0713$underbound_othervap_3pct)
table(pl_annex_var_0713$underbound_nbmin_3pct)
table(pl_annex_var_0713$underbound_nhwhitevap_3pct)
table(pl_annex_var_0713$underbound_blackvap_hpct)
table(pl_annex_var_0713$underbound_hispvap_hpct)
table(pl_annex_var_0713$underbound_asianvap_hpct)
table(pl_annex_var_0713$underbound_nativevap_hpct)
table(pl_annex_var_0713$underbound_othervap_hpct)
table(pl_annex_var_0713$underbound_nbminvap_hpct)
table(pl_annex_var_0713$underbound_nhwhitevap_hpct)
table(pl_annex_var_0713$underbound_blackvap_10pct)
table(pl_annex_var_0713$underbound_hispvap_10pct)
table(pl_annex_var_0713$underbound_asianvap_10pct)
table(pl_annex_var_0713$underbound_nativevap_10pct)
table(pl_annex_var_0713$underbound_othervap_10pct)
table(pl_annex_var_0713$underbound_nbminvap_10pct)
table(pl_annex_var_0713$underbound_nhwhitevap_10pct)
table(pl_annex_var_0713$underbound_black)
table(pl_annex_var_0713$underbound_hisp)
table(pl_annex_var_0713$underbound_asian)
table(pl_annex_var_0713$underbound_native)
table(pl_annex_var_0713$underbound_other)
table(pl_annex_var_0713$underbound_nbmin)
table(pl_annex_var_0713$underbound_nhwhite)
table(pl_annex_var_0713$underbound_black_vraa)
table(pl_annex_var_0713$underbound_hisp_vraa)
table(pl_annex_var_0713$underbound_asian_vraa)
table(pl_annex_var_0713$underbound_native_vraa)
table(pl_annex_var_0713$underbound_other_vraa)
table(pl_annex_var_0713$underbound_nbmin_vraa)
table(pl_annex_var_0713$underbound_nhwhite_vraa)
table(pl_annex_var_0713$underbound_black_3pct)
table(pl_annex_var_0713$underbound_hisp_3pct)
table(pl_annex_var_0713$underbound_asian_3pct)
table(pl_annex_var_0713$underbound_native_3pct)
table(pl_annex_var_0713$underbound_other_3pct)
table(pl_annex_var_0713$underbound_nbmin_3pct)
table(pl_annex_var_0713$underbound_nhwhite_3pct)
table(pl_annex_var_0713$underbound_black_hpct)
table(pl_annex_var_0713$underbound_hisp_hpct)
table(pl_annex_var_0713$underbound_asian_hpct)
table(pl_annex_var_0713$underbound_native_hpct)
table(pl_annex_var_0713$underbound_other_hpct)
table(pl_annex_var_0713$underbound_nbmin_hpct)
table(pl_annex_var_0713$underbound_nhwhite_hpct)
table(pl_annex_var_0713$underbound_black_10pct)
table(pl_annex_var_0713$underbound_hisp_10pct)
table(pl_annex_var_0713$underbound_asian_10pct)
table(pl_annex_var_0713$underbound_native_10pct)
table(pl_annex_var_0713$underbound_other_10pct)
table(pl_annex_var_0713$underbound_nbmin_10pct)
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
places2013 <- read_csv("acs13_interpolated.csv")
table(pl_annex_var_0713$plid %in% places2013$plid)

pl_annex_var_0713 %<>%
  filter(plid %in% places2013$plid) %>%
  left_join(places2013, by = "plid")

names(pl_annex_var_0713)
names(pl_annex_var_0713) <- gsub("13p", "_p1", names(pl_annex_var_0713))

pl_annex_var_0713 %<>%
  filter(pop_p1 > 0)
sapply(pl_annex_var_0713, function(x) sum(is.na(x)))

write_csv(pl_annex_var_0713, "analyticalfiles/pl_annex_var_0713.csv")
rm(aa0713, pl_annex_var_0713, pl0007, places2013, cdps07)

#repeat for 1420 ####
aa1420 <- read_csv("analyticalfiles/annexedblocks1420dem.csv") 
place_all <- aa1420 %>% 
  mutate(incopp = man + ret) %>%
  group_by(plid) %>%
  dplyr::summarize(pop_total = sum(pop, na.rm = T),
            nhblack_total = sum(nhblack, na.rm = T),
            nhwhite_total = sum(nhwhite, na.rm = T),
            h_total = sum(h, na.rm = T),
            asian_total = sum(asian, na.rm = T), 
            native_total = sum(native, na.rm = T), 
            other_total = sum(other, na.rm = T),
            nbmin_total = sum(nbmin, na.rm = T),
            vap_total = sum(vap, na.rm = T),
            nhblackvap_total = sum(nhbvap),
            nhwhitevap_total = sum(nhwvap),
            hvap_total = sum(hispvap),
            nativevap_total = sum(nativevap, na.rm = T),
            asianvap_total = sum(asianvap, na.rm = T),
            othervap_total = sum(othervap, na.rm = T),
            nbminvap_total = sum(nbminvap, na.rm = T),
            pct_annexed = mean(annexed, na.rm = T),
            njobs_total = sum(njobs14, na.rm = T),
            nhincjobs_total = sum(nhincjobs14, na.rm = T),
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
         pctasian_total = (asian_total/pop_total)*100,
         pctnative_total = (native_total/pop_total)*100,
         pctother_total = (other_total/pop_total)*100,
         pctnbmin_total = (nbmin_total/pop_total)*100,
         pctownerocc_total = (owneroccupied_total/hu_total)*100,
         pctincopp_total = ifelse(nwork_total == 0, 0, (incopp_total/nwork_total)*100),
         pcthincjobs_total = ifelse(njobs_total == 0, 0, (nhincjobs_total/njobs_total)*100),
         pctnhblackvap_total = (nhblackvap_total/vap_total)*100,
         pcthvap_total = (hvap_total/vap_total)*100,
         pctnhwhitevap_total = (nhwhitevap_total/vap_total)*100,
         pctasianvap_total = (asianvap_total/vap_total)*100,
         pctnativevap_total = (nativevap_total/vap_total)*100,
         pctothervap_total = (othervap_total/vap_total)*100,
         pctnbminvap_total = (nbminvap_total/vap_total)*100)
sapply(place_all, function(x) sum(is.na(x)))

place_by_annex <- aa1420 %>%
  mutate(incopp = man + ret
  ) %>%
  filter(annexed==1) %>%
  group_by(plid, annexed) %>%
  dplyr::summarize(pop_total = sum(pop, na.rm = T),
            nhblack_total = sum(nhblack, na.rm = T),
            nhwhite_total = sum(nhwhite, na.rm = T),
            h_total = sum(h, na.rm = T),
            asian_total = sum(asian, na.rm = T),
            native_total = sum(native, na.rm = T),
            other_total = sum(other, na.rm = T),
            nbmin_total = sum(nbmin, na.rm = T),
            njobs_total = sum(njobs14, na.rm = T),
            nhincjobs_total = sum(nhincjobs14, na.rm = T),
            nwork_total = sum(jobs, na.rm = T),
            incopp_total = sum(incopp, na.rm = T),
            hu_total = sum(hu, na.rm = T),
            owneroccupied_total = sum(owneroccupied, na.rm = T),
            vacancy_total = sum(vacancy, na.rm = T),
            vap_total = sum(vap, na.rm = T),
            nhblackvap_total = sum(nhbvap, na.rm = T),
            nhwhitevap_total = sum(nhwvap, na.rm = T),
            hvap_total = sum(hispvap, na.rm = T),
            nativevap_total = sum(nativevap, na.rm = T),
            asianvap_total = sum(asianvap, na.rm = T),
            othervap_total = sum(othervap, na.rm = T),
            nbminvap_total = sum(nbminvap, na.rm = T)) %>%
  ungroup() %>%
  pivot_wider(
    id_cols = plid,
    names_from = annexed,
    values_from = c(pop_total:nbminvap_total)
  ) %>%
  mutate(pctnhblack_total_1 = (nhblack_total_1/pop_total_1)*100,
         pctnhwhite_total_1 = (nhwhite_total_1/pop_total_1)*100,
         pcth_total_1 = (h_total_1/pop_total_1)*100,
         pctnhblackvap_total_1 = (nhblackvap_total_1/vap_total_1)*100,
         pctnhwhitevap_total_1 = (nhwhitevap_total_1/vap_total_1)*100,
         pcthvap_total_1 = (hvap_total_1/vap_total_1)*100,
         pctasian_total_1 = (asian_total_1/pop_total_1)*100,
         pctnative_total_1 = (native_total_1/pop_total_1)*100,
         pctother_total_1 = (other_total_1/pop_total_1)*100,
         pctnbmin_total_1 = (nbmin_total_1/pop_total_1)*100,
         pctasianvap_total_1 = (asianvap_total_1/vap_total_1)*100,
         pctnativevap_total_1 = (nativevap_total_1/vap_total_1)*100,
         pctothervap_total_1 = (othervap_total_1/vap_total_1)*100,
         pctnbminvap_total_1 = (nbminvap_total_1/vap_total_1)*100,
         pctincopp_total_1 = ifelse(nwork_total_1 == 0, 0, (incopp_total_1/nwork_total_1)*100),
         pcthincjobs_total_1 = ifelse(njobs_total_1 == 0, 0, (nhincjobs_total_1/njobs_total_1)*100),
         pctownerocc_total_1 = (owneroccupied_total_1/hu_total_1)*100) %>%
  filter(pop_total_1 > 1 & hu_total_1 > 1)

pl_annex_var_1420 <- left_join(
  place_all, place_by_annex, 
  by = "plid"
) %>%
  mutate(annexing = ifelse(!is.na(pop_total_1), 1, 0))
table(pl_annex_var_1420$annexing)

# add vra indicator 
places_vra <- aa1420 %>%
  group_by(plid) %>%
  dplyr::summarize(vra = mean(vra, na.rm = T)) %>%
  ungroup() %>%
  mutate(vra = ifelse(vra > 0, 1, 0))

pl_annex_var_1420 %<>%
  left_join(places_vra, by = "plid")

places_vra_1420 <- unique(places_vra$plid[places_vra$vra==1])

# add place-level data for race and vap 
# vra violation = if pctwhite after annex > pctwhite before annex compared to
# what would have been, i.e. pctwhite given growth rate by 2013
# or if pctblack after annex < pctblack before annex compared to what would 
# have been, i.e. pctwhite given growth rate by 2013
# or if pcth after annex < pcth before annex compared to what would have been,
# i.e. pcth given growth rate by 2013 
pl0714 <- read_csv("pl0714_var.csv")
summary(pl0714$popgrowth)
table(pl_annex_var_1420$plid %in% pl0714$plid) #4614 false

cdps14 <- read_csv("places2014_cleaned.csv") %>% # want to know which places are CDPs--they do not annex
  select(Geo_NAME, plid) %>%
  mutate(cdp = ifelse(grepl("CDP|cdp", Geo_NAME), 1, 0)) %>%
  filter(cdp==1)

pl_annex_var_1420 %<>%
  filter(plid %in% pl0714$plid & !(plid %in% cdps14$plid)) %>%
  left_join(pl0714, by = "plid") %>%
  mutate(post = 1,
         time = "2014 to 2020",
         pctowneroccupied14p = (owneroccupied14p/hu14p)*100) 

table(pl_annex_var_1420$annexing)

# make underbound variable
# # of black_annexed(2013) + blackvap(2010)/(vap2000 + annexed_vap2013) - blackvap2010/
pl_annex_var_1420 %<>%
  mutate(
    increase_whitevap = ifelse((annexing == 1 & ((((nhwhitevap_total_1 + nhwhitevap14p)/(vap14p + vap_total_1)) - (nhwhitevap14p/vap14p))) > 0), 1, 0),
    underbound_blackvap_vraa = ifelse(
      (annexing == 1 & nhblackvap_total > 1 & increase_whitevap == 1 & vraa == 1 & ((((nhblackvap_total_1 + nhblackvap14p)/(vap_total_1 + vap14p)) - (nhblackvap14p/vap14p))) < -0.03), 1, 0), 
    underbound_hispvap_vraa = ifelse(
      (annexing == 1 & hvap_total > 1 & increase_whitevap == 1 & vraa == 1 & ((((hvap_total_1 + hispvap14p)/(vap_total_1 + vap14p)) - (hispvap14p/vap14p))) < -0.03), 1, 0),
    underbound_asianvap_vraa = ifelse(
      (annexing == 1 & increase_whitevap == 1 & asianvap_total > 1 & vraa == 1 & ((((asianvap_total_1 + asianvap14p)/(vap14p + vap_total_1)) - (asianvap14p/vap14p))) < -0.03), 1, 0), 
    underbound_nativevap_vraa = ifelse(
      (annexing == 1 & increase_whitevap == 1 & nativevap_total > 1 & vraa == 1 & ((((nativevap_total_1 + nativevap14p)/(vap14p + vap_total_1)) - (nativevap14p/vap14p))) < -0.03), 1, 0),
    underbound_othervap_vraa = ifelse(
      (annexing == 1 & increase_whitevap == 1 & othervap_total > 1 & vraa == 1 & ((((othervap_total_1 + othervap14p)/(vap14p + vap_total_1)) - (othervap14p/vap14p))) < -0.03), 1, 0),
    underbound_nbminvap_vraa = ifelse(
      (annexing == 1 & increase_whitevap == 1 & nbminvap_total > 1 & vraa == 1 & ((((nbminvap_total_1 + nbminvap14p)/(vap14p + vap_total_1)) - (nbminvap14p/vap14p))) < -0.03), 1, 0),
    underbound_nhwhitevap_vraa = ifelse(
      (annexing == 1 & nhwhitevap_total > 1 & vraa == 1 & ((((nhwhitevap_total_1 + nhwhitevap14p)/(vap14p + vap_total_1)) - (nhwhitevap14p/vap14p))) < -0.03), 1, 0),
    underbound_blackvap_3pct = ifelse(
      (annexing == 1 & nhblackvap_total > 1 & increase_whitevap == 1 & ((((nhblackvap_total_1 + nhblackvap14p)/(vap_total_1 + vap14p)) - (nhblackvap14p/vap14p))) < -0.03), 1, 0), 
    underbound_hispvap_3pct = ifelse(
      (annexing == 1 & hvap_total > 1 & increase_whitevap == 1 & ((((hvap_total_1 + hispvap14p)/(vap_total_1 + vap14p)) - (hispvap14p/vap14p))) < -0.03), 1, 0),
    underbound_asianvap_3pct = ifelse(
      (annexing == 1 & increase_whitevap == 1 & asianvap_total > 1 & ((((asianvap_total_1 + asianvap14p)/(vap14p + vap_total_1)) - (asianvap14p/vap14p))) < -0.03), 1, 0), 
    underbound_nativevap_3pct = ifelse(
      (annexing == 1 & increase_whitevap == 1 & nativevap_total > 1 & ((((nativevap_total_1 + nativevap14p)/(vap14p + vap_total_1)) - (nativevap14p/vap14p))) < -0.03), 1, 0),
    underbound_othervap_3pct = ifelse(
      (annexing == 1 & increase_whitevap == 1 & othervap_total > 1 & ((((othervap_total_1 + othervap14p)/(vap14p + vap_total_1)) - (othervap14p/vap14p))) < -0.03), 1, 0),
    underbound_nbminvap_3pct = ifelse(
      (annexing == 1 & increase_whitevap == 1 & nbminvap_total > 1 & ((((nbminvap_total_1 + nbminvap14p)/(vap14p + vap_total_1)) - (nbminvap14p/vap14p))) < -0.03), 1, 0),
    underbound_nhwhitevap_3pct = ifelse(
      (annexing == 1 & nhwhitevap_total > 1 & ((((nhwhitevap_total_1 + nhwhitevap14p)/(vap14p + vap_total_1)) - (nhwhitevap14p/vap14p))) < -0.03), 1, 0),
    underbound_blackvap_hpct = ifelse(
      (annexing == 1 & nhblackvap_total > 1 & increase_whitevap == 1 & ((((nhblackvap_total_1 + nhblackvap14p)/(vap_total_1 + vap14p)) - (nhblackvap14p/vap14p))) < -0.005), 1, 0), 
    underbound_hispvap_hpct = ifelse(
      (annexing == 1 & hvap_total > 1 & increase_whitevap == 1 & ((((hvap_total_1 + hispvap14p)/(vap_total_1 + vap14p)) - (hispvap14p/vap14p))) < -0.005), 1, 0),
    underbound_asianvap_hpct = ifelse(
      (annexing == 1 & increase_whitevap == 1 & asianvap_total > 1 & ((((asianvap_total_1 + asianvap14p)/(vap14p + vap_total_1)) - (asianvap14p/vap14p))) < -0.005), 1, 0), 
    underbound_nativevap_hpct = ifelse(
      (annexing == 1 & increase_whitevap == 1 & nativevap_total > 1 & ((((nativevap_total_1 + nativevap14p)/(vap14p + vap_total_1)) - (nativevap14p/vap14p))) < -0.005), 1, 0),
    underbound_othervap_hpct = ifelse(
      (annexing == 1 & increase_whitevap == 1 & othervap_total > 1 & ((((othervap_total_1 + othervap14p)/(vap14p + vap_total_1)) - (othervap14p/vap14p))) < -0.005), 1, 0),
    underbound_nbminvap_hpct = ifelse(
      (annexing == 1 & increase_whitevap == 1 & nbminvap_total > 1 & ((((nbminvap_total_1 + nbminvap14p)/(vap14p + vap_total_1)) - (nbminvap14p/vap14p))) < -0.005), 1, 0),
    underbound_nhwhitevap_hpct = ifelse(
      (annexing == 1 & nhwhitevap_total > 1 & ((((nhwhitevap_total_1 + nhwhitevap14p)/(vap14p + vap_total_1)) - (nhwhitevap14p/vap14p))) < -0.005), 1, 0),
    underbound_blackvap_10pct = ifelse(
      (annexing == 1 & nhblackvap_total > 1 & increase_whitevap == 1 & ((((nhblackvap_total_1 + nhblackvap14p)/(vap_total_1 + vap14p)) - (nhblackvap14p/vap14p))) < -0.1), 1, 0), 
    underbound_hispvap_10pct = ifelse(
      (annexing == 1 & hvap_total >1 & increase_whitevap == 1 & ((((hvap_total_1 + hispvap14p)/(vap_total_1 + vap14p)) - (hispvap14p/vap14p))) < -0.1), 1, 0),
    underbound_asianvap_10pct = ifelse(
      (annexing == 1 & increase_whitevap == 1 & asianvap_total > 1 & ((((asianvap_total_1 + asianvap14p)/(vap14p + vap_total_1)) - (asianvap14p/vap14p))) < -0.1), 1, 0), 
    underbound_nativevap_10pct = ifelse(
      (annexing == 1 & increase_whitevap == 1 & nativevap_total > 1 & ((((nativevap_total_1 + nativevap14p)/(vap14p + vap_total_1)) - (nativevap14p/vap14p))) < -0.1), 1, 0),
    underbound_othervap_10pct = ifelse(
      (annexing == 1 & increase_whitevap == 1 & othervap_total > 1 & ((((othervap_total_1 + othervap14p)/(vap14p + vap_total_1)) - (othervap14p/vap14p))) < -0.1), 1, 0),
    underbound_nbminvap_10pct = ifelse(
      (annexing == 1 & increase_whitevap == 1 & nbminvap_total > 1 & ((((nbminvap_total_1 + nbminvap14p)/(vap14p + vap_total_1)) - (nbminvap14p/vap14p))) < -0.1), 1, 0),
    underbound_nhwhitevap_10pct = ifelse(
      (annexing == 1 & nhwhitevap_total > 1 & ((((nhwhitevap_total_1 + nhwhitevap14p)/(vap14p + vap_total_1)) - (nhwhitevap14p/vap14p))) < -0.1), 1, 0),
    underbound_blackvap = ifelse(
      (annexing == 1 & nhblackvap_total > 1 & increase_whitevap == 1 & ((((nhblackvap_total_1 + nhblackvap14p)/(vap_total_1 + vap14p)) < (nhblackvap14p/vap14p)))), 1, 0), 
    underbound_hispvap = ifelse(
      (annexing == 1 & hvap_total > 1 & increase_whitevap == 1 & ((((hvap_total_1 + hispvap14p)/(vap_total_1 + vap14p)) < (hispvap14p/vap14p)))), 1, 0),
    underbound_asianvap = ifelse(
      (annexing == 1 & increase_whitevap == 1 & asianvap_total > 1 & ((((asianvap_total_1 + asianvap14p)/(vap14p + vap_total_1)) < (asianvap14p/vap14p)))), 1, 0), 
    underbound_nativevap = ifelse(
      (annexing == 1 & increase_whitevap == 1 & nativevap_total > 1 & ((((nativevap_total_1 + nativevap14p)/(vap14p + vap_total_1)) < (nativevap14p/vap14p)))), 1, 0),
    underbound_othervap = ifelse(
      (annexing == 1 & increase_whitevap == 1 & othervap_total > 1 & ((((othervap_total_1 + othervap14p)/(vap14p + vap_total_1)) < (othervap14p/vap14p)))), 1, 0),
    underbound_nbminvap = ifelse(
      (annexing == 1 & increase_whitevap == 1 & nbminvap_total > 1 & ((((nbminvap_total_1 + nbminvap14p)/(vap14p + vap_total_1)) < (nbminvap14p/vap14p)))), 1, 0),
    underbound_nhwhitevap = ifelse(
      (annexing == 1 & nhwhitevap_total > 1 & ((((nhwhitevap_total_1 + nhwhitevap14p)/(vap14p + vap_total_1)) < (nhwhitevap14p/vap14p)))), 1, 0)
  )

# not about VAP 
pl_annex_var_1420 %<>%
  mutate(
    increase_white = ifelse((annexing == 1 & ((((nhwhite_total_1 + nhwhite14p)/(pop14p + pop_total_1)) - (nhwhite14p/pop14p))) > 0), 1, 0),
    underbound_black_vraa = ifelse(
      (annexing == 1 & nhblack_total > 1 & increase_white == 1 & vraa == 1 & ((((nhblack_total_1 + nhblack14p)/(pop_total_1 + pop14p)) - (nhblack14p/pop14p))) < -0.03), 1, 0), 
    underbound_hisp_vraa = ifelse(
      (annexing == 1 & h_total > 1 & increase_white == 1 & vraa == 1 & ((((h_total_1 + h14p)/(pop_total_1 + pop14p)) - (h14p/pop14p))) < -0.03), 1, 0),
    underbound_asian_vraa = ifelse(
      (annexing == 1 & increase_white == 1 & vraa == 1 & asian_total > 1 & ((((asian_total_1 + asian14p)/(pop14p + pop_total_1)) - (asian14p/pop14p))) < -0.03), 1, 0),
    underbound_native_vraa = ifelse(
      (annexing == 1 & increase_white == 1 & vraa == 1 & native_total > 1 & ((((native_total_1 + native14p)/(pop14p + pop_total_1)) - (native14p/pop14p))) < -0.03), 1, 0),
    underbound_other_vraa = ifelse(
      (annexing == 1 & increase_white == 1 & vraa == 1 & other_total > 1 & ((((other_total_1 + other14p)/(pop14p + pop_total_1)) - (other14p/pop14p))) < -0.03), 1, 0),
    underbound_nbmin_vraa = ifelse(
      (annexing == 1 & increase_white == 1 & vraa == 1 & nbmin_total > 1 & ((((nbmin_total_1 + nbmin14p)/(pop14p + pop_total_1)) - (nbmin14p/pop14p))) < -0.03), 1, 0),
    underbound_nhwhite_vraa = ifelse(
      (annexing == 1 & nhwhite_total > 1 & vraa == 1 & ((((nhwhite_total_1 + nhwhite14p)/(pop14p + pop_total_1)) - (nhwhite14p/pop14p))) < -0.03), 1, 0),
    underbound_black_3pct = ifelse(
      (annexing == 1 & nhblack_total > 1 & increase_white == 1 & ((((nhblack_total_1 + nhblack14p)/(pop_total_1 + pop14p)) - (nhblack14p/pop14p))) < -0.03), 1, 0), 
    underbound_hisp_3pct = ifelse(
      (annexing == 1 & h_total > 1 & increase_white == 1 & ((((h_total_1 + h14p)/(pop_total_1 + pop14p)) - (h14p/pop14p))) < -0.03), 1, 0),
    underbound_asian_3pct = ifelse(
      (annexing == 1 & increase_white == 1 & asian_total > 1 & ((((asian_total_1 + asian14p)/(pop14p + pop_total_1)) - (asian14p/pop14p))) < -0.03), 1, 0),
    underbound_native_3pct = ifelse(
      (annexing == 1 & increase_white == 1 & native_total > 1 & ((((native_total_1 + native14p)/(pop14p + pop_total_1)) - (native14p/pop14p))) < -0.03), 1, 0),
    underbound_other_3pct = ifelse(
      (annexing == 1 & increase_white == 1 & other_total > 1 & ((((other_total_1 + other14p)/(pop14p + pop_total_1)) - (other14p/pop14p))) < -0.03), 1, 0),
    underbound_nbmin_3pct = ifelse(
      (annexing == 1 & increase_white == 1 & nbmin_total > 1 & ((((nbmin_total_1 + nbmin14p)/(pop14p + pop_total_1)) - (nbmin14p/pop14p))) < -0.03), 1, 0),
    underbound_nhwhite_3pct = ifelse(
      (annexing == 1 & nhwhite_total > 1 & ((((nhwhite_total_1 + nhwhite14p)/(pop14p + pop_total_1)) - (nhwhite14p/pop14p))) < -0.03), 1, 0),
    underbound_black_hpct = ifelse(
      (annexing == 1 & nhblack_total > 1 & increase_white == 1 & ((((nhblack_total_1 + nhblack14p)/(pop_total_1 + pop14p)) - (nhblack14p/pop14p))) < -0.005), 1, 0), 
    underbound_hisp_hpct = ifelse(
      (annexing == 1 & h_total > 1 & increase_white == 1 & ((((h_total_1 + h14p)/(pop_total_1 + pop14p)) - (h14p/pop14p))) < -0.005), 1, 0),
    underbound_asian_hpct = ifelse(
      (annexing == 1 & increase_white == 1 & asian_total > 1 & ((((asian_total_1 + asian14p)/(pop14p + pop_total_1)) - (asian14p/pop14p))) < -0.005), 1, 0),
    underbound_native_hpct = ifelse(
      (annexing == 1 & increase_white == 1 & native_total > 1 & ((((native_total_1 + native14p)/(pop14p + pop_total_1)) - (native14p/pop14p))) < -0.005), 1, 0),
    underbound_other_hpct = ifelse(
      (annexing == 1 & increase_white == 1 & other_total > 1 & ((((other_total_1 + other14p)/(pop14p + pop_total_1)) - (other14p/pop14p))) < -0.005), 1, 0),
    underbound_nbmin_hpct = ifelse(
      (annexing == 1 & increase_white == 1 & nbmin_total > 1 & ((((nbmin_total_1 + nbmin14p)/(pop14p + pop_total_1)) - (nbmin14p/pop14p))) < -0.005), 1, 0),
    underbound_nhwhite_hpct = ifelse(
      (annexing == 1 & nhwhite_total > 1 & ((((nhwhite_total_1 + nhwhite14p)/(pop14p + pop_total_1)) - (nhwhite14p/pop14p))) < -0.005), 1, 0),
    underbound_black_10pct = ifelse(
      (annexing == 1 & nhblack_total > 1 & increase_white == 1 & ((((nhblack_total_1 + nhblack14p)/(pop_total_1 + pop14p)) - (nhblack14p/pop14p))) < -0.1), 1, 0), 
    underbound_hisp_10pct = ifelse(
      (annexing == 1 & h_total > 1 & increase_white == 1 & ((((h_total_1 + h14p)/(pop_total_1 + pop14p)) - (h14p/pop14p))) < -0.1), 1, 0),
    underbound_asian_10pct = ifelse(
      (annexing == 1 & increase_white == 1 & asian_total > 1 & ((((asian_total_1 + asian14p)/(pop14p + pop_total_1)) - (asian14p/pop14p))) < -0.1), 1, 0),
    underbound_native_10pct = ifelse(
      (annexing == 1 & increase_white == 1 & native_total > 1 & ((((native_total_1 + native14p)/(pop14p + pop_total_1)) - (native14p/pop14p))) < -0.1), 1, 0),
    underbound_other_10pct = ifelse(
      (annexing == 1 & increase_white == 1 & other_total > 1 & ((((other_total_1 + other14p)/(pop14p + pop_total_1)) - (other14p/pop14p))) < -0.1), 1, 0),
    underbound_nbmin_10pct = ifelse(
      (annexing == 1 & increase_white == 1 & nbmin_total > 1 & ((((nbmin_total_1 + nbmin14p)/(pop14p + pop_total_1)) - (nbmin14p/pop14p))) < -0.1), 1, 0),
    underbound_nhwhite_10pct = ifelse(
      (annexing == 1 & nhwhite_total > 1 & ((((nhwhite_total_1 + nhwhite14p)/(pop14p + pop_total_1)) - (nhwhite14p/pop14p))) < -0.1), 1, 0),
    underbound_black = ifelse(
      (annexing == 1 & nhblack_total > 1 & increase_white == 1 & ((((nhblack_total_1 + nhblack14p)/(pop_total_1 + pop14p)) < (nhblack14p/pop14p)))), 1, 0), 
    underbound_hisp = ifelse(
      (annexing == 1 & h_total > 1 & increase_white == 1 & ((((h_total_1 + h14p)/(pop_total_1 + pop14p)) < (h14p/pop14p)))), 1, 0),
    underbound_asian = ifelse(
      (annexing == 1 & increase_white == 1 & asian_total > 1 & ((((asian_total_1 + asian14p)/(pop14p + pop_total_1)) < (asian14p/pop14p)))), 1, 0),
    underbound_native = ifelse(
      (annexing == 1 & increase_white == 1 & native_total > 1 & ((((native_total_1 + native14p)/(pop14p + pop_total_1)) < (native14p/pop14p)))), 1, 0),
    underbound_other = ifelse(
      (annexing == 1 & increase_white == 1 & other_total > 1 & ((((other_total_1 + other14p)/(pop14p + pop_total_1)) < (other14p/pop14p)))), 1, 0),
    underbound_nbmin = ifelse(
      (annexing == 1 & increase_white == 1 & nbmin_total > 1 & ((((nbmin_total_1 + nbmin14p)/(pop14p + pop_total_1)) < (nbmin14p/pop14p)))), 1, 0),
    underbound_nhwhite = ifelse(
      (annexing == 1 & nhwhite_total > 1 & ((((nhwhite_total_1 + nhwhite14p)/(pop14p + pop_total_1)) < (nhwhite14p/pop14p)))), 1, 0)
  )

# compare SES
pl_annex_var_1420 %<>% 
  mutate(
    property_inc = ifelse(
      (annexing == 1 & owneroccupied_total > 1 & (((owneroccupied_total_1 + owneroccupied14p)/(hu14p + hu_total_1)) > (hu14p + hu_total_1))), 1, 0),
    hincjobs = ifelse(
      (annexing == 1 & nhincjobs_total > 1 & (((nhincjobs_total_1 + overlodesthresh14p)/(emp14p + njobs_total_1)) > (emp14p + njobs_total_1))), 1, 0)
  )

pl_annex_var_1420 %<>%
  filter(pop_total > 1 & hu_total > 1 & pop14p > 0) 

table(pl_annex_var_1420$underbound_blackvap)
table(pl_annex_var_1420$underbound_hispvap)
table(pl_annex_var_1420$underbound_asianvap)
table(pl_annex_var_1420$underbound_nativevap)
table(pl_annex_var_1420$underbound_othervap)
table(pl_annex_var_1420$underbound_nbminvap)
table(pl_annex_var_1420$underbound_nhwhitevap)
table(pl_annex_var_1420$underbound_blackvap_vraa)
table(pl_annex_var_1420$underbound_hispvap_vraa)
table(pl_annex_var_1420$underbound_asianvap_vraa)
table(pl_annex_var_1420$underbound_nativevap_vraa)
table(pl_annex_var_1420$underbound_othervap_vraa)
table(pl_annex_var_1420$underbound_nbminvap_vraa)
table(pl_annex_var_1420$underbound_nhwhitevap_vraa)
table(pl_annex_var_1420$underbound_blackvap_3pct)
table(pl_annex_var_1420$underbound_hispvap_3pct)
table(pl_annex_var_1420$underbound_asianvap_3pct)
table(pl_annex_var_1420$underbound_nativevap_3pct)
table(pl_annex_var_1420$underbound_othervap_3pct)
table(pl_annex_var_1420$underbound_nbmin_3pct)
table(pl_annex_var_1420$underbound_nhwhitevap_3pct)
table(pl_annex_var_1420$underbound_blackvap_hpct)
table(pl_annex_var_1420$underbound_hispvap_hpct)
table(pl_annex_var_1420$underbound_asianvap_hpct)
table(pl_annex_var_1420$underbound_nativevap_hpct)
table(pl_annex_var_1420$underbound_othervap_hpct)
table(pl_annex_var_1420$underbound_nbminvap_hpct)
table(pl_annex_var_1420$underbound_nhwhitevap_hpct)
table(pl_annex_var_1420$underbound_blackvap_10pct)
table(pl_annex_var_1420$underbound_hispvap_10pct)
table(pl_annex_var_1420$underbound_asianvap_10pct)
table(pl_annex_var_1420$underbound_nativevap_10pct)
table(pl_annex_var_1420$underbound_othervap_10pct)
table(pl_annex_var_1420$underbound_nbminvap_10pct)
table(pl_annex_var_1420$underbound_nhwhitevap_10pct)
table(pl_annex_var_1420$underbound_black)
table(pl_annex_var_1420$underbound_hisp)
table(pl_annex_var_1420$underbound_asian)
table(pl_annex_var_1420$underbound_native)
table(pl_annex_var_1420$underbound_other)
table(pl_annex_var_1420$underbound_nbmin)
table(pl_annex_var_1420$underbound_nhwhite)
table(pl_annex_var_1420$underbound_black_vraa)
table(pl_annex_var_1420$underbound_hisp_vraa)
table(pl_annex_var_1420$underbound_asian_vraa)
table(pl_annex_var_1420$underbound_native_vraa)
table(pl_annex_var_1420$underbound_other_vraa)
table(pl_annex_var_1420$underbound_nbmin_vraa)
table(pl_annex_var_1420$underbound_nhwhite_vraa)
table(pl_annex_var_1420$underbound_black_3pct)
table(pl_annex_var_1420$underbound_hisp_3pct)
table(pl_annex_var_1420$underbound_asian_3pct)
table(pl_annex_var_1420$underbound_native_3pct)
table(pl_annex_var_1420$underbound_other_3pct)
table(pl_annex_var_1420$underbound_nbmin_3pct)
table(pl_annex_var_1420$underbound_nhwhite_3pct)
table(pl_annex_var_1420$underbound_black_hpct)
table(pl_annex_var_1420$underbound_hisp_hpct)
table(pl_annex_var_1420$underbound_asian_hpct)
table(pl_annex_var_1420$underbound_native_hpct)
table(pl_annex_var_1420$underbound_other_hpct)
table(pl_annex_var_1420$underbound_nbmin_hpct)
table(pl_annex_var_1420$underbound_nhwhite_hpct)
table(pl_annex_var_1420$underbound_black_10pct)
table(pl_annex_var_1420$underbound_hisp_10pct)
table(pl_annex_var_1420$underbound_asian_10pct)
table(pl_annex_var_1420$underbound_native_10pct)
table(pl_annex_var_1420$underbound_other_10pct)
table(pl_annex_var_1420$underbound_nbmin_10pct)
table(pl_annex_var_1420$underbound_nhwhite_10pct)
table(pl_annex_var_1420$property_inc)
table(pl_annex_var_1420$hincjobs)

names(pl_annex_var_1420)
pl_annex_var_1420 %<>%
  select(-ends_with("07p"))
names(pl_annex_var_1420)

names(pl_annex_var_1420) <- gsub("14p", "_p0", names(pl_annex_var_1420))
names(pl_annex_var_1420)

# merge in 2013 data 
places2020 <- read_csv("places2020_interpolated.csv")
table(pl_annex_var_1420$plid %in% places2020$plid)

pl_annex_var_1420 %<>%
  filter(plid %in% places2020$plid) %>%
  left_join(places2020, by = "plid")

names(pl_annex_var_1420)
names(pl_annex_var_1420) <- gsub("20p", "_p1", names(pl_annex_var_1420))

pl_annex_var_1420 %<>%
  filter(pop_p1 >0)
sapply(pl_annex_var_1420, function(x) sum(is.na(x)))

write_csv(pl_annex_var_1420, "analyticalfiles/pl_annex_var_1420.csv")
rm(aa1420, pl_annex_var_1420, pl0714, places2020, cdps14)

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
            nhblack_total = sum(nhblack, na.rm = T),
            nhwhite_total = sum(nhwhite, na.rm = T),
            h_total = sum(h, na.rm = T),
            asian_total = sum(asian, na.rm = T), 
            native_total = sum(native, na.rm = T), 
            other_total = sum(other, na.rm = T),
            nbmin_total = sum(nbmin, na.rm = T),
            vap_total = sum(vap, na.rm = T),
            nhblackvap_total = sum(nhbvap),
            nhwhitevap_total = sum(nhwvap),
            hvap_total = sum(hispvap),
            nativevap_total = sum(nativevap, na.rm = T),
            asianvap_total = sum(asianvap, na.rm = T),
            othervap_total = sum(othervap, na.rm = T),
            nbminvap_total = sum(nbminvap, na.rm = T),
            pct_annexed = mean(annexed, na.rm = T),
            njobs_total = sum(njobs00, na.rm = T),
            nhincjobs_total = sum(nhincjobs00, na.rm = T),
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
         pctasian_total = (asian_total/pop_total)*100,
         pctnative_total = (native_total/pop_total)*100,
         pctother_total = (other_total/pop_total)*100,
         pctnbmin_total = (nbmin_total/pop_total)*100,
         pctownerocc_total = (owneroccupied_total/hu_total)*100,
         pctincopp_total = ifelse(nwork_total == 0, 0, (incopp_total/nwork_total)*100),
         pcthincjobs_total = ifelse(njobs_total == 0, 0, (nhincjobs_total/njobs_total)*100),
         pctnhblackvap_total = (nhblackvap_total/vap_total)*100,
         pcthvap_total = (hvap_total/vap_total)*100,
         pctnhwhitevap_total = (nhwhitevap_total/vap_total)*100,
         pctasianvap_total = (asianvap_total/vap_total)*100,
         pctnativevap_total = (nativevap_total/vap_total)*100,
         pctothervap_total = (othervap_total/vap_total)*100,
         pctnbminvap_total = (nbminvap_total/vap_total)*100)

place_by_annex <- aa0007 %>%
  mutate(incopp = man + ret
  ) %>%
  filter(annexed==1) %>%
  group_by(plid, annexed) %>%
  dplyr::summarize(pop_total = sum(pop, na.rm = T),
            nhblack_total = sum(nhblack, na.rm = T),
            nhwhite_total = sum(nhwhite, na.rm = T),
            h_total = sum(h, na.rm = T),
            asian_total = sum(asian, na.rm = T),
            native_total = sum(native, na.rm = T),
            other_total = sum(other, na.rm = T),
            nbmin_total = sum(nbmin, na.rm = T),
            njobs_total = sum(njobs00, na.rm = T),
            nhincjobs_total = sum(nhincjobs00, na.rm = T),
            nwork_total = sum(jobs, na.rm = T),
            incopp_total = sum(incopp, na.rm = T),
            hu_total = sum(hu, na.rm = T),
            owneroccupied_total = sum(owneroccupied, na.rm = T),
            vacancy_total = sum(vacancy, na.rm = T),
            vap_total = sum(vap, na.rm = T),
            nhblackvap_total = sum(nhbvap, na.rm = T),
            nhwhitevap_total = sum(nhwvap, na.rm = T),
            hvap_total = sum(hispvap, na.rm = T),
            nativevap_total = sum(nativevap, na.rm = T),
            asianvap_total = sum(asianvap, na.rm = T),
            othervap_total = sum(othervap, na.rm = T),
            nbminvap_total = sum(nbminvap, na.rm = T)) %>%
  ungroup() %>%
  pivot_wider(
    id_cols = plid,
    names_from = annexed,
    values_from = c(pop_total:nbminvap_total)
  ) %>%
  mutate(pctnhblack_total_1 = (nhblack_total_1/pop_total_1)*100,
         pctnhwhite_total_1 = (nhwhite_total_1/pop_total_1)*100,
         pcth_total_1 = (h_total_1/pop_total_1)*100,
         pctnhblackvap_total_1 = (nhblackvap_total_1/vap_total_1)*100,
         pctnhwhitevap_total_1 = (nhwhitevap_total_1/vap_total_1)*100,
         pcthvap_total_1 = (hvap_total_1/vap_total_1)*100,
         pctasian_total_1 = (asian_total_1/pop_total_1)*100,
         pctnative_total_1 = (native_total_1/pop_total_1)*100,
         pctother_total_1 = (other_total_1/pop_total_1)*100,
         pctnbmin_total_1 = (nbmin_total_1/pop_total_1)*100,
         pctasianvap_total_1 = (asianvap_total_1/vap_total_1)*100,
         pctnativevap_total_1 = (nativevap_total_1/vap_total_1)*100,
         pctothervap_total_1 = (othervap_total_1/vap_total_1)*100,
         pctnbminvap_total_1 = (nbminvap_total_1/vap_total_1)*100,
         pctincopp_total_1 = ifelse(nwork_total_1 == 0, 0, (incopp_total_1/nwork_total_1)*100),
         pcthincjobs_total_1 = ifelse(njobs_total_1 == 0, 0, (nhincjobs_total_1/njobs_total_1)*100),
         pctownerocc_total_1 = (owneroccupied_total_1/hu_total_1)*100) %>%
  filter(pop_total_1 > 1 & hu_total_1 > 1)

pl_annex_var_0007 <- left_join(
  place_all, place_by_annex, 
  by = "plid"
) %>%
  mutate(annexing = ifelse(!is.na(pop_total_1), 1, 0))
table(pl_annex_var_0007$annexing)

# add vra indicator 
places_vra <- aa0007 %>%
  group_by(plid) %>%
  dplyr::summarize(vra = mean(vra, na.rm = T)) %>%
  ungroup() %>%
  mutate(vra = ifelse(vra > 0, 1, 0))

pl_annex_var_0007 %<>%
  left_join(places_vra, by = "plid")

places_vra_0007 <- unique(places_vra$plid[places_vra$vra==1])

# add place-level data for race and vap 
# vra violation = if pctwhite after annex > pctwhite before annex compared to
# what would have been, i.e. pctwhite given growth rate by 2013
# or if pctblack after annex < pctblack before annex compared to what would 
# have been, i.e. pctwhite given growth rate by 2013
# or if pcth after annex < pcth before annex compared to what would have been,
# i.e. pcth given growth rate by 2013 
pl9000 <- read_csv("pl9000_var.csv")

table(pl_annex_var_0007$plid %in% pl9000$plid) #2078 false

cdps00 <- read_csv("pl2000_cleaned.csv") %>% # want to know which places are CDPs--they do not annex
  select(Geo_NAME, plid) %>%
  mutate(cdp = ifelse(grepl("CDP|cdp", Geo_NAME), 1, 0)) %>%
  filter(cdp==1)

pl_annex_var_0007 %<>%
  filter(plid %in% pl9000$plid & !(plid %in% cdps00$plid)) %>%
  left_join(pl9000, by = "plid") %>%
  mutate(post = -1,
         time = "2000 to 2007",
         pctowneroccupied00p = (owneroccupied00p/hu00p)*100) 

table(pl_annex_var_0007$annexing)

# make underbound variable
# # of black_annexed(2013) + blackvap(2010)/(vap2000 + annexed_vap2013) - blackvap2010/
pl_annex_var_0007 %<>%
  mutate(
    increase_whitevap = ifelse((annexing == 1 & ((((nhwhitevap_total_1 + nhwhitevap00p)/(vap00p + vap_total_1)) - (nhwhitevap00p/vap00p))) > 0), 1, 0),
    underbound_blackvap_vraa = ifelse(
      (annexing == 1 & nhblackvap_total > 1 & increase_whitevap == 1 & vraa == 1 & ((((nhblackvap_total_1 + nhblackvap00p)/(vap_total_1 + vap00p)) - (nhblackvap00p/vap00p))) < -0.03), 1, 0), 
    underbound_hispvap_vraa = ifelse(
      (annexing == 1 & hvap_total > 1 & increase_whitevap == 1 & vraa == 1 & ((((hvap_total_1 + hispvap00p)/(vap_total_1 + vap00p)) - (hispvap00p/vap00p))) < -0.03), 1, 0),
    underbound_asianvap_vraa = ifelse(
      (annexing == 1 & increase_whitevap == 1 & asianvap_total > 1 & vraa == 1 & ((((asianvap_total_1 + asianvap00p)/(vap00p + vap_total_1)) - (asianvap00p/vap00p))) < -0.03), 1, 0), 
    underbound_nativevap_vraa = ifelse(
      (annexing == 1 & increase_whitevap == 1 & nativevap_total > 1 & vraa == 1 & ((((nativevap_total_1 + nativevap00p)/(vap00p + vap_total_1)) - (nativevap00p/vap00p))) < -0.03), 1, 0),
    underbound_othervap_vraa = ifelse(
      (annexing == 1 & increase_whitevap == 1 & othervap_total > 1 & vraa == 1 & ((((othervap_total_1 + othervap00p)/(vap00p + vap_total_1)) - (othervap00p/vap00p))) < -0.03), 1, 0),
    underbound_nbminvap_vraa = ifelse(
      (annexing == 1 & increase_whitevap == 1 & nbminvap_total > 1 & vraa == 1 & ((((nbminvap_total_1 + nbminvap00p)/(vap00p + vap_total_1)) - (nbminvap00p/vap00p))) < -0.03), 1, 0),
    underbound_nhwhitevap_vraa = ifelse(
      (annexing == 1 & nhwhitevap_total > 1 & vraa == 1 & ((((nhwhitevap_total_1 + nhwhitevap00p)/(vap00p + vap_total_1)) - (nhwhitevap00p/vap00p))) < -0.03), 1, 0),
    underbound_blackvap_3pct = ifelse(
      (annexing == 1 & nhblackvap_total > 1 & increase_whitevap == 1 & ((((nhblackvap_total_1 + nhblackvap00p)/(vap_total_1 + vap00p)) - (nhblackvap00p/vap00p))) < -0.03), 1, 0), 
    underbound_hispvap_3pct = ifelse(
      (annexing == 1 & hvap_total > 1 & increase_whitevap == 1 & ((((hvap_total_1 + hispvap00p)/(vap_total_1 + vap00p)) - (hispvap00p/vap00p))) < -0.03), 1, 0),
    underbound_asianvap_3pct = ifelse(
      (annexing == 1 & increase_whitevap == 1 & asianvap_total > 1 & ((((asianvap_total_1 + asianvap00p)/(vap00p + vap_total_1)) - (asianvap00p/vap00p))) < -0.03), 1, 0), 
    underbound_nativevap_3pct = ifelse(
      (annexing == 1 & increase_whitevap == 1 & nativevap_total > 1 & ((((nativevap_total_1 + nativevap00p)/(vap00p + vap_total_1)) - (nativevap00p/vap00p))) < -0.03), 1, 0),
    underbound_othervap_3pct = ifelse(
      (annexing == 1 & increase_whitevap == 1 & othervap_total > 1 & ((((othervap_total_1 + othervap00p)/(vap00p + vap_total_1)) - (othervap00p/vap00p))) < -0.03), 1, 0),
    underbound_nbminvap_3pct = ifelse(
      (annexing == 1 & increase_whitevap == 1 & nbminvap_total > 1 & ((((nbminvap_total_1 + nbminvap00p)/(vap00p + vap_total_1)) - (nbminvap00p/vap00p))) < -0.03), 1, 0),
    underbound_nhwhitevap_3pct = ifelse(
      (annexing == 1 & nhwhitevap_total > 1 & ((((nhwhitevap_total_1 + nhwhitevap00p)/(vap00p + vap_total_1)) - (nhwhitevap00p/vap00p))) < -0.03), 1, 0),
    underbound_blackvap_hpct = ifelse(
      (annexing == 1 & nhblackvap_total > 1 & increase_whitevap == 1 & ((((nhblackvap_total_1 + nhblackvap00p)/(vap_total_1 + vap00p)) - (nhblackvap00p/vap00p))) < -0.005), 1, 0), 
    underbound_hispvap_hpct = ifelse(
      (annexing == 1 & hvap_total > 1 & increase_whitevap == 1 & ((((hvap_total_1 + hispvap00p)/(vap_total_1 + vap00p)) - (hispvap00p/vap00p))) < -0.005), 1, 0),
    underbound_asianvap_hpct = ifelse(
      (annexing == 1 & increase_whitevap == 1 & asianvap_total > 1 & ((((asianvap_total_1 + asianvap00p)/(vap00p + vap_total_1)) - (asianvap00p/vap00p))) < -0.005), 1, 0), 
    underbound_nativevap_hpct = ifelse(
      (annexing == 1 & increase_whitevap == 1 & nativevap_total > 1 & ((((nativevap_total_1 + nativevap00p)/(vap00p + vap_total_1)) - (nativevap00p/vap00p))) < -0.005), 1, 0),
    underbound_othervap_hpct = ifelse(
      (annexing == 1 & increase_whitevap == 1 & othervap_total > 1 & ((((othervap_total_1 + othervap00p)/(vap00p + vap_total_1)) - (othervap00p/vap00p))) < -0.005), 1, 0),
    underbound_nbminvap_hpct = ifelse(
      (annexing == 1 & increase_whitevap == 1 & nbminvap_total > 1 & ((((nbminvap_total_1 + nbminvap00p)/(vap00p + vap_total_1)) - (nbminvap00p/vap00p))) < -0.005), 1, 0),
    underbound_nhwhitevap_hpct = ifelse(
      (annexing == 1 & nhwhitevap_total > 1 & ((((nhwhitevap_total_1 + nhwhitevap00p)/(vap00p + vap_total_1)) - (nhwhitevap00p/vap00p))) < -0.005), 1, 0),
    underbound_blackvap_10pct = ifelse(
      (annexing == 1 & nhblackvap_total > 1 & increase_whitevap == 1 & ((((nhblackvap_total_1 + nhblackvap00p)/(vap_total_1 + vap00p)) - (nhblackvap00p/vap00p))) < -0.1), 1, 0), 
    underbound_hispvap_10pct = ifelse(
      (annexing == 1 & hvap_total >1 & increase_whitevap == 1 & ((((hvap_total_1 + hispvap00p)/(vap_total_1 + vap00p)) - (hispvap00p/vap00p))) < -0.1), 1, 0),
    underbound_asianvap_10pct = ifelse(
      (annexing == 1 & increase_whitevap == 1 & asianvap_total > 1 & ((((asianvap_total_1 + asianvap00p)/(vap00p + vap_total_1)) - (asianvap00p/vap00p))) < -0.1), 1, 0), 
    underbound_nativevap_10pct = ifelse(
      (annexing == 1 & increase_whitevap == 1 & nativevap_total > 1 & ((((nativevap_total_1 + nativevap00p)/(vap00p + vap_total_1)) - (nativevap00p/vap00p))) < -0.1), 1, 0),
    underbound_othervap_10pct = ifelse(
      (annexing == 1 & increase_whitevap == 1 & othervap_total > 1 & ((((othervap_total_1 + othervap00p)/(vap00p + vap_total_1)) - (othervap00p/vap00p))) < -0.1), 1, 0),
    underbound_nbminvap_10pct = ifelse(
      (annexing == 1 & increase_whitevap == 1 & nbminvap_total > 1 & ((((nbminvap_total_1 + nbminvap00p)/(vap00p + vap_total_1)) - (nbminvap00p/vap00p))) < -0.1), 1, 0),
    underbound_nhwhitevap_10pct = ifelse(
      (annexing == 1 & nhwhitevap_total > 1 & ((((nhwhitevap_total_1 + nhwhitevap00p)/(vap00p + vap_total_1)) - (nhwhitevap00p/vap00p))) < -0.1), 1, 0),
    underbound_blackvap = ifelse(
      (annexing == 1 & nhblackvap_total > 1 & increase_whitevap == 1 & ((((nhblackvap_total_1 + nhblackvap00p)/(vap_total_1 + vap00p)) < (nhblackvap00p/vap00p)))), 1, 0), 
    underbound_hispvap = ifelse(
      (annexing == 1 & hvap_total > 1 & increase_whitevap == 1 & ((((hvap_total_1 + hispvap00p)/(vap_total_1 + vap00p)) < (hispvap00p/vap00p)))), 1, 0),
    underbound_asianvap = ifelse(
      (annexing == 1 & increase_whitevap == 1 & asianvap_total > 1 & ((((asianvap_total_1 + asianvap00p)/(vap00p + vap_total_1)) < (asianvap00p/vap00p)))), 1, 0), 
    underbound_nativevap = ifelse(
      (annexing == 1 & increase_whitevap == 1 & nativevap_total > 1 & ((((nativevap_total_1 + nativevap00p)/(vap00p + vap_total_1)) < (nativevap00p/vap00p)))), 1, 0),
    underbound_othervap = ifelse(
      (annexing == 1 & increase_whitevap == 1 & othervap_total > 1 & ((((othervap_total_1 + othervap00p)/(vap00p + vap_total_1)) < (othervap00p/vap00p)))), 1, 0),
    underbound_nbminvap = ifelse(
      (annexing == 1 & increase_whitevap == 1 & nbminvap_total > 1 & ((((nbminvap_total_1 + nbminvap00p)/(vap00p + vap_total_1)) < (nbminvap00p/vap00p)))), 1, 0),
    underbound_nhwhitevap = ifelse(
      (annexing == 1 & nhwhitevap_total > 1 & ((((nhwhitevap_total_1 + nhwhitevap00p)/(vap00p + vap_total_1)) < (nhwhitevap00p/vap00p)))), 1, 0)
  )

# not about VAP 
pl_annex_var_0007 %<>%
  mutate(
    increase_white = ifelse((annexing == 1 & ((((nhwhite_total_1 + nhwhite00p)/(pop00p + pop_total_1)) - (nhwhite00p/pop00p))) > 0), 1, 0),
    underbound_black_vraa = ifelse(
      (annexing == 1 & nhblack_total > 1 & increase_white == 1 & vraa == 1 & ((((nhblack_total_1 + nhblack00p)/(pop_total_1 + pop00p)) - (nhblack00p/pop00p))) < -0.03), 1, 0), 
    underbound_hisp_vraa = ifelse(
      (annexing == 1 & h_total > 1 & increase_white == 1 & vraa == 1 & ((((h_total_1 + h00p)/(pop_total_1 + pop00p)) - (h00p/pop00p))) < -0.03), 1, 0),
    underbound_asian_vraa = ifelse(
      (annexing == 1 & increase_white == 1 & vraa == 1 & asian_total > 1 & ((((asian_total_1 + asian00p)/(pop00p + pop_total_1)) - (asian00p/pop00p))) < -0.03), 1, 0),
    underbound_native_vraa = ifelse(
      (annexing == 1 & increase_white == 1 & vraa == 1 & native_total > 1 & ((((native_total_1 + native00p)/(pop00p + pop_total_1)) - (native00p/pop00p))) < -0.03), 1, 0),
    underbound_other_vraa = ifelse(
      (annexing == 1 & increase_white == 1 & vraa == 1 & other_total > 1 & ((((other_total_1 + other00p)/(pop00p + pop_total_1)) - (other00p/pop00p))) < -0.03), 1, 0),
    underbound_nbmin_vraa = ifelse(
      (annexing == 1 & increase_white == 1 & vraa == 1 & nbmin_total > 1 & ((((nbmin_total_1 + nbmin00p)/(pop00p + pop_total_1)) - (nbmin00p/pop00p))) < -0.03), 1, 0),
    underbound_nhwhite_vraa = ifelse(
      (annexing == 1 & nhwhite_total > 1 & vraa == 1 & ((((nhwhite_total_1 + nhwhite00p)/(pop00p + pop_total_1)) - (nhwhite00p/pop00p))) < -0.03), 1, 0),
    underbound_black_3pct = ifelse(
      (annexing == 1 & nhblack_total > 1 & increase_white == 1 & ((((nhblack_total_1 + nhblack00p)/(pop_total_1 + pop00p)) - (nhblack00p/pop00p))) < -0.03), 1, 0), 
    underbound_hisp_3pct = ifelse(
      (annexing == 1 & h_total > 1 & increase_white == 1 & ((((h_total_1 + h00p)/(pop_total_1 + pop00p)) - (h00p/pop00p))) < -0.03), 1, 0),
    underbound_asian_3pct = ifelse(
      (annexing == 1 & increase_white == 1 & asian_total > 1 & ((((asian_total_1 + asian00p)/(pop00p + pop_total_1)) - (asian00p/pop00p))) < -0.03), 1, 0),
    underbound_native_3pct = ifelse(
      (annexing == 1 & increase_white == 1 & native_total > 1 & ((((native_total_1 + native00p)/(pop00p + pop_total_1)) - (native00p/pop00p))) < -0.03), 1, 0),
    underbound_other_3pct = ifelse(
      (annexing == 1 & increase_white == 1 & other_total > 1 & ((((other_total_1 + other00p)/(pop00p + pop_total_1)) - (other00p/pop00p))) < -0.03), 1, 0),
    underbound_nbmin_3pct = ifelse(
      (annexing == 1 & increase_white == 1 & nbmin_total > 1 & ((((nbmin_total_1 + nbmin00p)/(pop00p + pop_total_1)) - (nbmin00p/pop00p))) < -0.03), 1, 0),
    underbound_nhwhite_3pct = ifelse(
      (annexing == 1 & nhwhite_total > 1 & ((((nhwhite_total_1 + nhwhite00p)/(pop00p + pop_total_1)) - (nhwhite00p/pop00p))) < -0.03), 1, 0),
    underbound_black_hpct = ifelse(
      (annexing == 1 & nhblack_total > 1 & increase_white == 1 & ((((nhblack_total_1 + nhblack00p)/(pop_total_1 + pop00p)) - (nhblack00p/pop00p))) < -0.005), 1, 0), 
    underbound_hisp_hpct = ifelse(
      (annexing == 1 & h_total > 1 & increase_white == 1 & ((((h_total_1 + h00p)/(pop_total_1 + pop00p)) - (h00p/pop00p))) < -0.005), 1, 0),
    underbound_asian_hpct = ifelse(
      (annexing == 1 & increase_white == 1 & asian_total > 1 & ((((asian_total_1 + asian00p)/(pop00p + pop_total_1)) - (asian00p/pop00p))) < -0.005), 1, 0),
    underbound_native_hpct = ifelse(
      (annexing == 1 & increase_white == 1 & native_total > 1 & ((((native_total_1 + native00p)/(pop00p + pop_total_1)) - (native00p/pop00p))) < -0.005), 1, 0),
    underbound_other_hpct = ifelse(
      (annexing == 1 & increase_white == 1 & other_total > 1 & ((((other_total_1 + other00p)/(pop00p + pop_total_1)) - (other00p/pop00p))) < -0.005), 1, 0),
    underbound_nbmin_hpct = ifelse(
      (annexing == 1 & increase_white == 1 & nbmin_total > 1 & ((((nbmin_total_1 + nbmin00p)/(pop00p + pop_total_1)) - (nbmin00p/pop00p))) < -0.005), 1, 0),
    underbound_nhwhite_hpct = ifelse(
      (annexing == 1 & nhwhite_total > 1 & ((((nhwhite_total_1 + nhwhite00p)/(pop00p + pop_total_1)) - (nhwhite00p/pop00p))) < -0.005), 1, 0),
    underbound_black_10pct = ifelse(
      (annexing == 1 & nhblack_total > 1 & increase_white == 1 & ((((nhblack_total_1 + nhblack00p)/(pop_total_1 + pop00p)) - (nhblack00p/pop00p))) < -0.1), 1, 0), 
    underbound_hisp_10pct = ifelse(
      (annexing == 1 & h_total > 1 & increase_white == 1 & ((((h_total_1 + h00p)/(pop_total_1 + pop00p)) - (h00p/pop00p))) < -0.1), 1, 0),
    underbound_asian_10pct = ifelse(
      (annexing == 1 & increase_white == 1 & asian_total > 1 & ((((asian_total_1 + asian00p)/(pop00p + pop_total_1)) - (asian00p/pop00p))) < -0.1), 1, 0),
    underbound_native_10pct = ifelse(
      (annexing == 1 & increase_white == 1 & native_total > 1 & ((((native_total_1 + native00p)/(pop00p + pop_total_1)) - (native00p/pop00p))) < -0.1), 1, 0),
    underbound_other_10pct = ifelse(
      (annexing == 1 & increase_white == 1 & other_total > 1 & ((((other_total_1 + other00p)/(pop00p + pop_total_1)) - (other00p/pop00p))) < -0.1), 1, 0),
    underbound_nbmin_10pct = ifelse(
      (annexing == 1 & increase_white == 1 & nbmin_total > 1 & ((((nbmin_total_1 + nbmin00p)/(pop00p + pop_total_1)) - (nbmin00p/pop00p))) < -0.1), 1, 0),
    underbound_nhwhite_10pct = ifelse(
      (annexing == 1 & nhwhite_total > 1 & ((((nhwhite_total_1 + nhwhite00p)/(pop00p + pop_total_1)) - (nhwhite00p/pop00p))) < -0.1), 1, 0),
    underbound_black = ifelse(
      (annexing == 1 & nhblack_total > 1 & increase_white == 1 & ((((nhblack_total_1 + nhblack00p)/(pop_total_1 + pop00p)) < (nhblack00p/pop00p)))), 1, 0), 
    underbound_hisp = ifelse(
      (annexing == 1 & h_total > 1 & increase_white == 1 & ((((h_total_1 + h00p)/(pop_total_1 + pop00p)) < (h00p/pop00p)))), 1, 0),
    underbound_asian = ifelse(
      (annexing == 1 & increase_white == 1 & asian_total > 1 & ((((asian_total_1 + asian00p)/(pop00p + pop_total_1)) < (asian00p/pop00p)))), 1, 0),
    underbound_native = ifelse(
      (annexing == 1 & increase_white == 1 & native_total > 1 & ((((native_total_1 + native00p)/(pop00p + pop_total_1)) < (native00p/pop00p)))), 1, 0),
    underbound_other = ifelse(
      (annexing == 1 & increase_white == 1 & other_total > 1 & ((((other_total_1 + other00p)/(pop00p + pop_total_1)) < (other00p/pop00p)))), 1, 0),
    underbound_nbmin = ifelse(
      (annexing == 1 & increase_white == 1 & nbmin_total > 1 & ((((nbmin_total_1 + nbmin00p)/(pop00p + pop_total_1)) < (nbmin00p/pop00p)))), 1, 0),
    underbound_nhwhite = ifelse(
      (annexing == 1 & nhwhite_total > 1 & ((((nhwhite_total_1 + nhwhite00p)/(pop00p + pop_total_1)) < (nhwhite00p/pop00p)))), 1, 0)
  )

# compare SES
pl_annex_var_0007 %<>% 
  mutate(
    property_inc = ifelse(
      (annexing == 1 & owneroccupied_total > 1 & (((owneroccupied_total_1 + owneroccupied00p)/(hu00p + hu_total_1)) > (hu00p + hu_total_1))), 1, 0),
    hincjobs = ifelse(
      (annexing == 1 & nhincjobs_total > 1 & (((nhincjobs_total_1 + overlodesthresh00p)/(emp00p + njobs_total_1)) > (emp00p + njobs_total_1))), 1, 0)
  )

pl_annex_var_0007 %<>%
  filter(pop_total > 1 & hu_total > 1 & pop00p > 1) 

table(pl_annex_var_0007$underbound_blackvap)
table(pl_annex_var_0007$underbound_hispvap)
table(pl_annex_var_0007$underbound_asianvap)
table(pl_annex_var_0007$underbound_nativevap)
table(pl_annex_var_0007$underbound_othervap)
table(pl_annex_var_0007$underbound_nbminvap)
table(pl_annex_var_0007$underbound_nhwhitevap)
table(pl_annex_var_0007$underbound_blackvap_vraa)
table(pl_annex_var_0007$underbound_hispvap_vraa)
table(pl_annex_var_0007$underbound_asianvap_vraa)
table(pl_annex_var_0007$underbound_nativevap_vraa)
table(pl_annex_var_0007$underbound_othervap_vraa)
table(pl_annex_var_0007$underbound_nbminvap_vraa)
table(pl_annex_var_0007$underbound_nhwhitevap_vraa)
table(pl_annex_var_0007$underbound_blackvap_3pct)
table(pl_annex_var_0007$underbound_hispvap_3pct)
table(pl_annex_var_0007$underbound_asianvap_3pct)
table(pl_annex_var_0007$underbound_nativevap_3pct)
table(pl_annex_var_0007$underbound_othervap_3pct)
table(pl_annex_var_0007$underbound_nbmin_3pct)
table(pl_annex_var_0007$underbound_nhwhitevap_3pct)
table(pl_annex_var_0007$underbound_blackvap_hpct)
table(pl_annex_var_0007$underbound_hispvap_hpct)
table(pl_annex_var_0007$underbound_asianvap_hpct)
table(pl_annex_var_0007$underbound_nativevap_hpct)
table(pl_annex_var_0007$underbound_othervap_hpct)
table(pl_annex_var_0007$underbound_nbminvap_hpct)
table(pl_annex_var_0007$underbound_nhwhitevap_hpct)
table(pl_annex_var_0007$underbound_blackvap_10pct)
table(pl_annex_var_0007$underbound_hispvap_10pct)
table(pl_annex_var_0007$underbound_asianvap_10pct)
table(pl_annex_var_0007$underbound_nativevap_10pct)
table(pl_annex_var_0007$underbound_othervap_10pct)
table(pl_annex_var_0007$underbound_nbminvap_10pct)
table(pl_annex_var_0007$underbound_nhwhitevap_10pct)
table(pl_annex_var_0007$underbound_black)
table(pl_annex_var_0007$underbound_hisp)
table(pl_annex_var_0007$underbound_asian)
table(pl_annex_var_0007$underbound_native)
table(pl_annex_var_0007$underbound_other)
table(pl_annex_var_0007$underbound_nbmin)
table(pl_annex_var_0007$underbound_nhwhite)
table(pl_annex_var_0007$underbound_black_vraa)
table(pl_annex_var_0007$underbound_hisp_vraa)
table(pl_annex_var_0007$underbound_asian_vraa)
table(pl_annex_var_0007$underbound_native_vraa)
table(pl_annex_var_0007$underbound_other_vraa)
table(pl_annex_var_0007$underbound_nbmin_vraa)
table(pl_annex_var_0007$underbound_nhwhite_vraa)
table(pl_annex_var_0007$underbound_black_3pct)
table(pl_annex_var_0007$underbound_hisp_3pct)
table(pl_annex_var_0007$underbound_asian_3pct)
table(pl_annex_var_0007$underbound_native_3pct)
table(pl_annex_var_0007$underbound_other_3pct)
table(pl_annex_var_0007$underbound_nbmin_3pct)
table(pl_annex_var_0007$underbound_nhwhite_3pct)
table(pl_annex_var_0007$underbound_black_hpct)
table(pl_annex_var_0007$underbound_hisp_hpct)
table(pl_annex_var_0007$underbound_asian_hpct)
table(pl_annex_var_0007$underbound_native_hpct)
table(pl_annex_var_0007$underbound_other_hpct)
table(pl_annex_var_0007$underbound_nbmin_hpct)
table(pl_annex_var_0007$underbound_nhwhite_hpct)
table(pl_annex_var_0007$underbound_black_10pct)
table(pl_annex_var_0007$underbound_hisp_10pct)
table(pl_annex_var_0007$underbound_asian_10pct)
table(pl_annex_var_0007$underbound_native_10pct)
table(pl_annex_var_0007$underbound_other_10pct)
table(pl_annex_var_0007$underbound_nbmin_10pct)
table(pl_annex_var_0007$underbound_nhwhite_10pct)
table(pl_annex_var_0007$property_inc)
table(pl_annex_var_0007$hincjobs)

names(pl_annex_var_0007)
pl_annex_var_0007 %<>%
  select(-ends_with("90p"))
names(pl_annex_var_0007)

names(pl_annex_var_0007) <- gsub("00p", "_p0", names(pl_annex_var_0007))
names(pl_annex_var_0007)

# merge in 2007 data 
places2007 <- read_csv("places2007_interpolated.csv")
table(pl_annex_var_0007$plid %in% places2007$plid)

pl_annex_var_0007 %<>%
  filter(plid %in% places2007$plid) %>%
  left_join(places2007, by = "plid")

names(pl_annex_var_0007)
names(pl_annex_var_0007) <- gsub("07p", "_p1", names(pl_annex_var_0007))

pl_annex_var_0007 %<>%
  filter(pop_p1 > 0)
sapply(pl_annex_var_0007, function(x) sum(is.na(x)))
write_csv(pl_annex_var_0007, "analyticalfiles/pl_annex_var_0007.csv")

vraplids <- unique(c(places_vra_0007, places_vra_0713, places_vra_1420))
write_csv(as.data.frame(vraplids), "analyticalfiles/vra_places.csv")
rm(list = ls())

# make panel data!!!!! ####
vraplids <- read_csv("analyticalfiles/vra_places.csv")
NE <- c("09", "23", "25", "33", "34", "36", "42", "44", "50", "15", "02")
bas <- read_csv("analyticalfiles/bas_years.csv")

pl0007 <- read_csv("analyticalfiles/pl_annex_var_0007.csv") %>%
  mutate(STATE = substr(plid, 1, 2)) %>%
  filter(!(STATE %in% NE)) 

pl0713 <- read_csv("analyticalfiles/pl_annex_var_0713.csv") %>%
  mutate(STATE = substr(plid, 1, 2)) %>%
  filter(!(STATE %in% NE)) 

pl1420 <- read_csv("analyticalfiles/pl_annex_var_1420.csv") %>%
  mutate(STATE = substr(plid, 1, 2)) %>%
  filter(!(STATE %in% NE)) 

plids <- Reduce(intersect, list(unique(pl0713$plid), unique(pl1420$plid)))
names_list <- Reduce(intersect, list(names(pl0007), names(pl0713), names(pl1420)))

pl0007 %<>% 
  filter(plid %in% plids) %>%
  select(all_of(names_list)) %>%
  filter(!duplicated(plid)) %>%
  mutate(vra = ifelse(plid %in% vraplids$vraplids, 1, 0))
pl0713 %<>%
  filter(plid %in% plids) %>%
  select(all_of(names_list)) %>%
  mutate(vra = ifelse(plid %in% vraplids$vraplids, 1, 0))
pl1420 %<>%
  filter(plid %in% plids) %>%
  select(all_of(names_list)) %>%
  mutate(vra = ifelse(plid %in% vraplids$vraplids, 1, 0))

panel0020_did <- base::rbind(
  pl0007, pl0713, pl1420
)

names(panel0020_did) <- gsub("ppov", "pctpov", names(panel0020_did))
panel0020_did %<>%
  mutate_at(vars(ends_with("_total_1")), ~ifelse(annexing == 0, NA, .))

panel0020_did %<>% 
  mutate(period = ifelse(post ==1, 1, 0), 
         black_diff = pctnhblack_total - pctnhblack_p0,
         more_black = ifelse(pctnhblack_total > pctnhblack_p0, 1, 0),
         white_diff = (pctnhwhite_total - pctnhwhite_p0), 
         more_white = ifelse(pctnhwhite_total > pctnhwhite_p0, 1, 0),
         hisp_diff = (pcth_total - pcth_p0), 
         more_hisp = ifelse(pcth_total > pcth_p0, 1, 0),
         asian_diff = (pctasian_total - pctasian_p0), 
         more_asian = ifelse(pctasian_total > pctasian_p0, 1, 0),
         native_diff = (pctnative_total - pctnative_p0), 
         more_native = ifelse(pctnative_total > pctnative_p0, 1, 0),
         other_diff = (pctother_total - pctother_p0), 
         more_other = ifelse(pctother_total > pctother_p0, 1, 0),
         nbmin_diff = (pctnbmin_total - pctnbmin_p0),
         more_nbmin = ifelse(pctnbmin_total > pctnbmin_p0, 1, 0)) #%>% 
filter_at(vars(starts_with("pct")), ~ifelse(is.na(.) | .<0.1, 0.1, .)) 

overtime_diff_p0 <- names(panel0020_did)[which(grepl("_p0", names(panel0020_did)))]
overtime_diff_p0 <- gsub("_p0", "", overtime_diff_p0)
overtime_diff_p1 <- names(panel0020_did)[which(grepl("_p1", names(panel0020_did)))]
overtime_diff_p1 <- gsub("_p1", "", overtime_diff_p1)

overtime_diff <- Reduce(intersect, list(overtime_diff_p0, overtime_diff_p1))
overtime_diff <- overtime_diff[which(grepl("pct", overtime_diff))]

for (variable in overtime_diff) {
  panel0020_did[[paste0(variable, "_diff")]] <- (panel0020_did[[paste0(variable, "_p1")]] - panel0020_did[[paste0(variable, "_p0")]])
}

rm(overtime_diff, overtime_diff_p0, overtime_diff_p1)
sapply(panel0020_did, function(x) sum(is.na(x)))

panel0020_did %<>%
  filter_at(vars(pop_p0, popdensity_p0, popgrowth, pctnhwhite_p0, pctnhblackgrowth, pctnbmingrowth, pctowneroccupied_p0, mhmval_p0, hinc_p0, pctpov_p0, pctnhwhitegrowth, more_white, pctnhblack_total, pctnbmin_total, pctownerocc_total, pcthincjobs_total, pctincopp_total, pctnhblack_p0, pcth_p0, pcthgrowth, pctnhwhite_total, pcth_total, pctnbmin_total), ~!is.na(.))

plids <- Reduce(intersect, list(unique(panel0020_did$plid[panel0020_did$time == "2000 to 2007"]), unique(panel0020_did$plid[panel0020_did$time == "2007 to 2013"]), unique(panel0020_did$plid[panel0020_did$time == "2014 to 2020"])))

panel0020_did %<>%
  filter(plid %in% plids)

# which places are most segregated?
panel0020_did %<>%
  mutate(blackdiff = pctnhblack_total - pctnhblack_p0)

panel0020_did %>%
  filter(underbound_black_hpct == 1 & period == 1 & vra == 1) %>%
  arrange(desc(blackdiff), desc(pop_p0)) %>%
  View()

panel0020_did %>%
  filter(underbound_black_10pct == 1 & period == 1 & vra == 1) %>%
  arrange(desc(blackdiff), desc(annexing)) %>%
  View()
