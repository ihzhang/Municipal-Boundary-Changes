# get environment ready 
setwd("~/Google Drive/My Drive/Stanford/QE2")

library("stringr")
library("dplyr")
library("stargazer")
library("tidyverse")
library("tidycensus")
library("fixest")
library("readr")
library("data.table")
library("magrittr")
library("openxlsx")
library("broom")
library("sjPlot")
library("PSweight")
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
aa0713 <- read_csv("analyticalfiles/annexedblocks0713dem_bas.csv") 
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
         pctincopp_total = (incopp_total/nwork_total)*100,
         pcthincjobs_total = (nhincjobs_total/njobs_total)*100,
         pctnhblackvap_total = (nhblackvap_total/vap_total)*100,
         pcthvap_total = (hvap_total/vap_total)*100,
         pctnhwhitevap_total = (nhwhitevap_total/vap_total)*100,
         pctasianvap_total = (asianvap_total/vap_total)*100,
         pctnativevap_total = (nativevap_total/vap_total)*100,
         pctothervap_total = (othervap_total/vap_total)*100,
         pctnbminvap_total = (nbminvap_total/vap_total)*100)

# add vra indicator 
places_vra <- aa0713 %>%
  group_by(plid) %>%
  summarize(vra = mean(vra, na.rm = T),
            annexing = mean(annexed, na.rm = T)) %>%
  ungroup() %>%
  mutate(vra = ifelse(vra > 0, 1, 0),
         annexing = ifelse(annexing > 0, 1, 0))

pl_annex_var_0713 <- place_all %<>%
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

table(pl_annex_var_0713$plid %in% pl0007$plid) #175 false

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

write_csv(pl_annex_var_0713, "analyticalfiles/pl_annex_var_0713_bas.csv")

rm(aa0713, pl_annex_var_0713, pl0007, places2013, cdps07)

#repeat for 1420 ####
aa1420 <- read_csv("analyticalfiles/annexedblocks1420dem_bas.csv") 

place_all <- aa1420 %>% 
  mutate(incopp = man + ret) %>%
  group_by(plid) %>%
  summarize(pop_total = sum(pop, na.rm = T),
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
         pctincopp_total = (incopp_total/nwork_total)*100,
         pcthincjobs_total = (nhincjobs_total/njobs_total)*100,
         pctnhblackvap_total = (nhblackvap_total/vap_total)*100,
         pcthvap_total = (hvap_total/vap_total)*100,
         pctnhwhitevap_total = (nhwhitevap_total/vap_total)*100,
         pctasianvap_total = (asianvap_total/vap_total)*100,
         pctnativevap_total = (nativevap_total/vap_total)*100,
         pctothervap_total = (othervap_total/vap_total)*100,
         pctnbminvap_total = (nbminvap_total/vap_total)*100)

pl_annex_var_1420 <- place_all

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

places_vra_1420 <- unique(places_vra$plid[places_vra$vra==1])

# add place-level data for race and vap 
# vra violation = if pctwhite after annex > pctwhite before annex compared to
# what would have been, i.e. pctwhite given growth rate by 2013
# or if pctblack after annex < pctblack before annex compared to what would 
# have been, i.e. pctwhite given growth rate by 2013
# or if pcth after annex < pcth before annex compared to what would have been,
# i.e. pcth given growth rate by 2013 
pl0714 <- read_csv("pl0714_var.csv")

table(pl_annex_var_1420$plid %in% pl0714$plid) #4533 false

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

names(pl_annex_var_1420)
pl_annex_var_1420 %<>%
  select(-ends_with("07p"))
names(pl_annex_var_1420)

names(pl_annex_var_1420) <- gsub("14p", "_p0", names(pl_annex_var_1420))
names(pl_annex_var_1420)

# merge in 2020 data 
places2020 <- read_csv("places2020_cleaned.csv")
table(pl_annex_var_1420$plid %in% places2020$plid)

pl_annex_var_1420 %<>%
  filter(plid %in% places2020$plid) %>%
  left_join(places2020 %>% select(-Geo_NAME), by = "plid")

names(pl_annex_var_1420)
names(pl_annex_var_1420) <- gsub("20p", "_p1", names(pl_annex_var_1420))

write_csv(pl_annex_var_1420, "analyticalfiles/pl_annex_var_1420_bas.csv")

rm(aa1420, pl_annex_var_1420, pl0714, places2020, cdps14)

# repeat for 0007 ####
aa0007 <- read_csv("analyticalfiles/annexedblocks0007dem_bas.csv") 
names(aa0007)

names(aa0007) <- gsub("00b", "", names(aa0007))

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
         pctincopp_total = (incopp_total/nwork_total)*100,
         pcthincjobs_total = (nhincjobs_total/njobs_total)*100,
         pctnhblackvap_total = (nhblackvap_total/vap_total)*100,
         pcthvap_total = (hvap_total/vap_total)*100,
         pctnhwhitevap_total = (nhwhitevap_total/vap_total)*100,
         pctasianvap_total = (asianvap_total/vap_total)*100,
         pctnativevap_total = (nativevap_total/vap_total)*100,
         pctothervap_total = (othervap_total/vap_total)*100,
         pctnbminvap_total = (nbminvap_total/vap_total)*100)

pl_annex_var_0007 <- place_all

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

places_vra_0007 <- unique(places_vra$plid[places_vra$vra==1])

# add place-level data for race and vap 
# vra violation = if pctwhite after annex > pctwhite before annex compared to
# what would have been, i.e. pctwhite given growth rate by 2013
# or if pctblack after annex < pctblack before annex compared to what would 
# have been, i.e. pctwhite given growth rate by 2013
# or if pcth after annex < pcth before annex compared to what would have been,
# i.e. pcth given growth rate by 2013 
pl9000 <- read_csv("pl9000_var.csv")

table(pl_annex_var_0007$plid %in% pl9000$plid) #1845 false

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

names(pl_annex_var_0007)
pl_annex_var_0007 %<>%
  select(-ends_with("90p"))
names(pl_annex_var_0007)

names(pl_annex_var_0007) <- gsub("00p", "_p0", names(pl_annex_var_0007))
names(pl_annex_var_0007)

# merge in 2007 data 
places2007 <- read_csv("pl2007_cleaned.csv")

pl_annex_var_0007 %<>%
  left_join(places2007 %>% select(-Geo_NAME), by = "plid")

names(pl_annex_var_0007)
names(pl_annex_var_0007) <- gsub("07p", "_p1", names(pl_annex_var_0007))

table(pl_annex_var_0007$annexing)

write_csv(pl_annex_var_0007, "analyticalfiles/pl_annex_var_0007_bas.csv")

rm(aa0007, pl_annex_var_0007, pl9000, places2007, cdps00)

vraplids <- unique(c(places_vra_0007, places_vra_0713, places_vra_1420))
write_csv(as.data.frame(vraplids), "analyticalfiles/vra_places_bas.csv")
rm(list = ls())

# make panel data!!!!! ####
# take out ne and hawaii
vraplids <- read_csv("analyticalfiles/vra_places_bas.csv")
NE <- c("09", "23", "25", "33", "34", "36", "42", "24", "44", "50", "15")
pl0007 <- read_csv("analyticalfiles/pl_annex_var_0007_bas.csv") %>%
  mutate(STATE = substr(plid, 1, 2)) %>%
  filter(!(STATE %in% NE)) 
pl0713 <- read_csv("analyticalfiles/pl_annex_var_0713_bas.csv") %>%
  mutate(STATE = substr(plid, 1, 2)) %>%
  filter(!(STATE %in% NE)) 
pl1420 <- read_csv("analyticalfiles/pl_annex_var_1420_bas.csv") %>%
  mutate(STATE = substr(plid, 1, 2)) %>%
  filter(!(STATE %in% NE)) 

# they must be in the latter periods 
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

laws <- read_csv("statelawsdatabase1.csv")
laws %<>%
  mutate(statefips = str_pad(`FIPS Code`, 2, "left", "0")) %>%
  mutate_at(vars(referenda_risk:township_state), ~as.character(.))

panel0020_did %<>%
  left_join(laws, by = c("STATE" = "statefips"))

rm(pl0007, pl0713, pl1420)

panel0020_did %<>% 
  mutate(period = ifelse(post ==1, 1, 0), 
         vra = as.factor(vra),
         period = as.factor(period), 
         black_diff = (pctnhblack_total - pctnhblack_p0),
         white_diff = (pctnhwhite_total - pctnhwhite_p0), 
         hisp_diff = (pcth_total - pcth_p0), 
         asian_diff = (pctasian_total - pctasian_p0), 
         native_diff = (pctnative_total - pctnative_p0), 
         other_diff = (pctother_total - pctother_p0), 
         nbmin_diff = (pctnbmin_total - pctnbmin_p0),
         maj_white = ifelse(pctnhwhite_p0 >= 50, 1, 0)) %>% 
  mutate_at(vars(starts_with("pct")), ~ifelse(is.na(.) | .<0.1, 0.1, .)) %>%
  filter(pop_p0 > 100) 

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

panel0020_did %<>%
  mutate_at(vars(c(ends_with("total"), ends_with("_p0"), ends_with("_p1"), ends_with("_total_1"), contains("growth"), contains("_annexed"))), 
            ~((.-mean(., na.rm = T))/sd(., na.rm = T))) 

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

dclus1 <- svydesign(id=~plid, data=panel0020_did)

annex_basic <- glm(annexing ~ as.factor(period)*as.factor(vra) + as.factor(STATE), family = "binomial", data = panel0020_did)
coeftest(annex_basic, vcov = vcovCL, cluster = panel0020_did$plid)

annex_basic_diff_0 <- glm(annexing ~ as.factor(period) + as.factor(STATE), family = "binomial", data = panel0020_did %>% filter(vra==0))
coeftest(annex_basic_diff_0, vcov = vcovCL, cluster = panel0020_did$plid[panel0020_did$vra==0])

annex_basic_diff_1 <- glm(annexing ~ as.factor(period) + as.factor(STATE), family = "binomial", data = panel0020_did %>% filter(vra==1))
coeftest(annex_basic_diff_1, vcov = vcovCL, cluster = panel0020_did$plid[panel0020_did$vra==1])

ann_test_0 <- glm(annexing ~ as.factor(period) + pop_p0 + popdensity_p0 + popgrowth + pctnhwhite_p0 + pcthgrowth + pctowneroccupied_p0 + mhmval_p0 + hinc_p0 + pov_p0 + pctblackpov_p0 + pctemp_p0 + pctnhwhitegrowth + pctnhblack_total + recimmgrowth + pctownerocc_total + pcthincjobs_total + as.factor(STATE), family = "binomial", data = panel0020_did %>% filter(vra==0))
coeftest(ann_test_0, vcov = vcovCL, cluster = panel0020_did$plid[panel0020_did$vra==0])

ann_test_1 <- feglm(annexing ~ as.factor(period) + pop_p0 + popdensity_p0 + popgrowth + pctnhwhite_p0 + pcthgrowth + pctowneroccupied_p0 + mhmval_p0 + hinc_p0 + pov_p0 + pctblackpov_p0 + pctemp_p0 + pctnhwhitegrowth + pctnhblack_total + recimmgrowth + pctownerocc_total + pcthincjobs_total + as.factor(STATE), family = "binomial", data = panel0020_did %>% filter(vra==1))
coeftest(ann_test_1, vcov = vcovCL, cluster = panel0020_did$plid[panel0020_did$vra==1])

nhb_base_0 <- feols(pctnhblack_p1 ~ as.factor(annexing)*as.factor(period) | plid + STATE, data = panel0020_did)
summary(nhb_base_0) # N = 27987

nhb_base_0 <- fixest::feols(pctnhblack_p1 ~ as.factor(annexing)*as.factor(period) + pctnhwhite_p0 + pctnhblack_p0 + pctnhwhitegrowth + pctnhblackgrowth + pctnhblack_total + pctnhwhite_total + popgrowth + recimmgrowth | plid + STATE, data = panel0020_did)
summary(nhb_base_0) # N = 27987

nhb_base_1 <- fixest::feols(pctnhblack_p1 ~ as.factor(annexing)*as.factor(period) + pctnhwhite_p0 + pctnhblack_p0 + pctnhwhitegrowth + pctnhblackgrowth + pctnhblack_total + pctnhwhite_total + popgrowth + recimmgrowth | plid + STATE, data = panel0020_did %>% filter(vra==1))
summary(nhb_base_1) # N = 27987

h_base_1 <- fixest::feols(pcth_p1 ~ as.factor(annexing)*as.factor(period) + pctnhwhite_p0 + pcth_p0 + pctnhwhitegrowth + pcthgrowth + pcth_total + recimmgrowth | plid + STATE, data = panel0020_did)
summary(h_base_1) # N = 27987

nhb_base <- fixest::feols(pctnhblack_p1 ~ as.factor(period)*as.factor(vra) + pctnhblack_p0 + pctnhblackgrowth + pctnhwhitegrowth + nhblack_total | plid, data = panel0020_did %>% filter(annexing == 1))
summary(nhb_base) # N = 27987

nhw_base <- fixest::feols(pctnhwhite_p1 ~ as.factor(period)*as.factor(vra) + pctnhwhite_p0 + pctnhwhitegrowth + nhwhite_total | plid, data = panel0020_did %>% filter(annexing == 1))
summary(nhw_base) # N = 27987

hpct <- fixest::feols(pcth_p1 ~ as.factor(vra)*as.factor(period) + pcth_p0 + pcth_total + pcthgrowth + pctnhwhitegrowth | plid, data = panel0020_did %>% filter(annexing == 1))
summary(hpct)

black_all <- list(tidy(all), tidy(hpct), tidy(pct3), tidy(pct10))
openxlsx::write.xlsx(black_all, "analyticalfiles/results/black_all_nowhite.xlsx")


# p1 race ####
nhb_base <- fixest::feols(pctnhblack_p1 ~ as.factor(annexing) + pctnhblack_p0 + pctnhblackgrowth + nhblack_total | plid, data = panel0020_did)
summary(nhb_base) # N = 27987

nhb_annex <- fixest::feols(pctnhblack_p1 ~ as.factor(vra) + as.factor(period)*as.factor(annexing) + pctnhwhitegrowth + pctnhwhite_p0 + pctnhblackgrowth + pctnhblack_p0 + nhblack_total | plid, data = panel0020_did)
summary(nhb_annex) # N = 3267

nhw <- fixest::feols(pctnhwhite_p1 ~ as.factor(annexing) | plid + period + vra, data = nhwpanel)
summary(nhw) # N = 744

nhw_annex <- fixest::feols(pctnhwhite_p1 ~ as.factor(vra) + as.factor(period)*as.factor(annexing) + pctemp_p0 + pctowneroccupied_p0 + mhmval_p0 + valgrowth + hinc_p0 + incomegrowth + pctnhwhitegrowth + pctnhwhite_p0 + pctnhblackgrowth + pctnhblack_p0 + pctnhblack_total + pctownerocc_total + pcthincjobs_total + pctincocc_total | plid, data = panel0020_did)
summary(nhw_annex) # N = 627

h <- fixest::feols(pcth_p1 ~ as.factor(annexing) | plid + period + vra, data = hpanel)
summary(h) # N = 219

h_annex <- fixest::feols(pcth_p1 ~ as.factor(period)*as.factor(vra) + pcth_p0 + h_total + pcthgrowth + pctnhwhitegrowth + pctnhwhite_p0 + pcthpov_p0 + pctowneroccupied_p0 + pctemp_p0 + mhmval_p0 | plid, data = hpanel_a)
summary(h_annex) # N = 174

asian <- fixest::feols(pctasian_p1 ~ as.factor(annexing) | plid + period, data = asianpanel)
summary(asian) # N = 234

asian_annex <- fixest::feols(pctasian_p1 ~ as.factor(period)*as.factor(vra) + pctasian_p0 + asian_total + pctasiangrowth + pctnhwhitegrowth + pctasianpov_p0 + pctnhwhite_p0 + pctowneroccupied_p0 + pctemp_p0 + mhmval_p0 | plid, data = asianpanel_a)
summary(asian_annex) # N = 171

native <- fixest::feols(pctnative_p1 ~ as.factor(annexing) | plid + period, data = nativepanel)
summary(native) #336

native_annex <- fixest::feols(pctnative_p1 ~ as.factor(period)*as.factor(vra) + pctnative_p0 + native_total + pctnativegrowth + pctnativepov_p0 + pctnhwhitegrowth + pctnhwhite_p0 + pctowneroccupied_p0 + pctemp_p0 + mhmval_p0 | plid, data = nativepanel_a)
summary(native_annex) # 255

other <- fixest::feols(pctother_p1 ~ as.factor(annexing) | plid + period, data = otherpanel)
summary(other) #405

other_annex <- fixest::feols(pctother_p1 ~ as.factor(period)*as.factor(vra) + pctother_p0 + other_total + pctothergrowth + pctotherpov_p0 + pctnhwhitegrowth + pctnhwhite_p0 + pctowneroccupied_p0 + pctemp_p0 + mhmval_p0 | plid, data = otherpanel_a)
summary(other_annex) # 339

nbmin <- fixest::feols(pctnbmin_p1 ~ as.factor(annexing) | plid + period, data = nbminpanel)
summary(nbmin) #261

nbmin_annex <- fixest::feols(pctnbmin_p1 ~ as.factor(period)*as.factor(vra) + pctnbmin_p0 + nbmin_total + pctnbmingrowth + pctnbminpov_p0 + pctnhwhitegrowth + pctnhwhite_p0 + pctowneroccupied_p0 + pctemp_p0 + mhmval_p0 | plid, data = nbminpanel_a)
summary(nbmin_annex) #195

race_all <- list(tidy(nhb), tidy(h), tidy(nhw), tidy(native), tidy(asian), tidy(other), tidy(nbmin))
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
  filter(underbound_black_hpct == 1 & period == 1 & vra == 1) %>%
  arrange(desc(blackdiff), desc(pop_p0)) %>%
  View()

panel0020_did %>%
  filter(underbound_black_10pct == 1 & period == 1 & vra == 1) %>%
  arrange(desc(blackdiff), desc(annexing)) %>%
  View()
