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

# what happens if we just limit to VRA places and remove the DiD set-up? 

# make panel data!!!!! ####
# take out ne and hawaii
NE <- c("09", "23", "25", "33", "34", "36", "42", "24", "44", "50", "15")
pl0007 <- read_csv("analyticalfiles/pl_annex_var_0007.csv") %>%
  mutate(STATE = substr(plid, 1, 2)) %>%
  filter(!(STATE %in% NE)) %>%
  filter(vra==1) 
pl0713 <- read_csv("analyticalfiles/pl_annex_var_0713.csv") %>%
  mutate(STATE = substr(plid, 1, 2)) %>%
  filter(!(STATE %in% NE)) %>%
  filter(vra==1) 
pl1420 <- read_csv("analyticalfiles/pl_annex_var_1420.csv") %>%
  mutate(STATE = substr(plid, 1, 2)) %>%
  filter(!(STATE %in% NE)) %>%
  filter(vra==1) 

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

nhw <- fixest::feols(annexing ~ period + pop_p0 + popdensity_p0 + pctnhwhite_p0 +
                       pctemp_p0 + hinc_p0 + pctowneroccupied_p0 + mhmval_p0 + incomepp_p0 | plid, data = panel0020_did)
sjPlot::plot_model(nhw)

annex <- tidy(nhw)
openxlsx::write.xlsx(annex, "analyticalfiles/results/annex.xlsx")

# what kind of annex or not ####
# black 
# not vap 
all <- fixest::feols(underbound_black ~ as.factor(period) + pop_p0 + pctnhwhite_p0 + pctnhwhite_total + pctincocc_total + pctownerocc_total +
                       pcthincjobs_total + pctnhblack_total + pctnhblack_p0 + pcth_total + pcth_p0 + pctowneroccupied_p0 + pctemp_p0 + mhmval_p0 + incomepp_p0 + hinc_p0 | plid, data = panel0020_did %>% filter(nhblack_total >= 1))
summary(all)

hpct <- fixest::feols(underbound_black_hpct ~ as.factor(period) + pop_p0 + pctnhwhite_p0 + pctnhwhite_total + pctincocc_total + pctownerocc_total +
                        pcthincjobs_total + pctnhblack_total + pctnhblack_p0 + pcth_total + pcth_p0 + pctowneroccupied_p0 + pctemp_p0 + mhmval_p0 + incomepp_p0 + hinc_p0 | plid, data = panel0020_did %>% filter(nhblack_total >= 1))
summary(hpct)

pct3 <- fixest::feols(underbound_black_3pct ~ as.factor(period) + pop_p0 + pctnhwhite_p0 + pctnhwhite_total + pctincocc_total + pctownerocc_total +
                        pcthincjobs_total + pctnhblack_total + pctnhblack_p0 + pcth_total + pcth_p0 + pctowneroccupied_p0 + pctemp_p0 + mhmval_p0 + incomepp_p0 + hinc_p0 | plid, data = panel0020_did %>% filter(nhblack_total >= 1))
summary(pct3)
pct10 <- fixest::feols(underbound_black_10pct ~ as.factor(period) + pop_p0 + pctnhwhite_p0 + pctnhwhite_total + pctincocc_total + pctownerocc_total +
                         pcthincjobs_total + pctnhblack_total + pctnhblack_p0 + pcth_total + pcth_p0 + pctowneroccupied_p0 + pctemp_p0 +mhmval_p0 + incomepp_p0 + hinc_p0 | plid, data = panel0020_did %>% filter(nhblack_total >= 1))
summary(pct10)
black_all <- list(tidy(all), tidy(hpct), tidy(pct3), tidy(pct10))
openxlsx::write.xlsx(black_all, "analyticalfiles/results/black_all_nowhite.xlsx")

#hisp 
all <- fixest::feols(underbound_hisp ~ as.factor(period) + pop_p0 + pctnhwhite_p0 + pctnhwhite_total + pctincocc_total + pctownerocc_total +
                       pcthincjobs_total + pctnhblack_total + pctnhblack_p0 + pcth_total + pcth_p0 + pctowneroccupied_p0 + pctemp_p0 +mhmval_p0 + incomepp_p0 + hinc_p0 | plid, data = panel0020_did %>% filter(h_total >= 1))
summary(all)
hpct <- fixest::feols(underbound_hisp_hpct ~ as.factor(period) + pop_p0 + pctnhwhite_p0 + pctnhwhite_total + pctincocc_total + pctownerocc_total +
                        pcthincjobs_total + pctnhblack_total + pctnhblack_p0 + pcth_total + pcth_p0 + pctowneroccupied_p0 + pctemp_p0 +mhmval_p0 + incomepp_p0 + hinc_p0 | plid, data = panel0020_did %>% filter(h_total >= 1))
summary(hpct)
pct3 <- fixest::feols(underbound_hisp_3pct ~ as.factor(period) + pop_p0 + pctnhwhite_p0 + pctnhwhite_total + pctincocc_total + pctownerocc_total +
                        pcthincjobs_total + pctnhblack_total + pctnhblack_p0 + pcth_total + pcth_p0 + pctowneroccupied_p0 + pctemp_p0 +mhmval_p0 + incomepp_p0 + hinc_p0 | plid, data = panel0020_did %>% filter(h_total >= 1))
summary(pct3)
pct10 <- fixest::feols(underbound_hisp_10pct ~ as.factor(period) + pop_p0 + pctnhwhite_p0 + pctnhwhite_total + pctincocc_total + pctownerocc_total +
                         pcthincjobs_total + pctnhblack_total + pctnhblack_p0 + pcth_total + pcth_p0 + pctowneroccupied_p0 + pctemp_p0 +mhmval_p0 + incomepp_p0 + hinc_p0 | plid, data = panel0020_did %>% filter(h_total >= 1))
summary(pct10)

hisp_all <- list(tidy(all), tidy(hpct), tidy(pct3), tidy(pct10))
openxlsx::write.xlsx(hisp_all, "analyticalfiles/results/hisp_all_nowhite.xlsx")

#nhw
all <- fixest::feols(underbound_nhwhite ~ as.factor(period) + pop_p0 + pctnhwhite_p0 + pctnhwhite_total + pctincocc_total + pctownerocc_total +
                       pcthincjobs_total + pctnhblack_total + pctnhblack_p0 + pcth_total + pcth_p0 + pctowneroccupied_p0 + pctemp_p0 + mhmval_p0 + incomepp_p0 + hinc_p0 | plid, data = panel0020_did %>% filter(nhwhite_total >= 1))
summary(all)
hpct <- fixest::feols(underbound_nhwhite_hpct ~ as.factor(period) + pop_p0 + pctnhwhite_p0 + pctnhwhite_total + pctincocc_total + pctownerocc_total +
                        pcthincjobs_total + pctnhblack_total + pctnhblack_p0 + pcth_total + pcth_p0 + pctowneroccupied_p0 + pctemp_p0 + mhmval_p0 + incomepp_p0 + hinc_p0 | plid, data = panel0020_did %>% filter(nhwhite_total >= 1))
summary(hpct)
pct3 <- fixest::feols(underbound_nhwhite_3pct ~ as.factor(period) + pop_p0 + pctnhwhite_p0 + pctnhwhite_total + pctincocc_total + pctownerocc_total +
                        pcthincjobs_total + pctnhblack_total + pctnhblack_p0 + pcth_total + pcth_p0 + pctowneroccupied_p0 + pctemp_p0 + mhmval_p0 + incomepp_p0 + hinc_p0 | plid, data = panel0020_did %>% filter(nhwhite_total >= 1))
summary(pct3)

nhwhite_all <- list(tidy(all), tidy(hpct), tidy(pct3))
openxlsx::write.xlsx(nhwhite_all, "analyticalfiles/results/nhwhite_all.xlsx")

# vap 
all <- fixest::feols(underbound_blackvap ~ as.factor(period) + pop_p0 + pctnhwhitevap_p0 + pctnhwhitevap_total + pctincocc_total + pctownerocc_total +
                       pcthincjobs_total + pctnhblackvap_total + pctnhblackvap_p0 + pcthvap_total + pcthispvap_p0 + pctowneroccupied_p0 + pctemp_p0 + mhmval_p0 + incomepp_p0 + hinc_p0 | plid, data = panel0020_did %>% filter(nhblackvap_total >= 1))
summary(all)
hpct <- fixest::feols(underbound_blackvap_hpct ~ as.factor(period) + pop_p0 + pctnhwhitevap_p0 + pctnhwhitevap_total + pctincocc_total + pctownerocc_total +
                        pcthincjobs_total + pctnhblackvap_total + pctnhblackvap_p0 + pcthvap_total + pcthispvap_p0 + pctowneroccupied_p0 + pctemp_p0 + mhmval_p0 + incomepp_p0 + hinc_p0 | plid, data = panel0020_did %>% filter(nhblackvap_total >= 1))
summary(hpct)
pct3 <- fixest::feols(underbound_blackvap_3pct ~ as.factor(period) + pop_p0 + pctnhwhitevap_p0 + pctnhwhitevap_total + pctincocc_total + pctownerocc_total +
                        pcthincjobs_total + pctnhblackvap_total + pctnhblackvap_p0 + pcthvap_total + pcthispvap_p0 + pctowneroccupied_p0 + pctemp_p0 + mhmval_p0 + incomepp_p0 + hinc_p0 | plid, data = panel0020_did %>% filter(nhblackvap_total >= 1))
summary(pct3)
pct10 <- fixest::feols(underbound_blackvap_10pct ~ as.factor(period) + pop_p0 + pctnhwhitevap_p0 + pctnhwhitevap_total + pctincocc_total + pctownerocc_total +
                         pcthincjobs_total + pctnhblackvap_total + pctnhblackvap_p0 + pcthvap_total + pcthispvap_p0 + pctowneroccupied_p0 + pctemp_p0 + mhmval_p0 + incomepp_p0 + hinc_p0 | plid, data = panel0020_did %>% filter(nhblackvap_total >= 1))
summary(pct10)
black_all <- list(tidy(all), tidy(hpct), tidy(pct3), tidy(pct10))
openxlsx::write.xlsx(black_all, "analyticalfiles/results/blackvap_all_nowhite.xlsx")

#hisp 
all <- fixest::feols(underbound_hispvap ~ as.factor(period) + pop_p0 + pctnhwhitevap_p0 + pctnhwhitevap_total + pctincocc_total + pctownerocc_total +
                       pcthincjobs_total + pctnhblackvap_total + pctnhblackvap_p0 + pcthvap_total + pcthispvap_p0 + pctowneroccupied_p0 + pctemp_p0 + mhmval_p0 + incomepp_p0 + hinc_p0 | plid, data = panel0020_did %>% filter(hvap_total >= 1))
summary(all)
hpct <- fixest::feols(underbound_hispvap_hpct ~ as.factor(period) + pop_p0 + pctnhwhitevap_p0 + pctnhwhitevap_total + pctincocc_total + pctownerocc_total +
                        pcthincjobs_total + pctnhblackvap_total + pctnhblackvap_p0 + pcthvap_total + pcthispvap_p0 + pctowneroccupied_p0 + pctemp_p0 + mhmval_p0 + incomepp_p0 + hinc_p0 | plid, data = panel0020_did %>% filter(hvap_total >= 1))
summary(hpct)
pct3 <- fixest::feols(underbound_hispvap_3pct ~ as.factor(period) + pop_p0 + pctnhwhitevap_p0 + pctnhwhitevap_total + pctincocc_total + pctownerocc_total +
                        pcthincjobs_total + pctnhblackvap_total + pctnhblackvap_p0 + pcthvap_total + pcthispvap_p0 + pctowneroccupied_p0 + pctemp_p0 + mhmval_p0 + incomepp_p0 + hinc_p0 | plid, data = panel0020_did %>% filter(hvap_total >= 1))
summary(pct3)
pct10 <- fixest::feols(underbound_hispvap_10pct ~ as.factor(period) + pop_p0 + pctnhwhitevap_p0 + pctnhwhitevap_total + pctincocc_total + pctownerocc_total +
                         pcthincjobs_total + pctnhblackvap_total + pctnhblackvap_p0 + pcthvap_total + pcthispvap_p0 + pctowneroccupied_p0 + pctemp_p0 + mhmval_p0 + incomepp_p0 + hinc_p0 | plid, data = panel0020_did %>% filter(hvap_total >= 1))
summary(pct10)

hisp_all <- list(tidy(all), tidy(hpct), tidy(pct3), tidy(pct10))
openxlsx::write.xlsx(hisp_all, "analyticalfiles/results/hispvap_all_nowhite.xlsx")

#nhw
all <- fixest::feols(underbound_nhwhitevap ~ as.factor(period) + pop_p0 + pctnhwhitevap_p0 + pctnhwhitevap_total + pctincocc_total + pctownerocc_total +
                       pcthincjobs_total + pctnhblackvap_total + pctnhblackvap_p0 + pcthvap_total + pcthispvap_p0 + pctowneroccupied_p0 + pctemp_p0 + mhmval_p0 + incomepp_p0 + hinc_p0 | plid, data = panel0020_did %>% filter(nhwhitevap_total >= 1))
summary(all)
hpct <- fixest::feols(underbound_nhwhitevap_hpct ~ as.factor(period) + pop_p0 + pctnhwhitevap_p0 + pctnhwhitevap_total + pctincocc_total + pctownerocc_total +
                        pcthincjobs_total + pctnhblackvap_total + pctnhblackvap_p0 + pcthvap_total + pcthispvap_p0 + pctowneroccupied_p0 + pctemp_p0 + mhmval_p0 + incomepp_p0 + hinc_p0 | plid, data = panel0020_did %>% filter(nhwhitevap_total >= 1))
summary(hpct)
pct3 <- fixest::feols(underbound_nhwhitevap_3pct ~ as.factor(period) + pop_p0 + pctnhwhitevap_p0 + pctnhwhitevap_total + pctincocc_total + pctownerocc_total +
                        pcthincjobs_total + pctnhblackvap_total + pctnhblackvap_p0 + pcthvap_total + pcthispvap_p0 + pctowneroccupied_p0 + pctemp_p0 + mhmval_p0 + incomepp_p0 + hinc_p0 | plid, data = panel0020_did %>% filter(nhwhitevap_total >= 1))
summary(pct3)

nhwhite_all <- list(tidy(all), tidy(hpct), tidy(pct3))
openxlsx::write.xlsx(nhwhite_all, "analyticalfiles/results/nhwhitevap_all.xlsx")

# p1 race ####
nhb <- fixest::feols(pctnhblack_p1 ~ as.factor(period)*as.factor(annexing) + pctnhblack_p0 + pctnhblack_total | plid, data = panel0020_did %>% filter(nhblack_total >= 1))
summary(nhb)
hisp <- fixest::feols(pcth_p1 ~ as.factor(period)*as.factor(annexing) + pcth_p0 + pcth_total | plid, data = panel0020_did %>% filter(h_total >= 1))
summary(hisp)
nhw <- fixest::feols(pctnhwhite_p1 ~ as.factor(period)*as.factor(annexing) + pctnhwhite_p0 + pctnhwhite_total | plid, data = panel0020_did %>% filter(nhwhite_total >= 1))
summary(nhw)
race_all <- list(tidy(nhb), tidy(hisp), tidy(nhw))
openxlsx::write.xlsx(race_all, "analyticalfiles/results/race_all.xlsx")

# vap 
nhb <- fixest::feols(pctnhblackvap_p1 ~ as.factor(period)*as.factor(annexing) + pctnhblackvap_p0 + pctnhblackvap_total | plid, data = panel0020_did %>% filter(nhblackvap_total >= 1))
summary(nhb)
hisp <- fixest::feols(pcthispvap_p1 ~ as.factor(period)*as.factor(annexing) + pcthispvap_p0 + pcthvap_total | plid, data = panel0020_did %>% filter(hvap_total >= 1))
summary(hisp)
nhw <- fixest::feols(pctnhwhitevap_p1 ~ as.factor(period)*as.factor(annexing) + pctnhwhitevap_p0 + pctnhwhitevap_total | plid, data = panel0020_did %>% filter(nhwhitevap_total >= 1))
summary(nhw)
race_all <- list(tidy(nhb), tidy(hisp), tidy(nhw))
openxlsx::write.xlsx(race_all, "analyticalfiles/results/racevap_all.xlsx")