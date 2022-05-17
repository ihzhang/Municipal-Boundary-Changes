rm(list = ls())
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
  #filter(plid %in% plids) %>%
  select(all_of(names_list)) %>%
  filter(!duplicated(plid))
pl0713 %<>%
  #filter(plid %in% plids) %>%
  select(all_of(names_list))
pl1420 %<>%
  #filter(plid %in% plids) %>%
  select(all_of(names_list)) 

panel0020_did <- base::rbind(
  pl0007, pl0713, pl1420
)

rm(pl0007, pl0713, pl1420)

panel0020_did %<>% 
  mutate(vra = as.factor(vra),
         vra_real = ifelse(vra == 1 & post == 1, 0, vra),
         period = relevel(as.factor(post), ref = "-1"))

panel0020_did %<>%
  mutate_at(c("pctincocc_total", "pctownerocc_total", "pcthincjobs_total"), ~ifelse(!is.finite(.) | is.na(.), 0, .)) %>%
  mutate_at(c("pctowneroccupied_p0", "pctemp_p0"), ~ifelse(!is.finite(.) | is.na(.), 0, .))

panel0020_did %<>%
  mutate_at(vars("pop_total", "pop_p0", "pctnhwhite_p0", "pctincocc_total", "pctownerocc_total", "pcthincjobs_total", "pctnhblack_total",
              "pctnhblack_p0", "pctowneroccupied_p0", "mhmval_p0", "incomepp_p0", "popdensity_p0",
              "pctnhwhitevap_p0", "pctnhblackvap_total", "pctnhblackvap_p0", "pcth_total", "pcthvap_total",
              "pcth_p0", "pcthispvap_p0", "pctemp_p0", "hinc_p0", contains("growth")), 
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

nhw <- fixest::feols(annexing ~ nhwhitegrowth + nhblackgrowth + hgrowth + pop_p0 + popdensity_p0 + pctnhwhite_p0 + pctnhblack_p0 + pcth_p0 + pctemp_p0 + hinc_p0 + pctowneroccupied_p0 + mhmval_p0 + incomepp_p0 + pctnhwhite_total + pctnhblack_total + pcth_total + pctownerocc_total + pctincocc_total + pcthincjobs_total + vra_real | plid + period, data = panel0020_did)
sjPlot::plot_model(nhw, 
                   dot.size = 1,
                   show.values = TRUE)

annex <- tidy(nhw)
openxlsx::write.xlsx(annex, "analyticalfiles/results/annex.xlsx")

# race at p1
nhb <- fixest::feols(pctnhblack_p1 ~ pctnhblack_p0 + pctnhwhite_p0 + pcth_p0 + pctemp_p0 + hinc_p0 + pctowneroccupied_p0 + mhmval_p0 + incomepp_p0 + pctnhwhite_total + pctnhblack_total + pcth_total + pctownerocc_total + pctincocc_total + pcthincjobs_total + vra_real + annexing + nhwhitegrowth | plid + period, data = panel0020_did) 
summary(nhb)

nhb <- fixest::feols(pctnhblack_p1 ~ nhwhitegrowth + pctnhblack_p0 + pctnhwhite_p0 + hgrowth + pcth_p0 + vra_real + annexing*nhblackgrowth | plid + period, data = panel0020_did) 
summary(nhb)
sjPlot::plot_model(nhb, 
                   dot.size = 1,
                   show.values = TRUE)

hisp <- fixest::feols(pcth_p1 ~ as.factor(vra)*as.factor(period) + pcth_p0 + pcth_total | plid, data = panel0020_did %>% filter(annexing==1 & h_total >= 1))
summary(hisp)
nhw <- fixest::feols(pctnhwhite_p1 ~ as.factor(vra)*as.factor(period) + pctnhwhite_p0 + pctnhwhite_total | plid, data = panel0020_did %>% filter(annexing==1 & nhwhite_total >= 1))
summary(nhw)