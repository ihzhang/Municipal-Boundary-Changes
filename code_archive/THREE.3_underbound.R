# -------------------------------------------------------------------------
# Created by: Iris Zhang                     
# Date created: 2021             
# Last revised: 6/30/2022             
# Project: MBC         
# Subproject: Analysis
# Re: Run analysis using BAS-validated version of annexing places        
# -------------------------------------------------------------------------

rm(list = ls())
# Script Description ------------------------------------------------------
# Manually identified annexations appear to be unreliable, especially for cross-boundary years, 
# Inputs:
# 

# Outputs:
# 

# Updates log: 
# #6/30: add 2019 

# Setup -------------------------------------------------------------------

# Packages: 


# Directories: 
setwd("~/Google Drive/My Drive/Stanford/QE2")

# homedir <- The smallest directory that contains all input/output folders.
# workdir <- The smallest directory that contains all necessary inputs.
# savedir <- The smallest directory that contains all output folders.
# setwd(paste0(homedir, workdir))

# Import data: 

# Parameters:

# Main Script -------------------------------------------------------------

# get environment ready 
library("stringr")
library("tidyverse")
library("dplyr")
library("stargazer")
library("fixest")
library("readr")
library("data.table")
library("magrittr")
library("openxlsx")
library("broom")
library("sjPlot")

panel0020_did <- read_csv("analyticalfiles/panel_prestandard.csv")

summary(panel0020_did$popgrowth)
panel0020_did %<>%
  mutate_at(vars(c(ends_with("total"), ends_with("_p0"), ends_with("_p1"), ends_with("_total_1"), contains("growth"), contains("_annexed"), ends_with("_log"), contains("diff"))), 
            ~((.-mean(., na.rm = T))/sd(., na.rm = T))) 
summary(panel0020_did$popgrowth)
summary(panel0020_did)

# models ####
outcomes <- c("", "_hpct", "_1pct", "_3pct", "_10pct")
races <- c("black", "hisp", "native", "asian", "nhwhite", "other")

#annex or not ####
# lagged value 
panel0020_did <- panel0020_did %>%
  group_by(plid) %>% 
  arrange(plid, time) %>% 
  mutate(lag_annexed = dplyr::lag(annexing, 1, NA), 
         lag_annexed = ifelse(is.na(lag_annexed), 0, lag_annexed)) %>%
  ungroup() #%>%
  filter(time != "2000 to 2007")

table(panel0020_did$lag_annexed, exclude = NULL)  

black <- feols(underbound_black ~ as.factor(vra)*as.factor(period) | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid, fixef.rm = "none")
summary(black)

black_twop <- feols(underbound_black ~ as.factor(vra)*as.factor(period) + pop_p0 + popdensity_p0 + popgrowth + pctnhblack_p0 + pctnhblackgrowth + pctnbmin_p0 + pctnbmingrowth + pctblackpov_p0 + pctnbminpov_p0 + pctowneroccupied_p0 + mhmval_p0 + hinc_p0 + pctpov_p0 + as.factor(more_white) + pctnhblack_total + pctnbmin_total + pctownerocc_total + pcthincjobs_total + pctincopp_total | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid, fixef.rm = "none")
summary(black_twop)

black_hpct <- feols(underbound_black_hpct ~ as.factor(vra)*as.factor(period) | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid, fixef.rm = "none")
summary(black_hpct)

black_twop_hpct <- feols(underbound_black_hpct ~ as.factor(vra)*as.factor(period) + pop_p0 + popdensity_p0 + popgrowth + pctnhblack_p0 + pctnhblackgrowth + pctnbmin_p0 + pctnbmingrowth + pctblackpov_p0 + pctnbminpov_p0 + pctowneroccupied_p0 + mhmval_p0 + hinc_p0 + pctpov_p0 + as.factor(more_white) + pctnhblack_total + pctnbmin_total + pctownerocc_total + pcthincjobs_total + pctincopp_total | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid, fixef.rm = "none")
summary(black_twop_hpct)

white <- feols(underbound_nhwhite ~ as.factor(vra)*as.factor(period) | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid, fixef.rm = "none")
summary(white)

white_twop <- feols(underbound_nhwhite ~ as.factor(vra)*as.factor(period) + pop_p0 + popdensity_p0 + popgrowth + pctnhblack_p0 + pctnhblackgrowth + pctnbmin_p0 + pctnbmingrowth + pctblackpov_p0 + pctnbminpov_p0 + pctowneroccupied_p0 + mhmval_p0 + hinc_p0 + pctpov_p0 + as.factor(more_white) + pctnhblack_total + pctnbmin_total + pctownerocc_total + pcthincjobs_total + pctincopp_total | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid, fixef.rm = "none")
summary(white_twop)

white_hpct <- feols(underbound_nhwhite_hpct ~ as.factor(vra)*as.factor(period) | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid, fixef.rm = "none")
summary(white_hpct)

white_twop_hpct <- feols(underbound_nhwhite_hpct ~ as.factor(vra)*as.factor(period) + pop_p0 + popdensity_p0 + popgrowth + pctnhblack_p0 + pctnhblackgrowth + pctnbmin_p0 + pctnbmingrowth + pctblackpov_p0 + pctnbminpov_p0 + pctowneroccupied_p0 + mhmval_p0 + hinc_p0 + pctpov_p0 + as.factor(more_white) + pctnhblack_total + pctnbmin_total + pctownerocc_total + pcthincjobs_total + pctincopp_total | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid, fixef.rm = "none")
summary(white_twop_hpct)

nbm <- feols(underbound_nbmin ~ as.factor(vra)*as.factor(period) | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid, fixef.rm = "none")
summary(nbm)

nbm_twop <- feols(underbound_nbmin ~ as.factor(vra)*as.factor(period) + pop_p0 + popdensity_p0 + popgrowth + pctnhblack_p0 + pctnhblackgrowth + pctnbmin_p0 + pctnbmingrowth + pctblackpov_p0 + pctnbminpov_p0 + pctowneroccupied_p0 + mhmval_p0 + hinc_p0 + pctpov_p0 + as.factor(more_white) + pctnhblack_total + pctnbmin_total + pctownerocc_total + pcthincjobs_total + pctincopp_total | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid, fixef.rm = "none")
summary(nbm_twop)

nbm_hpct <- feols(underbound_nbmin_hpct ~ as.factor(vra)*as.factor(period) | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid, fixef.rm = "none")
summary(nbm_hpct)

nbm_twop_hpct <- feols(underbound_nbmin_hpct ~ as.factor(vra)*as.factor(period) + pop_p0 + popdensity_p0 + popgrowth + pctnhblack_p0 + pctnhblackgrowth + pctnbmin_p0 + pctnbmingrowth + pctblackpov_p0 + pctnbminpov_p0 + pctowneroccupied_p0 + mhmval_p0 + hinc_p0 + pctpov_p0 + as.factor(more_white) + pctnhblack_total + pctnbmin_total + pctownerocc_total + pcthincjobs_total + pctincopp_total | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid, fixef.rm = "none")
summary(nbm_twop_hpct)

a <- tidy(black)
for (model_stat in names(glance(black))) {
  a[[model_stat]] <- glance(black)[[model_stat]]
}

b <- tidy(black_twop)
for (model_stat in names(glance(black_twop))) {
  b[[model_stat]] <- glance(black_twop)[[model_stat]]
}

c <- tidy(black_hpct)
for (model_stat in names(glance(black_hpct))) {
  c[[model_stat]] <- glance(black_hpct)[[model_stat]]
}

d <- tidy(black_twop_hpct)
for (model_stat in names(glance(black_twop_hpct))) {
  d[[model_stat]] <- glance(black_twop_hpct)[[model_stat]]
}

annex_list <- list(a, b, c, d)

openxlsx::write.xlsx(annex_list, "analyticalfiles/results/underbound_black.xlsx")

a <- tidy(white)
for (model_stat in names(glance(white))) {
  a[[model_stat]] <- glance(white)[[model_stat]]
}

b <- tidy(white_twop)
for (model_stat in names(glance(white_twop))) {
  b[[model_stat]] <- glance(white_twop)[[model_stat]]
}

c <- tidy(white_hpct)
for (model_stat in names(glance(white_hpct))) {
  c[[model_stat]] <- glance(white_hpct)[[model_stat]]
}

d <- tidy(white_twop_hpct)
for (model_stat in names(glance(white_twop_hpct))) {
  d[[model_stat]] <- glance(white_twop_hpct)[[model_stat]]
}

annex_list <- list(a, b, c, d)

openxlsx::write.xlsx(annex_list, "analyticalfiles/results/underbound_white.xlsx")

a <- tidy(nbm)
for (model_stat in names(glance(nbm))) {
  a[[model_stat]] <- glance(nbm)[[model_stat]]
}

b <- tidy(nbm_twop)
for (model_stat in names(glance(nbm_twop))) {
  b[[model_stat]] <- glance(nbm_twop)[[model_stat]]
}

c <- tidy(nbm_hpct)
for (model_stat in names(glance(nbm_hpct))) {
  c[[model_stat]] <- glance(nbm_hpct)[[model_stat]]
}

d <- tidy(nbm_twop_hpct)
for (model_stat in names(glance(nbm_twop_hpct))) {
  d[[model_stat]] <- glance(nbm_twop_hpct)[[model_stat]]
}

annex_list <- list(a, b, c, d)

openxlsx::write.xlsx(annex_list, "analyticalfiles/results/underbound_nbm.xlsx")
