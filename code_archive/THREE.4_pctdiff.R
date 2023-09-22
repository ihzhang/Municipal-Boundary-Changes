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
panel0020_did %<>%
  mutate_at(vars(c(ends_with("total"), ends_with("_p0"), ends_with("_p1"), ends_with("_total_1"), contains("growth"), contains("_annexed"), ends_with("_log"), contains("diff"))), 
            ~((.-mean(., na.rm = T))/sd(., na.rm = T))) 

# consequences of annexing ####
# ref: before Shelby, no VRA, not annexing ----
panel0020_did %<>%
  mutate(annexing = relevel(as.factor(annexing), ref = "0"),
         period = relevel(as.factor(period), ref = "0"),
         vra = relevel(as.factor(vra), ref = "0"))

nhb_base <- feols(pctnhblack_diff ~ as.factor(annexing)*as.factor(period)*as.factor(vra) | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid)
summary(nhb_base)

nhb_base_cov <- feols(pctnhblack_diff ~ as.factor(annexing)*as.factor(period)*as.factor(vra) + pctnhblackgrowth + pctnhblack_total | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid)
summary(nhb_base_cov) # N = 27987

nhw_base <- feols(pctnhwhite_diff ~ as.factor(annexing)*as.factor(vra)*as.factor(period) | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid)
summary(nhw_base) # N = 27987

nhw_base_cov <- feols(pctnhwhite_diff ~ as.factor(annexing)*as.factor(vra)*as.factor(period) + pctnhwhitegrowth + pctnhwhite_total | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid)
summary(nhw_base_cov) # N = 27987

nbmin_base <- feols(pctnbmin_diff ~ as.factor(annexing)*as.factor(vra)*as.factor(period) | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid)
summary(nbmin_base) # N = 27987

nbmin_base_cov <- feols(pctnbmin_diff ~ as.factor(annexing)*as.factor(vra)*as.factor(period) + pctnbmingrowth + pctnbmin_total | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid)
summary(nbmin_base_cov) # N = 27987

p1_diff <- list(tidy(nhb_base), tidy(nhb_base_cov), tidy(nhw_base), tidy(nhw_base_cov), tidy(nbmin_base), tidy(nbmin_base_cov))
p1_diff_glance <- list(nhb_base, nhb_base_cov, nhw_base, nhw_base_cov, nbmin_base, nbmin_base_cov)

for (model_stat in names(glance(nhb_base))) {
  for (i in 1:6) {
    p1_diff[[i]][[model_stat]] <- glance(p1_diff_glance[[i]])[[model_stat]]
  }
}
openxlsx::write.xlsx(p1_diff, "analyticalfiles/results/p1_pre-Shelby_uncovered_diff.xlsx")

# ref: before Shelby, VRA, not annexing ----
panel0020_did %<>%
  mutate(annexing = relevel(as.factor(annexing), ref = "0"),
         period = relevel(as.factor(period), ref = "0"),
         vra = relevel(as.factor(vra), ref = "1"))

nhb_base <- feols(pctnhblack_diff ~ as.factor(annexing)*as.factor(period)*as.factor(vra) | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid)
summary(nhb_base)

nhb_base_cov <- feols(pctnhblack_diff ~ as.factor(annexing)*as.factor(period)*as.factor(vra) + pctnhblackgrowth + pctnhblack_total | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid)
summary(nhb_base_cov) # N = 27987

nhw_base <- feols(pctnhwhite_diff ~ as.factor(annexing)*as.factor(vra)*as.factor(period) | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid)
summary(nhw_base) # N = 27987

nhw_base_cov <- feols(pctnhwhite_diff ~ as.factor(annexing)*as.factor(vra)*as.factor(period) + pctnhwhitegrowth + pctnhwhite_total | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid)
summary(nhw_base_cov) # N = 27987

nbmin_base <- feols(pctnbmin_diff ~ as.factor(annexing)*as.factor(vra)*as.factor(period) | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid)
summary(nbmin_base) # N = 27987

nbmin_base_cov <- feols(pctnbmin_diff ~ as.factor(annexing)*as.factor(vra)*as.factor(period) + pctnbmingrowth + pctnbmin_total | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid)
summary(nbmin_base_cov) # N = 27987

p1_diff <- list(tidy(nhb_base), tidy(nhb_base_cov), tidy(nhw_base), tidy(nhw_base_cov), tidy(nbmin_base), tidy(nbmin_base_cov))
p1_diff_glance <- list(nhb_base, nhb_base_cov, nhw_base, nhw_base_cov, nbmin_base, nbmin_base_cov)

for (model_stat in names(glance(nhb_base))) {
  for (i in 1:6) {
    p1_diff[[i]][[model_stat]] <- glance(p1_diff_glance[[i]])[[model_stat]]
  }
}
openxlsx::write.xlsx(p1_diff, "analyticalfiles/results/p1_pre-Shelby_covered_diff.xlsx")

# ref: after Shelby, VRA, not Annexing ---- 
panel0020_did %<>%
  mutate(annexing = relevel(as.factor(annexing), ref = "0"), 
         period = relevel(as.factor(period), ref = "1"),
         vra = relevel(as.factor(vra), ref = "1"))

nhb_base <- feols(pctnhblack_diff ~ as.factor(annexing)*as.factor(period)*as.factor(vra) | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid)
summary(nhb_base)

nhb_base_cov <- feols(pctnhblack_diff ~ as.factor(annexing)*as.factor(period)*as.factor(vra) + pctnhblackgrowth + pctnhblack_total | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid)
summary(nhb_base_cov) # N = 27987

nhw_base <- feols(pctnhwhite_diff ~ as.factor(annexing)*as.factor(vra)*as.factor(period) | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid)
summary(nhw_base) # N = 27987

nhw_base_cov <- feols(pctnhwhite_diff ~ as.factor(annexing)*as.factor(vra)*as.factor(period) + pctnhwhitegrowth + pctnhwhite_total | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid)
summary(nhw_base_cov) # N = 27987

nbmin_base <- feols(pctnbmin_diff ~ as.factor(annexing)*as.factor(vra)*as.factor(period) | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid)
summary(nbmin_base) # N = 27987

nbmin_base_cov <- feols(pctnbmin_diff ~ as.factor(annexing)*as.factor(vra)*as.factor(period) + pctnbmingrowth + pctnbmin_total | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid)
summary(nbmin_base_cov) # N = 27987

p1_diff <- list(tidy(nhb_base), tidy(nhb_base_cov), tidy(nhw_base), tidy(nhw_base_cov), tidy(nbmin_base), tidy(nbmin_base_cov))
p1_diff_glance <- list(nhb_base, nhb_base_cov, nhw_base, nhw_base_cov, nbmin_base, nbmin_base_cov)

for (model_stat in names(glance(nhb_base))) {
  for (i in 1:6) {
    p1_diff[[i]][[model_stat]] <- glance(p1_diff_glance[[i]])[[model_stat]]
  }
}
openxlsx::write.xlsx(p1_diff, "analyticalfiles/results/p1_post-Shelby_covered_diff.xlsx")

# ref: after Shelby, non-Section V, non-annexing ----
panel0020_did %<>%
  mutate(annexing = relevel(as.factor(annexing), ref = "0"), 
         period = relevel(as.factor(period), ref = "1"),
         vra = relevel(as.factor(vra), ref = "0"))

nhb_base <- feols(pctnhblack_diff ~ as.factor(annexing)*as.factor(period)*as.factor(vra) | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid)
summary(nhb_base)

nhb_base_cov <- feols(pctnhblack_diff ~ as.factor(annexing)*as.factor(period)*as.factor(vra) + pctnhblackgrowth + pctnhblack_total | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid)
summary(nhb_base_cov) # N = 27987

nhw_base <- feols(pctnhwhite_diff ~ as.factor(annexing)*as.factor(vra)*as.factor(period) | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid)
summary(nhw_base) # N = 27987

nhw_base_cov <- feols(pctnhwhite_diff ~ as.factor(annexing)*as.factor(vra)*as.factor(period) + pctnhwhitegrowth + pctnhwhite_total | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid)
summary(nhw_base_cov) # N = 27987

nbmin_base <- feols(pctnbmin_diff ~ as.factor(annexing)*as.factor(vra)*as.factor(period) | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid)
summary(nbmin_base) # N = 27987

nbmin_base_cov <- feols(pctnbmin_diff ~ as.factor(annexing)*as.factor(vra)*as.factor(period) + pctnbmingrowth + pctnbmin_total | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid)
summary(nbmin_base_cov) # N = 27987

p1_diff <- list(tidy(nhb_base), tidy(nhb_base_cov), tidy(nhw_base), tidy(nhw_base_cov), tidy(nbmin_base), tidy(nbmin_base_cov))
p1_diff_glance <- list(nhb_base, nhb_base_cov, nhw_base, nhw_base_cov, nbmin_base, nbmin_base_cov)

for (model_stat in names(glance(nhb_base))) {
  for (i in 1:6) {
    p1_diff[[i]][[model_stat]] <- glance(p1_diff_glance[[i]])[[model_stat]]
  }
}
openxlsx::write.xlsx(p1_diff, "analyticalfiles/results/p1_post-Shelby_uncovered_diff.xlsx")

# is it just an anticipation effect? ----
plids <- Reduce(intersect, list(unique(panel0020_did$plid[panel0020_did$time == "2000 to 2007"]), unique(panel0020_did$plid[panel0020_did$time == "2014 to 2020"])))

panel0020_did %<>%
  filter(plid %in% plids)

# ref: pre, no VRA, no annexing ----
panel0020_did %<>%
  mutate(vra = relevel(as.factor(vra), ref = "0"),
         period = relevel(as.factor(period), ref = "0"), 
         annexing = relevel(as.factor(annexing), ref = "0"))

nhb_base <- feols(pctnhblack_diff ~ as.factor(annexing)*as.factor(period)*as.factor(vra) | plid, data = panel0020_did %>% filter(time %in% c("2000 to 2007", "2014 to 2020")), cluster = ~plid)
summary(nhb_base)

nhb_base_cov <- feols(pctnhblack_diff ~ as.factor(annexing)*as.factor(period)*as.factor(vra) + pctnhblackgrowth + pctnhblack_total | plid, data = panel0020_did %>% filter(time %in% c("2000 to 2007", "2014 to 2020")), cluster = ~plid)
summary(nhb_base_cov) # N = 27987

nhw_base <- feols(pctnhwhite_diff ~ as.factor(annexing)*as.factor(vra)*as.factor(period) | plid, data = panel0020_did %>% filter(time %in% c("2000 to 2007", "2014 to 2020")), cluster = ~plid)
summary(nhw_base) # N = 27987

nhw_base_cov <- feols(pctnhwhite_diff ~ as.factor(annexing)*as.factor(vra)*as.factor(period) + pctnhwhitegrowth + pctnhwhite_total | plid, data = panel0020_did %>% filter(time %in% c("2000 to 2007", "2014 to 2020")), cluster = ~plid)
summary(nhw_base_cov) # N = 27987

nbmin_base <- feols(pctnbmin_diff ~ as.factor(annexing)*as.factor(vra)*as.factor(period) | plid, data = panel0020_did %>% filter(time %in% c("2000 to 2007", "2014 to 2020")), cluster = ~plid)
summary(nbmin_base) # N = 27987

nbmin_base_cov <- feols(pctnbmin_diff ~ as.factor(annexing)*as.factor(vra)*as.factor(period) + pctnbmingrowth + pctnbmin_total | plid, data = panel0020_did %>% filter(time %in% c("2000 to 2007", "2014 to 2020")), cluster = ~plid)
summary(nbmin_base_cov) # N = 27987

p1_diff <- list(tidy(nhb_base), tidy(nhb_base_cov), tidy(nhw_base), tidy(nhw_base_cov), tidy(nbmin_base), tidy(nbmin_base_cov))
p1_diff_glance <- list(nhb_base, nhb_base_cov, nhw_base, nhw_base_cov, nbmin_base, nbmin_base_cov)

for (model_stat in names(glance(nhb_base))) {
  for (i in 1:6) {
    p1_diff[[i]][[model_stat]] <- glance(p1_diff_glance[[i]])[[model_stat]]
  }
}

openxlsx::write.xlsx(p1_diff, "analyticalfiles/results/0007_pre_uncovered_diff.xlsx")

# ref: post, no VRA, no annexing ----
panel0020_did %<>%
  mutate(vra = relevel(as.factor(vra), ref = "0"),
         period = relevel(as.factor(period), ref = "1"))

nhb_base <- feols(pctnhblack_diff ~ as.factor(annexing)*as.factor(period)*as.factor(vra) | plid, data = panel0020_did %>% filter(time %in% c("2000 to 2007", "2014 to 2020")), cluster = ~plid)
summary(nhb_base)

nhb_base_cov <- feols(pctnhblack_diff ~ as.factor(annexing)*as.factor(period)*as.factor(vra) + pctnhblackgrowth + pctnhblack_total | plid, data = panel0020_did %>% filter(time %in% c("2000 to 2007", "2014 to 2020")), cluster = ~plid)
summary(nhb_base_cov) # N = 27987

nhw_base <- feols(pctnhwhite_diff ~ as.factor(annexing)*as.factor(vra)*as.factor(period) | plid, data = panel0020_did %>% filter(time %in% c("2000 to 2007", "2014 to 2020")), cluster = ~plid)
summary(nhw_base) # N = 27987

nhw_base_cov <- feols(pctnhwhite_diff ~ as.factor(annexing)*as.factor(vra)*as.factor(period) + pctnhwhitegrowth + pctnhwhite_total | plid, data = panel0020_did %>% filter(time %in% c("2000 to 2007", "2014 to 2020")), cluster = ~plid)
summary(nhw_base_cov) # N = 27987

nbmin_base <- feols(pctnbmin_diff ~ as.factor(annexing)*as.factor(vra)*as.factor(period) | plid, data = panel0020_did %>% filter(time %in% c("2000 to 2007", "2014 to 2020")), cluster = ~plid)
summary(nbmin_base) # N = 27987

nbmin_base_cov <- feols(pctnbmin_diff ~ as.factor(annexing)*as.factor(vra)*as.factor(period) + pctnbmingrowth + pctnbmin_total | plid, data = panel0020_did %>% filter(time %in% c("2000 to 2007", "2014 to 2020")), cluster = ~plid)
summary(nbmin_base_cov) # N = 27987

p1_diff <- list(tidy(nhb_base), tidy(nhb_base_cov), tidy(nhw_base), tidy(nhw_base_cov), tidy(nbmin_base), tidy(nbmin_base_cov))
p1_diff_glance <- list(nhb_base, nhb_base_cov, nhw_base, nhw_base_cov, nbmin_base, nbmin_base_cov)

for (model_stat in names(glance(nhb_base))) {
  for (i in 1:6) {
    p1_diff[[i]][[model_stat]] <- glance(p1_diff_glance[[i]])[[model_stat]]
  }
}

openxlsx::write.xlsx(p1_diff, "analyticalfiles/results/0007_post_uncovered_diff.xlsx")

# ref: pre, VRA, no annexing ----
panel0020_did %<>%
  mutate(vra = relevel(as.factor(vra), ref = "1"),
         period = relevel(as.factor(period), ref = "0"))

nhb_base <- feols(pctnhblack_diff ~ as.factor(annexing)*as.factor(period)*as.factor(vra) | plid, data = panel0020_did %>% filter(time %in% c("2000 to 2007", "2014 to 2020")), cluster = ~plid)
summary(nhb_base)

nhb_base_cov <- feols(pctnhblack_diff ~ as.factor(annexing)*as.factor(period)*as.factor(vra) + pctnhblackgrowth + pctnhblack_total | plid, data = panel0020_did %>% filter(time %in% c("2000 to 2007", "2014 to 2020")), cluster = ~plid)
summary(nhb_base_cov) # N = 27987

nhw_base <- feols(pctnhwhite_diff ~ as.factor(annexing)*as.factor(vra)*as.factor(period) | plid, data = panel0020_did %>% filter(time %in% c("2000 to 2007", "2014 to 2020")), cluster = ~plid)
summary(nhw_base) # N = 27987

nhw_base_cov <- feols(pctnhwhite_diff ~ as.factor(annexing)*as.factor(vra)*as.factor(period) + pctnhwhitegrowth + pctnhwhite_total | plid, data = panel0020_did %>% filter(time %in% c("2000 to 2007", "2014 to 2020")), cluster = ~plid)
summary(nhw_base_cov) # N = 27987

nbmin_base <- feols(pctnbmin_diff ~ as.factor(annexing)*as.factor(vra)*as.factor(period) | plid, data = panel0020_did %>% filter(time %in% c("2000 to 2007", "2014 to 2020")), cluster = ~plid)
summary(nbmin_base) # N = 27987

nbmin_base_cov <- feols(pctnbmin_diff ~ as.factor(annexing)*as.factor(vra)*as.factor(period) + pctnbmingrowth + pctnbmin_total | plid, data = panel0020_did %>% filter(time %in% c("2000 to 2007", "2014 to 2020")), cluster = ~plid)
summary(nbmin_base_cov) # N = 27987

p1_diff <- list(tidy(nhb_base), tidy(nhb_base_cov), tidy(nhw_base), tidy(nhw_base_cov), tidy(nbmin_base), tidy(nbmin_base_cov))
p1_diff_glance <- list(nhb_base, nhb_base_cov, nhw_base, nhw_base_cov, nbmin_base, nbmin_base_cov)

for (model_stat in names(glance(nhb_base))) {
  for (i in 1:6) {
    p1_diff[[i]][[model_stat]] <- glance(p1_diff_glance[[i]])[[model_stat]]
  }
}

openxlsx::write.xlsx(p1_diff, "analyticalfiles/results/0007_pre_covered_diff.xlsx")

# ref: post, VRA, no annexing ----
panel0020_did %<>%
  mutate(vra = relevel(as.factor(vra), ref = "1"),
         period = relevel(as.factor(period), ref = "1"))

nhb_base <- feols(pctnhblack_diff ~ as.factor(annexing)*as.factor(period)*as.factor(vra) | plid, data = panel0020_did %>% filter(time %in% c("2000 to 2007", "2014 to 2020")), cluster = ~plid)
summary(nhb_base)

nhb_base_cov <- feols(pctnhblack_diff ~ as.factor(annexing)*as.factor(period)*as.factor(vra) + pctnhblackgrowth + pctnhblack_total | plid, data = panel0020_did %>% filter(time %in% c("2000 to 2007", "2014 to 2020")), cluster = ~plid)
summary(nhb_base_cov) # N = 27987

nhw_base <- feols(pctnhwhite_diff ~ as.factor(annexing)*as.factor(vra)*as.factor(period) | plid, data = panel0020_did %>% filter(time %in% c("2000 to 2007", "2014 to 2020")), cluster = ~plid)
summary(nhw_base) # N = 27987

nhw_base_cov <- feols(pctnhwhite_diff ~ as.factor(annexing)*as.factor(vra)*as.factor(period) + pctnhwhitegrowth + pctnhwhite_total | plid, data = panel0020_did %>% filter(time %in% c("2000 to 2007", "2014 to 2020")), cluster = ~plid)
summary(nhw_base_cov) # N = 27987

nbmin_base <- feols(pctnbmin_diff ~ as.factor(annexing)*as.factor(vra)*as.factor(period) | plid, data = panel0020_did %>% filter(time %in% c("2000 to 2007", "2014 to 2020")), cluster = ~plid)
summary(nbmin_base) # N = 27987

nbmin_base_cov <- feols(pctnbmin_diff ~ as.factor(annexing)*as.factor(vra)*as.factor(period) + pctnbmingrowth + pctnbmin_total | plid, data = panel0020_did %>% filter(time %in% c("2000 to 2007", "2014 to 2020")), cluster = ~plid)
summary(nbmin_base_cov) # N = 27987

p1_diff <- list(tidy(nhb_base), tidy(nhb_base_cov), tidy(nhw_base), tidy(nhw_base_cov), tidy(nbmin_base), tidy(nbmin_base_cov))
p1_diff_glance <- list(nhb_base, nhb_base_cov, nhw_base, nhw_base_cov, nbmin_base, nbmin_base_cov)

for (model_stat in names(glance(nhb_base))) {
  for (i in 1:6) {
    p1_diff[[i]][[model_stat]] <- glance(p1_diff_glance[[i]])[[model_stat]]
  }
}

openxlsx::write.xlsx(p1_diff, "analyticalfiles/results/0007_post_covered_diff.xlsx")

# do again for non-validated sample...----
# consequences of annexing ####
# ref: before Shelby, no VRA, not annexing ----
panel0020_did <- read_csv("analyticalfiles/panel_prestandard.csv")
panel0020_did %<>%
  mutate(annexing_bas = relevel(as.factor(annexing_bas), ref = "0"),
         period = relevel(as.factor(period), ref = "0"),
         vra = relevel(as.factor(vra), ref = "0"))

nhb_base <- feols(pctnhblack_diff ~ as.factor(annexing_bas)*as.factor(period)*as.factor(vra) | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid)
summary(nhb_base)

nhb_base_cov <- feols(pctnhblack_diff ~ as.factor(annexing_bas)*as.factor(period)*as.factor(vra) + pctnhblackgrowth + pctnhblack_total | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid)
summary(nhb_base_cov) # N = 27987

nhw_base <- feols(pctnhwhite_diff ~ as.factor(annexing_bas)*as.factor(vra)*as.factor(period) | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid)
summary(nhw_base) # N = 27987

nhw_base_cov <- feols(pctnhwhite_diff ~ as.factor(annexing_bas)*as.factor(vra)*as.factor(period) + pctnhwhitegrowth + pctnhwhite_total | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid)
summary(nhw_base_cov) # N = 27987

nbmin_base <- feols(pctnbmin_diff ~ as.factor(annexing_bas)*as.factor(vra)*as.factor(period) | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid)
summary(nbmin_base) # N = 27987

nbmin_base_cov <- feols(pctnbmin_diff ~ as.factor(annexing_bas)*as.factor(vra)*as.factor(period) + pctnbmingrowth + pctnbmin_total | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid)
summary(nbmin_base_cov) # N = 27987

p1_diff <- list(tidy(nhb_base), tidy(nhb_base_cov), tidy(nhw_base), tidy(nhw_base_cov), tidy(nbmin_base), tidy(nbmin_base_cov))
p1_diff_glance <- list(nhb_base, nhb_base_cov, nhw_base, nhw_base_cov, nbmin_base, nbmin_base_cov)

for (model_stat in names(glance(nhb_base))) {
  for (i in 1:6) {
    p1_diff[[i]][[model_stat]] <- glance(p1_diff_glance[[i]])[[model_stat]]
  }
}
openxlsx::write.xlsx(p1_diff, "analyticalfiles/results/bas/p1_pre-Shelby_uncovered_diff.xlsx")

# ref: before Shelby, VRA, not annexing_bas ----
panel0020_did %<>%
  mutate(annexing_bas = relevel(as.factor(annexing_bas), ref = "0"),
         period = relevel(as.factor(period), ref = "0"),
         vra = relevel(as.factor(vra), ref = "1"))

nhb_base <- feols(pctnhblack_diff ~ as.factor(annexing_bas)*as.factor(period)*as.factor(vra) | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid)
summary(nhb_base)

nhb_base_cov <- feols(pctnhblack_diff ~ as.factor(annexing_bas)*as.factor(period)*as.factor(vra) + pctnhblackgrowth + pctnhblack_total | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid)
summary(nhb_base_cov) # N = 27987

nhw_base <- feols(pctnhwhite_diff ~ as.factor(annexing_bas)*as.factor(vra)*as.factor(period) | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid)
summary(nhw_base) # N = 27987

nhw_base_cov <- feols(pctnhwhite_diff ~ as.factor(annexing_bas)*as.factor(vra)*as.factor(period) + pctnhwhitegrowth + pctnhwhite_total | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid)
summary(nhw_base_cov) # N = 27987

nbmin_base <- feols(pctnbmin_diff ~ as.factor(annexing_bas)*as.factor(vra)*as.factor(period) | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid)
summary(nbmin_base) # N = 27987

nbmin_base_cov <- feols(pctnbmin_diff ~ as.factor(annexing_bas)*as.factor(vra)*as.factor(period) + pctnbmingrowth + pctnbmin_total | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid)
summary(nbmin_base_cov) # N = 27987

p1_diff <- list(tidy(nhb_base), tidy(nhb_base_cov), tidy(nhw_base), tidy(nhw_base_cov), tidy(nbmin_base), tidy(nbmin_base_cov))
p1_diff_glance <- list(nhb_base, nhb_base_cov, nhw_base, nhw_base_cov, nbmin_base, nbmin_base_cov)

for (model_stat in names(glance(nhb_base))) {
  for (i in 1:6) {
    p1_diff[[i]][[model_stat]] <- glance(p1_diff_glance[[i]])[[model_stat]]
  }
}
openxlsx::write.xlsx(p1_diff, "analyticalfiles/results/bas/p1_pre-Shelby_covered_diff.xlsx")

# ref: after Shelby, VRA, not annexing_bas ---- 
panel0020_did %<>%
  mutate(annexing_bas = relevel(as.factor(annexing_bas), ref = "0"), 
         period = relevel(as.factor(period), ref = "1"),
         vra = relevel(as.factor(vra), ref = "1"))

nhb_base <- feols(pctnhblack_diff ~ as.factor(annexing_bas)*as.factor(period)*as.factor(vra) | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid)
summary(nhb_base)

nhb_base_cov <- feols(pctnhblack_diff ~ as.factor(annexing_bas)*as.factor(period)*as.factor(vra) + pctnhblackgrowth + pctnhblack_total | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid)
summary(nhb_base_cov) # N = 27987

nhw_base <- feols(pctnhwhite_diff ~ as.factor(annexing_bas)*as.factor(vra)*as.factor(period) | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid)
summary(nhw_base) # N = 27987

nhw_base_cov <- feols(pctnhwhite_diff ~ as.factor(annexing_bas)*as.factor(vra)*as.factor(period) + pctnhwhitegrowth + pctnhwhite_total | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid)
summary(nhw_base_cov) # N = 27987

nbmin_base <- feols(pctnbmin_diff ~ as.factor(annexing_bas)*as.factor(vra)*as.factor(period) | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid)
summary(nbmin_base) # N = 27987

nbmin_base_cov <- feols(pctnbmin_diff ~ as.factor(annexing_bas)*as.factor(vra)*as.factor(period) + pctnbmingrowth + pctnbmin_total | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid)
summary(nbmin_base_cov) # N = 27987

p1_diff <- list(tidy(nhb_base), tidy(nhb_base_cov), tidy(nhw_base), tidy(nhw_base_cov), tidy(nbmin_base), tidy(nbmin_base_cov))
p1_diff_glance <- list(nhb_base, nhb_base_cov, nhw_base, nhw_base_cov, nbmin_base, nbmin_base_cov)

for (model_stat in names(glance(nhb_base))) {
  for (i in 1:6) {
    p1_diff[[i]][[model_stat]] <- glance(p1_diff_glance[[i]])[[model_stat]]
  }
}
openxlsx::write.xlsx(p1_diff, "analyticalfiles/results/bas/p1_post-Shelby_covered_diff.xlsx")

# ref: after Shelby, non-Section V, non-annexing_bas ----
panel0020_did %<>%
  mutate(annexing_bas = relevel(as.factor(annexing_bas), ref = "0"), 
         period = relevel(as.factor(period), ref = "1"),
         vra = relevel(as.factor(vra), ref = "0"))

nhb_base <- feols(pctnhblack_diff ~ as.factor(annexing_bas)*as.factor(period)*as.factor(vra) | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid)
summary(nhb_base)

nhb_base_cov <- feols(pctnhblack_diff ~ as.factor(annexing_bas)*as.factor(period)*as.factor(vra) + pctnhblackgrowth + pctnhblack_total | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid)
summary(nhb_base_cov) # N = 27987

nhw_base <- feols(pctnhwhite_diff ~ as.factor(annexing_bas)*as.factor(vra)*as.factor(period) | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid)
summary(nhw_base) # N = 27987

nhw_base_cov <- feols(pctnhwhite_diff ~ as.factor(annexing_bas)*as.factor(vra)*as.factor(period) + pctnhwhitegrowth + pctnhwhite_total | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid)
summary(nhw_base_cov) # N = 27987

nbmin_base <- feols(pctnbmin_diff ~ as.factor(annexing_bas)*as.factor(vra)*as.factor(period) | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid)
summary(nbmin_base) # N = 27987

nbmin_base_cov <- feols(pctnbmin_diff ~ as.factor(annexing_bas)*as.factor(vra)*as.factor(period) + pctnbmingrowth + pctnbmin_total | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid)
summary(nbmin_base_cov) # N = 27987

p1_diff <- list(tidy(nhb_base), tidy(nhb_base_cov), tidy(nhw_base), tidy(nhw_base_cov), tidy(nbmin_base), tidy(nbmin_base_cov))
p1_diff_glance <- list(nhb_base, nhb_base_cov, nhw_base, nhw_base_cov, nbmin_base, nbmin_base_cov)

for (model_stat in names(glance(nhb_base))) {
  for (i in 1:6) {
    p1_diff[[i]][[model_stat]] <- glance(p1_diff_glance[[i]])[[model_stat]]
  }
}
openxlsx::write.xlsx(p1_diff, "analyticalfiles/results/bas/p1_post-Shelby_uncovered_diff.xlsx")

# is it just an anticipation effect? ----
plids <- Reduce(intersect, list(unique(panel0020_did$plid[panel0020_did$time == "2000 to 2007"]), unique(panel0020_did$plid[panel0020_did$time == "2014 to 2020"])))
panel0020_did %<>%
  filter(plid %in% plids)

# ref: pre, no VRA, no annexing_bas ----
panel0020_did %<>%
  mutate(vra = relevel(as.factor(vra), ref = "0"),
         period = relevel(as.factor(period), ref = "0"), 
         annexing_bas = relevel(as.factor(annexing_bas), ref = "0"))

nhb_base <- feols(pctnhblack_diff ~ as.factor(annexing_bas)*as.factor(period)*as.factor(vra) | plid, data = panel0020_did %>% filter(time %in% c("2000 to 2007", "2014 to 2020")), cluster = ~plid)
summary(nhb_base)

nhb_base_cov <- feols(pctnhblack_diff ~ as.factor(annexing_bas)*as.factor(period)*as.factor(vra) + pctnhblackgrowth + pctnhblack_total | plid, data = panel0020_did %>% filter(time %in% c("2000 to 2007", "2014 to 2020")), cluster = ~plid)
summary(nhb_base_cov) # N = 27987

nhw_base <- feols(pctnhwhite_diff ~ as.factor(annexing_bas)*as.factor(vra)*as.factor(period) | plid, data = panel0020_did %>% filter(time %in% c("2000 to 2007", "2014 to 2020")), cluster = ~plid)
summary(nhw_base) # N = 27987

nhw_base_cov <- feols(pctnhwhite_diff ~ as.factor(annexing_bas)*as.factor(vra)*as.factor(period) + pctnhwhitegrowth + pctnhwhite_total | plid, data = panel0020_did %>% filter(time %in% c("2000 to 2007", "2014 to 2020")), cluster = ~plid)
summary(nhw_base_cov) # N = 27987

nbmin_base <- feols(pctnbmin_diff ~ as.factor(annexing_bas)*as.factor(vra)*as.factor(period) | plid, data = panel0020_did %>% filter(time %in% c("2000 to 2007", "2014 to 2020")), cluster = ~plid)
summary(nbmin_base) # N = 27987

nbmin_base_cov <- feols(pctnbmin_diff ~ as.factor(annexing_bas)*as.factor(vra)*as.factor(period) + pctnbmingrowth + pctnbmin_total | plid, data = panel0020_did %>% filter(time %in% c("2000 to 2007", "2014 to 2020")), cluster = ~plid)
summary(nbmin_base_cov) # N = 27987

p1_diff <- list(tidy(nhb_base), tidy(nhb_base_cov), tidy(nhw_base), tidy(nhw_base_cov), tidy(nbmin_base), tidy(nbmin_base_cov))
p1_diff_glance <- list(nhb_base, nhb_base_cov, nhw_base, nhw_base_cov, nbmin_base, nbmin_base_cov)

for (model_stat in names(glance(nhb_base))) {
  for (i in 1:6) {
    p1_diff[[i]][[model_stat]] <- glance(p1_diff_glance[[i]])[[model_stat]]
  }
}

openxlsx::write.xlsx(p1_diff, "analyticalfiles/results/bas/0007_pre_uncovered_diff.xlsx")

# ref: post, no VRA, no annexing_bas ----
panel0020_did %<>%
  mutate(vra = relevel(as.factor(vra), ref = "0"),
         period = relevel(as.factor(period), ref = "1"))

nhb_base <- feols(pctnhblack_diff ~ as.factor(annexing_bas)*as.factor(period)*as.factor(vra) | plid, data = panel0020_did %>% filter(time %in% c("2000 to 2007", "2014 to 2020")), cluster = ~plid)
summary(nhb_base)

nhb_base_cov <- feols(pctnhblack_diff ~ as.factor(annexing_bas)*as.factor(period)*as.factor(vra) + pctnhblackgrowth + pctnhblack_total | plid, data = panel0020_did %>% filter(time %in% c("2000 to 2007", "2014 to 2020")), cluster = ~plid)
summary(nhb_base_cov) # N = 27987

nhw_base <- feols(pctnhwhite_diff ~ as.factor(annexing_bas)*as.factor(vra)*as.factor(period) | plid, data = panel0020_did %>% filter(time %in% c("2000 to 2007", "2014 to 2020")), cluster = ~plid)
summary(nhw_base) # N = 27987

nhw_base_cov <- feols(pctnhwhite_diff ~ as.factor(annexing_bas)*as.factor(vra)*as.factor(period) + pctnhwhitegrowth + pctnhwhite_total | plid, data = panel0020_did %>% filter(time %in% c("2000 to 2007", "2014 to 2020")), cluster = ~plid)
summary(nhw_base_cov) # N = 27987

nbmin_base <- feols(pctnbmin_diff ~ as.factor(annexing_bas)*as.factor(vra)*as.factor(period) | plid, data = panel0020_did %>% filter(time %in% c("2000 to 2007", "2014 to 2020")), cluster = ~plid)
summary(nbmin_base) # N = 27987

nbmin_base_cov <- feols(pctnbmin_diff ~ as.factor(annexing_bas)*as.factor(vra)*as.factor(period) + pctnbmingrowth + pctnbmin_total | plid, data = panel0020_did %>% filter(time %in% c("2000 to 2007", "2014 to 2020")), cluster = ~plid)
summary(nbmin_base_cov) # N = 27987

p1_diff <- list(tidy(nhb_base), tidy(nhb_base_cov), tidy(nhw_base), tidy(nhw_base_cov), tidy(nbmin_base), tidy(nbmin_base_cov))
p1_diff_glance <- list(nhb_base, nhb_base_cov, nhw_base, nhw_base_cov, nbmin_base, nbmin_base_cov)

for (model_stat in names(glance(nhb_base))) {
  for (i in 1:6) {
    p1_diff[[i]][[model_stat]] <- glance(p1_diff_glance[[i]])[[model_stat]]
  }
}

openxlsx::write.xlsx(p1_diff, "analyticalfiles/results/bas/0007_post_uncovered_diff.xlsx")

# ref: pre, VRA, no annexing_bas ----
panel0020_did %<>%
  mutate(vra = relevel(as.factor(vra), ref = "1"),
         period = relevel(as.factor(period), ref = "0"))

nhb_base <- feols(pctnhblack_diff ~ as.factor(annexing_bas)*as.factor(period)*as.factor(vra) | plid, data = panel0020_did %>% filter(time %in% c("2000 to 2007", "2014 to 2020")), cluster = ~plid)
summary(nhb_base)

nhb_base_cov <- feols(pctnhblack_diff ~ as.factor(annexing_bas)*as.factor(period)*as.factor(vra) + pctnhblackgrowth + pctnhblack_total | plid, data = panel0020_did %>% filter(time %in% c("2000 to 2007", "2014 to 2020")), cluster = ~plid)
summary(nhb_base_cov) # N = 27987

nhw_base <- feols(pctnhwhite_diff ~ as.factor(annexing_bas)*as.factor(vra)*as.factor(period) | plid, data = panel0020_did %>% filter(time %in% c("2000 to 2007", "2014 to 2020")), cluster = ~plid)
summary(nhw_base) # N = 27987

nhw_base_cov <- feols(pctnhwhite_diff ~ as.factor(annexing_bas)*as.factor(vra)*as.factor(period) + pctnhwhitegrowth + pctnhwhite_total | plid, data = panel0020_did %>% filter(time %in% c("2000 to 2007", "2014 to 2020")), cluster = ~plid)
summary(nhw_base_cov) # N = 27987

nbmin_base <- feols(pctnbmin_diff ~ as.factor(annexing_bas)*as.factor(vra)*as.factor(period) | plid, data = panel0020_did %>% filter(time %in% c("2000 to 2007", "2014 to 2020")), cluster = ~plid)
summary(nbmin_base) # N = 27987

nbmin_base_cov <- feols(pctnbmin_diff ~ as.factor(annexing_bas)*as.factor(vra)*as.factor(period) + pctnbmingrowth + pctnbmin_total | plid, data = panel0020_did %>% filter(time %in% c("2000 to 2007", "2014 to 2020")), cluster = ~plid)
summary(nbmin_base_cov) # N = 27987

p1_diff <- list(tidy(nhb_base), tidy(nhb_base_cov), tidy(nhw_base), tidy(nhw_base_cov), tidy(nbmin_base), tidy(nbmin_base_cov))
p1_diff_glance <- list(nhb_base, nhb_base_cov, nhw_base, nhw_base_cov, nbmin_base, nbmin_base_cov)

for (model_stat in names(glance(nhb_base))) {
  for (i in 1:6) {
    p1_diff[[i]][[model_stat]] <- glance(p1_diff_glance[[i]])[[model_stat]]
  }
}

openxlsx::write.xlsx(p1_diff, "analyticalfiles/results/bas/0007_pre_covered_diff.xlsx")

# ref: post, VRA, no annexing_bas ----
panel0020_did %<>%
  mutate(vra = relevel(as.factor(vra), ref = "0"),
         period = relevel(as.factor(period), ref = "1"))

nhb_base <- feols(pctnhblack_diff ~ as.factor(annexing_bas)*as.factor(period)*as.factor(vra) | plid, data = panel0020_did %>% filter(time %in% c("2000 to 2007", "2014 to 2020")), cluster = ~plid)
summary(nhb_base)

nhb_base_cov <- feols(pctnhblack_diff ~ as.factor(annexing_bas)*as.factor(period)*as.factor(vra) + pctnhblackgrowth + pctnhblack_total | plid, data = panel0020_did %>% filter(time %in% c("2000 to 2007", "2014 to 2020")), cluster = ~plid)
summary(nhb_base_cov) # N = 27987

nhw_base <- feols(pctnhwhite_diff ~ as.factor(annexing_bas)*as.factor(vra)*as.factor(period) | plid, data = panel0020_did %>% filter(time %in% c("2000 to 2007", "2014 to 2020")), cluster = ~plid)
summary(nhw_base) # N = 27987

nhw_base_cov <- feols(pctnhwhite_diff ~ as.factor(annexing_bas)*as.factor(vra)*as.factor(period) + pctnhwhitegrowth + pctnhwhite_total | plid, data = panel0020_did %>% filter(time %in% c("2000 to 2007", "2014 to 2020")), cluster = ~plid)
summary(nhw_base_cov) # N = 27987

nbmin_base <- feols(pctnbmin_diff ~ as.factor(annexing_bas)*as.factor(vra)*as.factor(period) | plid, data = panel0020_did %>% filter(time %in% c("2000 to 2007", "2014 to 2020")), cluster = ~plid)
summary(nbmin_base) # N = 27987

nbmin_base_cov <- feols(pctnbmin_diff ~ as.factor(annexing_bas)*as.factor(vra)*as.factor(period) + pctnbmingrowth + pctnbmin_total | plid, data = panel0020_did %>% filter(time %in% c("2000 to 2007", "2014 to 2020")), cluster = ~plid)
summary(nbmin_base_cov) # N = 27987

p1_diff <- list(tidy(nhb_base), tidy(nhb_base_cov), tidy(nhw_base), tidy(nhw_base_cov), tidy(nbmin_base), tidy(nbmin_base_cov))
p1_diff_glance <- list(nhb_base, nhb_base_cov, nhw_base, nhw_base_cov, nbmin_base, nbmin_base_cov)

for (model_stat in names(glance(nhb_base))) {
  for (i in 1:6) {
    p1_diff[[i]][[model_stat]] <- glance(p1_diff_glance[[i]])[[model_stat]]
  }
}

openxlsx::write.xlsx(p1_diff, "analyticalfiles/results/bas/0007_post_covered_diff.xlsx")

