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

# make panel data!!!!! ####
# take out ne, alaska, and hawaii
vraplids <- read_csv("analyticalfiles/vra_places.csv")
NE <- c("09", "23", "25", "33", "34", "36", "42", "44", "50")
bas <- read_csv("analyticalfiles/bas_years.csv")

pl0007 <- read_csv("analyticalfiles/pl_annex_var_0007.csv") %>%
  mutate(STATE = substr(plid, 1, 2)) %>%
  filter(!(STATE %in% NE)) 

pl0007 %<>%
  mutate(annexing_bas = ifelse(plid %in% bas$plid[bas$period=="2000 to 2007"], 1, 0),
         annexing_use = ifelse(annexing == 1 & annexing_bas == 1, 1, 0))
table(pl0007$annexing, pl0007$annexing_use)
2149/(2149 + 118)
# 94.5% agreement

pl0713 <- read_csv("analyticalfiles/pl_annex_var_0713.csv") %>%
  mutate(STATE = substr(plid, 1, 2)) %>%
  filter(!(STATE %in% NE)) 

pl0713 %<>%
  mutate(annexing_bas = ifelse(plid %in% bas$plid[bas$period=="2007 to 2013"], 1, 0),
         annexing_use = ifelse(annexing == 1 & annexing_bas == 1, 1, 0))
table(pl0713$annexing, pl0713$annexing_use)
2287/(2287+828)
#73.4%

pl1420 <- read_csv("analyticalfiles/pl_annex_var_1420.csv") %>%
  mutate(STATE = substr(plid, 1, 2)) %>%
  filter(!(STATE %in% NE)) 

pl1420 %<>%
  mutate(annexing_bas = ifelse(plid %in% bas$plid[bas$period=="2014 to 2020"], 1, 0),
         annexing_use = ifelse(annexing == 1 & annexing_bas == 1, 1, 0))
table(pl1420$annexing, pl1420$annexing_use)
330/(330+1085)
# 23.3% agreement - very bad 

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
         more_nbmin = ifelse(pctnbmin_total > pctnbmin_p0, 1, 0)) 

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
pl0007 <- panel0020_did %>% filter(time=="2000 to 2007")

sapply(panel0020_did, function(x) sum(is.na(x)))

panel0020_did %<>%
  filter_at(vars(pop_p0, popdensity_p0, popgrowth, pctnhwhite_p0, pctnhwhitegrowth, pctnhblack_total, pctnbmin_total, pctnhblackgrowth, pctnbmingrowth, more_white, pctowneroccupied_p0, mhmval_p0, hinc_p0, pctpov_p0, pctownerocc_total, pcthincjobs_total, pctincopp_total, pctnhblack_p1, pctnhwhite_p1, pctnbmin_p1), ~!is.na(.))

panel0020_did %<>%
  filter_at(vars(annexing, annexing_bas, pctnhwhite_diff, pctnhblack_diff, pctnbmin_diff), ~!is.na(.))

plids <- Reduce(intersect, list(unique(panel0020_did$plid[panel0020_did$time == "2007 to 2013"]), unique(panel0020_did$plid[panel0020_did$time == "2014 to 2020"])))

#plids <- Reduce(intersect, list(unique(panel0020_did$plid[panel0020_did$time == "2000 to 2007"]), unique(panel0020_did$plid[panel0020_did$time == "2007 to 2013"]), unique(panel0020_did$plid[panel0020_did$time == "2014 to 2020"])))

panel0020_did %<>%
  filter(plid %in% plids)

table(panel0020_did$annexing[panel0020_did$time=="2000 to 2007"], panel0020_did$annexing_use[panel0020_did$time=="2000 to 2007"])
1666/(1666+92)
# 94.8

table(panel0020_did$annexing[panel0020_did$time=="2007 to 2013"], panel0020_did$annexing_use[panel0020_did$time=="2007 to 2013"])
1890/(1890 + 862)
# 68.7%

table(panel0020_did$annexing[panel0020_did$time=="2014 to 2020"], panel0020_did$annexing_use[panel0020_did$time=="2014 to 2020"])
270/(270+1160)
# 18.94%

length(unique(panel0020_did$plid))

write_csv(panel0020_did, "analyticalfiles/panel_prestandard.csv")
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

# event study ####
mod_sa = feols(annexing ~ i(post, vra, ref = -1) + 
                 pop_p0 + popgrowth + popdensity_p0 + pctnhblack_p0 + pctnbmin_p0 + pctnhblackgrowth + pctnbmingrowth + pctowneroccupied_p0 + mhmval_p0 + hinc_p0 + pctpov_p0 + as.factor(more_white) + pctnhblack_total + pctnbmin_total + pctownerocc_total + pcthincjobs_total + pctincopp_total |
                 plid + STATE,
               NW(0.5) ~ plid + post,
               data = panel0020_did)

iplot(mod_sa, 
      xlab = 'Time to Shelby',
      main = 'Event study: Probability to Annex')

annex_basic <- feols(annexing ~ period*vra | plid + STATE, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), NW(0.5) ~ plid + period, fixef.rm = "none")
summary(annex_basic)

# add variables 
annex <- feols(annexing ~ as.factor(vra)*as.factor(period) | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid, fixef.rm = "none")
summary(annex)

annex_st <- feols(annexing ~ as.factor(vra)*as.factor(period) | plid + STATE, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid, fixef.rm = "none")
summary(annex)

annex_twop <- feols(annexing ~ as.factor(vra)*as.factor(period) + pop_p0 + popdensity_p0 + popgrowth + pctnhblack_p0 + pctnhblackgrowth + pctnbmin_p0 + pctnbmingrowth + pctblackpov_p0 + pctnbminpov_p0 + pctowneroccupied_p0 + mhmval_p0 + hinc_p0 + pctpov_p0 + as.factor(more_white) + pctnhblack_total + pctnbmin_total + pctownerocc_total + pcthincjobs_total + pctincopp_total | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid, fixef.rm = "none")
summary(annex_twop)

annex_twop_st <- feols(annexing ~ as.factor(vra)*as.factor(period) + pop_p0 + popdensity_p0 + popgrowth + pctnhblack_p0 + pctnhblackgrowth + pctnbmin_p0 + pctnbmingrowth + pctblackpov_p0 + pctnbminpov_p0 + pctowneroccupied_p0 + mhmval_p0 + hinc_p0 + pctpov_p0 + as.factor(more_white) + pctnhblack_total + pctnbmin_total + pctownerocc_total + pcthincjobs_total + pctincopp_total | plid + STATE, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid, fixef.rm = "none")
summary(annex_twop)

ann_tid <- tidy(annex)
for (model_stat in names(glance(annex))) {
  ann_tid[[model_stat]] <- glance(annex)[[model_stat]]
}

ann_2_tid <- tidy(annex_twop)
for (model_stat in names(glance(annex_twop))) {
  ann_2_tid[[model_stat]] <- glance(annex_twop)[[model_stat]]
}

ann_tid_st <- tidy(annex_st)
for (model_stat in names(glance(annex_st))) {
  ann_tid[[model_stat]] <- glance(annex_st)[[model_stat]]
}

ann_2_tid_st <- tidy(annex_twop_st)
for (model_stat in names(glance(annex_twop_st))) {
  ann_2_tid[[model_stat]] <- glance(annex_twop_st)[[model_stat]]
}

annex_list <- list(ann_tid, ann_tid_st, ann_2_tid, ann_2_tid_st)

openxlsx::write.xlsx(annex_list, "analyticalfiles/results/annex_reg.xlsx")

# selection into annexation? ####
# how black/white/nonwhite are annexing places? 
white_p0 <- feols(pctnhwhite_p0 ~ as.factor(annexing_use)*as.factor(period) | plid + STATE, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), NW(0.5) ~ plid + period, fixef.rm = "none")
summary(white_p0)

black_p0 <- feols(pctnhblack_p0 ~ as.factor(annexing_use)*as.factor(period) | plid + STATE, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), NW(1) ~ plid + period, fixef.rm = "none")
summary(black_p0)

h_p0 <- feols(pcth_p0 ~ as.factor(annexing_use)*as.factor(period) | plid + STATE, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), NW(1) ~ plid + period, fixef.rm = "none")
summary(h_p0)

nbmin_p0 <- feols(pctnbmin_p0 ~ as.factor(annexing_use)*as.factor(period) | plid + STATE, NW(1) ~ plid + period, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), fixef.rm = "none")
summary(nbmin_p0)

# consequences of annexing ####
# ref: before Shelby, no VRA, not annexing ----
panel0020_did %<>%
  mutate(annexing = relevel(as.factor(annexing), ref = "0"),
         period = relevel(as.factor(period), ref = "0"),
         vra = relevel(as.factor(vra), ref = "0"))

nhb_base <- feols(pctnhblack_p1 ~ as.factor(annexing)*as.factor(period)*as.factor(vra) | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid)
summary(nhb_base)

nhb_base_cov <- feols(pctnhblack_p1 ~ as.factor(annexing)*as.factor(period)*as.factor(vra) + pctnhblack_p0 + pctnhblackgrowth + pctnhblack_total | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid)
summary(nhb_base_cov) # N = 27987

nhw_base <- feols(pctnhwhite_p1 ~ as.factor(annexing)*as.factor(vra)*as.factor(period) | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid)
summary(nhw_base) # N = 27987

nhw_base_cov <- feols(pctnhwhite_p1 ~ as.factor(annexing)*as.factor(vra)*as.factor(period) + pctnhwhite_p0 + pctnhwhitegrowth + pctnhwhite_total | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid)
summary(nhw_base_cov) # N = 27987

nbmin_base <- feols(pctnbmin_p1 ~ as.factor(annexing)*as.factor(vra)*as.factor(period) | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid)
summary(nbmin_base) # N = 27987

nbmin_base_cov <- feols(pctnbmin_p1 ~ as.factor(annexing)*as.factor(vra)*as.factor(period) + pctnbmin_p0 + pctnbmingrowth + pctnbmin_total | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid)
summary(nbmin_base_cov) # N = 27987

p1_diff <- list(tidy(nhb_base), tidy(nhb_base_cov), tidy(nhw_base), tidy(nhw_base_cov), tidy(nbmin_base), tidy(nbmin_base_cov))
p1_diff_glance <- list(nhb_base, nhb_base_cov, nhw_base, nhw_base_cov, nbmin_base, nbmin_base_cov)

for (model_stat in names(glance(nhb_base))) {
  for (i in 1:6) {
    p1_diff[[i]][[model_stat]] <- glance(p1_diff_glance[[i]])[[model_stat]]
  }
}
openxlsx::write.xlsx(p1_diff, "analyticalfiles/results/p1_pre-Shelby_uncovered.xlsx")

# ref: before Shelby, VRA, not annexing ----
panel0020_did %<>%
  mutate(annexing = relevel(as.factor(annexing), ref = "0"),
         period = relevel(as.factor(period), ref = "0"),
         vra = relevel(as.factor(vra), ref = "1"))

nhb_base <- feols(pctnhblack_p1 ~ as.factor(annexing)*as.factor(period)*as.factor(vra) | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid)
summary(nhb_base)

nhb_base_cov <- feols(pctnhblack_p1 ~ as.factor(annexing)*as.factor(period)*as.factor(vra) + pctnhblack_p0 + pctnhblackgrowth + pctnhblack_total | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid)
summary(nhb_base_cov) # N = 27987

nhw_base <- feols(pctnhwhite_p1 ~ as.factor(annexing)*as.factor(vra)*as.factor(period) | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid)
summary(nhw_base) # N = 27987

nhw_base_cov <- feols(pctnhwhite_p1 ~ as.factor(annexing)*as.factor(vra)*as.factor(period) + pctnhwhite_p0 + pctnhwhitegrowth + pctnhwhite_total | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid)
summary(nhw_base_cov) # N = 27987

nbmin_base <- feols(pctnbmin_p1 ~ as.factor(annexing)*as.factor(vra)*as.factor(period) | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid)
summary(nbmin_base) # N = 27987

nbmin_base_cov <- feols(pctnbmin_p1 ~ as.factor(annexing)*as.factor(vra)*as.factor(period) + pctnbmin_p0 + pctnbmingrowth + pctnbmin_total | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid)
summary(nbmin_base_cov) # N = 27987

p1_diff <- list(tidy(nhb_base), tidy(nhb_base_cov), tidy(nhw_base), tidy(nhw_base_cov), tidy(nbmin_base), tidy(nbmin_base_cov))
p1_diff_glance <- list(nhb_base, nhb_base_cov, nhw_base, nhw_base_cov, nbmin_base, nbmin_base_cov)

for (model_stat in names(glance(nhb_base))) {
  for (i in 1:6) {
    p1_diff[[i]][[model_stat]] <- glance(p1_diff_glance[[i]])[[model_stat]]
  }
}
openxlsx::write.xlsx(p1_diff, "analyticalfiles/results/p1_pre-Shelby_covered.xlsx")

# ref: after Shelby, VRA, not Annexing ---- 
panel0020_did %<>%
  mutate(annexing = relevel(as.factor(annexing), ref = "0"), 
         period = relevel(as.factor(period), ref = "1"),
         vra = relevel(as.factor(vra), ref = "1"))

nhb_base <- feols(pctnhblack_p1 ~ as.factor(annexing)*as.factor(period)*as.factor(vra) | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid)
summary(nhb_base)

nhb_base_cov <- feols(pctnhblack_p1 ~ as.factor(annexing)*as.factor(period)*as.factor(vra) + pctnhblack_p0 + pctnhblackgrowth + pctnhblack_total | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid)
summary(nhb_base_cov) # N = 27987

nhw_base <- feols(pctnhwhite_p1 ~ as.factor(annexing)*as.factor(vra)*as.factor(period) | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid)
summary(nhw_base) # N = 27987

nhw_base_cov <- feols(pctnhwhite_p1 ~ as.factor(annexing)*as.factor(vra)*as.factor(period) + pctnhwhite_p0 + pctnhwhitegrowth + pctnhwhite_total | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid)
summary(nhw_base_cov) # N = 27987

nbmin_base <- feols(pctnbmin_p1 ~ as.factor(annexing)*as.factor(vra)*as.factor(period) | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid)
summary(nbmin_base) # N = 27987

nbmin_base_cov <- feols(pctnbmin_p1 ~ as.factor(annexing)*as.factor(vra)*as.factor(period) + pctnbmin_p0 + pctnbmingrowth + pctnbmin_total | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid)
summary(nbmin_base_cov) # N = 27987

p1_diff <- list(tidy(nhb_base), tidy(nhb_base_cov), tidy(nhw_base), tidy(nhw_base_cov), tidy(nbmin_base), tidy(nbmin_base_cov))
p1_diff_glance <- list(nhb_base, nhb_base_cov, nhw_base, nhw_base_cov, nbmin_base, nbmin_base_cov)

for (model_stat in names(glance(nhb_base))) {
  for (i in 1:6) {
    p1_diff[[i]][[model_stat]] <- glance(p1_diff_glance[[i]])[[model_stat]]
  }
}
openxlsx::write.xlsx(p1_diff, "analyticalfiles/results/p1_post_covered.xlsx")

# ref: after Shelby, non-Section V, non-annexing ----
panel0020_did %<>%
  mutate(annexing = relevel(as.factor(annexing), ref = "0"), 
         period = relevel(as.factor(period), ref = "1"),
         vra = relevel(as.factor(vra), ref = "0"))

nhb_base <- feols(pctnhblack_p1 ~ as.factor(annexing)*as.factor(period)*as.factor(vra) | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid)
summary(nhb_base)

nhb_base_cov <- feols(pctnhblack_p1 ~ as.factor(annexing)*as.factor(period)*as.factor(vra) + pctnhblack_p0 + pctnhblackgrowth + pctnhblack_total | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid)
summary(nhb_base_cov) # N = 27987

nhw_base <- feols(pctnhwhite_p1 ~ as.factor(annexing)*as.factor(vra)*as.factor(period) | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid)
summary(nhw_base) # N = 27987

nhw_base_cov <- feols(pctnhwhite_p1 ~ as.factor(annexing)*as.factor(vra)*as.factor(period) + pctnhwhite_p0 + pctnhwhitegrowth + pctnhwhite_total | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid)
summary(nhw_base_cov) # N = 27987

nbmin_base <- feols(pctnbmin_p1 ~ as.factor(annexing)*as.factor(vra)*as.factor(period) | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid)
summary(nbmin_base) # N = 27987

nbmin_base_cov <- feols(pctnbmin_p1 ~ as.factor(annexing)*as.factor(vra)*as.factor(period) + pctnbmin_p0 + pctnbmingrowth + pctnbmin_total | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid)
summary(nbmin_base_cov) # N = 27987

p1_diff <- list(tidy(nhb_base), tidy(nhb_base_cov), tidy(nhw_base), tidy(nhw_base_cov), tidy(nbmin_base), tidy(nbmin_base_cov))
p1_diff_glance <- list(nhb_base, nhb_base_cov, nhw_base, nhw_base_cov, nbmin_base, nbmin_base_cov)

for (model_stat in names(glance(nhb_base))) {
  for (i in 1:6) {
    p1_diff[[i]][[model_stat]] <- glance(p1_diff_glance[[i]])[[model_stat]]
  }
}
openxlsx::write.xlsx(p1_diff, "analyticalfiles/results/p1_post_uncovered.xlsx")

# is it just an anticipation effect? ----
plids <- Reduce(intersect, list(unique(panel0020_did$plid[panel0020_did$time == "2000 to 2007"]), unique(panel0020_did$plid[panel0020_did$time == "2014 to 2020"])))

panel0020_did %<>%
  filter(plid %in% plids)

# ref: pre, no VRA, no annexing ----
panel0020_did %<>%
  mutate(vra = relevel(as.factor(vra), ref = "0"),
         period = relevel(as.factor(period), ref = "0"), 
         annexing = relevel(as.factor(annexing), ref = "0"))

nhb_base <- feols(pctnhblack_p1 ~ as.factor(annexing)*as.factor(period)*as.factor(vra) | plid, data = panel0020_did %>% filter(time %in% c("2000 to 2007", "2014 to 2020") & plid %in% plids), cluster = ~plid)
summary(nhb_base)

nhb_base_cov <- feols(pctnhblack_p1 ~ as.factor(annexing)*as.factor(period)*as.factor(vra) + pctnhblack_p0 + pctnhblackgrowth + pctnhblack_total | plid, data = panel0020_did %>% filter(time %in% c("2000 to 2007", "2014 to 2020") & plid %in% plids), cluster = ~plid)
summary(nhb_base_cov) # N = 27987

nhw_base <- feols(pctnhwhite_p1 ~ as.factor(annexing)*as.factor(period)*as.factor(vra) | plid, data = panel0020_did %>% filter(time %in% c("2000 to 2007", "2014 to 2020") & plid %in% plids), cluster = ~plid)
summary(nhw_base) # N = 27987

nhw_base_cov <- feols(pctnhwhite_p1 ~ as.factor(annexing)*as.factor(period)*as.factor(vra) + pctnhwhite_p0 + pctnhwhitegrowth + pctnhwhite_total | plid, data = panel0020_did %>% filter(time %in% c("2000 to 2007", "2014 to 2020") & plid %in% plids), cluster = ~plid)
summary(nhw_base_cov) # N = 27987

nbmin_base <- feols(pctnbmin_p1 ~ as.factor(annexing)*as.factor(period)*as.factor(vra) | plid, data = panel0020_did %>% filter(time %in% c("2000 to 2007", "2014 to 2020") & plid %in% plids), cluster = ~plid)
summary(nbmin_base) # N = 27987

nbmin_base_cov <- feols(pctnbmin_p1 ~ as.factor(annexing)*as.factor(period)*as.factor(vra) + pctnbmin_p0 + pctnbmingrowth + pctnbmin_total | plid, data = panel0020_did %>% filter(time %in% c("2000 to 2007", "2014 to 2020") & plid %in% plids), cluster = ~plid)
summary(nbmin_base_cov) # N = 27987

p1_diff <- list(tidy(nhb_base), tidy(nhb_base_cov), tidy(nhw_base), tidy(nhw_base_cov), tidy(nbmin_base), tidy(nbmin_base_cov))
p1_diff_glance <- list(nhb_base, nhb_base_cov, nhw_base, nhw_base_cov, nbmin_base, nbmin_base_cov)

for (model_stat in names(glance(nhb_base))) {
  for (i in 1:6) {
    p1_diff[[i]][[model_stat]] <- glance(p1_diff_glance[[i]])[[model_stat]]
  }
}
openxlsx::write.xlsx(p1_diff, "analyticalfiles/results/0007_pre_uncovered.xlsx")

# ref: post, no VRA, no annexing ----
panel0020_did %<>%
  mutate(vra = relevel(as.factor(vra), ref = "0"),
         period = relevel(as.factor(period), ref = "1"))

nhb_base <- feols(pctnhblack_p1 ~ as.factor(annexing)*as.factor(period)*as.factor(vra) | plid, data = panel0020_did %>% filter(time %in% c("2000 to 2007", "2014 to 2020") & plid %in% plids), cluster = ~plid)
summary(nhb_base)

nhb_base_cov <- feols(pctnhblack_p1 ~ as.factor(annexing)*as.factor(period)*as.factor(vra) + pctnhblack_p0 + pctnhblackgrowth + pctnhblack_total | plid, data = panel0020_did %>% filter(time %in% c("2000 to 2007", "2014 to 2020") & plid %in% plids), cluster = ~plid)
summary(nhb_base_cov) # N = 27987

nhw_base <- feols(pctnhwhite_p1 ~ as.factor(annexing)*as.factor(period)*as.factor(vra) | plid, data = panel0020_did %>% filter(time %in% c("2000 to 2007", "2014 to 2020") & plid %in% plids), cluster = ~plid)
summary(nhw_base) # N = 27987

nhw_base_cov <- feols(pctnhwhite_p1 ~ as.factor(annexing)*as.factor(period)*as.factor(vra) + pctnhwhite_p0 + pctnhwhitegrowth + pctnhwhite_total | plid, data = panel0020_did %>% filter(time %in% c("2000 to 2007", "2014 to 2020") & plid %in% plids), cluster = ~plid)
summary(nhw_base_cov) # N = 27987

nbmin_base <- feols(pctnbmin_p1 ~ as.factor(annexing)*as.factor(period)*as.factor(vra) | plid, data = panel0020_did %>% filter(time %in% c("2000 to 2007", "2014 to 2020") & plid %in% plids), cluster = ~plid)
summary(nbmin_base) # N = 27987

nbmin_base_cov <- feols(pctnbmin_p1 ~ as.factor(annexing)*as.factor(period)*as.factor(vra) + pctnbmin_p0 + pctnbmingrowth + pctnbmin_total | plid, data = panel0020_did %>% filter(time %in% c("2000 to 2007", "2014 to 2020") & plid %in% plids), cluster = ~plid)
summary(nbmin_base_cov) # N = 27987

p1_diff <- list(tidy(nhb_base), tidy(nhb_base_cov), tidy(nhw_base), tidy(nhw_base_cov), tidy(nbmin_base), tidy(nbmin_base_cov))
p1_diff_glance <- list(nhb_base, nhb_base_cov, nhw_base, nhw_base_cov, nbmin_base, nbmin_base_cov)

for (model_stat in names(glance(nhb_base))) {
  for (i in 1:6) {
    p1_diff[[i]][[model_stat]] <- glance(p1_diff_glance[[i]])[[model_stat]]
  }
}
openxlsx::write.xlsx(p1_diff, "analyticalfiles/results/0007_post_uncovered.xlsx")

# ref: pre, VRA, no annexing ----
panel0020_did %<>%
  mutate(vra = relevel(as.factor(vra), ref = "1"),
         period = relevel(as.factor(period), ref = "0"))

nhb_base <- feols(pctnhblack_p1 ~ as.factor(annexing)*as.factor(period)*as.factor(vra) | plid, data = panel0020_did %>% filter(time %in% c("2000 to 2007", "2014 to 2020") & plid %in% plids), cluster = ~plid)
summary(nhb_base)

nhb_base_cov <- feols(pctnhblack_p1 ~ as.factor(annexing)*as.factor(period)*as.factor(vra) + pctnhblack_p0 + pctnhblackgrowth + pctnhblack_total | plid, data = panel0020_did %>% filter(time %in% c("2000 to 2007", "2014 to 2020") & plid %in% plids), cluster = ~plid)
summary(nhb_base_cov) # N = 27987

nhw_base <- feols(pctnhwhite_p1 ~ as.factor(annexing)*as.factor(period)*as.factor(vra) | plid, data = panel0020_did %>% filter(time %in% c("2000 to 2007", "2014 to 2020") & plid %in% plids), cluster = ~plid)
summary(nhw_base) # N = 27987

nhw_base_cov <- feols(pctnhwhite_p1 ~ as.factor(annexing)*as.factor(period)*as.factor(vra) + pctnhwhite_p0 + pctnhwhitegrowth + pctnhwhite_total | plid, data = panel0020_did %>% filter(time %in% c("2000 to 2007", "2014 to 2020") & plid %in% plids), cluster = ~plid)
summary(nhw_base_cov) # N = 27987

nbmin_base <- feols(pctnbmin_p1 ~ as.factor(annexing)*as.factor(period)*as.factor(vra) | plid, data = panel0020_did %>% filter(time %in% c("2000 to 2007", "2014 to 2020") & plid %in% plids), cluster = ~plid)
summary(nbmin_base) # N = 27987

nbmin_base_cov <- feols(pctnbmin_p1 ~ as.factor(annexing)*as.factor(period)*as.factor(vra) + pctnbmin_p0 + pctnbmingrowth + pctnbmin_total | plid, data = panel0020_did %>% filter(time %in% c("2000 to 2007", "2014 to 2020") & plid %in% plids), cluster = ~plid)
summary(nbmin_base_cov) # N = 27987

p1_diff <- list(tidy(nhb_base), tidy(nhb_base_cov), tidy(nhw_base), tidy(nhw_base_cov), tidy(nbmin_base), tidy(nbmin_base_cov))
p1_diff_glance <- list(nhb_base, nhb_base_cov, nhw_base, nhw_base_cov, nbmin_base, nbmin_base_cov)

for (model_stat in names(glance(nhb_base))) {
  for (i in 1:6) {
    p1_diff[[i]][[model_stat]] <- glance(p1_diff_glance[[i]])[[model_stat]]
  }
}
openxlsx::write.xlsx(p1_diff, "analyticalfiles/results/0007_pre_covered.xlsx")

# ref: post, VRA, no annexing ----
panel0020_did %<>%
  mutate(vra = relevel(as.factor(vra), ref = "1"),
         period = relevel(as.factor(period), ref = "1"))

nhb_base <- feols(pctnhblack_p1 ~ as.factor(annexing)*as.factor(period)*as.factor(vra) | plid, data = panel0020_did %>% filter(time %in% c("2000 to 2007", "2014 to 2020") & plid %in% plids), cluster = ~plid)
summary(nhb_base)

nhb_base_cov <- feols(pctnhblack_p1 ~ as.factor(annexing)*as.factor(period)*as.factor(vra) + pctnhblack_p0 + pctnhblackgrowth + pctnhblack_total | plid, data = panel0020_did %>% filter(time %in% c("2000 to 2007", "2014 to 2020") & plid %in% plids), cluster = ~plid)
summary(nhb_base_cov) # N = 27987

nhw_base <- feols(pctnhwhite_p1 ~ as.factor(annexing)*as.factor(period)*as.factor(vra) | plid, data = panel0020_did %>% filter(time %in% c("2000 to 2007", "2014 to 2020") & plid %in% plids), cluster = ~plid)
summary(nhw_base) # N = 27987

nhw_base_cov <- feols(pctnhwhite_p1 ~ as.factor(annexing)*as.factor(vra)*as.factor(period) + pctnhwhite_p0 + pctnhwhitegrowth + pctnhwhite_total | plid, data = panel0020_did %>% filter(time %in% c("2000 to 2007", "2014 to 2020") & plid %in% plids), cluster = ~plid)
summary(nhw_base_cov) # N = 27987

nbmin_base <- feols(pctnbmin_p1 ~ as.factor(annexing)*as.factor(vra)*as.factor(period) | plid, data = panel0020_did %>% filter(time %in% c("2000 to 2007", "2014 to 2020") & plid %in% plids), cluster = ~plid)
summary(nbmin_base) # N = 27987

nbmin_base_cov <- feols(pctnbmin_p1 ~ as.factor(annexing)*as.factor(vra)*as.factor(period) + pctnbmin_p0 + pctnbmingrowth + pctnbmin_total | plid, data = panel0020_did %>% filter(time %in% c("2000 to 2007", "2014 to 2020") & plid %in% plids), cluster = ~plid)
summary(nbmin_base_cov) # N = 27987

p1_diff <- list(tidy(nhb_base), tidy(nhb_base_cov), tidy(nhw_base), tidy(nhw_base_cov), tidy(nbmin_base), tidy(nbmin_base_cov))
p1_diff_glance <- list(nhb_base, nhb_base_cov, nhw_base, nhw_base_cov, nbmin_base, nbmin_base_cov)

for (model_stat in names(glance(nhb_base))) {
  for (i in 1:6) {
    p1_diff[[i]][[model_stat]] <- glance(p1_diff_glance[[i]])[[model_stat]]
  }
}
openxlsx::write.xlsx(p1_diff, "analyticalfiles/results/0007_post_covered.xlsx")

# poverty and hinc ---- 
panel0020_did %<>%
  mutate(vra = relevel(as.factor(vra), ref = "0"),
         period = relevel(as.factor(period), ref = "0"))

ppov_v0p0 <- feols(pctpov_p1 ~ as.factor(annexing)*as.factor(period)*as.factor(vra) + pctpov_p0 | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid)
summary(ppov_v0p0) 

hinc_v0p0 <- feols(hinc_p1 ~ as.factor(annexing)*as.factor(vra)*as.factor(period) + hinc_p0 | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid)
summary(hinc_v0p0) 

panel0020_did %<>%
  mutate(vra = relevel(as.factor(vra), ref = "0"),
         period = relevel(as.factor(period), ref = "1"))

ppov_v0p1 <- feols(pctpov_p1 ~ as.factor(annexing)*as.factor(period)*as.factor(vra) + pctpov_p0 | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid)
summary(ppov_v0p1) 

hinc_v0p1 <- feols(hinc_p1 ~ as.factor(annexing)*as.factor(vra)*as.factor(period) + hinc_p0 | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid)
summary(hinc_v0p1) 

panel0020_did %<>%
  mutate(vra = relevel(as.factor(vra), ref = "1"),
         period = relevel(as.factor(period), ref = "0"))

ppov_v1p0 <- feols(pctpov_p1 ~ as.factor(annexing)*as.factor(period)*as.factor(vra) + pctpov_p0 | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid)
summary(ppov_v1p0) 

hinc_v1p0 <- feols(hinc_p1 ~ as.factor(annexing)*as.factor(vra)*as.factor(period) + hinc_p0 | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid)
summary(hinc_v1p0) 

panel0020_did %<>%
  mutate(vra = relevel(as.factor(vra), ref = "1"),
         period = relevel(as.factor(period), ref = "1"))

ppov_v1p1 <- feols(pctpov_p1 ~ as.factor(annexing)*as.factor(period)*as.factor(vra) + pctpov_p0 | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid)
summary(ppov_v1p1) 

hinc_v1p1 <- feols(hinc_p1 ~ as.factor(annexing)*as.factor(vra)*as.factor(period) + hinc_p0 | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid)
summary(hinc_v1p1) 

p1_diff <- list(tidy(ppov_v0p0), tidy(hinc_v0p0), tidy(ppov_v0p1), tidy(hinc_v0p1), tidy(ppov_v1p0), tidy(hinc_v1p0), tidy(ppov_v1p1), tidy(hinc_v1p1))
p1_diff_glance <- list(ppov_v0p0, hinc_v0p0, ppov_v0p1, hinc_v0p1, ppov_v1p0, hinc_v1p0, ppov_v1p1, hinc_v1p1)

for (model_stat in names(glance(ppov_v0p0))) {
  for (i in 1:8) {
    p1_diff[[i]][[model_stat]] <- glance(p1_diff_glance[[i]])[[model_stat]]
  }
}
openxlsx::write.xlsx(p1_diff, "analyticalfiles/results/ppov_hinc.xlsx")

# who's living on the periphery over time? ----
panel0020_did %<>%
  mutate(vra = relevel(as.factor(vra), ref = "0"),
         period = relevel(as.factor(period), ref = "0"))

nhb_base <- feols(pctnhblack_total ~ period | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid)
summary(nhb_base)

nhw_base <- feols(pctnhwhite_total ~ as.factor(period) | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid)
summary(nhw_base) # N = 27987

nbmin_base <- feols(pctnbmin_total ~ as.factor(period) | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid)
summary(nbmin_base) # N = 27987

p1_diff <- list(tidy(nhb_base), tidy(nhw_base), tidy(nbmin_base))
p1_diff_glance <- list(nhb_base, nhw_base, nbmin_base)

for (model_stat in names(glance(nhb_base))) {
  for (i in 1:3) {
    p1_diff[[i]][[model_stat]] <- glance(p1_diff_glance[[i]])[[model_stat]]
  }
}
openxlsx::write.xlsx(p1_diff, "analyticalfiles/results/periphery_pct.xlsx")

# do again for validated sample...----
panel0020_did <- read_csv("analyticalfiles/panel_prestandard.csv")
panel0020_did %<>%
  mutate_at(vars(c(ends_with("total"), ends_with("_p0"), ends_with("_p1"), ends_with("_total_1"), contains("growth"), contains("_annexed"), ends_with("_log"), contains("diff"))), 
            ~((.-mean(., na.rm = T))/sd(., na.rm = T))) 

# add variables 
annex <- feols(annexing_bas ~ as.factor(vra)*as.factor(period) | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid, fixef.rm = "none")
summary(annex)

annex_twop <- feols(annexing_bas ~ as.factor(vra)*as.factor(period) + pop_p0 + popdensity_p0 + popgrowth +  pctnhblack_p0 + pctnhblackgrowth + pctnbmin_p0 + pctnbmingrowth + pctblackpov_p0 + pctnbminpov_p0 + pctowneroccupied_p0 + mhmval_p0 + hinc_p0 + pctpov_p0 + as.factor(more_white) + pctnhblack_total + pctnbmin_total + pctownerocc_total + pcthincjobs_total + pctincopp_total + lag_annexed | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid, fixef.rm = "none")
summary(annex_twop)

ann_tid <- tidy(annex)
for (model_stat in names(glance(annex))) {
  ann_tid[[model_stat]] <- glance(annex)[[model_stat]]
}

ann_2_tid <- tidy(annex_twop)
for (model_stat in names(glance(annex_twop))) {
  ann_2_tid[[model_stat]] <- glance(annex_twop)[[model_stat]]
}

annex_list <- list(ann_tid, ann_2_tid)

openxlsx::write.xlsx(annex_list, "analyticalfiles/results/bas/annex_reg.xlsx")

# consequences of annexing ####
# ref: pre, uncovered ----
panel0020_did %<>%
  mutate(annexing = relevel(as.factor(annexing), ref = "0"),
         period = relevel(as.factor(period), ref = "0"),
         vra = relevel(as.factor(vra), ref = "0"))

nhb_base <- feols(pctnhblack_p1 ~ as.factor(annexing_bas)*as.factor(period)*as.factor(vra) | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid)
summary(nhb_base)

nhb_base_cov <- feols(pctnhblack_p1 ~ as.factor(annexing_bas)*as.factor(period)*as.factor(vra) + pctnhblack_p0 + pctnhblackgrowth + pctnhblack_total | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid)
summary(nhb_base_cov) # N = 27987

nhw_base <- feols(pctnhwhite_p1 ~ as.factor(annexing_bas)*as.factor(vra)*as.factor(period) | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid)
summary(nhw_base) # N = 27987

nhw_base_cov <- feols(pctnhwhite_p1 ~ as.factor(annexing_bas)*as.factor(vra)*as.factor(period) + pctnhwhite_p0 + pctnhwhitegrowth + pctnhwhite_total | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid)
summary(nhw_base_cov) # N = 27987

nbmin_base <- feols(pctnbmin_p1 ~ as.factor(annexing_bas)*as.factor(vra)*as.factor(period) | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid)
summary(nbmin_base) # N = 27987

nbmin_base_cov <- feols(pctnbmin_p1 ~ as.factor(annexing_bas)*as.factor(vra)*as.factor(period) + pctnbmin_p0 + pctnbmingrowth + pctnbmin_total | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid)
summary(nbmin_base_cov) # N = 27987

p1_diff <- list(tidy(nhb_base), tidy(nhb_base_cov), tidy(nhw_base), tidy(nhw_base_cov), tidy(nbmin_base), tidy(nbmin_base_cov))
p1_diff_glance <- list(nhb_base, nhb_base_cov, nhw_base, nhw_base_cov, nbmin_base, nbmin_base_cov)

for (model_stat in names(glance(nhb_base))) {
  for (i in 1:6) {
    p1_diff[[i]][[model_stat]] <- glance(p1_diff_glance[[i]])[[model_stat]]
  }
}
openxlsx::write.xlsx(p1_diff, "analyticalfiles/results/bas/p1_pre-Shelby_uncovered.xlsx")

# ref: before Shelby, VRA, not annexing ----
panel0020_did %<>%
  mutate(annexing = relevel(as.factor(annexing), ref = "0"),
         period = relevel(as.factor(period), ref = "0"),
         vra = relevel(as.factor(vra), ref = "1"))

nhb_base <- feols(pctnhblack_p1 ~ as.factor(annexing_bas)*as.factor(period)*as.factor(vra) | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid)
summary(nhb_base)

nhb_base_cov <- feols(pctnhblack_p1 ~ as.factor(annexing_bas)*as.factor(period)*as.factor(vra) + pctnhblack_p0 + pctnhblackgrowth + pctnhblack_total | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid)
summary(nhb_base_cov) # N = 27987

nhw_base <- feols(pctnhwhite_p1 ~ as.factor(annexing_bas)*as.factor(vra)*as.factor(period) | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid)
summary(nhw_base) # N = 27987

nhw_base_cov <- feols(pctnhwhite_p1 ~ as.factor(annexing_bas)*as.factor(vra)*as.factor(period) + pctnhwhite_p0 + pctnhwhitegrowth + pctnhwhite_total | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid)
summary(nhw_base_cov) # N = 27987

nbmin_base <- feols(pctnbmin_p1 ~ as.factor(annexing_bas)*as.factor(vra)*as.factor(period) | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid)
summary(nbmin_base) # N = 27987

nbmin_base_cov <- feols(pctnbmin_p1 ~ as.factor(annexing_bas)*as.factor(vra)*as.factor(period) + pctnbmin_p0 + pctnbmingrowth + pctnbmin_total | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid)
summary(nbmin_base_cov) # N = 27987

p1_diff <- list(tidy(nhb_base), tidy(nhb_base_cov), tidy(nhw_base), tidy(nhw_base_cov), tidy(nbmin_base), tidy(nbmin_base_cov))
p1_diff_glance <- list(nhb_base, nhb_base_cov, nhw_base, nhw_base_cov, nbmin_base, nbmin_base_cov)

for (model_stat in names(glance(nhb_base))) {
  for (i in 1:6) {
    p1_diff[[i]][[model_stat]] <- glance(p1_diff_glance[[i]])[[model_stat]]
  }
}
openxlsx::write.xlsx(p1_diff, "analyticalfiles/results/bas/p1_pre-Shelby_covered.xlsx")

# ref: after Shelby, VRA, not annexing_bas ---- 
panel0020_did %<>%
  mutate(annexing_bas = relevel(as.factor(annexing_bas), ref = "0"), 
         period = relevel(as.factor(period), ref = "1"),
         vra = relevel(as.factor(vra), ref = "1"))

nhb_base <- feols(pctnhblack_p1 ~ as.factor(annexing_bas)*as.factor(period)*as.factor(vra) | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid)
summary(nhb_base)

nhb_base_cov <- feols(pctnhblack_p1 ~ as.factor(annexing_bas)*as.factor(period)*as.factor(vra) + pctnhblack_p0 + pctnhblackgrowth + pctnhblack_total | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid)
summary(nhb_base_cov) # N = 27987

nhw_base <- feols(pctnhwhite_p1 ~ as.factor(annexing_bas)*as.factor(vra)*as.factor(period) | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid)
summary(nhw_base) # N = 27987

nhw_base_cov <- feols(pctnhwhite_p1 ~ as.factor(annexing_bas)*as.factor(vra)*as.factor(period) + pctnhwhite_p0 + pctnhwhitegrowth + pctnhwhite_total | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid)
summary(nhw_base_cov) # N = 27987

nbmin_base <- feols(pctnbmin_p1 ~ as.factor(annexing_bas)*as.factor(vra)*as.factor(period) | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid)
summary(nbmin_base) # N = 27987

nbmin_base_cov <- feols(pctnbmin_p1 ~ as.factor(annexing_bas)*as.factor(vra)*as.factor(period) + pctnbmin_p0 + pctnbmingrowth + pctnbmin_total | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid)
summary(nbmin_base_cov) # N = 27987

p1_diff <- list(tidy(nhb_base), tidy(nhb_base_cov), tidy(nhw_base), tidy(nhw_base_cov), tidy(nbmin_base), tidy(nbmin_base_cov))
p1_diff_glance <- list(nhb_base, nhb_base_cov, nhw_base, nhw_base_cov, nbmin_base, nbmin_base_cov)

for (model_stat in names(glance(nhb_base))) {
  for (i in 1:6) {
    p1_diff[[i]][[model_stat]] <- glance(p1_diff_glance[[i]])[[model_stat]]
  }
}
openxlsx::write.xlsx(p1_diff, "analyticalfiles/results/bas/p1_post_covered.xlsx")

# ref: after Shelby, non-Section V, non-annexing_bas ----
panel0020_did %<>%
  mutate(annexing_bas = relevel(as.factor(annexing_bas), ref = "0"), 
         period = relevel(as.factor(period), ref = "1"),
         vra = relevel(as.factor(vra), ref = "0"))

nhb_base <- feols(pctnhblack_p1 ~ as.factor(annexing_bas)*as.factor(period)*as.factor(vra) | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid)
summary(nhb_base)

nhb_base_cov <- feols(pctnhblack_p1 ~ as.factor(annexing_bas)*as.factor(period)*as.factor(vra) + pctnhblack_p0 + pctnhblackgrowth + pctnhblack_total | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid)
summary(nhb_base_cov) # N = 27987

nhw_base <- feols(pctnhwhite_p1 ~ as.factor(annexing_bas)*as.factor(vra)*as.factor(period) | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid)
summary(nhw_base) # N = 27987

nhw_base_cov <- feols(pctnhwhite_p1 ~ as.factor(annexing_bas)*as.factor(vra)*as.factor(period) + pctnhwhite_p0 + pctnhwhitegrowth + pctnhwhite_total | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid)
summary(nhw_base_cov) # N = 27987

nbmin_base <- feols(pctnbmin_p1 ~ as.factor(annexing_bas)*as.factor(vra)*as.factor(period) | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid)
summary(nbmin_base) # N = 27987

nbmin_base_cov <- feols(pctnbmin_p1 ~ as.factor(annexing_bas)*as.factor(vra)*as.factor(period) + pctnbmin_p0 + pctnbmingrowth + pctnbmin_total | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid)
summary(nbmin_base_cov) # N = 27987

p1_diff <- list(tidy(nhb_base), tidy(nhb_base_cov), tidy(nhw_base), tidy(nhw_base_cov), tidy(nbmin_base), tidy(nbmin_base_cov))
p1_diff_glance <- list(nhb_base, nhb_base_cov, nhw_base, nhw_base_cov, nbmin_base, nbmin_base_cov)

for (model_stat in names(glance(nhb_base))) {
  for (i in 1:6) {
    p1_diff[[i]][[model_stat]] <- glance(p1_diff_glance[[i]])[[model_stat]]
  }
}
openxlsx::write.xlsx(p1_diff, "analyticalfiles/results/bas/p1_post_uncovered.xlsx")

# is it just an anticipation effect? ----
# ref: pre, no VRA, no annexing_bas ----
plids <- Reduce(intersect, list(unique(panel0020_did$plid[panel0020_did$time == "2000 to 2007"]), unique(panel0020_did$plid[panel0020_did$time == "2014 to 2020"])))

panel0020_did %<>%
  filter(plid %in% plids)

panel0020_did %<>%
  mutate(vra = relevel(as.factor(vra), ref = "0"),
         period = relevel(as.factor(period), ref = "0"), 
         annexing_bas = relevel(as.factor(annexing_bas), ref = "0"))

nhb_base <- feols(pctnhblack_p1 ~ as.factor(annexing_bas)*as.factor(period)*as.factor(vra) | plid, data = panel0020_did %>% filter(time %in% c("2000 to 2007", "2014 to 2020")), cluster = ~plid)
summary(nhb_base)

nhb_base_cov <- feols(pctnhblack_p1 ~ as.factor(annexing_bas)*as.factor(period)*as.factor(vra) + pctnhblack_p0 + pctnhblackgrowth + pctnhblack_total | plid, data = panel0020_did %>% filter(time %in% c("2000 to 2007", "2014 to 2020")), cluster = ~plid)
summary(nhb_base_cov) # N = 27987

nhw_base <- feols(pctnhwhite_p1 ~ as.factor(annexing_bas)*as.factor(period)*as.factor(vra) | plid, data = panel0020_did %>% filter(time %in% c("2000 to 2007", "2014 to 2020")), cluster = ~plid)
summary(nhw_base) # N = 27987

nhw_base_cov <- feols(pctnhwhite_p1 ~ as.factor(annexing_bas)*as.factor(period)*as.factor(vra) + pctnhwhite_p0 + pctnhwhitegrowth + pctnhwhite_total | plid, data = panel0020_did %>% filter(time %in% c("2000 to 2007", "2014 to 2020")), cluster = ~plid)
summary(nhw_base_cov) # N = 27987

nbmin_base <- feols(pctnbmin_p1 ~ as.factor(annexing_bas)*as.factor(period)*as.factor(vra) | plid, data = panel0020_did %>% filter(time %in% c("2000 to 2007", "2014 to 2020")), cluster = ~plid)
summary(nbmin_base) # N = 27987

nbmin_base_cov <- feols(pctnbmin_p1 ~ as.factor(annexing_bas)*as.factor(period)*as.factor(vra) + pctnbmin_p0 + pctnbmingrowth + pctnbmin_total | plid, data = panel0020_did %>% filter(time %in% c("2000 to 2007", "2014 to 2020")), cluster = ~plid)
summary(nbmin_base_cov) # N = 27987

p1_diff <- list(tidy(nhb_base), tidy(nhb_base_cov), tidy(nhw_base), tidy(nhw_base_cov), tidy(nbmin_base), tidy(nbmin_base_cov))
p1_diff_glance <- list(nhb_base, nhb_base_cov, nhw_base, nhw_base_cov, nbmin_base, nbmin_base_cov)

for (model_stat in names(glance(nhb_base))) {
  for (i in 1:6) {
    p1_diff[[i]][[model_stat]] <- glance(p1_diff_glance[[i]])[[model_stat]]
  }
}
openxlsx::write.xlsx(p1_diff, "analyticalfiles/results/bas/0007_pre_uncovered.xlsx")

# ref: post, no VRA, no annexing_bas ----
panel0020_did %<>%
  mutate(vra = relevel(as.factor(vra), ref = "0"),
         period = relevel(as.factor(period), ref = "1"))

nhb_base <- feols(pctnhblack_p1 ~ as.factor(annexing_bas)*as.factor(period)*as.factor(vra) | plid, data = panel0020_did %>% filter(time %in% c("2000 to 2007", "2014 to 2020")), cluster = ~plid)
summary(nhb_base)

nhb_base_cov <- feols(pctnhblack_p1 ~ as.factor(annexing_bas)*as.factor(period)*as.factor(vra) + pctnhblack_p0 + pctnhblackgrowth + pctnhblack_total | plid, data = panel0020_did %>% filter(time %in% c("2000 to 2007", "2014 to 2020")), cluster = ~plid)
summary(nhb_base_cov) # N = 27987

nhw_base <- feols(pctnhwhite_p1 ~ as.factor(annexing_bas)*as.factor(period)*as.factor(vra) | plid, data = panel0020_did %>% filter(time %in% c("2000 to 2007", "2014 to 2020")), cluster = ~plid)
summary(nhw_base) # N = 27987

nhw_base_cov <- feols(pctnhwhite_p1 ~ as.factor(annexing_bas)*as.factor(period)*as.factor(vra) + pctnhwhite_p0 + pctnhwhitegrowth + pctnhwhite_total | plid, data = panel0020_did %>% filter(time %in% c("2000 to 2007", "2014 to 2020")), cluster = ~plid)
summary(nhw_base_cov) # N = 27987

nbmin_base <- feols(pctnbmin_p1 ~ as.factor(annexing_bas)*as.factor(period)*as.factor(vra) | plid, data = panel0020_did %>% filter(time %in% c("2000 to 2007", "2014 to 2020")), cluster = ~plid)
summary(nbmin_base) # N = 27987

nbmin_base_cov <- feols(pctnbmin_p1 ~ as.factor(annexing_bas)*as.factor(period)*as.factor(vra) + pctnbmin_p0 + pctnbmingrowth + pctnbmin_total | plid, data = panel0020_did %>% filter(time %in% c("2000 to 2007", "2014 to 2020")), cluster = ~plid)
summary(nbmin_base_cov) # N = 27987

p1_diff <- list(tidy(nhb_base), tidy(nhb_base_cov), tidy(nhw_base), tidy(nhw_base_cov), tidy(nbmin_base), tidy(nbmin_base_cov))
p1_diff_glance <- list(nhb_base, nhb_base_cov, nhw_base, nhw_base_cov, nbmin_base, nbmin_base_cov)

for (model_stat in names(glance(nhb_base))) {
  for (i in 1:6) {
    p1_diff[[i]][[model_stat]] <- glance(p1_diff_glance[[i]])[[model_stat]]
  }
}
openxlsx::write.xlsx(p1_diff, "analyticalfiles/results/bas/0007_post_uncovered.xlsx")

# ref: pre, VRA, no annexing_bas ----
panel0020_did %<>%
  mutate(vra = relevel(as.factor(vra), ref = "1"),
         period = relevel(as.factor(period), ref = "0"))

nhb_base <- feols(pctnhblack_p1 ~ as.factor(annexing_bas)*as.factor(period)*as.factor(vra) | plid, data = panel0020_did %>% filter(time %in% c("2000 to 2007", "2014 to 2020")), cluster = ~plid)
summary(nhb_base)

nhb_base_cov <- feols(pctnhblack_p1 ~ as.factor(annexing_bas)*as.factor(period)*as.factor(vra) + pctnhblack_p0 + pctnhblackgrowth + pctnhblack_total | plid, data = panel0020_did %>% filter(time %in% c("2000 to 2007", "2014 to 2020")), cluster = ~plid)
summary(nhb_base_cov) # N = 27987

nhw_base <- feols(pctnhwhite_p1 ~ as.factor(annexing_bas)*as.factor(period)*as.factor(vra) | plid, data = panel0020_did %>% filter(time %in% c("2000 to 2007", "2014 to 2020")), cluster = ~plid)
summary(nhw_base) # N = 27987

nhw_base_cov <- feols(pctnhwhite_p1 ~ as.factor(annexing_bas)*as.factor(period)*as.factor(vra) + pctnhwhite_p0 + pctnhwhitegrowth + pctnhwhite_total | plid, data = panel0020_did %>% filter(time %in% c("2000 to 2007", "2014 to 2020")), cluster = ~plid)
summary(nhw_base_cov) # N = 27987

nbmin_base <- feols(pctnbmin_p1 ~ as.factor(annexing_bas)*as.factor(period)*as.factor(vra) | plid, data = panel0020_did %>% filter(time %in% c("2000 to 2007", "2014 to 2020")), cluster = ~plid)
summary(nbmin_base) # N = 27987

nbmin_base_cov <- feols(pctnbmin_p1 ~ as.factor(annexing_bas)*as.factor(period)*as.factor(vra) + pctnbmin_p0 + pctnbmingrowth + pctnbmin_total | plid, data = panel0020_did %>% filter(time %in% c("2000 to 2007", "2014 to 2020")), cluster = ~plid)
summary(nbmin_base_cov) # N = 27987

p1_diff <- list(tidy(nhb_base), tidy(nhb_base_cov), tidy(nhw_base), tidy(nhw_base_cov), tidy(nbmin_base), tidy(nbmin_base_cov))
p1_diff_glance <- list(nhb_base, nhb_base_cov, nhw_base, nhw_base_cov, nbmin_base, nbmin_base_cov)

for (model_stat in names(glance(nhb_base))) {
  for (i in 1:6) {
    p1_diff[[i]][[model_stat]] <- glance(p1_diff_glance[[i]])[[model_stat]]
  }
}
openxlsx::write.xlsx(p1_diff, "analyticalfiles/results/bas/0007_pre_covered.xlsx")

# ref: post, VRA, no annexing_bas ----
panel0020_did %<>%
  mutate(vra = relevel(as.factor(vra), ref = "1"),
         period = relevel(as.factor(period), ref = "1"))

nhb_base <- feols(pctnhblack_p1 ~ as.factor(annexing_bas)*as.factor(period)*as.factor(vra) | plid, data = panel0020_did %>% filter(time %in% c("2000 to 2007", "2014 to 2020")), cluster = ~plid)
summary(nhb_base)

nhb_base_cov <- feols(pctnhblack_p1 ~ as.factor(annexing_bas)*as.factor(period)*as.factor(vra) + pctnhblack_p0 + pctnhblackgrowth + pctnhblack_total | plid, data = panel0020_did %>% filter(time %in% c("2000 to 2007", "2014 to 2020")), cluster = ~plid)
summary(nhb_base_cov) # N = 27987

nhw_base <- feols(pctnhwhite_p1 ~ as.factor(annexing_bas)*as.factor(period)*as.factor(vra) | plid, data = panel0020_did %>% filter(time %in% c("2000 to 2007", "2014 to 2020")), cluster = ~plid)
summary(nhw_base) # N = 27987

nhw_base_cov <- feols(pctnhwhite_p1 ~ as.factor(annexing_bas)*as.factor(vra)*as.factor(period) + pctnhwhite_p0 + pctnhwhitegrowth + pctnhwhite_total | plid, data = panel0020_did %>% filter(time %in% c("2000 to 2007", "2014 to 2020")), cluster = ~plid)
summary(nhw_base_cov) # N = 27987

nbmin_base <- feols(pctnbmin_p1 ~ as.factor(annexing_bas)*as.factor(vra)*as.factor(period) | plid, data = panel0020_did %>% filter(time %in% c("2000 to 2007", "2014 to 2020")), cluster = ~plid)
summary(nbmin_base) # N = 27987

nbmin_base_cov <- feols(pctnbmin_p1 ~ as.factor(annexing_bas)*as.factor(vra)*as.factor(period) + pctnbmin_p0 + pctnbmingrowth + pctnbmin_total | plid, data = panel0020_did %>% filter(time %in% c("2000 to 2007", "2014 to 2020")), cluster = ~plid)
summary(nbmin_base_cov) # N = 27987

p1_diff <- list(tidy(nhb_base), tidy(nhb_base_cov), tidy(nhw_base), tidy(nhw_base_cov), tidy(nbmin_base), tidy(nbmin_base_cov))
p1_diff_glance <- list(nhb_base, nhb_base_cov, nhw_base, nhw_base_cov, nbmin_base, nbmin_base_cov)

for (model_stat in names(glance(nhb_base))) {
  for (i in 1:6) {
    p1_diff[[i]][[model_stat]] <- glance(p1_diff_glance[[i]])[[model_stat]]
  }
}
openxlsx::write.xlsx(p1_diff, "analyticalfiles/results/bas/0007_post_covered.xlsx")

# poverty and hinc ---- 
panel0020_did %<>%
  mutate(vra = relevel(as.factor(vra), ref = "0"),
         period = relevel(as.factor(period), ref = "0"))

ppov_v0p0 <- feols(pctpov_p1 ~ as.factor(annexing_bas)*as.factor(period)*as.factor(vra) + pctpov_p0 | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid)
summary(ppov_v0p0) 

hinc_v0p0 <- feols(hinc_p1 ~ as.factor(annexing_bas)*as.factor(vra)*as.factor(period) + hinc_p0 | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid)
summary(hinc_v0p0) 

panel0020_did %<>%
  mutate(vra = relevel(as.factor(vra), ref = "0"),
         period = relevel(as.factor(period), ref = "1"))

ppov_v0p1 <- feols(pctpov_p1 ~ as.factor(annexing_bas)*as.factor(period)*as.factor(vra) + pctpov_p0 | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid)
summary(ppov_v0p1) 

hinc_v0p1 <- feols(hinc_p1 ~ as.factor(annexing_bas)*as.factor(vra)*as.factor(period) + hinc_p0 | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid)
summary(hinc_v0p1) 

panel0020_did %<>%
  mutate(vra = relevel(as.factor(vra), ref = "1"),
         period = relevel(as.factor(period), ref = "0"))

ppov_v1p0 <- feols(pctpov_p1 ~ as.factor(annexing_bas)*as.factor(period)*as.factor(vra) + pctpov_p0 | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid)
summary(ppov_v1p0) 

hinc_v1p0 <- feols(hinc_p1 ~ as.factor(annexing_bas)*as.factor(vra)*as.factor(period) + hinc_p0 | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid)
summary(hinc_v1p0) 

panel0020_did %<>%
  mutate(vra = relevel(as.factor(vra), ref = "1"),
         period = relevel(as.factor(period), ref = "1"))

ppov_v1p1 <- feols(pctpov_p1 ~ as.factor(annexing_bas)*as.factor(period)*as.factor(vra) + pctpov_p0 | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid)
summary(ppov_v1p1) 

hinc_v1p1 <- feols(hinc_p1 ~ as.factor(annexing_bas)*as.factor(vra)*as.factor(period) + hinc_p0 | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid)
summary(hinc_v1p1) 

p1_diff <- list(tidy(ppov_v0p0), tidy(hinc_v0p0), tidy(ppov_v0p1), tidy(hinc_v0p1), tidy(ppov_v1p0), tidy(hinc_v1p0), tidy(ppov_v1p1), tidy(hinc_v1p1))
p1_diff_glance <- list(ppov_v0p0, hinc_v0p0, ppov_v0p1, hinc_v0p1, ppov_v1p0, hinc_v1p0, ppov_v1p1, hinc_v1p1)

for (model_stat in names(glance(ppov_v0p0))) {
  for (i in 1:8) {
    p1_diff[[i]][[model_stat]] <- glance(p1_diff_glance[[i]])[[model_stat]]
  }
}
openxlsx::write.xlsx(p1_diff, "analyticalfiles/results/bas/ppov_hinc.xlsx")

