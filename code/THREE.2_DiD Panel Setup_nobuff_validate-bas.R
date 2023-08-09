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

curdir <- paste0(dirname(rstudioapi::getSourceEditorContext()$path), "/")
savedir <- paste0(curdir, "../results/")

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

load(paste0(savedir, "THREE_July.RData"))

# make panel data!!!!! ####
# take out ne, alaska, and hawaii
#vraplids <- read_csv("analyticalfiles/vra_places.csv")
NE <- c("09", "23", "25", "33", "34", "36", "42", "44", "50")
bas <- read_csv("analyticalfiles/bas_years.csv")

pl0007 %<>%
  mutate(STATE = substr(plid, 1, 2)) %>%
  filter(!(STATE %in% NE)) 

pl0007 %<>%
  mutate(annexing_bas = ifelse(plid %in% bas$plid[bas$period=="2000 to 2007"], 1, 0),
         annexing_use = ifelse(annexing == 1 & annexing_bas == 1, 1, 0))
table(pl0007$annexing, pl0007$annexing_use)
2149/(2149 + 118)
# 94.5% agreement

pl0713 %<>%
  mutate(STATE = substr(plid, 1, 2)) %>%
  filter(!(STATE %in% NE)) 

pl0713 %<>%
  mutate(annexing_bas = ifelse(plid %in% bas$plid[bas$period=="2007 to 2013"], 1, 0),
         annexing_use = ifelse(annexing == 1 & annexing_bas == 1, 1, 0))
table(pl0713$annexing, pl0713$annexing_use)
2287/(2287+828)
#73.4%

pl1420 %<>%
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
  mutate(vra = ifelse(plid %in% vraplids, 1, 0))
pl0713 %<>%
  filter(plid %in% plids) %>%
  select(all_of(names_list)) %>%
  mutate(vra = ifelse(plid %in% vraplids, 1, 0))
pl1420 %<>%
  filter(plid %in% plids) %>%
  select(all_of(names_list)) %>%
  mutate(vra = ifelse(plid %in% vraplids, 1, 0))

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
  filter_at(vars(pop_p0, popdensity_p0, popgrowth, pctnhwhite_p0, pctnhwhitegrowth, pctnhblack_total, pctnbmin_total, pctnhblackgrowth, pctnbmingrowth, more_white, pctowneroccupied_p0, mhmval_p0, hinc_p0, pctpov_p0, pctownerocc_total, pcthincjobs_total, pctincopp_total, pctnhblack_p1, pctnhwhite_p1, pctnbmin_p1, annexing, annexing_bas, pctnhwhite_diff, pctnhblack_diff, pctnbmin_diff), ~!is.na(.))

plids <- Reduce(intersect, list(unique(panel0020_did$plid[panel0020_did$time == "2007 to 2013"]), unique(panel0020_did$plid[panel0020_did$time == "2014 to 2020"])))

#plids <- Reduce(intersect, list(unique(panel0020_did$plid[panel0020_did$time == "2000 to 2007"]), unique(panel0020_did$plid[panel0020_did$time == "2007 to 2013"]), unique(panel0020_did$plid[panel0020_did$time == "2014 to 2020"])))

panel0020_did %<>%
  filter(plid %in% plids)

table(panel0020_did$annexing[panel0020_did$time=="2000 to 2007"], panel0020_did$annexing_use[panel0020_did$time=="2000 to 2007"])
2092/(2092+109)

table(panel0020_did$annexing[panel0020_did$time=="2007 to 2013"], panel0020_did$annexing_use[panel0020_did$time=="2007 to 2013"])
2272/(2272+795)

table(panel0020_did$annexing[panel0020_did$time=="2014 to 2020"], panel0020_did$annexing_use[panel0020_did$time=="2014 to 2020"])
328/(328+1049)
# 18.94%

length(unique(panel0020_did$plid))

#write_csv(panel0020_did, "analyticalfiles/panel_prestandard.csv")
#panel0020_did <- read_csv("analyticalfiles/panel_prestandard.csv")

summary(panel0020_did$popgrowth)
panel0020_did %<>%
  mutate_at(vars(c(ends_with("total"), ends_with("_p0"), ends_with("_p1"), ends_with("_total_1"), contains("growth"), contains("_annexed"), ends_with("_log"), contains("diff"))), 
            ~((.-mean(., na.rm = T))/sd(., na.rm = T))) 
summary(panel0020_did$popgrowth)
summary(panel0020_did)

# models ####

# annex or not ----
annex <- feols(annexing ~ as.factor(vra)*as.factor(period) | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid + STATE, fixef.rm = "none")
summary(annex)

annex_twop <- feols(annexing ~ as.factor(vra)*as.factor(period) + pop_p0 + popdensity_p0 + popgrowth + pctnhblack_p0 + pctnhblackgrowth + pctnbmin_p0 + pctnbmingrowth + pctblackpov_p0 + pctnbminpov_p0 + pctowneroccupied_p0 + mhmval_p0 + hinc_p0 + pctpov_p0 + as.factor(more_white) + pctnhblack_total + pctnbmin_total + pctownerocc_total + pcthincjobs_total + pctincopp_total | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid + STATE, fixef.rm = "none")
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

openxlsx::write.xlsx(annex_list, paste0(savedir, "annex_reg.xlsx"))

# binary comparison of racial composition ----
base <- feols(white_comp ~ as.factor(period)*as.factor(vra) | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid + STATE)
summary(base)

base_cov <- feols(white_comp ~ as.factor(period)*as.factor(vra) + pctnhwhite_p0 + pctnhwhitegrowth + pctnhwhite_total | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid + STATE)
summary(base_cov) 

nhb <- feols(underbound_black ~ as.factor(period)*as.factor(vra) | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid + STATE)
summary(nhb)

nhb_cov <- feols(underbound_black ~ as.factor(period)*as.factor(vra) + pctnhblack_p0 + pctnhblackgrowth + pctnhblack_total | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid + STATE)
summary(nhb_cov) 

h <- feols(underbound_hisp ~ as.factor(period)*as.factor(vra) | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid + STATE)
summary(h)

h_cov <- feols(underbound_hisp ~ as.factor(period)*as.factor(vra) + pcth_p0 + pcthgrowth + pcth_total | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid + STATE)
summary(h_cov) 

native <- feols(underbound_native ~ as.factor(period)*as.factor(vra) | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid + STATE)
summary(native)

native_cov <- feols(underbound_native ~ as.factor(period)*as.factor(vra) + pctnative_p0 + pctnativegrowth + pctnative_total | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid + STATE)
summary(native_cov) 

asian <- feols(underbound_asian ~ as.factor(period)*as.factor(vra) | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid + STATE)
summary(asian)

asian_cov <- feols(underbound_asian ~ as.factor(period)*as.factor(vra) + pctasian_p0 + pctasiangrowth + pctasian_total | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid + STATE)
summary(asian_cov) 

nbmin <- feols(underbound_nbmin ~ as.factor(period)*as.factor(vra) | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid + STATE)
summary(nbmin)

nbmin_cov <- feols(underbound_nbmin ~ as.factor(period)*as.factor(vra) + pctnbmin_p0 + pctnbmingrowth + pctnbmin_total | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid + STATE)
summary(nbmin_cov) 

base_tid <- tidy(base)
for (model_stat in names(glance(base))) {
  base_tid[[model_stat]] <- glance(base)[[model_stat]]
}

base_cov_tid <- tidy(base_cov)
for (model_stat in names(glance(base_cov))) {
  base_cov_tid[[model_stat]] <- glance(base_cov)[[model_stat]]
}

nhb_tid <- tidy(nhb)
for (model_stat in names(glance(nhb))) {
  nhb_tid[[model_stat]] <- glance(nhb)[[model_stat]]
}

nhb_cov_tid <- tidy(nhb_cov)
for (model_stat in names(glance(nhb_cov))) {
  nhb_cov_tid[[model_stat]] <- glance(nhb_cov)[[model_stat]]
}

h_tid <- tidy(h)
for (model_stat in names(glance(h))) {
  h_tid[[model_stat]] <- glance(h)[[model_stat]]
}

h_cov_tid <- tidy(h_cov)
for (model_stat in names(glance(h_cov))) {
  h_cov_tid[[model_stat]] <- glance(h_cov)[[model_stat]]
}

native_tid <- tidy(native)
for (model_stat in names(glance(native))) {
  native_tid[[model_stat]] <- glance(native)[[model_stat]]
}

native_cov_tid <- tidy(native_cov)
for (model_stat in names(glance(native_cov))) {
  native_cov_tid[[model_stat]] <- glance(native_cov)[[model_stat]]
}

asian_tid <- tidy(asian)
for (model_stat in names(glance(asian))) {
  asian_tid[[model_stat]] <- glance(asian)[[model_stat]]
}

asian_cov_tid <- tidy(asian_cov)
for (model_stat in names(glance(asian_cov))) {
  asian_cov_tid[[model_stat]] <- glance(asian_cov)[[model_stat]]
}

nbmin_tid <- tidy(nbmin)
for (model_stat in names(glance(nbmin))) {
  nbmin_tid[[model_stat]] <- glance(nbmin)[[model_stat]]
}

nbmin_cov_tid <- tidy(nbmin_cov)
for (model_stat in names(glance(nbmin_cov))) {
  nbmin_cov_tid[[model_stat]] <- glance(nbmin_cov)[[model_stat]]
}

annex_list <- list(base_tid, base_cov_tid, 
                   nhb_tid, nhb_cov_tid, 
                   h_tid, h_cov_tid, 
                   native_tid, native_cov_tid, 
                   asian_tid, asian_cov_tid, 
                   nbmin_tid, nbmin_cov_tid)

openxlsx::write.xlsx(annex_list, paste0(savedir, "outcome_reg.xlsx"))

# validated against BAS ----
annex <- feols(annexing_use ~ as.factor(vra)*as.factor(period) | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid + STATE, fixef.rm = "none")
summary(annex)

annex_twop <- feols(annexing_use ~ as.factor(vra)*as.factor(period) + pop_p0 + popdensity_p0 + popgrowth + pctnhblack_p0 + pctnhblackgrowth + pctnbmin_p0 + pctnbmingrowth + pctblackpov_p0 + pctnbminpov_p0 + pctowneroccupied_p0 + mhmval_p0 + hinc_p0 + pctpov_p0 + as.factor(more_white) + pctnhblack_total + pctnbmin_total + pctownerocc_total + pcthincjobs_total + pctincopp_total | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid + STATE, fixef.rm = "none")
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

openxlsx::write.xlsx(annex_list, paste0(savedir, "annex_reg_bas.xlsx"))

# binary comparison of racial composition ----
panel0020_did_bas <- panel0020_did

panel0020_did_bas %<>%
  mutate(white_comp = ifelse(white_comp == 1 & annexing_use == 0, 0, white_comp),
         underbound_black = ifelse(underbound_black == 1 & annexing_use == 0, 0, underbound_black),
         underbound_hisp = ifelse(underbound_hisp == 1 & annexing_use == 0, 0, underbound_hisp),
         underbound_native = ifelse(underbound_native == 1 & annexing_use == 0, 0, underbound_native),
         underbound_asian = ifelse(underbound_asian == 1 & annexing_use == 0, 0, underbound_asian),
         underbound_nbmin = ifelse(underbound_nbmin == 1 & annexing_use == 0, 0, underbound_nbmin))

base <- feols(white_comp ~ as.factor(period)*as.factor(vra) | plid, data = panel0020_did_bas %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid + STATE)
summary(base)

base_cov <- feols(white_comp ~ as.factor(period)*as.factor(vra) + pctnhwhite_p0 + pctnhwhitegrowth + pctnhwhite_total | plid, data = panel0020_did_bas %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid + STATE)
summary(base_cov) 

nhb <- feols(underbound_black ~ as.factor(period)*as.factor(vra) | plid, data = panel0020_did_bas %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid + STATE)
summary(nhb)

nhb_cov <- feols(underbound_black ~ as.factor(period)*as.factor(vra) + pctnhblack_p0 + pctnhblackgrowth + pctnhblack_total | plid, data = panel0020_did_bas %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid + STATE)
summary(nhb_cov) 

h <- feols(underbound_hisp ~ as.factor(period)*as.factor(vra) | plid, data = panel0020_did_bas %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid + STATE)
summary(h)

h_cov <- feols(underbound_hisp ~ as.factor(period)*as.factor(vra) + pcth_p0 + pcthgrowth + pcth_total | plid, data = panel0020_did_bas %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid + STATE)
summary(h_cov) 

native <- feols(underbound_native ~ as.factor(period)*as.factor(vra) | plid, data = panel0020_did_bas %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid + STATE)
summary(native)

native_cov <- feols(underbound_native ~ as.factor(period)*as.factor(vra) + pctnative_p0 + pctnativegrowth + pctnative_total | plid, data = panel0020_did_bas %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid + STATE)
summary(native_cov) 

asian <- feols(underbound_asian ~ as.factor(period)*as.factor(vra) | plid, data = panel0020_did_bas %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid + STATE)
summary(asian)

asian_cov <- feols(underbound_asian ~ as.factor(period)*as.factor(vra) + pctasian_p0 + pctasiangrowth + pctasian_total | plid, data = panel0020_did_bas %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid + STATE)
summary(asian_cov) 

nbmin <- feols(underbound_nbmin ~ as.factor(period)*as.factor(vra) | plid, data = panel0020_did_bas %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid + STATE)
summary(nbmin)

nbmin_cov <- feols(underbound_nbmin ~ as.factor(period)*as.factor(vra) + pctnbmin_p0 + pctnbmingrowth + pctnbmin_total | plid, data = panel0020_did_bas %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid + STATE)
summary(nbmin_cov) 

base_tid <- tidy(base)
for (model_stat in names(glance(base))) {
  base_tid[[model_stat]] <- glance(base)[[model_stat]]
}

base_cov_tid <- tidy(base_cov)
for (model_stat in names(glance(base_cov))) {
  base_cov_tid[[model_stat]] <- glance(base_cov)[[model_stat]]
}

nhb_tid <- tidy(nhb)
for (model_stat in names(glance(nhb))) {
  nhb_tid[[model_stat]] <- glance(nhb)[[model_stat]]
}

nhb_cov_tid <- tidy(nhb_cov)
for (model_stat in names(glance(nhb_cov))) {
  nhb_cov_tid[[model_stat]] <- glance(nhb_cov)[[model_stat]]
}

h_tid <- tidy(h)
for (model_stat in names(glance(h))) {
  h_tid[[model_stat]] <- glance(h)[[model_stat]]
}

h_cov_tid <- tidy(h_cov)
for (model_stat in names(glance(h_cov))) {
  h_cov_tid[[model_stat]] <- glance(h_cov)[[model_stat]]
}

native_tid <- tidy(native)
for (model_stat in names(glance(native))) {
  native_tid[[model_stat]] <- glance(native)[[model_stat]]
}

native_cov_tid <- tidy(native_cov)
for (model_stat in names(glance(native_cov))) {
  native_cov_tid[[model_stat]] <- glance(native_cov)[[model_stat]]
}

asian_tid <- tidy(asian)
for (model_stat in names(glance(asian))) {
  asian_tid[[model_stat]] <- glance(asian)[[model_stat]]
}

asian_cov_tid <- tidy(asian_cov)
for (model_stat in names(glance(asian_cov))) {
  asian_cov_tid[[model_stat]] <- glance(asian_cov)[[model_stat]]
}

nbmin_tid <- tidy(nbmin)
for (model_stat in names(glance(nbmin))) {
  nbmin_tid[[model_stat]] <- glance(nbmin)[[model_stat]]
}

nbmin_cov_tid <- tidy(nbmin_cov)
for (model_stat in names(glance(nbmin_cov))) {
  nbmin_cov_tid[[model_stat]] <- glance(nbmin_cov)[[model_stat]]
}

annex_list <- list(base_tid, base_cov_tid, 
                   nhb_tid, nhb_cov_tid, 
                   h_tid, h_cov_tid, 
                   native_tid, native_cov_tid, 
                   asian_tid, asian_cov_tid, 
                   nbmin_tid, nbmin_cov_tid)

openxlsx::write.xlsx(annex_list, paste0(savedir, "outcome_reg_bas.xlsx"))

# falsification test ----
panel0020_did %<>%
  filter(vra==0)

# randomly assign "treatment" to untreated units and run did on them 
length(unique(panel0020_did$plid))
fplid <- unique(panel0020_did$plid)
x <- sample(0:1,length(unique(panel0020_did$plid)),replace=T)
plid_ftreat <- as.data.frame(cbind(fplid, x)) %>%
  rename(plid = fplid, 
         ftreat = x)

panel0020_did %<>%
  left_join(plid_ftreat, by = "plid")

annex <- feols(annexing ~ as.factor(ftreat)*as.factor(period) | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid + STATE, fixef.rm = "none")
summary(annex)

annex_twop <- feols(annexing ~ as.factor(ftreat)*as.factor(period) + pop_p0 + popdensity_p0 + popgrowth + pctnhblack_p0 + pctnhblackgrowth + pctnbmin_p0 + pctnbmingrowth + pctblackpov_p0 + pctnbminpov_p0 + pctowneroccupied_p0 + mhmval_p0 + hinc_p0 + pctpov_p0 + as.factor(more_white) + pctnhblack_total + pctnbmin_total + pctownerocc_total + pcthincjobs_total + pctincopp_total | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid + STATE, fixef.rm = "none")
summary(annex_twop)

base <- feols(white_comp ~ as.factor(period)*as.factor(ftreat) | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid + STATE)
summary(base)

base_cov <- feols(white_comp ~ as.factor(period)*as.factor(ftreat) + pctnhwhite_p0 + pctnhwhitegrowth + pctnhwhite_total | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid + STATE)
summary(base_cov) 

nhb <- feols(underbound_black ~ as.factor(period)*as.factor(ftreat) | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid + STATE)
summary(nhb)

nhb_cov <- feols(underbound_black ~ as.factor(period)*as.factor(ftreat) + pctnhblack_p0 + pctnhblackgrowth + pctnhblack_total | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid + STATE)
summary(nhb_cov) 

nbmin <- feols(underbound_nbmin ~ as.factor(period)*as.factor(ftreat) | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid + STATE)
summary(nbmin)

nbmin_cov <- feols(underbound_nbmin ~ as.factor(period)*as.factor(ftreat) + pctnbmin_p0 + pctnbmingrowth + pctnbmin_total | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid + STATE)
summary(nbmin_cov) 

# another sensitivity test: vary treatment time ----
panel0020_did %<>%
  filter(!time %in% "2014 to 2020") %>%
  mutate(period = ifelse(time %in% "2007 to 2013", 1, 0))

annex <- feols(annexing ~ as.factor(vra)*as.factor(period) | plid, data = panel0020_did, cluster = ~plid + STATE, fixef.rm = "none")
summary(annex)

annex_twop <- feols(annexing ~ as.factor(vra)*as.factor(period) + pop_p0 + popdensity_p0 + popgrowth + pctnhblack_p0 + pctnhblackgrowth + pctnbmin_p0 + pctnbmingrowth + pctblackpov_p0 + pctnbminpov_p0 + pctowneroccupied_p0 + mhmval_p0 + hinc_p0 + pctpov_p0 + as.factor(more_white) + pctnhblack_total + pctnbmin_total + pctownerocc_total + pcthincjobs_total + pctincopp_total | plid, data = panel0020_did, cluster = ~plid + STATE, fixef.rm = "none")
summary(annex_twop)

base <- feols(white_comp ~ as.factor(period)*as.factor(vra) | plid, data = panel0020_did, cluster = ~plid + STATE)
summary(base)

base_cov <- feols(white_comp ~ as.factor(period)*as.factor(vra) + pctnhwhite_p0 + pctnhwhitegrowth + pctnhwhite_total | plid, data = panel0020_did, cluster = ~plid + STATE)
summary(base_cov) 

nhb <- feols(underbound_black ~ as.factor(period)*as.factor(vra) | plid, data = panel0020_did, cluster = ~plid + STATE)
summary(nhb)

nhb_cov <- feols(underbound_black ~ as.factor(period)*as.factor(vra) + pctnhblack_p0 + pctnhblackgrowth + pctnhblack_total | plid, data = panel0020_did, cluster = ~plid + STATE)
summary(nhb_cov) 

nbmin_cov <- feols(underbound_nbmin ~ as.factor(period)*as.factor(vra) + pctnbmin_p0 + pctnbmingrowth + pctnbmin_total | plid, data = panel0020_did, cluster = ~plid + STATE)
summary(nbmin_cov) 
