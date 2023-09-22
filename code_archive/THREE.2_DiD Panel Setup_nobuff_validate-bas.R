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

load(paste0("THREE_July.RData"))

curdir <- paste0(dirname(rstudioapi::getSourceEditorContext()$path), "/")
savedir <- paste0(curdir, "/../results/")

# make panel data!!!!! ####
# take out ne, alaska, and hawaii
#vraplids <- read_csv("analyticalfiles/vra_places.csv")
NE <- c("09", "15", "23", "25", "33", "34", "36", "42", "44", "50")
bas <- read_csv("analyticalfiles/bas_years_0020.csv")

pl_annex_var_0007 %<>%
  mutate(STATE = substr(plid, 1, 2)) %>%
  filter(!(STATE %in% NE)) 

pl_annex_var_0007 %<>%
  mutate(annexing_bas = ifelse(plid %in% bas$plid[bas$period=="2000 to 2007"], 1, 0),
         annexing_use = ifelse(annexing == 1 & annexing_bas == 1, 1, 0))
table(pl_annex_var_0007$annexing, pl_annex_var_0007$annexing_use)
2496/(2496 + 177)

pl_annex_var_0713 %<>%
  mutate(STATE = substr(plid, 1, 2)) %>%
  filter(!(STATE %in% NE)) 

pl_annex_var_0713 %<>%
  mutate(annexing_bas = ifelse(plid %in% bas$plid[bas$period=="2007 to 2013"], 1, 0),
         annexing_use = ifelse(annexing == 1 & annexing_bas == 1, 1, 0))
table(pl_annex_var_0713$annexing, pl_annex_var_0713$annexing_use)
2505/(2505+869)

pl_annex_var_1420 %<>%
  mutate(STATE = substr(plid, 1, 2)) %>%
  filter(!(STATE %in% NE)) 

pl_annex_var_1420 %<>%
  mutate(annexing_bas = ifelse(plid %in% bas$plid[bas$period=="2014 to 2020"], 1, 0),
         annexing_use = ifelse(annexing == 1 & annexing_bas == 1, 1, 0))
table(pl_annex_var_1420$annexing, pl_annex_var_1420$annexing_use)
362/(362+1242)

panel_annual <- rbind(pl_annex_var_0007, pl_annex_var_0713, pl_annex_var_1420)
panel0020_did <- panel_annual
rm(panel_annual)
names(panel0020_did) <- gsub("ppov", "pctpov", names(panel0020_did))
panel0020_did %<>%
  mutate(post = ifelse(time %in% "2014 to 2020", 1, 0), 
         STATE = substr(plid, 1, 2))

panel0020_did %<>%
  mutate_at(vars(c(pop, popdensity, pctnhblack_total, pctnbmin_total, pctowneroccupied, mhmval, hinc, pctpov, pctblackpov, pctnbminpov, pctownerocc_total, pcthincjobs_total, pctincopp_total)), 
            ~((.-mean(., na.rm = T))/sd(., na.rm = T))) 

# models ####
annex <- feols(annexing ~ as.factor(vra)*as.factor(post) | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid + STATE, fixef.rm = "none")
summary(annex)

annex_twop <- feols(annexing ~ as.factor(vra)*as.factor(post) + pop + popdensity + pctnhblack + pctnbmin + pctblackpov + pctnbminpov + pctowneroccupied + mhmval + hinc + pctpov + as.factor(more_white) + pctnhblack_total + pctnbmin_total + pctownerocc_total + pcthincjobs_total + pctincopp_total | plid + time, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid + STATE, fixef.rm = "none")
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

openxlsx::write.xlsx(annex_list, paste0(savedir, "t0020/annex_reg_annual.xlsx"))

# make coefficient plot ----
base_reg <- read_xlsx(path = paste0(savedir, "t0020/annex_reg_annual.xlsx"), sheet = 1) %>%
  select(est = estimate, se = std.error, term = term) %>%
  mutate(
    ci = 1.96*se,
    model = "Baseline",
    type = "Annexations, General"
  )

cov_reg <- read_xlsx(path = paste0(savedir, "t0020/annex_reg_annual.xlsx"), sheet = 2) %>%
  select(est = estimate, se = std.error, term = term) %>%
  mutate(
    ci = 1.96*se,
    model = "With Covariates",
    type = "Annexations, General"
  )

# binary comparison of racial composition ----
nhb <- feols(underbound_black ~ as.factor(post)*as.factor(vra) | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid + STATE)
summary(nhb)

nhb_cov <- feols(underbound_black ~ as.factor(post)*as.factor(vra) + pctnhblack + pctnhblack_total | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid + STATE)
summary(nhb_cov) 

nbmin <- feols(underbound_nbmin ~ as.factor(post)*as.factor(vra) | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid + STATE)
summary(nbmin)

nbmin_cov <- feols(underbound_nbmin ~ as.factor(post)*as.factor(vra) + pctnbmin + pctnbmin_total | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid + STATE)
summary(nbmin_cov) 

nhb_tid <- tidy(nhb)
for (model_stat in names(glance(nhb))) {
  nhb_tid[[model_stat]] <- glance(nhb)[[model_stat]]
}

nhb_cov_tid <- tidy(nhb_cov)
for (model_stat in names(glance(nhb_cov))) {
  nhb_cov_tid[[model_stat]] <- glance(nhb_cov)[[model_stat]]
}

nbmin_tid <- tidy(nbmin)
for (model_stat in names(glance(nbmin))) {
  nbmin_tid[[model_stat]] <- glance(nbmin)[[model_stat]]
}

nbmin_cov_tid <- tidy(nbmin_cov)
for (model_stat in names(glance(nbmin_cov))) {
  nbmin_cov_tid[[model_stat]] <- glance(nbmin_cov)[[model_stat]]
}

annex_list <- list(nhb_tid, nhb_cov_tid, 
                   nbmin_tid, nbmin_cov_tid)

openxlsx::write.xlsx(annex_list, paste0(savedir, "t0020/outcome_reg_annual.xlsx"))

# coef plot for underbound ----
base_nhb <- read_xlsx(path = paste0(savedir, "t0020/outcome_reg_annual.xlsx"), sheet = 1) %>%
  select(est = estimate, se = std.error, term = term) %>%
  mutate(
    ci = 1.96*se,
    model = "Baseline",
    type = "Annexations, Black-Diluting"
  )

cov_nhb <- read_xlsx(path = paste0(savedir, "t0020/outcome_reg_annual.xlsx"), sheet = 2) %>%
  select(est = estimate, se = std.error, term = term) %>%
  mutate(
    ci = 1.96*se,
    model = "With Covariates",
    type = "Annexations, Black-Diluting"
  )

base_nbm <- read_xlsx(path = paste0(savedir, "t0020/outcome_reg_annual.xlsx"), sheet = 3) %>%
  select(est = estimate, se = std.error, term = term) %>%
  mutate(
    ci = 1.96*se,
    model = "Baseline",
    type = "Annexations, Non-Black \nMinority-Diluting"
  )

cov_nbm <- read_xlsx(path = paste0(savedir, "t0020/outcome_reg_annual.xlsx"), sheet = 4) %>%
  select(est = estimate, se = std.error, term = term) %>%
  mutate(
    ci = 1.96*se,
    model = "With Covariates",
    type = "Annexations, Non-Black \nMinority-Diluting"
  )

df <- base::rbind(base_reg, cov_reg, base_nhb, cov_nhb, base_nbm, cov_nbm) %>%
  mutate(type = factor(type, levels = c("Annexations, General", "Annexations, Black-Diluting", "Annexations, Non-Black \nMinority-Diluting")))

plot = ggplot(df,
              aes(x = type, y = est, shape = model, color = type)) +
  scale_color_grey() + 
  geom_point(position=position_dodge(width=0.5), size = 2) + 
  geom_errorbar(aes(x = type, ymin = est - ci, ymax = est + ci), 
                width = 0.2, position=position_dodge(width=0.5)) + 
  geom_hline(yintercept = 0, linetype = "dashed", size = 0.2) +
  ylab("Coefficients, 95% CIs") +
  xlab(NULL) +
  theme_bw() + 
  guides(color = FALSE) +
  theme(
    #axis.text.x=element_blank(),
    axis.ticks.x = element_blank(),
    legend.title = element_blank(),
    legend.position = "bottom") 
plot
ggsave(filename = paste0(savedir, "t0020/coeffs.pdf"),
       plot = plot, 
       dpi = 300)

# BAS ----
annex <- feols(annexing_use ~ as.factor(vra)*as.factor(post) | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid + STATE, fixef.rm = "none")
summary(annex)

annex_twop <- feols(annexing_use ~ as.factor(vra)*as.factor(post) + pop + popdensity + pctnhblack + pctnbmin + pctblackpov + pctnbminpov + pctowneroccupied + mhmval + hinc + pctpov + as.factor(more_white) + pctnhblack_total + pctnbmin_total + pctownerocc_total + pcthincjobs_total + pctincopp_total | plid + time, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid + STATE, fixef.rm = "none")
summary(annex_twop)

panel0020_did %<>%
  mutate(underbound_black_use = ifelse(underbound_black == 1 & annexing_use == 1, 1, 0),
         underbound_nbmin_use = ifelse(underbound_nbmin == 1 & annexing_use == 1, 1, 0))

nhb <- feols(underbound_black_use ~ as.factor(post)*as.factor(vra) | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid + STATE)
summary(nhb)

nhb_cov <- feols(underbound_black_use ~ as.factor(post)*as.factor(vra) + pctnhblack + pctnhblack_total | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid + STATE)
summary(nhb_cov) 

nbmin <- feols(underbound_nbmin_use ~ as.factor(post)*as.factor(vra) | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid + STATE)
summary(nbmin)

nbmin_cov <- feols(underbound_nbmin_use ~ as.factor(post)*as.factor(vra) + pctnbmin + pctnbmin_total | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid + STATE)
summary(nbmin_cov) 
