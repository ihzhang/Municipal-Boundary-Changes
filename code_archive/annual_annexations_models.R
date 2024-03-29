# -------------------------------------------------------------------------
# Created by: Iris Zhang                     
# Date created: 8/3/2023             
# Last revised:              
# Project: MBC         
# Subproject: Analysis
# Re: Run analysis using annual annexation data (annual_annexations.R)        
# -------------------------------------------------------------------------

rm(list = ls())
# Script Description ------------------------------------------------------

# Inputs:
# 

# Outputs:
# 

# Updates log: 

# Setup -------------------------------------------------------------------

# Packages: 


# Directories: 
setwd("~/Google Drive/My Drive/Stanford/QE2")

homedir <- paste0(dirname(rstudioapi::getSourceEditorContext()$path), "/")
savedir <- paste0(homedir, "/../results/")

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

load("annual_annexations.RData")

names(panel_annual) <- gsub("ppov", "pctpov", names(panel_annual))
panel_annual %<>%
  mutate(STATE = substr(plid, 1, 2))
sapply(panel_annual, function(x) sum(is.na(x)))

panel_annual %<>%
  filter_at(vars(pop, popdensity, pctnhblack_total, pctnbmin_total, more_white, pctowneroccupied, mhmval, hinc, pctpov, pctblackpov, pctnbminpov, pctownerocc_total, pcthincjobs_total, pctincopp_total), ~!is.na(.))

panel_annual %<>%
  mutate_at(vars(c(pop, popdensity, pctnhblack_total, pctnbmin_total, pctowneroccupied, mhmval, hinc, pctpov, pctblackpov, pctnbminpov, pctownerocc_total, pcthincjobs_total, pctincopp_total)), 
            ~((.-mean(., na.rm = T))/sd(., na.rm = T))) 

# annex or not ----
annex <- feols(annexing ~ as.factor(vra)*as.factor(time) | plid + time, data = panel_annual, cluster = ~plid + STATE, fixef.rm = "none")
summary(annex)

annex_twop <- feols(annexing ~ as.factor(vra)*as.factor(time) + pop + popdensity + pctnhblack + pctnbmin + pctblackpov + pctnbminpov + pctowneroccupied + mhmval + hinc + pctpov + as.factor(more_white) + pctnhblack_total + pctnbmin_total + pctownerocc_total + pcthincjobs_total + pctincopp_total | plid + time, data = panel_annual, cluster = ~plid + STATE, fixef.rm = "none")
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

openxlsx::write.xlsx(annex_list, paste0(savedir, "annex_reg_annual.xlsx"))

# make coefficient plot ----
base <- read_xlsx(path = paste0(savedir, "annex_reg_annual.xlsx"), sheet = 1) %>%
  select(est = estimate, se = std.error, term = term) %>%
  mutate(
    ci = 1.96*se,
    model = "Baseline"
      )

cov <- read_xlsx(path = paste0(savedir, "annex_reg_annual.xlsx"), sheet = 2) %>%
  select(est = estimate, se = std.error, term = term) %>%
  mutate(
    ci = 1.96*se,
    model = "With Covariates"
  )

data <- base::rbind(base, cov) 

plot = ggplot(data,
              aes(x = term, y = est, shape = model, group = model)) +
  geom_point(position=position_dodge(width=0.5), size = 2) + 
  geom_errorbar(aes(x = term, ymin = est - ci, ymax = est + ci), 
                width = 0.2, position=position_dodge(width=0.5)) + 
  geom_hline(yintercept = 0, linetype = "dashed", size = 0.2) +
  ylab("Coefficient, 95% CIs") +
  xlab(NULL) +
  theme_bw() + 
  theme(
        axis.ticks.x = element_blank(),
        legend.title = element_blank(),
        legend.position = "bottom")
plot
ggsave(filename = paste0(savedir, "reg_annual.pdf"),
                        plot = plot, 
                        dpi = 300)

# binary comparison of racial composition ----
nhb <- feols(underbound_black ~ as.factor(time)*as.factor(vra) | plid + time, data = panel_annual, cluster = ~plid + STATE)
summary(nhb)

nhb_cov <- feols(underbound_black ~ as.factor(time)*as.factor(vra) + pctnhblack + pctnhblack_total | plid + time, data = panel_annual, cluster = ~plid + STATE)
summary(nhb_cov) 

nbmin <- feols(underbound_nbmin ~ as.factor(time)*as.factor(vra) | plid + time, data = panel_annual, cluster = ~plid + STATE)
summary(nbmin)

nbmin_cov <- feols(underbound_nbmin ~ as.factor(time)*as.factor(vra) + pctnbmin + pctnbmin_total | plid + time, data = panel_annual, cluster = ~plid + STATE)
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

openxlsx::write.xlsx(annex_list, paste0(savedir, "outcome_reg_annual.xlsx"))

# coef plot for underbound ----
base <- read_xlsx(path = paste0(savedir, "outcome_reg_annual.xlsx"), sheet = 1) %>%
  select(est = estimate, se = std.error, term = term) %>%
  mutate(
    ci = 1.96*se,
    model = "Baseline"
  )

cov <- read_xlsx(path = paste0(savedir, "outcome_reg_annual.xlsx"), sheet = 2) %>%
  select(est = estimate, se = std.error, term = term) %>%
  mutate(
    ci = 1.96*se,
    model = "With Covariates"
  )

nhb <- base::rbind(base, cov) 

plot = ggplot(nhb,
              aes(x = term, y = est, shape = model, group = model)) +
  geom_point(position=position_dodge(width=0.5), size = 2) + 
  geom_errorbar(aes(x = term, ymin = est - ci, ymax = est + ci), 
                width = 0.2, position=position_dodge(width=0.5)) + 
  geom_hline(yintercept = 0, linetype = "dashed", size = 0.2) +
  ylab("Coefficient, 95% CIs") +
  xlab(NULL) +
  theme_bw() + 
  theme(
    axis.ticks.x = element_blank(),
    legend.title = element_blank(),
    legend.position = "bottom") 
plot
ggsave(filename = paste0(savedir, "nhb_annual.pdf"),
       plot = plot, 
       dpi = 300)

base <- read_xlsx(path = paste0(savedir, "outcome_reg_annual.xlsx"), sheet = 3) %>%
  select(est = estimate, se = std.error, term = term) %>%
  mutate(
    ci = 1.96*se,
    model = "Baseline"
  )

cov <- read_xlsx(path = paste0(savedir, "outcome_reg_annual.xlsx"), sheet = 4) %>%
  select(est = estimate, se = std.error, term = term) %>%
  mutate(
    ci = 1.96*se,
    model = "With Covariates"
  )

nbm <- base::rbind(base, cov) 

plot = ggplot(nbm,
              aes(x = term, y = est, shape = model, group = model)) +
  geom_point(position=position_dodge(width=0.5), size = 2) + 
  geom_errorbar(aes(x = term, ymin = est - ci, ymax = est + ci), 
                width = 0.2, position=position_dodge(width=0.5)) + 
  geom_hline(yintercept = 0, linetype = "dashed", size = 0.2) +
  ylab("Coefficient, 95% CIs") +
  xlab(NULL) +
  theme_bw() + 
  theme(
    axis.ticks.x = element_blank(),
    legend.title = element_blank(),
    legend.position = "bottom") 
plot
ggsave(filename = paste0(savedir, "nhb_annual.pdf"),
       plot = plot, 
       dpi = 300)

# falsification test ----
# randomly assign "treatment" to untreated units and run did on them 
panel_annual %<>%
  filter(vra==0)

length(unique(panel_annual$plid))
fplid <- unique(panel_annual$plid)
x <- sample(0:1,length(unique(panel_annual$plid)),replace=T)
plid_ftreat <- as.data.frame(cbind(fplid, x)) %>%
  rename(plid = fplid, 
         ftreat = x)

panel_annual %<>%
  left_join(plid_ftreat, by = "plid")

annex <- feols(annexing ~ as.factor(ftreat)*as.factor(post) | plid, data = panel_annual, cluster = ~plid + STATE, fixef.rm = "none")
summary(annex)

annex_twop <- feols(annexing ~ as.factor(ftreat)*as.factor(post) + pop + popdensity + pctnhblack + pctnbmin + pctblackpov + pctnbminpov + pctowneroccupied + mhmval + hinc + pctpov + as.factor(more_white) + pctnhblack_total + pctnbmin_total + pctownerocc_total + pcthincjobs_total + pctincopp_total | plid, data = panel_annual, cluster = ~plid + STATE, fixef.rm = "none")
summary(annex_twop)

base <- feols(white_comp ~ as.factor(ftreat)*as.factor(post) | plid, data = panel_annual, cluster = ~plid + STATE)
summary(base)

base_cov <- feols(white_comp ~ as.factor(post)*as.factor(ftreat) + pctnhwhite + pctnhwhite_total | plid, data = panel_annual, cluster = ~plid + STATE)
summary(base_cov) 

nhb <- feols(underbound_black ~ as.factor(post)*as.factor(ftreat) | plid, data = panel_annual, cluster = ~plid + STATE)
summary(nhb)

nhb_cov <- feols(underbound_black ~ as.factor(post)*as.factor(ftreat) + pctnhblack + pctnhblack_total | plid, data = panel_annual, cluster = ~plid + STATE)
summary(nhb_cov) 

nbmin <- feols(underbound_nbmin ~ as.factor(post)*as.factor(ftreat) | plid, data = panel_annual, cluster = ~plid + STATE)
summary(nbmin)

nbmin_cov <- feols(underbound_nbmin ~ as.factor(post)*as.factor(ftreat) + pctnbmin + pctnbmin_total | plid, data = panel_annual, cluster = ~plid + STATE)
summary(nbmin_cov) 

# falsification test - different years ----
panel_annual %<>%
  filter(!time %in% c("2014 to 2015", "2015 to 2016", "2016 to 2017", "2017 to 2018", "2018 to 2019", "2019 to 2020")) %>%
  mutate(period = ifelse(time %in% c("2010 to 2011", "2011 to 2012", "2012 to 2013"), 1, 0))

annex <- feols(annexing ~ as.factor(vra)*as.factor(period) | plid, data = panel_annual, cluster = ~plid + STATE, fixef.rm = "none")
summary(annex)

annex_twop <- feols(annexing ~ as.factor(vra)*as.factor(period) + pop + popdensity + pctnhblack + pctnbmin + pctblackpov + pctnbminpov + pctowneroccupied + mhmval + hinc + pctpov + as.factor(more_white) + pctnhblack_total + pctnbmin_total + pctownerocc_total + pcthincjobs_total + pctincopp_total | plid, data = panel_annual, cluster = ~plid + STATE, fixef.rm = "none")
summary(annex_twop)

base <- feols(white_comp ~ as.factor(period)*as.factor(vra) | plid, data = panel_annual, cluster = ~plid + STATE)
summary(base)

base_cov <- feols(white_comp ~ as.factor(period)*as.factor(vra) + pctnhwhite +  pctnhwhite_total | plid, data = panel_annual, cluster = ~plid + STATE)
summary(base_cov) 

nhb <- feols(underbound_black ~ as.factor(period)*as.factor(vra) | plid, data = panel_annual, cluster = ~plid + STATE)
summary(nhb)

nhb_cov <- feols(underbound_black ~ as.factor(period)*as.factor(vra) + pctnhblack +  pctnhblack_total | plid, data = panel_annual, cluster = ~plid + STATE)
summary(nhb_cov) 

nbmin_cov <- feols(underbound_nbmin ~ as.factor(period)*as.factor(vra) + pctnbmin + pctnbmin_total | plid, data = panel_annual, cluster = ~plid + STATE)
summary(nbmin_cov) 

# honestDID package ----
remotes::install_github("asheshrambachan/HonestDiD")
library(HonestDiD)

twfe_results <- fixest::feols(annexing ~ i(time, vra, ref = "2012 to 2013") | plid + time, 
                              cluster = ~plid + STATE,
                              data = panel_annual)


betahat <- summary(twfe_results)$coefficients #save the coefficients
sigma <- summary(twfe_results)$cov.scaled #save the covariance matrix

fixest::iplot(twfe_results)

delta_rm_results <- 
  HonestDiD::createSensitivityResults_relativeMagnitudes(
    betahat = betahat, #coefficients
    sigma = sigma, #covariance matrix
    numPrePeriods = 5, #num. of pre-treatment coefs
    numPostPeriods = 6, #num. of post-treatment coefs
    Mbarvec = seq(0.5,2,by=0.5) #values of Mbar
  )

delta_rm_results

originalResults <- HonestDiD::constructOriginalCS(betahat = betahat,
                                                  sigma = sigma,
                                                  numPrePeriods = 5,
                                                  numPostPeriods = 6)

HonestDiD::createSensitivityPlot_relativeMagnitudes(delta_rm_results, originalResults)

twfe_results <- fixest::feols(underbound_black ~ i(time, vra, ref = "2012 to 2013") | plid + time, 
                              cluster = ~plid + STATE,
                              data = panel_annual)


betahat <- summary(twfe_results)$coefficients #save the coefficients
sigma <- summary(twfe_results)$cov.scaled #save the covariance matrix

fixest::iplot(twfe_results)

delta_rm_results <- 
  HonestDiD::createSensitivityResults_relativeMagnitudes(
    betahat = betahat, #coefficients
    sigma = sigma, #covariance matrix
    numPrePeriods = 5, #num. of pre-treatment coefs
    numPostPeriods = 6, #num. of post-treatment coefs
    Mbarvec = seq(0.5,2,by=0.5) #values of Mbar
  )

delta_rm_results

originalResults <- HonestDiD::constructOriginalCS(betahat = betahat,
                                                  sigma = sigma,
                                                  numPrePeriods = 5,
                                                  numPostPeriods = 6)

HonestDiD::createSensitivityPlot_relativeMagnitudes(delta_rm_results, originalResults)

twfe_results <- fixest::feols(underbound_nbmin ~ i(time, vra, ref = "2012 to 2013") | plid + time, 
                              cluster = ~plid + STATE,
                              data = panel_annual)


betahat <- summary(twfe_results)$coefficients #save the coefficients
sigma <- summary(twfe_results)$cov.scaled #save the covariance matrix

fixest::iplot(twfe_results)

delta_rm_results <- 
  HonestDiD::createSensitivityResults_relativeMagnitudes(
    betahat = betahat, #coefficients
    sigma = sigma, #covariance matrix
    numPrePeriods = 5, #num. of pre-treatment coefs
    numPostPeriods = 6, #num. of post-treatment coefs
    Mbarvec = seq(0.5,2,by=0.5) #values of Mbar
  )

delta_rm_results

originalResults <- HonestDiD::constructOriginalCS(betahat = betahat,
                                                  sigma = sigma,
                                                  numPrePeriods = 5,
                                                  numPostPeriods = 6)

HonestDiD::createSensitivityPlot_relativeMagnitudes(delta_rm_results, originalResults)

# validation against BAS ----
bas <- read_csv("analyticalfiles/bas_years.csv") %>%
  rename(n_annex = n)

panel_annual %<>%
  left_join(bas, by = c("plid" = "plid",
                        "time" = "period")) %>%
mutate(annexing_bas = ifelse(!is.na(n_annex), 1, 0),
       annexing_use = ifelse(annexing == 1 & annexing_bas == 1, 1, 0))

table(panel_annual$annexing_use[panel_annual$time == "2007 to 2008"])
table(panel_annual$annexing[panel_annual$time == "2007 to 2008"], panel_annual$annexing_use[panel_annual$time == "2007 to 2008"])

table(panel_annual$annexing_use[panel_annual$time == "2008 to 2009"])
table(panel_annual$annexing[panel_annual$time == "2008 to 2009"], panel_annual$annexing_use[panel_annual$time == "2008 to 2009"])

table(panel_annual$annexing_use[panel_annual$time == "2009 to 2010"])
table(panel_annual$annexing[panel_annual$time == "2009 to 2010"], panel_annual$annexing_use[panel_annual$time == "2009 to 2010"])

table(panel_annual$annexing_use[panel_annual$time == "2010 to 2011"])
table(panel_annual$annexing[panel_annual$time == "2010 to 2011"], panel_annual$annexing_use[panel_annual$time == "2010 to 2011"])

table(panel_annual$annexing_use[panel_annual$time == "2011 to 2012"])
table(panel_annual$annexing[panel_annual$time == "2011 to 2012"], panel_annual$annexing_use[panel_annual$time == "2011 to 2012"])

table(panel_annual$annexing_use[panel_annual$time == "2012 to 2013"])
table(panel_annual$annexing[panel_annual$time == "2012 to 2013"], panel_annual$annexing_use[panel_annual$time == "2012 to 2013"])

table(panel_annual$annexing_use[panel_annual$time == "2014 to 2015"])
table(panel_annual$annexing[panel_annual$time == "2014 to 2015"], panel_annual$annexing_use[panel_annual$time == "2014 to 2015"])

table(panel_annual$annexing_use[panel_annual$time == "2015 to 2016"])
table(panel_annual$annexing[panel_annual$time == "2015 to 2016"], panel_annual$annexing_use[panel_annual$time == "2015 to 2016"])

table(panel_annual$annexing_use[panel_annual$time == "2016 to 2017"])
table(panel_annual$annexing[panel_annual$time == "2016 to 2017"], panel_annual$annexing_use[panel_annual$time == "2016 to 2017"])

table(panel_annual$annexing_use[panel_annual$time == "2017 to 2018"])
table(panel_annual$annexing[panel_annual$time == "2017 to 2018"], panel_annual$annexing_use[panel_annual$time == "2017 to 2018"])

table(panel_annual$annexing_use[panel_annual$time == "2018 to 2019"])
table(panel_annual$annexing[panel_annual$time == "2018 to 2019"], panel_annual$annexing_use[panel_annual$time == "2018 to 2019"])

table(panel_annual$annexing_use[panel_annual$time == "2019 to 2020"])
table(panel_annual$annexing[panel_annual$time == "2019 to 2020"], panel_annual$annexing_use[panel_annual$time == "2019 to 2020"])

# just compare vra to itself 
annex <- feols(annexing ~ as.factor(post) | plid, data = panel_annual %>% filter(vra == 1), cluster = ~plid + STATE, fixef.rm = "none")
summary(annex)

annex_twop <- feols(annexing ~ as.factor(post) + pop + popdensity + pctnhblack + pctnbmin + pctblackpov + pctnbminpov + pctowneroccupied + mhmval + hinc + pctpov + as.factor(more_white) + pctnhblack_total + pctnbmin_total + pctownerocc_total + pcthincjobs_total + pctincopp_total | plid, data = panel_annual %>% filter(vra == 1), cluster = ~plid + STATE, fixef.rm = "none")
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

openxlsx::write.xlsx(annex_list, paste0(savedir, "annex_reg_annual.xlsx"))

# collapse into two years ----
shrunk <- panel_annual %>%
  mutate(collapsed = case_when(
    time %in% c("2007 to 2008", "2008 to 2009") ~ "2007 to 2009",
    time %in% c("2009 to 2010", "2010 to 2011") ~ "2009 to 2011",
    time %in% c("2011 to 2012", "2012 to 2013") ~ "2011 to 2013",
    time %in% c("2014 to 2015", "2015 to 2016") ~ "2014 to 2016",
    time %in% c("2016 to 2017", "2017 to 2018") ~ "2016 to 2018",
    TRUE ~ "2018 to 2020"
  )) %>%
  group_by(plid, collapsed) %>%
  summarize(vra = mean(vra),
            post = mean(post),
            annexing = mean(annexing),
            underbound_black = mean(underbound_black),
            underbound_nbmin = mean(underbound_nbmin)) %>%
  mutate(underbound_black = ifelse(underbound_black > 0, 1, 0),
         underbound_nbmin = ifelse(underbound_nbmin > 0, 1, 0),
         time = case_when(
           collapsed == "2007 to 2009" ~ "2007 to 2008",
           collapsed == "2009 to 2011" ~ "2009 to 2010",
           collapsed == "2011 to 2013" ~ "2011 to 2012",
           collapsed == "2014 to 2016" ~ "2014 to 2015",
           collapsed == "2016 to 2018" ~ "2016 to 2017",
           collapsed == "2018 to 2020" ~ "2018 to 2019"
         ))

pta <- shrunk %>%
  group_by(vra, collapsed) %>%
  dplyr::summarize(annexing = mean(annexing)*100) %>%
  ungroup() %>%
  mutate(vra = as.factor(vra)) 

shelby <- pta %>%
  filter(collapsed == "2011 to 2013") %>%
  mutate(collapsed = "2013, Shelby County", 
         annexing = NA)

pta <- rbind(pta, shelby)  

gg = ggplot(pta, aes(y=annexing,x=collapsed, group = vra)) +
  geom_point() + geom_line(aes(linetype = vra)) + 
  geom_vline(xintercept="2013, Shelby County") +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5)) +
  ylab("% of Places Annexing") + 
  xlab("Annual Period") + 
  labs(linetype = "VRA Coverage")
gg

ggsave(filename = paste0(savedir, "two_year/annex_PTA.pdf"),
       plot = gg, 
       dpi = 300)

pta <- shrunk %>%
  group_by(vra, collapsed) %>%
  dplyr::summarize(annexing = mean(underbound_black)*100) %>%
  ungroup() %>%
  mutate(vra = as.factor(vra)) 

shelby <- pta %>%
  filter(collapsed == "2011 to 2013") %>%
  mutate(collapsed = "2013, Shelby County", 
         annexing = NA)

pta <- rbind(pta, shelby)  

gg = ggplot(pta, aes(y=annexing,x=collapsed, group = vra)) +
  geom_point() + geom_line(aes(linetype = vra)) + 
  geom_vline(xintercept="2013, Shelby County") +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5)) +
  ylab("% of Places Annexing") + 
  xlab("Annual Period") + 
  labs(linetype = "VRA Coverage")
gg

ggsave(filename = paste0(savedir, "two_year/annex_PTA.pdf"),
       plot = gg, 
       dpi = 300)

pta <- shrunk %>%
  group_by(vra, collapsed) %>%
  dplyr::summarize(annexing = mean(underbound_nbmin)*100) %>%
  ungroup() %>%
  mutate(vra = as.factor(vra)) 

shelby <- pta %>%
  filter(collapsed == "2011 to 2013") %>%
  mutate(collapsed = "2013, Shelby County", 
         annexing = NA)

pta <- rbind(pta, shelby)  

gg = ggplot(pta, aes(y=annexing,x=collapsed, group = vra)) +
  geom_point() + geom_line(aes(linetype = vra)) + 
  geom_vline(xintercept="2013, Shelby County") +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5)) +
  ylab("% of Places Annexing") + 
  xlab("Annual Period") + 
  labs(linetype = "VRA Coverage")
gg

ggsave(filename = paste0(savedir, "two_year/annex_PTA.pdf"),
       plot = gg, 
       dpi = 300)

shrunk %<>% 
  left_join(panel_annual %>% select(-c(annexing, underbound_black, underbound_nbmin, post, vra)), by = c("plid", "time"))

annex <- feols(annexing ~ as.factor(vra)*as.factor(collapsed) | plid + collapsed, data = shrunk, cluster = ~plid + STATE, fixef.rm = "none")
summary(annex)

annex_twop <- feols(annexing ~ as.factor(vra)*as.factor(collapsed) + pop + popdensity + pctnhblack + pctnbmin + pctblackpov + pctnbminpov + pctowneroccupied + mhmval + hinc + pctpov + as.factor(more_white) + pctnhblack_total + pctnbmin_total + pctownerocc_total + pcthincjobs_total + pctincopp_total | plid + collapsed, data = shrunk, cluster = ~plid + STATE, fixef.rm = "none")
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

openxlsx::write.xlsx(annex_list, paste0(savedir, "two_year/annex_reg_annual.xlsx"))

# make coefficient plot ----
base <- read_xlsx(path = paste0(savedir, "two_year/annex_reg_annual.xlsx"), sheet = 1) %>%
  select(est = estimate, se = std.error, term = term) %>%
  mutate(
    ci = 1.96*se,
    model = "Baseline"
  )

cov <- read_xlsx(path = paste0(savedir, "two_year/annex_reg_annual.xlsx"), sheet = 2) %>%
  select(est = estimate, se = std.error, term = term) %>%
  mutate(
    ci = 1.96*se,
    model = "With Covariates"
  )

data <- base::rbind(base, cov) 

plot = ggplot(data,
              aes(x = term, y = est, shape = model, group = model)) +
  geom_point(position=position_dodge(width=0.5), size = 2) + 
  geom_errorbar(aes(x = term, ymin = est - ci, ymax = est + ci), 
                width = 0.2, position=position_dodge(width=0.5)) + 
  geom_hline(yintercept = 0, linetype = "dashed", size = 0.2) +
  ylab("Coefficient, 95% CIs") +
  xlab(NULL) +
  theme_bw() + 
  theme(
    axis.ticks.x = element_blank(),
    legend.title = element_blank(),
    legend.position = "bottom")
plot
ggsave(filename = paste0(savedir, "two_year/reg_annual.pdf"),
       plot = plot, 
       dpi = 300)

# binary comparison of racial composition ----
nhb <- feols(underbound_black ~ as.factor(collapsed)*as.factor(vra) | plid + collapsed, data = shrunk, cluster = ~plid + STATE)
summary(nhb)

nhb_cov <- feols(underbound_black ~ as.factor(collapsed)*as.factor(vra) + pctnhblack + pctnhblack_total | plid + collapsed, data = shrunk, cluster = ~plid + STATE)
summary(nhb_cov) 

nbmin <- feols(underbound_nbmin ~ as.factor(collapsed)*as.factor(vra) | plid + collapsed, data = shrunk, cluster = ~plid + STATE)
summary(nbmin)

nbmin_cov <- feols(underbound_nbmin ~ as.factor(collapsed)*as.factor(vra) + pctnbmin + pctnbmin_total | plid + collapsed, data = shrunk, cluster = ~plid + STATE)
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

openxlsx::write.xlsx(annex_list, paste0(savedir, "two_year/outcome_reg_annual.xlsx"))

# coef plot for underbound ----
base <- read_xlsx(path = paste0(savedir, "two_year/outcome_reg_annual.xlsx"), sheet = 1) %>%
  select(est = estimate, se = std.error, term = term) %>%
  mutate(
    ci = 1.96*se,
    model = "Baseline"
  )

cov <- read_xlsx(path = paste0(savedir, "two_year/outcome_reg_annual.xlsx"), sheet = 2) %>%
  select(est = estimate, se = std.error, term = term) %>%
  mutate(
    ci = 1.96*se,
    model = "With Covariates"
  )

nhb <- base::rbind(base, cov) 

plot = ggplot(nhb,
              aes(x = term, y = est, shape = model, group = model)) +
  geom_point(position=position_dodge(width=0.5), size = 2) + 
  geom_errorbar(aes(x = term, ymin = est - ci, ymax = est + ci), 
                width = 0.2, position=position_dodge(width=0.5)) + 
  geom_hline(yintercept = 0, linetype = "dashed", size = 0.2) +
  ylab("Coefficient, 95% CIs") +
  xlab(NULL) +
  theme_bw() + 
  theme(
    axis.ticks.x = element_blank(),
    legend.title = element_blank(),
    legend.position = "bottom") 
plot
ggsave(filename = paste0(savedir, "two_year/nhb_annual.pdf"),
       plot = plot, 
       dpi = 300)

base <- read_xlsx(path = paste0(savedir, "two_year/outcome_reg_annual.xlsx"), sheet = 3) %>%
  select(est = estimate, se = std.error, term = term) %>%
  mutate(
    ci = 1.96*se,
    model = "Baseline"
  )

cov <- read_xlsx(path = paste0(savedir, "two_year/outcome_reg_annual.xlsx"), sheet = 4) %>%
  select(est = estimate, se = std.error, term = term) %>%
  mutate(
    ci = 1.96*se,
    model = "With Covariates"
  )

nbm <- base::rbind(base, cov) 

plot = ggplot(nbm,
              aes(x = term, y = est, shape = model, group = model)) +
  geom_point(position=position_dodge(width=0.5), size = 2) + 
  geom_errorbar(aes(x = term, ymin = est - ci, ymax = est + ci), 
                width = 0.2, position=position_dodge(width=0.5)) + 
  geom_hline(yintercept = 0, linetype = "dashed", size = 0.2) +
  ylab("Coefficient, 95% CIs") +
  xlab(NULL) +
  theme_bw() + 
  theme(
    axis.ticks.x = element_blank(),
    legend.title = element_blank(),
    legend.position = "bottom") 
plot
ggsave(filename = paste0(savedir, "two_year/nhb_annual.pdf"),
       plot = plot, 
       dpi = 300)
