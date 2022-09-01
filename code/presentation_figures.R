# Header -------------------------------------------------------------------------
# Created by: Vasudha Kumar, Modified by Iris Zhang         
# Date created: 2 Aug 2022
# Last revised:
# Project: MBC
# Subproject: N/A
# Re: Create figures for ASA
# -------------------------------------------------------------------------

# Script Description ------------------------------------------------------

## Inputs:

# Rent stabilization diff-in-diff estimates for each SES
#
# 'did/results/july_results/RC/moved_bg'
# 'did/results/july_results/inmover'
# 'did/results/july_results/constrained_ppov'

# Just Cause diff-in-diff estimates for each SES
#
# 'report_briefs/results/main_models/full/mnl/jc.csv'

## Outputs:
# Dot & whisker plots with rent stabilization diff-in-diff estimates (for moving out, moving in, and constrained moves based on ppov)
# Bar chart with results for rent stabilization multinomial logit results

# Update log: 
# 8/2/22: update for new results to make plots for ASA 

# Setup -------------------------------------------------------------------

## Libraries -----
library(tidyverse)
library(readxl)

## Directories ----
homedir <- paste0(dirname(rstudioapi::getSourceEditorContext()$path))
workdir <- "/../results/july_results"
savedir <- "/../figures/"
setwd(paste0(homedir, workdir))

## Parameters ----
ses_levels <- c("ELI", "VLI-LI", "Moderate-Middle", "Middle-High")

## Import data ----
# Diff-in-diff data

# Function to import and wrangle did data
read_did <- function(subdir, sheet) {
  data <- NULL
  
  files <- list.files(path = getwd(), pattern = subdir)
  
  for (file in files) {
    temp <- read_xlsx(path = paste0(getwd(), "/", file), sheet = sheet) %>%
      filter(term %in% "treated") %>%
      select(est = estimate, se = std.error) %>%
      mutate(
        # Relabel ses cats
        ses = case_when(
          grepl("Low", file) ~ "ELI",
          grepl("Moderate", file) ~ "VLI-LI",
          grepl("Middle", file) ~ "Moderate-Middle",
          grepl("High", file) ~ "Middle-High",
        ),
        # Calculate 95% confidence intervals
        ci = 1.96*se
      )
    data <- rbind(data, temp)
  }
  data$ses = factor(data$ses,
                  levels = ses_levels)
  return(data)
}

moveout <- read_did(subdir = "test_twfe_moved_ct", 2)
movein <- read_did(subdir = "test_twfe_inmover_ct", 2)
constrain <- read_did(subdir = "test_twfe_constrained_ppov", 2)

# Create plots -------------------------------------------------------------
setwd(paste0(homedir, savedir))

## Plot diff-in-diff estimates ------
# Moving out for each ses group
plot = ggplot(moveout,
              aes(ses, est, color = ses)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = est - ci, ymax = est + ci), 
                width = 0.2) + 
  geom_hline(yintercept = 0, linetype = "dashed", size = 0.2) +
  ylab("DID Estimate") +
  xlab(NULL) +
  theme_bw() + 
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.title = element_blank(),
        legend.position = "bottom")
plot
ggsave("rc_moved_ct.png",
       plot = plot,
       width = 6,
       height = 3,
       dpi = 300)

# Moving in for each ses group
plot = ggplot(movein,
              aes(ses, est, color = ses)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = est - ci, ymax = est + ci), 
                width = 0.2) + 
  geom_hline(yintercept = 0, linetype = "dashed", size = 0.2) +
  ylab("DID Estimate") +
  xlab(NULL) +
  theme_bw() + 
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.title = element_blank(),
        legend.position = "bottom")
plot
ggsave("rc_inmover_ct.png",
       plot = plot, 
       width = 6,
       height = 3,
       dpi = 300)

# Constrained based on poverty rates for each ses group
plot = ggplot(constrain,
              aes(ses, est, color = ses)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = est - ci, ymax = est + ci), 
                width = 0.2) + 
  geom_hline(yintercept = 0, linetype = "dashed", size = 0.2) +
  ylab("DID Estimate") +
  xlab(NULL) +
  theme_bw() + 
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.title = element_blank(),
        legend.position = "bottom")
plot
ggsave("rc_constrained_ppov.png",
       plot = plot, 
       width = 6,
       height = 3,
       dpi = 300)

# JC ----
setwd(paste0(homedir, workdir))
moveout <- read_did(subdir = "test_twfe_moved_ct", 1)
movein <- read_did(subdir = "test_twfe_inmover_ct", 1)
constrain <- read_did(subdir = "test_twfe_constrained_ppov", 1)

# Create plots -------------------------------------------------------------
setwd(paste0(homedir, savedir))

## Plot diff-in-diff estimates ------
# Moving out for each ses group
plot = ggplot(moveout,
              aes(ses, est, color = ses)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = est - ci, ymax = est + ci), 
                width = 0.2) + 
  geom_hline(yintercept = 0, linetype = "dashed", size = 0.2) +
  ylab("DID Estimate") +
  xlab(NULL) +
  theme_bw() + 
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.title = element_blank(),
        legend.position = "bottom")
plot
ggsave("jc_moved_ct.png",
       plot = plot,
       width = 6,
       height = 3,
       dpi = 300)

# Moving in for each ses group
plot = ggplot(movein,
              aes(ses, est, color = ses)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = est - ci, ymax = est + ci), 
                width = 0.2) + 
  geom_hline(yintercept = 0, linetype = "dashed", size = 0.2) +
  ylab("DID Estimate") +
  xlab(NULL) +
  theme_bw() + 
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.title = element_blank(),
        legend.position = "bottom")
plot
ggsave("jc_inmover_ct.png",
       plot = plot, 
       width = 6,
       height = 3,
       dpi = 300)

# Constrained based on poverty rates for each ses group
plot = ggplot(constrain,
              aes(ses, est, color = ses)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = est - ci, ymax = est + ci), 
                width = 0.2) + 
  geom_hline(yintercept = 0, linetype = "dashed", size = 0.2) +
  ylab("DID Estimate") +
  xlab(NULL) +
  theme_bw() + 
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.title = element_blank(),
        legend.position = "bottom")
plot
ggsave("jc_constrained_ppov.png",
       plot = plot, 
       width = 6,
       height = 3,
       dpi = 300)

# minority neighborhoods ---- 
setwd(paste0(homedir, workdir))
moveout <- read_did(subdir = "test_twfe_min_moved_ct", 1)
movein <- read_did(subdir = "test_twfe_min_inmover_ct", 1)
constrain <- read_did(subdir = "test_twfe_min_constrained_ppov", 1)

# Create plots -------------------------------------------------------------
setwd(paste0(homedir, savedir))

## Plot diff-in-diff estimates ------
# Moving out for each ses group
plot = ggplot(moveout,
              aes(ses, est, color = ses)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = est - ci, ymax = est + ci), 
                width = 0.2) + 
  geom_hline(yintercept = 0, linetype = "dashed", size = 0.2) +
  ylab("DID Estimate") +
  xlab(NULL) +
  theme_bw() + 
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.title = element_blank(),
        legend.position = "bottom")
plot
ggsave("jc_moved_ct_min.png",
       plot = plot,
       width = 6,
       height = 3,
       dpi = 300)

# Moving in for each ses group
plot = ggplot(movein,
              aes(ses, est, color = ses)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = est - ci, ymax = est + ci), 
                width = 0.2) + 
  geom_hline(yintercept = 0, linetype = "dashed", size = 0.2) +
  ylab("DID Estimate") +
  xlab(NULL) +
  theme_bw() + 
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.title = element_blank(),
        legend.position = "bottom")
plot
ggsave("jc_inmover_ct_min.png",
       plot = plot, 
       width = 6,
       height = 3,
       dpi = 300)

# Constrained based on poverty rates for each ses group
plot = ggplot(constrain,
              aes(ses, est, color = ses)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = est - ci, ymax = est + ci), 
                width = 0.2) + 
  geom_hline(yintercept = 0, linetype = "dashed", size = 0.2) +
  ylab("DID Estimate") +
  xlab(NULL) +
  theme_bw() + 
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.title = element_blank(),
        legend.position = "bottom")
plot
ggsave("jc_constrained_ppov_min.png",
       plot = plot, 
       width = 6,
       height = 3,
       dpi = 300)

# RS
setwd(paste0(homedir, workdir))
moveout <- read_did(subdir = "test_twfe_min_moved_ct", 2)
movein <- read_did(subdir = "test_twfe_min_inmover_ct", 2)
constrain <- read_did(subdir = "test_twfe_min_constrained_ppov", 2)

# Create plots -------------------------------------------------------------
setwd(paste0(homedir, savedir))

## Plot diff-in-diff estimates ------
# Moving out for each ses group
plot = ggplot(moveout,
              aes(ses, est, color = ses)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = est - ci, ymax = est + ci), 
                width = 0.2) + 
  geom_hline(yintercept = 0, linetype = "dashed", size = 0.2) +
  ylab("DID Estimate") +
  xlab(NULL) +
  theme_bw() + 
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.title = element_blank(),
        legend.position = "bottom")
plot
ggsave("rc_moved_ct_min.png",
       plot = plot,
       width = 6,
       height = 3,
       dpi = 300)

# Moving in for each ses group
plot = ggplot(movein,
              aes(ses, est, color = ses)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = est - ci, ymax = est + ci), 
                width = 0.2) + 
  geom_hline(yintercept = 0, linetype = "dashed", size = 0.2) +
  ylab("DID Estimate") +
  xlab(NULL) +
  theme_bw() + 
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.title = element_blank(),
        legend.position = "bottom")
plot
ggsave("rc_inmover_ct_min.png",
       plot = plot, 
       width = 6,
       height = 3,
       dpi = 300)

# Constrained based on poverty rates for each ses group
plot = ggplot(constrain,
              aes(ses, est, color = ses)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = est - ci, ymax = est + ci), 
                width = 0.2) + 
  geom_hline(yintercept = 0, linetype = "dashed", size = 0.2) +
  ylab("DID Estimate") +
  xlab(NULL) +
  theme_bw() + 
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.title = element_blank(),
        legend.position = "bottom")
plot
ggsave("rc_constrained_ppov_min.png",
       plot = plot, 
       width = 6,
       height = 3,
       dpi = 300)

# gentrifying neighborhoods ----
setwd(paste0(homedir, workdir))
moveout <- read_did(subdir = "test_twfe_gent_moved_ct", 1)
movein <- read_did(subdir = "test_twfe_gent_inmover_ct", 1)
constrain <- read_did(subdir = "test_twfe_gent_constrained_ppov", 1)

# Create plots -------------------------------------------------------------
setwd(paste0(homedir, savedir))

## Plot diff-in-diff estimates ------
# Moving out for each ses group
plot = ggplot(moveout,
              aes(ses, est, color = ses)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = est - ci, ymax = est + ci), 
                width = 0.2) + 
  geom_hline(yintercept = 0, linetype = "dashed", size = 0.2) +
  ylab("DID Estimate") +
  xlab(NULL) +
  theme_bw() + 
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.title = element_blank(),
        legend.position = "bottom")
plot
ggsave("jc_moved_ct_gent.png",
       plot = plot,
       width = 6,
       height = 3,
       dpi = 300)

# Moving in for each ses group
plot = ggplot(movein,
              aes(ses, est, color = ses)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = est - ci, ymax = est + ci), 
                width = 0.2) + 
  geom_hline(yintercept = 0, linetype = "dashed", size = 0.2) +
  ylab("DID Estimate") +
  xlab(NULL) +
  theme_bw() + 
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.title = element_blank(),
        legend.position = "bottom")
plot
ggsave("jc_inmover_ct_gent.png",
       plot = plot, 
       width = 6,
       height = 3,
       dpi = 300)

# Constrained based on poverty rates for each ses group
plot = ggplot(constrain,
              aes(ses, est, color = ses)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = est - ci, ymax = est + ci), 
                width = 0.2) + 
  geom_hline(yintercept = 0, linetype = "dashed", size = 0.2) +
  ylab("DID Estimate") +
  xlab(NULL) +
  theme_bw() + 
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.title = element_blank(),
        legend.position = "bottom")
plot
ggsave("jc_constrained_ppov_gent.png",
       plot = plot, 
       width = 6,
       height = 3,
       dpi = 300)

# RS
setwd(paste0(homedir, workdir))
moveout <- read_did(subdir = "test_twfe_gent_moved_ct", 2)
movein <- read_did(subdir = "test_twfe_gent_inmover_ct", 2)
constrain <- read_did(subdir = "test_twfe_gent_constrained_ppov", 2)

# Create plots -------------------------------------------------------------
setwd(paste0(homedir, savedir))

## Plot diff-in-diff estimates ------
# Moving out for each ses group
plot = ggplot(moveout,
              aes(ses, est, color = ses)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = est - ci, ymax = est + ci), 
                width = 0.2) + 
  geom_hline(yintercept = 0, linetype = "dashed", size = 0.2) +
  ylab("DID Estimate") +
  xlab(NULL) +
  theme_bw() + 
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.title = element_blank(),
        legend.position = "bottom")
plot
ggsave("rc_moved_ct_gent.png",
       plot = plot,
       width = 6,
       height = 3,
       dpi = 300)

# Moving in for each ses group
plot = ggplot(movein,
              aes(ses, est, color = ses)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = est - ci, ymax = est + ci), 
                width = 0.2) + 
  geom_hline(yintercept = 0, linetype = "dashed", size = 0.2) +
  ylab("DID Estimate") +
  xlab(NULL) +
  theme_bw() + 
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.title = element_blank(),
        legend.position = "bottom")
plot
ggsave("rc_inmover_ct_gent.png",
       plot = plot, 
       width = 6,
       height = 3,
       dpi = 300)

# Constrained based on poverty rates for each ses group
plot = ggplot(constrain,
              aes(ses, est, color = ses)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = est - ci, ymax = est + ci), 
                width = 0.2) + 
  geom_hline(yintercept = 0, linetype = "dashed", size = 0.2) +
  ylab("DID Estimate") +
  xlab(NULL) +
  theme_bw() + 
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.title = element_blank(),
        legend.position = "bottom")
plot
ggsave("rc_constrained_ppov_gent.png",
       plot = plot, 
       width = 6,
       height = 3,
       dpi = 300)


