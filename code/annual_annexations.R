# -------------------------------------------------------------------------
# Created by: Iris Zhang                     
# Date created: 07/10/2023             
# Last revised:            
# Project: MBC         
# Subproject: Analysis
# Re: Do annual annexations analysis for R&R       
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

# 2007-2008 ----
aa0708 <- read_csv("analyticalfiles/annexedblocks0708dem.csv") 
names(aa0708)

# transform this into place-level summaries
# characteristic of all annexable blocks
# diff between annexed and not annexed 
pl0708 <- aa0708 %>% 
  group_by(plid) %>%
  dplyr::summarize(pop_total = sum(pop, na.rm = T),
                  pct_annexed = mean(annexed, na.rm = T), 
                  vra = mean(vra, na.rm = T)) %>%
  ungroup() %>%
  mutate(annexing = ifelse(pct_annexed > 0, 1, 0),
         vra = ifelse(vra > 0, 1, 0),
         period = "0708",
         state = substr(plid, 1, 2))
table(pl0708$annexing)
sapply(pl0708, function(x) sum(is.na(x)))  
