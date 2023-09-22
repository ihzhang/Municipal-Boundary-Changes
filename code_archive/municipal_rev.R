# -------------------------------------------------------------------------
# Created by: Iris Zhang                     
# Date created: 2021             
# Last revised: 6/30/2022             
# Project: MBC         
# Subproject: Analysis
# Re: Get time-varying place-level data for all places in dataset, time-varying would be best
# if not, interpolate and extrapolate. 
# -------------------------------------------------------------------------

rm(list = ls())
# Script Description ------------------------------------------------------
# 
# Inputs:
# data/'PropertyTax8.19.22.csv', CREDIT: Brenden Beck 

# Outputs:
# pctrev from property taxes 

# Updates log: 
# 

# Setup -------------------------------------------------------------------

# Packages: 


# Directories: 
homedir <- paste0(dirname(rstudioapi::getSourceEditorContext()$path))
workdir <- "/../data/"
savedir <- "/../results"
setwd(paste0(homedir, workdir))

# Import data: 
mr <- read_csv("PropertyTax8.19.22.csv")

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

# explore completeness 
length(unique(mr$fips))
table(mr$year)

mr_plids <- unique(mr$fips)
table(mr_plids %in% plids)
table(plids %in% mr_plids)

