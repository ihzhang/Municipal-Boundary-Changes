# Clean LODES data ####
setwd("~/Google Drive/My Drive/Stanford/QE2")

library("stringr")
library("dplyr")
library("stargazer")
library("tidyverse")
library("tidycensus")
library("lme4")
library("readr")
library("data.table")
#library("readstata13")
library("magrittr")


state_list <- list.files("Demography/LODES data/2010/rac/", all.files = FALSE, full.names = FALSE) # read in all files, saves file names in a vector
file_list <- list() # initiate an empty list 
for (i in 1:length(state_list)) {
  file_list[[i]] <- read_csv(file = paste0("Demography/LODES data/2010/rac/", state_list[[i]])) %>%
    mutate(pct_hincome = (CE03/C000)*100) %>%
    select(h_geocode, pct_hincome)
} 

rac_2010 <- rbindlist(file_list, use.names = TRUE)
rm(file_list, state_list) # I like to remove useless objects from the environment ASAP to avoid clutter
rac_2010$Year <- 2010
write_csv(rac_2010, file = "Demography/LODES data/rac_2010.csv")

# repeat for 2010 WAC (double-check variables needed in Google Doc), and 2014. 
# output: 
# 2 files -- one for 2010, one for 2014 that has block IDs, the pct_hincome variable and the # of jobs variable
# look into the left_join function 



