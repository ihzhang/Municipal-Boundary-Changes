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

state_list <- list.files("SHP_blk_0010/2000/", all.files = FALSE, full.names = FALSE)
contig_list <- list()
for (i in 1:length(state_list)) {
  contig_list[[i]] <- read_csv(file = paste0("SHP_blk_0010/2000/", state_list[[i]], "/", substr(state_list[[i]], 1, 2), "_contig.csv")) %>%
    mutate(State = substr(state_list[[i]], 4, 5))
} 

names(contig_list) <- state_list
contigall2000 <- rbindlist(contig_list, use.names = TRUE)
rm(contig_list, state_list)
table(contigall2000$State)
write_csv(contigall2000, file = "allcontigblocks2000.csv")