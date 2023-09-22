# Clean LODES data ####
rm(list = ls())
setwd("~/Google Drive/My Drive/Stanford/QE2") # @RA you should modify this file path as necessary

library("stringr")
library("dplyr")
#library("stargazer")
library("tidyverse")
#library("tidycensus")
library("lme4")
library("readr")
library("data.table")
#library("readstata13")
library("magrittr")
library(rvest)

# need rac for 2018, 2019
# need wac for 2018, 2019 

# use the following to download 2000, 2007 and 2014 files ####
# we want a .txt file that contains all the URLS for all the files we want to download 
# then, in terminal, I will download those files using the .txt file 
# we use LODES 5 for 2000s data but LODES 7 for 2017 

# 2000 ----
# 2002 is the earliest year 
base_url <- "https://lehd.ces.census.gov/data/lodes/LODES7/"
page <- read_html(base_url)

# this is just a standard list of states I copied and pasted from other files
# except I changed AS_02 for alaska to AK_02, since that's how the LODES data is coded 
# one easy way to check is to see that your character vector is of length 50, because we have 50 states! 
# note--we don't need HI, so actually there's 49.
state_codes <- c("AL_01", "AK_02", "AR_05", "AZ_04", "CA_06", "CO_08", "CT_09", 
                 "DE_10", "FL_12", "GA_13", "IA_19", "ID_16", "IL_17", "IN_18",
                 "KS_20", "KY_21", "LA_22", 
                 "MD_24", "ME_23", "MI_26", "MN_27", "MS_28", "MO_29", "MT_30", 
                 "NC_37", "ND_38", "NE_31", "NJ_34", "NM_35", "NV_32", "NY_36",
                 "OH_39", "OK_40", "OR_41", "PA_42", "RI_44",
                 "SC_45", "SD_46", "TN_47", "TX_48", "UT_49", "VT_50", "VA_51",
                 "WA_53", "WV_54", "WI_55", "WY_56"
)
state_codes <- tolower(state_codes)

state_list <- page %>%
  html_nodes("a") %>%       
  html_attr("href") %>%     
  str_subset("^[a-z][a-z]/$") 

state_codes <- substr(state_codes, 1, 2)
state_list <- state_list[substr(state_list, 1, 2) %in% state_codes]

url_list <- list() 
for (state in 1:length(state_list)) {
  state_url <- paste0(base_url, state_list[state], "rac/")
  state_page <- read_html(state_url)
  zipfile <- state_page %>%
    html_nodes("a") %>%       
    html_attr("href") %>%     
    str_subset("S000_JT00_2002.csv.gz") 
  url <- paste0(state_url, zipfile)
  url_list <- append(url_list, url)
  print(url)
}

# turn list into txt file - a good place to double-check that the list has 49 observations
lapply(url_list, cat, "\n", file="LODES data/LODES_00.txt", append=TRUE)
rm(url_list)

# code 
# wget -P Google\ Drive/My\ Drive/Stanford/QE2/Demography/LODES\ data/2000/rac -i Google\ Drive/My\ Drive/Stanford/QE2/LODES_00.txt
# cd Google\ Drive/My\ Drive/Stanford/QE2/LODES\ data/2008/rac 
# use gunzip *.gz in your wd with the files

state_list_2000_rac <- list.files("LODES data/2000/rac/", all.files = FALSE, full.names = T) 
file_list_2000_rac <- list() 
for (i in 1:length(state_list_2000_rac)) {
  file_list_2000_rac[[i]] <- read_csv(file = state_list_2000_rac[[i]]) %>%
    mutate(njobs00 = C000,
           nhincjobs00 = CE03) %>%
    select(h_geocode, njobs00, nhincjobs00)
}

rac_2000 <- rbindlist(file_list_2000_rac, use.names = TRUE)
rm(file_list_2000_rac, state_list_2000_rac) 
rac_2000$Year <- 2000
write_csv(rac_2000, file = "LODES data/rac_2000.csv")
rm(rac_2000)

# WAC
base_url <- "https://lehd.ces.census.gov/data/lodes/LODES7/"
page <- read_html(base_url)

state_codes <- c("AL_01", "AK_02", "AR_05", "AZ_04", "CA_06", "CO_08", "CT_09", 
                 "DE_10", "FL_12", "GA_13", "IA_19", "ID_16", "IL_17", "IN_18",
                 "KS_20", "KY_21", "LA_22", 
                 "MD_24", "ME_23", "MI_26", "MN_27", "MS_28", "MO_29", "MT_30", 
                 "NC_37", "ND_38", "NE_31", "NJ_34", "NM_35", "NV_32", "NY_36",
                 "OH_39", "OK_40", "OR_41", "PA_42", "RI_44",
                 "SC_45", "SD_46", "TN_47", "TX_48", "UT_49", "VT_50", "VA_51",
                 "WA_53", "WV_54", "WI_55", "WY_56"
)
state_codes <- tolower(state_codes)

state_list <- page %>%
  html_nodes("a") %>%       
  html_attr("href") %>%     
  str_subset("^[a-z][a-z]/$") 

state_codes <- substr(state_codes, 1, 2)
state_list <- state_list[substr(state_list, 1, 2) %in% state_codes]

url_list <- list() 
for (state in 1:length(state_list)) {
  state_url <- paste0(base_url, state_list[state], "wac/")
  state_page <- read_html(state_url)
  zipfile <- state_page %>%
    html_nodes("a") %>%       
    html_attr("href") %>%     
    str_subset("S000_JT00_2002.csv.gz") 
  if (length(zipfile) == 0) {
    zipfile <- state_page %>%
      html_nodes("a") %>%       
      html_attr("href") %>%     
      str_subset("S000_JT00_2003.csv.gz") 
  } 
  if (length(zipfile) == 0) {
    zipfile <- state_page %>%
      html_nodes("a") %>%       
      html_attr("href") %>%     
      str_subset("S000_JT00_2004.csv.gz") 
  } 
  url <- paste0(state_url, zipfile)
  url_list <- append(url_list, url)
  print(url)
}

# turn list into txt file - a good place to double-check that the list has 49 observations
lapply(url_list, cat, "\n", file="LODES data/LODES_wac_00.txt", append=TRUE)
rm(url_list)

# code 
# wget -P Google\ Drive/My\ Drive/Stanford/QE2/Demography/LODES\ data/2000/rac -i Google\ Drive/My\ Drive/Stanford/QE2/LODES_00.txt

state_list_2000_wac <- list.files("LODES data/2000/wac/", full.names = T) 
file_list_2000_wac <- list() 
for (i in 1:length(state_list_2000_wac)) {
  file_list_2000_wac[[i]] <- read_csv(file = state_list_2000_wac[[i]]) %>%
    mutate(jobs = C000,
           man = CNS05,
           ret = CNS07) %>%
    select(w_geocode, jobs, man, ret)
}

wac_2000 <- rbindlist(file_list_2000_wac, use.names = TRUE)
rm(file_list_2000_wac) 
wac_2000$Year <- 2000
write_csv(wac_2000, file = "LODES data/wac_2000.csv")
rm(wac_2000)

# 2007 ----
base_url <- "https://lehd.ces.census.gov/data/lodes/LODES7/"
page <- read_html(base_url)

state_codes <- c("AL_01", "AK_02", "AR_05", "AZ_04", "CA_06", "CO_08", "CT_09", 
                 "DE_10", "FL_12", "GA_13", "IA_19", "ID_16", "IL_17", "IN_18",
                 "KS_20", "KY_21", "LA_22", 
                 "MD_24", "ME_23", "MI_26", "MN_27", "MS_28", "MO_29", "MT_30", 
                 "NC_37", "ND_38", "NE_31", "NJ_34", "NM_35", "NV_32", "NY_36",
                 "OH_39", "OK_40", "OR_41", "PA_42", "RI_44",
                 "SC_45", "SD_46", "TN_47", "TX_48", "UT_49", "VT_50", "VA_51",
                 "WA_53", "WV_54", "WI_55", "WY_56"
)
state_codes <- tolower(state_codes)

state_list <- page %>%
  html_nodes("a") %>%       
  html_attr("href") %>%     
  str_subset("^[a-z][a-z]/$") 

state_codes <- substr(state_codes, 1, 2)
state_list <- state_list[substr(state_list, 1, 2) %in% state_codes]

url_list <- list() 
for (state in 1:length(state_list)) {
  state_url <- paste0(base_url, state_list[state], "rac/")
  state_page <- read_html(state_url)
  zipfile <- state_page %>%
    html_nodes("a") %>%       
    html_attr("href") %>%     
    str_subset("S000_JT00_2007.csv.gz") 
  url <- paste0(state_url, zipfile)
  url_list <- append(url_list, url)
  print(url)
}

lapply(url_list, cat, "\n", file="LODES data/LODES_07.txt", append=TRUE)
rm(url_list)

state_list_2007_rac <- list.files("LODES data/2007/rac/", all.files = FALSE, full.names = T) 
file_list_2007_rac <- list() 
for (i in 1:length(state_list_2007_rac)) {
  file_list_2007_rac[[i]] <- read_csv(file = state_list_2007_rac[[i]]) %>%
    mutate(njobs07 = C000,
           nhincjobs07 = CE03) %>%
    select(h_geocode, njobs07, nhincjobs07)
}

rac_2007 <- rbindlist(file_list_2007_rac, use.names = TRUE)
rm(file_list_2007_rac, state_list_2007_rac) 
rac_2007$Year <- 2007
write_csv(rac_2007, file = "rac_2007.csv")
rm(rac_2007)

# WAC 
state_codes <- c("AL_01", "AK_02", "AR_05", "AZ_04", "CA_06", "CO_08", "CT_09", 
                 "DE_10", "FL_12", "GA_13", "IA_19", "ID_16", "IL_17", "IN_18",
                 "KS_20", "KY_21", "LA_22", 
                 "MD_24", "ME_23", "MI_26", "MN_27", "MS_28", "MO_29", "MT_30", 
                 "NC_37", "ND_38", "NE_31", "NJ_34", "NM_35", "NV_32", "NY_36",
                 "OH_39", "OK_40", "OR_41", "PA_42", "RI_44",
                 "SC_45", "SD_46", "TN_47", "TX_48", "UT_49", "VT_50", "VA_51",
                 "WA_53", "WV_54", "WI_55", "WY_56"
)
state_codes <- tolower(state_codes)
page <- read_html(base_url)

state_list <- page %>%
  html_nodes("a") %>%       
  html_attr("href") %>%     
  str_subset("^[a-z][a-z]/$") 

state_codes <- substr(state_codes, 1, 2)
state_list <- state_list[substr(state_list, 1, 2) %in% state_codes]

url_list <- list()  
for (state in 1:length(state_list)) {
  state_url <- paste0(base_url, state_list[state], "wac/")
  state_page <- read_html(state_url)
  zipfile <- state_page %>%
    html_nodes("a") %>%       
    html_attr("href") %>%     
    str_subset("S000_JT00_2007.csv.gz") 
  url <- paste0(state_url, zipfile)
  url_list <- append(url_list, url)
  print(url)
}

lapply(url_list, cat, "\n", file="LODES data/LODES_wac_07.txt", append=TRUE)
rm(url_list)

# code 
# wget -P Google\ Drive/My\ Drive/Stanford/QE2/Demography/LODES\ data/2000/rac -i Google\ Drive/My\ Drive/Stanford/QE2/LODES_00.txt

state_list_2007_wac <- list.files("LODES data/2007/wac/", full.names = T) 
file_list_2007_wac <- list() 
for (i in 1:length(state_list_2007_wac)) {
  file_list_2007_wac[[i]] <- read_csv(file = state_list_2007_wac[[i]]) %>%
    mutate(jobs = C000,
           man = CNS05,
           ret = CNS07) %>%
    select(w_geocode, jobs, man, ret) 
}

wac_2007 <- rbindlist(file_list_2007_wac, use.names = TRUE)
rm(file_list_2007_wac, state_list_2007_wac) 
wac_2007$Year <- 2007
write_csv(wac_2007, file = "LODES data/wac_2007.csv")
rm(wac_2007)

# 2008 ----
base_url <- "https://lehd.ces.census.gov/data/lodes/LODES7/"
page <- read_html(base_url)

state_codes <- c("AL_01", "AK_02", "AR_05", "AZ_04", "CA_06", "CO_08", "CT_09", 
                 "DE_10", "FL_12", "GA_13", "IA_19", "ID_16", "IL_17", "IN_18",
                 "KS_20", "KY_21", "LA_22", 
                 "MD_24", "ME_23", "MI_26", "MN_27", "MS_28", "MO_29", "MT_30", 
                 "NC_37", "ND_38", "NE_31", "NJ_34", "NM_35", "NV_32", "NY_36",
                 "OH_39", "OK_40", "OR_41", "PA_42", "RI_44",
                 "SC_45", "SD_46", "TN_47", "TX_48", "UT_49", "VT_50", "VA_51",
                 "WA_53", "WV_54", "WI_55", "WY_56"
)
state_codes <- tolower(state_codes)

state_list <- page %>%
  html_nodes("a") %>%       
  html_attr("href") %>%     
  str_subset("^[a-z][a-z]/$") 

state_codes <- substr(state_codes, 1, 2)
state_list <- state_list[substr(state_list, 1, 2) %in% state_codes]

url_list <- list() 
for (state in 1:length(state_list)) {
  state_url <- paste0(base_url, state_list[state], "rac/")
  state_page <- read_html(state_url)
  zipfile <- state_page %>%
    html_nodes("a") %>%       
    html_attr("href") %>%     
    str_subset("S000_JT00_2008.csv.gz") 
  url <- paste0(state_url, zipfile)
  url_list <- append(url_list, url)
  print(url)
}

# turn list into txt file - a good place to double-check that the list has 49 observations
lapply(url_list, cat, "\n", file="LODES data/LODES_08.txt", append=TRUE)
rm(url_list)

state_list_2008_rac <- list.files("LODES data/2008/rac/", full.names = T) 
file_list_2008_rac <- list() 
for (i in 1:length(state_list_2008_rac)) {
  file_list_2008_rac[[i]] <- read_csv(file =  state_list_2008_rac[[i]]) %>%
    mutate(njobs08 = C000,
           nhincjobs08 = CE03) %>%
    select(h_geocode, njobs08, nhincjobs08)
}

rac_2008 <- rbindlist(file_list_2008_rac, use.names = TRUE)
rm(file_list_2008_rac, state_list_2008_rac) 
rac_2008$Year <- 2008
write_csv(rac_2008, file = "LODES data/rac_2008.csv")
rm(rac_2008)

# WAC 
state_codes <- c("AL_01", "AK_02", "AR_05", "AZ_04", "CA_06", "CO_08", "CT_09", 
                 "DE_10", "FL_12", "GA_13", "IA_19", "ID_16", "IL_17", "IN_18",
                 "KS_20", "KY_21", "LA_22", 
                 "MD_24", "ME_23", "MI_26", "MN_27", "MS_28", "MO_29", "MT_30", 
                 "NC_37", "ND_38", "NE_31", "NJ_34", "NM_35", "NV_32", "NY_36",
                 "OH_39", "OK_40", "OR_41", "PA_42", "RI_44",
                 "SC_45", "SD_46", "TN_47", "TX_48", "UT_49", "VT_50", "VA_51",
                 "WA_53", "WV_54", "WI_55", "WY_56"
)
state_codes <- tolower(state_codes)
page <- read_html(base_url)

state_list <- page %>%
  html_nodes("a") %>%       
  html_attr("href") %>%    
  str_subset("^[a-z][a-z]/$") 

state_codes <- substr(state_codes, 1, 2)
state_list <- state_list[substr(state_list, 1, 2) %in% state_codes]

url_list <- list() 
for (state in 1:length(state_list)) {
  state_url <- paste0(base_url, state_list[state], "wac/")
  state_page <- read_html(state_url)
  zipfile <- state_page %>%
    html_nodes("a") %>%       
    html_attr("href") %>%     
    str_subset("S000_JT00_2008.csv.gz") 
  url <- paste0(state_url, zipfile)
  url_list <- append(url_list, url)
  print(url)
}

lapply(url_list, cat, "\n", file="LODES data/LODES_wac_08.txt", append=TRUE)
rm(url_list)

state_list_2008_wac <- list.files("LODES data/2008/wac/", all.files = FALSE, full.names = T) 
file_list_2008_wac <- list() 
for (i in 1:length(state_list_2008_wac)) {
  file_list_2008_wac[[i]] <- read_csv(file = state_list_2008_wac[[i]]) %>%
    mutate(jobs = C000,
           man = CNS05,
           ret = CNS07) %>%
    select(w_geocode, jobs, man, ret)
}

wac_2008 <- rbindlist(file_list_2008_wac, use.names = TRUE)
rm(file_list_2008_wac, state_list_2008_wac) 
wac_2008$Year <- 2008
write_csv(wac_2008, file = "LODES data/wac_2008.csv")
rm(wac_2008)

# 2009 ----
page <- read_html(base_url)

state_list <- page %>%
  html_nodes("a") %>%       
  html_attr("href") %>%     
  str_subset("^[a-z][a-z]/$") 

state_list <- state_list[substr(state_list, 1, 2) %in% state_codes]

url_list <- list() 
for (state in 1:length(state_list)) {
  state_url <- paste0(base_url, state_list[state], "rac/")
  state_page <- read_html(state_url)
  zipfile <- state_page %>%
    html_nodes("a") %>%       
    html_attr("href") %>%     
    str_subset("S000_JT00_2009.csv.gz") 
  url <- paste0(state_url, zipfile)
  url_list <- append(url_list, url)
  print(url)
}

lapply(url_list, cat, "\n", file="LODES data/LODES_09.txt", append=TRUE)
rm(url_list)

state_list_2009_rac <- list.files("LODES data/2009/rac/", full.names = T) 
file_list_2009_rac <- list() 
for (i in 1:length(state_list_2009_rac)) {
  file_list_2009_rac[[i]] <- read_csv(file =  state_list_2009_rac[[i]]) %>%
    mutate(njobs09 = C000,
           nhincjobs09 = CE03) %>%
    select(h_geocode, njobs09, nhincjobs09)
}

rac_2009 <- rbindlist(file_list_2009_rac, use.names = TRUE)
rm(file_list_2009_rac, state_list_2009_rac) 
rac_2009$Year <- 2009
write_csv(rac_2009, file = "LODES data/rac_2009.csv")
rm(rac_2009)

# WAC 
page <- read_html(base_url)

state_list <- page %>%
  html_nodes("a") %>%       
  html_attr("href") %>%    
  str_subset("^[a-z][a-z]/$") 

state_list <- state_list[substr(state_list, 1, 2) %in% state_codes]

url_list <- list() 
for (state in 1:length(state_list)) {
  state_url <- paste0(base_url, state_list[state], "wac/")
  state_page <- read_html(state_url)
  zipfile <- state_page %>%
    html_nodes("a") %>%       
    html_attr("href") %>%     
    str_subset("S000_JT00_2009.csv.gz") 
  url <- paste0(state_url, zipfile)
  url_list <- append(url_list, url)
  print(url)
}

lapply(url_list, cat, "\n", file="LODES data/LODES_wac_09.txt", append=TRUE)
rm(url_list)

state_list_2009_wac <- list.files("LODES data/2009/wac/", full.names = T) 
file_list_2009_wac <- list() 
for (i in 1:length(state_list_2009_wac)) {
  file_list_2009_wac[[i]] <- read_csv(file = state_list_2009_wac[[i]]) %>%
    mutate(jobs = C000,
           man = CNS05,
           ret = CNS07) %>%
    select(w_geocode, jobs, man, ret)
}

wac_2009 <- rbindlist(file_list_2009_wac, use.names = TRUE)
rm(file_list_2009_wac, state_list_2009_wac) 
wac_2009$Year <- 2009
write_csv(wac_2009, file = "LODES data/wac_2009.csv")
rm(wac_2009)

# 2010 ----
page <- read_html(base_url)

state_list <- page %>%
  html_nodes("a") %>%       
  html_attr("href") %>%     
  str_subset("^[a-z][a-z]/$") 

state_list <- state_list[substr(state_list, 1, 2) %in% state_codes]

url_list <- list() 
for (state in 1:length(state_list)) {
  state_url <- paste0(base_url, state_list[state], "rac/")
  state_page <- read_html(state_url)
  zipfile <- state_page %>%
    html_nodes("a") %>%       
    html_attr("href") %>%     
    str_subset("S000_JT00_2010.csv.gz") 
  url <- paste0(state_url, zipfile)
  url_list <- append(url_list, url)
  print(url)
}

lapply(url_list, cat, "\n", file="LODES data/LODES_10.txt", append=TRUE)
rm(url_list)

state_list_2010_rac <- list.files("LODES data/2010/rac/", full.names = T) 
file_list_2010_rac <- list() 
for (i in 1:length(state_list_2010_rac)) {
  file_list_2010_rac[[i]] <- read_csv(file =  state_list_2010_rac[[i]]) %>%
    mutate(njobs10 = C000,
           nhincjobs10 = CE03) %>%
    select(h_geocode, njobs10, nhincjobs10)
}

rac_2010 <- rbindlist(file_list_2010_rac, use.names = TRUE)
rm(file_list_2010_rac, state_list_2010_rac) 
rac_2010$Year <- 2010
write_csv(rac_2010, file = "LODES data/rac_2010.csv")
rm(rac_2010)

# WAC 
page <- read_html(base_url)

state_list <- page %>%
  html_nodes("a") %>%       
  html_attr("href") %>%    
  str_subset("^[a-z][a-z]/$") 

state_list <- state_list[substr(state_list, 1, 2) %in% state_codes]

url_list <- list() 
for (state in 1:length(state_list)) {
  state_url <- paste0(base_url, state_list[state], "wac/")
  state_page <- read_html(state_url)
  zipfile <- state_page %>%
    html_nodes("a") %>%       
    html_attr("href") %>%     
    str_subset("S000_JT00_2010.csv.gz") 
  url <- paste0(state_url, zipfile)
  url_list <- append(url_list, url)
  print(url)
}

lapply(url_list, cat, "\n", file="LODES data/LODES_wac_10.txt", append=TRUE)
rm(url_list)

state_list_2010_wac <- list.files("LODES data/2010/wac/", full.names = T) 
file_list_2010_wac <- list() 
for (i in 1:length(state_list_2010_wac)) {
  file_list_2010_wac[[i]] <- read_csv(file = state_list_2010_wac[[i]]) %>%
    mutate(jobs = C000,
           man = CNS05,
           ret = CNS07) %>%
    select(w_geocode, jobs, man, ret)
}

wac_2010 <- rbindlist(file_list_2010_wac, use.names = TRUE)
rm(file_list_2010_wac, state_list_2010_wac) 
wac_2010$Year <- 2010
write_csv(wac_2010, file = "LODES data/wac_2010.csv")
rm(wac_2010)

# 2011 ----
page <- read_html(base_url)

state_list <- page %>%
  html_nodes("a") %>%       
  html_attr("href") %>%     
  str_subset("^[a-z][a-z]/$") 

state_list <- state_list[substr(state_list, 1, 2) %in% state_codes]

url_list <- list() 
for (state in 1:length(state_list)) {
  state_url <- paste0(base_url, state_list[state], "rac/")
  state_page <- read_html(state_url)
  zipfile <- state_page %>%
    html_nodes("a") %>%       
    html_attr("href") %>%     
    str_subset("S000_JT00_2011.csv.gz") 
  url <- paste0(state_url, zipfile)
  url_list <- append(url_list, url)
  print(url)
}

lapply(url_list, cat, "\n", file="LODES data/LODES_11.txt", append=TRUE)
rm(url_list)

state_list_2011_rac <- list.files("LODES data/2011/rac/", full.names = T) 
file_list_2011_rac <- list() 
for (i in 1:length(state_list_2011_rac)) {
  file_list_2011_rac[[i]] <- read_csv(file =  state_list_2011_rac[[i]]) %>%
    mutate(njobs11 = C000,
           nhincjobs11 = CE03) %>%
    select(h_geocode, njobs11, nhincjobs11)
}

rac_2011 <- rbindlist(file_list_2011_rac, use.names = TRUE)
rm(file_list_2011_rac, state_list_2011_rac) 
rac_2011$Year <- 2011
write_csv(rac_2011, file = "LODES data/rac_2011.csv")
rm(rac_2011)

# WAC 
page <- read_html(base_url)

state_list <- page %>%
  html_nodes("a") %>%       
  html_attr("href") %>%    
  str_subset("^[a-z][a-z]/$") 

state_list <- state_list[substr(state_list, 1, 2) %in% state_codes]

url_list <- list() 
for (state in 1:length(state_list)) {
  state_url <- paste0(base_url, state_list[state], "wac/")
  state_page <- read_html(state_url)
  zipfile <- state_page %>%
    html_nodes("a") %>%       
    html_attr("href") %>%     
    str_subset("S000_JT00_2011.csv.gz") 
  url <- paste0(state_url, zipfile)
  url_list <- append(url_list, url)
  print(url)
}

lapply(url_list, cat, "\n", file="LODES data/LODES_wac_11.txt", append=TRUE)
rm(url_list)

state_list_2011_wac <- list.files("LODES data/2011/wac/", full.names = T) 
file_list_2011_wac <- list() 
for (i in 1:length(state_list_2011_wac)) {
  file_list_2011_wac[[i]] <- read_csv(file = state_list_2011_wac[[i]]) %>%
    mutate(jobs = C000,
           man = CNS05,
           ret = CNS07) %>%
    select(w_geocode, jobs, man, ret)
}

wac_2011 <- rbindlist(file_list_2011_wac, use.names = TRUE)
rm(file_list_2011_wac, state_list_2011_wac) 
wac_2011$Year <- 2011
write_csv(wac_2011, file = "LODES data/wac_2011.csv")
rm(wac_2011)

# 2012 ----
page <- read_html(base_url)

state_list <- page %>%
  html_nodes("a") %>%       
  html_attr("href") %>%     
  str_subset("^[a-z][a-z]/$") 

state_list <- state_list[substr(state_list, 1, 2) %in% state_codes]

url_list <- list() 
for (state in 1:length(state_list)) {
  state_url <- paste0(base_url, state_list[state], "rac/")
  state_page <- read_html(state_url)
  zipfile <- state_page %>%
    html_nodes("a") %>%       
    html_attr("href") %>%     
    str_subset("S000_JT00_2012.csv.gz") 
  url <- paste0(state_url, zipfile)
  url_list <- append(url_list, url)
  print(url)
}

lapply(url_list, cat, "\n", file="LODES data/LODES_12.txt", append=TRUE)
rm(url_list)

state_list_2012_rac <- list.files("LODES data/2012/rac/", full.names = T) 
file_list_2012_rac <- list() 
for (i in 1:length(state_list_2012_rac)) {
  file_list_2012_rac[[i]] <- read_csv(file =  state_list_2012_rac[[i]]) %>%
    mutate(njobs12 = C000,
           nhincjobs12 = CE03) %>%
    select(h_geocode, njobs12, nhincjobs12)
}

rac_2012 <- rbindlist(file_list_2012_rac, use.names = TRUE)
rm(file_list_2012_rac, state_list_2012_rac) 
rac_2012$Year <- 2012
write_csv(rac_2012, file = "LODES data/rac_2012.csv")
rm(rac_2012)

# WAC 
url_list <- list() 
for (state in 1:length(state_list)) {
  state_url <- paste0(base_url, state_list[state], "wac/")
  state_page <- read_html(state_url)
  zipfile <- state_page %>%
    html_nodes("a") %>%       
    html_attr("href") %>%     
    str_subset("S000_JT00_2012.csv.gz") 
  url <- paste0(state_url, zipfile)
  url_list <- append(url_list, url)
  print(url)
}

lapply(url_list, cat, "\n", file="LODES data/LODES_wac_12.txt", append=TRUE)
rm(url_list)

state_list_2012_wac <- list.files("LODES data/2012/wac/", full.names = T) 
file_list_2012_wac <- list() 
for (i in 1:length(state_list_2012_wac)) {
  file_list_2012_wac[[i]] <- read_csv(file = state_list_2012_wac[[i]]) %>%
    mutate(jobs = C000,
           man = CNS05,
           ret = CNS07) %>%
    select(w_geocode, jobs, man, ret)
}

wac_2012 <- rbindlist(file_list_2012_wac, use.names = TRUE)
rm(file_list_2012_wac, state_list_2012_wac) 
wac_2012$Year <- 2012
write_csv(wac_2012, file = "LODES data/wac_2012.csv")
rm(wac_2012)

# 2013 ----
url_list <- list() 
for (state in 1:length(state_list)) {
  state_url <- paste0(base_url, state_list[state], "rac/")
  state_page <- read_html(state_url)
  zipfile <- state_page %>%
    html_nodes("a") %>%       
    html_attr("href") %>%     
    str_subset("S000_JT00_2013.csv.gz") 
  url <- paste0(state_url, zipfile)
  url_list <- append(url_list, url)
  print(url)
}

lapply(url_list, cat, "\n", file="LODES data/LODES_13.txt", append=TRUE)
rm(url_list)

state_list_2013_rac <- list.files("LODES data/2013/rac/", full.names = T) 
file_list_2013_rac <- list() 
for (i in 1:length(state_list_2013_rac)) {
  file_list_2013_rac[[i]] <- read_csv(file =  state_list_2013_rac[[i]]) %>%
    mutate(njobs13 = C000,
           nhincjobs13 = CE03) %>%
    select(h_geocode, njobs13, nhincjobs13)
}

rac_2013 <- rbindlist(file_list_2013_rac, use.names = TRUE)
rm(file_list_2013_rac, state_list_2013_rac) 
rac_2013$Year <- 2013
write_csv(rac_2013, file = "LODES data/rac_2013.csv")
rm(rac_2013)

# WAC 
url_list <- list() 
for (state in 1:length(state_list)) {
  state_url <- paste0(base_url, state_list[state], "wac/")
  state_page <- read_html(state_url)
  zipfile <- state_page %>%
    html_nodes("a") %>%       
    html_attr("href") %>%     
    str_subset("S000_JT00_2013.csv.gz") 
  url <- paste0(state_url, zipfile)
  url_list <- append(url_list, url)
  print(url)
}

lapply(url_list, cat, "\n", file="LODES data/LODES_wac_13.txt", append=TRUE)
rm(url_list)

state_list_2013_wac <- list.files("LODES data/2013/wac/", full.names = T) 
file_list_2013_wac <- list() 
for (i in 1:length(state_list_2013_wac)) {
  file_list_2013_wac[[i]] <- read_csv(file = state_list_2013_wac[[i]]) %>%
    mutate(jobs = C000,
           man = CNS05,
           ret = CNS07) %>%
    select(w_geocode, jobs, man, ret)
}

wac_2013 <- rbindlist(file_list_2013_wac, use.names = TRUE)
rm(file_list_2013_wac, state_list_2013_wac) 
wac_2013$Year <- 2013
write_csv(wac_2013, file = "LODES data/wac_2013.csv")
rm(wac_2013)

# 2014 ----
url_list <- list() 
for (state in 1:length(state_list)) {
  state_url <- paste0(base_url, state_list[state], "rac/")
  state_page <- read_html(state_url)
  zipfile <- state_page %>%
    html_nodes("a") %>%       
    html_attr("href") %>%     
    str_subset("S000_JT00_2014.csv.gz") 
  url <- paste0(state_url, zipfile)
  url_list <- append(url_list, url)
  print(url)
}

lapply(url_list, cat, "\n", file="LODES data/LODES_14.txt", append=TRUE)
rm(url_list)

state_list_2014_rac <- list.files("LODES data/2014/rac/", full.names = T) 
file_list_2014_rac <- list() 
for (i in 1:length(state_list_2014_rac)) {
  file_list_2014_rac[[i]] <- read_csv(file =  state_list_2014_rac[[i]]) %>%
    mutate(njobs14 = C000,
           nhincjobs14 = CE03) %>%
    select(h_geocode, njobs14, nhincjobs14)
}

rac_2014 <- rbindlist(file_list_2014_rac, use.names = TRUE)
rm(file_list_2014_rac, state_list_2014_rac) 
rac_2014$Year <- 2014
write_csv(rac_2014, file = "LODES data/rac_2014.csv")
rm(rac_2014)

# WAC 
url_list <- list() 
for (state in 1:length(state_list)) {
  state_url <- paste0(base_url, state_list[state], "wac/")
  state_page <- read_html(state_url)
  zipfile <- state_page %>%
    html_nodes("a") %>%       
    html_attr("href") %>%     
    str_subset("S000_JT00_2014.csv.gz") 
  url <- paste0(state_url, zipfile)
  url_list <- append(url_list, url)
  print(url)
}

lapply(url_list, cat, "\n", file="LODES data/LODES_wac_14.txt", append=TRUE)
rm(url_list)

state_list_2014_wac <- list.files("LODES data/2014/wac/", full.names = T) 
file_list_2014_wac <- list() 
for (i in 1:length(state_list_2014_wac)) {
  file_list_2014_wac[[i]] <- read_csv(file = state_list_2014_wac[[i]]) %>%
    mutate(jobs = C000,
           man = CNS05,
           ret = CNS07) %>%
    select(w_geocode, jobs, man, ret)
}

wac_2014 <- rbindlist(file_list_2014_wac, use.names = TRUE)
rm(file_list_2014_wac, state_list_2014_wac) 
wac_2014$Year <- 2014
write_csv(wac_2014, file = "LODES data/wac_2014.csv")
rm(wac_2014)

# 2015 ----
url_list <- list() 
for (state in 1:length(state_list)) {
  state_url <- paste0(base_url, state_list[state], "rac/")
  state_page <- read_html(state_url)
  zipfile <- state_page %>%
    html_nodes("a") %>%       
    html_attr("href") %>%     
    str_subset("S000_JT00_2015.csv.gz") 
  url <- paste0(state_url, zipfile)
  url_list <- append(url_list, url)
  print(url)
}

lapply(url_list, cat, "\n", file="LODES data/LODES_15.txt", append=TRUE)
rm(url_list)

state_list_2015_rac <- list.files("LODES data/2015/rac/", full.names = T) 
file_list_2015_rac <- list() 
for (i in 1:length(state_list_2015_rac)) {
  file_list_2015_rac[[i]] <- read_csv(file =  state_list_2015_rac[[i]]) %>%
    mutate(njobs15 = C000,
           nhincjobs15 = CE03) %>%
    select(h_geocode, njobs15, nhincjobs15)
}

rac_2015 <- rbindlist(file_list_2015_rac, use.names = TRUE)
rm(file_list_2015_rac, state_list_2015_rac) 
rac_2015$Year <- 2015
write_csv(rac_2015, file = "LODES data/rac_2015.csv")
rm(rac_2015)

# WAC 
url_list <- list() 
for (state in 1:length(state_list)) {
  state_url <- paste0(base_url, state_list[state], "wac/")
  state_page <- read_html(state_url)
  zipfile <- state_page %>%
    html_nodes("a") %>%       
    html_attr("href") %>%     
    str_subset("S000_JT00_2015.csv.gz") 
  url <- paste0(state_url, zipfile)
  url_list <- append(url_list, url)
  print(url)
}

lapply(url_list, cat, "\n", file="LODES data/LODES_wac_15.txt", append=TRUE)
rm(url_list)

state_list_2015_wac <- list.files("LODES data/2015/wac/", full.names = T) 
file_list_2015_wac <- list() 
for (i in 1:length(state_list_2015_wac)) {
  file_list_2015_wac[[i]] <- read_csv(file = state_list_2015_wac[[i]]) %>%
    mutate(jobs = C000,
           man = CNS05,
           ret = CNS07) %>%
    select(w_geocode, jobs, man, ret)
}

wac_2015 <- rbindlist(file_list_2015_wac, use.names = TRUE)
rm(file_list_2015_wac, state_list_2015_wac) 
wac_2015$Year <- 2015
write_csv(wac_2015, file = "LODES data/wac_2015.csv")
rm(wac_2015)

# 2016 ----
url_list <- list() 
for (state in 1:length(state_list)) {
  state_url <- paste0(base_url, state_list[state], "rac/")
  state_page <- read_html(state_url)
  zipfile <- state_page %>%
    html_nodes("a") %>%       
    html_attr("href") %>%     
    str_subset("S000_JT00_2016.csv.gz") 
  url <- paste0(state_url, zipfile)
  url_list <- append(url_list, url)
  print(url)
}

lapply(url_list, cat, "\n", file="LODES data/LODES_16.txt", append=TRUE)
rm(url_list)

state_list_2016_rac <- list.files("LODES data/2016/rac/", full.names = T) 
file_list_2016_rac <- list() 
for (i in 1:length(state_list_2016_rac)) {
  file_list_2016_rac[[i]] <- read_csv(file =  state_list_2016_rac[[i]]) %>%
    mutate(njobs16 = C000,
           nhincjobs16 = CE03) %>%
    select(h_geocode, njobs16, nhincjobs16)
}

rac_2016 <- rbindlist(file_list_2016_rac, use.names = TRUE)
rm(file_list_2016_rac, state_list_2016_rac) 
rac_2016$Year <- 2016
write_csv(rac_2016, file = "LODES data/rac_2016.csv")
rm(rac_2016)

# WAC 
url_list <- list() 
for (state in 1:length(state_list)) {
  state_url <- paste0(base_url, state_list[state], "wac/")
  state_page <- read_html(state_url)
  zipfile <- state_page %>%
    html_nodes("a") %>%       
    html_attr("href") %>%     
    str_subset("S000_JT00_2016.csv.gz") 
  url <- paste0(state_url, zipfile)
  url_list <- append(url_list, url)
  print(url)
}

lapply(url_list, cat, "\n", file="LODES data/LODES_wac_16.txt", append=TRUE)
rm(url_list)

state_list_2016_wac <- list.files("LODES data/2016/wac/", full.names = T) 
file_list_2016_wac <- list() 
for (i in 1:length(state_list_2016_wac)) {
  file_list_2016_wac[[i]] <- read_csv(file = state_list_2016_wac[[i]]) %>%
    mutate(jobs = C000,
           man = CNS05,
           ret = CNS07) %>%
    select(w_geocode, jobs, man, ret)
}

wac_2016 <- rbindlist(file_list_2016_wac, use.names = TRUE)
rm(file_list_2016_wac, state_list_2016_wac) 
wac_2016$Year <- 2016
write_csv(wac_2016, file = "LODES data/wac_2016.csv")
rm(wac_2016)

# 2017 ----
url_list <- list() 
for (state in 1:length(state_list)) {
  state_url <- paste0(base_url, state_list[state], "rac/")
  state_page <- read_html(state_url)
  zipfile <- state_page %>%
    html_nodes("a") %>%       
    html_attr("href") %>%     
    str_subset("S000_JT00_2017.csv.gz") 
  url <- paste0(state_url, zipfile)
  url_list <- append(url_list, url)
  print(url)
}

lapply(url_list, cat, "\n", file="LODES data/LODES_17.txt", append=TRUE)
rm(url_list)

state_list_2017_rac <- list.files("LODES data/2017/rac/", full.names = T) 
file_list_2017_rac <- list() 
for (i in 1:length(state_list_2017_rac)) {
  file_list_2017_rac[[i]] <- read_csv(file =  state_list_2017_rac[[i]]) %>%
    mutate(njobs17 = C000,
           nhincjobs17 = CE03) %>%
    select(h_geocode, njobs17, nhincjobs17)
}

rac_2017 <- rbindlist(file_list_2017_rac, use.names = TRUE)
rm(file_list_2017_rac, state_list_2017_rac) 
rac_2017$Year <- 2017
write_csv(rac_2017, file = "LODES data/rac_2017.csv")
rm(rac_2017)

# WAC 
url_list <- list() 
for (state in 1:length(state_list)) {
  state_url <- paste0(base_url, state_list[state], "wac/")
  state_page <- read_html(state_url)
  zipfile <- state_page %>%
    html_nodes("a") %>%       
    html_attr("href") %>%     
    str_subset("S000_JT00_2017.csv.gz") 
  url <- paste0(state_url, zipfile)
  url_list <- append(url_list, url)
  print(url)
}

lapply(url_list, cat, "\n", file="LODES data/LODES_wac_17.txt", append=TRUE)
rm(url_list)

state_list_2017_wac <- list.files("LODES data/2017/wac/", full.names = T) 
file_list_2017_wac <- list() 
for (i in 1:length(state_list_2017_wac)) {
  file_list_2017_wac[[i]] <- read_csv(file = state_list_2017_wac[[i]]) %>%
    mutate(jobs = C000,
           man = CNS05,
           ret = CNS07) %>%
    select(w_geocode, jobs, man, ret)
}

wac_2017 <- rbindlist(file_list_2017_wac, use.names = TRUE)
rm(file_list_2017_wac, state_list_2017_wac) 
wac_2017$Year <- 2017
write_csv(wac_2017, file = "LODES data/wac_2017.csv")
rm(wac_2017)

# 2018 ----
url_list <- list() 
for (state in 1:length(state_list)) {
  state_url <- paste0(base_url, state_list[state], "rac/")
  state_page <- read_html(state_url)
  zipfile <- state_page %>%
    html_nodes("a") %>%       
    html_attr("href") %>%     
    str_subset("S000_JT00_2018.csv.gz") 
  url <- paste0(state_url, zipfile)
  url_list <- append(url_list, url)
  print(url)
}

lapply(url_list, cat, "\n", file="LODES data/LODES_18.txt", append=TRUE)
rm(url_list)

state_list_2018_rac <- list.files("LODES data/2018/rac/", full.names = T) 
file_list_2018_rac <- list() 
for (i in 1:length(state_list_2018_rac)) {
  file_list_2018_rac[[i]] <- read_csv(file =  state_list_2018_rac[[i]]) %>%
    mutate(njobs18 = C000,
           nhincjobs18 = CE03) %>%
    select(h_geocode, njobs18, nhincjobs18)
}

rac_2018 <- rbindlist(file_list_2018_rac, use.names = TRUE)
rm(file_list_2018_rac, state_list_2018_rac) 
rac_2018$Year <- 2018
write_csv(rac_2018, file = "LODES data/rac_2018.csv")
rm(rac_2018)

# WAC 
state_codes <- c("AL_01", "AR_05", "AZ_04", "CA_06", "CO_08", "CT_09", 
                 "DE_10", "FL_12", "GA_13", "IA_19", "ID_16", "IL_17", "IN_18",
                 "KS_20", "KY_21", "LA_22", 
                 "MD_24", "ME_23", "MI_26", "MN_27", "MS_28", "MO_29", "MT_30", 
                 "NC_37", "ND_38", "NE_31", "NJ_34", "NM_35", "NV_32", "NY_36",
                 "OH_39", "OK_40", "OR_41", "PA_42", "RI_44",
                 "SC_45", "SD_46", "TN_47", "TX_48", "UT_49", "VT_50", "VA_51",
                 "WA_53", "WV_54", "WI_55", "WY_56"
)
state_codes <- tolower(state_codes)
page <- read_html(base_url)

state_list <- page %>%
  html_nodes("a") %>%       
  html_attr("href") %>%    
  str_subset("^[a-z][a-z]/$") 

state_codes <- substr(state_codes, 1, 2)
state_list <- state_list[substr(state_list, 1, 2) %in% state_codes]
url_list <- list() 
for (state in 1:length(state_list)) {
  state_url <- paste0(base_url, state_list[state], "wac/")
  state_page <- read_html(state_url)
  zipfile <- state_page %>%
    html_nodes("a") %>%       
    html_attr("href") %>%     
    str_subset("S000_JT00_2018.csv.gz") 
  url <- paste0(state_url, zipfile)
  url_list <- append(url_list, url)
  print(url)
}

lapply(url_list, cat, "\n", file="LODES data/LODES_wac_18.txt", append=TRUE)
rm(url_list)

state_list_2018_wac <- list.files("LODES data/2018/wac/", full.names = T) 
file_list_2018_wac <- list() 
for (i in 1:length(state_list_2018_wac)) {
  file_list_2018_wac[[i]] <- read_csv(file = state_list_2018_wac[[i]]) %>%
    mutate(jobs = C000,
           man = CNS05,
           ret = CNS07) %>%
    select(w_geocode, jobs, man, ret)
}

wac_2018 <- rbindlist(file_list_2018_wac, use.names = TRUE)
rm(file_list_2018_wac, state_list_2018_wac) 
wac_2018$Year <- 2018
write_csv(wac_2018, file = "LODES data/wac_2018.csv")
rm(wac_2018)

# 2019 ----
state_codes <- c("AL_01", "AK_02", "AR_05", "AZ_04", "CA_06", "CO_08", "CT_09", 
                 "DE_10", "FL_12", "GA_13", "IA_19", "ID_16", "IL_17", "IN_19",
                 "KS_20", "KY_21", "LA_22", 
                 "MD_24", "ME_23", "MI_26", "MN_27", "MS_28", "MO_29", "MT_30", 
                 "NC_37", "ND_38", "NE_31", "NJ_34", "NM_35", "NV_32", "NY_36",
                 "OH_39", "OK_40", "OR_41", "PA_42", "RI_44",
                 "SC_45", "SD_46", "TN_47", "TX_48", "UT_49", "VT_50", "VA_51",
                 "WA_53", "WV_54", "WI_55", "WY_56"
)
state_codes <- tolower(state_codes)
page <- read_html(base_url)

state_list <- page %>%
  html_nodes("a") %>%       
  html_attr("href") %>%    
  str_subset("^[a-z][a-z]/$") 

state_codes <- substr(state_codes, 1, 2)
state_list <- state_list[substr(state_list, 1, 2) %in% state_codes]
url_list <- list() 
for (state in 1:length(state_list)) {
  state_url <- paste0(base_url, state_list[state], "rac/")
  state_page <- read_html(state_url)
  zipfile <- state_page %>%
    html_nodes("a") %>%       
    html_attr("href") %>%     
    str_subset("S000_JT00_2019.csv.gz") 
  url <- paste0(state_url, zipfile)
  url_list <- append(url_list, url)
  print(url)
}

lapply(url_list, cat, "\n", file="LODES data/LODES_19.txt", append=TRUE)
rm(url_list)

state_list_2019_rac <- list.files("LODES data/2019/rac/", full.names = T) 
file_list_2019_rac <- list() 
for (i in 1:length(state_list_2019_rac)) {
  file_list_2019_rac[[i]] <- read_csv(file =  state_list_2019_rac[[i]]) %>%
    mutate(njobs19 = C000,
           nhincjobs19 = CE03) %>%
    select(h_geocode, njobs19, nhincjobs19)
}

rac_2019 <- rbindlist(file_list_2019_rac, use.names = TRUE)
rm(file_list_2019_rac, state_list_2019_rac) 
rac_2019$Year <- 2019
write_csv(rac_2019, file = "LODES data/rac_2019.csv")
rm(rac_2019)

# WAC 
state_codes <- c("AL_01", "AR_05", "AZ_04", "CA_06", "CO_08", "CT_09", 
                 "DE_10", "FL_12", "GA_13", "IA_19", "ID_16", "IL_17", "IN_19",
                 "KS_20", "KY_21", "LA_22", 
                 "MD_24", "ME_23", "MI_26", "MN_27", "MS_28", "MO_29", "MT_30", 
                 "NC_37", "ND_38", "NE_31", "NJ_34", "NM_35", "NV_32", "NY_36",
                 "OH_39", "OK_40", "OR_41", "PA_42", "RI_44",
                 "SC_45", "SD_46", "TN_47", "TX_48", "UT_49", "VT_50", "VA_51",
                 "WA_53", "WV_54", "WI_55", "WY_56"
)
state_codes <- tolower(state_codes)
page <- read_html(base_url)

state_list <- page %>%
  html_nodes("a") %>%       
  html_attr("href") %>%    
  str_subset("^[a-z][a-z]/$") 

state_codes <- substr(state_codes, 1, 2)
state_list <- state_list[substr(state_list, 1, 2) %in% state_codes]
url_list <- list() 
for (state in 1:length(state_list)) {
  state_url <- paste0(base_url, state_list[state], "wac/")
  state_page <- read_html(state_url)
  zipfile <- state_page %>%
    html_nodes("a") %>%       
    html_attr("href") %>%     
    str_subset("S000_JT00_2019.csv.gz") 
  url <- paste0(state_url, zipfile)
  url_list <- append(url_list, url)
  print(url)
}

lapply(url_list, cat, "\n", file="LODES data/LODES_wac_19.txt", append=TRUE)
rm(url_list)

state_list_2019_wac <- list.files("LODES data/2019/wac/", full.names = T) 
file_list_2019_wac <- list() 
for (i in 1:length(state_list_2019_wac)) {
  file_list_2019_wac[[i]] <- read_csv(file = state_list_2019_wac[[i]]) %>%
    mutate(jobs = C000,
           man = CNS05,
           ret = CNS07) %>%
    select(w_geocode, jobs, man, ret)
}

wac_2019 <- rbindlist(file_list_2019_wac, use.names = TRUE)
rm(file_list_2019_wac, state_list_2019_wac) 
wac_2019$Year <- 2019
write_csv(wac_2019, file = "LODES data/wac_2019.csv")
rm(wac_2019)

