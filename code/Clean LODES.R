# Clean LODES data ####
# Clean LODES data ####
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

state_list_2010_rac <- list.files("Demography/LODES data/2010/rac/", all.files = FALSE, full.names = FALSE) # read in all files, saves file names in a vector
file_list_2010_rac <- list() # initiate an empty list
for (i in 1:length(state_list_2010_rac)) {
  file_list_2010_rac[[i]] <- read_csv(file = paste0("Demography/LODES data/2010/rac/", state_list_2010_rac[[i]])) %>%
    mutate(njobs10 = C000,
           nhincjobs10 = CE03) %>%
    select(h_geocode, njobs10, nhincjobs10)
}

rac_2010 <- rbindlist(file_list_2010_rac, use.names = TRUE)
rm(file_list_2010_rac, state_list_2010_rac) # I like to remove useless objects from the environment ASAP to avoid clutter
rac_2010$Year <- 2010
write_csv(rac_2010, file = "rac_2010.csv")
rm(rac_2010)

state_list_2014_rac <- list.files("Demography/LODES data/2014/rac/", all.files = FALSE, full.names = FALSE) # read in all files, saves file names in a vector
file_list_2014_rac <- list() # initiate an empty list
for (i in 1:length(state_list_2014_rac)) {
  file_list_2014_rac[[i]] <- read_csv(file = paste0("Demography/LODES data/2014/rac/", state_list_2014_rac[[i]])) %>%
    mutate(njobs14 = C000,
           nhincjobs14 = CE03) %>%
    select(h_geocode, njobs14, nhincjobs14)
}

rac_2014 <- rbindlist(file_list_2014_rac, use.names = TRUE)
rm(file_list_2014_rac, state_list_2014_rac) # I like to remove useless objects from the environment ASAP to avoid clutter
rac_2014$Year <- 2014
write_csv(rac_2014, file = "rac_2014.csv")
rm(rac_2014)

# use the following to download 2007 and 2017 files ####
# we want a .txt file that contains all the URLS for all the files we want to download 
# then, in terminal, I will download those files using the .txt file 
# we use LODES 5 for 2000 data but LODES 7 for 2017 
base_url <- "https://lehd.ces.census.gov/data/lodes/LODES5/"
page <- read_html(base_url)

# this is just a standard list of states I copied and pasted from other files
# except I changed AS_02 for alaska to AK_02, since that's how the LODES data is coded 
# one easy way to check is to see that your character vector is of length 50, because we have 50 states! 
# note--we don't need HI, so actually there's 49.
state_codes <- c("AL_01", "AK_02", "AR_05", "AZ_04", "CA_06", "CO_08", "CT_09", 
                 "DE_10", "FL_12", "GA_13", "IA_19", "ID_16", "IL_17", "IN_18",
                 "KS_20", "KY_21", "LA_22", 
                 "MA_25", "MD_24", "ME_23", "MI_26", "MN_27", "MS_28", "MO_29", "MT_30", 
                 "NC_37", "ND_38", "NE_31", "NH_33", "NJ_34", "NM_35", "NV_32", "NY_36",
                 "OH_39", "OK_40", "OR_41", "PA_42", "RI_44",
                 "SC_45", "SD_46", "TN_47", "TX_48", "UT_49", "VT_50", "VA_51",
                 "WA_53", "WV_54", "WI_55", "WY_56"
)
state_codes <- tolower(state_codes)

# state_list
state_list <- page %>%
  html_nodes("a") %>%       # find all links
  html_attr("href") %>%     # get the url
  str_subset("^[a-z][a-z]/$") 

state_codes <- substr(state_codes, 1, 2)
state_list <- state_list[substr(state_list, 1, 2) %in% state_codes]

url_list <- list() # store each url in a list 
for (state in 1:length(state_list)) {
  state_url <- paste0(base_url, state_list[state], "rac/")
  state_page <- read_html(state_url)
  zipfile <- state_page %>%
    html_nodes("a") %>%       # find all links
    html_attr("href") %>%     
    str_subset("S000_JT00_2007.csv.gz") 
  url <- paste0(state_url, zipfile)
  url_list <- append(url_list, url)
  print(url)
}

# turn list into txt file - a good place to double-check that the list has 49 observations
lapply(url_list, cat, "\n", file="LODES_07.txt", append=TRUE)
rm(url_list)

# 2017 
# remember--we use LODES7, so the base url is: 
base_url <- "https://lehd.ces.census.gov/data/lodes/LODES7/"

state_codes <- c("AL_01", "AK_02", "AR_05", "AZ_04", "CA_06", "CO_08", "CT_09", 
                 "DE_10", "FL_12", "GA_13", "IA_19", "ID_16", "IL_17", "IN_18",
                 "KS_20", "KY_21", "LA_22", 
                 "MA_25", "MD_24", "ME_23", "MI_26", "MN_27", "MS_28", "MO_29", "MT_30", 
                 "NC_37", "ND_38", "NE_31", "NH_33", "NJ_34", "NM_35", "NV_32", "NY_36",
                 "OH_39", "OK_40", "OR_41", "PA_42", "RI_44",
                 "SC_45", "SD_46", "TN_47", "TX_48", "UT_49", "VT_50", "VA_51",
                 "WA_53", "WV_54", "WI_55", "WY_56"
)
state_codes <- tolower(state_codes)
page <- read_html(base_url)

state_list <- page %>%
  html_nodes("a") %>%       # find all links
  html_attr("href") %>%     # get the url
  str_subset("^[a-z][a-z]/$") 

state_codes <- substr(state_codes, 1, 2)
state_list <- state_list[substr(state_list, 1, 2) %in% state_codes]

url_list <- list() # store each url in a list 
for (state in 1:length(state_list)) {
  state_url <- paste0(base_url, state_list[state], "rac/")
  state_page <- read_html(state_url)
  zipfile <- state_page %>%
    html_nodes("a") %>%       # find all links
    html_attr("href") %>%     
    str_subset("S000_JT00_2017.csv.gz") 
  url <- paste0(state_url, zipfile)
  url_list <- append(url_list, url)
  print(url)
}

# turn list into txt file - a good place to double-check that the list has 49 observations
lapply(url_list, cat, "\n", file="LODES_17.txt", append=TRUE)
rm(url_list)

# clean 2007 data ####
state_list_2007_rac <- list.files("Demography/LODES data/2007/rac/", all.files = FALSE, full.names = FALSE) # read in all files, saves file names in a vector
file_list_2007_rac <- list() # initiate an empty list
for (i in 1:length(state_list_2007_rac)) {
  file_list_2007_rac[[i]] <- read_csv(file = paste0("Demography/LODES data/2007/rac/", state_list_2007_rac[[i]])) %>%
    mutate(njobs07 = C000,
           nhincjobs07 = CE03) %>%
    select(h_geocode, njobs07, nhincjobs07)
}

rac_2007 <- rbindlist(file_list_2007_rac, use.names = TRUE)
rm(file_list_2007_rac, state_list_2007_rac) # I like to remove useless objects from the environment ASAP to avoid clutter
rac_2007$Year <- 2007
write_csv(rac_2007, file = "rac_2007.csv")
rm(rac_2007)

# clean 2017 data ####
state_list_2017_rac <- list.files("Demography/LODES data/2017/rac/", all.files = FALSE, full.names = FALSE) # read in all files, saves file names in a vector
file_list_2017_rac <- list() # initiate an empty list
for (i in 1:length(state_list_2017_rac)) {
  file_list_2017_rac[[i]] <- read_csv(file = paste0("Demography/LODES data/2017/rac/", state_list_2017_rac[[i]])) %>%
    mutate(njobs17 = C000,
           nhincjobs17 = CE03) %>%
    select(h_geocode, njobs17, nhincjobs17)
}

rac_2017 <- rbindlist(file_list_2017_rac, use.names = TRUE)
rm(file_list_2017_rac, state_list_2017_rac) # I like to remove useless objects from the environment ASAP to avoid clutter
rac_2017$Year <- 2017
write_csv(rac_2017, file = "rac_2017.csv")
rm(rac_2017)

# OLD--don't need wac anymore ####
# repeat for 2010 WAC (double-check variables needed in Google Doc), and 2014.
# output:
# 2 files -- one for 2010, one for 2014 that has block IDs, the pct_hincome variable and the # of jobs variable
# look into the left_join function

# state_list_2010_wac <- list.files("/Users/phalpha/Desktop/Stanford/Winter 2022/changing cities/csv_files/2010/wac/", all.files = FALSE, full.names = FALSE) # read in all files, saves file names in a vector
# file_list_2010_wac <- list() # initiate an empty list
# for (i in 1:length(state_list_2010_wac)) {
#   file_list_2010_wac[[i]] <- read_csv(file = paste0("/Users/phalpha/Desktop/Stanford/Winter 2022/changing cities/csv_files/2010/wac/", state_list_2010_wac[[i]])) %>%
#     mutate(Manufacturing = CNS05) %>%
#     mutate(Retail = CNS07) %>%
# 
#     select(w_geocode, Manufacturing, Retail)
# }
# 
# wac_2010 <- rbindlist(file_list_2010_wac, use.names = TRUE)
# rm(file_list_2010_wac, state_list_2010_wac) # I like to remove useless objects from the environment ASAP to avoid clutter
# wac_2010$Year <- 2010
# write_csv(wac_2010, file = "/Users/phalpha/Desktop/Stanford/Winter 2022/changing cities/wac_2010.csv")
# 
# 
# state_list_2014_wac <- list.files("/Users/phalpha/Desktop/Stanford/Winter 2022/changing cities/csv_files/2014/wac/", all.files = FALSE, full.names = FALSE) # read in all files, saves file names in a vector
# file_list_2014_wac <- list() # initiate an empty list
# for (i in 1:length(state_list_2014_wac)) {
#   file_list_2014_wac[[i]] <- read_csv(file = paste0("/Users/phalpha/Desktop/Stanford/Winter 2022/changing cities/csv_files/2010/wac/", state_list_2014_wac[[i]])) %>%
#     mutate(Manufacturing = CNS05) %>%
#     mutate(Retail = CNS07) %>%
# 
#     select(w_geocode, Manufacturing, Retail)
# }
# 
# wac_2014 <- rbindlist(file_list_2014_wac, use.names = TRUE)
# rm(file_list_2014_wac, state_list_2014_wac) # I like to remove useless objects from the environment ASAP to avoid clutter
# wac_2014$Year <- 2014
# write_csv(wac_2014, file = "/Users/phalpha/Desktop/Stanford/Winter 2022/changing cities/wac_2014.csv")
# 

# lodes_2010_file <- rbind(rac_2010,wac_2010)
# dim(lodes_2010_file)
# write.csv(lodes_2010_file,"/Users/phalpha/Desktop/Stanford/Winter 2022/changing cities/lodes_2010_file.csv")

# repeat for 2010 WAC (double-check variables needed in Google Doc), and 2014.
# output:
# 2 files -- one for 2010, one for 2014 that has block IDs, the pct_hincome variable and the # of jobs variable
# look into the left_join function
