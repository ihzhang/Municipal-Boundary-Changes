# -------------------------------------------------------------------------
# Created by: Iris Zhang                     
# Date created: 2021             
# Last revised: 6/30/2022             
# Project: MBC         
# Subproject: Download Shapefiles   
# Re: Shapefiles for 2000, 2007, 2013, 2014, 2019, and 2020       
# -------------------------------------------------------------------------

# Script Description ------------------------------------------------------

# This script uses scraping methods to automate the process of downloading shapefiles from the Census FTP archive 
# [Include 2-3 sentences on any unique aspects/parameters of this script.]

# Inputs:
# You need to know the URLs for each year's shapefiles. I've included them here but generally speaking you should be able to navigate to and then reproduce the URLs by followinug this link: https://www.census.gov/geographies/mapping-files/time-series/geo/tiger-line-file.2010.html 

# Outputs:
# .txt files containing unique links to all the shapefiles for each state for each year for each geography (2020 block file not needed)

# blk_00.txt --> not here; did manually back in like 2019 
# pl_00.txt --> not here; did manually back then 
# blk_07.txt
# pl_07.txt
# blk_08.txt
# pl_08.txt
# blk_09.txt
# pl_09.txt
# blk_10.txt
# pl_10.txt
# blk_11.txt
# pl_11.txt
# blk_12.txt
# pl_12.txt
# blk_13.txt --> not here; did manually
# pl_13.txt --> not here; did manually 
# blk_14.txt --> not here; did manually
# pl_14.txt --> not here; did manually
# blk_15.txt
# pl_15.txt
# blk_16.txt
# pl_16.txt
# blk_17.txt
# pl_17.txt
# blk_18.txt
# pl_18.txt
# blk_19.txt 
# pl_19.txt
# pl_20.txt

# Updates log: 
# #6/30: add 2019 

# Setup -------------------------------------------------------------------
rm(list = ls())
# Packages: 
library(rvest)
library(stringr)
library(htmltools)
library(downloader)

# Directories: 
setwd("~/Google Drive/My Drive/Stanford/QE2")

# homedir <- The smallest directory that contains all input/output folders.
# workdir <- The smallest directory that contains all necessary inputs.
# savedir <- The smallest directory that contains all output folders.
# setwd(paste0(homedir, workdir))

# Import data: 

# Parameters:

# Main Script -------------------------------------------------------------

# 2007 on 2000 boundaries ####
base_url <- "https://www2.census.gov/geo/tiger/TIGER2007FE/"
page <- read_html(base_url)

# state_list
state_list <- page %>%
  html_nodes("a") %>%       # find all links
  html_attr("href") %>%     # get the url
  str_subset("[0-9][0-9]_[A-Z]") 

state_codes <- c("AL_01", "AS_02", "AR_05", "AZ_04", "CA_06", "CO_08", "CT_09", 
                 "DE_10", "FL_12", "GA_13", "HI_15", "IA_19", "ID_16", "IL_17", "IN_18",
                 "KS_20", "KY_21", "LA_22", 
                 "MA_25", "MD_24", "ME_23", "MI_26", "MN_27", "MS_28", "MO_29", "MT_30", 
                 "NC_37", "ND_38", "NE_31", "NH_33", "NJ_34", "NM_35", "NV_32", "NY_36",
                 "OH_39", "OK_40", "OR_41", "PA_42", "RI_44",
                 "SC_45", "SD_46", "TN_47", "TX_48", "UT_49", "VT_50", "VA_51",
                 "WA_53", "WV_54", "WI_55", "WY_56"
)

state_codes <- substr(state_codes, nchar(state_codes)-1, nchar(state_codes))
state_list <- state_list[substr(state_list, 1, 2) %in% state_codes]
url_list <- list()
for (state in 1:length(state_list)) {
  state_url <- paste0(base_url, state_list[state])
  state_page <- read_html(state_url)
  county_codes <- state_page %>%
    html_nodes("a") %>%       # find all links
    html_attr("href") 
  county_codes <- county_codes[substr(county_codes, 1, 2) %in% state_codes]
  for (county in 1:length(county_codes)) {
    county_url <- paste0(state_url, county_codes[county])
    county_page <- read_html(county_url)
    zipfile <- county_page %>%
      html_nodes("a") %>%       # find all links
      html_attr("href") %>%     
      str_subset("tabblock\\.zip") 
    url <- paste0(county_url, zipfile)
    url_list <- append(url_list, url)
    print(url)
  }
} 

lapply(url_list, cat, "\n", file="tabblock_07.txt", append=TRUE)
# use terminal & wget to batch download from this link 
# my code was as follows 
# wget -P Google\ Drive/My\ Drive/Stanford/QE2/SHP_blk_0010/2007/ -i Google\ Drive/My\ Drive/Stanford/QE2/tabblock_07.txt


# repeat for place 
base_url <- "https://www2.census.gov/geo/tiger/TIGER2007FE/"
page <- read_html(base_url)

# state_list
state_list <- page %>%
  html_nodes("a") %>%       # find all links
  html_attr("href") %>%     # get the url
  str_subset("[0-9][0-9]_[A-Z]") 

state_list <- state_list[substr(state_list, 1, 2) %in% state_codes]
url_list <- list()
for (state in 1:length(state_list)) {
  state_url <- paste0(base_url, state_list[state])
  state_page <- read_html(state_url)
  zipfile <- state_page %>%
    html_nodes("a") %>%       # find all links
    html_attr("href") %>%     
    str_subset("place\\.zip") 
  url <- paste0(state_url, zipfile)
  url_list <- append(url_list, url)
  print(url)
} 

lapply(url_list, cat, "\n", file="pl_07.txt", append=TRUE)

# same with terminal & wget 
# wget -P Google\ Drive/My\ Drive/Stanford/QE2/SHP_pl/2007/ -i Google\ Drive/My\ Drive/Stanford/QE2/pl_07.txt
# once downloaded, use unzip \*.zip in your wd with the files, followed by rm -f *.zip to remove unneeded zip files

# the rest of this was done in sherlock, after transferring files over

# 2008 on 2000 boundaries ####
base_url <- "https://www2.census.gov/geo/tiger/TIGER2008/"
page <- read_html(base_url)

# state_list
state_list <- page %>%
  html_nodes("a") %>%       # find all links
  html_attr("href") %>%     # get the url
  str_subset("[0-9][0-9]_[A-Z]") 

state_list <- state_list[substr(state_list, 1, 2) %in% state_codes]
url_list <- list()
for (state in 1:length(state_list)) {
  state_url <- paste0(base_url, state_list[state])
  state_page <- read_html(state_url)
  zipfile <- state_page %>%
    html_nodes("a") %>%       # find all links
    html_attr("href") %>%     
    str_subset("tabblock\\.zip") 
  url <- paste0(state_url, zipfile)
  url_list <- append(url_list, url)
  print(url)
}

lapply(url_list, cat, "\n", file="shapefile_urls/tabblock_08.txt", append=TRUE)

# use terminal & wget to batch download from this link 
# my code was as follows 
# wget -P Google\ Drive/My\ Drive/Stanford/QE2/SHP_blk_0010/2007/ -i Google\ Drive/My\ Drive/Stanford/QE2/tabblock_07.txt

# repeat for place 
url_list <- list()
for (state in 1:length(state_list)) {
  state_url <- paste0(base_url, state_list[state])
  state_page <- read_html(state_url)
  zipfile <- state_page %>%
    html_nodes("a") %>%       # find all links
    html_attr("href") %>%     
    str_subset("place\\.zip") 
  url <- paste0(state_url, zipfile)
  url_list <- append(url_list, url)
  print(url)
}

lapply(url_list, cat, "\n", file="shapefile_urls/pl_08.txt", append=TRUE)

# same with terminal & wget 
# wget -P Google\ Drive/My\ Drive/Stanford/QE2/SHP_pl/2007/ -i Google\ Drive/My\ Drive/Stanford/QE2/pl_07.txt
# once downloaded, use unzip \*.zip in your wd with the files, followed by rm -f *.zip to remove unneeded zip files

# 2009 on 2000 boundaries ####
base_url <- "https://www2.census.gov/geo/tiger/TIGER2009/"
page <- read_html(base_url)

# state_list
state_list <- page %>%
  html_nodes("a") %>%       # find all links
  html_attr("href") %>%     # get the url
  str_subset("[0-9][0-9]_[A-Z]") 
state_list <- state_list[substr(state_list, 1, 2) %in% state_codes]

block_list <- list()
place_list <- list()
for (state in 1:length(state_list)) {
  state_url <- paste0(base_url, state_list[state])
  state_page <- read_html(state_url)
  blockfile <- state_page %>%
    html_nodes("a") %>%       # find all links
    html_attr("href") %>%     
    str_subset("tabblock\\.zip") 
  block <- paste0(state_url, blockfile)
  block_list <- append(block_list, block)
  placefile <- state_page %>%
    html_nodes("a") %>%       # find all links
    html_attr("href") %>%     
    str_subset("place\\.zip") 
  place <- paste0(state_url, placefile)
  place_list <- append(place_list, place)
  print(block)
  print(place)
}

lapply(block_list, cat, "\n", file="shapefile_urls/tabblock_09.txt", append=TRUE)
lapply(place_list, cat, "\n", file="shapefile_urls/place_09.txt", append=TRUE)

# use terminal & wget to batch download from this link 
# my code was as follows 
# wget -P Google\ Drive/My\ Drive/Stanford/QE2/SHP_blk_0010/2007/ -i Google\ Drive/My\ Drive/Stanford/QE2/tabblock_07.txt

# 2011 on 2000 boundaries ####
place_url <- "https://www2.census.gov/geo/tiger/TIGER2011/PLACE/"
place <- read_html(place_url)

block_url <- "https://www2.census.gov/geo/tiger/TIGER2011/TABBLOCK/"
block <- read_html(block_url)

# place_list
place_urls <- place %>%
  html_nodes("a") %>%       # find all links
  html_attr("href") %>%     # get the url
  str_subset("place") 
place_urls <- place_urls[substr(place_urls, 9, 10) %in% state_codes]
block_urls <- block %>%
  html_nodes("a") %>%       # find all links
  html_attr("href") %>%     # get the url
  str_subset("tabblock") 
block_urls <- block_urls[substr(block_urls, 9, 10) %in% state_codes]
block_list <- list()
place_list <- list()

for (state in 1:length(place_urls)) {
  block_use <- paste0(block_url, block_urls[state])
  block_list <- append(block_list, block_use)
  place_use <- paste0(place_url, place_urls[state])
  place_list <- append(place_list, place_use)
  print(block_use)
  print(place_use)
}

lapply(block_list, cat, "\n", file="shapefile_urls/tabblock_11.txt", append=TRUE)
lapply(place_list, cat, "\n", file="shapefile_urls/place_11.txt", append=TRUE)

# use terminal & wget to batch download from this link 
# my code was as follows 
# wget -P Google\ Drive/My\ Drive/Stanford/QE2/SHP_blk_0010/2007/ -i Google\ Drive/My\ Drive/Stanford/QE2/tabblock_07.txt

# 2012 on 2000 boundaries ####
place_url <- "https://www2.census.gov/geo/tiger/TIGER2012/PLACE/"
place <- read_html(place_url)

block_url <- "https://www2.census.gov/geo/tiger/TIGER2012/TABBLOCK/"
block <- read_html(block_url)

# place_list
place_urls <- place %>%
  html_nodes("a") %>%       # find all links
  html_attr("href") %>%     # get the url
  str_subset("place") 
place_urls <- place_urls[substr(place_urls, 9, 10) %in% state_codes]
block_urls <- block %>%
  html_nodes("a") %>%       # find all links
  html_attr("href") %>%     # get the url
  str_subset("tabblock") 
block_urls <- block_urls[substr(block_urls, 9, 10) %in% state_codes]
block_list <- list()
place_list <- list()

for (state in 1:length(place_urls)) {
  block_use <- paste0(block_url, block_urls[state])
  block_list <- append(block_list, block_use)
  place_use <- paste0(place_url, place_urls[state])
  place_list <- append(place_list, place_use)
  print(block_use)
  print(place_use)
}

lapply(block_list, cat, "\n", file="shapefile_urls/tabblock_12.txt", append=TRUE)
lapply(place_list, cat, "\n", file="shapefile_urls/place_12.txt", append=TRUE)

# use terminal & wget to batch download from this link 
# my code was as follows 
# wget -P Google\ Drive/My\ Drive/Stanford/QE2/SHP_blk_0010/2007/ -i Google\ Drive/My\ Drive/Stanford/QE2/tabblock_07.txt

# 2015 on 2000 boundaries ####
place_url <- "https://www2.census.gov/geo/tiger/TIGER2015/PLACE/"
place <- read_html(place_url)

block_url <- "https://www2.census.gov/geo/tiger/TIGER2015/TABBLOCK/"
block <- read_html(block_url)

# place_list
place_urls <- place %>%
  html_nodes("a") %>%       # find all links
  html_attr("href") %>%     # get the url
  str_subset("place") 
place_urls <- place_urls[substr(place_urls, 9, 10) %in% state_codes]
block_urls <- block %>%
  html_nodes("a") %>%       # find all links
  html_attr("href") %>%     # get the url
  str_subset("tabblock") 
block_urls <- block_urls[substr(block_urls, 9, 10) %in% state_codes]
block_list <- list()
place_list <- list()

for (state in 1:length(place_urls)) {
  block_use <- paste0(block_url, block_urls[state])
  block_list <- append(block_list, block_use)
  place_use <- paste0(place_url, place_urls[state])
  place_list <- append(place_list, place_use)
  print(block_use)
  print(place_use)
}

lapply(block_list, cat, "\n", file="shapefile_urls/tabblock_15.txt", append=TRUE)
lapply(place_list, cat, "\n", file="shapefile_urls/place_15.txt", append=TRUE)

# use terminal & wget to batch download from this link 
# my code was as follows 
# wget -P Google\ Drive/My\ Drive/Stanford/QE2/SHP_blk_0010/2007/ -i Google\ Drive/My\ Drive/Stanford/QE2/tabblock_07.txt

# 2016 on 2000 boundaries ####
place_url <- "https://www2.census.gov/geo/tiger/TIGER2016/PLACE/"
place <- read_html(place_url)

block_url <- "https://www2.census.gov/geo/tiger/TIGER2016/TABBLOCK/"
block <- read_html(block_url)

# place_list
place_urls <- place %>%
  html_nodes("a") %>%       # find all links
  html_attr("href") %>%     # get the url
  str_subset("place") 
place_urls <- place_urls[substr(place_urls, 9, 10) %in% state_codes]
block_urls <- block %>%
  html_nodes("a") %>%       # find all links
  html_attr("href") %>%     # get the url
  str_subset("tabblock") 
block_urls <- block_urls[substr(block_urls, 9, 10) %in% state_codes]
block_list <- list()
place_list <- list()

for (state in 1:length(place_urls)) {
  block_use <- paste0(block_url, block_urls[state])
  block_list <- append(block_list, block_use)
  place_use <- paste0(place_url, place_urls[state])
  place_list <- append(place_list, place_use)
  print(block_use)
  print(place_use)
}

lapply(block_list, cat, "\n", file="shapefile_urls/tabblock_16.txt", append=TRUE)
lapply(place_list, cat, "\n", file="shapefile_urls/place_16.txt", append=TRUE)

# use terminal & wget to batch download from this link 
# my code was as follows 
# wget -P Google\ Drive/My\ Drive/Stanford/QE2/SHP_blk_0010/2007/ -i Google\ Drive/My\ Drive/Stanford/QE2/tabblock_07.txt

# 2017 on 2000 boundaries ####
place_url <- "https://www2.census.gov/geo/tiger/TIGER2017/PLACE/"
place <- read_html(place_url)

block_url <- "https://www2.census.gov/geo/tiger/TIGER2017/TABBLOCK/"
block <- read_html(block_url)

# place_list
place_urls <- place %>%
  html_nodes("a") %>%       # find all links
  html_attr("href") %>%     # get the url
  str_subset("place") 
place_urls <- place_urls[substr(place_urls, 9, 10) %in% state_codes]
block_urls <- block %>%
  html_nodes("a") %>%       # find all links
  html_attr("href") %>%     # get the url
  str_subset("tabblock") 
block_urls <- block_urls[substr(block_urls, 9, 10) %in% state_codes]
block_list <- list()
place_list <- list()

for (state in 1:length(place_urls)) {
  block_use <- paste0(block_url, block_urls[state])
  block_list <- append(block_list, block_use)
  place_use <- paste0(place_url, place_urls[state])
  place_list <- append(place_list, place_use)
  print(block_use)
  print(place_use)
}

lapply(block_list, cat, "\n", file="shapefile_urls/tabblock_17.txt", append=TRUE)
lapply(place_list, cat, "\n", file="shapefile_urls/place_17.txt", append=TRUE)

# use terminal & wget to batch download from this link 
# my code was as follows 
# wget -P Google\ Drive/My\ Drive/Stanford/QE2/SHP_blk_0010/2007/ -i Google\ Drive/My\ Drive/Stanford/QE2/tabblock_07.txt

# 2018 on 2000 boundaries ####
place_url <- "https://www2.census.gov/geo/tiger/TIGER2018/PLACE/"
place <- read_html(place_url)

block_url <- "https://www2.census.gov/geo/tiger/TIGER2018/TABBLOCK/"
block <- read_html(block_url)

# place_list
place_urls <- place %>%
  html_nodes("a") %>%       # find all links
  html_attr("href") %>%     # get the url
  str_subset("place") 
place_urls <- place_urls[substr(place_urls, 9, 10) %in% state_codes]
block_urls <- block %>%
  html_nodes("a") %>%       # find all links
  html_attr("href") %>%     # get the url
  str_subset("tabblock") 
block_urls <- block_urls[substr(block_urls, 9, 10) %in% state_codes]
block_list <- list()
place_list <- list()

for (state in 1:length(place_urls)) {
  block_use <- paste0(block_url, block_urls[state])
  block_list <- append(block_list, block_use)
  place_use <- paste0(place_url, place_urls[state])
  place_list <- append(place_list, place_use)
  print(block_use)
  print(place_use)
}

lapply(block_list, cat, "\n", file="shapefile_urls/tabblock_18.txt", append=TRUE)
lapply(place_list, cat, "\n", file="shapefile_urls/place_18.txt", append=TRUE)

# use terminal & wget to batch download from this link 
# my code was as follows 
# wget -P Google\ Drive/My\ Drive/Stanford/QE2/SHP_blk_0010/2007/ -i Google\ Drive/My\ Drive/Stanford/QE2/tabblock_07.txt

# 2019 on 2000 boundaries ####
place_url <- "https://www2.census.gov/geo/tiger/TIGER2019/PLACE/"
place <- read_html(place_url)

block_url <- "https://www2.census.gov/geo/tiger/TIGER2019/TABBLOCK/"
block <- read_html(block_url)

# place_list
place_urls <- place %>%
  html_nodes("a") %>%       # find all links
  html_attr("href") %>%     # get the url
  str_subset("place") 
place_urls <- place_urls[substr(place_urls, 9, 10) %in% state_codes]
block_urls <- block %>%
  html_nodes("a") %>%       # find all links
  html_attr("href") %>%     # get the url
  str_subset("tabblock") 
block_urls <- block_urls[substr(block_urls, 9, 10) %in% state_codes]
block_list <- list()
place_list <- list()

for (state in 1:length(place_urls)) {
  block_use <- paste0(block_url, block_urls[state])
  block_list <- append(block_list, block_use)
  place_use <- paste0(place_url, place_urls[state])
  place_list <- append(place_list, place_use)
  print(block_use)
  print(place_use)
}

lapply(block_list, cat, "\n", file="shapefile_urls/tabblock_19.txt", append=TRUE)
lapply(place_list, cat, "\n", file="shapefile_urls/place_19.txt", append=TRUE)

# use terminal & wget to batch download from this link 
# my code was as follows 
# wget -P Google\ Drive/My\ Drive/Stanford/QE2/SHP_blk_0010/2007/ -i Google\ Drive/My\ Drive/Stanford/QE2/tabblock_07.txt


# the rest of this was done in sherlock, after transferring files over

# 2021 on 2020 boundaries ####
# blocks 
base_url <- "https://www2.census.gov/geo/tiger/TIGER2021/TABBLOCK20/"
page <- read_html(base_url)
state_list <- page %>%
  html_nodes("a") %>%       # find all links
  html_attr("href") %>%     # get the url
  str_subset(".zip") 

state_codes <- c("AL_01", "AS_02", "AR_05", "AZ_04", "CA_06", "CO_08", "CT_09", 
                 "DE_10", "FL_12", "GA_13", "HI_15", "IA_19", "ID_16", "IL_17", "IN_18",
                 "KS_20", "KY_21", "LA_22", 
                 "MA_25", "MD_24", "ME_23", "MI_26", "MN_27", "MS_28", "MO_29", "MT_30", 
                 "NC_37", "ND_38", "NE_31", "NH_33", "NJ_34", "NM_35", "NV_32", "NY_36",
                 "OH_39", "OK_40", "OR_41", "PA_42", "RI_44",
                 "SC_45", "SD_46", "TN_47", "TX_48", "UT_49", "VT_50", "VA_51",
                 "WA_53", "WV_54", "WI_55", "WY_56"
)

state_codes <- substr(state_codes, nchar(state_codes)-1, nchar(state_codes))
state_list <- state_list[substr(state_list, 9, 10) %in% state_codes]
url_list <- list()
for (state in 1:length(state_list)) {
  state_url <- paste0(base_url, state_list[state])
  url_list <- append(url_list, state_url)
  print(state_url)
} 

lapply(url_list, cat, "\n", file="blk_2021.txt", append=TRUE)

# use terminal & wget to batch download from this link 
# my code was as follows 
# wget -P Google\ Drive/My\ Drive/Stanford/QE2/SHP_blk_0010/2007/ -i Google\ Drive/My\ Drive/Stanford/QE2/tabblock_07.txt

# repeat for place 
base_url <- "https://www2.census.gov/geo/tiger/TIGER2021/PLACE/"
page <- read_html(base_url)
state_list <- page %>%
  html_nodes("a") %>%       # find all links
  html_attr("href") %>%     # get the url
  str_subset(".zip") 

state_codes <- c("AL_01", "AS_02", "AR_05", "AZ_04", "CA_06", "CO_08", "CT_09", 
                 "DE_10", "FL_12", "GA_13", "HI_15", "IA_19", "ID_16", "IL_17", "IN_18",
                 "KS_20", "KY_21", "LA_22", 
                 "MA_25", "MD_24", "ME_23", "MI_26", "MN_27", "MS_28", "MO_29", "MT_30", 
                 "NC_37", "ND_38", "NE_31", "NH_33", "NJ_34", "NM_35", "NV_32", "NY_36",
                 "OH_39", "OK_40", "OR_41", "PA_42", "RI_44",
                 "SC_45", "SD_46", "TN_47", "TX_48", "UT_49", "VT_50", "VA_51",
                 "WA_53", "WV_54", "WI_55", "WY_56"
)
state_codes <- substr(state_codes, nchar(state_codes)-1, nchar(state_codes))
state_list <- state_list[substr(state_list, 9, 10) %in% state_codes]
url_list <- list()
for (state in 1:length(state_list)) {
  state_url <- paste0(base_url, state_list[state])
  url_list <- append(url_list, state_url)
  print(state_url)
} 

lapply(url_list, cat, "\n", file="pl_21.txt", append=TRUE)

# 2020 on 2020 boundaries ####
base_url <- "https://www2.census.gov/geo/tiger/TIGER2020/TABBLOCK20/"
page <- read_html(base_url)
state_list <- page %>%
  html_nodes("a") %>%       # find all links
  html_attr("href") %>%     # get the url
  str_subset(".zip") 

state_codes <- c("AL_01", "AS_02", "AR_05", "AZ_04", "CA_06", "CO_08", "CT_09", 
                 "DE_10", "FL_12", "GA_13", "HI_15", "IA_19", "ID_16", "IL_17", "IN_18",
                 "KS_20", "KY_21", "LA_22", 
                 "MA_25", "MD_24", "ME_23", "MI_26", "MN_27", "MS_28", "MO_29", "MT_30", 
                 "NC_37", "ND_38", "NE_31", "NH_33", "NJ_34", "NM_35", "NV_32", "NY_36",
                 "OH_39", "OK_40", "OR_41", "PA_42", "RI_44",
                 "SC_45", "SD_46", "TN_47", "TX_48", "UT_49", "VT_50", "VA_51",
                 "WA_53", "WV_54", "WI_55", "WY_56"
)

state_codes <- substr(state_codes, nchar(state_codes)-1, nchar(state_codes))
state_list <- state_list[substr(state_list, 9, 10) %in% state_codes]
url_list <- list()
for (state in 1:length(state_list)) {
  state_url <- paste0(base_url, state_list[state])
  url_list <- append(url_list, state_url)
  print(state_url)
} 

lapply(url_list, cat, "\n", file="blk_2020.txt", append=TRUE)

# places 
base_url <- "https://www2.census.gov/geo/tiger/TIGER2020/PLACE/"
page <- read_html(base_url)
state_list <- page %>%
  html_nodes("a") %>%       # find all links
  html_attr("href") %>%     # get the url
  str_subset(".zip") 

state_codes <- c("AL_01", "AS_02", "AR_05", "AZ_04", "CA_06", "CO_08", "CT_09", 
                 "DE_10", "FL_12", "GA_13", "HI_15", "IA_19", "ID_16", "IL_17", "IN_18",
                 "KS_20", "KY_21", "LA_22", 
                 "MA_25", "MD_24", "ME_23", "MI_26", "MN_27", "MS_28", "MO_29", "MT_30", 
                 "NC_37", "ND_38", "NE_31", "NH_33", "NJ_34", "NM_35", "NV_32", "NY_36",
                 "OH_39", "OK_40", "OR_41", "PA_42", "RI_44",
                 "SC_45", "SD_46", "TN_47", "TX_48", "UT_49", "VT_50", "VA_51",
                 "WA_53", "WV_54", "WI_55", "WY_56"
)
state_codes <- substr(state_codes, nchar(state_codes)-1, nchar(state_codes))
state_list <- state_list[substr(state_list, 9, 10) %in% state_codes]
url_list <- list()
for (state in 1:length(state_list)) {
  state_url <- paste0(base_url, state_list[state])
  url_list <- append(url_list, state_url)
  print(state_url)
} 

lapply(url_list, cat, "\n", file="pl_20.txt", append=TRUE)

# 2007 on 2000 boundaries ####
base_url <- "https://www2.census.gov/geo/tiger/TIGER2007FE/"
page <- read_html(base_url)

# state_list
state_list <- page %>%
  html_nodes("a") %>%       # find all links
  html_attr("href") %>%     # get the url
  str_subset("[0-9][0-9]_[A-Z]") 

state_codes <- c("AL_01", "AS_02", "AR_05", "AZ_04", "CA_06", "CO_08", "CT_09", 
                 "DE_10", "FL_12", "GA_13", "HI_15", "IA_19", "ID_16", "IL_17", "IN_18",
                 "KS_20", "KY_21", "LA_22", 
                 "MA_25", "MD_24", "ME_23", "MI_26", "MN_27", "MS_28", "MO_29", "MT_30", 
                 "NC_37", "ND_38", "NE_31", "NH_33", "NJ_34", "NM_35", "NV_32", "NY_36",
                 "OH_39", "OK_40", "OR_41", "PA_42", "RI_44",
                 "SC_45", "SD_46", "TN_47", "TX_48", "UT_49", "VT_50", "VA_51",
                 "WA_53", "WV_54", "WI_55", "WY_56"
)

state_codes <- substr(state_codes, nchar(state_codes)-1, nchar(state_codes))
state_list <- state_list[substr(state_list, 1, 2) %in% state_codes]
url_list <- list()
for (state in 1:length(state_list)) {
  state_url <- paste0(base_url, state_list[state])
  state_page <- read_html(state_url)
  county_codes <- state_page %>%
    html_nodes("a") %>%       # find all links
    html_attr("href") 
  county_codes <- county_codes[substr(county_codes, 1, 2) %in% state_codes]
  for (county in 1:length(county_codes)) {
    county_url <- paste0(state_url, county_codes[county])
    county_page <- read_html(county_url)
    zipfile <- county_page %>%
      html_nodes("a") %>%       # find all links
      html_attr("href") %>%     
      str_subset("tabblock\\.zip") 
    url <- paste0(county_url, zipfile)
    url_list <- append(url_list, url)
    print(url)
  }
} 

lapply(url_list, cat, "\n", file="tabblock_07.txt", append=TRUE)
# use terminal & wget to batch download from this link 
# my code was as follows 
# wget -P Google\ Drive/My\ Drive/Stanford/QE2/SHP_blk_0010/2007/ -i Google\ Drive/My\ Drive/Stanford/QE2/tabblock_07.txt

# repeat for place 
base_url <- "https://www2.census.gov/geo/tiger/TIGER2007FE/"
page <- read_html(base_url)

# state_list
state_list <- page %>%
  html_nodes("a") %>%       # find all links
  html_attr("href") %>%     # get the url
  str_subset("[0-9][0-9]_[A-Z]") 

state_codes <- c("AL_01", "AS_02", "AR_05", "AZ_04", "CA_06", "CO_08", "CT_09", 
                 "DE_10", "FL_12", "GA_13", "HI_15", "IA_19", "ID_16", "IL_17", "IN_18",
                 "KS_20", "KY_21", "LA_22", 
                 "MA_25", "MD_24", "ME_23", "MI_26", "MN_27", "MS_28", "MO_29", "MT_30", 
                 "NC_37", "ND_38", "NE_31", "NH_33", "NJ_34", "NM_35", "NV_32", "NY_36",
                 "OH_39", "OK_40", "OR_41", "PA_42", "RI_44",
                 "SC_45", "SD_46", "TN_47", "TX_48", "UT_49", "VT_50", "VA_51",
                 "WA_53", "WV_54", "WI_55", "WY_56"
)

state_codes <- substr(state_codes, nchar(state_codes)-1, nchar(state_codes))
state_list <- state_list[substr(state_list, 1, 2) %in% state_codes]
url_list <- list()
for (state in 1:length(state_list)) {
  state_url <- paste0(base_url, state_list[state])
  state_page <- read_html(state_url)
  zipfile <- state_page %>%
    html_nodes("a") %>%       # find all links
    html_attr("href") %>%     
    str_subset("place\\.zip") 
  url <- paste0(state_url, zipfile)
  url_list <- append(url_list, url)
  print(url)
} 

lapply(url_list, cat, "\n", file="pl_07.txt", append=TRUE)

# 2019 on 2010 boundaries ####
base_url <- "https://www2.census.gov/geo/tiger/TIGER2019/TABBLOCK/"
page <- read_html(base_url)

# state_list
state_list <- page %>%
  html_nodes("a") %>%       # find all links
  html_attr("href") %>%     # get the url
  str_subset(".zip") 

state_codes <- c("AL_01", "AS_02", "AR_05", "AZ_04", "CA_06", "CO_08", "CT_09", 
                 "DE_10", "FL_12", "GA_13", "HI_15", "IA_19", "ID_16", "IL_17", "IN_18",
                 "KS_20", "KY_21", "LA_22", 
                 "MA_25", "MD_24", "ME_23", "MI_26", "MN_27", "MS_28", "MO_29", "MT_30", 
                 "NC_37", "ND_38", "NE_31", "NH_33", "NJ_34", "NM_35", "NV_32", "NY_36",
                 "OH_39", "OK_40", "OR_41", "PA_42", "RI_44",
                 "SC_45", "SD_46", "TN_47", "TX_48", "UT_49", "VT_50", "VA_51",
                 "WA_53", "WV_54", "WI_55", "WY_56"
)

state_codes <- substr(state_codes, nchar(state_codes)-1, nchar(state_codes))
state_list <- state_list[substr(state_list, 9, 10) %in% state_codes]
url_list <- list()
for (state in 1:length(state_list)) {
  state_url <- paste0(base_url, state_list[state])
  url_list <- append(url_list, state_url)
  print(state_url)
} 

lapply(url_list, cat, "\n", file="shapefile_urls/tabblock_19.txt", append=TRUE)
# use terminal & wget to batch download from this link 
# my code was as follows 
# wget -P Google\ Drive/My\ Drive/Stanford/QE2/SHP_blk_0010/2019/ -i Google\ Drive/My\ Drive/Stanford/QE2/shapefile_urls/tabblock_19.txt

# repeat for place 
base_url <- "https://www2.census.gov/geo/tiger/TIGER2019/PLACE/"
page <- read_html(base_url)

# state_list
state_list <- page %>%
  html_nodes("a") %>%       # find all links
  html_attr("href") %>%     # get the url
  str_subset(".zip") 

state_codes <- c("AL_01", "AS_02", "AR_05", "AZ_04", "CA_06", "CO_08", "CT_09", 
                 "DE_10", "FL_12", "GA_13", "HI_15", "IA_19", "ID_16", "IL_17", "IN_18",
                 "KS_20", "KY_21", "LA_22", 
                 "MA_25", "MD_24", "ME_23", "MI_26", "MN_27", "MS_28", "MO_29", "MT_30", 
                 "NC_37", "ND_38", "NE_31", "NH_33", "NJ_34", "NM_35", "NV_32", "NY_36",
                 "OH_39", "OK_40", "OR_41", "PA_42", "RI_44",
                 "SC_45", "SD_46", "TN_47", "TX_48", "UT_49", "VT_50", "VA_51",
                 "WA_53", "WV_54", "WI_55", "WY_56"
)

state_codes <- substr(state_codes, nchar(state_codes)-1, nchar(state_codes))
state_list <- state_list[substr(state_list, 9, 10) %in% state_codes]
url_list <- list()
for (state in 1:length(state_list)) {
  state_url <- paste0(base_url, state_list[state])
  url_list <- append(url_list, state_url)
  print(state_url)
} 

lapply(url_list, cat, "\n", file="shapefile_urls/pl_19.txt", append=TRUE)
