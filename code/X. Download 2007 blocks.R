library(rvest)
library(stringr)
library(htmltools)
library(downloader)

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

# same with terminal & wget 
# wget -P Google\ Drive/My\ Drive/Stanford/QE2/SHP_pl/2007/ -i Google\ Drive/My\ Drive/Stanford/QE2/pl_07.txt
# group shapefiles by state
