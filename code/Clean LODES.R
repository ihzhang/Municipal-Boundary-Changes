# Clean LODES data ####
# Clean LODES data ####
setwd("/Volumes/GoogleDrive/.shortcut-targets-by-id/1txEHbDEqbELgEgsEHW1S64HfeilcEXPS/QE2") # @RA you should modify this file path as necessary

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


state_list_2010_rac <- list.files("/Users/phalpha/Desktop/Stanford/Winter 2022/changing cities/csv_files/2010/rac/", all.files = FALSE, full.names = FALSE) # read in all files, saves file names in a vector
file_list_2010_rac <- list() # initiate an empty list
for (i in 1:length(state_list_2010_rac)) {
  file_list_2010_rac[[i]] <- read_csv(file = paste0("/Users/phalpha/Desktop/Stanford/Winter 2022/changing cities/csv_files/2010/rac/", state_list_2010_rac[[i]])) %>%
    mutate(pct_hincome = (CE03/C000)*100) %>%
    select(h_geocode, pct_hincome)
}

rac_2010 <- rbindlist(file_list_2010_rac, use.names = TRUE)
rm(file_list_2010_rac, state_list_2010_rac) # I like to remove useless objects from the environment ASAP to avoid clutter
rac_2010$Year <- 2010
write_csv(rac_2010, file = "/Users/phalpha/Desktop/Stanford/Winter 2022/changing cities/rac_2010.csv")
# repeat for 2010 WAC (double-check variables needed in Google Doc), and 2014.
# output:
# 2 files -- one for 2010, one for 2014 that has block IDs, the pct_hincome variable and the # of jobs variable
# look into the left_join function


state_list_2014_rac <- list.files("/Users/phalpha/Desktop/Stanford/Winter 2022/changing cities/csv_files/2014/rac/", all.files = FALSE, full.names = FALSE) # read in all files, saves file names in a vector
file_list_2014_rac <- list() # initiate an empty list
for (i in 1:length(state_list_2014_rac)) {
  file_list_2014_rac[[i]] <- read_csv(file = paste0("/Users/phalpha/Desktop/Stanford/Winter 2022/changing cities/csv_files/2014/rac/", state_list_2014_rac[[i]])) %>%
    mutate(pct_hincome = (CE03/C000)*100) %>%
    select(h_geocode, pct_hincome)
}

rac_2014 <- rbindlist(file_list_2014_rac, use.names = TRUE)
rm(file_list_2014_rac, state_list_2014_rac) # I like to remove useless objects from the environment ASAP to avoid clutter
rac_2014$Year <- 2014
write_csv(rac_2014, file = "/Users/phalpha/Desktop/Stanford/Winter 2022/changing cities/rac_2014.csv")



state_list_2010_wac <- list.files("/Users/phalpha/Desktop/Stanford/Winter 2022/changing cities/csv_files/2010/wac/", all.files = FALSE, full.names = FALSE) # read in all files, saves file names in a vector
file_list_2010_wac <- list() # initiate an empty list
for (i in 1:length(state_list_2010_wac)) {
  file_list_2010_wac[[i]] <- read_csv(file = paste0("/Users/phalpha/Desktop/Stanford/Winter 2022/changing cities/csv_files/2010/wac/", state_list_2010_wac[[i]])) %>%
    mutate(Manufacturing = CNS05) %>%
    mutate(Retail = CNS07) %>%

    select(w_geocode, Manufacturing, Retail)
}

wac_2010 <- rbindlist(file_list_2010_wac, use.names = TRUE)
rm(file_list_2010_wac, state_list_2010_wac) # I like to remove useless objects from the environment ASAP to avoid clutter
wac_2010$Year <- 2010
write_csv(wac_2010, file = "/Users/phalpha/Desktop/Stanford/Winter 2022/changing cities/wac_2010.csv")


state_list_2014_wac <- list.files("/Users/phalpha/Desktop/Stanford/Winter 2022/changing cities/csv_files/2014/wac/", all.files = FALSE, full.names = FALSE) # read in all files, saves file names in a vector
file_list_2014_wac <- list() # initiate an empty list
for (i in 1:length(state_list_2014_wac)) {
  file_list_2014_wac[[i]] <- read_csv(file = paste0("/Users/phalpha/Desktop/Stanford/Winter 2022/changing cities/csv_files/2010/wac/", state_list_2014_wac[[i]])) %>%
    mutate(Manufacturing = CNS05) %>%
    mutate(Retail = CNS07) %>%

    select(w_geocode, Manufacturing, Retail)
}

wac_2014 <- rbindlist(file_list_2014_wac, use.names = TRUE)
rm(file_list_2014_wac, state_list_2014_wac) # I like to remove useless objects from the environment ASAP to avoid clutter
wac_2014$Year <- 2014
write_csv(wac_2014, file = "/Users/phalpha/Desktop/Stanford/Winter 2022/changing cities/wac_2014.csv")




lodes_2010_file <- rbind(rac_2010,wac_2010)
dim(lodes_2010_file)
write.csv(lodes_2010_file,"/Users/phalpha/Desktop/Stanford/Winter 2022/changing cities/lodes_2010_file.csv")

# repeat for 2010 WAC (double-check variables needed in Google Doc), and 2014.
# output:
# 2 files -- one for 2010, one for 2014 that has block IDs, the pct_hincome variable and the # of jobs variable
# look into the left_join function
