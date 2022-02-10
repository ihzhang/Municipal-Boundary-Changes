# Exploratory analysis ####
install.packages("tidyverse")

setwd("/Volumes/GoogleDrive/.shortcut-targets-by-id/1txEHbDEqbELgEgsEHW1S64HfeilcEXPS/QE2") # @RA you should modify this file path as necessary

# refer to codebook for variable definitions 
# interesting variables would be: 
# white, immigrant, black, minority, income, mhmval, pop 
# compare annexed to non-annexed blocks on these variables, or more


library(tidyverse)
# using:  
aa <- read_csv("annexedblocks0020dem_pl00_newsample_unincorp.csv")

# you could try this code below to see a fast way to generate a table that shows summary statistics 
vars <- c("pop00b", "nhblack00b", "nhwhite00b", "h00b", "min00b",
          "pctnhblack00b", "pctnhwhite00b", "pcth00b", "pctmin00b",
          "dependencyratio00b", "pctowneroccupied",
          "hispvap00b", "nhwvap00b", "nhbvap00b", "minorityvap00b"
)

descriptives <- as.data.frame(aa %>% 
                                group_by(annexed) %>%
                                summarize_at(vars, 
                                             list(~mean(., na.rm = T), ~median(., na.rm = T), ~sd(., na.rm = T))) %>% 
                                t())

# however, I want to start with some "interesting" places
cali <- aa %>% #california is a nice one to pick because there are pockets of republican places even though it's very diverse
  filter(STATEA=="06") # state code for california is 06 
# more codes (we call them "FIPS") here: https://transition.fcc.gov/oet/info/maps/census/fips/fips.txt

#let's see what cities we have 
table(cali$Geo_QName)

as.data.frame(cali %>%
                filter(Geo_QName == "Bakersfield city, California") %>% # use == for characters, = for numbers only 
                group_by(annexed) %>%
                summarize_at(vars, 
                             ~mean(., na.rm = T)) %>% 
                t()) %>%
  mutate(difference = `2` - `1`) %>%
  View()

# next, what can you tell me about Bakersfield as a place? (00p data)
# here's a nifty trick to get all the 2000 place-level variable names 
# remember, you can always use ?grepl to find out more about the command

placevars <- c(names(aa)[which(grepl("00p", names(aa))=="TRUE")], names(aa)[which(grepl("growth", names(aa))=="TRUE")])

cali %>% 
  filter(Geo_QName == "Bakersfield city, California") %>%
  select(placevars) %>% 
  View()

# 1.  pick a few other places and let me know what you find! 

# 2. next, take a look at the place-level data
places <- read_csv("pl9000_var.csv") 
# I want to look at the differences in places that are annexing vs not (annexing_places)
# variables should be fairly intuitive if you are familiar with the variables in aa

