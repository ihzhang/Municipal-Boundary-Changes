# clean block and place data before identifying annexed blocks ##############
# Purpose of this script is to create necessary variables of interest #######
# for place90, place00, block00 and block10 datasets before doing ###########
# analysis outlined in BAS0010. This is script step ONE. ####################

rm(list = ls())
setwd("~/Google Drive/My Drive/Stanford/QE2") # use setwd() to set your home directory 

# you likely won't have some of these packages yet. To get them, you'll have to do install.packages("PACKAGENAME")
# luckily you only need to do this step once. You will have to call library("PACKAGENAME") each time, however.
# coding convention is to list all packages needed loaded for using the script.

library("tidyverse") # recall that we use this package to get the dplyr language 
library("readr") # for write_csv and read_csv functions
library("stargazer") # this is a handy package that produces regression tables and more
library("foreach") # for %do% function
library("data.table") # package for handling large datasets 
library("magrittr") # for %<>% operator
library("zoo") # for na.approx function

# updates log ----
# 5/3: 

#2020 dollars
#1990, 2000, 05-09 for 2007 (2009), 08-12 for 2010 (2012), 11-15 for 2013 (2015), 15-19 for 2017 (2019)
cpi <- c(2.13, 1.57, 1.22, 1.14, 1.1, 1.02) 
names(cpi) <- c("1990", "2000", "2007", "2010", "2013", "2017")

# 2000 block-level data ####
blocks2000 <- fread(file = "ipumsblocks_allstates/2000blocks/nhgis0032_ds147_2000_block.csv")
blocks2000 <- blocks2000[-1,]

# 1. convert factorized numerical variables back to numerical, but the file is very large, so we need 
# to split it into manageable chunks (stored in a list object) and run the analysis on each chunk separately. 
# The data is so large in part because there are >8 mil. blocks and 80 variables. However, we don't need 
# each variable. You can look at the codebook for the data too to get a sense of why we don't need each variable 
# in the analytical sample, but we DO need most of the variables to construct actual variables of use.

thesecolumns <- c(14:76) # these are the column indexes for variables that need to be converted from char to num.

f <- rep(seq_len(ceiling(nrow(blocks2000) / 100000)), each = 100000, length.out = nrow(blocks2000))
dat_use <- split(blocks2000, f = f)
rm(blocks2000)
rm(f)

# here's where we do processing on each chunk of the data 
foreach (i = 1:length(dat_use)) %do% {
  dat_use[[i]] <- dat_use[[i]] %>%
    mutate_at(thesecolumns, ~as.numeric(as.character(.))) %>% # mutate_at is a version of mutate that allows us to 
    #operate on multipe variables at the same time. 
      mutate( 
             pop00b = rowSums(across(14:27), na.rm = T),
             nhblack00b = FYF002,
             nhwhite00b = FYF001, 
             h00b = rowSums(across(c(FYF008:FYF014)), na.rm = T),
             min00b = rowSums(across(15:27), na.rm = T),
             pctnhblack00b = (nhblack00b/pop00b)*100,
             pctnhwhite00b = (nhwhite00b/pop00b)*100, 
             pcth00b = (h00b/pop00b)*100, 
             pctmin00b = (min00b/pop00b)*100, 
             hu00b = FV5001,
             dependants00b = rowSums(across(c(28:31, 51:54, 45:50, 68:73), na.rm = T)),
             workingage00b = rowSums(across(c(32:44, 55:67), na.rm = T)), 
             dependencyratio00b = dependants00b/workingage00b,
             owneroccupied00b = FWA001,
             vacancy00b = (FV5001 - (FWA001 + FWA002)),
             blkid = paste0(str_pad(STATEA, 2, side = "left", pad = "0"), str_pad(COUNTYA, 3, side = "left", pad = "0"),
                            str_pad(TRACTA, 6, side = "left", pad = "0"), str_pad(BLOCKA, 4, side = "left", pad = "0"))) %>%
    dplyr::select(STATEA, COUNTYA, TRACTA, BLOCKA, PLACEA, blkid, pop00b:vacancy00b, -dependants00b, -workingage00b)
  return(NULL)
}

# add in vap data
vap2000block <- read_csv("ipumsblocks_allstates/2000blocks/vap2000_block.csv")

vap2000block <- vap2000block %>%
  mutate(vap00b = FX4001,
         hispvap00b = FX9001,
         nhwvap00b = FYB001,
         nhbvap00b = FYB002,
         nativevap00b = FYB003,
         asianvap00b = FYB004 + FYB005,
         othervap00b = FYB006 + FYC001) %>%
  dplyr::select(STATEA, COUNTYA, TRACTA, BLOCKA, vap00b:othervap00b) %>%
  mutate(pcthispvap00b = (hispvap00b/vap00b)*100,
         pctnhwvap00b = (nhwvap00b/vap00b)*100,
         pctnhbvap00b = (nhbvap00b/vap00b)*100,
         pctasianvap00b = (asianvap00b/vap00b)*100,
         pctnativevap00b = (nativevap00b/vap00b)*100,
         pctothervap00b = (othervap00b/vap00b)*100,
         blkid = paste0(str_pad(STATEA, 2, side = "left", pad = "0"), str_pad(COUNTYA, 3, side = "left", pad = "0"),
                        str_pad(TRACTA, 6, side = "left", pad = "0"), sprintf("%04.0f", BLOCKA))) %>%
  dplyr::select(blkid, vap00b:pctothervap00b)
# by selecting the variables we need, we significantly reduce the file size of the data. 

# turn list into dataframe 
blocks2000 <- rbindlist(dat_use, use.names = TRUE)
rm(dat_use)

# here we save the new file we've made
blocks2000 <- blocks2000 %>%
  left_join(vap2000block, by = "blkid")
write_csv(blocks2000, "blocks2000_var.csv")

rm(list = ls())

# 2010 block-level data ####
clean_dat_10 <- function(datuse) {
    f <- rep(seq_len(ceiling(nrow(datuse) / 100000)), each = 100000, length.out = nrow(datuse))
    dat_use <- split(datuse, f = f)
    rm(datuse)
    rm(f)
    
    foreach (i = 1:length(dat_use)) %do% {
        dat_use[[i]] <- dat_use[[i]] %>%
            mutate( 
                pop10b = H7Z001,
                nhblack10b = H75006,
                nhwhite10b = H7Z003, 
                h10b = H7Z010,
                min10b = (pop10b-nhwhite10b),
                pctnhblack10b = (nhblack10b/pop10b)*100,
                pctnhwhite10b = (nhwhite10b/pop10b)*100, 
                pcth10b = (h10b/pop10b)*100, 
                pctmin10b = (min10b/pop10b)*100, 
                dependants10b = rowSums(across(c(H76003:H76006, H76020:H76025, H76027:H76030, H76044:H76049), na.rm = T)),
                workingage10b = rowSums(across(c(H76007:H76019, H76031:H76043), na.rm = T)), 
                dependencyratio10b = dependants10b/workingage10b,
                owneroccupied10b = (IFF002+IFF003),
                vacancy10b = (IFC001-IFF001),
                hu10b = IFC001,
                urbunits10b = (IFD002/IFD001)*100,
                vap10b = H75001,
                nhwvap10b = H75005,
                nhbvap10b = H75006,
                hispvap10b = H75002,
                nativevap10b = H75007,
                asianvap10b = H75008 + H75009,
                othervap10b = H75010 + H75011,
                pcthispvap10b = (H75002/H75001)*100,
                pctnhwvap10b = (H75005/H75001)*100,
                pctnhbvap10b = (H75006/H75001)*100,
                pctnativevap10b = (nativevap10b/vap10b)*100,
                pctasianvap10b = (asianvap10b/vap10b)*100,
                pctothervap10b = (othervap10b/vap10b)*100
            ) %>% 
            dplyr::select( # select can take a vector of column indexes c(number 1, number 2, number 3:number 7 etc.) or column names
                STATEA, COUNTYA, TRACTA, BLOCKA, PLACEA, pop10b:pctothervap10b)
        return(NULL)
    }
    
    blocks2010 <- rbindlist(dat_use, use.names = TRUE)
    rm(dat_use)
    return(blocks2010)
}

blocks2010 <- fread(file = "ipumsblocks_allstates/2010blocks/nhgis0036_ds172_2010_block.csv") 
blocks2010 %<>%
    select(c("STATEA", "COUNTYA", "TRACTA", "BLOCKA", "PLACEA", "H7W001":"IFF004"))
blocks2010 <- blocks2010[c(1:2000000), ]

blocks_clean <- clean_dat_10(blocks2010)
write_csv(blocks_clean, "blocks2010_var_pt1.csv")
rm(blocks_clean)

blocks2010 <- fread(file = "ipumsblocks_allstates/2010blocks/nhgis0036_ds172_2010_block.csv") 
blocks2010 %<>%
    select(c("STATEA", "COUNTYA", "TRACTA", "BLOCKA", "PLACEA", "H7W001":"IFF004"))
blocks2010 <- blocks2010[c(2000001:5000000), ]
blocks_clean <- clean_dat_10(blocks2010)
write_csv(blocks_clean, "blocks2010_var_pt2.csv")
rm(blocks_clean)

blocks2010 <- fread(file = "ipumsblocks_allstates/2010blocks/nhgis0036_ds172_2010_block.csv") 
blocks2010 %<>%
    select(c("STATEA", "COUNTYA", "TRACTA", "BLOCKA", "PLACEA", "H7W001":"IFF004"))
blocks2010 <- blocks2010[c(5000001:10000000), ]
blocks_clean <- clean_dat_10(blocks2010)
write_csv(blocks_clean, "blocks2010_var_pt3.csv")
rm(blocks_clean)

blocks2010 <- fread(file = "ipumsblocks_allstates/2010blocks/nhgis0036_ds172_2010_block.csv") 
blocks2010 %<>%
    select(c("STATEA", "COUNTYA", "TRACTA", "BLOCKA", "PLACEA", "H7W001":"IFF004"))
blocks2010 <- blocks2010[c(10000001:nrow(blocks2010)), ]
blocks_clean <- clean_dat_10(blocks2010)
rm(blocks2010)

pt1 <- read_csv("blocks2010_var_pt1.csv")
pt2 <- read_csv("blocks2010_var_pt2.csv")
pt3 <- read_csv("blocks2010_var_pt3.csv")

blocks2010 <- base::rbind(pt1, pt2, pt3, blocks_clean)
rm(pt1, pt2, pt3, blocks_clean)
write_csv(blocks2010, "blocks2010_var.csv")

rm(list = ls())

# 2000 place-level data ####
# $3333 in 2002 LODES (2001) = $3140.75 (1999) = $37,689 --> start from 35K-39.999K
places2000 <- read_csv(file = "seplaces_allstates/2000places.csv") # notice that place-level data is much smaller

places2000 <- places2000 %>%
  mutate(STATE = str_pad(Geo_STATE, 2, side = "left", pad = "0"),
         PLACE = str_pad(Geo_PLACE, 5, side = "left", pad = "0"),
         plid = paste0(STATE, PLACE))

emp2000 <- read_csv("seplaces_allstates/emp/c2000.csv")

emp2000 %<>%
  mutate(STATE = str_pad(Geo_STATE, 2, side = "left", pad = "0"),
         PLACE = str_pad(Geo_PLACE, 5, side = "left", pad = "0"),
         plid = paste0(STATE, PLACE))

places2000 %<>%
  left_join(emp2000 %>% select(plid, SE_T145_001:SF3_PCT046095), by = "plid")

# 2. create variables needed
places2000 %<>%
  mutate(pop00p = SE_T001_001, 
         #pcturb00p = NA,
         #pctrur00p = NA,
         popdensity00p = SE_T003_003,
         nhblack00p = SE_T015_004, 
         nhwhite00p = SE_T015_003, 
         h00p = SE_T015_010, 
         asian00p = SE_T015_006 + SE_T015_007,
         native00p = SE_T015_007,
         other00p = SE_T015_008 + SE_T015_009, 
         pctnhblack00p = (nhblack00p/pop00p) * 100,
         pctnhwhite00p = (nhwhite00p/pop00p) * 100, 
         pcth00p = (h00p/pop00p) * 100, 
         pctasian00p = (asian00p/pop00p)*100,
         pctnative00p = (native00p/pop00p)*100,
         pctother00p = (other00p/pop00p) * 100, 
         pctrecimm00p = ((SE_T202_002 + SE_T202_003)/pop00p) * 100,
         emp00p = SE_T069_005,
         pctemp00p = (emp00p/SE_T069_002)*100,
         hu00p = SE_T155_001, 
         owneroccupied00p = SE_T156_002,
         vacancy00p = SE_T157_003,
         incomepp00p = SE_T145_001*cpi[["2000"]],
         overlodesthresh00p = rowSums(across(c(SF3_PCT046018:SF3_PCT046025, SF3_PCT046041:SF3_PCT046048, SF3_PCT046065:SF3_PCT046072, SF3_PCT046088:SF3_PCT046095))), 
         mhmval00p = SE_T163_001*cpi[["2000"]],
         hinc00p = SE_T093_001 * cpi[["2000"]], 
         whitepov00p = (SE_T194_002/SE_T194_001)*100,
         blackpov00p = (SE_T187_002/SE_T187_001)*100,
         hpov00p = (SE_T193_002/SE_T193_001)*100,
         asianpov00p = ((SE_T189_002 + SE_T190_002)/(SE_T189_001 + SE_T190_001))*100,
         nativepov00p = (SE_T188_002/SE_T188_001)*100,
         otherpov00p =  ((SE_T191_002 + SE_T192_002)/(SE_T191_001 + SE_T192_001))*100,
         vap00p = SF1_P006001,
         nhwhitevap00p = SF1_P006005,
         pctnhwhitevap00p = (nhwhitevap00p/vap00p)*100,
         nhblackvap00p = SF1_P006006,
         pctnhblackvap00p = (nhblackvap00p/vap00p)*100,
         hispvap00p = SF1_P006002,
         pcthispvap00p = (hispvap00p/vap00p)*100,
         asianvap00p = SF1_P006008 + SF1_P006009,
         pctasianvap00p = (asianvap00p/vap00p)*100,
         nativevap00p = SF1_P006007,
         pctnativevap00p = (nativevap00p/vap00p)*100,
         othervap00p = rowSums(across(c(SF1_P006010, SF1_P006011))),
         pctothervap00p = (othervap00p/vap00p)*100
  ) %>%
  select(plid, Geo_NAME, pop00p:pctothervap00p)

write_csv(places2000, "pl2000_cleaned.csv")
rm(places2000)

# 1990 places ####
# cpi to 2016$, which is ACS$
# 1990(1989)
places1990_1 <- read_csv(file = "seplaces_allstates/nhgis_1990/nhgis0046_ds120_1990_place.csv")
places1990_2 <- read_csv(file = "seplaces_allstates/nhgis_1990/nhgis0046_ds123_1990_place.csv")

places1990_1 %<>%
  mutate(STATE = str_pad(STATEA, 2, side = "left", pad = "0"),
         PLACE = str_pad(PLACEA, 5, side = "left", pad = "0"),
         plid = paste0(STATE, PLACE))

places1990_2 %<>%
  mutate(STATE = str_pad(STATEA, 2, side = "left", pad = "0"),
         PLACE = str_pad(PLACEA, 5, side = "left", pad = "0"),
         plid = paste0(STATE, PLACE))

places1990 <- places1990_1 %>%
  left_join(places1990_2 %>% select(plid, E3F001:E09070))
rm(places1990_1, places1990_2)

# 2. create variables needed
places1990 %<>%
  mutate(pop90p = ET1001, 
         #pcturb90p = NA,
         #pctrur90p = NA,
         popdensity90p = NA,
         nhblack90p = ET2002, 
         nhwhite90p = ET2001, 
         h90p = rowSums(across(c(ET2006:ET2010))), 
         asian90p = ET2004,
         native90p = ET2003,
         other90p = ET2005, 
         pctnhblack90p = (nhblack90p/pop90p) * 100,
         pctnhwhite90p = (nhwhite90p/pop90p) * 100, 
         pcth90p = (h90p/pop90p) * 100, 
         pctasian90p = (asian90p/pop90p)*100,
         pctnative90p = (native90p/pop90p)*100,
         pctother90p = (other90p/pop90p) * 100, 
         pctrecimm90p = (rowSums(across(c(E3F001:E3F004)))/pop90p)*100,
         emp90p = E4K002 + E4K006,
         hu90p = ESA001, 
         owneroccupied90p = ES1001,
         vacancy90p = ESN002,
         incomepp90p = E01001*cpi[["1990"]],
         overlodesthresh90p = NA, 
         mhmval90p = EST001*cpi[["1990"]],
         hinc90p = E4U001*cpi[["1990"]], 
         whitepov90p = ((rowSums(across(c(E09036:E09042))))/nhwhite90p)*100,
         blackpov90p = ((rowSums(across(c(E09043:E09049))))/nhblack90p)*100,
         hpov90p = NA, 
         asianpov90p = ((rowSums(across(c(E09057:E09063))))/asian90p)*100,
         nativepov90p = ((rowSums(across(c(E09050:E09056))))/native90p)*100,
         otherpov90p = ((rowSums(across(c(E09064:E09070))))/other90p)*100,
         vap90p = rowSums(across(c(ET4013:ET4031, ET4044:ET4062, ET4075:ET4093, ET4106:ET4124, ET4137:ET4155, ET4168:ET4186, ET4199:ET4217, ET4230:ET4248, ET4261:ET4279, ET4292:ET4310))),
         nhwhitevap90p = rowSums(across(c(ET4013:ET4031, ET4044:ET4062))),
         pctnhwhitevap90p = (nhwhitevap90p/vap90p)*100,
         nhblackvap90p = rowSums(across(c(ET4075:ET4093, ET4106:ET4124))),
         pctnhblackvap90p = (nhblackvap90p/vap90p)*100,
         hispvap90p = NA,
         pcthispvap90p = NA,
         asianvap90p = rowSums(across(c(ET4199:ET4217, ET4230:ET4248))),
         pctasianvap90p = (asianvap90p/vap90p)*100,
         nativevap90p = rowSums(across(c(ET4137:ET4155, ET4168:ET4186))),
         pctnativevap90p = (nativevap90p/vap90p)*100,
         othervap90p = rowSums(across(c(ET4261:ET4279, ET4292:ET4310))),
         pctothervap90p = (othervap90p/vap90p)*100
  ) %>%
  select(plid, pop90p:pctothervap90p)

write_csv(places1990, "pl1990_cleaned.csv")

# merge for 1990-2000 ####
pl2000 <- read_csv("pl2000_cleaned.csv")
table(places1990$plid %in% pl2000$plid)

pl9000 <- left_join(
  places1990 %>% 
    filter(plid %in% pl2000$plid),
  pl2000,
  by = "plid"
) 

rm(places1990)

# make change variables
pl9000 %<>%
  mutate_at(all_of(c(2:86)),
            ~ifelse((is.na(.) | . == 0), 1, .)) %>%
  mutate(popgrowth = ((pop00p-pop90p)/pop90p) * 100,
         #densification = (popdensity00p - popdensity90p),
         nhwhitegrowth = ((nhwhite00p-nhwhite90p)/nhwhite90p) * 100,
         nhblackgrowth = ((nhblack00p-nhblack90p)/nhblack90p) * 100,
         hgrowth = ((h00p-h90p)/h90p) * 100,
         asiangrowth = ((asian00p-asian90p)/asian90p) * 100,
         nativegrowth = ((native00p-native90p)/native90p) * 100,
         othergrowth = ((other00p-other90p)/other90p) * 100,
         recimmgrowth = (pctrecimm00p - pctrecimm90p),
         incomeppgrowth = ((incomepp00p - incomepp90p)/incomepp00p)*100,
         incomegrowth = ((hinc00p - hinc90p)/hinc00p)*100,
         valgrowh = ((mhmval00p - mhmval90p)/mhmval90p)*100,
         hugrowth = ((hu00p - hu90p)/hu90p)*100,
         blackpovgrowth = (blackpov00p - blackpov90p),
         whitepovgrowth = (whitepov00p - whitepov90p),
         hpovgrowth = (hpov00p - hpov90p),
         asianpovgrowth = (asianpov00p - asianpov90p),
         otherpovgrowth = (otherpov00p - otherpov90p),
         nativepovgrowth = (nativepov00p - nativepov90p),
         nhwhitevapgrowth = ((nhwhitevap00p - nhwhitevap90p)/nhwhitevap90p)*100,
         nhblackvapgrowth = ((nhblackvap00p - nhblackvap90p)/nhblackvap90p)*100,
         hispvapgrowth = ((hispvap00p - hispvap90p)/hispvap90p)*100,
         nativevapgrowth = ((nativevap00p - nativevap90p)/nativevap90p)*100,
         asianvapgrowth = ((asianvap00p - asianvap90p)/asianvap90p)*100,
         othervapgrowth = ((othervap00p - othervap90p)/othervap90p)*100)

pl9000 %<>%
  mutate(vraa = case_when(
         (pctnhblackvap00p >= 20 & (pctnativevap00p >= 20 | pctasianvap00p >= 20 | pcthispvap00p >= 20 | pctothervap00p >= 20)) ~ "1",
         (pctnativevap00p >= 20 & (pctnhblackvap00p >= 20 | pctasianvap00p >= 20 | pcthispvap00p >= 20 | pctothervap00p >= 20)) ~ "1",
       (pctasianvap00p >= 20 & (pctnhblackvap00p >= 20 | pctnativevap00p >= 20 | pcthispvap00p >= 20 | pctothervap00p >= 20)) ~ "1",
       (pcthispvap00p >= 20 & (pctnhblackvap00p >= 20 | pctnativevap00p >= 20 | pctasianvap00p >= 20 | pctothervap00p >= 20)) ~ "1",
       (pctothervap00p >= 20 & (pctnhblackvap00p >= 20 | pctnativevap00p >= 20 | pctasianvap00p >= 20 | pcthispvap00p >= 20)) ~ "1",
       TRUE ~ "0")
  )
table(pl9000$vraa, exclude = NULL)

write_csv(pl9000, "pl9000_var.csv")
rm(pl2000, pl9000)

# 2007 ####
places2007 <- read_csv(file = "seplaces_allstates/2007places.csv")
vap2007 <- read_csv(file = "seplaces_allstates/vap/2007_vap.csv")
emp2007 <- read_csv("seplaces_allstates/emp/acs0509.csv")

places2007 %<>%
  mutate(STATE = str_pad(Geo_STATE, 2, side = "left", pad = "0"),
         PLACE = str_pad(Geo_PLACE, 5, side = "left", pad = "0"), 
         plid = paste0(STATE, PLACE))

vap2007 %<>%
  mutate(STATE = str_pad(Geo_STATE, 2, side = "left", pad = "0"),
         PLACE = str_pad(Geo_PLACE, 5, side = "left", pad = "0"), 
         plid = paste0(STATE, PLACE))

emp2007 %<>%
  mutate(STATE = str_pad(Geo_STATE, 2, side = "left", pad = "0"),
         PLACE = str_pad(Geo_PLACE, 5, side = "left", pad = "0"), 
         plid = paste0(STATE, PLACE))

places2007 %<>%
  left_join(vap2007 %>% select(plid, SE_T003_001:SE_T003_014), by = "plid") %>%
  left_join(emp2007 %>% select(plid, ACS09_5yr_B19301001:ACS09_5yr_B19325095s))

#3333 in 2007 LODES$ = 3493.99 in 2009$ = 41,927.88/yr --> use 40K threshold
rm(vap2007, emp2007)

# 2. make variables 
places2007 %<>%
  mutate(pop07p = SE_A00001_001, 
         #pcturb07p = NA,
         #pctrur07p = NA,
         popdensity07p = SE_A00002_002,
         nhblack07p = SE_A04001_004, 
         nhwhite07p = SE_A04001_003, 
         h07p = SE_A04001_010, 
         asian07p = SE_A04001_006 + SE_A04001_007, 
         native07p = SE_A04001_005,
         other07p = SE_A04001_008 + SE_A04001_009, 
         pctnhblack07p = (nhblack07p/pop07p) * 100,
         pctnhwhite07p = (nhwhite07p/pop07p) * 100, 
         pctasian07p = (asian07p/pop07p) * 100,
         pctnative07p = (native07p/pop07p) * 100, 
         pcth07p = (h07p/pop07p) * 100,
         pctother07p = (other07p/pop07p) * 100, 
         pctrecimm07p = (SE_A10058_002/pop07p) * 100,
         emp07p = SE_A17002_005,
         pctemp07p = (emp07p/SE_A17002_004)*100,
         incomepp07p = ACS09_5yr_B19301001*cpi[["2007"]],
         overlodesthresh07p = rowSums(across(c(ACS09_5yr_B19325019:ACS09_5yr_B19325025, ACS09_5yr_B19325042:ACS09_5yr_B19325048, ACS09_5yr_B19325066:ACS09_5yr_B19325072, ACS09_5yr_B19325089:ACS09_5yr_B19325095))),
         hu07p = SE_A10060_001,
         owneroccupied07p = SE_A10060_002,
         vacancy07p = SE_A10044_003,
         mhmval07p = SE_A10036_001*cpi[["2007"]],
         hinc07p = SE_A14006_001 * cpi[["2007"]], 
         whitepov07p = (SE_A13001I_002/SE_A13001I_001)*100,
         blackpov07p = (SE_A13001B_002/SE_A13001B_001)*100,
         hpov07p = (SE_A13001H_002/SE_A13001H_001)*100,
         asianpov07p = ((SE_A13001D_002 + SE_A13001E_002)/(SE_A13001D_001 + SE_A13001E_001))*100,
         nativepov07p = (SE_A13001C_002/SE_A13001C_001)*100, 
         otherpov07p = ((SE_A13001F_002 + SE_A13001G_002)/(SE_A13001F_001 + SE_A13001G_001))*100,
         vap07p = SE_T003_001,
         nhwhitevap07p = SE_T003_003,
         pctnhwhitevap07p = (nhwhitevap07p/vap07p)*100,
         nhblackvap07p = SE_T003_004,
         pctnhblackvap07p = (nhblackvap07p/vap07p)*100,
         hispvap07p = SE_T003_014,
         pcthispvap07p = (hispvap07p/vap07p)*100,
         asianvap07p = SE_T003_006 + SE_T003_007,
         pctasianvap07p = (asianvap07p/vap07p)*100,
         nativevap07p = SE_T003_005,
         pctnativevap07p = (nativevap07p/vap07p)*100,
         othervap07p = SE_T003_008,
         pctothervap07p = (othervap07p/vap07p)*100
  ) %>%
  select(Geo_NAME, plid, pop07p:pctothervap07p)

write_csv(places2007, "pl2007_cleaned.csv")

# merge for 2000-2007 ####
pl2000 <- read_csv("pl2000_cleaned.csv")

length(unique(pl2000$plid))
length(unique(places2007$plid))

table(unique(pl2000$plid) %in% unique(places2007$plid))

pl0007 <-
  left_join(
    pl2000 %>% filter(plid %in% places2007$plid),
    places2007 %>% select(-Geo_NAME),
    by = "plid")

pl0007 %<>%
  mutate_at(all_of(c(3:86)),
            ~ifelse((is.na(.) | . == 0), 1, .)) %>%
  mutate(popgrowth = ((pop07p-pop00p)/pop00p) * 100,
         densification = ifelse(popdensity07p > popdensity00p, 1, 0),
         nhwhitegrowth = ((nhwhite07p-nhwhite00p)/nhwhite00p) * 100,
         nhblackgrowth = ((nhblack07p-nhblack00p)/nhblack00p) * 100,
         hgrowth = ((h07p-h00p)/h00p) * 100,
         asiangrowth = ((asian07p-asian00p)/asian00p) * 100,
         nativegrowth = ((native07p-native00p)/native00p) * 100,
         othergrowth = ((other07p-other00p)/other00p) * 100,
         recimmgrowth = (pctrecimm07p - pctrecimm00p),
         incomeppgrowth = ((incomepp07p - incomepp00p)/incomepp07p)*100,
         incomegrowth = ((hinc07p - hinc00p)/hinc07p)*100,
         valgrowh = ((mhmval07p - mhmval00p)/mhmval00p)*100,
         hugrowth = ((hu07p - hu00p)/hu00p)*100,
         blackpovgrowth = (blackpov07p - blackpov00p),
         whitepovgrowth = (whitepov07p - whitepov00p),
         hpovgrowth = (hpov07p - hpov00p),
         asianpovgrowth = (asianpov07p - asianpov00p),
         otherpovgrowth = (otherpov07p - otherpov00p),
         nativepovgrowth = (nativepov07p - nativepov00p),
         nhwhitevapgrowth = ((nhwhitevap07p - nhwhitevap00p)/nhwhitevap00p)*100,
         nhblackvapgrowth = ((nhblackvap07p - nhblackvap00p)/nhblackvap00p)*100,
         hispvapgrowth = ((hispvap07p - hispvap00p)/hispvap00p)*100,
         nativevapgrowth = ((nativevap07p - nativevap00p)/nativevap00p)*100,
         asianvapgrowth = ((asianvap07p - asianvap00p)/asianvap00p)*100,
         othervapgrowth = ((othervap07p - othervap00p)/othervap00p)*100)

pl0007 %<>%
  mutate(vraa = case_when(
    (pctnhblackvap07p >= 20 & (pctnativevap07p >= 20 | pctasianvap07p >= 20 | pcthispvap07p >= 20 | pctothervap07p >= 20)) ~ "1",
    (pctnativevap07p >= 20 & (pctnhblackvap07p >= 20 | pctasianvap07p >= 20 | pcthispvap07p >= 20 | pctothervap07p >= 20)) ~ "1",
    (pctasianvap07p >= 20 & (pctnhblackvap07p >= 20 | pctnativevap07p >= 20 | pcthispvap07p >= 20 | pctothervap07p >= 20)) ~ "1",
    (pcthispvap07p >= 20 & (pctnhblackvap07p >= 20 | pctnativevap07p >= 20 | pctasianvap07p >= 20 | pctothervap07p >= 20)) ~ "1",
    (pctothervap07p >= 20 & (pctnhblackvap07p >= 20 | pctnativevap07p >= 20 | pctasianvap07p >= 20 | pcthispvap07p >= 20)) ~ "1",
    TRUE ~ "0")
  )
table(pl0007$vraa)

write_csv(pl0007, "pl0007_var.csv")
rm(pl0007, pl2000, places2007)

# 2010 places ####
# $3333 Lodes 2010 = 3486.48 in 2012 = 41837 --> 40K above threshold
places2010 <- read_csv(file = "seplaces_allstates/2010places.csv")
vap2010 <- read_csv(file = "seplaces_allstates/vap/2010_vap.csv")
emp2010 <- read_csv(file = "seplaces_allstates/emp/acs0812.csv")

places2010 <- places2010 %>%
  mutate(STATE = str_pad(Geo_STATE, 2, side = "left", pad = "0"),
         PLACE = str_pad(Geo_PLACE, 5, side = "left", pad = "0"), 
         plid = paste0(STATE, PLACE))

vap2010 <- vap2010 %>%
  mutate(STATE = str_pad(Geo_STATE, 2, side = "left", pad = "0"),
         PLACE = str_pad(Geo_PLACE, 5, side = "left", pad = "0"), 
         plid = paste0(STATE, PLACE))

emp2010 %<>%
  mutate(STATE = str_pad(Geo_STATE, 2, side = "left", pad = "0"),
         PLACE = str_pad(Geo_PLACE, 5, side = "left", pad = "0"), 
         plid = paste0(STATE, PLACE))

places2010 <- places2010 %>%
  left_join(vap2010 %>% select(plid, SF1_P0110001:SF1_P0110073), by = "plid") %>%
  left_join(emp2010 %>% select(plid, SE_A14024_001:ACS12_5yr_B20005095))

rm(vap2010, emp2010)

# 2. make variables 
places2010 %<>%
  mutate(pop10p = SE_A00001_001, 
         #pcturb10p = NA,
         #pctrur10p = NA,
         popdensity10p = SE_A00002_002,
         nhblack10p = SE_A04001_004, 
         nhwhite10p = SE_A04001_003, 
         h10p = SE_A04001_010, 
         asian10p = SE_A04001_006 + SE_A04001_007, 
         native10p = SE_A04001_005, 
         other10p = SE_A04001_008 + SE_A04001_009,
         pctnhblack10p = (nhblack10p/pop10p) * 100,
         pctnhwhite10p = (nhwhite10p/pop10p) * 100, 
         pcth10p = (h10p/pop10p) * 100, 
         pctasian10p = (asian10p/pop10p)*100, 
         pctnative10p = (native10p/pop10p)*100, 
         pctother10p = (other10p/pop10p)*100,
         pctrecimm10p = (SE_A10058_003/pop10p) * 100,
         emp10p = SE_A17002_005,
         pctemp10p = (emp10p/SE_A17002_002)*100,
         hu10p = SE_A10060_001, 
         owneroccupied10p = SE_A10060_002,
         vacancy10p = SE_A10060_003,
         incomepp10p = SE_A14024_001*cpi[["2010"]],
         overlodesthresh10p = rowSums(across(c(ACS12_5yr_B20005019:ACS12_5yr_B20005025, ACS12_5yr_B20005042:ACS12_5yr_B20005048, ACS12_5yr_B20005066:ACS12_5yr_B20005072, ACS12_5yr_B20005089:ACS12_5yr_B20005095))), 
         mhmval10p = SE_A10036_001*cpi[["2010"]],
         hinc10p = SE_A14006_001 * cpi[["2010"]], 
         whitepov10p = (SE_A13001I_002/SE_A13001I_001)*100,
         blackpov10p = (SE_A13001B_002/SE_A13001B_001)*100,
         hpov10p = (SE_A13001H_002/SE_A13001H_001)*100,
         asianpov10p = ((SE_A13001D_002 + SE_A13001E_002)/(SE_A13001D_001 + SE_A13001E_001))*100, 
         nativepov10p = (SE_A13001C_002/SE_A13001C_001)*100, 
         otherpov10p = ((SE_A13001F_002 + SE_A13001G_002)/(SE_A13001F_001 + SE_A13001G_002))*100, 
         vap10p = SF1_P0110001,
         nhwhitevap10p = SF1_P0110005,
         pctnhwhitevap10p = (nhwhitevap10p/vap10p)*100,
         nhblackvap10p = SF1_P0110006,
         pctnhblackvap10p = (nhblackvap10p/vap10p)*100,
         hispvap10p = SF1_P0110002,
         pcthispvap10p = (hispvap10p/vap10p)*100,
         asianvap10p = SF1_P0110008 + SF1_P0110009,
         pctasianvap10p = (asianvap10p/vap10p)*100,
         nativevap10p = SF1_P0110009,
         pctnativevap10p = (nativevap10p/vap10p)*100,
         othervap10p = rowSums(across(c(SF1_P0110010:SF1_P0110011, SF1_P0110028, SF1_P0110049, SF1_P0110065, SF1_P0110072))),
         pctothervap10p = (othervap10p/vap10p)*100
         ) %>%
  select(plid, Geo_NAME, pop10p:pctothervap10p)

write_csv(places2010, "pl2010_cleaned.csv")

# 2013 ACS places ####
vapacs13 <- read_csv("seplaces_allstates/vap/1115acsvap.csv") %>%
  mutate(plid = paste0(
    str_pad(Geo_STATE, 2, "left", "0"),
    str_pad(Geo_PLACE, 5, "left", "0")))

acs13 <- read_csv("seplaces_allstates/2013places.csv") %>%
  mutate(plid = paste0(
    str_pad(Geo_STATE, 2, "left", "0"),
    str_pad(Geo_PLACE, 5, "left", "0")))

acs13 %<>%
  left_join(vapacs13 %>% select(plid, SE_T003_001:SE_T003_014))

acs13 %<>%
    mutate(
      pop13p = SE_A04001_001,
      nhwhite13p = SE_A04001_003,
      pctnhwhite13p = (nhwhite13p/pop13p)*100,
      nhblack13p = SE_A04001_004,
      pctnhblack13p = (nhblack13p/pop13p)*100,
      h13p = SE_A04001_010,
      pcth13p = (h13p/pop13p)*100,
      asian13p = SE_A04001_006 + SE_A04001_007,
      pctasian13p = (asian13p/pop13p)*100,
      native13p = SE_A04001_005,
      pctnative13p = (native13p/pop13p)*100,
      other13p = SE_A04001_008 + SE_A04001_009,
      pctother13p = (other13p/pop13p)*100,
      vap13p = SE_T003_001,
      nhwhitevap13p = SE_T003_003,
      pctnhwhitevap13p = (nhwhitevap13p/vap13p)*100,
      nhblackvap13p = SE_T003_004,
      pctnhblackvap13p = (nhblackvap13p/vap13p)*100,
      hispvap13p = SE_T003_014,
      pcthispvap13p = (hispvap13p/vap13p)*100,
      asianvap13p = SE_T003_006 + SE_T003_007,
      pctasianvap13p = (asianvap13p/vap13p)*100,
      nativevap13p = SE_T003_005,
      pctnativevap13p = (nativevap13p/vap13p)*100,
      othervap13p = SE_T003_008,
      pctothervap13p = (othervap13p/vap13p)*100) %>%
  select(plid, Geo_NAME, pop13p:pctothervap13p)

write_csv(acs13, "acs13.csv")

places2007 <- read_csv("pl2007_cleaned.csv")

pl0713 <- left_join(
  places2007 %>% filter(plid %in% acs13$plid), 
  acs13 %>% select(-Geo_NAME),
  by = "plid"
)

pl0713 %<>%
  mutate_at(all_of(c(3:70)),
            ~ifelse((is.na(.) | . == 0), 1, .)) %>%
  mutate(popgrowth = ((pop13p-pop07p)/pop07p) * 100,
         #densification = ifelse(popdensity13p > popdensity07p, 1, 0),
         nhwhitegrowth = ((nhwhite13p-nhwhite07p)/nhwhite07p) * 100,
         nhblackgrowth = ((nhblack13p-nhblack07p)/nhblack07p) * 100,
         hgrowth = ((h13p-h07p)/h07p) * 100,
         asiangrowth = ((asian13p-asian07p)/asian07p) * 100,
         nativegrowth = ((native13p-native07p)/native07p) * 100,
         othergrowth = ((other13p-other07p)/other07p) * 100,
         #recimmgrowth = (pctrecimm13p - pctrecimm07p),
         #incomeppgrowth = ((incomepp13p - incomepp07p)/incomepp13p)*100,
         #incomegrowth = ((hinc13p - hinc07p)/hinc13p)*100,
         #valgrowh = ((mhmval13p - mhmval07p)/mhmval07p)*100,
         #hugrowth = ((hu13p - hu07p)/hu07p)*100,
         #blackpovgrowth = (blackpov13p - blackpov07p),
         #whitepovgrowth = (whitepov13p - whitepov07p),
         #hpovgrowth = (hpov13p - hpov07p),
         #asianpovgrowth = (asianpov13p - asianpov07p),
         #otherpovgrowth = (otherpov13p - otherpov07p),
         #nativepovgrowth = (nativepov13p - nativepov07p),
         #nhwhitevapgrowth = ((nhwhitevap13p - nhwhitevap07p)/nhwhitevap07p)*100,
         #nhblackvapgrowth = ((nhblackvap13p - nhblackvap07p)/nhblackvap07p)*100,
         hispvapgrowth = ((hispvap13p - hispvap07p)/hispvap07p)*100,
         nativevapgrowth = ((nativevap13p - nativevap07p)/nativevap07p)*100,
         asianvapgrowth = ((asianvap13p - asianvap07p)/asianvap07p)*100,
         othervapgrowth = ((othervap13p - othervap07p)/othervap07p)*100)

write_csv(pl0713, "pl0713_var.csv")
rm(acs13, pl0713, vapacs13)

# 2017 ####
# 3333 LODES $ is 3454.78 in 2019 = 41454 --> 40K threshold
# vap, emp, and hu 
places2017 <- read_csv("seplaces_allstates/2017places.csv")
race2017 <- read_csv("seplaces_allstates/vap/acs1519race.csv")
vap2017 <- read_csv("seplaces_allstates/vap/acs1519vap.csv")
emp2017 <- read_csv("seplaces_allstates/emp/acs1519.csv")
hu2017 <- read_csv("seplaces_allstates/hu/acs1519.csv")

places2017 %<>% 
    mutate(plid = paste0(str_pad(Geo_STATE, 2, "left", "0"),
                         str_pad(Geo_PLACE, 5, "left", "0"))) %>%
  select(-SE_A14024_001)

vap2017 %<>% 
  mutate(plid = paste0(str_pad(Geo_STATE, 2, "left", "0"),
                       str_pad(Geo_PLACE, 5, "left", "0")))

emp2017 %<>% 
  mutate(plid = paste0(str_pad(Geo_STATE, 2, "left", "0"),
                       str_pad(Geo_PLACE, 5, "left", "0")))

hu2017 %<>% 
  mutate(plid = paste0(str_pad(Geo_STATE, 2, "left", "0"),
                       str_pad(Geo_PLACE, 5, "left", "0")))

race2017 %<>% 
  mutate(plid = paste0(str_pad(Geo_STATE, 2, "left", "0"),
                       str_pad(Geo_PLACE, 5, "left", "0")))

places2017 %<>%
  left_join(emp2017 %>% select(plid, SE_A17002_001:ACS19_5yr_B20005095), by = "plid") %>%
  left_join(hu2017 %>% select(plid, SE_A10060_001:ACS19_5yr_B25004001), by = "plid") %>%
  left_join(vap2017 %>% select(plid, SE_T003_001:SE_T003_014), by = "plid") %>%
  left_join(race2017 %>% select(plid, SE_A04001_001:SE_A04001_017), by = "plid")

rm(emp2017, hu2017, vap2017, race2017)

places2017 %<>%
    mutate(pop17p = SE_A00001_001, 
           nhwhite17p = SE_A04001_003, 
           pctnhwhite17p = (nhwhite17p/pop17p)*100, 
           nhblack17p = SE_A04001_004,
           pctnhblack17p = (nhblack17p/pop17p)*100, 
           asian17p = SE_A04001_006 + SE_A04001_007, 
           pctasian17p = (asian17p/pop17p), 
           native17p = SE_A04001_005,
           pctnative17p = (native17p/pop17p), 
           other17p = SE_A04001_008 + SE_A04001_009, 
           pctother17p = (other17p/pop17p)*100, 
           h17p = SE_A04001_010, 
           pcth17p = (h17p/pop17p)*100,
           popdensity17p = SE_A00002_002, 
           hu17p = SE_A10060_001,
           owneroccupied17p = SE_A10060_002, 
           vacancy17p = ACS19_5yr_B25004001,
           incomepp17p = SE_A14024_001*cpi[["2017"]],
           emp17p = SE_A17002_005,
           pctemp17p = (emp17p/SE_A17002_004),
           overlodesthresh17p = rowSums(across(c(ACS19_5yr_B20005019:ACS19_5yr_B20005025, ACS19_5yr_B20005042:ACS19_5yr_B20005048, ACS19_5yr_B20005066:ACS19_5yr_B20005072, ACS19_5yr_B20005089:ACS19_5yr_B20005095))),
           pctrecimm17p = (SE_A10058_002/pop17p) * 100,
           incomepp17p = SE_A14024_001*cpi[["2017"]],
           mhmval17p = ACS19_5yr_B25077001*cpi[["2017"]],
           hinc17p = SE_A14006_001*cpi[["2017"]], 
           whitepov17p = (SE_A13001I_002/SE_A13001I_001)*100,
           blackpov17p = (SE_A13001B_002/SE_A13001B_001)*100,
           hpov17p = (SE_A13001H_002/SE_A13001H_001)*100,
           asianpov17p = ((SE_A13001D_002 + SE_A13001E_002)/(SE_A13001D_001 + SE_A13001E_001))*100,
           nativepov17p = (SE_A13001C_002/SE_A13001C_001)*100, 
           otherpov17p = ((SE_A13001F_002 + SE_A13001G_002)/(SE_A13001F_001 + SE_A13001G_001))*100,
           vap17p = SE_T003_001,
           nhwhitevap17p = SE_T003_003,
           pctnhwhitevap17p = (nhwhitevap17p/vap17p)*100,
           nhblackvap17p = SE_T003_004,
           pctnhblackvap17p = (nhblackvap17p/vap17p)*100,
           hispvap17p = SE_T003_014,
           pcthispvap17p = (hispvap17p/vap17p)*100,
           nativevap17p = SE_T003_005,
           pctnativevap17p = (nativevap17p/vap17p)*100,
           asianvap17p = (SE_T003_006 + SE_T003_007),
           pctasianvap17p = (asianvap17p/vap17p)*100,
           othervap17p = SE_T003_008,
           pctothervap17p = (othervap17p/vap17p)*100) %>%
    select(c(plid, Geo_NAME, contains("17p")))

write_csv(places2017, "places2017_cleaned.csv")

# generate 2014 by interpolation ####
names(places2017) <- gsub("17p", "", names(places2017))
names(places2010) <- gsub("10p", "", names(places2010))

commonvars <- Reduce(intersect, list(names(places2010), names(places2017))) # find only blocks common to each other 

places2017 %<>%
  select(all_of(commonvars)) 
places2010 %<>%
  select(all_of(commonvars))
places2014 <- places2017 

places2010$Year <- 2010
places2014$Year <- 2014
places2017$Year <- 2017

places <- base::rbind(places2010, places2014, places2017) %>%
  mutate_at(all_of(commonvars[3:45]), ~ifelse(is.na(.), 0, .))

places %<>%
  group_by(plid) %>%
  arrange(Year) %>%
  mutate_at(all_of(commonvars[3:45]), ~na.approx(., na.rm = F))

places2014 <- places %>%
  filter(Year == 2014)
names(places2014)[3:45] <- str_c(names(places2014)[3:45], "14p")

write_csv(places2014, "places2014_cleaned.csv")

#merge for 2007-2014 ####
places2007 <- read_csv("pl2007_cleaned.csv")
places2014 <- read_csv("pl2014_cleaned.csv")

pl0714 <- 
  left_join(
    places2007 %>% filter(plid %in% places2014$plid), 
    places2014 %>% select(-Geo_NAME), 
    by = "plid")

pl0714 %<>%
  mutate_at(all_of(c(3:86)),
            ~ifelse((is.na(.) | . == 0), 1, .)) %>%
  mutate(popgrowth = ((pop14p-pop07p)/pop07p) * 100,
         densification = ifelse(popdensity14p > popdensity07p, 1, 0),
         nhwhitegrowth = ((nhwhite14p-nhwhite07p)/nhwhite07p) * 100,
         nhblackgrowth = ((nhblack14p-nhblack07p)/nhblack07p) * 100,
         hgrowth = ((h14p-h07p)/h07p) * 100,
         asiangrowth = ((asian14p-asian07p)/asian07p) * 100,
         nativegrowth = ((native14p-native07p)/native07p) * 100,
         othergrowth = ((other14p-other07p)/other07p) * 100,
         recimmgrowth = (pctrecimm14p - pctrecimm07p),
         incomeppgrowth = ((incomepp14p - incomepp07p)/incomepp14p)*100,
         incomegrowth = ((hinc14p - hinc07p)/hinc14p)*100,
         valgrowh = ((mhmval14p - mhmval07p)/mhmval07p)*100,
         hugrowth = ((hu14p - hu07p)/hu07p)*100,
         blackpovgrowth = (blackpov14p - blackpov07p),
         whitepovgrowth = (whitepov14p - whitepov07p),
         hpovgrowth = (hpov14p - hpov07p),
         asianpovgrowth = (asianpov14p - asianpov07p),
         otherpovgrowth = (otherpov14p - otherpov07p),
         nativepovgrowth = (nativepov14p - nativepov07p),
         nhwhitevapgrowth = ((nhwhitevap14p - nhwhitevap07p)/nhwhitevap07p)*100,
         nhblackvapgrowth = ((nhblackvap14p - nhblackvap07p)/nhblackvap07p)*100,
         hispvapgrowth = ((hispvap14p - hispvap07p)/hispvap07p)*100,
         nativevapgrowth = ((nativevap14p - nativevap07p)/nativevap07p)*100,
         asianvapgrowth = ((asianvap14p - asianvap07p)/asianvap07p)*100,
         othervapgrowth = ((othervap14p - othervap07p)/othervap07p)*100)

pl0714 %<>%
  mutate(vraa = case_when(
    (pctnhblackvap14p >= 20 & (pctnativevap14p >= 20 | pctasianvap14p >= 20 | pcthispvap14p >= 20 | pctothervap14p >= 20)) ~ "1",
    (pctnativevap14p >= 20 & (pctnhblackvap14p >= 20 | pctasianvap14p >= 20 | pcthispvap14p >= 20 | pctothervap14p >= 20)) ~ "1",
    (pctasianvap14p >= 20 & (pctnhblackvap14p >= 20 | pctnativevap14p >= 20 | pcthispvap14p >= 20 | pctothervap14p >= 20)) ~ "1",
    (pcthispvap14p >= 20 & (pctnhblackvap14p >= 20 | pctnativevap14p >= 20 | pctasianvap14p >= 20 | pctothervap14p >= 20)) ~ "1",
    (pctothervap14p >= 20 & (pctnhblackvap14p >= 20 | pctnativevap14p >= 20 | pctasianvap14p >= 20 | pcthispvap14p >= 20)) ~ "1",
    TRUE ~ "0"))

table(pl0714$vraa)
write_csv(pl0714, "pl0714_var.csv")

rm(places2007, pl0710)

# 2020 places ####
places2020 <- read_csv("seplaces_allstates/2020places.csv") 

places2020 %<>% 
  mutate(plid = paste0(str_pad(Geo_STATE, 2, "left", "0"),
                       str_pad(Geo_PLACE, 5, "left", "0")),
         pop20p = SE_T003_001,
         nhwhite20p = SE_T004_003, 
         pctnhwhite20p = (nhwhite20p/pop20p)*100, 
         nhblack20p = SE_T004_005,
         pctnhblack20p = (nhblack20p/pop20p)*100, 
         asian20p = SE_T004_009 + SE_T004_011, 
         pctasian20p = (asian20p/pop20p), 
         native20p = SE_T004_007,
         pctnative20p = (native20p/pop20p), 
         other20p = SE_T004_013 + SE_T004_015, 
         pctother20p = (other20p/pop20p)*100, 
         h20p = SE_T004_017, 
         pcth20p = (h20p/pop20p)*100,
         popdensity20p = SE_T001_002,
         vap20p = SE_T011_001,
         nhwhitevap20p = SE_T011_003,
         pctnhwhitevap20p = (nhwhitevap20p/vap20p)*100,
         nhblackvap20p = SE_T011_004,
         pctnhblackvap20p = (nhblackvap20p/vap20p)*100,
         hispvap20p = SE_T011_010,
         pcthispvap20p = (hispvap20p/vap20p)*100,
         nativevap20p = SE_T011_005,
         pctnativevap20p = (nativevap20p/vap20p)*100,
         asianvap20p = (SE_T011_006 + SE_T011_007),
         pctasianvap20p = (asianvap20p/vap20p)*100,
         othervap20p = SE_T011_008 + SE_T011_009,
         pctothervap20p = (othervap20p/vap20p)*100) %>%
  select(c(plid, Geo_NAME, contains("20p")))

write_csv(places2020, "places2020_cleaned.csv")

rm(list = ls())

# make interpolated block data #### 
blocks2020 <- fread(file = "ipumsblocks_allstates/2020blocks/nhgis0031_ds248_2020_block.csv") 
blocks2020 %<>%
    select(c("STATEA", "COUNTYA", "TRACTA", "BLOCKA", "PLACEA", "U7C001":"U7G003"))
f <- rep(seq_len(ceiling(nrow(blocks2020) / 100000)), each = 100000, length.out = nrow(blocks2020))
dat_use <- split(blocks2020, f = f)
rm(blocks2020)
rm(f)

# here's where we do processing on each chunk of the data 
foreach (i = 1:length(dat_use)) %do% {
    dat_use[[i]] <- dat_use[[i]] %>%
        mutate( 
            pop20b = U7C001,
            nhblack20b = U7C006,
            nhwhite20b = U7C005, 
            h20b = U7C002,
            min20b = (pop20b-nhwhite20b),
            pctnhblack20b = (nhblack20b/pop20b)*100,
            pctnhwhite20b = (nhwhite20b/pop20b)*100, 
            pcth20b = (h20b/pop20b)*100, 
            pctmin20b = (min20b/pop20b)*100, 
            vap20b = U7E001,
            hispvap20b = U7E002,
            pcthispvap20b = (U7E002/U7E001)*100,
            nhwvap20b = U7E005,
            pctnhwvap20b = (U7E005/U7E001)*100,
            nhbvap20b = U7E006,
            pctnhbvap20b = (U7E006/U7E001)*100,
            asianvap20b = U7C008 + U7C009,
            pctasianvap20b = (asianvap20b/vap20b)*100,
            nativevap20b = U7C007,
            pctnativevap20b = (nativevap20b/vap20b)*100,
            othervap20b =  U7C010 + U7C011,
            pctothervap20b = (othervap20b/vap20b)*100,
            hu20b = U7G001,
            owneroccuped20b = U7G002,
            vacancy20b = U7G003
        ) %>% 
        dplyr::select( # select can take a vector of column indexes c(number 1, number 2, number 3:number 7 etc.) or column names
            STATEA, COUNTYA, TRACTA, BLOCKA, PLACEA, pop20b:vacancy20b)
    return(NULL)
}

blocks2020 <- rbindlist(dat_use, use.names = TRUE)
rm(dat_use)
write_csv(blocks2020, "blocks2020_var.csv")
rm(list = ls())

blocks2020 <- read_csv("blocks2020_var.csv")
blocks2020 %<>%
    mutate(blkid = paste0(str_pad(STATEA, 2, side = "left", pad = "0"),
                          str_pad(COUNTYA, 3, side = "left", pad = "0"),
                          str_pad(TRACTA, 6, side = "left", pad = "0"),
                          str_pad(BLOCKA, 4, side = "left", pad = "0"))) %>%
    select(blkid, pop20b:vacancy20b) 

names(blocks2020) <- gsub("20b", "", names(blocks2020))
blocks2020 %<>%
    mutate(Year = 2020)

blocks2000 <- read_csv("blocks2000_var.csv")
names(blocks2000) <- gsub("00b", "", names(blocks2000))
blocks2000 %<>%
    mutate(Year = "2000")

blocks2010 <- read_csv("blocks2010_var.csv")
blocks2010 %<>%
    mutate(blkid = paste0(str_pad(STATEA, 2, side = "left", pad = "0"),
                          str_pad(COUNTYA, 3, side = "left", pad = "0"),
                          str_pad(TRACTA, 6, side = "left", pad = "0"),
                          str_pad(BLOCKA, 4, side = "left", pad = "0")))

names(blocks2010) <- gsub("10b", "", names(blocks2010))
blocks2010 %<>%
    mutate(Year = 2010)

# blocks2000 %<>%
#     filter((blkid %in% blocks2010$blkid) & (blkid %in% blocks2020$blkid))
# 
# blocks2010 %<>%
#     filter(blkid %in% blocks2000$blkid)
# 
# blocks2020 %<>%
#     filter(blkid %in% blocks2000$blkid)

blkids <- Reduce(intersect, list(unique(blocks2000$blkid), unique(blocks2010$blkid), unique(blocks2020$blkid)))
names_list <- Reduce(intersect, list(names(blocks2010), names(blocks2020)))

blocks2000 %<>%
  filter(blkid %in% blkids) %>%
  select(all_of(names_list))

blocks2010 %<>%
  #filter(blkid %in% blkids) %>%
  select(all_of(names_list))

blocks2020 %<>%
  #filter(blkid %in% blkids) %>%
  select(all_of(names_list))

blocks <- base::rbind(blocks2010, blocks2020)
rm(blocks2000, blocks2020)

# make a version that turns NAs to 0s for interpolation
blocks %<>%
  mutate_at(all_of(names_list[2:25]), ~ifelse(is.na(.), 0, .))

# do 2014 
blocks2014 <- blocks2010 %>%
  mutate(Year = 2014)

blocks2014 %<>%
    mutate_at(all_of(names(blocks2014)[2:25]), ~NA) 

blocks2014 <- read_csv("blocks2014_int.csv")
blocks <- base::rbind(blocks, blocks2014)
rm(blocks2014)

blocks2017 <- blocks2010 
blocks2017 %<>%
  mutate_at(all_of(names(blocks2017)[2:25]), ~NA) %>%
  mutate(Year = 2017)

blocks <- base::rbind(blocks, blocks2017)

rm(blocks2010, blocks2017, blocks2014)
test <- blocks %>% filter(blkid %in% blkids[1:50])

test %<>%
    group_by(blkid) %>%
    arrange(Year) %>%
    mutate_at(all_of(names(blocks)[2:25]), zoo::na.approx, na.rm = F) %>%
    ungroup() 

rm(test)
rm(blkids)

blocks %<>%
  group_by(blkid) %>%
  arrange(Year) %>%
  mutate_at(all_of(names_list[2:25]), zoo::na.approx, na.rm = F) %>%
  ungroup() 

blocks14 <- blocks %>%
    filter(Year == "2014")
write_csv(blocks14, "blocks2014_int.csv")

blocks17 <- blocks %>%
  filter(Year == "2017")
write_csv(blocks17, "blocks2017_int.csv")

rm(list = ls())