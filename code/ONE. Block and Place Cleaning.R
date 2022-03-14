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

#2020 dollars
cpi <- c(1.19, 1.11, 1.08, 1.06, 1) #05-09 for 2007 (2009), 08-12 for 2010 (2012), 11-15 for 2013 (2015), 12-16 for 2014 (2016), 15-19 for 2017 (2019)

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

# OLD 2000 place-level data ####
# places2000 <- read_csv(file = "seplaces_allstates/2000places.csv") # notice that place-level data is much smaller
# 
# places2000 <- places2000 %>%
#   mutate(STATE = str_pad(Geo_STATE, 2, side = "left", pad = "0"),
#          PLACE = str_pad(Geo_PLACE, 5, side = "left", pad = "0"), 
#          plid = paste0(STATE, PLACE))
# 
# emp2000 <- read_csv("")
# # 2. create variables needed 
# places2000 %<>%
#   mutate(pop00p = SE_T001_001, 
#          #pcturb00p = (SE_T002_002/pop00p)*100, 
#          #pctrur00p = 100-pcturb00p,
#          popdensity00p = SE_T003_001,
#          nhblack00p = SE_T015_004, 
#          nhwhite00p = SE_T015_003, 
#          h00p = SE_T015_010, 
#          min00p = rowSums(places2000 %>% select(SE_T015_004:SE_T015_010)), 
#          pctnhblack00p = (nhblack00p/pop00p) * 100,
#          pctnhwhite00p = (nhwhite00p/pop00p) * 100, 
#          pcth00p = (h00p/pop00p) * 100, 
#          pctmin00p = (min00p/pop00p) * 100, 
#          pctrecimm00p = (SE_T202_002/pop00p) * 100,
#          unemp00p = (SE_T069_006/SE_T069_004)*100,
#          hinc00p = SE_T093_001 * cpi[1], 
#          pctowneroccupied00p = (SE_T156_002/SE_T156_001) * 100,
#          pctvacancy00p = (SE_T157_003/SE_T157_001) * 100,
#          mhmval00p = SE_T163_001*cpi[1],
#          blackpov00p = (SE_T187_002/SE_T187_001)*100,
#          hpov00p = (SE_T193_002/SE_T193_001)*100, 
#          nhwhitepov00p = (SE_T194_002/SE_T194_001)*100, 
#          minpov00p = ((SE_T187_002 + SE_T188_002 + SE_T189_002 + SE_T190_002 + SE_T191_002 + SE_T192_002 + SE_T193_002)/(SE_T187_001 + SE_T188_001 + SE_T189_001 + SE_T190_001 + SE_T191_001 + SE_T192_001 + SE_T193_001))*100,
#          vap00p = SF1_P006001,
#          nhwhitevap00p = SF1_P006005,
#          pctnhwhitevap00p = (nhwhitevap00p/vap00p)*100, #nh stands for non-Hispanic
#          nhblackvap00p = SF1_P006006,
#          nativevap00p = SF1_P006009,
#          pctnativevap00p = (nativevap00p/vap00p)*100,
#          asianvap00p = SF1_P006008 + SF1_P006009,
#          pctasianvap00p = (asianvap00p/vap00p)*100,
#          othervap00p = rowSums(across(c(SF1_P006010:SF1_P006011, SF1_P006028, SF1_P006049, SF1_P006065, SF1_P006072))),
#          pctothervap00p = (othervap00p/vap00p)*100,
#          pctnhblackvap00p = (nhblackvap00p/vap00p)*100,
#          hispvap00p = SF1_P006002,
#          pcthispvap00p = (hispvap00p/vap00p)*100) %>%
#   select(Geo_QName, plid, pop00p:pcthispvap00p)
# 
# write_csv(places2000, "pl2000_cleaned.csv")
# rm(places2000)
# 
# OLD 1990 places ####
# # cpi to 2016$, which is ACS$ 
# # 1990(1989)
# cpi <- c(1.93)
# places1990 <- read_csv(file = "seplaces_allstates/1990places.csv") # notice that place-level data is much smaller
# vap1990  <- read_csv(file = "seplaces_allstates/vap1990.csv")
# 
# places1990 %<>%
#   mutate(STATE = str_pad(Geo_STATE, 2, side = "left", pad = "0"),
#          PLACE = str_pad(Geo_PLACECE, 5, side = "left", pad = "0"), 
#          plid = paste0(STATE, PLACE))
# 
# vap1990 %<>%
#   mutate(STATE = str_pad(Geo_STATE, 2, side = "left", pad = "0"),
#          PLACE = str_pad(Geo_PLACECE, 5, side = "left", pad = "0"), 
#          plid = paste0(STATE, PLACE))
# 
# places1990 %<>%
#   left_join(vap1990 %>% select(plid, STF3_P014_001:STF3_P015_065), by = "plid")
# rm(vap1990)
# 
# # 2. create variables needed 
# places1990 %<>%
#   mutate(pop90p = SE_T001_001, 
#          #pcturb90p = (SE_T002_002/pop90p)*100, 
#          #pctrur90p = 100-pcturb90p,
#          popdensity90p = SE_T002_001,
#          nhblack90p = SE_T013_004, 
#          nhwhite90p = SE_T013_003, 
#          h90p = SE_T013_008, 
#          min90p = rowSums(across(SE_T013_004:SE_T013_008)), 
#          pctnhblack90p = (nhblack90p/pop90p) * 100,
#          pctnhwhite90p = (nhwhite90p/pop90p) * 100, 
#          pcth90p = (h90p/pop90p) * 100, 
#          pctmin90p = (min90p/pop90p) * 100, 
#          pctrecimm90p = (SE_T111_002/pop90p) * 100,
#          unemp90p = (SE_T029_003/SE_T029_001)*100,
#          hinc90p = SE_T043_001 * cpi[1], 
#          pctowneroccupied90p = (SE_T073_002/SE_T073_001) * 100,
#          pctvacancy90p = (SE_T074_003/SE_T074_001) * 100,
#          mhmval90p = SE_T080_001*cpi[1],
#          blackpov90p = (SE_T101_003/SE_T101_001)*100,
#          hpov90p = (SE_T105_003/SE_T105_003)*100, 
#          nhwhitepov90p = (SE_T100_003/SE_T100_001)*100, 
#          minpov90p = ((SE_T099_008 - SE_T099_009)/(SE_T099_001-SE_T099_002))*100,
#          vap90p = rowSums(across(c(STF3_P014_016:STF3_P014_034, STF3_P014_048:STF3_P014_066,
#                                                 STF3_P014_081:STF3_P014_099, STF3_P014_113:STF3_P014_131,
#                                                 STF3_P014_146:STF3_P014_164, STF3_P014_178:STF3_P014_196,
#                                                 STF3_P014_211:STF3_P014_229, STF3_P014_243:STF3_P014_261,
#                                                 STF3_P014_276:STF3_P014_294, STF3_P014_308:STF3_P014_326), na.rm = T)),
#          nhwhitevap90p = rowSums(across(c(STF3_P014_016:STF3_P014_034, STF3_P014_048:STF3_P014_066))),
#          nhblackvap90p = rowSums(across(c(STF3_P014_081:STF3_P014_099, STF3_P014_113:STF3_P014_131))),
#          nativevap90p = rowSums(across(c(STF3_P014_146:STF3_P014_164, STF3_P014_178:STF3_P014_196))),
#          asianvap90p = rowSums(across(c(STF3_P014_211:STF3_P014_229, STF3_P014_243:STF3_P014_261))),
#          othervap90p = rowSums(across(c(STF3_P014_276:STF3_P014_294, STF3_P014_308:STF3_P014_326))),
#          hispvap90p = rowSums(across(c(STF3_P015_015:STF3_P015_033, STF3_P015_047:STF3_P015_065))),
#          pctnhwhitevap90p = (nhwhitevap90p/vap90p)*100, #nh stands for non-Hispanic
#          pctnhblackvap90p = (nhblackvap90p/vap90p)*100,
#          pcthispvap90p = (hispvap90p/vap90p)*100,
#          pctnativevap90p = (nativevap90p/vap90p)*100,
#          pctasianvap90p = (asianvap90p/vap90p)*100,
#          pctothervap90p = (othervap90p/vap90p)) %>%
#   select(Geo_QName, plid, pop90p:pctothervap90p)
# 
# write_csv(places1990, "pl1990_cleaned.csv")
# 
# # merge for 1990-2000
# pl2000 <- read_csv("pl2000_cleaned.csv")
# table(places1990$plid %in% pl2000$plid)
# table(places1990$Geo_QName %in% pl2000$Geo_QName)
# 
# pl9000 <- left_join(
#   places1990 %>% rename(plid90 = plid),
#   pl2000,
#   by = "Geo_QName"
# )
# 
# rm(places1990)
# 
# # make change variables 
# pl9000 %<>% 
#   mutate_at(all_of(c("pop90p", "popdensity90p", "nhwhite90p", "nhblack90p", "h90p", "min90p",
#                      "hinc90p", "nhwhitevap90p", "nhblackvap90p", "hispvap90p", "nativevap90p", "asianvap90p", "othervap90p",
#                      "pop00p", "popdensity00p", "nhwhite00p", "nhblack00p", "h00p", "min00p",
#                      "hinc00p", "nhwhitevap00p", "nhblackvap00p", "hispvap00p", "nativevap00p", "asianvap00p", "othervap00p")), 
#             ~ifelse((is.na(.) | . == 0), 1, .)) %>%
#   mutate(popgrowth = ((pop00p-pop90p)/pop90p) * 100,
#          densification = (popdensity00p - popdensity90p),
#          nhwhitegrowth = ((nhwhite00p-nhwhite90p)/nhwhite90p) * 100, 
#          nhblackgrowth = ((nhblack00p-nhblack90p)/nhblack90p) * 100,
#          hgrowth = ((h00p-h90p)/h90p) * 100,
#          mingrowth = ((min00p-min90p)/min90p) * 100,
#          recimmgrowth = (pctrecimm00p - pctrecimm90p),
#          incomegrowth = ((hinc00p - hinc90p*cpi[1])/hinc00p)*100, 
#          blackpovgrowth = (blackpov00p - blackpov90p),
#          whitepovgrowth = (nhwhitepov00p - nhwhitepov90p),
#          hpovgrowth = (hpov00p - hpov90p),
#          minpovgrowth = (minpov00p - minpov90p), 
#          nhwhitevapgrowth = ((nhwhitevap00p - nhwhitevap90p)/nhwhitevap90p)*100,
#          nhblackvapgrowth = ((nhblackvap00p - nhblackvap90p)/nhblackvap90p)*100,
#          hispvapgrowth = ((hispvap00p - hispvap90p)/hispvap90p)*100,
#          nativevapgrowth = ((nativevap00p - nativevap90p)/nativevap90p)*100,
#          asianvapgrowth = ((asianvap00p - asianvap90p)/asianvap90p)*100,
#          othervapgrowth = ((othervap00p - othervap90p)/othervap90p)*100)
# 
# pl9000 %<>%
#   mutate(vraa = case_when(
#          (pctnhblackvap00p >= 20 & (pctnativevap00p >= 20 | pctasianvap00p >= 20 | pcthispvap00p >= 20 | pctothervap00p >= 20)) ~ "1",
#          (pctnativevap00p >= 20 & (pctnhblackvap00p | pctasianvap00p >= 20 | pcthispvap00p >= 20 | pctothervap00p >= 20)) ~ "1",
#        (pctasianvap00p >= 20 & (pctnhblackvap00p | pctnativevap00p >= 20 | pcthispvap00p >= 20 | pctothervap00p >= 20)) ~ "1",
#        (pcthispvap00p >= 20 & (pctnhblackvap00p | pctnativevap00p >= 20 | pctasianvap00p >= 20 | pctothervap00p >= 20)) ~ "1",
#        (pctothervap00p >= 20 & (pctnhblackvap00p | pctnativevap00p >= 20 | pctasianvap00p >= 20 | pcthispvap00p >= 20)) ~ "1",
#        TRUE ~ "0")
#   )
# 
# write_csv(pl9000, "pl9000_var.csv")

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
         pctnhblack07p = (nhblack07p/pop07p) * 100,
         pctnhwhite07p = (nhwhite07p/pop07p) * 100, 
         pctrecimm07p = (SE_A10058_002/pop07p) * 100,
         emp07p = SE_A17002_004,
         incomepp07p = ACS09_5yr_B19301001*cpi[1],
         overlodesthresh07p = rowSums(across(c(ACS09_5yr_B19325019:ACS09_5yr_B19325025, ACS09_5yr_B19325042:ACS09_5yr_B19325048, ACS09_5yr_B19325066:ACS09_5yr_B19325072, ACS09_5yr_B19325089:ACS09_5yr_B19325095))),
         hu07p = SE_A10060_001,
         owneroccupied07p = SE_A10060_002,
         vacancy07p = SE_A10044_003,
         mhmval07p = SE_A10036_001*cpi[1],
         hinc07p = SE_A14006_001 * cpi[1], 
         whitepov07p = (SE_A13001I_002/SE_A13001I_001)*100,
         blackpov07p = (SE_A13001B_002/SE_A13001B_001)*100,
         hpov07p = (SE_A13001H_002/SE_A13001H_001)*100,
         minpov07p =  ((hpov07p + blackpov07p + SE_A13001G_002 + SE_A13001F_002 + SE_A13001E_002 + SE_A13001D_002 + SE_A13001C_002)/(SE_A13001B_001 + SE_A13001B_001 + SE_A13001G_001 + SE_A13001F_001 + SE_A13001E_001 + SE_A13001D_001 + SE_A13001C_001))*100,
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
  select(plid, pop07p:pctothervap07p)

write_csv(places2007, "pl2007_cleaned.csv")

length(unique(pl2000$plid))
length(unique(places2007$plid))

table(unique(pl2000$plid) %in% unique(places2007$plid))

pl0007 <- 
  left_join(
    places2000, 
    places2007, 
    by = "plid")

pl0007 %<>%
  mutate_at(all_of(c("pop00p", "popdensity00p", "nhwhite00p", "nhblack00p", 
                     "hinc00p", "nhwhitevap00p", "nhblackvap00p", "hispvap00p", "asianvap00p", "nativevap00p", "othervap00p",
                     "pop07p", "popdensity07p", "nhwhite07p", "nhblack07p", 
                     "hinc07p", "nhwhitevap07p", "nhblackvap07p", "hispvap07p", "asianvap07p", "nativevap07p", "othervap07p")), 
            ~ifelse((is.na(.) | . == 0), 1, .)) %>%
  mutate(popgrowth = ((pop07p-pop00p)/pop00p) * 100,
         densification = (popdensity07p - popdensity00p),
         nhwhitegrowth = ((nhwhite07p-nhwhite00p)/nhwhite00p) * 100, 
         nhblackgrowth = ((nhblack07p-nhblack00p)/nhblack00p) * 100,
         recimmgrowth = (pctrecimm07p - pctrecimm00p),
         incomegrowth = ((hinc07p - hinc00p*cpi[1])/hinc07p)*100, 
         blackpovgrowth = (blackpov07p - blackpov00p),
         whitepovgrowth = (whitepov07p - nhwhitepov00p),
         hpovgrowth = (hpov07p - hpov00p),
         minpovgrowth = (minpov07p - minpov00p), 
         nhwhitevapgrowth = ((nhwhitevap07p - nhwhitevap00p)/nhwhitevap00p)*100,
         nhblackvapgrowth = ((nhblackvap07p - nhblackvap00p)/nhblackvap00p)*100,
         hispvapgrowth = ((hispvap07p - hispvap00p)/hispvap00p)*100,
         nativevapgrowth = ((nativevap07p - nativevap00p)/nativevap00p)*100,
         asianvapgrowth = ((asianvap07p - asianvap00p)/asianvap00p)*100,
         othervapgrowth = ((othervap07p - othervap00p)/othervap00p)*100)

pl0007 %<>%
  mutate(vraa = case_when(
    (pctnhblackvap07p >= 20 & (pctnativevap07p >= 20 | pctasianvap07p >= 20 | pcthispvap07p >= 20 | pctothervap07p >= 20)) ~ "1",
    (pctnativevap07p >= 20 & (pctnhblackvap07p | pctasianvap07p >= 20 | pcthispvap07p >= 20 | pctothervap07p >= 20)) ~ "1",
    (pctasianvap07p >= 20 & (pctnhblackvap07p | pctnativevap07p >= 20 | pcthispvap07p >= 20 | pctothervap07p >= 20)) ~ "1",
    (pcthispvap07p >= 20 & (pctnhblackvap07p | pctnativevap07p >= 20 | pctasianvap07p >= 20 | pctothervap07p >= 20)) ~ "1",
    (pctothervap07p >= 20 & (pctnhblackvap07p | pctnativevap07p >= 20 | pctasianvap07p >= 20 | pcthispvap07p >= 20)) ~ "1",
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
         min10p = (pop10p-nhwhite10p), 
         pctnhblack10p = (nhblack10p/pop10p) * 100,
         pctnhwhite10p = (nhwhite10p/pop10p) * 100, 
         pcth10p = (h10p/pop10p) * 100, 
         pctmin10p = (min10p/pop10p) * 100, 
         pctrecimm10p = (SE_A10058_003/pop10p) * 100,
         emp10p = SE_A17002_002,
         hu10p = SE_A10060_001, 
         owneroccupied10p = SE_A10060_002,
         vacancy10p = SE_A10060_003,
         incomepp10p = SE_A14024_001*cpi[2],
         overlodesthresh10p = rowSums(across(c(ACS12_5yr_B20005019:ACS12_5yr_B20005025, ACS12_5yr_B20005042:ACS12_5yr_B20005048, ACS12_5yr_B20005066:ACS12_5yr_B20005072, ACS12_5yr_B20005089:ACS12_5yr_B20005095))), 
         mhmval10p = SE_A10036_001*cpi[2],
         hinc10p = SE_A14006_001 * cpi[2], 
         whitepov10p = (SE_A13001I_002/SE_A13001I_001)*100,
         blackpov10p = (SE_A13001B_002/SE_A13001B_001)*100,
         hpov10p = (SE_A13001H_002/SE_A13001H_001)*100,
         minpov10p =  ((hpov10p + blackpov10p + SE_A13001G_002 + SE_A13001F_002 + SE_A13001E_002 + SE_A13001D_002 + SE_A13001C_002)/(SE_A13001B_001 + SE_A13001B_001 + SE_A13001G_001 + SE_A13001F_001 + SE_A13001E_001 + SE_A13001D_001 + SE_A13001C_001))*100,
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
  select(plid, pop10p:pctothervap10p)

write_csv(places2010, "pl2010_cleaned.csv")

length(unique(pl2000$plid))
length(unique(places2010$plid))

table(unique(pl2000$plid) %in% unique(places2010$plid))

pl0010 <- 
  left_join(
    places2000, 
    places2010, 
    by = "plid")

pl0010 %<>%
  mutate_at(all_of(c("pop00p", "popdensity00p", "nhwhite00p", "nhblack00p", "h00p", "min00p",
                     "hinc00p", "nhwhitevap00p", "nhblackvap00p", "hispvap00p", "asianvap00p", "nativevap00p", "othervap00p",
                     "pop10p", "popdensity10p", "nhwhite10p", "nhblack10p", "h10p", "min10p",
                     "hinc10p", "nhwhitevap10p", "nhblackvap10p", "hispvap10p", "asianvap10p", "nativevap10p", "othervap10p")), 
            ~ifelse((is.na(.) | . == 0), 1, .)) %>%
  mutate(popgrowth = ((pop10p-pop00p)/pop00p) * 100,
         densification = (popdensity10p - popdensity00p),
         nhwhitegrowth = ((nhwhite10p-nhwhite00p)/nhwhite00p) * 100, 
         nhblackgrowth = ((nhblack10p-nhblack00p)/nhblack00p) * 100,
         hgrowth = ((h10p-h00p)/h00p) * 100,
         mingrowth = ((min10p-min00p)/min00p) * 100,
         recimmgrowth = (pctrecimm10p - pctrecimm00p),
         incomegrowth = ((hinc10p[3] - hinc00p*cpi[1])/hinc00p[1])*100, 
         blackpovgrowth = (blackpov10p - blackpov00p),
         whitepovgrowth = (whitepov10p - nhwhitepov00p),
         hpovgrowth = (hpov10p - hpov00p),
         minpovgrowth = (minpov10p - minpov00p), 
         nhwhitevapgrowth = ((nhwhitevap10p - nhwhitevap00p)/nhwhitevap00p)*100,
         nhblackvapgrowth = ((nhblackvap10p - nhblackvap00p)/nhblackvap00p)*100,
         hispvapgrowth = ((hispvap10p - hispvap00p)/hispvap00p)*100,
         nativevapgrowth = ((nativevap10p - nativevap00p)/nativevap00p)*100,
         asianvapgrowth = ((asianvap10p - asianvap00p)/asianvap00p)*100,
         othervapgrowth = ((othervap10p - othervap00p)/othervap00p)*100)

pl0010 %<>%
  mutate(vraa = case_when(
    (pctnhblackvap10p >= 20 & (pctnativevap10p >= 20 | pctasianvap10p >= 20 | pcthispvap10p >= 20 | pctothervap10p >= 20)) ~ "1",
    (pctnativevap10p >= 20 & (pctnhblackvap10p | pctasianvap10p >= 20 | pcthispvap10p >= 20 | pctothervap10p >= 20)) ~ "1",
    (pctasianvap10p >= 20 & (pctnhblackvap10p | pctnativevap10p >= 20 | pcthispvap10p >= 20 | pctothervap10p >= 20)) ~ "1",
    (pcthispvap10p >= 20 & (pctnhblackvap10p | pctnativevap10p >= 20 | pctasianvap10p >= 20 | pctothervap10p >= 20)) ~ "1",
    (pctothervap10p >= 20 & (pctnhblackvap10p | pctnativevap10p >= 20 | pctasianvap10p >= 20 | pcthispvap10p >= 20)) ~ "1",
    TRUE ~ "0")
  )
table(pl0010$vraa)

write_csv(pl0010, "pl0010_var.csv")
#rm(list = ls())
rm(places2010, vap2010)

#0710 ####
places2007 <- read_csv("pl2007_cleaned.csv")
places2010 <- read_csv("pl2010_cleaned.csv")

table(unique(places2007$plid) %in% unique(places2010$plid))

pl0710 <- 
  left_join(
    places2007, 
    places2010, 
    by = "plid")

pl0710 %<>%
  mutate(vraa = case_when(
    (pctnhblackvap07p >= 20 & (pctnativevap07p >= 20 | pctasianvap07p >= 20 | pcthispvap07p >= 20 | pctothervap07p >= 20)) ~ "1",
    (pctnativevap07p >= 20 & (pctnhblackvap07p | pctasianvap07p >= 20 | pcthispvap07p >= 20 | pctothervap07p >= 20)) ~ "1",
    (pctasianvap07p >= 20 & (pctnhblackvap07p | pctnativevap07p >= 20 | pcthispvap07p >= 20 | pctothervap07p >= 20)) ~ "1",
    (pcthispvap07p >= 20 & (pctnhblackvap07p | pctnativevap07p >= 20 | pctasianvap07p >= 20 | pctothervap07p >= 20)) ~ "1",
    (pctothervap07p >= 20 & (pctnhblackvap07p | pctnativevap07p >= 20 | pctasianvap07p >= 20 | pcthispvap07p >= 20)) ~ "1",
    TRUE ~ "0"))

table(pl0710$vraa)
write_csv(pl0710, "pl0710_var.csv")

rm(places2007, pl0710)

# 2013 ACS places ####
# we just need vap info 
acs13 <- read_csv("seplaces_allstates/vap/1115acsvap.csv")

acs13 %<>%
    mutate(plid = paste0(
        str_pad(Geo_STATE, 2, "left", "0"),
        str_pad(Geo_PLACE, 5, "left", "0")),
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
    select(plid, vap13p:pctothervap13p)

write_csv(acs13, "acs13.csv")

pl1013 <- left_join(
  places2010, 
  acs13,
  by = "plid"
)

pl1013 %<>%
  mutate(vraa = case_when(
    (pctnhblackvap10p >= 20 & (pctnativevap10p >= 20 | pctasianvap10p >= 20 | pcthispvap10p >= 20 | pctothervap10p >= 20)) ~ "1",
    (pctnativevap10p >= 20 & (pctnhblackvap10p | pctasianvap10p >= 20 | pcthispvap10p >= 20 | pctothervap10p >= 20)) ~ "1",
    (pctasianvap10p >= 20 & (pctnhblackvap10p | pctnativevap10p >= 20 | pcthispvap10p >= 20 | pctothervap10p >= 20)) ~ "1",
    (pcthispvap10p >= 20 & (pctnhblackvap10p | pctnativevap10p >= 20 | pctasianvap10p >= 20 | pctothervap10p >= 20)) ~ "1",
    (pctothervap10p >= 20 & (pctnhblackvap10p | pctnativevap10p >= 20 | pctasianvap10p >= 20 | pcthispvap10p >= 20)) ~ "1",
    TRUE ~ "0"))

write_csv(pl1013, "pl1013_var.csv")
rm(list = ls())

# 2014 ACS places ####
# $3333 LODES in 2014 is 3375.75 in 2016 = 40,509 --> 40K threshold
places2014 <- read_csv("seplaces_allstates/2014places.csv")
vap2014 <- read_csv("seplaces_allstates/vap/acs1216vap.csv")
emp2014 <- read_csv("seplaces_allstates/emp/acs1216.csv")
employed <- read_csv("seplaces_allstates/acs14_emp.csv")
hu2014 <- read_csv("seplaces_allstates/hu/acs1216.csv")

places2014 <- places2014 %>%
    mutate(STATE = str_pad(Geo_STATE, 2, side = "left", pad = "0"),
           PLACE = str_pad(Geo_PLACE, 5, side = "left", pad = "0"), 
           plid = paste0(STATE, PLACE))

vap2014 <- vap2014 %>%
    mutate(STATE = str_pad(Geo_STATE, 2, side = "left", pad = "0"),
           PLACE = str_pad(Geo_PLACE, 5, side = "left", pad = "0"), 
           plid = paste0(STATE, PLACE))

emp2014 %<>%
    mutate(STATE = str_pad(Geo_STATE, 2, side = "left", pad = "0"),
           PLACE = str_pad(Geo_PLACE, 5, side = "left", pad = "0"), 
           plid = paste0(STATE, PLACE))

employed %<>%
  mutate(STATE = str_pad(Geo_STATE, 2, side = "left", pad = "0"),
         PLACE = str_pad(Geo_PLACE, 5, side = "left", pad = "0"), 
         plid = paste0(STATE, PLACE))

hu2014 %<>%
    mutate(STATE = str_pad(Geo_STATE, 2, side = "left", pad = "0"),
           PLACE = str_pad(Geo_PLACE, 5, side = "left", pad = "0"), 
           plid = paste0(STATE, PLACE))

places2014 %<>%
    left_join(vap2014 %>% select(plid, SE_T003_001:SE_T003_014), by = "plid") %>%
    left_join(employed %>% select(plid, SE_A17002_001:SE_A17002_007), by = "plid") %>%
    left_join(hu2014 %>% select(plid, SE_A10001_001) ,by = "plid") %>%
    left_join(emp2014 %>% select(plid, SE_A14024_001:ACS16_5yr_B20005095))

rm(emp2014, employed, hu2014, vap2014)
places2014 %<>%
    mutate(pop14p = SE_A00002_001, 
           popdensity14p = SE_A00002_002,
           nhblack14p = ACS16_5yr_B03002004, 
           nhwhite14p = ACS16_5yr_B03002003, 
           h14p = ACS16_5yr_B03002012, 
           min14p = (pop14p-nhwhite14p), 
           pctnhblack14p = (nhblack14p/pop14p) * 100,
           pctnhwhite14p = (nhwhite14p/pop14p) * 100, 
           pcth14p = (h14p/pop14p) * 100, 
           pctmin14p = (min14p/pop14p) * 100, 
           pctrecimm14p = (SE_A10058_002/pop14p) * 100,
           emp14p = SE_A17002_002,
           incomepp14p = SE_A14024_001*cpi[4],
           overlodesthresh14p = rowSums(across(c(ACS16_5yr_B20005019:ACS16_5yr_B20005025, ACS16_5yr_B20005042:ACS16_5yr_B20005048, ACS16_5yr_B20005066:ACS16_5yr_B20005072, ACS16_5yr_B20005089:ACS16_5yr_B20005095))),
           hu14p = ACS16_5yr_B25003001,
           owneroccupied14p = ACS16_5yr_B25003002,
           vacancy14p = ACS16_5yr_B25004001,
           mhmval14p = ACS16_5yr_B25077001*cpi[4],
           hinc14p = ACS16_5yr_B19013001 * cpi[4], 
           whitepov14p = (SE_A13001I_002/SE_A13001I_001)*100,
           blackpov14p = (SE_A13001B_002/SE_A13001B_001)*100,
           hpov14p = (SE_A13001H_002/SE_A13001H_001)*100,
           minpov14p =  ((hpov14p + blackpov14p + SE_A13001G_002 + SE_A13001F_002 + SE_A13001E_002 + SE_A13001D_002 + SE_A13001C_002)/(SE_A13001B_001 + SE_A13001B_001 + SE_A13001G_001 + SE_A13001F_001 + SE_A13001E_001 + SE_A13001D_001 + SE_A13001C_001))*100,
           vap14p = SE_T003_001,
           nhwhitevap14p = SE_T003_003,
           pctnhwhitevap14p = (nhwhitevap14p/vap14p)*100,
           nhblackvap14p = SE_T003_004,
           pctnhblackvap14p = (nhblackvap14p/vap14p)*100,
           hispvap14p = SE_T003_014,
           pcthispvap14p = (hispvap14p/vap14p)*100,
           nativevap14p = SE_T003_005,
           pctnativevap14p = (nativevap14p/vap14p)*100,
           asianvap14p = (SE_T003_006 + SE_T003_007),
           pctasianvap14p = (asianvap14p/vap14p)*100,
           othervap14p = SE_T003_008,
           pctothervap14p = (othervap14p/vap14p)*100) 

write_csv(places2014, "places2014_cleaned.csv")

# 1014 lagged var ####
places2010 <- read_csv("pl2010_cleaned.csv")
places2014 <- read_csv("places2014_cleaned.csv")

pl1014 <- 
  left_join(
    places2010 %>% select(
      c(plid, pop10p:pctothervap10p)), 
    places2014 %>% select(
      c(plid, pop14p:pctothervap14p)), 
    by = "plid")

pl1014 %<>%
  mutate_at(all_of(c("pop10p", "popdensity10p", "nhwhite10p", "nhblack10p", "h10p", "min10p",
                   "hinc10p", "nhwhitevap10p", "nhblackvap10p", "hispvap10p", "nativevap10p", "asianvap10p", "othervap10p",
                   "pop14p", "popdensity14p", "nhwhite14p", "nhblack14p", "h14p", "min14p",
                   "hinc14p", "nhwhitevap14p", "nhblackvap14p", "hispvap14p", "nativevap14p", "asianvap14p", "othervap14p")), 
            ~ifelse((is.na(.) | . == 0), 1, .)) %>%
  mutate(popgrowth = ((pop14p-pop10p)/pop10p) * 100,
         densification = (popdensity14p - popdensity10p),
         nhwhitegrowth = ((nhwhite14p-nhwhite10p)/nhwhite10p) * 100, 
         nhblackgrowth = ((nhblack14p-nhblack10p)/nhblack10p) * 100,
         hgrowth = ((h14p-h10p)/h10p) * 100,
         mingrowth = ((min14p-min10p)/min10p) * 100,
         recimmgrowth = (pctrecimm14p - pctrecimm10p),
         incomegrowth = ((hinc14p[4] - hinc10p[3]*1.09)/hinc10p[3])*100, 
         blackpovgrowth = (blackpov14p - blackpov10p),
         whitepovgrowth = (whitepov14p - whitepov10p),
         hpovgrowth = (hpov14p - hpov10p),
         minpovgrowth = (minpov14p - minpov10p), 
         nhwhitevapgrowth = ((nhwhitevap14p - nhwhitevap10p)/nhwhitevap10p)*100,
         nhblackvapgrowth = ((nhblackvap14p - nhblackvap10p)/nhblackvap10p)*100,
         hispvapgrowth = ((hispvap14p - hispvap10p)/hispvap10p)*100,
         nativevapgrowth = ((nativevap14p - nativevap10p)/nativevap10p)*100,
         asianvapgrowth = ((asianvap14p - asianvap10p)/asianvap10p)*100,
         othervapgrowth = ((othervap14p - othervap10p)/othervap10p)*100)

pl1014 %<>%
  mutate(vraa = case_when(
    (pctnhblackvap14p >= 20 & (pctnativevap14p >= 20 | pctasianvap14p >= 20 | pcthispvap14p >= 20 | pctothervap14p >= 20)) ~ "1",
    (pctnativevap14p >= 20 & (pctnhblackvap14p | pctasianvap14p >= 20 | pcthispvap14p >= 20 | pctothervap14p >= 20)) ~ "1",
    (pctasianvap14p >= 20 & (pctnhblackvap14p | pctnativevap14p >= 20 | pcthispvap14p >= 20 | pctothervap14p >= 20)) ~ "1",
    (pcthispvap14p >= 20 & (pctnhblackvap14p | pctnativevap14p >= 20 | pctasianvap14p >= 20 | pctothervap14p >= 20)) ~ "1",
    (pctothervap14p >= 20 & (pctnhblackvap14p | pctnativevap14p >= 20 | pctasianvap14p >= 20 | pcthispvap14p >= 20)) ~ "1",
    TRUE ~ "0")
  )
table(pl1014$vraa)

write_csv(pl1014, "pl1014_var.csv")

#rm(list = ls())
rm(places2010, pl1014)

# 2017 census ####
# 3333 LODES $ is 3454.78 in 2019 = 41454 --> 40K threshold
# vap, emp, and hu 
places2017 <- read_csv("seplaces_allstates/vap/acs1519vap.csv")
emp2017 <- read_csv("seplaces_allstates/emp/acs1519.csv")
hu2017 <- read_csv("seplaces_allstates/hu/acs1519.csv")

places2017 %<>% 
    rename("Geo_QName" = "Geo_QNAME") %>%
    mutate(plid = paste0(str_pad(Geo_STATE, 2, "left", "0"),
                         str_pad(Geo_PLACE, 5, "left", "0")))

emp2017 %<>% 
  mutate(plid = paste0(str_pad(Geo_STATE, 2, "left", "0"),
                       str_pad(Geo_PLACE, 5, "left", "0")))

hu2017 %<>% 
  mutate(plid = paste0(str_pad(Geo_STATE, 2, "left", "0"),
                       str_pad(Geo_PLACE, 5, "left", "0")))

places2017 %<>%
  left_join(emp2017 %>% select(plid, SE_A14024_001:ACS19_5yr_B20005095), by = "plid") %>%
  left_join(hu2017 %>% select(plid, SE_A10060_001:ACS19_5yr_B25004001), by = "plid")

rm(emp2017, hu2017)

places2017 %<>%
    mutate(hu17p = SE_A10060_001,
           owneroccuped17p = SE_A10060_002, 
           vacancy17p = ACS19_5yr_B25004001,
           incomepp17p = SE_A14024_001*cpi[5],
           overlodesthresh17p = rowSums(across(c(ACS19_5yr_B20005019:ACS19_5yr_B20005025, ACS19_5yr_B20005042:ACS19_5yr_B20005048, ACS19_5yr_B20005066:ACS19_5yr_B20005072, ACS19_5yr_B20005089:ACS19_5yr_B20005095))),
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
    select(c(plid, Geo_QName, contains("17p")))

pop2017 <- read_csv("seplaces_allstates/acs1519pop.csv")
pop2017 %<>%
  mutate(
    plid = paste0(str_pad(Geo_STATE, 2, "left", "0"),
                  str_pad(Geo_PLACE, 5, "left", "0")),
    pop17p = SE_A00001_001
  ) %>%
  select(plid, pop17p)
places2017 %<>%
  left_join(pop2017)

write_csv(places2017, "places2017_cleaned.csv")

places2014 <- read_csv("places2014_cleaned.csv")

pl1417 <- 
  left_join(
    places2014 %>% select(
      c(plid, pop14p:pctothervap14p)), 
    places2017, 
    by = "plid")

pl1417 %<>%
  mutate_at(all_of(c("pop14p", "popdensity14p", "nhwhite14p", "nhblack14p", "h14p", "min14p",
                     "hinc14p", "nhwhitevap14p", "nhblackvap14p", "hispvap14p", "nativevap14p", "asianvap14p", "othervap14p",
                     "pop17p", "vap17p", "nhwhitevap17p", "nhblackvap17p", "hispvap17p", "nativevap17p", "asianvap17p", "othervap17p")), 
            ~ifelse((is.na(.) | . == 0), 1, .)) %>%
  mutate(nhwhitevapgrowth = ((nhwhitevap17p - nhwhitevap14p)/nhwhitevap14p)*100,
         nhblackvapgrowth = ((nhblackvap17p - nhblackvap14p)/nhblackvap14p)*100,
         hispvapgrowth = ((hispvap17p - hispvap14p)/hispvap14p)*100,
         nativevapgrowth = ((nativevap17p - nativevap14p)/nativevap14p)*100,
         asianvapgrowth = ((asianvap17p - asianvap14p)/asianvap14p)*100,
         othervapgrowth = ((othervap17p - othervap14p)/othervap14p)*100)

pl1417 %<>%
  mutate(vraa = case_when(
    (pctnhblackvap14p >= 20 & (pctnativevap14p >= 20 | pctasianvap14p >= 20 | pcthispvap14p >= 20 | pctothervap14p >= 20)) ~ "1",
    (pctnativevap14p >= 20 & (pctnhblackvap14p | pctasianvap14p >= 20 | pcthispvap14p >= 20 | pctothervap14p >= 20)) ~ "1",
    (pctasianvap14p >= 20 & (pctnhblackvap14p | pctnativevap14p >= 20 | pcthispvap14p >= 20 | pctothervap14p >= 20)) ~ "1",
    (pcthispvap14p >= 20 & (pctnhblackvap14p | pctnativevap14p >= 20 | pctasianvap14p >= 20 | pctothervap14p >= 20)) ~ "1",
    (pctothervap14p >= 20 & (pctnhblackvap14p | pctnativevap14p >= 20 | pctasianvap14p >= 20 | pcthispvap14p >= 20)) ~ "1",
    TRUE ~ "0")
  )
table(pl1417$vraa)
write_csv(pl1417, "places1417_var.csv")

rm(pl1417, places2014, pop2017)

# 2020 places ####
places2020 <- read_csv("seplaces_allstates/vap/vap2020.csv")
places2020 %<>% 
  rename("Geo_QName" = "Geo_QNAME") %>%
  mutate(vap20p = SE_T011_001,
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
         pctothervap20p = (othervap20p/vap20p)*100,
         plid = paste0(str_pad(Geo_STATE, 2, "left", "0"),
                       str_pad(Geo_PLACE, 5, "left", "0"))) %>%
  select(c(plid, Geo_QName, contains("20p")))

write_csv(places2020, "places2020_cleaned.csv")

pl1720 <- 
  left_join(
    places2017 %>% select(
      c(plid, vap17p:pop17p)), 
    places2020, 
    by = "plid")

names(pl1720)
pl1720 %<>%
  mutate(vraa = case_when(
    (pctnhblackvap17p >= 20 & (pctnativevap17p >= 20 | pctasianvap17p >= 20 | pcthispvap17p >= 20 | pctothervap17p >= 20)) ~ "1",
    (pctnativevap17p >= 20 & (pctnhblackvap17p | pctasianvap17p >= 20 | pcthispvap17p >= 20 | pctothervap17p >= 20)) ~ "1",
    (pctasianvap17p >= 20 & (pctnhblackvap17p | pctnativevap17p >= 20 | pcthispvap17p >= 20 | pctothervap17p >= 20)) ~ "1",
    (pcthispvap17p >= 20 & (pctnhblackvap17p | pctnativevap17p >= 20 | pctasianvap17p >= 20 | pctothervap17p >= 20)) ~ "1",
    (pctothervap17p >= 20 & (pctnhblackvap17p | pctnativevap17p >= 20 | pctasianvap17p >= 20 | pcthispvap17p >= 20)) ~ "1",
    TRUE ~ "0")
  )
table(pl1720$vraa)
write_csv(pl1720, "places1720_var.csv")

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