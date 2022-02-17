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

# 2000 block-level data ####
blocks2000 <- fread(file = "ipumsblocks_allstates/2000blocks/nhgis0032_ds147_2000_block.csv")
blocks2000 <- blocks2000[-1,]

# 1. convert factorized numerical variables back to numerical, but the file is very large, so we need 
# to split it into manageable chunks (stored in a list object) and run the analysis on each chunk separately. 
# The data is so large in part because there are >8 mil. blocks and 80 variables. However, we don't need 
# each variable. You can look at the codebook for the data too to get a sense of why we don't need each variable 
# in the analytical sample, but we DO need most of the variables to construct actual variables of use.

thesecolumns <- c(14:76) # these are the column indexes for variables that need to be convereted from char to num.

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
             pctowneroccupied00b = (FWA001/(FWA001 + FWA002))*100,
             vacancy00b = ((FV5001 - (FWA001 + FWA002))/FV5001)*100,
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
  dplyr::select(STATEA, COUNTYA, TRACTA, BLOCKA, vap00b, hispvap00b, nhwvap00b, nhbvap00b, minorityvap00b) %>%
  mutate(pcthispvap00b = (hispvap00b/vap00b)*100,
         pctnhwvap00b = (nhwvap00b/vap00b)*100,
         pctnhbvap00b = (nhbvap00b/vap00b)*100,
         pctasianvap00b = (asianvap00b/vap00b)*100,
         pctnativevap00b = (nativevap00b/vap00b)*100,
         pctothervap00b = (othervap00b/vap00b)*100,
         blkid = paste0(str_pad(STATEA, 2, side = "left", pad = "0"), str_pad(COUNTYA, 3, side = "left", pad = "0"),
                        str_pad(TRACTA, 6, side = "left", pad = "0"), sprintf("%04.0f", BLOCKA))) %>%
  dplyr::select(blkid, vap00b:pctotherap00b)
# by selecting the variables we need, we significantly reduce the file size of the data. 

# turn list into dataframe 
blocks2000 <- rbindlist(dat_use, use.names = TRUE)
rm(dat_use)

# here we save the new file we've made
blocks2000 <- blocks2000 %>%
  left_join(vap2000block, by = "blkid")
write_csv(blocks2000, "blocks2000_var.csv")

# get vra data for 2000
rm(vap2000block)

vrastates <- c("01", "02", "04", "13", "22", "28", "45", "48", "51")
vra <- read_csv("vra_counties.csv")
vra %<>% 
    mutate(countyfips = str_pad(countyfips, 5, side = "left", pad = "0"),
           sectionv = 1)

blocks2000 %<>%
    mutate(countyfips = paste0(STATEA, COUNTYA)) %>%
    left_join(vra, 
              by = "countyfips") %>%
    mutate(sectionv = 
               case_when(
                   STATEA %in% vrastates ~ 1,
                   is.na(sectionv) ~ 0,
                   TRUE ~ sectionv
               ))
table(blocks2000$sectionv, exclude = NULL)

vra_plids_2000 <- blocks2000 %>%
    filter(PLACEA != "99999") %>%
    mutate(plid = paste0(
        str_pad(STATEA, 2, side = "left", pad = "0"),
        str_pad(PLACEA, 5, "left", "0")
    )) %>%
    group_by(plid) %>%
    summarize(sectionv = mean(sectionv)) %>%
    mutate(sectionv = ifelse(sectionv > 0 & sectionv < 1, 1, sectionv))

table(vra_plids_2000$sectionv)
write_csv(vra_plids_2000, "vra_plids_2000.csv")

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
                pctowneroccupied10b = ((IFF002+IFF003)/IFF001)*100,
                vacancy10b = ((IFC001-IFF001)/IFC001)*100,
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

vrastates <- c("01", "02", "04", "13", "22", "28", "45", "48", "51")
vra <- read_csv("vra_counties.csv")
vra %<>% 
    mutate(countyfips = str_pad(countyfips, 5, side = "left", pad = "0"),
           sectionv = 1)

blocks2010 %<>%
    mutate(countyfips = paste0(STATEA, COUNTYA)) %>%
    left_join(vra, 
              by = "countyfips") %>%
    mutate(sectionv = 
               case_when(
                   STATEA %in% vrastates ~ 1,
                   is.na(sectionv) ~ 0,
                   TRUE ~ sectionv
               ))
table(blocks2010$sectionv, exclude = NULL)

vra_plids_2010 <- blocks2010 %>%
    filter(PLACEA != "99999") %>%
    mutate(plid = paste0(
        str_pad(STATEA, 2, side = "left", pad = "0"),
        str_pad(PLACEA, 5, "left", "0")
    )) %>%
    group_by(plid) %>%
    summarize(sectionv = mean(sectionv)) %>%
    mutate(sectionv = ifelse(sectionv > 0 & sectionv < 1, 1, sectionv))

table(vra_plids_2010$sectionv)
write_csv(vra_plids_2010, "vra_plids_2010.csv")

rm(list = ls())

# 2000 place-level data ####
# cpi to 2016$, which is ACS$ 
# 2000(1999), 2010(2009), 2013(2012), 2014(2013)
cpi <- c(1.42, 1.11, 1.03, 1)
places2000 <- read_csv(file = "seplaces_allstates/2000places.csv") # notice that place-level data is much smaller

places2000 <- places2000 %>%
  mutate(STATE = str_pad(Geo_STATE, 2, side = "left", pad = "0"),
         PLACE = str_pad(Geo_PLACE, 5, side = "left", pad = "0"), 
         plid = paste0(STATE, PLACE))

# 2. create variables needed 
places2000 %<>%
  mutate(pop00p = SE_T001_001, 
         #pcturb00p = (SE_T002_002/pop00p)*100, 
         #pctrur00p = 100-pcturb00p,
         popdensity00p = SE_T003_001,
         nhblack00p = SE_T015_004, 
         nhwhite00p = SE_T015_003, 
         h00p = SE_T015_010, 
         min00p = rowSums(places2000 %>% select(SE_T015_004:SE_T015_010)), 
         pctnhblack00p = (nhblack00p/pop00p) * 100,
         pctnhwhite00p = (nhwhite00p/pop00p) * 100, 
         pcth00p = (h00p/pop00p) * 100, 
         pctmin00p = (min00p/pop00p) * 100, 
         pctrecimm00p = (SE_T202_002/pop00p) * 100,
         unemp00p = (SE_T069_006/SE_T069_004)*100,
         hinc00p = SE_T093_001 * cpi[1], 
         pctowneroccupied00p = (SE_T156_002/SE_T156_001) * 100,
         pctvacancy00p = (SE_T157_003/SE_T157_001) * 100,
         mhmval00p = SE_T163_001*cpi[1],
         blackpov00p = (SE_T187_002/SE_T187_001)*100,
         hpov00p = (SE_T193_002/SE_T193_001)*100, 
         nhwhitepov00p = (SE_T194_002/SE_T194_001)*100, 
         minpov00p = ((SE_T187_002 + SE_T188_002 + SE_T189_002 + SE_T190_002 + SE_T191_002 + SE_T192_002 + SE_T193_002)/(SE_T187_001 + SE_T188_001 + SE_T189_001 + SE_T190_001 + SE_T191_001 + SE_T192_001 + SE_T193_001))*100,
         vap00p = SF1_P006001,
         nhwhitevap00p = SF1_P006005,
         pctnhwhitevap00p = (nhwhitevap00p/vap00p)*100, #nh stands for non-Hispanic
         nhblackvap00p = SF1_P006006,
         nativevap00p = SF1_P006009,
         pctnativevap00p = (nativevap00p/vap00p)*100,
         asianvap00p = SF1_P006008 + SF1_P006009,
         pctasianvap00p = (asianvap00p/vap00p)*100,
         othervap00p = rowSums(across(c(SF1_P006010:SF1_P006073))),
         pctothervap00p = (othervap00p/vap00p)*100,
         pctnhblackvap00p = (nhblackvap00p/vap00p)*100,
         hispvap00p = SF1_P006002,
         pcthispvap00p = (hispvap00p/vap00p)*100) %>%
  select(Geo_QName, plid, pop00p:pcthispvap00p)

write_csv(places2000, "pl2000_cleaned.csv")
rm(places2000)

# 1990 places ####
# cpi to 2016$, which is ACS$ 
# 1990(1989)
cpi <- c(1.93)
places1990 <- read_csv(file = "seplaces_allstates/1990places.csv") # notice that place-level data is much smaller
vap1990  <- read_csv(file = "seplaces_allstates/vap1990.csv")

places1990 %<>%
  mutate(STATE = str_pad(Geo_STATE, 2, side = "left", pad = "0"),
         PLACE = str_pad(Geo_PLACECE, 5, side = "left", pad = "0"), 
         plid = paste0(STATE, PLACE))

vap1990 %<>%
  mutate(STATE = str_pad(Geo_STATE, 2, side = "left", pad = "0"),
         PLACE = str_pad(Geo_PLACECE, 5, side = "left", pad = "0"), 
         plid = paste0(STATE, PLACE))

places1990 %<>%
  left_join(vap1990 %>% select(plid, STF3_P014_001:STF3_P015_065), by = "plid")
rm(vap1990)

# 2. create variables needed 
places1990 %<>%
  mutate(pop90p = SE_T001_001, 
         #pcturb90p = (SE_T002_002/pop90p)*100, 
         #pctrur90p = 100-pcturb90p,
         popdensity90p = SE_T002_001,
         nhblack90p = SE_T013_004, 
         nhwhite90p = SE_T013_003, 
         h90p = SE_T013_008, 
         min90p = rowSums(across(SE_T013_004:SE_T013_008)), 
         pctnhblack90p = (nhblack90p/pop90p) * 100,
         pctnhwhite90p = (nhwhite90p/pop90p) * 100, 
         pcth90p = (h90p/pop90p) * 100, 
         pctmin90p = (min90p/pop90p) * 100, 
         pctrecimm90p = (SE_T111_002/pop90p) * 100,
         unemp90p = (SE_T029_003/SE_T029_001)*100,
         hinc90p = SE_T043_001 * cpi[1], 
         pctowneroccupied90p = (SE_T073_002/SE_T073_001) * 100,
         pctvacancy90p = (SE_T074_003/SE_T074_001) * 100,
         mhmval90p = SE_T080_001*cpi[1],
         blackpov90p = (SE_T101_003/SE_T101_001)*100,
         hpov90p = (SE_T105_003/SE_T105_003)*100, 
         nhwhitepov90p = (SE_T100_003/SE_T100_001)*100, 
         minpov90p = ((SE_T099_008 - SE_T099_009)/(SE_T099_001-SE_T099_002))*100,
         vap90p = rowSums(across(c(STF3_P014_016:STF3_P014_034, STF3_P014_048:STF3_P014_066,
                                                STF3_P014_081:STF3_P014_099, STF3_P014_113:STF3_P014_131,
                                                STF3_P014_146:STF3_P014_164, STF3_P014_178:STF3_P014_196,
                                                STF3_P014_211:STF3_P014_229, STF3_P014_243:STF3_P014_261,
                                                STF3_P014_276:STF3_P014_294, STF3_P014_308:STF3_P014_326), na.rm = T)),
         nhwhitevap90p = rowSums(across(c(STF3_P014_016:STF3_P014_034, STF3_P014_048:STF3_P014_066))),
         nhblackvap90p = rowSums(across(c(STF3_P014_081:STF3_P014_099, STF3_P014_113:STF3_P014_131))),
         nativevap90p = rowSums(across(c(STF3_P014_146:STF3_P014_164, STF3_P014_178:STF3_P014_196))),
         asianvap90p = rowSums(across(c(STF3_P014_211:STF3_P014_229, STF3_P014_243:STF3_P014_261))),
         othervap90p = rowSums(across(c(STF3_P014_276:STF3_P014_294, STF3_P014_308:STF3_P014_326))),
         hispvap90p = rowSums(across(c(STF3_P015_015:STF3_P015_033, STF3_P015_047:STF3_P015_065))),
         pctnhwhitevap90p = (nhwhitevap90p/vap90p)*100, #nh stands for non-Hispanic
         pctnhblackvap90p = (nhblackvap90p/vap90p)*100,
         pcthispvap90p = (hispvap90p/vap90p)*100,
         pctnativevap90p = (nativevap90p/vap90p)*100,
         pctasianvap90p = (asianvap90p/vap90p)*100,
         pctothervap90p = (othervap90p/vap90p)) %>%
  select(Geo_QName, plid, pop90p:pctothervap90p)

write_csv(places1990, "pl1990_cleaned.csv")

# merge for 1990-2000
pl2000 <- read_csv("pl2000_cleaned.csv")
table(places1990$plid %in% pl2000$plid)
table(places1990$Geo_QName %in% pl2000$Geo_QName)

pl9000 <- left_join(
  places1990 %>% rename(plid90 = plid),
  pl2000,
  by = "Geo_QName"
)

rm(places1990)

# make change variables 
pl9000 %<>% 
  mutate_at(all_of(c("pop90p", "popdensity90p", "nhwhite90p", "nhblack90p", "h90p", "min90p",
                     "hinc90p", "nhwhitevap90p", "nhblackvap90p", "hispvap90p", "nativevap90p", "asianvap90p", "othervap90p",
                     "pop00p", "popdensity00p", "nhwhite00p", "nhblack00p", "h00p", "min00p",
                     "hinc00p", "nhwhitevap00p", "nhblackvap00p", "hispvap00p", "nativevap00p", "asianvap00p", "othervap00p")), 
            ~ifelse((is.na(.) | . == 0), 1, .)) %>%
  mutate(popgrowth = ((pop00p-pop90p)/pop90p) * 100,
         densification = (popdensity00p - popdensity90p),
         nhwhitegrowth = ((nhwhite00p-nhwhite90p)/nhwhite90p) * 100, 
         nhblackgrowth = ((nhblack00p-nhblack90p)/nhblack90p) * 100,
         hgrowth = ((h00p-h90p)/h90p) * 100,
         mingrowth = ((min00p-min90p)/min90p) * 100,
         recimmgrowth = (pctrecimm00p - pctrecimm90p),
         incomegrowth = ((hinc00p - hinc90p*cpi[1])/hinc00p)*100, 
         blackpovgrowth = (blackpov00p - blackpov90p),
         whitepovgrowth = (nhwhitepov00p - nhwhitepov90p),
         hpovgrowth = (hpov00p - hpov90p),
         minpovgrowth = (minpov00p - minpov90p), 
         nhwhitevapgrowth = ((nhwhitevap00p - nhwhitevap90p)/nhwhitevap90p)*100,
         nhblackvapgrowth = ((nhblackvap00p - nhblackvap90p)/nhblackvap90p)*100,
         hispvapgrowth = ((hispvap00p - hispvap90p)/hispvap90p)*100,
         nativevapgrowth = ((nativevap00p - nativevap90p)/nativevap90p)*100,
         asianvapgrowth = ((asianvap00p - asianvap90p)/asianvap90p)*100,
         othervapgrowth = ((othervap00p - othervap90p)/othervap90p)*100)

pl9000 %<>%
  mutate(vraa = case_when(
         (pctnhblackvap00p >= 20 & (pctnativevap00p >= 20 | pctasianvap00p >= 20 | pcthispvap00p >= 20 | pctothervap00p >= 20)) ~ "1",
         (pctnativevap00p >= 20 & (pctnhblackvap00p | pctasianvap00p >= 20 | pcthispvap00p >= 20 | pctothervap00p >= 20)) ~ "1",
       (pctasianvap00p >= 20 & (pctnhblackvap00p | pctnativevap00p >= 20 | pcthispvap00p >= 20 | pctothervap00p >= 20)) ~ "1",
       (pcthispvap00p >= 20 & (pctnhblackvap00p | pctnativevap00p >= 20 | pctasianvap00p >= 20 | pctothervap00p >= 20)) ~ "1",
       (pctothervap00p >= 20 & (pctnhblackvap00p | pctnativevap00p >= 20 | pctasianvap00p >= 20 | pcthispvap00p >= 20)) ~ "1",
       TRUE ~ "0")
  )

write_csv(pl9000, "pl9000_var.csv")


# 2010 places ####
cpi <- c(1.42, 1.11, 1.03, 1)
places2010 <- read_csv(file = "seplaces_allstates/2010places.csv")
vap2010 <- read_csv(file = "seplaces_allstates/2010_vap.csv")

places2010 <- places2010 %>%
  mutate(STATE = str_pad(Geo_STATE, 2, side = "left", pad = "0"),
         PLACE = str_pad(Geo_PLACE, 5, side = "left", pad = "0"), 
         plid = paste0(STATE, PLACE))

vap2010 <- vap2010 %>%
  mutate(STATE = str_pad(Geo_STATE, 2, side = "left", pad = "0"),
         PLACE = str_pad(Geo_PLACE, 5, side = "left", pad = "0"), 
         plid = paste0(STATE, PLACE))

places2010 <- places2010 %>%
  left_join(vap2010 %>% select(plid, SF1_P0110001:SF1_P0110073), by = "plid")

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
         unemp10p = (SE_A17002_006/SE_A17002_004)*100,
         pctowneroccupied10p = (SE_A10060_002/SE_A10060_001)*100,
         pctvacancy10p = (SE_A10044_003/SE_A10044_001)*100,
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
         othervap10p = rowSums(across(c(SF1_P0110010:SF1_P0110073))),
         pctothervap10p = (othervap10p/vap10p)*100,
         ) %>%
  select(plid, pop10p:pctothervap10p)

write_csv(places2010, "pl2010_cleaned.csv")

length(unique(pl2000$plid))
length(unique(places2010$plid))

table(unique(pl2000$plid) %in% unique(places2010$plid))

pl0010 <- 
  left_join(
    pl2000, 
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
         incomegrowth = ((hinc10p - hinc00p*cpi[1])/hinc10p)*100, 
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

# 2013 ACS places ####
# wee just need vap info 
acs13 <- read_csv("seplaces_allstates/acs0913vap.csv")

acs13 %<>%
    mutate(plid = paste0(
        str_pad(Geo_STATE, 2, "left", "0"),
        str_pad(Geo_PLACE, 5, "left", "0")
    ),
           popover18 = SE_T003_001,
           nhwhitevap13p = (SE_T003_003/popover18)*100,
           nhblackvap13p = (SE_T003_004/popover18)*100,
           hispvap13p = (SE_T003_014/popover18)*100,
           minvap13p = ((popover18-SE_T003_003)/popover18)*100) %>%
    select(plid, nhwhitevap13p:minvap13p)

write_csv(acs13, "acs13.csv")
rm(acs13)

# 2014 ACS places ####
places2014 <- read_csv("seplaces_allstates/2014places.csv")
vap2014 <- read_csv("seplaces_allstates/acs1014vap.csv")
emp2014 <- read_csv("seplaces_allstates/acs14_emp.csv")
hu2014 <- read_csv("seplaces_allstates/acs14_hu.csv")

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

hu2014 %<>%
    mutate(STATE = str_pad(Geo_STATE, 2, side = "left", pad = "0"),
           PLACE = str_pad(Geo_PLACE, 5, side = "left", pad = "0"), 
           plid = paste0(STATE, PLACE))

places2014 %<>%
    left_join(vap2014 %>% select(plid, SE_T003_001:SE_T003_014), by = "plid") %>%
    left_join(emp2014 %>% select(plid, SE_A17002_001:SE_A17002_007), by = "plid") %>%
    left_join(hu2014 %>% select(plid, SE_A10001_001) ,by = "plid")

places2014 %<>%
    mutate(pop14p = SE_A00002_001, 
           #pcturb14p = NA,
           #pctrur14p = NA,
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
           unemp14p = (SE_A17002_006/SE_A17002_004)*100,
           pctowneroccupied14p = (ACS16_5yr_B25003002/ACS16_5yr_B25003001)*100,
           pctvacancy14p = (ACS16_5yr_B25004001/SE_A10001_001)*100,
           mhmval14p = ACS16_5yr_B25077001*cpi[3],
           hinc14p = ACS16_5yr_B19013001 * cpi[3], 
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
           nativevap14p = SE_T003_007,
           pctnativevap14p = (nativevap14p/vap14p)*100,
           asianvap14p = (SE_T003_006 + SE_T003_007),
           pctasianvap14p = (asianvap14p/vap14p)*100,
           othervap14p = rowSums(across(c(SE_T003_008:SE_T003_013))),
           pctothervap14p = (othervap14p/vap14p)*100) 

write_csv(places2014, "places2014_cleaned.csv")
rm(emp2014, hu2014, places2014, vap2014)

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
         incomegrowth = ((hinc14p - hinc10p*1.09)/hinc14p)*100, 
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
table(pl0010$vraa)

write_csv(pl1014, "pl1014_var.csv")

#rm(list = ls())
rm(places2010, vap2010)

# 2020 census ####
# only need vap data 
places2020 <- read_csv("seplaces_allstates/acs1519vap.csv")
places2020 %<>% 
    rename("Geo_QName" = "Geo_QNAME") %>%
    mutate(popover18 = SE_T003_001,
           nhwhitevap20p = (SE_T003_003/popover18)*100,
           nhblackvap20p = (SE_T003_004/popover18)*100,
           hispvap20p = (SE_T003_014/popover18)*100,
           minvap20p = ((popover18-SE_T003_003)/popover18)*100,
           STATE = str_pad(Geo_STATE, 2, side = "left", pad = "0"),
           PLACE = str_pad(Geo_PLACE, 5, side = "left", pad = "0"), 
           plid = paste0(STATE, PLACE)) %>%
    select(c(plid, Geo_QName, contains("20p")))

write_csv(places2020, "places2020_cleaned.csv")

# now do 2000-2013 and 2013-2020 ####
length(unique(acs13$plid))
length(unique(places2020$plid))

places2020 <- places2020 %>%
    select(-pop20p) 

vars <- c("nhwhite", "nhblack", "h", "min")
names(places2020)[3:6] <- vars
names(acs13)[3:6] <- vars

acs13 <- acs13 %>%
    mutate(Year = "2013", 
           Time = 0) %>% 
    filter(plid %in% unique(places2020$plid))

places2020 <- places2020 %>% 
    mutate(Year = "2020", 
           Time = 0) %>%
    filter(plid %in% unique(acs13$plid))

panel1320 <- base::rbind(acs13, places2020)
write_csv(panel1320, "panel1320_did.csv")

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
            vacancy20b = (U7G003/U7G001)*100
        ) %>% 
        dplyr::select( # select can take a vector of column indexes c(number 1, number 2, number 3:number 7 etc.) or column names
            STATEA, COUNTYA, TRACTA, BLOCKA, PLACEA, pop20b:vacancy20b)
    return(NULL)
}

blocks2020 <- rbindlist(dat_use, use.names = TRUE)
rm(dat_use)
write_csv(blocks2020, "blocks2020_var.csv")

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
blocks2000 %<>%
    select(blkid, pop00b, pctnhblack00b, pctnhwhite00b, pcth00b, pctmin00b,
           hispvap00b, nhwvap00b, nhbvap00b, minorityvap00b, vacancy00b) 
names(blocks2000) <- gsub("00b", "", names(blocks2000))
blocks2000 %<>%
    mutate(Year = "2000")

blocks2010 <- read_csv("blocks2010_var.csv")
blocks2010 %<>%
    mutate(blkid = paste0(str_pad(STATEA, 2, side = "left", pad = "0"),
                          str_pad(COUNTYA, 3, side = "left", pad = "0"),
                          str_pad(TRACTA, 6, side = "left", pad = "0"),
                          str_pad(BLOCKA, 4, side = "left", pad = "0"))) %>%
    select(blkid, pop10b:pctmin10b, vacancy10b, vap10b:pctminorityvap10b) 

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

blocks2010 %<>%
  filter(blkid %in% blocks2020$blkid) 

blocks2020 %<>%
  filter(blkid %in% blocks2010$blkid)

blocks2010 %<>%
  filter(blkid %in% blocks2020$blkid) 

blocks <- base::rbind(blocks2010, blocks2020)
rm(blocks2010, blocks2020)

blocks2013 <- read_csv("blocks2010_var.csv")
blocks2013 %<>%
    mutate(blkid = paste0(str_pad(STATEA, 2, side = "left", pad = "0"),
                          str_pad(COUNTYA, 3, side = "left", pad = "0"),
                          str_pad(TRACTA, 6, side = "left", pad = "0"),
                          str_pad(BLOCKA, 4, side = "left", pad = "0"))) %>%
    select(blkid, pop10b, pctnhblack10b, pctnhwhite10b, pcth10b, pctmin10b,
           hispvap10b, nhwvap10b, nhbvap10b, minorityvap10b, vacancy10b) 
names(blocks2013) <- gsub("10b", "", names(blocks2013))
blocks2013 %<>%
    mutate(Year = "2013")

blocks2013 %<>%
    mutate(pop = NA,
           pctnhblack = NA,
           pctnhwhite = NA,
           pcth = NA,
           pctmin = NA,
           hispvap = NA,
           nhwvap = NA,
           nhbvap = NA,
           minorityvap = NA,
           vacancy = NA)

blocks <- base::rbind(blocks, blocks2013)
rm(blocks2013)

blocks %<>%
    mutate(Year = as.numeric(as.character(Year))) 

blocks %<>%
    group_by(blkid) %>%
    arrange(Year) %>%
    mutate_at(c(names(blocks)[2:11]), zoo::na.approx, na.rm = F) %>%
    ungroup()

blocks13 <- blocks %>%
    filter(Year=="2013")

write_csv(blocks13, "blocks2013_int.csv")
rm(blocks13)

# do 2014 
blocks2014 <- blocks2010

blocks2014 %<>%
    mutate_at(all_of(names(blocks2014)[2:20]), ~NA) %>%
  mutate(Year = 2014)

blocks <- base::rbind(blocks2010, blocks2014, blocks2020)

rm(blocks2010, blocks2014, blocks2020)

test <- blocks %>% filter(blkid %in% unique(blkid)[1:50])

test %<>%
    group_by(blkid) %>%
    arrange(Year) %>%
    mutate_at(all_of(names(blocks)[2:20]), zoo::na.approx, na.rm = F) %>%
    ungroup() 

blocks %<>%
  group_by(blkid) %>%
  arrange(Year) %>%
  mutate_at(all_of(names(blocks)[2:20]), zoo::na.approx, na.rm = F) %>%
  ungroup() 

rm(test)

blocks14 <- blocks %>%
    filter(Year == "2014")
write_csv(blocks14, "blocks2014_int.csv")

rm(list = ls())

# # clean block group data ####
# bg2013 <- fread(file = "ipumsblocks_allstates/2013bg/nhgis0037_ds215_20155_blck_grp.csv") 
# bg2013 %<>%
#     mutate( 
#         pop13bg = ADK5E001,
#         nhblack13bg = ADK5E004,
#         nhwhite13bg = ADK5E003, 
#         h13bg = ADK5E012,
#         min13bg = (pop13bg-nhwhite13bg),
#         pctnhblack13bg = (nhblack13bg/pop13bg)*100,
#         pctnhwhite13bg = (nhwhite13bg/pop13bg)*100, 
#         pcth13bg = (h13bg/pop13bg)*100, 
#         pctmin13bg = (min13bg/pop13bg)*100
#     ) %>% 
#     dplyr::select( # select can take a vector of column indexes c(number 1, number 2, number 3:number 7 etc.) or column names
#         STATEA, COUNTYA, TRACTA, BLKGRPA, PLACEA, pop13bg:pctmin13bg)
# 
# write_csv(bg2013, "bg2013_var.csv")
# 
# bg2020 <- fread(file = "ipumsblocks_allstates/2020bg/nhgis0039_ds248_2020_blck_grp.csv") 
# bg2020 %<>%
#     mutate( 
#         pop20bg = U7C001,
#         nhblack20bg = U7C006,
#         nhwhite20bg = U7C005, 
#         h20bg = U7C002,
#         min20bg = (pop20bg-nhwhite20bg),
#         pctnhblack20bg = (nhblack20bg/pop20bg)*100,
#         pctnhwhite20bg = (nhwhite20bg/pop20bg)*100, 
#         pcth20bg = (h20bg/pop20bg)*100, 
#         pctmin20bg = (min20bg/pop20bg)*100
#     ) %>% 
#     dplyr::select( # select can take a vector of column indexes c(number 1, number 2, number 3:number 7 etc.) or column names
#         STATEA, COUNTYA, TRACTA, BLKGRPA, PLACEA, pop20bg:pctmin20bg)
# 
# write_csv(bg2020, "bg2020_var.csv")
