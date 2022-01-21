# clean block and place data before identifying annexed blocks ##############
# Purpose of this script is to create necessary variables of interest #######
# for place90, place00, block00 and block10 datasets before doing ###########
# analysis outlined in BAS0010. This is script step ONE. ####################


rm(list = ls())
setwd("~/Google Drive/Stanford/QE2") # use setwd() to set your home directory 

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
blocks2000 <- read_csv(file = "ipumsblocks_allstates/2000blocks/nhgis0032_ds147_2000_block.csv")
blocks2000 <- blocks2000[-1,] # for some reason the first row of this df is irrelevant so we remove it.

# 1. rename variables
# names2000 <- c("nhwhite", "nhblack", "nhaian", "nhasian", "nhpi",
#                "nhother", "nh2p", "hwhite", "hblack", "haian", "hasian",
#                "hpi", "hother", "h2p",
#                "m5", "m5to9", "m10to14", "m15to17", "m18to19", "m20", "m21",
#                "m22to24", "m25to29", "m30to34", "m35to39", "m40to44", "m45to49", 
#                "m50to54", "m55to59", "m60to61", "m62to64", "m65to66", "m67to69",
#                "m70to74", "m75to79", "m80to84", "m85p",
#                "f5", "f5to9", "f10to14", "f15to17", "f18to19", "f20", "f21",
#                "f22to24", "f25to29", "f30to34", "f35to39", "f40to44", "f45to49", 
#                "f50to54", "f55to59", "f60to61", "f62to64", "f65to66", "f67to69",
#                "f70to74", "f75to79", "f80to84", "f85p",
#                "hu", "ownerocc", "renterocc")

# names(blocks2000)[14:76] <- names2000 
# ^ we can use the names() -- it returns a vector, which we index with [] notation, and an assignment to rename variables
# this is old code, however. Since I have the codebook, I decided to skip the renaming process, which is very tedious, and 
# use the mutate option instead to conduct operations on the variables. 

# 2. convert factorized numerical variables back to numerical, but the file is very large, so we need 
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
             dependants00b = rowSums(across(c(28:31, 51:54, 45:50, 68:73), na.rm = T)),
             workingage00b = rowSums(across(c(32:44, 55:67), na.rm = T)), 
             dependencyratio00b = dependants00b/workingage00b,
             pctowneroccupied00b = (FWA001/(FWA001 + FWA002))*100,
             vacancy00b = ((FV5001 - (FWA001 + FWA002))/FV5001)*100,
             blkid = paste0(str_pad(STATEA, 2, side = "left", pad = "0"), str_pad(COUNTYA, 3, side = "left", pad = "0"),
                            str_pad(TRACTA, 6, side = "left", pad = "0"), str_pad(BLOCKA, 4, side = "left", pad = "0"))) %>%
    dplyr::select( # select can take a vector of column indexes c(number 1, number 2, number 3:number 7 etc.) or column names
               GISJOIN, STATEA, COUNTYA, TRACTA, BLOCKA, PLACEA, blkid, pop00b:vacancy00b)
  return(NULL)
}

vap2000block <- read_csv("ipumsblocks_allstates/2000blocks/vap2000_block.csv")

vap2000block <- vap2000block %>%
  mutate(vap = FX4001,
         hispvap = FX9001,
         nhwvap = FYB001,
         nhbvap = FYB002,
         minorityvap = vap - nhwvap) %>%
  dplyr::select(STATEA, COUNTYA, TRACTA, BLOCKA, vap, hispvap, nhwvap, nhbvap, minorityvap) %>%
  mutate(hispvap00b = (hispvap/vap)*100,
         nhwvap00b = (nhwvap/vap)*100,
         nhbvap00b = (nhbvap/vap)*100,
         minorityvap00b = (minorityvap/vap)*100,
         blkid = paste0(str_pad(STATEA, 2, side = "left", pad = "0"), str_pad(COUNTYA, 3, side = "left", pad = "0"),
                        str_pad(TRACTA, 6, side = "left", pad = "0"), sprintf("%04.0f", BLOCKA))) %>%
  dplyr::select(blkid, hispvap00b, nhwvap00b, nhbvap00b, minorityvap00b)
# by selecting the variables we need, we significantly reduce the file size of the data. 

# turn list into dataframe 
blocks2000 <- rbindlist(dat_use, use.names = TRUE)
rm(dat_use)

# here we save the new file we've made
blocks2000 <- blocks2000 %>%
  left_join(vap2000block, by = "blkid")
write_csv(blocks2000, "blocks2000_var.csv")

# 2010 block-level data ####
blocks2010 <- fread(file = "ipumsblocks_allstates/2010blocks/nhgis0036_ds172_2010_block.csv") 
blocks2010 %<>%
    select(c("STATEA", "COUNTYA", "TRACTA", "BLOCKA", "PLACEA", "H7W001":"IFF004"))
blocks2010 <- blocks2010[c(1:500000), ]
f <- rep(seq_len(ceiling(nrow(blocks2010) / 100000)), each = 100000, length.out = nrow(blocks2010))
dat_use <- split(blocks2010, f = f)
rm(blocks2010)
rm(f)

# here's where we do processing on each chunk of the data 
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
      urbunits10b = (IFD002/IFD001)*100,
      hispvap10b = (H75002/H75001)*100,
      nhwvap10b = (H75005/H75001)*100,
      nhbvap10b = (H75006/H75001)*100,
      minorityvap10b = ((H75001 - H75005)/H75001)*100
      ) %>% 
    dplyr::select( # select can take a vector of column indexes c(number 1, number 2, number 3:number 7 etc.) or column names
      STATEA, COUNTYA, TRACTA, BLOCKA, PLACEA, pop10b:minorityvap10b)
  return(NULL)
}

# by selecting the variables we need, we significantly reduce the file size of the data. 

# turn list into dataframe 
blocks2010 <- rbindlist(dat_use, use.names = TRUE)
rm(dat_use)

# here we save the new file we've made
write_csv(blocks2010, "blocks2010_var_pt1.csv")

blocks2010 <- fread(file = "ipumsblocks_allstates/2010blocks/nhgis0036_ds172_2010_block.csv") 
blocks2010 %<>%
    select(c("STATEA", "COUNTYA", "TRACTA", "BLOCKA", "PLACEA", "H7W001":"IFF004"))
blocks2010 <- blocks2010[c(500001:1000000), ]
f <- rep(seq_len(ceiling(nrow(blocks2010) / 100000)), each = 100000, length.out = nrow(blocks2010))
dat_use <- split(blocks2010, f = f)
rm(blocks2010)
rm(f)

# here's where we do processing on each chunk of the data 
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
            urbunits10b = (IFD002/IFD001)*100,
            hispvap10b = (H75002/H75001)*100,
            nhwvap10b = (H75005/H75001)*100,
            nhbvap10b = (H75006/H75001)*100,
            minorityvap10b = ((H75001 - H75005)/H75001)*100
        ) %>% 
        dplyr::select( # select can take a vector of column indexes c(number 1, number 2, number 3:number 7 etc.) or column names
            STATEA, COUNTYA, TRACTA, BLOCKA, PLACEA, pop10b:minorityvap10b)
    return(NULL)
}

# by selecting the variables we need, we significantly reduce the file size of the data. 

# turn list into dataframe 
blocks2010 <- rbindlist(dat_use, use.names = TRUE)
rm(dat_use)

write_csv(blocks2010, "blocks2010_var_pt2.csv")

blocks2010 <- fread(file = "ipumsblocks_allstates/2010blocks/nhgis0036_ds172_2010_block.csv") 
blocks2010 %<>%
    select(c("STATEA", "COUNTYA", "TRACTA", "BLOCKA", "PLACEA", "H7W001":"IFF004"))
blocks2010 <- blocks2010[c(1000001:5000000), ]
f <- rep(seq_len(ceiling(nrow(blocks2010) / 100000)), each = 100000, length.out = nrow(blocks2010))
dat_use <- split(blocks2010, f = f)
rm(blocks2010)
rm(f)

# here's where we do processing on each chunk of the data 
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
            urbunits10b = (IFD002/IFD001)*100,
            hispvap10b = (H75002/H75001)*100,
            nhwvap10b = (H75005/H75001)*100,
            nhbvap10b = (H75006/H75001)*100,
            minorityvap10b = ((H75001 - H75005)/H75001)*100
        ) %>% 
        dplyr::select( # select can take a vector of column indexes c(number 1, number 2, number 3:number 7 etc.) or column names
            STATEA, COUNTYA, TRACTA, BLOCKA, PLACEA, pop10b:minorityvap10b)
    return(NULL)
}

# by selecting the variables we need, we significantly reduce the file size of the data. 

# turn list into dataframe 
blocks2010 <- rbindlist(dat_use, use.names = TRUE)
rm(dat_use)
write_csv(blocks2010, "blocks2010_var_pt3.csv")

blocks2010 <- fread(file = "ipumsblocks_allstates/2010blocks/nhgis0036_ds172_2010_block.csv") 
blocks2010 %<>%
    select(c("STATEA", "COUNTYA", "TRACTA", "BLOCKA", "PLACEA", "H7W001":"IFF004"))
blocks2010 <- blocks2010[c(5000001:10000000), ]
f <- rep(seq_len(ceiling(nrow(blocks2010) / 100000)), each = 100000, length.out = nrow(blocks2010))
dat_use <- split(blocks2010, f = f)
rm(blocks2010)
rm(f)

# here's where we do processing on each chunk of the data 
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
            urbunits10b = (IFD002/IFD001)*100,
            hispvap10b = (H75002/H75001)*100,
            nhwvap10b = (H75005/H75001)*100,
            nhbvap10b = (H75006/H75001)*100,
            minorityvap10b = ((H75001 - H75005)/H75001)*100
        ) %>% 
        dplyr::select( # select can take a vector of column indexes c(number 1, number 2, number 3:number 7 etc.) or column names
            STATEA, COUNTYA, TRACTA, BLOCKA, PLACEA, pop10b:minorityvap10b)
    return(NULL)
}

# by selecting the variables we need, we significantly reduce the file size of the data. 

# turn list into dataframe 
blocks2010 <- rbindlist(dat_use, use.names = TRUE)
rm(dat_use)
write_csv(blocks2010, "blocks2010_var_pt4.csv")

blocks2010 <- fread(file = "ipumsblocks_allstates/2010blocks/nhgis0036_ds172_2010_block.csv") 
blocks2010 %<>%
    select(c("STATEA", "COUNTYA", "TRACTA", "BLOCKA", "PLACEA", "H7W001":"IFF004"))
blocks2010 <- blocks2010[c(10000001:nrow(blocks2010)), ]
f <- rep(seq_len(ceiling(nrow(blocks2010) / 100000)), each = 100000, length.out = nrow(blocks2010))
dat_use <- split(blocks2010, f = f)
rm(blocks2010)
rm(f)

# here's where we do processing on each chunk of the data 
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
            urbunits10b = (IFD002/IFD001)*100,
            hispvap10b = (H75002/H75001)*100,
            nhwvap10b = (H75005/H75001)*100,
            nhbvap10b = (H75006/H75001)*100,
            minorityvap10b = ((H75001 - H75005)/H75001)*100
        ) %>% 
        dplyr::select( # select can take a vector of column indexes c(number 1, number 2, number 3:number 7 etc.) or column names
            STATEA, COUNTYA, TRACTA, BLOCKA, PLACEA, pop10b:minorityvap10b)
    return(NULL)
}

# by selecting the variables we need, we significantly reduce the file size of the data. 

# turn list into dataframe 
blocks2010 <- rbindlist(dat_use, use.names = TRUE)
rm(dat_use)

pt1 <- read_csv("blocks2010_var_pt1.csv")
pt2 <- read_csv("blocks2010_var_pt2.csv")
pt3 <- read_csv("blocks2010_var_pt3.csv")
pt4 <- read_csv("blocks2010_var_pt4.csv")
blocks2010 <- base::rbind(pt1, pt2, pt3, pt4, blocks2010)
rm(pt1, pt2, pt3, pt4)
write_csv(blocks2010, "blocks2010_var.csv")

# 2000 place-level data ####
places2000 <- read_csv(file = "seplaces_allstates/2000places.csv") # notice that place-level data is much smaller
poverty00 <- read_csv("seplaces_allstates/povt00.csv")
vap2000 <- read_csv("seplaces_allstates/vap2000.csv") 
# vap stands for voting-age population. 

names(places2000)
#names(places2000)[12:216]

#1. rename variables 
#names2000 <- c("poppl00", "pop2pl00", "popdensitypl00", "areapl00", "pop3pl00", "a5pl00", "a10pl00", "a15pl00", "a18pl00", "a20pl00", "a21pl00",
               # "a22pl00", "a25pl00", "a30pl00", "a35pl00", "a40pl00", "a45pl00", "a50pl00", "a55pl00", "a60pl00", "a62pl00", "a65pl00", "a67pl00",
               # "a70pl00", "a75pl00", "a80pl00", "a85pl00",
               # "pop4pl00", "nhtotalpl00", "nhwhitepl00", "nhblackpl00", "nhaianpl00", "nhasianpl00", "nhpipl00", "nhotherpl00", "nh2ppl00", 
               # "htotalpl00", "hwhitepl00", "hblackpl00", "haianpl00", "hasianpl00", "hpipl00", "hotherpl00", "h2ppl00",
               # "lfppl00", "lfparmedpl00", "lfpcivilianpl00", "mhincpl00", 
               # "blackpoppl00", "blackbelowpovpl00", "blackatabovepovpl00", 
               # "pop5pl00", "nativebornpl00", "foreignbornpl00", "naturalizedpl00", "notcitizenpl00",
               # "foreignborntotalpl00", "foreign9500pl00", "foreign9094pl00", "foreign8589pl00", "foreign8084pl00", "foreign7579pl00", "foreign7074pl00", "foreign6569pl00", "foreignb65pl00",
               # "foreignborntotal2pl00", "europepl00", "neuropepl00", "ukpl00", "irepl00", "swepl00", "othernepl00", 
               # "weuropepl00", "austriapl00", "frpl00", "gerpl00", "netherpl00", "otherwepl00", 
               # "seuropepl00", "greecepl00", "italypl00", "portpl00", "spainpl00", "othersepl00",
               # "eeuropepl00", "czechpl00", "hungpl00", "polpl00", "romaniapl00", "belaruspl00", "russiapl00", "ukrainepl00", "bosniahpl00", "yugopl00", "othereepl00", "othereuropepl00",
               # "asiapl00", "easiapl00", "chinapl00", "chinamainpl00", "hkpl00", "taiwanpl00", "japanpl00", "koreapl00", "othereapl00", 
               # "scasiapl00", "afghanpl00", "banglapl00", "indiapl00", "iranpl00", "pakistanpl00", "otherscapl00", 
               # "seasiapl00", "cambodiapl00", "indopl00", "laospl00", "malaypl00", "philippl00", "thaipl00", "vietpl00", "otherseapl00", 
               # "wasiapl00", "iraqpl00", "israelpl00", "jordanpl00", "lebanonpl00", "syriapl00", "turkeypl00", "armeniapl00", "otherwapl00", "otherasiapl00", 
               # "africapl00", "eafricapl00", "ethiopiapl00", "othereafrpl00", 
               # "mafricapl00", "norafricapl00", "egyptpl00", "othernorafricapl00", 
               # "safricapl00", "safpl00", "othersafpl00",
               # "wafricapl00", "ghanapl00", "nigeriapl00", "sierrapl00", "otherwafricapl00", "otherafricapl00", 
               # "oceaniapl00", "ausnzpl00", "auspl00", "otherausnzpl00", "melanisiapl00", "micronesiapl00", "polynesiapl00", "otheroceaniapl00", 
               # "americaspl00", "latampl00", "caribpl00", "barbadospl00", "cubapl00", "drpl00", "haitipl00", "jamaicapl00", "trintobpl00", "othercaribpl00", 
               # "centampl00", "mexicopl00", "othercentampl00", "costaricapl00", "elsavpl00", "guatpl00", "honduraspl00", "nicarpl00", "panamapl00", "othercentam2pl00", 
               # "sampl00", "argentinapl00", "boliviapl00", "brazilpl00", "chilepl00", "colompl00", "ecuapl00", "guyanapl00", "perupl00", "venezpl00", "othersampl00",
               # "norampl00", "canpl00", "othernorpl00", "bornatseapl00")

# names(places2000)[12:202] <- names2000 

# left_join is how we merge datasets together. 
?left_join #? + the command name is how you get more information on the command. 
# here, you'll find documentation on how left_join and right_join etc. are different from each other. 
# when joining data, you want to specify the by-variable. i.e. how are the two datasets matched to each other?
# there are other tricks/issues with using left_join but we won't address them here. the main one is 
# avoiding having duplicate variables across datasets. That's why I use select to only keep variables 
# I know don't repeat, and keep only the by-variable across both. You can also match by more than one variable, 
# it would look like by = c("A", "B", "C")
# here, we're joining place dataset A to place dataset B to place dataset C, so we are joining by the place identifier. 
# we need a unique ID for each place, i.e. plid, which is a 7-digit number generated 
# from State (2-digit) and Place (5-digit) IDs 
# we use string pad because we need the ids to be 7-digits for maximum uniqueness 
# but sometimes the data is formatted such that state 01 shows up as 1, so we need a leading 0 in those cases

places2000 <- places2000 %>%
  mutate(STATE = str_pad(Geo_STATE, 2, side = "left", pad = "0"),
         PLACE = str_pad(Geo_PLACE, 5, side = "left", pad = "0"), 
         plid = paste0(STATE, PLACE))

poverty00 <- poverty00 %>%
  mutate(STATE = str_pad(Geo_STATE, 2, side = "left", pad = "0"),
         PLACE = str_pad(Geo_PLACE, 5, side = "left", pad = "0"), 
         plid = paste0(STATE, PLACE))

vap2000 <- vap2000 %>%
  mutate(STATE = str_pad(Geo_STATE, 2, side = "left", pad = "0"),
         PLACE = str_pad(Geo_PLACE, 5, side = "left", pad = "0"), 
         plid = paste0(STATE, PLACE))

places2000 <- places2000 %>%
  left_join(poverty00 %>% select(plid, SE_T187_001:PCT_SE_T194_003), by = "plid") %>%
  left_join(vap2000 %>% select(plid, SF1_P006001:SF1_P006073), by = "plid")

# 2. create variables needed 
places2000 <- places2000 %>%
  mutate(pop00p = SE_T001_001, 
         pcturb00p = (SE_T002_002/pop00p)*100, 
         pctrur00p = 100-pcturb00p,
         popdensity00p = SE_T003_001,
         nhblack00p = SE_T015_004, 
         nhwhite00p = SE_T015_003, 
         h00p = SE_T015_010, 
         min00p = rowSums(places2000 %>% select(SE_T015_004:SE_T015_010)), 
         pctnhblack00p = (nhblack00p/pop00p) * 100,
         pctnhwhite00p = (nhwhite00p/pop00p) * 100, 
         pcth00p = (h00p/pop00p) * 100, 
         pctmin00p = (min00p/pop00p) * 100, 
         pctrecimm00p = ((SE_T202_003 + SE_T202_002) / pop00p) * 100,
         hinc00p = SE_T093_001, 
         blackpov00p = PCT_SE_T187_002,
         hpov00p = PCT_SE_T193_002, 
         nhwhitepov00p = PCT_SE_T194_002, 
         minpov00p = ((SE_T187_002 + SE_T188_002 + SE_T189_002 + SE_T190_002 + SE_T191_002 + SE_T192_002 + SE_T193_002)/(SE_T187_001 + SE_T188_001 + SE_T189_001 + SE_T190_001 + SE_T191_001 + SE_T192_001 + SE_T193_001))*100,
         nhwhitevap00p = (SF1_P006005/SF1_P006001)*100, #nh stands for non-Hispanic
         nhblackvap00p = (SF1_P006006/SF1_P006001)*100,
         hispvap00p = (SF1_P006002/SF1_P006001)*100,
         minvap00p = ((SF1_P006001 - SF1_P006005)/SF1_P006001)*100) %>%
  select(Geo_QName, plid, pop00p:minvap00p)

rm(poverty00, vap2000)
write_csv(places2000, "pl2000_cleaned.csv")

# 1990 place-level data ####
places1990 <- read_csv(file = "seplaces_allstates/1990places.csv")
vap1990 <- read_csv(file = "seplaces_allstates/vap1990.csv")

places1990 <- places1990 %>%
  mutate(STATE = str_pad(Geo_STATE, 2, side = "left", pad = "0"),
         PLACE = str_pad(Geo_PLACECE, 5, side = "left", pad = "0"), 
         plid = paste0(STATE, PLACE))

vap1990 <- vap1990 %>%
  mutate(STATE = str_pad(Geo_STATE, 2, side = "left", pad = "0"),
         PLACE = str_pad(Geo_PLACECE, 5, side = "left", pad = "0"), 
         plid = paste0(STATE, PLACE))

places1990 <- places1990 %>%
  left_join(vap1990 %>% select(plid, STF3_P014_001:STF3_P015_065), by = "plid")

# we only want places that existed in both 1990 and 2000 otherwise we cannot make time-lagged variables 
# but we can't join 1990 to 2000 by plid because place IDs changed between 1990-2000. Place name is a better bet. 
places1990 <- places1990 %>%
  rename("plid90" = "plid") %>%
  filter(Geo_QName %in% places2000$Geo_QName)

# places1990 <- places1990[places1990$plid %in% places2000$plid, ] # just to let you know how 
# doing this would look like in base r--as you can see, it can get very annoying with more variables 
# and more operations, because you always have to type df$ to refer to each variable

# 1. get names 
# names1990 <- c("Geo_Name", "Geo_QName", "Geo_SUMLEV90", "Geo_GEOCOMP90", "Geo_REGION90", "Geo_DIVISION90", "Geo_FIPS90", "Geo_STATE", "Geo_PLACECE", 
#                "pop90", "pop902", "popdensity90", "landarea90", "pop903", "urban90", "inurban90", "outurban90", "rural90", "farm90", "nonfarm90", 
#                "pop904", "nh90", "nhwhite90", "nhblack90", "nhaian90", "nhapi90", "nhother90", "h90", "hwhite90", "hblack90", "haian90", "hapi90", "hother90", 
#                "pop905", "nothorigin90", "horigin90", "mexican90", "pr90", "cuban90", "otherhorigin90", "dr90", "centam90", 
#                "guatemalan90", "honduran90", "nica90", "panama90", "salvador90", "othercentam90", "sam90", "colombia90", "ecuador90", 
#                "peru90", "othersam90", "otherhisp90", "lfp90", "inlfp90", "armed90", "civilian90", 
#                "households90", "r1", "r2", "r3", "r4", "r5", "r6", "r7", "r8", "r9", "r10", "r11", "r12", "r13", "r14", "r15", "r16", "r17", "r18", "r19", "r20", 
#                "r21", "r22", "r23", "r24", "r25", "r26", "hinc90", "povpop90", "notpov90", "wnotpov90", "bnotpov90", "aiannotpov90", "apinotpov90", "othernotpov90", 
#                "inpov90", "winpov90", "binpov90", "aianinpov90", "apiinpov90", "otherinpov90", "pop906", "native90", "fborn90", "naturalized90", "noncit90", 
#                "fborn809090", "fborn707990", "fborn606990", "fbornb6090")
# names(places1990) <- names1990

# 2. make variables 
cpi <- 1.36 #1990$ in 2000$ value. cpi is the inflation coefficient. 

places1990 <- places1990 %>%
  mutate(pop90p = SE_T001_001, 
         pcturb90p = (SE_T004_002/pop90p)*100, 
         pctrur90p = 100-pcturb90p,
         popdensity90p = SE_T002_001,
         nhblack90p = SE_T013_004, 
         nhwhite90p = SE_T013_003, 
         h90p = SE_T013_008, 
         min90p = rowSums(across(c(SE_T013_004:SE_T013_008))), 
         pctnhblack90p = (nhblack90p/pop90p) * 100,
         pctnhwhite90p = (nhwhite90p/pop90p) * 100, 
         pcth90p = (h90p/pop90p) * 100, 
         pctmin90p = (min90p/pop90p) * 100, 
         pctrecimm90p = (SE_T111_002 / pop90p) * 100,
         hinc90pinf = SE_T043_001*cpi, 
         whitepov90p = (SE_T099_009)/(SE_T099_003 + SE_T099_009)*100,
         blackpov90p = (SE_T099_010/(SE_T099_004 + SE_T099_010))*100,
         minpov90p = ((SE_T099_008 - SE_T099_009)/((SE_T099_002-SE_T099_003) + (SE_T099_008 - SE_T099_009)))*100,
         popover18 = rowSums(across(c(STF3_P014_016:STF3_P014_034, STF3_P014_048:STF3_P014_066, 
                                      STF3_P014_081:STF3_P014_099, STF3_P014_113:STF3_P014_131,
                                      STF3_P014_146:STF3_P014_164, STF3_P014_178:STF3_P014_196,
                                      STF3_P014_211:STF3_P014_229, STF3_P014_243:STF3_P014_261,
                                      STF3_P014_276:STF3_P014_294, STF3_P014_308:STF3_P014_326))),
         nhwhitevap90p = (rowSums(across(c(STF3_P014_016:STF3_P014_034, STF3_P014_048:STF3_P014_066)))/popover18)*100, 
         nhblackvap90p = (rowSums(across(c(STF3_P014_081:STF3_P014_099, STF3_P014_113:STF3_P014_131)))/popover18)*100, 
         hispvap90p = (rowSums(across(c(STF3_P015_015:STF3_P015_033, STF3_P015_047:STF3_P015_065)))/popover18)*100,
         minvap90p = ((popover18-rowSums(across(c(STF3_P014_016:STF3_P014_034, STF3_P014_048:STF3_P014_066))))/popover18)*100)

pl9000 <- 
  left_join(
    places1990 %>% select(
  c(Geo_QName, plid90, pop90p:minvap90p)), 
  places2000 %>% select(
  c(Geo_QName, plid, popdensity00p, pop00p:minvap00p)), 
  by = "Geo_QName")

# have to remove duplicates--this is an issue I can't get around. There are duplicated places in the 1990 data
# see:
nrow(places1990) #22551
length(unique(places1990$Geo_QName)) #22526 

# those duplicated places are not analyzable because they don't have a stable unique place ID 
pl9000 <- pl9000 %>%
  filter(!duplicated(Geo_QName))

# make change variables 
pl9000 <- 
  mutate(pl9000, 
         popgrowth = ((pop00p-pop90p)/pop90p) * 100,
         urbanization = (pcturb00p - pcturb90p),
         ruralization = (pctrur00p - pctrur90p),
         densification = (popdensity00p - popdensity90p),
         nhwhitegrowth = ((nhwhite00p-nhwhite90p)/nhwhite90p) * 100, 
         nhblackgrowth = ((nhblack00p-nhblack90p)/nhblack90p) * 100,
         hgrowth = ((h00p-h90p)/h90p) * 100,
         mingrowth = ((min00p-min90p)/min90p) * 100,
         recimmgrowth = (pctrecimm00p - pctrecimm90p),
         incomegrowth = ((hinc00p - hinc90pinf)/hinc00p)*100, 
         blackpovgrowth = (blackpov00p - blackpov90p),
         whitepovgrowth = (nhwhitepov00p - whitepov90p),
         minpovgrowth = (minpov00p - minpov90p), 
         nhwhitevapgrowth = nhwhitevap00p - nhwhitevap90p,
         nhblackvapgrowth = nhblackvap00p - nhblackvap90p,
         hispvapgrowth = hispvap00p - hispvap90p,
         minvapgrowth = minvap00p - minvap90p)

write_csv(pl9000, "pl9000_var.csv")
rm(places1990, places2000, pl9000, vap1990)

# 2010 places ####
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
cpi <- 1.38

places2010 <- places2010 %>%
  mutate(pop10p = SE_A00001_001, 
         pcturb10p = NA,
         pctrur10p = NA,
         popdensity10p = SE_A00002_002,
         nhblack10p = SE_A04001_004, 
         nhwhite10p = SE_A04001_003, 
         h10p = SE_A04001_010, 
         min10p = (pop10p-nhwhite10p), 
         pctnhblack10p = (nhblack10p/pop10p) * 100,
         pctnhwhite10p = (nhwhite10p/pop10p) * 100, 
         pcth10p = (h10p/pop10p) * 100, 
         pctmin10p = (min10p/pop10p) * 100, 
         pctrecimm10p = (SE_A10058_002 / pop10p) * 100,
         hinc10p = SE_A14006_001, 
         whitepov10p = (SE_A13001I_002/SE_A13001I_001)*100,
         blackpov10p = (SE_A13001B_002/SE_A13001B_001)*100,
         hpov10p = (SE_A13001H_002/SE_A13001H_001)*100,
         minpov10p =  ((hpov10p + blackpov10p + SE_A13001G_002 + SE_A13001F_002 + SE_A13001E_002 + SE_A13001D_002 + SE_A13001C_002)/(SE_A13001B_001 + SE_A13001B_001 + SE_A13001G_001 + SE_A13001F_001 + SE_A13001E_001 + SE_A13001D_001 + SE_A13001C_001))*100,
         popover18 = SF1_P0110001,
         nhwhitevap10p = (SF1_P0110005/popover18)*100,
         nhblackvap10p = (SF1_P0110006/popover18)*100,
         hispvap10p = (SF1_P0110002/popover18)*100,
         minvap10p = ((popover18-SF1_P0110005)/popover18)*100)

length(unique(places2000$plid))
length(unique(places2010$plid))

table(unique(places2000$plid) %in% unique(places2010$plid))

pl0010 <- 
  left_join(
    places2000 %>% select(
      c(plid, pop00p:minvap00p)), 
    places2010 %>% select(
      c(plid, Geo_COUNTY, pop10p:minvap10p)), 
    by = "plid")

pl0010 <- 
  mutate(pl0010, 
         popgrowth = ((pop10p-pop00p)/pop00p) * 100,
         densification = (popdensity10p - popdensity00p),
         nhwhitegrowth = ((nhwhite10p-nhwhite00p)/nhwhite00p) * 100, 
         nhblackgrowth = ((nhblack10p-nhblack00p)/nhblack00p) * 100,
         hgrowth = ((h10p-h00p)/h00p) * 100,
         mingrowth = ((min10p-min00p)/min00p) * 100,
         recimmgrowth = (pctrecimm10p - pctrecimm00p),
         incomegrowth = ((hinc10p - hinc00p*cpi)/hinc10p)*100, 
         blackpovgrowth = (blackpov10p - blackpov00p),
         whitepovgrowth = (whitepov10p - nhwhitepov00p),
         hpovgrowth = (hpov10p - hpov00p),
         minpovgrowth = (minpov10p - minpov00p), 
         nhwhitevapgrowth = nhwhitevap10p - nhwhitevap00p,
         nhblackvapgrowth = nhblackvap10p - nhblackvap00p,
         hispvapgrowth = hispvap10p - hispvap00p,
         minvapgrowth = minvap10p - minvap00p)

write_csv(pl0010, "pl0010_var.csv")
rm(list = ls())

# 2013 ACS ####
acs13 <- read_csv("seplaces_allstates/2013places.csv")
acs13 %<>%
    rename("nhwhite13p" = "PCT_SE_A04001_003",
           "nhblack13p" = "PCT_SE_A04001_004",
           "h13p" = "PCT_SE_A04001_010") %>%
    mutate(min13p = 100-nhwhite13p,
           STATE = str_pad(Geo_STATE, 2, side = "left", pad = "0"),
           PLACE = str_pad(Geo_PLACE, 5, side = "left", pad = "0"), 
           plid = paste0(STATE, PLACE)) %>%
    select(c(plid, Geo_QName, contains("13p")))

write_csv(acs13, "acs13.csv")

# 2020 census ####
places2020 <- read_csv("seplaces_allstates/2020places.csv")
places2020 %<>% 
    rename("Geo_QName" = "Geo_QNAME") %>%
    mutate(pop20p = SE_T003_001, 
           nhwhite20p = SE_T004_003,
           nhblack20p = SE_T004_005,
           h20p = SE_T004_017,
           min20p = 100-nhwhite20p,
           STATE = str_pad(Geo_STATE, 2, side = "left", pad = "0"),
           PLACE = str_pad(Geo_PLACE, 5, side = "left", pad = "0"), 
           plid = paste0(STATE, PLACE)) %>%
    select(c(plid, Geo_QName, contains("20p")))

write_csv(places2020, "places2020_var.csv")

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
            hispvap20b = (U7E002/U7E001)*100,
            nhwvap20b = (U7E005/U7E001)*100,
            nhbvap20b = (U7E006/U7E001)*100,
            minorityvap20b = ((U7E001 - U7E005)/U7E001)*100,
            vacancy20b = (U7G003/U7G001)*100
        ) %>% 
        dplyr::select( # select can take a vector of column indexes c(number 1, number 2, number 3:number 7 etc.) or column names
            STATEA, COUNTYA, TRACTA, BLOCKA, PLACEA, pop20b:minorityvap20b)
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
    select(blkid, pop20b, pctnhblack20b, pctnhwhite20b, pcth20b, pctmin20b,
           hispvap20b, nhwvap20b, nhbvap20b, minorityvap20b, vacancy20b) 

names(blocks2020) <- gsub("20b", "", names(blocks2020))
blocks2020 %<>%
    mutate(Year = "2020")

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
    select(blkid, pop10b, pctnhblack10b, pctnhwhite10b, pcth10b, pctmin10b,
           hispvap10b, nhwvap10b, nhbvap10b, minorityvap10b, vacancy10b) 

names(blocks2010) <- gsub("10b", "", names(blocks2010))
blocks2010 %<>%
    mutate(Year = "2010")

# blocks2000 %<>%
#     filter((blkid %in% blocks2010$blkid) & (blkid %in% blocks2020$blkid))
# 
# blocks2010 %<>%
#     filter(blkid %in% blocks2000$blkid)
# 
# blocks2020 %<>%
#     filter(blkid %in% blocks2000$blkid)

blocks <- base::rbind(blocks2000, blocks2010, blocks2020)
rm(blocks2000, blocks2010, blocks2020)

blocks2013 <- read_csv("blocks2000_var.csv")
blocks2013 %<>%
    select(blkid, pop00b, pctnhblack00b, pctnhwhite00b, pcth00b, pctmin00b,
           hispvap00b, nhwvap00b, nhbvap00b, minorityvap00b, vacancy00b) 
names(blocks2013) <- gsub("00b", "", names(blocks2013))
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
    mutate_at(c(names(blocks)[2:6]), zoo::na.approx, na.rm = F) %>%
    ungroup()

blocks13 %<>%
    filter(Year=="2013")

write_csv(blocks13, "blocks2013_int.csv")
rm(blocks13)

# do 2014 
blocks2014 <- read_csv("blocks2000_var.csv")
blocks2014 %<>%
    select(blkid, pop00b, pctnhblack00b, pctnhwhite00b, pcth00b, pctmin00b,
           hispvap00b, nhwvap00b, nhbvap00b, minorityvap00b, vacancy00b) 
names(blocks2014) <- gsub("00b", "", names(blocks2014))
blocks2014 %<>%
    mutate(Year = "2014")

blocks2014 %<>%
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

blocks <- base::rbind(blocks, blocks2014)
rm(blocks2014)
blocks %<>%
    mutate(Year = as.numeric(as.character(Year))) 

blocks %<>%
    group_by(blkid) %>%
    arrange(Year) %>%
    mutate_at(c(names(blocks)[2:6]), zoo::na.approx, na.rm = F) %>%
    ungroup()

blocks14 <- blocks %>%
    filter(Year == "2014")
write_csv(blocks14, "blocks2014_int.csv")

rm(blocks)
rm(blocks14)
