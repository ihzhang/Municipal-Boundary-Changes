# clean block and place data before identifying annexed blocks ##############
# Purpose of this script is to create necessary variables of interest #######
# for place90, place00, block00 and block10 datasets before doing ###########
# analysis outlined in BAS0010. This is script step ONE. ####################


rm(list = ls())
setwd("~/Google Drive/Stanford/QE2")

library("tidyverse")
library("readr")
library("stargazer")
library("foreach")
library("data.table")

# block 00 ####
blocks2000 <- read_csv(file = "ipumsblocks_allstates/2000blocks/nhgis0032_ds147_2000_block.csv")
blocks2000 <- blocks2000[-1,]

# 1. rename variables
names2000 <- c("nhwhite", "nhblack", "nhaian", "nhasian", "nhpi",
               "nhother", "nh2p", "hwhite", "hblack", "haian", "hasian",
               "hpi", "hother", "h2p",
               "m5", "m5to9", "m10to14", "m15to17", "m18to19", "m20", "m21",
               "m22to24", "m25to29", "m30to34", "m35to39", "m40to44", "m45to49", 
               "m50to54", "m55to59", "m60to61", "m62to64", "m65to66", "m67to69",
               "m70to74", "m75to79", "m80to84", "m85p",
               "f5", "f5to9", "f10to14", "f15to17", "f18to19", "f20", "f21",
               "f22to24", "f25to29", "f30to34", "f35to39", "f40to44", "f45to49", 
               "f50to54", "f55to59", "f60to61", "f62to64", "f65to66", "f67to69",
               "f70to74", "f75to79", "f80to84", "f85p",
               "hu", "ownerocc", "renterocc")
names(blocks2000)[14:76] <- names2000 # we can use the names() -- it returns a vector, which we index with [] notation, to rename variables

# 2. convert factorized numerical variables back to numerical
thesecolumns <- c(14:76)

f <- rep(seq_len(ceiling(nrow(blocks2000) / 100000)), each = 100000, length.out = nrow(blocks2000))
dat_use <- split(blocks2000, f = f)
rm(blocks2000)
rm(f)

# 
foreach (i = 1:length(dat_use)) %do% {
  dat_use[[i]] <- dat_use[[i]] %>%
    mutate_at(thesecolumns, ~as.numeric(as.character(.))) %>%
      mutate( 
             pop00b = rowSums(across(14:27), na.rm = T),
             nhblack00b = nhblack,
             nhwhite00b = nhwhite, 
             h00b = rowSums(across(21:27), na.rm = T),
             min00b = rowSums(across(15:27), na.rm = T),
             pctnhblack00b = (nhblack00b/pop00b)*100,
             pctnhwhite00b = (nhwhite00b/pop00b)*100, 
             pcth00b = (h00b/pop00b)*100, 
             pctmin00b = (min00b/pop00b)*100, 
             dependants00b = rowSums(across(c(28:31, 51:54, 45:50, 68:73), na.rm = T)),
             workingage00b = rowSums(across(c(32:44, 55:67), na.rm = T)), 
             dependencyratio00b = dependants00b/workingage00b,
             pctowneroccupied = (ownerocc/hu)*100) %>% 
    dplyr::select(
               GISJOIN, STATEA, COUNTYA, TRACTA, BLOCKA, PLACEA, pop00b, nhblack00b, nhwhite00b, h00b, min00b, pctnhblack00b, pctnhwhite00b, pcth00b, pctmin00b, dependencyratio00b, pctowneroccupied, hu)
  return(NULL)
}

blocks2000 <- dat_use[[1]]
foreach (i = 2:length(dat_use)) %do% {
  blocks2000 <- bind_rows(blocks2000, 
                   dat_use[[i]])
  return(NULL)
}
blocks2000 <- rbindlist(dat_use, use.names = TRUE)
rm(dat_use)

write_csv(blocks2000, "blocks2000_var.csv")

# place 00 ####
places2000 <- read_csv(file = "seplaces_allstates/2000places.csv")
poverty00 <- read_csv("seplaces_allstates/povt00.csv")

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
places2000 <- places2000 %>%
  left_join(poverty00 %>% select(Geo_QName, SE_T187_001:PCT_SE_T194_003), by = "Geo_QName")

# cdp00 <- places2000 %>%
#   filter(str_detect(Geo_QName, "CDP")) %>%
#   select(c(Geo_QName, Geo_STATE, Geo_PLACE)) %>%
#   mutate(STATEA = sprintf("%02.0f", Geo_STATE), 
#          PLACEA = sprintf("%05.0f", Geo_PLACE),
#          plid = paste0(STATEA, PLACEA))

# 2. create variables needed 
places2000 <- 
  mutate(places2000, 
         pop00p = SE_T001_001, 
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
         nhwhitevap00p = ,
         nhblackvap00p = ,
         hispvap00p = ,
         minvap00p = ,)

rm(poverty00)

# place 90 ####
places1990 <- read.csv(file = "seplaces_allstates/1990places.csv", sep = ",", header = T, na = "")
places1990$Geo_QName <- as.character(places1990$Geo_QName)
places2000$Geo_QName <- as.character(places2000$Geo_QName)

# we only want places that existed in 1990 otherwise we cannot make time-lagged variables 
places1990 <- places1990[places1990$Geo_QName %in% places2000$Geo_QName, ]

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
cpi <- 1.36 #1990$ in 2000$ value 

places1990 <- places1990 %>%
  mutate(places1990, 
         pop90p = SE_T001_001, 
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
         nhwhitevap90p = , 
         nhblackvap90p = , 
         hispvap90p = ,
         minvap90p = )

pl9000 <- 
  left_join(
    places1990 %>% select(
  c(Geo_QName, pop90p:minpov90p)), 
  places2000 %>% select(
  c(Geo_QName, Geo_STATE, Geo_PLACE, popdensity00p, pop00p:minpov00p)), 
  by = "Geo_QName")

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
         nhwhitevapgrowth = ,
         nhblackvapgrowth = ,
         hispvapgrowth = ,
         minvapgrowth = )

write_csv(pl9000, "pl9000_var.csv")
rm(places1990, places2000, names1990, names2000)

pl9000_var <- read_csv("pl9000_var.csv")

# get vap data for places in 2000 ####
vap2000 <- read_csv("vap/places_2000.csv")
vap2000 <- vap2000 %>%
  mutate(vap = SF1_P006001,
         hispvap = SF1_P006002,
         nhwvap = SF1_P006005,
         nhbvap = SF1_P006006,
         minorityvap = vap - nhwvap,
         plid = paste0(Geo_STATE, Geo_PLACE)) %>%
  select(plid, vap, hispvap, nhwvap, nhbvap, minorityvap) %>%
  mutate(hispvap00p = (hispvap/vap)*100,
         nhwvap00p = (nhwvap/vap)*100,
         nhbvap00p = (nhbvap/vap)*100,
         minorityvap00p = (minorityvap/vap)*100) %>%
  select(plid, hispvap00p, nhwvap00p, nhbvap00p, minorityvap00p)

write_csv(vap2000, "vap2000.csv")

pl9000_var <- pl9000_var %>% 
  mutate(Geo_STATE = str_pad(Geo_STATE, 2, side = "left", pad = "0"),
         Geo_PLACE = str_pad(Geo_PLACE, 5, side = "left", pad = "0"),
         plid2 = paste0(Geo_STATE, Geo_PLACE)) %>%
  left_join(vap2000, by = c("plid2" = "plid"))

# 2000 block ####
vap2000block <- read_csv("vap/blocks_2000.csv")

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

write_csv(vap2000block, "vap2000block.csv")



# # create the 2000-2010 white dataset ####
# pl2010 <- read_csv("seplaces_allstates/2010places.csv")
# pl2010 <- pl2010 %>%
#   mutate(pctnhwhite10 = PCT_SE_T055_003,
#          pctnhblack10 = PCT_SE_T055_004,
#          pcth10 = PCT_SE_T055_010,
#          pctmin10 = ((SE_T055_004 + SE_T055_005 + SE_T055_006 + SE_T055_007 + SE_T055_008 + SE_T055_009 + SE_T055_010)/SE_T055_001) * 100,
#          plid = paste0(Geo_STATE, Geo_PLACE))
# 
# pl9000 <- read_csv("pl9000_var.csv")
# pl9000 <- pl9000 %>%
#   mutate(plid = paste0(sprintf("%02.0f", Geo_STATE),
#                        sprintf("%05.0f", Geo_PLACE)))
# 
# nhwhite9010 <- left_join(pl2010 %>% select(plid, pctnhwhite10:pctmin10),
#                          pl9000 %>% select(plid, pctnhwhite00p, nhwhite00p, nhwhitegrowth, pcth00p, h00p, hgrowth, pctmin00p, min00p, mingrowth, pctnhblack00p, nhblack00p, nhblackgrowth, Geo_STATE))
# 
# write_csv(nhwhite9010, "nhwhite9010.csv")
# rm(vap2000)
