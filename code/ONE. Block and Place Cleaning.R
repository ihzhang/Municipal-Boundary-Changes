# clean block and place data before identifying annexed blocks ##############
# Purpose of this script is to create necessary variables of interest #######
# for place90, place00, block00 and block10 datasets before doing ###########
# analysis outlined in BAS0010. This is script step ONE. ####################
rm(list = ls())
setwd("~/Google Drive/Stanford/QE2")

library("stringr")
library("dplyr")
library("stargazer")
library("tidyverse")
library("tidycensus")
library("data.table")

# block 00 ####
blocks2000 <- read.csv(file = "ipumsblocks_allstates/2000blocks/nhgis0012_ds147_2000_block.csv", sep = ",", header = T, na = "")
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
names(blocks2000)[14:76] <- names2000

# 2. convert factorized numerical variables back to numerical
thesecolumns <- c(14:76)
blocks2000[, thesecolumns] <- apply(blocks2000[, thesecolumns], 2, function(x) as.numeric(as.character(as.factor(x))))

# 3. create percentages for various racial proportions
blocks2000 <- 
  mutate(blocks2000, 
         pop00b = rowSums(blocks2000[, c(14:27)]),
         nhblack00b = nhblack,
         nhwhite00b = nhwhite, 
         h00b = rowSums(blocks2000[, c(21:27)]),
         min00b = blocks2000$min <- rowSums(blocks2000[, c(15:27)]),
         pctnhblack00b = (nhblack00b/pop00b)*100,
         pctnhwhite00b = (nhwhite00b/pop00b)*100, 
         pcth00b = (h00b/pop00b)*100, 
         pctmin00b = (min00b/pop00b)*100, 
         dependants00b = rowSums(blocks2000[, c(28:31, 51:54, 45:50, 68:73)]),
         workingage00b = rowSums(blocks2000[c(32:44, 55:67)]), 
         dependencyratio00b = dependants00b/workingage00b,
         pctowneroccupied = (ownerocc/hu)*100)

blocks2000 <- 
  blocks2000 %>% select(
    GISJOIN, STATEA, PLACEA, pop00b, nhblack00b, nhwhite00b, h00b, min00b, pctnhblack00b, pctnhwhite00b, pcth00b, pctmin00b, dependencyratio00b, pctowneroccupied
  )

write_csv(blocks2000, "blocks2000_var.csv")

# place 00 ####
places2000 <- read.csv(file = "seplaces_allstates/2000places.csv", sep = ",", header = T, na = "")
poverty00 <- read_csv("seplaces_allstates/povt00.csv")

names(places2000)
names(places2000)[12:202]

#1. rename variables 
names2000 <- c("poppl00", "pop2pl00", "popdensitypl00", "areapl00", "pop3pl00", "a5pl00", "a10pl00", "a15pl00", "a18pl00", "a20pl00", "a21pl00",
               "a22pl00", "a25pl00", "a30pl00", "a35pl00", "a40pl00", "a45pl00", "a50pl00", "a55pl00", "a60pl00", "a62pl00", "a65pl00", "a67pl00",
               "a70pl00", "a75pl00", "a80pl00", "a85pl00",
               "pop4pl00", "nhtotalpl00", "nhwhitepl00", "nhblackpl00", "nhaianpl00", "nhasianpl00", "nhpipl00", "nhotherpl00", "nh2ppl00", 
               "htotalpl00", "hwhitepl00", "hblackpl00", "haianpl00", "hasianpl00", "hpipl00", "hotherpl00", "h2ppl00",
               "lfppl00", "lfparmedpl00", "lfpcivilianpl00", "mhincpl00", 
               "blackpoppl00", "blackbelowpovpl00", "blackatabovepovpl00", 
               "pop5pl00", "nativebornpl00", "foreignbornpl00", "naturalizedpl00", "notcitizenpl00",
               "foreignborntotalpl00", "foreign9500pl00", "foreign9094pl00", "foreign8589pl00", "foreign8084pl00", "foreign7579pl00", "foreign7074pl00", "foreign6569pl00", "foreignb65pl00",
               "foreignborntotal2pl00", "europepl00", "neuropepl00", "ukpl00", "irepl00", "swepl00", "othernepl00", 
               "weuropepl00", "austriapl00", "frpl00", "gerpl00", "netherpl00", "otherwepl00", 
               "seuropepl00", "greecepl00", "italypl00", "portpl00", "spainpl00", "othersepl00",
               "eeuropepl00", "czechpl00", "hungpl00", "polpl00", "romaniapl00", "belaruspl00", "russiapl00", "ukrainepl00", "bosniahpl00", "yugopl00", "othereepl00", "othereuropepl00",
               "asiapl00", "easiapl00", "chinapl00", "chinamainpl00", "hkpl00", "taiwanpl00", "japanpl00", "koreapl00", "othereapl00", 
               "scasiapl00", "afghanpl00", "banglapl00", "indiapl00", "iranpl00", "pakistanpl00", "otherscapl00", 
               "seasiapl00", "cambodiapl00", "indopl00", "laospl00", "malaypl00", "philippl00", "thaipl00", "vietpl00", "otherseapl00", 
               "wasiapl00", "iraqpl00", "israelpl00", "jordanpl00", "lebanonpl00", "syriapl00", "turkeypl00", "armeniapl00", "otherwapl00", "otherasiapl00", 
               "africapl00", "eafricapl00", "ethiopiapl00", "othereafrpl00", 
               "mafricapl00", "norafricapl00", "egyptpl00", "othernorafricapl00", 
               "safricapl00", "safpl00", "othersafpl00",
               "wafricapl00", "ghanapl00", "nigeriapl00", "sierrapl00", "otherwafricapl00", "otherafricapl00", 
               "oceaniapl00", "ausnzpl00", "auspl00", "otherausnzpl00", "melanisiapl00", "micronesiapl00", "polynesiapl00", "otheroceaniapl00", 
               "americaspl00", "latampl00", "caribpl00", "barbadospl00", "cubapl00", "drpl00", "haitipl00", "jamaicapl00", "trintobpl00", "othercaribpl00", 
               "centampl00", "mexicopl00", "othercentampl00", "costaricapl00", "elsavpl00", "guatpl00", "honduraspl00", "nicarpl00", "panamapl00", "othercentam2pl00", 
               "sampl00", "argentinapl00", "boliviapl00", "brazilpl00", "chilepl00", "colompl00", "ecuapl00", "guyanapl00", "perupl00", "venezpl00", "othersampl00",
               "norampl00", "canpl00", "othernorpl00", "bornatseapl00")

names(places2000)[12:202] <- names2000 
places2000 <- places2000 %>%
  left_join(poverty00 %>% select(Geo_QName, SE_T187_001:PCT_SE_T194_003), by = "Geo_QName")

cdp00 <- places2000 %>%
  filter(str_detect(Geo_QName, "CDP")) %>%
  select(c(Geo_QName, Geo_STATE, Geo_PLACE)) %>%
  mutate(STATEA = sprintf("%02.0f", Geo_STATE), 
         PLACEA = sprintf("%05.0f", Geo_PLACE),
         plid = paste0(STATEA, PLACEA))

# 2. create variables needed 
places2000 <- 
  mutate(places2000, 
         pop00p = poppl00, 
         nhblack00p = nhblackpl00, 
         nhwhite00p = nhwhitepl00, 
         h00p = htotalpl00, 
         min00p = rowSums(places2000 %>% select(nhblackpl00:htotalpl00)), 
         pctnhblack00p = (nhblack00p/pop00p) * 100,
         pctnhwhite00p = (nhwhite00p/pop00p) * 100, 
         pcth00p = (htotalpl00/pop00p) * 100, 
         pctmin00p = (min00p/pop00p) * 100, 
         pctrecimm00p = ((foreign9094pl00 + foreign9500pl00) / pop00p) * 100,
         hinc00p = mhincpl00, 
         blackpov00p = PCT_SE_T187_002,
         hpov00p = PCT_SE_T193_002, 
         nhwhitepov00p = PCT_SE_T194_002, 
         minpov00p = ((SE_T187_002 + SE_T188_002 + SE_T189_002 + SE_T190_002 + SE_T191_002 + SE_T192_002 + SE_T193_002)/(SE_T187_001 + SE_T188_001 + SE_T189_001 + SE_T190_001 + SE_T191_001 + SE_T192_001 + SE_T193_001))*100)

# place 90 ####
places1990 <- read.csv(file = "seplaces_allstates/1990places.csv", sep = ",", header = T, na = "")
places1990$Geo_QName <- as.character(places1990$Geo_QName)
places2000$Geo_QName <- as.character(places2000$Geo_QName)

# we only want places that existed in 1990 otherwise we cannot make time-lagged variables 
places1990 <- places1990[places1990$Geo_QName %in% places2000$Geo_QName, ]

# 1. get names 
names1990 <- c("Geo_Name", "Geo_QName", "Geo_SUMLEV90", "Geo_GEOCOMP90", "Geo_REGION90", "Geo_DIVISION90", "Geo_FIPS90", "Geo_STATE", "Geo_PLACECE", 
               "pop90", "pop902", "popdensity90", "landarea90", "pop903", "urban90", "inurban90", "outurban90", "rural90", "farm90", "nonfarm90", 
               "pop904", "nh90", "nhwhite90", "nhblack90", "nhaian90", "nhapi90", "nhother90", "h90", "hwhite90", "hblack90", "haian90", "hapi90", "hother90", 
               "pop905", "nothorigin90", "horigin90", "mexican90", "pr90", "cuban90", "otherhorigin90", "dr90", "centam90", 
               "guatemalan90", "honduran90", "nica90", "panama90", "salvador90", "othercentam90", "sam90", "colombia90", "ecuador90", 
               "peru90", "othersam90", "otherhisp90", "lfp90", "inlfp90", "armed90", "civilian90", 
               "households90", "r1", "r2", "r3", "r4", "r5", "r6", "r7", "r8", "r9", "r10", "r11", "r12", "r13", "r14", "r15", "r16", "r17", "r18", "r19", "r20", 
               "r21", "r22", "r23", "r24", "r25", "r26", "hinc90", "povpop90", "notpov90", "wnotpov90", "bnotpov90", "aiannotpov90", "apinotpov90", "othernotpov90", 
               "inpov90", "winpov90", "binpov90", "aianinpov90", "apiinpov90", "otherinpov90", "pop906", "native90", "fborn90", "naturalized90", "noncit90", 
               "fborn809090", "fborn707990", "fborn606990", "fbornb6090")
names(places1990) <- names1990

# 2. make variables 
cpi <- 1.36 #1990$ in 2000$ value 

places1990 <- 
  mutate(places1990, 
         pop90p = pop90, 
         nhblack90p = nhblack90, 
         nhwhite90p = nhwhite90, 
         h90p = h90, 
         min90p = rowSums(places1990 %>% select(nhblack90:h90)), 
         pctnhblack90p = (nhblack90p/pop90p) * 100, 
         pctnhwhite90p = (nhwhite90p/pop90p) * 100, 
         pcth90p = (h90p/pop90p) * 100, 
         pctmin90p = (min90p/pop90p)*100, 
         pctrecimm90p = (fborn809090/pop90p) * 100,
         hincinf90p = hinc90*cpi)

pl9000 <- 
  left_join(
    places1990 %>% select(
  c(Geo_QName, pop90p:hincinf90p)), 
  places2000 %>% select(
  c(Geo_QName, Geo_STATE, Geo_PLACE, popdensitypl00, pop00p:minpov00p)), 
  by = "Geo_QName")

# make change variables 
pl9000 <- 
  mutate(pl9000, 
         popgrowth = ((pop00p-pop90p)/pop90p) * 100,
         nhwhitegrowth = ((nhwhite00p-nhwhite90p)/nhwhite90p) * 100, 
         nhblackgrowth = ((nhblack00p-nhblack90p)/nhblack90p) * 100,
         hgrowth = ((h00p-h90p)/h90p) * 100,
         mingrowth = ((min00p-min90p)/min90p) * 100,
         recimmgrowth = (pctrecimm00p - pctrecimm90p),
         incomegrowth = (hinc00p - hincinf90p))

write_csv(pl9000, "pl9000_var.csv")
rm(places1990, places2000, names1990, names2000)

# create the 2000-2010 white dataset ####
pl2010 <- read_csv("seplaces_allstates/2010places.csv")
pl2010 <- pl2010 %>%
  mutate(pctnhwhite10 = PCT_SE_T055_003,
         pctnhblack10 = PCT_SE_T055_004,
         pcth10 = PCT_SE_T055_010,
         pctmin10 = ((SE_T055_004 + SE_T055_005 + SE_T055_006 + SE_T055_007 + SE_T055_008 + SE_T055_009 + SE_T055_010)/SE_T055_001) * 100,
         plid = paste0(Geo_STATE, Geo_PLACE))

pl9000 <- read_csv("pl9000_var.csv")
pl9000 <- pl9000 %>%
  mutate(plid = paste0(sprintf("%02.0f", Geo_STATE),
                       sprintf("%05.0f", Geo_PLACE)))

nhwhite9010 <- left_join(pl2010 %>% select(plid, pctnhwhite10:pctmin10),
                         pl9000 %>% select(plid, pctnhwhite00p, nhwhite00p, nhwhitegrowth, pcth00p, h00p, hgrowth, pctmin00p, min00p, mingrowth, pctnhblack00p, nhblack00p, nhblackgrowth, Geo_STATE))
write_csv(nhwhite9010, "nhwhite9010.csv")

# if in BAS, annex = 1, 
rm(list = ls())

# get seg indices? 
blocks2000 <- read_csv("blocks2000_var.csv")

blocks2000 <- blocks2000 %>% 
  mutate(plid2 = paste0(str_pad(STATEA, 2, side = "left", pad = "0"), 
                       str_pad(PLACEA, 5, side = "left", pad = "0")))

pldat<- blocks2000 %>% 
  filter(PLACEA != 99999 | !is.na(PLACEA)) %>% 
  group_by(plid2) %>%
  summarise(pl_total = sum(pop00b), 
            pl_wht = sum(nhwhite00b), 
            pl_blk = sum(nhblack00b), 
            pl_min = sum(min00b), 
            pl_hisp = sum(h00b))

seg00 <- left_join(blocks2000, pldat, by = "plid2")

pl.dis <- seg00 %>%
  mutate(d.wb = abs((nhwhite00b/pl_wht) - (nhblack00b/pl_blk)),
         d.wmin = abs((nhwhite00b/pl_wht) - (min00b/pl_min)),
         d.wh = abs((nhwhite00b/pl_wht) - (h00b/pl_hisp))) %>%
  group_by(plid2) %>%
  summarise(wb.dissim00 = .5*sum(d.wb, na.rm=T),
            wmin.dissim00 = .5*sum(d.wmin, na.rm = T),
            wh.dissim00 = .5*sum(d.wh, na.rm = T))

pl.int <- seg00 %>%
  mutate(int.wb = (nhblack00b/pl_blk * nhwhite00b/pop00b),
         int.wmin = (min00b/pl_min * nhwhite00b/pop00b),
         int.wh = (h00b/pl_hisp * nhwhite00b/pop00b)) %>%
  group_by(plid2) %>%
  summarise(wb.int00 = sum(int.wb, na.rm=T),
            wmin.int00 = sum(int.wmin, na.rm = T),
            wh.int00 = sum(int.wh, na.rm = T))

#block 10 ####
blocks2010 <- read_csv("ipumsblocks_allstates/2010blocks/nhgis0013_ds172_2010_block.csv")
blocks2010 <- blocks2010 %>%
  select(1:56)

# 1. rename variables
names2010 <- c("pop", "nhtotal", "nhwhite", "nhblack", "nhaian", "nhasian", "nhpi",
               "nhother", "nh2p", "htotal", "hwhite", "hblack", "haian", "hasian",
               "hhpi", "hother", "h2p")
names(blocks2010)[40:56] <- names2010

blocks2010 <- 
  mutate(blocks2010, 
         min10b = rowSums(blocks2010[, c(43:49)]),
         plid2 = paste0(str_pad(STATEA, 2, side = "left", pad = "0"), 
                         str_pad(PLACEA, 5, side = "left", pad = "0")))

pldat<- blocks2010 %>% 
  filter(PLACEA != 99999 | !is.na(PLACEA)) %>% 
  group_by(plid2) %>%
  summarise(pl_total = sum(pop), 
            pl_wht = sum(nhwhite), 
            pl_blk = sum(nhblack), 
            pl_min = sum(min10b), 
            pl_hisp = sum(htotal))

seg10 <- left_join(blocks2010, pldat, by = "plid2")

pl.dis10 <- seg10 %>%
  mutate(d.wb = abs((nhwhite/pl_wht) - (nhblack/pl_blk)),
         d.wmin = abs((nhwhite/pl_wht) - (min10b/pl_min)),
         d.wh = abs((nhwhite/pl_wht) - (htotal/pl_hisp))) %>%
  group_by(plid2) %>%
  summarise(wb.dissim10 = .5*sum(d.wb, na.rm=T),
            wmin.dissim10 = .5*sum(d.wmin, na.rm = T),
            wh.dissim10 = .5*sum(d.wh, na.rm = T))

pl.int10 <- seg10 %>%
  mutate(int.wb = (nhblack/pl_blk * nhwhite/pop),
         int.wmin = (min10b/pl_min * nhwhite/pop),
         int.wh = (htotal/pl_hisp * nhwhite/pop)) %>%
  group_by(plid2) %>%
  summarise(wb.int10 = sum(int.wb, na.rm=T),
            wmin.int10 = sum(int.wmin, na.rm = T),
            wh.int10 = sum(int.wh, na.rm = T))
