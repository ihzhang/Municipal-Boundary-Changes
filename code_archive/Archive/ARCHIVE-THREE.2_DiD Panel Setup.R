# get environment ready 
setwd("~/Google Drive/My Drive/Stanford/QE2")

library("stringr")
library("dplyr")
library("stargazer")
library("tidyverse")
library("tidycensus")
library("lme4")
library("readr")
library("data.table")
#library("readstata13")
library("magrittr")
library("fixest")

# make DiD panel ####
# 1. get plid for annexations to 2000-2010 and 2010-2013 
# and their avg % annexed, avg % between annexed and non-annexed (2013 block)
### filter out 2010-2013 annexations 
# 2. for the did panel, make outcome of annexed-annexable at 2013 and 2020 

# outcome var = 
# 1/0 to make a racially selective annexation 
# racially selective = not annexing the block because of racial considerations 
# this is nebulous to define, so create a few thresholds 
# if annexed %black is negative at all compared to annexable (or the place, if did not annex)
# if annexed % block is 75% less black comparedd to annexable 
# first need to clean 2010-2020 annexation data compared to BAS 
# next, if a place annexed from 2000-2013, they are given a 0 for time, and 1 otherwise 
# calculate outcomes 
# need county code for each place 

# load in places that definitely annexed prior to 2013, using 0010 data 
annexed0010 <- read_csv("annexedblocks0010dem_pl00_newsample_unincorp.csv")

# get 2010-2013 
# find annexations 2010-2013 
aa2013 <- read_csv("annexedblocks1020_base_unincorp.csv")
year_list <- c(11:21)
bas_list <- list()
for (i in 1:length(year_list)) {
    bas_list[[i]] <- read.delim(file = paste0("BAS/US_bas", year_list[i], ".txt"), 
                                header = TRUE, na = "") 
} 

bas <- rbindlist(bas_list, use.names = T, fill = T)
rm(bas_list)

bas %<>% 
    filter(Action=="Annexation" & (!is.na(Place.Name.and.LSAD) & !is.na(FIPS.Place.Code)) & !is.na(State)) %>%
    mutate(State = str_pad(State, 2, side = "left", pad = "0"),
           FIPS.Place.Code = str_pad(FIPS.Place.Code, 5, side = "left", pad = "0"),
           plid = paste0(State, FIPS.Place.Code),
           st_pln = paste0(State, Place.Name.and.LSAD),
           ann_date = lubridate::mdy(Effective.Date)) %>%
    filter(ann_date >= as.Date("2010-01-01") & ann_date <= as.Date("2020-12-31"))

ann_prevra <- bas %>%
    filter(ann_date < as.Date("2013-06-25"))

prev <- unique(ann_prevra$plid)
postv <- unique(bas$plid[bas$ann_date >= as.Date("2013-06-25")])

double <- postv[postv %in% prev]
postv <- postv[!postv %in% double]
prev <- prev[!prev %in% double]

aa_ann <- as.character(unique(aa2013$plid))

# this is over 10K places - we really miss a lot by excluding them
length(!unique(aa2013$plid) %in% unique(bas$plid))
plid_excl <- unique(aa2013$plid)[!unique(aa2013$plid) %in% unique(bas$plid)]

aa2013 %<>% 
    filter((plid %in% bas$plid) & 
               (!plid %in% double))
aa2013 %<>%
    filter(plid %in% prev)

annexed0013 <- base::rbind(
    annexed0010 %>% select(blkid, plid, annexed, STATEA, COUNTYA),
    aa2013 %>% select(blkid, plid, annexed) %>% 
        mutate(STATEA = substr(blkid, 1, 2), COUNTYA = substr(blkid, 3, 5))
)

rm(aa2013, ann_prevra, annexed0010, aa_ann, year_list)

write_csv(annexed0013, "annexed0013.csv")

# load in VRA data 
annexed0013 <- read_csv("annexed0013.csv")

vrastates <- c("01", "02", "04", "13", "22", "28", "45", "48", "51")
vra <- read_csv("vra_counties.csv")
vra %<>% 
    mutate(countyfips = str_pad(countyfips, 5, side = "left", pad = "0"),
           sectionv = 1)
annexed0013 %<>%
    mutate(countyfips = paste0(STATEA, COUNTYA))

annexed0013 %<>%
    mutate(vra = case_when(
        STATEA %in% vrastates ~ 1,
        countyfips %in% vra$countyfips ~ 1,
        TRUE ~ 0
    ))

table(annexed0013$vra, exclude = NULL)

# make a block version of the joined data 
blocks2000 <- read_csv("blocks2000_var.csv")
annexed0013 %<>%
    filter(blkid %in% blocks2000$blkid)
table(annexed0013$vra, exclude = NULL)
table(annexed0013$annexed)

annexed0013 %<>%
    left_join(blocks2000 %>% select(blkid, pop00b, pctnhblack00b:pctmin00b))

annexed0013 %<>%
    filter(pop00b > 0 & !is.na(pop00b))

annexed0013 %<>%
    mutate_at(c(names(annexed0013)[9:ncol(annexed0013)]), ~ifelse(is.na(.), 0, .))

places0013 <- 
    annexed0013 %>%
    group_by(plid, annexed) %>%
    summarize_at(c(names(annexed0013)[7:ncol(annexed0013)]), ~mean(., na.rm = T)) 

places0013 %<>%
    mutate(vra = ifelse(vra >0 & vra < 1, 1, vra))
table(places0013$vra)

places0013 %<>%
    pivot_wider(
        id_cols = c(plid, vra),
        names_from = "annexed",
        values_from = c(annexed, pop00b:pctmin00b)
    )

varindex = names(annexed0013)[8:ncol(annexed0013)] # create change variables

for (variable in varindex) {
    varname <- paste0(variable, "_diff")
    places0013[[varname]] <- places0013[[paste0(variable, "_1")]] - places0013[[paste0(variable, "_0")]]
}

places0013 %<>%
    select(c(plid, vra, contains("_diff"))) %>%
    filter(!is.na(pop00b_diff)) %>%
    mutate(annexed = 1)

# everything that's not in places0013 are non-annexing places, and we need 
# to know their average contiguous characteristics 

contig <- read_csv("allcontigblocks2000.csv")
contig %<>%
    filter(blkid %in% blocks2000$blkid) %<>%
    select(blkid, contigplace) %>%
    rename(plid = contigplace)

contig %<>%
    left_join(blocks2000 %>% select(blkid, pop00b, pctnhblack00b:pctmin00b))

contig %<>%
    filter(pop00b > 0 & !is.na(pop00b))

contig %<>%
    mutate(STATEA = substr(blkid, 1, 2), 
           countyfips = substr(blkid, 1, 5))

contig %<>%
    mutate(vra = case_when(
        STATEA %in% vrastates ~ 1,
        countyfips %in% vra$countyfips ~ 1,
        TRUE ~ 0
    ))

contigplaces0013 <- 
    contig %>%
    group_by(plid) %>%
    summarize_at(c(names(contig)[c(3:7, 10)]), ~mean(., na.rm = T)) 

contigplaces0013 %<>%
    mutate(vra = ifelse(vra >0 & vra < 1, 1, vra))

table(contigplaces0013$vra, exclude = NULL)

contigplaces0013 %<>%
    mutate_at(c(names(contigplaces0013)[c(3:6)]), ~ifelse(is.na(.), 0, .))

# if the places in contigplaces are already covered by annexed, remove 
contigplaces0013 %<>%
    filter(!plid %in% places0013$plid)

# get diff var for non-annexing places 
places2000 <- read_csv("pl2000_cleaned.csv")

places2000 %<>%
    select(plid, pctnhblack00p:pctmin00p) 

places2000 %<>%
    filter(!plid %in% places0013$plid) %>%
    filter(plid %in% contigplaces0013$plid)

contigplaces0013 %<>%
    left_join(places2000, by = "plid")

varindex = names(contigplaces0013)[c(3:6)] # create change variables
varindex = gsub("00b", "", varindex)

for (variable in varindex) {
    varname <- paste0(variable, "_diff")
    contigplaces0013[[varname]] <- contigplaces0013[[paste0(variable, "00p")]] - contigplaces0013[[paste0(variable, "00b")]]
}

contigplaces0013 %<>%
    select(c(plid, vra, contains("_diff"))) 

places_to_merge2013 <- base::rbind(
    places0013 %>% select(c(plid, vra, contains("pct"), annexed)),
    contigplaces0013 %>% mutate(annexed = 0)
)

places_to_merge2013$Year <- 2013
write_csv(places_to_merge2013, "places_to_merge2013.csv")

rm(places2000, annexed0013, blocks2000, contig, contigplaces0013, paneldid, places0013, places13)

# repeat this for 2013-2020 ####
aa2020 <- read_csv("annexedblocks1020_base_unincorp.csv")

# remove these plids from places2020 data 
# because we know they have annexed but we can't find info about when they 
# annexed 

plids_to_remove <- aa2020 %>%
    filter(!plid %in% bas$plid) %>%
    select(plid)

plids_to_remove <- unique(plids_to_remove)

aa2020 %<>% 
    filter((plid %in% bas$plid) & 
               (!plid %in% double))
aa2020 %<>%
    filter(plid %in% postv)

write_csv(aa2020, "annexed2020.csv")

aa2020 <- read_csv("annexed2020.csv")
aa2020 %<>%
    mutate(countyfips = substr(blkid, 1, 5),
           STATEA = substr(blkid, 1, 2))

aa2020 %<>%
    mutate(vra = case_when(
        STATEA %in% vrastates ~ 1,
        countyfips %in% vra$countyfips ~ 1,
        TRUE ~ 0
    ))

table(aa2020$vra, exclude = NULL)

# get 2013 block data 
blocks2013 <- read_csv("blocks2013_int.csv")

length(aa2020$blkid %in% blocks2013$blkid)

aa2020 %<>%
    mutate(exclude = ifelse(!blkid %in% blocks2013$blkid, 1, 0))
table(aa2020$exclude)
aa2020 %<>%
    select(-exclude)

aa2020 %<>%
    filter(blkid %in% blocks2013$blkid)

aa2020 %<>%
    left_join(blocks2013 %>% select(blkid, pop, pctnhblack:pctmin))

aa2020 %<>%
    filter(pop > 0 & !is.na(pop))

aa2020 %<>%
    mutate_at(c(names(aa2020)[8:ncol(aa2020)]), ~ifelse(is.na(.), 0, .))

places2020 <- 
    aa2020 %>%
    group_by(plid, annexed) %>%
    summarize_at(c(names(aa2020)[6:ncol(aa2020)]), ~mean(., na.rm = T)) 

places2020 %<>%
    mutate(vra = ifelse(vra >0 & vra < 1, 1, vra))
table(places2020$vra)

places2020 %<>%
    pivot_wider(
        id_cols = c(plid, vra),
        names_from = "annexed",
        values_from = c(annexed, pop:pctmin)
    )

varindex = names(aa2020)[7:ncol(aa2020)] # create change variables

for (variable in varindex) {
    varname <- paste0(variable, "_diff")
    places2020[[varname]] <- places2020[[paste0(variable, "_1")]] - places2020[[paste0(variable, "_0")]]
}

places2020 %<>%
    select(c(plid, vra, contains("_diff"))) %>%
    filter(!is.na(pop_diff)) %>%
    mutate(annexed = 1)

# everything that's not in places2020 are non-annexing places, and we need 
# to know their average contiguous characteristics 

contig <- read_csv("allcontigblocks2010.csv")
contig %<>%
    filter(blkid %in% blocks2013$blkid) %<>%
    select(blkid, contigplace) %>%
    rename(plid = contigplace)

contig %<>%
    left_join(blocks2013 %>% select(blkid, pop, pctnhblack:pctmin))

contig %<>%
    filter(pop > 0 & !is.na(pop))

contig %<>%
    mutate(STATEA = substr(blkid, 1, 2), 
           countyfips = substr(blkid, 1, 5))

contig %<>%
    mutate(vra = case_when(
        STATEA %in% vrastates ~ 1,
        countyfips %in% vra$countyfips ~ 1,
        TRUE ~ 0
    ))

contigplaces2020 <- 
    contig %>%
    group_by(plid) %>%
    summarize_at(c(names(contig)[c(3:7, 10)]), ~mean(., na.rm = T)) 

contigplaces2020 %<>%
    mutate(vra = ifelse(vra >0 & vra < 1, 1, vra))

table(contigplaces2020$vra, exclude = NULL)

contigplaces2020 %<>%
    mutate_at(c(names(contigplaces2020)[c(3:6)]), ~ifelse(is.na(.), 0, .))

# if the places in contigplaces are already covered by annexed, remove 
contigplaces2020 %<>%
    filter(!plid %in% places2020$plid) %>%
    filter(!plid %in% plids_to_remove)

# get diff var for non-annexing places 
places20census <- read_csv("places2020_var.csv")

contigplaces2020 %<>%
    select(plid, vra, pctnhblack:pctmin)

places20census %<>%
    select(-Geo_QName) 

names(places20census)[2:ncol(places20census)] <- paste0("pct", names(places20census)[2:ncol(places20census)])

places20census %<>%
    filter(!plid %in% places2020$plid) %>%
    filter(!plid %in% plids_to_remove) %>%
    filter(plid %in% contigplaces2020$plid)

contigplaces2020 %<>%
    left_join(places20census, by = "plid")

varindex = names(contigplaces2020)[c(3:6)] # create change variables

for (variable in varindex) {
    varname <- paste0(variable, "_diff")
    contigplaces2020[[varname]] <- contigplaces2020[[paste0(variable, "20p")]] - contigplaces2020[[paste0(variable)]]
}

contigplaces2020 %<>%
    select(c(plid, vra, contains("_diff"))) %>%
    mutate(annexed = 0)

places_to_merge2020 <- base::rbind(
    places2020 %>% select(c(plid, vra, contains("pct"), annexed)),
    contigplaces2020
)

places_to_merge2020$Year <- 2020
table(places_to_merge2020$annexed, exclude = NULL)

write_csv(places_to_merge2020, "places_to_merge2020.csv")

rm(places20census, aa2020, blocks2013, contig, contigplaces2020, places2020)

# make panel data!!!!! ####
places_to_merge2013 <- read_csv("places_to_merge2013.csv")
places_to_merge2013 %<>%
    mutate(pctnhblack_diff = ifelse(is.na(pctnhblack_diff), pctnhblack00b_diff, pctnhblack_diff),
           pctnhwhite_diff = ifelse(is.na(pctnhwhite_diff), pctnhwhite00b_diff, pctnhwhite_diff),
           pcth_diff = ifelse(is.na(pcth_diff), pcth00b_diff, pcth_diff),
           pctmin_diff = ifelse(is.na(pctmin_diff), pctmin00b_diff, pctmin_diff),
    ) %>%
    select(-c(contains("00b")))
   
places_to_merge2013 %<>%
    filter(plid %in% places_to_merge2020$plid)

places_to_merge2020 %<>%
    filter(plid %in% places_to_merge2013$plid)

panel1320_did <- base::rbind(
    places_to_merge2013,
    places_to_merge2020
)

write_csv(panel1320_did, "panel1320_did.csv")

# test reg
panel1320_did <- read_csv("panel1320_did.csv")
panel1320_did %<>%
    mutate(post = ifelse(Year > 2013, 1, 0),
           treatment = ifelse(vra==1, 1, 0)
           )

panel1320_did %<>% 
    filter(!is.na(pctnhblack_diff)) %>%
    group_by(plid) %>%
    mutate(appear = n()) %>%
    filter(appear == 2) %>%
    ungroup()

panel1320_did %<>%
    mutate(underbound_nhb_abs = ifelse(pctnhblack_diff < 0, 1, 0),
           underbound_nhb_med = ifelse(pctnhblack_diff < 6.06, 1, 0),
           underbound_nhw_med = ifelse(pctnhwhite_diff > 5.03, 1, 0))

testdid <- fixest::feglm(underbound_nhb_abs ~ treatment + post + treatment*post | as.factor(as.character(plid)), family = "binomial", data = panel1320_did)
testdid2 <- fixest::feglm(underbound_nhb_med ~ treatment + post + treatment*post | as.factor(as.character(plid)), family = "binomial", data = panel1320_did)
testdid2 <- fixest::feglm(underbound_nhw_med ~ treatment + post + treatment*post | as.factor(as.character(plid)), family = "binomial", data = panel1320_did)

summary(testdid)
summary(testdid2)

predictdf <- with(panel1320_did, data.frame(
                  treatment = c(0, 0, 1, 1),
                      post = c(0, 1, 0, 1),
                  plid = rep("0100124", 4)))

predictdf <- cbind(predictdf, predict(testdid, newdata = predictdf)) 
names(predictdf)[4] <- "pred_val"
predictdf %<>% mutate(predicted_val = exp(pred_val - 1))

summary(testdid)

panel1320_did %<>%
    mutate(plid = as.numeric(as.character(plid)))

testdid2 <- att_gt(
    yname = "pctnhblack_diff",
    gname = "treatment",
    tname = "post",
    idname = "plid",
    xformla = ~1,
    data = panel1320_did,
    est_method = "reg"
)

