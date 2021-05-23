rm(list = ls())

# get environment ready 
setwd("~/Google Drive/Stanford/QE2")

library("stringr")
library("dplyr")
library("stargazer")
library("tidyverse")
library("tidycensus")
library("lme4")

#IF DATA IS TO BE REPLICATED, START HERE ####
#BAS from Census, 2000-2010 
bas <- 
  read.delim(file = "BAS0010.txt", header = T, na = "")

#save only States that are identified
bas <- bas[!is.na(bas$State), ]

#create unique ids for each annexing place
bas$State <- str_pad(bas$State, 2, side = "left", pad = "0")
bas$Place <- str_pad(bas$FIPS.Place.Code, 5, side = "left", pad = "0")
bas$plid <- paste0(bas$State, bas$Place)
bas %>%
  filter(Action == "Annexation") %>%
  group_by(plid) 

length(unique(bas$plid))
7943/25376

# states for analysis ####
bas_states <- bas[bas$State==1 | bas$State==5 | bas$State==13 | bas$State==22 | bas$State==28 | bas$State==37 | bas$State==45 |
                    bas$State==51 | bas$State==24 | bas$State==38 | bas$State==46 | bas$State==47 | bas$State==21 | bas$State==10,]
bas_states_places <- as.data.frame(unique(bas_states$plid))
names(bas_states_places) <- "plid"
bas_states_places$plid <- as.character(bas_states_places$plid)
rm(bas_states)

astate <- bas %>% 
  filter(Action == "Annexation") %>%
  group_by(State) %>%
  tally()
View(astate)
# some states have way more annexations than others 
hist(astate$n)

# create annexed blocks file ####
# create a file of annexed blocks between 2000 and 2010, defined as: 
# a) blocks in 2010 places that were not in those places in 2000 
# b) blocks that were previously not part of any place and are now in 2010 places 
# this is done place by place; first isolate the place in 2000 and 2010, then compare the blocks in each list
blocks2000 <- read_csv("blocks2000_var.csv")
blocks2010 <- read_csv("2010blocks_converted.csv")

# only want places that are in both block files 
blocks2000$PLACEA <- as.character(blocks2000$PLACEA)
blocks2000$STATEA <- as.character(blocks2000$STATEA)

blocks2000$STATEA <- str_pad(blocks2000$STATEA, 2, side = "left", pad = "0")
blocks2000$PLACEA <- str_pad(blocks2000$PLACEA, 5, side = "left", pad = "0")
blocks2000$plid <- paste0(blocks2000$STATEA, blocks2000$PLACEA)

#these are just blocks in places that were entered as having annexed between 2000 and 2010, plus unincorporated blocks, hence no plid in 2000
annexingplacesblocks2000 <- blocks2000[(which(blocks2000$plid %in% bas_states_places$plid |
                                                (is.na(blocks2000$PLACEA) | blocks2000$PLACEA=="99999"))),]


blocks2010$STATEA <- str_pad(blocks2010$STATEA, 2, side = "left", pad = "0")
blocks2010$PLACEA <- str_pad(blocks2010$PLACEA, 5, side = "left", pad = "0")
blocks2010$plid <- paste0(blocks2010$STATEA, blocks2010$PLACEA)

blocks2000$GISJOIN <- as.character(blocks2000$GISJOIN)
blocks2010$GISJOIN <- as.character(blocks2010$GISJOIN)

# get blocks in 2010 where they were part of a place that did annex between 2000 and 2010, or who were identified already as having existed in 2000 
annexingplacesblocks2010 <- blocks2010[((blocks2010$plid %in% annexingplacesblocks2000$plid) | (blocks2010$plid %in% bas_states_places$plid)),]

# filter out unincorporated blocks 
annexingplacesblocks2010 <- annexingplacesblocks2010[!annexingplacesblocks2010$PLACEA=="99999",]

# populated blocks in 2010 only
# annexingplacesblocks2000 <- annexingplacesblocks2000[annexingplacesblocks2000$pop > 0,]
annexingplacesblocks2010 <- annexingplacesblocks2010[annexingplacesblocks2010$totalr > 0,]

annexedblocks <- data.frame()
plids <- unique(annexingplacesblocks2010$plid)
for (i in 1:length(plids)) {
  block00 <- annexingplacesblocks2000[annexingplacesblocks2000$plid==plids[i], ]
  block10 <- annexingplacesblocks2010[annexingplacesblocks2010$plid==plids[i],]
  block <- block10[!block10$GISJOIN %in% block00$GISJOIN,]
  annexedblocks <- rbind(annexedblocks, block)
}

table(is.na(annexedblocks$PLACEA))
annexedblocks <- annexedblocks[annexedblocks$totalr > 0, ]
annexedblocks <- annexedblocks[annexedblocks$GISJOIN %in% annexingplacesblocks2000$GISJOIN,] # filter out newly created blocks 
blocks2000$GISJOIN <- as.character(blocks2000$GISJOIN)

# we just need the blockids of blocks that are annexed
annexedblocks <- as.data.frame(annexedblocks[, c("GISJOIN", "STATEA", "PLACEA", "BLKGRPA")])
names(annexedblocks) <- c("GISJOIN", "STATEA_annexed", "PLACEA_annexed","BLKGRPA_annexed")
annexedblocks$GISJOIN <- as.character(annexedblocks$GISJOIN)
annexedblocks$annexed <- 1
annexedblocks$GISJOIN <- as.character(annexedblocks$GISJOIN)
annexedblocks <- left_join(blocks2000, annexedblocks, by = "GISJOIN")

# save this file: all places in 2000 that annexed, the annexed blocks and their corresponding places annexed to between 2000 and 2010
write.csv(annexedblocks, file = "annexedblocks0010dem.csv", row.names = F, na = "")
annexedblocks <- read.csv("annexedblocks0010dem.csv", header = T, na = "")

# contiguous blocks ####
al_contig <- read.csv(file = "Shapefiles_contig/AL_01/al_contigblocks_by_place.csv", sep = ",", header = T, na = "")
al_contig$State <- "01"
ar_contig <- read.csv(file = "Shapefiles_contig/AR_05/ar_contigblocks_by_place.csv", sep = ",", header = T, na = "")
ar_contig$State <- "05"
de_contig <- read.csv(file = "Shapefiles_contig/DE_10/de_contigblocks_by_place.csv", sep = ",", header = T, na = "")
de_contig$State <- "10"
ga_contig <- read.csv(file = "Shapefiles_contig/GA_13/ga_contigblocks_by_place.csv", sep = ",", header = T, na = "")
ga_contig$State <- "13"
ky_contig <- read.csv(file = "Shapefiles_contig/KY_21/ky_contigblocks_by_place.csv", sep = ",", header = T, na = "")
ky_contig$State <- "21"
la_contig <- read.csv(file = "Shapefiles_contig/LA_22/la_contigblocks_by_place.csv", sep = ",", header = T, na = "")
la_contig$State <- "22"
md_contig <- read.csv(file = "Shapefiles_contig/MD_24/md_contigblocks_by_place.csv", sep = ",", header = T, na = "")
md_contig$State <- "24"
ms_contig <- read.csv(file = "Shapefiles_contig/MS_28/ms_contigblocks_by_place.csv", sep = ",", header = T, na = "")
ms_contig$State <- "28"
nc_contig <- read.csv(file = "Shapefiles_contig/NC_37/nc_contigblocks_by_place.csv", sep = ",", header = T, na = "")
nc_contig$State <- "37"
nd_contig <- read.csv(file = "Shapefiles_contig/ND_38/nd_contigblocks_by_place.csv", sep = ",", header = T, na = "")
nd_contig$State <- "38"
sc_contig <- read.csv(file = "Shapefiles_contig/SC_45/sc_contigblocks_by_place.csv", sep = ",", header = T, na = "")
sc_contig$State <- "45"
sd_contig <- read.csv(file = "Shapefiles_contig/SD_46/sd_contigblocks_by_place.csv", sep = ",", header = T, na = "")
sd_contig$State <- "46"
tn_contig <- read.csv(file = "Shapefiles_contig/TN_47/tn_contigblocks_by_place.csv", sep = ",", header = T, na = "")
tn_contig$State <- "47"
va_contig <- read.csv(file = "Shapefiles_contig/VA_51/va_contigblocks_by_place.csv", sep = ",", header = T, na = "")
va_contig$State <- "51"

names(ar_contig)[1] <- "FID"
contigall2000 <- rbind(al_contig, ar_contig, de_contig, ga_contig, ky_contig, la_contig, md_contig,
                       ms_contig, nc_contig, nd_contig, nd_contig, sc_contig, sd_contig, tn_contig, va_contig)
rm(al_contig, ar_contig, de_contig, ga_contig, ky_contig, la_contig, md_contig,
   ms_contig, nc_contig, nd_contig, nd_contig, sc_contig, sd_contig, tn_contig, va_contig)

contigall2000$GISJOIN <- as.character(contigall2000$GISJOIN)
write.csv(contigall2000, file = "allcontigblocks_20200201.csv")

# identify contiguous blocks and actually annexed blocks in the all-block file ####
contigall2000 <- read.csv("allcontigblocks_20200201.csv", header = T, na = "")
contigall2000 <- contigall2000 %>%
  filter(PLACEA=="0" | PLACEA=="99999" | is.na(PLACEA))

#actual information we need 
contigall2000$GISJOIN <- as.character(contigall2000$GISJOIN)
contigblocks00 <- as.data.frame(contigall2000[, c("GISJOIN", "PLACEA_1")])
contigblocks00$contig <- 1

# annexing analytical file "aa"
aa <- left_join(annexedblocks, contigblocks00, by = "GISJOIN")
aa <- aa[aa$pop00b > 0,]
aa <- aa[which(aa$contig==1 | aa$annexed==1),]

#_ca for contiguous and annexed 
write.csv(aa, "annexedblocks0010dem_ca.csv", row.names = F, na = "")
aa <- read.csv("annexedblocks0010dem_ca.csv", header = T, na = "")

#generate the new plids 
aa$PLACEA_1 <- str_pad(aa$PLACEA_1, 5, side = "left", pad = "0")
aa$annexed <- ifelse(is.na(aa$annexed), 0, aa$annexed)
aa$contig <- ifelse(is.na(aa$contig), 0, aa$contig)

#if the original block was unincorporated but annexed, get the place id associated with the annexing place 
aa$PLACEA <- ifelse((aa$PLACEA == 99999 | is.na(aa$PLACEA) & !is.na(aa$PLACEA_annexed)), aa$PLACEA_annexed, aa$PLACEA)
# if still does not have a place id, get the place id of the place it is contiguous to
aa$PLACEA <- ifelse((is.na(aa$PLACEA) & !is.na(aa$PLACEA_1)), aa$PLACEA_1, aa$PLACEA)
table(is.na(aa$STATEA))
aa$STATEA <- str_pad(aa$STATEA, 2, side = "left", pad = "0")
aa$plid2 <- paste0(aa$STATEA, aa$PLACEA)
length(unique(aa$plid2))
#way more plids than in the bas_states_places list
aa <- aa[aa$plid2 %in% bas_states_places$plid,]
#still keeps a healthy amount of annexations
table(aa$annexed)
length(unique(aa$plid2))

write.csv(aa, "annexedblocks0010dem_ca.csv", row.names = F, na = "")

# not sure why there are some GISJOINs that are not unique 
aa <- aa[!duplicated(aa$GISJOIN),]
#drops ~500 more annxed blocks and 3 unique places 
ur2010 <- blocks2010[, c("GISJOIN", "URBRURALA")]
aa <- left_join(aa, ur2010, by = "GISJOIN")

write.csv(aa, "annexedblocks0010dem_ca.csv", row.names = F, na = "")

#clean up and get ready for place data ####
rm(list = ls())

aa <- read.csv("annexedblocks0010dem_ca.csv", header = T, na = "")
bas <- 
  read.delim(file = "BAS0010.txt", header = T, na = "")

# get places from BAS states ####
bas_states <- bas[bas$State==1 | bas$State==5 | bas$State==13 | bas$State==22 | bas$State==28 | bas$State==37 | bas$State==45 |
                    bas$State==51 | bas$State==24 | bas$State==38 | bas$State==46 | bas$State==47 | bas$State==21 | bas$State==10,]
bas_states <- bas_states[!is.na(bas_states$State), ]
bas_states$State <- str_pad(bas_states$State, 2, side = "left", pad = "0")
bas_states$Place <- str_pad(bas_states$FIPS.Place.Code, 5, side = "left", pad = "0")
bas_states$plid <- paste0(bas_states$State, bas_states$Place)

bas_states_places <- as.data.frame(unique(bas_states$plid))
names(bas_states_places) <- "plid"
bas_states_places$plid <- as.character(bas_states_places$plid)
rm(bas, bas_states)

aa <- aa %>%
  select(GISJOIN:plid2)
names(aa)[which(names(aa)=="plid.x")] <- "plid"

# place data ####
pl9000 <- read_csv("pl9000_var.csv")
pl9000$Geo_STATE <- str_pad(pl9000$Geo_STATE, 2, side = "left", pad = "0")
pl9000$Geo_PLACE <- str_pad(pl9000$Geo_PLACE, 5, side = "left", pad = "0")
pl9000 <- mutate(pl9000, 
                 plid2 = paste0(Geo_STATE, Geo_PLACE))

pl9000$annexedpl <- ifelse(pl9000$plid %in% bas_states_places$plid, 1, 0)
pl9000 <- pl9000[pl9000$plid %in% bas_states_places$plid,]

#merge with aa
aa$plid2 <- str_pad(aa$plid2, 7, side = "left", pad = "0")
names(pl9000)[ncol(pl9000)] <- "plid2"
aa <- left_join(aa, pl9000, by = "plid2")

# _pl00 to show that pl00 characteristics have been merged
write_csv(aa, "annexedblocks0010dem_pl00.csv")
write_csv(aa, "annexedblock0010dem_pl00_newsample.csv")

# ANALYSIS REPLICATION -- START HERE####
rm(list = ls())
aa <- read_csv("annexedblocks0010dem_pl00.csv")

# m0 - block-level only 
# create analytical dataset with complete cases for all our variables 
aa <- aa %>%
  filter(!is.na(pop00b) & !is.na(pctnhwhite00b) & !is.na(dependencyratio00b) & !is.na(pctowneroccupied) & 
           is.finite(pop00b) & is.finite(pctnhwhite00b) & is.finite(dependencyratio00b) & is.finite(pctowneroccupied) & 
           !is.na(pcth00b) & !is.na(pctmin00b) & !is.na(pctnhwhite00p) & !is.na(pctmin00p) & !is.na(pcth00p) & !is.na(popgrowth) & 
           !is.na(recimmgrowth) & !is.na(incomegrowth)) 

m0 <- lm(annexed ~ pop00b + pctnhwhite00b + dependencyratio00b + pctowneroccupied, data = aa)
summary(m0)

m0.log <- glm(annexed ~ pop00b + pctnhwhite00b + dependencyratio00b + pctowneroccupied, data = aa, family = "binomial")
summary(m0.log)

aa <- mutate(aa, m0hat = fitted(m0))
aa$r_std <- scale(resid(m0))

ggplot(aa, aes(x = m0hat, y = r_std)) +
  geom_point() +
  xlab("Predicted attitudes") +
  ylab("Standardized residuals") + 
  geom_hline(yintercept = 0, color = "red")

# not a good plot lmao 
m1 <- lm(annexed ~ pop00b + pctnhwhite00b + dependencyratio00b + pctowneroccupied + 
           pop00p + pctnhwhite00p + popgrowth + nhwhitegrowth + incomegrowth + recimmgrowth, data = aa)
summary(m1)

m1.log <- glm(annexed ~ pop00b + pctnhwhite00b + dependencyratio00b + pctowneroccupied + 
           pop00p + pctnhwhite00p + popgrowth + nhwhitegrowth + incomegrowth + recimmgrowth, data = aa, family = "binomial")
summary(m1.log)

# m2 random intercept model 
m2 <- glmer(annexed ~ pop00b + pctnhwhite00b + dependencyratio00b + pctowneroccupied + 
              pop00p + pctnhwhite00p + popgrowth + nhwhitegrowth + incomegrowth + recimmgrowth + (1 | plid2), data = aa, family = "binomial")
summary(m2)
plot(y = predict(m2), x = aa$nhwhitegrowth) 

# for each random effect
for (i in 1:length(u_condvar$id)) {
  # select the random parameter value
  myu <- u_condvar$id[i]
  names(myu) <- "myu"
  # select the variance, take the square root to get the standard error
  myu_se <- apply(varcovar, 3, function(x) sqrt(x[i,i]))   
  # combine them and arrange my myu and add a rank
  u_se <- as_tibble(cbind(myu, myu_se)) %>%
    arrange(myu) %>%
    mutate(rank = 1:237)
  # and store the plots in a list with two elements
  myylabs <- c("Intercept", "Slope")
  myplots[[i]] <- ggplot(u_se, aes(x = rank, y = myu)) +
    geom_errorbar(aes(ymin = myu - (myu_se * 1.39), 
                      ymax = myu + (myu_se * 1.39))) + 
    geom_point() +
    geom_hline(yintercept = 0, color = "red") +
    ylab(myylabs[i]) +
    xlab("Rank")
}    


myplots[[1]]
myplots[[2]]   
# descriptives 
d1 <- as.data.frame(aa %>% 
  group_by(annexed) %>%
  summarize_at(c("pctnhblack00p", "pctnhwhite00p", "pcth00p", "pctmin00p", "pop00p", "popgrowth", "incomegrowth", "blackpov00p"),
               list(~mean(., na.rm = T), ~sd(., na.rm = T))) %>%
  t())

logitvariables %>% 
  group_by(annexed) %>%
  summarize_at(c("pctnhblack", "pctnhwhite", "pcth", "pctmin", "pop00blk", "dependencyratio", "pctownerocc", "ruralpl00"), 
               list(~mean(., na.rm = T), ~sd(., na.rm = T))) 

logitvariables %>% 
  filter(pctnhblack > 0 & ruralpl00==1) %>%
  group_by(annexed) %>%
  summarize_at(c("pctnhblack", "pctnhwhite", "pcth", "pctmin", "pop00blk", "dependencyratio", "pctownerocc", "ruralpl00"), 
               list(~mean(., na.rm = T), ~sd(., na.rm = T))) %>%
  t()

#need identifiers, block00 characteristics, pl00 characteristics, and pl9000 lagged variables 
aa$dependencyratio00b <- ifelse(!is.finite(aa$dependencyratio00b), NA, aa$dependencyratio00b)

aa[, c(1:52)] <- apply(aa[, c(1:52)], 2, function(x) ifelse(!is.finite(x), NA, x))

aa <- aa[aa$pop00b > 0,]

aa$STATEA <- as.factor(aa$STATEA)
aa$STATEA <- relevel(aa$STATEA, ref = "51")

# models ####
logit1 <- glm(annexed ~ pctnhwhite00b + pop00b + pctowneroccupied + dependencyratio00b + STATEA, data = aa, family = "binomial", na.action = na.omit)
summary(logit1)

logit2 <- glm(annexed ~ pop00b + pctowneroccupied + dependencyratio00b + STATEA, data = aa, family = "binomial", na.action = na.omit)
summary(logit2)

anova(logit2, logit1, test = "LRT")

logit3 <- glm(annexed ~ pop00blk+
                pctownerocc + dependencyratio + ruralpl00 +
                STATEA, data = logitvariables, family = "binomial")
summary(logit3)

logit4 <- glm(annexed ~ majwhite*pctnhblack + pop00blk + 
                          pctownerocc + dependencyratio + ruralpl00 +
                          STATEA, data = logitvariables, family = "binomial")
summary(logit4)

stargazer(logit3, logit1, logit2,
          type = "html",
          single.row = TRUE,
          align = TRUE,
          model.names = FALSE,
          out = "logits_blocks.htm")

# OLS models
logitvariables$pctminpl00 <- scale((logitvariables$minpl00/logitvariables$poppl00)*100, scale = F)
ols1 <- lm(pctmin ~ pctnhwhite00pl + popdensitypl00 + pop9000changeratepl + mhincpl00 + pctbpovpl00 + pctownerocc + dependencyratio + STATEA, 
           data = logitvariables)
summary(ols1)

ols2 <- lm(pctmin ~ pctnhwhite00pl + popdensitypl00 + pop9000changeratepl + mhincpl00 + pctbpovpl00 + pctownerocc + dependencyratio + annexablemin +  STATEA, 
           data = logitvariables)
summary(ols2)

stargazer(ols1, ols2, 
          type = "html", 
          single.row = TRUE, 
          align = TRUE, 
          model.names = FALSE, 
          out = "ols.htm")
# descriptives ####
# for random intercept
logitvariables %>% 
  filter(annexed==1) %>%
  group_by(plid2) %>%
  count(annexed) %>%
  with(plot(plid2, n, main = "Total number of blocks annexed by unique places")) 

m0 <- lmer(annexed ~ (1 | plid2),  data = logitvariables, REML = FALSE)
summary(m0)
#intra-class correlation 
(0.05336/(0.05336+0.15759))

# for white decline and annexing minority blocks
logitvariables %>% 
  filter(nhwhitedecline9000==1) %>%
  group_by(annexed) %>% 
  summarize(mean(pctmin, na.rm = T))

logitvariables %>% 
  filter(!is.na(nhwhitedecline9000)) %>%
  group_by(nhwhitedecline9000) %>%
  summarize(mean(annexed, na.rm = T))

logitvariables %>% 
  filter(nhwhitedecline9000==1 & noncitgrowth9000pl==1) %>%
  group_by(annexed) %>% 
  summarize(mean(pctmin, na.rm = T))

m1 <- lmer(annexed ~ (1 + recimmgrowth9000pl | plid2), data = logitvariables, REML = FALSE)
summary(m1)

logitvariables %>% 
  group_by(annexed) %>% 
  summarize(mean(pctmin, na.rm = T))

logitvariables %>% 
  group_by(annexed) %>% 
  summarize(mean(pctnhblack, na.rm = T))

logitvariables %>% 
  group_by(annexed) %>% 
  summarize(mean(pcth, na.rm = T))

logitvariables %>% 
  group_by(annexed, ruralpl00) %>% 
  summarize(mean(pctnhblack00pl, na.rm = T))


# get place characteristics for both annexing and non-annexing places 
names(pl9000)
pl9000 <-pl9000[, c(1, 2, 9, 10, 12, 21:33, 59, 62:63, 68:69, 77:78, 92, 94, 120:135, 139, 141:142, 147, 149:150, 283:287)]

pl9000$pop9000changeratepl <- (((pl9000$poppl00)-(pl9000$pop90))/pl9000$pop90)*100
pl9000$popgrowth9000pl <- ifelse(pl9000$pop9000changeratepl > 0, 1, 0)
pl9000$recimmgrowth9000pl <- ifelse(((pl9000$foreign9500pl00+pl9000$foreign9094pl00) > (pl9000$fborn809090)), 1, 0) 
pl9000$hgrowth9000pl <- ifelse(((pl9000$htotalpl00/pl9000$poppl00) > (pl9000$h90/pl9000$pop90)), 1, 0)
pl9000$noncitgrowth9000pl <- ifelse(((pl9000$notcitizenpl00/pl9000$poppl00) > (pl9000$noncit90/pl9000$pop90)), 1, 0)

pl9000$pctbpovpl00 <- (pl9000$blackbelowpovpl00/pl9000$blackpoppl00)*100

pl9000$nhwhitedecline9000 <- ifelse(((pl9000$nhwhitepl00/pl9000$poppl00) < (pl9000$nhwhite90/pl9000$pop90)), 1, 0)

pl9000$minpl00 <- rowSums(pl9000[, c(30:36)])
pl9000$minpl90 <- rowSums(pl9000[, c(9:13)])
pl9000$mingrowth9000 <- ifelse(((pl9000$minpl00/pl9000$poppl00) > (pl9000$minpl90/pl9000$pop90)), 1, 0)
pl9000$pctnhblack00pl <- (pl9000$nhblackpl00/pl9000$poppl00) * 100
pl9000$nhbgrowth9000 <- ifelse(((pl9000$nhblackpl00/pl9000$poppl00) > (pl9000$nhblack90/pl9000$pop90)), 1, 0)

cpi <- c(2.08, 1.53) #in 2019 dollars 
pl9000$incgrowth9000pl <- ifelse((pl9000$mhincpl00 > (pl9000$hinc90*1.36)), 1, 0)
plidst <- places2000[, c("Geo_STATE", "plid")]
plidst$plid <- as.integer(as.character(plidst$plid))
pl9000 <- left_join(pl9000, plidst, by = "plid")

pl9000$Geo_STATE <- as.factor(pl9000$Geo_STATE)
pl9000$Geo_STATE <- relevel(pl9000$Geo_STATE, ref = "51")
pl9000$pcthpl00 <- (pl9000$htotalpl00/pl9000$poppl00) * 100
pl9000$pctminpl00 <- (pl9000$minpl00/pl9000$poppl00) * 100

pl9000$hinc90i <- pl9000$hinc90*cpi[1]
pl9000$hinc00i <- pl9000$mhincpl00*cpi[2]

logitpl <- glm(annexedpl ~ logpoppl00 + popgrowth9000pl + recimmgrowth9000pl + 
                 incgrowth9000pl + Geo_STATE, data = pl9000, family = "binomial")
summary(logitpl)

pl9000 %>% 
  group_by(annexedpl) %>% 
  summarize(mean(recimmgrowth9000pl, na.rm = T))

pl9000 %>% 
  group_by(annexedpl) %>% 
  summarize(mean(nhwhitedecline9000, na.rm = T))

logitpl <- glm(annexedpl ~ poppl00 + popgrowth9000pl + recimmgrowth9000pl + mingrowth9000 + incgrowth9000pl + Geo_STATE, data = pl9000, family = "binomial")
summary(logitpl) 
summary1 <- exp(cbind(coef(logitpl), confint(logitpl))) 

cor(pl9000$recimmgrowth9000pl, pl9000$hgrowth9000pl)
cor(pl9000$nhwhitedecline9000, pl9000$recimmgrowth9000pl)
cor(pl9000$mingrowth9000, pl9000$nhbgrowth9000)
cor(pl9000$mingrowth9000, pl9000$hgrowth9000pl)

logitpl2 <- glm(annexedpl ~ poppl00 + popgrowth9000pl + mingrowth9000 + recimmgrowth9000pl*incgrowth9000pl + Geo_STATE, data = pl9000, family = "binomial")
summary(logitpl2)
summary2 <- exp(cbind(coef(logitpl2), confint(logitpl2))) 

logitpl3 <- glm(annexedpl ~ poppl00 + popgrowth9000pl + mingrowth9000*incgrowth9000pl + recimmgrowth9000pl + Geo_STATE, data = pl9000, family = "binomial")
summary(logitpl3) 
summary3 <- exp(cbind(coef(logitpl3), confint(logitpl3))) 

pl9000$whitecat00 <- relevel(pl9000$whitecat00, ref = "minority")
logitpl4 <- glm(annexedpl ~ poppl00 + whitecat00 + incgrowth9000pl + nhbgrowth9000 + Geo_STATE, data = pl9000, family = "binomial")
summary(logitpl4) 

stargazer(logitpl, logitpl2, logitpl3,
            type = "html",
          single.row = T,
            out = "logitpls.htm")

names(pl9000)[50] <- "plid2"
pl9000$plid2 <- as.integer(as.character(pl9000$plid2))
pl9000block <- full_join(pl9000, logitvariables)
names(pl9000block)

pl9000$pop9000changeratepl <- ifelse(is.infinite(pl9000$pop9000changeratepl), NA, pl9000$pop9000changeratepl)



pl9000$whitecat00 <- ifelse(pl9000$pctwhitepl00 > 0.75, "majority", 
                            ifelse(pl9000$pctwhitepl00 > 0.5, "slight majority", 
                                   ifelse(pl9000$pctwhitepl00 > 0.25, "slight minority", 
                                          ifelse(pl9000$pctwhitepl00 >= 0, "minority", NA))))
pl9000 %>% 
  group_by(annexedpl, whitecat00) %>%
  summarize_at(c("pctnhblack00pl", "pctwhitepl00", "pcthpl00", "pctminpl00", "popdensitypl00", "poppl00", "pop9000changeratepl", "mhincpl00", "pctblackpovpl00"),
               list(~mean(., na.rm = T), ~sd(., na.rm = T))) %>%
  t()

logitvariables %>%
  filter(annexed == 1) %>%
  summarize(sum((pctnhblack/100)*pop00blk, na.rm = T))

logitvariables %>%
  filter(annexed == 1) %>%
  summarize(mean(pctnhblack, na.rm = T))

logitvariables %>%
  filter(annexed == 0) %>%
  summarize(mean(pctnhblack, na.rm = T))


#MLM for 326 ####
#intercept model, predicting annexations by place variance 
m0 <- lmer(annexed ~ (1 | plid2), data = logitvariables, REML = FALSE)
summary(m0)
#ICC 
0.05336/(0.05336+0.15759)
#25% variance explained by place-level variation 

# whether a block gets annexed depends on a set of socioeconomic and race characteristics at both the place and block level 
# some effects are fixed, and some vary. 
# m1 tests block race effects only 
m1 <- lmer(annexed ~ pctnhblack + (1 | plid2), data = logitvariables, REML = FALSE)
summary(m1)

#ok, pct black has a positive predictive effect, and it is not signficant, and, it didn't decrease variance, but it is a better fit 
anova(m1, m0)
#plus, it's showing a negative correlation between slope and intercept: that is, 
# the higher the %black in the annexing place to begin with, the more negative the slope and vice versa 

m2 <- lmer(annexed ~ pctmin + (1 | plid2), data = logitvariables, REML = FALSE)
summary(m2)

anova(m2, m1)
anova(m2, m0)

#hm, m2 is much better than m1, but not better than m1 at being better than m0
m3 <- lmer(annexed ~ pctmin + I(recimmgrowth9000pl*pctmin) + (1 + pctmin | plid2), data = logitvariables, REML = FALSE)
summary(m3)

#it's possible the effects of pctmin are quadratic...
ggplot(logitvariables, aes(x = pctmin, y = annexed)) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE) +
  geom_smooth(method = "lm", colour = "red")

m4 <- lmer(annexed ~ I(pctmin*pctmin) + (1 | plid2), data = logitvariables, REML = FALSE)
summary(m4)

anova(m4, m2)
logitvariableslmer <- logitvariables[(!is.na(logitvariables$dependencyratio) & !is.na(logitvariables$pctownerocc) &
                                        !is.na(logitvariables$incgrowth9000pl) & !is.na(logitvariables$popdensitypl00) & 
                                        !is.na(logitvariables$pctbpovpl00)), ]

#null model: intercept-only model
m0 <- lm(annexed ~ 1, data = logitvariableslmer)
summary(m0)

#m1: random intercept 
m1 <- lmer(annexed ~ (1 | plid2), data = logitvariableslmer, REML = FALSE)
summary(m1)

(2* logLik(m1)) - (2* logLik(m0))

# m1 performs better
randompar <- as.data.frame(VarCorr(m1))
var_class <- randompar[1,4]
var_pupil <- randompar[2,4]
var_class / (var_pupil + var_class)
#23.3% variation between places (level 2/group)
#m2: model with level 1 predictors: testing effects of race with class controls
m2 <- lmer(annexed ~ pctmin + dependencyratio + pctownerocc + (1 | plid2), data = logitvariableslmer, REML = FALSE)
summary(m2)
anova(m2, m1)

#some problems with fitting on different dataset sizes, because there are NA values in pctmin/dr/ownerocc
# fixed, and m2 performs better 
#r^2(1)
(0.15864-0.15857)/0.15864 #basically 0 
#r^2(2)
(0.04838-0.04828)/0.04838 #also basically 0 

#m3: model with level 2 predictors 
m3 <- lmer(annexed ~ pctmin + dependencyratio + pctownerocc + 
             mingrowth9000 + incgrowth9000pl + popdensitypl00 + pctbpovpl00 + (1 | plid2), data = logitvariableslmer, REML = FALSE)
summary(m3)
anova(m3, m2)

#place-level variance on income and population characteristics don't add much explanatory power 
#m4: model with random slopes, level 1 variables only, so m2 with random slopes 
m4 <- lmer(annexed ~ pctmin + dependencyratio + pctownerocc + (1 + pctmin | plid2), data = logitvariableslmer, REML = FALSE)
summary(m4)
anova(m4, m2)
#m4 performs better than m2

# to compare cross-level interaction, let's do a version of level 2 var that is just income, and then an interaction
m3b <- lmer(annexed ~ pctmin + incgrowth9000pl + dependencyratio + pctownerocc + (1 + pctmin | plid2), data = logitvariableslmer, REML = FALSE)
summary(m3b)

m3c <- lmer(annexed ~ pctmin + incgrowth9000pl + dependencyratio + pctownerocc + (pctmin*incgrowth9000pl) + (1 + pctmin | plid2), data = logitvariableslmer, REML = FALSE)
summary(m3c)

anova(m3c, m3b)

#m3c performs better than m3b. Does it perform better than m4?
anova(m3c, m4)

#it doesn't. 

m5 <- lmer(annexed ~ pctmin + incgrowth9000pl + dependencyratio + pctownerocc + (1 + pctmin | plid2/STATEA), data = logitvariableslmer, REML = FALSE)
summary(m5)





















