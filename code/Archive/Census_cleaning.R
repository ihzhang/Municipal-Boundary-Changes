# Script: Clean Census Data ################################################################
# Create descriptive tables showing: #######################################################
# 1. % NHW, NHB, H, Minority, recent immigrants over time (1990, 2000, 2010, 2020) #########
# 2. % racial breakdown for VAP (1990, 2000, 2010, 2020) ###################################
# data are for the 14 states used in the Racial Gerrymandering Paper #######################
# created by: Iris Zhang, Sept 27, 2021 ####################################################

# updates log ----

rm(list = ls())
setwd("~/Google Drive/Stanford/QE2/PNAS")

library("tidyverse")
library("readr") #for write_csv and read_csv functions

# Clean 1990 data, downloaded from Social Explorer ####
c1990 <- read_csv("1990.csv")

# rename variables we need 
c1990 <- c1990 %>%
  mutate(pop = SE_T013_001,
         nhw = SE_T013_003,
         nhb = SE_T013_004,
         hisp = SE_T013_008,
         minority = (SE_T013_002 - nhw) + hisp,
         recimm = SE_T111_002,
         nhw_vap = rowSums(across(c(STF3_P014_016:STF3_P014_034, STF3_P014_048:STF3_P014_066))),
         nhb_vap = rowSums(across(c(STF3_P014_081:STF3_P014_099, STF3_P014_113:STF3_P014_131))),
         hisp_vap = NA,
         min_vap = rowSums(across(c(nhb_vap, STF3_P014_146:STF3_P014_164, STF3_P014_178:STF3_P014_196, STF3_P014_211:STF3_P014_229, STF3_P014_243:STF3_P014_261,
                       STF3_P014_276:STF3_P014_294, STF3_P014_308:STF3_P014_326))),
         vap = nhw_vap + min_vap) %>%
  select(Geo_STATE, pop, nhw, nhb, hisp, minority, recimm, nhw_vap, nhb_vap, hisp_vap, min_vap, vap) 

pop1990 <- c1990 %>%
  summarize(pop = sum(pop),
            nhw = sum(nhw),
            nhb = sum(nhb),
            hisp = NA,
            minority = sum(minority),
            recimm = sum(recimm),
            nhw_vap = sum(nhw_vap),
            nhb_vap = sum(nhb_vap),
            hisp_vap = NA,
            min_vap = sum(min_vap),
            vap = sum(vap)) %>%
  mutate(nhw_pct = (nhw/pop)*100,
         nhb_pct = (nhb/pop)*100,
         hisp_pct = (hisp/pop)*100,
         minority_pct = (minority/pop)*100,
         recimm_pct = (recimm/pop)*100,
         nhw_vap_pct = (nhw_vap/vap)*100,
         nhb_vap_pct = (nhb_vap/vap)*100,
         hisp_vap_pct = NA,
         min_vap_pct = (min_vap/vap)*100,
         Year = 1990)

# make long 
race1990 <- pop1990 %>% 
  select(Year, nhw_pct:minority_pct, recimm_pct) %>%
  pivot_longer(
    cols = !Year,
    names_to = c("race"),
    values_to = "pct"
  ) %>%
  mutate(race = gsub("_pct", "", race))

vap1990 <- pop1990 %>% 
  select(Year, nhw_vap_pct:min_vap_pct) %>%
  pivot_longer(
    cols = !Year,
    names_to = c("race"),
    values_to = "pct"
  ) %>%
  mutate(race = gsub("_vap_pct", "", race))

rm(c1990, pop1990)

# Clean 2000 data, downloaded from Social Explorer ####
c2000 <- read_csv("2000.csv")

c2000<- c2000 %>%
  mutate(pop = SE_T015_001,
         nhw = SE_T015_003,
         nhb = SE_T015_004,
         hisp = SE_T015_010,
         minority = (SE_T015_002 - nhw) + hisp,
         recimm = SF3_P022002 + SF3_P022003,
         vap = SF1_P005001,
         nhw_vap = SF1_P005003,
         nhb_vap = SF1_P005004,
         hisp_vap = NA,
         min_vap = (vap - nhw_vap)
         ) %>%
  select(Geo_STATE, pop, nhw, nhb, hisp, minority, recimm, nhw_vap, nhb_vap, hisp_vap, min_vap, vap) 

pop2000 <- c2000 %>%
  summarize(pop = sum(pop),
            nhw = sum(nhw),
            nhb = sum(nhb),
            hisp = sum(hisp),
            minority = sum(minority),
            recimm = sum(recimm),
            nhw_vap = sum(nhw_vap),
            nhb_vap = sum(nhb_vap),
            hisp_vap = NA,
            min_vap = sum(min_vap),
            vap = sum(vap)) %>%
  mutate(nhw_pct = (nhw/pop)*100,
         nhb_pct = (nhb/pop)*100,
         hisp_pct = (hisp/pop)*100,
         minority_pct = (minority/pop)*100,
         recimm_pct = (recimm/pop)*100,
         nhw_vap_pct = (nhw_vap/vap)*100,
         nhb_vap_pct = (nhb_vap/vap)*100,
         hisp_vap_pct = NA,
         min_vap_pct = (min_vap/vap)*100,
         Year = 2000)

# make long 
race2000 <- pop2000 %>% 
  select(Year, nhw_pct:minority_pct, recimm_pct) %>%
  pivot_longer(
    cols = !Year,
    names_to = c("race"),
    values_to = "pct"
  ) %>%
  mutate(race = gsub("_pct", "", race))

vap2000 <- pop2000 %>% 
  select(Year, nhw_vap_pct:min_vap_pct) %>%
  pivot_longer(
    cols = !Year,
    names_to = c("race"),
    values_to = "pct"
  ) %>%
  mutate(race = gsub("_vap_pct", "", race))

rm(c2000, pop2000)

# clean 2010 data, downloaded from Social Explorer ####
# need to combine 1-year ACS estimates for foreign born 

c2010 <- read_csv("2010.csv")
pfb <- read_csv("2010_pfb.csv")

c2010 <- c2010 %>%
  left_join(pfb %>% select(Geo_STATE, SE_A10058_002), by = "Geo_STATE")
rm(pfb)

# rename for variables needed
c2010 <- c2010 %>%
  mutate(pop = SE_T055_001,
         nhw = SE_T055_003,
         nhb = SE_T055_004,
         hisp = SE_T055_010,
         minority = (SE_T055_002 - nhw) + hisp,
         recimm = SE_A10058_002,
         vap = SF1_P0100001,
         nhw_vap = SF1_P0100003,
         nhb_vap = SF1_P0100004,
         hisp_vap = NA,
         min_vap = (vap - nhw_vap)
  ) %>%
  select(Geo_STATE, pop, nhw, nhb, hisp, minority, recimm, nhw_vap, nhb_vap, hisp_vap, min_vap, vap) 

pop2010 <- c2010 %>%
  summarize(pop = sum(pop),
            nhw = sum(nhw),
            nhb = sum(nhb),
            hisp = sum(hisp),
            minority = sum(minority),
            recimm = sum(recimm),
            nhw_vap = sum(nhw_vap),
            nhb_vap = sum(nhb_vap),
            hisp_vap = NA,
            min_vap = sum(min_vap),
            vap = sum(vap)) %>%
  mutate(nhw_pct = (nhw/pop)*100,
         nhb_pct = (nhb/pop)*100,
         hisp_pct = (hisp/pop)*100,
         minority_pct = (minority/pop)*100,
         recimm_pct = (recimm/pop)*100,
         nhw_vap_pct = (nhw_vap/vap)*100,
         nhb_vap_pct = (nhb_vap/vap)*100,
         hisp_vap_pct = NA,
         min_vap_pct = (min_vap/vap)*100,
         Year = 2010)

# make long 
race2010 <- pop2010 %>% 
  select(Year, nhw_pct:minority_pct, recimm_pct) %>%
  pivot_longer(
    cols = !Year,
    names_to = c("race"),
    values_to = "pct"
  ) %>%
  mutate(race = gsub("_pct", "", race))

vap2010 <- pop2010 %>% 
  select(Year, nhw_vap_pct:min_vap_pct) %>%
  pivot_longer(
    cols = !Year,
    names_to = c("race"),
    values_to = "pct"
  ) %>%
  mutate(race = gsub("_vap_pct", "", race))

rm(c2010, pop2010)

# clean data for 2020, downloaded from Social Explorer ####
# need to combine 1-year ACS estimates for foreign born in 2019 (latest available)

c2020 <- read_csv("2020.csv")
pfb <- read_csv("2019_pfb.csv")

c2020 <- c2020 %>%
  left_join(pfb %>% select(Geo_STATE, SE_A10058_002), by = "Geo_STATE")
rm(pfb)

# rename for variables needed
c2020 <- c2020 %>%
  mutate(pop = SE_T004_019,
         nhw = SE_T004_003,
         nhb = SE_T004_005,
         hisp = SE_T004_017,
         minority = (SE_T004_001 - nhw) + hisp,
         recimm = SE_A10058_002,
         vap = SE_T012_001,
         nhw_vap = SE_T012_003,
         nhb_vap = SE_T012_004,
         hisp_vap = SE_T012_010,
         min_vap = (vap - nhw_vap)
  ) %>%
  select(Geo_STATE, pop, nhw, nhb, hisp, minority, recimm, nhw_vap, nhb_vap, hisp_vap, min_vap, vap) 

pop2020 <- c2020 %>%
  summarize(pop = sum(pop),
            nhw = sum(nhw),
            nhb = sum(nhb),
            hisp = sum(hisp),
            minority = sum(minority),
            recimm = sum(recimm),
            nhw_vap = sum(nhw_vap),
            nhb_vap = sum(nhb_vap),
            hisp_vap = sum(hisp_vap),
            min_vap = sum(min_vap),
            vap = sum(vap)) %>%
  mutate(nhw_pct = (nhw/pop)*100,
         nhb_pct = (nhb/pop)*100,
         hisp_pct = (hisp/pop)*100,
         minority_pct = (minority/pop)*100,
         recimm_pct = (recimm/pop)*100,
         nhw_vap_pct = (nhw_vap/vap)*100,
         nhb_vap_pct = (nhb_vap/vap)*100,
         hisp_vap_pct = (hisp_vap/vap)*100,
         min_vap_pct = (min_vap/vap)*100,
         Year = 2020)

# make long 
race2020 <- pop2020 %>% 
  select(Year, nhw_pct:minority_pct, recimm_pct) %>%
  pivot_longer(
    cols = !Year,
    names_to = c("race"),
    values_to = "pct"
  ) %>%
  mutate(race = gsub("_pct", "", race))

vap2020 <- pop2020 %>% 
  select(Year, nhw_vap_pct:min_vap_pct) %>%
  pivot_longer(
    cols = !Year,
    names_to = c("race"),
    values_to = "pct"
  ) %>%
  mutate(race = gsub("_vap_pct", "", race))

rm(c2020, pop2020)

# combine data ####
race <- base::rbind(race1990, race2000, race2010, race2020)
vap <- base::rbind(vap1990, vap2000, vap2010, vap2020)

rm(race1990, race2000, race2010, race2020)
rm(vap1990, vap2000, vap2010, vap2020)

write_csv(race, "race_time.csv")
write_csv(vap, "vap_time.csv")

# make plots ####
race <- race %>%
  mutate(race = gsub("nhw", "Non-Hispanic White", race),
         race = gsub("nhb", "Non-Hispanic Black", race),
         race = gsub("hisp", "Hispanic", race),
         race = gsub("minority", "Non-White", race),
         race = gsub("recimm", "Recent Immigrant", race)) %>%
  mutate(race = factor(race, levels = c("Non-Hispanic White", "Non-White", "Non-Hispanic Black", "Hispanic", "Recent Immigrant")))

race_overtime = ggplot(
  race, 
  aes(x = race, 
      y = pct, 
      fill = race)) +
  geom_bar(stat="identity", position = "stack", colour="black", size = .2) + 
  scale_y_continuous(limits = c(0, 80)) +
  scale_fill_discrete() + 
  scale_fill_grey() + 
  facet_wrap(~ Year, ncol = 4) +
  theme_bw() + 
  theme(
    # Panel
    panel.spacing = unit(0,"lines"),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(), 
    panel.background = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(colour = "black"),
    # Legend
    legend.title = element_blank(),
    legend.position = "bottom",
    legend.text = element_text(size = 10),
    # x-axis 
    axis.text.x = element_text(angle = 45, hjust = 1),
    # Title
    plot.title = element_text(size = 12, hjust = .5),
    # Caption
    plot.caption = element_text(size = 10, hjust = .5, face = "italic")) +
  geom_text(aes(label=round(pct, digits = 1)), position=position_dodge(width=0.9), vjust=-0.25, size = 4) + 
  labs(x = "", y = "Percent of Total Population", 
       title = "Racial Composition Across 14 states from 1990-2020\n ", 
       caption = "\nData from Decennial Census; Recent Immigrant data for 2010 from 2010 1-Yr ACS; for 2020 from 2019 1-Yr ACS")

ggsave(race_overtime,
       file = "Figure 1.png",
       height = 10,
       width = 12,
       units = "in",
       dpi = 300)

# vap 
vap <- vap %>%
  mutate(race = gsub("nhw", "Non-Hispanic White", race),
         race = gsub("nhb", "Non-Hispanic Black", race),
         race = gsub("hisp", "Hispanic", race),
         race = gsub("min", "Non-White", race)) %>%
  mutate(race = factor(race, levels = c("Non-Hispanic White", "Non-White", "Non-Hispanic Black", "Hispanic")))

vap_overtime = ggplot(
  vap, 
  aes(x = race, 
      y = pct, 
      fill = race)) +
  geom_bar(stat="identity", position = "stack", colour="black", size = .2) + 
  scale_y_continuous(limits = c(0, 80)) +
  scale_fill_discrete() + 
  scale_fill_grey() + 
  facet_wrap(~ Year, ncol = 4) +
  theme_bw() + 
  theme(
    # Panel
    panel.spacing = unit(0,"lines"),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(), 
    panel.background = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(colour = "black"),
    # Legend
    legend.title = element_blank(),
    legend.position = "bottom",
    legend.text = element_text(size = 10),
    # x-axis 
    axis.text.x = element_text(angle = 45, hjust = 1),
    # Title
    plot.title = element_text(size = 12, hjust = .5),
    # Caption
    plot.caption = element_text(size = 10, hjust = .5, face = "italic")) +
  geom_text(aes(label=round(pct, digits = 1)), position=position_dodge(width=0.9), vjust=-0.25, size = 4) + 
  labs(x = "", y = "Percent of Total Population", 
       title = "Racial Composition of Voting Age Population Across 14 states from 1990-2020\n ", 
       caption = "\nData from Decennial Census")

ggsave(vap_overtime,
       file = "Figure 2.png",
       height = 10,
       width = 12,
       units = "in",
       dpi = 300)

rm(list = ls())
