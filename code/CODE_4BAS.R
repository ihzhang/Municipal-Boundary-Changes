# Use BAS 
# downloaded here: https://www.census.gov/geographies/reference-files/time-series/geo/bas/annex.html. 
library(lubridate)

# annual periods ----
bas0010 <- read.delim("BAS/BAS0010.txt", header = T)

bas0010 %<>%
  mutate(plid = paste0(str_pad(State, 2, "left", "0"), 
                       str_pad(FIPS.Place.Code, 5, "left", "0")),
         date = mdy(Effective.Date)) %>% 
  #filter(Action=="Annexation") %>%
  select(plid, date, Action)

years <- seq(11, 21, 1)

bas_list <- list()
for (i in 1:length(years)) {
  bas_list[[i]] <- read.delim(paste0("BAS/US_bas", years[[i]], ".txt"), header = T) 
}
bas_list <- data.table::rbindlist(bas_list, fill = TRUE)

bas_list %<>%
  mutate(plid = paste0(str_pad(State, 2, "left", "0"), 
                       str_pad(FIPS.Place.Code, 5, "left", "0")),
         plid = ifelse(is.na(plid), paste0(str_pad(State.FIPS, 2, "left", "0"), 
                                          str_pad(Entity.FIPS.Code, 5, "left", "0")), plid),
         date = if_else(is.na(Effective.Date), mdy(Local.Effective.Date), mdy(Effective.Date)), 
         Action = ifelse(Action=="A", "Annexation", Action)) %>% 
  #filter(Action=="Annexation") %>%
  select(plid, date, Action)

table(is.na(bas_list$date))

bas0020 <- base::rbind(bas0010, bas_list)
rm(bas_list, bas0010)

pl_year <- bas0020 %>%
  filter(dplyr::between(date, as.Date("2007-01-01"), as.Date("2019-12-31"))) %>%
  mutate(period = case_when(
    date >= "2007-01-01" & date <= "2007-12-31" ~ "2007 to 2008",
    date >= "2008-01-01" & date <= "2008-12-31" ~ "2008 to 2009",
    date >= "2009-01-01" & date <= "2009-12-31" ~ "2009 to 2010",
    date >= "2010-01-01" & date <= "2010-12-31" ~ "2010 to 2011",
    date >= "2011-01-01" & date <= "2011-12-31" ~ "2011 to 2012",
    date >= "2012-01-01" & date <= "2012-12-31" ~ "2012 to 2013",
    date >= "2014-01-01" & date <= "2014-12-31" ~ "2014 to 2015",
    date >= "2015-01-01" & date <= "2015-12-31" ~ "2015 to 2016",
    date >= "2016-01-01" & date <= "2016-12-31" ~ "2016 to 2017",
    date >= "2017-01-01" & date <= "2017-12-31" ~ "2017 to 2018",
    date >= "2018-01-01" & date <= "2018-12-31" ~ "2018 to 2019",
    date >= "2019-01-01" & date <= "2019-12-31" ~ "2019 to 2020",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(period))

bas_stats <- pl_year %>%
  group_by(period) %>%
  dplyr::summarize(n = n(), 
         n_annex = sum(Action == "Annexation")) %>%
  mutate(annexed = (n_annex/n)*100)

write_csv(bas_stats, "analyticalfiles/final/bas_stats.csv")

pl_year %>%
  filter(Action == "Annexation") %>%
  group_by(plid) %>%
  dplyr::summarize(n = n())
#8,180 unique plids 

length(unique(pl_year$plid))

write_csv(bas_stats, "analyticalfiles/final/bas_stats.csv")

pl_year %<>%
  group_by(plid, period) %>%
  dplyr::summarize(n = n()) %>%
  ungroup()

table(pl_year$period)
write_csv(pl_year, "analyticalfiles/bas_years.csv")

rm(list = ls())

# 6-year periods ----
library(lubridate)

bas0010 <- read.delim("BAS/BAS0010.txt", header = T)

bas0010 %<>%
  mutate(plid = paste0(str_pad(State, 2, "left", "0"), 
                       str_pad(FIPS.Place.Code, 5, "left", "0")),
         date = mdy(Effective.Date)) %>% 
  #filter(Action=="Annexation") %>%
  select(plid, date, Action)

years <- seq(11, 21, 1)

bas_list <- list()
for (i in 1:length(years)) {
  bas_list[[i]] <- read.delim(paste0("BAS/US_bas", years[[i]], ".txt"), header = T) 
}
bas_list <- data.table::rbindlist(bas_list, fill = TRUE)

bas_list %<>%
  mutate(plid = paste0(str_pad(State, 2, "left", "0"), 
                       str_pad(FIPS.Place.Code, 5, "left", "0")),
         plid = ifelse(is.na(plid), paste0(str_pad(State.FIPS, 2, "left", "0"), 
                                           str_pad(Entity.FIPS.Code, 5, "left", "0")), plid),
         date = if_else(is.na(Effective.Date), mdy(Local.Effective.Date), mdy(Effective.Date)), 
         Action = ifelse(Action=="A", "Annexation", Action)) %>% 
  #filter(Action=="Annexation") %>%
  select(plid, date, Action)

table(is.na(bas_list$date))

bas0020 <- base::rbind(bas0010, bas_list)
rm(bas_list, bas0010)

pl_year <- bas0020 %>%
  filter(dplyr::between(date, as.Date("2000-01-01"), as.Date("2019-12-31"))) %>%
  mutate(period = case_when(
    date >= "2000-01-01" & date <= "2006-12-31" ~ "2000 to 2007",
    date >= "2007-01-01" & date <= "2012-12-31" ~ "2007 to 2013",
    date >= "2014-01-01" & date <= "2019-12-31" ~ "2014 to 2020",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(period))

bas_stats <- pl_year %>%
  group_by(period) %>%
  dplyr::summarize(n = n(), 
                   n_annex = sum(Action == "Annexation")) %>%
  mutate(annexed = (n_annex/n)*100)

pl_year %>%
  filter(Action == "Annexation") %>%
  group_by(plid) %>%
  dplyr::summarize(n = n())
#8,180 unique plids 

length(unique(pl_year$plid))

pl_year %<>%
  group_by(plid, period) %>%
  dplyr::summarize(n = n()) %>%
  ungroup()

table(pl_year$period)
write_csv(pl_year, "analyticalfiles/bas_years_0020.csv")

rm(list = ls())
