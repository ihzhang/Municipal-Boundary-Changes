# Use BAS 
# downloaded here: https://www.census.gov/geographies/reference-files/time-series/geo/bas/annex.html. 
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
  filter(between(date, "2000-01-01", "2019-12-31")) %>%
  mutate(period = case_when(
    between(date, "2000-01-01", "2006-12-31") ~ "2000 to 2007",
    between(date, "2007-01-01", "2012-12-31") ~ "2007 to 2013",
    between(date, "2014-01-01", "2019-12-31") ~ "2014 to 2020",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(period))

bas_stats <- pl_year %>%
  group_by(period) %>%
  summarize(n = n(), 
         n_annex = sum(Action == "Annexation")) %>%
  mutate(annexed = (n_annex/n)*100)

write_csv(bas_stats, "analyticalfiles/final/bas_stats.csv")

pl_year %>%
  filter(Action == "Annexation") %>%
  group_by(plid) %>%
  summarize(n = n())
#8,180 unique plids 

length(unique(pl_year$plid))

write_csv(bas_stats, "analyticalfiles/final/bas_stats.csv")

pl_year %<>%
  group_by(plid, period) %>%
  summarize(n = n()) %>%
  ungroup()

table(pl_year$period)
write_csv(pl_year, "analyticalfiles/bas_years.csv")

rm(list = ls())

# cog 
cog <- read.delim("COG/2007/IndFin07a.txt", header = T)
