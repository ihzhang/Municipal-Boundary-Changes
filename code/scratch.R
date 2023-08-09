# generate 2014 by interpolation ####
names(places2017) <- gsub("17p", "", names(places2017))
names(places2010) <- gsub("10p", "", names(places2010))

plids <- Reduce(intersect, list(unique(places2010$plid), unique(places2017$plid))) # find only places common to each other 
places2017 %<>%
  filter(plid %in% plids) 
places2010 %<>%
  filter(plid %in% plids)

commonvars <- Reduce(intersect, list(names(places2010), names(places2017)))
places2014 <- places2017 %>%
  mutate_at(all_of(commonvars[3:59]), ~NA)

places2010$Year <- 2010
places2014$Year <- 2014
places2017$Year <- 2017

places <- base::rbind(places2010, places2014, places2017) 
rm(plids)

places %<>%
  group_by(plid) %>%
  arrange(Year) %>%
  mutate_at(all_of(commonvars[3:59]), ~na.approx(., na.rm = F))

places2014 <- places %>%
  filter(Year == 2014)

summary(places2014)

# pctvars <- names(places2014)[grep("pct", names(places2014))]
# moneyvars <- c("mhmval", "hinc", "incomepp")
# vars <- names(places2014)[!names(places2014) %in% pctvars & !names(places2014) %in% moneyvars & !names(places2014) %in% c("plid", "Geo_NAME", "Year")]
# 
# places2014 %<>%
#   mutate_at(all_of(pctvars), ~ifelse(is.na(.) | !is.finite(.) | . < 0.1, 0.1, 
#                                      ifelse(. > 100, 100, .))) %>%
#   mutate_at(all_of(vars), ~ifelse(is.na(.) | . < 1, 1, .)) 
# 
# places2014 %<>%
#   mutate(mhmval = ifelse(mhmval < 10000 | is.na(mhmval), 9999,
#                          ifelse(mhmval > 2000001, 2000001, mhmval)),
#          hinc = ifelse(hinc < 2500 | is.na(hinc), 2499, 
#                        ifelse(hinc > 250001, 250001, hinc)),
#          incomepp = ifelse(incomepp < 2500 | is.na(incomepp), 2499, 
#                        ifelse(incomepp > 250001, 250001, incomepp)))
# summary(places2014)

names(places2014)[3:59] <- str_c(names(places2014)[3:59], "14p")

write_csv(places2014, "places2014_cleaned.csv")
rm(places, places2010, places2014, places2017)