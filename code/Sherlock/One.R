library("dplyr")
library("readr") # for write_csv and read_csv functions
library("foreach") # for %do% function
library("data.table") # package for handling large datasets 
library("magrittr") # for %<>% operator
library("zoo") # for na.approx function
library("stringr") # for str_pad
rm(list = ls())

# make interpolated block data #### 
# 2010 blocks ####
blocks2010 <- read_csv("blocks2010_var.csv")
blocks2010 %<>%
  dplyr::mutate(blkid = paste0(stringr::str_pad(STATEA, 2, side = "left", pad = "0"),
                        stringr::str_pad(COUNTYA, 3, side = "left", pad = "0"),
                        stringr::str_pad(TRACTA, 6, side = "left", pad = "0"),
                        stringr::str_pad(BLOCKA, 4, side = "left", pad = "0"))) %>%
  dplyr::select(blkid, pop10b:nbminvap10b) 
head(blocks2010$blkid)
length(unique(blocks2010$blkid))
names(blocks2010) <- gsub("10b", "", names(blocks2010))
blocks2010 %<>%
  dplyr::mutate(Year = "2010")

# blocks2010 %<>%
#   mutate_at(all_of(names(blocks2010)[2:(length(names(blocks2010))-1)]), ~ifelse(is.na(.) | .<1, 1, .))

# 2020 blocks ####
blocks2020 <- read_csv("blocks2020_var.csv")

blocks2020 %<>%
  dplyr::mutate(blkid = paste0(stringr::str_pad(STATEA, 2, side = "left", pad = "0"),
                               stringr::str_pad(COUNTYA, 3, side = "left", pad = "0"),
                               stringr::str_pad(TRACTA, 6, side = "left", pad = "0"),
                               stringr::str_pad(BLOCKA, 4, side = "left", pad = "0"))) %>%
  dplyr::select(blkid, pop20b:vacancy20b) %>%
  rename(owneroccupied20b = owneroccuped20b)

names(blocks2020) <- gsub("20b", "", names(blocks2020))
blocks2020 %<>%
  dplyr::mutate(Year = "2020")
head(blocks2020$blkid)
length(unique(blocks2020$blkid))

# do crosswalk 
cw <- read_csv("2020-to-2010_unique.csv")

blocks2020 %<>%
  left_join(cw, by = c("blkid" = "GEOID20")) %>%
  filter(!is.na(WEIGHT) & !is.na(PAREA)) %>%
  mutate_at(vars(pop:vacancy), ~(.*WEIGHT))

write_csv(blocks2020, "blocks2020_vars_2010b.csv")

blocks2020 %<>%
  mutate(blkid = GEOID10) %>%
  select(blkid:Year) 

# blocks2020 %<>%
#   mutate_at(all_of(names(blocks2020)[2:(length(names(blocks2020))-1)]), ~ifelse(is.na(.) | .<1, 1, .))

blocks2020 %<>%
  filter(!is.na(blkid))

# make unique blocks mutual to both ####
names_list <- Reduce(intersect, list(names(blocks2010), names(blocks2020)))
blkids <- Reduce(intersect, list(unique(blocks2010$blkid), unique(blocks2020$blkid)))

blocks2010 %<>%
  filter(blkid %in% blkids) %>%
  select(all_of(names_list))
blocks2020 %<>%
  filter(blkid %in% blkids) %>%
  select(all_of(names_list))

blocks <- base::rbind(blocks2010, blocks2020)
rm(blocks2020)

#2014 ####
blocks2014 <- blocks2010 %>%
  mutate_at(all_of(names_list[2:(length(names_list)-1)]), ~NA) %>%
  mutate(Year = "2014")

blocks <- base::rbind(blocks, blocks2014)
rm(blocks2014, blkids, cw, blocks2010)

blocks %<>%
  dplyr::group_by(blkid) %>%
  dplyr::arrange(Year) %>%
  dplyr::mutate_at(names_list[c(2:(length(names_list)-1))], ~zoo::na.approx(., na.rm = F)) %>%
  dplyr::ungroup()

blocks14 <- blocks %>%
  dplyr::filter(Year=="2014")

write_csv(blocks14, "blocks2014_int.csv")
rm(blocks14)

# 2007 interpolate -- 2010b ####
rm(blocks)
blocks2000 <- read_csv("blocks2000_var.csv")
names(blocks2000) <- gsub("00b", "", names(blocks2000))
blocks2000 %<>%
   mutate(Year = 2000) %>%
   select(-c(STATEA, COUNTYA, TRACTA, BLOCKA, PLACEA, dependencyratio)) 

cw <- read_csv("2000-to-2010_unique.csv")

blocks2000 %<>%
  left_join(cw, by = c("blkid" = "GEOID00")) %>%
  filter(!is.na(WEIGHT) & !is.na(PAREA)) %>%
  mutate(blkid = GEOID10) %>%
  select(-GEOID10) %>%
  mutate_at(vars(pop:nbminvap), ~(.*WEIGHT))

# blocks2000 %<>%
#    mutate_at(all_of(names(blocks2000)[2:(length(names(blocks2000))-3)]), ~ifelse(is.na(.) | .<1, 1, .)) %>%
#   select(-c(WEIGHT, PAREA)) 
 
blocks2010 <- read_csv("blocks2010_var.csv")
blocks2010 %<>%
  mutate(blkid = paste0(str_pad(STATEA, 2, side = "left", pad = "0"),
                        str_pad(COUNTYA, 3, side = "left", pad = "0"),
                        str_pad(TRACTA, 6, side = "left", pad = "0"),
                        str_pad(BLOCKA, 4, side = "left", pad = "0")))

names(blocks2010) <- gsub("10b", "", names(blocks2010))
blocks2010 %<>%
  mutate(Year = 2010) 

# blocks2010 %<>%
#   mutate_at(all_of(names(blocks2010)[6:(length(names(blocks2010))-2)]), ~ifelse(is.na(.) | .<1, 1, .))

blkids <- Reduce(intersect, list(unique(blocks2000$blkid), unique(blocks2010$blkid)))
names_list <- Reduce(intersect, list(names(blocks2010), names(blocks2000)))

blocks2000 %<>%
  filter(blkid %in% blkids) %>%
  select(all_of(names_list))

blocks2010 %<>%
  filter(blkid %in% blkids) %>%
  select(all_of(names_list))

blocks <- base::rbind(blocks2000, blocks2010)
rm(blocks2000)

# do 2007
blocks2007 <- blocks2010 %>%
  mutate_at(all_of(names_list[1:19]), ~NA) %>%
  mutate(Year = 2007)

blocks <- base::rbind(blocks, blocks2007)
rm(blocks2010, blocks2007)

table(blocks$Year)

blkids <- unique(blocks$blkid)
test <- blocks %>% filter(blkid %in% blkids[1:50])

test %<>%
  group_by(blkid) %>%
  arrange(Year) %>%
  mutate_at(all_of(names_list[1:19]), ~zoo::na.approx(., na.rm = F)) %>%
  ungroup() 

rm(test)
rm(blkids)

blocks %<>%
  group_by(blkid) %>%
  arrange(Year) %>%
  mutate_at(all_of(names_list[1:19]), ~zoo::na.approx(., na.rm = F)) %>%
  ungroup() 

blocks07 <- blocks %>%
  filter(Year == "2007")
write_csv(blocks07, "blocks2007_int.csv")

# 2007: 2000b ####
blocks2000 <- read_csv("blocks2000_var.csv")
names(blocks2000) <- gsub("00b", "", names(blocks2000))
blocks2000 %<>%
  mutate(Year = 2000) %>%
  select(-c(STATEA, COUNTYA, TRACTA, BLOCKA, PLACEA, dependencyratio)) 

cw <- read_csv("2000-to-2010_unique.csv")

blocks2010 <- read_csv("blocks2010_var.csv") %>%
  mutate(blkid = paste0(str_pad(STATEA, 2, side = "left", pad = "0"),
                        str_pad(COUNTYA, 3, side = "left", pad = "0"),
                        str_pad(TRACTA, 6, side = "left", pad = "0"),
                        str_pad(BLOCKA, 4, side = "left", pad = "0")))

blocks2010 %<>%
  left_join(cw, by = c("blkid" = "GEOID10")) %>%
  filter(!is.na(WEIGHT) & !is.na(PAREA)) %>%
  mutate(blkid = GEOID00) %>%
  select(-GEOID00) %>%
  mutate_at(vars(pop:nbminvap), ~(.*WEIGHT))

names(blocks2010) <- gsub("10b", "", names(blocks2010))
blocks2010 %<>%
  mutate(Year = 2010) 

# blocks2010 %<>%
#   mutate_at(all_of(names(blocks2010)[6:(length(names(blocks2010))-2)]), ~ifelse(is.na(.) | .<1, 1, .))

blkids <- Reduce(intersect, list(unique(blocks2000$blkid), unique(blocks2010$blkid)))
names_list <- Reduce(intersect, list(names(blocks2010), names(blocks2000)))

blocks2000 %<>%
  filter(blkid %in% blkids) %>%
  select(all_of(names_list))

blocks2010 %<>%
  filter(blkid %in% blkids) %>%
  select(all_of(names_list))

blocks <- base::rbind(blocks2000, blocks2010)
rm(blocks2000)

# do 2007
blocks2007 <- blocks2010 %>%
  mutate_at(all_of(names_list[1:19]), ~NA) %>%
  mutate(Year = 2007)

blocks <- base::rbind(blocks, blocks2007)
rm(blocks2010, blocks2007)

table(blocks$Year)

blkids <- unique(blocks$blkid)
test <- blocks %>% filter(blkid %in% blkids[1:50])

test %<>%
  group_by(blkid) %>%
  arrange(Year) %>%
  mutate_at(all_of(names_list[1:19]), ~zoo::na.approx(., na.rm = F)) %>%
  ungroup() 

rm(test)
rm(blkids)

blocks %<>%
  group_by(blkid) %>%
  arrange(Year) %>%
  mutate_at(all_of(names_list[1:19]), ~zoo::na.approx(., na.rm = F)) %>%
  ungroup() 

blocks07 <- blocks %>%
  filter(Year == "2007")
write_csv(blocks07, "blocks2007_int.csv")

blocks17 <- blocks14 %>%
  mutate_at(all_of(names_list[1:25]), ~NA) %>%
  mutate(Year = 2017)
rm(blocks14)

blocks %<>% 
  filter(Year != 2014)

blocks <- base::rbind(blocks, blocks17)
blocks %<>%
  group_by(blkid) %>%
  arrange(Year) %>%
  mutate_at(all_of(names_list[1:25]), zoo::na.approx, na.rm = F) %>%
  ungroup() 
  
blocks17 <- blocks %>%
  filter(Year == "2017")
write_csv(blocks17, "blocks2017_int.csv")

rm(list = ls())

#2007
blocks2007 <- blocks2000 %>%
  mutate_at(all_of(names_list[1:26]), ~NA) %>%
  mutate(Year = 2007)
blocks <- base::rbind(blocks, blocks2007)
rm(blocks2007, blocks2000)

blkids <- unique(blocks$blkid)
test <- blocks %>% filter(blkid %in% blkids[1:50])
test %<>%
  group_by(blkid) %>%
  arrange(Year) %>%
  mutate_at(all_of(names(blocks)[1:26]), zoo::na.approx, na.rm = F) %>%
  ungroup() 
rm(test, blkids)

blocks %<>%
  group_by(blkid) %>%
  arrange(Year) %>%
  mutate_at(all_of(names_list[1:26]), zoo::na.approx, na.rm = F) %>%
  ungroup() 

blocks07 <- blocks %>%
  filter(Year == "2007")
write_csv(blocks07, "blocks2007_int.csv")

f <- rep(seq_len(ceiling(length(blkids) / 1000000)), each = 1000000, length.out = nrow(blocks))
dat_use <- split(blocks, f = f)
foreach (i = 1:length(dat_use)) %do% {
  dat_use[[i]] <- 
    blk_interp(dat_use[[i]])
  return(NULL)
}














