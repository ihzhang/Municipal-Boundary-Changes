# 1. all annexing statese as from BAS 
# 2. get list of their blocks in 2000
# 3. get list of their blocks in 2010 
# 4. get list of their contiguous blocks in 2000 
# 5. identify blocks that got added (existed in 2000 but not in 2000 block)
# 6. get 1990-2000 data 

# 1. bas_states_places gives annexing places ####
bas <- 
  read.delim(file = "BAS0010.txt", header = T, na = "")

#save only States that are identified
bas <- bas[!is.na(bas$State), ]

#create unique ids for each annexing place
bas <- bas %>% 
  mutate(State = sprintf("%02.0f", State),
         FIPS.Place.Code = sprintf("%05.0f", FIPS.Place.Code),
         plid = paste0(State, FIPS.Place.Code))

bas %>%
  filter(Action == "Annexation") %>%
  group_by(plid) 

length(unique(bas$plid))
7943/25376

# states for analysis 
bas_states <- bas[bas$State=="01" | bas$State=="05" | bas$State==13 | bas$State==22 | bas$State==28 | bas$State==37 | bas$State==45 |
                    bas$State==51 | bas$State==24 | bas$State==38 | bas$State==46 | bas$State==47 | bas$State==21 | bas$State==10,]
bas_states_places <- as.data.frame(unique(bas_states$plid))
names(bas_states_places) <- "plid"
bas_states_places$plid <- as.character(bas_states_places$plid)
rm(bas_states)

# 2. get the list of blocks for annexing places in 2000 ####
blocks2000 <- read_csv("blocks2000_var.csv")
ablocks00 <- blocks2000 %>% 
  mutate(PLACEA = sprintf("%05.0f", PLACEA),
         plid = paste0(STATEA, PLACEA)) 

# 3. get the list of blocks for annexing places in 2010 ####
blocks2010 <- read_csv("2010blocks_converted.csv")
ablocks10 <- blocks2010 %>% 
  mutate(PLACEA = sprintf("%05.0f", PLACEA),
         plid = paste0(STATEA, PLACEA)) 

# 4. get the list of contiguous blocks for annexing places in 2000 ####
contigall2000 <- read_csv("allcontigblocks_20200201.csv")
table(contigall2000$PLACEA, exclude = NULL)

contigall2000 <- contigall2000 %>%
  mutate(plid = paste0(State, sprintf("%05.0f", PLACEA)))

contigall2000 <- contigall2000 %>%
  filter(PLACEA=="0" | PLACEA=="99999" | is.na(PLACEA) | PLACEA=="NA" | plid %in% cdp00$plid)

contig00 <- contigall2000 %>%
  mutate(PLACEA_1 = sprintf("%05.0f", PLACEA_1),
         plid = paste0(State, PLACEA_1)) %>%
  filter(plid %in% bas_states_places$plid)

# 5. identify annexations ####
annexedblocks <- data.frame()
plids <- unique(bas_states_places$plid)
for (i in plids) {
  block00 <- ablocks00[ablocks00$plid==i, c("GISJOIN", "plid")]
  block10 <- ablocks10[ablocks10$plid==i, c("GISJOIN", "plid")]
  block <- block10[!block10$GISJOIN %in% block00$GISJOIN,]
  annexedblocks <- rbind(annexedblocks, block)
}
names(annexedblocks)[2] <- "annexingplid"
annexedblocks$annexed <- 1
ablocks00 <- left_join(ablocks00, annexedblocks, by = c("GISJOIN"))
table(ablocks00$annexed)

# here are our annexed blocks: 
ablocks00 <- ablocks00 %>%
  filter(annexed==1 & !is.na(annexingplid)) %>%
  select(c("GISJOIN", "annexingplid", "annexed"))

# here are our contig blocks
contig00 <- contig00 %>% 
  select(c(plid, GISJOIN))
names(contig00)[1] <- "contig_plid"
contig00$contig <- 1

contig00 <- blocks2000 %>% 
  mutate(PLACEA = sprintf("%05.0f", PLACEA),
         plid = paste0(STATEA, PLACEA)) %>% 
  left_join(contig00, by = c("GISJOIN")) #%>%
  #select(c("GISJOIN", "contig_plid", "contig"))
table(contig00$contig)

# add annexed blocks to contig
aa <- contig00 %>%
  left_join(ablocks00, by = "GISJOIN")

aa <- aa %>%
  filter(!is.na(contig_plid) | !is.na(annexingplid))

aa <- aa %>% 
  mutate(annexed = ifelse(is.na(annexed), 0, annexed))
table(aa$annexed)

aa <- aa %>%
  mutate(plid = ifelse(plid!=contig_plid, contig_plid, plid),
         annexingplid = ifelse(is.na(annexingplid), plid, annexingplid))

names(aa)[which(names(aa)=="annexingplid")] <- "plid2"

# 6. Merge with 1990-2000 data ####
pl9000 <- read_csv("pl9000_var.csv")

pl9000 <- pl9000 %>%
  mutate(Geo_STATE = sprintf("%02.0f", Geo_STATE),
         Geo_PLACE = sprintf("%05.0f", Geo_PLACE),
         plid2 = paste0(Geo_STATE, Geo_PLACE)) 

#merge with aa
aa <- left_join(aa, pl9000, by = "plid2")

aa <- write_csv(aa, "annexedblocks0010dem_pl00_newsample.csv")
