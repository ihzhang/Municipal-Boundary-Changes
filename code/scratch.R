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


panel0020_did %<>% 
  mutate(period = ifelse(post == 1, 1, 0), 
         black_diff = pctnhblack_total - pctnhblack_p0,
         more_black = ifelse(pctnhblack_total > pctnhblack_p0, 1, 0),
         white_diff = (pctnhwhite_total - pctnhwhite_p0), 
         more_white = ifelse(pctnhwhite_total > pctnhwhite_p0, 1, 0),
         hisp_diff = (pcth_total - pcth_p0), 
         more_hisp = ifelse(pcth_total > pcth_p0, 1, 0),
         asian_diff = (pctasian_total - pctasian_p0), 
         more_asian = ifelse(pctasian_total > pctasian_p0, 1, 0),
         native_diff = (pctnative_total - pctnative_p0), 
         more_native = ifelse(pctnative_total > pctnative_p0, 1, 0),
         other_diff = (pctother_total - pctother_p0), 
         more_other = ifelse(pctother_total > pctother_p0, 1, 0),
         nbmin_diff = (pctnbmin_total - pctnbmin_p0),
         more_nbmin = ifelse(pctnbmin_total > pctnbmin_p0, 1, 0)) 

overtime_diff_p0 <- names(panel0020_did)[which(grepl("_p0", names(panel0020_did)))]
overtime_diff_p0 <- gsub("_p0", "", overtime_diff_p0)
overtime_diff_p1 <- names(panel0020_did)[which(grepl("_p1", names(panel0020_did)))]
overtime_diff_p1 <- gsub("_p1", "", overtime_diff_p1)

overtime_diff <- Reduce(intersect, list(overtime_diff_p0, overtime_diff_p1))
overtime_diff <- overtime_diff[which(grepl("pct", overtime_diff))]

for (variable in overtime_diff) {
  panel0020_did[[paste0(variable, "_diff")]] <- (panel0020_did[[paste0(variable, "_p1")]] - panel0020_did[[paste0(variable, "_p0")]])
}

rm(overtime_diff, overtime_diff_p0, overtime_diff_p1)
pl0007 <- panel0020_did %>% filter(time=="2000 to 2007")

sapply(panel0020_did, function(x) sum(is.na(x)))

panel0020_did %<>%
  filter_at(vars(pop_p0, popdensity_p0, popgrowth, pctnhwhite_p0, pctnhwhitegrowth, pctnhblack_total, pctnbmin_total, pctnhblackgrowth, pctnbmingrowth, more_white, pctowneroccupied_p0, mhmval_p0, hinc_p0, pctpov_p0, pctownerocc_total, pcthincjobs_total, pctincopp_total, pctnhblack_p1, pctnhwhite_p1, pctnbmin_p1, annexing, annexing_bas, pctnhwhite_diff, pctnhblack_diff, pctnbmin_diff), ~!is.na(.))

plids <- Reduce(intersect, list(unique(panel0020_did$plid[panel0020_did$time == "2007 to 2013"]), unique(panel0020_did$plid[panel0020_did$time == "2014 to 2020"])))

#plids <- Reduce(intersect, list(unique(panel0020_did$plid[panel0020_did$time == "2000 to 2007"]), unique(panel0020_did$plid[panel0020_did$time == "2007 to 2013"]), unique(panel0020_did$plid[panel0020_did$time == "2014 to 2020"])))

panel0020_did %<>%
  filter(plid %in% plids)

table(panel0020_did$annexing[panel0020_did$time=="2000 to 2007"], panel0020_did$annexing_use[panel0020_did$time=="2000 to 2007"])
2092/(2092+109)

table(panel0020_did$annexing[panel0020_did$time=="2007 to 2013"], panel0020_did$annexing_use[panel0020_did$time=="2007 to 2013"])
2272/(2272+795)

table(panel0020_did$annexing[panel0020_did$time=="2014 to 2020"], panel0020_did$annexing_use[panel0020_did$time=="2014 to 2020"])
328/(328+1049)
# 18.94%

length(unique(panel0020_did$plid))

#write_csv(panel0020_did, "analyticalfiles/panel_prestandard.csv")
#panel0020_did <- read_csv("analyticalfiles/panel_prestandard.csv")

summary(panel0020_did$popgrowth)
panel0020_did %<>%
  mutate_at(vars(c(ends_with("total"), ends_with("_p0"), ends_with("_p1"), ends_with("_total_1"), contains("growth"), contains("_annexed"), ends_with("_log"), contains("diff"))), 
            ~((.-mean(., na.rm = T))/sd(., na.rm = T))) 
summary(panel0020_did$popgrowth)
summary(panel0020_did)

panel0020_did <- read_csv("analyticalfiles/panel_prestandard.csv")

# 1. time and annex (trends, outcomes) ####
# show p0 over time 
vra_time <- panel0020_did %>%
  mutate(vra = as.numeric(as.character(vra))) %>%
  group_by(annexing, time, vra) %>%
  mutate(n = n()) %>%
  ungroup() %>%
  group_by(annexing, time, vra, n) %>%
  summarize_at(vars(c(contains("p0"), ends_with("growth"), ends_with("_total"), ends_with("_total_1"), ends_with("_diff"), more_white, lag_annexed)), ~mean(., na.rm = T)) %>%
  t() %>%
  as.data.frame() %>%
  tibble::rownames_to_column(., "variable")

# just look at VRA places ####
annex_time_vra <- panel0020_did %>%
  mutate(vra = as.numeric(as.character(vra))) %>%
  group_by(time, vra) %>%
  mutate(n = n()) %>%
  ungroup() %>%
  group_by(time, vra, n) %>%
  summarize_at(vars(c(contains("p0"), contains("p1"), ends_with("growth"), ends_with("total"), contains("diff"), annexing, more_white)), ~mean(., na.rm = T)) %>%
  t() %>%
  as.data.frame() %>%
  tibble::rownames_to_column(., "variable")

# fringe by annex
# show p0 over time 
annex_time <- panel0020_did %>%
  group_by(time, annexing) %>%
  mutate(n = n()) %>%
  ungroup() %>%
  group_by(time, annexing, n) %>%
  summarize_at(vars(c(contains("p0"), ends_with("growth"), ends_with("_total"), ends_with("_total_1"), ends_with("_diff"), more_white, vra, lag_annexed)), ~mean(., na.rm = T)) %>%
  t() %>%
  as.data.frame() %>%
  tibble::rownames_to_column(., "variable")

sd_outcome <- panel0020_did %>%
  summarize_at(vars(ends_with("_p1")), ~sd(., na.rm = T)) %>%
  t() %>%
  as.data.frame() %>%
  tibble::rownames_to_column(., "variable")

x <- list(annex_time_vra, annex_time, vra_time, sd_outcome)
write.xlsx(x, "analyticalfiles/annex_time_desc_vra.xlsx", rowNames = T)


# diff_outcome
annex_time_vra <- panel0020_did %>%
  mutate(vra = as.numeric(as.character(vra))) %>%
  group_by(annexing, time, vra) %>%
  mutate(n = n()) %>%
  ungroup() %>%
  group_by(annexing, time, vra, n) %>%
  summarize_at(vars(c(pctnhblack_diff, pctnhwhite_diff, pctnbmin_diff)), ~mean(., na.rm = T)) %>%
  t() %>%
  as.data.frame() %>%
  tibble::rownames_to_column(., "variable")

write.xlsx(annex_time_vra, "analyticalfiles/differences.xlsx", rowNames = T)

# annex or not ####
# conditional PTA 
reg = lm(annexing~vra*post + popdensity_p0 + pctnhwhite_p0 + pop_total, data=panel0020_did)

# Fictitious data set where control variables
# have a constant value for treatment and control group: here just set everywhere 0
dat0 = mutate(panel0020_did, 
              popdensity_p0 = 0,
              pctnhwhite_p0 = 0, 
              pop_total = 0)

# Now predict y and add residuals of original regression
panel0020_did$y = predict(reg, dat0) + resid(reg)

gdat = panel0020_did %>%
  group_by(vra, time) %>%
  dplyr::summarize(y = mean(y)) %>%
  ungroup() %>%
  mutate(vra = as.factor(vra))

gg = ggplot(gdat, aes(y=y,x=time, group = vra)) +
  geom_point() + geom_line(aes(linetype = vra)) + 
  geom_vline(xintercept="2007 to 2013") +
  theme_bw() + 
  ylab("% of Places Annexing") + 
  xlab("Time Period") + 
  labs(linetype = "VRA Coverage")
gg

x <- panel0020_did %>%
  mutate(vra = as.character(vra)) %>%
  group_by(vra, time) %>%
  dplyr::summarize(Percent = Hmisc::wtd.mean(as.numeric(as.character(annexing)), weights = popdensity_p0 + pctnhwhite_p0 + pop_total, na.rm = T)*100) %>%
  ungroup() %>%
  rename("Period" = "time")

# add # label 
plot_pct <- ggplot(x, aes(y = Percent, x = Period, group = vra)) + 
  geom_line(aes(linetype = vra)) + geom_point() + 
  theme(axis.text.x = element_text(angle = 50, hjust = 1, size = 12),
        axis.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12)) + 
  ggtitle("Proportion") + 
  labs(linetype = "Section 5")
plot_pct

ggsave("analyticalfiles/final/Annex_bin_PTA_bas_pct.pdf", 
       plot = plot_pct)

# number
x <- panel0020_did %>%
  mutate(vra = as.character(vra)) %>%
  group_by(vra, time) %>%
  summarize(Number = sum(annexing == 1)) %>%
  ungroup() %>%
  rename("Period" = "time")

# add # label 
plot_number <- ggplot(x, aes(y = Number, x = Period, group = vra)) + 
  geom_line(aes(linetype = vra)) + geom_point() + 
  theme(axis.text.x = element_text(angle = 50, hjust = 1, size = 12),
        axis.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12)) + 
  ggtitle("Number") + 
  labs(linetype = "Section 5")
plot_number

plot <- ggarrange(plot_pct, plot_number) 
plot

ggsave("analyticalfiles/final/Annex_bin_PTA_bas.pdf", 
       plot = plot)

# pctdiff 
x <- panel0020_did %>%
  mutate(vra = as.character(vra)) %>%
  group_by(vra, time, annexing) %>%
  dplyr::summarize_at(vars(pctnhblack_p1, pctnhwhite_p1, pctnbmin_p1), ~mean(., na.rm = T)) %>%
  ungroup() %>%
  rename("Period" = "time") %>%
  pivot_longer(cols = ends_with("_p1"),
               names_to = "Race",
               values_to = "pct") %>%
  mutate(Race = case_when(
    grepl("nhblack", Race) ~ "Black",
    grepl("nhwhite", Race) ~ "White",
    grepl("nbmin", Race) ~ "Non-Black Minority"
  ),
  vra_group = case_when(
    vra == 0 & annexing == 0 ~ "Not Section V, Non-Annexing",
    vra == 0 & annexing == 1 ~ "Not Section V, Annexing",
    vra == 1 & annexing == 0 ~ "Section V, Non-Annexing",
    vra == 1 & annexing == 1 ~ "Section V, Annexing"
  ))

plot_pct_0 <- ggplot(x %>% filter(annexing ==0), aes(y = pct, x = Period, group = vra_group)) + 
  geom_line(aes(linetype = vra_group)) + geom_point() + 
  facet_wrap(~Race) +
  theme(axis.text.x = element_text(angle = 50, hjust = 1, size = 12),
        axis.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12)) + 
  labs(linetype = "Section 5 Coverage, \nPeriod, and Annexation",
       y = "Population Share")
plot_pct_0

plot_pct_1 <- ggplot(x %>% filter(annexing == 1), aes(y = pct, x = Period, group = vra_group)) + 
  geom_line(aes(linetype = vra_group)) + geom_point() + 
  facet_wrap(~Race) +
  theme(axis.text.x = element_text(angle = 50, hjust = 1, size = 12),
        axis.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12)) + 
  labs(linetype = "Section 5 Coverage, \nPeriod, and Annexation",
       y = "Population Share")
plot_pct_1

x <- panel0020_did %>%
  mutate(vra = as.character(vra)) %>%
  group_by(vra, time) %>%
  dplyr::summarize_at(vars(pctnhblack_p1, pctnhwhite_p1, pctnbmin_p1), ~mean(., na.rm = T)) %>%
  ungroup() %>%
  rename("Period" = "time") %>%
  pivot_longer(cols = ends_with("_p1"),
               names_to = "Race",
               values_to = "pct") %>%
  mutate(Race = case_when(
    grepl("nhblack", Race) ~ "Black",
    grepl("nhwhite", Race) ~ "White",
    grepl("nbmin", Race) ~ "Non-Black Minority"
  ),
  Race = factor(Race, levels = c("Black", "White", "Non-Black Minority")))

# add # label 
plot_pct <- ggplot(x, aes(y = pct, x = Period, group = vra)) + 
  geom_line(aes(linetype = vra)) + geom_point() + 
  facet_wrap(~Race) +
  theme(axis.text.x = element_text(angle = 50, hjust = 1, size = 12),
        axis.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12)) + 
  ggtitle("Proportion") + 
  labs(linetype = "Section 5 \nCoverage",
       y = "Mean % at end of period")
plot_pct

plot <- ggarrange(plot_pct_0, plot_pct_1, nrow = 2)
ggsave("analyticalfiles/final/p1_vra_annex.pdf", 
       plot = plot, 
       dpi = 300)

ggsave("analyticalfiles/final/p1_did.pdf", 
       plot = plot_pct, 
       dpi = 300)

# find maps to make ####
panel0020_did %>%
  group_by(plid) %>%
  mutate(n_ann = sum(annexing==1)) %>%
  ungroup() %>%
  filter(n_ann == 3 & vra==1) %>% 
  select(plid, pop_total_1) %>%
  arrange(desc(pop_total_1)) %>% 
  View()
