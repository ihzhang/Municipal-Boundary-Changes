# consequences of annexing ####
# ref: before Shelby, no VRA, not annexing ----
panel0020_did %<>%
  mutate(annexing = relevel(as.factor(annexing), ref = "0"),
         period = relevel(as.factor(period), ref = "0"),
         vra = relevel(as.factor(vra), ref = "0"))

nhb_base <- feols(pctnhblack_p1 ~ as.factor(annexing)*as.factor(period)*as.factor(vra) | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid)
summary(nhb_base)

nhb_base_cov <- feols(pctnhblack_p1 ~ as.factor(annexing)*as.factor(period)*as.factor(vra) + pctnhblack_p0 + pctnhblackgrowth + pctnhblack_total | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid)
summary(nhb_base_cov) # N = 27987

nhw_base <- feols(pctnhwhite_p1 ~ as.factor(annexing)*as.factor(vra)*as.factor(period) | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid)
summary(nhw_base) # N = 27987

nhw_base_cov <- feols(pctnhwhite_p1 ~ as.factor(annexing)*as.factor(vra)*as.factor(period) + pctnhwhite_p0 + pctnhwhitegrowth + pctnhwhite_total | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid)
summary(nhw_base_cov) # N = 27987

nbmin_base <- feols(pctnbmin_p1 ~ as.factor(annexing)*as.factor(vra)*as.factor(period) | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid)
summary(nbmin_base) # N = 27987

nbmin_base_cov <- feols(pctnbmin_p1 ~ as.factor(annexing)*as.factor(vra)*as.factor(period) + pctnbmin_p0 + pctnbmingrowth + pctnbmin_total | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid)
summary(nbmin_base_cov) # N = 27987

p1_diff <- list(tidy(nhb_base), tidy(nhb_base_cov), tidy(nhw_base), tidy(nhw_base_cov), tidy(nbmin_base), tidy(nbmin_base_cov))
p1_diff_glance <- list(nhb_base, nhb_base_cov, nhw_base, nhw_base_cov, nbmin_base, nbmin_base_cov)

for (model_stat in names(glance(nhb_base))) {
  for (i in 1:6) {
    p1_diff[[i]][[model_stat]] <- glance(p1_diff_glance[[i]])[[model_stat]]
  }
}
openxlsx::write.xlsx(p1_diff, "analyticalfiles/results/p1_pre-Shelby_uncovered.xlsx")

# ref: before Shelby, VRA, not annexing ----
panel0020_did %<>%
  mutate(annexing = relevel(as.factor(annexing), ref = "0"),
         period = relevel(as.factor(period), ref = "0"),
         vra = relevel(as.factor(vra), ref = "1"))

nhb_base <- feols(pctnhblack_p1 ~ as.factor(annexing)*as.factor(period)*as.factor(vra) | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid)
summary(nhb_base)

nhb_base_cov <- feols(pctnhblack_p1 ~ as.factor(annexing)*as.factor(period)*as.factor(vra) + pctnhblack_p0 + pctnhblackgrowth + pctnhblack_total | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid)
summary(nhb_base_cov) # N = 27987

nhw_base <- feols(pctnhwhite_p1 ~ as.factor(annexing)*as.factor(vra)*as.factor(period) | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid)
summary(nhw_base) # N = 27987

nhw_base_cov <- feols(pctnhwhite_p1 ~ as.factor(annexing)*as.factor(vra)*as.factor(period) + pctnhwhite_p0 + pctnhwhitegrowth + pctnhwhite_total | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid)
summary(nhw_base_cov) # N = 27987

nbmin_base <- feols(pctnbmin_p1 ~ as.factor(annexing)*as.factor(vra)*as.factor(period) | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid)
summary(nbmin_base) # N = 27987

nbmin_base_cov <- feols(pctnbmin_p1 ~ as.factor(annexing)*as.factor(vra)*as.factor(period) + pctnbmin_p0 + pctnbmingrowth + pctnbmin_total | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid)
summary(nbmin_base_cov) # N = 27987

p1_diff <- list(tidy(nhb_base), tidy(nhb_base_cov), tidy(nhw_base), tidy(nhw_base_cov), tidy(nbmin_base), tidy(nbmin_base_cov))
p1_diff_glance <- list(nhb_base, nhb_base_cov, nhw_base, nhw_base_cov, nbmin_base, nbmin_base_cov)

for (model_stat in names(glance(nhb_base))) {
  for (i in 1:6) {
    p1_diff[[i]][[model_stat]] <- glance(p1_diff_glance[[i]])[[model_stat]]
  }
}
openxlsx::write.xlsx(p1_diff, "analyticalfiles/results/p1_pre-Shelby_covered.xlsx")

# ref: after Shelby, VRA, not Annexing ---- 
panel0020_did %<>%
  mutate(annexing = relevel(as.factor(annexing), ref = "0"), 
         period = relevel(as.factor(period), ref = "1"),
         vra = relevel(as.factor(vra), ref = "1"))

nhb_base <- feols(pctnhblack_p1 ~ as.factor(annexing)*as.factor(period)*as.factor(vra) | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid)
summary(nhb_base)

nhb_base_cov <- feols(pctnhblack_p1 ~ as.factor(annexing)*as.factor(period)*as.factor(vra) + pctnhblack_p0 + pctnhblackgrowth + pctnhblack_total | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid)
summary(nhb_base_cov) # N = 27987

nhw_base <- feols(pctnhwhite_p1 ~ as.factor(annexing)*as.factor(vra)*as.factor(period) | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid)
summary(nhw_base) # N = 27987

nhw_base_cov <- feols(pctnhwhite_p1 ~ as.factor(annexing)*as.factor(vra)*as.factor(period) + pctnhwhite_p0 + pctnhwhitegrowth + pctnhwhite_total | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid)
summary(nhw_base_cov) # N = 27987

nbmin_base <- feols(pctnbmin_p1 ~ as.factor(annexing)*as.factor(vra)*as.factor(period) | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid)
summary(nbmin_base) # N = 27987

nbmin_base_cov <- feols(pctnbmin_p1 ~ as.factor(annexing)*as.factor(vra)*as.factor(period) + pctnbmin_p0 + pctnbmingrowth + pctnbmin_total | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid)
summary(nbmin_base_cov) # N = 27987

p1_diff <- list(tidy(nhb_base), tidy(nhb_base_cov), tidy(nhw_base), tidy(nhw_base_cov), tidy(nbmin_base), tidy(nbmin_base_cov))
p1_diff_glance <- list(nhb_base, nhb_base_cov, nhw_base, nhw_base_cov, nbmin_base, nbmin_base_cov)

for (model_stat in names(glance(nhb_base))) {
  for (i in 1:6) {
    p1_diff[[i]][[model_stat]] <- glance(p1_diff_glance[[i]])[[model_stat]]
  }
}
openxlsx::write.xlsx(p1_diff, "analyticalfiles/results/p1_post_covered.xlsx")

# ref: after Shelby, non-Section V, non-annexing ----
panel0020_did %<>%
  mutate(annexing = relevel(as.factor(annexing), ref = "0"), 
         period = relevel(as.factor(period), ref = "1"),
         vra = relevel(as.factor(vra), ref = "0"))

nhb_base <- feols(pctnhblack_p1 ~ as.factor(annexing)*as.factor(period)*as.factor(vra) | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid)
summary(nhb_base)

nhb_base_cov <- feols(pctnhblack_p1 ~ as.factor(annexing)*as.factor(period)*as.factor(vra) + pctnhblack_p0 + pctnhblackgrowth + pctnhblack_total | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid)
summary(nhb_base_cov) # N = 27987

nhw_base <- feols(pctnhwhite_p1 ~ as.factor(annexing)*as.factor(vra)*as.factor(period) | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid)
summary(nhw_base) # N = 27987

nhw_base_cov <- feols(pctnhwhite_p1 ~ as.factor(annexing)*as.factor(vra)*as.factor(period) + pctnhwhite_p0 + pctnhwhitegrowth + pctnhwhite_total | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid)
summary(nhw_base_cov) # N = 27987

nbmin_base <- feols(pctnbmin_p1 ~ as.factor(annexing)*as.factor(vra)*as.factor(period) | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid)
summary(nbmin_base) # N = 27987

nbmin_base_cov <- feols(pctnbmin_p1 ~ as.factor(annexing)*as.factor(vra)*as.factor(period) + pctnbmin_p0 + pctnbmingrowth + pctnbmin_total | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid)
summary(nbmin_base_cov) # N = 27987

p1_diff <- list(tidy(nhb_base), tidy(nhb_base_cov), tidy(nhw_base), tidy(nhw_base_cov), tidy(nbmin_base), tidy(nbmin_base_cov))
p1_diff_glance <- list(nhb_base, nhb_base_cov, nhw_base, nhw_base_cov, nbmin_base, nbmin_base_cov)

for (model_stat in names(glance(nhb_base))) {
  for (i in 1:6) {
    p1_diff[[i]][[model_stat]] <- glance(p1_diff_glance[[i]])[[model_stat]]
  }
}
openxlsx::write.xlsx(p1_diff, "analyticalfiles/results/p1_post_uncovered.xlsx")

# is it just an anticipation effect? ----
plids <- Reduce(intersect, list(unique(panel0020_did$plid[panel0020_did$time == "2000 to 2007"]), unique(panel0020_did$plid[panel0020_did$time == "2014 to 2020"])))

panel0020_did %<>%
  filter(plid %in% plids)

# ref: pre, no VRA, no annexing ----
panel0020_did %<>%
  mutate(vra = relevel(as.factor(vra), ref = "0"),
         period = relevel(as.factor(period), ref = "0"), 
         annexing = relevel(as.factor(annexing), ref = "0"))

nhb_base <- feols(pctnhblack_p1 ~ as.factor(annexing)*as.factor(period)*as.factor(vra) | plid, data = panel0020_did %>% filter(time %in% c("2000 to 2007", "2014 to 2020") & plid %in% plids), cluster = ~plid)
summary(nhb_base)

nhb_base_cov <- feols(pctnhblack_p1 ~ as.factor(annexing)*as.factor(period)*as.factor(vra) + pctnhblack_p0 + pctnhblackgrowth + pctnhblack_total | plid, data = panel0020_did %>% filter(time %in% c("2000 to 2007", "2014 to 2020") & plid %in% plids), cluster = ~plid)
summary(nhb_base_cov) # N = 27987

nhw_base <- feols(pctnhwhite_p1 ~ as.factor(annexing)*as.factor(period)*as.factor(vra) | plid, data = panel0020_did %>% filter(time %in% c("2000 to 2007", "2014 to 2020") & plid %in% plids), cluster = ~plid)
summary(nhw_base) # N = 27987

nhw_base_cov <- feols(pctnhwhite_p1 ~ as.factor(annexing)*as.factor(period)*as.factor(vra) + pctnhwhite_p0 + pctnhwhitegrowth + pctnhwhite_total | plid, data = panel0020_did %>% filter(time %in% c("2000 to 2007", "2014 to 2020") & plid %in% plids), cluster = ~plid)
summary(nhw_base_cov) # N = 27987

nbmin_base <- feols(pctnbmin_p1 ~ as.factor(annexing)*as.factor(period)*as.factor(vra) | plid, data = panel0020_did %>% filter(time %in% c("2000 to 2007", "2014 to 2020") & plid %in% plids), cluster = ~plid)
summary(nbmin_base) # N = 27987

nbmin_base_cov <- feols(pctnbmin_p1 ~ as.factor(annexing)*as.factor(period)*as.factor(vra) + pctnbmin_p0 + pctnbmingrowth + pctnbmin_total | plid, data = panel0020_did %>% filter(time %in% c("2000 to 2007", "2014 to 2020") & plid %in% plids), cluster = ~plid)
summary(nbmin_base_cov) # N = 27987

p1_diff <- list(tidy(nhb_base), tidy(nhb_base_cov), tidy(nhw_base), tidy(nhw_base_cov), tidy(nbmin_base), tidy(nbmin_base_cov))
p1_diff_glance <- list(nhb_base, nhb_base_cov, nhw_base, nhw_base_cov, nbmin_base, nbmin_base_cov)

for (model_stat in names(glance(nhb_base))) {
  for (i in 1:6) {
    p1_diff[[i]][[model_stat]] <- glance(p1_diff_glance[[i]])[[model_stat]]
  }
}
openxlsx::write.xlsx(p1_diff, "analyticalfiles/results/0007_pre_uncovered.xlsx")

# ref: post, no VRA, no annexing ----
panel0020_did %<>%
  mutate(vra = relevel(as.factor(vra), ref = "0"),
         period = relevel(as.factor(period), ref = "1"))

nhb_base <- feols(pctnhblack_p1 ~ as.factor(annexing)*as.factor(period)*as.factor(vra) | plid, data = panel0020_did %>% filter(time %in% c("2000 to 2007", "2014 to 2020") & plid %in% plids), cluster = ~plid)
summary(nhb_base)

nhb_base_cov <- feols(pctnhblack_p1 ~ as.factor(annexing)*as.factor(period)*as.factor(vra) + pctnhblack_p0 + pctnhblackgrowth + pctnhblack_total | plid, data = panel0020_did %>% filter(time %in% c("2000 to 2007", "2014 to 2020") & plid %in% plids), cluster = ~plid)
summary(nhb_base_cov) # N = 27987

nhw_base <- feols(pctnhwhite_p1 ~ as.factor(annexing)*as.factor(period)*as.factor(vra) | plid, data = panel0020_did %>% filter(time %in% c("2000 to 2007", "2014 to 2020") & plid %in% plids), cluster = ~plid)
summary(nhw_base) # N = 27987

nhw_base_cov <- feols(pctnhwhite_p1 ~ as.factor(annexing)*as.factor(period)*as.factor(vra) + pctnhwhite_p0 + pctnhwhitegrowth + pctnhwhite_total | plid, data = panel0020_did %>% filter(time %in% c("2000 to 2007", "2014 to 2020") & plid %in% plids), cluster = ~plid)
summary(nhw_base_cov) # N = 27987

nbmin_base <- feols(pctnbmin_p1 ~ as.factor(annexing)*as.factor(period)*as.factor(vra) | plid, data = panel0020_did %>% filter(time %in% c("2000 to 2007", "2014 to 2020") & plid %in% plids), cluster = ~plid)
summary(nbmin_base) # N = 27987

nbmin_base_cov <- feols(pctnbmin_p1 ~ as.factor(annexing)*as.factor(period)*as.factor(vra) + pctnbmin_p0 + pctnbmingrowth + pctnbmin_total | plid, data = panel0020_did %>% filter(time %in% c("2000 to 2007", "2014 to 2020") & plid %in% plids), cluster = ~plid)
summary(nbmin_base_cov) # N = 27987

p1_diff <- list(tidy(nhb_base), tidy(nhb_base_cov), tidy(nhw_base), tidy(nhw_base_cov), tidy(nbmin_base), tidy(nbmin_base_cov))
p1_diff_glance <- list(nhb_base, nhb_base_cov, nhw_base, nhw_base_cov, nbmin_base, nbmin_base_cov)

for (model_stat in names(glance(nhb_base))) {
  for (i in 1:6) {
    p1_diff[[i]][[model_stat]] <- glance(p1_diff_glance[[i]])[[model_stat]]
  }
}
openxlsx::write.xlsx(p1_diff, "analyticalfiles/results/0007_post_uncovered.xlsx")

# ref: pre, VRA, no annexing ----
panel0020_did %<>%
  mutate(vra = relevel(as.factor(vra), ref = "1"),
         period = relevel(as.factor(period), ref = "0"))

nhb_base <- feols(pctnhblack_p1 ~ as.factor(annexing)*as.factor(period)*as.factor(vra) | plid, data = panel0020_did %>% filter(time %in% c("2000 to 2007", "2014 to 2020") & plid %in% plids), cluster = ~plid)
summary(nhb_base)

nhb_base_cov <- feols(pctnhblack_p1 ~ as.factor(annexing)*as.factor(period)*as.factor(vra) + pctnhblack_p0 + pctnhblackgrowth + pctnhblack_total | plid, data = panel0020_did %>% filter(time %in% c("2000 to 2007", "2014 to 2020") & plid %in% plids), cluster = ~plid)
summary(nhb_base_cov) # N = 27987

nhw_base <- feols(pctnhwhite_p1 ~ as.factor(annexing)*as.factor(period)*as.factor(vra) | plid, data = panel0020_did %>% filter(time %in% c("2000 to 2007", "2014 to 2020") & plid %in% plids), cluster = ~plid)
summary(nhw_base) # N = 27987

nhw_base_cov <- feols(pctnhwhite_p1 ~ as.factor(annexing)*as.factor(period)*as.factor(vra) + pctnhwhite_p0 + pctnhwhitegrowth + pctnhwhite_total | plid, data = panel0020_did %>% filter(time %in% c("2000 to 2007", "2014 to 2020") & plid %in% plids), cluster = ~plid)
summary(nhw_base_cov) # N = 27987

nbmin_base <- feols(pctnbmin_p1 ~ as.factor(annexing)*as.factor(period)*as.factor(vra) | plid, data = panel0020_did %>% filter(time %in% c("2000 to 2007", "2014 to 2020") & plid %in% plids), cluster = ~plid)
summary(nbmin_base) # N = 27987

nbmin_base_cov <- feols(pctnbmin_p1 ~ as.factor(annexing)*as.factor(period)*as.factor(vra) + pctnbmin_p0 + pctnbmingrowth + pctnbmin_total | plid, data = panel0020_did %>% filter(time %in% c("2000 to 2007", "2014 to 2020") & plid %in% plids), cluster = ~plid)
summary(nbmin_base_cov) # N = 27987

p1_diff <- list(tidy(nhb_base), tidy(nhb_base_cov), tidy(nhw_base), tidy(nhw_base_cov), tidy(nbmin_base), tidy(nbmin_base_cov))
p1_diff_glance <- list(nhb_base, nhb_base_cov, nhw_base, nhw_base_cov, nbmin_base, nbmin_base_cov)

for (model_stat in names(glance(nhb_base))) {
  for (i in 1:6) {
    p1_diff[[i]][[model_stat]] <- glance(p1_diff_glance[[i]])[[model_stat]]
  }
}
openxlsx::write.xlsx(p1_diff, "analyticalfiles/results/0007_pre_covered.xlsx")

# ref: post, VRA, no annexing ----
panel0020_did %<>%
  mutate(vra = relevel(as.factor(vra), ref = "1"),
         period = relevel(as.factor(period), ref = "1"))

nhb_base <- feols(pctnhblack_p1 ~ as.factor(annexing)*as.factor(period)*as.factor(vra) | plid, data = panel0020_did %>% filter(time %in% c("2000 to 2007", "2014 to 2020") & plid %in% plids), cluster = ~plid)
summary(nhb_base)

nhb_base_cov <- feols(pctnhblack_p1 ~ as.factor(annexing)*as.factor(period)*as.factor(vra) + pctnhblack_p0 + pctnhblackgrowth + pctnhblack_total | plid, data = panel0020_did %>% filter(time %in% c("2000 to 2007", "2014 to 2020") & plid %in% plids), cluster = ~plid)
summary(nhb_base_cov) # N = 27987

nhw_base <- feols(pctnhwhite_p1 ~ as.factor(annexing)*as.factor(period)*as.factor(vra) | plid, data = panel0020_did %>% filter(time %in% c("2000 to 2007", "2014 to 2020") & plid %in% plids), cluster = ~plid)
summary(nhw_base) # N = 27987

nhw_base_cov <- feols(pctnhwhite_p1 ~ as.factor(annexing)*as.factor(vra)*as.factor(period) + pctnhwhite_p0 + pctnhwhitegrowth + pctnhwhite_total | plid, data = panel0020_did %>% filter(time %in% c("2000 to 2007", "2014 to 2020") & plid %in% plids), cluster = ~plid)
summary(nhw_base_cov) # N = 27987

nbmin_base <- feols(pctnbmin_p1 ~ as.factor(annexing)*as.factor(vra)*as.factor(period) | plid, data = panel0020_did %>% filter(time %in% c("2000 to 2007", "2014 to 2020") & plid %in% plids), cluster = ~plid)
summary(nbmin_base) # N = 27987

nbmin_base_cov <- feols(pctnbmin_p1 ~ as.factor(annexing)*as.factor(vra)*as.factor(period) + pctnbmin_p0 + pctnbmingrowth + pctnbmin_total | plid, data = panel0020_did %>% filter(time %in% c("2000 to 2007", "2014 to 2020") & plid %in% plids), cluster = ~plid)
summary(nbmin_base_cov) # N = 27987

p1_diff <- list(tidy(nhb_base), tidy(nhb_base_cov), tidy(nhw_base), tidy(nhw_base_cov), tidy(nbmin_base), tidy(nbmin_base_cov))
p1_diff_glance <- list(nhb_base, nhb_base_cov, nhw_base, nhw_base_cov, nbmin_base, nbmin_base_cov)

for (model_stat in names(glance(nhb_base))) {
  for (i in 1:6) {
    p1_diff[[i]][[model_stat]] <- glance(p1_diff_glance[[i]])[[model_stat]]
  }
}
openxlsx::write.xlsx(p1_diff, "analyticalfiles/results/0007_post_covered.xlsx")

# poverty and hinc ---- 
panel0020_did %<>%
  mutate(vra = relevel(as.factor(vra), ref = "0"),
         period = relevel(as.factor(period), ref = "0"))

ppov_v0p0 <- feols(pctpov_p1 ~ as.factor(annexing)*as.factor(period)*as.factor(vra) + pctpov_p0 | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid)
summary(ppov_v0p0) 

hinc_v0p0 <- feols(hinc_p1 ~ as.factor(annexing)*as.factor(vra)*as.factor(period) + hinc_p0 | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid)
summary(hinc_v0p0) 

panel0020_did %<>%
  mutate(vra = relevel(as.factor(vra), ref = "0"),
         period = relevel(as.factor(period), ref = "1"))

ppov_v0p1 <- feols(pctpov_p1 ~ as.factor(annexing)*as.factor(period)*as.factor(vra) + pctpov_p0 | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid)
summary(ppov_v0p1) 

hinc_v0p1 <- feols(hinc_p1 ~ as.factor(annexing)*as.factor(vra)*as.factor(period) + hinc_p0 | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid)
summary(hinc_v0p1) 

panel0020_did %<>%
  mutate(vra = relevel(as.factor(vra), ref = "1"),
         period = relevel(as.factor(period), ref = "0"))

ppov_v1p0 <- feols(pctpov_p1 ~ as.factor(annexing)*as.factor(period)*as.factor(vra) + pctpov_p0 | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid)
summary(ppov_v1p0) 

hinc_v1p0 <- feols(hinc_p1 ~ as.factor(annexing)*as.factor(vra)*as.factor(period) + hinc_p0 | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid)
summary(hinc_v1p0) 

panel0020_did %<>%
  mutate(vra = relevel(as.factor(vra), ref = "1"),
         period = relevel(as.factor(period), ref = "1"))

ppov_v1p1 <- feols(pctpov_p1 ~ as.factor(annexing)*as.factor(period)*as.factor(vra) + pctpov_p0 | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid)
summary(ppov_v1p1) 

hinc_v1p1 <- feols(hinc_p1 ~ as.factor(annexing)*as.factor(vra)*as.factor(period) + hinc_p0 | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid)
summary(hinc_v1p1) 

p1_diff <- list(tidy(ppov_v0p0), tidy(hinc_v0p0), tidy(ppov_v0p1), tidy(hinc_v0p1), tidy(ppov_v1p0), tidy(hinc_v1p0), tidy(ppov_v1p1), tidy(hinc_v1p1))
p1_diff_glance <- list(ppov_v0p0, hinc_v0p0, ppov_v0p1, hinc_v0p1, ppov_v1p0, hinc_v1p0, ppov_v1p1, hinc_v1p1)

for (model_stat in names(glance(ppov_v0p0))) {
  for (i in 1:8) {
    p1_diff[[i]][[model_stat]] <- glance(p1_diff_glance[[i]])[[model_stat]]
  }
}
openxlsx::write.xlsx(p1_diff, "analyticalfiles/results/ppov_hinc.xlsx")

# who's living on the periphery over time? ----
panel0020_did %<>%
  mutate(vra = relevel(as.factor(vra), ref = "0"),
         period = relevel(as.factor(period), ref = "0"))

nhb_base <- feols(pctnhblack_total ~ period | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid)
summary(nhb_base)

nhw_base <- feols(pctnhwhite_total ~ as.factor(period) | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid)
summary(nhw_base) # N = 27987

nbmin_base <- feols(pctnbmin_total ~ as.factor(period) | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid)
summary(nbmin_base) # N = 27987

p1_diff <- list(tidy(nhb_base), tidy(nhw_base), tidy(nbmin_base))
p1_diff_glance <- list(nhb_base, nhw_base, nbmin_base)

for (model_stat in names(glance(nhb_base))) {
  for (i in 1:3) {
    p1_diff[[i]][[model_stat]] <- glance(p1_diff_glance[[i]])[[model_stat]]
  }
}
openxlsx::write.xlsx(p1_diff, "analyticalfiles/results/periphery_pct.xlsx")

# do again for validated sample...----
panel0020_did <- read_csv("analyticalfiles/panel_prestandard.csv")
panel0020_did %<>%
  mutate_at(vars(c(ends_with("total"), ends_with("_p0"), ends_with("_p1"), ends_with("_total_1"), contains("growth"), contains("_annexed"), ends_with("_log"), contains("diff"))), 
            ~((.-mean(., na.rm = T))/sd(., na.rm = T))) 

# add variables 
annex <- feols(annexing_bas ~ as.factor(vra)*as.factor(period) | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid, fixef.rm = "none")
summary(annex)

annex_twop <- feols(annexing_bas ~ as.factor(vra)*as.factor(period) + pop_p0 + popdensity_p0 + popgrowth +  pctnhblack_p0 + pctnhblackgrowth + pctnbmin_p0 + pctnbmingrowth + pctblackpov_p0 + pctnbminpov_p0 + pctowneroccupied_p0 + mhmval_p0 + hinc_p0 + pctpov_p0 + as.factor(more_white) + pctnhblack_total + pctnbmin_total + pctownerocc_total + pcthincjobs_total + pctincopp_total + lag_annexed | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid, fixef.rm = "none")
summary(annex_twop)

ann_tid <- tidy(annex)
for (model_stat in names(glance(annex))) {
  ann_tid[[model_stat]] <- glance(annex)[[model_stat]]
}

ann_2_tid <- tidy(annex_twop)
for (model_stat in names(glance(annex_twop))) {
  ann_2_tid[[model_stat]] <- glance(annex_twop)[[model_stat]]
}

annex_list <- list(ann_tid, ann_2_tid)

openxlsx::write.xlsx(annex_list, "analyticalfiles/results/bas/annex_reg.xlsx")

# consequences of annexing ####
# ref: pre, uncovered ----
panel0020_did %<>%
  mutate(annexing = relevel(as.factor(annexing), ref = "0"),
         period = relevel(as.factor(period), ref = "0"),
         vra = relevel(as.factor(vra), ref = "0"))

nhb_base <- feols(pctnhblack_p1 ~ as.factor(annexing_bas)*as.factor(period)*as.factor(vra) | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid)
summary(nhb_base)

nhb_base_cov <- feols(pctnhblack_p1 ~ as.factor(annexing_bas)*as.factor(period)*as.factor(vra) + pctnhblack_p0 + pctnhblackgrowth + pctnhblack_total | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid)
summary(nhb_base_cov) # N = 27987

nhw_base <- feols(pctnhwhite_p1 ~ as.factor(annexing_bas)*as.factor(vra)*as.factor(period) | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid)
summary(nhw_base) # N = 27987

nhw_base_cov <- feols(pctnhwhite_p1 ~ as.factor(annexing_bas)*as.factor(vra)*as.factor(period) + pctnhwhite_p0 + pctnhwhitegrowth + pctnhwhite_total | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid)
summary(nhw_base_cov) # N = 27987

nbmin_base <- feols(pctnbmin_p1 ~ as.factor(annexing_bas)*as.factor(vra)*as.factor(period) | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid)
summary(nbmin_base) # N = 27987

nbmin_base_cov <- feols(pctnbmin_p1 ~ as.factor(annexing_bas)*as.factor(vra)*as.factor(period) + pctnbmin_p0 + pctnbmingrowth + pctnbmin_total | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid)
summary(nbmin_base_cov) # N = 27987

p1_diff <- list(tidy(nhb_base), tidy(nhb_base_cov), tidy(nhw_base), tidy(nhw_base_cov), tidy(nbmin_base), tidy(nbmin_base_cov))
p1_diff_glance <- list(nhb_base, nhb_base_cov, nhw_base, nhw_base_cov, nbmin_base, nbmin_base_cov)

for (model_stat in names(glance(nhb_base))) {
  for (i in 1:6) {
    p1_diff[[i]][[model_stat]] <- glance(p1_diff_glance[[i]])[[model_stat]]
  }
}
openxlsx::write.xlsx(p1_diff, "analyticalfiles/results/bas/p1_pre-Shelby_uncovered.xlsx")

# ref: before Shelby, VRA, not annexing ----
panel0020_did %<>%
  mutate(annexing = relevel(as.factor(annexing), ref = "0"),
         period = relevel(as.factor(period), ref = "0"),
         vra = relevel(as.factor(vra), ref = "1"))

nhb_base <- feols(pctnhblack_p1 ~ as.factor(annexing_bas)*as.factor(period)*as.factor(vra) | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid)
summary(nhb_base)

nhb_base_cov <- feols(pctnhblack_p1 ~ as.factor(annexing_bas)*as.factor(period)*as.factor(vra) + pctnhblack_p0 + pctnhblackgrowth + pctnhblack_total | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid)
summary(nhb_base_cov) # N = 27987

nhw_base <- feols(pctnhwhite_p1 ~ as.factor(annexing_bas)*as.factor(vra)*as.factor(period) | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid)
summary(nhw_base) # N = 27987

nhw_base_cov <- feols(pctnhwhite_p1 ~ as.factor(annexing_bas)*as.factor(vra)*as.factor(period) + pctnhwhite_p0 + pctnhwhitegrowth + pctnhwhite_total | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid)
summary(nhw_base_cov) # N = 27987

nbmin_base <- feols(pctnbmin_p1 ~ as.factor(annexing_bas)*as.factor(vra)*as.factor(period) | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid)
summary(nbmin_base) # N = 27987

nbmin_base_cov <- feols(pctnbmin_p1 ~ as.factor(annexing_bas)*as.factor(vra)*as.factor(period) + pctnbmin_p0 + pctnbmingrowth + pctnbmin_total | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid)
summary(nbmin_base_cov) # N = 27987

p1_diff <- list(tidy(nhb_base), tidy(nhb_base_cov), tidy(nhw_base), tidy(nhw_base_cov), tidy(nbmin_base), tidy(nbmin_base_cov))
p1_diff_glance <- list(nhb_base, nhb_base_cov, nhw_base, nhw_base_cov, nbmin_base, nbmin_base_cov)

for (model_stat in names(glance(nhb_base))) {
  for (i in 1:6) {
    p1_diff[[i]][[model_stat]] <- glance(p1_diff_glance[[i]])[[model_stat]]
  }
}
openxlsx::write.xlsx(p1_diff, "analyticalfiles/results/bas/p1_pre-Shelby_covered.xlsx")

# ref: after Shelby, VRA, not annexing_bas ---- 
panel0020_did %<>%
  mutate(annexing_bas = relevel(as.factor(annexing_bas), ref = "0"), 
         period = relevel(as.factor(period), ref = "1"),
         vra = relevel(as.factor(vra), ref = "1"))

nhb_base <- feols(pctnhblack_p1 ~ as.factor(annexing_bas)*as.factor(period)*as.factor(vra) | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid)
summary(nhb_base)

nhb_base_cov <- feols(pctnhblack_p1 ~ as.factor(annexing_bas)*as.factor(period)*as.factor(vra) + pctnhblack_p0 + pctnhblackgrowth + pctnhblack_total | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid)
summary(nhb_base_cov) # N = 27987

nhw_base <- feols(pctnhwhite_p1 ~ as.factor(annexing_bas)*as.factor(vra)*as.factor(period) | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid)
summary(nhw_base) # N = 27987

nhw_base_cov <- feols(pctnhwhite_p1 ~ as.factor(annexing_bas)*as.factor(vra)*as.factor(period) + pctnhwhite_p0 + pctnhwhitegrowth + pctnhwhite_total | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid)
summary(nhw_base_cov) # N = 27987

nbmin_base <- feols(pctnbmin_p1 ~ as.factor(annexing_bas)*as.factor(vra)*as.factor(period) | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid)
summary(nbmin_base) # N = 27987

nbmin_base_cov <- feols(pctnbmin_p1 ~ as.factor(annexing_bas)*as.factor(vra)*as.factor(period) + pctnbmin_p0 + pctnbmingrowth + pctnbmin_total | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid)
summary(nbmin_base_cov) # N = 27987

p1_diff <- list(tidy(nhb_base), tidy(nhb_base_cov), tidy(nhw_base), tidy(nhw_base_cov), tidy(nbmin_base), tidy(nbmin_base_cov))
p1_diff_glance <- list(nhb_base, nhb_base_cov, nhw_base, nhw_base_cov, nbmin_base, nbmin_base_cov)

for (model_stat in names(glance(nhb_base))) {
  for (i in 1:6) {
    p1_diff[[i]][[model_stat]] <- glance(p1_diff_glance[[i]])[[model_stat]]
  }
}
openxlsx::write.xlsx(p1_diff, "analyticalfiles/results/bas/p1_post_covered.xlsx")

# ref: after Shelby, non-Section V, non-annexing_bas ----
panel0020_did %<>%
  mutate(annexing_bas = relevel(as.factor(annexing_bas), ref = "0"), 
         period = relevel(as.factor(period), ref = "1"),
         vra = relevel(as.factor(vra), ref = "0"))

nhb_base <- feols(pctnhblack_p1 ~ as.factor(annexing_bas)*as.factor(period)*as.factor(vra) | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid)
summary(nhb_base)

nhb_base_cov <- feols(pctnhblack_p1 ~ as.factor(annexing_bas)*as.factor(period)*as.factor(vra) + pctnhblack_p0 + pctnhblackgrowth + pctnhblack_total | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid)
summary(nhb_base_cov) # N = 27987

nhw_base <- feols(pctnhwhite_p1 ~ as.factor(annexing_bas)*as.factor(vra)*as.factor(period) | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid)
summary(nhw_base) # N = 27987

nhw_base_cov <- feols(pctnhwhite_p1 ~ as.factor(annexing_bas)*as.factor(vra)*as.factor(period) + pctnhwhite_p0 + pctnhwhitegrowth + pctnhwhite_total | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid)
summary(nhw_base_cov) # N = 27987

nbmin_base <- feols(pctnbmin_p1 ~ as.factor(annexing_bas)*as.factor(vra)*as.factor(period) | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid)
summary(nbmin_base) # N = 27987

nbmin_base_cov <- feols(pctnbmin_p1 ~ as.factor(annexing_bas)*as.factor(vra)*as.factor(period) + pctnbmin_p0 + pctnbmingrowth + pctnbmin_total | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid)
summary(nbmin_base_cov) # N = 27987

p1_diff <- list(tidy(nhb_base), tidy(nhb_base_cov), tidy(nhw_base), tidy(nhw_base_cov), tidy(nbmin_base), tidy(nbmin_base_cov))
p1_diff_glance <- list(nhb_base, nhb_base_cov, nhw_base, nhw_base_cov, nbmin_base, nbmin_base_cov)

for (model_stat in names(glance(nhb_base))) {
  for (i in 1:6) {
    p1_diff[[i]][[model_stat]] <- glance(p1_diff_glance[[i]])[[model_stat]]
  }
}
openxlsx::write.xlsx(p1_diff, "analyticalfiles/results/bas/p1_post_uncovered.xlsx")

# is it just an anticipation effect? ----
# ref: pre, no VRA, no annexing_bas ----
plids <- Reduce(intersect, list(unique(panel0020_did$plid[panel0020_did$time == "2000 to 2007"]), unique(panel0020_did$plid[panel0020_did$time == "2014 to 2020"])))

panel0020_did %<>%
  filter(plid %in% plids)

panel0020_did %<>%
  mutate(vra = relevel(as.factor(vra), ref = "0"),
         period = relevel(as.factor(period), ref = "0"), 
         annexing_bas = relevel(as.factor(annexing_bas), ref = "0"))

nhb_base <- feols(pctnhblack_p1 ~ as.factor(annexing_bas)*as.factor(period)*as.factor(vra) | plid, data = panel0020_did %>% filter(time %in% c("2000 to 2007", "2014 to 2020")), cluster = ~plid)
summary(nhb_base)

nhb_base_cov <- feols(pctnhblack_p1 ~ as.factor(annexing_bas)*as.factor(period)*as.factor(vra) + pctnhblack_p0 + pctnhblackgrowth + pctnhblack_total | plid, data = panel0020_did %>% filter(time %in% c("2000 to 2007", "2014 to 2020")), cluster = ~plid)
summary(nhb_base_cov) # N = 27987

nhw_base <- feols(pctnhwhite_p1 ~ as.factor(annexing_bas)*as.factor(period)*as.factor(vra) | plid, data = panel0020_did %>% filter(time %in% c("2000 to 2007", "2014 to 2020")), cluster = ~plid)
summary(nhw_base) # N = 27987

nhw_base_cov <- feols(pctnhwhite_p1 ~ as.factor(annexing_bas)*as.factor(period)*as.factor(vra) + pctnhwhite_p0 + pctnhwhitegrowth + pctnhwhite_total | plid, data = panel0020_did %>% filter(time %in% c("2000 to 2007", "2014 to 2020")), cluster = ~plid)
summary(nhw_base_cov) # N = 27987

nbmin_base <- feols(pctnbmin_p1 ~ as.factor(annexing_bas)*as.factor(period)*as.factor(vra) | plid, data = panel0020_did %>% filter(time %in% c("2000 to 2007", "2014 to 2020")), cluster = ~plid)
summary(nbmin_base) # N = 27987

nbmin_base_cov <- feols(pctnbmin_p1 ~ as.factor(annexing_bas)*as.factor(period)*as.factor(vra) + pctnbmin_p0 + pctnbmingrowth + pctnbmin_total | plid, data = panel0020_did %>% filter(time %in% c("2000 to 2007", "2014 to 2020")), cluster = ~plid)
summary(nbmin_base_cov) # N = 27987

p1_diff <- list(tidy(nhb_base), tidy(nhb_base_cov), tidy(nhw_base), tidy(nhw_base_cov), tidy(nbmin_base), tidy(nbmin_base_cov))
p1_diff_glance <- list(nhb_base, nhb_base_cov, nhw_base, nhw_base_cov, nbmin_base, nbmin_base_cov)

for (model_stat in names(glance(nhb_base))) {
  for (i in 1:6) {
    p1_diff[[i]][[model_stat]] <- glance(p1_diff_glance[[i]])[[model_stat]]
  }
}
openxlsx::write.xlsx(p1_diff, "analyticalfiles/results/bas/0007_pre_uncovered.xlsx")

# ref: post, no VRA, no annexing_bas ----
panel0020_did %<>%
  mutate(vra = relevel(as.factor(vra), ref = "0"),
         period = relevel(as.factor(period), ref = "1"))

nhb_base <- feols(pctnhblack_p1 ~ as.factor(annexing_bas)*as.factor(period)*as.factor(vra) | plid, data = panel0020_did %>% filter(time %in% c("2000 to 2007", "2014 to 2020")), cluster = ~plid)
summary(nhb_base)

nhb_base_cov <- feols(pctnhblack_p1 ~ as.factor(annexing_bas)*as.factor(period)*as.factor(vra) + pctnhblack_p0 + pctnhblackgrowth + pctnhblack_total | plid, data = panel0020_did %>% filter(time %in% c("2000 to 2007", "2014 to 2020")), cluster = ~plid)
summary(nhb_base_cov) # N = 27987

nhw_base <- feols(pctnhwhite_p1 ~ as.factor(annexing_bas)*as.factor(period)*as.factor(vra) | plid, data = panel0020_did %>% filter(time %in% c("2000 to 2007", "2014 to 2020")), cluster = ~plid)
summary(nhw_base) # N = 27987

nhw_base_cov <- feols(pctnhwhite_p1 ~ as.factor(annexing_bas)*as.factor(period)*as.factor(vra) + pctnhwhite_p0 + pctnhwhitegrowth + pctnhwhite_total | plid, data = panel0020_did %>% filter(time %in% c("2000 to 2007", "2014 to 2020")), cluster = ~plid)
summary(nhw_base_cov) # N = 27987

nbmin_base <- feols(pctnbmin_p1 ~ as.factor(annexing_bas)*as.factor(period)*as.factor(vra) | plid, data = panel0020_did %>% filter(time %in% c("2000 to 2007", "2014 to 2020")), cluster = ~plid)
summary(nbmin_base) # N = 27987

nbmin_base_cov <- feols(pctnbmin_p1 ~ as.factor(annexing_bas)*as.factor(period)*as.factor(vra) + pctnbmin_p0 + pctnbmingrowth + pctnbmin_total | plid, data = panel0020_did %>% filter(time %in% c("2000 to 2007", "2014 to 2020")), cluster = ~plid)
summary(nbmin_base_cov) # N = 27987

p1_diff <- list(tidy(nhb_base), tidy(nhb_base_cov), tidy(nhw_base), tidy(nhw_base_cov), tidy(nbmin_base), tidy(nbmin_base_cov))
p1_diff_glance <- list(nhb_base, nhb_base_cov, nhw_base, nhw_base_cov, nbmin_base, nbmin_base_cov)

for (model_stat in names(glance(nhb_base))) {
  for (i in 1:6) {
    p1_diff[[i]][[model_stat]] <- glance(p1_diff_glance[[i]])[[model_stat]]
  }
}
openxlsx::write.xlsx(p1_diff, "analyticalfiles/results/bas/0007_post_uncovered.xlsx")

# ref: pre, VRA, no annexing_bas ----
panel0020_did %<>%
  mutate(vra = relevel(as.factor(vra), ref = "1"),
         period = relevel(as.factor(period), ref = "0"))

nhb_base <- feols(pctnhblack_p1 ~ as.factor(annexing_bas)*as.factor(period)*as.factor(vra) | plid, data = panel0020_did %>% filter(time %in% c("2000 to 2007", "2014 to 2020")), cluster = ~plid)
summary(nhb_base)

nhb_base_cov <- feols(pctnhblack_p1 ~ as.factor(annexing_bas)*as.factor(period)*as.factor(vra) + pctnhblack_p0 + pctnhblackgrowth + pctnhblack_total | plid, data = panel0020_did %>% filter(time %in% c("2000 to 2007", "2014 to 2020")), cluster = ~plid)
summary(nhb_base_cov) # N = 27987

nhw_base <- feols(pctnhwhite_p1 ~ as.factor(annexing_bas)*as.factor(period)*as.factor(vra) | plid, data = panel0020_did %>% filter(time %in% c("2000 to 2007", "2014 to 2020")), cluster = ~plid)
summary(nhw_base) # N = 27987

nhw_base_cov <- feols(pctnhwhite_p1 ~ as.factor(annexing_bas)*as.factor(period)*as.factor(vra) + pctnhwhite_p0 + pctnhwhitegrowth + pctnhwhite_total | plid, data = panel0020_did %>% filter(time %in% c("2000 to 2007", "2014 to 2020")), cluster = ~plid)
summary(nhw_base_cov) # N = 27987

nbmin_base <- feols(pctnbmin_p1 ~ as.factor(annexing_bas)*as.factor(period)*as.factor(vra) | plid, data = panel0020_did %>% filter(time %in% c("2000 to 2007", "2014 to 2020")), cluster = ~plid)
summary(nbmin_base) # N = 27987

nbmin_base_cov <- feols(pctnbmin_p1 ~ as.factor(annexing_bas)*as.factor(period)*as.factor(vra) + pctnbmin_p0 + pctnbmingrowth + pctnbmin_total | plid, data = panel0020_did %>% filter(time %in% c("2000 to 2007", "2014 to 2020")), cluster = ~plid)
summary(nbmin_base_cov) # N = 27987

p1_diff <- list(tidy(nhb_base), tidy(nhb_base_cov), tidy(nhw_base), tidy(nhw_base_cov), tidy(nbmin_base), tidy(nbmin_base_cov))
p1_diff_glance <- list(nhb_base, nhb_base_cov, nhw_base, nhw_base_cov, nbmin_base, nbmin_base_cov)

for (model_stat in names(glance(nhb_base))) {
  for (i in 1:6) {
    p1_diff[[i]][[model_stat]] <- glance(p1_diff_glance[[i]])[[model_stat]]
  }
}
openxlsx::write.xlsx(p1_diff, "analyticalfiles/results/bas/0007_pre_covered.xlsx")

# ref: post, VRA, no annexing_bas ----
panel0020_did %<>%
  mutate(vra = relevel(as.factor(vra), ref = "1"),
         period = relevel(as.factor(period), ref = "1"))

nhb_base <- feols(pctnhblack_p1 ~ as.factor(annexing_bas)*as.factor(period)*as.factor(vra) | plid, data = panel0020_did %>% filter(time %in% c("2000 to 2007", "2014 to 2020")), cluster = ~plid)
summary(nhb_base)

nhb_base_cov <- feols(pctnhblack_p1 ~ as.factor(annexing_bas)*as.factor(period)*as.factor(vra) + pctnhblack_p0 + pctnhblackgrowth + pctnhblack_total | plid, data = panel0020_did %>% filter(time %in% c("2000 to 2007", "2014 to 2020")), cluster = ~plid)
summary(nhb_base_cov) # N = 27987

nhw_base <- feols(pctnhwhite_p1 ~ as.factor(annexing_bas)*as.factor(period)*as.factor(vra) | plid, data = panel0020_did %>% filter(time %in% c("2000 to 2007", "2014 to 2020")), cluster = ~plid)
summary(nhw_base) # N = 27987

nhw_base_cov <- feols(pctnhwhite_p1 ~ as.factor(annexing_bas)*as.factor(vra)*as.factor(period) + pctnhwhite_p0 + pctnhwhitegrowth + pctnhwhite_total | plid, data = panel0020_did %>% filter(time %in% c("2000 to 2007", "2014 to 2020")), cluster = ~plid)
summary(nhw_base_cov) # N = 27987

nbmin_base <- feols(pctnbmin_p1 ~ as.factor(annexing_bas)*as.factor(vra)*as.factor(period) | plid, data = panel0020_did %>% filter(time %in% c("2000 to 2007", "2014 to 2020")), cluster = ~plid)
summary(nbmin_base) # N = 27987

nbmin_base_cov <- feols(pctnbmin_p1 ~ as.factor(annexing_bas)*as.factor(vra)*as.factor(period) + pctnbmin_p0 + pctnbmingrowth + pctnbmin_total | plid, data = panel0020_did %>% filter(time %in% c("2000 to 2007", "2014 to 2020")), cluster = ~plid)
summary(nbmin_base_cov) # N = 27987

p1_diff <- list(tidy(nhb_base), tidy(nhb_base_cov), tidy(nhw_base), tidy(nhw_base_cov), tidy(nbmin_base), tidy(nbmin_base_cov))
p1_diff_glance <- list(nhb_base, nhb_base_cov, nhw_base, nhw_base_cov, nbmin_base, nbmin_base_cov)

for (model_stat in names(glance(nhb_base))) {
  for (i in 1:6) {
    p1_diff[[i]][[model_stat]] <- glance(p1_diff_glance[[i]])[[model_stat]]
  }
}
openxlsx::write.xlsx(p1_diff, "analyticalfiles/results/bas/0007_post_covered.xlsx")

# poverty and hinc ---- 
panel0020_did %<>%
  mutate(vra = relevel(as.factor(vra), ref = "0"),
         period = relevel(as.factor(period), ref = "0"))

ppov_v0p0 <- feols(pctpov_p1 ~ as.factor(annexing_bas)*as.factor(period)*as.factor(vra) + pctpov_p0 | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid)
summary(ppov_v0p0) 

hinc_v0p0 <- feols(hinc_p1 ~ as.factor(annexing_bas)*as.factor(vra)*as.factor(period) + hinc_p0 | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid)
summary(hinc_v0p0) 

panel0020_did %<>%
  mutate(vra = relevel(as.factor(vra), ref = "0"),
         period = relevel(as.factor(period), ref = "1"))

ppov_v0p1 <- feols(pctpov_p1 ~ as.factor(annexing_bas)*as.factor(period)*as.factor(vra) + pctpov_p0 | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid)
summary(ppov_v0p1) 

hinc_v0p1 <- feols(hinc_p1 ~ as.factor(annexing_bas)*as.factor(vra)*as.factor(period) + hinc_p0 | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid)
summary(hinc_v0p1) 

panel0020_did %<>%
  mutate(vra = relevel(as.factor(vra), ref = "1"),
         period = relevel(as.factor(period), ref = "0"))

ppov_v1p0 <- feols(pctpov_p1 ~ as.factor(annexing_bas)*as.factor(period)*as.factor(vra) + pctpov_p0 | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid)
summary(ppov_v1p0) 

hinc_v1p0 <- feols(hinc_p1 ~ as.factor(annexing_bas)*as.factor(vra)*as.factor(period) + hinc_p0 | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid)
summary(hinc_v1p0) 

panel0020_did %<>%
  mutate(vra = relevel(as.factor(vra), ref = "1"),
         period = relevel(as.factor(period), ref = "1"))

ppov_v1p1 <- feols(pctpov_p1 ~ as.factor(annexing_bas)*as.factor(period)*as.factor(vra) + pctpov_p0 | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid)
summary(ppov_v1p1) 

hinc_v1p1 <- feols(hinc_p1 ~ as.factor(annexing_bas)*as.factor(vra)*as.factor(period) + hinc_p0 | plid, data = panel0020_did %>% filter(time %in% c("2007 to 2013", "2014 to 2020")), cluster = ~plid)
summary(hinc_v1p1) 

p1_diff <- list(tidy(ppov_v0p0), tidy(hinc_v0p0), tidy(ppov_v0p1), tidy(hinc_v0p1), tidy(ppov_v1p0), tidy(hinc_v1p0), tidy(ppov_v1p1), tidy(hinc_v1p1))
p1_diff_glance <- list(ppov_v0p0, hinc_v0p0, ppov_v0p1, hinc_v0p1, ppov_v1p0, hinc_v1p0, ppov_v1p1, hinc_v1p1)

for (model_stat in names(glance(ppov_v0p0))) {
  for (i in 1:8) {
    p1_diff[[i]][[model_stat]] <- glance(p1_diff_glance[[i]])[[model_stat]]
  }
}
openxlsx::write.xlsx(p1_diff, "analyticalfiles/results/bas/ppov_hinc.xlsx")

