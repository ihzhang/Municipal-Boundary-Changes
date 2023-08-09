rm(list = ls())

# 1. time and annex (trends, outcomes) ####
# show p0 over time 
vra_time <- panel_annual %>%
  mutate(vra = as.numeric(as.character(vra))) %>%
  group_by(vra, post) %>%
  mutate(n = n()) %>%
  ungroup() %>%
  group_by(vra, post, n) %>%
  summarize_at(vars(c(contains("p0"), ends_with("growth"), ends_with("_total"), ends_with("_total_1"), ends_with("_diff"), more_white, annexing)), ~mean(., na.rm = T)) %>%
  t() %>%
  as.data.frame() %>%
  tibble::rownames_to_column(., "variable")

x <- list(annex_time_vra, annex_time, vra_time, sd_outcome)
write.xlsx(x, "analyticalfiles/annex_time_desc_vra.xlsx", rowNames = T)

# annex or not ####
# conditional PTA 
reg = lm(annexing~vra*post + popdensity_p0 + pctnhwhite_p0 + pop_total, data=panel_annual)

# Fictitious data set where control variables
# have a constant value for treatment and control group: here just set everywhere 0
dat0 = mutate(panel_annual, 
              popdensity_p0 = 0,
              pctnhwhite_p0 = 0, 
              pop_total = 0)

# Now predict y and add residuals of original regression
panel_annual$y = predict(reg, dat0) + resid(reg)

gdat = panel_annual %>%
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

x <- panel_annual %>%
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

