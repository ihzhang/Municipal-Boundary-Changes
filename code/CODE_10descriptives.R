rm(list = ls())

load(paste0("THREE_July.RData"))

curdir <- paste0(dirname(rstudioapi::getSourceEditorContext()$path), "/")
savedir <- paste0(curdir, "/../results/")

panel_annual %<>%
  rename(pctpov = ppov) %>%
  mutate(post = ifelse(time %in% "2014 to 2020", 1, 0))

# t-tests ----
desc_vars <- c("pop", "popdensity", "pctnhblack", "pctnbmin", "pctnhblack_total", "pctnbmin_total", "pctowneroccupied", "mhmval", "hinc", "pctpov", "pctblackpov", "pctnbminpov", "pctownerocc_total", "pcthincjobs_total", "pctincopp_total", "annexing", "underbound_black", "underbound_nbmin", "more_white_1")

pre_difference <- panel_annual %>%
  filter(!time %in% "2000 to 2007") %>% 
  mutate(more_white_1 = ifelse(more_white == 1, 1, 0)) %>%
  group_by(vra, post) %>%
  summarize_at(all_of(desc_vars), ~mean(., na.rm = T)) %>%
  filter(post == 0) %>% 
  pivot_wider(names_from = vra,
              values_from = c(pop:more_white_1))

for (i in 1:length(desc_vars)) {
  pre_difference[[paste0(desc_vars[[i]], "_diff")]] <- pre_difference[[paste0(desc_vars[[i]], "_1")]] - pre_difference[[paste0(desc_vars[[i]], "_0")]]
}

names(pre_difference)

pre_0 <- pre_difference %>%
  select(post, ends_with("_0")) %>%
  pivot_longer(cols = ends_with("_0"),
               names_to = "var", 
               names_pattern = "(.*)_0") %>%
  rename(vra_0 = value)
pre_1 <- pre_difference %>%
  select(post, ends_with("_1")) %>%
  pivot_longer(cols = ends_with("_1"),
               names_to = "var", 
               names_pattern = "(.*)_1")  %>%
  rename(vra_1 = value)
diff <- pre_difference %>%
  select(post, ends_with("_diff")) %>%
  pivot_longer(cols = ends_with("_diff"),
               names_to = "var", 
               names_pattern = "(.*)_diff")  %>%
  rename(diff = value)

pre <- pre_0 %>%
  left_join(pre_1 %>% select(-post), by = "var") %>%
  left_join(diff %>% select(-post), by = "var")

pval <- panel_annual %>%
  filter(!time %in% "2000 to 2007") %>% 
  mutate(more_white_1 = ifelse(more_white == 1, 1, 0)) %>%
  summarize_at(all_of(desc_vars), ~t.test(.[vra == 0 & post == 0], .[vra == 1 & post == 0])$p.value) %>%
  t() %>%
  as.data.frame() %>%
  rownames_to_column() %>%
  rename(pval = V1)

pre %<>%
  left_join(pval, by = c("var" = "rowname"))

post_difference <- panel_annual %>%
  filter(!time %in% "2000 to 2007") %>% 
  mutate(more_white_1 = ifelse(more_white == 1, 1, 0)) %>%
  group_by(vra, post) %>%
  summarize_at(all_of(desc_vars), ~mean(., na.rm = T)) %>%
  filter(post == 1) %>% 
  pivot_wider(names_from = vra,
              values_from = c(pop:more_white_1))

for (i in 1:length(desc_vars)) {
  post_difference[[paste0(desc_vars[[i]], "_diff")]] <- post_difference[[paste0(desc_vars[[i]], "_1")]] - post_difference[[paste0(desc_vars[[i]], "_0")]]
}

names(post_difference)

post_0 <- post_difference %>%
  select(post, ends_with("_0")) %>%
  pivot_longer(cols = ends_with("_0"),
               names_to = "var", 
               names_pattern = "(.*)_0") %>%
  rename(vra_0 = value)
post_1 <- post_difference %>%
  select(post, ends_with("_1")) %>%
  pivot_longer(cols = ends_with("_1"),
               names_to = "var", 
               names_pattern = "(.*)_1")  %>%
  rename(vra_1 = value)
diff <- post_difference %>%
  select(post, ends_with("_diff")) %>%
  pivot_longer(cols = ends_with("_diff"),
               names_to = "var", 
               names_pattern = "(.*)_diff")  %>%
  rename(diff = value)

post <- post_0 %>%
  left_join(post_1 %>% select(-post), by = "var") %>%
  left_join(diff %>% select(-post), by = "var")

pval <- panel_annual %>%
  filter(!time %in% "2000 to 2007") %>% 
  mutate(more_white_1 = ifelse(more_white == 1, 1, 0)) %>%
  summarize_at(all_of(desc_vars), ~t.test(.[vra == 0 & post == 1], .[vra == 1 & post == 1])$p.value) %>%
  t() %>%
  as.data.frame() %>%
  rownames_to_column() %>%
  rename(pval = V1)

post %<>%
  left_join(pval, by = c("var" = "rowname"))

df <- base::rbind(pre, post)
write_csv(df, paste0(savedir, "t0020/diff_pval.csv"))

rm(df, diff, post, post_0, post_1, post_difference, pre, pre_0, pre_1, pre_difference, pval)

panel_annual %>%
  filter(!time %in% "2000 to 2007") %>% 
  summarize_at(all_of(c("pop_total_1", "area_total_1")), ~t.test(.[vra == 0 & post == 0], .[vra == 1 & post == 0])$p.value) 

panel_annual %>%
  filter(!time %in% "2000 to 2007") %>% 
  summarize_at(all_of(c("pop_total_1", "area_total_1")), ~t.test(.[vra == 0 & post == 1], .[vra == 1 & post == 1])$p.value) 

# conditional PTA, annexing ----
reg = lm(annexing~vra*post + popdensity + pctnhwhite + pop_total, data=panel_annual)

# Fictitious data set where control variables
# have a constant value for treatment and control group: here just set everywhere 0
dat0 = mutate(panel_annual, 
              popdensity = 0,
              pctnhwhite = 0, 
              pop_total = 0)

# Now predict y and add residuals of original regression
panel_annual$y = predict(reg, dat0) + resid(reg)

gdat = panel_annual %>%
  group_by(vra, time) %>%
  dplyr::summarize(y = mean(y)) %>%
  ungroup() %>%
  mutate(vra = as.factor(vra)) 

shelby <- gdat %>%
  filter(time == "2012 to 2013") %>%
  mutate(time = "2013, Shelby County", 
         y = NA)

gdat <- rbind(gdat, shelby)  

gg = ggplot(gdat, aes(y=y,x=time, group = vra)) +
  geom_point() + geom_line(aes(linetype = vra)) + 
  geom_vline(xintercept="2013, Shelby County") +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5)) +
  ylab("% of Places Annexing") + 
  xlab("Annual Period") + 
  labs(linetype = "VRA Coverage")
gg

ggsave(filename = paste0(savedir, "Conditional PTA.pdf"),
       plot = gg, 
       dpi = 300)

# regular PTA, annexing ----
pta_reg <- panel_annual %>%
  group_by(vra, time) %>%
  dplyr::summarize(annexing = mean(annexing)*100) %>%
  ungroup() %>%
  mutate(vra = as.factor(vra),
         model = "Annexations, General") 

pta_nhb <- panel_annual %>%
  group_by(vra, time) %>%
  dplyr::summarize(annexing = mean(underbound_black)*100) %>%
  ungroup() %>%
  mutate(vra = as.factor(vra),
         model = "Annexations, Black-Diluting") 

pta_nbm <- panel_annual %>%
  group_by(vra, time) %>%
  dplyr::summarize(annexing = mean(underbound_nbmin)*100) %>%
  ungroup() %>%
  mutate(vra = as.factor(vra),
         model = "Annexations, Non-Black \nMinority-Diluting") 

pta <- rbind(pta_reg, pta_nhb, pta_nbm) %>%
  mutate(model = factor(model, levels = c("Annexations, General", "Annexations, Black-Diluting", "Annexations, Non-Black \nMinority-Diluting")))
gg = ggplot(pta, aes(y=annexing,x=time, group = vra)) +
  geom_point() + geom_line(aes(linetype = vra)) + 
  facet_wrap(~model, nrow = 3, ncol = 1, scales = "free") + 
  theme_bw() + 
  #theme(axis.text.x = element_text(angle = 45, vjust = 0.5)) +
  ylab("% of Municipalities") + 
  xlab("Annual Period") + 
  labs(linetype = "VRA Coverage")
gg

ggsave(filename = paste0(savedir, "t0020/PTA.pdf"),
       plot = gg, 
       dpi = 300)

# area and population ----
panel_annual %>%
  filter(!time %in% "2000 to 2007" & annexing == 1) %>%
  group_by(vra, post) %>%
  summarize(pop_total = mean(pop_total_1, na.rm = T),
            area_total = mean(area_total_1/4047, na.rm = T))
