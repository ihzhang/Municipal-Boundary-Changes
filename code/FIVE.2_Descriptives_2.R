rm(list = ls())
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
  group_by(annexing, time, vra) %>%
  mutate(n = n()) %>%
  ungroup() %>%
  group_by(annexing, time, vra, n) %>%
  summarize_at(vars(c(contains("p0"), contains("p1"), ends_with("growth"), ends_with("total"), contains("diff"))), ~mean(., na.rm = T)) %>%
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

x <- list(vra_time, annex_time_vra, annex_time, sd_outcome)
write.xlsx(x, "analyticalfiles/annex_time_desc_vra.xlsx", rowNames = T)

# annex or not ####
x <- panel0020_did %>%
  mutate(vra = as.character(vra)) %>%
  group_by(vra, time) %>%
  dplyr::summarize(Percent = mean(as.numeric(as.character(annexing)), na.rm = T)*100) %>%
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
