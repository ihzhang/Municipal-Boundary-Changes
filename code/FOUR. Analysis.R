rm(list = ls())
library(haven)
library(tidyverse)
library(lme4)
library(lattice)
library(lmerTest)
library(sjPlot)
library(stargazer)

setwd("~/Google Drive/Stanford/QE2")
aa <- read_csv("annexedblocks0010dem_pl00_newsample_unincorp.csv")
names(aa)



length(unique(aa$plid)) #1792
table(aa$annexed) #22,548 vs. 57640

aa <- aa %>%
  filter(!is.na(pop00b) & !is.na(pctnhwhite00b) & !is.na(dependencyratio00b) & !is.na(pctnhblack00b) & 
          !is.na(pctowneroccupied) & !is.na(pcth00b) & !is.na(nhblack00b) & !is.na(h00b) & !is.na(nhwhite00b) &
          !is.na(min00b) & !is.na(nhwhite00p) & !is.na(nhblack00p) & !is.na(h00p) & !is.na(popgrowth) & 
         !is.na(pctmin00b) & !is.na(pctnhwhite00p) & !is.na(pcth00p) & !is.na(pctnhblack00p) & !is.na(pctmin00p) & 
          !is.na(blackpov00p) & !is.na(popdensitypl00) & !is.na(hinc00p) & !is.na(recimmgrowth) & 
           is.finite(recimmgrowth) & !is.na(nhwhitegrowth) & is.finite(nhwhitegrowth) & !is.na(hpov00p) & !is.na(minpov00p) & 
         !is.na(nhwhitepov00p) & !is.na(hispvap00p) & is.finite(hispvap00p) & !is.na(nhwvap00p) & is.finite(nhwvap00p) & 
           !is.na(minorityvap00p) & is.finite(minorityvap00p) & !is.na(hispvap00b) & is.finite(hispvap00b) & 
           !is.na(nhwvap00b) & is.finite(nhwvap00b) & !is.na(minorityvap00b) & is.finite(minorityvap00b) & 
           !is.na(nhbvap00b) & is.finite(nhbvap00b) & is.finite(mingrowth) & is.finite(hgrowth) & is.finite(nhblackgrowth)) 
summary(aa)

aa <- aa %>%
  group_by(plid) %>%
  mutate(n_annexed = sum(annexed==1),
         n = n()) %>%
  filter(n_annexed < n)

# descriptives ####
# blocks--annexed vs. not annexed 

vars <- c("pop00b", "nhblack00b", "nhwhite00b", "h00b", "min00b",
          "pctnhblack00b", "pctnhwhite00b", "pcth00b", "pctmin00b",
          "dependencyratio00b", "pctowneroccupied",
          "hispvap00b", "nhwvap00b", "nhbvap00b", "minorityvap00b"
)

annexedblocks <- as.data.frame(aa %>% 
                                 group_by(annexed) %>%
                                 summarize_at(vars, 
                                              list(~mean(., na.rm = T), ~median(., na.rm = T), ~sd(., na.rm = T))) %>% 
                                 t())

write.csv(annexedblocks, "blocks_descriptives.csv", row.names = T)

# variable controls ####
vars <- c("pop00b", "dependencyratio00b", "pctowneroccupied",
          "h00b", "nhwhite00b", "min00b", "nhblack00b", 
          "pctnhblack00b", "pctnhwhite00b", "pcth00b", "pctmin00b",
          "hu", "hispvap00b", "nhwvap00b", "nhbvap00b", "minorityvap00b")

sample <- as.data.frame(aa %>% 
                        summarize_at(vars, 
                        list(~mean(., na.rm = T), ~median(., na.rm = T), ~sd(., na.rm = T))) %>% 
                        t())
sample
write.csv(sample, "sample_descriptives_block.csv", row.name = T)

plids <- unique(aa$plid)
pl9000 <- read_csv("pl9000_var.csv")

pl9000 <- pl9000 %>%
  filter(plid2 %in% plids)
length(unique(pl9000$plid2))

vars <- c("nhblack00p", "nhwhite00p", "h00p", "min00p",
          "pctnhblack00p", "pctnhwhite00p", "pcth00p", "pctmin00p",
          "hinc00p", "popgrowth", "incomegrowth", "pop00p",
          "hispvap00p", "nhwvap00p", "nhbvap00p", "minorityvap00p",
          "recimmgrowth", "blackpov00p", "hpov00p", "minpov00p", "nhwhitepov00p")

sample <- as.data.frame(pl9000 %>% 
                          summarize_at(vars, 
                                       list(~mean(., na.rm = T), ~median(., na.rm = T), ~sd(., na.rm = T))) %>% 
                          t())
sample
write.csv(sample, "sample_descriptives_places.csv", row.name = T)

aa <- aa %>%
  mutate(logpop00p = log(pop00p))

# Center 
varsc <- c("pop00b", "pctnhwhite00b", "dependencyratio00b", 
           "pctnhblack00b", "pctowneroccupied", "pcth00b", 
           "nhblack00b", "h00b", "nhwhite00b", "min00b",
           "logpop00p", "nhwhite00p", "nhblack00p", "h00p", "popgrowth",
           "pctmin00b", "pctnhwhite00p", "pcth00p", "pctnhblack00p", "pctmin00p", 
           "blackpov00p", "popdensitypl00", "hinc00p",
           "recimmgrowth", "nhwhitegrowth", "hpov00p", "minpov00p", "nhwhitepov00p",
           "hispvap00p", "nhwvap00p", "minorityvap00p", "hispvap00b", "nhwvap00b", "minorityvap00b", "nhbvap00b")

varsc <- setNames(varsc, str_c(varsc, "_c"))

aa <- aa %>%
  ungroup() %>%
  mutate_at(varsc, ~base::scale(.))

summary(aa)
names(aa)

# hypotheses proceed like this ####
# 1a. probability of being annexed ~ black/white 
# 1b. probability of being annexed ~ Hisp/white
# 1c. probability of being annexed ~ Min/white 
# 2. place-pct white in 2010 ~ annexed 

# 1a. probability of being annexed ~ black/white ####
#null model: intercept-only model
m0 <- glm(annexed ~ 1, data = aa, family = "binomial")
summary(m0)

#m1: random intercept 
m1 <- glmer(annexed ~ (1 | plid), data = aa, family = "binomial")
tab_model(m1) #28% variation between places (level 2/group)

(2* logLik(m1)) - (2* logLik(m0))
# m1 performs better

#m2: model with level 1 predictors: testing effects of race with class controls
m2 <- glmer(annexed ~ nhbvap00b_c + dependencyratio00b_c + pctowneroccupied_c + (1 | plid), data = aa, family = "binomial")
anova(m2, m1)

# m2 performs better 
tab_model(m2)

#m3: model with level 2 predictors 
m3 <- glmer(annexed ~ nhbvap00b_c + dependencyratio00b_c + pctowneroccupied_c + 
            popdensitypl00_c + nhwvap00p_c + blackpov00p_c + nhwhitegrowth_c + recimmgrowth_c + (1 | plid), data = aa, family = "binomial")
anova(m3, m2)
tab_model(m3)
#m3 is better

#m4: model with random slopes, level 1 variables only, so m2 with random slopes 
m4 <- glmer(annexed ~ nhbvap00b_c + dependencyratio00b_c + pctowneroccupied_c + (1 + nhbvap00b_c | plid), data = aa, family = "binomial")
anova(m4, m2) # m4 better
anova(m4, m3) # no difference
tab_model(m2, m4)

# to compare models with cross-level interaction to without, let's do a version of level 2 var that is just white and imm, and then an interaction
m3c_nhb <- glmer(annexed ~ popdensitypl00_c + log(pop00p) + popgrowth_c + hinc00p_c + blackpov00p_c +
                   recimmgrowth_c*nhbvap00b_c + nhwvap00p_c*nhbvap00b_c + 
                   pctowneroccupied_c + dependencyratio00b_c + 
               (1 | plid), data = aa, family = "binomial")
tab_model(m3c_nhb)
anova(m3c_nhb, m4)
anova(m3c_nhb, m2)

# my models ####
#nhb ####
m3c_nhb <- glmer(annexed ~ popdensitypl00_c + logpop00p_c + popgrowth_c + hinc00p_c + blackpov00p_c +
                   nhbvap00b_c*recimmgrowth_c + nhbvap00b_c*nhwhitegrowth_c +
                   pctowneroccupied_c + dependencyratio00b_c +
                   (1 | plid), data = aa, family = "binomial")
tab_model(m3c_nhb)
nhbimm <- plot_model(m3c_nhb, type = "int")[[1]]
nhbnhw <- plot_model(m3c_nhb, type = "int")[[2]]
# 
# m3c_nhb3 <- glmer(annexed ~ popdensitypl00_c + logpop00p_c + popgrowth_c + hinc00p_c + blackpov00p_c +
#                    hpov00p_c + nhwhitepov00p_c + nhbvap00b_c*recimmgrowth_c + hispvap00b_c*recimmgrowth_c + 
#                     nhwvap00b_c*recimmgrowth_c + nhbvap00b_c*pctnhwhite00p_c + hispvap00b_c*pctnhwhite00p_c + 
#                     nhwvap00b_c*pctnhwhite00p_c + pctowneroccupied_c + dependencyratio00b_c + 
#                    (1 | plid), data = aa, family = "binomial")
# tab_model(m3c_nhb3)
# p1 <- plot_model(m3c_nhb3, type = "int")[[1]]
# p2 <- plot_model(m3c_nhb3, type = "int")[[2]] 
# p3 <- plot_model(m3c_nhb3, type = "int")[[3]]
# p4 <- plot_model(m3c_nhb3, type = "int")[[4]] 
# p5 <- plot_model(m3c_nhb3, type = "int")[[5]]
# p6 <- plot_model(m3c_nhb3, type = "int")[[6]] 
# 
# p1
# p2
# p3
# p4
# p5
# p6

m3c_nhb2 <- glmer(annexed ~ popdensitypl00_c + logpop00p_c + popgrowth_c + hinc00p_c + blackpov00p_c +
                   pctnhblack00b_c*recimmgrowth_c + pctnhblack00b_c*nhwhitegrowth_c + 
                   pctowneroccupied_c + dependencyratio00b_c + 
                   (1 | plid), data = aa, family = "binomial")
nhbimm2 <- plot_model(m3c_nhb2, type = "int")[[1]]
nhbnhw2 <- plot_model(m3c_nhb2, type = "int")[[2]] # hold 

#h ####
m3c_h <-glmer(annexed ~ popdensitypl00_c + logpop00p_c + popgrowth_c + hinc00p_c + hpov00p_c +
                hispvap00b_c*recimmgrowth_c + hispvap00b_c*nhwhitegrowth_c + 
                pctowneroccupied_c + dependencyratio00b_c + 
                (1 | plid), data = aa, family = "binomial")
tab_model(m3c_h)
hispimm <- plot_model(m3c_h, type = "int")[[1]]
hispnhw <- plot_model(m3c_h, type = "int")[[2]] 

m3c_h2 <-glmer(annexed ~ popdensitypl00_c + logpop00p_c + popgrowth_c + hinc00p_c + hpov00p_c +
                pcth00b_c*recimmgrowth_c + pcth00b_c*nhwhitegrowth_c + 
                pctowneroccupied_c + dependencyratio00b_c + 
                (1 | plid), data = aa, family = "binomial")
hispimm2 <- plot_model(m3c_h2, type = "int")[[1]]
hispnhw2 <- plot_model(m3c_h2, type = "int")[[2]] 

#min ####
m3c_min <- glmer(annexed ~ popdensitypl00_c + logpop00p_c + popgrowth_c + hinc00p_c + minpov00p_c +
                   minorityvap00b_c*recimmgrowth_c + minorityvap00b_c*nhwhitegrowth_c + 
                   pctowneroccupied_c + dependencyratio00b_c + 
                   (1 | plid), data = aa, family = "binomial")
tab_model(m3c_min)
minimm <- plot_model(m3c_min, type = "int")[[1]]
minnhw <- plot_model(m3c_min, type = "int")[[2]]

m3c_min2 <- glmer(annexed ~ popdensitypl00_c + logpop00p_c + popgrowth_c + hinc00p_c + minpov00p_c +
                   pctmin00b_c*recimmgrowth_c + pctmin00b_c*nhwhitegrowth_c + 
                   pctowneroccupied_c + dependencyratio00b_c + 
                   (1 | plid), data = aa, family = "binomial")
minimm2 <- plot_model(m3c_min2, type = "int")[[1]]
minnhw2 <- plot_model(m3c_min2, type = "int")[[2]]

#nhwhite ####
m3c_nhw <- glmer(annexed ~ popdensitypl00_c + logpop00p_c + popgrowth_c + hinc00p_c + nhwhitepov00p_c +
                   nhwvap00b_c*recimmgrowth_c + nhwvap00b_c*nhwhitegrowth_c + 
                   pctowneroccupied_c + dependencyratio00b_c + 
                   (1 | plid), data = aa, family = "binomial")
tab_model(m3c_nhw)

nhwimm <- plot_model(m3c_nhw, type = "int")[[1]]
nhwnhw <- plot_model(m3c_nhw, type = "int")[[2]]

m3c_nhw2 <- glmer(annexed ~ popdensitypl00_c + logpop00p_c + popgrowth_c + hinc00p_c + nhwhitepov00p_c +
                   pctnhwhite00b_c*recimmgrowth_c + pctnhwhite00b_c*nhwhitegrowth_c + 
                   pctowneroccupied_c + dependencyratio00b_c + 
                   (1 | plid), data = aa, family = "binomial")
nhwimm2 <- plot_model(m3c_nhw2, type = "int")[[1]]
nhwnhw2 <- plot_model(m3c_nhw2, type = "int")[[2]]

tab_model(m3c_nhb, m3c_h, m3c_min, m3c_nhw)
tab_model(m3c_nhb2, m3c_h2, m3c_min2, m3c_nhw2)


# lichter replication ####
# same states 
aa <- aa %>%
  mutate(STATEA = substr(plid, 1, 2))

aa$STATEA <- relevel(as.factor(aa$STATEA), ref = "51")
lichter_nhb <- glm(annexed ~ pctnhwhite00p + popdensitypl00 + log(pop00p) + popgrowth + hinc00p + blackpov00p +
                     pctowneroccupied + pctnhblack00b + dependencyratio00b + as.factor(STATEA), 
                   data = aa %>% filter(STATEA=="51" | STATEA=="01" | STATEA=="05" | STATEA=="13" | STATEA=="22" | STATEA=="28" | STATEA=="37" | STATEA=="45"), 
                   family = "binomial")
summary(lichter_nhb)


lichter_nhb_full <- glm(annexed ~ pctnhwhite00p + popdensitypl00 + log(pop00p) + popgrowth + hinc00p + blackpov00p +
                          pctowneroccupied + pctnhblack00b + dependencyratio00b + as.factor(STATEA), 
                        data = aa,
                        family = "binomial")
summary(lichter_nhb_full)

stargazer(lichter_nhb, lichter_nhb_full,
          type = "html",
          out = "lichter_nhb_comp.htm")

lichter_h <- glm(annexed ~ pctnhwhite00p + popdensitypl00 + log(pop00p) + popgrowth + hinc00p + hpov00p +
                   pctowneroccupied + pcth00b + dependencyratio00b + as.factor(STATEA), 
                 data = aa %>% filter(STATEA=="51" | STATEA=="01" | STATEA=="05" | STATEA=="13" | STATEA=="22" | STATEA=="28" | STATEA=="37" | STATEA=="45"), 
                 family = "binomial")
summary(lichter_h)

lichter_h_full <- glm(annexed ~ pctnhwhite00p + popdensitypl00 + log(pop00p) + popgrowth + hinc00p + hpov00p +
                        pctowneroccupied + pcth00b + dependencyratio00b + as.factor(STATEA), 
                      data = aa,
                      family = "binomial")
summary(lichter_h_full)


lichter_min <- glm(annexed ~ pctnhwhite00p + popdensitypl00 + log(pop00p) + popgrowth + hinc00p + minpov00p +
                     pctowneroccupied + pctmin00b + dependencyratio00b + as.factor(STATEA), 
                   data = aa %>% filter(STATEA=="51" | STATEA=="01" | STATEA=="05" | STATEA=="13" | STATEA=="22" | STATEA=="28" | STATEA=="37" | STATEA=="45"), 
                   family = "binomial")
summary(lichter_min)

lichter_min_full <- glm(annexed ~ pctnhwhite00p + popdensitypl00 + log(pop00p) + popgrowth + hinc00p + minpov00p +
                          pctowneroccupied + pctmin00b + dependencyratio00b + as.factor(STATEA), 
                        data = aa,
                        family = "binomial")
summary(lichter_min_full)

lichter_nhw <- glm(annexed ~ pctnhwhite00p + popdensitypl00 + log(pop00p) + popgrowth + hinc00p + nhwhitepov00p +
                     pctowneroccupied + pctnhwhite00b + dependencyratio00b + as.factor(STATEA), 
                   data = aa %>% filter(STATEA=="51" | STATEA=="01" | STATEA=="05" | STATEA=="13" | STATEA=="22" | STATEA=="28" | STATEA=="37" | STATEA=="45"), 
                   family = "binomial")
summary(lichter_nhw)

lichter_nhw_full <- glm(annexed ~ pctnhwhite00p + popdensitypl00 + log(pop00p) + popgrowth + hinc00p + nhwhitepov00p +
                          pctowneroccupied + pctnhwhite00b + dependencyratio00b + as.factor(STATEA), 
                        data = aa, 
                        family = "binomial")
summary(lichter_nhw_full)

stargazer(lichter_h, lichter_h_full,
          type = "html",
          out = "lichter_h_comp.htm")

stargazer(lichter_min, lichter_min_full,
          type = "html",
          out = "lichter_min_comp.htm")

stargazer(lichter_nhw, lichter_nhw_full,
          type = "html",
          out = "lichter_nhw_comp.htm")

# change plot appearances ####
# recent immigrant, vap
immb <- nhbimm$data 
immb$Race <- "Non-Hispanic Black"

immh <- hispimm$data 
immh$Race <- "Hispanic"

immm <- minimm$data 
immm$Race <- "Non-white"

immw <- nhwimm$data 
immw$Race <- "Non-Hispanic white"

imm <- base::rbind(immb, immh, immm, immw) %>%
  mutate(group = as.numeric(as.character(group)),
         Group = ifelse(group < 0, "Minimum", "Maximum"))
rm(immb, immh, immm, immw)

imm$Race <- factor(imm$Race, levels = c("Non-Hispanic Black",
                                        "Hispanic",
                                        "Non-white",
                                        "Non-Hispanic white"))

immplot <- ggplot(imm,
       aes(x, predicted, linetype = Group)) +
  geom_line() +
  labs(linetype = "Place Recent Immigrant 
Population Growth Rate (scaled)") +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.3) +
  facet_wrap(~Race, scales = "free") +
  scale_color_grey() +
  ylab("Predicted probability of being Annexed") +
  xlab("Block % of Specified Race, VAP (scaled)") +
  theme_bw() 

immplot  
ggsave("immplot_vap.png",
       plot = immplot,
       width = 9.5,
       height = 4.5)

# NHW, vap
nhwb <- nhbnhw$data 
nhwb$Race <- "Non-Hispanic Black"

nhwh <- hispnhw$data 
nhwh$Race <- "Hispanic"

nhwm <- minnhw$data 
nhwm$Race <- "Non-white"

nhww <- nhwnhw$data 
nhww$Race <- "Non-Hispanic white"

nhw <- base::rbind(nhwb, nhwh, nhwm, nhww) %>%
  mutate(group = as.numeric(as.character(group)),
         Group = ifelse(group < 0, "Minimum", "Maximum"))
rm(nhwb, nhwh, nhwm, nhww)

nhw$Race <- factor(nhw$Race, levels = c("Non-Hispanic Black",
                                        "Hispanic",
                                        "Non-white",
                                        "Non-Hispanic white"))

nhwplot <- ggplot(nhw,
                  aes(x, predicted, linetype = Group)) +
  geom_line() +
  labs(linetype = "Place Non-Hispanic 
white growth rate, VAP (scaled)") +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.3) +
  facet_wrap(~Race, scales = "free") +
  scale_color_grey() +
  ylab("Predicted probability of being Annexed") +
  xlab("Block % of Specified Race, VAP (scaled)") +
  theme_bw() 

nhwplot  
ggsave("nhwplot_vap.png",
       plot = nhwplot,
       width = 9.5,
       height = 4.5)

# recent immigrant, total
immb2 <- nhbimm2$data 
immb2$Race <- "Non-Hispanic Black"

immh2 <- hispimm2$data 
immh2$Race <- "Hispanic"

immm2 <- minimm2$data 
immm2$Race <- "Non-white"

immw2 <- nhwimm2$data 
immw2$Race <- "Non-Hispanic white"

imm2 <- base::rbind(immb2, immh2, immm2, immw2) %>%
  mutate(group = as.numeric(as.character(group)),
         Group = ifelse(group < 0, "Minimum", "Maximum"))
rm(immb2, immh2, immm2, immw2)

imm2$Race <- factor(imm2$Race, levels = c("Non-Hispanic Black",
                                        "Hispanic",
                                        "Non-white",
                                        "Non-Hispanic white"))

immplot2 <- ggplot(imm2,
                  aes(x, predicted, linetype = Group)) +
  geom_line() +
  labs(linetype = "Place Recent Immigrant 
Population Growth Rate (scaled)") +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.3) +
  facet_wrap(~Race, scales = "free") +
  scale_color_grey() +
  ylab("Predicted probability of being Annexed") +
  xlab("Block % of Specified Race (scaled)") +
  theme_bw() 

immplot2  
ggsave("immplot_total.png",
       plot = immplot2,
       width = 9.5,
       height = 4.5)

#nhw, total
nhwb2 <- nhbnhw2$data 
nhwb2$Race <- "Non-Hispanic Black"

nhwh2 <- hispnhw2$data 
nhwh2$Race <- "Hispanic"

nhwm2 <- minnhw2$data 
nhwm2$Race <- "Non-white"

nhww2 <- nhwnhw2$data 
nhww2$Race <- "Non-Hispanic white"

nhw2 <- base::rbind(nhwb2, nhwh2, nhwm2, nhww2) %>%
  mutate(group = as.numeric(as.character(group)),
         Group = ifelse(group < 0, "Minimum", "Maximum"))
rm(nhwb2, nhwh2, nhwm2, nhww2)

nhw2$Race <- factor(nhw2$Race, levels = c("Non-Hispanic Black",
                                        "Hispanic",
                                        "Non-white",
                                        "Non-Hispanic white"))

nhwplot2 <- ggplot(nhw2,
                  aes(x, predicted, linetype = Group)) +
  geom_line() +
  labs(linetype = "Place Non-Hispanic 
white growth rate (scaled)") +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.3) +
  facet_wrap(~Race, scales = "free") +
  scale_color_grey() +
  ylab("Predicted probability of being Annexed") +
  xlab("Block % of Specified Race (scaled)") +
  theme_bw() 

nhwplot2  
ggsave("nhwplot_total.png",
       plot = nhwplot2,
       width = 9.5,
       height = 4.5)
