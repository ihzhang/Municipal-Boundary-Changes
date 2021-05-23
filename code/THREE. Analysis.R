rm(list = ls())
library(haven)
library(tidyverse)
library(lme4)
library(lattice)
library(lmerTest)
library(sjPlot)
library(stargazer)

setwd("~/Google Drive/Stanford/QE2")
aa <- read_csv("annexedblocks0010dem_pl00_newsample.csv")
names(aa)

aa <- aa %>%
  filter(!is.na(pop00b) & !is.na(pctnhwhite00b) & !is.na(dependencyratio00b) & !is.na(pctowneroccupied) & 
           is.finite(pop00b) & is.finite(pctnhwhite00b) & is.finite(dependencyratio00b) & is.finite(pctowneroccupied) & 
           !is.na(pcth00b) & !is.na(pctmin00b) & !is.na(pctnhwhite00p) & !is.na(pctmin00p) & !is.na(pcth00p) & !is.na(popgrowth) & 
           !is.na(recimmgrowth) & !is.na(blackpov00p) & !is.na(hinc00p)) 

aa <- aa %>%
  mutate(plid2 = as.character(plid2)) %>%
  left_join(pl.dis, by = "plid2") %>%
  left_join(pl.dis10, by = "plid2") %>%
  mutate(wbchange = wb.dissim10 - wb.dissim00,
         wminchange = wmin.dissim10 - wmin.dissim00,
         whchange = wh.dissim10 - wh.dissim00)

aa <- aa %>%
  mutate(plid2 = as.character(plid2)) %>%
  left_join(pl.int, by = "plid2") %>%
  left_join(pl.int10, by = "plid2") %>%
  mutate(wbintchange = wb.int10 - wb.int00,
         wminintchange = wmin.int10 - wmin.int00,
         whintchange = wh.int10 - wh.int00)

# Center 
varsc <- c("pop00b", "pctnhwhite00b", "dependencyratio00b", 
           "pctnhblack00b", "pctowneroccupied", "pcth00b", 
           "nhblack00b", "h00b", "nhwhite00b", "min00b",
           "pop00p", "nhwhite00p", "nhblack00p", "h00p", "popgrowth",
           "pctmin00b", "pctnhwhite00p", "pcth00p", "pctnhblack00p", "pctmin00p", 
           "blackpov00p", "popdensitypl00", "hinc00p",
           "recimmgrowth", "nhwhitegrowth", "hpov00p", "minpov00p", "nhwhitepov00p",
           "wb.dissim00", "wmin.dissim00", "wh.dissim00", "wbchange", "wminchange", "whchange",
           "wb.int00", "wmin.int00", "wh.int00", "wbintchange", "wminintchange", "whintchange")

varsc <- setNames(varsc, str_c(varsc, "_c"))

aa <- aa %>% 
  mutate_at(varsc, ~(as.numeric(scale(.))))
summary(aa)

aa <- aa %>%
  mutate(plid = as.factor(as.character(as.numeric(plid))),
         plid2 = as.factor(as.character(as.numeric(plid2))))

table(aa$annexed)
length(unique(aa$plid2))

aa$annexed <- as.factor(aa$annexed)

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
m1 <- glmer(annexed ~ (1 | plid2), data = aa, family = "binomial")
tab_model(m1)

(2* logLik(m1)) - (2* logLik(m0))

# m1 performs better

r.squaredGLMM(m1)
#23% variation between places (level 2/group)

#m2: model with level 1 predictors: testing effects of race with class controls
m2 <- glmer(annexed ~ pctnhblack00b_c + dependencyratio00b_c + pctowneroccupied_c + (1 | aa$plid2), data = aa, family = "binomial")
anova(m2, m1)

# m2 performs better 
r.squaredGLMM(m2)
repeat_hat_m2 <- predict(m2, type = "response")
head(repeat_hat_m2)
tab_model(m2)

#m3: model with level 2 predictors 
m3 <- glmer(annexed ~ pctnhblack00b_c + dependencyratio00b_c + pctowneroccupied_c + 
             pctnhwhite00p_c + blackpov00p_c + nhwhitegrowth_c + recimmgrowth_c + (1 | plid2), data = aa, family = "binomial")
anova(m3, m2)
tab_model(m3)
#m3 is better

#m4: model with random slopes, level 1 variables only, so m2 with random slopes 
m4 <- glmer(annexed ~ pctnhblack00b_c + dependencyratio00b_c + pctowneroccupied_c + (1 + pctnhblack00b_c | plid2), data = aa, family = "binomial")
anova(m4, m2) # m4 better
anova(m4, m3) # no difference
tab_model(m2, m4)

# to compare models with cross-level interaction to without, let's do a version of level 2 var that is just white and imm, and then an interaction
m3c_nhb <- glmer(annexed ~ popdensitypl00_c + log(pop00p) + popgrowth_c + hinc00p_c + blackpov00p_c +
                   recimmgrowth_c*pctnhblack00b_c + pctnhwhite00p_c*pctnhblack00b_c + 
                   pctowneroccupied_c + pctnhwhite00b_c + dependencyratio00b_c + 
               (1 | plid2), data = aa, family = "binomial")
tab_model(m3c_nhb)
plot_model(m3c_nhb, type = "pred", terms = c("recimmgrowth_c"))

# 1b. probability of being annexed ~ Hisp/white ####
#null model 
m0 <- glm(annexed ~ 1, data = aa, family = "binomial")
summary(m0)

#m1: random intercept 
m1 <- glmer(annexed ~ (1 | plid2), data = aa, family = "binomial")
tab_model(m1)

(2* logLik(m1)) - (2* logLik(m0))

# m1 performs better

r.squaredGLMM(m1)
#23% variation between places (level 2/group)

#m2: model with level 1 predictors: testing effects of race with class controls
m2 <- glmer(annexed ~ pcth00b_c + dependencyratio00b_c + pctowneroccupied_c + (1 | aa$plid2), data = aa, family = "binomial")
anova(m2, m1)

# m2 performs better 
r.squaredGLMM(m2)
repeat_hat_m2 <- predict(m2, type = "response")
head(repeat_hat_m2)
tab_model(m2)

#m3: model with level 2 predictors 
m3_h <- glmer(annexed ~ pcth00b_c*pcth00p_c + dependencyratio00b_c + pctowneroccupied_c + 
              pctnhwhite00p_c + blackpov00p_c + nhwhitegrowth_c + recimmgrowth_c + wh.int00_c + (1 | plid2), data = aa, family = "binomial")
anova(m3_h, m2)
tab_model(m3_h)
#m3 is better

#m4: model with random slopes, level 1 variables only, so m2 with random slopes 
m4 <- glmer(annexed ~ pcth00b_c + dependencyratio00b_c + pctowneroccupied_c + (1 + pcth00b_c | plid2), data = aa, family = "binomial")
anova(m4, m2) # m4 better
anova(m4, m3) # no difference
tab_model(m2, m4)

# to compare models with cross-level interaction to without, let's do a version of level 2 var that is just white and imm, and then an interaction
m3c_h <- glmer(annexed ~ pcth00b_c*pcth00p_c + pcth00b_c*hpov00p_c + pop00b_c + dependencyratio00b_c + pctowneroccupied_c + 
                   pop00p_c + popdensitypl00_c + pctnhwhite00p_c + recimmgrowth_c*pcth00b_c + 
                   (1 | plid2), data = aa, family = "binomial")
tab_model(m3c_h)

# 1c. probability of being annexed ~ min/white ####
#null model: intercept-only model
m0 <- glm(annexed ~ 1, data = aa, family = "binomial")
summary(m0)

#m1: random intercept 
m1 <- glmer(annexed ~ (1 | plid2), data = aa, family = "binomial")
tab_model(m1)

m1fe <- glm(annexed ~ plid2 - 1, data = aa, family = "binomial")
summary(m1fe)
tab_model(m1fe)
anova(m1, m1fe)
(2* logLik(m1)) - (2* logLik(m0))

# m1 performs better

r.squaredGLMM(m1)
#23% variation between places (level 2/group)

#m2: model with level 1 predictors: testing effects of race with class controls
m2 <- glmer(annexed ~ pctmin00b_c + dependencyratio00b_c + pctowneroccupied_c + (1 | aa$plid2), data = aa, family = "binomial")
anova(m2, m1)

# m2 performs better 
r.squaredGLMM(m2)
repeat_hat_m2 <- predict(m2, type = "response")
head(repeat_hat_m2)
tab_model(m2)

#m3: model with level 2 predictors 
m3_min <- glmer(annexed ~ pctmin00b_c + dependencyratio00b_c + pctowneroccupied_c + 
              pctnhwhite00p_c + blackpov00p_c + nhwhitegrowth_c + recimmgrowth_c + wmin.int00 +  (1 | plid2), data = aa, family = "binomial")
anova(m3, m2)
tab_model(m3_min)

#m3 is better
#m4: model with random slopes, level 1 variables only, so m2 with random slopes 
m4 <- glmer(annexed ~ pctmin00b_c + dependencyratio00b_c + pctowneroccupied_c + (1 + pctmin00b_c | plid2), data = aa, family = "binomial")
anova(m4, m2) # m4 better
anova(m4, m3) # no difference
tab_model(m2, m4)

# to compare models with cross-level interaction to without, let's do a version of level 2 var that is just white and imm, and then an interaction
m3c_min <- glmer(annexed ~ pctmin00b_c*pctmin00p_c + pctmin00b_c*minpov00p_c + pop00b_c + dependencyratio00b_c + pctowneroccupied_c + 
                 pop00p_c + popdensitypl00_c + nhwhite00p_c + recimmgrowth_c*pctmin00b_c + 
                 (1 | plid2), data = aa, family = "binomial")
tab_model(m3c_min)

# white ####
#null model: intercept-only model
m0 <- glm(annexed ~ 1, data = aa, family = "binomial")
summary(m0)

#m1: random intercept 
m1 <- glmer(annexed ~ (1 | plid2), data = aa, family = "binomial")
tab_model(m1)

(2* logLik(m1)) - (2* logLik(m0))

# m1 performs better

r.squaredGLMM(m1)
#23% variation between places (level 2/group)

#m2: model with level 1 predictors: testing effects of race with class controls
m2 <- glmer(annexed ~ pctnhwhite00b_c + dependencyratio00b_c + pctowneroccupied_c + (1 | aa$plid2), data = aa, family = "binomial")
anova(m2, m1)

# m2 performs better 
r.squaredGLMM(m2)
repeat_hat_m2 <- predict(m2, type = "response")
head(repeat_hat_m2)
tab_model(m2)

#m3: model with level 2 predictors 
m3_nhw <- glmer(annexed ~ pctnhwhite00b_c + dependencyratio00b_c + pctowneroccupied_c + 
              pctnhwhite00p_c + blackpov00p_c + nhwhitegrowth_c + recimmgrowth_c +  (1 | plid2), data = aa, family = "binomial")
anova(m3, m2)
tab_model(m3)

#m3 is better
#m4: model with random slopes, level 1 variables only, so m2 with random slopes 
m4 <- glmer(annexed ~ pctnhwhite00b_c + dependencyratio00b_c + pctowneroccupied_c + (1 + pctnhwhite00b_c | plid2), data = aa, family = "binomial")
anova(m4, m2) # m4 better
anova(m4, m3) # no difference
tab_model(m2, m4)

# to compare models with cross-level interaction to without, let's do a version of level 2 var that is just white and imm, and then an interaction
m3b <- glmer(annexed ~ pctnhwhite00b_c + dependencyratio00b_c + pctowneroccupied_c + 
               nhwhitegrowth_c + recimmgrowth_c + (1 + pctnhwhite00b_c | plid2), data = aa, family = "binomial")
tab_model(m3b)

m3c_nhw <- glmer(annexed ~ pctnhwhite00b_c*pctnhwhite00p_c + dependencyratio00b_c + pctowneroccupied_c + 
                   nhwhitegrowth_c + recimmgrowth_c*pctnhwhite00p_c + nhwhitepov00p_c + 
                   (pctnhwhite00b_c | plid2), data = aa, family = "binomial")
tab_model(m3c_nhw)
plot_model(m3c_nhw, type = "int")[[1]]
plot_model(m3c_nhw, type = "pred", terms = c("recimmgrowth_c"))

m3c_nhw_fe <- glmer(annexed ~ pctnhwhite00b_c*pctnhwhite00p_c + dependencyratio00b_c + pctowneroccupied_c + 
                   nhwhitegrowth_c + recimmgrowth_c*pctnhwhite00p_c + nhwhitepov00p_c + as.factor(plid2),
                   data = aa, family = "binomial")

# for paper 
tab_model(m3c_nhb, m3c_h, m3c_min, m3c_nhw)
logLik(m3c_nhb)
logLik(m3c_h)
logLik(m3c_min)
logLik(m3c_nhw)

# lichter replication ####
# same states 
aa$STATEA <- relevel(as.factor(aa$STATEA), ref = "51")
lichter_nhb <- glm(annexed ~ pctnhwhite00p_c + popdensitypl00_c + log(pop00p) + popgrowth_c + hinc00p_c + blackpov00p_c +
                     pctowneroccupied_c + pctnhblack00b_c + dependencyratio00b_c + as.factor(STATEA), 
                   data = aa %>% filter(STATEA=="51" | STATEA=="01" | STATEA=="05" | STATEA=="13" | STATEA=="22" | STATEA=="28" | STATEA=="37" | STATEA=="45"), 
                   family = "binomial")
summary(lichter_nhb)
plot_model(lichter_nhb, type = "pred", terms = c("pctnhblack00b_c"))

lichter_nhb2 <- glm(annexed ~ pctnhwhite00p_c + popdensitypl00_c + log(pop00p) + popgrowth_c + hinc00p_c + blackpov00p_c +
                     pctowneroccupied_c + pctnhblack00b_c + dependencyratio00b_c + as.factor(STATEA), 
                   data = aa %>% filter((STATEA=="51" | STATEA=="01" | STATEA=="05" | STATEA=="13" | STATEA=="22" | STATEA=="28" | STATEA=="37" | STATEA=="45") & pctnhwhite00p < 50), 
                   family = "binomial")
summary(lichter_nhb2)

lichter_nhb_full <- glm(annexed ~ pctnhwhite00p_c + popdensitypl00_c + log(pop00p) + popgrowth_c + hinc00p_c + blackpov00p_c +
                          pctowneroccupied_c + pctnhblack00b_c + dependencyratio00b_c + as.factor(STATEA), 
                        data = aa, 
                        family = "binomial")
summary(lichter_nhb_full)
plot_model(lichter_nhb_full, type = "pred", terms = c("pctnhblack00b_c"))

stargazer(lichter_nhb, lichter_nhb_full,
          type = "html",
          out = "lichter_nhb_comp.htm")

lichter_h <- glm(annexed ~ pctnhwhite00p_c + popdensitypl00_c + log(pop00p) + popgrowth_c + hinc00p_c + hpov00p_c +
                     pctowneroccupied_c + pcth00b_c + dependencyratio00b_c + as.factor(STATEA), 
                   data = aa %>% filter(STATEA=="51" | STATEA=="01" | STATEA=="05" | STATEA=="13" | STATEA=="22" | STATEA=="28" | STATEA=="37" | STATEA=="45"), 
                   family = "binomial")
summary(lichter_h)
plot_model(lichter_h, type = "pred", terms = c("pctnhblack00b_c"))

lichter_h_full <- glm(annexed ~ pctnhwhite00p_c + popdensitypl00_c + log(pop00p) + popgrowth_c + hinc00p_c + hpov00p_c +
                          pctowneroccupied_c + pcth00b_c + dependencyratio00b_c + as.factor(STATEA), 
                        data = aa, 
                        family = "binomial")
summary(lichter_h_full)
plot_model(lichter_h_full, type = "pred", terms = c("pctnhblack00b_c"))

lichter_min <- glm(annexed ~ pctnhwhite00p_c + popdensitypl00_c + log(pop00p) + popgrowth_c + hinc00p_c + minpov00p_c +
                   pctowneroccupied_c + pctmin00b_c + dependencyratio00b_c + as.factor(STATEA), 
                 data = aa %>% filter(STATEA=="51" | STATEA=="01" | STATEA=="05" | STATEA=="13" | STATEA=="22" | STATEA=="28" | STATEA=="37" | STATEA=="45"), 
                 family = "binomial")
summary(lichter_min)
plot_model(lichter_min, type = "pred", terms = c("pctmin00b_c"))

lichter_min_full <- glm(annexed ~ pctnhwhite00p_c + popdensitypl00_c + log(pop00p) + popgrowth_c + hinc00p_c + minpov00p_c +
                          pctowneroccupied_c + pctmin00b_c + dependencyratio00b_c + as.factor(STATEA), 
                        data = aa, 
                        family = "binomial")
summary(lichter_min_full)
plot_model(lichter_min_full, type = "pred", terms = c("pctmin00b_c"))

lichter_nhw <- glm(annexed ~ pctnhwhite00p_c + popdensitypl00_c + log(pop00p) + popgrowth_c + hinc00p_c + nhwhitepov00p_c +
                     pctowneroccupied_c + pctnhwhite00b_c + dependencyratio00b_c + as.factor(STATEA), 
                   data = aa %>% filter(STATEA=="51" | STATEA=="01" | STATEA=="05" | STATEA=="13" | STATEA=="22" | STATEA=="28" | STATEA=="37" | STATEA=="45"), 
                   family = "binomial")
summary(lichter_nhw)
plot_model(lichter_nhw, type = "pred", terms = c("pctnhwhite00b_c"))

lichter_nhw_full <- glm(annexed ~ pctnhwhite00p_c + popdensitypl00_c + log(pop00p) + popgrowth_c + hinc00p_c + nhwhitepov00p_c +
                          pctowneroccupied_c + pctnhwhite00b_c + dependencyratio00b_c + as.factor(STATEA), 
                        data = aa, 
                        family = "binomial")
summary(lichter_nhw_full)
plot_model(lichter_nhw_full, type = "pred", terms = c("pctnhwhite00b_c"))

stargazer(lichter_h, lichter_h_full,
          type = "html",
          out = "lichter_h_comp.htm")

stargazer(lichter_min, lichter_min_full,
          type = "html",
          out = "lichter_min_comp.htm")

stargazer(lichter_nhw, lichter_nhw_full,
          type = "html",
          out = "lichter_nhw_comp.htm")

# my models ####
#nhb ####
m3c_nhb <- glmer(annexed ~ popdensitypl00_c + log(pop00p) + popgrowth_c + hinc00p_c + blackpov00p_c +
                   recimmgrowth_c*pctnhblack00b_c + pctnhwhite00p_c*pctnhblack00b_c + 
                   pctowneroccupied_c + dependencyratio00b_c + 
                   (1 | plid2), data = aa, family = "binomial")
tab_model(m3c_nhb)
plot_model(m3c_nhb, type = "pred", terms = c("recimmgrowth_c"))

#h ####
m3c_h <- glmer(annexed ~ popdensitypl00_c + log(pop00p) + popgrowth_c + hinc00p_c + hpov00p_c +
                   pcth00b_c*recimmgrowth_c + pcth00b_c*pctnhwhite00p_c + 
                   pctowneroccupied_c + dependencyratio00b_c + 
                   (1 | plid2), data = aa, family = "binomial")
tab_model(m3c_h)
plot_model(m3c_h, type = "pred", terms = c("recimmgrowth_c"))
plot_model(m3c_h, type = "int")[[1]]

#min ####
m3c_min <- glmer(annexed ~ popdensitypl00_c + log(pop00p) + popgrowth_c + hinc00p_c + minpov00p_c +
                 pctmin00b_c*recimmgrowth_c + pctmin00b_c*pctnhwhite00p_c + 
                 pctowneroccupied_c + dependencyratio00b_c + 
                 (1 | plid2), data = aa, family = "binomial")
tab_model(m3c_min)
plot_model(m3c_min, type = "pred", terms = c("recimmgrowth_c"))
plot_model(m3c_min, type = "int")[[1]]

#nhwhite ####
m3c_nhw <- glmer(annexed ~ popdensitypl00_c + log(pop00p) + popgrowth_c + hinc00p_c + nhwhitepov00p_c +
                   pctnhwhite00b_c*recimmgrowth_c + pctnhwhite00b_c*pctnhwhite00p_c + 
                   pctowneroccupied_c + dependencyratio00b_c + 
                   (1 | plid2), data = aa, family = "binomial")
tab_model(m3c_nhw)
plot_model(m3c_nhw, type = "pred", terms = c("pctnhwhite00b_c"))
plot_model(m3c_nhw, type = "int")[[1]]

tab_model(m3c_nhb, m3c_h, m3c_min, m3c_nhw)
# descriptives 
descriptives <- aa %>% 
  select(c(pop00b:pctowneroccupied, annexed, popdensitypl00:incomegrowth)) %>%
  group_by(annexed) %>%
  summarize_if(is.numeric, list("mean" = mean, 
                                        "median" = median, 
                                        "sd" = sd), na.rm = T) %>%
  pivot_longer(cols = -annexed,
               names_to = c("variable", "statistic"),
               names_sep = "_",
               values_to = "result") %>%
  pivot_wider(names_from = "annexed", 
              values_from = "result") 

write_csv(descriptives, "paper_descriptives.csv")

# 2010 models ####
nhwhite9010 <- read_csv("nhwhite9010.csv")

nhwhite9010 <- nhwhite9010 %>%
  mutate(annexed = ifelse(nhwhite9010$plid %in% aa$plid2, 1, 0))
table(nhwhite9010$annexed, exclude = NULL)
table(nhwhite9010$Geo_STATE, exclude= NULL)

nhwhite9010 <- nhwhite9010 %>%
  mutate(nhwhitegrowth = ifelse(!is.finite(nhwhitegrowth), 0, nhwhitegrowth),
         nhblackgrowth = ifelse(!is.finite(nhblackgrowth), 0, nhblackgrowth),
         hgrowth = ifelse(!is.finite(hgrowth), 0, hgrowth),
         mingrowth = ifelse(!is.finite(mingrowth), 0, mingrowth))

nhwhite9010 <- nhwhite9010 %>%
  filter(!is.na(Geo_STATE))

nhw <- lm(pctnhwhite10 ~ pctnhwhite00p + nhwhitegrowth + as.factor(annexed) + as.factor(plid), data = nhwhite9010)
summary(nhw)

plot_model(nhw, type = "pred", term = "annexed")

h <- lm(pcth10 ~ pcth00p + hgrowth + as.factor(annexed) + as.factor(plid), data = nhwhite9010)
summary(h)
plot_model(h, type = "pred", term = "annexed")

nhb <- lm(pctnhblack10 ~ pctnhblack00p + nhblackgrowth + as.factor(annexed) + as.factor(plid), data = nhwhite9010)
summary(nhb)
plot_model(nhb, type = "pred", term = "annexed")

min <- lm(pctmin10 ~ pctmin00p + mingrowth + as.factor(annexed) + as.factor(plid), data = nhwhite9010)
summary(min)
plot_model(min, type = "pred", term = "annexed")

plot_model(lm(pctmin10 ~ as.factor(annexed), data = nhwhite9010), type = "pred")

stargazer(nhw, nhb, h, min,
          omit = "plid",
          type = "html",
          out = "pop10lm.htm")


min <- lmer(pctmin10 ~ pctmin00p + mingrowth + annexed + plid + (1 | plid), data = nhwhite9010)

