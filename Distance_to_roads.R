library(tidyverse)
library(car)
library(lme4)
library(MuMIn)
library(lattice)
library(cowplot)
source("stepAIC.R")

df <- read.table("data_yellowhammer.csv", sep=";", header=T) %>% 
  group_by(week, loc) %>% 
  mutate(locality = as.factor(cur_group_id())) %>% 
  ungroup %>% 
  mutate(wind_sc = scale(wind),
         temp_sc = scale(temp),
         pressure_sc = scale(pressure),
         cloudiness_sc = scale(cloudiness),
         humidity_sc = scale(humidity))

# Song duration -------------------------------------------------------------------------------

# multicollinearity check
lmer(length_sum~day+dist_road_resc+temp_sc+wind_sc+humidity_sc+pressure_sc+cloudiness_sc+(1|code)+(1|locality),
     data=df[df$loc=="AL",]) %>% 
  vif # humidity will be removed
lmer(length_sum~day+dist_road_resc+temp_sc+wind_sc+pressure_sc+cloudiness_sc+(1|code)+(1|locality),
     data=df[df$loc=="AL",]) %>% 
  vif # fine

# model selection
lmerTest::ranova(lmerTest::lmer(length_sum~day*(dist_road_resc+temp_sc+wind_sc+pressure_sc+cloudiness_sc)+(1|code)+(1|locality),
                                data=df[df$loc=="AL",]))
m2.1 <- lmer(length_sum~day*(dist_road_resc+temp_sc+wind_sc+pressure_sc+cloudiness_sc)+(1|code),
             data=df[df$loc=="AL",])
m2.1 <- stepAIC(m2.1)

# model diagnostics and output
plot(m2.1)
qqmath(m2.1)
r.squaredGLMM(m2.1)
summary(m2.1) %>% coef %>% round(3) %>% write.table("coef2.1.csv", sep=";")
Anova(m2.1) %>% round(4) %>% write.table("anova2.1.csv", sep=";")

# effect plots
nd2.1 <- data.frame(dist_road_resc = seq(min(df$dist_road_resc), max(df$dist_road_resc), l=100),
                    wind_sc = 0)
bm2.1 <- bootMer(m2.1, function(m){predict(m, newdata = nd2.1, re.form = NA)}, nsim = 500, .progress = "txt")
nd2.1 <- nd2.1 %>%
  mutate(y = bm2.1$t0,
         lwr = apply(bm2.1$t, 2, quantile, 0.025),
         upr = apply(bm2.1$t, 2, quantile, 0.975),
         dist_road = dist_road_resc*sd(df$distance_road) + mean(df$distance_road))
<<<<<<< HEAD
ggplot(nd2.1, aes(x=dist_road, y=y)) +
=======
(p2.1.1 <- ggplot(nd2.1.1, aes(x=dist_road, y=y)) +
    geom_line(color="chartreuse4") +
    geom_ribbon(aes(ymin=lwr, ymax=upr), alpha=.3, fill="chartreuse4") +
    labs(x="Distance to nearest road (m)", y="Song duration (s)") +
    theme_bw())

# syllable length -----------------------------------------------------------------------------

# model selection
m2.2 <- lmer(length_mean~day*(dist_road_resc+temp+wind+pressure+cloudiness)+(1|code),
             data=df[df$loc=="AL",])
Anova(m2.2) # removing interactions
m2.2 <- lmer(length_mean~day+dist_road_resc+temp+wind+pressure+cloudiness+(1|code),
             data=df[df$loc=="AL",])
Anova(m2.2) # final model (not significant)


# Number of syllables -------------------------------------------------------------------------

# model selection
m2.3 <- lmer(syll_sum~day*(dist_road_resc+temp+wind+pressure+cloudiness)+(1|code),
             data=df[df$loc=="AL",])
Anova(m2.3) # removing interactions
m2.3 <- lmer(syll_sum~day+dist_road_resc+temp+wind+pressure+cloudiness+(1|code),
             data=df[df$loc=="AL",])
Anova(m2.3) # removing temp, day, and cloudiness
m2.3 <- lmer(syll_sum~dist_road_resc+wind+pressure+(1|code),
             data=df[df$loc=="AL",])
Anova(m2.3) # final model

# model diagnostics and output
plot(m2.3)
qqmath(m2.3)
(s2.3 <- summary(m2.3))
r.squaredGLMM(m2.3)

# effect plots
nd2.3.1 <- data.frame(dist_road_resc = seq(min(df$dist_road_resc), max(df$dist_road_resc), l=100),
                      wind = mean(df$wind),
                      pressure = mean(df$pressure))
bm2.3.1 <- bootMer(m2.3, function(m){predict(m, newdata = nd2.3.1, re.form = NA)}, nsim = 500, .progress = "txt")
nd2.3.1 <- nd2.3.1 %>%
  mutate(y = bm2.3.1$t0,
         lwr = apply(bm2.3.1$t, 2, quantile, 0.025),
         upr = apply(bm2.3.1$t, 2, quantile, 0.975),
         dist_road = dist_road_resc*sd(df$distance_road) + mean(df$distance_road))
(p2.3.1 <- ggplot(nd2.3.1, aes(x=dist_road, y=y)) +
>>>>>>> 3bf4b21e3030bee334accf16672b54939eb4cb31
  geom_line(color="chartreuse4") +
  geom_ribbon(aes(ymin=lwr, ymax=upr), alpha=.3, fill="chartreuse4") +
  labs(x="Distance to nearest road (m)", y="Song duration (s)") +
  theme_bw()
ggsave("Figures_Yellowhammer_revision/effects2.1.png", dpi=300, height = 12, width = 14, units = "cm")


# Strophe length -----------------------------------------------------------------------------

# model selection
lmerTest::ranova(lmerTest::lmer(length_mean~day*(dist_road_resc+temp_sc+wind_sc+pressure_sc+cloudiness_sc)+(1|code)+(1|locality),
                                data=df[df$loc=="AL",]))
m2.2 <- lmer(length_mean~day*(dist_road_resc+temp_sc+wind_sc+pressure_sc+cloudiness_sc)+(1|code),
             data=df[df$loc=="AL",])
m2.2 <- stepAIC(m2.2)

# model diagnostics and output
plot(m2.2)
qqmath(m2.2)
r.squaredGLMM(m2.2)
summary(m2.2) %>% coef %>% round(3) %>% write.table("coef2.2.csv", sep=";")
Anova(m2.2) %>% round(4) %>% write.table("anova2.2.csv", sep=";")


# Onset of singing ----------------------------------------------------------------------------

# model selection
lmerTest::ranova(lmerTest::lmer(singing_sunrise~day*(dist_road_resc+temp_sc+wind_sc+pressure_sc+cloudiness_sc)+(1|code)+(1|locality),
                                data=df[df$loc=="AL",]))
m2.3 <- lmer(singing_sunrise~day*(dist_road_resc+temp_sc+wind_sc+pressure_sc+cloudiness_sc)+(1|code)+(1|locality),
             data=df[df$loc=="AL",])
m2.3 <- stepAIC(m2.3)
plot(m2.3)
qqmath(m2.3)
r.squaredGLMM(m2.3)
summary(m2.3) %>% coef %>% round(3) %>% write.table("coef2.3.csv", sep=";")
Anova(m2.3) %>% round(4) %>% write.table("anova2.3.csv", sep=";")

save(m2.1, m2.2, m2.3, m2.3, file = "models_m.2.x.RData")
