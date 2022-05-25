library(tidyverse)
library(car)
library(lme4)
library(MuMIn)
library(lattice)
library(cowplot)

df <- read.table("data_yellowhammer.csv", sep=";", header=T)

# Song duration -------------------------------------------------------------------------------

# multicollinearity check
lmer(length_sum~day+dist_road_resc+temp+wind+humidity+pressure+cloudiness+(1|code),
     data=df[df$loc=="AL",]) %>% 
  vif # humidity will be removed
lmer(length_sum~day+dist_road_resc+temp+wind+pressure+cloudiness+(1|code),
     data=df[df$loc=="AL",]) %>% 
  vif # fine

# model selection
m2.1 <- lmer(length_sum~day*(dist_road_resc+temp+wind+pressure+cloudiness)+(1|code),
             data=df[df$loc=="AL",])
Anova(m2.1) # removing interactions
m2.1 <- lmer(length_sum~day+dist_road_resc+temp+wind+pressure+cloudiness+(1|code),
             data=df[df$loc=="AL",])
Anova(m2.1) # removing temp, day, and cloudiness
m2.1 <- lmer(length_sum~dist_road_resc+wind+pressure+(1|code),
             data=df[df$loc=="AL",])
Anova(m2.1) # final model

# model diagnostics and output
plot(m2.1)
qqmath(m2.1)
(s2.1 <- summary(m2.1))
r.squaredGLMM(m2.1)

# effect plots
nd2.1.1 <- data.frame(dist_road_resc = seq(min(df$dist_road_resc), max(df$dist_road_resc), l=100),
                      wind = mean(df$wind),
                      pressure = mean(df$pressure))
bm2.1.1 <- bootMer(m2.1, function(m){predict(m, newdata = nd2.1.1, re.form = NA)}, nsim = 500, .progress = "txt")
nd2.1.1 <- nd2.1.1 %>%
  mutate(y = bm2.1.1$t0,
         lwr = apply(bm2.1.1$t, 2, quantile, 0.025),
         upr = apply(bm2.1.1$t, 2, quantile, 0.975),
         dist_road = dist_road_resc*sd(df$distance_road) + mean(df$distance_road))
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
  geom_line(color="chartreuse4") +
  geom_ribbon(aes(ymin=lwr, ymax=upr), alpha=.3, fill="chartreuse4") +
  labs(x="Distance to nearest road (m)", y="Number of syllables") +
  theme_bw())

plot_grid(p2.1.1 + labs(), 
          p2.3.1 + labs(), 
          ncol = 2)
ggsave("effects_m2.1_m2.3_dist.png", dpi=300, height = 10, width = 18, units = "cm")


# Onset of singing ----------------------------------------------------------------------------

# model selection
m2.4 <- lmer(singing_sunrise~day*(dist_road_resc+temp+wind+pressure+cloudiness)+(1|code),
             data=df[df$loc=="AL",])
Anova(m2.4) # removing insignificant interactions
m2.4 <- update(m2.4, ~.-day:dist_road_resc-day:temp)
Anova(m2.4) # final model (not significant)

save(m2.1, m2.2, m2.3, m2.4, file = "models_m.2.x.RData")
