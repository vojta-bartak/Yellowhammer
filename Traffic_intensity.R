library(tidyverse)
library(car)
library(lme4)
library(MuMIn)
library(lattice)
library(cowplot)

df <- read.table("data_yellowhammer.csv", sep=";", header=T) %>% 
  group_by(week, loc) %>% 
  mutate(locality = as.factor(cur_group_id())) %>% 
  ungroup %>% 
  mutate(wind_sc = scale(wind),
         temp_sc = scale(temp),
         pressure_sc = scale(pressure),
         cloudiness_sc = scale(cloudiness),
         humidity_sc = scale(humidity),
         vehicle_sc = scale(vehicle))

# Song duration -------------------------------------------------------------------------------

# multicollinearity check
lmer(length_sum ~ vehicle_sc+RPDI2sc+temp_sc+wind_sc+humidity_sc+pressure_sc+cloudiness+(1|code)+(1|locality),
     data=df[df$loc=="HW",]) %>% 
  vif # humidity will be removed
lmer(length_sum ~ vehicle_sc+RPDI2sc+temp_sc+wind_sc+pressure_sc+cloudiness+(1|code)+(1|locality),
     data=df[df$loc=="HW",]) %>% 
  vif # fine

# model selection
lmerTest::ranova(lmerTest::lmer(length_sum ~ vehicle_sc+RPDI2sc+temp_sc+wind_sc+pressure_sc+cloudiness_sc+(1|code)+(1|locality),
                                data=df[df$loc=="HW",]))
m3.1 <- lmer(length_sum ~ vehicle_sc+RPDI2sc+temp_sc+wind_sc+pressure_sc+cloudiness_sc+(1|code),
             data=df[df$loc=="HW",])
m3.1 <- stepAIC(m3.1)

# model diagnostics and output
plot(m3.1)
qqmath(m3.1)
r.squaredGLMM(m3.1)
summary(m3.1) %>% coef %>% round(3) %>% write.table("coef3.1.csv", sep=";")
Anova(m3.1) %>% round(4) %>% write.table("anova3.1.csv", sep=";")

<<<<<<< HEAD
# strophe length -----------------------------------------------------------------------------
=======

# syllable length -----------------------------------------------------------------------------
>>>>>>> 3bf4b21e3030bee334accf16672b54939eb4cb31

# model selection
lmerTest::ranova(lmerTest::lmer(length_mean ~ vehicle_sc+RPDI2sc+temp_sc+wind_sc+pressure_sc+cloudiness_sc+(1|code)+(1|locality),
                                data=df[df$loc=="HW",]))
m3.2 <- lmer(length_mean ~ vehicle_sc+RPDI2sc+temp_sc+wind_sc+pressure_sc+cloudiness_sc+(1|code),
             data=df[df$loc=="HW",])
m3.2 <- stepAIC(m3.2)

# model diagnostics and output
plot(m3.2)
qqmath(m3.2)
r.squaredGLMM(m3.2)
summary(m3.2) %>% coef %>% round(3) %>% write.table("coef3.2.csv", sep=";")
Anova(m3.2) %>% round(4) %>% write.table("anova3.2.csv", sep=";")


# Onset of singing ----------------------------------------------------------------------------

# model selection
lmerTest::ranova(lmerTest::lmer(singing_sunrise ~ vehicle_sc+RPDI2sc+temp_sc+wind_sc+pressure_sc+cloudiness_sc+(1|code)+(1|locality),
                                data=df[df$loc=="HW",]))
m3.3 <- lmer(singing_sunrise ~ vehicle_sc+RPDI2sc+temp_sc+wind_sc+pressure_sc+cloudiness_sc+(1|code),
             data=df[df$loc=="HW",])
m3.3 <- stepAIC(m3.3)

# model diagnostics and output
plot(m3.3)
qqmath(m3.3)
r.squaredGLMM(m3.3)
summary(m3.3) %>% coef %>% round(3) %>% write.table("coef3.3.csv", sep=";")
Anova(m3.3) %>% round(4) %>% write.table("anova3.3.csv", sep=";")


# Effect plots --------------------------------------------------------------------------------
nd3.1 <- data.frame(RPDI2sc = seq(min(df[df$loc=="HW",]$RPDI2sc), max(df[df$loc=="HW",]$RPDI2sc), l=100),
                    temp_sc=0,
                    wind_sc=0)
bm3.1 <- bootMer(m3.1, function(m){predict(m, newdata = nd3.1, re.form = NA)}, nsim = 500, .progress = "txt")
nd3.1 <- nd3.1 %>%
  mutate(y = bm3.1$t0,
         lwr = apply(bm3.1$t, 2, quantile, 0.025),
         upr = apply(bm3.1$t, 2, quantile, 0.975))

nd3.2 <- data.frame(vehicle_sc = seq(min(df[df$loc=="HW",]$vehicle_sc), max(df[df$loc=="HW",]$vehicle_sc), l=100),
                    temp_sc=0,
                    wind_sc=0,
                    pressure_sc=0,
                    cloudiness_sc=0)
bm3.2 <- bootMer(m3.2, function(m){predict(m, newdata = nd3.2, re.form = NA)}, nsim = 500, .progress = "txt")
nd3.2 <- nd3.2 %>%
  mutate(vehicle = vehicle_sc*sd(df$vehicle) + mean(df$vehicle),
         y = bm3.2$t0,
         lwr = apply(bm3.2$t, 2, quantile, 0.025),
         upr = apply(bm3.2$t, 2, quantile, 0.975))


nd3.3 <- data.frame(vehicle_sc = seq(min(df[df$loc=="HW",]$vehicle_sc), max(df[df$loc=="HW",]$vehicle_sc), l=100),
                    pressure_sc=0,
                    wind_sc=0,
                    cloudiness_sc=0)
bm3.3 <- bootMer(m3.3, function(m){predict(m, newdata = nd3.3, re.form = NA)}, nsim = 500, .progress = "txt")
nd3.3 <- nd3.3 %>%
  mutate(vehicle = vehicle_sc*sd(df$vehicle) + mean(df$vehicle),
         y = bm3.3$t0,
         lwr = apply(bm3.3$t, 2, quantile, 0.025),
         upr = apply(bm3.3$t, 2, quantile, 0.975))

plot_grid(ggplot(nd3.1, aes(x=RPDI2sc*sd(df[df$loc=="HW",]$RPDI2)+mean(df[df$loc=="HW",]$RPDI2), y=y)) +
            geom_line(color="coral4") +
            geom_ribbon(aes(ymin=lwr, ymax=upr), alpha=.3, fill="coral4") +
            labs(x="Long-term traffic intensity (cars/day)", y="Song duration (s)") +
            theme_bw(),
          ggplot(nd3.2, aes(x=vehicle, y=y)) +
            geom_line(color="coral4") +
            geom_ribbon(aes(ymin=lwr, ymax=upr), alpha=.3, fill="coral4") +
<<<<<<< HEAD
            labs(x="Instant traffic intensity (cars/hour)", y="Strophe length (s)") +
=======
            labs(x="", y="Syllable length (s)") +
>>>>>>> 3bf4b21e3030bee334accf16672b54939eb4cb31
            theme_bw(),
          ggplot(nd3.3, aes(x=vehicle, y=y)) +
            geom_line(color="coral4") +
            geom_ribbon(aes(ymin=lwr, ymax=upr), alpha=.3, fill="coral4") +
            labs(x="Instant traffic intensity (cars/hour)", y="Onset of singing (mins rel. to sunrise)") +
            theme_bw(),
          ncol = 3)
ggsave("Figures_Yellowhammer_revision/effects_m3.x_version1.png", dpi=300, height = 12, width = 24, units = "cm")

plot_grid(ggplot(nd3.1, aes(x=RPDI2sc*sd(df[df$loc=="HW",]$RPDI2)+mean(df[df$loc=="HW",]$RPDI2), y=y)) +
            geom_line(color="coral4") +
            geom_ribbon(aes(ymin=lwr, ymax=upr), alpha=.3, fill="coral4") +
            labs(x="Long-term traffic intensity (cars/day)", y="Song duration (s)") +
            theme_bw(),
          ggplot(nd3.2, aes(x=vehicle, y=y)) +
            geom_line(color="coral4") +
            geom_ribbon(aes(ymin=lwr, ymax=upr), alpha=.3, fill="coral4") +
            labs(x="Instant traffic intensity (cars/hour)", y="Strophe length (s)") +
            theme_bw(),
          ggplot(nd3.3, aes(x=vehicle, y=y)) +
            geom_line(color="coral4") +
            geom_ribbon(aes(ymin=lwr, ymax=upr), alpha=.3, fill="coral4") +
            labs(x="Instant traffic intensity (cars/hour)", y="Onset of singing (mins rel. to sunrise)") +
            theme_bw(),
          ncol = 2)
ggsave("Figures_Yellowhammer_revision/effects_m3.x_version2.png", dpi=300, height = 16, width = 21, units = "cm")

save(m3.1, m3.2, m3.3, file = "models_m.3.x.RData")
