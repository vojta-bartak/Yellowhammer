library(tidyverse)
library(car)
library(lme4)
library(MuMIn)
library(lattice)
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
lmer(length_sum ~ day+loc+temp_sc+wind_sc+humidity_sc+pressure_sc+cloudiness_sc+(1|locality/code), data=df) %>% 
  vif # humidity will be removed
lmer(length_sum ~ day+loc+temp_sc+wind_sc+pressure_sc+cloudiness_sc+(1|locality/code), data=df) %>% 
  vif # fine

# model selection
lmerTest::ranova(lmerTest::lmer(length_sum ~ day*loc*(temp_sc+wind_sc+pressure_sc+cloudiness_sc)+(1|locality/code), data=df))
m1.1 <- lmer(length_sum ~ day*loc*(temp_sc+wind_sc+pressure_sc+cloudiness_sc)+(1|code), data=df)
m1.1 <- stepAIC(m1.1)

# model diagnostics and output
plot(m1.1)
qqmath(m1.1)
r.squaredGLMM(m1.1)
summary(m1.1) %>% coef %>% round(3) %>% write.table("coef1.1.csv", sep=";")
Anova(m1.1) %>% round(4) %>% write.table("anova1.1.csv", sep=";")

# effect plots
nd1.1 <- data.frame(loc=c("AL", "HW"),
                    wind_sc = 0,
                    pressure_sc = 0,
                    cloudiness_sc = 0,
                    day = factor(c("Monday"), levels=c("Sunday","Monday")))
bm1.1 <- bootMer(m1.1, 
                 function(m){predict(m, newdata = nd1.1, re.form = NA)}, 
                 nsim = 500, 
                 .progress = "txt")
nd1.1 <- nd1.1 %>%
  mutate(
    y = bm1.1$t0,
    lwr = apply(bm1.1$t, 2, quantile, 0.025),
    upr = apply(bm1.1$t, 2, quantile, 0.975),
    loc = factor(loc, levels = c("AL", "HW"), labels = c("Agricultural landscape", "Highway"))
  )
(p1.1 <- ggplot(nd1.1, aes(x=loc, y=y)) +
  geom_point() +
  geom_errorbar(aes(ymin=lwr, ymax=upr), width=0.2) +
  labs(x="", y="Song duration (s)") +
  theme_bw() +
  theme(legend.position = "top"))
ggsave("Figures_Yellowhammer_revision/effects1.1.png", dpi=300, height = 12, width = 14, units = "cm")

nd1.S1 <- data.frame(loc=factor(c("AL"), levels=c("AL","HW")),
                    wind_sc = seq(min(df$wind_sc), max(df$wind_sc), l=100),
                    pressure_sc=0,
                    cloudiness_sc = 0,
                    day = factor(c("Monday"), levels=c("Sunday","Monday")))
bm1.S1 <- bootMer(m1.1, 
                 function(m){predict(m, newdata = nd1.S1, re.form = NA)}, 
                 nsim = 500, 
                 .progress = "txt")
nd1.S1 <- nd1.S1 %>%
  mutate(
    wind = wind_sc*sd(df$wind) + mean(df$wind),
    y = bm1.S1$t0,
    lwr = apply(bm1.S1$t, 2, quantile, 0.025),
    upr = apply(bm1.S1$t, 2, quantile, 0.975),
    loc = factor(loc, levels = c("AL", "HW"), labels = c("Agricultural landscape", "Highway"))
  )
ggplot(nd1.S1, aes(x=wind, y=y)) +
  geom_line() +
  geom_ribbon(aes(ymin=lwr, ymax=upr), alpha=0.3) +
  labs(x="Wind speed (m/s)", y="Song duration (s)") +
  theme_bw() +
  theme(legend.position = "top")
ggsave("Figures_Yellowhammer_revision/effects1.1_S1.png", dpi=300, height = 12, width = 14, units = "cm")

nd1.S2 <- data.frame(loc=factor(c("AL"), levels=c("AL","HW")),
                     pressure_sc = seq(min(df$pressure_sc), max(df$pressure_sc), l=100),
                     wind_sc = 0,
                     cloudiness_sc = 0,
                     day = factor(c("Monday"), levels=c("Sunday","Monday")))
bm1.S2 <- bootMer(m1.1, 
                  function(m){predict(m, newdata = nd1.S2, re.form = NA)}, 
                  nsim = 500, 
                  .progress = "txt")
nd1.S2 <- nd1.S2 %>%
  mutate(
    pressure = pressure_sc*sd(df$pressure) + mean(df$pressure),
    y = bm1.S2$t0,
    lwr = apply(bm1.S2$t, 2, quantile, 0.025),
    upr = apply(bm1.S2$t, 2, quantile, 0.975),
    loc = factor(loc, levels = c("AL", "HW"), labels = c("Agricultural landscape", "Highway"))
  )
ggplot(nd1.S2, aes(x=pressure, y=y)) +
  geom_line() +
  geom_ribbon(aes(ymin=lwr, ymax=upr), alpha=0.3) +
  labs(x="Air pressure (hPa)", y="Song duration (s)") +
  theme_bw() +
  theme(legend.position = "top")
ggsave("Figures_Yellowhammer_revision/effects1.2_S2.png", dpi=300, height = 12, width = 14, units = "cm")


# Strophe length -----------------------------------------------------------------------------
# model selection
lmerTest::ranova(lmerTest::lmer(length_mean ~ day*loc*(temp_sc+wind_sc+pressure_sc+cloudiness_sc)+(1|locality/code), data=df))
m1.2 <- lmer(length_mean ~ day*loc*(temp_sc+wind_sc+pressure_sc+cloudiness_sc)+(1|code), data=df)
m1.2 <- stepAIC(m1.2)

# model diagnostics and output
plot(m1.2)
qqmath(m1.2)
r.squaredGLMM(m1.2)
summary(m1.2) %>% coef %>% round(3) %>% write.table("coef1.2.csv", sep=";")
Anova(m1.2) %>% round(4) %>% write.table("anova1.2.csv", sep=";")

# effect plots
nd1.2 <- expand.grid(loc=c("AL", "HW"),
                     temp_sc=seq(min(df$temp_sc), max(df$temp_sc), l=100))
bm1.2 <- bootMer(m1.2, 
                 function(m){predict(m, newdata = nd1.2, re.form = NA)}, 
                 nsim = 500, 
                 .progress = "txt")
nd1.2 <- nd1.2 %>%
  mutate(
    temp = temp_sc*sd(df$temp) + mean(df$temp),
    y = bm1.2$t0,
    lwr = apply(bm1.2$t, 2, quantile, 0.025),
    upr = apply(bm1.2$t, 2, quantile, 0.975),
    loc = factor(loc, levels = c("AL", "HW"), labels = c("Agricultural landscape", "Highway"))
  )
(p1.2 <- ggplot(nd1.2, aes(x=temp, y=y)) +
  geom_line(aes(color=loc)) +
  geom_ribbon(aes(ymin=lwr, ymax=upr, fill=loc), alpha=.3) +
  scale_color_manual(values = c("chartreuse4", "coral4")) +
  scale_fill_manual(values = c("chartreuse4", "coral4")) +
  labs(x="Temperature (°C)", y="Strophe length (s)", color="", fill="") +
  theme_bw() +
  theme(legend.position = "top"))
ggsave("Figures_Yellowhammer_revision/effects1.2.png", dpi=300, height = 12, width = 14, units = "cm")


# Onset of singing ----------------------------------------------------------------------------

# model selection
lmerTest::ranova(lmerTest::lmer(singing_sunrise ~ day*loc*(temp_sc+wind_sc+pressure_sc+cloudiness_sc)+(1|locality/code), data=df))
m1.3 <- lmer(singing_sunrise ~ day*loc*(temp_sc+wind_sc+pressure_sc+cloudiness_sc)+(1|locality/code), data=df)
m1.3 <- stepAIC(m1.3)

# model diagnostics and output
plot(m1.3)
qqmath(m1.3)
r.squaredGLMM(m1.3)
summary(m1.3) %>% coef %>% round(3) %>% write.table("coef1.3.csv", sep=";")
Anova(m1.3) %>% round(4) %>% write.table("anova1.3.csv", sep=";")

# effect plots
nd1.3 <- expand.grid(loc=c("AL", "HW"), 
                     day=factor(c("Monday"), levels=c("Sunday", "Monday")),
                     pressure_sc=seq(min(df$pressure_sc), max(df$pressure_sc), l=100)) %>%
  mutate(wind_sc = 0,
         cloudiness_sc = 0)
bm1.3 <- bootMer(m1.3, 
                 function(m){predict(m, newdata = nd1.3, re.form = NA)}, 
                 nsim = 500, 
                 .progress = "txt")
nd1.3 <- nd1.3 %>%
  mutate(
    pressure = pressure_sc*sd(df$pressure) + mean(df$pressure),
    y = bm1.3$t0,
    lwr = apply(bm1.3$t, 2, quantile, 0.025),
    upr = apply(bm1.3$t, 2, quantile, 0.975),
    loc = factor(loc, levels = c("AL", "HW"), labels = c("Agricultural landscape", "Highway"))
  )
(p1.3 <- ggplot(nd1.3, aes(x=pressure, y=y)) +
  geom_line(aes(color=loc)) +
  geom_ribbon(aes(ymin=lwr, ymax=upr, fill=loc), alpha=.3) +
  scale_color_manual(values = c("chartreuse4", "coral4")) +
  scale_fill_manual(values = c("chartreuse4", "coral4")) +
  labs(y = "Onset of singing (mins rel. to sunrise)",
       x = "Air pressure (hPa)",
       color="", fill="") +
  theme_bw() +
  theme(legend.position = "top"))
ggsave("Figures_Yellowhammer_revision/effects1.3.png", dpi=300, height = 12, width = 14, units = "cm")

nd1.3_S3 <- expand.grid(loc=factor(c("AL"), levels=c("AL", "HW")),
                        day=factor(c("Monday"), levels=c("Sunday", "Monday")),
                        wind_sc=seq(min(df$wind_sc), max(df$wind_sc), l=100)) %>%
  mutate(pressure_sc = 0,
         cloudiness_sc = 0)
bm1.3_S3 <- bootMer(m1.3, 
                 function(m){predict(m, newdata = nd1.3_S3, re.form = NA)}, 
                 nsim = 500, 
                 .progress = "txt")
nd1.3_S3 <- nd1.3_S3 %>%
  mutate(
    wind = wind_sc*sd(df$wind) + mean(df$wind),
    y = bm1.3_S3$t0,
    lwr = apply(bm1.3_S3$t, 2, quantile, 0.025),
    upr = apply(bm1.3_S3$t, 2, quantile, 0.975),
    loc = factor(loc, levels = c("AL", "HW"), labels = c("Agricultural landscape", "Highway"))
  )
ggplot(nd1.3_S3, aes(x=wind, y=y)) +
  geom_line() +
  geom_ribbon(aes(ymin=lwr, ymax=upr), alpha=.3) +
  labs(y = "Onset of singing (mins rel. to sunrise)",
       x = "Wind_speed (m/s)",
       color="", fill="") +
  theme_bw() +
  theme(legend.position = "top")
ggsave("Figures_Yellowhammer_revision/effects1.3_S3.png", dpi=300, height = 12, width = 14, units = "cm")

nd1.3_S4 <- expand.grid(loc=factor(c("AL"), levels=c("AL", "HW")),
                        day=factor(c("Monday"), levels=c("Sunday", "Monday")),
                        cloudiness_sc=seq(min(df$cloudiness_sc), max(df$cloudiness_sc), l=100)) %>%
  mutate(pressure_sc = 0,
         wind_sc = 0)
bm1.3_S4 <- bootMer(m1.3, 
                    function(m){predict(m, newdata = nd1.3_S4, re.form = NA)}, 
                    nsim = 500, 
                    .progress = "txt")
nd1.3_S4 <- nd1.3_S4 %>%
  mutate(
    cloudiness = cloudiness_sc*sd(df$cloudiness) + mean(df$cloudiness),
    y = bm1.3_S4$t0,
    lwr = apply(bm1.3_S4$t, 2, quantile, 0.025),
    upr = apply(bm1.3_S4$t, 2, quantile, 0.975),
    loc = factor(loc, levels = c("AL", "HW"), labels = c("Agricultural landscape", "Highway"))
  )
ggplot(nd1.3_S4, aes(x=cloudiness, y=y)) +
  geom_line() +
  geom_ribbon(aes(ymin=lwr, ymax=upr), alpha=.3) +
  labs(y = "Onset of singing (mins rel. to sunrise)",
       x = "Cloudiness",
       color="", fill="") +
  theme_bw() +
  theme(legend.position = "top")
ggsave("Figures_Yellowhammer_revision/effects1.3_S4.png", dpi=300, height = 12, width = 14, units = "cm")

# All effect plots together -------------------------------------------
ggpubr::ggarrange(p1.1,p1.2,p1.3,ncol=3,nrow=1,common.legend = T)
ggsave("Figures_Yellowhammer_revision/effects1.x_all_version1.png", width = 21, height = 12, units = "cm")

ggpubr::ggarrange(p1.1,p1.2,p1.3,ncol=2,nrow=2,common.legend = T)
ggsave("Figures_Yellowhammer_revision/effects1.x_all_version2.png", width = 21, height = 16, units = "cm")

save(m1.1, m1.2, m1.3, file = "models_m.1.x.RData")
