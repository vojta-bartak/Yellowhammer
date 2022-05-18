library(tidyverse)
library(car)
library(lme4)
library(MuMIn)
library(lattice)

df <- read.table("data_yellowhammer.csv", sep=";", header=T)

# Song duration -------------------------------------------------------------------------------

# multicollinearity check
lmer(length_sum ~ day+loc+temp+wind+humidity+pressure+cloudiness+(1|code), data=df) %>% 
  vif # humidity will be removed
lmer(length_sum ~ day+loc+temp+wind+pressure+cloudiness+(1|code), data=df) %>% 
  vif # fine

# model selection
m1.1 <- lmer(length_sum ~ day*loc*(temp+wind+pressure+cloudiness)+(1|code), data=df)
Anova(m1.1) # removing three-fold interactions
m1.1 <- update(m1.1, ~.-day:loc:temp-day:loc:wind-day:loc:pressure-day:loc:cloudiness)
Anova(m1.1) # removing interactions with day
m1.1 <- lmer(length_sum ~ day+loc*(temp+wind+pressure+cloudiness)+(1|code), data=df)
Anova(m1.1) # removing interactions with loc except loc:pressure
m1.1 <- lmer(length_sum ~ day+loc*pressure+temp+wind+cloudiness+(1|code), data=df)
Anova(m1.1) # removing temp
m1.1 <- lmer(length_sum ~ day+loc*pressure+wind+cloudiness+(1|code), data=df)
Anova(m1.1) # final model

# model diagnostics and output
plot(m1.1)
qqmath(m1.1)
(s1.1 <- summary(m1.1))
r.squaredGLMM(m1.1)

# effect plots
nd1.1 <- expand.grid(day=factor(c("Monday"), levels=c("Monday", "Sunday")), 
                     loc=c("AL", "HW"),
                     pressure=seq(min(df$pressure), max(df$pressure), length.out=100),
                     wind = c(0,5,10)) %>%
  mutate(
    cloudiness = mean(df$cloudiness)
  )
bm1.1 <- bootMer(m1.1, 
                 function(m){predict(m, newdata = nd1.1, re.form = NA)}, 
                 nsim = 500, 
                 .progress = "txt")
nd1.1 <- nd1.1 %>%
  mutate(
    y = bm1.1$t0,
    lwr = apply(bm1.1$t, 2, quantile, 0.025),
    upr = apply(bm1.1$t, 2, quantile, 0.975),
    wind = factor(wind, levels=c(0,5,10), labels = paste("Wind speed:",c(0,5,10),"m/s")),
    loc = factor(loc, levels = c("AL", "HW"), labels = c("Agricultural landscape", "Highway"))
  )
ggplot(nd1.1, aes(x=pressure, y=y)) +
  geom_line(aes(color=loc)) +
  geom_ribbon(aes(ymin=lwr, ymax=upr, fill=loc), alpha=.3) +
  scale_color_manual(values = c("chartreuse4", "coral4")) +
  scale_fill_manual(values = c("chartreuse4", "coral4")) +
  facet_grid(.~wind) +
  labs(x="Air pressure (hPa)", y="Song duration (sec)", color="", fill="") +
  theme_bw() +
  theme(legend.position = "top")
ggsave("effects_m1.1.png", dpi=300, height = 14, width = 18, units = "cm")


# Syllabus length -----------------------------------------------------------------------------

# model selection
m1.2 <- lmer(length_mean ~ day*loc*(temp+wind+pressure+cloudiness)+(1|code), data=df)
Anova(m1.2) # removing three-fold interactions
m1.2 <- update(m1.2, ~.-day:loc:temp-day:loc:wind-day:loc:pressure-day:loc:cloudiness)
Anova(m1.2) # removing interactions with day, except loc:day
m1.2 <- lmer(length_mean ~ day+loc*(temp+wind+pressure+cloudiness)+day:loc+(1|code), data=df)
Anova(m1.2) # final model

# model diagnostics and output
plot(m1.2)
qqmath(m1.2)
(s1.2 <- summary(m1.2))
r.squaredGLMM(m1.2)

# effect plots
nd1.2 <- expand.grid(day=factor(c("Monday"), levels=c("Monday", "Sunday")), 
                     loc=c("AL", "HW"),
                     pressure=seq(min(df$pressure), max(df$pressure), length.out=100)) %>%
  mutate(
    cloudiness = mean(df$cloudiness),
    wind = mean(df$wind),
    temp = mean(df$temp),
    var = "Air pressure (hPa)",
    x = pressure
  ) %>%
  rbind(expand.grid(day=factor(c("Monday"), levels=c("Monday", "Sunday")), 
                    loc=c("AL", "HW"),
                    temp=seq(min(df$temp), max(df$temp), length.out=100)) %>%
          mutate(
            cloudiness = mean(df$cloudiness),
            wind = mean(df$wind),
            pressure = mean(df$pressure),
            var = "Temperature (°C)",
            x = temp)) %>%
  rbind(expand.grid(day=factor(c("Monday"), levels=c("Monday", "Sunday")), 
                    loc=c("AL", "HW"),
                    cloudiness=seq(min(df$cloudiness), max(df$cloudiness), length.out=100)) %>%
          mutate(
            temp = mean(df$temp),
            wind = mean(df$wind),
            pressure = mean(df$pressure),
            var = "Cloudiness",
            x = cloudiness)) %>%
  rbind(expand.grid(day=factor(c("Monday"), levels=c("Monday", "Sunday")), 
                    loc=c("AL", "HW"),
                    wind=seq(min(df$wind), max(df$wind), length.out=100)) %>%
          mutate(
            temp = mean(df$temp),
            cloudiness = mean(df$cloudiness),
            pressure = mean(df$pressure),
            var = "Wind speed (m/s)",
            x = wind))
bm1.2 <- bootMer(m1.2, 
                 function(m){predict(m, newdata = nd1.2, re.form = NA)}, 
                 nsim = 500, 
                 .progress = "txt")
nd1.2 <- nd1.2 %>%
  mutate(
    y = bm1.2$t0,
    lwr = apply(bm1.2$t, 2, quantile, 0.025),
    upr = apply(bm1.2$t, 2, quantile, 0.975),
    loc = factor(loc, levels = c("AL", "HW"), labels = c("Agricultural landscape", "Highway"))
  )
ggplot(nd1.2, aes(x=x, y=y)) +
  geom_line(aes(color=loc)) +
  geom_ribbon(aes(ymin=lwr, ymax=upr, fill=loc), alpha=.3) +
  scale_color_manual(values = c("chartreuse4", "coral4")) +
  scale_fill_manual(values = c("chartreuse4", "coral4")) +
  facet_wrap(~var, scales = "free_x") +
  labs(x="", y="Syllabus length (sec)", color="", fill="") +
  theme_bw() +
  theme(legend.position = "top")
ggsave("effects_m1.2.png", dpi=300, height = 14, width = 18, units = "cm")

nd1.2.2 <- expand.grid(day=factor(c("Monday", "Sunday"), levels=c("Sunday", "Monday")), 
                       loc=c("AL", "HW")) %>%
  mutate(
    cloudiness = mean(df$cloudiness),
    wind = mean(df$wind),
    temp = mean(df$temp),
    pressure = mean(df$pressure)
  )
bm1.2.2 <- bootMer(m1.2, 
                   function(m){predict(m, newdata = nd1.2.2, re.form = NA)}, 
                   nsim = 500, 
                   .progress = "txt")
nd1.2.2 <- nd1.2.2 %>%
  mutate(
    y = bm1.2.2$t0,
    lwr = apply(bm1.2.2$t, 2, quantile, 0.025),
    upr = apply(bm1.2.2$t, 2, quantile, 0.975),
    loc = factor(loc, levels = c("AL", "HW"), labels = c("Agricultural landscape", "Highway"))
  )
ggplot(nd1.2.2, aes(x=day, y=y, color=loc)) +
  geom_point(position = position_dodge(width = .2)) +
  geom_errorbar(aes(ymin=lwr, ymax=upr, fill=loc), position = position_dodge(), width = .2) +
  scale_color_manual(values = c("chartreuse4", "coral4")) +
  scale_fill_manual(values = c("chartreuse4", "coral4")) +
  labs(x="", y="Syllabus length (sec)", color="", fill="") +
  theme_bw() +
  theme(legend.position = "top")
ggsave("effects_m1.2_day.png", dpi=300, height = 10, width = 10, units = "cm")


# Number of syllables -------------------------------------------------------------------------

# model selection
m1.3 <- lmer(syll_sum ~ day*loc*(temp+wind+pressure+cloudiness)+(1|code), data=df)
Anova(m1.3) # removing three-fold interactions
m1.3 <- update(m1.3, ~.-day:loc:temp-day:loc:wind-day:loc:pressure-day:loc:cloudiness)
Anova(m1.3) # removing interactions with day
m1.3 <- lmer(syll_sum ~ day+loc*(temp+wind+pressure+cloudiness)+(1|code), data=df)
Anova(m1.3) # removing loc:wind and loc:cloudiness
m1.3 <- lmer(syll_sum ~ day+loc*(pressure+temp)+wind+cloudiness+(1|code), data=df)
Anova(m1.3) # removing cloudiness
m1.3 <- lmer(syll_sum ~ day+loc*(pressure+temp)+wind+(1|code), data=df)
Anova(m1.3) # final model

# model diagnostics and output
plot(m1.3)
qqmath(m1.3)
(s1.3 <- summary(m1.3))
r.squaredGLMM(m1.3)

# effect plots
nd1.3 <- expand.grid(day=factor(c("Monday"), levels=c("Monday", "Sunday")), 
                     loc=c("AL", "HW"),
                     pressure=seq(min(df$pressure), max(df$pressure), length.out=100)) %>%
  mutate(
    cloudiness = mean(df$cloudiness),
    wind = mean(df$wind),
    temp = mean(df$temp)
  )
bm1.3 <- bootMer(m1.3, 
                 function(m){predict(m, newdata = nd1.3, re.form = NA)}, 
                 nsim = 500, 
                 .progress = "txt")
nd1.3 <- nd1.3 %>%
  mutate(
    y = bm1.3$t0,
    lwr = apply(bm1.3$t, 2, quantile, 0.025),
    upr = apply(bm1.3$t, 2, quantile, 0.975),
    wind = factor(wind, levels=c(0,5,10), labels = paste("Wind speed:",c(0,5,10),"m/s")),
    loc = factor(loc, levels = c("AL", "HW"), labels = c("Agricultural landscape", "Highway"))
  )
ggplot(nd1.3, aes(x=pressure, y=y)) +
  geom_line(aes(color=loc)) +
  geom_ribbon(aes(ymin=lwr, ymax=upr, fill=loc), alpha=.3) +
  scale_color_manual(values = c("chartreuse4", "coral4")) +
  scale_fill_manual(values = c("chartreuse4", "coral4")) +
  labs(x="Air pressure (hPa)", y="Number of syllables", color="", fill="") +
  theme_bw() +
  theme(legend.position = "top")
ggsave("effects_m1.3.png", dpi=300, height = 10, width = 12, units = "cm")


# Onset of singing ----------------------------------------------------------------------------

# model selection
m1.4 <- lmer(singing_sunrise ~ day*loc*(temp+wind+pressure+cloudiness)+(1|code), data=df)
Anova(m1.4) # removing three-fold interactions
m1.4 <- update(m1.4, ~.-day:loc:temp-day:loc:wind-day:loc:pressure-day:loc:cloudiness)
Anova(m1.4) # removing day:temp
m1.4 <- update(m1.4, ~.-day:temp)
Anova(m1.4) # removing loc:wind
m1.4 <- update(m1.4, ~.-loc:wind)
Anova(m1.4) # removing loc:cloudiness
m1.4 <- update(m1.4, ~.-loc:cloudiness)
Anova(m1.4) # final model

# model diagnostics and output
plot(m1.4)
qqmath(m1.4)
(s1.4 <- summary(m1.4))
r.squaredGLMM(m1.4)

# effect plots
nd1.4 <- expand.grid(loc=c("AL", "HW"), 
                     day=c("Sunday", "Monday"),
                     pressure=seq(min(df$pressure), max(df$pressure), length.out=100)) %>%
  mutate(wind = mean(df$wind),
         cloudiness = mean(df$cloudiness),
         temp = mean(df$temp),
         x = pressure,
         variable = "Air pressure") %>% 
  rbind(expand.grid(loc=c("AL", "HW"), 
                    day=c("Sunday", "Monday"),
                    temp=seq(min(df$temp), max(df$temp), length.out=100)) %>%
          mutate(wind = mean(df$wind),
                 cloudiness = mean(df$cloudiness),
                 pressure = mean(df$pressure),
                 x = temp,
                 variable = "Temperature")) %>% 
  rbind(expand.grid(loc=c("AL", "HW"),   
                    day=c("Sunday", "Monday"),
                    cloudiness=seq(min(df$cloudiness), max(df$cloudiness), length.out=100)) %>%
          mutate(  wind = mean(df$wind),
                   pressure = mean(df$pressure),
                   temp = mean(df$temp),
                   x = cloudiness,
                   variable = "Cloudiness")) %>%
  rbind(expand.grid(loc=c("AL", "HW"), 
                    day=c("Sunday", "Monday"),
                    wind=seq(min(df$wind), max(df$wind), length.out=100)) %>%
          mutate(pressure = mean(df$pressure),
                 temp = mean(df$temp),
                 cloudiness = mean(df$cloudiness),
                 x = wind,
                 variable = "Wind speed"))
bm1.4 <- bootMer(m1.4, 
                 function(m){predict(m, newdata = nd1.4, re.form = NA)}, 
                 nsim = 500, 
                 .progress = "txt")
nd1.4 <- nd1.4 %>%
  mutate(
    y = bm1.4$t0,
    lwr = apply(bm1.4$t, 2, quantile, 0.025),
    upr = apply(bm1.4$t, 2, quantile, 0.975),
    loc = factor(loc, levels = c("AL", "HW"), labels = c("Agricultural landscape", "Highway"))
  )
ggplot(nd1.4, aes(x=x, y=y)) +
  geom_line(aes(color=loc)) +
  geom_ribbon(aes(ymin=lwr, ymax=upr, fill=loc), alpha=.3) +
  scale_color_manual(values = c("chartreuse4", "coral4")) +
  scale_fill_manual(values = c("chartreuse4", "coral4")) +
  facet_grid(day~variable, scales = "free_x") +
  labs(y = "Onset of singing (mins rel. to sunrise)",
       x = "hPa | intensity | °C | m/s",
       color="", fill="") +
  theme_bw() +
  theme(legend.position = "top")
ggsave("effects_m1.4.png", dpi=300, height = 16, width = 18, units = "cm")

save(m1.1, m1.2, m1.3, m1.4, file = "models_m.1.x.RData")
