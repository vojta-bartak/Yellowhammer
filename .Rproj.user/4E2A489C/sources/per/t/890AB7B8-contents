library(tidyverse)
library(car)
library(lme4)
library(MuMIn)
library(lattice)

df <- read.table("data_yellowhammer.csv", sep=";", header=T)

# Song duration -------------------------------------------------------------------------------

# multicollinearity check
lmer(length_sum ~ vehicle+RPDI2sc+temp+wind+humidity+pressure+cloudiness+(1|code),
     data=df[df$loc=="HW",]) %>% 
  vif # humidity will be removed
lmer(length_sum ~ vehicle+RPDI2sc+temp+wind+pressure+cloudiness+(1|code),
     data=df[df$loc=="HW",]) %>% 
  vif # fine

# model selection
m3.1 <- lmer(length_sum ~ vehicle+RPDI2sc+temp+wind+pressure+cloudiness+(1|code),
             data=df[df$loc=="HW",])
Anova(m3.1) # removing pressure
m3.1 <- lmer(length_sum ~ vehicle+RPDI2sc+temp+wind+cloudiness+(1|code),
             data=df[df$loc=="HW",])
Anova(m3.1) # removing vehicle
m3.1 <- lmer(length_sum ~ RPDI2sc+temp+wind+cloudiness+(1|code),
             data=df[df$loc=="HW",])
Anova(m3.1) # removing cloudiness
m3.1 <- lmer(length_sum ~ RPDI2sc+temp+wind+(1|code), data=df[df$loc=="HW",])
Anova(m3.1) # final model

# model diagnostics and output
plot(m3.1)
qqmath(m3.1)
(s3.1 <- summary(m3.1))
r.squaredGLMM(m3.1)


# Syllabus length -----------------------------------------------------------------------------

# model selection
m3.2 <- lmer(length_mean ~ vehicle+RPDI2sc+temp+wind+pressure+cloudiness+(1|code),
             data=df[df$loc=="HW",])
Anova(m3.2) # removing RPDI
m3.2 <- lmer(length_mean ~ vehicle+temp+wind+pressure+cloudiness+(1|code),
             data=df[df$loc=="HW",])
Anova(m3.2) # final model

# model diagnostics and output
plot(m3.2)
qqmath(m3.2)
(s3.2 <- summary(m3.2))
r.squaredGLMM(m3.2)


# Number of syllables -------------------------------------------------------------------------

# model selection
m3.3 <- lmer(syll_sum ~ vehicle+RPDI2sc+temp+wind+pressure+cloudiness+(1|code),
             data=df[df$loc=="HW",])
Anova(m3.3) # removing pressure
m3.3 <- lmer(syll_sum ~ vehicle+RPDI2sc+temp+wind+cloudiness+(1|code),
             data=df[df$loc=="HW",])
Anova(m3.3) # removing vehicle
m3.3 <- lmer(syll_sum ~ RPDI2sc+temp+wind+cloudiness+(1|code), data=df[df$loc=="HW",])
Anova(m3.3) # removing cloudiness
m3.3 <- lmer(syll_sum ~ RPDI2sc+temp+wind+(1|code), data=df[df$loc=="HW",])
Anova(m3.3) # removing wind
m3.3 <- lmer(syll_sum ~ RPDI2sc+temp+(1|code), data=df[df$loc=="HW",])
Anova(m3.3) # final model

# model diagnostics and output
plot(m3.3)
qqmath(m3.3)
(s1.3 <- summary(m3.3))
r.squaredGLMM(m3.3)


# Onset of singing ----------------------------------------------------------------------------

# model selection
m3.4 <- lmer(singing_sunrise ~ vehicle+RPDI2sc+temp+wind+pressure+cloudiness+(1|code),
             data=df[df$loc=="HW",])
Anova(m3.4) # removing temp
m3.4 <- lmer(singing_sunrise ~ vehicle+RPDI2sc+pressure+wind+cloudiness+(1|code),
             data=df[df$loc=="HW",])
Anova(m3.4) # removing RPDI
m3.4 <- lmer(singing_sunrise ~ vehicle+pressure+wind+cloudiness+(1|code),
             data=df[df$loc=="HW",])
Anova(m3.4) # final model

# model diagnostics and output
plot(m3.4)
qqmath(m3.4)
(s1.4 <- summary(m3.4))
r.squaredGLMM(m3.4)


# Effect plots --------------------------------------------------------------------------------
nd3.1 <- data.frame(RPDI2sc = seq(min(df[df$loc=="HW",]$RPDI2sc), max(df[df$loc=="HW",]$RPDI2sc), l=100),
                    temp=mean(df$temp),
                    wind=mean(df$wind))
bm3.1 <- bootMer(m3.1, function(m){predict(m, newdata = nd3.1, re.form = NA)}, nsim = 500, .progress = "txt")
nd3.1 <- nd3.1 %>%
  mutate(y = bm3.1$t0,
         lwr = apply(bm3.1$t, 2, quantile, 0.025),
         upr = apply(bm3.1$t, 2, quantile, 0.975))

nd3.2 <- data.frame(vehicle = seq(min(df[df$loc=="HW",]$vehicle), max(df[df$loc=="HW",]$vehicle), l=100),
                    temp=mean(df$temp),
                    wind=mean(df$wind),
                    pressure=mean(df$pressure),
                    cloudiness=mean(df$cloudiness))
bm3.2 <- bootMer(m3.2, function(m){predict(m, newdata = nd3.2, re.form = NA)}, nsim = 500, .progress = "txt")
nd3.2 <- nd3.2 %>%
  mutate(y = bm3.2$t0,
         lwr = apply(bm3.2$t, 2, quantile, 0.025),
         upr = apply(bm3.2$t, 2, quantile, 0.975))

nd3.3 <- data.frame(RPDI2sc = seq(min(df[df$loc=="HW",]$RPDI2sc), max(df[df$loc=="HW",]$RPDI2sc), l=100),
                    temp=mean(df$temp))
bm3.3 <- bootMer(m3.3, function(m){predict(m, newdata = nd3.3, re.form = NA)}, nsim = 500, .progress = "txt")
nd3.3 <- nd3.3 %>%
  mutate(y = bm3.3$t0,
         lwr = apply(bm3.3$t, 2, quantile, 0.025),
         upr = apply(bm3.3$t, 2, quantile, 0.975))

nd3.4 <- data.frame(vehicle = seq(min(df[df$loc=="HW",]$vehicle), max(df[df$loc=="HW",]$vehicle), l=100),
                    pressure=mean(df$pressure),
                    wind=mean(df$wind),
                    cloudiness=mean(df$cloudiness))
bm3.4 <- bootMer(m3.4, function(m){predict(m, newdata = nd3.4, re.form = NA)}, nsim = 500, .progress = "txt")
nd3.4 <- nd3.4 %>%
  mutate(y = bm3.4$t0,
         lwr = apply(bm3.4$t, 2, quantile, 0.025),
         upr = apply(bm3.4$t, 2, quantile, 0.975))

plot_grid(ggplot(nd3.1, aes(x=RPDI2sc*sd(df[df$loc=="HW",]$RPDI2)+mean(df[df$loc=="HW",]$RPDI2), y=y)) +
            geom_line(color="coral4") +
            geom_ribbon(aes(ymin=lwr, ymax=upr), alpha=.3, fill="coral4") +
            labs(x="", y="Song duration (sec)") +
            theme_bw(),
          ggplot(nd3.2, aes(x=vehicle, y=y)) +
            geom_line(color="coral4") +
            geom_ribbon(aes(ymin=lwr, ymax=upr), alpha=.3, fill="coral4") +
            labs(x="", y="Syllabus length (sec)") +
            theme_bw(),
          ggplot(nd3.3, aes(x=RPDI2sc*sd(df[df$loc=="HW",]$RPDI2)+mean(df[df$loc=="HW",]$RPDI2), y=y)) +
            geom_line(color="coral4") +
            geom_ribbon(aes(ymin=lwr, ymax=upr), alpha=.3, fill="coral4") +
            labs(x="Long-term traffic intensity (cars/day)", y="Number of syllables") +
            theme_bw(),
          ggplot(nd3.4, aes(x=vehicle, y=y)) +
            geom_line(color="coral4") +
            geom_ribbon(aes(ymin=lwr, ymax=upr), alpha=.3, fill="coral4") +
            labs(x="Instant traffic intensity (cars/hour)", y="Onset of singing (mins rel. to sunrise)") +
            theme_bw(),
          ncol = 2)
ggsave("effects_m3.x.png", dpi=300, height = 16, width = 18, units = "cm")

save(m3.1, m3.2, m3.3, m3.4, file = "models_m.3.x.RData")
