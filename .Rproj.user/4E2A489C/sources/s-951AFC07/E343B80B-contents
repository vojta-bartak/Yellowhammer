library(lme4)
library(car)
library(tidyverse)
library(MuMIn)
library(lattice)
library(cowplot)

df <- read.table("data_yellowhammer.csv", sep=";", header=T)
load("models_m.1.x.RData")
load("models_m.2.x.RData")
load("models_m.3.x.RData")

# anova tables -----------------------------------------------------------------------------------------------------------------------
list(m1.1,m1.2,m1.3,m1.4) %>%
  lapply(Anova) %>%
  lapply(round, 4) %>%
  lapply(rownames_to_column) %>%
  lapply(as.data.frame) %>%
  reduce(full_join, by="rowname") %>%
  write.table("anova.tables.1.csv", row.names = F, sep=",")
list(m2.1,m2.2,m2.3,m2.4) %>%
  lapply(Anova) %>%
  lapply(round, 4) %>%
  lapply(rownames_to_column) %>%
  lapply(as.data.frame) %>%
  reduce(full_join, by="rowname") %>%
  write.table("anova.tables.2.csv", row.names = F, sep=",")
list(m3.1,m3.2,m3.3,m3.4) %>%
  lapply(Anova) %>%
  lapply(round, 4) %>%
  lapply(rownames_to_column) %>%
  lapply(as.data.frame) %>%
  reduce(full_join, by="rowname") %>%
  write.table("anova.tables.3.csv", row.names = F, sep=",")
list(m3.1,m3.2,m3.3,m3.4) %>% lapply(r.squaredGLMM)

# table of coefficients --------------------------------------------------------------------------------------------------------------
list(m1.1,m1.2,m1.3,m1.4) %>%
  lapply(summary) %>%
  lapply(coef) %>%
  lapply(function(d) round(d, 3)) %>%
  lapply(as.data.frame) %>%
  lapply(rownames_to_column) %>%
  reduce(full_join, by="rowname") %>%
  write.table("coef.tab.1.csv", row.names = F, sep = ",")
list(m2.1,m2.2,m2.3,m2.4) %>%
  lapply(summary) %>%
  lapply(coef) %>%
  lapply(function(d) round(d, 3)) %>%
  lapply(as.data.frame) %>%
  lapply(rownames_to_column) %>%
  reduce(full_join, by="rowname") %>%
  write.table("coef.tab.2.csv", row.names = F, sep = ",")
list(m3.1,m3.2,m3.3,m3.4) %>%
  lapply(summary) %>%
  lapply(coef) %>%
  lapply(function(d) round(d, 3)) %>%
  lapply(as.data.frame) %>%
  lapply(rownames_to_column) %>%
  reduce(full_join, by="rowname") %>%
  write.table("coef.tab.3.csv", row.names = F, sep = ",")

# specific values of total chorus length
expand.grid(day=factor(c("Monday"), levels=c("Monday", "Sunday")), 
                  loc=c("AL", "HW"),
                  pressure=c(1005, 1010, 1015, 1020),
                  wind = c(0,5,10)) %>%
  mutate(
    cloudiness = mean(df$cloudiness)
  ) %>%
  mutate(y = predict(m1.1, newdata=., re.form=NA)) %>%
  View

