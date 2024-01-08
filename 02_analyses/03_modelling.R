library(tidyverse)
library(here)
library(glmmTMB)
library(bbmle)
library(performance)
library(DHARMa)

conclima <- read_rds(here("01_data", "conclima.rds"))

conclima_mod <- conclima |>
  mutate(across(c(site, type), as.factor),
         across(c(meand, cvd,
                  prec, frost,
                  heat),
                \(x) as.numeric(scale(
                  x, center = TRUE, scale = TRUE
                )),
                .names = "{.col}_std"))

map(conclima_mod, \(x) class(x))

map(conclima_mod, \(x) summary(x))

mc1 <- glmmTMB(
  C ~ type + prec_std +
    frost_std + heat_std + (1 | site),
  family = beta_family(link = "logit"),
  data = conclima_mod
)

summary(mc1)

mc2 <- glmmTMB(C ~ prec_std +
                 frost_std + heat_std + (1 | site),
               family = beta_family(link = "logit"),
               data = conclima_mod)

mc3 <- glmmTMB(C ~ type + (1 | site),
               family = beta_family(link = "logit"),
               data = conclima_mod)

AICctab(mc1, mc2, mc3)
map(list(mc1, mc2, mc3), \(x) r2(x))

simres_mc1 <- simulateResiduals(mc1)
x11()
# jpeg(
#   filename = here("04_output", "dharma1.jpg"),
#   width = 680,
#   height = 480
# )
par(mfrow = c(2, 2))
plot(simres_mc1)
# dev.off()

mc4 <- glmmTMB(
  C ~ meand_std + cvd_std + prec_std +
    frost_std + heat_std + (1 | site),
  family = beta_family(link = "logit"),
  data = conclima_mod
)

mc5 <- glmmTMB(
  C ~ type + meand_std + cvd_std + prec_std +
    frost_std + heat_std + (1 | site),
  family = beta_family(link = "logit"),
  data = conclima_mod
)

AICctab(mc1, mc4, mc5)
map(list(mc1, mc4, mc5), \(x) r2(x))

summary(mc4)

simres_mc4 <- simulateResiduals(mc4)
x11()
# jpeg(
#   filename = here("04_output", "dharma2.jpg"),
#   width = 680,
#   height = 480
# )
par(mfrow = c(2, 2))
plot(simres_mc4)
# dev.off()

write_rds(mc4, here("01_data", "model_sync.rds"))
