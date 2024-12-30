library(tidyverse)
library(here)
library(glmmTMB)
library(performance)
library(DHARMa)
library(ggeffects)
library(RColorBrewer)
library(spdep)

theme_set(theme_minimal(base_size = 14))

my_colors <- brewer.pal(12, "Paired")[c(7:10)]


# load data ---------------------------------------------------------------

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

map(conclima_mod, class)

map(conclima, mean)

map(conclima_mod, summary)


# model (heat * cv) ---------------------------------------------------

mod_str_cc <- glmmTMB(
  C ~ meand_std + prec_std + type + 
    frost_std + heat_std * cvd_std + 
    (1 | site),
  family = beta_family(link = "logit"),
  data = conclima_mod
)

summary(mod_str_cc)


# moran test --------------------------------------------------------------

conclima_df <- as.data.frame(conclima_mod)

sim_mat <- as.matrix(dist(conclima_mod[, "year"]))

sim_mat_adj <- sim_mat - 20

sim_mat_adj[sim_mat_adj > 0] <- 0

sim_mat_adj_pos <- (sim_mat_adj * -1) / 20

diag(sim_mat_adj_pos) <- 0

w <- mat2listw(sim_mat_adj_pos, style = "W")

moran.test(resid(mod_str_cc), listw = w, alternative = "greater")

# moran.test(
#   resid(mod_str_cc_x1), listw = w, alternative = "two.sided"
# )


# eigenvector filtering ---------------------------------------------------
sim_mat_adj_pos_eg <- eigen(sim_mat_adj_pos)$vectors

dim(sim_mat_adj_pos_eg)

calculate_moran <- function(j) {
  ev <- sim_mat_adj_pos_eg[, j]
  
  model <- glmmTMB(
    C ~ meand_std + prec_std + type + 
      frost_std + heat_std * cvd_std + 
      ev +
      (1 | site),
    family = beta_family(link = "logit"),
    data = conclima_mod
  )
  
  # https://www.paulamoraga.com/book-spatial/spatial-autocorrelation.html
  # positive autocorrelation
  moran_output <- moran.test(
    resid(model), listw = w, alternative = "greater"
  )
  
  return(moran_output)
}

moran_values <- map(1:ncol(sim_mat_adj_pos_eg),
                calculate_moran, .progress = T)

write_rds(moran_values, here("01_data", "moran_values.rds"))

moran_values <- read_rds(here("01_data", "moran_values.rds"))

moran_i <- map_dbl(seq_along(moran_values), \(x)
    abs(moran_values[[x]][["estimate"]][["Moran I statistic"]]))

names(moran_i) <- seq_along(moran_i)

# lowest autocorrelation
sort(moran_i)

# add eigenvectors as predictors in the model
# that minimizes the amount of autocorrelation
conclima_mod <- data.frame(
  cbind(conclima_mod, 
        sim_mat_adj_pos_eg[, c(1, 3, 9)])
)

names(conclima_mod)

mod_str_cc_x1 <- glmmTMB(
  C ~ meand_std + prec_std + type + 
    frost_std + heat_std * cvd_std + 
    X1 +
    (1 | site),
  family = beta_family(link = "logit"),
  data = conclima_mod
)

moran.test(
  resid(mod_str_cc_x1), listw = w, alternative = "greater"
)
# moran.test(
#   resid(mod_str_cc_x1), listw = w, alternative = "two.sided"
# )

summary(mod_str_cc_x1)

r2(mod_str_cc_x1)


# model evaluation --------------------------------------------------------

model_performance(mod_str_cc_x1)

simres_mod_str_cc_x1 <- simulateResiduals(mod_str_cc_x1)
X11()
jpeg(
  filename = here("03_results", "dharma.jpg"),
  width = 1080,
  height = 480
)
par(mfrow = c(1, 2))
plot(simres_mod_str_cc_x1)
dev.off()


# visualisation---------------------------------------------------------------

# https://strengejacke.github.io/ggeffects/reference/predict_response.html
predict_str_cc_x1 <- predict_response(
  mod_str_cc_x1,
  terms = c("heat_std", "cvd_std"),
  condition = c(type = "long_established"),
  ci_level = 0.95
)

plot_predict_str_cc_x1 <- plot(
  predict_str_cc_x1,
  alpha = .15,
  jitter = F,
  colors = "circus"
  ) +
  labs(
    y = "Tree growth synchrony (%)",
    x = "Heatwaves frequency",
    colour = "Tree size\n heterogeneity"
  ) +
  ggtitle("") +
  theme(
    legend.position = "right",
    legend.text = element_text(size = 12, color = "grey20"),
    legend.title = element_text(size = 12, color = "grey20"),
    axis.line.x = element_line(color = "grey20"),
    axis.line.y = element_line(color = "grey20"),
    axis.ticks = element_line(color = "grey20"),
    axis.text = element_text(size = 12, color = "grey20"),
    axis.title = element_text(size = 12, color = "grey20")
  )

plot_predict_str_cc_x1_rev <- plot_predict_str_cc_x1 +
  scale_x_continuous(
    breaks = unique(predict_str_cc_x1[["x"]])[c(3, 8, 13, 18)],
    labels = round(unique(predict_str_cc_x1[["x"]])[c(3, 8, 13, 18)] * 
                     sd(conclima_mod$heat) +
                     mean(conclima_mod$heat))
  )

fig3 <- plot_predict_str_cc_x1_rev

ggsave(
  plot = fig3,
  here("03_results", "fig3.jpg"),
  width = 6,
  height = 4,
  dpi = 600
)

