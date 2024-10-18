library(tidyverse)
library(here)
library(RColorBrewer)
library(magick)

conclima <- read_rds(here("01_data", "conclima.rds"))
net_dense <- image_read(here("03_results", "networks_dense.jpg"))
net_nodense <- image_read(here("03_results", "networks_nodense.jpg"))


theme_set(theme_minimal(base_size = 14))
theme_update(
  panel.grid.major.x = element_blank(),
  panel.grid.minor = element_blank(),
  legend.position = "top",
  legend.title = element_text(size = 12, color = "grey20"),
  legend.text = element_text(size = 10, color = "grey50"),
  plot.title = element_text(
    size = 16,
    face = "bold",
    margin = margin(b = 15)
  ),
  plot.subtitle = element_text(size = 14, margin = margin(b = 15)),
  plot.caption = element_text(
    size = 14,
    color = "grey50",
    margin = margin(t = 25)
  ),
  plot.title.position = "plot",
  plot.caption.position = "plot",
  axis.text = element_text(size = 11),
  axis.title = element_text(size = 11),
  axis.line.x = element_line(color = "grey20"),
  axis.ticks.x = element_line(color = "grey20"),
  axis.ticks.y = element_line(color = "grey20"),
  plot.margin = margin(20, 20, 10, 20)
)

my_colors <- brewer.pal(12, "Paired")[c(7:10)]

normalize <- function(x){
  ((x - min(x)) / (max(x) - min(x)))
}

heat_norm <- normalize(conclima$heat)

heat_norm_year <- heat_norm / 12

conclima <- conclima |>
  group_by(type, year) |>
  mutate(
    mean_syn = mean(C),
    sd_syn = sd(C),
    mean_bai = mean(meanbai),
    sd_bai = sd(meanbai),
  ) |>
  group_by(year) |>
  mutate(overall_mean_syn = mean(mean_syn),
         mean_name = "Mean")

ggplot(conclima, aes(year, mean_syn,
                     color = type)) +
  geom_col(
    aes(y = heat_norm_year, x = year),
    color = alpha("salmon", 0.02),
    fill = alpha("salmon", 0.2),
    size = 2,
  ) +
  geom_line(aes(linetype = type), size = .9) +
  geom_line(aes(
    y = overall_mean_syn,
    color = mean_name,
    linetype = mean_name
  ), size = .9) +
  scale_color_manual(
    name = "Land- and forest-use legacies",
    values = c(my_colors, "black"),
    limits =
      c(
        "recently_pruned_pollards",
        "long_pruned_pollards",
        "recently_established",
        "long_established",
        "Mean"
      ),
    labels =
      c(
        "recently_pruned_pollards" = "Recently-pruned pollards",
        "long_pruned_pollards" = "Old-pruned pollards",
        "recently_established" = "Recently-established",
        "long_established" = "Long-established",
        "Mean" = "Mean"
      )
  ) +
  scale_y_continuous(
    name = "Tree growth syncrhony (%)",
    sec.axis = sec_axis(~ . * (max(conclima$heat) - min(conclima$heat)) + 
                          min(conclima$heat),
                        name = "Mean heatwaves frequency"),
    expand = c(0, 0)
  ) +
  xlab("Initial year of 20-year moving windows") +
  scale_linetype_manual(
    name = "Land- and forest-use legacies",
    values = c(rep("solid", 4), "dashed"),
    limits =
      c(
        "recently_pruned_pollards",
        "long_pruned_pollards",
        "recently_established",
        "long_established",
        "Mean"
      ),
    labels =
      c(
        "recently_pruned_pollards" = "Recently-pruned pollards",
        "long_pruned_pollards" = "Old-pruned pollards",
        "recently_established" = "Recently-established",
        "long_established" = "Long-established",
        "Mean" = "Mean"
      )
  ) +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 12),
    axis.line.y = element_line(color = "black"),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 12)
  ) +
  guides(color = guide_legend(nrow = 2))

ggsave(here("03_results", "graphical_abstract.jpg"),
       width = 20, height = 14, units = "cm",
       dpi = 1200)

# add newtorks 
abstract <- image_read(here("03_results", "graphical_abstract.jpg"))

image_info(net_nodense)
net_dense_c <- image_crop(net_dense, "400x400+50")
net_nodense_c <- image_crop(net_nodense, "400x400+50")

abstract_nodense <- image_composite(
  abstract, image_scale(net_nodense_c, "x650"),
  offset = "+100+3900"
)

abstract_net <- image_composite(
  abstract_nodense, image_scale(net_dense_c, "x650"),
  offset = "+100+500"
)

image_write(abstract_net, here(
  "03_results", "graphical_abstract_net.jpg"
), density = 1200)


