library(tidyverse)
library(here)
library(RColorBrewer)
library(patchwork)
library(ggdist)
library(sf)
library(ggmap)
library(ggspatial)
library(magick)

tree <- read_rds(here("01_data", "tree_cores_char.rds"))
forchar <- read_csv(here("01_data", "for_char.csv"))
tree_loc <- read_csv(here("01_data", "tree_info.csv"))
conclima <- read_rds(here("01_data", "conclima.rds"))
tree_bai <- read_rds(here("01_data", "tree_bai.rds"))
mc4 <- read_rds(here("01_data", "model_sync.rds"))


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


# figure 1 ----------------------------------------------------------------

## study area --------------------------------------------------------------

coord <- tree_loc |>
  filter(
    localidad == 159 |
      localidad == 168 |
      localidad == 232 |
      localidad == 130 |
      localidad == 212 |
      localidad == 185 |
      localidad == 100 |
      localidad == 35 |
      localidad == 65 |
      localidad == 60 |
      localidad == 14 |
      localidad == 105
  )

coord <- coord |>
  separate(lonlat, c("lat", "lon"), " ") |>
  relocate(lon, lat)

coordsf <- coord |>
  st_as_sf(
    coords = c("lon", "lat"), crs = 4326
  )

ona <- c(
  left = -2.45,
  bottom = 42.93,
  right = -2.34,
  top = 42.99
)

map_base <- get_stamenmap(
  ona,
  color = "color",
  maptype = "terrain",
  zoom = 12
)

map_base_attributes <- attributes(map_base)

map_base_transparent <- matrix(adjustcolor(map_base,
                                           alpha.f = 0.8),
                               nrow = nrow(map_base))

attributes(map_base_transparent) <- map_base_attributes

# fixed coordinates
coord <- coordsf |>
  ungroup() |>
  mutate(lon = unlist(map(coordsf$geometry, 1)),
         lat = unlist(map(coordsf$geometry, 2))) |>
  as_tibble() |>
  dplyr::select(!geometry) |>
  mutate(f.use = fct_relevel(
    f.use, c("pollard_prun", "pollard_no_prun", "rec_esta", "long_esta"))
  )

map_onati <- ggmap(map_base_transparent) +
  geom_point(aes(
    x = lon,
    y = lat,
    shape = f.use,
    color = f.use
  ),
  data = coord,
  size = 3) +
  scale_color_manual(
    values = my_colors,
    name = "Land- and forest-use legacies",
    labels =
      c(
        "pollard_prun" = "Recently-pruned pollards",
        "pollard_no_prun" = "Old-pruned pollards",
        "rec_esta" = "Recently-established",
        "long_esta" = "Long-established"
      )
  ) +
  scale_shape_manual(
    values = 15:18,
    name = "Land- and forest-use legacies",
    labels =
      c(
        "pollard_prun" = "Recently-pruned pollards",
        "pollard_no_prun" = "Old-pruned pollards",
        "rec_esta" = "Recently-established",
        "long_esta" = "Long-established"
      )
  ) +
  labs(x = "", y = "") +
  theme(
    legend.position = "right",
    axis.line.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),
    legend.text = element_text(size = 12),
    axis.text = element_text(colour = "grey50", size = 8),
  ) +
  annotation_north_arrow(location = "tl", style = north_arrow_nautical()) +
  ggsn::scalebar(
    x.min = -2.36,
    x.max = -2.35,
    y.min = 42.936,
    y.max = 42.95,
    dist = 1.5,
    transform = TRUE,
    dist_unit = "km",
    model = "WGS84",
    height = 0.10,
    st.dist = 0.2,
    st.size = 4
  )

ggsave(plot = map_onati,
       here("03_results", "fig1.jpg"),
       width = 20, height = 15, units = "cm")

## location map ------------------------------------------------------------

map_loc <- map_data("world", region = c("Portugal", "Spain", "France",
                                        "Switzerland", "Germany", "Andorra",
                                        "Austria", "Belgium", "UK",
                                        "Netherlands", "Albania", "Armenia",
                                        "Azerbaijan", "Belarus",
                                        "Bosnia and Herzegovina", "Bulgaria",
                                        "Croatia", "Cyprus", "Czechia",
                                        "Estonia", "Finland", "Georgia", "Greece",
                                        "Iceland", "Ireland", "Kazakhstan",
                                        "Kosovo", "Latvia", "Liechtenstein",
                                        "Lithuania", "Malta", "Moldova", "Monaco",
                                        "Montenegro", "Macedonia",
                                        "Norway", "Romania", "Russia", "San Marino",
                                        "Serbia", "Slovakia",
                                        "Slovenia", "Sweden", "Turkey",
                                        "Ukraine", "United Kingdom",
                                        "Vatican City",
                                        "Denmark", "Poland",
                                        "Italy", "Luxembourg",
                                        "Croatia", "Slovenia",
                                        "Hungary", "Slovakia",
                                        "Czech republic",
                                        "Algeria", "Egypt", "Libya",
                                        "Morocco", "Sudan", "Tunisia", "Western Sahara")) %>% # Coords WGS84
  ggplot(aes(x = long, y = lat)) +
  geom_polygon(aes(group = group),
               fill = "gray",
               color = "white") +
  coord_fixed(xlim = c(-11, 30),
              ylim = c(35, 60),
              ratio = 1.3) +
  xlab("Longitude") +
  ylab("Latitude") +
  theme(legend.title = element_text(colour = "black", face = "bold", size = 12),
        legend.position = c(0.9, 0.2),
        legend.text = element_text(size = 10),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(colour = "grey50", size = 8),
        axis.text.y = element_text(colour = "grey50", size = 8),
        axis.ticks = element_blank(),
        panel.grid.major = element_line(colour = "grey90", size = 0.5),
        axis.line = element_blank(),
        panel.background = element_blank())

map_marked <- map_loc +
  geom_point(aes(x = -2.4, y = 43.02),
             size = 1) +
  coord_fixed(xlim = c(-11, 30),  ylim = c(36.5, 57),
              ratio = 1.3) +
  theme(panel.border = element_rect(size = 1,color = "black", fill = NA))

rectangle <- data.frame(long = c(-4, -1, -1., -4, -4),
                        lat = c(42, 42, 43.8, 43.8, 42))

map_marked2 <-  map_marked +
  geom_path(data = rectangle, aes(x = long,  y = lat),
            size = 1, color = "darkred", inherit.aes = TRUE)

ggsave(
  plot = map_marked2,
  here("03_results", "map_marked.jpg"),
  width = 4, height = 2
)

# to upload it to the author center of the journal
# i need to resize the figure
fig1_pic <- image_read(here("03_results", "fig1_pic.jpg"))

fig1_pic_res <- image_resize(fig1_pic, "80%")

image_write(fig1_pic_res, here(
  "03_results", "fig1_picthanks!.jpg"
), density = 1200)


# figure 2 ----------------------------------------------------------------

conclima <- conclima |>
  group_by(type, year) |>
  mutate(mean_syn = mean(C),
         sd_syn = sd(C),
         mean_bai = mean(meanbai),
         sd_bai = sd(meanbai),
  ) |>
  group_by(year) |>
  mutate(overall_mean_syn = mean(mean_syn))

summary(conclima$mean_syn)
summary(conclima$sd_syn)
summary(conclima$mean_bai)
summary(conclima$sd_bai)

## fig 2A ------------------------------------------------------------------

gg_syn <- ggplot(conclima, aes(year, mean_syn, color = type)) +
  geom_line(size = .9) +
  geom_line(
    aes(y = overall_mean_syn),
    color = "black",
    linetype = "dashed",
    size = .9
  ) +
  scale_color_manual(
    values = my_colors,
    limits =
      c(
        "recently_pruned_pollards",
        "long_pruned_pollards",
        "recently_established",
        "long_established"
      ),
    labels =
      c(
        "recently_pruned_pollards" = "Recently-pruned pollards",
        "long_pruned_pollards" = "Old-pruned pollards",
        "recently_established" = "Recently-established",
        "long_established" = "Long-established"
      )
  ) +
  scale_y_continuous(name = "Tree growth synchrony (%)") +
  xlab("Initial year of 20-year moving windows") +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 12),
    legend.title = element_blank(),
    axis.line.y = element_line(color = "black"),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 12)
  ) +
  guides(color = guide_legend(nrow = 2))

## fig 2B ------------------------------------------------------------------

# sampled tree
tree_sam <- tree |>
  distinct(tree_cored, .keep_all = T) |>
  dplyr::select(tree_cored, d)

# 10 m radius
for_typo_dbh <- forchar |>
  filter(dead == F) |>
  group_by(tree_cored) |>
  mutate(mean_dbh = mean(dbh, na.rm = T))

for_typo_dbh_sam <-
  left_join(for_typo_dbh, tree_sam, by = "tree_cored")

gg_tree_size <-
  ggplot(for_typo_dbh_sam,
         aes(d, tree_typo, fill = tree_typo, color = tree_typo)) +
  stat_halfeye(alpha = .4,
               color = NA,
               size = 0) +
  stat_halfeye(slab_fill = NA,
               point_interval = "mean_qi") +
  scale_fill_manual(
    values = my_colors,
    limits =
      c(
        "recently_pruned_pollards",
        "long_pruned_pollards",
        "recently_established",
        "long_established"
      ),
    labels =
      c(
        "recently_pruned_pollards" = "Recently-pruned pollards",
        "long_pruned_pollards" = "Long-pruned pollards",
        "recently_established" = "Recently-established",
        "long_established" = "Long-established"
      )
  ) +
  scale_color_manual(
    values = my_colors,
    limits =
      c(
        "recently_pruned_pollards",
        "long_pruned_pollards",
        "recently_established",
        "long_established"
      ),
    labels =
      c(
        "recently_pruned_pollards" = "Recently-pruned pollards",
        "long_pruned_pollards" = "Long-pruned pollards",
        "recently_established" = "Recently-established",
        "long_established" = "Long-established"
      )
  ) +
  scale_y_discrete(limits = rev(
    c(
      "recently_pruned_pollards",
      "long_pruned_pollards",
      "recently_established",
      "long_established"
    )
  ),
  labels = rev(
    c(
      "recently_pruned_pollards" = "Recently-pruned pollards",
      "long_pruned_pollards" = "Long-pruned pollards",
      "recently_established" = "Recently-established",
      "long_established" = "Long-established"
    )
  )) +
  xlab("Diameter at breast height (cm)") +
  theme(
    legend.position = "none",
    legend.title = element_blank(),
    axis.title.y = element_blank(),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 12)
  )

## fig 2C-E ----------------------------------------------------------------

conclima_clim <- conclima |> 
  distinct(year, .keep_all = T)

normalize <- function(x) {
  ((x - min(x)) / (max(x) - min(x)))
}

prec_norm <- normalize(conclima_clim$prec)
heat_norm <- normalize(conclima_clim$heat)
frost_norm <- normalize(conclima_clim$frost)

denormalize <- function(x, minval, maxval) {
  x * (maxval - minval) + minval
}

prec_denorm <- denormalize(
  x = prec_norm,
  minval = min(conclima_clim$prec),
  maxval = max(conclima_clim$prec)
)

heat_denorm <- denormalize(
  x = heat_norm,
  minval = min(conclima_clim$heat),
  maxval = max(conclima_clim$heat)
)
frost_denorm <- denormalize(
  x = frost_norm,
  minval = min(conclima_clim$frost),
  maxval = max(conclima_clim$frost)
)

gg_prec <- ggplot(conclima_clim, aes(year, mean_syn)) +
  geom_col(
    aes(y = prec_norm, x = year),
    color = alpha("dodgerblue3", 0.4),
    fill = "dodgerblue3",
    size = 2
  ) +
  scale_y_continuous(
    sec.axis = sec_axis( ~ . * (
      max(conclima$prec) - min(conclima$prec)
    ) + min(conclima$prec),
    name = "Mean annual\nprecipitation (mm)"),
    position = "right"
  ) +
  xlab("") +
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 12),
    axis.line.y = element_line(color = "black"),
    axis.text.y.right = element_blank(),
    axis.ticks.y.right = element_blank(),
    axis.line.y.right = element_blank(),
    axis.title.y.right = element_blank()
  )

gg_heat <- ggplot(conclima_clim, aes(year, mean_syn)) +
  geom_col(
    aes(y = heat_norm, x = year),
    color = alpha("salmon", 0.4),
    fill = "salmon",
    size = 2
  ) +
  scale_y_continuous(
    sec.axis = sec_axis( ~ . * (
      max(conclima$heat) - min(conclima$heat)
    ) + min(conclima$heat),
    name = "Mean heatwaves\nfrequency"),
    position = "right"
  ) +
  xlab("") +
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 12),
    axis.line.y = element_line(color = "black"),
    axis.text.y.right = element_blank(),
    axis.ticks.y.right = element_blank(),
    axis.line.y.right = element_blank(),
    axis.title.y.right = element_blank()
  )

gg_frost <- ggplot(conclima_clim, aes(year, mean_syn)) +
  geom_col(
    aes(y = frost_norm, x = year),
    color = alpha("lightblue1", 0.4),
    fill = "lightblue1",
    size = 2
  ) +
  scale_y_continuous(
    sec.axis = sec_axis( ~ . * (
      max(conclima$frost) - min(conclima$frost)
    ) + min(conclima$frost),
    name = "Mean late spring\n frosts frequency"),
    position = "right"
  ) +
  xlab("Initial year of 20-year moving windows") +
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 12),
    axis.line.y = element_line(color = "black"),
    axis.text.y.right = element_blank(),
    axis.ticks.y.right = element_blank(),
    axis.line.y.right = element_blank(),
    axis.title.y.right = element_blank()
  )

gg_clim <- gg_prec / gg_heat / gg_frost

gg_tree <- gg_syn / gg_tree_size + theme(axis.ticks.y = element_blank(),
                                         axis.text.y = element_blank()) +
  plot_layout(heights = c(2, 1.2))

fig2_tree_clim <- gg_tree | gg_clim

fig2_tree_clim <- fig2_tree_clim +
  plot_annotation(tag_levels = "A",
                  tag_prefix = "(",
                  tag_suffix = ")")

ggsave(
  plot = fig2_tree_clim,
  filename = here("03_results", "fig2.jpg"),
  width = 35,
  height = 22,
  units = "cm",
  dpi = 1200
)


### networks ----------------------------------------------------------------

c_clim <- image_read(here("03_results", "fig2.jpg"))
net_dense <- image_read(here("03_results", "networks_dense.jpg"))
net_nodense <- image_read(here("03_results", "networks_nodense.jpg"))

image_info(net_nodense)
net_dense_c <- image_crop(net_dense, "400x400+50")
net_nodense_c <- image_crop(net_nodense, "400x400+50")

c_clim_nodense <- image_composite(
  c_clim, image_scale(net_nodense_c, "x850"),
  offset = "+3250+2250"
)

c_clim_net <- image_composite(
  c_clim_nodense, image_scale(net_dense_c, "x850"),
  offset = "+5400+000"
)

# to upload it to the author center of the journal
# i need to resize the figure
c_clim_net_res <- image_resize(c_clim_net, "40%")

image_write(c_clim_net_res, here(
  "03_results", "fig2.jpg"
), density = 1200)


# SI figures --------------------------------------------------------------

## figure s1 ------------------------------------------------------------------

new_labels <- c(
  "recently_pruned_pollards" = "Recently-pruned pollards",
  "long_pruned_pollards" = "Old-pruned pollards",
  "recently_established" = "Recently-established",
  "long_established" = "Long-established"
)

tree_bai_site <- tree_bai |> 
  group_by(site, year, tree_typo) |> 
  summarise(
    bai_mean = mean(bai),
    bai_sd = sd(bai)
  )

ggplot(tree_bai_site, aes(year, bai_mean, group = site)) +
  geom_line() +
  facet_wrap(~ tree_typo,
             labeller = labeller(tree_typo = new_labels),
             scales = "free_y") +
  scale_y_continuous(
    name = (bquote("Basal area annual increment "(cm^2)))
    ) +
  xlab("Years") +
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    axis.line.y = element_line(color = "black")
  )

ggsave(
  here("03_results", "fig_s1.jpg"),
  width = 24,
  height = 15,
  units = "cm",
  dpi = 1200
)


## figure s2 ------------------------------------------------------------------

ggplot(conclima, aes(year, C, group = site)) +
  geom_line() +
  facet_wrap(~ type,
             labeller = labeller(type = new_labels)) +
  scale_y_continuous(name = "Tree growth synchrony (%)") +
  xlab("Initial year of 20-year moving windows") +
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    axis.line.y = element_line(color = "black")
  )

ggsave(
  here("03_results", "fig_s2.jpg"),
  width = 24,
  height = 15,
  units = "cm",
  dpi = 1200
)
