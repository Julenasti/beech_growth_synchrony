# process core & forest characterisation data
library(tidyverse)
library(here)
library(gt)

cores <- read_rds(here("01_data", "cores.rds"))

for_char <- read_csv(here("01_data", "for_char.csv"))

# without any trees in the 10m radius
for_char |>
  filter(is.na(species))

for_char_d <- for_char |>
  drop_na(species) |>
  filter(dbh >= 7.5)

# per measured trees ------------------------------------------------------

## number of trees around each measured tree (10m radius) ----------------
for_char_d |>
  group_by(tree_cored) |>
  count()

## mean tree size and standard deviation around each measured tree ------------------------------
for_char_d |>
  group_by(tree_cored) |>
  summarise(
    dbh_mean = mean(dbh, na.rm = T),
    dbh_sd = sd(dbh, na.rm = T)
  )


# per management legacy ---------------------------------------------------------------

## number of tree species in each stand ----------------------------------
species <- for_char_d |>
  group_by(site, tree_typo) |>
  distinct(species, .keep_all = T) |>
  count() |>
  group_by(tree_typo) |>
  summarise(mean_species = round(mean(n)))

## number of dead trees in each stand ------------------------------------
# number of trees/ha
dead <- for_char_d |>
  group_by(tree_cored, tree_typo) |>
  count(dead) |>
  filter(dead == T) |>
  select(!dead) |>
  rename(n_dead = n) |> 
  group_by(tree_typo) |>
  summarise(mean_n_dead = mean(n_dead)) 

dead_ha <- dead |> 
  mutate(
    area = pi * 10 ^ 2,
    mean_n_dead_ha = round(10000 * mean_n_dead / area)
  ) |> 
  select(
    tree_typo,
    mean_n_dead_ha
  )

## sapling in each stand (dbh 7.5-12.5cm) -------------------------------
# number of trees/ha
sapling <- for_char_d |>
  filter(dbh >= 7.5 & dbh <= 12.5) |>
  group_by(tree_cored, tree_typo) |>
  count(name = "n_sapling") |> 
  group_by(tree_typo) |>
  summarise(mean_n_sapling = mean(n_sapling)) 

sapling_ha <- sapling |> 
  mutate(
    area = pi * 10 ^ 2,
    mean_n_sapling_ha = round(10000 * mean_n_sapling / area)
  ) |> 
  select(
    tree_typo,
    mean_n_sapling_ha
  )

## mean tree size and standard deviation of tree size in each stand-------------------------
tree_size <- for_char_d |>
  group_by(tree_typo) |>
  summarise(
    dbh_mean = mean(dbh, na.rm = T),
    dbh_sd = sd(dbh, na.rm = T)
  )

## number of coppice trees in each stand ---------------------------------
for_char_d |>
  group_by(tree_typo) |>
  count(same_individual) |>
  drop_na(same_individual) |>
  group_by(tree_typo) |>
  summarise(
    n = sum(n)
  )

## number of trees ---------------------------------
# number of trees/ha
individuals <- for_char_d |>
  group_by(tree_cored, tree_typo) |>
  count(name = "n_individuals") |> 
  group_by(tree_typo) |>
  summarise(mean_n_individuals = mean(n_individuals)) 

individuals_ha <- individuals |> 
  mutate(
    area = pi * 10 ^ 2,
    mean_n_individuals_ha = round(10000 * mean_n_individuals / area)
    ) |> 
  select(
    tree_typo,
    mean_n_individuals_ha
  )
  
for_char_data <- list(
  individuals_ha, tree_size, species, sapling_ha, dead_ha
  ) |>
  reduce(left_join, by = "tree_typo")

# table
table_S1 <- for_char_data |>
  mutate(
    tree_typo2 = tree_typo,
    order = case_when(
      tree_typo == "recently_pruned_pollards" ~ 1,
      tree_typo == "long_pruned_pollards" ~ 2,
      tree_typo == "recently_established" ~ 3,
      tree_typo == "long_established" ~ 4
      ),
    tree_typo = recode_factor(
      tree_typo,
      "recently_pruned_pollards" = "Recently-pruned pollards",
      "long_pruned_pollards" = "Old-pruned pollards",
      "recently_established" = "Recently-established",
      "long_established" = "Long-established"
    )
    ) |>
  arrange(order) |>
  select(!c(tree_typo2, order)) |>
  gt(
    groupname_col = "tree_typo2"
  ) |>
  fmt_number(
    columns = c(dbh_mean, dbh_sd),
    decimals = 2
  ) |>
  cols_label(
    tree_typo = "Land- and forest-use legacies",
    mean_species = "No. species",
    mean_n_dead_ha = "No. dead trees/ha",
    mean_n_sapling_ha = "No. saplings/ha",
    dbh_mean = "Mean d.b.h.",
    dbh_sd = "SD d.b.h.",
    mean_n_individuals_ha = "No. trees/ha"
  ) |>
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = list(
      cells_column_labels(gt::everything())
    )
  )  |>
  cols_align(
    align = "center",
    columns = gt::everything()
  )

table_S1

gtsave(table_S1, "table_S1.rtf",
       path = here("03_results"))


# process data ---------------------------------------------------

names(cores)

names(cores) <- gsub(".*_","", names(cores))


cores_tb <- map2_df(cores, names(cores),
                    \(x, y) mutate(x, tree_cored = y))

cores_tb <- cores_tb |>
  mutate(
    tree_cored = as.numeric(tree_cored)
  ) |>
  select(!...1)

for_char_d <- for_char |>
  distinct(
    tree_typo, site, tree_cored
  )

cores_char <- left_join(cores_tb, for_char_d, by = "tree_cored")

cores_char <- cores_char |>
  select(
    year, distance, tree_cored, tree_typo,  site
  ) |>
  mutate(
    tree_typo = if_else(is.na(tree_typo), "recently_pruned_pollards", tree_typo),
    site = if_else(is.na(site), "artaso_goi", site)
    )

get_tree_data <- function(file_tree) {
  read_csv(
    file_tree
  ) |>
    mutate(
      diam.per = perimeter / pi,
      d = if_else(is.na(diameter), diam.per, diameter),
      f.use = fct_relevel(
        f.use, c("pollard_no_prun",  "pollard_prun", "rec_esta", "long_esta")
      )
    )
}

tree_data <- get_tree_data("01_data/tree_info.csv")

tree_data_s <- tree_data |>
  mutate(
    tree_cored = gsub(".*_","", localidad),
    tree_cored = as.numeric(tree_cored)
  ) |>
  select(tree_cored, d)

tree_cores_char <- left_join(
  cores_char, tree_data_s, by = "tree_cored"
  )

write_rds(tree_cores_char, here("01_data", "tree_cores_char.rds"))

