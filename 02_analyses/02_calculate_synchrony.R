library(tidyverse)
library(here)
library(igraph)

source(here("02_analyses", "functions.R"))


# clean data --------------------------------------------------------------

tree <- read_rds(here("01_data", "tree_cores_char.rds"))
clim <- read_rds(here("01_data", "clim_d_data.rds"))
forchar <- read_csv(here("01_data", "for_char.csv"))
tree_loc <- read_csv(here("01_data", "tree_info.csv"))

idt <- unique(tree$tree_cored)
tree.bai <- list()
for (j in 1:length(idt)) {
  tj <- tree[tree$tree_cored == idt[j], ]
  tj <- tj[order(tj$year),]
  for (i in nrow(tj):1) {
    tj$diametro[i] <- tj$d[1] - sum(tj$distance[nrow(tj):i]) * 2
    tj$area[i] <- (tj$diametro[i] / 2) ^ 2 * pi
  }
  for (i in nrow(tj):2) {
    tj$bai[i] <- tj$area[i] - tj$area[i - 1]
  }
  tj$year <- tj$year - 1
  tree.bai[[j]] <- tj
}

year.ini <- sapply(tree.bai, function(x)
  min(x$year))
sum(year.ini <= 1970 & year.ini >= 1600)
tree.bai <- tree.bai[year.ini <= 1970 & year.ini >= 1600]
tree.bai <-
  lapply(tree.bai, function(x)
    x[-1, c("year", "tree_cored", "bai", "tree_typo", "site", "diametro")])
tree.bai <- lapply(tree.bai , function(x)
  x[x$year >= 1970, ])
sapply(tree.bai, function(x)
  min(x$year))
# sapply(tree.bai, function(x)cor(x$distance, x$bai, use = "na.or.complete"))
tree.bai <- do.call(rbind, tree.bai)

write_rds(tree.bai, here("01_data", "tree_bai.rds"))


# tree growth correlations ---------------------------------------------------------

length.window <- 20
move.window <- 1
p.threshold <- 0.05

ids <- unique(tree.bai$site)
res.net <- list()
for (j in 1:length(ids)) {
  sj <-
    as.data.frame(tree.bai[tree.bai$site == ids[j], c("year", "tree_cored", "bai")])
  cor.mat <- cor.growth(sj, length.window, move.window)
  res.net[[j]] <-
    net.properties(cor.mat, p.threshold, length.window)
  print(rep(j, 10))
}

connec <- lapply(res.net, function(x)
  x$pos[, c("year", "C")])

for(i in 1:length(res.net)) {
  connec[[i]]$site <- ids[i]
  connec[[i]]$type <-
    as.vector(unique(tree.bai[tree.bai$site == ids[i], "tree_typo"]$tree_typo))
}


# climate -----------------------------------------------------------------

clim <- clim[clim$year >= 1970 & clim$year <= 2019,]
idy <- unique(clim$year)
clima <- NULL
for (i in 1:length(idy)) {
  yi <- clim[clim$year == idy[i],]
  tminpri <- mean(yi[yi$yday %in% 60:150, "Tmin"]$Tmin)
  tmaxver <- mean(yi[yi$yday %in% 151:242, "Tmax"]$Tmax)
  nminpri <- sum(yi[yi$yday %in% 60:150, "Tmin"] < 0)
  nmaxver <- sum(yi[yi$yday %in% 151:242, "Tmax"] > 32)
  tmeananu <- mean(yi$Tmean)
  panul <- sum(yi$Prcp)

  clima <-
    rbind(clima,
          c(idy[i], tminpri, tmaxver, nminpri, nmaxver, tmeananu, panul))
}

colnames(clima) <-
  c("year", "tmin", "tmax", "frost", "heat", "tmean", "prec")
winclima <- var.mwin(clima, length.window, move.window)
conclima <- lapply(connec, function(x)
  cbind(x, winclima[, -1]))


# diameter changes --------------------------------------------------------
# mean
ids <- unique(tree.bai$site)
for (j in 1:length(ids)) {
  sj <-
    tree.bai[tree.bai$site == ids[j], c("year", "tree_cored", "diametro")]

  dj <- tapply(sj$diametro, sj$year, mean)
  windj <-
    var.mwin(cbind(as.numeric(rownames(dj)), dj), length.window, move.window)
  conclima[[j]]$meand <- windj[, -1]
}

# cv
for (j in 1:length(ids)) {
  sj <-
    tree.bai[tree.bai$site == ids[j], c("year", "tree_cored", "diametro")]

  dj <- tapply(sj$diametro, sj$year, function(x)
    sd(x) / mean(x))
  windj <-
    var.mwin(cbind(as.numeric(rownames(dj)), dj), length.window, move.window)
  conclima[[j]]$cvd <- windj[, -1]
}


# BAI changes -------------------------------------------------------------

ids <- unique(tree.bai$site)
for (j in 1:length(ids)) {
  sj <-
    tree.bai[tree.bai$site == ids[j], c("year", "tree_cored", "bai")]
  baij <-
    do.call(rbind, tapply(sj$bai, sj$year, function(x)
      c(sum(x), mean(x))))
  winbaij <- var.mwin(cbind(as.numeric(rownames(baij)), baij),
                      length.window, move.window)
  colnames(winbaij) <- c("", "sumbai", "meanbai")
  conclima[[j]] <- cbind(conclima[[j]], winbaij[, -1])
}

conclima_df <- as_tibble(do.call(rbind, conclima))

names(conclima_df)

write_rds(conclima_df, here("01_data", "conclima.rds"))


# for plotting ------------------------------------------------------------

names(res.net[[1]])
lapply(res.net, function(x)
  max(x$pos[, "C"]))
ids[3]
harrinet <- res.net[[3]]
harrinet$nets.pos[[9]]
harrinet$nets.pos[[19]]
res.net[[3]]$pos

el9 <- as.matrix(harrinet$nets.pos[[9]][, 1:2])
el19 <- as.matrix(harrinet$nets.pos[[19]][, 1:2])
el19 <- (el19 - min(el19)) + 1
el9 <- (el9 - min(el9)) + 1

net9 <- graph_from_edgelist(el9, directed = F)
net19 <- graph_from_edgelist(el19, directed = F)

jpeg(
  filename = here("03_results", "networks_dense.jpg"),
  width = 480,
  height = 480
)

plot(
  net19,
  layout = layout_in_circle(net19),
  vertex.color = "darkgreen",
  vertex.label = "",
  vertex.size = 15,
  edge.color = "black"
)

dev.off()

jpeg(filename = here("03_results", "networks_nodense.jpg"))

plot(
  net9,
  layout = layout_in_circle(net19),
  vertex.color = "darkgreen",
  vertex.label = "",
  vertex.size = 15,
  edge.color = "black"
)

dev.off()
