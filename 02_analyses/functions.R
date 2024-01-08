cor.growth <- function(x, length.window, move.window) {
  # function to calculate growth correlations between all tree pairs in specific time moving windows
  # x data with three columns: year, tree id, growth
  # output list names = initial year in window
  year.1 <- win.i <- min(x[, 1])
  year.2 <- max(x[, 1])
  res <- list()
  while (win.i <= (year.2 - length.window)) {
    win.n <- win.i:(win.i + length.window - 1)
    x.i <- x[x[, 1] %in% win.n, ]
    id.tree.i <- combn(unique(x.i[, 2]), 2, simplify = F)
    for (j in 1:length(id.tree.i)) {
      da.j <- merge(x.i[x.i[, 2] == id.tree.i[[j]][1],],
                    x.i[x.i[, 2] == id.tree.i[[j]][2],], by = 1, all = F)
      if (nrow(da.j) > 2) {
        ct <- cor.test(da.j[, 3], da.j[, 5], method = "spearman")
        id.tree.i[[j]][3] <- ct$estimate
        id.tree.i[[j]][4] <- ct$p.value
        id.tree.i[[j]][5] <- nrow(da.j)
      } else{
        id.tree.i[[j]][3] <- NA
        id.tree.i[[j]][4] <- NA
        id.tree.i[[j]][5] <- nrow(da.j)
      }
    }
    res[[length(res) + 1]] <- do.call(rbind, id.tree.i)
    names(res)[length(res)] <- win.i
    print(length(res))
    win.i <- win.i + move.window
  }

  lapply(res, function(x) {
    colnames(x) <- c("id.1", "id.2", "cor", "p.value", "N")
    data.frame(x)
  })
}

net.properties <- function(nets, p.value, length.window) {
  old.trees <- unique(c(nets[[1]][, 1], nets[[1]][, 2]))
  properties.pos <-
    properties.neg <-
    data.frame(
      Nnodes = NA,
      Nnodes.con = NA,
      Nlinks = NA,
      Nnodes.old = rep(length(old.trees), length(nets)),
      Nlinks.old = NA
    )
  nets.pos <- nets.neg <- list()
  for (j in 1:length(nets)) {
    net <- nets[[j]]
    net[, 3:5] <- apply(net[, 3:5], 2, as.numeric)
    net <- net [net[, "N"] == length.window, ]
    properties.pos[j, "Nnodes"] <-
      properties.neg[j, "Nnodes"] <- length(unique(c(net[, 1], net[, 2])))
    nets.pos[[j]] <-
      net.pos <- net[net[, "cor"] > 0 & net[, "p.value"] <= p.value, ]
    nets.neg[[j]] <-
      net.neg <- net[net[, "cor"] < 0 & net[, "p.value"] <= p.value, ]
    properties.pos[j, "Nnodes.con"] <-
      length(unique(c(net.pos[, 1], net.pos[, 2])))
    properties.neg[j, "Nnodes.con"] <-
      length(unique(c(net.neg[, 1], net.neg[, 2])))
    properties.pos[j, "Nlinks"] <- nrow(net.pos)
    properties.neg[j, "Nlinks"] <- nrow(net.neg)

    # old trees
    net <- net [net[, 1] %in% old.trees & net[, 2] %in% old.trees, ]
    net.pos <- net[net[, "cor"] > 0 & net[, "p.value"] <= p.value, ]
    net.neg <- net[net[, "cor"] < 0 & net[, "p.value"] <= p.value, ]
    # properties.pos[j, "Nnodes.old"] <- length(unique(c(net.pos[,1],net.pos[,2])))
    # properties.neg[j, "Nnodes.old"] <- length(unique(c(net.neg[,1],net.neg[,2])))
    properties.pos[j, "Nlinks.old"] <- nrow(net.pos)
    properties.neg[j, "Nlinks.old"] <- nrow(net.neg)
  }
  mc.pos <-
    lm(log(properties.pos$Nlinks + 1) ~ log(properties.pos$Nnodes + 1))
  properties.pos[, "resC"] <- resid(mc.pos)
  mc.neg <-
    lm(log(properties.neg$Nlinks + 1) ~ log(properties.neg$Nnodes + 1))
  properties.neg[, "resC"] <- resid(mc.neg)
  properties.pos[, "C"] <-
    properties.pos[, "Nlinks"] / choose(properties.pos[, "Nnodes"], 2)
  properties.neg[, "C"] <-
    properties.neg[, "Nlinks"] / choose(properties.neg[, "Nnodes"], 2)
  properties.pos[, "year"] <-
    properties.neg[, "year"] <- as.numeric(names(nets))
  res <-
    list(
      summary(mc.neg),
      summary(mc.pos),
      nets.neg,
      nets.pos,
      properties.neg,
      properties.pos
    )
  names(res) <-
    c("summary resC neg",
      "summary resC pos",
      "nets.neg",
      "nets.pos",
      "neg",
      "pos")
  res
}

var.mwin <- function(x, length.window, move.window) {
  # x data with N columns being the 1st the year
  year.1 <- win.i <- min(x[, 1])
  year.2 <- max(x[, 1])
  mean.var <- ini.year <- c()
  while (win.i <= (year.2 - length.window)) {
    win.n <- win.i:(win.i + length.window - 1)
    mean.var[[length(mean.var) + 1]] <-
      colMeans(x[x[, 1] %in% win.n, ], na.rm = T)
    ini.year[length(ini.year) + 1] <- win.i
    print(length(mean.var))
    win.i <- win.i + move.window
  }
  res <- do.call(rbind, mean.var)
  res[, 1] <- ini.year
  res
}

var.swin <- function(x, length.window, move.window) {
  # x data with N columns being the 1st the year
  year.1 <- win.i <- min(x[, 1])
  year.2 <- max(x[, 1])
  mean.var <- ini.year <- c()
  while (win.i <= (year.2 - length.window)) {
    win.n <- win.i:(win.i + length.window - 1)
    mean.var[[length(mean.var) + 1]] <-
      apply(x[x[, 1] %in% win.n, ], 2, sum, na.rm = T)
    ini.year[length(ini.year) + 1] <- win.i
    print(length(mean.var))
    win.i <- win.i + move.window
  }
  res <- do.call(rbind, mean.var)
  res[, 1] <- ini.year
  res
}
