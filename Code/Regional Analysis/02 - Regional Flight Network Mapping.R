# Step 02 - Regional Flight Network Mapping
# Alec Stashevsky
# January 7, 2022


# Setup -------------------------------------------------------------------
rm(list = ls())
library(tidyverse)
library(data.table)
library(maps)
library(rgeos)
library(maptools)
library(geosphere)
library(viridis)

path.in <- "~/AAO-climate-research/Data/"
path.viz <- "~/AAO-climate-research/Visualizations/"


# Import ------------------------------------------------------------------

# AAO Geo-Unique Frequency Data
aao <- readRDS(paste0(path.in, "AAO_REGIONAL_CONFIG.RDs"))
conventions <- readRDS(paste0(path.in, "AAO_CONVENTIONS.RDs"))


# Plot Setup --------------------------------------------------------------
pdf(
  file = paste0(path.viz, "AAO Regional Flight Networks.pdf"),
  onefile = TRUE,
  width = 8,
  height = 6
)

# List conference destinations
dests <- list("SFO", "ORD")

# Generate colors
dest.cols <- viridis(length(dests), begin = 0.5, end = 0.8, alpha = 0.38)
dest.cols.list <- as.list(viridis(length(dests), begin = 0.5, end = 0.8, alpha = 0.38))

# Rename list elements
for (i in 1:length(dest.cols)) {
  names(dest.cols.list)[i] <- dests[[1]][i]
}


# Plot Regional Attendance ------------------------------------------------

# Plot a map of the united states:
map("world", col="grey20", fill=TRUE, bg="black", lwd=0.1)

# Generate points, colored by APA region
for (i in 1:length(dests)) {

  points(
    x = aao[`Regional Format 1` == dests[[i]]]$Longitude,
    y = aao[`Regional Format 1` == dests[[i]]]$Latitude,
    pch=19, cex = log(aao[`Regional Format 1` == dests[[i]]]$Frequency)/50,
    col = dest.cols[i]
  )

}

# Get city names
lgnd.vec <- rep(NA, length(dests))
k <- 1
for (d in dests) {
  lgnd.vec[k] <- conventions[IATA == d]$`Airport City`
  k <- k + 1
}

# Add Legend
legend("bottomleft",
  legend = lgnd.vec,
  col = viridis(length(dests), begin = 0.5, end = 0.8, alpha = 0.9),
  pch = 19,
  bty = "n",
  pt.bg = "white",
  pt.cex = 1.1,
  cex = 0.45,
  text.col = "white",
  ncol = 2,
  title = "Regional Conference")

# Add title
title(main = "Regional AAO Meeting (2 Conference)",
  col.main = "white", col.sub = "white")

