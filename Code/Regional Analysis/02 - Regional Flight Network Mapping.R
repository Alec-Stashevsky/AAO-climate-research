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
dests2 <- list("SFO", "MCO", "ORD")

# Generate colors
dest.cols <- viridis(length(dests), begin = 0.6, end = 0.95, alpha = 0.38)
dest.cols.list <- as.list(viridis(length(dests), begin = 0.6, end = 0.95, alpha = 0.38))

dest.cols2 <- viridis(length(dests2), begin = 0.6, end = 0.95, alpha = 0.38)
dest.cols.list2 <- as.list(viridis(length(dests2), begin = 0.6, end = 0.95, alpha = 0.38))

# Rename list elements
for (i in 1:length(dest.cols)) {
  names(dest.cols.list)[i] <- dests[[i]]
}

for (i in 1:length(dest.cols2)) {
  names(dest.cols.list2)[i] <- dests2[[i]]
}

# Get city names for legend
lgnd.vec <- rep(NA, length(dests))
k <- 1
for (d in dests) {
  lgnd.vec[k] <- conventions[IATA == d]$`Airport City`
  k <- k + 1
}

lgnd.vec2 <- rep(NA, length(dests2))
k <- 1
for (d in dests2) {
  lgnd.vec2[k] <- conventions[IATA == d]$`Airport City`
  k <- k + 1
}

# Plot Regional Attendance ------------------------------------------------

# 2 Meetings ---------------

# Plot a map of the united states:
map("world", col="grey20", fill=TRUE, bg="black", lwd=0.1)

# Generate points, colored by AAO region
for (i in 1:length(dests)) {

  points(
    x = aao[`Regional Format 1` == dests[[i]]]$Longitude,
    y = aao[`Regional Format 1` == dests[[i]]]$Latitude,
    pch=19,
    cex = log(aao$Frequency) / (max(log(aao$Frequency)) * 2),
    col = dest.cols[i]
  )

}

# Add Legend
legend("bottomleft",
  legend = lgnd.vec,
  col = viridis(length(dests), begin = 0.6, end = 0.95, alpha = 0.9),
  pch = 19,
  bty = "n",
  pt.bg = "white",
  pt.cex = 1.4,
  cex = 0.8,
  text.col = "white",
  ncol = 1,
  title = "Conference Location")

# Add title
title(main = "Regional AAO Meeting (2 Locations)",
  col.main = "white", col.sub = "white")


# 3 Meetings ---------------

# Plot a map of the united states:
map("world", col="grey20", fill=TRUE, bg="black", lwd=0.1)

# Generate points, colored by AAO region
for (i in 1:length(dests2)) {

  points(
    x = aao[`Regional Format 2` == dests2[[i]]]$Longitude,
    y = aao[`Regional Format 2` == dests2[[i]]]$Latitude,
    pch=19,
    cex = log(aao$Frequency) / (max(log(aao$Frequency)) * 2),
    col = dest.cols2[i]
  )

}

# Add Legend
legend("bottomleft",
  legend = lgnd.vec2,
  col = viridis(length(dests2), begin = 0.6, end = 0.95, alpha = 0.9),
  pch = 19,
  bty = "n",
  pt.bg = "white",
  pt.cex = 1.4,
  cex = 0.8,
  text.col = "white",
  ncol = 1,
  title = "Conference Location")

# Add title
title(main = "Regional AAO Meeting (3 Locations)",
  col.main = "white", col.sub = "white")


# Plot Regional Flight Networks -------------------------------------------

# Define function to generate flight path arcs
flight.network <- function(
  data,
  config,
  region,
  cols,
  endpts = FALSE) {

  # Set config
  regional.format <- paste("Regional Format", config)

  # Filter data to only region
  plot.data <- data[get(regional.format) == region]

  # Set destination
  if (config == 1) {
    dest.coords <- c(
      conventions[IATA == plot.data[i]$`Regional Format 1`]$Longitude,
      conventions[IATA == plot.data[i]$`Regional Format 1`]$Latitude
      )
  } else if (config == 2) {
    dest.coords <- c(
      conventions[IATA == plot.data[i]$`Regional Format 2`]$Longitude,
      conventions[IATA == plot.data[i]$`Regional Format 2`]$Latitude
    )
  } else {
    stop("config needs to be specified as 1 or 2")
  }

  for (i in 1:nrow(plot.data)) {

    routes <- gcIntermediate(

      # Origin coords
      c(plot.data[i]$Longitude,
        plot.data[i]$Latitude),

      # Destination coords
      dest.coords,

      100,
      addStartEnd = endpts,
      breakAtDateLine = TRUE,
      sp = TRUE
    )

    if (plot.data[i]$Frequency >= quantile(plot.data$Frequency, probs = 0.95)) {
      edge.ind <- 12
    } else {
      edge.ind <- round(500 * plot.data[i]$Frequency / max(plot.data$Frequency))
    }

    lines(routes, col = alpha(cols[[region]], 0.3),
      lwd = edge.ind / 50)
  }
}

# Flight Paths for 2-Meeting Format ---------------------------------------

# Plot a map of the united states:
map("world", col="grey20", fill=TRUE, bg="black", lwd=0.1)

# Flight paths by Region
lapply(
  names(dest.cols.list),
  flight.network,
  data = aao,
  config = 1,
  cols = dest.cols.list,
  endpts = TRUE
  )

# Add Legend
legend("bottomleft",
  legend = lgnd.vec,
  col = viridis(length(dests), begin = 0.6, end = 0.95, alpha = 0.9),
  pch = 19,
  bty = "n",
  pt.bg = "white",
  pt.cex = 1.4,
  cex = 0.8,
  text.col = "white",
  ncol = 1,
  title = "Conference Location")

# Add Flight plot aesthetics
title(main = "AAO Conference Flight Network (2 Meetings)",
  col.main = "white", cex.main = 0.75)


# Flight Paths for 3-Meeting Format ---------------------------------------

# Plot a map of the united states:
map("world", col="grey20", fill=TRUE, bg="black", lwd=0.1)

# Flight paths by Region
lapply(
  names(dest.cols.list2),
  flight.network,
  data = aao,
  config = 2,
  cols = dest.cols.list2,
  endpts = TRUE
)

# Add Legend
legend("bottomleft",
  legend = lgnd.vec2,
  col = viridis(length(dests2), begin = 0.6, end = 0.95, alpha = 0.9),
  pch = 19,
  bty = "n",
  pt.bg = "white",
  pt.cex = 1.4,
  cex = 0.8,
  text.col = "white",
  ncol = 1,
  title = "Conference Location")


# Add Flight plot aesthetics
title(main = "AAO Conference Flight Network (3 Meetings)",
  col.main = "white", cex.main = 0.75)


# Export ------------------------------------------------------------------
dev.off()
