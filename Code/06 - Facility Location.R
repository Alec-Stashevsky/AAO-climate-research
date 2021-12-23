# Step 06 - Facility Location
# Alec Stashevsky
# December 22, 2021


# Setup -------------------------------------------------------------------
rm(list = ls())
library(tidyverse)
library(data.table)
library(geosphere)
library(maps)
library(sp)

set.seed(32) # Needed to keep polygon consistent

path.in <- "~/AAO-climate-research/Data/"
path.out <- "~/AAO-climate-research/Output/"
path.viz <- "~/AAO-climate-research/Visualizations/"


# Import ------------------------------------------------------------------
aao <- readRDS(paste0(path.in, "AAO_EMISSIONS.RDs"))


# Facility Location Algorithm ---------------------------------------------

# Sample 50,000 coordinates for whole-earth grid, can be increased to be more precise
coordinate.sample <- as.data.table(
  cbind(
    lon = round(runif(50000, -180, 180), 8),
    lat = round(runif(50000, -90, 90), 8)
    )
  )

# Calculate total geodesic distance for each coordinate in sample
distance.log <- rep(NA, nrow(coordinate.sample))

for (i in 1:nrow(coordinate.sample)) {

  distance.log[i] <- sum(

    # Weight distance by frequency
    aao$Frequency * distGeo(
      aao[, .(Longitude, Latitude)],
      c(coordinate.sample$lon[i], coordinate.sample$lat[i])
      )
    )
}

# Sort by smallest aggregate distance
coordinate.rank <- cbind(
  arrange(
    cbind(
      coordinate.sample,
      "Aggregate Distance" = distance.log),
    distance.log
    ),
  "Rank" = 1:nrow(coordinate.sample)
)

# Create grid from 10 coordinates with the smallest Aggregate Distance
grid <- as.matrix(rbind(
  c(min(coordinate.rank$lon[1:30]),
    min(coordinate.rank$lat[1:30])),
  c(max(coordinate.rank$lon[1:30]),
    max(coordinate.rank$lat[1:30]))))


# Plot Region of Optimal Location -----------------------------------------
pdf(
  file = paste0(path.viz, "AAO Facility Location Results.pdf"),
  onefile = TRUE,
  width = 6,
  height = 8
)

# Plot 15 best sample coordinates to create region of confidence
map("state", col = "grey20", fill = TRUE, bg = "black", lwd = 0.1
  ,
  xlim = c(grid[1,1] - 3, grid[2,1] + 4),
  ylim = c(grid[1,2] - 3, grid[2,2] + 4)
  )

points(
  x = coordinate.rank$lon[1:3],
  y = coordinate.rank$lat[1:3],
  col = "orange red",
  pch = 10, cex = 1
  )

text(
  x = coordinate.rank$lon[1:3],
  y = coordinate.rank$lat[1:3],
  labels = coordinate.rank$Rank[1:3],
  col = "white",
  cex = 0.65,
  pos = 1
  )

# Isolate maximum bounding polygon with best 15 approximations
target <- c(10, 5, 8, 9, 6)

poly.ordering <- as.matrix(
  rbind(
    coordinate.rank[target[1]],
    coordinate.rank[target[2]],
    coordinate.rank[target[3]],
    coordinate.rank[target[4]],
    coordinate.rank[target[5]]
    )
  )

# Identify the centroid/geometric median/center of mass
region <- makePoly(poly.ordering[,1:2], sp = TRUE)
center <- centroid(region)

# Draw polygon
polygon(
  poly.ordering,
  col = rgb(red = 1, green = 140/255, blue = 0, alpha = 0.25),
  border = "orange"
  )

# Add centroid
points(x = center[1], y= center[2], col = "gold", pch = 3)

# Add plot aesthetics
title(main = "AAO Meeting Optimal Location Region", col.main = "white")


# Export ------------------------------------------------------------------
dev.off()
