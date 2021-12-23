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

path.in <- "~/AAO-climate-research/Data/"
path.out <- "~/AAO-climate-research/Output/"


# Import ------------------------------------------------------------------
aao <- readRDS(paste0(path.in, "AAO_EMISSIONS.RDs"))
# conventions <- readRDS(paste0(path.in, "AAO_CONVENTIONS.RDs"))


# Facility Location Algorithm ---------------------------------------------

# Latitudes range from [-90, 90] and longitudes ranges from [-180, 180]. Here we create an algorithm to identify the optimal conference location based on the AAO Annual Meetings geographic makeup of attendees.

# Sample 30000 coordinates for initial whole-earth grid
## This number can be increased to augment accuracy
coordinate.sample <- as.data.table(
  cbind(
    lon = round(runif(30000, -180, 180), 8),
    lat = round(runif(30000, -90, 90), 8)
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
coordinate.rank <- arrange(
  cbind(
    coordinate.sample,
    "Aggregate Distance" = distance.log),
  distance.log
  )

coordinate.rank[, Rank := seq_len(length(coordinate.sample))]

# Create grid from 10 coordinates with the smallest Aggregate Distance
grid.nyc <- as.matrix(rbind(
  c(min(coordinate.rank$lon[1:10]),
    min(coordinate.rank$lat[1:10])),
  c(max(coordinate.rank$lon[1:10]),
    max(coordinate.rank$lat[1:10]))))


# Plot Region of Optimal Location -----------------------------------------


