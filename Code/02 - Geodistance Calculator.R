# Step 02 - Geodistance Calculator
# Alec Stashevsky
# December 21, 2021


# Setup -------------------------------------------------------------------
rm(list = ls())
library(tidyverse)
library(data.table)
library(geosphere)

path.in <- "~/AAO-climate-research/Data/"


# Import ------------------------------------------------------------------
aao <- readRDS(paste0(path.in, "AAO_GEO_UNIQUE.RDs"))
conventions <- readRDS(paste0(path.in, "AAO_CONVENTIONS.RDs"))


# Calculate Geodesic Distance ---------------------------------------------

# Destination coordinates
dests <- conventions[, .(Longitude, Latitude)]

# Distance in meters - with ellipsoidal method
for(i in 1:nrow(conventions)){
  assign(paste0("gdist.", conventions$IATA[i]),
    distGeo(aao[, c("Longitude", "Latitude")], dests[i]))
}

# Merge onto attendance geo-data
aao.geodistance <- cbind(aao, gdist.SFO, gdist.LAS, gdist.MCO, gdist.MSY, gdist.ORD)


# Flag Drivers ------------------------------------------------------------

# Set max driving distance
drive.threshold <- 241402 # 150 miles = 241402 meters

aao.geodistance[, `:=`(
  drive.SFO = ifelse(gdist.SFO <= drive.threshold, 1, 0),
  drive.LAS = ifelse(gdist.LAS <= drive.threshold, 1, 0),
  drive.MCO = ifelse(gdist.MCO <= drive.threshold, 1, 0),
  drive.MSY = ifelse(gdist.MSY <= drive.threshold, 1, 0),
  drive.ORD = ifelse(gdist.ORD <= drive.threshold, 1, 0)
)]


# Export ------------------------------------------------------------------
saveRDS(aao.geodistance, file = paste0(path.in, "AAO_GEODISTANCE.RDs"))
