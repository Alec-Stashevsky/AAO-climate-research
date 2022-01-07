# Step 07 - Miscellaneous Visualizations
# Alec Stashevsky
# January 6, 2022


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
aao <- readRDS(paste0(path.in, "AAO_GEODISTANCE.RDs"))
conventions <- readRDS(paste0(path.in, "AAO_CONVENTIONS.RDs"))


# Meeting Location Map ----------------------------------------------------

# Initialize write to pdf
pdf(file = paste0(path.viz,"AAO Meeting Location Map.pdf"),
  width = 8, height = 6)

# Plot a map of the united states:
map("world", col = "grey20", fill = TRUE, bg = "black", lwd = 0.1)

# Generate points, colored by APA region
for (i in 1:nrow(conventions)) {

  points(
    x = conventions$Longitude[i],
    y = conventions$Latitude[i],
    pch = 3, cex = 0.2, lwd = 0.4,
    col = viridis(nrow(conventions), begin = 0.25, alpha = 1)[i]
  )

}

# Cities to plot separately to space text
custom.text <- c("LAS", "MCO")

# Labels cities
text(
  x = conventions[!(IATA %in% custom.text)]$Longitude,
  y = conventions[!(IATA %in% custom.text)]$Latitude,
  labels = conventions[!(IATA %in% custom.text)]$`Airport City`,
  col = "white", cex = 0.21, pos = 3, offset = 0.15
)

# Add Las Vegas
text(
  x = conventions[IATA == "LAS"]$Longitude,
  y = conventions[IATA == "LAS"]$Latitude,
  labels = conventions[IATA == "LAS"]$`Airport City`,
  col = "white", cex = 0.21, pos = 1, offset = 0.15
)

# Add Orlando
text(
  x = conventions[IATA == "MCO"]$Longitude,
  y = conventions[IATA == "MCO"]$Latitude,
  labels = conventions[IATA == "MCO"]$`Airport City`,
  col = "white", cex = 0.21, pos = 1, offset = 0.15
)


# Export ------------------------------------------------------------------
dev.off()
