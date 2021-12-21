# Step 03 - Flight Network Mapping
# Alec Stashevsky
# December 20, 2021


# Setup -------------------------------------------------------------------
rm(list = ls())
library(tidyverse)
library(data.table)
library(readxl)
library(openxlsx)
library(maps)
library(rgeos)
library(maptools)
library(geosphere)
library(plyr)
library(viridis)

path.in <- "~/AAO-climate-research/Data/"
path.viz <- "~/AAO-climate-research/Visualizations/"


# Import ------------------------------------------------------------------

# AAO Geo-Unique Frequency Data
aao <- readRDS(paste0(path.in, "AAO_GEODISTANCE.RDs"))


# Plot Attendance Map -----------------------------------------------------

pdf(
  file = paste0(path.viz, "AAO Flight Networks.pdf"),
  onefile = TRUE,
  width = 8,
  height = 6
  )

# Plot a map of the united states
map("world", col = "grey20", fill = TRUE, bg = "black", lwd = 0.1)

title(
  main = "AAO San Francisco Meeting Attendance Base",
  col.main = "white", col.sub = "white"
  )

# Plot attendee origins (proxy via airports locations)
points(
  x = aao$Longitude,
  y = aao$Latitude,
  pch = 20,
  cex = log(aao$Frequency) / (max(log(aao$Frequency)) * 2),
  col = rgb(red = 1, green = .69, blue = 0, alpha = 0.3)
  )


# Plot Flight Networks ----------------------------------------------------

# Get coordinates for destination airport SFO
SFO <- as.numeric(aao[`Airport Code` == "SFO", .(Longitude, Latitude)])

# Plot a map of the united states
map("world", col = "grey20", fill = TRUE, bg = "black", lwd = 0.1)

title(
  main = "AAO San Francisco Meeting Flight Network",
  col.main = "white", col.sub = "white"
)

# Plot attendee origins (proxy via airports locations)
points(
  x = aao[drive.SFO == 0]$Longitude,
  y = aao[drive.SFO == 0]$Latitude,
  pch = 20,
  cex = log(aao[drive.SFO == 0]$Frequency) / (max(log(aao[drive.SFO == 0]$Frequency)) * 2),
  col = rgb(red = 1, green = .69, blue = 0, alpha = 0.3)
)

# Parameterize color of lines
col.1 <- adjustcolor("orange red", alpha = 0.10)
col.2 <- adjustcolor("orange", alpha = 0.15)
edge.pal <- colorRampPalette(c(col.1, col.2), alpha = TRUE)
edge.col <- edge.pal(100)


flight_network <- function(data, destination, rows = "default", endpts = TRUE) {

  if (rows == "default") {
    rows <- c(1, nrow(data))
  }  else if (length(rows) != 2 | !is.numeric(rows)) {
    stop("`rows` must be a numeric vector of length 2")
  }

  for (i in rows[1]:rows[2]) {

    # Build geodesic distance to SFO Airport
    routes <- gcIntermediate(
      c(data$Longitude[i], data$Latitude[i]),
      destination,
      100,
      addStartEnd = endpts,
      breakAtDateLine = TRUE,
      sp = TRUE)

    edge.ind <- round(50 * data$Frequency[i] / max(data$Frequency[i]))

    lines(routes, col = edge.col[edge.ind], lwd = edge.ind / 200)

  }
}


flight_network(aao[drive.SFO == 0], SFO)


# Export ------------------------------------------------------------------
dev.off()
