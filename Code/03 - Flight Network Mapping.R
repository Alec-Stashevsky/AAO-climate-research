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
conventions <- readRDS(paste0(path.in, "AAO_CONVENTIONS.RDs"))


# Plot Attendance Map -----------------------------------------------------

pdf(
  file = paste0(path.viz, "AAO Flight Networks All.pdf"),
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


# Build Flight Network Plotter -------------------------------------------
flight_network <- function(
  data,
  destination,
  lgd.col,
  main = "",
  sub = "",
  point.col = rgb(red = 1, green = .69, blue = 0, alpha = 0.3),
  edge.col = edge.pal(100),
  edge.ind.adj = 50,
  lwd.adj = 200,
  title.adj = 4,
  rows = "default",
  endpts = TRUE) {

  if (rows == "default") {
    rows <- c(1, nrow(data))
  }  else if (length(rows) != 2 | !is.numeric(rows)) {
    stop("`rows` must be a numeric vector of length 2")
  }

  # Plot a map of the united states
  map("world", col = "grey20", fill = TRUE, bg = "black", lwd = 0.1)

  # Add title
  title(
    main = main,
    sub = sub,
    col.main = "white", col.sub = "white"
  )

  # Plot origin points, don't include drivers as points to remove clutter
  points(
    x = data$Longitude,
    y = data$Latitude,
    pch = 20,
    cex = log(data$Frequency) / (max(log(data$Frequency)) * 2),
    col = point.col
  )

  # Plot Geodesic Arcs
  for (i in rows[1]:rows[2]) {

    # Build geodesic distance to destination
    routes <- gcIntermediate(
      c(data$Longitude[i], data$Latitude[i]),
      conventions[IATA == destination, .(Longitude, Latitude)],
      100,
      addStartEnd = endpts,
      breakAtDateLine = TRUE,
      sp = TRUE
      )

    edge.ind <- round(edge.ind.adj * data$Frequency[i] / max(data$Frequency[i]))

    lines(routes, col = edge.col[edge.ind], lwd = edge.ind / lwd.adj)

  }

  # Add Legend
  legend("bottomleft",
    legend = conventions[IATA == destination]$`Airport City`,
    col = lgd.col,
    pch = 19,
    bty = "n",
    pt.bg = "white",
    pt.cex = 1.4,
    cex = 0.8,
    text.col = "white",
    ncol = 1,
    title.adj = title.adj,
    title = "Conference Location")
}



# Build Flight Network Plots ----------------------------------------------

# Parameterize color of lines
plot.cols1 <- viridis(nrow(conventions), alpha = 0.30, begin = 0.35)
plot.cols2 <- viridis(nrow(conventions), alpha = 0.35, begin = 0.4)

for (i in 1:nrow(conventions)) {

  edge.pal <- colorRampPalette(c(plot.cols1[i], plot.cols2[i]), alpha = TRUE)
  edge.col <- edge.pal(100)

  flight_network(
    data = aao[get(paste0("drive.", conventions$IATA[i])) == 0], # exclude drivers
    destination = conventions$IATA[i],
    main = paste("AAO", conventions$`Airport City`[i], "Meeting Flight Network"),
    point.col = plot.cols1[i],
    edge.col = edge.col,
    lgd.col = plot.cols1[i]
    )

}

# San Francisco Final Map
col.SFO1 <- adjustcolor("orange red", alpha=0.12)
col.SFO2 <- adjustcolor("orange", alpha=0.3)
edge.pal.SFO <- colorRampPalette(c(col.SFO1, col.SFO2), alpha = TRUE)
edge.col.SFO <- edge.pal.SFO(100)

flight_network(
  data = aao,
  destination = "SFO",
  main = paste("AAO", conventions[IATA == "SFO"]$`Airport City`, "Meeting Flight Network Final"),
  point.col = col.SFO2,
  edge.col = edge.col.SFO,
  lgd.col = "#Cf7000",
  title.adj = 1
)

# Chicago Final Map
col.ORD1 <- adjustcolor("dodgerblue2", alpha=0.2)
col.ORD2 <- adjustcolor("cyan", alpha=0.18)
edge.pal.ORD <- colorRampPalette(c(col.ORD1, col.ORD2), alpha = TRUE)
edge.col.ORD <- edge.pal.ORD(100)

flight_network(
  data = aao,
  destination = "ORD",
  main = paste("AAO", conventions[IATA == "ORD"]$`Airport City`, "Meeting Flight Network Final"),
  point.col = col.ORD2,
  edge.col = edge.col.ORD,
  lgd.col = "#0ebcf1"
)


# Export ------------------------------------------------------------------
dev.off()
