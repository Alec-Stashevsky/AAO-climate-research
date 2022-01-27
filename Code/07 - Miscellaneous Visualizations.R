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
path.final <- "~/AAO-climate-research/Output/"

# Import ------------------------------------------------------------------

# AAO Geo-Unique Frequency Data
aao <- readRDS(paste0(path.in, "AAO_GEODISTANCE.RDs"))

# Final Emissions Output
aao.final <- readRDS(paste0(path.final, "AAO Total Emissions.RDs"))
aao.final.regional <- readRDS(paste0(path.final, "AAO Total Regional Emissions.RDs"))

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



# Cumulative Footprint and Travel Distance Plots -------------------------

# Expand data by frequency column
aao.final.expanded <- aao.final[, `:=`(
  PC.Footprint.SFO = `Total Emissions SFO (Kg)` / Frequency,
  PC.Footprint.LAS = `Total Emissions LAS (Kg)` / Frequency,
  PC.Footprint.MCO = `Total Emissions MCO (Kg)` / Frequency,
  PC.Footprint.MSY = `Total Emissions MSY (Kg)` / Frequency,
  PC.Footprint.ORD = `Total Emissions ORD (Kg)` / Frequency
  )][rep(seq(.N), Frequency), !"Frequency"]


emissions_plot <- function(
  data,
  region,
  offset = 0,
  binwidth = 600
  ){

  # Plot initial histogram
  p <- ggplot() +
    geom_histogram(
      data = data,
      aes(x = get(paste0("gdist.", region)) / 1000),
      binwidth = binwidth,
      col = "gray28"
    )

  # Build data behind plot
  p.build <- ggplot_build(p)


  #### Calculate Scaling Parameters ####

  # Round max histogram bar height to nearest thousands
  max.bar.height <- round(max(p.build[["data"]][[1]][["y"]]), digits = -3)

  # Total Emissions for region
  total.emissions <- sum(aao.final.expanded[, .(get(paste0("PC.Footprint.", region)))])

  # Scaling Parameters
  cumsum_coef <- total.emissions / max.bar.height
  sec_axis_trans <- 1 / (max.bar.height + offset)

  # Output actual plot
  ggplot() +
    geom_histogram(
      data = data,
      aes(x = get(paste0("gdist.", region)) / 1000),
      binwidth = binwidth,
      col = "gray28"
    ) +
    geom_line(
      data = data[
        order(get(paste0("gdist.", region))),
        .("CumSum" = cumsum(get(paste0("PC.Footprint.", region)) / cumsum_coef),
          "Distance" = get(paste0("gdist.", region)))
      ],
      aes(x = Distance / 1000, y = CumSum),
      col = "salmon"
    ) +
    scale_x_continuous(
      name = "One-way Travel Distance (km)",
      labels = scales::label_comma()
    ) +
    scale_y_continuous(
      name = "Number of Attendees",
      labels = scales::label_comma(),
      sec.axis = sec_axis(
        trans = ~.*sec_axis_trans,
        name = "Cummulative CO2 Emissions",
        labels = scales::label_percent())
    ) +
    ggtitle(paste0("Conference Location: ", region))

}



# Output Plots ------------------------------------------------------------
emissions_plot(aao.final.expanded, "SFO")
emissions_plot(aao.final.expanded, "LAS")
emissions_plot(aao.final.expanded, "MCO")
emissions_plot(aao.final.expanded, "MSY")
emissions_plot(aao.final.expanded, "ORD", offset = 18) # Needed to fix second y-axis labels


# OLD ----
# ggplot() +
#   geom_histogram(
#     data = aao.final.expanded,
#     aes(x = gdist.ORD/1000),
#     binwidth = 600,
#     col = "gray28"
#     ) +
#   geom_line(
#     data = aao.final.expanded[
#       order(gdist.ORD), CumSum := cumsum(PC.Footprint.ORD / 5275)
#       ],
#     aes(x = gdist.ORD/1000, y = CumSum),
#     col = "salmon"
#     ) +
#   scale_x_continuous(
#     name = "One-way Travel Distance (km)",
#     labels = scales::label_comma()
#     ) +
#   scale_y_continuous(
#     name = "Number of Attendees",
#     labels = scales::label_comma(),
#     sec.axis = sec_axis(
#       trans= ~.* (1/6018),
#       name = "Cummulative CO2 Emissions",
#       labels = scales::label_percent())
#     )





# Export ------------------------------------------------------------------
dev.off()
