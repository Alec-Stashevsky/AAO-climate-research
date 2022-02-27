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

# Export
dev.off()


# Cumulative Footprint / Travel Distance Plot Data Preparation ------------

# Expand data by frequency column
aao.final.expanded <- aao.final[, `:=`(
  PC.Footprint.SFO = `Total Emissions SFO (Kg)` / Frequency,
  PC.Footprint.LAS = `Total Emissions LAS (Kg)` / Frequency,
  PC.Footprint.MCO = `Total Emissions MCO (Kg)` / Frequency,
  PC.Footprint.MSY = `Total Emissions MSY (Kg)` / Frequency,
  PC.Footprint.ORD = `Total Emissions ORD (Kg)` / Frequency
  )][rep(seq(.N), Frequency), !"Frequency"]

# Coalesce travel distance in one column for plotting
aao.final.regional.expanded <- aao.final.regional[, `:=`(
  # Code as factor variables to keep legend consistent
  `Regional Format 1` = factor(`Regional Format 1`, levels = c("ORD", "SFO"), ordered = TRUE),
  `Regional Format 2` = factor(`Regional Format 2`, levels = c("MCO", "ORD", "SFO"), ordered = TRUE),

  # Per capita Footprint
  "PC.Footprint.1" = `Total Emissions Format 1 (Kg)` / Frequency,
  "PC.Footprint.2" = `Total Emissions Format 2 (Kg)` / Frequency

)][rep(seq(.N), Frequency), !"Frequency"][, `:=`(
  gdist.1 = fcase(
    `Regional Format 1` == "SFO", gdist.SFO,
    `Regional Format 1` == "ORD", gdist.ORD
    ),
  gdist.2 = fcase(
    `Regional Format 2` == "SFO", gdist.SFO,
    `Regional Format 2` == "ORD", gdist.ORD,
    `Regional Format 2` == "MCO", gdist.MCO
  )
)]


# Build Plotting Function -------------------------------------------------
emissions_plot <- function(
  data,
  region,
  offset = 0,
  binwidth = 600,
  single.meeting.bar.col = "lightblue",
  legend.position = "bottom",
  title = NULL,
  regional = FALSE
  ) {

  # Plot initial histogram
  p <- ggplot() +
    geom_histogram(
      data = data,
      aes(x = get(paste0("gdist.", region)) / 1000),
      binwidth = binwidth
    )

  # Build data behind plot
  p.build <- ggplot_build(p)

  # Set title
  if (is.null(title)) {
    title <- paste0("Conference Location: ", region)
  }

  #### Calculate Scaling Parameters ####

  # Round max histogram bar height to nearest thousands
  max.bar.height <- round(max(p.build[["data"]][[1]][["y"]]), digits = -3)

  # Total Emissions for region
  total.emissions <- sum(data[, .(get(paste0("PC.Footprint.", region)))])

  # Scaling Parameters
  cumsum_coef <- total.emissions / max.bar.height
  sec_axis_trans <- 1 / (max.bar.height + offset)

  #### Build Plots ####

  # Regional Format 1
  if (regional & region == "1") {

    # Text annotation
    label <- paste0(
      "list(",
      substr(
        round(as.numeric(data[, .(sum(get(paste0("PC.Footprint.", region))))]) / 1000),
        1,
        2
        ),
      ",",
      substr(
        round(as.numeric(data[, .(sum(get(paste0("PC.Footprint.", region))))]) / 1000),
        3,
        5
      ),
      ") ",
      expression(~tCO[2])
    )

    # Output actual plot
    ggplot() +
      geom_histogram(
        data = data,
        aes(
          x = get(paste0("gdist.", region)) / 1000,
          fill = get(paste0("Regional Format ", region))
          ),
        binwidth = binwidth,
        col = "gray28",
        alpha = 0.75
      ) +
      geom_line(
        data = data[
          order(get(paste0("gdist.", region))),
          .("CumSum" = cumsum(get(paste0("PC.Footprint.", region)) / cumsum_coef),
            "Distance" = get(paste0("gdist.", region)))
        ],
        aes(x = Distance / 1000, y = CumSum, col = "Cummulative C02 Emissions"),
        size = 1.5,
        alpha = 1
      ) +
      geom_text(
        aes(x = 15000, y = max.bar.height),
        label = label,
        parse = TRUE,
        nudge_y = 250
      ) +
      scale_x_continuous(
        name = "Distance to Conference (km)",
        labels = scales::label_comma()
      ) +
      scale_y_continuous(
        name = "Number of Attendees",
        labels = scales::label_comma(),
        breaks = seq(from = 0, to = max.bar.height, by = 1000),
        sec.axis = sec_axis(
          trans = ~.*sec_axis_trans,
          name = expression(Cummulative~tCO[2]~Emissions),
          labels = scales::label_percent())
      ) +
      scale_color_manual(
        name = " ",
        labels = expression(Cummulative~tCO[2]~Emissions),
        values = c("gray28")
      ) +
      scale_fill_manual(
        name = "Attendance at:",
        labels = c("San Francisco", "Chicago"),
        values = c("SFO" = "#22a581", "ORD" = "#dbe018")
        ) +
      guides(
        fill  = guide_legend(order = 1, title.position = "top", title.hjust = 0.5),
        color = guide_legend(order = 2, title.position = "top")
        ) +
      ggtitle(title) +
      theme_minimal() +
      theme(legend.position = legend.position)


  } else if (regional & region == "2") {

    # Text annotation
    label <- paste0(
      "list(",
      substr(
        round(as.numeric(data[, .(sum(get(paste0("PC.Footprint.", region))))]) / 1000),
        1,
        2
      ),
      ",",
      substr(
        round(as.numeric(data[, .(sum(get(paste0("PC.Footprint.", region))))]) / 1000),
        3,
        5
      ),
      ") ",
      expression(~tCO[2])
    )

    # Regional Format 2
    ggplot() +
      geom_histogram(
        data = data,
        aes(
          x = get(paste0("gdist.", region)) / 1000,
          fill = get(paste0("Regional Format ", region))
        ),
        binwidth = binwidth,
        col = "gray28",
        alpha = 0.75
      ) +
      geom_line(
        data = data[
          order(get(paste0("gdist.", region))),
          .("CumSum" = cumsum(get(paste0("PC.Footprint.", region)) / cumsum_coef),
            "Distance" = get(paste0("gdist.", region)))
        ],
        aes(x = Distance / 1000, y = CumSum, col = "Cummulative C02 Emissions"),
        size = 1.5,
        alpha = 1

      ) +
      geom_text(
        aes(x = 15000, y = max.bar.height),
        label = label,
        parse = TRUE,
        nudge_y = 250
        ) +
      scale_x_continuous(
        name = "Distance to Conference (km)",
        labels = scales::label_comma()
      ) +
      scale_y_continuous(
        name = "Number of Attendees",
        labels = scales::label_comma(),
        breaks = seq(from = 0, to = max.bar.height, by = 1000),
        sec.axis = sec_axis(
          trans = ~.*sec_axis_trans,
          name = expression(Cummulative~tCO[2]~Emissions),
          labels = scales::label_percent())
      ) +
      scale_color_manual(
        name = " ",
        labels = expression(Cummulative~tCO[2]~Emissions),
        values = c("gray28")
        ) +
      scale_fill_manual(
        name = "Attendance at:",
        labels = c("San Francisco", "Chicago", "Orlando"),
        values = c("SFO" = "#26828e", "ORD" = "#6ece58", "MCO" = "#fde725")
        ) +
      guides(
        fill  = guide_legend(order = 1, title.position = "top", title.hjust = 0.5),
        color = guide_legend(order = 2, title.position = "top", title.hjust = 0.5)
      ) +
      ggtitle(title) +
      theme_minimal() +
      theme(legend.position = legend.position)


  } else {

    # Text annotation
    label <- paste0(
      "list(",
      substr(
        round(as.numeric(data[, .(sum(get(paste0("PC.Footprint.", region))))]) / 1000),
        1,
        2
      ),
      ",",
      substr(
        round(as.numeric(data[, .(sum(get(paste0("PC.Footprint.", region))))]) / 1000),
        3,
        5
      ),
      ") ",
      expression(~tCO[2])
    )

    # Single-Meeting format
    ggplot() +
      geom_histogram(
        data = data,
        aes(x = get(paste0("gdist.", region)) / 1000, fill = region),
        binwidth = binwidth,
        col = "gray28",
        alpha = 0.55
      ) +
      geom_line(
        data = data[
          order(get(paste0("gdist.", region))),
          .("CumSum" = cumsum(get(paste0("PC.Footprint.", region)) / cumsum_coef),
            "Distance" = get(paste0("gdist.", region)))
        ],
        aes(x = Distance / 1000, y = CumSum, col = "Cummulative C02 Emissions"),
        size = 1.5,
        alpha = 1

      ) +
      geom_text(
        aes(x = 17000, y = max.bar.height),
        label = label,
        parse = TRUE,
        nudge_y = 250
      ) +
      scale_x_continuous(
        name = "Distance to Conference (km)",
        labels = scales::label_comma()
      ) +
      scale_y_continuous(
        name = "Number of Attendees",
        labels = scales::label_comma(),
        breaks = seq(from = 0, to = max.bar.height, by = 1000),
        sec.axis = sec_axis(
          trans = ~.*sec_axis_trans,
          name = expression(Cummulative~tCO[2]~Emissions),
          labels = scales::label_percent())
      ) +
      scale_color_manual(
        name = " ",
        labels = expression(Cummulative~tCO[2]~Emissions),
        values = c("gray28")
      ) +
      scale_fill_manual(
        name = "Attendance at:",
        labels = conventions[IATA == region]$`Airport City`,
        values = c(assign(region, single.meeting.bar.col))
        ) +
      guides(
        fill  = guide_legend(order = 1, title.position = "top"),
        color = guide_legend(order = 2, title.position = "top")
        ) +
      ggtitle(title) +
      theme_minimal() +
      theme(legend.position = legend.position)

  }
}


# Output Plots ------------------------------------------------------------

# Initialize write to pdf
pdf(file = paste0(path.viz,"AAO Cummulative Footprint Plots.pdf"),
  width = 8, height = 6)

# Single-meeting plots
emissions_plot(aao.final.expanded, "SFO", single.meeting.bar.col = "#fb7000")
emissions_plot(aao.final.expanded, "LAS")
emissions_plot(aao.final.expanded, "MCO")
emissions_plot(aao.final.expanded, "MSY")
emissions_plot(
  aao.final.expanded,
  "ORD", # Needed to fix second y-axis labels
  offset = 18,
  single.meeting.bar.col = "#0ebcf1"
  )

# Regional format plots
emissions_plot(
  aao.final.regional.expanded,
  "1",
  regional = TRUE,
  title = "Regional Format 1"
  )
emissions_plot(
  aao.final.regional.expanded,
  "2",
  regional = TRUE,
  title = "Regional Format 2")

# Export
dev.off()
