# Step 01 - Regional Configuration Analysis
# Alec Stashevsky
# January 6, 2022


# Setup -------------------------------------------------------------------
rm(list = ls())
library(tidyverse)
library(data.table)
library(geosphere)

path.in <- "~/AAO-climate-research/Data/"
# path.viz <- "~/AAO-climate-research/Visualizations/"


# Import ------------------------------------------------------------------

# AAO Geo-Unique Frequency Data
aao <- readRDS(paste0(path.in, "AAO_GEODISTANCE.RDs"))
conventions <- readRDS(paste0(path.in, "AAO_CONVENTIONS.RDs"))


# Aggregate Commute Distance for Various Regional Configurations ----------

# Regional Configuration Grid
conf.grid <- data.table(
  expand.grid(
    "West" = c("SFO", "LAS"),
    "East" = c("ORD", "MSY", "MCO")
    )
  )

# Get closest meeting for each regional configuration
aao[, `:=`(
  regional_SFO_ORD = ifelse(gdist.SFO < gdist.ORD, "SFO", "ORD"),
  regional_LAS_ORD = ifelse(gdist.LAS < gdist.ORD, "LAS", "ORD"),
  regional_SFO_MSY = ifelse(gdist.SFO < gdist.MSY, "SFO", "MSY"),
  regional_LAS_MSY = ifelse(gdist.LAS < gdist.MSY, "LAS", "MSY"),
  regional_SFO_MCO = ifelse(gdist.SFO < gdist.MCO, "SFO", "MCO"),
  regional_LAS_MCO = ifelse(gdist.LAS < gdist.MCO, "LAS", "MCO")
  )]

distance.log <- data.table(
  "Configuration" = colnames(aao)[(ncol(aao) - 5):ncol(aao)],
  "Aggregate Distance" = rep(NA, nrow(conf.grid))
)

# Calculate total geodesic distance for each coordinate in sample
for (i in 1:nrow(conf.grid)) {

  distance.log$`Aggregate Distance`[i] <- sum(

    # West Coast ----

    # Weight by frequency
    aao[
      get(
        paste(
          "regional", conf.grid$West[i], conf.grid$East[i], sep = "_"
        )
      ) == conf.grid$West[i]
      ]$Frequency * distGeo(

      # Attendee origin
      aao[
        get(
          paste(
            "regional", conf.grid$West[i], conf.grid$East[i], sep = "_"
            )
          ) == conf.grid$West[i],
        .(Longitude, Latitude)
        ],

      # West coast destination
      conventions[IATA == conf.grid$West[i], .(Longitude, Latitude)]
    ),


    # East Coast ----

    # Weight by frequency
    aao[
      get(
        paste(
          "regional", conf.grid$West[i], conf.grid$East[i], sep = "_"
        )
      ) == conf.grid$East[i]
    ]$Frequency * distGeo(

      # Attendee origin
      aao[
        get(
          paste(
            "regional", conf.grid$West[i], conf.grid$East[i], sep = "_"
          )
        ) == conf.grid$East[i],
        .(Longitude, Latitude)
      ],

      # East coast destination
      conventions[IATA == conf.grid$East[i], .(Longitude, Latitude)]
    )
  )
}

# Add % difference and sort
distance.log <- arrange(
  distance.log[,
  `Percent Change` := (`Aggregate Distance` - first(`Aggregate Distance`)) / first(`Aggregate Distance`)
  ],
  `Aggregate Distance`
)
