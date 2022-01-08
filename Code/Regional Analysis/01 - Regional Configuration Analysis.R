# Step 01 - Regional Configuration Analysis
# Alec Stashevsky
# January 6, 2022


# Setup -------------------------------------------------------------------
rm(list = ls())
library(tidyverse)
library(data.table)
library(geosphere)
library(matrixStats)

path.in <- "~/AAO-climate-research/Data/"


# Import ------------------------------------------------------------------

# AAO Geo-Unique Frequency Data
aao <- readRDS(paste0(path.in, "AAO_GEODISTANCE.RDs"))
conventions <- readRDS(paste0(path.in, "AAO_CONVENTIONS.RDs"))


# Aggregate Commute Distance for Various Regional Configurations ----------

# Regional Configuration Grid
conf.grid <- data.table(
  expand.grid(
    "West" = c("SFO", "LAS"),
    "East" = c("ORD", "MSY", "MCO"),
    stringsAsFactors = FALSE
    )
  )

# Regional Configuration Grid
conf.grid2 <- data.table(
  expand.grid(
    "West" = c("SFO", "LAS"),
    "East" = c("ORD", "MSY", "MCO"),
    "East2" = c("ORD", "MSY", "MCO"),
    stringsAsFactors = FALSE
  )
)

# Dedupe
conf.grid2 <- conf.grid2[East != East2]

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


# Add Second East Coast Conference ----------------------------------------

for (i in 1:nrow(conf.grid2)) {

  aao[,
    paste(
      "regional",
      conf.grid2$West[i],
      conf.grid2$East[i],
      conf.grid2$East2[i],
      sep = "_"
      ) := fcase(

        # Test if min distance to all 3 conferences is equal to:

        # West
        get(paste0("gdist.", conf.grid2$West[i])) == rowMins(
          as.matrix(data.frame(
            get(paste0("gdist.", conf.grid2$West[i])),
            get(paste0("gdist.", conf.grid2$East[i])),
            get(paste0("gdist.", conf.grid2$East2[i]))
            ))
          ), conf.grid2$West[i],

        # East
        get(paste0("gdist.", conf.grid2$East[i])) == rowMins(
          as.matrix(data.frame(
            get(paste0("gdist.", conf.grid2$West[i])),
            get(paste0("gdist.", conf.grid2$East[i])),
            get(paste0("gdist.", conf.grid2$East2[i]))
            ))
          ), conf.grid2$East[i],

        # East2
        get(paste0("gdist.", conf.grid2$East2[i])) == rowMins(
          as.matrix(data.frame(
            get(paste0("gdist.", conf.grid2$West[i])),
            get(paste0("gdist.", conf.grid2$East[i])),
            get(paste0("gdist.", conf.grid2$East2[i]))
            ))
          ), conf.grid2$East2[i]
        )
    ]
}

distance.log2 <- data.table(
  "Configuration" = colnames(aao)[(ncol(aao) - 11):ncol(aao)],
  "Aggregate Distance" = rep(NA, nrow(conf.grid2))
)


# Calculate total geodesic distance for each coordinate in sample
for (i in 1:nrow(conf.grid2)) {

  distance.log2$`Aggregate Distance`[i] <- sum(

    # West Coast ----

    # Weight by frequency
    aao[
      get(
        paste(
          "regional",
          conf.grid2$West[i],
          conf.grid2$East[i],
          conf.grid2$East2[i],
          sep = "_"
        )
      ) == conf.grid2$West[i]
    ]$Frequency * distGeo(

      # Attendee origin
      aao[
        get(
          paste(
            "regional",
            conf.grid2$West[i],
            conf.grid2$East[i],
            conf.grid2$East2[i],
            sep = "_"
          )
        ) == conf.grid2$West[i],
        .(Longitude, Latitude)
      ],

      # West coast destination
      conventions[IATA == conf.grid2$West[i], .(Longitude, Latitude)]
    ),


    # East Coast ----

    # Weight by frequency
    aao[
      get(
        paste(
          "regional",
          conf.grid2$West[i],
          conf.grid2$East[i],
          conf.grid2$East2[i],
          sep = "_"
        )
      ) == conf.grid2$East[i]
    ]$Frequency * distGeo(

      # Attendee origin
      aao[
        get(
          paste(
            "regional",
            conf.grid2$West[i],
            conf.grid2$East[i],
            conf.grid2$East2[i],
            sep = "_"
          )
        ) == conf.grid2$East[i],
        .(Longitude, Latitude)
      ],

      # East coast destination
      conventions[IATA == conf.grid2$East[i], .(Longitude, Latitude)]
    ),

    # East Coast 2 ----

    # Weight by frequency
    aao[
      get(
        paste(
          "regional",
          conf.grid2$West[i],
          conf.grid2$East[i],
          conf.grid2$East2[i],
          sep = "_"
        )
      ) == conf.grid2$East2[i]
    ]$Frequency * distGeo(

      # Attendee origin
      aao[
        get(
          paste(
            "regional",
            conf.grid2$West[i],
            conf.grid2$East[i],
            conf.grid2$East2[i],
            sep = "_"
          )
        ) == conf.grid2$East2[i],
        .(Longitude, Latitude)
      ],

      # East coast destination
      conventions[IATA == conf.grid2$East2[i], .(Longitude, Latitude)]
    )


  )
}

# Add % difference and sort
distance.log2 <- arrange(
  distance.log2[,
    `Percent Change` := (`Aggregate Distance` - min(`Aggregate Distance`)) / min(`Aggregate Distance`)
  ],
  `Aggregate Distance`
)

# Drop every other row
distance.log2 <- distance.log2[seq(1, nrow(distance.log2), 2)]


# Encode Regional Destination ---------------------------------------------
aao[, `:=`(

  # Use SFO and ORD
  `Regional Format 1` = ifelse(gdist.SFO < gdist.ORD, "SFO", "ORD"),

  # Use SFO, ORD, and MCO
  `Regional Format 2` = fcase(

    # IF SFO is min
    gdist.SFO == rowMins(
      as.matrix(
        data.frame(
          gdist.SFO,
          gdist.ORD,
          gdist.MCO
          )
        )
      ), "SFO",

    # If ORD is min
    gdist.ORD == rowMins(
      as.matrix(
        data.frame(
          gdist.SFO,
          gdist.ORD,
          gdist.MCO
        )
      )
    ), "ORD",

    # If MCO is min
    gdist.MCO == rowMins(
      as.matrix(
        data.frame(
          gdist.SFO,
          gdist.ORD,
          gdist.MCO
        )
      )
    ), "MCO"

  )
)]

# Drop configuration columns

aao[, grep("regional_", colnames(aao)) := NULL]


# Export ------------------------------------------------------------------
saveRDS(aao, file = paste0(path.in, "AAO_REGIONAL_CONFIG.RDs"))
