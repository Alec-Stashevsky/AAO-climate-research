# Step 05 - Total Emissions Estimation
# Alec Stashevsky
# December 21, 2021


# Setup -------------------------------------------------------------------
rm(list = ls())
library(tidyverse)
library(data.table)
library(openxlsx)

path.in <- "~/AAO-climate-research/Data/"
path.out <- "~/AAO-climate-research/Output/"


# Import ------------------------------------------------------------------
aao <- readRDS(paste0(path.in, "AAO_EMISSIONS.RDs"))


# Set Conversion Factors --------------------------------------------------
meter.to.mile <- 0.000621371
grams.to.kg <- 0.001
car.emissons.grams.per.mile <- 404
conversion.factor <- meter.to.mile * car.emissons.grams.per.mile * grams.to.kg

accomdation.dist <- 30 * 1 / meter.to.mile # Driving distance between SFO and OAK is used

# Calculate Total Emissions -----------------------------------------------
aao[, `:=`(
  "Total Emissions SFO (Kg)" = fcase(
    # Round trip driving emissions for 4 trips (Kg)
    drive.SFO == 1 & gdist.SFO < accomdation.dist,
    Frequency * 2 * gdist.SFO * conversion.factor * 4,

    # Round trip driving emissions for 1 trip (Kg)
    drive.SFO == 1 & gdist.SFO >=accomdation.dist,
    Frequency * 2 * gdist.SFO * conversion.factor,

    # Round trip flying emissions (Kg)
    drive.SFO == 0,
    Frequency * Footprint.SFO
    ),

  "Total Emissions ORD (Kg)" = fcase(
    # Round trip driving emissions for 4 trips (Kg)
    drive.ORD == 1 & gdist.ORD < accomdation.dist,
    Frequency * 2 * gdist.ORD * conversion.factor * 4,

    # Round trip driving emissions for 1 trip (Kg)
    drive.ORD == 1 & gdist.ORD >=accomdation.dist,
    Frequency * 2 * gdist.ORD * conversion.factor,

    # Round trip flying emissions (Kg)
    drive.ORD == 0,
    Frequency * Footprint.ORD
  ),

  "Total Emissions MSY (Kg)" = fcase(
    # Round trip driving emissions for 4 trips (Kg)
    drive.MSY == 1 & gdist.MSY < accomdation.dist,
    Frequency * 2 * gdist.MSY * conversion.factor * 4,

    # Round trip driving emissions for 1 trip (Kg)
    drive.MSY == 1 & gdist.MSY >=accomdation.dist,
    Frequency * 2 * gdist.MSY * conversion.factor,

    # Round trip flying emissions (Kg)
    drive.MSY == 0,
    Frequency * Footprint.MSY
  ),

  "Total Emissions LAS (Kg)" = fcase(
    # Round trip driving emissions for 4 trips (Kg)
    drive.LAS == 1 & gdist.LAS < accomdation.dist,
    Frequency * 2 * gdist.LAS * conversion.factor * 4,

    # Round trip driving emissions for 1 trip (Kg)
    drive.LAS == 1 & gdist.LAS >=accomdation.dist,
    Frequency * 2 * gdist.LAS * conversion.factor,

    # Round trip flying emissions (Kg)
    drive.LAS == 0,
    Frequency * Footprint.LAS
  ),

  "Total Emissions MCO (Kg)" = fcase(
    # Round trip driving emissions for 4 trips (Kg)
    drive.MCO == 1 & gdist.MCO < accomdation.dist,
    Frequency * 2 * gdist.MCO * conversion.factor * 4,

    # Round trip driving emissions for 1 trip (Kg)
    drive.MCO == 1 & gdist.MCO >=accomdation.dist,
    Frequency * 2 * gdist.MCO * conversion.factor,

    # Round trip flying emissions (Kg)
    drive.MCO == 0,
    Frequency * Footprint.MCO
  ))
  ]


# Export ------------------------------------------------------------------
saveRDS(aao, file = paste0(path.out, "AAO Total Emissions.RDs"))
write.xlsx(aao, file = paste0(path.out, "AAO Total Emissions.xlsx"))
