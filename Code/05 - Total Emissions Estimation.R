# Step 05 - Total Emissions Estimation
# Alec Stashevsky
# December 21, 2021


# Setup -------------------------------------------------------------------
rm(list = ls())
options(scipen = 999)
library(tidyverse)
library(data.table)

path.in <- "~/AAO-climate-research/Data/"


# Import ------------------------------------------------------------------
aao <- readRDS(paste0(path.in, "AAO_EMISSIONS.RDs"))
conventions <- readRDS(paste0(path.in, "AAO_CONVENTIONS.RDs"))


# Set Conversion Factors --------------------------------------------------
meter.to.mile <- 0.000621371
grams.to.kg <- 0.001
car.emissons.grams.per.mile <- 404
conversion.factor <- meter.to.mile * car.emissons.grams.per.mile * grams.to.kg


# Calculate Total Emissions -----------------------------------------------
aao[, `:=`(
  "Total Emissions SFO (Kg)" = ifelse(
    drive.SFO == 1,
    Frequency * 2 * gdist.SFO * conversion.factor, # Round trip driving emissions (Kg)
    Frequency * Footprint.SFO
    ),

  "Total Emissions LAS (Kg)" = ifelse(
    drive.LAS == 1,
    Frequency * 2 * gdist.LAS * conversion.factor, # Round trip driving emissions (Kg)
    Frequency * Footprint.LAS
  ),

  "Total Emissions MCO (Kg)" = ifelse(
    drive.MCO == 1,
    Frequency * 2 * gdist.MCO * conversion.factor, # Round trip driving emissions (Kg)
    Frequency * Footprint.MCO
  ),

  "Total Emissions MSY (Kg)" = ifelse(
    drive.MSY == 1,
    Frequency * 2 * gdist.MSY * conversion.factor, # Round trip driving emissions (Kg)
    Frequency * Footprint.MSY
  ),

  "Total Emissions ORD (Kg)" = ifelse(
    drive.ORD == 1,
    Frequency * 2 * gdist.ORD * conversion.factor, # Round trip driving emissions (Kg)
    Frequency * Footprint.ORD

  ))]




