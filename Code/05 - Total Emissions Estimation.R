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


# Calculate Total Emissions -----------------------------------------------
aao[, `:=`(
  "Total Emissions SFO (Kg)" = ifelse(
    drive.SFO == 1,
    Frequency * 2 * gdist.SFO * conversion.factor, # Round trip driving emissions (Kg)
    Frequency * Footprint.SFO                      # Round trip flying emissions (Kg)
    ),

  "Total Emissions ORD (Kg)" = ifelse(
    drive.ORD == 1,
    Frequency * 2 * gdist.ORD * conversion.factor,
    Frequency * Footprint.ORD
    ),

  "Total Emissions MSY (Kg)" = ifelse(
    drive.MSY == 1,
    Frequency * 2 * gdist.MSY * conversion.factor,
    Frequency * Footprint.MSY
    ),

  "Total Emissions LAS (Kg)" = ifelse(
    drive.LAS == 1,
    Frequency * 2 * gdist.LAS * conversion.factor,
    Frequency * Footprint.LAS
    ),

  "Total Emissions MCO (Kg)" = ifelse(
    drive.MCO == 1,
    Frequency * 2 * gdist.MCO * conversion.factor,
    Frequency * Footprint.MCO
    )

  )]


# Export ------------------------------------------------------------------
saveRDS(aao, file = paste0(path.out, "AAO Total Emissions.RDs"))
write.xlsx(aao, file = paste0(path.out, "AAO Total Emissions.xlsx"))
