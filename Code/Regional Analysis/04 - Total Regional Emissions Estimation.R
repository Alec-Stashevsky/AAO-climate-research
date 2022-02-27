# Step 04 - Total Regional Emissions Estimation
# Alec Stashevsky
# January 8, 2022


# Setup -------------------------------------------------------------------
rm(list = ls())
library(tidyverse)
library(data.table)
library(openxlsx)

path.in <- "~/AAO-climate-research/Data/"
path.out <- "~/AAO-climate-research/Output/"


# Import ------------------------------------------------------------------
aao <- readRDS(paste0(path.in, "AAO_REGIONAL_EMISSIONS.RDs"))


# Set Conversion Factors --------------------------------------------------
meter.to.mile <- 0.000621371
grams.to.kg <- 0.001
car.emissons.grams.per.mile <- 404
conversion.factor <- meter.to.mile * car.emissons.grams.per.mile * grams.to.kg

accomdation.dist <- 30 * 1 / meter.to.mile # Driving distance between SFO and OAK is used

# Calculate Total Emissions -----------------------------------------------
aao[, `:=`(
  "Total Emissions Format 1 (Kg)" = fcase(

    # SFO Drivers - No Accommodation
    `Regional Format 1` == "SFO" & drive.SFO == 1 & gdist.SFO < accomdation.dist,
    Frequency * 2 * gdist.SFO * conversion.factor * 4, # Round trip driving emissions (Kg)

    # SFO Drivers
    `Regional Format 1` == "SFO" & drive.SFO == 1 & gdist.SFO >= accomdation.dist,
    Frequency * 2 * gdist.SFO * conversion.factor, # Round trip driving emissions (Kg)

    # SFO Flyers
    `Regional Format 1` == "SFO" & drive.SFO == 0,
    Frequency * Footprint.Format1,                 # Round trip flying emissions (Kg)

    # ORD Drivers - No Accommodation
    `Regional Format 1` == "ORD" & drive.ORD == 1 & gdist.ORD < accomdation.dist,
    Frequency * 2 * gdist.ORD * conversion.factor * 4, # Round trip driving emissions (Kg)

    # ORD Drivers
    `Regional Format 1` == "ORD" & drive.ORD == 1 & gdist.ORD >= accomdation.dist,
    Frequency * 2 * gdist.ORD * conversion.factor, # Round trip driving emissions (Kg)

    # ORD Flyers
    `Regional Format 1` == "ORD" & drive.ORD == 0,
    Frequency * Footprint.Format1                  # Round trip flying emissions (Kg)

    ),

  "Total Emissions Format 2 (Kg)" = fcase(

    # SFO Drivers - No Accommodation
    `Regional Format 2` == "SFO" & drive.SFO == 1 & gdist.SFO < accomdation.dist,
    Frequency * 2 * gdist.SFO * conversion.factor * 4, # Round trip driving emissions (Kg)

    # SFO Drivers
    `Regional Format 2` == "SFO" & drive.SFO == 1 & gdist.SFO >= accomdation.dist,
    Frequency * 2 * gdist.SFO * conversion.factor, # Round trip driving emissions (Kg)

    # SFO Flyers
    `Regional Format 2` == "SFO" & drive.SFO == 0,
    Frequency * Footprint.Format2,                 # Round trip flying emissions (Kg)

    # ORD Drivers - No Accommodation
    `Regional Format 2` == "ORD" & drive.ORD == 1 & gdist.ORD < accomdation.dist,
    Frequency * 2 * gdist.ORD * conversion.factor * 4, # Round trip driving emissions (Kg)

    # ORD Drivers
    `Regional Format 2` == "ORD" & drive.ORD == 1 & gdist.ORD >= accomdation.dist,
    Frequency * 2 * gdist.ORD * conversion.factor, # Round trip driving emissions (Kg)

    # ORD Flyers
    `Regional Format 2` == "ORD" & drive.ORD == 0,
    Frequency * Footprint.Format2,                 # Round trip flying emissions (Kg)

    # MCO Drivers - No Accommodation
    `Regional Format 2` == "MCO" & drive.MCO == 1 & gdist.MCO < accomdation.dist,
    Frequency * 2 * gdist.MCO * conversion.factor * 4, # Round trip driving emissions (Kg)

    # MCO Drivers
    `Regional Format 2` == "MCO" & drive.MCO == 1 & gdist.MCO >= accomdation.dist,
    Frequency * 2 * gdist.MCO * conversion.factor, # Round trip driving emissions (Kg)

    # MCO Flyers
    `Regional Format 2` == "MCO" & drive.MCO == 0,
    Frequency * Footprint.Format2                  # Round trip flying emissions (Kg)

  )

)]


# Export ------------------------------------------------------------------
saveRDS(aao, file = paste0(path.out, "AAO Total Regional Emissions.RDs"))
write.xlsx(aao, file = paste0(path.out, "AAO Total Regional Emissions.xlsx"))
