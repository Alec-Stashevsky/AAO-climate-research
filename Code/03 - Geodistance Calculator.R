# Step 03 - Geodistance Calculator
# Alec Stashevsky
# December 21, 2021


# Setup -------------------------------------------------------------------
rm(list = ls())
library(tidyverse)
library(data.table)
library(geosphere)


# Import ------------------------------------------------------------------
aao <- readRDS(paste0(path.in, "AAO_GEO_UNIQUE.RDs"))


