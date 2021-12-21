# Step 02 - Proxy Geocode through Airport Data
# Alec Stashevsky
# December 20, 2021


# Setup -------------------------------------------------------------------
rm(list = ls())
library(tidyverse)
library(data.table)
library(readxl)
library(openxlsx)

path.in <- "~/AAO-climate-research/Data/"
path.airports <- "~/AAO-climate-research/Data/GlobalAirportDatabase/GlobalAirportDatabase.txt"


# Import ------------------------------------------------------------------
airports.raw <- read.delim(path.airports, header = FALSE, sep = ":") %>% setDT()

# Drop extraneous columns
airports <- airports.raw[, c(1:5, 14:16)]

# Set column names
setnames(airports, names(airports), c("ICAO", "IATA", "Airport Name",
  "City", "Country", "Altitude", "Latitude", "Longitude"))
