# Step 03 - Flight Emissions
# Alec Stashevsky
# December 21, 2021


# Setup -------------------------------------------------------------------
rm(list = ls())
library(httr)
library(jsonlite)
library(tidyverse)
library(data.table)

path.in <- "~/AAO-climate-research/Data/"

# From GoClimate
api.key <- readRDS("C:/Users/Alec/Documents/GAP Research/GAP Climate Research/GoClimate API/Api_key.rds" )

# Import ------------------------------------------------------------------
aao <- readRDS(paste0(path.in, "AAO_GEO_UNIQUE.RDs"))


# Build API Queries -------------------------------------------------------

# Set Query Parameters
seat.class <- "economy" # Assume most people will fly economy class
destination <- "SFO"

# Extract origin IATA
origins <- aao$`Airport Code`

# Initialize list
query.list <- rep(NA, length(origins))

# Generate list for each round-trip query
for (i in 1:length(origins)) {

  query <- paste0("https://api.goclimate.com/v1/flight_footprint?segments[0][origin]=",
    origins[i],          # Loop through each unique origin airport
    "&segments[0][destination]=",
    destination,         # Nearest airport to convention center
    "&segments[1][origin]=",
    destination,         # 2nd-leg of round trip flight
    "&segments[1][destination]=",
    origins[i],          # Return home
    "&cabin_class=",
    seat.class,              # Seat class of passenger
    "&currencies[]=USD")     # Price offsets in USD

  query.list[i] <- query

}


# Query GoClimate API -----------------------------------------------------

# Initialize query responses
responses.footprint <- rep(NA, length(query.list))
responses.offsets <- rep(NA, length(query.list))

# Loop through all queries in query.list
for (i in 1:length(query.list)) {

  r <- GET(query.list[i],
    authenticate(api.key, ""))

  c <- fromJSON(content(r, as = "text"))

  # Ignore null return values from API
  tryCatch({
    responses.footprint[i] <- as.numeric(c$footprint)
    responses.offsets[i] <- as.numeric(c$offset_prices[1])
  },

    error = function(e) {
      print(paste0("NULL QUERY ", i))
    })
}

# Append to Geodata
aao.emissions <- cbind(aao,
  Footprint = responses.footprint,
  Offset = responses.offsets)


# Export ------------------------------------------------------------------
saveRDS(aao.emissions, file = paste0(path.in, "AAO_EMISSIONS.RDs"))
