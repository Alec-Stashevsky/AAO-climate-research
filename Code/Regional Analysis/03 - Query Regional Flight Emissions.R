# Step 03 - Query Regional Flight Emissions
# Alec Stashevsky
# January 8, 2022


# Setup -------------------------------------------------------------------
rm(list = ls())
library(tidyverse)
library(data.table)
library(httr)
library(jsonlite)

path.in <- "~/AAO-climate-research/Data/"
path.out <- "~/AAO-climate-research/Output/"

# From GoClimate
api.key <- readRDS("C:/Users/Alec/Documents/GAP Research/GAP Climate Research/GoClimate API/Api_key.rds" )


# Import ------------------------------------------------------------------
aao <- readRDS(paste0(path.in, "AAO_REGIONAL_CONFIG.RDs"))
conventions <- readRDS(paste0(path.in, "AAO_CONVENTIONS.RDs"))


# Build API Queries -------------------------------------------------------

# Set Query Parameters
seat.class <- "economy" # Assume most people will fly economy class

# Extract origin and destination IATA
origins <- aao$`Airport Code`
destination.table <- data.table(
  "destination1" = aao$`Regional Format 1`,
  "destination2" = aao$`Regional Format 2`
  )

# Initialize list for each Regional Format
query.list <- vector(mode = "list", length = 2)
names(query.list) <- c("Format1", "Format2")

for (fmt in 1:length(query.list)) {

  # Build interim query vector
  query.vec <- rep(NA, length(origins))

  # Set destination based on regional config
  if (fmt == 1) {
    destination <- destination.table$destination1
  } else if (fmt == 2) {
    destination <- destination.table$destination2
  } else {
    stop("Setup for additional conference format. Current only supports up to 2")
  }

  # Generate list for each round-trip query
  for (i in 1:length(origins)) {

    query <- paste0("https://api.goclimate.com/v1/flight_footprint?segments[0][origin]=",
      origins[i],          # Loop through each unique origin airport
      "&segments[0][destination]=",
      destination[i],         # Nearest airport to convention center
      "&segments[1][origin]=",
      destination[i],         # 2nd-leg of round trip flight
      "&segments[1][destination]=",
      origins[i],          # Return home
      "&cabin_class=",
      seat.class,              # Seat class of passenger
      "&currencies[]=USD")     # Price offsets in USD

    query.vec[i] <- query

  }

  # Put vector in list
  query.list[[fmt]] <- query.vec

}


# Query GoClimate API -----------------------------------------------------

# Initialize list
response.list <- vector(mode = "list", length = 2)
names(response.list) <- c("Format1", "Format2")

for (fmt in 1:length(response.list)) {

  # Initialize query responses
  responses.footprint <- rep(NA, length(origins))
  responses.offsets <- rep(NA, length(origins))

  # Loop through all queries in query.list
  for (i in 1:length(origins)) {

    r <- GET(query.list[[fmt]][i],
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

    # Add status output
    if (i %% 100 == 0) {
      print(paste("On query", i, "of Regional Format", fmt))
    }

  }

  # Coerce responses into list
  response.list[[fmt]] <- data.table(
    "Footprint" = responses.footprint,
    "Offset" = responses.offsets
  )

  # Be explicit in column names
  setnames(
    response.list[[fmt]],
    c(paste0("Footprint.Format", fmt),
      paste0("Offset.Format", fmt)
    )
  )

}


# Append to Geodata
aao.emissions <- cbind(aao, bind_cols(response.list))


# Export ------------------------------------------------------------------
saveRDS(aao.emissions, file = paste0(path.in, "AAO_REGIONAL_EMISSIONS.RDs"))
openxlsx::write.xlsx(aao.emissions, file = paste0(path.out, "AAO_REGIONAL_EMISSIONS.xlsx"))
