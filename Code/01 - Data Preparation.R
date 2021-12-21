# Step 01 - Data Preparation
# Alec Stashevsky
# December 20, 2021


# Setup -------------------------------------------------------------------
rm(list = ls())
library(tidyverse)
library(data.table)
library(readxl)

path.in <- "~/AAO-climate-research/Data/Raw/"
path.out <- "~/AAO-climate-research/Data/"
path.airports <- "~/AAO-climate-research/Data/GlobalAirportDatabase/GlobalAirportDatabase.txt"

# From previous GAP research
path.misqueries <- "C:/Users/Alec/Documents/GAP Research/GAP Climate Research/GeoData/GoClimate API Misqueries.xlsx"


# Import AAO Data ----------------------------------------------------------
raw.sheets <- excel_sheets(paste0(path.in, "AAO 2019 Attendees_Airport Code.xlsx"))

# Initialize
raw.list <- vector(mode = "list", length(raw.sheets))
names(raw.list) <- raw.sheets

i <- 1

# Import each sheet in raw excel file
for (sheet in raw.sheets) {

  raw.list[[i]] <- read_excel(
    path = paste0(path.in, "AAO 2019 Attendees_Airport Code.xlsx"),
    sheet = sheet
    ) %>% setDT()

  setnames(raw.list[[i]], "...1", "Attendee")

  i <- i + 1

}

# Right now, just work w/ the already cleaned data Scott added IATA codes to
clean <- raw.list$`Airport Code - cleaned`


# Import Partow Airport Data ----------------------------------------------
airports.raw <- read.delim(path.airports, header = FALSE, sep = ":") %>% setDT()

# Drop extraneous columns
airports <- airports.raw[, c(1:5, 14:16)]

# Set column names
setnames(airports, names(airports), c("ICAO", "IATA", "Airport Name",
  "Airport City", "Airport Country", "Altitude", "Latitude", "Longitude"))

# Import Misqueries
misqueries <- read_excel(path.misqueries, sheet = "Misqueries") %>% setDT()


# Clean Airport Data ------------------------------------------------------

airports.clean <- airports[
  # Exclude airports w/o coordinates and IATA codes
  Latitude != 0 & Longitude != 0 & IATA != 'N/A'][
    # Filter out any which cause GoClimate Misqueries
    !(IATA %in% misqueries$IATA)][
      # Filter out duplicated IATA codes causing issues downstream
      !(`Airport Name` %in% c(
        "INCIRLIK AB",
        "ARMILLA",
        "MALABANG",
        "ANGEL S ADAMI")
        )
      ]


# Hold for AAO Cleaning Section ------------------------------------------



# Proxy Geocode with Airport Data -----------------------------------------
aao.proxy.geocode <- merge(
  clean,
  airports.clean,
  by.x = "Airport Code",
  by.y = "IATA",
  all.x = TRUE
  )


# Collapse by Location (Airport as Proxy) ---------------------------------
aao.geo.unique <- unique(
  aao.proxy.geocode[, Frequency := .N, by = .(
    `Airport Code`, Latitude, Longitude)
    ],
  by = c("Airport Code", "Latitude", "Longitude")
  )


# Export ------------------------------------------------------------------
saveRDS(clean, file = paste0(path.out, "AAO_BASE_SCOTT_IATA.RDs"))
saveRDS(aao.proxy.geocode, file = paste0(path.out, "AAO_PROXY_GEOCODE.RDs"))
saveRDS(aao.geo.unique, file = paste0(path.out, "AAO_GEO_UNIQUE.RDs"))

