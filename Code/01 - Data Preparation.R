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
path.airports <- "~/AAO-climate-research/Data/OpenFlights/airports.txt"
path.airports.supp <- "~/AAO-climate-research/Data/AAO Airport Supplement.xlsx"

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

  # Fix data entry errors when Scott input IATA codes
  raw.list[[i]][`Airport Code` == "KUZ"]$`Airport Code` <- "KUV"
  raw.list[[i]][`Airport Code` == "NML"]$`Airport Code` <- "MNL"
  raw.list[[i]][`Airport Code` == "WSX"]$`Airport Code` <- "SEA"
  raw.list[[i]][`Airport Code` == "YWO"]$`Airport Code` <- "YYZ"

  i <- i + 1

}

# Right now, just work w/ the already cleaned data Scott added IATA codes to
clean <- raw.list$`Airport Code - cleaned`


# Import OpenFlights Airport Data -----------------------------------------
airports.raw <- fread(path.airports)

# Drop extraneous columns
airports <- airports.raw[, -c(12:14)]

# Set column names
setnames(airports,
  c("OpenFlights ID",
    "Airport Name",
    "Airport City",
    "Airport Country",
    "IATA",
    "ICAO",
    "Latitude",
    "Longitude",
    "Altitude",
    "Timezone",
    "DST")
  )

# Import Airport Supplement (Manual Research to impute OpenFlights)
airports.supp <- read_excel(paste0(path.airports.supp)) %>% setDT()

# Import Misqueries
misqueries <- read_excel(path.misqueries, sheet = "Misqueries") %>% setDT()


# Clean Airport Data ------------------------------------------------------

airports.clean <- airports[
  # Exclude airports w/o coordinates and IATA codes
  Latitude != 0 & Longitude != 0 & IATA != "\\N"][
    # Filter out any which cause GoClimate Misqueries
    !(IATA %in% misqueries$IATA)]

# Add in supplement
airports.clean <- rbind(airports.clean, airports.supp, use.names = TRUE, fill = TRUE)


# Hold for AAO Cleaning Section ------------------------------------------



# Proxy Geocode with Airport Data -----------------------------------------
aao.proxy.geocode <- merge(
  clean,
  airports.clean,
  by.x = "Airport Code",
  by.y = "IATA",
  all.x = TRUE
  )

# Add driver flag (per Scott's calculation) -- TODO: Perform my own calculation
aao.proxy.geocode[, Driver := ifelse(
  `Airport Code` %in% c(
    "MCE",
    "MRY",
    "OAK",
    "SCK",
    "SFO",
    "SMF",
    "STS"
    ),
  1,
  0)]



# Collapse by Location (Airport as Proxy) ---------------------------------
aao.geo.unique <- unique(
  aao.proxy.geocode[, Frequency := .N, by = .(
    `Airport Code`, Driver, Latitude, Longitude)
    ],
  by = c("Airport Code", "Driver", "Latitude", "Longitude")
  )


# Export ------------------------------------------------------------------
saveRDS(clean, file = paste0(path.out, "AAO_BASE_SCOTT_IATA.RDs"))
saveRDS(aao.proxy.geocode, file = paste0(path.out, "AAO_PROXY_GEOCODE.RDs"))
saveRDS(aao.geo.unique, file = paste0(path.out, "AAO_GEO_UNIQUE.RDs"))
