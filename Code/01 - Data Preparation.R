# Step 01 - Data Preparation
# Alec Stashevsky
# December 20, 2021


# Setup -------------------------------------------------------------------
rm(list = ls())
library(tidyverse)
library(data.table)
library(readxl)
library(openxlsx)

path.in <- "~/AAO-climate-research/Data/Raw/"
path.out <- "~/AAO-climate-research/Data/"


# Import ------------------------------------------------------------------
raw.sheets <- excel_sheets(paste0(path.in, "AAO 2019 Attendees_Airport Code.xlsx"))

# Initialize
raw.list <- vector(mode = "list", length(raw.sheets))
names(raw.list) <- raw.sheets

i <- 1

# Import each sheet in raw excel file
for (sheet in raw.sheets) {

  raw.list[[i]] <- read_excel(
    paste0(path.in, "AAO 2019 Attendees_Airport Code.xlsx")
    ) %>% setDT()

  setnames(raw.list[[i]], "...1", "Attendee")

  i <- i + 1

}

clean <- raw.list$`Airport Code - cleaned`

