# Title: Processing Presence Only Data of Pine Martens
# Purpose: Process presance only data of pine marten in Northen Ireland 
#
# Author: Leighanna Teal Dawson 
# Date: 20 Jan 2025
# 
##### Contence of the script:#####
# 0. Admin (load libraries, set working directory, load data)
# 1. Merge the presence-only data with habitat information and plot the data on a map
# 2. 
# 3.  
#

#### 0. Admin (load libraries, set working directory, load data) ####
#set working directory
setwd("C:/VScode/all-ireland-occupancy-modelling")

# Check if the directory is set correctly
getwd()


# Load required libraries
library(sf)          # For handling spatial vector data
library(raster)      # For handling raster data
library(tidyverse)   # For data manipulation and visualization (# Tidyverse packages:
# ggplot2, dplyr, tidyr, readr, purrr, tibble, stringr, forcats, lubridate, hms)
library(leaflet)     # For interactive maps
print("Libraries loaded successfully.")

# Import data 
habitatinfo <- read.csv("1.data/1.1.raw/presenceonlydata/habitatinfo201520182020sitesonly.csv")
pmpresenceonly <- read.csv("1.data/1.1.raw/presenceonlydata/pinemarten201520182020noname.csv") 

#########################################################################################################################
#### 1. Merge the presence-only data with habitat information and plot the data on a map #####
colnames(habitat_info) 
colnames(pmpresenceonly)
view(habitatinfo)
view(pmpresenceonly)
# Add observation ID to each data frame
habitatinfo$observation_id <- seq_len(nrow(habitatinfo))
pmpresenceonly$observation_id <- seq_len(nrow(pmpresenceonly))

colnames(habitatinfo)
colnames(pmpresenceonly)

# Merge the two data frames
pmandhabitat <- merge(habitatinfo, pmpresenceonly, by = "observation_id")
view(pmandhabitat)

#save data 
write.csv(pmandhabitat, "1.data/1.2.processed/paandhabitat.csv", row.names = FALSE)

# Create map of data
leaflet(data = pmandhabitat) %>%
  addTiles() %>%
  addCircleMarkers(
    lng = ~long, lat = ~lat, 
    color = "red", radius = 4, 
    fillOpacity = 0.7, 
    stroke = FALSE
  ) %>%
  setView(lng = mean(pmandhabitat$long), lat = mean(pmandhabitat$lat), zoom = 10)



####################
