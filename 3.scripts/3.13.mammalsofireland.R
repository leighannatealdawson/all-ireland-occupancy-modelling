
#reformat the data from the Atlas of Mammals of Ireland 2010-2015 and Mammals of Ireland 2016-2025 datasets. The data is in the form of presence-only data, which means that it only contains information about the presence of species in a given location, but not about the absence of species. The data is stored in two separate CSV files, which we will import into R and combine into a single dataset. We will then clean and reformat the data to make it suitable for analysis.
rm(list = ls())
# Load necessary libraries
library(readr)
library(dplyr)
library(tidyverse)
library(igr)
library(ggplot2)
library(sf)
library(ggplot2)
library(rnaturalearth)  
library(rnaturalearthdata)
library(lubridate)


# Import the first CSV file
atlas_mammals_2010_2015 <- read.csv("1.data/1.1.raw/presenceonlydata/AtlasOfMammals2010-2015.csv")

# Import the second CSV file
mammals_ireland_2016_2025 <- read.csv("1.data/1.1.raw/presenceonlydata/MammalsOfIreland2016-2025.csv")

View(atlas_mammals_2010_2015)
View(mammals_ireland_2016_2025)

colnames(atlas_mammals_2010_2015)
colnames(mammals_ireland_2016_2025)


# Convert RecordKey to character in both data frames
atlas_mammals_2010_2015$RecordKey <- as.character(atlas_mammals_2010_2015$RecordKey)
mammals_ireland_2016_2025$RecordKey <- as.character(mammals_ireland_2016_2025$RecordKey)

# Stack the two data frames vertically
stacked_data <- bind_rows(atlas_mammals_2010_2015, mammals_ireland_2016_2025)

# View the stacked data
View(stacked_data)


###########################################################################################################
# divide by species order 
#view unique in taxon column
unique(stacked_data$TaxonName)

# Create the data again
species_data <- data.frame(
  TaxonName = c("Vulpes vulpes", "Meles meles", "Mustela furo", "Oryctolagus cuniculus", "Erinaceus europaeus", 
                "Lutra lutra", "Cervus nippon", "Lepus timidus subsp. hibernicus", "Cervus elaphus", "Mustela vison", 
                "Sciurus vulgaris", "Sorex minutus", "Martes martes", "Mustela erminea subsp. hibernica", 
                "Apodemus sylvaticus", "Myodes glareolus", "Ondatra zibethicus", "Rattus norvegicus", 
                "Mus musculus", "Sciurus carolinensis", "Crocidura russula", "Phoca vitulina", "Dama dama", 
                "Lepus europaeus", "Halichoerus grypus", "Capra hircus", "Odobenus rosmarus", "Macropus rufogriseus", 
                "Rattus rattus", "Erignathus barbatus", "Pipistrellus pygmaeus", "Nyctalus leisleri", 
                "Balaenoptera physalus", "Pipistrellus pipistrellus sensu stricto", "Myotis daubentonii", 
                "Pipistrellus pipistrellus sensu lato", "Plecotus auritus", "Rhinolophus hipposideros", 
                "Phocoena phocoena", "Muscardinus avellanarius", "Myotis nattereri", "Pipistrellus nathusii", 
                "Myotis mystacinus", "Muntiacus reevesi", "Rhinolophus ferrumequinum", "Delphinapterus leucas"),
  CommonName = c("Red Fox", "European Badger", "Ferret", "European Rabbit", "European Hedgehog", "European Otter", 
                 "Sika Deer", "Irish Hare", "Red Deer", "American Mink", "Red Squirrel", "Pygmy Shrew", "Pine Marten", 
                 "European Weasel", "Wood Mouse", "Bank Vole", "Muskrat", "Norway Rat", "House Mouse", "Grey Squirrel", 
                 "European Mole", "Harbour Seal", "Fallow Deer", "Brown Hare", "Grey Seal", "Domestic Goat", 
                 "Walrus", "Red Kangaroo", "Black Rat", "Barbary Coast Seal", "Pipistrelle Bat", "Leisler's Bat", 
                 "Fin Whale", "Common Pipistrelle", "Daubenton's Bat", "Common Pipistrelle", "Brown Long-eared Bat", 
                 "Greater Horseshoe Bat", "Harbour Porpoise", "Hazel Dormouse", "Natterer's Bat", "Nathusius' Pipistrelle", 
                 "Whiskered Bat", "Muntjac Deer", "Lesser Horseshoe Bat", "Beluga Whale"),
  Order = c("Carnivora", "Carnivora", "Carnivora", "Lagomorpha", "Eulipotyphla", "Carnivora", "Artiodactyla", "Lagomorpha", 
            "Artiodactyla", "Carnivora", "Rodentia", "Eulipotyphla", "Carnivora", "Carnivora", "Rodentia", "Rodentia", 
            "Rodentia", "Rodentia", "Rodentia", "Rodentia", "Eulipotyphla", "Carnivora", "Artiodactyla", "Lagomorpha", 
            "Carnivora", "Artiodactyla", "Carnivora", "Diprotodontia", "Rodentia", "Carnivora", "Chiroptera", 
            "Chiroptera", "Cetacea", "Chiroptera", "Chiroptera", "Chiroptera", "Chiroptera", "Chiroptera", "Cetacea", 
            "Rodentia", "Chiroptera", "Chiroptera", "Chiroptera", "Artiodactyla", "Chiroptera", "Cetacea")
)

# View the result
View(species_data)

# Merge the Order information into your original stacked data using the species_data dataframe
all_IRE_species_df <- stacked_data %>%
  left_join(species_data, by = "TaxonName")

# View the updated dataset with the Order column added
View(all_IRE_species_df)

# Define the species to remove
species_to_remove <- c(
  "Phoca vitulina",    # Harbour seal
  "Halichoerus grypus", # Grey seal
  "Odobenus rosmarus",  # Walrus
  "Balaenoptera physalus", # Fin whale
  "Delphinapterus leucas", # Beluga whale
  "Phocoena phocoena", # Harbour porpoise
  "Macropus rufogriseus",  # Red kangaroo
  "Erignathus barbatus"  # Bearded seal
)

# Remove the species from the stacked data
all_IRE_species_df <- all_IRE_species_df %>%
  filter(!TaxonName %in% species_to_remove)

# Summarize the cleaned data by TaxonName
taxon_summary <- all_IRE_species_df %>%
  group_by(TaxonName) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count))

# Check the result
View(taxon_summary)

###########################################################################################################
# check how many records are missing in the data in grid col

#count empty for GridREf col
sum(all_IRE_species_df$GridRef == "")

#count unique in each projection 
all_IRE_species_df %>%
  group_by(Projection) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count))


###########################################################################################################
all_IRE_species_df_OSI <- all_IRE_species_df %>%
  filter(Projection == "OSI") %>%
  select(-Projection)
View(all_IRE_species_df_OSI)
#this data has all Gridref data required 
########################################################################################################### 
#### Process WGS84 data to be reprojected into irishgrid 
all_IRE_species_df_WGS84 <- all_IRE_species_df %>%
  filter(Projection == "WGS84") %>%
  select(-Projection)

View(all_IRE_species_df_WGS84)  

# Filter rows with East and North coordinates having more than 2 decimal places
all_IRE_species_df_WGS84 <- all_IRE_species_df_WGS84 %>%
  filter(
    (nchar(sub("\\.", "", as.character(East))) - nchar(as.character(floor(East)))) > 2 &
    (nchar(sub("\\.", "", as.character(North))) - nchar(as.character(floor(North)))) > 2
  )
View(all_IRE_species_df_WGS84)


# Convert the data frame to an sf object
all_IRE_species_df_WGS84 <- st_as_sf(all_IRE_species_df_WGS84, coords = c("East", "North"), crs = 4326)

# Plot the data with a background map to check projection 
ggplot() +
    borders("world", xlim = c(-11, -5), ylim = c(51, 56), fill = "gray80") +
    geom_sf(data = all_IRE_species_df_WGS84, aes(geometry = geometry), color = "blue", size = 0.5) +
    coord_sf(crs = st_crs(4326)) +
    theme_minimal() +
    labs(title = "Mammals of Ireland (WGS84 Projection)", x = "Longitude", y = "Latitude")

# Transform to Irish Grid
all_IRE_species_df_IrishGrid <- st_transform(all_IRE_species_df_WGS84, crs = 29903)
#checked on map and it looks good

#  Get the map of Ireland using rnaturalearth
ireland <- ne_countries(scale = "large", returnclass = "sf") %>%
    filter(name == "Ireland")

#  Plot the data with a background map to check projection
ggplot() +
    # Add Ireland map as background
    geom_sf(data = ireland, fill = "gray80") +
    # Overlay the Irish Grid species data in red
    geom_sf(data = all_IRE_species_df_IrishGrid, aes(geometry = geometry), color = "red", size = 0.5) +
    # Set the projection to Irish Grid (EPSG:29903)
    coord_sf(crs = st_crs(29903)) +
    theme_minimal() +
    labs(title = "Mammals of Ireland (Irish Grid Projection)", x = "Easting", y = "Northing")
 
# head(all_IRE_species_df_IrishGrid)
View(all_IRE_species_df_IrishGrid)

# Add Irish Grid references to the sf object
all_IRE_species_df_IrishGrid$GridReference <- st_irishgridrefs(all_IRE_species_df_IrishGrid)

# View the updated sf object
View(all_IRE_species_df_IrishGrid)

# Convert the sf object back to a data frame
all_IRE_species_df_IrishGrid <- as.data.frame(all_IRE_species_df_IrishGrid)

colnames(all_IRE_species_df)

#remerge with OSI data 
all_IRE_species_df <- bind_rows(all_IRE_species_df_OSI, all_IRE_species_df_IrishGrid)

View(all_IRE_species_df)

#keep only df all_IRE_species_df in environment
rm(list = setdiff(ls(), "all_IRE_species_df"))
###########################################################################################################
# check data formats 
#check al unique in date col 
date <- unique(all_IRE_species_df$StartDate)
View(date)

# Convert StartDate to Date format (assuming it's in "dd/mm/yyyy")
all_IRE_species_df$StartDate <- dmy(all_IRE_species_df$StartDate)

# Extract year, month, and date into new columns
all_IRE_species_df$Year <- year(all_IRE_species_df$StartDate)
all_IRE_species_df$Month <- month(all_IRE_species_df$StartDate)
all_IRE_species_df$Day <- day(all_IRE_species_df$StartDate)

View(all_IRE_species_df)

#filter for year 2015 - 2020 
all_IRE_species_df_2015_2020 <- all_IRE_species_df %>%
  filter(Year >= 2015 & Year <= 2020)
View(all_IRE_species_df_2015_2020) 

summarise by species 
# Summarize the data by species by count of each species 
species_summary <- all_IRE_species_df_2015_2020 %>%
  group_by(TaxonName) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count))
View(species_summary)

make wide data where each row is a unqiue date and grid ref and each species has a col for present of absent 
# Create a wide data frame with one row per unique date and grid reference
all_IRE_species_df_wide <- all_IRE_species_df_2015_2020 %>%
  select(StartDate, GridReference, TaxonName) %>%
  distinct() %>%
  pivot_wider(names_from = TaxonName, values_from = TaxonName, values_fn = length, values_fill = 0)

View(all_IRE_species_df_wide)

# Check if any rows have more than one species present
all_IRE_species_df_wide$MultipleSpecies <- rowSums(all_IRE_species_df_wide[, -c(1, 2)]) > 1

you were here 25/03 



###########################################################################################################
#save data in correct 1km and 10 format for model. 

# Add Irish Grid references and convert them to uppercase
all_IRE_species_df$GridReference <- toupper(all_IRE_species_df$GridReference)

# Rename the dataframe by ending "_1km"
all_IRE_species_df_1km <- all_IRE_species_df

# Round the references to keep the first four digits of the number (e.g., 'F456054' -> 'F4560')
all_IRE_species_df_1km$GridReference <- 
  sub("([A-Z])([0-9]{4})[0-9]*", "\\1\\2", all_IRE_species_df_1km$GridReference)

#save data
st_write(all_IRE_species_df_1km, "1.data/1.2.processed/all_IRE_species_df_1km.csv", delete_layer = TRUE)

View(all_IRE_species_df_1km)

# Repeate with 10km grid cells (2 digits)
all_IRE_species_df_10km <- all_IRE_species_df

# Round the references to keep the first 2 digits of the number (e.g., 'F456054' -> 'F45')
all_IRE_species_df_10km$GridReference <- 
  sub("([A-Z])([0-9]{2})[0-9]*", "\\1\\2", all_IRE_species_df_10km$GridReference)
 View(all_IRE_species_df_10km)

#save data
st_write(all_IRE_species_df_10km, "1.data/1.2.processed/all_IRE_species_df_10km.csv", delete_layer = TRUE)

View(all_IRE_species_df_10km)

###########################################################################################################
 
