# reformatting the presece absence data for tpine martens from 2015-2020 data 
#### Intro ##### 
#clear envuronment
rm(list = ls())

# Load necessary libraries
library(tidyverse)
library(dplyr)
library(tidyr)
# Import data 
# Presence absence data for NI for PM 
PA_species_data_NI <- read.csv("6.provided-scripts/pinemarten201520182020noname.csv")
PA_site_data_NI <- read.csv("6.provided-scripts/habitatinfo201520182020sitesonly.csv")
# irish grid data for 1km and 10km cells in NI
irish_grid_1km_NI <- read.csv("1.data/1.3.processedinarc/irishgrid1kmwithDD_NI.csv")
irish_grid_10km_NI <- read.csv("1.data/1.3.processedinarc/irishgrid10kmwithDD_NI.csv")
#keep only the columns needed for the analysis (gridid, x and y)
irish_grid_1km <- irish_grid_1km %>%select(gridid, x, y)
irish_grid_10km <- irish_grid_10km %>%select(gridid, x, y)

#merge dfs by rownumber
PA_data_NI <- cbind(PA_species_data_NI, PA_site_data_NI)

# make all cols starting with Day. numeric 
PA_data_NI[, grep("^Day", names(PA_data_NI))] <- lapply(PA_data_NI[, grep("^Day", names(PA_data_NI))], as.numeric)
# Create new column 'overall_pa' by summing columns from Day.1 to Day.14, 
# then change all values greater than 1 to 1
PA_data_NI$overall_pa <- rowSums(PA_data_NI[, grep("^Day", names(PA_data_NI))], na.rm = TRUE)
PA_data_NI$overall_pa[PA_data_NI$overall_pa > 1] <- 1
View(PA_data_NI)
#rename variables in col year 1=2015, year 2=2018, year 3=2020
PA_data_NI <- PA_data_NI %>%
  mutate(year = recode(year, `1` = "2015", `2` = "2018", `3` = "2020"))

PA_data_NI$year <- as.numeric(PA_data_NI$year)

View(PA_data_NI)

# Remove spaces from the X1km_square column
PA_data_NI$X1km_square <- gsub(" ", "", PA_data_NI$X1km_square)

#summerise the data by gridid and year
PA_data_NI_summary <- PA_data_NI %>%
  group_by(X1km_square, year) %>%
  summarise(unique_presence = sum(overall_pa == 1, na.rm = TRUE)) %>%
  pivot_wider(names_from = year, values_from = unique_presence, names_prefix = "year_")


View(PA_data_NI_summary)


############################################################################################
# Detection History Matrix for Occupancy Modeling in unmarked
#made using PA data from 2015, 2018 and 2020
head(PA_data_NI)

#2015 
#filter by year = 2015, 2018, 2020
PA_data_NI_2015 <- PA_data_NI %>% filter(year == "2015") 
#check for repeated data 
PA_data_NI_2015 %>% group_by(X1km_square) %>% filter(n() > 1)

#2018
PA_data_NI_2018 <- PA_data_NI %>% filter(year == "2018")
#add row id 
PA_data_NI_2018 <- PA_data_NI_2018 %>% mutate(row_id = row_number())
View(PA_data_NI_2018)

#change all na to -1
PA_data_NI_2018[is.na(PA_data_NI_2018)] <- -1

#check for repeated data
d2018rep <-PA_data_NI_2018 %>% group_by(X1km_square) %>% filter(n() > 1)
View(d2018rep)

#recalculate overall_pa for 2018 with dummy na values(-1) so we can sort by max 
PA_data_NI_2018$overall_pa <- rowSums(PA_data_NI_2018[, grep("^Day", names(PA_data_NI_2018))], na.rm = TRUE)

#keep rows with the largest overall_pa value where gridID is replicated 
PA_data_NI_2018 <- PA_data_NI_2018 %>% group_by(X1km_square) %>%
 filter(overall_pa == max(overall_pa)) %>% ungroup()

#check for repeated data
d2018rep <-PA_data_NI_2018 %>% group_by(X1km_square) %>% filter(n() > 1)
View(d2018rep)

#tidy data change all -1 back to na and recalculate overall PA
PA_data_NI_2018[PA_data_NI_2018 == -1] <- NA
PA_data_NI_2018$overall_pa <- rowSums(PA_data_NI_2018[, grep("^Day", names(PA_data_NI_2018))], na.rm = TRUE)
PA_data_NI$overall_pa[PA_data_NI$overall_pa > 1] <- 1

View(PA_data_NI_2018)

#2020
# 2020
PA_data_NI_2020 <- PA_data_NI %>% filter(year == "2020")

# Add row id
PA_data_NI_2020 <- PA_data_NI_2020 %>% mutate(row_id = row_number())
View(PA_data_NI_2020)

# Change all NA to -1
PA_data_NI_2020[is.na(PA_data_NI_2020)] <- -1

# Check for repeated data
d2020rep <- PA_data_NI_2020 %>% group_by(X1km_square) %>% filter(n() > 1)
View(d2020rep)

# Recalculate overall_pa for 2020 with dummy NA values (-1) so we can sort by max
PA_data_NI_2020$overall_pa <- rowSums(PA_data_NI_2020[, grep("^Day", names(PA_data_NI_2020))], na.rm = TRUE)

# Keep rows with the largest overall_pa value where gridID is replicated
PA_data_NI_2020 <- PA_data_NI_2020 %>% group_by(X1km_square) %>%
  filter(overall_pa == max(overall_pa)) %>% ungroup()

# Check for repeated data again
d2020rep <- PA_data_NI_2020 %>% group_by(X1km_square) %>% filter(n() > 1)
View(d2020rep)

# Tidy data: change all -1 back to NA and recalculate overall PA
PA_data_NI_2020[PA_data_NI_2020 == -1] <- NA
PA_data_NI_2020$overall_pa <- rowSums(PA_data_NI_2020[, grep("^Day", names(PA_data_NI_2020))], na.rm = TRUE)

# Ensure overall_pa is capped at 1
PA_data_NI_2020$overall_pa[PA_data_NI_2020$overall_pa > 1] <- 1

View(PA_data_NI_2020)



# renameoverall pa to overall_pa_2015, overall_pa_2018, overall_pa_2020 to allow merging 
PA_data_NI_2015 <- PA_data_NI_2015 %>% rename(overall_pa_2015 = overall_pa)
PA_data_NI_2018 <- PA_data_NI_2018 %>% rename(overall_pa_2018 = overall_pa)
PA_data_NI_2020 <- PA_data_NI_2020 %>% rename(overall_pa_2020 = overall_pa)

#rename col Day.1 to Day.1_2018 etc 
PA_data_NI_2015 <- PA_data_NI_2015 %>%
  rename_with(~ paste0(., "_2015"), starts_with("Day."))
  PA_data_NI_2018 <- PA_data_NI_2018 %>%
  rename_with(~ paste0(., "_2018"), starts_with("Day."))
  PA_data_NI_2020 <- PA_data_NI_2020 %>%
  rename_with(~ paste0(., "_2020"), starts_with("Day."))



#check colnames changed 
colnames(PA_data_NI_2015)
colnames(PA_data_NI_2018)
colnames(PA_data_NI_2020)
# merge 3dfs by gridid 
# Merge the data frames by gridid
PA_data_merged <- PA_data_NI_2015 %>%
  left_join(PA_data_NI_2018, by = "X1km_square") %>%
  left_join(PA_data_NI_2020, by = "X1km_square")
View(PA_data_merged)
colnames(PA_data_merged)

#remove cols with duplicated names to tidy up the data
PA_data_merged <- PA_data_merged %>%
  select(-ends_with(".x"), -ends_with(".y"))

# Reorder columns
PA_data_NI <- PA_data_merged %>%
  select(
    X1km_square,      # Move X1km_square to the front
    overall_pa_2015,  # Overall PA for 2015
    overall_pa_2018,  # Overall PA for 2018
    overall_pa_2020,  # Overall PA for 2020
    bait, bog, broadleaf, built, conifer, grassland, dwarf, river, rough, # site covariates
    lat, long,        # Location info (lat/long) after site covariates
    people_km2,       # People per km2
    starts_with("Day.1_"), starts_with("Day.2_"), starts_with("Day.3_"), starts_with("Day.4_"),
    starts_with("Day.5_"), starts_with("Day.6_"), starts_with("Day.7_"), starts_with("Day.8_"),
    starts_with("Day.9_"), starts_with("Day.10_"), starts_with("Day.11_"), starts_with("Day.12_"),
    starts_with("Day.13_"), starts_with("Day.14_") # Group day columns together
  )

# View the reordered data
View(PA_data_NI)


#save dataset to processed folder called PA_data_NI_1km.csv
write.csv(PA_data_NI, file = "1.data/1.2.processed/PA_data_NI_1km.csv")
############################################################################################
# reformat for chapter 10.9 AHM 
# 1. Each row representing a unique site.
# 2. Columns for detection/non-detection (0/1) across multiple survey occasions (e.g., Day.1, Day.2, Day.3).
# 3. Site-level covariates such as habitat, elevation, latitude, and longitude.
#see below for detials 
fn <- file.path(find.package("AHMbook"), "extdata", "SwissSquirrels.txt")
data <- read.table(fn, header = TRUE)

# Import the dataset for pinemarten presence/absence data
pinemarten_data <- read.csv("6.provided-scripts/pinemarten201520182020noname.csv")
View(PA_data_NI)
#columns required -
# Creating a new dataframe with the required column names

# cols requiredgidcell1kn, x cood, y cood, ele (not including)), route (keep as 1 number ), 
# forest (311-313), detection1, detection2,detection3, date1 , date2, date3, duration1, duration2, duration3

overall P/A 2015 
total days sampleed (0 and 1) 
date 1 
colnames(PA_data_NI)
#calcualte total days sampled in each row for cells ending _2015 sand starting with Day (call col duration_2015)
use data 
input_data <- PA_data_NI

#create new column called duraction to assess survey effor per year 
input_data$duration_2015 <- rowSums(!is.na(input_data[, grepl("^Day.*2015", colnames(input_data))]), na.rm = TRUE)
input_data$duration_2018 <- rowSums(!is.na(input_data[, grepl("^Day.*2018", colnames(input_data))]), na.rm = TRUE)
input_data$duration_2020 <- rowSums(!is.na(input_data[, grepl("^Day.*2020", colnames(input_data))]), na.rm = TRUE)

#add new col called date for each year where 2015 = 1, 2018 = 2, 2020 = 3
input_data$date_2015 <- 1 
input_data$date_2018 <- 2
input_data$date_2020 <- 3
head(# Create the new dataframe input_data with selected columns
input_data <- input_data[, c("X1km_square", 
                             "overall_pa_2015", "overall_pa_2018", "overall_pa_2020",
                             "bog", "broadleaf", "built",
                             "conifer", "grassland", "dwarf", "river",
                             "rough", "lat", "long",
                             "duration_2015", "duration_2018", "duration_2020",
                             "date_2015", "date_2018", "date_2020")]

# View the first few rows of input_data to verify
head(input_data))

PA_data_NI$route <- 1

# Create the new dataframe input_data with selected columns
input_data <- input_data[, c("X1km_square", 
                             "overall_pa_2015", "overall_pa_2018", "overall_pa_2020",
                             "bog", "broadleaf", "built",
                             "conifer", "grassland", "dwarf", "river",
                             "rough", "lat", "long",
                             "duration_2015", "duration_2018", "duration_2020",
                             "date_2015", "date_2018", "date_2020" ,"route")]


# Rename the overall_pa columns to detection for each year
input_data <- input_data %>%
  rename(detection_2015 = overall_pa_2015,
         detection_2018 = overall_pa_2018,
         detection_2020 = overall_pa_2020)

# View the first few rows of input_data to verify
head(input_data)

# Save the input_data dataframe to a CSV file 
write.csv(input_data, file = "1.data/1.2.processed/input_data_test1km.csv")
############################################################################################
#'# Repeat the process for 10km squares
rm(list = ls())
#reimport data to check clean 
PA_species_data_NI <- read.csv("6.provided-scripts/pinemarten201520182020noname.csv")
PA_site_data_NI <- read.csv("6.provided-scripts/habitatinfo201520182020sitesonly.csv")

#merge dfs by rownumber
PA_data_NI <- cbind(PA_species_data_NI, PA_site_data_NI)

# Create a new column X10km_square by removing the last 2 characters and remove spaces 
PA_data_NI$X10km_square <- str_sub(PA_data_NI$X1km_square, 1, -3)
PA_data_NI$X10km_square <- gsub(" ", "", PA_data_NI$X10km_square)
View(PA_data_NI)

# Create new column 'overall_pa' by summing columns from Day.1 to Day.14, 
# then change all values greater than 1 to 1
PA_data_NI$overall_pa <- rowSums(PA_data_NI[, grep("^Day", names(PA_data_NI))], na.rm = TRUE)
PA_data_NI$overall_pa[PA_data_NI$overall_pa > 1] <- 1
View(PA_data_NI)
#rename variables in col year 1=2015, year 2=2018, year 3=2020
PA_data_NI <- PA_data_NI %>%
  mutate(year = recode(year, `1` = "2015", `2` = "2018", `3` = "2020"))

# 2015
# Filter by year = 2015, 
PA_data_NI_2015 <- PA_data_NI %>% filter(year == "2015") 
# Check for repeated data
PA_data_NI_2015_REP <-PA_data_NI_2015 %>% group_by(X10km_square) %>% filter(n() > 1)
PA_data_NI_2015_REP #Note 243 rows with duplicate X10km_square values

#change all na to -inf
PA_data_NI_2015[is.na(PA_data_NI_2015)] <- -Inf

View(PA_data_NI_2015)
#  Remove duplicates by X10km_square and keep the max value for each day
PA_data_NI_2015_processed <- PA_data_NI_2015 %>%
  group_by(X10km_square) %>%
  summarise(across(starts_with("Day"), max, na.rm = TRUE), .groups = "drop")

# change all -inf to na
PA_data_NI_2015_processed[PA_data_NI_2015_processed == -Inf] <- NA
# Recalculate overall_pa for 2015 capped at 1 
PA_data_NI_2015_processed$overall_pa <- rowSums(PA_data_NI_2015_processed[, grep("^Day", names(PA_data_NI_2015_processed))], na.rm = TRUE)
PA_data_NI_2015_processed$overall_pa[PA_data_NI_2015_processed$overall_pa > 1] <- 1
nrow(PA_data_NI_2015_processed)

View(PA_data_NI_2015_processed)

# 2018
# Filter by year = 2018
PA_data_NI_2018 <- PA_data_NI %>% filter(year == "2018") 
nrow(PA_data_NI_2018)
# Check for repeated data
PA_data_NI_2018_REP <- PA_data_NI_2018 %>% group_by(X10km_square) %>% filter(n() > 1)
PA_data_NI_2018_REP # Note the number of duplicate X10km_square values

# Change all NA to -Inf
PA_data_NI_2018[is.na(PA_data_NI_2018)] <- -Inf

View(PA_data_NI_2018)
head(PA_data_NI_2018)
# Remove duplicates by X10km_square and keep the max value for each day
PA_data_NI_2018_processed <- PA_data_NI_2018 %>%
  group_by(X10km_square) %>%
  summarise(across(starts_with("Day"), max, na.rm = TRUE), .groups = "drop")

# Change all -Inf to NA
PA_data_NI_2018_processed[PA_data_NI_2018_processed == -Inf] <- NA

# Recalculate overall_pa for 2018 capped at 1
PA_data_NI_2018_processed$overall_pa <- rowSums(PA_data_NI_2018_processed[, grep("^Day", names(PA_data_NI_2018_processed))], na.rm = TRUE)
PA_data_NI_2018_processed$overall_pa[PA_data_NI_2018_processed$overall_pa > 1] <- 1

nrow(PA_data_NI_2018_processed)

View(PA_data_NI_2018_processed)

# 2020
# Filter by year = 2020
PA_data_NI_2020 <- PA_data_NI %>% filter(year == "2020") 
nrow(PA_data_NI_2020)
# Check for repeated data
PA_data_NI_2020_REP <- PA_data_NI_2020 %>% group_by(X10km_square) %>% filter(n() > 1)
PA_data_NI_2020_REP # Note the number of duplicate X10km_square values

# Change all NA to -Inf
PA_data_NI_2020[is.na(PA_data_NI_2020)] <- -Inf

View(PA_data_NI_2020)

# Remove duplicates by X10km_square and keep the max value for each day
PA_data_NI_2020_processed <- PA_data_NI_2020 %>%
  group_by(X10km_square) %>%
  summarise(across(starts_with("Day"), max, na.rm = TRUE), .groups = "drop")

# Change all -Inf to NA
PA_data_NI_2020_processed[PA_data_NI_2020_processed == -Inf] <- NA

# Recalculate overall_pa for 2020 capped at 1
PA_data_NI_2020_processed$overall_pa <- rowSums(PA_data_NI_2020_processed[, grep("^Day", names(PA_data_NI_2020_processed))], na.rm = TRUE)
PA_data_NI_2020_processed$overall_pa[PA_data_NI_2020_processed$overall_pa > 1] <- 1

nrow(PA_data_NI_2020_processed)

View(PA_data_NI_2020_processed)

######################################## 

merge all PA_data_NI_YEAR_processed df 
# Merge datasets by X10km_square
PA_data_merged <- PA_data_NI_2015_processed %>%
  left_join(PA_data_NI_2018_processed, by = "X10km_square") %>%
  left_join(PA_data_NI_2020_processed, by = "X10km_square")
View(PA_data_merged)

############################################################################################
#check for duplicates in x10km square in each df
PA_data_NI_2015_processed %>% group_by(X10km_square) %>% filter(n() > 1)
PA_data_NI_2018_processed %>% group_by(X10km_square) %>% filter(n() > 1)
PA_data_NI_2020_processed %>% group_by(X10km_square) %>% filter(n() > 1)

# Rename overall_pa for merging
PA_data_NI_2015_processed <- PA_data_NI_2015 %>% rename(overall_pa_2015_processed = overall_pa)
PA_data_NI_2018_processed <- PA_data_NI_2018 %>% rename(overall_pa_2018_processed = overall_pa)
PA_data_NI_2020_processed <- PA_data_NI_2020 %>% rename(overall_pa_2020_processed = overall_pa)


n_distinct(PA_data_NI_2015$X10km_square) # Number of unique X10km_square in 2015 dataset
n_distinct(PA_data_NI_2018$X10km_square) # Number of unique X10km_square in 2018 dataset
n_distinct(PA_data_NI_2020$X10km_square) # Number of unique X10km_square in 2020 dataset

#rename dfs 
PA_data_NI_2015 <- PA_data_NI_2015_processed 
PA_data_NI_2018 <- PA_data_NI_2018_processed
PA_data_NI_2020 <- PA_data_NI_2020_processed 
# Rename Day columns for each year
PA_data_NI_2015 <- PA_data_NI_2015 %>%
  rename_with(~ paste0(., "_2015"), starts_with("Day."))
PA_data_NI_2018 <- PA_data_NI_2018 %>%
  rename_with(~ paste0(., "_2018"), starts_with("Day."))
PA_data_NI_2020 <- PA_data_NI_2020 %>%
  rename_with(~ paste0(., "_2020"), starts_with("Day."))
colnames(PA_data_NI_2015)

# Removing the extra columns from data at these are no longer accurate 
PA_data_NI_2015 <- PA_data_NI_2015[, !(names(PA_data_NI_2015) %in% c("Site", "bog", "broadleaf", 
                                                                      "built", "conifer", "dwarf", "rough", 
                                                                      "river", "people_km2", "grassland", 
                                                                      "lat", "long", "year" , "bait", "X1km_square"  ))]


PA_data_NI_2018 <- PA_data_NI_2018[, !(names(PA_data_NI_2018) %in% c("Site", "bog", "broadleaf",
                                                                      "built", "conifer", "dwarf", "rough",
                                                                      "river", "people_km2", "grassland",
                                                                      "lat", "long", "year", "bait", "X1km_square"))]

PA_data_NI_2020 <- PA_data_NI_2020[, !(names(PA_data_NI_2020) %in% c("Site", "bog", "broadleaf",  
                                                                      "built", "conifer", "dwarf", "rough", 
                                                                      "river", "people_km2", "grassland", 
                                                                      "lat", "long", "year" , "bait", "X1km_square"  ))]  



check for duplicates in x10km square in each df
PA_data_NI_2015 %>% group_by(X10km_square) %>% filter(n() > 1)
View(PA_data_NI_2015)
# Merge datasets by X10km_square
PA_data_merged <- PA_data_NI_2015 %>%
  left_join(PA_data_NI_2018, by = "X10km_square") %>%
  left_join(PA_data_NI_2020, by = "X10km_square")
   View(PA_data_merged)



############################################################################################
############################################################################################
# Check for duplicates in X10km_square for each dataset before processing
PA_data_NI_2015_processed %>% group_by(X10km_square) %>% filter(n() > 1)
PA_data_NI_2018_processed %>% group_by(X10km_square) %>% filter(n() > 1)
PA_data_NI_2020_processed %>% group_by(X10km_square) %>% filter(n() > 1)

# Rename overall_pa for each year to avoid column name conflicts during the merge
PA_data_NI_2015_processed <- PA_data_NI_2015_processed %>% rename(overall_pa_2015_processed = overall_pa)
PA_data_NI_2018_processed <- PA_data_NI_2018_processed %>% rename(overall_pa_2018_processed = overall_pa)
PA_data_NI_2020_processed <- PA_data_NI_2020_processed %>% rename(overall_pa_2020_processed = overall_pa)

# Check the number of unique X10km_square values in each dataset
n_distinct(PA_data_NI_2015_processed$X10km_square)  # Unique X10km_square in 2015 dataset
n_distinct(PA_data_NI_2018_processed$X10km_square)  # Unique X10km_square in 2018 dataset
n_distinct(PA_data_NI_2020_processed$X10km_square)  # Unique X10km_square in 2020 dataset

# Rename Day columns for each dataset to reflect the year in the column names
PA_data_NI_2015_processed <- PA_data_NI_2015_processed %>%
  rename_with(~ paste0(., "_2015"), starts_with("Day"))
PA_data_NI_2018_processed <- PA_data_NI_2018_processed %>%
  rename_with(~ paste0(., "_2018"), starts_with("Day"))
PA_data_NI_2020_processed <- PA_data_NI_2020_processed %>%
  rename_with(~ paste0(., "_2020"), starts_with("Day"))

# Remove unnecessary columns that are no longer relevant
PA_data_NI_2015_processed <- PA_data_NI_2015_processed[, !(names(PA_data_NI_2015_processed) %in% 
                                                            c("Site", "bog", "broadleaf", "built", "conifer", "dwarf", 
                                                              "rough", "river", "people_km2", "grassland", "lat", 
                                                              "long", "year", "bait", "X1km_square"))]

PA_data_NI_2018_processed <- PA_data_NI_2018_processed[, !(names(PA_data_NI_2018_processed) %in% 
                                                            c("Site", "bog", "broadleaf", "built", "conifer", "dwarf", 
                                                              "rough", "river", "people_km2", "grassland", "lat", 
                                                              "long", "year", "bait", "X1km_square"))]

PA_data_NI_2020_processed <- PA_data_NI_2020_processed[, !(names(PA_data_NI_2020_processed) %in% 
                                                            c("Site", "bog", "broadleaf", "built", "conifer", "dwarf", 
                                                              "rough", "river", "people_km2", "grassland", "lat", 
                                                              "long", "year", "bait", "X1km_square"))]

# Check for any duplicates in X10km_square post-processing
PA_data_NI_2015_processed %>% group_by(X10km_square) %>% filter(n() > 1)
PA_data_NI_2018_processed %>% group_by(X10km_square) %>% filter(n() > 1)
PA_data_NI_2020_processed %>% group_by(X10km_square) %>% filter(n() > 1)

# Merge datasets by X10km_square using left join
PA_data_merged <- PA_data_NI_2015_processed %>%
  left_join(PA_data_NI_2018_processed, by = "X10km_square") %>%
  left_join(PA_data_NI_2020_processed, by = "X10km_square")

# View the merged data
View(PA_data_merged)
nrow(PA_data_NI_2020_processed)

save data 
write.csv(PA_data_merged, file = "1.data/1.2.processed/PA_data_NI_10km.csv")
############################################################################################

