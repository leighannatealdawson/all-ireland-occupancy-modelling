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
write.csv(PA_data_NI, file = "1.data/1.2.processed/1.2.2.PA_data_NI_1km.csv")