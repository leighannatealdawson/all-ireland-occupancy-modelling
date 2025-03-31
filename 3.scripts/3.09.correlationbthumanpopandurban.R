#this file will compare urban corrine data from 1km and northern irish census data 

# Load necessary libraries
library(readr)
library(tidyverse)
library(dplyr)
library(stringr)
library(ggplot2)
library(GGally)
library(dplyr)
library(nortest)
#import the data
corine_data <- read_csv("1.data/1.3.processedinarc/1kmwidecorine.csv")
census_data <- read_csv("1.data/1.1.raw/humanpopulationdata/NorthernIrishCensusGrid/census-2021-grid-square-product-1km-full-dataset.csv")


View(corine_data)
View(census_data)

#streamline census_data as there are hundres of columns
census_data <- census_data %>%
  select(gridid, HOUSEHOLDS, PERSONS)

# create a new col in corine called percenturban (cols start with 1) 
# note general definition of artificial surfaces being used currently
corine_data <- corine_data %>%
  mutate(urban = rowSums(across(starts_with(c("1"))), na.rm = TRUE))

View(corine_data)
 

# rename col Geography to gridid to match census data
census_data <- census_data %>% rename(gridid = Geography)

# clean the gridid column in census_data (remove spaces from gridid) to match corrine data
census_data <- census_data %>%  mutate(gridid = str_replace_all(gridid, " ", ""))

# check changes
View(census_data)

# merge datasets by gridid 
#note all = FALSE means only rows with matching gridid will be kept so only grid cells from NI 
corine_census <- merge(corine_data, census_data, by = "gridid", all = FALSE)

#check this worked as intended  
#count rows 
nrow(census_data)
nrow(corine_census)


# test both varibles for normaility 
ad.test(corine_census$urban)
ad.test(corine_census$PERSONS)

qqnorm(corine_census$urban, main = "QQ Plot of Urban Data")
qqnorm(corine_census$PERSONS, main = "QQ Plot of Population Data")
qqnorm(corine_census$HOUSEHOLDS, main = "QQ Plot of Household Data")

hist(corine_census$urban, breaks = 30, probability = TRUE, 
     main = "Histogram of Urban Data", xlab = "Urban Data")
hist(corine_census$PERSONS, breaks = 30, probability = TRUE, 
     main = "Histogram of Population Data", xlab = "Population Data")

# both variables not normal so use spearman correlation will be used 
cor(corine_census$urban, corine_census$PERSONS, use = "complete.obs", method = "spearman")
cor(corine_census$urban, corine_census$HOUSEHOLDS, use = "complete.obs", method = "spearman")


# plot correlation of this data 
#correlation between ubran and persons 
ggplot(corine_census, aes(x = urban, y = PERSONS)) +
  geom_point(alpha = 0.6, color = "blue") +  # Scatter points
  geom_smooth(method = "lm", color = "red", se = FALSE) +  # Trend line
  labs(title = "Correlation between % Urban and Population",
       x = "% Urban",
       y = "Population") +
  theme_minimal()

#correlation between urban and households
ggplot(corine_census, aes(x = urban, y = HOUSEHOLDS)) +
  geom_point(alpha = 0.6, color = "blue") +  # Scatter points
  geom_smooth(method = "lm", color = "red", se = FALSE) +  # Trend line
  labs(title = "Correlation between % Urban and Population",
       x = "% Urban",
       y = "Population") +
  theme_minimal()




ggpairs(corine_census, columns = c("urban", "PERSONS", "HOUSEHOLDS"))

#Really strong correlation between urban and persons/households. 
# find correlation between col 311 and 312 
cor(corine_census$X311, corine_census$X312, use = "complete.obs")

ggplot(corine_census, aes(x = `311`, y = `312`)) +
  geom_point(alpha = 0.6, color = "blue") +  # Scatter points
  geom_smooth(method = "lm", color = "red", se = FALSE) +  # Trend line
  labs(title = "Correlation between Column 311 and Column 312",
       x = "Column 311",
       y = "Column 312") +
  theme_minimal()

cor(corine_census$`311`, corine_census$`312`, use = "complete.obs")

#create qqplot 
qqnorm(corine_census$`311`)
