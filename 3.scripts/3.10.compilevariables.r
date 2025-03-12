#compiling the data variables 
#ending up with 1 df with all the variables needed for the analysis (corrine, populations etc)

# Load necessary libraries
library(sf)

# import data 
corine1km <- read.csv("1.data/1.3.processedinarc/1kmwidecorine.csv")
humanpop_data <- st_read("1.data/1.2.processed/processed_humanpop_ni_1kmgrid.shp")
# Print the first few rows of the data
print(head(humanpop_data))
 
