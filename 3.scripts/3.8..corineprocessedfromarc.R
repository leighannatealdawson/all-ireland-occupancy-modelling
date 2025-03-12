# Load necessary library
library(readr)

library(dplyr)
library(tidyr)

# Define the file path
file_path <- "1.data/1.3.processedinarc/widecorine.csv"

# Import the data
widecorine_data <- read_csv(file_path)

# Display the first few rows of the data
head(widecorine_data)
colnames(widecorine_data)

#add cols F111:F523
# Add columns F111:F523 and create a new column called total_ha
widecorine_data$total_ha <- rowSums(widecorine_data$F111:F523, na.rm = TRUE)

# Display the first few rows of the updated data
View(widecorine_data)


widecorine_data <- widecorine_data %>%
  unite("Combined_Column", starts_with("F"), sep = "_", na.rm = TRUE)
 


 widecorine_data$Combined_Column <- apply(widecorine_data[, c("F111", "F112", "F121", "F122", "F123", "F124", "F131", "F132", 
                                   "F133", "F141", "F142", "F211", "F222", "F231", "F242", "F243", 
                                   "F311", "F312", "F313", "F321", "F322", "F324", "F331", "F332", 
                                   "F333", "F334", "F411", "F412", "F421", "F423", "F511", "F512", 
                                   "F521", "F522", "F523")], 1, function(x) paste(na.omit(x), collapse = "_"))


# show all unqiue in corrine  