library(dplyr)

# Example dataset
example_data <- data.frame(
  X10km_square = c("A", "A", "B", "B", "C"),
  Day1 = c(0, 0, 0, 1, NA),
  Day2 = c(0, NA, 0, 1, 0),
  Day3 = c(NA, 0, 0, NA, 0)
)

print("Original Data:")
print(example_data)
View(example_data)
# Merge rows by X10km_square, keeping the max value for each day
merged_example <- example_data %>%
  group_by(X10km_square) %>%
  summarise(across(starts_with("Day"), max, na.rm = TRUE), .groups = "drop")

# Convert -Inf (from na.rm = TRUE with all NAs) back to NA
merged_example[merged_example == -Inf] <- NA

# Recalculate overall_pa_dummy (summarizing presence per day)
merged_example$overall_pa_dummy <- rowSums(merged_example[, grep("^Day", names(merged_example))], na.rm = TRUE)
merged_example$overall_pa_dummy[merged_example$overall_pa_dummy > 1] <- 1

# Remove duplicates if there are any after summarizing
final_example <- merged_example %>%
  distinct(X10km_square, .keep_all = TRUE)

print("Final Merged and Deduplicated Data:")
print(final_example)
