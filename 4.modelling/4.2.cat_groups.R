siteCovs5km  <- read.csv("1.data/1.2.processed/lcm_df_5km_buffer_3035.csv")
colnames(siteCovs5km)
siteCovs <- siteCovs5km

library(GGally)
library(corrplot)

# List of your CLC variable names
clc_vars <- c("CLC_111", "CLC_112", "CLC_121", "CLC_122", "CLC_123", "CLC_124", 
              "CLC_131", "CLC_132", "CLC_133", "CLC_141", "CLC_142", "CLC_211", 
              "CLC_222", "CLC_231", "CLC_242", "CLC_243", "CLC_311", "CLC_312", 
              "CLC_313", "CLC_321", "CLC_322", "CLC_324", "CLC_332", "CLC_333", 
              "CLC_411", "CLC_412", "CLC_423", "CLC_511", "CLC_512", "CLC_522")
names(clc_vars)
# Subset the data frame to include only these variables
clc_data <- siteCovs5km[ , clc_vars]

cor_matrix <- cor(clc_data, use = "complete.obs", method = "pearson")


# Plot the correlation matrix
corrplot(cor_matrix, method = "color", tl.cex = 0.6)

# Plot with numbers and color
corrplot(cor_matrix,
         method = "color",        # Use color shading
         type = "upper",          # Show only upper triangle
         addCoef.col = "black",   # Add correlation coefficients in black
         tl.cex = 0.8,            # Text label size
         number.cex = 0.6,        # Coefficient number size
         col = colorRampPalette(c("blue", "white", "red"))(200),
         diag = FALSE)            # Hide the diagonal

# Save the correlation plot to a file
        png("correlation_plot.png", width = 800, height = 600)
        corrplot(cor_matrix,
                 method = "color",        # Use color shading
                 type = "upper",          # Show only upper triangle
                 addCoef.col = "black",   # Add correlation coefficients in black
                 tl.cex = 0.8,            # Text label size
                 number.cex = 0.6,        # Coefficient number size
                 col = colorRampPalette(c("blue", "white", "red"))(200),
                 diag = FALSE)            # Hide the diagonal
        dev.off()

###############################################################################
#urban 

#  Define only CLC_1xx variables
clc_1xx_vars <- c("CLC_111", "CLC_112", "CLC_121", "CLC_122", "CLC_123",
                  "CLC_124", "CLC_131", "CLC_132", "CLC_133", "CLC_141", "CLC_142")

# Subset  data
clc_1xx_data <- siteCovs5km[, clc_1xx_vars]

# Compute correlation matrix
clc_1xx_cor <- cor(clc_1xx_data, use = "complete.obs", method = "pearson")

# Plot the correlation matrix 
corrplot(clc_1xx_cor,
         method = "color",
         type = "upper",
         addCoef.col = "black",
         tl.cex = 0.8,
         number.cex = 0.7,
         col = colorRampPalette(c("blue", "white", "red"))(200),
         diag = FALSE)


# Create a pairwise scatterplot matrix
ggpairs(clc_1xx_data,
        upper = list(continuous = "cor"),    # Show correlation in upper
        lower = list(continuous = "points"), # Show scatterplots in lower
        diag = list(continuous = "densityDiag")) # Density plots on diagonal

######################################################################################
#agri 
# Define only CLC_2xx variables (agri)
clc_2xx_vars <- c("CLC_211", "CLC_222", "CLC_231", "CLC_242", "CLC_243")

# Subset data
clc_2xx_data <- siteCovs5km[, clc_2xx_vars]

# Compute correlation matrix
clc_2xx_cor <- cor(clc_2xx_data, use = "complete.obs", method = "pearson")

# Plot the correlation matrix
corrplot(clc_2xx_cor,
         method = "color",
         type = "upper",
         addCoef.col = "black",
         tl.cex = 0.8,
         number.cex = 0.7,
         col = colorRampPalette(c("blue", "white", "red"))(200),
         diag = FALSE)

# Create a pairwise scatterplot matrix
ggpairs(clc_2xx_data,
        upper = list(continuous = "cor"),
        lower = list(continuous = "points"),
        diag = list(continuous = "densityDiag"))

#######################################################################################
#forest
# Define only CLC_3xx variables (forest amd peat bog)
clc_3xx_vars <- c("CLC_311", "CLC_312", "CLC_313", "CLC_321", "CLC_322",
                  "CLC_324", "CLC_332", "CLC_333" , "CLC_412")

clc_3xx_vars <- c("CLC_322", "CLC_412")
# Subset data
clc_3xx_data <- siteCovs5km[, clc_3xx_vars]

# Compute correlation matrix
clc_3xx_cor <- cor(clc_3xx_data, use = "complete.obs", method = "pearson")

# Plot the correlation matrix
corrplot(clc_3xx_cor,
         method = "color",
         type = "upper",
         addCoef.col = "black",
         tl.cex = 0.8,
         number.cex = 0.7,
         col = colorRampPalette(c("blue", "white", "red"))(200),
         diag = FALSE)

# Create a pairwise scatterplot matrix
ggpairs(clc_3xx_data,
        upper = list(continuous = "cor"),
        lower = list(continuous = "points"),
        diag = list(continuous = "densityDiag"))

siteCovs$conifer <- siteCovs$CLC_312
siteCovs$morepeat <- rowSums(siteCovs[, c("CLC_322", "CLC_412")], na.rm = TRUE)

#######################################################################################
# Load required package
library(dplyr)

# Import additional data
allirelandgrid <- read.csv("1.data/1.3.processedinarc/compiled10km.csv")

# Display the first few rows
View(allirelandgrid)


# Rename columns that start with "X" to start with "CLC_"
allirelandgrid <- allirelandgrid %>%
  rename_with(.cols = starts_with("X"),
              .fn = ~ paste0("CLC_", sub("^X", "", .)))

allirelandgrid$x <- allirelandgrid$CLC_x


colnames(allirelandgrid)
 # Define the columns to keep
cols_to_keep <- c(
  "gridid", "CLC_321", "CLC_412", "CLC_231", "CLC_242", "CLC_322",
  "CLC_423", "CLC_243", "CLC_333", "CLC_331", "CLC_142", "CLC_522", "CLC_512",
  "CLC_112", "CLC_324", "CLC_312", "CLC_124", "CLC_421", "CLC_121", "CLC_131",
  "CLC_311", "CLC_332", "CLC_313", "CLC_411", "CLC_211", "CLC_334", "CLC_511",
  "CLC_141", "CLC_111", "CLC_132", "CLC_133", "CLC_122", "CLC_123", "CLC_521",
  "CLC_222", "total", "CLC_x", "CLC_xcood", "ycood"
)

# Subset the dataframe to include only those columns
allirelandgrid <- allirelandgrid[, cols_to_keep]
View(allirelandgrid)

# List of your CLC variable names
clc_vars_allireland <- c("CLC_111", "CLC_112", "CLC_121", "CLC_122", "CLC_123", "CLC_124", 
              "CLC_131", "CLC_132", "CLC_133", "CLC_141", "CLC_142", "CLC_211", 
              "CLC_222", "CLC_231", "CLC_242", "CLC_243", "CLC_311", "CLC_312", 
              "CLC_313", "CLC_321", "CLC_322", "CLC_324", "CLC_332", "CLC_333", 
              "CLC_411", "CLC_412", "CLC_423", "CLC_511", "CLC_512", "CLC_522")
names(clc_vars_allireland)
# Subset the data frame to include only these variables
clc_data <- allirelandgrid[ , clc_vars_allireland]

cor_matrix <- cor(clc_data, use = "complete.obs", method = "spearman")


# Plot the correlation matrix
corrplot(cor_matrix, method = "color", tl.cex = 0.6)

# Plot with numbers and color
corrplot(cor_matrix,
         method = "color",        # Use color shading
         type = "upper",          # Show only upper triangle
         addCoef.col = "black",   # Add correlation coefficients in black
         tl.cex = 0.8,            # Text label size
         number.cex = 0.6,        # Coefficient number size
         col = colorRampPalette(c("blue", "white", "red"))(200),
         diag = FALSE)            # Hide the diagonal

#################################################################################
#urban
#  Define only CLC_1xx variables
clc_1xx_vars_allireland <- c("CLC_111", "CLC_112", "CLC_121", "CLC_122", "CLC_123",
                  "CLC_124", "CLC_131", "CLC_132", "CLC_133", "CLC_141", "CLC_142")

# Subset  data
clc_1xx_data_allireland <- allirelandgrid[, clc_1xx_vars_allireland]

#compute correlation matrix   
clc_1xx_cor_allireland <- cor(clc_1xx_data_allireland, use = "complete.obs", method = "spearman")

# Plot the correlation matrix
corrplot(clc_1xx_cor_allireland,  
         method = "color",
         type = "upper",
         addCoef.col = "black",
         tl.cex = 0.8,
         number.cex = 0.7,
         col = colorRampPalette(c("blue", "white", "red"))(200),
         diag = FALSE)

# Create a pairwise scatterplot matrix
ggpairs(clc_1xx_data_allireland,
        upper = list(continuous = "cor"),    # Show correlation in upper
        lower = list(continuous = "points"), # Show scatterplots in lower
        diag = list(continuous = "densityDiag")) # Density plots on diagonal

#######################################################################################
#agri
# Define only CLC_2xx variables (agri)
clc_2xx_vars_allireland <- c("CLC_211", "CLC_222", "CLC_231", "CLC_242", "CLC_243")

# subset data
clc_2xx_data_allireland <- allirelandgrid[, clc_2xx_vars_allireland]

# Compute correlation matrix 
clc_2xx_cor_allireland <- cor(clc_2xx_data_allireland, use = "complete.obs", method = "pearson")

# Plot the correlation matrix
corrplot(clc_2xx_cor_allireland,
         method = "color",
         type = "upper",
         addCoef.col = "black",
         tl.cex = 0.8,
         number.cex = 0.7,
         col = colorRampPalette(c("blue", "white", "red"))(200),
         diag = FALSE)

# Create a pairwise scatterplot matrix
ggpairs(clc_2xx_data_allireland,
        upper = list(continuous = "cor"),
        lower = list(continuous = "points"),
        diag = list(continuous = "densityDiag"))

########################################################################################
#forest
# Define only CLC_3xx variables (forest and peat bog)
clc_3xx_vars_allireland <- c("CLC_311", "CLC_312", "CLC_313", "CLC_321", "CLC_322",
                  "CLC_324", "CLC_332", "CLC_333" , "CLC_412")

clc_3xx_vars_allireland <- c("CLC_322", "CLC_412")

# Subset data
clc_3xx_data_allireland <- allirelandgrid[, clc_3xx_vars_allireland]

# Compute correlation matrix
clc_3xx_cor_allireland <- cor(clc_3xx_data_allireland, use = "complete.obs", method = "pearson")

# Plot the correlation matrix
corrplot(clc_3xx_cor_allireland,
         method = "color",
         type = "upper",
         addCoef.col = "black",
         tl.cex = 0.8,
         number.cex = 0.7,
         col = colorRampPalette(c("blue", "white", "red"))(200),
         diag = FALSE)

# Create a pairwise scatterplot matrix
ggpairs(clc_3xx_data_allireland,
        upper = list(continuous = "cor"),
        lower = list(continuous = "points"),
        diag = list(continuous = "densityDiag"))

########################### 

cor(allirelandgrid$CLC_111, allirelandgrid$CLC_112, use = "complete.obs")
cor_value <- cor(allirelandgrid$CLC_111, allirelandgrid$CLC_112, use = "complete.obs")

ggplot(allirelandgrid, aes(x = CLC_111, y = CLC_112)) +
  geom_point(alpha = 0.5, color = "steelblue") +
  geom_smooth(method = "lm", se = FALSE, color = "darkred") +
   theme_minimal()

###################################################
# Select only CLC_ columns
clc_cols <- grep("^CLC_", colnames(allirelandgrid), value = TRUE)

# Sum each CLC column (ignoring NAs)
clc_totals <- colSums(allirelandgrid[ , clc_cols], na.rm = TRUE)

# Convert to a data frame for a neat table
clc_totals_df <- data.frame(Cover_Type = names(clc_totals),
                            Total = clc_totals)

# View the table
View(clc_totals_df)
##############################################################################################
# create new df of mean, min, max and Df of each clc type 
colnames(allirelandgrid)

# List of your specified column names
cols_to_summarise_CLC <- c("X231", "X243", "X312", "X324", "X412", "X512", "X111", "X112",
                       "X121", "X142", "X311", "X313", "X321", "X322", "X332", "X333",
                       "X122", "X211", "X242", "X421", "X423", "X522", "X131", "X411",
                       "X331", "X521", "X133", "X511", "X334", "X124", "X141", "X222",
                       "X132", "X123")

# Compute summary statistics using sapply
CLC_summary_all_ireland <- t(sapply(allirelandgrid[, cols_to_summarise_CLC], function(x) {
  c(Sum = sum(x, na.rm = TRUE),
    Mean = mean(x, na.rm = TRUE),
    SD = sd(x, na.rm = TRUE),
    Min = min(x, na.rm = TRUE),
    Max = max(x, na.rm = TRUE))
}))

# Convert to data frame and round for readability
CLC_summary_all_ireland <- as.data.frame(round(CLC_summary_all_ireland,10))

# Print the summary table
View(CLC_summary_all_ireland)
