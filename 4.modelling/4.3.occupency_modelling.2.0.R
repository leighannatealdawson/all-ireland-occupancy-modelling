library(unmarked)
library(MuMIn)
library(AICcmodavg)
library(ggplot2)
library(tidyverse)

#Try the 1km buffer and grid 
# remember we can always smooth the outputs later if we want to 

# import the 1km grid with data
# read in detection none detection data 
y <- read.csv("6.provided-scripts/pinemarten201520182020noname.csv")
y <- as.matrix(y)

str(y)
# consider which szie buffer we want to use for our analysis 
lcm_data_1km <- read.csv("1.data/1.2.processed/lcm_df_1km_buffer_3035.csv")

str(lcm_data_1km)

# note a stack data fromat is being used here, so each row is a unique site and year combination

#  therefore we MUST include year in each site covariate as detection and occupancy probabilities

#####################################################################
# 1.create occupancy covariates  

# occasion number covariate
#' this will allow us to acount for a change in samelling over time. e.g. bait may be more attractive 
#' at the start of the sampling period than at the end once it has been eaten etc.
occ <- y
for (i in 1:nrow(y)) {
  tmp <- y[i, ]
  n.samp <- sum(!is.na(tmp))
  tmp[!is.na(tmp)] <- 1:n.samp
  occ[i, ] <- tmp
}

View(occ)

# Create occupancy covariates list 

#'# Bait covariate
#' note in samelling in 2018 sunflowers only were used as bait, in 2015 and 2020 both sunflowers and peanuts were used
#' likely impacting detection probability, so we will include this as a covariate

siteCovs$bait[siteCovs$bait == "sunflower"] <- 0
siteCovs$bait[siteCovs$bait == "sunflower and peanuts"] <- 1


# create observation covariates list
# NOTE these vary by site and observation (unlike site covariates which are constant for each site)
obvsCov <-
  list(
    occ = occ )

# check formatting is correct - observation covariates should mirror the observation data (y)
str(y)
str(occ)

##############################################################################  
# 2. Create covarite groupings 
# Create new columns by summing relevant CLC classes into conifer, mixedwoodland, open none agriculture,
#' scrub, urban, greenurban, water
# all agriculture execpt pasture 
siteCovs$agrinonpas <- rowSums(siteCovs[, c("CLC_211", "CLC_242", "CLC_243")], na.rm = TRUE)

# pasture land cover 
siteCovs$pasture <- siteCovs$CLC_231

# conifer including conifer and transitional woodland 
siteCovs$conifer <- rowSums(siteCovs[, c("CLC_312", "CLC_324")], na.rm = TRUE)

# mixed wood including broadleaf and mixed forest 
siteCovs$mixedwood <- rowSums(siteCovs[, c("CLC_311", "CLC_313")], na.rm = TRUE)

# open non agriculture including natural grassland, peat bog, moore and heathland
siteCovs$opennoneagri <- rowSums(siteCovs[, c("CLC_321", "CLC_412", "CLC_322")], na.rm = TRUE)

# urban - all urban cover types 
siteCovs$urban <- rowSums(siteCovs[, c("CLC_111", "CLC_112", "CLC_121", "CLC_123", 
                                       "CLC_124", "CLC_131", "CLC_132", "CLC_133", "CLC_142")], na.rm = TRUE)

# make a table of each table categoies mean and sd and range 
# List the columns you want to summarise
groupings <- c("agrinonpas", "pasture", "conifer", "mixedwood", 
               "opennoneagri", "urban")

# Roads  

# Rivers 

# Use sapply to generate the summary table
summary_table <- t(sapply(siteCovs[, groupings], function(x) {
  c(Mean = mean(x, na.rm = TRUE),
    SD = sd(x, na.rm = TRUE),
    Min = min(x, na.rm = TRUE),
    Max = max(x, na.rm = TRUE))
}))
# Print the summary table
print(summary_table)
