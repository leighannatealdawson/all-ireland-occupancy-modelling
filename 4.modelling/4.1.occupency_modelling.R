 rm(list = ls())

library(unmarked)
library(MuMIn)
library(AICcmodavg)
library(ggplot2)
library(tidyverse)




lcm_data_1km <- read.csv("1.data/1.2.processed/lcm_df_1km_buffer_3035.csv")

View(lcm_data_1km)


siteCovs  <- read.csv("1.data/1.2.processed/lcm_df_1km_buffer_3035.csv")
siteCovs5km  <- read.csv("1.data/1.2.processed/lcm_df_5km_buffer_3035.csv")
siteCovs10km  <- read.csv("1.data/1.2.processed/lcm_df_10km_buffer_3035.csv")


# Get the column names from each dataframe
cols_1km <- colnames(siteCovs)
cols_5km <- colnames(siteCovs5km)
cols_10km <- colnames(siteCovs10km)

# Get a unique set of all column names
all_cols <- sort(unique(c(cols_1km, cols_5km, cols_10km)))

# Create a data frame indicating presence (TRUE/FALSE) of each column in each dataset
col_comparison <- data.frame(
  Column = all_cols,
  In_1km = all_cols %in% cols_1km,
  In_5km = all_cols %in% cols_5km,
  In_10km = all_cols %in% cols_10km
)

View(col_comparison)


# Load necessary library
library(dplyr)

# Define the CLC columns (all starting with "CLC_")
clc_cols <- grep("^CLC_", colnames(siteCovs5km), value = TRUE)

# Create summary of total area per CLC type for each buffer size
clc_totals <- data.frame(
  CLC_Type = clc_cols,
  Area_1km = sapply(clc_cols, function(clc) sum(siteCovs[[clc]], na.rm = TRUE)),
  Area_5km = sapply(clc_cols, function(clc) sum(siteCovs5km[[clc]], na.rm = TRUE)),
  Area_10km = sapply(clc_cols, function(clc) sum(siteCovs10km[[clc]], na.rm = TRUE))
)
# View table
print(clc_totals)
########################################################################################################


# read in detection none detection data 
y <- read.csv("6.provided-scripts/pinemarten201520182020noname.csv")
y <- as.matrix(y)

nrow(y)
head(y)
str(y) # this doesnt look right?? 

# lets use the 5km buffer to preserve resolution 
 siteCovs  <- read.csv("1.data/1.2.processed/lcm_df_5km_buffer_3035.csv")
str(siteCovs)
# note a stack data fromat is being used here, so each row is a unique site and year combination


#  therefore we MUST include year in each site covariate as detection and occupancy probabilities



# create some detection covariates, for example: occasion number covariate
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
##########################################################################################################
#'# Bait covariate
#' note in samelling in 2018 sunflowers only were used as bait, in 2015 and 2020 both sunflowers and peanuts were used
#' likely impacting detection probability, so we will include this as a covariate
#' 
# format bait as dummy cov
siteCovs$bait[siteCovs$bait == "sunflower"] <- 0
siteCovs$bait[siteCovs$bait == "sunflower and peanuts"] <- 1

#############################################################################################################################
# create observation covariates list
# NOTE these vary by site and observation (unlike site covariates which are constant for each site)
obvsCov <-
  list(
    occ = occ )


    # check formatting is correct - observation covariates should mirror the observation data (y)
str(y)
str(occ)

# Create new columns by summing relevant CLC classes into conifer, mixedwoodland, open none agriculture,
#' scrub, urban, greenurban, water

siteCovs$conifer <- siteCovs$CLC_312

siteCovs$mixedwood <- rowSums(siteCovs[, c("CLC_311", "CLC_313")], na.rm = TRUE)

siteCovs$opennoneagri <- rowSums(siteCovs[, c("CLC_321",  "CLC_332", "CLC_412")], na.rm = TRUE)

siteCovs$scrub <- rowSums(siteCovs[, c("CLC_322", "CLC_324")], na.rm = TRUE)

siteCovs$urban <- rowSums(siteCovs[, c("CLC_111", "CLC_112", "CLC_121", "CLC_122", "CLC_123", 
                                       "CLC_124", "CLC_131", "CLC_132", "CLC_133")], na.rm = TRUE)

siteCovs$greenurban <- rowSums(siteCovs[, c("CLC_141", "CLC_142")], na.rm = TRUE)

siteCovs$water <- rowSums(siteCovs[, c("CLC_423", 
                                       "CLC_511", "CLC_512", "CLC_522", "CLC_523")], na.rm = TRUE)

siteCovs$agri <- rowSums(siteCovs[, c("CLC_211", "CLC_222", "CLC_231", "CLC_242", "CLC_243")], na.rm = TRUE)


##########################################################################################################################
# plot and check distribution of covariates sampled 
#' NOTE many of these covarates will show we have lots of areas sampled with very little of our covaraites, this okay as long as we scale the data 
#' the importand thing to avoid is having big gaps 
hist(siteCovs$CLC_111)

# check distribution of covariates sampled
hist(siteCovs$conifer) # looks good 
hist(siteCovs$mixedwood) #looks good
hist(siteCovs$opennoneagri) 
hist(siteCovs$scrub)
hist(siteCovs$urban) # This one produced some gap, is this a problem? 

hist(siteCovs$greenurban)
hist(siteCovs$water) # This one produced some gap, is this a problem? 


hist(siteCovs$agri) #' This one prodeuces a very different shaped histogram, should i still scale 
#' as with the others? Should i consider reclassifying this land cover class?  
#' here is how each class looks separately, 
hist(siteCovs$CLC_211)
hist(siteCovs$CLC_222)
hist(siteCovs$CLC_231)
hist(siteCovs$CLC_242)
hist(siteCovs$CLC_243)
#add total for each CLC class for agri 
colSums(siteCovs[, c("CLC_211", "CLC_222", "CLC_231", "CLC_242", "CLC_243")], na.rm = TRUE)
# play around here more 
hist(scale(siteCovs$conifer))
hist(scale(siteCovs$mixedwood))
hist(scale(siteCovs$opennoneagri))
hist(scale(siteCovs$scrub))
hist(scale(siteCovs$urban))
hist(scale(siteCovs$greenurban))
hist(scale(siteCovs$water))
hist(scale(siteCovs$agri))

#create unmarked df
umf2 = unmarkedFrameOccu(y = y, siteCovs = siteCovs, obsCovs = obvsCov)
# Warning message: siteCovs contains characters. Converting them to factors.
str(umf2)
head(siteCovs)
colnames(umf2@siteCovs)
View(umf2@siteCovs

str(obsCovs(umf2))

unmaked 
#' mod1 <- occu(~ "covariates that determine detection probability" 
#' 
#' scale so data is transformed so it has a mean of 0 and SD/unit variance of 1- we can do this in the model when using unmarked but not others
#' with unmarked we can scale here and it will automatically scale covariates when we make predictions (this makes life easier)
#' 
mod1 <- occu(~ scale(occ) + year + bait
             ~ scale(conifer) 
             + scale(mixedwood) 
             + scale(greenurban) 
             + scale(scrub) 
             + scale(urban) 
             + scale(agri)
             + scale(year), umf2)

coef(mod1)

confint(mod1, type = "state", level = 0.95)
str(siteCovs$year)


# this will explore marginal relationship between marten psi and coniferous forest
df <- data.frame(cbind(conifer = seq(min(siteCovs$conifer), max(siteCovs$conifer), length.out = 200),
                       mixedwood = mean(siteCovs$mixedwood),
                       opennoneagri = mean(siteCovs$opennoneagri),
                       scrub = mean(siteCovs$scrub),
                       urban = mean(siteCovs$urban),
                       greenurban = mean(siteCovs$greenurban),
                       water = mean(siteCovs$water),
                       agri = mean(siteCovs$agri),
                       year = mean(siteCovs$year)))

# create predictions from mod1
preds <- predict(mod1, newdata = df, type = "state")

# set type to state for occupancy probability, set type to detection for detection probability

# combine with the dataframe
preds_df <- cbind(df, preds)

# plot it.
ggplot(preds_df,
         aes(
           x = conifer,
           y = Predicted,
         )) +
  geom_ribbon(aes(ymin = lower, ymax = upper),
              alpha = 0.15,
              linetype = "dashed",) +
  geom_path(size = 1) +
  labs(x = "Covariates (standardized)", y = "Occupancy probability") +
  theme_bw() + theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black")
  )
