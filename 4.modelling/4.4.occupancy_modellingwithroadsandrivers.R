rm(list = ls())
# occupency modelling with roads and rivers - attempt 2 
library(unmarked)
library(MuMIn)
library(AICcmodavg)
library(ggplot2)
library(tidyverse)
library(AER)

#notes: 
# step by step guide to building occupancy models in unmarked
#' Step 1. Extract covariates at 1km2, 5km2, and 10km2 buffers (or as chosen scale) 
#' Step 2. Create a fishnet or grid, as the same size as the buffers across all Ireland, and extract covariates at this grid scale
#' Step 3. Sanity check - plot your covariates on your grid in ArcGIS - do they look right?
#' You can start building models.... But first
#' Step 4.  Write down your hypotheses for covariate effects on detection probability and occupancy based on species ecology
#' Step 5. Think about how you might combine some of the various classes in the CORINE data (for example, broadleaf + mixed forest becomes broadleaf and mixed).
#' Step 6. Build a global model - and check goodness of fit
#' Step 7. Decide on whether you are going to interpret your global model or conduct AIC-based model selection
#' Step 8. Make marginal predictions for covariates in top model (explore covariate space across range of sampled values for that covariate, while keeping all others at their mean). 
#' Step 9. Make predictions for marten psi (occupancy probability) across the landscape, plot them in ArcGIS
#' Step 10. Celebrate, you did it!


setwd("D:/all-ireland-occupancy-modelling")
#Try the 1km buffer and grid 
# remember we can always smooth the outputs later if we want to 

# import the 1km grid with data
# read in raw detection none detection data 
y <- read.csv("6.provided-scripts/pinemarten201520182020noname.csv")
y <- as.matrix(y)

nrow(y)
head(y)
str(y) 

# lets use the 1km buffer to preserve resolution
siteCovs <- read.csv( "1.data/1.3.processedinarc/roads_and_rivers_plot_id1km_buffer.csv")

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
# Create occupancy covariates list 

#'# Bait covariate
#' note in samelling in 2018 sunflowers only were used as bait, in 2015 and 2020 both sunflowers and peanuts were used
#' likely impacting detection probability, so we will include this as a covariate
#' 
# format bait as dummy cov
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
####################################################################################################
#' Corvariate groupings
#' 
#' # Create new columns by summing relevant CLC classes into conifer, mixedwoodland, open none agriculture,
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

# road length in m inside each 1km2 buffer 
siteCovs$road_length <- siteCovs$road_length

#river length in m inside each 1km2 buffer
siteCovs$river_length <- siteCovs$river_length

head(siteCovs)

View(siteCovs)
# make a table of each table categoies mean and sd and range 
# List the columns you want to summarise
groupings <- c("agrinonpas", "pasture", "conifer", "mixedwood", 
               "opennoneagri", "road_length", "river_length")

# Use sapply to generate the summary table
summary_table <- t(sapply(siteCovs[, groupings], function(x) {
  c(Mean = mean(x, na.rm = TRUE),
    SD = sd(x, na.rm = TRUE),
    Min = min(x, na.rm = TRUE),
    Max = max(x, na.rm = TRUE))
}))
# Print the summary table
print(summary_table)

##########################################################################################################################
# plot and check distribution of covariates sampled 
#' NOTE many of these covarates will show we have lots of areas sampled with very little of our covaraites, this okay as long as we scale the data 
#' the important thing to avoid is having big gaps like this >  hist(siteCovs$CLC_111)

# check distribution of covariates sampled
hist(siteCovs$agrinonpas) # small gap around 0.6- problem? 
hist(siteCovs$pasture) # looks good
hist(siteCovs$conifer) # looks good
hist(siteCovs$mixedwood) #looks good
hist(siteCovs$opennoneagri) # looks good
hist(siteCovs$road_length) 
hist(siteCovs$river_length) 

# what about when we scale 
hist(scale(siteCovs$agrinonpas))
hist(scale(siteCovs$pasture)) 
hist(scale(siteCovs$conifer))
hist(scale(siteCovs$mixedwood))
hist(scale(siteCovs$opennoneagri))
hist(scale(siteCovs$road_length))
hist(scale(siteCovs$river_length)) 

######################################################################################################
# create unmarked df
umf2 = unmarkedFrameOccu(y = y, siteCovs = siteCovs, obsCovs = obvsCov)
# Warning message: siteCovs contains characters. Converting them to factors.
str(umf2)
head(siteCovs)
colnames(umf2@siteCovs)
View(umf2@siteCovs)

str(obsCovs(umf2))

###############################################################################################################
#' model creation - Mod1 
#' mod1 <- "covariates that determine detection probability" ~ "covariates that determine detection probability" 
#' 
#' scale so data is transformed so it has a mean of 0 and SD/unit variance of 1- we can do this in the model when using unmarked but not others
#' with unmarked we can scale here and it will automatically scale covariates when we make predictions (this makes life easier)
#' 
mod1 <- occu(~ scale(occ) + scale(year) + bait
             ~ scale(agrinonpas) 
             + scale(pasture) 
             + scale(conifer) 
             + scale(mixedwood) 
             + scale(opennoneagri)
             + scale(road_length)
             + scale(river_length)
             + scale(year), umf2)


# always look at the model 
# lets look at estimates and confidence intervals  
#' remeber everything is on a log scale so -5 = -infinity and +5 = +infinity if we see 
#' values over this our model failed to converge or converges at a local minimum
#' if this happens remove covariates and try again

coef(mod1)

confint(mod1, type = "state", level = 0.95)

#####################################################################################################################
# create marginal estimates 
# take covariate of interest, using min and max of what we sampled and keeping all other covariates at their mean 
# controlling for everything else but exploring the effect of this covariate on occupancy probability

# remeber every covariate in the model needs to be included in this new df 
# this will explore marginal relationship between marten psi and coniferous forest

########################## coniferous forest #######################################
df <- data.frame(cbind(conifer = seq(min(siteCovs$conifer), max(siteCovs$conifer), length.out = 200),
                        agrinonpas = mean(siteCovs$agrinonpas),
                        pasture = mean(siteCovs$pasture),
                        mixedwood = mean(siteCovs$mixedwood),
                        road_length = mean(siteCovs$road_length),
                        river_length = mean(siteCovs$river_length),
                        opennoneagri = mean(siteCovs$opennoneagri),
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

######################### mixedwood ####################################### 
df <- data.frame(cbind(mixedwood = seq(min(siteCovs$mixedwood), max(siteCovs$mixedwood), length.out = 200),
                        agrinonpas = mean(siteCovs$agrinonpas),
                        pasture = mean(siteCovs$pasture),
                        conifer = mean(siteCovs$conifer),
                        opennoneagri = mean(siteCovs$opennoneagri),
                        road_length = mean(siteCovs$road_length),
                        river_length = mean(siteCovs$river_length),
                        year = mean(siteCovs$year)))


                    
# create predictions from mod1
preds <- predict(mod1, newdata = df, type = "state")
# set type to state for occupancy probability, set type to detection for detection probability

# combine with the dataframe
preds_df <- cbind(df, preds)

# plot it.
ggplot(preds_df,
       aes(
         x = mixedwood,
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

####################################  openoneagri#######################################
# set type to state for occupancy probability, set type to detection for detection probability
# combine with the dataframe
df <- data.frame(cbind(opennoneagri = seq(min(siteCovs$opennoneagri), max(siteCovs$opennoneagri), length.out = 200),
                        agrinonpas = mean(siteCovs$agrinonpas),
                        pasture = mean(siteCovs$pasture),
                        conifer = mean(siteCovs$conifer),
                        mixedwood = mean(siteCovs$mixedwood),
                        road_length = mean(siteCovs$road_length),
                        river_length = mean(siteCovs$river_length),
                        year = mean(siteCovs$year)))

# create predictions from mod1
preds <- predict(mod1, newdata = df, type = "state")
# set type to state for occupancy probability, set type to detection for detection probability
# combine with the dataframe
preds_df <- cbind(df, preds)
# plot it.
ggplot(preds_df,
       aes(
         x = opennoneagri,
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

######################### agrinonpas #######################################
df <- data.frame(cbind(agrinonpas = seq(min(siteCovs$agrinonpas), max(siteCovs$agrinonpas), length.out = 200),
                        pasture = mean(siteCovs$pasture),
                        conifer = mean(siteCovs$conifer),
                        mixedwood = mean(siteCovs$mixedwood),
                        opennoneagri = mean(siteCovs$opennoneagri),
                        road_length = mean(siteCovs$road_length),
                        river_length = mean(siteCovs$river_length),
                        year = mean(siteCovs$year)))
# create predictions from mod1
preds <- predict(mod1, newdata = df, type = "state")
# set type to state for occupancy probability, set type to detection for detection probability
# combine with the dataframe

preds_df <- cbind(df, preds)
# plot it.
ggplot(preds_df,
       aes(
         x = agrinonpas,
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
######################### pasture #######################################
df <- data.frame(cbind(pasture = seq(min(siteCovs$pasture), max(siteCovs$pasture), length.out = 200),
                        agrinonpas = mean(siteCovs$agrinonpas),
                        conifer = mean(siteCovs$conifer),
                        mixedwood = mean(siteCovs$mixedwood),
                        opennoneagri = mean(siteCovs$opennoneagri),
                        road_length = mean(siteCovs$road_length),
                        river_length = mean(siteCovs$river_length),
                        year = mean(siteCovs$year)))
# create predictions from mod1
preds <- predict(mod1, newdata = df, type = "state")
# set type to state for occupancy probability, set type to detection for detection probability
# combine with the dataframe
preds_df <- cbind(df, preds)
# plot it.
ggplot(preds_df,
       aes(
         x = pasture,
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


################## river length #######################################
df <- data.frame(cbind(river_length = seq(min(siteCovs$river_length), max(siteCovs$river_length), length.out = 200),
                        agrinonpas = mean(siteCovs$agrinonpas),
                        pasture = mean(siteCovs$pasture),
                        conifer = mean(siteCovs$conifer),
                        opennoneagri = mean(siteCovs$opennoneagri),
                        mixedwood = mean(siteCovs$mixedwood),
                        urban = mean(siteCovs$urban),
                        road_length = mean(siteCovs$road_length),
                        year = mean(siteCovs$year)))

# create predictions from mod1
preds <- predict(mod1, newdata = df, type = "state")

# set type to state for occupancy probability, set type to detection for detection probability

# combine with the dataframe
preds_df <- cbind(df, preds)
# plot it.

ggplot(preds_df,
       aes(
         x = river_length,
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


######################### roads #######################################
df <- data.frame(cbind(road_length = seq(min(siteCovs$road_length), max(siteCovs$road_length), length.out = 200),
                        agrinonpas = mean(siteCovs$agrinonpas),
                        pasture = mean(siteCovs$pasture),
                        opennoneagri = mean(siteCovs$opennoneagri),
                        conifer = mean(siteCovs$conifer),
                        mixedwood = mean(siteCovs$mixedwood),
                        urban = mean(siteCovs$urban),
                        river_length = mean(siteCovs$river_length),
                        year = mean(siteCovs$year)))

# create predictions from mod1
preds <- predict(mod1, newdata = df, type = "state")

# set type to state for occupancy probability, set type to detection for detection probability

# combine with the dataframe
preds_df <- cbind(df, preds)

# plot it.
ggplot(preds_df,
       aes(
         x = road_length,
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
################################All Ireland ###############################################################
#### how does this look for all of Ireland ####
landscape <- read.csv("1.data/1.3.processedinarc/1kmwidecorine.csv")

View(landscape)
colnames(landscape)

# Rename columns from X### to CLC_###
colnames(landscape) <- gsub("^X(\\d{3})$", "CLC_\\1", colnames(landscape))

# Import rivers and roads data for all Ireland 1km grid
rivers_roads <- read.csv("1.data/1.3.processedinarc/1km_all_ireland_grid_rivers_and_roads.csv")

head(rivers_roads)
# Check for NAs in rivers_roads$gridid
sum(is.na(rivers_roads$gridid))
head(landscape)
# Merge landscape and rivers_roads by gridid
landscape <- merge(landscape, rivers_roads, by = "gridid", all.x = TRUE)

View(landscape)
####################################################################################################
#' Corvariate groupings to match buffers 
landscape$agrinonpas <- rowSums(landscape[, c("CLC_211", "CLC_242", "CLC_243")], na.rm = TRUE)

# pasture land cover 
landscape$pasture <- landscape$CLC_231

# conifer including conifer and transitional woodland 
landscape$conifer <- rowSums(landscape[, c("CLC_312", "CLC_324")], na.rm = TRUE)

# mixed wood including broadleaf and mixed forest 
landscape$mixedwood <- rowSums(landscape[, c("CLC_311", "CLC_313")], na.rm = TRUE)

# open non agriculture including natural grassland, peat bog, moore and heathland
landscape$opennoneagri <- rowSums(landscape[, c("CLC_321", "CLC_412", "CLC_322")], na.rm = TRUE)

# road length in m inside each 1km2 buffer 
landscape$road_length <- landscape$SUM_Length_m_roads

#river length in m inside each 1km2 buffer
landscape$river_length <- landscape$SUM_Shape_Length
# create dummy year covariate
landscape$year <- mean(siteCovs$year)

in df replace nas with 0 
landscape[is.na(landscape)] <- 0
View(landscape)
# make predictions of marten psi across landscape
colnames(landscape)
colnames(siteCovs)

preds_allireland <- predict(mod1, landscape, type = "state")
View(preds_allireland)
# combine preds with landscape
preds_df <- cbind(landscape, preds_allireland)# pred are all nas 

write.csv(preds_df, "4.modelling/4.modeloutputs/predicted_occupancy_allirelandmod2.csv", row.names = FALSE)

View(preds_df)
View(landscape)

str(siteCovs)
str(landscape)
check landscape for nas 
# check for NAs in landscape$gridid
sum(is.na(landscape$gridid))
