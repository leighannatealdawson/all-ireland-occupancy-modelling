 rm(list = ls())
# first atempt at occupency modelling, this needs a good but more playing around with and i want to try the AIC model selection - 09/05/2025 

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

# consider which szie buffer we want to use for our analysis - to be removed 
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
# create matrix of detection data and landscape data  
# lets use 5km buffer to preserve resolution 

# read in detection none detection data 
y <- read.csv("6.provided-scripts/pinemarten201520182020noname.csv")
y <- as.matrix(y)

nrow(y)
head(y)
str(y) 

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
hist(siteCovs$urban) # looks good

# what about when we scale 
hist(scale(siteCovs$agrinonpas))
hist(scale(siteCovs$pasture)) 
hist(scale(siteCovs$conifer))
hist(scale(siteCovs$mixedwood))
hist(scale(siteCovs$opennoneagri))
hist(scale(siteCovs$urban))


######################################################################################################
# create unmarked df
umf2 = unmarkedFrameOccu(y = y, siteCovs = siteCovs, obsCovs = obvsCov)
# Warning message: siteCovs contains characters. Converting them to factors.
str(umf2)
head(siteCovs)
colnames(umf2@siteCovs)
View(umf2@siteCovs

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
             + scale(urban)
             + scale(year), umf2)

# always look at the model 
# lets look at estimates and confidence intervals  
#' remeber everything is on a log scale so -5 = -infinity and +5 = +infinity if we see 
#' values over this our model failed to converge or converges at a local minimum
#' if this happens remove covariates and try again

coef(mod1)

confint(mod1, type = "state", level = 0.95)


# stay here until you have a model you are happy with. You can either build and interpret a single global model, or you can conduct two-stage AIC-based model selection (see end of script).

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
                        urban = mean(siteCovs$urban),
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

######################## urban #######################################
df <- data.frame(cbind(  urban = seq(min(siteCovs$urban), max(siteCovs$urban), length.out = 200),
                        conifer = mean(siteCovs$conifer),
                        agrinonpas = mean(siteCovs$agrinonpas),
                        pasture = mean(siteCovs$pasture),
                        mixedwood = mean(siteCovs$mixedwood),
                        year = mean(siteCovs$year)))

# create predictions from mod1
preds <- predict(mod1, newdata = df, type = "state")

# combine with the dataframe
preds_df <- cbind(df, preds)

# plot it.
ggplot(preds_df,
         aes(
           x = urban,
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


###################### mixedwood #######################################
df <- data.frame(cbind(  mixedwood = seq(min(siteCovs$mixedwood), max(siteCovs$mixedwood), length.out = 200),
            conifer = mean(siteCovs$conifer),
            agrinonpas = mean(siteCovs$agrinonpas),
            pasture = mean(siteCovs$pasture),
            urban = mean(siteCovs$urban),
            year = mean(siteCovs$year)))

# create predictions from mod1
preds <- predict(mod1, newdata = df, type = "state")

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

#############################pasture ################################
df <- data.frame(cbind( pasture = seq(min(siteCovs$pasture), max(siteCovs$pasture), length.out = 200),
                        conifer = mean(siteCovs$conifer),
                        agrinonpas = mean(siteCovs$agrinonpas),
                        urban = mean(siteCovs$urban),
                        mixedwood = mean(siteCovs$mixedwood),
                        year = mean(siteCovs$year)))

# create predictions from mod1
preds <- predict(mod1, newdata = df, type = "state")

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

############################agri non-pasture ###############################
df <- data.frame(cbind( agrinonpas = seq(min(siteCovs$agrinonpas), max(siteCovs$agrinonpas), length.out = 200),
                        conifer = mean(siteCovs$conifer),
                        pasture = mean(siteCovs$pasture),
                        urban = mean(siteCovs$urban),
                        mixedwood = mean(siteCovs$mixedwood),
                        year = mean(siteCovs$year)))

# create predictions from mod1
preds <- predict(mod1, newdata = df, type = "state")

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

############################# year ###############################
df <- data.frame(cbind( year = seq(min(siteCovs$year), max(siteCovs$year), length.out = 200),
                        conifer = mean(siteCovs$conifer),
                        pasture = mean(siteCovs$pasture),
                        urban = mean(siteCovs$urban),
                        mixedwood = mean(siteCovs$mixedwood),
                        agrinonpas = mean(siteCovs$agrinonpas)))

# create predictions from mod1
preds <- predict(mod1, newdata = df, type = "state")

# combine with the dataframe
preds_df <- cbind(df, preds)

# plot it.
ggplot(preds_df,
         aes(
           x = year,
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



###########################################################################################
#### how does this look for all of Ireland ####
landscape <- read.csv("1.data/1.3.processedinarc/10kmwidecorine.csv")

View(landscape)
colnames(landscape)

rename to match model matrix 
colnames(landscape)[5:38] <- paste0("CLC_", substring(colnames(landscape)[5:38], 2))

# group CLC categories to match model matrix 
# all agriculture execpt pasture 
landscape$agrinonpas <- rowSums(landscape[, c("CLC_211", "CLC_242", "CLC_243")], na.rm = TRUE)

# pasture land cover 
landscape$pasture <- landscape$CLC_231

# conifer including conifer and transitional woodland 
landscape$conifer <- rowSums(landscape[, c("CLC_312", "CLC_324")], na.rm = TRUE)

# mixed wood including broadleaf and mixed forest 
landscape$mixedwood <- rowSums(landscape[, c("CLC_311", "CLC_313")], na.rm = TRUE)

# open non agriculture including natural grassland, peat bog, moore and heathland
landscape$opennoneagri <- rowSums(landscape[, c("CLC_321", "CLC_412", "CLC_322")], na.rm = TRUE)

# urban - all urban cover types 
landscape$urban <- rowSums(landscape[, c("CLC_111", "CLC_112", "CLC_121", "CLC_123", 
                                       "CLC_124", "CLC_131", "CLC_132", "CLC_133", "CLC_142")], na.rm = TRUE)
View(landscape)

# create dummy year covariate
landscape$year <- mean(siteCovs$year)
###################

# make predictions of marten psi across landscape
preds_allireland <- predict(mod1, landscape, type = "state")

# combine preds with landscape
preds_df <- cbind(landscape, preds_allireland)

View(preds_df)

# save to look at in GIS 
write.csv(preds_df, "4.modelling/4.modeloutputs/predictions_allirelandmod1.csv", row.names = FALSE)
 ###### NOTE i used a 5km buffer and 10km grid to extract covariates, THIS IS WRONG I KNOW
 # we need to use a 5km landscpae grid to make this a valid pred but cant get LCM to work. 

 ############################################## *****Below here isnt something ive considered.  i need to review this section as i wanted to play with the model more beforehand 
# what if i use two step model selection

#' were making preds from out model and comparing these to out real data, looking at distrution of the detections 
#' we get, if they are similar this is good, too similar isnt good as the model can only predicts itself. if theyre 
#' too different this is also a huge issue. 
 
#goodness of fit- for global model 
occ_gof1 <- mb.gof.test(mod1, , nsim = 1000, plot.hist = TRUE)  
occ_gof1
#' 2 important values are p value and chat estimate 
#' sig test gives idea of how well data fits the model so we want a value greater than 0.1 
#' chatestimate is a measure of over/underdispertion (common in det/non-det data- as we usually have lots of non-det therefore overdis data)
# we will use chat to do chat adjustments to inform the size of our confidence intervals as with overdisp data if not adjusted for everything will cme out as sig

# 2 step model selection Part 1: 
#' dreding function can be used to compare our global model, it will compareevery possible combo
#' # first build global detection model (every cov you interest in on p)
mod1 <- occu(~ occ + bait + year + scale(Broadleaf_and_mixed)
             ~ 1, umf2)
# then conduct model selection
modelList <- dredge(mod1, rank = "QAIC", fixed = "p(year)", chat = 1.77)

model_list_df <- as.data.frame(modelList.fullp.nullpsi)

# we will use AIC model selection when chat = 1 or near 1
# we will use QAIC model selection what chat > 1

# whatever comes out as top model - use in part of model selection

# Part 2 of model selection - the occupancy submodel
mod2 <- occu(~ bait + year + scale(Broadleaf_and_mixed)
             ~ scale(Broadleaf_and_mixed) + scale(Coniferous.forest) + scale(Agriculture.with.natural.vegetation) +scale(Moors.and.heathland) + scale(Transitional.woodland.shrub) + scale(year), umf2)

# then conduct model selection
modelList <- dredge(mod2, rank = "QAIC", fixed = c("p(year)","p(bait)", "p(scale(Broadleaf_and_mixed))"), chat = 1.77)


# what ever is the top model in this model selection, is your top model and you should use it to make predictions as we did above. 



####################################################################################################
#options for model selection 
#option 1 - global model interprtation

#' regardless - step 1 
#' write a series of hypthese for for detection and occupancy probability
#' for example: i think detection probability will be pos/neg associated with bait and time of year, 
#' occupancy probability will be  pos/neg associated with coniferous forest and roads etc etc 
#' 
#' option 1 - global model interprtation 
#' generally considered the prefered approach by statisticians but often looked down on as it doesnt use AIC 
#' inferecne will be the same regarless 
#' #' build model including all of these covariates and look at this model 
#' 