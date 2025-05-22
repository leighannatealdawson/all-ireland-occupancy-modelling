rm(list = ls())
# occupency modelling with roads and rivers - all variables

library(unmarked)
library(MuMIn)
library(AICcmodavg)
library(ggplot2)
library(tidyverse)
library(AER)

# notes:
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



# Try the 1km buffer and grid
# remember we can always smooth the outputs later if we want to

# import the 1km grid with data
# read in raw detection none detection data
y <- read.csv("6.provided-scripts/pinemarten201520182020noname.csv")
y <- as.matrix(y)

nrow(y)
head(y)
str(y)

# lets use the 1km buffer to preserve resolution

siteCovs <- read.csv("1.data/1.4.final_data_gropupings/buffer_datainput_1km_grid_groupings.csv")
colnames(siteCovs)

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

#' # Bait covariate
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
    occ = occ
  )

# check formatting is correct - observation covariates should mirror the observation data (y)
str(y)
str(occ)
# set all to numeric 
siteCovs$bait <- as.numeric(siteCovs$bait)
##########################################################################################################################
# plot and check distribution of covariates sampled
#' NOTE many of these covarates will show we have lots of areas sampled with very little of our covaraites, this okay as long as we scale the data
#' the important thing to avoid is having big gaps like this >  hist(siteCovs$CLC_111)

# check distribution of covariates sampled
hist(siteCovs$agri)
hist(siteCovs$conifer)
hist(siteCovs$mixedwood)
hist(siteCovs$opennoneagri)
hist(siteCovs$roads)
hist(siteCovs$rivers)

# now chek scaling
hist(scale(siteCovs$agri))
hist(scale(siteCovs$conifer))
hist(scale(siteCovs$mixedwood))
hist(scale(siteCovs$opennoneagri))
hist(scale(siteCovs$roads))
hist(scale(siteCovs$rivers))



######################################################################################################
# create unmarked df
umf2 <- unmarkedFrameOccu(y = y, siteCovs = siteCovs, obsCovs = obvsCov)
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
~ scale(agri)
  + scale(conifer)
  + scale(mixedwood)
  + scale(roads)
  + scale(rivers)
  + scale(opennoneagri)
  + scale(year), umf2)

colnames(siteCovs)
# always look at the model
# lets look at estimates and confidence intervals
#' remember everything is on a log scale so -5 = -infinity and +5 = +infinity if we see
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



library(ggplot2)
library(gridExtra)

# List of covariates to vary
covariates_to_plot <- c(
  "agri", "conifer", "mixedwood", "opennoneagri",
  "roads", "rivers", "year"
)

sapply(covariates_to_plot, function(v) class(siteCovs[[v]]))
head(siteCovs)

covariates_to_plot <- c(
  "agri", "conifer", "mixedwood", "opennoneagri",
  "roads", "rivers", "year"
)

means <- sapply(covariates_to_plot, function(v) mean(siteCovs[[v]], na.rm = TRUE))

plot_list <- list()
# Create a list of means for all these covariates
means <- sapply(covariates_to_plot, function(v) mean(siteCovs[[v]], na.rm = TRUE), simplify = TRUE)

for (var in covariates_to_plot) {
  df <- as.data.frame(matrix(ncol = length(means), nrow = 200))
  names(df) <- covariates_to_plot

  for (col in covariates_to_plot) {
    if (col == var) {
      df[[col]] <- seq(min(siteCovs[[col]], na.rm = TRUE),
        max(siteCovs[[col]], na.rm = TRUE),
        length.out = 200
      )
    } else {
      df[[col]] <- means[[col]]
    }
  }

  preds <- predict(mod1, newdata = df, type = "state")
  preds_df <- cbind(df, preds)

  p <- ggplot(preds_df, aes_string(x = var, y = "Predicted")) +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.15, linetype = "dashed") +
    geom_path(size = 1) +
    labs(x = paste(var, "(standardized)"), y = "Occupancy probability") +
    theme_bw() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.line = element_line(colour = "black")
    )

  plot_list[[var]] <- p
}

# Show all plots in a grid (e.g., 2 columns)
grid.arrange(grobs = plot_list, ncol = 2)


################################ All Ireland ###############################################################
#### how does this look for all of Ireland ####
landscape <- read.csv("1.data/1.4.final_data_gropupings/landscape_datainput_1km_grid_groupings.csv")
`#check for nas 
sum(is.na(landscape$rivers))
 
# add dummy year covariate 
landscape$year <- mean(siteCovs$year)

colnames(landscape)
class(landscape)

all.vars <- all.vars(formula(mod1))  # Get variable names used in the model
all.vars
colnames(landscape)


####################################################################################################

preds_allireland <- predict(mod1, landscape, type = "state")
class(landscape)
# combine preds with landscape
preds_df <- cbind(landscape, preds_allireland)

View(preds_df)

# Import Irish grid 1km with DD for NI
irish_grid <- read.csv("1.data/1.3.processedinarc/irishgrid1kmwithDD.csv")

irish_grid_preds <- merge(irish_grid, preds_df, by = "gridid", all.x = TRUE)

write.csv(irish_grid_preds, "4.modelling/4.modeloutputs/predicted_occupancy_allirelandmod_all.csv", row.names = FALSE)

View(irish_grid_preds)
# go check this in ArcGIS
# Create histogram of predicted occupancy probabilities
hist(irish_grid_preds$Predicted,
  breaks = 30,
  main = "Histogram of Predicted Occupancy Probability",
  xlab = "Predicted Occupancy Probability",
  col = "skyblue",
  border = "white"
)
######################################################################################
# check goodness of fit- over/under dispersion
# goodness of fit
occ_gof1 <- mb.gof.test(mod1, nsim = 1000, plot.hist = TRUE)
occ_gof1
#we have a p-value of 0.04 - too low - too many covariates? 
# we want a c-hat of around 1
# here we have a c-hat of 1.79 - therefore overdispersed

#######################################################################################




ß