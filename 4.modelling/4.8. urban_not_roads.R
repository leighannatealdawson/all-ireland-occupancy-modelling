# run  model with all covariates except rivers - swapping urban for roads
# covariates: 
# detction - occ, bait, year, 
# occupancy - agri, conifer, mixedwood, urban , opennonagri, year

rm(list = ls())

library(unmarked)
library(MuMIn)
library(AICcmodavg)
library(ggplot2)
library(tidyverse)
library(AER)


# Load detection data
y <- read.csv("6.provided-scripts/pinemarten201520182020noname.csv")
y <- as.matrix(y)

siteCovs <- read.csv("1.data/1.4.final_data_gropupings/buffer_datainput_1km_grid_groupings.csv")
head(siteCovs)

head(siteCovs(umf))
str(siteCovs(umf)$urban)
all(rownames(y) == rownames(siteCovs))  # should be TRUE
summary(siteCovs$urban)
unique(site_covs$urban)


# Create observation occasion number covariate
occ <- y
for (i in 1:nrow(y)) {
  tmp <- y[i, ]
  n.samp <- sum(!is.na(tmp))
  tmp[!is.na(tmp)] <- 1:n.samp
  occ[i, ] <- tmp
}

# Format bait covariate
siteCovs$bait[siteCovs$bait == "sunflower"] <- 0
siteCovs$bait[siteCovs$bait == "sunflower and peanuts"] <- 1
siteCovs$bait <- as.numeric(siteCovs$bait)

# Observation covariates
obvsCov <- list(occ = occ)

# Create unmarkedFrame
umf1<- unmarkedFrameOccu(y = y, siteCovs = siteCovs, obsCovs = obvsCov)

############################ CREATE MODEL 3 #######################################
mod3 <- occu(~ scale(occ) + scale(year) + bait ~ 
               scale(agri) + scale(conifer) + scale(mixedwood) + scale(urban) +
               scale(opennoneagri) + scale(year), umf1)



# Coefficients and confidence intervals
coef(mod3)
confint(mod3, type = "state", level = 0.95)


############################# MARGINAL ESTIMATES PLOTS ##############################
covariates_to_plot <- c("agri", "conifer", "mixedwood", "opennoneagri", "urban", "year")
means <- sapply(covariates_to_plot, function(v) mean(siteCovs[[v]], na.rm = TRUE), simplify = TRUE)


plot_list <- list()
for (var in covariates_to_plot) {
  df <- as.data.frame(matrix(ncol = length(means), nrow = 200))
  names(df) <- covariates_to_plot
  
  for (col in covariates_to_plot) {
    df[[col]] <- if (col == var) {
      seq(min(siteCovs[[col]], na.rm = TRUE), max(siteCovs[[col]], na.rm = TRUE), length.out = 200)
    } else {
      means[[col]]
    }
  }
  
  preds <- predict(mod3, newdata = df, type = "state")
  preds_df <- cbind(df, preds)
  
  p <- ggplot(preds_df, aes_string(x = var, y = "Predicted")) +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.15, linetype = "dashed") +
    geom_path(size = 1) +
    labs(x = paste(var, "(standardized)"), y = "Occupancy probability") +
    theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line = element_line(colour = "black"))

  plot_list[[var]] <- p
}
# Plot it 
gridExtra::grid.arrange(grobs = plot_list, ncol = 2)

#' opennonagri shows to be slighly better, showing slight negative relationship with 
#' high degree of uncertainty at high levels 
 

############################## PREDICT ACROSS FULL LANDSCAPE ##############################
# Predict across full landscape uasing pre grouped data from 4.0.2 so we use the landscape metrics 
landscape <- read.csv("1.data/1.4.final_data_gropupings/landscape_datainput_1km_grid_groupings_27_5.csv")

# set year to mean year of study
landscape$year <- mean(siteCovs$year)

# make predictions of marten psi across landscape
preds_allireland <- predict(mod3, landscape, type = "state")

preds_df <- cbind(landscape, preds_allireland)

# import grid with DD for plotting ease 
irish_grid <- read.csv("1.data/1.3.processedinarc/irishgrid1kmwithDD.csv")
irish_grid_preds <- merge(irish_grid, preds_df, by = "gridid", all.x = TRUE)

#Save to check in arcgis
write.csv(irish_grid_preds, "4.modelling/4.modeloutputs/predicted_occupancy_allirelandmod3_urban_not_roads.csv", row.names = FALSE)

# plot histogram of predicted occupancy
hist(irish_grid_preds$Predicted, breaks = 30, main = "Histogram of Predicted Occupancy Probability", xlab = "Predicted Occupancy Probability", col = "skyblue", border = "white")

#goodness of fit
occ_gof1 <- mb.gof.test(mod3, nsim = 1000, plot.hist = TRUE) 
occ_gof1
#results from global model including , urban, opennonagri, conifer, mixedwood, agri, year, bait, occ








