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
umf2 <- unmarkedFrameOccu(y = y, siteCovs = siteCovs, obsCovs = obvsCov)

# Fit model 2 (excluding rivers)
mod2 <- occu(~ scale(occ) + scale(year) + bait ~
    scale(agri) + scale(conifer) + scale(mixedwood) +
    scale(roads) + scale(opennoneagri) + scale(year), umf2)

# Coefficients and confidence intervals
coef(mod2)
confint(mod2, type = "state", level = 0.95)

# Marginal prediction plots
covariates_to_plot <- c("agri", "conifer", "mixedwood", "opennoneagri", "roads", "year")
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

    preds <- predict(mod2, newdata = df, type = "state")
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

gridExtra::grid.arrange(grobs = plot_list, ncol = 2)

# Predict across full landscape
landscape <- read.csv("1.data/1.4.final_data_gropupings/landscape_datainput_1km_grid_groupings.csv")
landscape$year <- mean(siteCovs$year)

# Scale variables in landscape using training data mean/sd
scale_var <- function(var) {
    (landscape[[var]] - mean(siteCovs[[var]], na.rm = TRUE)) / sd(siteCovs[[var]], na.rm = TRUE)
}

landscape$agri <- scale_var("agri")
landscape$conifer <- scale_var("conifer")
landscape$mixedwood <- scale_var("mixedwood")
landscape$opennoneagri <- scale_var("opennoneagri")
landscape$roads <- scale_var("roads")
landscape$year <- scale_var("year")

preds_allireland <- predict(mod2, newdata = landscape, type = "state")
preds_df <- cbind(landscape, preds_allireland)

irish_grid <- read.csv("1.data/1.3.processedinarc/irishgrid1kmwithDD.csv")
irish_grid_preds <- merge(irish_grid, preds_df, by = "gridid", all.x = TRUE)

write.csv(irish_grid_preds, "4.modelling/4.modeloutputs/predicted_occupancy_allirelandmod2_norivers.csv", row.names = FALSE)

hist(irish_grid_preds$Predicted, breaks = 30, main = "Histogram of Predicted Occupancy Probability", xlab = "Predicted Occupancy Probability", col = "skyblue", border = "white")

# Goodness of fit test
gof_test <- mb.gof.test(mod2, nsim = 1000, plot.hist = TRUE)
gof_test

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
umf2 <- unmarkedFrameOccu(y = y, siteCovs = siteCovs, obsCovs = obvsCov)

# Fit model 2 (excluding rivers)
mod2 <- occu(~ scale(occ) + scale(year) + bait ~
    scale(agri) + scale(conifer) + scale(mixedwood) +
    scale(roads) + scale(opennoneagri) + scale(year), umf2)

# Coefficients and confidence intervals
coef(mod2)
confint(mod2, type = "state", level = 0.95)

# Marginal prediction plots
covariates_to_plot <- c("agri", "conifer", "mixedwood", "opennoneagri", "roads", "year")
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

    preds <- predict(mod2, newdata = df, type = "state")
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

gridExtra::grid.arrange(grobs = plot_list, ncol = 2)

# Predict across full landscape
landscape <- read.csv("1.data/1.4.final_data_gropupings/landscape_datainput_1km_grid_groupings.csv")
landscape$year <- mean(siteCovs$year)


preds_allireland <- predict(mod2, newdata = landscape, type = "state")
preds_df <- cbind(landscape, preds_allireland)

irish_grid <- read.csv("1.data/1.3.processedinarc/irishgrid1kmwithDD.csv")
irish_grid_preds <- merge(irish_grid, preds_df, by = "gridid", all.x = TRUE)

write.csv(irish_grid_preds, "4.modelling/4.modeloutputs/predicted_occupancy_allirelandmod2_norivers.csv", row.names = FALSE)

hist(irish_grid_preds$Predicted,
    breaks = 30,
    main = "Histogram of Predicted Occupancy Probability",
    xlab = "Predicted Occupancy Probability",
    col = "skyblue", border = "white"
)

# Goodness of fit test
gof_test <- mb.gof.test(mod2, nsim = 10, plot.hist = TRUE)
gof_test
