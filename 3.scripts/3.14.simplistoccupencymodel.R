# Trying occupnay modelling with example data 
#
rm(list=ls())
# Load the libraries
library(unmarked)
library(AHMbook)
library(jagsUI)
library(R2WinBUGS)
install.packages("R2WinBUGS")
##############################################################################################################
# 10.3.the simplist occuoancy model from pg 561 from AHM book -chater 10.4 
# Load the data
PA_data_NI_1km <- read.csv("1.data/1.2.processed/PA_data_NI_1km.csv")
 View(PA_data_NI_1km)
 head(PA_data_NI_1km)
# Create the model
PA_data_NI_1km <- read.csv("C:/VScode/all-ireland-occupancy-modelling/1.data/1.2.processed/1.2.2.PA_data_NI_1km.csv")

#remove any col with nas 
PA_data_NI_1km <- PA_data_NI_1km[, colSums(is.na(PA_data_NI_1km)) == 0]

#extract into a matrix 

y <- as.matrix(PA_data_NI_1km[, grep("Day", colnames(PA_data_NI_1km))])

y <- y[rowSums(!is.na(y)) > 0, ]

# Create an unmarkedFrame object
umf <- unmarkedFrameOccu(y = y)

# Check data structure
summary(umf)

# Fit basic occupancy model
fm1 <- occu(~1 ~1, data = umf)

# View results
summary(fm1)

backTransform(fm1, "state")      # Get estimates on probability scale
backTransform(fm1, "det")

# Bundle data and summarize data bundle
str( win.data <- list(y = y, M = nrow(y), J = ncol(y)) )

# Specify model in BUGS language
sink("model.txt")
cat("
model {
  # Priors
  psi ~ dunif(0, 1)
  p ~ dunif(0, 1)
  # Likelihood
  for (i in 1:M) {    # Loop over sites
    z[i] ~ dbern(psi)         # State model
    for (j in 1:J) { # Loop over replicate surveys
      y[i,j] ~ dbern(z[i]*p)  # Observation model (only JAGS !)
      # y[i,j] ~ dbern(mu[i])  # For WinBUGS define 'straw man'
    }
    # mu[i] <- z[i]*p          # Only WinBUGS
  }
}
",fill = TRUE)
sink()

# Initial values
zst <- apply(y, 1, max)       # Avoid data/model/inits conflict
inits <- function(){list(z = zst)}

# Parameters monitored
params <- c("psi", "p")

# MCMC settings
ni <- 5000   ;   nt <- 1   ;   nb <- 1000   ;   nc <- 3

# Call JAGS and summarize posteriors


library(jagsUI)
fm2 <- jags(win.data, inits, params, "model.txt", n.chains = nc,
            n.thin = nt, n.iter = ni, n.burnin = nb)
print(fm2, dig = 3)

##############################################################################################################
# 10.4 Site occupancy model with covariates
# Load the data
rm(list=ls())
 PA_data_NI_1km <- read.csv("1.data/1.2.processed/PA_data_NI_1km.csv")
   
head(PA_data_NI_1km)  



