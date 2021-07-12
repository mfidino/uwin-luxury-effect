############################
#
# Analysis for socioeconomic analysis
#
# Written by M. Fidino
#
############################

library(dplyr)
library(lubridate)
library(runjags)
library(parallel)
library(coda)

# This script assumes you have ran the following scripts, which write
#  data into the "./data/cleaned_data" folder:
#  "./R/cleaning_scripts/data_cleaning_script.R"
#  "./R/cleaning_scripts/covariate_cleaning_script.R"

# Prepares all the cleaned data for the model. The objects you use from this
#  are:

#  data_list: Is all of the data input into the model (list)
#  inits: function that supplies initial values to JAGS (function)

min_dets <- 19

model <- "./JAGS/multi_scale_occupancy_simpler_RE.R"
include_inxs <- FALSE

source("./R/prep_data_for_model.R")

my_start <- Sys.time()

cl <- parallel::makeCluster(3)

# Note: Takes about a day and a half to run.
m1 <- run.jags(
  model,
  monitor = c(
    # among-city regression
    "a_among", "tau_among", "b_among",
    # within-city occupancy
    "b_within", "tau_within", "tau_shape", "tau_rate",
    "b_species", "tau_species","b_species_city",
    # seasonal variation occupancy
    "c_shape_psi", "c_rate_psi", "c_tau_psi",
    # within-city detection
    "c_det", "tau_det", "tau_shape_rho", "tau_rate_rho",
    "c_species_det","tau_species_det", "c_species_city",
    # seasonal variation detection
    "c_shape_rho","c_rate_rho", "c_tau_rho",
    # latent state of whether species is available for sampling in city
    "x"
    ),
  inits = inits,
  data = data_list,
  n.chains = 3,
  adapt =  1000,
  burnin = 55000,
  sample = 20000,
  thin = 3,
  modules = "glm",
 # method = "parallel"
  method = "rjparallel",
  cl = cl
)

parallel::stopCluster(cl)

saveRDS(m1, "./results/final_model_fit.RDS")

my_end <- Sys.time()