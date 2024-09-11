#### Set-up ####

# Open risk_analysis_template.Rproj (RECOMMENDED)
# Or set working directory to this folder:
# setwd("[directory]\\risk_analysis_template")
# install.packages(c("mc2d", "ggplot2"))

# Load packages
library(mc2d)    # Monte-Carlo simulations
library(ggplot2) # Plot graphs

# Load data
data <- read.csv(
  "data.csv",
  sep = ";",
  stringsAsFactors = TRUE,
  na.strings = c(NA, "NA", "")
)
print(data) # One row equals one variate in Monte Carlo simulation

#### Create mc objects (mcnodes) ####

##### Fixed variables #####
mcnodes_name <- names(data[sapply(data, is.numeric)])

for (i in 1:length(mcnodes_name)) {
  mcnode_i <- mcdata(data[[mcnodes_name[i]]], type = "0", nvariates = nrow(data))
  assign(mcnodes_name[i], mcnode_i)
}

##### Stochastic Variables #####
h_prev <- mcstoc(
  func = runif,
  min = h_prev_min,
  max = h_prev_max,
  nvariates = nrow(data)
)

w_prev <- mcstoc(
  func = runif,
  min = w_prev_min,
  max = w_prev_max,
  nvariates = nrow(data)
)

n_animals <- mcstoc(
  func = rnorm,
  mean = n_animals_mean,
  sd = n_animals_sd,
  nvariates = nrow(data)
)

test_sensi <- mcstoc(
  func = rpert,
  min = test_sensi_min,
  mode = test_sensi_mode,
  max = test_sensi_max,
  nvariates = nrow(data)
)

#### Write model ####

# Probability that an animal in a herd is infected (a = an animal)
a_inf <- h_prev * w_prev

# Probability an animal is a false negative (test specificity assumed to be 100%)
a_false_neg <- a_inf * (1 - test_sensi)

# Probability at least one animal from a farm is a false negative (aloa = at least one animal)
aloa_false_neg <- 1 - (1 - a_false_neg) ^ n_animals

# Probability at least one animal from a farm is a false negative (alof = at least one farm)
alof_false_neg <- 1 - (1 - aloa_false_neg) ^ n_farms

# Results
print(alof_false_neg)

#### Further analysis ####

# Load custom functions
source("custom_functions.R")

# Load packages
library(dplyr)   # Data manipulation
library(tidyr)   # Data cleaning
library(ggplot2) # Plot graphs

# Results mcnode
result_mc <- alof_false_neg

# Summarize adding data keys
# If keys is null, defaults to factor columns
mc_keys_summary(result_mc, data)

# Transform mc to long object
# This allows compatibility with most tidyverse functions (such as ggplot)
result_long <- long_mc(result_mc, data)

# Boxplot by key
long_mc_boxplot(result_long)

# Result is a ggplot, then you can use all ggplot functionalities
# For example, wrap by disease
long_mc_boxplot(result_long, key_label = "origin") +
  facet_wrap(vars(disease), scales = "free")
