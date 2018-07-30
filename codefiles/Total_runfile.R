# Complete data preparation, formatting, model fitting, and simulation framework

# LIBRARIES
library(reshape2)
library(magrittr)
library(dplyr)
library(brms)
library(rstan)
library(lme4)
library(merTools)
library(openxlsx)
library(ggplot2)
library(magrittr)
library(dplyr)
library(plotly)
library(chillR)
library(ggjoy)

# Functions file
source("./codefiles/Functions.R")

# Merge observation data, extract summary weather variables from weather data
source("./codefiles/Data_preparation.R")

# Mutate data and prep models (if run as-is, will take a very long time)
source("./codefiles/Fit_models.R")

# Create Figure X 
source("./codefiles/fig7_posterior_violin_plotter.R")

# Create Figure X 
source("./codefiles/fig8_posterior_contour_plotter_3p.R")

# Create Figure X
source("./codefiles/fig9_raw_posterior_activity_plotter.R")

