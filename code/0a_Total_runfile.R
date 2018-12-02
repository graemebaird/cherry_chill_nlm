# Complete data preparation, formatting, model fitting, and simulation framework for 
# the PUCV cherry NLM project

# LIBRARIES
library(ggjoy)
library(ggridges)
library(gridExtra)
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


# Convenience code: load raw data into environment
d <- read.csv("datafiles/Raw_combined_excel.csv")

# Convenience code: load summary statistics into environment
e <- read.csv("datafiles/Summary_stats.csv")

# Convenience code: model-ready dataframe

varlist <- unique(d$variety)
long_d <- d %>%
  melt(measure=c(6:20)) %>%           
  na.omit(CP) %>%
  select(variety,tree,week,sub,CP_c,GDH_c,value) %>%
  mutate(value = as.integer(value + 1))

# Functions file
source("./codefiles/0b_Functions.R")

# Merge observation data, extract summary weather variables from weather data
source("./codefiles/1_Data_preparation.R")

# Mutate data and prep models (if run as-is, will take a very long time)
source("./codefiles/2_Fit_models.R")

# Create Figure 7 
source("./codefiles/fig7_posterior_violin_plotter.R")

# Create Figure 8 
source("./codefiles/fig8_posterior_contour_plotter_3p.R")

# Create Figure 9
source("./codefiles/fig9_raw_posterior_activity_plotter.R")

# Create Figure X (not included in paper)
source("./codefiles/figX_summary_requirements.R")

