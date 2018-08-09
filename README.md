### Introduction
This collection of R scripts performs the analysis and generates the figures found in Baird et al. 2018 submitted paper "Nonlinear Ordinal Regression to Predict Bud Dormancy Requirements and Bud Burst in Deciduous Trees". 

The objectives of this project were to consolidate experimental cherry phenology data, field phenology data, and incubator/field climate data into a single analytical model to:

1. Determine cherry varietal differences in winter chill requirements
2. Determine cherry varietal differences in spring/summer heat requirements
3. Formulate a reproducible, extensible approach to modeling bud phenology observations under varying climate regimes

To accomplish these objectives, here we develop and implement a Bayesian hierarchical cumulative link model. The model used in our paper is fit using Stan, a probabilistic programming language for MCMC-based Bayesian statistical inference. Raw and processed data are in `datafiles/`. Scripts required to perform all analyses and figure generation are in `codefiles/`. Stan files, located in `stanfiles/`, are executed with the R package `rstan`. Posterior samples and MCMC chain information/diagnostics from fitting these models are stored as stanfit object files in `modelfits/`. Markdown source code used to generate Appendix 1 is in `docs/`.

Scripts are split into data cleanup, model fitting/sampling, and table/figure generation. Required libraries are called in `Total_runfile.R`. Sourcing this script will perform all tasks required to clean data, fit models, and generate figures/tables. Some work may be required to install all of the relevant packages, especially `rstan`, which will require a linked C++ compiler to function (this is a problem for computers with restricted environments).

Several components of this code are processor-intensive, especially the MCMC sampling, and may take a while to run depending on your computer's processor speed (and will generate large ~400MB model fits which will require disc and memory storage). 

### Data cleanup
To keep data provenance, the steps of data cleanup are described here and can be found in `codefiles/Data_preparation.R`. First, the original budburst data collection excel file is collated into a single data frame including all varieties and some summary statistics are calculated and exported for data exploration purposes. Second, the weather station and HOBO datalogger datafiles are collated into a single data frame and matched with budburst observation dates to provide start-to-end temperature vectors for each budburst observation. These vectors are then converted to Growing Degree Hours and Chilling Portions and matched with each observation for later input into the model. Twig-based observations are melted into long-form and each bud observation is associated with an individual row.

### Model run/fit
In our paper, the non-linear cumulative link model uses a logistic function to estimate the saturating effects of chilling on bud development. Other forms are plausible, including a logistic model without random effects, a Monod model, and a linear plateau model. They are included here in `stanfiles/` but not discussed in the main paper. 

In the sampling segment of `Fit_models.R`, the script iterates through each variety, subsamples the training dataset, reformats the data to match each model's Stan script variable requirements, runs MCMC sampling on the model, and saves the model file to disk. The essential component of the Stan script is in the `model{}` section, where the non-linear component is calculated and fed into a cumulative link probability mass function `ordered_logistic_lpmf`. Log probability is updated for each sample and each interation in the MCMC chain, i.e.

```
  for (n in 1:N) { 
    mu_alpha[n] = mu_alpha[n] + (r_1_alpha_1[J_1[n]]) * Z_1_alpha_1[n] + (r_2_alpha_1[J_2[n]])               * Z_2_alpha_1[n]; 
    mu[n] = C_1[n] * (mu_alpha[n] / (1 + exp(-mu_beta - C_2[n]*mu_eta)));
    target += ordered_logistic_lpmf(Y[n] | mu[n], temp_Intercept);
  } 
```

Where `mu_alpha` is a global parameter with varying intercepts per variety-tree-chill-twig observation, and `mu` is the output of the nonlinear heat-chill equation, a function of `alpha` `beta` `eta` heat and chill. Because the `ordered_logistic_lpmf` log probability mass function does not support vector inputs, this script cannot be vectorized to calculate the entire dataset in batch, and instead iterates over each row `n` in the data for each iteration in the MCMC run. 

The posterior predictive distribution (Yrep) and posterior saturation points (Ysat) are estimated simultaneously in the MCMC run via the generated quantities block code, iterated over all training datapoints and marginalizing out varying intercepts.

```
  for (n in 1:N) { 
    mu[n] = C_1[n] * (mu_alpha[n] / (1 + exp(-mu_beta - C_2[n]*mu_eta)));
    Yrep = ordered_logistic_rng(mu[n], temp_Intercept);
    Ysat = -(mu_beta[n] - 2.292432)/mu_eta[n];
  } 
```

also in the generated quantities block are posterior predictions over a range of plausible GDH/CP values (`GDH_Y` and `CP_Y`)

```
  for (n in 1:N_Y) { 
    mu_rep[n] = GDH_Y[n] * (mu_alpha[n] / (1 + exp(-mu_beta - CP_Y[n]*mu_eta)));
    Yhat = ordered_logistic_rng(mu[n], temp_Intercept);
  } 
```

Model fits are not included in this repository as at 2000 iterations each variety's stanfit file is >400MB.

### Model diagnostics
Model diagnostics were performed using the `shinystan` tool, available by running the function `shinystan::launch_shinystan()` on any saved modelfit generated by `Fit_models.R`. R-hat values, chain diagnostics, and divergence information are available, as well as tools for examining and verifying posterior distribution shapes. 

### Figure generation
Figures are generated using the following script files:

`fig7_posterior_violin_plotter.R`, a plot which visualizes the posterior distribution of the NLM saturation threshold and the 50% heuristic threshold.

`fig8_posterior_contour_plotter_3p.R`, a plot which visualizes the posterior distribution 90% credible intervals for saturation and the alpha-cutpoint ratio.
 
`fig9_raw_posterior_activity_plotter.R`, a plot which visualizes the raw data for bud activity, 50% threshold, and NLM saturation thresholds. 

The main processor bottleneck for these figure-generating function is loading stanfits into memory and extracting samples - posterior predictive samples are already present in these files and do not need to be generated.

