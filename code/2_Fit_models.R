# Model fitting
#
# This code uses the collated and transformed data produced by `Data_preparation.R` to fit a
# variety of Stan-based nonlinear models. First, the input data is further manipulated and 
# filtered to transform the outcome variable and separate the dataset into variety-specific
# subsets. Then, the `make_standata` function from the `brms` library is used to generate the
# input vectors and matrices appropriate for the called .stan files, and some additional `dplyr`
# functions provide vectors for posterior predictive distributions in the MCMC. The final list of 
# transformed data is pushed to the `stan` function from `rstan`, which performs MCMC sampling. 
# Model fits are stored in the `modelfits/` directory.
#
# Note that the only sampling loop uncommented is the varying-intercept logistic model. If this 
# file is run as-is, those will be the only files generated.


d <- read.csv("./data/Raw_combined_excel.csv")

# GENERATE VARLIST

varlist <- unique(d$variety)


# 
# # FIT PLATEAU/MONOD FUNCTION
# 
# fit_bayes <- function(model,filename,variety_in) { 
#   tempbr <- d %>%   #### Import raw data
#     mutate(treesub = paste0(tree,sub)) %>%
#     filter(!(treesub %in% c("61", "62"))) %>%
#     melt(measure=c(6:20)) %>%                        #### Mutate into long form w/bud observations
#     filter(!is.na(CP_c),                             #### Remove any missing CP obs
#            variety == variety_in) %>%                    #### Set variety to run (zero pooling model)
#     mutate(value = as.integer(value + 1)) %>%        #### Format ordinal outcome to be positive         
#     make_standata(bf(value ~ GDH_c*umax*CP_c*k, #### Generate model matrices + vectors
#                      umax + k ~ 1, 
#                      nl = TRUE),
#                   ., family = gaussian(),
#                   prior = c(prior(normal(.5, 10), nlpar = "umax"),
#                             prior(normal(0, 10), nlpar = "k")))
#   
#   fit.test <- stan(file=paste(model), data = tempbr, #### Sampler command
#                    iter=2000, warmup = 200, chains=4, cores=4,
#                    control = list(adapt_delta = 0.95))
#   
#   save(fit.test, file=paste("./modelfits/",variety_in,filename,sep="")) #### Save model fit
# }
# 
# # FIT/SAVE PLATEAU LOOP
# 
# for(i in 1:9) fit_bayes("NL_cerezos_plat.stan","_plat",varlist[i])
# 
# # FIT/SAVE MONOD LOOP
# 
# for(i in 1:9) fit_bayes("NL_cerezos_monod.stan","_monod",varlist[i])
# 
# # FIT 3P LOGISTIC
# 
# fit_bayes <- function(model,filename,variety_in) { 
#   tempbr <- d %>%   #### Import raw data
#     mutate(treesub = paste0(tree,sub)) %>%
#     filter(!(treesub %in% c("61", "62"))) %>%
#     melt(measure=c(6:20)) %>%                        #### Mutate into long form w/bud observations
#     filter(!is.na(CP_c),                             #### Remove any missing CP obs
#            variety == variety_in) %>%                    #### Set variety to run (zero pooling model)
#     mutate(value = as.integer(value + 1)) %>%        #### Format ordinal outcome to be positive         
#     make_standata(bf(value ~ GDH_c*(Asym/(1+exp((xmid-CP_c)/scal))),
#                      Asym + scal + xmid ~ 1, 
#                      nl = TRUE),
#                   ., family = gaussian(),
#                   prior = c(prior(normal(.5, 10), nlpar = "Asym"),prior(normal(0, 10), nlpar = "xmid"),prior(normal(0, 10), nlpar = "scal")))
#   
#   
#   fit.test <- stan(file=paste(model), data = tempbr, #### Sampler command
#                    iter=2000, warmup = 200, chains=1, cores=1,
#                    control = list(adapt_delta = 0.95))
#   
#   save(fit.test, file=paste("./modelfits/",variety_in,filename,sep="")) #### Save model fit
# }
# 
# # FIT/SAVE 3P LOGISTIC LOOP
# 
# for(i in 1:9) fit_bayes("./stanfiles/NL_cerezos_3p.stan","_3p",varlist[i])

##



# FIT 3P LOGISTIC MODIFIED

fit_bayes <- function(model,filename,variety_in) { 
  tempbr <- d %>% 
    mutate(treesub = paste0(chill,sub),
           vartree = as.integer(as.factor(paste0(variety,tree)))) %>%
    melt(measure=c(6:20)) %>%           
    filter(!is.na(CP_c), variety == variety_in) %>%                 
    mutate(value = as.integer(value + 1), 
           variable = scale(as.integer(variable))[,1]) %>%              
    make_standata(bf(value ~ GDH_c*(alpha/(1+exp(-beta-CP_c*eta))),
                     beta + eta~ 1, 
                     alpha ~ 1 + (1|vartree) + (1|treesub), 
                     nl = TRUE),
                  data=., 
                  family = gaussian,
                  chains=4, 
                  cores=4,
                  prior = c(prior(normal(0, 10), nlpar = "alpha"),
                            prior(normal(0, 10), nlpar = "beta"),
                            prior(normal(0, 10), nlpar = "eta")))
  
  tempbr$CP_Y <- rep(seq(0,4.5,length.out=10),10)
  tempbr$GDH_Y <- rep(seq(0,4.5,length.out=10),each=10)
  tempbr$N_Y <- 100
  
  fit.test <- stan(file=paste(model), data = tempbr, #### Sampler command
                   iter=3000, warmup = 1000, chains=1, cores=1,
                   control = list(adapt_delta = 0.95))
  
  save(fit.test, file=paste("./modelfits/",variety_in,filename,sep="")) #### Save model fit
  print(variety_in)
}


# FIT/SAVE 3P LOGISTIC MODIFIED LOOP

for(i in 1:9) fit_bayes("./stan/NL_cerezos_3p_vi_updated.stan","_3p_updated",varlist[i])

##






