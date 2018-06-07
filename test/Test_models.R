
# Variable intercept test -------------------------------------------------

d <- read.csv("./datafiles/Raw_combined_excel.csv")
varlist <- unique(d$variety)

fit_bayes <- function(model,filename,variety_in) { 
  tempbr <- d %>%   #### Import raw data
    mutate(treesub = paste0(tree,sub)) %>%
    filter(!(treesub %in% c("61", "62"))) %>%
    melt(measure=c(6:20)) %>%                        #### Mutate into long form w/bud observations
    filter(!is.na(CP_c),                             #### Remove any missing CP obs
           variety == variety_in) %>%                    #### Set variety to run (zero pooling model)
    mutate(value = as.integer(value + 1)) %>%        #### Format ordinal outcome to be positive         
    make_standata(bf(value ~ GDH_c*(Asym/(1+exp((xmid-CP_c)/scal))),
                     scal + xmid~ 1, Asym ~ (1|tree/sub), nl = TRUE),data=., family = gaussian,
                  chains=4, cores=4,
                  prior = c(prior(uniform(.5, 10), nlpar = "Asym"),
                            prior(uniform(0, 10), nlpar = "xmid"),
                            prior(uniform(0, 10), nlpar = "scal")))
  
  tempbr$CP_Y <- rep(seq(0,4.5,length.out=10),10)
  tempbr$GDH_Y <- rep(seq(0,4.5,length.out=10),each=10)
  tempbr$N_Y <- 100
  
  fit.test <- stan(file=paste(model), data = tempbr, #### Sampler command
                   iter=2800, warmup = 800, chains=1, cores=1,
                   control = list(adapt_delta = 0.95))
  
  save(fit.test, file=paste("./modelfits/",variety_in,filename,sep="")) #### Save model fit
}

# FIT/SAVE 3P LOGISTIC LOOP

for(i in 1:9) fit_bayes("./stanfiles/NL_cerezos_3p_vi_form.stan","_3p_vi_form",varlist[i])
