d <- read.csv("./datafiles/Raw_combined_excel.csv")
varlist <- unique(d$variety)



tempbr <- d %>% 
  mutate(treesub = paste0(tree,sub),
         vartree = as.integer(as.factor(paste0(variety,tree)))) %>%
  filter(!(treesub %in% c("31","32","41","42","51","52","61","62"))) %>%
  melt(measure=c(6:20)) %>%           
  filter(!is.na(CP_c)) %>%                 
  mutate(value = as.integer(value + 1)) %>%              
  make_standata(bf(value ~ GDH_c*(Asym/(1+exp((xmid-CP_c)/scal))),
                   scal + xmid ~ variety, 
                   Asym ~ variety +  (1|vartree/sub), 
                   nl = TRUE),
                data=., 
                family = gaussian,
                chains=4, 
                cores=4,
                prior = c(prior(uniform(.5, 10), nlpar = "Asym"),
                          prior(uniform(0, 10), nlpar = "xmid"),
                          prior(uniform(0, 10), nlpar = "scal")))

tempbr$CP_Y <- rep(seq(0,4.5,length.out=10),10)
tempbr$GDH_Y <- rep(seq(0,4.5,length.out=10),each=10)
tempbr$N_Y <- 100

fit.test <- stan(file="stanfiles/NL_cerezos_3p_vi_pooled_form.stan", data = tempbr, #### Sampler command
                 iter=2800, warmup = 800, chains=1, cores=1,
                 control = list(adapt_delta = 0.95))

save(fit.test, file=paste("./modelfits/",variety_in,filename,sep="")) #### Save model fit
}


