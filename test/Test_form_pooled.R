tempbr <- d %>% 
  mutate(treesub = paste0(chill,sub),
         vartree = as.integer(as.factor(paste0(variety,tree)))) %>%
  melt(measure=c(6:20)) %>%           
  filter(!is.na(CP_c)) %>%                 
  mutate(value = as.integer(value + 1)) %>%              
  make_standata(bf(value ~ GDH_c*(alpha/(1+exp(-beta-CP_c*eta))),
                   beta + eta~ variety, 
                   alpha ~ variety + (1|vartree/treesub), 
                   nl = TRUE),
                data=., 
                family = gaussian,
                chains=4, 
                cores=4,
                prior = c(prior(normal(0, 10), nlpar = "alpha"),
                          prior(normal(0, 10), nlpar = "beta"),
                          prior(normal(0, 10), nlpar = "eta")))

tempbr$CP_Y <- rep(seq(0,4.5,length.out=10),90)
tempbr$GDH_Y <- rep(rep(seq(0,4.5,length.out=10),each=10),9)
tempbr$N_Y <- 900



fit.test <- stan(file="./stanfiles/NL_cerezos_3p_vi_updated_test2.stan", data = tempbr, #### Sampler command
                 iter=2800, warmup = 800, chains=1, cores=1,
                 control = list(adapt_delta = 0.95))





tempbr <- d %>% 
  mutate(treesub = paste0(chill,sub),
         vartree = as.integer(as.factor(paste0(variety,tree)))) %>%
  filter(!(treesub %in% c("61","62"))) %>%
  melt(measure=c(6:20)) %>%           
  filter(!is.na(CP_c)) %>%                 
  mutate(value = as.integer(value + 1)) %>%              
  make_stancode(bf(value ~ GDH_c*(alpha/(1+exp(-beta-CP_c*eta))),
                   beta + eta~ variety, 
                   alpha ~ variety + (1|vartree/treesub), 
                   nl = TRUE),
                data=., 
                family = gaussian,
                chains=4, 
                cores=4,
                prior = c(prior(normal(0, 10), nlpar = "alpha"),
                          prior(normal(0, 10), nlpar = "beta"),
                          prior(normal(0, 10), nlpar = "eta")))