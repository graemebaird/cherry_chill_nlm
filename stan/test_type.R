tempbr <-
  d %>% 
  mutate(treesub = paste0(chill,sub),
         vartree = as.integer(as.factor(paste0(variety,tree)))) %>%
  melt(measure=c(6:20)) %>%           
  filter(!is.na(CP_c)) %>%                 
  mutate(value = as.integer(value + 1), 
         position = scale(as.integer(variable))[,1]) %>%  
  group_by(variety, tree, chill, sub, variable) %>%
  filter(variable %in% c('s5', 's6')) %>%           
    make_standata(bf(value ~ GDH_c*(alpha/(1+exp(-beta-CP_c*eta))),
                     alpha + beta + eta ~ 1 + variety + (1|vartree) + (1|treesub), 
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

fit.test <- stan(file="./stan/NL_part_pool.stan", data = tempbr, #### Sampler command
                 iter=1000, warmup = 100, chains=1, cores=1,
                 control = list(adapt_delta = 0.98))
