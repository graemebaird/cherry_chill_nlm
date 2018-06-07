d <- read.csv("datafiles/Raw_combined_excel.csv")

varlist <- unique(d$variety)

tempbr <- d %>% 
  mutate(treesub = paste0(tree,sub)) %>%
  filter(!(treesub %in% c("41", "42", "51", "52", "61", "62"))) %>%
  melt(measure=c(6:20)) %>%           
  filter(!is.na(CP_c), variety == variety_in) %>%                 
  mutate(value = as.integer(value + 1)) %>%              
  make_standata(bf(value ~ GDH_c*(alpha/(1+exp(-beta-CP_c*eta))),
                   beta + eta~ 1, 
                   alpha ~ 1 + (1|tree/sub), 
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

fit.test <- stan(file="./stanfiles/NL_cerezos_3p_vi_updated.stan", data = tempbr, #### Sampler command
                   iter=2000, warmup = 200, chains=1, cores=1,
                   control = list(adapt_delta = 0.95))
  