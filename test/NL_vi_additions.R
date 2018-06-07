d <- read.csv("./datafiles/Raw_combined_excel.csv")

# GENERATE VARLIST

varlist <- unique(d$variety)


tempbr <- d %>% 
  mutate(treesub = paste0(chill,sub),
         vartree = as.integer(as.factor(paste0(variety,tree)))) %>%
  filter(!(treesub %in% c("61","62"))) %>%
  melt(measure=c(6:20)) %>%           
  filter(!is.na(CP_c), variety == variety_in) %>%                 
  mutate(value = as.integer(value + 1)) %>%         
  make_standata(bf(value ~ GDH_c*alpha*CP_c*beta,      #### Generate model matrices + vectors
                   alpha + beta ~ 1 + (1|vartree/treesub), 
                   nl = TRUE),
                ., family = gaussian(),
                prior = c(prior(normal(0, 10), nlpar = "alpha"),
                          prior(normal(0, 10), nlpar = "beta")))

fit.test <- stan(file="./stanfiles/NL_cerezos_plat_vi_form.stan", data = tempbr, #### Sampler command
                 iter=2000, warmup = 200, chains=4, cores=4,
                 control = list(adapt_delta = 0.95))
